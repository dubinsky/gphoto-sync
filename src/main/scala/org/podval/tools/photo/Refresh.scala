package org.podval.tools.photo

import scala.annotation.tailrec
import java.io.File
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NFiles, StandardCopyOption, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions

private[photo] final case class AlbumCard(
  id: String,
  title: String,
  countText: String,
  auth: String,
  leaves: String = ""
)

// Gradle tests pass a fake. The Playwright class is loaded only when this default is called.
private[photo] trait PhotosUi:
  def albumCards(): Seq[AlbumCard]
  def readOwner(card: AlbumCard): Option[String]
  def readAlbum(card: AlbumCard): Catalog.Album
  def close(): Unit

// A refresh fake is not a mutation fake.
private[photo] trait PhotosAct extends PhotosUi:
  def renameAlbum(album: Catalog.Album, updated: String): Unit
  def setDescription(item: Catalog.Item, text: String): Unit
  def setDate(item: Catalog.Item, local: String): Unit
  def downloadItem(item: Catalog.Item, destination: File): Unit
  def putInAlbum(item: Catalog.Item, destination: Catalog.Album, current: String): Catalog.Item
  def takeOut(album: Catalog.Album, item: Catalog.Item): Unit
  def trash(album: Catalog.Album, item: Catalog.Item): Unit
  def uploadItem(destination: Catalog.Album, file: File, current: String): Catalog.Item
  def createAlbum(title: String): (Seq[AlbumCard], Option[Catalog.Album])
  def deleteAlbum(album: Catalog.Album): Seq[AlbumCard]

final class Refresh(args: Cli.RefreshArgs) extends Operation:
  def execute(session: Session): (Session, Result) =
    Refresh.run(session, args, Refresh.openUi, Refresh.placesOf())

object Refresh:
  private[photo] final case class Notice(phase: String, counts: Seq[(String, String)])
  private[photo] final case class Places(directory: File, lock: File)

  private[photo] enum ReadKind derives CanEqual:
    case Ok
    case Short
    case Other

  private[photo] val AlreadyRunning: String = "already running"
  private[photo] val MutationFailed: String = "mutation failed"

  // confirmed means the edit was committed. A miss before that is one-click.
  private[photo] final class ActMiss(detail: String, val confirmed: Boolean) extends IllegalStateException(detail)

  private[photo] var placesOf: () => Places = () =>
    val root: String = Option(System.getenv("TMPDIR")).filter(_.nonEmpty).getOrElse("/tmp")
    Places(File(root, "gphoto-sync-refresh"), File(root, "gphoto-sync-refresh.lock"))

  private[photo] var openUi: (Notice => Unit) => PhotosUi = notice => loadUi(notice)

  // Gradle tests pass a fake. The Playwright class is loaded only when this default is called.
  private[photo] var openAct: (Notice => Unit) => PhotosAct = notice => PlaywrightPhotosUi.open(notice)

  private[photo] def needsAnother(kinds: Seq[ReadKind]): Boolean =
    if kinds.exists(_ == ReadKind.Ok) then false
    else if kinds.isEmpty then true
    else if kinds.forall(_ == ReadKind.Short) then kinds.length < 3
    else kinds.length < 2

  private[photo] def run(
    session: Session,
    args: Cli.RefreshArgs,
    open: (Notice => Unit) => PhotosUi,
    places: Places,
    pause: () => Unit = () => Thread.sleep(500)
  ): (Session, Result) =
    try
      claim(places.lock) match
        case Claim.Busy =>
          busy(session, places)
        case Claim.Owned(channel, lock) =>
          try
            work(session, args, open, places, pause)
          catch case error: Exception =>
            (session, Result.Usage(oneLine(error)))
          finally
            release(channel, lock)
    catch case error: Exception =>
      (session, Result.Usage(oneLine(error)))

  private def work(
    session: Session,
    args: Cli.RefreshArgs,
    open: (Notice => Unit) => PhotosUi,
    places: Places,
    pause: () => Unit
  ): (Session, Result) =
    if lockInside(places) then throw IllegalStateException("refresh lock is inside the work directory")
    val (stored, source) = loadCatalog(session)
    val resumed: Boolean = canResume(places.directory)
    if !resumed then wipe(places.directory) else ensure(places.directory)
    val state = new RunState
    if resumed then state.phase = readStatus(File(places.directory, "status.txt")).getOrElse("phase", "items")
    var ui: Option[PhotosUi] = None
    try
      val lines: Seq[String] =
        if resumed then readLines(File(places.directory, "albums.tsv"))
        else
          state.mark(places.directory, "scroll", Seq("seen" -> "0"))
          val opened: PhotosUi = open(notice => state.mark(places.directory, notice.phase, notice.counts))
          ui = Some(opened)
          val cards: Seq[AlbumCard] = opened.albumCards()
          state.mark(places.directory, "scroll", Seq("seen" -> cards.length.toString))
          cards.map(lineOf)
      RefreshPlan.diff(lines, stored, args.mode, resumed || args.allowShrink) match
        case Left(message) =>
          state.mark(places.directory, "error", Seq("error" -> "list"))
          (session, Result.Usage(message))
        case Right(found) if args.list =>
          (session, Result.Ok(textOf(found, listing = true), valueOf(found, listing = true)))
        case Right(found) =>
          if !resumed then
            atomicWrite(File(places.directory, "albums.tsv"), lines.map(_ + "\n").mkString)
            state.mark(places.directory, "items", Seq("seen" -> lines.length.toString))
          val opened: PhotosUi = ui.getOrElse:
            val created: PhotosUi = open(notice => state.mark(places.directory, notice.phase, notice.counts))
            ui = Some(created)
            created
          val cards: Map[String, AlbumCard] = lines.map(cardOf).map(card => card.id -> card).toMap
          val winners: Map[String, Catalog.Album] = readItems(places.directory, opened, found, cards, state, pause)
          val owners: Map[String, Catalog.Album] = readOwners(opened, found, cards, pause)
          publish(session, places, state, found, lines, stored, source, resumed || args.allowShrink, winners ++ owners)
    finally
      ui.foreach(closeQuiet)

  private def publish(
    session: Session,
    places: Places,
    state: RunState,
    found: RefreshPlan.Diff,
    lines: Seq[String],
    stored: Seq[Catalog.Album],
    source: String,
    allowShrink: Boolean,
    reads: Map[String, Catalog.Album]
  ): (Session, Result) =
    val directory: File = File(session.root.configuration.archive, ".gphoto-sync")
    if !directory.isDirectory then directory.mkdirs()
    RefreshPlan.plan(lines, stored, found.mode, allowShrink, reads, source) match
      case Left(message) =>
        state.mark(places.directory, "error", Seq("error" -> "plan"))
        (session, Result.Usage(message))
      case Right(prepared) =>
        RefreshPlan.publish(directory, prepared) match
          case Left(message) =>
            state.mark(places.directory, "error", Seq("error" -> "check"))
            (session, Result.Usage(message))
          case Right(()) =>
            state.mark(places.directory, "done", Seq.empty)
            deleteQuiet(places.directory)
            (
              session.copy(catalogCache = None),
              Result.Ok(textOf(found, listing = false), valueOf(found, listing = false))
            )

  private def readItems(
    directory: File,
    ui: PhotosUi,
    found: RefreshPlan.Diff,
    cards: Map[String, AlbumCard],
    state: RunState,
    pause: () => Unit
  ): Map[String, Catalog.Album] =
    val planById: Map[String, RefreshPlan.Card] = found.cards.map(card => card.id -> card).toMap
    val order: Seq[String] = found.itemReads.toSeq.sortBy(id => planById.get(id).fold(0)(_.count))
    def loop(pass: Int, memory: Map[String, Seq[Catalog.Album]]): Unit =
      if pass <= 4 then
        state.mark(directory, "items", Seq(
          "pass" -> pass.toString,
          "need" -> order.count(id => !hasWinner(directory, id)).toString,
          "done" -> order.count(id => hasWinner(directory, id)).toString,
          "total" -> order.length.toString
        ))
        val (next, wrote) = order.foldLeft((memory, false)):
          case ((acc, wrote), id) =>
            val soFar: Seq[Catalog.Album] = acc.getOrElse(id, Seq.empty)
            if hasWinner(directory, id) || !needsAnother(soFar.map(kindOf)) then (acc, wrote)
            else
              val card: AlbumCard = cards.getOrElse(id, throw IllegalArgumentException("bad album key"))
              val plan: RefreshPlan.Card = planById.getOrElse(id, throw IllegalArgumentException("bad album key"))
              val (reads, failed) = pull(ui, card, plan, soFar)
              if settle(directory, id, reads, failed) then
                pause()
                (acc.updated(id, reads), true)
              else (acc.updated(id, reads), wrote)
        if wrote then loop(pass + 1, next)
    if order.nonEmpty then loop(1, Map.empty)
    order.flatMap(id => loadWinner(directory, id).map(id -> _)).toMap

  // The three shortfall reads stay in memory. One winner file is written after they settle.
  private def pull(
    ui: PhotosUi,
    card: AlbumCard,
    plan: RefreshPlan.Card,
    acc: Seq[Catalog.Album]
  ): (Seq[Catalog.Album], Option[String]) =
    if !needsAnother(acc.map(kindOf)) then (acc, None)
    else
      try pull(ui, card, plan, acc :+ normalize(ui.readAlbum(card), plan))
      catch case error: Exception =>
        (acc, Some(oneLine(error)))

  private def settle(directory: File, id: String, reads: Seq[Catalog.Album], failed: Option[String]): Boolean =
    val settled: Boolean = reads.nonEmpty && !needsAnother(reads.map(kindOf))
    if settled then
      writeWinner(directory, id, choose(reads))
      true
    else if failed.isDefined then
      writeFailed(directory, id, failed.get)
      true
    else false

  private def choose(reads: Seq[Catalog.Album]): Catalog.Album =
    if reads.map(kindOf).forall(_ == ReadKind.Short) then pickShort(reads)
    else pickMerge(reads)

  private def pickShort(reads: Seq[Catalog.Album]): Catalog.Album =
    val head: Catalog.Album = reads.head
    val keys: Set[String] = itemKeys(head)
    if reads.forall(album => itemKeys(album) == keys && album.count == head.count) then head
    else reads.reduceLeft((best, album) => if itemKeys(album).size > itemKeys(best).size then album else best)

  private def pickMerge(reads: Seq[Catalog.Album]): Catalog.Album =
    reads.reduceLeft: (best, album) =>
      if best.error.isDefined && album.error.isEmpty then album
      else if best.error.isEmpty && album.error.isDefined then best
      else if album.items.length > best.items.length then album
      else best

  private def kindOf(album: Catalog.Album): ReadKind = album.error match
    case Some(error) if error.nameFailures == 0 && error.got < error.of => ReadKind.Short
    case None if album.unsets.isEmpty && album.items.length == album.count => ReadKind.Ok
    case _ => ReadKind.Other

  private def normalize(read: Catalog.Album, plan: RefreshPlan.Card): Catalog.Album =
    val error: Option[Catalog.Error] = read.error.map: err =>
      if err.of == read.count then err.copy(of = plan.count) else err
    read.copy(id = plan.id, count = plan.count, title = plan.title, autoAdd = false, error = error)

  private def readOwners(
    ui: PhotosUi,
    found: RefreshPlan.Diff,
    cards: Map[String, AlbumCard],
    pause: () => Unit
  ): Map[String, Catalog.Album] =
    val storedById: Map[String, Catalog.Album] = found.stored.map(album => album.id -> album).toMap
    val listed: Set[String] = found.cards.map(_.id).toSet
    val ids: Seq[String] = storedById.collect:
      case (id, album) if album.autoAdd && listed.contains(id) => id
    .toSeq
    val wanted: Seq[String] = found.mode match
      case RefreshPlan.Mode.Album(_) => ids.filter(found.named.contains)
      case _ => ids
    val planById: Map[String, RefreshPlan.Card] = found.cards.map(card => card.id -> card).toMap
    wanted.sortBy(id => planById.get(id).fold(0)(_.count)).foldLeft(Map.empty[String, Catalog.Album]): (acc, id) =>
      val card: AlbumCard = cards.getOrElse(id, throw IllegalArgumentException("bad album key"))
      val plan: RefreshPlan.Card = planById.getOrElse(id, throw IllegalArgumentException("bad album key"))
      val owner: Option[String] = retryOwner(ui, card)
      pause()
      acc.updated(id, stub(plan, owner))

  private def retryOwner(ui: PhotosUi, card: AlbumCard): Option[String] =
    def once(left: Int): Option[String] =
      if left == 0 then throw IllegalStateException("owner read failed")
      else
        try ui.readOwner(card)
        catch case _: Exception => once(left - 1)
    once(4)

  private def stub(plan: RefreshPlan.Card, owner: Option[String]): Catalog.Album =
    Catalog.Album(plan.id, plan.count, plan.title, owner, autoAdd = true, None, Seq.empty, Seq.empty)

  private def writeWinner(directory: File, id: String, album: Catalog.Album): Unit =
    val rendered: String = RefreshPlan.render(Seq(forPart(album)))
    val nl: Int = rendered.indexOf('\n')
    val body: String = if nl < 0 then rendered else rendered.drop(nl + 1)
    atomicWrite(partFile(directory, id), s"# part\t$id\n$body")

  private def writeFailed(directory: File, id: String, message: String): Unit =
    atomicWrite(partFile(directory, id), s"# part\t$id\n# FAILED\t$message\n")

  private def loadWinner(directory: File, id: String): Option[Catalog.Album] =
    if !hasWinner(directory, id) then None
    else
      val text: String = NFiles.readString(partFile(directory, id).toPath, StandardCharsets.UTF_8)
      val nl: Int = text.indexOf('\n')
      val body: String = if nl < 0 then "" else text.drop(nl + 1)
      Some(parseBody(body))

  private def hasWinner(directory: File, id: String): Boolean =
    if !safe(id) then false
    else
      val file: File = File(File(directory, "parts"), id)
      file.isFile && !failedText(NFiles.readString(file.toPath, StandardCharsets.UTF_8))

  private def failedText(text: String): Boolean =
    text.split("\n", -1).exists(_.startsWith("# FAILED"))

  // The album key is the part filename. Anything else could write outside parts/.
  private def partFile(directory: File, id: String): File =
    if !safe(id) then throw IllegalArgumentException("bad album key")
    val parts: File = File(directory, "parts")
    if !parts.isDirectory then
      parts.mkdirs()
      NFiles.setPosixFilePermissions(parts.toPath, PosixFilePermissions.fromString("rwx------"))
    File(parts, id)

  private def safe(id: String): Boolean = id.matches("[A-Za-z0-9_-]+")

  // A part has to parse. A tab or newline in a filename is encoded here and restored by parseBody.
  // The catalog render keeps those characters, so the check rejects the partial.
  private def forPart(album: Catalog.Album): Catalog.Album =
    val hide: Catalog.Item => Boolean = item => badName(item.filename) || item.filename.contains('\u0001')
    if !album.items.exists(hide) then album
    else album.copy(
      items = album.items.map: item =>
        if hide(item) then item.copy(filename = escapeName(item.filename, restore = false)) else item
      ,
      error = album.error.map(_.copy(nameFailures = album.unsets.length))
    )

  private def badName(filename: String): Boolean =
    filename.exists(ch => ch == '\t' || ch == '\n' || ch == '\r')

  private[photo] def parseBody(body: String): Catalog.Album =
    decodeNames(readCatalogBody(body))

  private def readCatalogBody(body: String): Catalog.Album =
    val text: String = if body.endsWith("\n") then body else body + "\n"
    val wrapped: String = "# gphoto-catalog 2\n" + text
    val tmp = NFiles.createTempFile("gphoto-album", ".txt")
    try
      NFiles.setPosixFilePermissions(tmp, PosixFilePermissions.fromString("rw-------"))
      NFiles.writeString(tmp, wrapped, StandardCharsets.UTF_8)
      val albums: Seq[Catalog.Album] = Catalog.read(tmp.toFile)
      if albums.isEmpty then throw IllegalArgumentException("missing album")
      else albums.head
    finally
      NFiles.deleteIfExists(tmp)

  private def decodeNames(album: Catalog.Album): Catalog.Album =
    if !album.items.exists(_.filename.contains('\u0001')) then album
    else
      val items: Seq[Catalog.Item] = album.items.map: item =>
        if item.filename.contains('\u0001') then item.copy(filename = escapeName(item.filename, restore = true))
        else item
      val bad: Int = items.count(item => badName(item.filename))
      if bad == 0 then album.copy(items = items)
      else
        val got: Int = items.length + album.unsets.length
        val of: Int = album.error.fold(album.count)(_.of)
        album.copy(items = items, error = Some(Catalog.Error(got, of, album.unsets.length + bad)))

  // Hide a tab, newline, CR, or a literal U+0001 so a part stays one column. The catalog partial keeps the original characters.
  private def escapeName(text: String, restore: Boolean): String =
    if !restore then
      text.flatMap:
        case '\u0001' => "\u0001\u0001"
        case '\t' => "\u0001t"
        case '\n' => "\u0001n"
        case '\r' => "\u0001r"
        case ch => String.valueOf(ch)
    else
      @tailrec
      def rec(rest: String, acc: String): String =
        if rest.isEmpty then acc
        else
          val (plain, marked) = rest.span(_ != '\u0001')
          if marked.isEmpty then acc + plain
          else if marked.startsWith("\u0001\u0001") then rec(marked.drop(2), acc + plain + "\u0001")
          else if marked.startsWith("\u0001t") then rec(marked.drop(2), acc + plain + "\t")
          else if marked.startsWith("\u0001n") then rec(marked.drop(2), acc + plain + "\n")
          else if marked.startsWith("\u0001r") then rec(marked.drop(2), acc + plain + "\r")
          else rec(marked.drop(1), acc + plain + "\u0001")
      rec(text, "")

  private def itemKeys(album: Catalog.Album): Set[String] = album.items.map(_.key).toSet

  private def textOf(found: RefreshPlan.Diff, listing: Boolean): String =
    val lines: Seq[String] = found.actions.flatMap(action => actionLines(action, listing, found.mode))
    val kept: Int = keptCount(found, listing)
    val shown: Seq[String] =
      if found.mode == RefreshPlan.Mode.Incremental || kept > 0 then lines :+ s"kept $kept" else lines
    shown.map(_ + "\n").mkString

  private def keptCount(found: RefreshPlan.Diff, listing: Boolean): Int =
    found.actions.count:
      case action: RefreshPlan.Action.Keep => extraLines(action, listing, found.mode).isEmpty
      case _ => false

  private def actionLines(action: RefreshPlan.Action, listing: Boolean, mode: RefreshPlan.Mode): Seq[String] =
    baseLines(action) ++ extraLines(action, listing, mode)

  private def baseLines(action: RefreshPlan.Action): Seq[String] = action match
    case RefreshPlan.Action.Add(card) => Seq(s"new ${card.title} ${card.count}")
    case RefreshPlan.Action.CountChange(stored, card) => Seq(s"reread ${card.title} ${stored.count} ${card.count}")
    case RefreshPlan.Action.Retitle(_, card) => Seq(s"retitle ${card.title}")
    case RefreshPlan.Action.Keep(_, _) => Seq.empty
    case RefreshPlan.Action.Drop(stored) => Seq(s"gone ${stored.title}")

  private def extraLines(action: RefreshPlan.Action, listing: Boolean, mode: RefreshPlan.Mode): Seq[String] =
    if !listing then Seq.empty
    else mode match
      case RefreshPlan.Mode.Incremental => Seq.empty
      case RefreshPlan.Mode.Full => listedExtra(action, owner = false)
      case RefreshPlan.Mode.Album(_) => listedExtra(action, owner = true)

  private def listedExtra(action: RefreshPlan.Action, owner: Boolean): Seq[String] = action match
    case RefreshPlan.Action.Drop(_) => Seq.empty
    case other if isAuto(other) =>
      val card: RefreshPlan.Card = planCard(other).get
      val verb: String = if owner then "owner-only" else "auto-add"
      Seq(s"$verb ${card.title} ${card.count}")
    case other =>
      planCard(other).toSeq.map(card => s"would-read ${card.title} ${card.count}")

  private def isAuto(action: RefreshPlan.Action): Boolean = action match
    case RefreshPlan.Action.Add(_) => false
    case RefreshPlan.Action.CountChange(album, _) => album.autoAdd
    case RefreshPlan.Action.Retitle(album, _) => album.autoAdd
    case RefreshPlan.Action.Keep(album, _) => album.autoAdd
    case RefreshPlan.Action.Drop(album) => album.autoAdd

  private def planCard(action: RefreshPlan.Action): Option[RefreshPlan.Card] = action match
    case RefreshPlan.Action.Add(card) => Some(card)
    case RefreshPlan.Action.CountChange(_, card) => Some(card)
    case RefreshPlan.Action.Retitle(_, card) => Some(card)
    case RefreshPlan.Action.Keep(_, card) => Some(card)
    case RefreshPlan.Action.Drop(_) => None

  private def valueOf(found: RefreshPlan.Diff, listing: Boolean): Json =
    Json.Obj(Seq(
      "kept" -> Json.Long(keptCount(found, listing)),
      "rows" -> Json.Arr(found.actions.map(actionValue))
    ))

  private def actionValue(action: RefreshPlan.Action): Json =
    val (verb, id, title, count) = action match
      case RefreshPlan.Action.Add(card) => ("new", card.id, card.title, card.count)
      case RefreshPlan.Action.CountChange(_, card) => ("reread", card.id, card.title, card.count)
      case RefreshPlan.Action.Retitle(_, card) => ("retitle", card.id, card.title, card.count)
      case RefreshPlan.Action.Keep(_, card) => ("keep", card.id, card.title, card.count)
      case RefreshPlan.Action.Drop(album) => ("gone", album.id, album.title, album.count)
    Json.Obj(Seq(
      "verb" -> Json.Str(verb),
      "id" -> Json.Str(id),
      "title" -> Json.Str(title),
      "count" -> Json.Long(count)
    ))

  private[photo] def lineOf(card: AlbumCard): String =
    Seq(card.title, card.countText, card.id, card.auth, card.leaves).mkString("\t")

  private[photo] def cardOf(line: String): AlbumCard =
    val fields: Seq[String] = line.split("\t", -1).toSeq
    val leaves: String = if fields.length > 4 then fields.drop(4).mkString("\t") else ""
    AlbumCard(fields(2), fields.head, fields(1), fields.lift(3).getOrElse(""), leaves)

  private def loadCatalog(session: Session): (Seq[Catalog.Album], String) =
    val file: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    if !file.isFile then (Seq.empty, "")
    else (Catalog.read(file), NFiles.readString(file.toPath, StandardCharsets.UTF_8))

  private def canResume(directory: File): Boolean =
    readStatus(File(directory, "status.txt")).get("phase").contains("items") &&
      File(directory, "albums.tsv").isFile

  private def readLines(file: File): Seq[String] =
    NFiles.readString(file.toPath, StandardCharsets.UTF_8).split("\n", -1).toSeq.filter(_.nonEmpty)

  private def readStatus(file: File): Map[String, String] =
    if !file.isFile then Map.empty
    else
      NFiles.readString(file.toPath, StandardCharsets.UTF_8).split("\n", -1).toSeq.filter(_.nonEmpty).flatMap: line =>
        val (key, rest) = line.span(_ != '=')
        if key.nonEmpty && rest.startsWith("=") then Some(key -> rest.drop(1)) else None
      .toMap

  private[photo] def busy(session: Session, places: Places): (Session, Result) =
    val status: String =
      if File(places.directory, "status.txt").isFile then
        NFiles.readString(File(places.directory, "status.txt").toPath, StandardCharsets.UTF_8)
      else ""
    (session, Result.Failed(s"$AlreadyRunning\n$status", Json.Null, AlreadyRunning))

  // String startsWith would treat the sibling lock name as inside the work directory.
  private def lockInside(places: Places): Boolean =
    places.lock.toPath.normalize.startsWith(places.directory.toPath.normalize)

  private def wipe(directory: File): Unit =
    if directory.exists then deleteTree(directory)
    ensure(directory)

  private def ensure(directory: File): Unit =
    NFiles.createDirectories(directory.toPath)
    NFiles.setPosixFilePermissions(directory.toPath, PosixFilePermissions.fromString("rwx------"))

  private def atomicWrite(file: File, text: String): Unit =
    val parent: File = file.getParentFile
    val tmp = NFiles.createTempFile(parent.toPath, ".part-", ".tmp")
    try
      NFiles.writeString(tmp, text, StandardCharsets.UTF_8)
      NFiles.move(tmp, file.toPath, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch case error: Exception =>
      NFiles.deleteIfExists(tmp)
      throw error

  private def deleteQuiet(directory: File): Unit =
    try deleteTree(directory)
    catch case _: Exception => ()

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).toSeq.flatten.foreach(deleteTree)
    if file.exists then NFiles.delete(file.toPath)

  private def closeQuiet(ui: PhotosUi): Unit =
    try ui.close()
    catch case _: Exception => ()

  private def oneLine(error: Exception): String =
    val message: String = Operation.messageOf(error).replace('\t', ' ').replace('\r', ' ').replace('\n', ' ')
    if message.isEmpty then "refresh failed" else message

  private def loadUi(notice: Notice => Unit): PhotosUi =
    PlaywrightPhotosUi.open(notice)

  // A check failure is the only writer of phase error. Leaving items lets the next process resume.
  private final class RunState:
    var phase: String = "start"
    def mark(directory: File, next: String, counts: Seq[(String, String)]): Unit =
      val keepItems: Boolean = phase == "items" && next != "items" && next != "done" && next != "error"
      if !keepItems then
        phase = next
        writeStatus(directory, next, counts)

  private def writeStatus(directory: File, phase: String, counts: Seq[(String, String)]): Unit =
    val rows: Seq[(String, String)] =
      Seq("heartbeat" -> System.currentTimeMillis.toString, "phase" -> phase) ++ counts
    atomicWrite(File(directory, "status.txt"), rows.map((key, value) => s"$key=$value\n").mkString)

  // Closing any descriptor for this file drops every fcntl lock this process holds.
  private[photo] enum Claim:
    case Owned(channel: FileChannel, lock: FileLock)
    case Busy

  private var ownerChannel: FileChannel = null
  private var ownerPath: String = null
  private var pinned: List[FileChannel] = Nil

  private[photo] def claim(lockFile: File): Claim = synchronized {
    if held(lockFile) then Claim.Busy
    else
      val channel: FileChannel = FileChannel.open(
        lockFile.toPath,
        StandardOpenOption.CREATE,
        StandardOpenOption.READ,
        StandardOpenOption.WRITE
      )
      NFiles.setPosixFilePermissions(lockFile.toPath, PosixFilePermissions.fromString("rw-------"))
      try
        val lock: FileLock = channel.tryLock()
        if lock == null then
          channel.close()
          Claim.Busy
        else
          ownerChannel = channel
          ownerPath = lockFile.getAbsolutePath
          Claim.Owned(channel, lock)
      catch case _: OverlappingFileLockException =>
        pinned = channel :: pinned
        Claim.Busy
  }

  private def held(lockFile: File): Boolean =
    ownerChannel != null && ownerChannel.isOpen && ownerPath == lockFile.getAbsolutePath

  private[photo] def release(channel: FileChannel, lock: FileLock): Unit = synchronized {
    try if lock != null && lock.isValid then lock.release()
    catch case _: Exception => ()
    if ownerChannel eq channel then
      ownerChannel = null
      ownerPath = null
    try channel.close()
    catch case _: Exception => ()
  }
