package org.podval.tools.photo

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.key.TomlKey
import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.{IteratorHasAsScala, ListHasAsScala, SetHasAsScala}
import java.io.File
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.charset.StandardCharsets
import java.nio.file.{FileAlreadyExistsException, Files as NFiles, StandardCopyOption, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import java.time.LocalDate
import java.util.UUID
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock

private[photo] enum Publication:
  case Pending(publishedTitle: Option[String])
  case Published(id: String, title: String)

private[photo] enum AlbumRole:
  case External(googleTitle: Option[String], owner: Option[String])
  case Client(googleTitle: Option[String], publication: Publication)

// One album. The title is not the path: the file sits in the start folder, and a span's end is `to-` in the name.
final case class Album private (
  title: String,
  span: Option[Title.Span],
  include: Seq[Include.Entry],
  private[photo] val role: AlbumRole
):
  // fromProduct and copy call this constructor and skip the factories.
  role match
    case AlbumRole.Client(_, Publication.Published(id, _)) =>
      require(id.nonEmpty, "id must be non-empty")
    case AlbumRole.External(googleTitle, owner) =>
      owner.foreach: name =>
        require(!name.trim.isEmpty, "owner must be a non-empty display name")
        require(googleTitle.exists(_.nonEmpty), "owner requires a non-empty google-title")
    case AlbumRole.Client(_, Publication.Pending(_)) =>
      ()

  def external: Boolean = role match
    case _: AlbumRole.External => true
    case _: AlbumRole.Client => false

  def googleAlbumId: String = role match
    case AlbumRole.Client(_, Publication.Published(id, _)) => id
    case _ => ""

  def publishedTitle: String = role match
    case AlbumRole.Client(_, Publication.Published(_, stored)) => stored
    case AlbumRole.Client(_, Publication.Pending(stored)) => stored.getOrElse("")
    case _: AlbumRole.External => ""

  def googleTitle: String = role match
    case AlbumRole.External(stored, _) => stored.getOrElse("")
    case AlbumRole.Client(stored, _) => stored.getOrElse("")

  def owner: Option[String] = role match
    case AlbumRole.External(_, stored) => stored
    case _: AlbumRole.Client => None

  def desiredGoogleTitle: String =
    if googleTitle.nonEmpty then googleTitle else title

  def startDir: String = span.fold(title)(_.startDir)

  def claims(day: LocalDate, stem: String): Boolean =
    include.exists(entry => Include.claims(span, entry, day, stem))

  def positive(day: LocalDate, stem: String): Boolean =
    include.exists(entry => Include.positive(span, entry, day, stem))

object Album:
  private[photo] def external(
    title: String,
    span: Option[Title.Span],
    include: Seq[Include.Entry],
    googleTitle: Option[String],
    owner: Option[String]
  ): Album =
    val keptTitle: Option[String] = googleTitle.filter(_.nonEmpty)
    owner.foreach: name =>
      require(!name.trim.isEmpty, "owner must be a non-empty display name")
      require(keptTitle.nonEmpty, "owner requires a non-empty google-title")
    new Album(title, span, include, AlbumRole.External(keptTitle, owner))

  private[photo] def pending(
    title: String,
    span: Option[Title.Span],
    include: Seq[Include.Entry],
    googleTitle: Option[String] = None,
    publishedTitle: Option[String] = None
  ): Album =
    new Album(
      title,
      span,
      include,
      AlbumRole.Client(
        googleTitle.filter(_.nonEmpty),
        Publication.Pending(publishedTitle.filter(_.nonEmpty))
      )
    )

  private[photo] def published(
    title: String,
    span: Option[Title.Span],
    include: Seq[Include.Entry],
    id: String,
    publishedTitle: String,
    googleTitle: Option[String] = None
  ): Album =
    require(id.nonEmpty, "id must be non-empty")
    new Album(
      title,
      span,
      include,
      AlbumRole.Client(
        googleTitle.filter(_.nonEmpty),
        Publication.Published(id, publishedTitle)
      )
    )

  def relative(originals: File, file: File): String =
    originals.toPath.relativize(file.toPath).iterator.asScala.map(_.toString).mkString("/")

  def fileFor(originals: File, title: String): File =
    Title.parse(title) match
      case Some(parsed) => Title.file(originals, parsed)
      case None => throw IllegalArgumentException(s"not an album title: $title")

  def read(originals: File, file: File): Album =
    val parsed: Title.Parsed = Title.fromLocation(directoryOf(originals, file), stemOf(file)).getOrElse(
      throw IllegalArgumentException(s"$file: not an album file")
    )
    val values: Map[String, TomlValue] = checked(file, keys)
    val common: Common = commonOf(file, values)
    val span: Option[Title.Span] = parsed.span
    val include: Seq[Include.Entry] = stringSeq(file, values, "include").map: text =>
      val entry: Include.Entry =
        try Include.parse(text)
        catch case error: IllegalArgumentException =>
          reject(file, "include", Album.messageOf(error))
      if Include.inside(span, entry) then entry
      else reject(file, "include", s"'$text' is outside the album")
    val googleTitle: Option[String] = Option.when(common.googleTitle.nonEmpty)(common.googleTitle)
    val role: AlbumRole =
      if common.external then AlbumRole.External(googleTitle, common.owner)
      else if common.googleAlbumId.nonEmpty then
        AlbumRole.Client(googleTitle, Publication.Published(common.googleAlbumId, common.publishedTitle))
      else
        AlbumRole.Client(
          googleTitle,
          Publication.Pending(Option.when(common.publishedTitle.nonEmpty)(common.publishedTitle))
        )
    new Album(parsed.title, span, include, role)

  // Hand-written order.
  def renderFile(album: Album): String =
    def googleLine(googleTitle: Option[String]): Seq[String] =
      googleTitle.filter(_.nonEmpty).map(text => s"google-title = ${quote(text)}").toSeq
    val head: Seq[String] = album.role match
      case AlbumRole.External(googleTitle, owner) =>
        googleLine(googleTitle) ++ owner.map(name => s"owner = ${quote(name)}").toSeq
      case AlbumRole.Client(googleTitle, Publication.Pending(_)) =>
        Seq("external = false") ++ googleLine(googleTitle)
      case AlbumRole.Client(googleTitle, Publication.Published(id, published)) =>
        Seq("external = false") ++ googleLine(googleTitle) ++ Seq(
          s"google-album-id = ${quote(id)}",
          s"published-title = ${quote(published)}"
        )
    val lines: Seq[String] =
      head ++ (if album.include.isEmpty then Seq.empty else includeLines(album.include))
    if lines.isEmpty then "" else lines.mkString("", "\n", "\n")

  private[photo] final case class Loaded(file: File, album: Album)

  private[photo] def needsRecursive(prefix: Option[String]): Boolean =
    val segments: Seq[String] = prefix.filter(_.nonEmpty).toSeq.flatMap(_.split('/').filter(_.nonEmpty))
    segments.isEmpty || (segments.sizeIs == 1 && DateTree.isNumber(segments.head, 4))

  def pictureMatches(path: String, prefix: Option[String]): Boolean =
    prefix.filter(_.nonEmpty).forall(selected => path == selected || path.startsWith(s"$selected/"))

  def albumMatches(startDir: String, title: String, prefix: Option[String]): Boolean =
    prefix.filter(_.nonEmpty).forall: selected =>
      val titleHit: Boolean = title == selected || title.startsWith(s"$selected/")
      val startHit: Boolean = startDir == selected || startDir.startsWith(s"$selected/")
      titleHit || startHit

  private[photo] def messageOf(error: Exception): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)

  def quote(text: String): String =
    "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private[photo] def quoteInclude(text: String): String =
    if text.indexOf('\'') >= 0 then quote(text) else "'" + text + "'"

  // Present keys are one single-line assignment each. The write splices the original text.
  // The read is inside the lock. Text read before tryLock is not spliced.
  def storeGoogle(file: File, googleAlbumId: String, publishedTitle: String): Unit =
    rejectControl(file, "google-album-id", googleAlbumId)
    rejectControl(file, "published-title", publishedTitle)
    withWriteLock:
      val text: String = Files.read(file)
      // Held here, the file is still the bytes just read. The write is below.
      afterCompute()
      if text.isBlank then Files.write(blankGoogle(googleAlbumId, publishedTitle), file)
      else
        val table: TomlTable = JToml.jToml.readFromString(text)
        val values: Map[String, String] = Map(
          "google-album-id" -> googleAlbumId,
          "published-title" -> publishedTitle
        )
        val present: Set[String] = values.keySet.filter(key => hasKey(table, key))
        Files.write(joinLines(spliceGoogle(file, splitLines(text), values, present)), file)

  // Recompress each include entry that claims one of these stems. An empty album writes include = [].
  // The read is inside the lock. A next include computed before tryLock is not spliced.
  def storeRemovals(
    file: File,
    originals: File,
    removals: Seq[(LocalDate, String)],
    universe: Map[LocalDate, Set[String]]
  ): Unit =
    withWriteLock:
      val album: Album = read(originals, file)
      val next: Seq[Include.Entry] = removals.foldLeft(album.include): (include, pair) =>
        Include.drop(include, album.span, pair, universe).sortBy(Include.logical)
      // Held here, the file is still the bytes just read. The rename is the splice below.
      afterCompute()
      if !sameEntries(next, album.include) then spliceInclude(file, next)

  // Curated include paths, before any picture moves. A bad curated file fails first.
  // Each file is re-read inside its own lock. The picture set stays outside that lock.
  def rewriteInclude(originals: File, from: String, to: String): Unit =
    if from != to then
      val fromPair: (LocalDate, String) = splitPath(from)
      val toPair: (LocalDate, String) = splitPath(to)
      val universe: Map[LocalDate, Set[String]] = Archive.universe(Archive.pictures(originals))
      val files: Seq[File] = curatedFiles(originals)
      files.foreach(file => read(originals, file))
      files.foreach: file =>
        withWriteLock:
          val album: Album = read(originals, file)
          val next: Seq[Include.Entry] = Include.relocate(album.include, album.span, fromPair, toPair, universe)
          if !sameEntries(next, album.include) then spliceInclude(file, next)

  // Same reads as rewriteInclude, no write. A bad curated file fails before a move.
  private[photo] def planCurated(
    originals: File,
    from: (LocalDate, String),
    to: (LocalDate, String)
  ): Seq[(File, Seq[Include.Entry])] =
    if from._1.equals(to._1) && from._2 == to._2 then Seq.empty
    else
      val universe: Map[LocalDate, Set[String]] = PlanningWalk.planningUniverse(originals)
      val files: Seq[File] = curatedFiles(originals)
      files.foreach(file => read(originals, file))
      files.flatMap: file =>
        val album: Album = read(originals, file)
        val next: Seq[Include.Entry] = Include.relocate(album.include, album.span, from, to, universe)
        if sameEntries(next, album.include) then None
        else
          renderedInclude(file, next)
          Some(file -> next)

  private[photo] def planDated(
    originals: File,
    from: (LocalDate, String),
    to: (LocalDate, String),
    fromPath: String
  ): Seq[(File, Seq[Include.Entry])] =
    val universe: Map[LocalDate, Set[String]] = PlanningWalk.planningUniverse(originals)
    val loaded: Seq[Loaded] = PlanningWalk.planningAlbums(originals)
    val outside: Seq[String] =
      loaded.flatMap: one =>
        one.album.span match
          case Some(span) if one.album.claims(from._1, from._2) && !span.contains(to._1) =>
            Some(one.album.title)
          case _ =>
            None
    if outside.nonEmpty then
      throw IllegalArgumentException(
        s"$fromPath is claimed by ${outside.sorted.mkString(", ")}, which does not contain ${to._1}"
      )
    val edits: Seq[(File, Seq[Include.Entry])] =
      loaded.flatMap: one =>
        one.album.span match
          case None =>
            None
          case Some(_) if !one.album.claims(from._1, from._2) =>
            None
          case Some(_) =>
            val rewrite: Seq[Include.Entry] =
              one.album.include.collect:
                case entry @ Include.Entry.Dates(_, Some(_), _)
                    if Include.claims(one.album.span, entry, from._1, from._2) &&
                       !Include.claims(one.album.span, entry, to._1, to._2) =>
                  entry
            if rewrite.isEmpty then None
            else
              // Album span None: prepare cannot emit Entry.All.
              // Entry.Dates claims and expand use the entry span.
              val rewritten: Seq[Include.Entry] =
                Include.add(
                  Include.drop(rewrite, None, from, universe),
                  None,
                  to,
                  universe
                )
              val rewriteSet: Set[Include.Entry] = rewrite.toSet
              val (prefix, tail) = one.album.include.span(entry => !rewriteSet.contains(entry))
              val rest: Seq[Include.Entry] = tail.drop(1).filterNot(rewriteSet.contains)
              Some(one.file -> (prefix ++ rewritten ++ rest))
    edits.foreach((file, include) => renderedInclude(file, include))
    edits

  def picturePath(path: String): Option[(LocalDate, String)] =
    path.split('/').filter(_.nonEmpty).toSeq match
      case Seq(year, month, day, stem)
          if DateTree.isNumber(year, 4) && DateTree.isNumber(month, 2) && DateTree.isNumber(day, 2) =>
        Some((LocalDate.of(year.toInt, month.toInt, day.toInt), stem))
      case _ =>
        None

  def splitPath(path: String): (LocalDate, String) =
    picturePath(path).getOrElse(throw IllegalArgumentException(s"not a picture path: $path"))

  private def sameEntries(left: Seq[Include.Entry], right: Seq[Include.Entry]): Boolean =
    left.map(Include.render).equals(right.map(Include.render))

  private def directoryOf(originals: File, file: File): String =
    val directory: String = relative(originals, file.getParentFile)
    if directory == "." then "" else directory

  private def stemOf(file: File): String =
    val (name, extension) = Files.nameAndExtension(file)
    if extension.contains("toml") then name
    else throw IllegalArgumentException(s"$file: not an album file")

  // Empty include writes `include = []`. Callers that mean "no key" must not call this for a missing span.
  // The include argument was computed inside this same lock. The toggle handler does not call this.
  private[photo] def putInclude(file: File, include: Seq[Include.Entry]): Unit =
    withWriteLock:
      spliceInclude(file, include)

  private[photo] def renderedInclude(file: File, include: Seq[Include.Entry]): String =
    val text: String = Files.read(file)
    val lines: Seq[TextLine] = splitLines(text)
    val block: Seq[TextLine] =
      if include.isEmpty then Seq(TextLine("include = []", ""))
      else blockLines(includeLines(include))
    val next: Seq[TextLine] = findInclude(file, lines) match
      case Some((from, to)) => replaceSpan(lines, from, to, block)
      case None if includePresent(text) =>
        throw IllegalArgumentException(s"$file: include array does not end on its own line")
      case None => appendBlock(lines, block)
    joinLines(next)

  // Unlocked splice. Equal text skips the rename. The caller holds the write lock.
  private[photo] def spliceInclude(file: File, include: Seq[Include.Entry]): Unit =
    val text: String = Files.read(file)
    val rendered: String = renderedInclude(file, include)
    if rendered != text then replaceAtomic(file, rendered, replace = true)

  // Checks and the create share one lock, so two creates of the same title cannot both pass.
  private[photo] def create(originals: File, album: Album): File =
    val parsed: Title.Parsed = Title.parse(album.title).getOrElse(
      throw IllegalArgumentException(s"not an album title: ${album.title}")
    )
    val file: File = Title.file(originals, parsed)
    withWriteLock:
      if file.exists then throw IllegalArgumentException(s"album already exists: ${album.title}")
      val parent: File = file.getParentFile
      if parent == null || !parent.isDirectory then throw IllegalArgumentException("no such directory")
      try replaceAtomic(file, renderFile(album), replace = false)
      catch case _: FileAlreadyExistsException =>
        throw IllegalArgumentException(s"album already exists: ${album.title}")
    file

  // The scope is the whole call, not one lock. withWriteLock reads it and does not clear it.
  private[photo] def withWriteLockDirectory[A](directory: File)(body: => A): A =
    val previous: Option[File] = writeLockDirectory.get
    writeLockDirectory.set(Some(directory))
    try body
    finally
      if previous == null then writeLockDirectory.remove()
      else writeLockDirectory.set(previous)

  // One deadline covers the in-process nest and the file lock. The same thread reenters and does not open a channel.
  private[photo] def withWriteLock[A](body: => A): A =
    val deadline: Long = System.nanoTime() + writeWaitNanos
    val acquired: Boolean =
      try writeGate.tryLock(math.max(0L, deadline - System.nanoTime()), TimeUnit.NANOSECONDS)
      catch case _: InterruptedException =>
        Thread.currentThread.interrupt()
        false
    if !acquired then throw IllegalArgumentException("album file is busy")
    try
      if writeGate.getHoldCount > 1 then body
      else withFileLock(deadline)(body)
    finally writeGate.unlock()

  private[photo] def writeWaiters: Int = writeGate.getQueueLength

  @volatile private[photo] var afterCompute: () => Unit = () => ()

  private val writeGate: ReentrantLock = new ReentrantLock()
  private val writeLockDirectory = new ThreadLocal[Option[File]]
  private val writeWaitNanos: Long = 5000000000L
  private val lockName: String = "album-write.lock"
  private val lockMode: String = "rw-------"

  private def withFileLock[A](deadline: Long)(body: => A): A =
    lockPlace() match
      case Left(_) =>
        body
      case Right(dir) =>
        Serve.prepare(dir)
        val lockFile: File = File(dir, lockName)
        // One channel for the whole wait. Closing it would drop this JVM's fcntl lock.
        val channel: FileChannel = FileChannel.open(
          lockFile.toPath,
          StandardOpenOption.CREATE,
          StandardOpenOption.READ,
          StandardOpenOption.WRITE
        )
        try
          NFiles.setPosixFilePermissions(lockFile.toPath, PosixFilePermissions.fromString(lockMode))
          val lock: FileLock = pollLock(channel, deadline)
          try body
          finally if lock.isValid then lock.release()
        finally channel.close()

  @tailrec
  private def pollLock(channel: FileChannel, deadline: Long): FileLock =
    val locked: FileLock =
      try channel.tryLock()
      catch case _: OverlappingFileLockException => null
    if locked != null then locked
    else if System.nanoTime() >= deadline then throw IllegalArgumentException("album file is busy")
    else
      try Thread.sleep(20)
      catch case _: InterruptedException =>
        Thread.currentThread.interrupt()
        throw IllegalArgumentException("album file is busy")
      pollLock(channel, deadline)

  // ThreadLocal when the caller set it, else the running web server, else the serve runtime directory.
  private def lockPlace(): Either[String, File] =
    val chosen: Option[File] = writeLockDirectory.get
    if chosen != null && chosen.isDefined then Right(chosen.get)
    else Web.handlerRuntime match
      case Some(dir) => Right(dir)
      case None => Serve.runtimeDirectory()

  private def replaceAtomic(file: File, text: String, replace: Boolean): Unit =
    val parent: File = file.getParentFile
    val temp: File = File(parent, s".${file.getName}.${UUID.randomUUID}.partial")
    var renamed: Boolean = false
    try
      writeForced(temp, text)
      if replace then
        NFiles.move(
          temp.toPath,
          file.toPath,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
      else NFiles.move(temp.toPath, file.toPath, StandardCopyOption.ATOMIC_MOVE)
      renamed = true
    finally if !renamed then NFiles.deleteIfExists(temp.toPath)

  private def writeForced(file: File, text: String): Unit =
    val channel: FileChannel = FileChannel.open(
      file.toPath,
      StandardOpenOption.CREATE_NEW,
      StandardOpenOption.WRITE
    )
    try
      NFiles.setPosixFilePermissions(file.toPath, PosixFilePermissions.fromString(lockMode))
      val bytes: ByteBuffer = ByteBuffer.wrap(text.getBytes(StandardCharsets.UTF_8))
      while bytes.hasRemaining do channel.write(bytes)
      channel.force(true)
    finally channel.close()

  // jtoml sorts keys and drops comments, so a title change is one assignment line.
  // googleTitle None leaves that key. Some(None) removes it. Some(Some(text)) sets it.
  // publishedTitle None leaves that key.
  private[photo] def spliceTitles(
    file: File,
    googleTitle: Option[Option[String]],
    publishedTitle: Option[String]
  ): Unit =
    googleTitle.foreach(_.foreach(text => rejectControl(file, "google-title", text)))
    publishedTitle.foreach(text => rejectControl(file, "published-title", text))
    val original: String = Files.read(file)
    val titled: Seq[TextLine] = googleTitle match
      case None => splitLines(original)
      case Some(value) => assign(file, splitLines(original), "google-title", value)
    val next: Seq[TextLine] = publishedTitle match
      case None => titled
      case Some(text) => assign(file, titled, "published-title", Some(text))
    val rendered: String = joinLines(next)
    if rendered != original then Files.write(rendered, file)

  // None removes a present line. Leaving published-title alone is the None branch in spliceTitles.
  private def assign(
    file: File,
    lines: Seq[TextLine],
    key: String,
    value: Option[String]
  ): Seq[TextLine] =
    val at: Seq[Int] = lines.zipWithIndex.collect:
      case (line, index) if singleAssignment(line.text, key) => index
    if at.sizeIs > 1 then failGoogle(file, s"$key is not a single line")
    (at.headOption, value) match
      case (None, None) =>
        lines
      case (Some(index), None) =>
        lines.patch(index, Seq.empty, 1)
      case (Some(index), Some(text)) =>
        lines.updated(index, lines(index).copy(text = s"$key = ${quote(text)}"))
      case (None, Some(text)) =>
        insertAssignment(lines, key, TextLine(s"$key = ${quote(text)}", "\n"))

  private def insertAssignment(lines: Seq[TextLine], key: String, added: TextLine): Seq[TextLine] =
    val beforeKey: Set[String] = key match
      case "google-title" => Set("owner", "google-album-id", "published-title", "include")
      case "published-title" => Set("include")
      case _ => Set.empty
    val pivot: Option[Int] = lines.zipWithIndex.collectFirst:
      case (line, index) if beforeKey.exists(name => singleAssignment(line.text, name)) => index
    pivot match
      case Some(index) =>
        val (before, after) = lines.splitAt(index)
        val base: Seq[TextLine] = before match
          case prefix :+ last if last.ending.isEmpty && last.text.nonEmpty =>
            prefix :+ last.copy(ending = "\n")
          case _ => before
        base ++ Seq(added) ++ after
      case None =>
        appendBlock(lines, Seq(added))

  private final case class Common(
    external: Boolean,
    googleAlbumId: String,
    publishedTitle: String,
    googleTitle: String,
    owner: Option[String]
  )

  private val keys: Set[String] =
    Set("external", "google-album-id", "published-title", "google-title", "owner", "include")

  private def commonOf(file: File, values: Map[String, TomlValue]): Common =
    val common: Common = readCommon(file, values)
    checkGoogle(file, common)
    common

  private def readCommon(file: File, values: Map[String, TomlValue]): Common = Common(
    external = boolean(file, values, "external", default = true),
    googleAlbumId = string(file, values, "google-album-id", default = ""),
    publishedTitle = string(file, values, "published-title", default = ""),
    googleTitle = string(file, values, "google-title", default = ""),
    owner = optionalString(file, values, "owner")
  )

  // Spaces are significant. A title of only spaces is a title. Tab, CR, and LF are not.
  private def checkGoogle(file: File, common: Common): Unit =
    rejectControl(file, "google-title", common.googleTitle)
    common.owner.foreach(name => rejectControl(file, "owner", name))
    common.owner.foreach: name =>
      if name.trim.isEmpty then failGoogle(file, "owner must be a non-empty display name")
      else if !common.external then failGoogle(file, "owner requires external = true")
      else if common.googleAlbumId.nonEmpty then failGoogle(file, "owner must not be combined with google-album-id")
      else if common.googleTitle.isEmpty then failGoogle(file, "owner requires a non-empty google-title")
    if common.googleAlbumId.nonEmpty && common.external then
      failGoogle(file, "google-album-id requires external = false")

  private def rejectControl(file: File, key: String, text: String): Unit =
    if text.exists(ch => ch == '\t' || ch == '\r' || ch == '\n') then
      reject(file, key, "must not contain a tab, CR, or LF")

  private def failGoogle(file: File, message: String): Nothing =
    throw IllegalArgumentException(s"$file: $message")

  private def checked(file: File, allowed: Set[String]): Map[String, TomlValue] =
    parsed(file, allowed)._2

  private def parsed(file: File, allowed: Set[String]): (TomlTable, Map[String, TomlValue]) =
    val text: String = Files.read(file)
    val table: TomlTable = if text.isBlank then TomlTable.create() else JToml.jToml.readFromString(text)
    val entries: Seq[(String, TomlValue)] = table.keys(false).asScala.toSeq.flatMap: key =>
      Option(table.get(key)).map(keyName(key) -> _)
    val unknown: Seq[String] = entries.map(_._1).filterNot(allowed.contains)
    if unknown.isEmpty then (table, entries.toMap)
    else throw IllegalArgumentException(s"$file: unknown key '${unknown.mkString(", ")}'")

  private def keyName(key: TomlKey): String =
    if key.size == 1 then key.get(0) else key.toString

  private def optionalString(file: File, values: Map[String, TomlValue], key: String): Option[String] =
    values.get(key).map(asString(file, key, _))

  private def string(file: File, values: Map[String, TomlValue], key: String, default: String): String =
    optionalString(file, values, key).getOrElse(default)

  private def boolean(file: File, values: Map[String, TomlValue], key: String, default: Boolean): Boolean =
    values.get(key).fold(default): value =>
      val primitive: TomlPrimitive = primitiveOf(file, key, value)
      if primitive.isBoolean then primitive.asBoolean
      else reject(file, key, "must be a boolean")

  private def stringSeq(file: File, values: Map[String, TomlValue], key: String): Seq[String] =
    values.get(key).fold(Seq.empty): value =>
      if !value.isArray then reject(file, key, "must be an array of strings")
      else value.asArray.asScala.toSeq.map(asString(file, key, _))

  private def asString(file: File, key: String, value: TomlValue): String =
    val primitive: TomlPrimitive = primitiveOf(file, key, value)
    if primitive.isString then primitive.asString
    else reject(file, key, "must be a string")

  private def primitiveOf(file: File, key: String, value: TomlValue): TomlPrimitive =
    if value.isPrimitive then value.asPrimitive
    else reject(file, key, "must be a primitive")

  private def reject(file: File, key: String, message: String): Nothing =
    throw IllegalArgumentException(s"$file: key '$key' $message")

  private def curatedFiles(originals: File): Seq[File] =
    Option(originals.listFiles).map(_.toSeq).getOrElse(Seq.empty)
      .filter(file => file.isFile && file.getName.endsWith(".toml"))
      .sortBy(_.getName)

  private[photo] def children(directory: File): Seq[File] =
    Option(directory.listFiles).map(_.toSeq).getOrElse(Seq.empty)

  private def includeLines(include: Seq[Include.Entry]): Seq[String] =
    Seq("include = [") ++ include.map(entry => s"  ${quoteInclude(Include.render(entry))},") ++ Seq("]")

  private def blankGoogle(id: String, title: String): String =
    s"external = false\ngoogle-album-id = ${quote(id)}\npublished-title = ${quote(title)}\n"

  private def hasKey(table: TomlTable, name: String): Boolean =
    table.keys(false).asScala.exists(key => keyName(key) == name)

  private[photo] def includePresent(text: String): Boolean =
    !text.isBlank && hasKey(JToml.jToml.readFromString(text), "include")

  private final case class TextLine(text: String, ending: String)

  private def splitLines(text: String): Seq[TextLine] =
    def loop(rest: String): Seq[TextLine] =
      if rest.isEmpty then Seq.empty
      else
        val (body, afterBody) = rest.span(ch => ch != '\n' && ch != '\r')
        val (ending, after) =
          if afterBody.startsWith("\r\n") then ("\r\n", afterBody.substring(2))
          else if afterBody.startsWith("\n") then ("\n", afterBody.substring(1))
          else if afterBody.startsWith("\r") then ("\r", afterBody.substring(1))
          else ("", afterBody)
        TextLine(body, ending) +: loop(after)
    loop(text)

  private def joinLines(lines: Seq[TextLine]): String =
    lines.map(line => line.text + line.ending).mkString

  private def blockLines(bodies: Seq[String]): Seq[TextLine] = bodies match
    case Seq() => Seq.empty
    case Seq(last) => Seq(TextLine(last, ""))
    case head +: tail => TextLine(head, "\n") +: blockLines(tail)

  private def appendBlock(lines: Seq[TextLine], block: Seq[TextLine]): Seq[TextLine] =
    val base: Seq[TextLine] = lines match
      case prefix :+ last if last.ending.isEmpty && last.text.nonEmpty =>
        prefix :+ last.copy(ending = "\n")
      case _ => lines
    val ended: Seq[TextLine] = block match
      case prefix :+ last if last.ending.isEmpty => prefix :+ last.copy(ending = "\n")
      case _ => block
    base ++ ended

  private def replaceSpan(lines: Seq[TextLine], from: Int, to: Int, block: Seq[TextLine]): Seq[TextLine] =
    val (before, rest) = lines.splitAt(from)
    val (span, after) = rest.splitAt(to - from + 1)
    val ending: String = span.lastOption.map(_.ending).getOrElse("")
    val fitted: Seq[TextLine] = block match
      case Seq() => Seq.empty
      case prefix :+ last => prefix :+ last.copy(ending = ending)
    before ++ fitted ++ after

  private def spliceGoogle(
    file: File,
    lines: Seq[TextLine],
    values: Map[String, String],
    present: Set[String]
  ): Seq[TextLine] =
    val slots: Map[String, Int] = present.map: key =>
      val found: Seq[Int] = lines.zipWithIndex.collect:
        case (line, index) if singleAssignment(line.text, key) => index
      if found.sizeIs != 1 then failGoogle(file, s"$key is not a single line")
      key -> found.head
    .toMap
    val missing: Seq[String] = Seq("google-album-id", "published-title").filterNot(present.contains)
    val includeAt: Option[Int] = lines.zipWithIndex.collectFirst:
      case (line, index) if assignmentRest(line.text.trim, "include").isDefined => index
    val replaced: Seq[TextLine] = lines.zipWithIndex.map: (line, index) =>
      slots.collectFirst:
        case (key, at) if at == index => line.copy(text = s"$key = ${quote(values(key))}")
      .getOrElse(line)
    val added: Seq[TextLine] = missing.map(key => TextLine(s"$key = ${quote(values(key))}", "\n"))
    includeAt match
      case Some(index) =>
        val (before, after) = replaced.splitAt(index)
        before ++ added ++ after
      case None =>
        appendBlock(replaced, added)

  private def findInclude(file: File, lines: Seq[TextLine]): Option[(Int, Int)] =
    val opens: Seq[Int] = lines.zipWithIndex.collect:
      case (line, index) if assignmentRest(line.text.trim, "include").isDefined => index
    opens match
      case Seq() => None
      case Seq(index) => Some(includeBounds(file, lines, index))
      case _ => throw IllegalArgumentException(s"$file: include is not a single array")

  private def includeBounds(file: File, lines: Seq[TextLine], index: Int): (Int, Int) =
    val value: String = assignmentRest(lines(index).text.trim, "include").getOrElse("")
    if value == "[" then
      lines.zipWithIndex.drop(index + 1).collectFirst:
        case (line, end) if line.text.trim == "]" => index -> end
      .getOrElse(throw IllegalArgumentException(s"$file: include array does not end on its own line"))
    else if value.startsWith("[") && arrayClosed(value) then index -> index
    else throw IllegalArgumentException(s"$file: include array does not end on its own line")

  private def isTomlWs(ch: Char): Boolean = ch == ' ' || ch == '\t'

  private def singleAssignment(line: String, key: String): Boolean =
    assignmentRest(line.trim, key).exists(valueClosed)

  private def assignmentRest(trimmed: String, key: String): Option[String] =
    keyRest(trimmed, key).flatMap: afterKey =>
      val after: String = afterKey.dropWhile(isTomlWs)
      if after.startsWith("=") then Some(after.substring(1).dropWhile(isTomlWs)) else None

  // A bare key, or a basic or literal quoted key. Space and tab may surround '='.
  private def keyRest(trimmed: String, key: String): Option[String] =
    if trimmed.startsWith("\"") && !trimmed.startsWith("\"\"\"") then
      takeBasic(trimmed.substring(1)).flatMap: (decoded, rest) =>
        if decoded == key then Some(rest) else None
    else if trimmed.startsWith("'") && !trimmed.startsWith("'''") then
      takeLiteral(trimmed.substring(1)).flatMap: (decoded, rest) =>
        if decoded == key then Some(rest) else None
    else if trimmed.startsWith(key) then
      val rest: String = trimmed.substring(key.length)
      if rest.isEmpty || isTomlWs(rest.head) || rest.startsWith("=") then Some(rest) else None
    else None

  // An escaped delimiter does not finish a multiline string. A trailing '\' continues the line.
  private def valueClosed(value: String): Boolean =
    if value.startsWith("\"\"\"") then skipMlBasic(value.substring(3)).exists(tailAfterValue)
    else if value.startsWith("'''") then skipMlLiteral(value.substring(3)).exists(tailAfterValue)
    else true

  private def tailAfterValue(rest: String): Boolean =
    val tail: String = rest.dropWhile(isTomlWs)
    tail.isEmpty || tail.startsWith("#")

  private def arrayClosed(value: String): Boolean =
    closedArray(value).exists(tailAfterValue)

  private def closedArray(value: String): Option[String] =
    if !value.startsWith("[") then None else scanValues(value.substring(1), 1)

  private def scanValues(rest: String, depth: Int): Option[String] =
    if depth == 0 then Some(rest)
    else if rest.isEmpty then None
    else if rest.startsWith("\"\"\"") then
      skipMlBasic(rest.substring(3)).flatMap(next => scanValues(next, depth))
    else if rest.startsWith("'''") then
      skipMlLiteral(rest.substring(3)).flatMap(next => scanValues(next, depth))
    else if rest.startsWith("\"") then
      skipBasic(rest.substring(1)).flatMap(next => scanValues(next, depth))
    else if rest.startsWith("'") then
      skipLiteral(rest.substring(1)).flatMap(next => scanValues(next, depth))
    else if rest.startsWith("#") then None
    else rest.head match
      case '[' => scanValues(rest.substring(1), depth + 1)
      case ']' => scanValues(rest.substring(1), depth - 1)
      case _ => scanValues(rest.substring(1), depth)

  private def skipMlBasic(rest: String): Option[String] =
    if rest.isEmpty then None
    else if rest.startsWith("\\") then
      if rest.length == 1 then None else skipMlBasic(rest.substring(2))
    else if rest.startsWith("\"\"\"") then Some(rest.substring(3))
    else skipMlBasic(rest.substring(1))

  private def skipMlLiteral(rest: String): Option[String] =
    val at: Int = rest.indexOf("'''")
    if at < 0 then None else Some(rest.substring(at + 3))

  private def skipBasic(rest: String): Option[String] = takeBasic(rest).map((_, after) => after)

  private def takeBasic(rest: String): Option[(String, String)] =
    def loop(rest: String, acc: String): Option[(String, String)] =
      if rest.isEmpty then None
      else if rest.startsWith("\\") then
        if rest.length == 1 then None
        else loop(rest.substring(2), acc + unescapeBasic(rest.substring(1, 2)))
      else if rest.startsWith("\"") then Some((acc, rest.substring(1)))
      else loop(rest.substring(1), acc + rest.substring(0, 1))
    loop(rest, "")

  private def unescapeBasic(ch: String): String = ch match
    case "b" => "\b"
    case "t" => "\t"
    case "n" => "\n"
    case "f" => "\f"
    case "r" => "\r"
    case "\"" => "\""
    case "\\" => "\\"
    case other => other

  private def skipLiteral(rest: String): Option[String] = takeLiteral(rest).map((_, after) => after)

  private def takeLiteral(rest: String): Option[(String, String)] =
    def loop(rest: String, acc: String): Option[(String, String)] =
      if rest.isEmpty then None
      else if rest.startsWith("'") then Some((acc, rest.substring(1)))
      else loop(rest.substring(1), acc + rest.substring(0, 1))
    loop(rest, "")
