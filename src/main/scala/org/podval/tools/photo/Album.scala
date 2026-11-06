package org.podval.tools.photo

import org.podval.tools.photo.albumfile.AlbumFile
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.IteratorHasAsScala
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
    case _: AlbumRole.Client   => false

  def googleAlbumId: String = role match
    case AlbumRole.Client(_, Publication.Published(id, _)) => id
    case _                                                 => ""

  def publishedTitle: String = role match
    case AlbumRole.Client(_, Publication.Published(_, stored)) => stored
    case AlbumRole.Client(_, Publication.Pending(stored))      => stored.getOrElse("")
    case _: AlbumRole.External                                 => ""

  def googleTitle: String = role match
    case AlbumRole.External(stored, _) => stored.getOrElse("")
    case AlbumRole.Client(stored, _)   => stored.getOrElse("")

  def owner: Option[String] = role match
    case AlbumRole.External(_, stored) => stored
    case _: AlbumRole.Client           => None

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
      case None         => throw IllegalArgumentException(s"not an album title: $title")

  def read(originals: File, file: File): Album =
    val (title, span, include, role) = AlbumFile.read(originals, file)
    new Album(title, span, include, role)

  def renderFile(album: Album): String = AlbumFile.renderFile(album)

  private[photo] final case class Loaded(file: File, album: Album)

  private[photo] def needsRecursive(prefix: Option[String]): Boolean =
    val segments: Seq[String] = prefix.filter(_.nonEmpty).toSeq.flatMap(_.split('/').filter(_.nonEmpty))
    segments.isEmpty || (segments.sizeIs == 1 && DateTree.isNumber(segments.head, 4))

  def pictureMatches(path: String, prefix: Option[String]): Boolean =
    prefix.filter(_.nonEmpty).forall(selected => path == selected || path.startsWith(s"$selected/"))

  def albumMatches(startDir: String, title: String, prefix: Option[String]): Boolean =
    prefix
      .filter(_.nonEmpty)
      .forall: selected =>
        val titleHit: Boolean = title == selected || title.startsWith(s"$selected/")
        val startHit: Boolean = startDir == selected || startDir.startsWith(s"$selected/")
        titleHit || startHit

  private[photo] def messageOf(error: Exception): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)

  def quote(text: String): String =
    "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  // Present keys are one single-line assignment each. The write splices the original text.
  // The read is inside the lock. Text read before tryLock is not spliced.
  def storeGoogle(file: File, googleAlbumId: String, publishedTitle: String): Unit =
    AlbumFile.rejectControl(file, "google-album-id", googleAlbumId)
    AlbumFile.rejectControl(file, "published-title", publishedTitle)
    withWriteLock:
      val text: String = Files.read(file)
      // Held here, the file is still the bytes just read. The write is below.
      afterCompute()
      if text.isBlank then replaceAtomic(file, AlbumFile.blankGoogle(googleAlbumId, publishedTitle), replace = true)
      else replaceAtomic(file, AlbumFile.spliceGoogleText(file, text, googleAlbumId, publishedTitle), replace = true)

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
          case Some(_) if stemlessClaimMisses(one.album, from, to) =>
            Some(one.album.title)
          case _ =>
            None
    if outside.nonEmpty then
      throw IllegalArgumentException(
        s"$fromPath is claimed by ${outside.distinct.sorted.mkString(", ")}, which does not contain ${to._1}"
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

  private def stemlessClaimMisses(
    album: Album,
    from: (LocalDate, String),
    to: (LocalDate, String)
  ): Boolean =
    album.include.exists:
      case entry @ Include.Entry.Dates(_, None, _) =>
        Include.claims(album.span, entry, from._1, from._2) &&
        !Include.claims(album.span, entry, to._1, to._2)
      case _ =>
        false

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

  // Empty include writes `include = []`. Callers that mean "no key" must not call this for a missing span.
  // The include argument was computed inside this same lock. The toggle handler does not call this.
  private[photo] def putInclude(file: File, include: Seq[Include.Entry]): Unit =
    withWriteLock:
      spliceInclude(file, include)

  private[photo] def renderedInclude(file: File, include: Seq[Include.Entry]): String =
    AlbumFile.renderedInclude(file, include)

  // Unlocked splice. Equal text skips the rename. The caller holds the write lock.
  private[photo] def spliceInclude(file: File, include: Seq[Include.Entry]): Unit =
    val text: String = Files.read(file)
    val rendered: String = renderedInclude(file, include)
    if rendered != text then replaceAtomic(file, rendered, replace = true)

  // Checks and the create share one lock, so two creates of the same title cannot both pass.
  private[photo] def create(originals: File, album: Album): File =
    val parsed: Title.Parsed = Title
      .parse(album.title)
      .getOrElse(
        throw IllegalArgumentException(s"not an album title: ${album.title}")
      )
    val file: File = Title.file(originals, parsed)
    withWriteLock:
      if file.exists then throw IllegalArgumentException(s"album already exists: ${album.title}")
      val parent: File = file.getParentFile
      if parent == null || !parent.isDirectory then throw IllegalArgumentException("no such directory")
      try replaceAtomic(file, renderFile(album), replace = false)
      catch
        case _: FileAlreadyExistsException =>
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
      catch
        case _: InterruptedException =>
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
      catch
        case _: InterruptedException =>
          Thread.currentThread.interrupt()
          throw IllegalArgumentException("album file is busy")
      pollLock(channel, deadline)

  // ThreadLocal when the caller set it, else the running web server, else the serve runtime directory.
  private def lockPlace(): Either[String, File] =
    val chosen: Option[File] = writeLockDirectory.get
    if chosen != null && chosen.isDefined then Right(chosen.get)
    else
      Web.handlerRuntime match
        case Some(dir) => Right(dir)
        case None      => Serve.runtimeDirectory()

  private[photo] def writeAtomic(file: File, text: String, replace: Boolean): Unit =
    withWriteLock:
      replaceAtomic(file, text, replace)

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

  private[photo] def spliceTitles(
    file: File,
    googleTitle: Option[Option[String]],
    publishedTitle: Option[String]
  ): Unit =
    googleTitle.foreach(_.foreach(text => AlbumFile.rejectControl(file, "google-title", text)))
    publishedTitle.foreach(text => AlbumFile.rejectControl(file, "published-title", text))
    withWriteLock:
      val original: String = Files.read(file)
      val rendered: String = AlbumFile.spliceTitles(file, original, googleTitle, publishedTitle)
      if rendered != original then replaceAtomic(file, rendered, replace = true)

  private def curatedFiles(originals: File): Seq[File] =
    Option(originals.listFiles)
      .map(_.toSeq)
      .getOrElse(Seq.empty)
      .filter(file => file.isFile && file.getName.endsWith(".toml"))
      .sortBy(_.getName)

  private[photo] def children(directory: File): Seq[File] =
    Option(directory.listFiles).map(_.toSeq).getOrElse(Seq.empty)

  private[photo] def includePresent(text: String): Boolean =
    AlbumFile.includePresent(text)
