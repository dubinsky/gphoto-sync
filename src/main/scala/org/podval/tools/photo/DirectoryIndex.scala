package org.podval.tools.photo

import java.io.File
import java.time.Instant

private[photo] final case class FileStamp(name: String, size: Long, mtimeMillis: Long) derives CanEqual

private[photo] final case class ListedExtension(
  name: String,
  fromMetadata: Option[Instant],
  current: Instant
)

// One complete ls fact row. A missing field is a listing miss, even when grouped is filled.
private[photo] final case class ListedPicture(
  stem: String,
  textPath: String,
  hasMetadata: Boolean,
  hasNormal: Boolean,
  extensionNames: Vector[String],
  metadata: Metadata,
  timestampFromName: Option[(Instant, String)],
  audit: PictureAudit.Result,
  extensions: Vector[ListedExtension]
)

private[photo] final case class DirEntry(
  fingerprint: Vector[FileStamp],
  grouped: Seq[(String, Set[String])],
  albums: Seq[Album.Loaded],
  listing: Option[Seq[ListedPicture]]
)

// In-memory for the life of the session. One-shot never reads or fills it.
private[photo] final case class DirectoryIndex(entries: Map[String, DirEntry]):
  def isEmpty: Boolean = entries.isEmpty

  def get(directory: File): Option[DirEntry] = entries.get(DirectoryIndex.key(directory))

  def updated(directory: File, entry: DirEntry): DirectoryIndex =
    DirectoryIndex(entries.updated(DirectoryIndex.key(directory), entry))

  def removed(directory: File): DirectoryIndex =
    DirectoryIndex(entries.removed(DirectoryIndex.key(directory)))

  def contains(directory: File): Boolean = entries.contains(DirectoryIndex.key(directory))

private[photo] object DirectoryIndex:
  val empty: DirectoryIndex = DirectoryIndex(Map.empty)

  def key(file: File): String = file.toPath.toAbsolutePath.normalize.toString

  // Directory mtime is not a shortcut: rewriting a file does not touch the parent.
  // A day fingerprint is its file children. A directory's own mtime is not the bytes inside it.
  def fingerprint(directory: File, filesOnly: Boolean): Vector[FileStamp] =
    val (directories, files) = Files.list(directory)
    val children: Seq[File] = if filesOnly then files else directories ++ files
    children
      .map(file => FileStamp(file.getName, file.length, file.lastModified))
      .sortBy(_.name)
      .toVector

  private[photo] final class Frame(var session: Session, var dropReads: Boolean)

  private val active = new ThreadLocal[Frame]

  // Returns the frame that was current, so end can restore it. Not a drop.
  def begin(session: Session): Option[Frame] =
    val previous: Option[Frame] = Option(active.get)
    active.set(new Frame(session, false))
    previous

  def end(previous: Option[Frame]): Unit = previous match
    case None => active.remove()
    case Some(frame) => active.set(frame)

  def cursor: Option[Session] =
    val frame: Frame = active.get
    if frame == null then None
    else frame.session.style match
      case PathStyle.Cursor => Some(frame.session)
      case PathStyle.OneShot => None

  def now: Session = active.get.session

  def replace(session: Session): Unit =
    active.get.session = session

  def drop(directory: File): Unit =
    val frame: Frame = active.get
    if frame != null then frame.session = frame.session.drop(directory)

  def dropAll(): Unit =
    val frame: Frame = active.get
    if frame != null then frame.session = frame.session.dropAll

  def armDropReads(): Unit =
    val frame: Frame = active.get
    if frame != null then frame.dropReads = true

  def noteRead(directory: File): Unit =
    val frame: Frame = active.get
    if frame != null && frame.dropReads then frame.session = frame.session.drop(directory)
