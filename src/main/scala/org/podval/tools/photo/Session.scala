package org.podval.tools.photo

import java.io.File
import scala.annotation.tailrec

enum PathStyle:
  case OneShot, Cursor, Serve

// The bot replaces catalog.txt as a whole file, so size and mtime are the generation.
// A failed parse is not stored. Requests and results are not cached.
private[photo] final case class CatalogCache(
  size: Long,
  mtimeMillis: Long,
  albums: Seq[Catalog.Album]
)

final case class Session(
  root: Root,
  style: PathStyle,
  cursor: Directory,
  private[photo] val index: DirectoryIndex = DirectoryIndex.empty,
  private[photo] val catalogCache: Option[CatalogCache] = None
):
  def prompt: String =
    val shown: String = Session.display(cursor)
    if shown.isEmpty then "> " else s"$shown> "

  def pwdLine: String =
    val shown: String = Session.display(cursor)
    if shown.isEmpty then "/\n" else s"$shown\n"

  def parse(path: String): Path = style match
    // Serve keeps no shell cursor. Every argv path is an archive-root path, same as one-shot.
    case PathStyle.OneShot | PathStyle.Serve =>
      root.parse(path)
    case PathStyle.Cursor =>
      // Leading '/' selects the archive root; Root.parse would drop it.
      val base: Directory = if path.startsWith("/") then root else cursor
      walk(base, path.split('/').toList.filter(_.nonEmpty))

  def parsePicture(path: String): Picture = parse(path) match
    case picture: Picture => picture
    case path => throw IllegalArgumentException(s"'$path' is not a picture")

  @tailrec
  private def walk(current: Path, segments: List[String]): Path = segments match
    case Nil =>
      current
    case "." :: rest =>
      walk(current, rest)
    case ".." :: rest =>
      walk(parentOf(current), rest)
    case _ =>
      val (names, rest) = segments.span(segment => segment != "." && segment != "..")
      current match
        case directory: Directory =>
          walk(directory.item(names), rest)
        case item =>
          throw IllegalArgumentException(s"Item does not have sub-items: $item.item($names)")

  private[photo] def cached(directory: File): Option[DirEntry] = index.get(directory)

  private[photo] def remember(directory: File, entry: DirEntry): Session =
    copy(index = index.updated(directory, entry))

  private[photo] def drop(directory: File): Session =
    if index.contains(directory) then copy(index = index.removed(directory)) else this

  private[photo] def dropAll: Session =
    if index.isEmpty then this else copy(index = DirectoryIndex.empty)

  // Stat first. A hit does not read. Catalog.read throws before the cache is stored.
  private[photo] def loadCatalog: (Session, Seq[Catalog.Album]) =
    val file: File = File(File(root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    if !file.isFile then throw IllegalArgumentException("catalog is absent")
    val size: Long = file.length
    val mtime: Long = file.lastModified
    catalogCache match
      case Some(cache) if cache.size == size && cache.mtimeMillis == mtime =>
        (this, cache.albums)
      case _ =>
        val albums: Seq[Catalog.Album] = Catalog.read(file)
        (copy(catalogCache = Some(CatalogCache(size, mtime, albums))), albums)

  private def parentOf(path: Path): Directory = path match
    case _: Root =>
      throw IllegalArgumentException(".. above the archive root")
    case item: Item =>
      item.parent
    case upload: UploadDirectory =>
      upload.root
    case other =>
      throw IllegalStateException(s"no parent for $other")

object Session:
  def display(directory: Directory): String = directory.toString.stripPrefix("/")

  def apply(root: Root): Session = new Session(root, PathStyle.OneShot, root, DirectoryIndex.empty)

  def start: Session = open(PathStyle.OneShot)

  private[photo] def open(style: PathStyle): Session =
    val root: Root = Root(Configuration.read)
    new Session(root, style, root, DirectoryIndex.empty)
