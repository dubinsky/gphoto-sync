package org.podval.tools.photo

import scala.annotation.tailrec

enum PathStyle:
  case OneShot, Cursor

final case class Session(
  root: Root,
  style: PathStyle,
  cursor: Directory
):
  def prompt: String =
    val shown: String = Session.display(cursor)
    if shown.isEmpty then "> " else s"$shown> "

  def pwdLine: String =
    val shown: String = Session.display(cursor)
    if shown.isEmpty then "/\n" else s"$shown\n"

  def parse(path: String): Path = style match
    case PathStyle.OneShot =>
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

  def apply(root: Root): Session = new Session(root, PathStyle.OneShot, root)

  def start: Session = open(PathStyle.OneShot)

  private[photo] def open(style: PathStyle): Session =
    val root: Root = Root(Configuration.read)
    new Session(root, style, root)
