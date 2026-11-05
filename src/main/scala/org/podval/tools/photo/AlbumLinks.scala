package org.podval.tools.photo

import java.io.File
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NFiles, StandardCopyOption, StandardOpenOption}
import java.util.UUID

// Identity after the first reconcile. Refresh does not write this file.
// Absent until a reconcile has a row: do not create it empty.
object AlbumLinks:
  final case class Row(id: String, path: String) derives CanEqual

  private val Header: String = "# gphoto-album-links 1"
  private val VersionError: String = "album links are not version 1"

  def file(archive: File): File = File(File(archive, ".gphoto-sync"), "album-links.tsv")

  // An absent file is no rows. A blank line is an error: this file has no unused columns to leave empty.
  def read(archive: File): Seq[Row] =
    val path: File = file(archive)
    if !path.isFile then Seq.empty
    else parse(TextLines.read(path, Header, VersionError))

  def parse(text: String): Seq[Row] =
    TextLines
      .dataLines(text)
      .foldLeft(Seq.empty[Row]): (rows, line) =>
        if line.isEmpty then throw IllegalArgumentException("blank line")
        val row: Row = parseRow(line)
        if rows.exists(_.id == row.id) || rows.exists(_.path == row.path) then
          throw IllegalArgumentException("duplicate album link")
        rows :+ row

  def render(rows: Seq[Row]): String =
    val body: String = rows.map(row => s"${row.id}\t${row.path}\n").mkString
    Header + "\n" + body

  // True when the file is created, replaced, or removed.
  def publish(archive: File, rows: Seq[Row]): Boolean =
    val dest: File = file(archive)
    if rows.isEmpty then
      if dest.isFile then
        NFiles.delete(dest.toPath)
        true
      else false
    else
      val text: String = render(rows)
      if dest.isFile && Files.read(dest) == text then false
      else
        move(archive, dest, text, replace = true)
        true

  // A new toml must not replace a file that appeared after the plan. The link file may.
  private[photo] def writeNew(archive: File, dest: File, text: String): Unit =
    if dest.exists then throw IllegalStateException("file exists")
    move(archive, dest, text, replace = false)

  private def parseRow(line: String): Row =
    TextLines.columns(line, 2) match
      case Seq(id, path) =>
        if id.isEmpty || path.isEmpty then throw IllegalArgumentException("empty album link")
        rejectPath(path)
        Row(id, path)
      case _ =>
        throw IllegalArgumentException("wrong column count")

  private def rejectPath(path: String): Unit =
    val segments: Seq[String] = path.split("/", -1).toSeq
    val bad: Boolean =
      path.startsWith("/") || segments.isEmpty || segments.exists(segment =>
        segment.isEmpty || segment == "." || segment == ".."
      )
    if bad then throw IllegalArgumentException("album link path")

  private def move(archive: File, dest: File, text: String, replace: Boolean): Unit =
    val dot: File = File(archive, ".gphoto-sync")
    if !dot.isDirectory && !dot.mkdirs() then throw IllegalStateException("no .gphoto-sync directory")
    val partial: File = File(dot, s"reconcile-${UUID.randomUUID()}.partial")
    var renamed: Boolean = false
    try
      writeForced(partial, text)
      if replace then
        NFiles.move(
          partial.toPath,
          dest.toPath,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
      else NFiles.move(partial.toPath, dest.toPath, StandardCopyOption.ATOMIC_MOVE)
      renamed = true
    finally if !renamed then NFiles.deleteIfExists(partial.toPath)

  private def writeForced(file: File, text: String): Unit =
    val channel: FileChannel = FileChannel.open(
      file.toPath,
      StandardOpenOption.CREATE_NEW,
      StandardOpenOption.WRITE
    )
    try
      val bytes: ByteBuffer = ByteBuffer.wrap(text.getBytes(StandardCharsets.UTF_8))
      while bytes.hasRemaining do channel.write(bytes)
      channel.force(true)
    finally channel.close()
