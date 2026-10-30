package org.podval.tools.photo

import scala.annotation.tailrec
import java.io.File
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.{CREATE_NEW, READ, WRITE}
import java.nio.file.{FileAlreadyExistsException, Files as NFiles, NoSuchFileException}
import java.util.concurrent.ConcurrentHashMap

object Requests:
  enum Action(val wire: String) derives CanEqual:
    case RenameAlbum extends Action("rename-album")
    case MoveItem extends Action("move-item")
    case SetDescription extends Action("set-description")
    case SetDate extends Action("set-date")
    case DownloadItem extends Action("download-item")
    case DeleteItem extends Action("delete-item")

  object Action:
    def fromWire(text: String): Action = text match
      case "rename-album" => Action.RenameAlbum
      case "move-item" => Action.MoveItem
      case "set-description" => Action.SetDescription
      case "set-date" => Action.SetDate
      case "download-item" => Action.DownloadItem
      case "delete-item" => Action.DeleteItem
      case _ => throw IllegalArgumentException("unknown request action")

  final case class Row(
    id: String,
    action: Action,
    albumId: String,
    itemKey: String,
    current: String,
    `new`: String
  ) derives CanEqual

  private val Header: String = "# gphoto-requests 2"
  private val VersionError: String = "requests are not version 2"
  private val gates: ConcurrentHashMap[String, AnyRef] = ConcurrentHashMap()

  // Test seams. beforeCreateNew runs after the absence check and before CREATE_NEW.
  // afterLock runs while the exclusive FileChannel lock is held.
  // onOverlap runs when this JVM already holds that lock, which does not wait.
  @volatile private[photo] var beforeCreateNew: () => Unit = () => ()
  @volatile private[photo] var afterLock: () => Unit = () => ()
  @volatile private[photo] var onOverlap: () => Unit = () => ()

  // Same monitor and exclusive lock as append, so a same-JVM read cannot close another descriptor
  // and drop the writer's fcntl lock, and so it waits until the new line is complete.
  // An absent file is no rows and is not created.
  def read(file: File): Seq[Row] =
    if !file.isFile then Seq.empty
    else gate(file).synchronized:
      if !file.isFile then Seq.empty
      else openRead(file) match
        case None => Seq.empty
        case Some(channel) =>
          try
            withExclusiveLock(channel):
              parse(TextLines.fromBytes(readBytes(channel), Header, VersionError))
          finally channel.close()

  // Absent file: CREATE_NEW, then the same exclusive lock as an existing file.
  // The file is visible before the lock, so the body re-reads and does not overwrite a committed row.
  def append(
    file: File,
    action: Action,
    albumId: String,
    itemKey: String,
    current: String,
    `new`: String,
    id: Option[String] = None
  ): Row =
    Seq(albumId, itemKey, current, `new`).foreach(rejectField)
    id.foreach(requireId)
    gate(file).synchronized:
      val channel: FileChannel = openChannel(file)
      try
        withExclusiveLock(channel):
          commit(channel, id, action, albumId, itemKey, current, `new`)
      finally channel.close()

  private[photo] def requireId(id: String): Unit =
    if !id.matches("r[1-9][0-9]*") then throw IllegalArgumentException("request id is not valid")

  private def parse(text: String): Seq[Row] = rowsOf(TextLines.dataLines(text))

  private def rowsOf(lines: Seq[String]): Seq[Row] =
    lines.filter(_.nonEmpty).foldLeft(Seq.empty[Row]): (rows, line) =>
      val row: Row = parseRow(line)
      if rows.exists(_.id == row.id) then throw IllegalArgumentException("duplicate request id")
      rows :+ row

  private def parseRow(line: String): Row =
    TextLines.columns(line, 6) match
      case Seq(id, action, albumId, itemKey, current, updated) =>
        requireId(id)
        Row(id, Action.fromWire(action), albumId, itemKey, current, updated)
      case _ =>
        throw IllegalArgumentException("wrong column count")

  private def openRead(file: File): Option[FileChannel] =
    try Some(FileChannel.open(file.toPath, READ, WRITE))
    catch case _: NoSuchFileException => None

  private def openChannel(file: File): FileChannel =
    val path = file.toPath
    if NFiles.exists(path) then FileChannel.open(path, READ, WRITE)
    else
      try
        beforeCreateNew()
        FileChannel.open(path, CREATE_NEW, READ, WRITE)
      catch case _: FileAlreadyExistsException =>
        FileChannel.open(path, READ, WRITE)

  // Replace the file only when it is empty or a proper prefix of the header line.
  // That is the CREATE_NEW window before the header is durable.
  // A finished version line is appended to. Any other bytes are left unchanged.
  private def commit(
    channel: FileChannel,
    id: Option[String],
    action: Action,
    albumId: String,
    itemKey: String,
    current: String,
    `new`: String
  ): Row =
    val bytes: Array[Byte] = readBytes(channel)
    val fresh: Boolean = incompleteHeader(bytes)
    val existing: Seq[Row] =
      if fresh then Seq.empty
      else parse(TextLines.fromBytes(bytes, Header, VersionError))
    val assigned: String = id match
      case Some(value) =>
        if existing.exists(_.id == value) then throw IllegalArgumentException("duplicate request id")
        value
      case None =>
        nextId(existing.map(_.id))
    val row: Row = Row(assigned, action, albumId, itemKey, current, `new`)
    if fresh then
      channel.position(0L)
      writeAll(channel, Header + "\n" + line(row))
      channel.truncate(channel.position())
    else
      channel.position(channel.size())
      writeAll(channel, line(row))
    row

  private def incompleteHeader(bytes: Array[Byte]): Boolean =
    val headerBytes: Array[Byte] = (Header + "\n").getBytes(StandardCharsets.UTF_8)
    bytes.length < headerBytes.length && bytes.sameElements(headerBytes.take(bytes.length))

  private def line(row: Row): String =
    Seq(row.id, row.action.wire, row.albumId, row.itemKey, row.current, row.`new`).mkString("\t") + "\n"

  private def nextId(ids: Seq[String]): String =
    val max: BigInt = ids.foldLeft(BigInt(0))((acc, id) => acc.max(BigInt(id.drop(1))))
    s"r${max + 1}"

  private def withExclusiveLock[T](channel: FileChannel)(body: => T): T =
    val lock: FileLock = exclusiveLock(channel)
    try
      afterLock()
      body
    finally lock.release()

  // FileChannel.lock throws instead of waiting when this JVM already holds the lock.
  @tailrec
  private def exclusiveLock(channel: FileChannel): FileLock =
    try channel.lock()
    catch case _: OverlappingFileLockException =>
      onOverlap()
      Thread.sleep(1)
      exclusiveLock(channel)

  private def readBytes(channel: FileChannel): Array[Byte] =
    channel.position(0L)
    val size: Long = channel.size()
    if size > Int.MaxValue then throw IllegalArgumentException("requests file is too large")
    val buffer: ByteBuffer = ByteBuffer.allocate(size.toInt)
    while buffer.hasRemaining do
      val n: Int = channel.read(buffer)
      if n < 0 then throw IllegalArgumentException("trailing partial line")
    buffer.array()

  private def writeAll(channel: FileChannel, text: String): Unit =
    val buffer: ByteBuffer = ByteBuffer.wrap(text.getBytes(StandardCharsets.UTF_8))
    while buffer.hasRemaining do channel.write(buffer)

  private def rejectField(text: String): Unit =
    if text.contains('\t') || text.contains('\n') || text.contains('\r') then
      throw IllegalArgumentException("request field contains a tab or newline")

  private def gate(file: File): AnyRef =
    val key: String = file.toPath.toAbsolutePath.normalize.toString
    gates.computeIfAbsent(key, _ => new Object)
