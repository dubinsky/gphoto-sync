package org.podval.tools.photo

import scala.annotation.tailrec
import java.io.File
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption.{CREATE_NEW, READ, WRITE}
import java.nio.file.{FileAlreadyExistsException, Files as NFiles, NoSuchFileException}
import java.util.concurrent.ConcurrentHashMap

// Beside requests.tsv. Append-only. A line wins over the catalog for that request id.
object Outcomes:
  enum Status(val wire: String) derives CanEqual:
    case NeedsAgent extends Status("needs-agent")
    case Done extends Status("done")

  object Status:
    def fromWire(text: String): Status = text match
      case "needs-agent" => Status.NeedsAgent
      case "done" => Status.Done
      case _ => throw IllegalArgumentException("unknown outcome")

  final case class Row(id: String, status: Status, millis: Long) derives CanEqual

  final class AppendFailed(message: String) extends RuntimeException(message)

  private val Header: String = "# gphoto-request-outcomes 1"
  private val VersionError: String = "outcomes are not version 1"
  private val gates: ConcurrentHashMap[String, AnyRef] = ConcurrentHashMap()

  // Throws before the file is created, while the caller still holds the refresh lock.
  @volatile private[photo] var beforeWrite: () => Unit = () => ()

  def file(requests: File): File = File(requests.getParentFile, requests.getName + ".outcomes")

  // A missing file is no outcomes and is not created.
  def read(file: File): Map[String, Status] =
    if !file.isFile then Map.empty
    else gate(file).synchronized:
      if !file.isFile then Map.empty
      else openRead(file) match
        case None => Map.empty
        case Some(channel) =>
          try
            withExclusiveLock(channel):
              rows(TextLines.fromBytes(readBytes(channel), Header, VersionError)).map(row => row.id -> row.status).toMap
          finally channel.close()

  // Same exclusive lock as Requests.append. Never rewrites a committed line.
  def append(file: File, id: String, status: Status): Unit =
    try
      beforeWrite()
      Requests.requireId(id)
      gate(file).synchronized:
        val channel: FileChannel = openChannel(file)
        try
          withExclusiveLock(channel):
            commit(channel, id, status)
        finally channel.close()
    catch
      case failed: AppendFailed =>
        throw failed
      case error: Exception =>
        throw AppendFailed(Operation.messageOf(error))

  private def rows(text: String): Seq[Row] =
    TextLines.dataLines(text).filter(_.nonEmpty).foldLeft(Seq.empty[Row]): (acc, line) =>
      val row: Row = parseRow(line)
      if acc.exists(_.id == row.id) then throw IllegalArgumentException("duplicate request id")
      acc :+ row

  private def parseRow(line: String): Row =
    TextLines.columns(line, 3) match
      case Seq(id, status, millis) =>
        Requests.requireId(id)
        Row(id, Status.fromWire(status), parseMillis(millis))
      case _ =>
        throw IllegalArgumentException("wrong column count")

  private def parseMillis(text: String): Long =
    if text.isEmpty || text.exists(ch => ch < '0' || ch > '9') then
      throw IllegalArgumentException("outcome time is not valid")
    try text.toLong
    catch case _: NumberFormatException =>
      throw IllegalArgumentException("outcome time is not valid")

  private def openRead(file: File): Option[FileChannel] =
    try Some(FileChannel.open(file.toPath, READ, WRITE))
    catch case _: NoSuchFileException => None

  private def openChannel(file: File): FileChannel =
    val path = file.toPath
    if NFiles.exists(path) then FileChannel.open(path, READ, WRITE)
    else
      try FileChannel.open(path, CREATE_NEW, READ, WRITE)
      catch case _: FileAlreadyExistsException =>
        FileChannel.open(path, READ, WRITE)

  private def commit(channel: FileChannel, id: String, status: Status): Unit =
    val bytes: Array[Byte] = readBytes(channel)
    val fresh: Boolean = incompleteHeader(bytes)
    val existing: Seq[Row] =
      if fresh then Seq.empty
      else rows(TextLines.fromBytes(bytes, Header, VersionError))
    if existing.exists(_.id == id) then throw IllegalArgumentException("duplicate request id")
    val line: String = s"$id\t${status.wire}\t${System.currentTimeMillis}\n"
    if fresh then
      channel.position(0L)
      writeAll(channel, Header + "\n" + line)
      channel.truncate(channel.position())
    else
      channel.position(channel.size())
      writeAll(channel, line)

  private def incompleteHeader(bytes: Array[Byte]): Boolean =
    val headerBytes: Array[Byte] = (Header + "\n").getBytes(StandardCharsets.UTF_8)
    bytes.length < headerBytes.length && bytes.sameElements(headerBytes.take(bytes.length))

  private def withExclusiveLock[T](channel: FileChannel)(body: => T): T =
    val lock: FileLock = exclusiveLock(channel)
    try body
    finally lock.release()

  @tailrec
  private def exclusiveLock(channel: FileChannel): FileLock =
    try channel.lock()
    catch case _: OverlappingFileLockException =>
      Thread.sleep(1)
      exclusiveLock(channel)

  private def readBytes(channel: FileChannel): Array[Byte] =
    channel.position(0L)
    val size: Long = channel.size()
    if size > Int.MaxValue then throw IllegalArgumentException("outcomes file is too large")
    val buffer: ByteBuffer = ByteBuffer.allocate(size.toInt)
    while buffer.hasRemaining do
      val n: Int = channel.read(buffer)
      if n < 0 then throw IllegalArgumentException("trailing partial line")
    buffer.array()

  private def writeAll(channel: FileChannel, text: String): Unit =
    val buffer: ByteBuffer = ByteBuffer.wrap(text.getBytes(StandardCharsets.UTF_8))
    while buffer.hasRemaining do channel.write(buffer)

  private def gate(file: File): AnyRef =
    val key: String = file.toPath.toAbsolutePath.normalize.toString
    gates.computeIfAbsent(key, _ => new Object)
