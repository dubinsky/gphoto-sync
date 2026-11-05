package org.podval.tools.photo

import java.io.{ByteArrayOutputStream, File, IOException, InputStream, OutputStream}
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{FileAlreadyExistsException, Files as NFiles, LinkOption, StandardOpenOption}
import java.util.UUID
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.SeqHasAsJava

final case class DiscoverCard(dryRun: Boolean) extends Operation:
  override def execute(session: Session): (Session, Result) =
    Operation.run(session):
      Card.discover(dryRun) match
        case _ => ()

final case class CopyCard(dryRun: Boolean, path: Option[String]) extends Operation:
  override def execute(session: Session): (Session, Result) =
    Operation.run(session):
      Card.copy(session, dryRun, path)

object Card:
  private[photo] def discover(
      dryRun: Boolean,
      lsblk: () => Either[String, String] = readLsblk,
      mount: Seq[String] => (Int, String) = runMount
  ): Option[File] =
    val rows: Seq[Row] = judgeAll(readDocument(lsblk))
    rejectOrContinue(rows) match
      case Some(message) =>
        printRows(rows)
        throw IllegalArgumentException(message)
      case None =>
        val chosen: Seq[Row.Printed] = rows.collect:
          case row: Row.Printed if row.dcim == "yes" || row.dcim == "unknown" => row
        if chosen.sizeIs != 1 then
          printRows(rows)
          val message: String = if chosen.isEmpty then "no memory card" else "more than one memory card"
          throw IllegalArgumentException(message)
        else
          val one: Row.Printed = chosen.head
          if one.dcim == "yes" then
            printRows(rows)
            one.root
          else if dryRun then
            printRows(rows)
            None
          else mountAndReread(one.path, lsblk, mount)

  private[photo] def copy(
      session: Session,
      dryRun: Boolean,
      path: Option[String],
      lsblk: () => Either[String, String] = readLsblk,
      mount: Seq[String] => (Int, String) = runMount
  ): Unit =
    val upload: File = session.root.configuration.uploadDirectory
    path match
      case Some(text) =>
        copyTree(upload, File(text), dryRun, text)
      case None =>
        // A copy dry-run must list DCIM, so discovery is not dry-run. copyTree still gets the caller's dryRun.
        discover(dryRun = false, lsblk, mount) match
          case Some(root) =>
            copyTree(upload, root, dryRun, root.getPath)
          case None =>
            throw IllegalStateException("no mount root")

  private def mountAndReread(
      device: String,
      lsblk: () => Either[String, String],
      mount: Seq[String] => (Int, String)
  ): Option[File] =
    val (_, stderr) = mount(
      Seq(
        "udisksctl",
        "mount",
        "-b",
        device,
        "-o",
        "ro",
        "--no-user-interaction"
      )
    )
    val rows: Seq[Row] = judgeAll(readDocument(lsblk))
    rejectOrContinue(rows) match
      case Some(message) =>
        printRows(rows)
        throw IllegalArgumentException(message)
      case None =>
        rows.collectFirst:
          case row: Row.Printed if row.path == device => row
        match
          case Some(row) if row.dcim == "yes" =>
            printRows(rows)
            row.root
          case Some(row) if row.dcim == "no" =>
            printRows(rows)
            throw IllegalArgumentException("no memory card")
          case _ =>
            printRows(rows.collect { case row: Row.Printed if row.path != device => row })
            throw IllegalArgumentException(mountFailed(device, stderr))

  private def rejectOrContinue(rows: Seq[Row]): Option[String] =
    rows.collectFirst { case row: Row.Rejected => row.message }

  private def printRows(rows: Seq[Row]): Unit =
    rows.foreach:
      case row: Row.Printed => Transcript.println(row.line)
      case _: Row.Rejected  => ()

  private def mountFailed(device: String, stderr: String): String =
    if stderr.isEmpty then s"mount failed: $device"
    else
      val (line, _) = stderr.span(_ != '\n')
      val detail: String = if line.endsWith("\r") then line.dropRight(1) else line
      s"mount failed: $device: $detail"

  private def readDocument(lsblk: () => Either[String, String]): String =
    lsblk() match
      case Left(_)     => throw IllegalArgumentException("lsblk failed")
      case Right(text) => text

  private def judgeAll(text: String): Seq[Row] =
    volumesOf(text).map(judge)

  private def volumesOf(text: String): Seq[Volume] =
    val nodes: Seq[Json] = document(text) match
      case Json.Obj(fields) =>
        fields.collectFirst { case ("blockdevices", value) => value } match
          case Some(Json.Arr(values)) => values
          case _                      => throw IllegalArgumentException("lsblk output was not JSON")
      case _ =>
        throw IllegalArgumentException("lsblk output was not JSON")
    nodes.flatMap(nodeVolumes).sortBy(_.path)

  private def document(text: String): Json =
    Serve.readJson(text) match
      case Left(_) =>
        throw IllegalArgumentException("lsblk output was not JSON")
      case Right((json, rest)) =>
        if rest.exists(!_.isWhitespace) then throw IllegalArgumentException("lsblk output was not JSON")
        else json

  private def nodeVolumes(node: Json): Seq[Volume] = node match
    case Json.Obj(fields) =>
      val here: Seq[Volume] = volumeOf(Json.Obj(fields)).toSeq
      val kids: Seq[Json] = fields.collectFirst { case ("children", value) => value } match
        case None                   => Seq.empty
        case Some(Json.Arr(values)) => values
        case Some(_)                => throw IllegalArgumentException("lsblk output was not JSON")
      here ++ kids.flatMap(nodeVolumes)
    case _ =>
      throw IllegalArgumentException("lsblk output was not JSON")

  private def volumeOf(obj: Json.Obj): Option[Volume] =
    val path: Option[String] = stringField(obj, "path").filter(devicePath.matches)
    val fstype: Option[String] = stringField(obj, "fstype").filter: value =>
      value.equalsIgnoreCase("vfat") || value.equalsIgnoreCase("exfat")
    val removable: Boolean = field(obj, "rm").exists:
      case Json.Bool(true) => true
      case Json.Long(1)    => true
      case Json.Str("1")   => true
      case _               => false
    if path.isDefined && fstype.isDefined && removable then
      Some(
        Volume(
          path.get,
          stringField(obj, "label").getOrElse(""),
          fstype.get.toLowerCase(java.util.Locale.ROOT),
          mountsOf(obj)
        )
      )
    else None

  private def mountsOf(obj: Json.Obj): Seq[String] =
    field(obj, "mountpoints") match
      case Some(Json.Arr(values)) =>
        values.flatMap:
          case Json.Str(value) if value.startsWith("/") => Some(value)
          case _                                        => None
      case Some(_) =>
        throw IllegalArgumentException("lsblk output was not JSON")
      case None =>
        stringField(obj, "mountpoint").filter(_.startsWith("/")).toSeq

  private def stringField(obj: Json.Obj, name: String): Option[String] =
    field(obj, name) match
      case Some(Json.Str(value)) => Some(value)
      case _                     => None

  private def field(obj: Json.Obj, name: String): Option[Json] =
    obj.fields.collectFirst { case (`name`, value) => value }

  private val devicePath: scala.util.matching.Regex = "/dev/[A-Za-z0-9._/-]+".r

  private def judge(volume: Volume): Row =
    val badText: Boolean =
      !oneLine(volume.label) || volume.mounts.exists(mount => !oneLine(mount))
    if badText then Row.Rejected(volume.path, s"label or mountpoint is not one line: ${volume.path}")
    else if volume.mounts.sizeIs > 1 then Row.Rejected(volume.path, s"more than one mountpoint: ${volume.path}")
    else
      volume.mounts match
        case Seq(only) =>
          val yes: Boolean = realDcim(File(only)).sizeIs == 1
          val dcim: String = if yes then "yes" else "no"
          Row.Printed(volume.path, dcim, lineOf(volume, only, dcim), Some(File(only)))
        case _ =>
          Row.Printed(volume.path, "unknown", lineOf(volume, "unmounted", "unknown"), None)

  private def oneLine(text: String): Boolean =
    text.forall(ch => ch >= ' ')

  private def lineOf(volume: Volume, mountpoint: String, dcim: String): String =
    s"card device=${volume.path} label=${Album.quote(volume.label)} " +
      s"mountpoint=${Album.quote(mountpoint)} fstype=${volume.fstype} dcim=$dcim"

  private def realDcim(root: File): Seq[File] =
    Option(root.listFiles).toSeq.flatten.filter: child =>
      child.getName.equalsIgnoreCase("DCIM") &&
        NFiles.isDirectory(child.toPath, LinkOption.NOFOLLOW_LINKS)

  private def copyTree(upload: File, root: File, dryRun: Boolean, shown: String): Unit =
    val dcim: File = requireDcim(root, shown)
    prepareUpload(upload, dryRun)
    val entries: Seq[Entry] = entriesUnder(dcim.getName, dcim, enter = true).sortBy(_.rel)
    var claimed: Set[String] = Set.empty
    entries.foreach: entry =>
      entry match
        case Entry.Irregular(rel) =>
          Transcript.println(s"keep $rel not a regular file")
        case Entry.Nested(rel) =>
          Transcript.println(s"keep $rel nested")
        case Entry.Regular(file, rel) =>
          claimed = place(upload, file, rel, dryRun, claimed)

  private def requireDcim(root: File, shown: String): File =
    if !NFiles.isDirectory(root.toPath, LinkOption.NOFOLLOW_LINKS) then
      throw IllegalArgumentException(s"not a directory: $shown")
    val children: Seq[File] = Option(root.listFiles)
      .map(_.toSeq)
      .getOrElse(
        throw IllegalArgumentException(s"not a directory: $shown")
      )
    val dcims: Seq[File] = children.filter: child =>
      child.getName.equalsIgnoreCase("DCIM") &&
        NFiles.isDirectory(child.toPath, LinkOption.NOFOLLOW_LINKS)
    dcims match
      case Seq(only) => only
      case Seq()     => throw IllegalArgumentException("no DCIM directory")
      case _         => throw IllegalArgumentException("more than one DCIM directory")

  // File.isDirectory follows links. Files.list uses it, so this walk does not.
  private def entriesUnder(parentRel: String, directory: File, enter: Boolean): Seq[Entry] =
    val children: Seq[File] = Option(directory.listFiles)
      .map(_.toSeq)
      .getOrElse(
        throw IllegalArgumentException(s"not a directory: ${directory.getPath}")
      )
    children.flatMap: child =>
      val name: String = child.getName
      if name.startsWith(".") then Seq.empty
      else
        val rel: String = s"$parentRel/$name"
        if NFiles.isSymbolicLink(child.toPath) then Seq(Entry.Irregular(rel))
        else if NFiles.isDirectory(child.toPath, LinkOption.NOFOLLOW_LINKS) then
          if enter then entriesUnder(rel, child, enter = false)
          else Seq(Entry.Nested(rel))
        else if NFiles.isRegularFile(child.toPath, LinkOption.NOFOLLOW_LINKS) then Seq(Entry.Regular(child, rel))
        else Seq(Entry.Irregular(rel))

  private def place(
      upload: File,
      file: File,
      rel: String,
      dryRun: Boolean,
      claimed: Set[String]
  ): Set[String] =
    val (stem, extension) = Files.nameAndExtension(file)
    extension match
      case None =>
        Transcript.println(s"keep $rel no extension")
        claimed
      case Some(ext) =>
        val lower: String = ext.toLowerCase(java.util.Locale.ROOT)
        if !Extension.all.exists(_.name == lower) then
          Transcript.println(s"keep $rel illegal extension $lower")
          claimed
        else
          val (repaired, _) = Pictures.repaired(stem, ext)
          if repaired.isEmpty || !repaired.head.isDigit then
            Transcript.println(s"keep $rel ${file.getName} does not start with a digit")
            claimed
          else if claimed.contains(file.getName) then
            Transcript.println(s"keep $rel duplicate name")
            claimed
          else
            val destination: File = File(upload, file.getName)
            if destination.exists then
              val same: Boolean = NFiles.size(destination.toPath) == NFiles.size(file.toPath)
              val reason: String = if same then "already copied" else "size differs"
              Transcript.println(s"keep $rel $reason")
            else if dryRun then Transcript.println(s"copy $rel .upload/${file.getName}")
            else publish(upload, file, destination, rel)
            claimed + file.getName

  private def prepareUpload(upload: File, dryRun: Boolean): Unit =
    val path = upload.toPath
    val exists: Boolean = NFiles.exists(path, LinkOption.NOFOLLOW_LINKS)
    if exists && !NFiles.isDirectory(path, LinkOption.NOFOLLOW_LINKS) then
      throw IllegalArgumentException(".upload is not a directory")
    if !dryRun && exists then
      val children: Seq[File] = Option(upload.listFiles)
        .map(_.toSeq)
        .getOrElse(
          throw IllegalArgumentException(".upload is not a directory")
        )
      children.foreach: file =>
        val name: String = file.getName
        if name.startsWith(".card-") && name.endsWith(".partial") then
          try NFiles.deleteIfExists(file.toPath)
          catch
            case _: IOException =>
              throw IllegalArgumentException(".upload is not a directory")

  private def publish(upload: File, from: File, destination: File, rel: String): Unit =
    Files.mkDirs(upload)
    val partial: File = File(upload, s".card-${UUID.randomUUID}.partial")
    var linked: Boolean = false
    val source: FileChannel = FileChannel.open(from.toPath, StandardOpenOption.READ)
    try
      val channel: FileChannel = FileChannel.open(
        partial.toPath,
        StandardOpenOption.CREATE_NEW,
        StandardOpenOption.WRITE
      )
      try
        val size: Long = source.size()
        transfer(source, channel, 0L, size)
        channel.force(true)
      finally channel.close()
      // Close first. Closing a write channel can stamp the file with now.
      NFiles.setLastModifiedTime(partial.toPath, NFiles.getLastModifiedTime(from.toPath))
      try
        FileDownloads.linkPartial(partial, destination)
        linked = true
      catch
        case _: FileAlreadyExistsException =>
          val same: Boolean = NFiles.size(destination.toPath) == NFiles.size(from.toPath)
          val reason: String = if same then "already copied" else "size differs"
          Transcript.println(s"keep $rel $reason")
    finally
      source.close()
      NFiles.deleteIfExists(partial.toPath)
    if linked then Transcript.println(s"copy $rel .upload/${from.getName}")

  // FileDownloads.transfer is private.
  @tailrec
  private def transfer(source: FileChannel, channel: FileChannel, position: Long, size: Long): Unit =
    if position < size then
      val n: Long = channel.transferFrom(source, position, size - position)
      if n == 0 then throw IOException(s"short transfer at $position of $size")
      transfer(source, channel, position + n, size)

  private def readLsblk(): Either[String, String] =
    run(Seq("lsblk", "-J", "-o", "NAME,PATH,RM,TYPE,FSTYPE,LABEL,MOUNTPOINTS")) match
      case Left(_)                  => Left("lsblk failed")
      case Right((code, stdout, _)) =>
        if code == 0 then Right(stdout) else Left("lsblk failed")

  private def runMount(argv: Seq[String]): (Int, String) =
    run(argv) match
      case Left(_)                  => (1, "")
      case Right((code, _, stderr)) => (code, stderr)

  private def run(argv: Seq[String]): Either[String, (Int, String, String)] =
    val process: Process =
      try new ProcessBuilder(argv.asJava).start()
      catch case _: IOException => return Left("failed")
    val stdout = new ByteArrayOutputStream
    val stderr = new ByteArrayOutputStream
    val failed = new AtomicBoolean(false)
    def drain(in: InputStream, out: OutputStream): Unit =
      try in.transferTo(out)
      catch case _: IOException => failed.set(true)
    val outThread: Thread = new Thread(() => drain(process.getInputStream, stdout))
    val errThread: Thread = new Thread(() => drain(process.getErrorStream, stderr))
    outThread.start()
    errThread.start()
    try
      val code: Int = process.waitFor()
      outThread.join()
      errThread.join()
      if failed.get then Left("failed")
      else Right((code, stdout.toString(StandardCharsets.UTF_8), stderr.toString(StandardCharsets.UTF_8)))
    catch
      case _: InterruptedException =>
        process.destroyForcibly()
        outThread.join()
        errThread.join()
        Left("failed")

  private final case class Volume(path: String, label: String, fstype: String, mounts: Seq[String])

  private enum Row:
    case Rejected(path: String, message: String)
    case Printed(path: String, dcim: String, line: String, root: Option[File])

  private enum Entry:
    case Regular(file: File, relative: String)
    case Nested(relative: String)
    case Irregular(relative: String)

    def rel: String = this match
      case Regular(_, relative) => relative
      case Nested(relative)     => relative
      case Irregular(relative)  => relative
