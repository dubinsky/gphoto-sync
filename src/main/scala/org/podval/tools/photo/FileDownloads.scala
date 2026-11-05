package org.podval.tools.photo

import java.io.{File, IOException}
import java.nio.channels.FileChannel
import java.nio.file.{FileAlreadyExistsException, Files as NFiles, StandardOpenOption}
import java.time.{DateTimeException, LocalDate}
import java.util.UUID
import scala.annotation.tailrec

// Files dated staged downloads into civil day folders. Does not download and does not call Google.
final case class FileDownloads(dryRun: Boolean) extends Operation:
  override def execute(session: Session): (Session, Result) =
    Operation.run(session):
      FileDownloads.perform(FileDownloads.plan(session.root.configuration.archive), dryRun)

object FileDownloads:
  private enum Staged:
    case Dated(day: String, file: File, relative: String)
    case Kept(file: File, relative: String, reason: String)

    def path: String = this match
      case Dated(_, _, relative) => relative
      case Kept(_, relative, _)  => relative

  private enum Action:
    case Filing(staged: File, day: String, stagedName: String, archiveName: String, destination: File)
    case Keep(relative: String, reason: String)
    case DateMismatch(catalogDay: String, filename: String, hits: Seq[(String, String)])

  private def plan(archive: File): Seq[Action] =
    val inbox: File = File(File(archive, ".gphoto-sync"), ".download")
    if !inbox.isDirectory then Seq.empty
    else
      val staged: Seq[Staged] = scan(inbox)
      val dated: Seq[Staged.Dated] = staged.collect { case item: Staged.Dated => item }
      val byDay: Map[String, Seq[Staged.Dated]] = dated.groupBy(_.day)
      val existing: Map[String, Seq[File]] = byDay.keys.map(day => day -> listDay(archive, day)).toMap
      val actions: Map[String, Iterator[Action]] = byDay.map: (day, items) =>
        day -> decideDay(day, File(archive, day), existing(day), items).iterator
      staged.map:
        case Staged.Kept(_, relative, reason) => Action.Keep(relative, reason)
        case Staged.Dated(day, _, _)          => actions(day).next()

  private def perform(actions: Seq[Action], dryRun: Boolean): Unit =
    actions.foreach: action =>
      linesOf(action).foreach(Transcript.println)
      action match
        case Action.Filing(staged, day, _, archiveName, destination) if !dryRun =>
          if commit(staged, destination) then Transcript.println(s"wrote $day/$archiveName")
          else Transcript.println(s"keep $day/${staged.getName} $archiveName already exists")
        case _ => ()

  private def linesOf(action: Action): Seq[String] = action match
    case Action.Filing(_, day, stagedName, archiveName, _) =>
      Seq(s"file $day $stagedName $archiveName")
    case Action.Keep(relative, reason) =>
      Seq(s"keep $relative $reason")
    case Action.DateMismatch(catalogDay, filename, hits) =>
      hits.map: (archiveDay, stem) =>
        s"date-mismatch $catalogDay $filename $archiveDay $stem"

  // One plan per day, on the day files plus every stand-in, so a still and a video staged together
  // gain the same trailing v that repair --names would. The stand-in is a name in the day directory:
  // the destination check looks there, and the staged file under .download is not renamed.
  private def decideDay(day: String, dayDir: File, existing: Seq[File], dated: Seq[Staged.Dated]): Seq[Action] =
    val probed: Seq[Either[Action, (Staged.Dated, File)]] = dated.map: item =>
      val standIn: File = File(dayDir, item.file.getName)
      standInProblem(standIn) match
        case Some(message) => Left(Action.Keep(item.relative, message))
        case None          => Right((item, standIn))
    val good: Seq[(Staged.Dated, File)] = probed.collect { case Right(pair) => pair }
    val planned: Either[String, Seq[Pictures.Planned]] =
      if good.isEmpty then Right(Seq.empty)
      else
        try Right(Pictures.plan(existing ++ good.map(_._2)))
        catch case e: IllegalArgumentException => Left(e.getMessage)
    val onDay: Set[String] = planned match
      case Right(rows) =>
        val standIns: Seq[File] = good.map(_._2)
        rows.filterNot(row => standIns.exists(_ eq row.file)).map(_.stem).toSet
      case Left(_) =>
        Set.empty
    probed.map:
      case Left(keep)             => keep
      case Right((item, standIn)) =>
        planned match
          case Left(message) => Action.Keep(item.relative, message)
          case Right(rows)   => actionFor(day, dayDir, onDay, item, standIn, rows)

  // plannedFile throws before a row exists. Keep that download and plan the rest of the day.
  private def standInProblem(standIn: File): Option[String] =
    try
      Pictures.plan(Seq(standIn))
      None
    catch case e: IllegalArgumentException => Some(e.getMessage)

  private def actionFor(
    day: String,
    dayDir: File,
    onDay: Set[String],
    item: Staged.Dated,
    standIn: File,
    rows: Seq[Pictures.Planned]
  ): Action =
    rows.find(_.file eq standIn) match
      case None =>
        Action.Keep(item.relative, "not a picture")
      case Some(row) if row.renameRefused =>
        Action.Keep(item.relative, s"${row.finalName} does not start with a digit")
      case Some(row) if row.omitted =>
        Action.Keep(item.relative, row.notRenamed.getOrElse("collision"))
      case Some(row) if File(dayDir, row.finalName).exists =>
        Action.Keep(item.relative, s"${row.finalName} already exists")
      case Some(row) =>
        val hits: Seq[(String, String)] =
          if onDay.contains(row.stem) then Seq.empty else adjacentHits(day, item.file.getName)
        if hits.isEmpty then
          Action.Filing(item.file, day, item.file.getName, row.finalName, File(dayDir, row.finalName))
        else Action.DateMismatch(day, item.file.getName, hits)

  // Previous day, then next. loadGrouped does not create a missing neighbor.
  private def adjacentHits(day: String, filename: String): Seq[(String, String)] =
    val catalog: LocalDate = LocalDate.parse(day.replace('/', '-'))
    val (stem, extension) = Files.splitOnLast(filename, '.')
    Seq(catalog.minusDays(1), catalog.plusDays(1)).flatMap: neighbor =>
      val session: Session = DirectoryIndex.now
      // A neighbor Pictures.plan rejects is not a hit, and must not abort the other days.
      val grouped: Map[String, Set[String]] =
        try
          val (loaded, rows, _) =
            AgentSupport.loadGrouped(session, AgentSupport.civilDay(session.root, neighbor))
          if loaded ne session then DirectoryIndex.replace(loaded)
          rows.toMap
        catch
          case _: IllegalArgumentException =>
            Map.empty
      AgentSupport
        .resolve(grouped, stem, extension)
        .map: local =>
          Title.dayText(neighbor) -> local

  // commitFile moves with REPLACE_EXISTING and would overwrite an archive original.
  private def commit(staged: File, destination: File): Boolean =
    if destination.exists then false
    else
      val dayDir: File = destination.getParentFile
      Files.mkDirs(dayDir)
      if destination.exists then false
      else moveNew(staged, dayDir, destination)

  private def moveNew(staged: File, dayDir: File, destination: File): Boolean =
    deleteLeftoverPartials(dayDir)
    val partial: File = File(dayDir, s".file-${UUID.randomUUID}.partial")
    var linked: Boolean = false
    try
      copyForced(staged, partial)
      if !destination.exists then
        try
          linkPartial(partial, destination)
          linked = true
        catch case _: FileAlreadyExistsException => ()
    finally NFiles.deleteIfExists(partial.toPath)
    if linked then
      DirectoryIndex.dropAll()
      NFiles.delete(staged.toPath)
    linked

  // Linux rename replaces the destination and does not throw. createLink fails if the name exists.
  private[photo] def linkPartial(partial: File, destination: File): Unit =
    NFiles.createLink(destination.toPath, partial.toPath)

  // A killed copy leaves `.file-*.partial` in the day directory. Drop those before writing a new one.
  private def deleteLeftoverPartials(dayDir: File): Unit =
    Files
      .list(dayDir)
      ._2
      .filter(file => file.getName.startsWith(".file-") && file.getName.endsWith(".partial"))
      .foreach(file => NFiles.deleteIfExists(file.toPath))

  private def copyForced(from: File, partial: File): Unit =
    val source: FileChannel = FileChannel.open(from.toPath, StandardOpenOption.READ)
    try
      val channel: FileChannel = FileChannel.open(
        partial.toPath,
        StandardOpenOption.CREATE_NEW,
        StandardOpenOption.WRITE
      )
      try
        transfer(source, channel, 0L, source.size())
        channel.force(true)
      finally channel.close()
    finally source.close()

  @tailrec
  private def transfer(source: FileChannel, channel: FileChannel, position: Long, size: Long): Unit =
    if position < size then
      val n: Long = channel.transferFrom(source, position, size - position)
      if n == 0 then throw IOException(s"short transfer at $position of $size")
      transfer(source, channel, position + n, size)

  private def scan(inbox: File): Seq[Staged] =
    walkFiles(inbox).map(file => classify(inbox, file)).sortBy(_.path)

  private def walkFiles(directory: File): Seq[File] =
    val (dirs, files) = Files.list(directory)
    val visible: Seq[File] = files.filterNot(file => file.getName.startsWith("."))
    visible ++ dirs.filterNot(dir => dir.getName.startsWith(".")).flatMap(walkFiles)

  private def classify(inbox: File, file: File): Staged =
    val relative: String = Album.relative(inbox, file)
    relative.split("/", -1).toSeq match
      case Seq("undated", _) =>
        Staged.Kept(file, relative, "undated")
      case Seq(year, month, day, _) =>
        civil(year, month, day) match
          case Some(canonical) => Staged.Dated(canonical, file, relative)
          case None            => Staged.Kept(file, relative, "not a civil day")
      case _ =>
        Staged.Kept(file, relative, "not a civil day")

  // Dotfiles are not pictures. Pictures.plan ignores them too; this keeps a crash partial out of the stand-in set.
  private def listDay(archive: File, day: String): Seq[File] =
    val dir: File = File(archive, day)
    if !dir.isDirectory then Seq.empty
    else Files.list(dir)._2.filterNot(file => file.getName.startsWith("."))

  private def civil(year: String, month: String, day: String): Option[String] =
    try
      val date: LocalDate = LocalDate.parse(s"$year-$month-$day")
      Option.when(date.getYear > 1000)(s"$year/$month/$day")
    catch case _: DateTimeException => None
