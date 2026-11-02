package org.podval.tools.photo

import java.io.File
import java.nio.channels.FileChannel
import java.nio.file.{FileAlreadyExistsException, Files as NFiles, StandardCopyOption, StandardOpenOption}
import java.time.{DateTimeException, LocalDate}
import java.util.UUID

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
      case Kept(_, relative, _) => relative

  private enum Action:
    case Filing(staged: File, day: String, stagedName: String, archiveName: String, destination: File)
    case Keep(relative: String, reason: String)

  private def plan(archive: File): Seq[Action] =
    val inbox: File = File(File(archive, ".gphoto-sync"), ".download")
    if !inbox.isDirectory then Seq.empty
    else
      val staged: Seq[Staged] = scan(inbox)
      val days: Seq[String] = staged.collect { case Staged.Dated(day, _, _) => day }.distinct
      val existing: Map[String, Seq[File]] = days.map(day => day -> listDay(archive, day)).toMap
      val decided: (Map[String, Set[String]], Vector[Action]) =
        staged.foldLeft((Map.empty[String, Set[String]], Vector.empty[Action])):
          case ((claimed, acc), Staged.Kept(_, relative, reason)) =>
            (claimed, acc :+ Action.Keep(relative, reason))
          case ((claimed, acc), Staged.Dated(day, file, relative)) =>
            val names: Set[String] = claimed.getOrElse(day, Set.empty)
            val action: Action = decide(day, File(archive, day), existing(day), names, file, relative)
            val next: Set[String] = action match
              case Action.Filing(_, _, _, archiveName, _) => names + archiveName
              case Action.Keep(_, _) => names
            (claimed.updated(day, next), acc :+ action)
      decided._2

  private def perform(actions: Seq[Action], dryRun: Boolean): Unit =
    actions.foreach: action =>
      Transcript.println(lineOf(action))
      action match
        case Action.Filing(staged, day, _, archiveName, destination) if !dryRun =>
          if commit(staged, destination) then Transcript.println(s"wrote $day/$archiveName")
          else Transcript.println(s"keep $day/${staged.getName} $archiveName already exists")
        case _ => ()

  private def lineOf(action: Action): String = action match
    case Action.Filing(_, day, stagedName, archiveName, _) =>
      s"file $day $stagedName $archiveName"
    case Action.Keep(relative, reason) =>
      s"keep $relative $reason"

  // The stand-in is not the staged file. plan only suffixes a video when the still is in the set it is given,
  // and a destination check has to look at the day folder, not at .download.
  private def decide(
    day: String,
    dayDir: File,
    existing: Seq[File],
    claimed: Set[String],
    staged: File,
    relative: String
  ): Action =
    val standIn: File = File(dayDir, staged.getName)
    val rows: Seq[Pictures.Planned] = Pictures.plan(existing :+ standIn)
    rows.find(_.file eq standIn) match
      case None =>
        Action.Keep(relative, "not a picture")
      case Some(row) if row.renameRefused =>
        Action.Keep(relative, s"${row.finalName} does not start with a digit")
      case Some(row) if row.omitted =>
        Action.Keep(relative, row.notRenamed.getOrElse("collision"))
      case Some(row) if claimed.contains(row.finalName) || File(dayDir, row.finalName).exists =>
        Action.Keep(relative, s"${row.finalName} already exists")
      case Some(row) =>
        Action.Filing(staged, day, staged.getName, row.finalName, File(dayDir, row.finalName))

  // commitFile moves with REPLACE_EXISTING and would overwrite an archive original.
  private def commit(staged: File, destination: File): Boolean =
    if destination.exists then false
    else
      Files.mkDirs(destination.getParentFile)
      if destination.exists then false
      else moveNew(staged, destination)

  private def moveNew(staged: File, destination: File): Boolean =
    val partial: File = File(destination.getParentFile, s".file-${UUID.randomUUID}.partial")
    var moved: Boolean = false
    try
      copyForced(staged, partial)
      if !destination.exists then
        try
          NFiles.move(partial.toPath, destination.toPath, StandardCopyOption.ATOMIC_MOVE)
          moved = true
        catch case _: FileAlreadyExistsException => ()
    finally
      if !moved then NFiles.deleteIfExists(partial.toPath)
    if moved then
      DirectoryIndex.dropAll()
      NFiles.delete(staged.toPath)
    moved

  private def copyForced(from: File, partial: File): Unit =
    NFiles.copy(from.toPath, partial.toPath)
    val channel: FileChannel = FileChannel.open(partial.toPath, StandardOpenOption.WRITE)
    try channel.force(true)
    finally channel.close()

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
          case None => Staged.Kept(file, relative, "not a civil day")
      case _ =>
        Staged.Kept(file, relative, "not a civil day")

  // A dotfile partial left by a crash is not a picture. plan would reject its extension.
  private def listDay(archive: File, day: String): Seq[File] =
    val dir: File = File(archive, day)
    if !dir.isDirectory then Seq.empty
    else Files.list(dir)._2.filterNot(file => file.getName.startsWith("."))

  private def civil(year: String, month: String, day: String): Option[String] =
    if year.length == 4 && month.length == 2 && day.length == 2 &&
      digits(year) && digits(month) && digits(day) && year.toInt > 1000
    then
      try
        val date: LocalDate = LocalDate.of(year.toInt, month.toInt, day.toInt)
        val canonical: String =
          f"${date.getYear}%04d/${date.getMonthValue}%02d/${date.getDayOfMonth}%02d"
        Option.when(canonical == s"$year/$month/$day")(canonical)
      catch case _: DateTimeException => None
    else None

  private def digits(text: String): Boolean = text.forall(ch => ch >= '0' && ch <= '9')
