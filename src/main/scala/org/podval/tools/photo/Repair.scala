package org.podval.tools.photo

import java.io.File
import java.nio.file.{FileAlreadyExistsException, Files as NFiles}
import java.time.Instant

final case class Repair(
    path: String,
    names: Boolean,
    raw: Boolean,
    timestamps: Boolean,
    days: Boolean,
    dryRun: Boolean
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    Operation.run(session):
      val parsed: Path = session.parse(path)
      // A dry run writes nothing, so the index stays. Days move, and curated files at the root are rewritten.
      if !dryRun && days then DirectoryIndex.dropAll()
      else if !dryRun && (names || raw || timestamps) then DirectoryIndex.armDropReads()
      Repair.run(
        parsed,
        names = names,
        raw = raw,
        timestamps = timestamps,
        days = days,
        dryRun = dryRun
      )

object Repair:
  def run(
      path: Path,
      names: Boolean,
      raw: Boolean,
      timestamps: Boolean,
      days: Boolean,
      dryRun: Boolean,
      generate: Picture => Unit = RawConverter.DCRaw.generate,
      setTimestamp: (Picture, String, Instant) => Unit = Dates.FromFile.set,
      link: (File, File) => Unit = FileDownloads.linkPartial
  ): Unit =
    if names then walk(path, if dryRun then NameMode.Preview else NameMode.Apply)
    if raw then each(path)(repairRaw(_, dryRun, generate))
    if timestamps then each(path)(repairTimestamps(_, dryRun, setTimestamp))
    if days then
      if !dryRun then
        path match
          case _: Root | _: UploadDirectory =>
            path.root.lsd
              .collect { case year: Year => year }
              .foreach: year =>
                year.lsd
                  .collect { case month: Month => month }
                  .foreach: month =>
                    month.lsd
                      .collect { case day: Day => day }
                      .foreach: day =>
                        Files
                          .listFiles(day.directory)
                          .foreach: file =>
                            val name: String = file.getName
                            if name.startsWith(".inbox-") && name.endsWith(".partial") then
                              val inboxName: String = name.drop(".inbox-".length).dropRight(".partial".length)
                              val finalFile: File = File(day.directory, inboxName)
                              val uploadFile: File = File(path.root.configuration.uploadDirectory, inboxName)
                              if finalFile.exists && NFiles.isSameFile(finalFile.toPath, file.toPath) then
                                NFiles.deleteIfExists(file.toPath)
                              else if !finalFile.exists && !uploadFile.exists then Files.move(file, uploadFile)
                              else throw IllegalArgumentException(s"leftover inbox partial: $name")
          case _ =>
            ()
      each(path)(repairDays(_, dryRun, link))

  private def walk(path: Path, names: NameMode): Unit =
    note(path)
    path match
      case pictures: Pictures =>
        val _: Seq[Picture] = pictures.lsd(names)
      case directory: Directory =>
        directory.lsd.foreach(child => walk(child, names))

  private def each(path: Path)(use: Picture => Unit): Unit =
    note(path)
    path match
      case pictures: Pictures   => pictures.lsd(NameMode.Leave).foreach(use)
      case directory: Directory => directory.lsd.foreach(child => each(child)(use))

  private def note(path: Path): Unit = path match
    case directory: Directory => DirectoryIndex.noteRead(directory.directory)
    case _                    => ()

  private def repairRaw(picture: Picture, dryRun: Boolean, generate: Picture => Unit): Unit =
    val suppressed: Boolean = picture.metadata.`bad-raw`.contains(true)
    if picture.raw.isDefined && picture.normal.isEmpty && !suppressed then
      act(dryRun, s"$picture: generating '${RawConverter.extension}' from raw file")(generate(picture))

  private def repairTimestamps(
      picture: Picture,
      dryRun: Boolean,
      setTimestamp: (Picture, String, Instant) => Unit
  ): Unit =
    PictureAudit(picture).writesMtime.foreach: clock =>
      val instant: Instant = clock.civil.atZone(picture.zone).toInstant
      picture.extensions.foreach: extension =>
        if !Dates.sameSecond(clock.civil, extension.current, picture.zone) then
          act(dryRun, s"$extension: changing timestamp ${extension.current} to $instant"):
            setTimestamp(picture, extension.name, instant)

  private def repairDays(picture: Picture, dryRun: Boolean, link: (File, File) => Unit): Unit =
    val audit: Dates.Assessment = PictureAudit(picture)
    audit.disagreement match
      case Some(disagree) =>
        Transcript.println(
          s"DATE DISAGREES WITH TIMESTAMP $picture timestamp=${disagree.timestamp} date=${disagree.date}"
        )
      case None =>
        audit.dayShouldBe.foreach: dayShouldBe =>
          val originals: File = picture.root.configuration.archive
          val from: String = s"${Album.relative(originals, picture.parent.directory)}/${picture.name}"
          val to: String = s"${Album.relative(originals, dayShouldBe.directory)}/${picture.name}"
          val extensionNamesAll: Set[String] =
            picture.extensions.map(_.name) ++ Option.when(picture.hasMetadata)(Metadata.extension).toSet
          if Album.picturePath(from).isEmpty then
            val moves: Seq[(File, File, File)] = extensionNamesAll.toSeq.map: extension =>
              (
                File(picture.parent.directory, s"${picture.name}.$extension"),
                File(dayShouldBe.directory, s".inbox-${picture.name}.$extension.partial"),
                File(dayShouldBe.directory, s"${picture.name}.$extension")
              )
            if moves.exists(_._3.exists) then Transcript.println(s"keep $picture $dayShouldBe already exists")
            else if dryRun then Transcript.println(s"moving $picture to $dayShouldBe")
            else
              Files.mkDirs(dayShouldBe.directory)
              var staged: List[(File, File)] = Nil
              try
                moves.foreach: (fromFile, partial, _) =>
                  Files.move(fromFile, partial)
                  staged = (fromFile, partial) :: staged
              catch
                case error: Exception =>
                  staged.foreach: (fromFile, partial) =>
                    Files.move(partial, fromFile)
                  throw error
              var linked: List[File] = Nil
              val linkError: Option[Exception] =
                try
                  moves.foreach: (_, partial, toFile) =>
                    link(partial, toFile)
                    linked = toFile :: linked
                  None
                catch
                  case error: Exception =>
                    Some(error)
              linkError match
                case Some(error) =>
                  linked.foreach(file => NFiles.deleteIfExists(file.toPath))
                  moves.foreach: (fromFile, partial, _) =>
                    if partial.isFile then Files.move(partial, fromFile)
                  error match
                    case _: FileAlreadyExistsException =>
                      Transcript.println(s"keep $picture $dayShouldBe already exists")
                    case other =>
                      throw other
                case None =>
                  moves.foreach: (_, partial, _) =>
                    NFiles.deleteIfExists(partial.toPath)
                  Transcript.println(s"moving $picture to $dayShouldBe")
                  moves.foreach: (fromFile, _, toFile) =>
                    Transcript.println(s"moving $fromFile to $toFile")
          else
            act(dryRun, s"moving $picture to $dayShouldBe"):
              // Reject a bad curated file before any picture moves, so a later repair --days can retry.
              Album.rewriteInclude(originals, from, to)
              Files.mkDirs(dayShouldBe.directory)
              extensionNamesAll.foreach: extension =>
                val fromFile: File = File(picture.parent.directory, s"${picture.name}.$extension")
                val toFile: File = File(dayShouldBe.directory, s"${picture.name}.$extension")
                Transcript.println(s"moving $fromFile to $toFile")
                Files.move(fromFile, toFile)
              Files.deleteEmptyDirectories(picture.parent.directory, originals)

  private def act(dryRun: Boolean, message: String)(action: => Unit): Unit =
    Transcript.println(message)
    if !dryRun then action
