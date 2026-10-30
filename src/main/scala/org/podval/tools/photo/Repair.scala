package org.podval.tools.photo

import java.io.File
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
      Repair.run(
        session.root.parse(path),
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
    setTimestamp: (Picture, String, Instant) => Unit = Dates.FromFile.set
  ): Unit =
    if names then walk(path, if dryRun then NameMode.Preview else NameMode.Apply)
    if raw then each(path)(repairRaw(_, dryRun, generate))
    if timestamps then each(path)(repairTimestamps(_, dryRun, setTimestamp))
    if days then each(path)(repairDays(_, dryRun))

  private def walk(path: Path, names: NameMode): Unit = path match
    case pictures: Pictures =>
      val _: Seq[Picture] = pictures.lsd(names)
    case directory: Directory =>
      directory.lsd.foreach(child => walk(child, names))

  private def each(path: Path)(use: Picture => Unit): Unit = path match
    case pictures: Pictures => pictures.lsd(NameMode.Leave).foreach(use)
    case directory: Directory => directory.lsd.foreach(child => each(child)(use))

  private def repairRaw(picture: Picture, dryRun: Boolean, generate: Picture => Unit): Unit =
    val suppressed: Boolean = picture.metadata.`bad-raw`.contains(true)
    if picture.normal.isEmpty && !suppressed then
      act(dryRun, s"$picture: generating '${RawConverter.extension}' from raw file")(generate(picture))

  private def repairTimestamps(
    picture: Picture,
    dryRun: Boolean,
    setTimestamp: (Picture, String, Instant) => Unit
  ): Unit =
    PictureAudit(picture).timestamp.foreach: timestamp =>
      picture.extensions.foreach: extension =>
        if !Dates.equal(timestamp, extension.current) then
          act(dryRun, s"$extension: changing timestamp ${extension.current} to $timestamp"):
            setTimestamp(picture, extension.name, timestamp)

  private def repairDays(picture: Picture, dryRun: Boolean): Unit =
    PictureAudit(picture).dayShouldBe.foreach: dayShouldBe =>
      act(dryRun, s"moving $picture to $dayShouldBe"):
        val originals: File = picture.root.configuration.archive
        val from: String = s"${Album.relative(originals, picture.parent.directory)}/${picture.name}"
        val to: String = s"${Album.relative(originals, dayShouldBe.directory)}/${picture.name}"
        // Reject a bad curated file before any picture moves, so a later repair --days can retry.
        Album.rewriteInclude(originals, from, to)
        Files.mkDirs(dayShouldBe.directory)
        val extensionNamesAll: Set[String] =
          picture.extensions.map(_.name) ++ Option.when(picture.hasMetadata)(Metadata.extension).toSet
        extensionNamesAll.foreach: extension =>
          val fromFile: File = File(picture.parent.directory, s"${picture.name}.$extension")
          val toFile: File = File(dayShouldBe.directory, s"${picture.name}.$extension")
          Transcript.println(s"moving $fromFile to $toFile")
          Files.move(fromFile, toFile)
        Files.deleteEmptyDirectories(picture.parent.directory)

  private def act(dryRun: Boolean, message: String)(action: => Unit): Unit =
    Transcript.println(message)
    if !dryRun then action
