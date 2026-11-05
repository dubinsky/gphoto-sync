package org.podval.tools.photo

import java.io.File
import java.time.{DateTimeException, LocalDate}

final case class ItemSet(dryRun: Boolean, property: String, picture: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    var produced: Json = Json.Null
    Operation.run(session, produced):
      val parsed: Picture = session.parsePicture(picture)
      val (name, raw) = Files.splitOnLast(property, '=')
      Property.forName(name)
      // `date=` is a clear. LocalDate.parse rejects the empty string.
      val present: Option[String] = raw.filter(_.nonEmpty)
      val updated: Metadata =
        if name == "date" && present.isDefined then parsed.metadata.copy(date = Some(parsePlacementDay(present.get)))
        else Property.AndValue(Property.forName(name), present).set(parsed.metadata)
      val folder: Option[LocalDate] = parsed.parent.day.map(_.date)
      val placed: Option[LocalDate] = if name == "date" then updated.date else None
      val moves: Boolean = placed.exists(day => !folder.contains(day))
      val fromPath: String = parsed.toString.stripPrefix("/")
      val moved: Option[String] =
        if !moves then
          writeSidecar(parsed, updated)
          None
        else Some(movePicture(parsed, fromPath, placed.get, updated))
      val resultPath: String = if moves && !dryRun then moved.get else fromPath
      val movedTo: Json = if moves && !dryRun then Json.Str(moved.get) else Json.Null
      produced = Json.Obj(
        Seq(
          "dryRun" -> Json.Bool(dryRun),
          "path" -> Json.Str(resultPath),
          "property" -> Json.Str(name),
          "value" -> stored(name, updated),
          "movedTo" -> movedTo
        )
      )

  private def writeSidecar(parsed: Picture, updated: Metadata): Unit =
    if updated == Metadata.default then
      Transcript.println(s"$parsed: deleting metadata file")
      if !dryRun then
        parsed.parent.day.foreach(day => DirectoryIndex.drop(day.directory))
        Files.delete(parsed.metadataFile)
    else
      val string: String = Metadata.encode(updated)
      Transcript.println(s"$parsed: writing metadata file\n$string")
      if !dryRun then
        parsed.parent.day.foreach(day => DirectoryIndex.drop(day.directory))
        Files.write(string, parsed.metadataFile)

  private def movePicture(parsed: Picture, fromPath: String, toDay: LocalDate, updated: Metadata): String =
    val archive: File = parsed.root.configuration.archive
    val fromDay: Day = parsed.parent.day.getOrElse(throw IllegalArgumentException("not a date-tree path"))
    val stem: String = parsed.name
    val fromPair: (LocalDate, String) = (fromDay.date, stem)
    val destDay: Day = parsed.root.year(toDay.getYear).month(toDay.getMonthValue).day(toDay.getDayOfMonth)
    val toPath: String = s"${Album.relative(archive, destDay.directory)}/$stem"
    val extensions: Seq[String] = parsed.extensions.toSeq.map(_.name).sorted
    val taken: Boolean =
      extensions.exists(ext => File(destDay.directory, s"$stem.$ext").exists) ||
        Pictures.grouped(Files.listFiles(destDay.directory)).exists(_._1 == stem)
    if taken then throw IllegalArgumentException(s"destination exists: $toPath")
    val toPair: (LocalDate, String) = (toDay, stem)
    Album.planDated(archive, fromPair, toPair, fromPath)
    Album.planCurated(archive, fromPair, toPair)
    Transcript.println(s"moving $parsed to $destDay")
    extensions.foreach: ext =>
      Transcript.println(s"moving ${parsed.file(ext)} to ${File(destDay.directory, s"$stem.$ext")}")
    val encoded: String = Metadata.encode(updated)
    Transcript.println(s"$destDay/$stem: writing metadata file\n$encoded")
    if !dryRun then
      // The preflight entries are not spliced. Recompute under the lock, then putInclude.
      Album.withWriteLock:
        val curated = Album.planCurated(archive, fromPair, toPair)
        val dated = Album.planDated(archive, fromPair, toPair, fromPath)
        curated.foreach((file, include) => Album.putInclude(file, include))
        dated.foreach((file, include) => Album.putInclude(file, include))
      Files.mkDirs(destDay.directory)
      extensions.foreach(ext => Files.move(parsed.file(ext), File(destDay.directory, s"$stem.$ext")))
      val destSidecar: File = File(destDay.directory, s"$stem.${Metadata.extension}")
      Files.write(encoded, destSidecar)
      if parsed.hasMetadata && !parsed.metadataFile.equals(destSidecar) then Files.delete(parsed.metadataFile)
      Files.deleteEmptyDirectories(parsed.parent.directory, archive)
      DirectoryIndex.dropAll()
    toPath

  private def stored(name: String, metadata: Metadata): Json = name match
    case "source"               => metadata.source.fold(Json.Null)(Json.Str(_))
    case "timestamp"            => metadata.timestamp.fold(Json.Null)(Json.Str(_))
    case "date"                 => metadata.date.fold(Json.Null)(day => Json.Str(day.toString))
    case "bad-raw"              => metadata.`bad-raw`.fold(Json.Null)(Json.Bool(_))
    case "orientation"          => metadata.orientation.fold(Json.Null)(Json.Str(_))
    case "google-media-item-id" => metadata.`google-media-item-id`.fold(Json.Null)(Json.Str(_))
    case "already-in-google"    => metadata.`already-in-google`.fold(Json.Null)(Json.Bool(_))
    case "uploader"             => metadata.uploader.fold(Json.Null)(Json.Str(_))
    case _                      => Json.Null

  private def parsePlacementDay(raw: String): LocalDate =
    Title.parse(raw) match
      case Some(Title.Parsed(Some(span), None)) if span.grain.equals(Title.Grain.Day) && span.start.equals(span.end) =>
        span.start
      case _ =>
        try LocalDate.parse(raw)
        catch
          case _: DateTimeException =>
            throw IllegalArgumentException(s"invalid date '$raw'")
