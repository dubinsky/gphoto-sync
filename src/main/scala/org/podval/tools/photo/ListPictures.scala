package org.podval.tools.photo

import com.drew.imaging.ImageProcessingException
import java.io.File
import java.time.{Instant, LocalDateTime, ZoneId}
import java.time.temporal.ChronoUnit

final case class ListPictures(
  path: Option[String],
  long: Boolean,
  nameContains: Option[String],
  hasExtension: Option[String],
  property: Option[String],
  timestampFromNameFormat: Option[String],
  timestampFromName: Boolean,
  timestampFromNameOnly: Boolean,
  timestampsVary: Boolean,
  hasMetadata: Boolean,
  noNormal: Boolean,
  noTimestamp: Boolean,
  wrongTimestamp: Boolean,
  wrongDay: Boolean,
  wrong: Boolean,
  recursive: Boolean = false
) extends Operation:

  override def execute(session: Session): (Session, Result) =
    var produced: Json = Json.Null
    Operation.run(session, produced):
      produced = listed(session)

  private def listed(session: Session): Json =
    val raw: String = path.getOrElse("")
    val segments: List[String] = raw.split('/').toList.filter(_.nonEmpty)
    session.style match
      // Root.parse keeps '.' and '..' as names, so the first segment is the whole check.
      case PathStyle.OneShot | PathStyle.Serve =>
        if segments.headOption.contains(UploadDirectory.directoryName) then
          throw IllegalArgumentException("not a date-tree path")
      // Pictures.item throws when the stem is absent, and Leave would print before the real walk.
      // Session.walk climbs '..' off that picture, so the tail can re-enter .upload.
      case PathStyle.Cursor =>
        val start: Directory = if raw.startsWith("/") then session.root else session.cursor
        val _: (Directory, Boolean, Boolean) =
          segments.foldLeft((start, false, true)):
            case ((current, onPicture, live), segment) =>
              if !live then (current, false, false)
              else if onPicture then
                if segment == "." then (current, true, true)
                else if segment == ".." then (current, false, true)
                else (current, true, false)
              else if current.isInstanceOf[Pictures] && segment != "." && segment != ".." then
                if current.isInstanceOf[UploadDirectory] then throw IllegalArgumentException("not a date-tree path")
                else (current, true, true)
              else
                session.copy(cursor = current).parse(segment) match
                  case directory: Directory => (directory, false, true)
                  case _                    => (current, false, false)
    val parsed: Path = session.parse(raw)
    val propertyAndValue: Option[Property.AndValue[?]] = property.map(Cli.parsePropertyAndValue)
    parsed match
      case _: UploadDirectory =>
        throw IllegalArgumentException("not a date-tree path")
      case picture: Picture if picture.parent.isInstanceOf[UploadDirectory] =>
        throw IllegalArgumentException("not a date-tree path")
      case day: Day =>
        itemsJson(walk(day, propertyAndValue))
      case picture: Picture =>
        itemsJson(walk(picture, propertyAndValue))
      case directory: Directory if !pictureFilter && !recursive =>
        listChildren(directory)
      case other =>
        itemsJson(walk(other, propertyAndValue))

  private def childDirectories(directory: Directory): Seq[Directory] = directory match
    // Root.lsd prepends .upload.
    case root: Root => Year.lsd(root)
    case other      => other.lsd

  private def listChildren(directory: Directory): Json =
    val entries: Seq[Json] = childDirectories(directory).map: child =>
      val relative: String = child.toString.stripPrefix("/")
      val (key, count) = countOf(child)
      if long then Transcript.println(s"$relative $key=$count")
      else Transcript.println(relative)
      Json.Obj(Seq("path" -> Json.Str(relative), key -> Json.Long(count.toLong)))
    directoriesJson(entries)

  private def countOf(child: Directory): (String, Int) = child match
    case year: Year   => ("months", year.lsd.size)
    case month: Month => ("days", month.lsd.size)
    case day: Day     => ("items", Pictures.grouped(Files.listFiles(day.directory)).size)
    case other        => throw IllegalStateException(s"no directory count for ${other.getClass.getSimpleName}")

  private def pictureFilter: Boolean =
    nameContains.isDefined ||
      hasExtension.isDefined ||
      property.isDefined ||
      timestampFromNameFormat.isDefined ||
      timestampFromName ||
      timestampFromNameOnly ||
      timestampsVary ||
      hasMetadata ||
      noNormal ||
      noTimestamp ||
      wrongTimestamp ||
      wrongDay ||
      wrong

  private def walk(path: Path, propertyAndValue: Option[Property.AndValue[?]]): Seq[ListedLine] = path match
    case day: Day if DirectoryIndex.cursor.isDefined =>
      val (next, entries) = listDay(DirectoryIndex.now, day, propertyAndValue)
      DirectoryIndex.replace(next)
      entries
    case picture: Picture =>
      ls(picture, propertyAndValue).toSeq
    case pictures: Pictures =>
      pictures.lsd(NameMode.Leave).flatMap(picture => ls(picture, propertyAndValue))
    case directory: Directory =>
      childDirectories(directory).flatMap(child => walk(child, propertyAndValue))
    case _ =>
      throw IllegalArgumentException("not a date-tree path")

  private def listDay(
    session: Session,
    day: Day,
    propertyAndValue: Option[Property.AndValue[?]]
  ): (Session, Seq[ListedLine]) =
    val fingerprint: Vector[FileStamp] = DirectoryIndex.fingerprint(day.directory, filesOnly = true)
    val cached: Option[DirEntry] = session.cached(day.directory).filter(_.fingerprint == fingerprint)
    val zone: ZoneId = day.root.configuration.zoneId
    cached.flatMap(_.listing) match
      case Some(rows) =>
        val entries: Seq[ListedLine] = rows.flatMap(row => lsListed(row, propertyAndValue, zone))
        (session, entries)
      case None =>
        val grouped: Seq[(String, Set[String])] = Pictures.group(Files.listFiles(day.directory), NameMode.Leave)
        val pictures: Seq[Picture] = day.fromGrouped(grouped)
        val entries: Seq[ListedLine] = pictures.flatMap(picture => ls(picture, propertyAndValue))
        val listed: Seq[ListedPicture] = pictures.map(toListed)
        val albums: Seq[Album.Loaded] = cached
          .map(_.albums)
          .getOrElse(
            Archive.dayAlbums(day.root.configuration.archive, day.directory)
          )
        val remembered: Session = session.remember(
          day.directory,
          DirEntry(fingerprint, grouped, albums, Some(listed), cached.flatMap(_.sidecars))
        )
        (remembered, entries)

  private def toListed(picture: Picture): ListedPicture =
    val orderedNames: Vector[String] = picture.extensions.map(_.name).toVector
    val byName: Map[String, Extension] = picture.extensions.iterator.map(ext => ext.name -> ext).toMap
    val ordered: Vector[Extension] = orderedNames.map(byName)
    val metadata: Metadata = readSidecar(picture)
    val timestampFromName: Option[(Dates.Clock, String)] =
      Dates.fromFileName(picture.name, picture.zone, metadata.date.orElse(picture.parent.day.map(_.date)))
    val reads: Vector[ListedExtension] =
      ordered.map(ext => listedExtension(picture, ext, Dates.usable(timestampFromName.map(_._1))))
    val audit: Dates.Assessment = PictureAudit.assessed(
      picture,
      reads.map(ext => Dates.Observed(ext.name, Extension.votes(ext.name), ext.read)),
      reads.map(_.current),
      timestampFromName.map(_._1),
      metadata
    )
    ListedPicture(
      stem = picture.name,
      textPath = picture.toString,
      hasMetadata = picture.hasMetadata,
      hasNormal = picture.normal.isDefined,
      extensionNames = orderedNames,
      metadata = metadata,
      timestampFromName = timestampFromName,
      audit = audit,
      extensions = reads
    )

  // Leave does not create the corrected path. Read that file only when it is already there.
  private def readSidecar(picture: Picture): Metadata =
    if !picture.hasMetadata then Metadata.default
    else
      val corrected: File = picture.metadataFile
      if corrected.isFile then Metadata.read(corrected)
      else sourceFile(picture, Metadata.extension).filter(_.isFile).fold(Metadata.default)(Metadata.read)

  private def listedExtension(
    picture: Picture,
    extension: Extension,
    filename: Option[Dates.Clock]
  ): ListedExtension =
    val corrected: File = picture.file(extension.name)
    if corrected.isFile then ListedExtension(extension.name, extension.read, extension.current)
    else
      val source: File = sourceFile(picture, extension.name).getOrElse(corrected)
      ListedExtension(
        extension.name,
        extension.descriptor.dater.read(extension.name, source, filename, picture.zone),
        Dates.FromFile.instant(source)
      )

  private def sourceFile(picture: Picture, extension: String): Option[File] =
    Pictures
      .plan(Files.listFiles(picture.parent.directory))
      .find(row => !row.omitted && row.stem == picture.name && row.extension == extension)
      .map(_.file)

  private def ls(picture: Picture, propertyAndValue: Option[Property.AndValue[?]]): Option[ListedLine] =
    lazy val audit: Dates.Assessment = PictureAudit(picture)

    val included: Boolean =
      (nameContains.isEmpty || picture.name.contains(nameContains.get)) &&
        (hasExtension.isEmpty || picture.extensions.map(_.name).contains(hasExtension.get)) &&
        (propertyAndValue.isEmpty || propertyAndValue.get.is(picture.metadata)) &&
        (timestampFromNameFormat.isEmpty || picture.timestampFromName
          .map(_._2)
          .contains(timestampFromNameFormat.get)) &&
        (!timestampFromName || picture.timestampFromName.isDefined) &&
        (!timestampFromNameOnly || filenameOnly(
          picture.timestampFromName.isDefined,
          picture.extensions.map(extension => extension.votes -> extension.read)
        )) &&
        (!timestampsVary || audit.timestampsVary) &&
        (!hasMetadata || picture.hasMetadata) &&
        (!noNormal || picture.normal.isEmpty) &&
        (!noTimestamp || PictureAudit.noAssessedTimestamp(audit)) &&
        (!wrongTimestamp || audit.timestampWrong) &&
        (!wrongDay || audit.dayShouldBe.isDefined) &&
        (!wrong || (
          picture.normal.isEmpty ||
            PictureAudit.noAssessedTimestamp(audit) ||
            audit.timestampWrong ||
            audit.dayShouldBe.isDefined
        ))

    if !included then None
    else
      val extensionNames: Seq[String] = picture.extensions.map(_.name).toSeq
      val sidecar: String = if picture.hasMetadata then picture.metadata.toString else ""
      val showAudit: Boolean = long || timestampsVary || noTimestamp || wrongTimestamp || wrongDay || wrong
      val normalGenerationSuppressed: Boolean = picture.metadata.`bad-raw`.contains(true)
      val text: String =
        s"$picture " ++
          extensionNames.mkString("+") ++
          (if picture.normal.isDefined then ""
           else " NO NORMAL IMAGE" ++ (if !normalGenerationSuppressed then "" else "; GENERATION SUPPRESSED")) ++
          (if !picture.hasMetadata then "" else s" [$sidecar]") ++
          picture.timestampFromName.fold("")(found => s" timestampFromName=${show(found._1)} (${found._2})") ++
          (if !showAudit then ""
           else
             auditText(audit) ++
               audit.dayShouldBe.fold("")(dayShouldBe => s" WRONG DAY; SHOULD BE: $dayShouldBe") ++
               "\n" ++
               picture.extensions
                 .map(extension =>
                   extensionLine(extension.name, extension.read, extension.current, audit.writesMtime, picture.zone)
                 )
                 .mkString("\n"))
      // PictureAudit reads the corrected path. A short line has not renamed, and an empty file
      // must still print; listDay's own toListed is what rejects the extractor failure.
      val forValue: Option[Dates.Assessment] =
        if showAudit then Some(audit)
        else
          try Some(toListed(picture).audit)
          catch case _: ImageProcessingException => None
      val path: String = picture.toString.stripPrefix("/")
      Some(
        ListedLine(text, itemJson(picture.toString, extensionNames, sidecar, forValue), assessedCivil(forValue), path)
      )

  private def lsListed(
    row: ListedPicture,
    propertyAndValue: Option[Property.AndValue[?]],
    zone: ZoneId
  ): Option[ListedLine] =
    val included: Boolean =
      (nameContains.isEmpty || row.stem.contains(nameContains.get)) &&
        (hasExtension.isEmpty || row.extensionNames.contains(hasExtension.get)) &&
        (propertyAndValue.isEmpty || propertyAndValue.get.is(row.metadata)) &&
        (timestampFromNameFormat.isEmpty || row.timestampFromName.map(_._2).contains(timestampFromNameFormat.get)) &&
        (!timestampFromName || row.timestampFromName.isDefined) &&
        (!timestampFromNameOnly || filenameOnly(
          row.timestampFromName.isDefined,
          row.extensions.map(extension => Extension.votes(extension.name) -> extension.read)
        )) &&
        (!timestampsVary || row.audit.timestampsVary) &&
        (!hasMetadata || row.hasMetadata) &&
        (!noNormal || !row.hasNormal) &&
        (!noTimestamp || PictureAudit.noAssessedTimestamp(row.audit)) &&
        (!wrongTimestamp || row.audit.timestampWrong) &&
        (!wrongDay || row.audit.dayShouldBe.isDefined) &&
        (!wrong || (
          !row.hasNormal ||
            PictureAudit.noAssessedTimestamp(row.audit) ||
            row.audit.timestampWrong ||
            row.audit.dayShouldBe.isDefined
        ))

    if !included then None
    else
      val sidecar: String = if row.hasMetadata then row.metadata.toString else ""
      val showAudit: Boolean = long || timestampsVary || noTimestamp || wrongTimestamp || wrongDay || wrong
      val normalGenerationSuppressed: Boolean = row.metadata.`bad-raw`.contains(true)
      val text: String =
        s"${row.textPath} " ++
          row.extensionNames.mkString("+") ++
          (if row.hasNormal then ""
           else " NO NORMAL IMAGE" ++ (if !normalGenerationSuppressed then "" else "; GENERATION SUPPRESSED")) ++
          (if !row.hasMetadata then "" else s" [$sidecar]") ++
          row.timestampFromName.fold("")(found => s" timestampFromName=${show(found._1)} (${found._2})") ++
          (if !showAudit then ""
           else
             auditText(row.audit) ++
               row.audit.dayShouldBe.fold("")(dayShouldBe => s" WRONG DAY; SHOULD BE: $dayShouldBe") ++
               "\n" ++
               row.extensions
                 .map(extension =>
                   extensionLine(extension.name, extension.read, extension.current, row.audit.writesMtime, zone)
                 )
                 .mkString("\n"))
      Some(
        ListedLine(
          text,
          itemJson(row.textPath, row.extensionNames, sidecar, Some(row.audit)),
          assessedCivil(Some(row.audit)),
          row.textPath.stripPrefix("/")
        )
      )

  private def filenameOnly(hasName: Boolean, captured: Iterable[(Boolean, Option[Dates.Clock])]): Boolean =
    hasName && captured.forall((votes, clock) => !votes || Dates.usable(clock).isEmpty)

  private def show(clock: Dates.Clock): String =
    clock.offset.fold(clock.civil.toString)(offset => s"${clock.civil}${offset.getId}")

  private def assessedJson(audit: Dates.Assessment): Json =
    if PictureAudit.noAssessedTimestamp(audit) || audit.disagreement.isDefined then Json.Null
    else audit.clock.fold(Json.Null)(clock => Json.Str(show(clock)))

  // Same null as assessedJson. FolderDay, Undated, Disagree.
  // audit = None is only the catch in ls, not a cursor day.
  private def assessedCivil(audit: Option[Dates.Assessment]): Option[LocalDateTime] =
    audit.flatMap: value =>
      if PictureAudit.noAssessedTimestamp(value) || value.disagreement.isDefined then None
      else value.clock.map(_.civil)

  private def itemJson(
    textPath: String,
    extensionNames: Seq[String],
    sidecar: String,
    audit: Option[Dates.Assessment]
  ): Json =
    Json.Obj(
      Seq(
        "path" -> Json.Str(textPath.stripPrefix("/")),
        "extensions" -> Json.Arr(extensionNames.map(name => Json.Str(name))),
        "sidecar" -> Json.Str(sidecar),
        "assessed" -> audit.fold(Json.Null)(assessedJson),
        "wrongDay" -> Json.Bool(audit.exists(_.dayShouldBe.isDefined))
      )
    )

  private def directoriesJson(entries: Seq[Json]): Json =
    Json.Obj(Seq("kind" -> Json.Str("directories"), "entries" -> Json.Arr(entries)))

  private final case class ListedLine(
    text: String,
    json: Json,
    when: Option[LocalDateTime],
    path: String
  )

  private def itemsJson(entries: Seq[ListedLine]): Json =
    val ordered: Seq[ListedLine] =
      entries.sortBy(line => ListPictures.itemOrder(line.when, line.path))
    ordered.foreach(line => Transcript.println(line.text))
    Json.Obj(Seq("kind" -> Json.Str("items"), "entries" -> Json.Arr(ordered.map(_.json))))

  private def auditText(audit: Dates.Assessment): String =
    audit.disagreement match
      case Some(disagree) =>
        s" DATE DISAGREES WITH TIMESTAMP timestamp=${disagree.timestamp} date=${disagree.date}"
      case None =>
        audit.clock match
          case Some(clock) if clock.source == Dates.Source.FolderDay =>
            " NO TIMESTAMP source=FolderDay"
          case Some(clock) =>
            s" timestamp=${show(clock)} source=${clock.source}"
          case None =>
            " NO TIMESTAMP"

  private def extensionLine(
    name: String,
    read: Option[Dates.Clock],
    current: Instant,
    writes: Option[Dates.Clock],
    zone: ZoneId
  ): String =
    val currentText: String =
      current.atZone(zone).toLocalDateTime.truncatedTo(ChronoUnit.SECONDS).toString
    val toSet: String = writes
      .filter(clock => !Dates.sameSecond(clock.civil, current, zone))
      .map(clock => clock.civil.truncatedTo(ChronoUnit.SECONDS).toString)
      .fold("")(text => s" toSet=$text")
    s"  .$name" ++
      read.fold("")(clock => s" read=${show(clock)}") ++
      s" current=$currentText" ++
      toSet

object ListPictures:
  // Timed first. Epoch day, then nano-of-day, so subseconds participate. Path last.
  private[photo] def itemOrder(when: Option[LocalDateTime], path: String): (Boolean, Long, Long, String) =
    (
      when.isEmpty,
      when.fold(0L)(_.toLocalDate.toEpochDay),
      when.fold(0L)(_.toLocalTime.toNanoOfDay),
      path
    )
