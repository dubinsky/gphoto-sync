package org.podval.tools.photo

import java.io.File
import java.time.Instant

final case class ListPictures(
  path: String,
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
  wrong: Boolean
) extends Operation:

  override def execute(session: Session): (Session, Result) =
    Operation.run(session):
      val parsed: Path = session.parse(path)
      val propertyAndValue: Option[Property.AndValue[?]] = property.map(Cli.parsePropertyAndValue)
      forEachPicture(parsed, propertyAndValue)

  private def forEachPicture(path: Path, propertyAndValue: Option[Property.AndValue[?]]): Unit = path match
    case day: Day if DirectoryIndex.cursor.isDefined =>
      DirectoryIndex.replace(listDay(DirectoryIndex.now, day, propertyAndValue))
    case pictures: Pictures => pictures.lsd(NameMode.Leave).foreach(ls(_, propertyAndValue))
    case directory: Directory => directory.lsd.foreach(forEachPicture(_, propertyAndValue))

  // A hit re-renders this invocation's flags and does not construct Extension.
  // A miss is today's cold ls, including enact, then a complete row for every grouped picture.
  private def listDay(
    session: Session,
    day: Day,
    propertyAndValue: Option[Property.AndValue[?]]
  ): Session =
    val fingerprint: Vector[FileStamp] = DirectoryIndex.fingerprint(day.directory, filesOnly = true)
    val cached: Option[DirEntry] = session.cached(day.directory).filter(_.fingerprint == fingerprint)
    cached.flatMap(_.listing) match
      case Some(rows) =>
        rows.foreach(lsListed(_, propertyAndValue))
        session
      case None =>
        val grouped: Seq[(String, Set[String])] = Pictures.group(Files.listFiles(day.directory), NameMode.Leave)
        val pictures: Seq[Picture] = day.fromGrouped(grouped)
        pictures.foreach(ls(_, propertyAndValue))
        val listed: Seq[ListedPicture] = pictures.map(toListed)
        val albums: Seq[Album.Loaded] = cached.map(_.albums).getOrElse(
          Archive.dayAlbums(day.root.configuration.archive, day.directory)
        )
        session.remember(day.directory, DirEntry(fingerprint, grouped, albums, Some(listed)))

  private def toListed(picture: Picture): ListedPicture =
    val orderedNames: Vector[String] = picture.extensions.map(_.name).toVector
    val byName: Map[String, Extension] = picture.extensions.iterator.map(ext => ext.name -> ext).toMap
    val ordered: Vector[Extension] = orderedNames.map(byName)
    val present: Boolean = ordered.forall(ext => picture.file(ext.name).isFile)
    val readMetadataTimes: Vector[(ListedExtension, Boolean)] =
      if present then ordered.map: ext =>
        val (fromMetadata, readable) = safeFromMetadata(ext)
        (ListedExtension(ext.name, fromMetadata, ext.current), readable)
      else ordered.map(ext => (listedFromSource(picture, ext), false))
    val reads: Vector[ListedExtension] = readMetadataTimes.map(_._1)
    // PictureAudit reads the corrected path. A misnamed or unreadable file uses the instants already read.
    val audit: PictureAudit.Result =
      if present && readMetadataTimes.forall(_._2) then PictureAudit(picture)
      else PictureAudit.assessed(picture, reads.map(_.fromMetadata).toSet, reads.map(_.current))
    ListedPicture(
      stem = picture.name,
      textPath = picture.toString,
      hasMetadata = picture.hasMetadata,
      hasNormal = picture.normal.isDefined,
      extensionNames = orderedNames,
      metadata = readMetadata(picture),
      timestampFromName = picture.timestampFromName,
      audit = audit,
      extensions = reads
    )

  // An empty or non-image file has no metadata timestamp. One-shot ls --long still throws; the shell stores None.
  private def safeFromMetadata(extension: Extension): (Option[Instant], Boolean) =
    try (extension.fromMetadata, true)
    catch
      case _: com.drew.imaging.ImageProcessingException => (None, false)
      case _: java.io.IOException => (None, false)

  // Leave does not rename, so a misnamed sidecar is not at the corrected path yet.
  private def readMetadata(picture: Picture): Metadata =
    if !picture.hasMetadata then Metadata.default
    else if picture.metadataFile.isFile then picture.metadata
    else sourceFile(picture, Metadata.extension).map(Metadata.read).getOrElse(Metadata.default)

  // Leave does not rename, so Extension.current's corrected path is not on disk yet.
  private def listedFromSource(picture: Picture, extension: Extension): ListedExtension =
    val source: File = sourceFile(picture, extension.name).getOrElse(picture.file(extension.name))
    val fromMetadata: Option[Instant] =
      try extension.descriptor.dater.get(source, picture.zone)
      catch
        case _: com.drew.imaging.ImageProcessingException => None
        case _: java.io.IOException => None
    ListedExtension(extension.name, fromMetadata, Dates.FromFile.instant(source, picture.zone))

  private def sourceFile(picture: Picture, extension: String): Option[File] =
    Pictures.plan(Files.listFiles(picture.parent.directory))
      .find(row => !row.omitted && row.stem == picture.name && row.extension == extension)
      .map(_.file)

  private def ls(picture: Picture, propertyAndValue: Option[Property.AndValue[?]]): Unit =
    lazy val audit: PictureAudit.Result = PictureAudit(picture)

    val included: Boolean =
      (nameContains.isEmpty || picture.name.contains(nameContains.get)) &&
      (hasExtension.isEmpty || picture.extensions.map(_.name).contains(hasExtension.get)) &&
      (propertyAndValue.isEmpty || propertyAndValue.get.is(picture.metadata)) &&
      (timestampFromNameFormat.isEmpty || picture.timestampFromName.map(_._2).contains(timestampFromNameFormat.get)) &&
      (!timestampFromName || picture.timestampFromName.isDefined) &&
      (!timestampFromNameOnly || picture.timestampFromName.isDefined && picture.extensions.forall(_.fromMetadata.isEmpty)) &&
      (!timestampsVary || audit.timestampsVary) &&
      (!hasMetadata || picture.hasMetadata) &&
      (!noNormal || picture.normal.isEmpty) &&
      (!noTimestamp || audit.timestamp.isEmpty) &&
      (!wrongTimestamp || audit.timestampWrong) &&
      (!wrongDay || audit.dayShouldBe.isDefined) &&
      (!wrong || (picture.normal.isEmpty || audit.timestamp.isEmpty || audit.timestampWrong || audit.dayShouldBe.isDefined))

    if included then
      val showAudit: Boolean = long || timestampsVary || noTimestamp || wrongTimestamp || wrongDay || wrong
      val normalGenerationSuppressed: Boolean = picture.metadata.`bad-raw`.contains(true)

      Transcript.println(
        s"$picture " ++
        picture.extensions.map(_.name).mkString("+") ++
        (if picture.normal.isDefined then "" else " NO NORMAL IMAGE" ++ (if !normalGenerationSuppressed then "" else "; GENERATION SUPPRESSED")) ++
        (if !picture.hasMetadata then "" else s" [${picture.metadata}]") ++
        picture.timestampFromName.fold("")(timestampFromName => s" timestampFromName=${timestampFromName._1} (${timestampFromName._2})") ++
        (if !showAudit then "" else
          audit.timestamp.fold(" NO TIMESTAMP")(timestamp => s" timestamp=$timestamp") ++
            audit.dayShouldBe.fold("")(dayShouldBe => s" WRONG DAY; SHOULD BE: $dayShouldBe") ++
            "\n" ++
            picture.extensions.map(extensionToString(_, audit.timestamp)).mkString("\n")
          )
      )

  private def lsListed(row: ListedPicture, propertyAndValue: Option[Property.AndValue[?]]): Unit =
    val included: Boolean =
      (nameContains.isEmpty || row.stem.contains(nameContains.get)) &&
      (hasExtension.isEmpty || row.extensionNames.contains(hasExtension.get)) &&
      (propertyAndValue.isEmpty || propertyAndValue.get.is(row.metadata)) &&
      (timestampFromNameFormat.isEmpty || row.timestampFromName.map(_._2).contains(timestampFromNameFormat.get)) &&
      (!timestampFromName || row.timestampFromName.isDefined) &&
      (!timestampFromNameOnly || row.timestampFromName.isDefined && row.extensions.forall(_.fromMetadata.isEmpty)) &&
      (!timestampsVary || row.audit.timestampsVary) &&
      (!hasMetadata || row.hasMetadata) &&
      (!noNormal || !row.hasNormal) &&
      (!noTimestamp || row.audit.timestamp.isEmpty) &&
      (!wrongTimestamp || row.audit.timestampWrong) &&
      (!wrongDay || row.audit.dayShouldBe.isDefined) &&
      (!wrong || (!row.hasNormal || row.audit.timestamp.isEmpty || row.audit.timestampWrong || row.audit.dayShouldBe.isDefined))

    if included then
      val showAudit: Boolean = long || timestampsVary || noTimestamp || wrongTimestamp || wrongDay || wrong
      val normalGenerationSuppressed: Boolean = row.metadata.`bad-raw`.contains(true)
      Transcript.println(
        s"${row.textPath} " ++
        row.extensionNames.mkString("+") ++
        (if row.hasNormal then "" else " NO NORMAL IMAGE" ++ (if !normalGenerationSuppressed then "" else "; GENERATION SUPPRESSED")) ++
        (if !row.hasMetadata then "" else s" [${row.metadata}]") ++
        row.timestampFromName.fold("")(timestampFromName => s" timestampFromName=${timestampFromName._1} (${timestampFromName._2})") ++
        (if !showAudit then "" else
          row.audit.timestamp.fold(" NO TIMESTAMP")(timestamp => s" timestamp=$timestamp") ++
            row.audit.dayShouldBe.fold("")(dayShouldBe => s" WRONG DAY; SHOULD BE: $dayShouldBe") ++
            "\n" ++
            row.extensions.map(extension => extensionListed(extension, row.audit.timestamp)).mkString("\n")
          )
      )

  private def extensionListed(extension: ListedExtension, timestamp: Option[Instant]): String =
    s"  .${extension.name}" ++
      extension.fromMetadata.fold("")(fromMetadata => s" fromMetadata=$fromMetadata") ++
      s" current=${extension.current}" ++
      timestamp.flatMap(timestamp => Option.when(!Dates.equal(timestamp, extension.current))(timestamp)).fold("")(toSet => s" toSet=$toSet")

  private def extensionToString(extension: Extension, timestamp: Option[Instant]): String =
    s"  .${extension.name}" ++
      extension.fromMetadata.fold("")(fromMetadata => s" fromMetadata=$fromMetadata") ++
      s" current=${extension.current}" ++
      timestamp.flatMap(timestamp => Option.when(!Dates.equal(timestamp, extension.current))(timestamp)).fold("")(toSet => s" toSet=$toSet")
