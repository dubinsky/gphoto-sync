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
    val metadata: Metadata = readSidecar(picture)
    val timestampFromName: Option[(Instant, String)] =
      Dates.fromFileName(picture.name, picture.zone, metadata.date.orElse(picture.parent.day.map(_.date)))
    val reads: Vector[ListedExtension] = ordered.map(ext => listedExtension(picture, ext))
    val audit: PictureAudit.Result = PictureAudit.assessed(
      picture,
      reads.map(_.fromMetadata).toSet,
      reads.map(_.current),
      timestampFromName.map(_._1),
      Dates.FromMetadata.timestamp(metadata, picture.zone)
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

  private def listedExtension(picture: Picture, extension: Extension): ListedExtension =
    val corrected: File = picture.file(extension.name)
    if corrected.isFile then ListedExtension(extension.name, extension.fromMetadata, extension.current)
    else
      val source: File = sourceFile(picture, extension.name).getOrElse(corrected)
      ListedExtension(
        extension.name,
        extension.descriptor.dater.get(source, picture.zone),
        Dates.FromFile.instant(source, picture.zone)
      )

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
