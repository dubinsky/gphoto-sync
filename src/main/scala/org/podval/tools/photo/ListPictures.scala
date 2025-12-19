package org.podval.tools.photo

import java.io.File
import java.time.{Instant, LocalDate}

final case class ListPictures(
  path: Path,
  long: Boolean,
  fix: Boolean,
  nameContains: Option[String],
  hasExtension: Option[String],
  propertyAndValue: Option[Property.AndValue[?]],
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

  override def execute(): Unit = forEachPicture(path)

  private def forEachPicture(path: Path): Unit = path match
    case pictures: Pictures => pictures.lsd(fix).foreach(ls)
    case directory: Directory => directory.lsd.foreach(forEachPicture)

  private def ls(picture: Picture): Unit =
    var timestampCalculated: Boolean = false

    lazy val (
      isTimestampsVary: Boolean,
      timestamp: Option[Instant],
      isTimestampWrong: Boolean,
      dayShouldBe: Option[Day]
    ) =
      timestampCalculated = true

      // Note: in the pictures saw where metadata timestamp is present and file name also encodes the timestamp:
      // - for '3gp' and 'mp4' metadata timestamp is sometimes later than the file name timestamp
      //   (and a few hours for the timezone offset) - metadata probably records timestamp of the end of recording;
      // - for 'jpg' metadata timestamp is sometimes earlier than the file name timestamp by a second;
      // I pick the earliest ;)
      val timestamps: Set[Option[Instant]] = picture.extensions.map(_.fromMetadata) ++ Set(picture.timestampFromName.map(_._1))
      val timestampEarliest: Option[Instant] = Dates.earliest(timestamps)
      val timestampLatest: Option[Instant] = Dates.latest(timestamps)
      val isTimestampsVary: Boolean = timestampEarliest.isDefined && timestampLatest.isDefined &&
        Dates.distanceInMinutes(timestampEarliest.get, timestampLatest.get) > 60

      val timestamp: Option[Instant] = Dates.FromMetadata.get(picture).orElse(timestampEarliest)

      val isTimestampWrong: Boolean = timestamp.fold(false)(timestamp =>
        picture.extensions.exists(extension => !Dates.equal(timestamp, extension.current))
      )

      val dayShouldBe: Option[Day] = timestamp.flatMap: timestamp =>
        val date: LocalDate = Dates.toZonedDateTime(timestamp).toLocalDate
        val year: Int = date.getYear
        val month: Int = date.getMonthValue
        val day: Int = date.getDayOfMonth

        // some photos were taken with no time set on the camera...
        val isDefaultDate: Boolean = month == 1 && day == 1 && (year == 1980 || year == 2000)
        val isCorrectDay: Boolean = picture.parent.day.exists(_.date.isEqual(date))
        Option.when(!isDefaultDate && !isCorrectDay)(picture.root.year(year).month(month).day(day))

      (
        isTimestampsVary,
        timestamp,
        isTimestampWrong,
        dayShouldBe
      )

    val included: Boolean =
      (nameContains.isEmpty || picture.name.contains(nameContains.get)) &&
      (hasExtension.isEmpty || picture.extensions.map(_.name).contains(hasExtension.get)) &&
      (propertyAndValue.isEmpty || propertyAndValue.get.is(picture.metadata)) &&
      (timestampFromNameFormat.isEmpty || picture.timestampFromName.map(_._2).contains(timestampFromNameFormat.get)) &&
      (!timestampFromName || picture.timestampFromName.isDefined) &&
      (!timestampFromNameOnly || picture.timestampFromName.isDefined && picture.extensions.forall(_.fromMetadata.isEmpty)) &&
      (!timestampsVary || isTimestampsVary) &&
      (!hasMetadata || picture.hasMetadata) &&
      (!noNormal || picture.normal.isEmpty) &&
      (!noTimestamp || timestamp.isEmpty) &&
      (!wrongTimestamp || isTimestampWrong) &&
      (!wrongDay || dayShouldBe.isDefined) &&
      (!wrong || (picture.normal.isEmpty || timestamp.isEmpty || isTimestampWrong || dayShouldBe.isDefined))

    if included then
      if long then timestamp

      val normalGenerationSuppressed: Boolean = picture.metadata.`bad-raw`.contains(true)

      println(
        s"$picture " ++
        picture.extensions.map(_.name).mkString("+") ++
        (if picture.normal.isDefined then "" else " NO NORMAL IMAGE" ++ (if !normalGenerationSuppressed then "" else "; GENERATION SUPPRESSED")) ++
        (if !picture.hasMetadata then "" else s" [${picture.metadata}]") ++
        picture.timestampFromName.fold("")(timestampFromName => s" timestampFromName=${timestampFromName._1} (${timestampFromName._2})") ++
        (if !timestampCalculated then "" else
          timestamp.fold(" NO TIMESTAMP")(timestamp => s" timestamp=$timestamp") ++
            dayShouldBe.fold("")(dayShouldBe => s" WRONG DAY; SHOULD BE: $dayShouldBe") ++
            "\n" ++
            picture.extensions.map(extensionToString(_, timestamp)).mkString("\n")
          )
      )

      if picture.normal.isEmpty && !normalGenerationSuppressed then
        execute(s"$picture: generating '${RawConverter.extension}' from raw file")(
          RawConverter.DCRaw.generate(picture)
        )

      if timestampCalculated then
        timestamp.foreach: timestamp =>
          picture.extensions.foreach(extension =>
            if !Dates.equal(timestamp, extension.current) then
              execute(s"$extension: changing timestamp ${extension.current} to $timestamp")(
                Dates.FromFile.set(picture, extension.name, timestamp)
              )
          )

        dayShouldBe.foreach: dayShouldBe =>
          execute(s"moving $picture to $dayShouldBe") {
            Files.mkDirs(dayShouldBe.directory)
            val extensionNamesAll: Set[String] = picture.extensions.map(_.name) ++ Option.when(picture.hasMetadata)(Metadata.extension).toSet
            extensionNamesAll.foreach(extension => Files.move(
              File(picture.parent.directory, s"${picture.name}.$extension"),
              File(dayShouldBe   .directory, s"${picture.name}.$extension")
            ))
            Files.deleteEmptyDirectories(picture.parent.directory)
          }

  private def execute(message: String)(action: => Unit): Unit =
    if !fix then println(s"$message - skipping: no --fix ") else
      println(message)
      action

  private def extensionToString(extension: Extension, timestamp: Option[Instant]): String =
    s"  .${extension.name}" ++
      extension.fromMetadata.fold("")(fromMetadata => s" fromMetadata=$fromMetadata") ++
      s" current=${extension.current}" ++
      timestamp.flatMap(timestamp => Option.when(!Dates.equal(timestamp, extension.current))(timestamp)).fold("")(toSet => s" toSet=$toSet")
