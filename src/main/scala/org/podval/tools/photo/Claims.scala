package org.podval.tools.photo

import java.io.File
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, YearMonth, ZoneId, ZoneOffset}

object Claims:
  final case class Resolved(
    path: String,
    dated: Option[String],
    curated: Seq[String]
  ):
    def line: String =
      val datedText: String = dated.getOrElse("-")
      val curatedText: String = if curated.isEmpty then "-" else curated.mkString(",")
      s"$path dated=$datedText curated=$curatedText"

  def apply(originals: File, zone: ZoneId): Either[String, Seq[Resolved]] =
    val loaded: Loaded = load(originals, zone)
    val conflicts: Seq[String] = loaded.pictures.flatMap: picture =>
      val events: Seq[Album.Event] = claiming(loaded, picture)
      Option.when(events.sizeIs > 1)(s"${picture.path} claimed by ${events.map(_.title).mkString(" and ")}")

    if conflicts.nonEmpty then Left(conflicts.mkString("\n"))
    else Right(loaded.pictures.map(resolved(loaded, _)))

  private final case class Found(
    path: String,
    name: String,
    folderDay: LocalDate,
    yearMonth: YearMonth,
    time: LocalDateTime
  )

  private final case class Loaded(
    events: Seq[Album.Event],
    days: Map[LocalDate, Album.Day],
    months: Map[YearMonth, Album.Month],
    curated: Seq[Album.Curated],
    pictures: Seq[Found]
  ):
    def ++(that: Loaded): Loaded = Loaded(
      events ++ that.events,
      days ++ that.days,
      months ++ that.months,
      curated ++ that.curated,
      pictures ++ that.pictures
    )

  private val empty: Loaded = Loaded(Seq.empty, Map.empty, Map.empty, Seq.empty, Seq.empty)

  private val pictureExtensions: Set[String] = (Extension.normal ++ Extension.raw).map(_.name)

  private def load(originals: File, zone: ZoneId): Loaded =
    val curated: Loaded = empty.copy(curated = tomlFiles(originals).map: file =>
      Album.readCurated(file, Album.title(originals, file))
    )
    childDirectories(originals).filter(dir => isNumber(dir.getName, 4)).foldLeft(curated): (loaded, yearDir) =>
      loaded ++ loadYear(originals, yearDir, zone)

  private def loadYear(originals: File, yearDir: File, zone: ZoneId): Loaded =
    val year: Int = yearDir.getName.toInt
    val months: Map[YearMonth, Album.Month] = tomlFiles(yearDir).flatMap: file =>
      val (name, _) = Files.nameAndExtension(file)
      Option.when(isMonth(name)):
        YearMonth.of(year, name.toInt) -> Album.readMonth(file, Album.title(originals, file))
    .toMap

    childDirectories(yearDir).filter(dir => isMonth(dir.getName)).foldLeft(empty.copy(months = months)):
      (loaded, monthDir) => loaded ++ loadMonth(originals, year, monthDir, zone)

  private def loadMonth(originals: File, year: Int, monthDir: File, zone: ZoneId): Loaded =
    val month: Int = monthDir.getName.toInt
    val days: Map[LocalDate, Album.Day] = tomlFiles(monthDir).flatMap: file =>
      val (name, _) = Files.nameAndExtension(file)
      Option.when(isDay(name)):
        LocalDate.of(year, month, name.toInt) -> Album.readDay(file, Album.title(originals, file))
    .toMap

    childDirectories(monthDir).filter(dir => isDay(dir.getName)).foldLeft(empty.copy(days = days)):
      (loaded, dayDir) => loaded ++ loadDay(originals, year, month, dayDir, zone)

  private def loadDay(originals: File, year: Int, month: Int, dayDir: File, zone: ZoneId): Loaded =
    val folderDay: LocalDate = LocalDate.of(year, month, dayDir.getName.toInt)
    val files: Seq[File] = Files.listFiles(dayDir)
    val events: Seq[Album.Event] = files.filter(isEventFile).map: file =>
      Album.readEvent(file, Album.title(originals, file), folderDay)
    val pictures: Seq[Found] = Pictures.group(files, fix = false).flatMap: (name, extensions) =>
      Option.when(extensions.exists(pictureExtensions.contains)):
        Found(
          path = s"${Album.relative(originals, dayDir)}/$name",
          name = name,
          folderDay = folderDay,
          yearMonth = YearMonth.of(year, month),
          time = pictureTime(name, readMetadata(dayDir, name, extensions), folderDay, zone)
        )
    empty.copy(events = events, pictures = pictures)

  private def isEventFile(file: File): Boolean =
    val (name, extension) = Files.nameAndExtension(file)
    extension.contains(Metadata.extension) && !name.headOption.exists(_.isDigit)

  private def readMetadata(directory: File, name: String, extensions: Set[String]): Metadata =
    if extensions.contains(Metadata.extension)
    then Metadata.read(File(directory, s"$name.${Metadata.extension}"))
    else Metadata.default

  // timestamp wins over the filename. metadata.date is not a membership input; NameShort gets the folder day.
  private def pictureTime(name: String, metadata: Metadata, folderDay: LocalDate, zone: ZoneId): LocalDateTime =
    metadata.timestamp
      .map(LocalDateTime.parse)
      .orElse(timeFromName(name, zone, folderDay))
      .getOrElse(folderDay.atStartOfDay)

  // One trailing _1 is stripped only here, as in Picture.timestampFromName. The picture name is unchanged.
  private def timeFromName(name: String, zone: ZoneId, folderDay: LocalDate): Option[LocalDateTime] =
    val clock: String = if name.endsWith("_1") then name.dropRight("_1".length) else name
    Dates.NameFull.get(clock, zone).map(civil)
      .orElse(Dates.NameCompact.get(clock, zone).map(civil))
      .orElse(Dates.NameMillis.get(clock, zone).map(_.atZone(zone).toLocalDateTime))
      .orElse(Dates.NameShort.get(clock, zone, folderDay).map(civil))

  // NameFull, NameCompact, and NameShort store the civil clock as a UTC instant.
  private def civil(instant: Instant): LocalDateTime = LocalDateTime.ofInstant(instant, ZoneOffset.UTC)

  private def claiming(loaded: Loaded, picture: Found): Seq[Album.Event] =
    loaded.events.filter(eventClaims(_, picture))

  private def eventClaims(event: Album.Event, picture: Found): Boolean =
    inInterval(event, picture.time) &&
      inNameBounds(picture.name, event) &&
      !event.exclude.contains(picture.name)

  private def inInterval(event: Album.Event, time: LocalDateTime): Boolean =
    val (start, end) = interval(event)
    !time.isBefore(start) && !time.isAfter(end)

  private def interval(event: Album.Event): (LocalDateTime, LocalDateTime) =
    val start: Album.When = event.from.getOrElse(Album.When.Date(event.pathDay))
    val end: Album.When = event.to.getOrElse(Album.When.Date(dayOf(start)))
    (lower(start), upper(end))

  private def dayOf(when: Album.When): LocalDate = when match
    case Album.When.Date(date) => date
    case Album.When.DateTime(dateTime) => dateTime.toLocalDate

  private def lower(when: Album.When): LocalDateTime = when match
    case Album.When.Date(date) => date.atStartOfDay
    case Album.When.DateTime(dateTime) => dateTime

  private def upper(when: Album.When): LocalDateTime = when match
    case Album.When.Date(date) => date.atTime(LocalTime.MAX)
    case Album.When.DateTime(dateTime) => dateTime

  private def inNameBounds(name: String, event: Album.Event): Boolean =
    event.nameFrom.forall(name >= _) && event.nameTo.forall(name <= _)

  private def resolved(loaded: Loaded, picture: Found): Resolved =
    val dated: Option[String] = claiming(loaded, picture).headOption.map(_.title)
      .orElse(kept(loaded.days.get(picture.folderDay), picture.name))
      .orElse(kept(loaded.months.get(picture.yearMonth), picture.name))
    val curated: Seq[String] = loaded.curated
      .filter(album => album.members.contains(picture.path) && !album.exclude.contains(picture.name))
      .map(_.title)
      .sorted
    Resolved(picture.path, dated, curated)

  private def kept(album: Option[Album], name: String): Option[String] =
    album.filterNot(_.exclude.contains(name)).map(_.title)

  private def childDirectories(directory: File): Seq[File] =
    children(directory).filter(_.isDirectory).sortBy(_.getName)

  private def tomlFiles(directory: File): Seq[File] =
    children(directory).filter(file => file.isFile && file.getName.endsWith(".toml")).sortBy(_.getName)

  private def children(directory: File): Seq[File] =
    Option(directory.listFiles).map(_.toSeq).getOrElse(Seq.empty)

  private def isMonth(name: String): Boolean = isNumber(name, 2) && name.toInt >= 1 && name.toInt <= 12

  private def isDay(name: String): Boolean = isNumber(name, 2) && name.toInt >= 1 && name.toInt <= 31

  private def isNumber(name: String, length: Int): Boolean =
    name.length == length && name.forall(_.isDigit)

final case class ListAlbums(path: Option[Path]) extends Operation:
  override def execute(): Unit =
    val root: Root = path.fold(Root.get)(_.root)
    val pictures: Seq[Claims.Resolved] = Claims(root.configuration.byYearDirectory, root.configuration.zoneId) match
      case Left(error) =>
        println(error)
        throw IllegalArgumentException(error)
      case Right(pictures) => pictures

    selected(pictures).foreach(picture => println(picture.line))

  private def selected(pictures: Seq[Claims.Resolved]): Seq[Claims.Resolved] = path match
    case None => pictures
    case Some(path) =>
      val prefix: String = path.toString.stripPrefix("/")
      if prefix.isEmpty then pictures
      else pictures.filter(picture => picture.path == prefix || picture.path.startsWith(s"$prefix/"))
