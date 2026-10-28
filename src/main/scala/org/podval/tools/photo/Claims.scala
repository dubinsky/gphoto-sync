package org.podval.tools.photo

import java.io.File
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, YearMonth, ZoneId, ZoneOffset}

object Claims:
  final case class Resolved(
    path: String,
    dated: Option[String],
    curated: Seq[String],
    excludedOnly: Boolean
  ):
    def line: String =
      val datedText: String = dated.getOrElse("-")
      val curatedText: String = if curated.isEmpty then "-" else curated.mkString(",")
      s"$path dated=$datedText curated=$curatedText"

  def apply(originals: File, zone: ZoneId): Either[String, Seq[Resolved]] =
    resolve(Archive.scan(originals), zone)

  private[photo] def resolve(scan: Archive.Scan, zone: ZoneId): Either[String, Seq[Resolved]] =
    val loaded: Loaded = index(scan, zone)
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
  )

  private val empty: Loaded = Loaded(Seq.empty, Map.empty, Map.empty, Seq.empty, Seq.empty)

  private def index(scan: Archive.Scan, zone: ZoneId): Loaded =
    rejectDuplicateMonths(scan.albums)
    val albums: Loaded = scan.albums.foldLeft(empty): (loaded, item) =>
      item.album match
        case event: Album.Event =>
          loaded.copy(events = loaded.events :+ event)
        case day: Album.Day =>
          loaded.copy(days = loaded.days.updated(dayKey(day.title), day))
        case month: Album.Month =>
          loaded.copy(months = loaded.months.updated(monthKey(month.title), month))
        case curated: Album.Curated =>
          loaded.copy(curated = loaded.curated :+ curated)
    val pictures: Seq[Found] = scan.pictures.map: picture =>
      Found(
        path = picture.path,
        name = picture.name,
        folderDay = picture.folderDay,
        yearMonth = picture.yearMonth,
        time = pictureTime(picture.name, picture.metadata, picture.folderDay, zone)
      )
    albums.copy(pictures = pictures)

  private def dayKey(title: String): LocalDate = title.split('/').filter(_.nonEmpty).toSeq match
    case Seq(year, month, day) => LocalDate.of(year.toInt, month.toInt, day.toInt)
    case _ => throw IllegalArgumentException(s"not a day album title: $title")

  private def monthKey(title: String): YearMonth =
    Album.monthYearMonth(title).getOrElse(throw IllegalArgumentException(s"not a month album title: $title"))

  private def rejectDuplicateMonths(albums: Seq[Album.Loaded]): Unit =
    val dupes: Seq[(YearMonth, Seq[Album.Loaded])] = albums
      .filter(_.album.isInstanceOf[Album.Month])
      .groupBy(loaded => monthKey(loaded.album.title))
      .toSeq
      .filter(_._2.sizeIs > 1)
      .sortBy(_._1.toString)
    if dupes.nonEmpty then
      val text: String = dupes.map: (key, loaded) =>
        val paths: String = loaded.map(_.file.getPath).sorted.mkString(" and ")
        s"more than one month album for $key: $paths"
      .mkString("\n")
      throw IllegalArgumentException(text)

  // timestamp wins over the filename. metadata.date is not a membership input; NameShort gets the folder day.
  private def pictureTime(name: String, metadata: Metadata, folderDay: LocalDate, zone: ZoneId): LocalDateTime =
    metadata.timestamp
      .map(LocalDateTime.parse)
      .orElse(timeFromName(name, zone, folderDay))
      .getOrElse(folderDay.atStartOfDay)

  // millis is an epoch instant. The other filename forms store the civil clock as a UTC instant.
  private def timeFromName(name: String, zone: ZoneId, folderDay: LocalDate): Option[LocalDateTime] =
    Dates.fromFileName(name, zone, Some(folderDay)).map:
      case (instant, "millis") => instant.atZone(zone).toLocalDateTime
      case (instant, _) => civil(instant)

  // NameFull, NameCompact, and NameShort store the civil clock as a UTC instant.
  private def civil(instant: Instant): LocalDateTime = LocalDateTime.ofInstant(instant, ZoneOffset.UTC)

  // Conflict uses events that still claim after exclude. A match that excludes is not a claim.
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

  // Exclude follow-up uses the claim interval, not only the event file's path day.
  private[photo] def folderDayInInterval(event: Album.Event, day: LocalDate): Boolean =
    val (start, end) = interval(event)
    !day.isBefore(start.toLocalDate) && !day.isAfter(end.toLocalDate)

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
    val eventHits: Seq[Album.Event] = loaded.events.filter: event =>
      inInterval(event, picture.time) && inNameBounds(picture.name, event)
    val dayHit: Option[Album.Day] = loaded.days.get(picture.folderDay)
    val monthHit: Option[Album.Month] = loaded.months.get(picture.yearMonth)
    val curatedHits: Seq[Album.Curated] = loaded.curated.filter(_.members.contains(picture.path))
    val dated: Option[String] = eventHits.find(event => !event.exclude.contains(picture.name)).map(_.title)
      .orElse(kept(dayHit, picture.name))
      .orElse(kept(monthHit, picture.name))
    val curated: Seq[String] = curatedHits
      .filterNot(_.exclude.contains(picture.name))
      .map(_.title)
      .sorted
    val matched: Boolean = eventHits.nonEmpty || dayHit.nonEmpty || monthHit.nonEmpty || curatedHits.nonEmpty
    Resolved(picture.path, dated, curated, excludedOnly = matched && dated.isEmpty && curated.isEmpty)

  private def kept(album: Option[Album], name: String): Option[String] =
    album.filterNot(_.exclude.contains(name)).map(_.title)

final case class ListAlbums(path: Option[Path]) extends Operation:
  override def execute(): Unit =
    val root: Root = path.fold(Root.get)(_.root)
    val prefix: Option[String] = path.map(_.toString.stripPrefix("/")).filter(_.nonEmpty)
    val scan: Archive.Scan = Archive.scan(root.configuration.byYearDirectory)
    val pictures: Seq[Claims.Resolved] = Claims.resolve(scan, root.configuration.zoneId) match
      case Left(error) =>
        println(error)
        throw IllegalArgumentException(error)
      case Right(value) => value

    annotations(scan.albums, prefix).foreach(println)
    pictures.filter(picture => Album.matchesPrefix(picture.path, prefix)).foreach(picture => println(picture.line))

  private def annotations(albums: Seq[Album.Loaded], prefix: Option[String]): Seq[String] =
    albums
      .map(_.album)
      .filter(album => Album.matchesPrefix(album.title, prefix))
      .filter(showAnnotation)
      .sortBy(_.title)
      .map(annotation)

  private def showAnnotation(album: Album): Boolean =
    album.owner.nonEmpty || (album.googleTitle.nonEmpty && album.googleTitle != album.title)

  private def annotation(album: Album): String =
    val google: String =
      if album.googleTitle.isEmpty then ""
      else s" google=${Album.quote(album.googleTitle)}"
    val ownerText: String = album.owner.fold(""): name =>
      s" owner=${Album.quote(name)}"
    s"album ${Album.quote(album.title)}$google$ownerText"
