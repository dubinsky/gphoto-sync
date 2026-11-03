package org.podval.tools.photo

import com.drew.imaging.{ImageMetadataReader, ImageProcessingException}
import com.drew.metadata.Metadata
import com.drew.metadata.exif.{ExifDirectoryBase, ExifSubIFDDirectory}
import com.drew.metadata.mov.metadata.QuickTimeMetadataDirectory
import scala.sys.process.{Process, ProcessBuilder}
import java.io.{File, IOException}
import java.nio.file.Files
import java.nio.file.attribute.FileTime
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{
  DateTimeException,
  Instant,
  LocalDate,
  LocalDateTime,
  LocalTime,
  OffsetDateTime,
  ZoneId,
  ZoneOffset,
  ZonedDateTime
}
import java.time.temporal.ChronoUnit

// correct for single 'mov' and 'mp4' files with no thumbnails to bring the timestamp back;
// TODO from neighbourly-numbered pictures it is clear that sometimes 'mov' metadata timestamp
// is zoned, but sometimes it is not :(
// - or maybe I should treat timestamps from the 'jpg' metadata as zoned too?!

// metadata-extractor *code* suggests that old Canon raw format CRW *is* supported:
// - in https://github.com/drewnoakes/metadata-extractor/blob/main/Source/com/drew/imaging/tiff/TiffMetadataReader.java
//   "TIFF files include many digital camera RAW formats, including Canon (CRW, CR2)"
// - in https://github.com/drewnoakes/metadata-extractor/blob/main/Samples/com/drew/metadata/SampleUsage.java
//   "This will handle JPEG, TIFF, GIF, BMP and RAW (CRW/CR2/NEF/RW2/ORF)"
// metadata-extractor *issue* https://github.com/drewnoakes/metadata-extractor/issues/2
// "Support Canon CRW camera RAW format"
// and referenced code changes suggest that CRW is *not* supported:
// - "As CRW files are not TIFF, they should not be handled by TiffReader"
// TODO suggest clarification to code comments in metadata-extractor.

// TODO for 'cr2, metadata-extractor provides millisecond precision...

// metadata-extractor 2.22.0 reads CR3 (Cr3MetadataReader) and MOV (QuickTimeMetadataReader).
// HeifReader reads heic, heif, and avif, and ImageMetadataReader already calls it.
// This program still dates cr3 with dcraw, and mov, mp4, and m4v with ffprobe.
// Dates.MetadataExtractor is the dater for the stills, heic, heif, avif, png, webp, and the TIFF raw suffixes.
// RWL is the Leica suffix; that TIFF arm reads it when the bytes are TIFF. RW2 stays on the same row.
// Do not add a second reader for them.
object Dates:
  def equal(left: Instant, right: Instant): Boolean =
    Math.abs(left.toEpochMilli - right.toEpochMilli) == 0

  def earliest(options: Set[Option[Instant]]): Option[Instant] =
    val instants: Set[Instant] = options.flatten
    Option.when(instants.nonEmpty)(instants.min)

  def latest(options: Set[Option[Instant]]): Option[Instant] =
    val instants: Set[Instant] = options.flatten
    Option.when(instants.nonEmpty)(instants.max)

  def distanceInMinutes(from: Instant, to: Instant): Long =
    Math.abs(ChronoUnit.MINUTES.between(from, to))
  
  private def UTC: ZoneOffset = ZoneOffset.UTC

  private def fromZone(instant: Instant, zone: ZoneId): Instant = instant.atZone(zone).withZoneSameLocal(UTC).toInstant

  private def toZone(instant: Instant, zone: ZoneId): Instant = instant.atZone(UTC).withZoneSameLocal(zone).toInstant

  def toZonedDateTime(instant: Instant): ZonedDateTime = instant.atZone(UTC)

  sealed abstract class Dater(val isZoned: Boolean):
    def toInstant(zone: ZoneId)(localDateTime: LocalDateTime): Instant =
      val result: Instant = localDateTime.toInstant(UTC)
      if !isZoned then result else fromZone(result, zone)
  // TODO this probably should be:
  //    val zoneOffset: ZoneOffset =
  //      if !isZoned
  //      then UTC
  //      else zone.getRules.getOffset(localDateTime)
  //
  //    localDateTime.toInstant(zoneOffset)

  sealed abstract class FromFileMetadata(isZoned: Boolean) extends Dater(isZoned):
    final def get(picture: Picture, extension: String): Option[Instant] =
      get(picture.file(extension), picture.zone)
      
    def get(file: File, zone: ZoneId): Option[Instant]

  sealed abstract class External(isZoned: Boolean) extends FromFileMetadata(isZoned):
    protected def process(file: File): ProcessBuilder
    protected def lineStart: String
    protected def lineEffective(line: String): String = line
    protected def format: DateTimeFormatter

    final override def get(file: File, zone: ZoneId): Option[Instant] =
      try process(file)
        .!!
        .split('\n')
        .find(_.startsWith(lineStart))
        .map(_.substring(lineStart.length))
        .map(_.trim)
        .map(lineEffective)
        .map(LocalDateTime.parse(_, format))
        .map(toInstant(zone))
      catch
        case _: RuntimeException => None

  sealed class FFProbe(isZoned: Boolean) extends External(isZoned):
    final override protected def process(file: File): ProcessBuilder =
      Process(s"ffprobe -v quiet $file -show_entries stream=index,codec_type:stream_tags=creation_time:format_tags=creation_time")
    final override protected def lineStart: String =
      "TAG:creation_time="
    override protected def format: DateTimeFormatter =
      DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.nnnnnn'Z'") // 2010-07-06T20:50:02.000000Z

  object FFProbeZoned extends FFProbe(isZoned = true)
  object FFProbeUTC extends FFProbe(isZoned = false)
  object FFProbeAVI extends FFProbe(isZoned = false):
    override protected def format: DateTimeFormatter =
      DateTimeFormatter.ofPattern("yyyy-MM-dd' 'HH:mm:ss") // 2003-11-30 19:49:59

  sealed abstract class DCRrawOrRawIdentify(isZoned: Boolean) extends External(isZoned):
    final override protected def lineStart: String =
      "Timestamp:"
    final override protected def lineEffective(line: String): String =
      if line(8) == ' ' then line.updated(8, '0') else line  // pad day of the month
    final override protected def format: DateTimeFormatter =
      DateTimeFormatter.ofPattern("EEE MMM dd HH:mm:ss yyyy") // Wed Nov  7 03:53:52 2001

  object DCRaw extends DCRrawOrRawIdentify(isZoned = false):
    override protected def process(file: File): ProcessBuilder = Process(s"dcraw -i -v $file").#||(Process("true"))

  object RawIdentify extends DCRrawOrRawIdentify(isZoned = false):
    override protected def process(file: File): ProcessBuilder = Process(s"raw-identify -v $file")

  object MetadataExtractor extends FromFileMetadata(isZoned = false):
    final override def get(file: File, zone: ZoneId): Option[Instant] =
      val metadata: Metadata = ImageMetadataReader.readMetadata(file)
      for
        exif: ExifSubIFDDirectory <- Option(metadata.getFirstDirectoryOfType(classOf[ExifSubIFDDirectory]))
        instant: Instant <- Option(exif.getDateOriginal).map(_.toInstant)
      yield
        if !isZoned then instant else toZone(instant, zone)

  sealed abstract class FromLocalDateTimeName(isZoned: Boolean) extends Dater(isZoned):
    protected def format: DateTimeFormatter

    final def get(name: String, zone: ZoneId): Option[Instant] =
      try
        val localDateTime: LocalDateTime = LocalDateTime.parse(name, format)
        Some(toInstant(zone)(localDateTime))
      catch
        case _: DateTimeParseException => None

  object NameFull extends FromLocalDateTimeName(isZoned = false):
    override protected def format: DateTimeFormatter =
      DateTimeFormatter.ofPattern("yyyy-M-d-H-m-s") // 2010-09-14-19-49-2

  object NameCompact extends FromLocalDateTimeName(isZoned = false):
    override protected def format: DateTimeFormatter =
      DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss") // 20120812_013216

  object NameMillis extends Dater(isZoned = false):
    def get(name: String, zone: ZoneId): Option[Instant] =
      if name.length != 13 || name.exists(char => !char.isDigit) then None else
        try
          Some(Instant.ofEpochMilli(name.toLong))
          // TODO do zoned conversion
        catch
          case _: NumberFormatException => None

  object NameShort extends Dater(isZoned = false):
    private val format: DateTimeFormatter = DateTimeFormatter.ofPattern("HHmmss") // 013216

    def get(name: String, zone: ZoneId, date: LocalDate): Option[Instant] =
      try
        Some(toInstant(zone)(date.atTime(LocalTime.parse(name, format))))
      catch
        case _: DateTimeParseException => None

  // One trailing v, then one trailing _1. The stored stem is not changed. Callers choose the date NameShort may use.
  def fromFileName(name: String, zone: ZoneId, shortDate: Option[LocalDate]): Option[(Instant, String)] =
    val withoutV: String = if name.endsWith("v") then name.dropRight(1) else name
    val clock: String = if withoutV.endsWith("_1") then withoutV.dropRight("_1".length) else withoutV
    NameFull.get(clock, zone).map(_ -> "full")
      .orElse(NameCompact.get(clock, zone).map(_ -> "compact"))
      .orElse(NameMillis.get(clock, zone).map(_ -> "millis"))
      .orElse(shortDate.flatMap(date => NameShort.get(clock, zone, date).map(_ -> "short")))

  object FromMetadata extends Dater(isZoned = false):
    def get(picture: Picture): Option[Instant] =
      Option.when(picture.hasMetadata)(picture.metadata).flatMap(timestamp(_, picture.zone))

    // Same parse as get, from a sidecar already read. Does not open a file.
    private[photo] def timestamp(metadata: org.podval.tools.photo.Metadata, zone: ZoneId): Option[Instant] =
      metadata.timestamp.map(LocalDateTime.parse).map(toInstant(zone))

  object FromFile extends Dater(isZoned = true):
    def get(picture: Picture, extension: String): Instant =
      instant(picture.file(extension), picture.zone)

    private[photo] def instant(file: File, zone: ZoneId): Instant =
      val read: Instant =
        //Instant.ofEpochMilli(file.lastModified)
        Files.getLastModifiedTime(file.toPath).toInstant

      // TODO isZoned
      fromZone(read, zone)

    def set(picture: Picture, extension: String, instant: Instant): Unit =
      val file: File = picture.file(extension)
      // TODO isZoned
      val toSet: Instant = toZone(instant, picture.zone)
      //file.setLastModified(getMillis(toSet))
      Files.setLastModifiedTime(file.toPath, FileTime.from(toSet))

  val varyMinutes: Long = 60

  enum Source derives CanEqual:
    case SidecarTimestamp
    case SidecarDate
    case DateTimeOriginal
    case CreateDate
    case CrwDcraw
    case VideoCreationDate
    case VideoRealZ
    case VideoLocalZ
    case VideoAvi
    case FilenameFull
    case FilenameCompact
    case FilenameMillis
    case FilenameShort
    case FolderDay

  final case class Clock(
    civil: LocalDateTime,
    offset: Option[ZoneOffset],
    source: Source
  ) derives CanEqual:
    def day: LocalDate = civil.toLocalDate

  final case class Observed(
    extension: String,
    votes: Boolean,
    clock: Option[Clock]
  ) derives CanEqual

  final case class Disagreement(timestamp: LocalDateTime, date: LocalDate) derives CanEqual

  final case class Assessment(
    clock: Option[Clock],
    timestampsVary: Boolean,
    timestampWrong: Boolean,
    dayShouldBe: Option[Day],
    disagreement: Option[Disagreement]
  ) derives CanEqual:
    // Folder midnight is not a capture time, and a disagreement has no winner to stamp.
    def writesMtime: Option[Clock] =
      if disagreement.isDefined then None
      else clock.filter(_.source != Source.FolderDay)

  def isSentinel(day: LocalDate): Boolean =
    day.getMonthValue == 1 && day.getDayOfMonth == 1 &&
      (day.getYear == 1980 || day.getYear == 2000)

  def usable(clock: Option[Clock]): Option[Clock] =
    clock.filterNot(item => isSentinel(item.day))

  def minutesApart(left: LocalDateTime, right: LocalDateTime): Long =
    Math.abs(ChronoUnit.MINUTES.between(left, right))

  final case class RankInput(
    timestamp: Option[LocalDateTime],
    date: Option[LocalDate],
    embedded: Option[Clock],
    filename: Option[Clock],
    folder: Option[Clock]
  ) derives CanEqual

  enum Ranked derives CanEqual:
    case Disagree(timestamp: LocalDateTime, date: LocalDate)
    case Chosen(clock: Clock)
    case Undated

  // embedded and filename are already passed through usable. A sentinel never arrives there.
  def rank(input: RankInput): Ranked =
    (input.timestamp, input.date) match
      case (Some(timestamp), Some(date)) if !timestamp.toLocalDate.isEqual(date) =>
        Ranked.Disagree(timestamp, date)
      case (Some(timestamp), _) =>
        Ranked.Chosen(Clock(timestamp, None, Source.SidecarTimestamp))
      case (None, Some(date)) =>
        val timeOfDay: LocalTime =
          input.embedded.orElse(input.filename)
            .map(_.civil.toLocalTime)
            .getOrElse(LocalTime.MIDNIGHT)
        Ranked.Chosen(Clock(date.atTime(timeOfDay), None, Source.SidecarDate))
      case (None, None) =>
        input.embedded.orElse(input.filename).orElse(input.folder) match
          case Some(clock) => Ranked.Chosen(clock)
          case None => Ranked.Undated

  def dayShouldBe(picture: Picture, ranked: Ranked): Option[Day] =
    ranked match
      case Ranked.Disagree(_, _) => None
      case Ranked.Undated => None
      case Ranked.Chosen(clock) =>
        val date: LocalDate = clock.day
        val already: Boolean = picture.parent.day.exists(_.date.isEqual(date))
        Option.when(!already)(
          picture.root.year(date.getYear).month(date.getMonthValue).day(date.getDayOfMonth)
        )

  // Disagree and Undated store no clock and do not stamp, even when a caller passes raw mtimes.
  def assessment(
    picture: Picture,
    ranked: Ranked,
    timestampsVary: Boolean,
    currents: Iterable[Instant]
  ): Assessment =
    val moveTo: Option[Day] = dayShouldBe(picture, ranked)
    ranked match
      case Ranked.Disagree(timestamp, date) =>
        Assessment(
          clock = None,
          timestampsVary = timestampsVary,
          timestampWrong = false,
          dayShouldBe = None,
          disagreement = Some(Disagreement(timestamp, date))
        )
      case Ranked.Undated =>
        Assessment(None, timestampsVary, timestampWrong = false, dayShouldBe = None, disagreement = None)
      case Ranked.Chosen(clock) =>
        val writes: Option[Clock] = Option.when(clock.source != Source.FolderDay)(clock)
        val wrong: Boolean = writes.exists: chosen =>
          currents.exists(stored => !sameSecond(chosen.civil, stored, picture.zone))
        Assessment(Some(clock), timestampsVary, wrong, moveTo, disagreement = None)

  def captureDescriptor(picture: Picture): Option[Extension.Descriptor] =
    val still: Option[Extension.Descriptor] =
      picture.normal.filterNot(Extension.video.contains)
    still
      .orElse(picture.image)
      .orElse(picture.raw)
      .orElse(picture.normal.filter(Extension.video.contains))

  // dcraw still returns an instant, so CRW has no civil clock on this path.
  def embeddedClock(picture: Picture, filename: Option[Clock]): Option[Clock] =
    captureDescriptor(picture).flatMap: descriptor =>
      val file: File = picture.file(descriptor.name)
      val read: Option[Clock] =
        if descriptor eq Extension.CRW then None
        else if Extension.video.contains(descriptor) then
          readVideo(descriptor.name, file, filename, picture.zone)
        else readStill(file)
      usable(read)

  final case class ExifWhen(when: Int, sub: Int, offset: Int, source: Source)

  val dateTimeOriginal: ExifWhen =
    ExifWhen(
      ExifDirectoryBase.TAG_DATETIME_ORIGINAL,
      ExifDirectoryBase.TAG_SUBSECOND_TIME_ORIGINAL,
      ExifDirectoryBase.TAG_TIME_ZONE_ORIGINAL,
      Source.DateTimeOriginal
    )

  val createDate: ExifWhen =
    ExifWhen(
      ExifDirectoryBase.TAG_DATETIME_DIGITIZED,
      ExifDirectoryBase.TAG_SUBSECOND_TIME_DIGITIZED,
      ExifDirectoryBase.TAG_TIME_ZONE_DIGITIZED,
      Source.CreateDate
    )

  private val exifDateTime: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy:MM:dd HH:mm:ss")

  def parseExifClock(
    text: String,
    sub: Option[String],
    offsetText: Option[String],
    source: Source
  ): Option[Clock] =
    val trimmed: String = text.trim
    if trimmed.isEmpty || trimmed == "0000:00:00 00:00:00" then None
    else
      val parsed: Option[LocalDateTime] =
        try Some(LocalDateTime.parse(trimmed, exifDateTime))
        catch case _: DateTimeParseException => None
      parsed.map: civil =>
        val digits: String = sub.getOrElse("").takeWhile(_.isDigit).take(9)
        val nanos: Int = if digits.isEmpty then 0 else (digits + ("0" * (9 - digits.length))).toInt
        val offset: Option[ZoneOffset] = offsetText.flatMap: raw =>
          if raw.matches("[+-]\\d{2}:\\d{2}") then
            try Some(ZoneOffset.of(raw))
            catch case _: DateTimeException => None
          else None
        Clock(civil.withNano(nanos), offset, source)

  def readStill(file: File): Option[Clock] =
    val metadata: Metadata = ImageMetadataReader.readMetadata(file)
    Option(metadata.getFirstDirectoryOfType(classOf[ExifSubIFDDirectory])).flatMap: dir =>
      val original: Option[Clock] = readWhen(dir, dateTimeOriginal)
      val chosen: Option[Clock] =
        if usable(original).isDefined then original else readWhen(dir, createDate)
      usable(chosen)

  def readWhen(dir: ExifSubIFDDirectory, tags: ExifWhen): Option[Clock] =
    Option(dir.getString(tags.when)).flatMap: text =>
      parseExifClock(text, Option(dir.getString(tags.sub)), Option(dir.getString(tags.offset)), tags.source)

  def millisClock(epochMilli: Long, zone: ZoneId): Clock =
    val instant: Instant = Instant.ofEpochMilli(epochMilli)
    Clock(
      civil = instant.atZone(zone).toLocalDateTime,
      offset = Some(zone.getRules.getOffset(instant)),
      source = Source.FilenameMillis
    )

  val ffprobeShowEntries: String =
    "stream=index,codec_type:stream_tags=creation_time:format_tags=creation_time,com.apple.quicktime.creationdate"

  def parseCreationDate(text: String): Option[OffsetDateTime] =
    parseOffset(text).orElse(parseOffset(colonizeHHmm(text)))

  def parseOffset(text: String): Option[OffsetDateTime] =
    try Some(OffsetDateTime.parse(text))
    catch case _: DateTimeParseException => None

  def colonizeHHmm(text: String): String =
    val (body, tail) = text.splitAt((text.length - 5).max(0))
    if tail.length == 5 && (tail.head == '+' || tail.head == '-') && tail.drop(1).forall(_.isDigit) then
      val (hh, mm) = tail.drop(1).splitAt(2)
      s"$body${tail.head}$hh:$mm"
    else text

  def chooseZ(
    digits: LocalDateTime,
    filename: Option[Clock],
    zone: ZoneId
  ): Clock =
    val instant: Instant = digits.toInstant(ZoneOffset.UTC)
    val shifted: LocalDateTime = instant.atZone(zone).toLocalDateTime
    val archiveOffset: ZoneOffset = zone.getRules.getOffset(instant)
    val name: Option[LocalDateTime] = filename.map(_.civil)
    name match
      case Some(civil) if minutesApart(shifted, civil) <= varyMinutes =>
        Clock(shifted, Some(archiveOffset), Source.VideoRealZ)
      case Some(civil) if minutesApart(digits, civil) <= varyMinutes =>
        Clock(digits, None, Source.VideoLocalZ)
      case _ =>
        Clock(shifted, Some(archiveOffset), Source.VideoRealZ)

  def parseVideoTags(stdout: String, filename: Option[Clock], zone: ZoneId): Option[Clock] =
    val lines: Seq[String] = stdout.split('\n').toSeq.map(_.trim).filter(_.nonEmpty)
    val applePrefix: String = "TAG:com.apple.quicktime.creationdate="
    val timePrefix: String = "TAG:creation_time="
    val apple: Option[Clock] =
      lines.find(_.startsWith(applePrefix)).map(_.drop(applePrefix.length)).flatMap: text =>
        parseCreationDate(text).map: parsed =>
          Clock(parsed.toLocalDateTime, Some(parsed.getOffset), Source.VideoCreationDate)
    val fromCreation: Option[Clock] =
      lines.find(line => line.startsWith(timePrefix) && line.endsWith("Z")).flatMap: line =>
        val digits: String = line.drop(timePrefix.length).dropRight(1)
        val parsed: Option[LocalDateTime] =
          try Some(LocalDateTime.parse(digits))
          catch case _: DateTimeParseException => None
        parsed.map(value => chooseZ(value, filename, zone))
    usable(apple).orElse(fromCreation).flatMap(clock => usable(Some(clock)))

  // mov, mp4, m4v, and 3gp. A mov library failure is no clock, then ffprobe, same as a missing ffprobe.
  def readVideo(extension: String, file: File, filename: Option[Clock], zone: ZoneId): Option[Clock] =
    val read: Option[Clock] = extension match
      case "mov" =>
        readMovLibrary(file).flatMap(clock => usable(Some(clock))).orElse(probeClock(file, filename, zone))
      case "mp4" | "m4v" | "3gp" =>
        probeClock(file, filename, zone)
      case "avi" =>
        readAvi(file)
      case _ =>
        None
    read.flatMap(clock => usable(Some(clock)))

  private def readMovLibrary(file: File): Option[Clock] =
    try
      val metadata: Metadata = ImageMetadataReader.readMetadata(file)
      Option(metadata.getFirstDirectoryOfType(classOf[QuickTimeMetadataDirectory]))
        .flatMap(dir => Option(dir.getString(QuickTimeMetadataDirectory.TAG_CREATION_DATE)))
        .flatMap(parseCreationDate)
        .map: parsed =>
          Clock(parsed.toLocalDateTime, Some(parsed.getOffset), Source.VideoCreationDate)
    catch
      case _: ImageProcessingException => None
      case _: IOException => None

  private def probeStdout(file: File): Option[String] =
    try Some(Process(s"ffprobe -v quiet $file -show_entries $ffprobeShowEntries").!!)
    catch case _: RuntimeException => None

  private def probeClock(file: File, filename: Option[Clock], zone: ZoneId): Option[Clock] =
    probeStdout(file).flatMap(text => parseVideoTags(text, filename, zone))

  private def readAvi(file: File): Option[Clock] =
    val format: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val prefix: String = "TAG:creation_time="
    probeStdout(file).flatMap: stdout =>
      stdout.split('\n').toSeq.map(_.trim).find(_.startsWith(prefix)).flatMap: line =>
        val text: String = line.drop(prefix.length)
        try Some(Clock(LocalDateTime.parse(text, format), None, Source.VideoAvi))
        catch case _: DateTimeParseException => None

  def writeInstant(civil: LocalDateTime, zone: ZoneId): Instant =
    civil.atZone(zone).toInstant

  def sameSecond(civil: LocalDateTime, stored: Instant, zone: ZoneId): Boolean =
    civil.truncatedTo(ChronoUnit.SECONDS).isEqual(
      stored.atZone(zone).toLocalDateTime.truncatedTo(ChronoUnit.SECONDS)
    )
