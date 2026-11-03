package org.podval.tools.photo

import com.drew.imaging.{ImageMetadataReader, ImageProcessingException}
import com.drew.metadata.Metadata
import com.drew.metadata.exif.{ExifDirectoryBase, ExifSubIFDDirectory}
import com.drew.metadata.mov.metadata.QuickTimeMetadataDirectory
import scala.sys.process.Process
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
  ZoneOffset
}
import java.time.temporal.ChronoUnit

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

// metadata-extractor 2.22.0 reads CR3 (Cr3MetadataReader) and stills through ExifSubIFDDirectory.
// CRW is absent from that switch, so it stays on dcraw.
// RWL is the Leica suffix; that TIFF arm reads it when the bytes are TIFF. RW2 stays on the same row.
// Do not add a second reader for them.
object Dates:
  // Raw mtime. The caller stores civil.atZone(zone); this does not convert.
  object FromFile:
    def get(picture: Picture, extension: String): Instant =
      instant(picture.file(extension))

    private[photo] def instant(file: File): Instant =
      Files.getLastModifiedTime(file.toPath).toInstant

    def set(picture: Picture, extension: String, instant: Instant): Unit =
      Files.setLastModifiedTime(picture.file(extension).toPath, FileTime.from(instant))

  sealed abstract class Reader:
    def read(extension: String, file: File, filename: Option[Clock], zone: ZoneId): Option[Clock]

  object StillExif extends Reader:
    def read(extension: String, file: File, filename: Option[Clock], zone: ZoneId): Option[Clock] =
      readStill(file)

  object DCRaw extends Reader:
    def read(extension: String, file: File, filename: Option[Clock], zone: ZoneId): Option[Clock] =
      readCrw(file)

  // Library creationdate, then ffprobe. A library failure does not abort.
  object Mov extends Reader:
    def read(extension: String, file: File, filename: Option[Clock], zone: ZoneId): Option[Clock] =
      readVideo("mov", file, filename, zone)

  // mp4, m4v, and 3gp. Mp4MetadataReader never builds QuickTimeMetadataDirectory.
  object FFProbe extends Reader:
    def read(extension: String, file: File, filename: Option[Clock], zone: ZoneId): Option[Clock] =
      readVideo(extension, file, filename, zone)

  object FFProbeAVI extends Reader:
    def read(extension: String, file: File, filename: Option[Clock], zone: ZoneId): Option[Clock] =
      readVideo("avi", file, filename, zone)

  // One trailing v, then one trailing _1. The stored stem is not changed. Callers choose the date a short name may use.
  def fromFileName(name: String, zone: ZoneId, shortDate: Option[LocalDate]): Option[(Clock, String)] =
    val withoutV: String = if name.endsWith("v") then name.dropRight(1) else name
    val text: String = if withoutV.endsWith("_1") then withoutV.dropRight("_1".length) else withoutV
    def parse(format: DateTimeFormatter, source: Source, kind: String): Option[(Clock, String)] =
      try Some(Clock(LocalDateTime.parse(text, format), None, source) -> kind)
      catch case _: DateTimeParseException => None
    parse(DateTimeFormatter.ofPattern("yyyy-M-d-H-m-s"), Source.FilenameFull, "full")
      .orElse(parse(DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss"), Source.FilenameCompact, "compact"))
      .orElse:
        if text.length != 13 || text.exists(char => !char.isDigit) then None
        else
          try Some(millisClock(text.toLong, zone) -> "millis")
          catch case _: NumberFormatException => None
      .orElse(shortDate.flatMap: date =>
        val parsed: Option[(Clock, String)] =
          try
            Some(Clock(
              date.atTime(LocalTime.parse(text, DateTimeFormatter.ofPattern("HHmmss"))),
              None,
              Source.FilenameShort
            ) -> "short")
          catch case _: DateTimeParseException => None
        parsed
      )

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

  final case class Assessment(
    clock: Option[Clock],
    timestampsVary: Boolean,
    timestampWrong: Boolean,
    dayShouldBe: Option[Day],
    disagreement: Option[Ranked.Disagree]
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
      case disagree: Ranked.Disagree =>
        Assessment(
          clock = None,
          timestampsVary = timestampsVary,
          timestampWrong = false,
          dayShouldBe = None,
          disagreement = Some(disagree)
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

  def embeddedClock(picture: Picture, filename: Option[Clock]): Option[Clock] =
    captureDescriptor(picture).flatMap: descriptor =>
      usable(descriptor.dater.read(
        descriptor.name,
        picture.file(descriptor.name),
        usable(filename),
        picture.zone
      ))

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
    usable(apple).orElse(usable(fromCreation))

  // mov, mp4, m4v, and 3gp. A mov library failure is no clock, then ffprobe, same as a missing ffprobe.
  def readVideo(extension: String, file: File, filename: Option[Clock], zone: ZoneId): Option[Clock] =
    val read: Option[Clock] = extension match
      case "mov" =>
        usable(readMovLibrary(file)).orElse(probeClock(file, filename, zone))
      case "mp4" | "m4v" | "3gp" =>
        probeClock(file, filename, zone)
      case "avi" =>
        readAvi(file)
      case _ =>
        None
    usable(read)

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

  def readCrw(file: File): Option[Clock] =
    val format: DateTimeFormatter = DateTimeFormatter.ofPattern("EEE MMM dd HH:mm:ss yyyy")
    val prefix: String = "Timestamp:"
    try
      Process(s"dcraw -i -v $file").#||(Process("true")).!!.split('\n').find(_.startsWith(prefix)).map: line =>
        val text: String = line.drop(prefix.length).trim
        val (head, tail) = text.splitAt(8)
        val padded: String = if tail.startsWith(" ") then head + "0" + tail.drop(1) else text
        Clock(LocalDateTime.parse(padded, format), None, Source.CrwDcraw)
    catch
      case _: RuntimeException => None

  def sameSecond(civil: LocalDateTime, stored: Instant, zone: ZoneId): Boolean =
    civil.truncatedTo(ChronoUnit.SECONDS).isEqual(
      stored.atZone(zone).toLocalDateTime.truncatedTo(ChronoUnit.SECONDS)
    )
