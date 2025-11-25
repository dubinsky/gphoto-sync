package org.podval.tools.photo

import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.Metadata
import com.drew.metadata.exif.ExifSubIFDDirectory
import scala.sys.process.{Process, ProcessBuilder}
import java.io.File
import java.nio.file.Files
import java.nio.file.attribute.FileTime
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZoneId, ZoneOffset, ZonedDateTime}
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

// Note: metadata-extractor *still* does not support 'cr3' files!
// see ttps://github.com/drewnoakes/metadata-extractor/issues/374

// metadata-extractor does not support 'mov';
// dcraw on *some* mov files errors out with "Cannot decode".
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

    def get(name: String, zone: ZoneId, day: Day): Option[Instant] =
      try
        val localTime: LocalTime = LocalTime.parse(name, format)
        val localDateTime: LocalDateTime = LocalDate.of(day.parent.parent.number, day.parent.number, day.number).atTime(localTime)
        Some(toInstant(zone)(localDateTime))
      catch
        case _: DateTimeParseException => None

  object FromMetadata extends Dater(isZoned = false):
    def get(picture: Picture): Option[Instant] = Option.when(picture.hasMetadata)(picture.metadata)
      .flatMap(_.timestamp)
      .map(LocalDateTime.parse)
      .map(toInstant(picture.zone))

  object FromFile extends Dater(isZoned = true):
    def get(picture: Picture, extension: String): Instant =
      val file: File = picture.file(extension)
      val instant: Instant =
        //Instant.ofEpochMilli(file.lastModified)
        Files.getLastModifiedTime(file.toPath).toInstant

      // TODO isZoned
      fromZone(instant, picture.zone)

    def set(picture: Picture, extension: String, instant: Instant): Unit =
      val file: File = picture.file(extension)
      // TODO isZoned
      val toSet: Instant = toZone(instant, picture.zone)
      //file.setLastModified(getMillis(toSet))
      Files.setLastModifiedTime(file.toPath, FileTime.from(toSet))
