package org.podval.tools.photo

import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.Metadata
import com.drew.metadata.exif.ExifSubIFDDirectory
import scala.sys.process.{Process, ProcessBuilder}
import java.io.File
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZoneId, ZoneOffset, ZonedDateTime}
import java.util.Date

final class Extension(val picture: Picture, val extension: String):
  private def file: File = picture.file(extension)

  private lazy val current: Instant = Extension.fromZone(Files.getLastModifiedTime(file))

  private lazy val fromMetadata: Option[Instant] =
    Extension.extension2tool.getOrElse(extension, Extension.metadataExtractor)(file)

  private lazy val fromName: Option[Instant] =
    val string = Util.dropSuffix(picture.name, "_1")

    Seq[String => Instant](
      name => LocalDateTime
        .parse(name, Extension.compactFormat)
        .toInstant(Util.UTC),
      name => LocalDateTime
        .parse(name, Extension.dashedFormat)
        .toInstant(Util.UTC),
      name => LocalDate
        .of(picture.day.month.year.number, picture.day.month.number, picture.day.number)
        .atTime(LocalTime.parse(name, Extension.shortFormat))
        .toInstant(Util.UTC)
    )
      .flatMap(parser =>
        try Some(parser(string)) catch case e: DateTimeParseException => None
      )
      .headOption

  // Note: in the pictures saw where metadata timestamp is present and file name also encodes the timestamp:
  // - for '3gp' and 'mp4' metadata timestamp is sometimes later than the file name timestamp
  //   (and a few hours for the timezone offset) - metadata probably records timestamp of the end of recording;
  // - for 'jpg' metadata timestamp is sometimes earlier than the file name timestamp by a second;
  // I pick the earliest ;)
  def original: Option[Instant] = Extension.earliest(Set(fromMetadata, fromName))

  private var toSet: Option[Instant] = None

  def isSetting: Boolean = toSet.isDefined

  def maybeSet(shouldBe: Instant): Unit =
    toSet = Option.when(!Extension.equal(shouldBe, current))(shouldBe)

  def set(dryRun: Boolean): Unit = toSet.foreach: shouldBe =>
    val toSet: Instant = Extension.toZone(shouldBe)
    Util.execute(
      dryRun,
      s"$picture.$extension: changing timestamp $current to $toSet",
      Files.setLastModifiedTime(picture.file(extension), toSet)
    )

  override def toString: String =
    def optional(value: Option[Instant], name: String) = value.fold("")(value => s" $name=$value")
    s"  .$extension${optional(fromMetadata, "fromMetadata")}${optional(fromName, "fromName")} current=$current${optional(toSet, "toSet")}"

object Extension:
  private val compactFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss") // 20120812_013216
  private val dashedFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-M-d-H-m-s") // 2010-09-14-19-49-2
  private val shortFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("HHmmss") // 013216

  private val prefixFfprobe: String = "TAG:creation_time="
  private val formatFfprobe: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.nnnnnn'Z'") // 2010-07-06T20:50:02.000000Z
  private val formatFfprobeAvi: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd' 'HH:mm:ss") // 2003-11-30 19:49:59

  private def ffprobe(
    format: DateTimeFormatter = formatFfprobe,
    // correct for single 'mov' and 'mp4' files with no thumbnails to bring the timestamp back;
    // TODO from neighbourly-numbered pictures it is clear that sometimes 'mov' metadata timestamp
    // is zoned, but sometimes it is not :(
    // - or maybe I should treat timestamps from the 'jpg' metadata as zoned too?!
    isZoned: Boolean = false
  )(file: File): Option[Instant] = Util.grep(
    Process(s"ffprobe -v quiet $file -show_entries stream=index,codec_type:stream_tags=creation_time:format_tags=creation_time"),
    prefixFfprobe
  ).map: line =>
    val result: Instant = LocalDateTime.parse(line, format).toInstant(Util.UTC)
    if !isZoned then result else fromZone(result)

  private val prefixDcraw: String = "Timestamp:"
  private val formatDcraw: DateTimeFormatter = DateTimeFormatter.ofPattern("EEE MMM dd HH:mm:ss yyyy") // Wed Nov  7 03:53:52 2001

  private def dcraw(file: File): Option[Instant] =
    dcrawOrRawIdentify(Process(s"dcraw -i -v $file").#||(Process("true")))

  private def rawIdentify(file: File): Option[Instant] =
    dcrawOrRawIdentify(Process(s"raw-identify -v $file"))

  private def dcrawOrRawIdentify(process: ProcessBuilder): Option[Instant] =
    Util.grep(process, prefixDcraw).map: line =>
      LocalDateTime.parse(if line(8) == ' ' then line.updated(8, '0') else line, formatDcraw).toInstant(Util.UTC) // pad day of the month

  private def metadataExtractor(file: File): Option[Instant] =
    val metadata: Metadata = ImageMetadataReader.readMetadata(file)
    for
      exif: ExifSubIFDDirectory <- Option(metadata.getFirstDirectoryOfType(classOf[ExifSubIFDDirectory]))
      result: Date <- Option(exif.getDateOriginal)
    yield
      result.toInstant

  private val extension2tool: Map[String, File => Option[Instant]] = Map(
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
    "crw" -> dcraw,

    // Note: metadata-extractor *still* does not support 'cr3' files!
    // see ttps://github.com/drewnoakes/metadata-extractor/issues/374
    "cr3" -> dcraw,

    // TODO for 'cr2, metadata-extractor provides millisecond precision...

    // metadata-extractor does not support 'mov';
    // dcraw on *some* mov files errors out with "Cannot decode".

    "mov" -> ffprobe(isZoned = true),
    "mp4" -> ffprobe(isZoned = true),
    "avi" -> ffprobe(format = formatFfprobeAvi),
    "3gp" -> ffprobe()
  )

  // TODO get out of configuration
  private val zone: ZoneId = ZoneId.of("America/New_York") // TimeZone = TimeZone.getTimeZone(NY)

  private def fromZone(instant: Instant): Instant = instant.atZone(zone).withZoneSameLocal(Util.UTC).toInstant

  private def toZone(instant: Instant): Instant = instant.atZone(Util.UTC).withZoneSameLocal(zone).toInstant

  private def equal(left: Instant, right: Instant): Boolean =
    Math.abs(left.toEpochMilli - right.toEpochMilli) == 0

  def earliest(options: Set[Option[Instant]]): Option[Instant] =
    val instants: Set[Instant] = options.flatten
    Option.when(instants.nonEmpty)(instants.min)

  // some photos were taken with no time set on the camera...
  def isDefaultDate(date: ZonedDateTime): Boolean =
    (date.getYear == 2000 && date.getMonthValue == 1 && date.getDayOfMonth == 1) ||
    (date.getYear == 1980 && date.getMonthValue == 1 && date.getDayOfMonth == 1)
