package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Files as JFiles
import java.time.{Instant, LocalDate, LocalDateTime, OffsetDateTime, ZoneId, ZoneOffset}
import java.time.temporal.ChronoUnit
import java.util.TimeZone
import javax.imageio.ImageIO

class DatesTest extends AnyFlatSpec, Matchers:
  @targetName("sourceCanEqual")
  private given [A <: Dates.Source, B <: Dates.Source] => CanEqual[A, B] = CanEqual.derived

  @targetName("rankedCanEqual")
  private given [A <: Dates.Ranked, B <: Dates.Ranked] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")

  "fromFileName" should "label each filename clock" in:
    labeled("2010-09-14-19-49-2", "2010-09-14T19:49:02") shouldBe "full"
    labeled("20120812_013216", "2012-08-12T01:32:16") shouldBe "compact"
    val millis: (Dates.Clock, String) = Dates.fromFileName("1300000000000", zone, None).getOrElse(fail("no clock"))
    millis._1 shouldBe Dates.millisClock(1300000000000L, zone)
    millis._2 shouldBe "millis"
    labeled("013216", "2012-08-12T01:32:16", Some(LocalDate.of(2012, 8, 12))) shouldBe "short"

  it should "strip one trailing _1" in:
    labeled("20120812_013216_1", "2012-08-12T01:32:16") shouldBe "compact"

  it should "drop one trailing v before _1 without rewriting the stem" in:
    val plain: Dates.Clock = clock("20191220_141955")
    val burst: Dates.Clock = clock("20191220_141955_1")
    clock("20191220_141955v") shouldBe plain
    clock("20191220_141955_1v") shouldBe burst
    labeled("20191220_141955v", "2019-12-20T14:19:55") shouldBe "compact"
    labeled("20191220_141955_1v", "2019-12-20T14:19:55") shouldBe "compact"
    Dates.fromFileName("20191220_141955_2", zone, None) shouldBe None
    Dates.fromFileName("20191220_141955_2v", zone, None) shouldBe None

  it should "skip a short name when no date is supplied" in:
    Dates.fromFileName("013216", zone, None).isEmpty shouldBe true

  it should "convert a 13-digit name with the archive zone" in:
    val clock: Dates.Clock = Dates.millisClock(1236304777311L, zone)
    sameCivil(clock.civil, "2009-03-05T20:59:37.311")
    sameOffset(clock.offset, Some("-05:00"))
    clock.source shouldBe Dates.Source.FilenameMillis
    clock.day.isEqual(LocalDate.of(2009, 3, 5)) shouldBe true

  it should "keep an evening DateTimeOriginal with no offset on that civil day" in:
    val previous: TimeZone = TimeZone.getDefault
    try
      TimeZone.setDefault(TimeZone.getTimeZone("America/New_York"))
      val found: Dates.Clock = readStill("/dates/evening-no-offset.jpg")
      sameCivil(found.civil, "2009-06-11T23:41:10.050")
      found.offset shouldBe None
      found.source shouldBe Dates.Source.DateTimeOriginal
      found.day.isEqual(LocalDate.of(2009, 6, 11)) shouldBe true
    finally TimeZone.setDefault(previous)

  it should "keep that clock when OffsetTimeOriginal is -05:00" in:
    val found: Dates.Clock = readStill("/dates/evening-offset.jpg")
    sameCivil(found.civil, "2009-06-11T23:41:10.050")
    sameOffset(found.offset, Some("-05:00"))
    found.source shouldBe Dates.Source.DateTimeOriginal
    found.day.isEqual(LocalDate.of(2009, 6, 11)) shouldBe true

  it should "prefer DateTimeOriginal over a compact filename" in:
    val embedded: Dates.Clock = Dates.Clock(
      LocalDateTime.parse("2009-06-11T23:41:10.050"),
      None,
      Dates.Source.DateTimeOriginal
    )
    val filename: Dates.Clock = Dates.Clock(
      LocalDateTime.parse("2009-06-11T23:00:00"),
      None,
      Dates.Source.FilenameCompact
    )
    val ranked: Dates.Ranked = Dates.rank(Dates.RankInput(None, None, Some(embedded), Some(filename), None))
    ranked match
      case Dates.Ranked.Chosen(chosen) =>
        chosen.source shouldBe Dates.Source.DateTimeOriginal
        chosen shouldBe embedded
      case other => fail(s"$other")

  it should "let a sidecar timestamp replace the embedded clock" in:
    val timestamp: LocalDateTime = LocalDateTime.parse("2011-06-29T09:45:00")
    val embedded: Dates.Clock = Dates.Clock(
      LocalDateTime.parse("2009-06-11T23:41:10.050"),
      None,
      Dates.Source.DateTimeOriginal
    )
    Dates.rank(Dates.RankInput(Some(timestamp), None, Some(embedded), None, None)) shouldBe
      Dates.Ranked.Chosen(Dates.Clock(timestamp, None, Dates.Source.SidecarTimestamp))

  it should "let a sidecar date replace the day and keep the embedded time" in:
    val embedded: Dates.Clock = Dates.Clock(
      LocalDateTime.parse("2009-06-11T23:41:10.050"),
      Some(ZoneOffset.of("-04:00")),
      Dates.Source.DateTimeOriginal
    )
    val date: LocalDate = LocalDate.of(2012, 4, 4)
    Dates.rank(Dates.RankInput(None, Some(date), Some(embedded), None, None)) shouldBe
      Dates.Ranked.Chosen(
        Dates.Clock(
          LocalDateTime.parse("2012-04-04T23:41:10.050"),
          None,
          Dates.Source.SidecarDate
        )
      )

  it should "report a sidecar date that disagrees with the timestamp" in:
    val timestamp: LocalDateTime = LocalDateTime.parse("2011-06-29T09:45:00")
    val date: LocalDate = LocalDate.of(2011, 6, 30)
    val embedded: Dates.Clock = Dates.Clock(
      LocalDateTime.parse("2009-06-11T23:41:10"),
      None,
      Dates.Source.DateTimeOriginal
    )
    val ranked: Dates.Ranked = Dates.rank(Dates.RankInput(Some(timestamp), Some(date), Some(embedded), None, None))
    ranked match
      case Dates.Ranked.Disagree(foundTimestamp, foundDate) =>
        foundTimestamp.isEqual(timestamp) shouldBe true
        foundDate.isEqual(date) shouldBe true
      case other => fail(s"$other")
    val assessed: Dates.Assessment = Dates.assessment(
      uploadPicture(),
      ranked,
      timestampsVary = true,
      Seq(Instant.EPOCH)
    )
    assessed.clock shouldBe None
    assessed.timestampWrong shouldBe false
    assessed.writesMtime shouldBe None
    assessed.dayShouldBe shouldBe None
    val disagreement: Dates.Ranked.Disagree = assessed.disagreement.getOrElse(fail("no disagreement"))
    disagreement.timestamp.isEqual(timestamp) shouldBe true
    disagreement.date.isEqual(date) shouldBe true

  it should "skip January 1 of 1980 and of 2000 and use the filename" in:
    val filename: Dates.Clock = Dates.Clock(
      LocalDateTime.parse("2009-06-11T12:00:00"),
      None,
      Dates.Source.FilenameCompact
    )
    def ranked(year: Int): Dates.Ranked =
      val embedded: Dates.Clock = Dates.Clock(
        LocalDateTime.parse(s"$year-01-01T00:00:00"),
        None,
        Dates.Source.DateTimeOriginal
      )
      Dates.rank(
        Dates.RankInput(
          None,
          None,
          Dates.usable(Some(embedded)),
          Dates.usable(Some(filename)),
          Some(Dates.Clock(LocalDate.of(2009, 6, 11).atStartOfDay, None, Dates.Source.FolderDay))
        )
      )
    ranked(1980) shouldBe Dates.Ranked.Chosen(filename)
    ranked(2000) shouldBe Dates.Ranked.Chosen(filename)
    val honored: LocalDateTime = LocalDateTime.parse("1980-01-01T00:00:00")
    Dates.rank(Dates.RankInput(Some(honored), None, Dates.usable(Some(filename)), None, None)) shouldBe
      Dates.Ranked.Chosen(Dates.Clock(honored, None, Dates.Source.SidecarTimestamp))

  it should "ignore a thumbnail when choosing the embedded clock" in:
    withOriginals: originals =>
      val day: File = File(originals, "2009/06/11")
      day.mkdirs()
      val jpeg: File = File(day, "3404.jpg")
      ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", jpeg)
      JFiles.copy(resource("/dates/evening-no-offset.jpg").toPath, File(day, "3404.thm").toPath)
      val picture: Picture = pictureAt(originals, "2009/06/11/3404")
      val preferred: Option[Extension.Descriptor] = Dates.captureDescriptor(picture)
      preferred.map(_.name) shouldBe Some("jpg")
      val read: Option[Dates.Clock] = preferred.flatMap: descriptor =>
        descriptor.dater.read(descriptor.name, picture.file(descriptor.name), None, picture.zone)
      read shouldBe None
      Dates.readStill(picture.file("thm")).isDefined shouldBe true

  it should "treat a shifted creation_time as real Z when the filename is nine seconds off" in:
    val digits: LocalDateTime = LocalDateTime.parse("2010-09-14T14:05:45")
    val filename: Dates.Clock = Dates.Clock(
      LocalDateTime.parse("2010-09-14T10:05:36"),
      None,
      Dates.Source.FilenameCompact
    )
    val clock: Dates.Clock = Dates.chooseZ(digits, Some(filename), zone)
    sameCivil(clock.civil, "2010-09-14T10:05:45")
    sameOffset(clock.offset, Some("-04:00"))
    clock.source shouldBe Dates.Source.VideoRealZ
    clock.day.isEqual(LocalDate.of(2010, 9, 14)) shouldBe true

  it should "treat an unshifted creation_time as a local clock when the filename matches it" in:
    val digits: LocalDateTime = LocalDateTime.parse("2010-09-14T10:05:36")
    val filename: Dates.Clock = Dates.Clock(digits, None, Dates.Source.FilenameCompact)
    val clock: Dates.Clock = Dates.chooseZ(digits, Some(filename), zone)
    clock.civil.isEqual(digits) shouldBe true
    clock.offset shouldBe None
    clock.source shouldBe Dates.Source.VideoLocalZ

  it should "assume a real Z when no filename clock is present" in:
    val clock: Dates.Clock = Dates.chooseZ(LocalDateTime.parse("2010-09-14T14:05:45"), None, zone)
    sameCivil(clock.civil, "2010-09-14T10:05:45")
    sameOffset(clock.offset, Some("-04:00"))
    clock.source shouldBe Dates.Source.VideoRealZ

  it should "compare mtime to the second" in:
    val civil: LocalDateTime = LocalDateTime.parse("2009-06-11T23:41:10.050")
    val fraction: Instant = civil.atZone(zone).toInstant
    val onSecond: Instant = LocalDateTime.parse("2009-06-11T23:41:10").atZone(zone).toInstant
    Dates.sameSecond(civil, fraction, zone) shouldBe true
    Dates.sameSecond(civil, onSecond, zone) shouldBe true
    Dates.sameSecond(civil, onSecond.plusSeconds(1), zone) shouldBe false
    civil.truncatedTo(ChronoUnit.SECONDS).atZone(zone).toInstant.toEpochMilli shouldBe onSecond.toEpochMilli

  it should "leave a picture with no clock and no folder day undated" in:
    val picture: Picture = uploadPicture()
    val ranked: Dates.Ranked = Dates.rank(Dates.RankInput(None, None, None, None, None))
    ranked shouldBe Dates.Ranked.Undated
    Dates.dayShouldBe(picture, ranked) shouldBe None
    val assessed: Dates.Assessment = Dates.assessment(picture, ranked, timestampsVary = false, Seq.empty[Instant])
    assessed.clock shouldBe None
    assessed.dayShouldBe shouldBe None
    assessed.timestampWrong shouldBe false
    assessed.writesMtime shouldBe None
    val capture: Dates.Clock = Dates.Clock(
      LocalDateTime.parse("2009-06-11T23:41:10"),
      None,
      Dates.Source.DateTimeOriginal
    )
    val chosen: Dates.Ranked = Dates.rank(Dates.RankInput(None, None, Some(capture), None, None))
    val move: Day = Dates.dayShouldBe(picture, chosen).getOrElse(fail("upload capture should still name a day"))
    move.date.isEqual(LocalDate.of(2009, 6, 11)) shouldBe true

  it should "prefer an mp4 keys-atom offset over creation_time" in:
    val text: String = Files.read(resource("/dates/mp4-keys-creationdate.txt"))
    val clock: Dates.Clock = Dates.parseVideoTags(text, None, zone).getOrElse(fail("no clock"))
    sameCivil(clock.civil, "2010-09-14T10:05:36")
    sameOffset(clock.offset, Some("-04:00"))
    clock.source shouldBe Dates.Source.VideoCreationDate
    Dates.ffprobeShowEntries should include("com.apple.quicktime.creationdate")

  it should "accept a creation date with a colon offset, with Z, and not throw on a bad one" in:
    val colon: OffsetDateTime = Dates.parseCreationDate("2010-09-14T10:05:36-04:00").getOrElse(fail("colon"))
    colon.toLocalDateTime.isEqual(LocalDateTime.parse("2010-09-14T10:05:36")) shouldBe true
    colon.getOffset.getId shouldBe "-04:00"
    val compact: OffsetDateTime = Dates.parseCreationDate("2017-07-22T16:06:06+1000").getOrElse(fail("hhmm"))
    compact.toLocalDateTime.isEqual(LocalDateTime.parse("2017-07-22T16:06:06")) shouldBe true
    compact.getOffset.getId shouldBe "+10:00"
    val summer: OffsetDateTime = Dates.parseCreationDate("2018-01-04T12:55:44+1100").getOrElse(fail("hhmm"))
    summer.toLocalDateTime.isEqual(LocalDateTime.parse("2018-01-04T12:55:44")) shouldBe true
    summer.getOffset.getId shouldBe "+11:00"
    val fraction: OffsetDateTime =
      Dates.parseCreationDate("2010-09-14T10:05:36.12-0400").getOrElse(fail("fraction"))
    fraction.toLocalDateTime.isEqual(LocalDateTime.parse("2010-09-14T10:05:36.12")) shouldBe true
    fraction.getOffset.getId shouldBe "-04:00"
    val zulu: OffsetDateTime = Dates.parseCreationDate("2010-09-14T10:05:36Z").getOrElse(fail("z"))
    zulu.toLocalDateTime.isEqual(LocalDateTime.parse("2010-09-14T10:05:36")) shouldBe true
    zulu.getOffset.getTotalSeconds shouldBe 0
    Dates.parseCreationDate("not-a-date") shouldBe None

  it should "reject an EXIF date that is not yyyy:MM:dd HH:mm:ss" in:
    Dates.parseExifClock("2009-06-11 23:41:10", None, None, Dates.Source.DateTimeOriginal) shouldBe None
    Dates.parseExifClock("2009:06:11 23:41", None, None, Dates.Source.DateTimeOriginal) shouldBe None
    Dates.parseExifClock("0000:00:00 00:00:00", None, None, Dates.Source.DateTimeOriginal) shouldBe None
    Dates.parseExifClock("   ", None, None, Dates.Source.DateTimeOriginal) shouldBe None
    val kept: Dates.Clock = Dates
      .parseExifClock(
        "2009:06:11 23:41:10",
        Some("050"),
        Some("bogus"),
        Dates.Source.DateTimeOriginal
      )
      .getOrElse(fail("digits should survive a bad offset"))
    sameCivil(kept.civil, "2009-06-11T23:41:10.050")
    kept.offset shouldBe None

  private def sameCivil(found: LocalDateTime, text: String): Unit =
    found.isEqual(LocalDateTime.parse(text)) shouldBe true

  private def sameOffset(found: Option[ZoneOffset], id: Option[String]): Unit =
    found.map(_.getId) shouldBe id

  private def clock(name: String): Dates.Clock =
    Dates.fromFileName(name, zone, None).getOrElse(fail(s"no clock in $name"))._1

  private def labeled(name: String, text: String, shortDate: Option[LocalDate] = None): String =
    val found: (Dates.Clock, String) = Dates.fromFileName(name, zone, shortDate).getOrElse(fail(s"no clock in $name"))
    found._1.civil.isEqual(LocalDateTime.parse(text)) shouldBe true
    found._1.offset shouldBe None
    val expected: Dates.Source = found._2 match
      case "full"    => Dates.Source.FilenameFull
      case "compact" => Dates.Source.FilenameCompact
      case "short"   => Dates.Source.FilenameShort
      case other     => fail(other)
    (found._1.source eq expected) shouldBe true
    found._2

  private def resource(path: String): File =
    val url = Option(getClass.getResource(path)).getOrElse(fail(s"missing $path"))
    File(url.toURI)

  private def readStill(path: String): Dates.Clock =
    Dates.readStill(resource(path)).getOrElse(fail(s"no clock in $path"))

  private def uploadPicture(): Picture =
    new Picture(
      EmptyDay(Root(Configuration("Pictures/originals", zone))),
      "1000",
      hasMetadata = false,
      normal = Some(Extension.JPG),
      raw = None,
      image = None,
      thumbnail = None,
      aux = None
    )

  private def pictureAt(originals: File, path: String): Picture =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(originals.toPath).toString
    Root(Configuration(rootPath, zone)).parse(path) match
      case picture: Picture => picture
      case other            => fail(s"not a picture: $other")

  private def withOriginals(body: File => Unit): Unit =
    val originals: File = JFiles.createTempDirectory("dates").toFile
    try body(originals)
    finally deleteTree(originals)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()

private final class EmptyDay(override val root: Root) extends Pictures:
  override def name: String = ".upload"
  override def day: Option[Day] = None
  override protected def getDirectory: File = File("/tmp")
  override def toString: String = name
