package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Files as JFiles
import java.time.{LocalDateTime, ZoneId}
import javax.imageio.ImageIO

class ListingTest extends AnyFlatSpec, Matchers:
  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  // strictEquality does not relate a sealed parent to its cases.
  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")

  "Root.lsd" should "prepend .upload and skip a dot-directory" in :
    withArchive: archive =>
      JFiles.createDirectory(archive.toPath.resolve("2009"))
      JFiles.createDirectory(archive.toPath.resolve(".gphoto-sync"))
      JFiles.createFile(archive.toPath.resolve("AGENTS.md"))
      JFiles.createFile(archive.toPath.resolve("Best.toml"))
      rootFor(archive).lsd.map(_.toString) shouldBe Seq("/.upload", "/2009")

  it should "list .upload once when the directory exists" in :
    withArchive: archive =>
      JFiles.createDirectory(archive.toPath.resolve("2009"))
      JFiles.createDirectory(archive.toPath.resolve(".upload"))
      rootFor(archive).lsd.map(_.toString) shouldBe Seq("/.upload", "/2009")

  "Year.lsd" should "skip a dot-directory and keep the month" in :
    withArchive: archive =>
      val year: File = JFiles.createDirectory(archive.toPath.resolve("2009")).toFile
      JFiles.createDirectory(year.toPath.resolve(".secret"))
      JFiles.createDirectory(year.toPath.resolve("03"))
      rootFor(archive).year(2009).lsd.map(_.toString) shouldBe Seq("/2009/03")

  "Month.lsd" should "skip a dot-directory and keep the day" in :
    withArchive: archive =>
      val year: File = JFiles.createDirectory(archive.toPath.resolve("2009")).toFile
      val month: File = JFiles.createDirectory(year.toPath.resolve("03")).toFile
      JFiles.createDirectory(month.toPath.resolve(".secret"))
      JFiles.createDirectory(month.toPath.resolve("01"))
      rootFor(archive).year(2009).month(3).lsd.map(_.toString) shouldBe Seq("/2009/03/01")

  it should "reject a visible non-numeric directory" in :
    withArchive: archive =>
      val year: File = JFiles.createDirectory(archive.toPath.resolve("2009")).toFile
      JFiles.createDirectory(year.toPath.resolve("foo"))
      an [NumberFormatException] should be thrownBy rootFor(archive).year(2009).lsd

  "Root.item" should "route .upload to the inbox when the directory is absent" in :
    withArchive: archive =>
      val inbox: Directory = rootFor(archive).item(".upload")
      inbox.name shouldBe ".upload"
      inbox shouldBe a [UploadDirectory]

  it should "reject .gphoto-sync as not a year" in :
    withArchive: archive =>
      val exception: IllegalArgumentException = intercept[IllegalArgumentException]:
        rootFor(archive).item(".gphoto-sync")
      exception.getMessage should include ("not a year")

  it should "reject the old Upload name" in :
    withArchive: archive =>
      an [NumberFormatException] should be thrownBy rootFor(archive).item("Upload")

  "ls" should "list years and not the inbox or an album file" in :
    withArchive: archive =>
      JFiles.createDirectory(archive.toPath.resolve("2010"))
      JFiles.createDirectory(archive.toPath.resolve("2009"))
      JFiles.createDirectory(archive.toPath.resolve(".upload"))
      JFiles.createFile(archive.toPath.resolve("Best.toml"))
      val (_, result) = execute(sessionFor(archive), "ls")
      result shouldBe Result.Ok(
        "2009\n2010\n",
        directories(
          entry("2009", "months", 0),
          entry("2010", "months", 0)
        )
      )

  it should "list the months of a year and not fill the day index" in :
    withArchive: archive =>
      jpeg(File(archive, "2010/07/04/2412.jpg"))
      JFiles.createDirectories(archive.toPath.resolve("2010/03"))
      val root: Root = rootFor(archive)
      val (returned, result) = execute(Session(root, PathStyle.Cursor, root), "ls", "2010")
      result shouldBe Result.Ok(
        "2010/03\n2010/07\n",
        directories(
          entry("2010/03", "days", 0),
          entry("2010/07", "days", 1)
        )
      )
      returned.index.isEmpty shouldBe true

  it should "list items under -R and one picture, and not directory names" in :
    withArchive: archive =>
      jpeg(File(archive, "2010/07/04/2412.jpg"))
      val session: Session = sessionFor(archive)
      val item: Json = Json.Obj(Seq(
        "path" -> Json.Str("2010/07/04/2412"),
        "extensions" -> Json.Arr(Seq(Json.Str("jpg"))),
        "sidecar" -> Json.Str(""),
        "assessed" -> Json.Null,
        "wrongDay" -> Json.Bool(false)
      ))
      val listed: Result = Result.Ok("/2010/07/04/2412 jpg\n", items(item))
      val (_, recursive) = execute(session, "ls", "-R", "2010")
      recursive shouldBe listed
      recursive match
        case Result.Ok(text, _) =>
          text.linesIterator.toList should not contain "2010/07"
          text.linesIterator.toList should not contain "2010/07/04"
        case other =>
          fail(other.toString)
      execute(session, "ls", "2010/07/04/2412")._2 shouldBe listed

  it should "list wrong-day items under a month" in :
    withArchive: archive =>
      jpeg(File(archive, "2010/07/04/2412.jpg"))
      jpeg(File(archive, "2010/07/04/1000.jpg"))
      JFiles.writeString(File(archive, "2010/07/04/2412.toml").toPath, "date = 2010-08-01\n")
      val (_, result) = execute(sessionFor(archive), "ls", "--wrong-day", "2010/07")
      result match
        case Result.Ok(text, value) =>
          text should include ("/2010/07/04/2412")
          text should include ("WRONG DAY; SHOULD BE:")
          text should not include ("1000")
          value shouldBe items(Json.Obj(Seq(
            "path" -> Json.Str("2010/07/04/2412"),
            "extensions" -> Json.Arr(Seq(Json.Str("jpg"))),
            "sidecar" -> Json.Str("date=2010-08-01"),
            "assessed" -> Json.Str("2010-08-01T00:00"),
            "wrongDay" -> Json.Bool(true)
          )))
        case other =>
          fail(other.toString)

  it should "count stems without renaming and still list an album-only day" in :
    withArchive: archive =>
      jpeg(File(archive, "2010/07/19/VID_1.jpg"))
      val albumDay: File = File(archive, "2010/07/04")
      albumDay.mkdirs() shouldBe true
      JFiles.createFile(albumDay.toPath.resolve("Best.toml"))
      val (_, result) = execute(sessionFor(archive), "ls", "-l", "2010/07")
      result shouldBe Result.Ok(
        "2010/07/04 items=0\n2010/07/19 items=1\n",
        directories(
          entry("2010/07/04", "items", 0),
          entry("2010/07/19", "items", 1)
        )
      )
      File(archive, "2010/07/19/VID_1.jpg").isFile shouldBe true
      File(archive, "2010/07/19/1.jpg").exists shouldBe false

  it should "reject the inbox" in :
    withArchive: archive =>
      JFiles.createDirectories(archive.toPath.resolve("2010/07"))
      jpeg(File(archive, "2010/07/04/2412.jpg"))
      jpeg(File(archive, ".upload/2412.jpg"))
      val root: Root = rootFor(archive)
      val session: Session = Session(root)
      val serve: Session = Session(root, PathStyle.Serve, root)
      def usage(current: Session, args: String*): Unit =
        execute(current, args*)._2 match
          case Result.Usage(message) => message shouldBe "not a date-tree path"
          case other => fail(other.toString)
      List(session, serve).foreach: current =>
        usage(current, "ls", ".upload")
        usage(current, "ls", ".upload/2412")
        usage(current, "ls", ".upload/missing")
        usage(current, "ls", ".upload/missing/extra")
        usage(current, "ls", ".upload/../2010")
        usage(current, "ls", ".upload/./2412")
        usage(current, "ls", ".upload/2412/extra")
      val atInbox: Session = Session(root, PathStyle.Cursor, root.item(".upload"))
      usage(atInbox, "ls")
      usage(atInbox, "ls", "2412")
      usage(atInbox, "ls", "missing")
      usage(atInbox, "ls", "missing/extra")
      val rootListing: Result = Result.Ok(
        "2010\n",
        directories(entry("2010", "months", 1))
      )
      val yearListing: Result = Result.Ok(
        "2010/07\n",
        directories(entry("2010/07", "days", 1))
      )
      execute(atInbox, "ls", "..")._2 shouldBe rootListing
      execute(atInbox, "ls", "../2010")._2 shouldBe yearListing
      execute(atInbox, "ls", "./..")._2 shouldBe rootListing
      execute(atInbox, "ls", "./../2010")._2 shouldBe yearListing
      execute(atInbox, "ls", "././../2010")._2 shouldBe yearListing
      val year: Year = root.year(2010)
      val atYear: Session = Session(root, PathStyle.Cursor, year)
      usage(atYear, "ls", "../.upload")
      usage(atYear, "ls", "../.upload/2412")
      usage(atYear, "ls", "../.upload/missing/extra")
      val atDay: Session = Session(root, PathStyle.Cursor, root.year(2010).month(7).day(4))
      usage(atDay, "ls", "2412/../../../../.upload/missing")
      usage(atDay, "ls", "2412/../../../../.upload/2412/extra")

  it should "sort a day by assessed civil time, subseconds, and path" in :
    withArchive: archive =>
      val day: String = "2010/07/04"
      val tie: String = "timestamp = \"2010-07-04T12:00:00\"\n"
      writePicture(archive, s"$day/1100.jpg", Some(tie))
      writePicture(archive, s"$day/999.jpg", Some(tie))
      writePicture(archive, s"$day/1278266400000.jpg", None)
      writePicture(archive, s"$day/9000.jpg", Some("timestamp = \"2010-07-04T18:00:00.009\"\n"))
      writePicture(archive, s"$day/1000.jpg", Some("timestamp = \"2010-07-04T18:00:00.010\"\n"))
      writePicture(archive, s"$day/2000.jpg", None)
      writePicture(archive, s"$day/4000.jpg", Some("timestamp = \"2010-07-04T08:00:00\"\ndate = 2010-07-05\n"))
      val session: Session = Session(rootFor(archive, ZoneId.of("UTC")))
      val rows: Seq[Json] = Seq(
        item("2010/07/04/1100", "timestamp=2010-07-04T12:00:00", Json.Str("2010-07-04T12:00")),
        item("2010/07/04/999", "timestamp=2010-07-04T12:00:00", Json.Str("2010-07-04T12:00")),
        item("2010/07/04/1278266400000", "", Json.Str("2010-07-04T18:00Z")),
        item("2010/07/04/9000", "timestamp=2010-07-04T18:00:00.009", Json.Str("2010-07-04T18:00:00.009")),
        item("2010/07/04/1000", "timestamp=2010-07-04T18:00:00.010", Json.Str("2010-07-04T18:00:00.010")),
        item("2010/07/04/2000", "", Json.Null),
        item("2010/07/04/4000", "timestamp=2010-07-04T08:00:00, date=2010-07-05", Json.Null)
      )
      val lines: Seq[String] = Seq(
        "/2010/07/04/1100 jpg [timestamp=2010-07-04T12:00:00]",
        "/2010/07/04/999 jpg [timestamp=2010-07-04T12:00:00]",
        "/2010/07/04/1278266400000 jpg timestampFromName=2010-07-04T18:00Z (millis)",
        "/2010/07/04/9000 jpg [timestamp=2010-07-04T18:00:00.009]",
        "/2010/07/04/1000 jpg [timestamp=2010-07-04T18:00:00.010]",
        "/2010/07/04/2000 jpg",
        "/2010/07/04/4000 jpg [timestamp=2010-07-04T08:00:00, date=2010-07-05]"
      )
      val listed: Json = items(rows*)
      execute(session, "ls", day)._2 shouldBe Result.Ok(lines.mkString("", "\n", "\n"), listed)
      val (_, longResult) = execute(session, "ls", "--long", day)
      longResult match
        case Result.Ok(text, value) =>
          picturePaths(text) shouldBe lines.map(_.takeWhile(_ != ' '))
          text.linesIterator.find(_.startsWith("/2010/07/04/4000")).get should include ("DATE DISAGREES WITH TIMESTAMP")
          text.linesIterator.find(_.startsWith("/2010/07/04/2000")).get should include ("NO TIMESTAMP source=FolderDay")
          value shouldBe listed
        case other =>
          fail(other.toString)
      execute(session, "ls", s"$day/9000")._2 shouldBe Result.Ok(
        "/2010/07/04/9000 jpg [timestamp=2010-07-04T18:00:00.009]\n",
        items(rows(3))
      )
      val untimed: Seq[Json] = Seq(rows(5), rows(6))
      val (_, missingTime) = execute(session, "ls", "--no-timestamp", day)
      missingTime match
        case Result.Ok(text, value) =>
          picturePaths(text) shouldBe Seq("/2010/07/04/2000", "/2010/07/04/4000")
          value shouldBe items(untimed*)
        case other =>
          fail(other.toString)
      val withSidecar: Seq[Json] = Seq(rows(0), rows(1), rows(3), rows(4), rows(6))
      execute(session, "ls", "--has-metadata", day)._2 shouldBe Result.Ok(
        Seq(lines(0), lines(1), lines(3), lines(4), lines(6)).mkString("", "\n", "\n"),
        items(withSidecar*)
      )

  it should "sort -R across days and leave directory lines in name order" in :
    withArchive: archive =>
      writePicture(archive, "2010/07/05/3000.jpg", Some("timestamp = \"2010-07-04T09:00:00\"\n"))
      writePicture(archive, "2010/07/04/2000.jpg", Some("timestamp = \"2010-07-04T12:00:00\"\n"))
      writePicture(archive, "2010/07/05/1000.jpg", Some("timestamp = \"2010-07-04T12:00:00\"\n"))
      val session: Session = Session(rootFor(archive, ZoneId.of("UTC")))
      val rows: Seq[Json] = Seq(
        item("2010/07/05/3000", "timestamp=2010-07-04T09:00:00", Json.Str("2010-07-04T09:00"), wrongDay = true),
        item("2010/07/04/2000", "timestamp=2010-07-04T12:00:00", Json.Str("2010-07-04T12:00")),
        item("2010/07/05/1000", "timestamp=2010-07-04T12:00:00", Json.Str("2010-07-04T12:00"), wrongDay = true)
      )
      execute(session, "ls", "-R", "2010/07")._2 shouldBe Result.Ok(
        Seq(
          "/2010/07/05/3000 jpg [timestamp=2010-07-04T09:00:00]",
          "/2010/07/04/2000 jpg [timestamp=2010-07-04T12:00:00]",
          "/2010/07/05/1000 jpg [timestamp=2010-07-04T12:00:00]"
        ).mkString("", "\n", "\n"),
        items(rows*)
      )
      val directoryJson: Json = directories(
        entry("2010/07/04", "items", 1),
        entry("2010/07/05", "items", 2)
      )
      val (_, plain) = execute(session, "ls", "2010/07")
      plain shouldBe Result.Ok("2010/07/04\n2010/07/05\n", directoryJson)
      plain match
        case Result.Ok(text, _) => text should not include ("items=")
        case other => fail(other.toString)
      execute(session, "ls", "-l", "2010/07")._2 shouldBe Result.Ok(
        "2010/07/04 items=1\n2010/07/05 items=2\n",
        directoryJson
      )

  "ListPictures.itemOrder" should "put an earlier path first for equal times and for untimed rows" in :
    val when: LocalDateTime = LocalDateTime.parse("2010-07-04T12:00:00")
    Seq("2010/07/05/1000", "2010/07/04/2000")
      .sortBy(path => ListPictures.itemOrder(Some(when), path)) shouldBe
      Seq("2010/07/04/2000", "2010/07/05/1000")
    Seq("2010/07/04/4000", "2010/07/04/2000")
      .sortBy(path => ListPictures.itemOrder(None, path)) shouldBe
      Seq("2010/07/04/2000", "2010/07/04/4000")

  private def rootFor(archive: File, zoneId: ZoneId = zone): Root =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    Root(Configuration(rootPath, zoneId))

  private def withArchive(body: File => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("listing").toFile
    try body(archive)
    finally deleteTree(archive)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()

  private def sessionFor(archive: File): Session = Session(rootFor(archive))

  private def execute(session: Session, args: String*): (Session, Result) =
    Cli.parse(args.toList) match
      case Cli.Run(parsed) =>
        Cli.toOperation(parsed).execute(session)
      case other =>
        fail(other.toString)

  private def jpeg(file: File): Unit =
    file.getParentFile.mkdirs()
    ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", file)

  private def writePicture(archive: File, relative: String, sidecar: Option[String]): Unit =
    jpeg(File(archive, relative))
    sidecar.foreach(text => JFiles.writeString(File(archive, relative.stripSuffix(".jpg") + ".toml").toPath, text))

  private def picturePaths(text: String): Seq[String] =
    text.linesIterator.filter(_.startsWith("/")).map(_.takeWhile(_ != ' ')).toSeq

  private def item(path: String, sidecar: String, assessed: Json, wrongDay: Boolean = false): Json =
    Json.Obj(Seq(
      "path" -> Json.Str(path),
      "extensions" -> Json.Arr(Seq(Json.Str("jpg"))),
      "sidecar" -> Json.Str(sidecar),
      "assessed" -> assessed,
      "wrongDay" -> Json.Bool(wrongDay)
    ))

  private def entry(path: String, key: String, count: Long): Json =
    Json.Obj(Seq("path" -> Json.Str(path), key -> Json.Long(count)))

  private def directories(entries: Json*): Json =
    Json.Obj(Seq("kind" -> Json.Str("directories"), "entries" -> Json.Arr(entries)))

  private def items(entries: Json*): Json =
    Json.Obj(Seq("kind" -> Json.Str("items"), "entries" -> Json.Arr(entries)))
