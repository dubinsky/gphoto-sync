package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files as JFiles
import java.time.ZoneId
import javax.imageio.ImageIO

class ClaimsTest extends AnyFlatSpec, Matchers:
  private val zone: ZoneId = ZoneId.of("America/New_York")

  "Claims" should "date a picture inside an event to the event, not the day" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10.toml", "")
      write(originals, "2009/03/10/Purim Seuda.toml", "")
      write(originals, "Best.toml", "members = [\"2009/03/10/2412\", \"2009/01/01/0000\"]\n")
      val picture: Claims.Resolved = one(originals, "2009/03/10/2412")
      picture.dated shouldBe Some("2009/03/10/Purim Seuda")
      picture.curated shouldBe Seq("Best")

  it should "date only unclaimed pictures to the month" in :
    withOriginals: originals =>
      write(originals, "2009/03.toml", "")
      touch(originals, "2009/03/01/1000.jpg")
      touch(originals, "2009/03/02/2000.jpg")
      write(originals, "2009/03/02.toml", "")
      touch(originals, "2009/03/03/3000.jpg")
      write(originals, "2009/03/03/Party.toml", "")
      one(originals, "2009/03/01/1000").dated shouldBe Some("2009/03")
      one(originals, "2009/03/02/2000").dated shouldBe Some("2009/03/02")
      one(originals, "2009/03/03/3000").dated shouldBe Some("2009/03/03/Party")

  it should "not claim a picture named in exclude" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10.toml", "")
      write(originals, "2009/03/10/Purim Seuda.toml", "exclude = [\"2412\"]\n")
      one(originals, "2009/03/10/2412").dated shouldBe Some("2009/03/10")

  it should "honor a time-bounded from" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/2412.toml", "timestamp = \"2009-03-10T19:00\"\n")
      touch(originals, "2009/03/10/1000.jpg")
      write(originals, "2009/03/10/Late.toml", "from = 2009-03-10T18:00\n")
      one(originals, "2009/03/10/2412").dated shouldBe Some("2009/03/10/Late")
      one(originals, "2009/03/10/1000").dated shouldBe None

  it should "read a filename clock after one trailing _1" in :
    withOriginals: originals =>
      touch(originals, "2012/06/17/20120617_102453_1.jpg")
      write(originals, "2012/06/17/Morning.toml", "from = 2012-06-17T09:00\n")
      write(originals, "2012/06/17/Early.toml", "to = 2012-06-17T01:00\n")
      one(originals, "2012/06/17/20120617_102453_1").dated shouldBe Some("2012/06/17/Morning")

  it should "return Left when two events claim one picture" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/One.toml", "")
      write(originals, "2009/03/10/Two.toml", "")
      Claims(originals, zone) match
        case Left(error) =>
          error should include ("2009/03/10/2412")
          error should include ("2009/03/10/One")
          error should include ("2009/03/10/Two")
        case Right(_) => fail("expected a conflict")

  it should "reject an unknown key" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/Party.toml", "nope = true\n")
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy Claims(originals, zone)
      caught.getMessage should include ("nope")

  it should "load an exact google title and an owner on an external event" in :
    withOriginals: originals =>
      write(
        originals,
        "2009/03/10/Party.toml",
        "external = true\nfrom = 2009-03-10\n" +
          "google-title = \" Shifra Rabiski's wedding\"\nowner = \"Ada Owner\"\n"
      )
      Album.load(originals, "2009/03/10/Party").album match
        case event: Album.Event =>
          event.googleTitle shouldBe " Shifra Rabiski's wedding"
          event.desiredGoogleTitle shouldBe " Shifra Rabiski's wedding"
          event.owner shouldBe Some("Ada Owner")
          event.external shouldBe true
          event.googleAlbumId shouldBe ""
        case other => fail(s"expected an event, got $other")

  it should "load a trailing space on a curated google title" in :
    withOriginals: originals =>
      write(originals, "Hawaii.toml", "google-title = \"Hawaii \"\n")
      val curated: Album.Curated = Album.readCurated(File(originals, "Hawaii.toml"), "Hawaii")
      curated.googleTitle shouldBe "Hawaii "
      curated.desiredGoogleTitle shouldBe "Hawaii "
      curated.owner shouldBe None
      write(
        originals,
        "Spaces.toml",
        "external = true\ngoogle-title = \"   \"\nowner = \" Ada \"\n"
      )
      val padded: Album.Curated = Album.readCurated(File(originals, "Spaces.toml"), "Spaces")
      padded.googleTitle shouldBe "   "
      padded.owner shouldBe Some(" Ada ")

  it should "quote a title without trimming" in :
    Album.quote("a\\b\"c") shouldBe "\"a\\\\b\\\"c\""
    Album.quote(" Hawaii ") shouldBe "\" Hawaii \""

  it should "reject an illegal google title or owner" in :
    withOriginals: originals =>
      def reject(text: String, needle: String): Unit =
        write(originals, "2009/03/10/Party.toml", text)
        val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
          Album.load(originals, "2009/03/10/Party")
        caught.getMessage should include (needle)

      reject(
        "external = true\ngoogle-title = \"X\"\nowner = \"\"\n",
        "owner must be a non-empty display name"
      )
      reject(
        "external = true\ngoogle-title = \"X\"\nowner = \"   \"\n",
        "owner must be a non-empty display name"
      )
      reject(
        "google-title = \"X\"\nowner = \"Ada Owner\"\n",
        "owner requires external = true"
      )
      reject(
        "external = true\ngoogle-title = \"X\"\ngoogle-album-id = \"abc\"\nowner = \"Ada Owner\"\n",
        "owner must not be combined with google-album-id"
      )
      reject(
        "external = true\nowner = \"Ada Owner\"\n",
        "owner requires a non-empty google-title"
      )
      reject(
        "google-title = \"bad\\ttitle\"\nowner = \"   \"\nexternal = true\n",
        "key 'google-title' must not contain a tab, CR, or LF"
      )
      reject("owner = true\n", "must be a string")
      reject("owned = false\n", "unknown key 'owned'")
      reject("google_title = \"X\"\n", "unknown key 'google_title'")

  it should "rewrite a curated member when a picture moves" in :
    withOriginals: originals =>
      val best: String =
        "# keep\n" +
        "google-album-id = \"abc\"\n" +
        "google-title = \"Keep me\"\n" +
        "members = [\"2009/03/10/2412\", \"2009/01/01/0000\"]\n" +
        "exclude = [\"2412\"]\n" +
        "external = true\n" +
        "published-title = \"The Best\"\n"
      val owned: String =
        "external = true\n" +
        "google-title = \"Hawaii \"\n" +
        "owner = \"Ada Owner\"\n" +
        "members = [\"2009/03/10/2412\"]\n"
      val other: String = "members = [\"1999/01/01/0001\"]\nexternal = false\n"
      val event: String = "from = 2009-03-10\nexclude = [\"2412\"]\n"
      write(originals, "Best.toml", best)
      write(originals, "Owned.toml", owned)
      write(originals, "Other.toml", other)
      write(originals, "2009/03/10/Purim Seuda.toml", event)
      Album.rewriteMember(originals, "2009/03/10/2412", "2009/03/11/2412")
      val rewritten: String = Files.read(File(originals, "Best.toml"))
      rewritten should include ("2009/03/11/2412")
      rewritten should not include ("2009/03/10/2412")
      val curated: Album.Curated = Album.readCurated(File(originals, "Best.toml"), "Best")
      curated.members shouldBe Seq("2009/03/11/2412", "2009/01/01/0000")
      curated.exclude shouldBe Set("2412")
      curated.external shouldBe true
      curated.googleAlbumId shouldBe "abc"
      curated.publishedTitle shouldBe "The Best"
      curated.googleTitle shouldBe "Keep me"
      curated.owner shouldBe None
      val shared: Album.Curated = Album.readCurated(File(originals, "Owned.toml"), "Owned")
      shared.members shouldBe Seq("2009/03/11/2412")
      shared.external shouldBe true
      shared.googleAlbumId shouldBe ""
      shared.googleTitle shouldBe "Hawaii "
      shared.owner shouldBe Some("Ada Owner")
      Files.read(File(originals, "Other.toml")) shouldBe other
      Files.read(File(originals, "2009/03/10/Purim Seuda.toml")) shouldBe event

  it should "not duplicate a curated member that already lists the new path" in :
    withOriginals: originals =>
      write(originals, "Best.toml", "members = [\"2009/03/11/2412\", \"2009/03/10/2412\"]\n")
      Album.rewriteMember(originals, "2009/03/10/2412", "2009/03/11/2412")
      val rewritten: String = Files.read(File(originals, "Best.toml"))
      rewritten should not include ("2009/03/10/2412")
      Album.readCurated(File(originals, "Best.toml"), "Best").members shouldBe Seq("2009/03/11/2412")

  it should "reject an unknown key instead of rewriting members" in :
    withOriginals: originals =>
      val text: String = "nope = true\nmembers = [\"2009/03/10/2412\"]\n"
      write(originals, "Best.toml", text)
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        Album.rewriteMember(originals, "2009/03/10/2412", "2009/03/11/2412")
      caught.getMessage should include ("nope")
      Files.read(File(originals, "Best.toml")) shouldBe text

  it should "reject an unknown key before repair --days moves the picture" in :
    withOriginals: originals =>
      val name: String = "20090311_120000"
      val oldPath: String = s"2009/03/10/$name"
      val best: String = s"members = [\"$oldPath\"]\nexclude = [\"9999\"]\n"
      val other: String = "members = [\"1999/01/01/0001\"]\n"
      val bad: String = "nope = true\n"
      val event: String = "from = 2009-03-10\n"
      write(originals, "Best.toml", best)
      write(originals, "Other.toml", other)
      write(originals, "Zed.toml", bad)
      write(originals, "2009/03/10/Purim Seuda.toml", event)
      val picture: File = File(originals, s"$oldPath.jpg")
      picture.getParentFile.mkdirs()
      ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", picture)
      val root: Root = rootFor(originals)
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        Repair.run(
          root.parse("2009/03/10"),
          names = false,
          raw = false,
          timestamps = false,
          days = true,
          dryRun = false
        )
      caught.getMessage should include ("nope")
      picture.exists shouldBe true
      File(originals, s"2009/03/11/$name.jpg").exists shouldBe false
      Files.read(File(originals, "Best.toml")) shouldBe best
      Files.read(File(originals, "Other.toml")) shouldBe other
      Files.read(File(originals, "Zed.toml")) shouldBe bad
      Files.read(File(originals, "2009/03/10/Purim Seuda.toml")) shouldBe event

  it should "reject owner with a google album id before repair --days moves the picture" in :
    withOriginals: originals =>
      val name: String = "20090311_120000"
      val oldPath: String = s"2009/03/10/$name"
      val best: String =
        "external = true\n" +
        "google-title = \"Hawaii \"\n" +
        "google-album-id = \"abc\"\n" +
        "owner = \"Ada Owner\"\n" +
        s"members = [\"$oldPath\"]\n"
      val other: String = "members = [\"1999/01/01/0001\"]\n"
      val event: String = "from = 2009-03-10\n"
      write(originals, "Best.toml", best)
      write(originals, "Other.toml", other)
      write(originals, "2009/03/10/Purim Seuda.toml", event)
      val picture: File = File(originals, s"$oldPath.jpg")
      picture.getParentFile.mkdirs()
      ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", picture)
      val root: Root = rootFor(originals)
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        Repair.run(
          root.parse("2009/03/10"),
          names = false,
          raw = false,
          timestamps = false,
          days = true,
          dryRun = false
        )
      caught.getMessage should include ("owner must not be combined with google-album-id")
      picture.exists shouldBe true
      File(originals, s"2009/03/11/$name.jpg").exists shouldBe false
      Files.read(File(originals, "Best.toml")) shouldBe best
      Files.read(File(originals, "Other.toml")) shouldBe other
      Files.read(File(originals, "2009/03/10/Purim Seuda.toml")) shouldBe event

  it should "reject owner with a google album id on a curated file that lacks the member" in :
    withOriginals: originals =>
      val name: String = "20090311_120000"
      val oldPath: String = s"2009/03/10/$name"
      val best: String = s"members = [\"$oldPath\"]\nexclude = [\"9999\"]\n"
      val other: String =
        "external = true\n" +
        "google-title = \"Hawaii \"\n" +
        "google-album-id = \"abc\"\n" +
        "owner = \"Ada Owner\"\n" +
        "members = [\"1999/01/01/0001\"]\n"
      val event: String = "from = 2009-03-10\n"
      write(originals, "Best.toml", best)
      write(originals, "Other.toml", other)
      write(originals, "2009/03/10/Purim Seuda.toml", event)
      val picture: File = File(originals, s"$oldPath.jpg")
      picture.getParentFile.mkdirs()
      ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", picture)
      val root: Root = rootFor(originals)
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        Repair.run(
          root.parse("2009/03/10"),
          names = false,
          raw = false,
          timestamps = false,
          days = true,
          dryRun = false
        )
      caught.getMessage should include ("owner must not be combined with google-album-id")
      picture.exists shouldBe true
      File(originals, s"2009/03/11/$name.jpg").exists shouldBe false
      Files.read(File(originals, "Best.toml")) shouldBe best
      Files.read(File(originals, "Other.toml")) shouldBe other
      Files.read(File(originals, "2009/03/10/Purim Seuda.toml")) shouldBe event

  it should "load a labeled month and date a picture to it" in :
    withOriginals: originals =>
      val text: String =
        "exclude = [\"9999\"]\n" +
        "external = false\n" +
        "google-album-id = \"album-trip\"\n" +
        "published-title = \"March trip\"\n" +
        "google-title = \"March trip\"\n"
      write(originals, "2009/03/Trip.toml", text)
      touch(originals, "2009/03/01/1000.jpg")
      Album.load(originals, "2009/03/Trip").album match
        case month: Album.Month =>
          month.title shouldBe "2009/03/Trip"
          month.exclude shouldBe Set("9999")
          month.external shouldBe false
          month.googleAlbumId shouldBe "album-trip"
          month.publishedTitle shouldBe "March trip"
          month.googleTitle shouldBe "March trip"
          month.owner shouldBe None
          month.desiredGoogleTitle shouldBe "March trip"
        case other => fail(s"expected a month, got $other")
      one(originals, "2009/03/01/1000").dated shouldBe Some("2009/03/Trip")

  it should "keep a two-digit third segment as a day" in :
    withOriginals: originals =>
      write(originals, "2009/03/02.toml", "")
      write(originals, "2009/03/Trip.toml", "")
      touch(originals, "2009/03/02/2000.jpg")
      Album.load(originals, "2009/03/02").album match
        case day: Album.Day => day.title shouldBe "2009/03/02"
        case other => fail(s"expected a day, got $other")
      one(originals, "2009/03/02/2000").dated shouldBe Some("2009/03/02")
      write(originals, "2009/03/02/Party.toml", "")
      Album.load(originals, "2009/03/02/Party").album match
        case event: Album.Event => event.title shouldBe "2009/03/02/Party"
        case other => fail(s"expected an event, got $other")
      val extra: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        Album.load(originals, "2005/04/Toronto/extra")
      extra.getMessage shouldBe "not an album title: 2005/04/Toronto/extra"
      val yearName: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        Album.load(originals, "2005/Toronto")
      yearName.getMessage shouldBe "not an album title: 2005/Toronto"
    withOriginals: originals =>
      write(originals, "2009/03/4.toml", "")
      Album.load(originals, "2009/03/4").album match
        case month: Album.Month => month.title shouldBe "2009/03/4"
        case other => fail(s"expected a month, got $other")
      Album.matchesPrefix("2005/04/05/1", Some("2005/04/4")) shouldBe true
      Album.matchesPrefix("2005/04/05/1", Some("2005/04/04")) shouldBe false

  it should "ignore a two-digit name that is not a day" in :
    withOriginals: originals =>
      write(originals, "2009/03/32.toml", "")
      touch(originals, "2009/03/01/1000.jpg")
      Album.load(originals, "2009/03/32").album match
        case day: Album.Day => day.title shouldBe "2009/03/32"
        case other => fail(s"expected a day, got $other")
      one(originals, "2009/03/01/1000").dated shouldBe None

  it should "reject a plain month and a labeled month for one year-month" in :
    withOriginals: originals =>
      write(originals, "2009/03.toml", "")
      write(originals, "2009/03/Trip.toml", "")
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy Claims(originals, zone)
      monthClash(originals, Seq("2009/03.toml", "2009/03/Trip.toml"), caught.getMessage)

  it should "reject two labeled months for one year-month" in :
    withOriginals: originals =>
      write(originals, "2009/03/A.toml", "")
      write(originals, "2009/03/B.toml", "")
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy Claims(originals, zone)
      monthClash(originals, Seq("2009/03/A.toml", "2009/03/B.toml"), caught.getMessage)

  it should "reject three month files for one year-month" in :
    withOriginals: originals =>
      write(originals, "2009/03.toml", "")
      write(originals, "2009/03/A.toml", "")
      write(originals, "2009/03/B.toml", "")
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy Claims(originals, zone)
      monthClash(originals, Seq("2009/03.toml", "2009/03/A.toml", "2009/03/B.toml"), caught.getMessage)

  it should "let an event win over a labeled month" in :
    withOriginals: originals =>
      write(originals, "2009/03/Trip.toml", "")
      write(originals, "2009/03/03/Party.toml", "")
      touch(originals, "2009/03/03/3000.jpg")
      touch(originals, "2009/03/01/1000.jpg")
      one(originals, "2009/03/03/3000").dated shouldBe Some("2009/03/03/Party")
      one(originals, "2009/03/01/1000").dated shouldBe Some("2009/03/Trip")

  it should "not date a picture outside an event when there is no month album" in :
    withOriginals: originals =>
      write(
        originals,
        "2009/06/18/Toronto.toml",
        "to = 2009-06-22\nname-from = \"3427\"\nname-to = \"3899\"\n"
      )
      touch(originals, "2009/06/20/1000.jpg")
      Album.load(originals, "2009/06/18/Toronto").album shouldBe a[Album.Event]
      one(originals, "2009/06/20/1000").dated shouldBe None

  it should "exclude a stem from a labeled month" in :
    withOriginals: originals =>
      write(originals, "2009/03/Trip.toml", "exclude = [\"1000\"]\n")
      touch(originals, "2009/03/01/1000.jpg")
      val picture: Claims.Resolved = one(originals, "2009/03/01/1000")
      picture.dated shouldBe None
      picture.excludedOnly shouldBe true

  it should "select a labeled month by its year and month" in :
    Album.needsRecursive(None) shouldBe true
    Album.needsRecursive(Some("")) shouldBe true
    Album.needsRecursive(Some("2005")) shouldBe true
    Album.needsRecursive(Some("2005/04")) shouldBe false
    Album.needsRecursive(Some("2005/04/Toronto")) shouldBe false
    Album.needsRecursive(Some("2005/04/28")) shouldBe false
    Album.matchesPrefix("2005/04/28/4216", Some("2005/04/Toronto")) shouldBe true
    Album.matchesPrefix("2005/05/01/0001", Some("2005/04/Toronto")) shouldBe false
    Album.matchesPrefix("2005/04/29/1", Some("2005/04/28")) shouldBe false
    Album.matchesPrefix("2005/04/28/4216", Some("2005/13/Name")) shouldBe false

  it should "print the google title and owner before the picture line" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(
        originals,
        "2009/03/10/Party.toml",
        "external = true\nfrom = 2009-03-10\n" +
          "google-title = \" Shifra Rabiski's wedding\"\nowner = \"Ada Owner\"\n"
      )
      val lines: Seq[String] = listLines(originals, "")
      lines shouldBe Seq(
        "album \"2009/03/10/Party\" google=\" Shifra Rabiski's wedding\" owner=\"Ada Owner\"",
        "2009/03/10/2412 dated=2009/03/10/Party curated=-"
      )

  it should "print a trailing space on a curated google title and no owner" in :
    withOriginals: originals =>
      write(originals, "Hawaii.toml", "google-title = \"Hawaii \"\n")
      listLines(originals, "") shouldBe Seq("album \"Hawaii\" google=\"Hawaii \"")

  it should "print a month google title and keep the path title on dated" in :
    withOriginals: originals =>
      write(originals, "2009/03/Trip.toml", "google-title = \"March trip\"\n")
      touch(originals, "2009/03/01/1000.jpg")
      listLines(originals, "") shouldBe Seq(
        "album \"2009/03/Trip\" google=\"March trip\"",
        "2009/03/01/1000 dated=2009/03/Trip curated=-"
      )

  it should "print no album line when google title and owner are absent" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/Party.toml", "")
      val lines: Seq[String] = listLines(originals, "")
      lines shouldBe Seq("2009/03/10/2412 dated=2009/03/10/Party curated=-")
      lines.exists(_.startsWith("album ")) shouldBe false

  it should "print no album line when google title repeats the path title" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/Party.toml", "google-title = \"2009/03/10/Party\"\n")
      listLines(originals, "") shouldBe Seq("2009/03/10/2412 dated=2009/03/10/Party curated=-")

  it should "print no album line and no picture line when two events claim one picture" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/One.toml", "google-title = \"One night\"\n")
      write(originals, "2009/03/10/Two.toml", "google-title = \"Two night\"\n")
      val root: Root = rootFor(originals)
      val (out, thrown) = capture(ListAlbums(Some(root)).execute())
      thrown.get shouldBe a[IllegalArgumentException]
      out should include ("2009/03/10/2412")
      out should include ("claimed by")
      out should not include ("album ")
      out should not include ("dated=")

  it should "escape a quote and a backslash in the google title" in :
    withOriginals: originals =>
      write(originals, "Quoted.toml", "google-title = \"say \\\"hi\\\" \\\\ now\"\n")
      listLines(originals, "") shouldBe Seq("album \"Quoted\" google=\"say \\\"hi\\\" \\\\ now\"")

  it should "filter album lines by prefix and still print an album with no selected picture" in :
    withOriginals: originals =>
      write(originals, "2009/03/Trip.toml", "google-title = \"March trip\"\n")
      write(originals, "Hawaii.toml", "google-title = \"Hawaii \"\nmembers = [\"2009/04/01/2000\"]\n")
      touch(originals, "2009/04/01/2000.jpg")
      listLines(originals, "2009/03") shouldBe Seq("album \"2009/03/Trip\" google=\"March trip\"")
      val all: Seq[String] = listLines(originals, "")
      all shouldBe Seq(
        "album \"2009/03/Trip\" google=\"March trip\"",
        "album \"Hawaii\" google=\"Hawaii \"",
        "2009/04/01/2000 dated=- curated=Hawaii"
      )

  private def listLines(originals: File, prefix: String): Seq[String] =
    val root: Root = rootFor(originals)
    val path: Path = if prefix.isEmpty then root else root.parse(prefix)
    val (out, thrown) = capture(ListAlbums(Some(path)).execute())
    thrown match
      case Some(error) => fail(s"$error\n$out")
      case None => out.trim.linesIterator.toSeq

  private def capture(body: => Unit): (String, Option[Throwable]) =
    val buffer: ByteArrayOutputStream = new ByteArrayOutputStream
    val thrown: Option[Throwable] =
      try
        Console.withOut(buffer)(body)
        None
      catch case error: Throwable => Some(error)
    (buffer.toString(StandardCharsets.UTF_8), thrown)

  private def monthClash(originals: File, names: Seq[String], actual: String): Unit =
    actual should not include ("Photo Archive Tool:")
    val paths: String = names.map(name => File(originals, name).getPath).sorted.mkString(" and ")
    actual shouldBe s"more than one month album for 2009-03: $paths"

  private def one(originals: File, path: String): Claims.Resolved = Claims(originals, zone) match
    case Left(error) => fail(error)
    case Right(pictures) => pictures.find(_.path == path).getOrElse(fail(s"missing $path"))

  private def write(originals: File, path: String, content: String): Unit =
    val file: File = File(originals, path)
    file.getParentFile.mkdirs()
    Files.write(content, file)

  private def touch(originals: File, path: String): Unit = write(originals, path, "")

  private def rootFor(originals: File): Root =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(originals.getParentFile.toPath).toString
    Root(Configuration(rootPath, originals.getName, "Upload", zone))

  private def withOriginals(body: File => Unit): Unit =
    val originals: File = JFiles.createTempDirectory("claims").toFile
    try body(originals)
    finally deleteTree(originals)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
