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

  "Claims" should "list every album that includes the picture" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/album.toml", "include = [\"all\"]\n")
      write(originals, "2009/03/10/Purim Seuda.toml", "include = [\"all\"]\n")
      write(originals, "Best.toml", "include = [\"2009/03/10 2412\", \"2009/01/01 0000\"]\n")
      val picture: Claims.Resolved = one(originals, "2009/03/10/2412")
      picture.albums shouldBe Seq("2009/03/10", "2009/03/10/Purim Seuda", "Best")
      picture.excludedOnly shouldBe false

  it should "let a month and a day album both claim" in :
    withOriginals: originals =>
      write(originals, "2009/03/album.toml", "include = [\"all\"]\n")
      touch(originals, "2009/03/01/1000.jpg")
      touch(originals, "2009/03/02/2000.jpg")
      write(originals, "2009/03/02/album.toml", "include = [\"all\"]\n")
      touch(originals, "2009/03/03/3000.jpg")
      write(originals, "2009/03/03/Party.toml", "include = [\"all\"]\n")
      one(originals, "2009/03/01/1000").albums shouldBe Seq("2009/03")
      one(originals, "2009/03/02/2000").albums shouldBe Seq("2009/03", "2009/03/02")
      one(originals, "2009/03/03/3000").albums shouldBe Seq("2009/03", "2009/03/03/Party")

  it should "leave an excepted stem unclaimed" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/Purim Seuda.toml", "include = [\"all except 2412\"]\n")
      val picture: Claims.Resolved = one(originals, "2009/03/10/2412")
      picture.albums shouldBe Seq.empty[String]
      picture.excludedOnly shouldBe true
      picture.line shouldBe "2009/03/10/2412 albums=-"

  it should "list both titles when two albums claim one picture" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/One.toml", "include = [\"all\"]\n")
      write(originals, "2009/03/10/Two.toml", "include = [\"all\"]\n")
      one(originals, "2009/03/10/2412").albums shouldBe Seq("2009/03/10/One", "2009/03/10/Two")

  it should "not claim a stem outside the include range" in :
    withOriginals: originals =>
      write(originals, "2009/06/18/to-22 Toronto.toml", "include = [\"2009/06/18-22 3427..3899\"]\n")
      touch(originals, "2009/06/20/1000.jpg")
      val album: Album = Album.read(originals, File(originals, "2009/06/18/to-22 Toronto.toml"))
      album.title shouldBe "2009/06/18-22/Toronto"
      val picture: Claims.Resolved = one(originals, "2009/06/20/1000")
      picture.albums shouldBe Seq.empty[String]
      picture.excludedOnly shouldBe false

  it should "reject an unknown key" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/Party.toml", "nope = true\n")
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy Claims(originals)
      caught.getMessage should include ("nope")

  it should "load an exact google title and an owner" in :
    withOriginals: originals =>
      write(
        originals,
        "2009/03/10/Party.toml",
        "external = true\ninclude = [\"all\"]\n" +
          "google-title = \" Shifra Rabiski's wedding\"\nowner = \"Ada Owner\"\n"
      )
      val event: Album = Album.read(originals, File(originals, "2009/03/10/Party.toml"))
      event.googleTitle shouldBe " Shifra Rabiski's wedding"
      event.desiredGoogleTitle shouldBe " Shifra Rabiski's wedding"
      event.owner shouldBe Some("Ada Owner")
      event.external shouldBe true
      event.googleAlbumId shouldBe ""
      event.title shouldBe "2009/03/10/Party"

  it should "load a trailing space on a curated google title" in :
    withOriginals: originals =>
      write(originals, "Hawaii.toml", "google-title = \"Hawaii \"\n")
      val curated: Album = Album.read(originals, File(originals, "Hawaii.toml"))
      curated.googleTitle shouldBe "Hawaii "
      curated.desiredGoogleTitle shouldBe "Hawaii "
      curated.owner shouldBe None
      curated.title shouldBe "Hawaii"
      write(
        originals,
        "Spaces.toml",
        "external = true\ngoogle-title = \"   \"\nowner = \" Ada \"\n"
      )
      val padded: Album = Album.read(originals, File(originals, "Spaces.toml"))
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
          Album.read(originals, File(originals, "2009/03/10/Party.toml"))
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

  it should "rewrite a curated include when a picture moves" in :
    withOriginals: originals =>
      val best: String =
        "google-album-id = \"abc\"\n" +
        "google-title = \"Keep me\"\n" +
        "include = [\"2009/03/10 2412\", \"2009/01/01 0000\"]\n" +
        "external = true\n" +
        "published-title = \"The Best\"\n"
      val owned: String =
        "external = true\n" +
        "google-title = \"Hawaii \"\n" +
        "owner = \"Ada Owner\"\n" +
        "include = [\"2009/03/10 2412\"]\n"
      val other: String = "include = [\"1999/01/01 0001\"]\nexternal = false\n"
      val event: String = "include = [\"all\"]\n"
      write(originals, "Best.toml", best)
      write(originals, "Owned.toml", owned)
      write(originals, "Other.toml", other)
      write(originals, "2009/03/10/Purim Seuda.toml", event)
      Album.rewriteInclude(originals, "2009/03/10/2412", "2009/03/11/2412")
      val rewritten: String = Files.read(File(originals, "Best.toml"))
      rewritten should include ("2009/03/11 2412")
      rewritten should not include ("2009/03/10")
      val curated: Album = Album.read(originals, File(originals, "Best.toml"))
      curated.include.map(Include.logical) shouldBe Seq("2009/01/01 0000", "2009/03/11 2412")
      curated.external shouldBe true
      curated.googleAlbumId shouldBe "abc"
      curated.publishedTitle shouldBe "The Best"
      curated.googleTitle shouldBe "Keep me"
      curated.owner shouldBe None
      val shared: Album = Album.read(originals, File(originals, "Owned.toml"))
      shared.include.map(Include.logical) shouldBe Seq("2009/03/11 2412")
      shared.external shouldBe true
      shared.googleAlbumId shouldBe ""
      shared.googleTitle shouldBe "Hawaii "
      shared.owner shouldBe Some("Ada Owner")
      Files.read(File(originals, "Other.toml")) shouldBe other
      Files.read(File(originals, "2009/03/10/Purim Seuda.toml")) shouldBe event

  it should "not duplicate a curated include that already lists the new path" in :
    withOriginals: originals =>
      write(originals, "Best.toml", "include = [\"2009/03/11 2412\", \"2009/03/10 2412\"]\n")
      Album.rewriteInclude(originals, "2009/03/10/2412", "2009/03/11/2412")
      val rewritten: String = Files.read(File(originals, "Best.toml"))
      rewritten should not include ("2009/03/10")
      Album.read(originals, File(originals, "Best.toml")).include.map(Include.logical) shouldBe
        Seq("2009/03/11 2412")

  it should "reject an unknown key instead of rewriting include" in :
    withOriginals: originals =>
      val text: String = "nope = true\ninclude = [\"2009/03/10 2412\"]\n"
      write(originals, "Best.toml", text)
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        Album.rewriteInclude(originals, "2009/03/10/2412", "2009/03/11/2412")
      caught.getMessage should include ("nope")
      Files.read(File(originals, "Best.toml")) shouldBe text

  it should "reject an unknown key before repair --days moves the picture" in :
    withOriginals: originals =>
      val name: String = "20090311_120000"
      val oldPath: String = s"2009/03/10/$name"
      val best: String = "include = [\"2009/03/10 2412\"]\n"
      val other: String = "include = [\"1999/01/01 0001\"]\n"
      val bad: String = "nope = true\n"
      val event: String = "include = [\"all\"]\n"
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
        "include = [\"2009/03/10 2412\"]\n"
      val other: String = "include = [\"1999/01/01 0001\"]\n"
      val event: String = "include = [\"all\"]\n"
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

  it should "reject owner with a google album id on a curated file that lacks the stem" in :
    withOriginals: originals =>
      val name: String = "20090311_120000"
      val oldPath: String = s"2009/03/10/$name"
      val best: String = "include = [\"2009/03/10 2412\"]\n"
      val other: String =
        "external = true\n" +
        "google-title = \"Hawaii \"\n" +
        "google-album-id = \"abc\"\n" +
        "owner = \"Ada Owner\"\n" +
        "include = [\"1999/01/01 0001\"]\n"
      val event: String = "include = [\"all\"]\n"
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

  it should "load a named month and claim a picture in that month" in :
    withOriginals: originals =>
      val text: String =
        "include = [\"all except 9999\"]\n" +
        "external = false\n" +
        "google-album-id = \"album-trip\"\n" +
        "published-title = \"March trip\"\n" +
        "google-title = \"March trip\"\n"
      write(originals, "2009/03/Trip.toml", text)
      touch(originals, "2009/03/01/1000.jpg")
      val month: Album = Album.read(originals, File(originals, "2009/03/Trip.toml"))
      month.title shouldBe "2009/03/Trip"
      month.include.map(Include.logical) shouldBe Seq("all except 9999")
      month.external shouldBe false
      month.googleAlbumId shouldBe "album-trip"
      month.publishedTitle shouldBe "March trip"
      month.googleTitle shouldBe "March trip"
      month.owner shouldBe None
      month.desiredGoogleTitle shouldBe "March trip"
      one(originals, "2009/03/01/1000").albums shouldBe Seq("2009/03/Trip")

  it should "read a day album and a named day album from the day folder" in :
    withOriginals: originals =>
      write(originals, "2009/03/02/album.toml", "include = [\"all\"]\n")
      write(originals, "2009/03/02/Party.toml", "include = [\"all\"]\n")
      touch(originals, "2009/03/02/2000.jpg")
      Album.read(originals, File(originals, "2009/03/02/album.toml")).title shouldBe "2009/03/02"
      Album.read(originals, File(originals, "2009/03/02/Party.toml")).title shouldBe "2009/03/02/Party"
      one(originals, "2009/03/02/2000").albums shouldBe Seq("2009/03/02", "2009/03/02/Party")
      val extra: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        Album.fileFor(originals, "2005/04/Toronto/extra")
      extra.getMessage shouldBe "not an album title: 2005/04/Toronto/extra"
      val yearName: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        Album.fileFor(originals, "2005/Toronto")
      yearName.getMessage shouldBe "not an album title: 2005/Toronto"

  it should "reject a toml that is not an album file" in :
    withOriginals: originals =>
      write(originals, "2009/03/32.toml", "")
      touch(originals, "2009/03/01/1000.jpg")
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy Claims(originals)
      caught.getMessage should include ("not an album file")
    withOriginals: originals =>
      write(originals, "2009/03.toml", "include = [\"all\"]\n")
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy Claims(originals)
      caught.getMessage should include ("album files do not live in a year directory")

  it should "let two month albums both claim" in :
    withOriginals: originals =>
      write(originals, "2009/03/album.toml", "include = [\"all\"]\n")
      write(originals, "2009/03/Trip.toml", "include = [\"all\"]\n")
      touch(originals, "2009/03/01/1000.jpg")
      one(originals, "2009/03/01/1000").albums shouldBe Seq("2009/03", "2009/03/Trip")

  it should "mark a month except as excluded only" in :
    withOriginals: originals =>
      write(originals, "2009/03/Trip.toml", "include = [\"all except 1000\"]\n")
      touch(originals, "2009/03/01/1000.jpg")
      val picture: Claims.Resolved = one(originals, "2009/03/01/1000")
      picture.albums shouldBe Seq.empty[String]
      picture.excludedOnly shouldBe true

  it should "match a picture by path prefix and an album by its start folder" in :
    Album.needsRecursive(None) shouldBe true
    Album.needsRecursive(Some("")) shouldBe true
    Album.needsRecursive(Some("2005")) shouldBe true
    Album.needsRecursive(Some("2005/04")) shouldBe false
    Album.needsRecursive(Some("2005/04/Toronto")) shouldBe false
    Album.needsRecursive(Some("2005/04/28")) shouldBe false
    Album.pictureMatches("2005/04/28/4216", Some("2005/04")) shouldBe true
    Album.pictureMatches("2005/04/28/4216", Some("2005/04/Toronto")) shouldBe false
    Album.pictureMatches("2005/05/01/0001", Some("2005/04")) shouldBe false
    Album.pictureMatches("2005/04/29/1", Some("2005/04/28")) shouldBe false
    Album.albumMatches("2005/04", "2005/04/Toronto", Some("2005/04")) shouldBe true
    Album.albumMatches("2005/04", "2005/04/Toronto", Some("2005/04/28")) shouldBe false
    Album.albumMatches("2005/04", "2005/04/Toronto", Some("2005/04/Toronto")) shouldBe true
    Album.albumMatches("2016/07", "2016/07/Camp Menucha Rochel", Some("2016/07/19")) shouldBe false
    Album.albumMatches("2016/07", "2016/07/Camp Menucha Rochel", Some("2016/07")) shouldBe true
    Album.albumMatches("2010/07/19", "2010/07/19-08/23", Some("2010/07")) shouldBe true
    Album.albumMatches("2010/07/19", "2010/07/19-08/23", Some("2010/08")) shouldBe false

  it should "print the google title and owner before the picture line" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(
        originals,
        "2009/03/10/Party.toml",
        "external = true\ninclude = [\"all\"]\n" +
          "google-title = \" Shifra Rabiski's wedding\"\nowner = \"Ada Owner\"\n"
      )
      listLines(originals, "") shouldBe Seq(
        "album \"2009/03/10/Party\" google=\" Shifra Rabiski's wedding\" owner=\"Ada Owner\"",
        "2009/03/10/2412 albums=2009/03/10/Party"
      )

  it should "print a trailing space on a curated google title and no owner" in :
    withOriginals: originals =>
      write(originals, "Hawaii.toml", "google-title = \"Hawaii \"\n")
      listLines(originals, "") shouldBe Seq("album \"Hawaii\" google=\"Hawaii \"")

  it should "print a month google title and the path title on the picture line" in :
    withOriginals: originals =>
      write(originals, "2009/03/Trip.toml", "include = [\"all\"]\ngoogle-title = \"March trip\"\n")
      touch(originals, "2009/03/01/1000.jpg")
      listLines(originals, "") shouldBe Seq(
        "album \"2009/03/Trip\" google=\"March trip\"",
        "2009/03/01/1000 albums=2009/03/Trip"
      )

  it should "print no album line when google title and owner are absent" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val lines: Seq[String] = listLines(originals, "")
      lines shouldBe Seq("2009/03/10/2412 albums=2009/03/10/Party")
      lines.exists(_.startsWith("album ")) shouldBe false

  it should "print no album line when google title repeats the path title" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\ngoogle-title = \"2009/03/10/Party\"\n")
      listLines(originals, "") shouldBe Seq("2009/03/10/2412 albums=2009/03/10/Party")

  it should "print both albums when two albums claim one picture" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/One.toml", "include = [\"all\"]\ngoogle-title = \"One night\"\n")
      write(originals, "2009/03/10/Two.toml", "include = [\"all\"]\ngoogle-title = \"Two night\"\n")
      listLines(originals, "") shouldBe Seq(
        "album \"2009/03/10/One\" google=\"One night\"",
        "album \"2009/03/10/Two\" google=\"Two night\"",
        "2009/03/10/2412 albums=2009/03/10/One,2009/03/10/Two"
      )

  it should "escape a quote and a backslash in the google title" in :
    withOriginals: originals =>
      write(originals, "Quoted.toml", "google-title = \"say \\\"hi\\\" \\\\ now\"\n")
      listLines(originals, "") shouldBe Seq("album \"Quoted\" google=\"say \\\"hi\\\" \\\\ now\"")

  it should "filter album lines by prefix and still print an album with no selected picture" in :
    withOriginals: originals =>
      write(originals, "2009/03/Trip.toml", "google-title = \"March trip\"\n")
      write(originals, "Hawaii.toml", "google-title = \"Hawaii \"\ninclude = [\"2009/04/01 2000\"]\n")
      touch(originals, "2009/04/01/2000.jpg")
      listLines(originals, "2009/03") shouldBe Seq("album \"2009/03/Trip\" google=\"March trip\"")
      listLines(originals, "") shouldBe Seq(
        "album \"2009/03/Trip\" google=\"March trip\"",
        "album \"Hawaii\" google=\"Hawaii \"",
        "2009/04/01/2000 albums=Hawaii"
      )

  it should "match a span album from its start folder and still name it on a later picture" in :
    withOriginals: originals =>
      write(
        originals,
        "2010/07/19/to-08-23.toml",
        "include = [\"all\"]\ngoogle-title = \"July trip\"\n"
      )
      touch(originals, "2010/07/19/0001.jpg")
      touch(originals, "2010/08/01/0002.jpg")
      listLines(originals, "2010/07/19") shouldBe Seq(
        "album \"2010/07/19-08/23\" google=\"July trip\"",
        "2010/07/19/0001 albums=2010/07/19-08/23"
      )
      listLines(originals, "2010/08") shouldBe Seq("2010/08/01/0002 albums=2010/07/19-08/23")
      listLines(originals, "2010/07") shouldBe Seq(
        "album \"2010/07/19-08/23\" google=\"July trip\"",
        "2010/07/19/0001 albums=2010/07/19-08/23"
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

  private def one(originals: File, path: String): Claims.Resolved =
    Claims(originals).find(_.path == path).getOrElse(fail(s"missing $path"))

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
