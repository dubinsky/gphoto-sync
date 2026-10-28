package org.podval.tools.photo

import io.github.wasabithumb.jtoml.JToml
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files as JFiles
import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters.ListHasAsScala

class SyncTest extends AnyFlatSpec, Matchers:
  private val zone: ZoneId = ZoneId.of("America/New_York")

  "sync" should "exclude a picture removed from Best and leave the event alone" in :
    withOriginals: originals =>
      val jpeg: File = writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1, 2, 3))
      val sidecar: File = write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"media-1\"\n")
      val event: File = write(
        originals,
        "2009/03/10/Party.toml",
        "from = 2009-03-10\nname-from = \"1000\"\ngoogle-album-id = \"album-party\"\npublished-title = \"Party\"\n"
      )
      val best: File = write(
        originals,
        "Best.toml",
        "members = [\"2009/03/10/2412\", \"2009/05/01/0001\"]\nexclude = [\"other\"]\n" +
          "google-album-id = \"album-best\"\npublished-title = \"Best shots\"\n"
      )
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake(
        albums = Map("album-party" -> Set("media-1"), "album-best" -> Set.empty),
        library = Set("media-1")
      )
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      lines(out) should contain ("exclude Best 2412")
      lines(out).exists(_.startsWith("exclude 2009/03/10/Party")) shouldBe false
      lines(out).exists(_.startsWith("remove ")) shouldBe false
      fake.removed shouldBe empty
      fake.listed.toList shouldBe List("album-party", "album-best")
      without(snapshot(originals), best) shouldBe without(before, best)
      jpeg.isFile shouldBe true
      Metadata.read(sidecar).`google-media-item-id` shouldBe Some("media-1")
      Metadata.read(sidecar).`already-in-google` shouldBe None
      Files.read(sidecar) should not include ("already-in-google")
      bytes(event) shouldBe before(event.getAbsolutePath)
      val curated: Album.Curated = Album.readCurated(best, "Best")
      curated.members shouldBe Seq("2009/05/01/0001")
      curated.exclude shouldBe Set("other", "2412")
      curated.googleAlbumId shouldBe "album-best"
      curated.publishedTitle shouldBe "Best shots"
      stringArray(best, "exclude") shouldBe Seq("other", "2412")
      stringArray(best, "members") shouldBe Seq("2009/05/01/0001")

  it should "remove an excluded id that is still in Best and not rewrite the event" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"media-1\"\n")
      val event: File = write(
        originals,
        "2009/03/10/Party.toml",
        "from = 2009-03-10\ngoogle-album-id = \"album-party\"\n"
      )
      val best: File = write(
        originals,
        "Best.toml",
        "members = [\"2009/05/01/0001\"]\nexclude = [\"2412\"]\ngoogle-album-id = \"album-best\"\n"
      )
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake(
        albums = Map("album-party" -> Set("media-1"), "album-best" -> Set("media-1")),
        library = Set("media-1")
      )
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      lines(out) should contain ("remove Best 2412")
      lines(out).exists(_.startsWith("exclude ")) shouldBe false
      lines(out).exists(_.startsWith("remove 2009/03/10/Party")) shouldBe false
      fake.removed.toList shouldBe List("album-best" -> Seq("media-1"))
      fake.listed.toList should contain ("album-party")
      fake.listed.toList should contain ("album-best")
      snapshot(originals) shouldBe before
      bytes(event) shouldBe before(event.getAbsolutePath)
      bytes(best) shouldBe before(best.getAbsolutePath)
      Album.readCurated(best, "Best").exclude shouldBe Set("2412")
      Album.readCurated(best, "Best").members shouldBe Seq("2009/05/01/0001")

  it should "not list or change an external album" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](4))
      write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"media-1\"\n")
      val event: File = write(
        originals,
        "2009/03/10/Party.toml",
        "external = true\nfrom = 2009-03-10\ngoogle-album-id = \"album-ext\"\n"
      )
      val best: File = write(
        originals,
        "Best.toml",
        "members = [\"2009/03/10/2412\"]\ngoogle-album-id = \"album-best\"\n"
      )
      val beforeEvent: Seq[Byte] = bytes(event)
      val fake: Fake = Fake(
        albums = Map("album-ext" -> Set.empty, "album-best" -> Set.empty),
        library = Set("media-1")
      )
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.listed.toList shouldBe List("album-best")
      fake.removed shouldBe empty
      bytes(event) shouldBe beforeEvent
      lines(out) should contain ("exclude Best 2412")
      lines(out).exists(line => line.contains("Party")) shouldBe false
      Album.readCurated(best, "Best").exclude shouldBe Set("2412")
      Album.readCurated(best, "Best").members shouldBe Seq.empty[String]

  it should "print no albums when the only claim is external" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](4))
      val sidecar: File = write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"media-1\"\n")
      val event: File = write(
        originals,
        "2009/03/10/Party.toml",
        "external = true\nfrom = 2009-03-10\ngoogle-album-id = \"album-ext\"\n"
      )
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake(albums = Map("album-ext" -> Set.empty), library = Set("media-1"))
      var opened: Boolean = false
      val (out, thrown) = capture:
        run(originals, fake, Some("2009/03/10"), onOpen = () => opened = true)
      expectOk(thrown, out)
      opened shouldBe false
      fake.listed shouldBe empty
      fake.removed shouldBe empty
      out.trim shouldBe "no albums"
      snapshot(originals) shouldBe before
      bytes(event) shouldBe before(event.getAbsolutePath)
      bytes(sidecar) shouldBe before(sidecar.getAbsolutePath)

  it should "print the same lines on a dry run without writing or removing" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/10/2413.jpg", Array[Byte](2))
      write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"media-1\"\n")
      write(originals, "2009/03/10/2413.toml", "google-media-item-id = \"media-2\"\n")
      write(originals, "2009/03/10/Party.toml", "google-album-id = \"album-party\"\n")
      write(
        originals,
        "Best.toml",
        "members = [\"2009/03/10/2412\"]\nexclude = [\"2413\"]\ngoogle-album-id = \"album-best\"\n"
      )
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake(
        albums = Map("album-party" -> Set("media-1", "media-2"), "album-best" -> Set("media-2")),
        library = Set("media-1", "media-2")
      )
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10"), dryRun = true))
      expectOk(thrown, out)
      lines(out) should contain ("exclude Best 2412")
      lines(out) should contain ("remove Best 2413")
      fake.removed shouldBe empty
      snapshot(originals) shouldBe before

  it should "exclude every non-external claim when the id is gone from the library" in :
    withOriginals: originals =>
      val jpeg: File = writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](9))
      val sidecar: File = write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"media-1\"\n")
      val sidecarBytes: Seq[Byte] = bytes(sidecar)
      val event: File = write(
        originals,
        "2009/03/10/Party.toml",
        "from = 2009-03-10\nto = 2009-03-10T23:59:00\nname-from = \"1000\"\nname-to = \"9000\"\n" +
          "exclude = [\"9999\"]\ngoogle-album-id = \"album-party\"\npublished-title = \"Party\"\n"
      )
      val best: File = write(
        originals,
        "Best.toml",
        "members = [\"2009/03/10/2412\", \"2009/08/01/0008\"]\nexclude = [\"keep\"]\n" +
          "google-album-id = \"album-best\"\npublished-title = \"Best shots\"\n"
      )
      val shared: File = write(
        originals,
        "Shared.toml",
        "external = true\nmembers = [\"2009/03/10/2412\"]\ngoogle-album-id = \"album-shared\"\n"
      )
      val sharedBytes: Seq[Byte] = bytes(shared)
      val fake: Fake = Fake(
        albums = Map(
          "album-party" -> Set("media-1"),
          "album-best" -> Set("media-1"),
          "album-shared" -> Set("media-1")
        ),
        library = Set.empty
      )
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      lines(out) should contain ("exclude 2009/03/10/Party 2412")
      lines(out) should contain ("exclude Best 2412")
      lines(out).exists(_.contains("Shared")) shouldBe false
      lines(out).exists(_.startsWith("remove ")) shouldBe false
      fake.removed shouldBe empty
      fake.listed should not contain ("album-shared")
      jpeg.isFile shouldBe true
      bytes(sidecar) shouldBe sidecarBytes
      Metadata.read(sidecar).`google-media-item-id` shouldBe Some("media-1")
      Metadata.read(sidecar).`already-in-google` shouldBe None
      bytes(shared) shouldBe sharedBytes
      val party: Album.Event = Album.readEvent(event, "2009/03/10/Party", LocalDate.of(2009, 3, 10))
      party.from match
        case Some(Album.When.Date(date)) => date.toString shouldBe "2009-03-10"
        case other => fail(s"from was $other")
      party.to match
        case Some(Album.When.DateTime(value)) =>
          value.toString shouldBe LocalDateTime.parse("2009-03-10T23:59:00").toString
        case other => fail(s"to was $other")
      party.nameFrom shouldBe Some("1000")
      party.nameTo shouldBe Some("9000")
      party.googleAlbumId shouldBe "album-party"
      party.publishedTitle shouldBe "Party"
      party.external shouldBe false
      stringArray(event, "exclude") shouldBe Seq("9999", "2412")
      val curated: Album.Curated = Album.readCurated(best, "Best")
      curated.members shouldBe Seq("2009/08/01/0008")
      curated.googleAlbumId shouldBe "album-best"
      curated.publishedTitle shouldBe "Best shots"
      stringArray(best, "exclude") shouldBe Seq("keep", "2412")

  it should "reject a year or the root without --recursive and write nothing" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"media-1\"\n")
      write(originals, "2009/03/10/Party.toml", "google-album-id = \"album-party\"\n")
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake(albums = Map("album-party" -> Set.empty), library = Set("media-1"))
      var opened: Boolean = false
      val (yearOut, yearThrown) = capture:
        run(originals, fake, Some("2009"), recursive = false, onOpen = () => opened = true)
      yearThrown.get shouldBe a[IllegalArgumentException]
      yearThrown.get.getMessage shouldBe "year or root requires --recursive"
      yearOut.trim shouldBe ""
      val (rootOut, rootThrown) = capture:
        run(originals, fake, None, recursive = false, onOpen = () => opened = true)
      rootThrown.get shouldBe a[IllegalArgumentException]
      rootThrown.get.getMessage shouldBe "year or root requires --recursive"
      rootOut.trim shouldBe ""
      opened shouldBe false
      fake.listed shouldBe empty
      fake.removed shouldBe empty
      snapshot(originals) shouldBe before

  it should "select a day when the prefix has a leading slash" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/11/2413.jpg", Array[Byte](2))
      write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"media-1\"\n")
      write(originals, "2009/03/11/2413.toml", "google-media-item-id = \"media-2\"\n")
      write(originals, "2009/03/10/Party.toml", "google-album-id = \"album-party\"\n")
      val other: File = write(originals, "2009/03/11/Other.toml", "google-album-id = \"album-other\"\n")
      val otherBytes: Seq[Byte] = bytes(other)
      val fake: Fake = Fake(
        albums = Map("album-party" -> Set.empty, "album-other" -> Set.empty),
        library = Set("media-1", "media-2")
      )
      val (out, thrown) = capture(run(originals, fake, Some("/2009/03/10")))
      expectOk(thrown, out)
      lines(out) should contain ("exclude 2009/03/10/Party 2412")
      lines(out).exists(_.contains("2009/03/11")) shouldBe false
      lines(out).exists(_.contains("2413")) shouldBe false
      fake.listed.toList shouldBe List("album-party")
      fake.removed shouldBe empty
      bytes(other) shouldBe otherBytes
      Album.readEvent(File(originals, "2009/03/10/Party.toml"), "2009/03/10/Party", LocalDate.of(2009, 3, 10))
        .exclude shouldBe Set("2412")

  it should "remove excluded ids in batches of 50" in :
    withOriginals: originals =>
      val names: Seq[String] = (0 until 51).map(n => f"$n%04d")
      names.foreach: name =>
        writeBytes(originals, s"2009/03/10/$name.jpg", Array[Byte](1))
        write(originals, s"2009/03/10/$name.toml", s"google-media-item-id = \"id-$name\"\n")
      val excluded: String = names.map(name => s"\"$name\"").mkString("exclude = [", ", ", "]\n")
      val best: File = write(originals, "Best.toml", excluded + "google-album-id = \"album-best\"\n")
      val event: File = write(originals, "2009/03/10/Party.toml", "google-album-id = \"album-party\"\n")
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val ids: Set[String] = names.map(name => s"id-$name").toSet
      val fake: Fake = Fake(
        albums = Map("album-party" -> ids, "album-best" -> ids),
        library = ids
      )
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.removed.map(_._1).distinct.toList shouldBe List("album-best")
      fake.removed.map(_._2.size).toList shouldBe List(50, 1)
      fake.removed.forall(_._2.sizeIs <= 50) shouldBe true
      fake.removed.flatMap(_._2).toSet shouldBe ids
      lines(out).count(_.startsWith("remove Best ")) shouldBe 51
      lines(out).exists(_.startsWith("exclude ")) shouldBe false
      snapshot(originals) shouldBe before
      bytes(best) shouldBe before(best.getAbsolutePath)
      bytes(event) shouldBe before(event.getAbsolutePath)
      Metadata.read(File(originals, "2009/03/10/0050.toml")).`google-media-item-id` shouldBe Some("id-0050")

  it should "print a claims conflict and write nothing" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"media-1\"\n")
      write(originals, "2009/03/10/Dinner.toml", "google-album-id = \"album-dinner\"\n")
      write(originals, "2009/03/10/Party.toml", "google-album-id = \"album-party\"\n")
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake(albums = Map.empty, library = Set("media-1"))
      var opened: Boolean = false
      val (out, thrown) = capture:
        run(originals, fake, Some("2009/03/10"), onOpen = () => opened = true)
      thrown.get shouldBe a[IllegalArgumentException]
      opened shouldBe false
      fake.listed shouldBe empty
      fake.removed shouldBe empty
      out should include ("2009/03/10/2412")
      out should include ("claimed by")
      out should include ("2009/03/10/Dinner")
      out should include ("2009/03/10/Party")
      snapshot(originals) shouldBe before

  it should "print the picture path and album title when listing fails" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val sidecar: File = write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"media-1\"\n")
      val best: File = write(
        originals,
        "Best.toml",
        "members = [\"2009/03/10/2412\"]\ngoogle-album-id = \"album-best\"\n"
      )
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake(
        albums = Map("album-best" -> Set.empty),
        library = Set("media-1"),
        failList = Some("album-best")
      )
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      thrown.get shouldBe a[IllegalArgumentException]
      out should include ("2009/03/10/2412 Best: list failed")
      fake.removed shouldBe empty
      snapshot(originals) shouldBe before
      bytes(best) shouldBe before(best.getAbsolutePath)
      bytes(sidecar) shouldBe before(sidecar.getAbsolutePath)

  it should "print remove lines before a failed batchRemove" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val sidecar: File = write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"media-1\"\n")
      val best: File = write(
        originals,
        "Best.toml",
        "exclude = [\"2412\"]\ngoogle-album-id = \"album-best\"\n"
      )
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake(
        albums = Map("album-best" -> Set("media-1")),
        library = Set("media-1"),
        failRemove = true
      )
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      thrown.get shouldBe a[IllegalArgumentException]
      val removeAt: Int = out.indexOf("remove Best 2412")
      val errorAt: Int = out.indexOf("2009/03/10/2412 Best: remove failed")
      removeAt should be >= 0
      errorAt should be > removeAt
      fake.removed shouldBe empty
      snapshot(originals) shouldBe before
      bytes(best) shouldBe before(best.getAbsolutePath)
      Metadata.read(sidecar).`google-media-item-id` shouldBe Some("media-1")

  it should "remove an event exclude for every folder day inside from/to" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/09/2420.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/11/2420.jpg", Array[Byte](2))
      writeBytes(originals, "2009/03/13/2420.jpg", Array[Byte](3))
      write(originals, "2009/03/09/2420.toml", "google-media-item-id = \"media-before\"\n")
      write(originals, "2009/03/11/2420.toml", "google-media-item-id = \"media-span\"\n")
      write(originals, "2009/03/13/2420.toml", "google-media-item-id = \"media-after\"\n")
      val purim: File = write(
        originals,
        "2009/03/10/Purim Seuda.toml",
        "to = 2009-03-12\nexclude = [\"2420\"]\ngoogle-album-id = \"album-purim\"\n"
      )
      val day: File = write(originals, "2009/03/11.toml", "google-album-id = \"album-day\"\n")
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake(
        albums = Map(
          "album-purim" -> Set("media-before", "media-span", "media-after"),
          "album-day" -> Set("media-span")
        ),
        library = Set("media-before", "media-span", "media-after")
      )
      val (out, thrown) = capture(run(originals, fake, Some("2009/03")))
      expectOk(thrown, out)
      lines(out) shouldBe Seq("remove 2009/03/10/Purim Seuda 2420")
      fake.removed.toList shouldBe List("album-purim" -> Seq("media-span"))
      fake.listed.toList shouldBe List("album-purim", "album-day")
      snapshot(originals) shouldBe before
      bytes(purim) shouldBe before(purim.getAbsolutePath)
      bytes(day) shouldBe before(day.getAbsolutePath)

  it should "not remove a curated name that is still listed in members" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2011/06/29/2412.jpg", Array[Byte](2))
      write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"media-2009\"\n")
      write(originals, "2011/06/29/2412.toml", "google-media-item-id = \"media-2011\"\n")
      val best: File = write(
        originals,
        "Best.toml",
        "members = [\"2011/06/29/2412\"]\nexclude = [\"2412\"]\ngoogle-album-id = \"album-best\"\n"
      )
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake(
        albums = Map("album-best" -> Set("media-2009", "media-2011")),
        library = Set("media-2009", "media-2011")
      )
      val (out, thrown) = capture(run(originals, fake, None, recursive = true))
      expectOk(thrown, out)
      lines(out) shouldBe Seq("remove Best 2412")
      fake.removed.toList shouldBe List("album-best" -> Seq("media-2009"))
      snapshot(originals) shouldBe before
      bytes(best) shouldBe before(best.getAbsolutePath)
      Album.readCurated(best, "Best").members shouldBe Seq("2011/06/29/2412")
      Album.readCurated(best, "Best").exclude shouldBe Set("2412")

  it should "remove an excluded id that is still in a labeled month" in :
    withOriginals: originals =>
      val stem: String = "1000"
      writeBytes(originals, s"2009/03/01/$stem.jpg", Array[Byte](1))
      write(originals, s"2009/03/01/$stem.toml", "google-media-item-id = \"media-1\"\n")
      val month: File = write(
        originals,
        "2009/03/Trip.toml",
        s"exclude = [\"$stem\"]\ngoogle-album-id = \"album-trip\"\n"
      )
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake(
        albums = Map("album-trip" -> Set("media-1")),
        library = Set("media-1")
      )
      val (out, thrown) = capture(run(originals, fake, Some("2009/03")))
      expectOk(thrown, out)
      lines(out) shouldBe Seq(s"remove 2009/03/Trip $stem")
      fake.removed.toList shouldBe List("album-trip" -> Seq("media-1"))
      snapshot(originals) shouldBe before
      Album.readMonth(month, "2009/03/Trip").exclude shouldBe Set(stem)

  it should "remove an excluded id that is still in a plain month" in :
    withOriginals: originals =>
      val stem: String = "1000"
      writeBytes(originals, s"2009/03/01/$stem.jpg", Array[Byte](1))
      write(originals, s"2009/03/01/$stem.toml", "google-media-item-id = \"media-1\"\n")
      val month: File = write(
        originals,
        "2009/03.toml",
        s"exclude = [\"$stem\"]\ngoogle-album-id = \"album-month\"\n"
      )
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake(
        albums = Map("album-month" -> Set("media-1")),
        library = Set("media-1")
      )
      val (out, thrown) = capture(run(originals, fake, Some("2009/03")))
      expectOk(thrown, out)
      lines(out) shouldBe Seq(s"remove 2009/03 $stem")
      fake.removed.toList shouldBe List("album-month" -> Seq("media-1"))
      snapshot(originals) shouldBe before
      Album.readMonth(month, "2009/03").exclude shouldBe Set(stem)

  private def run(
    originals: File,
    fake: Fake,
    prefix: Option[String],
    recursive: Boolean = false,
    dryRun: Boolean = false,
    onOpen: () => Unit = () => ()
  ): Unit =
    Sync.run(
      originals = originals,
      zone = zone,
      prefix = prefix,
      recursive = recursive,
      dryRun = dryRun,
      google = () =>
        onOpen()
        fake
    )

  private def expectOk(thrown: Option[Throwable], out: String): Unit = thrown match
    case Some(error: org.scalatest.exceptions.TestFailedException) => throw error
    case Some(error) => fail(s"$error\n$out")
    case None => ()

  private def capture(body: => Unit): (String, Option[Throwable]) =
    val buffer: ByteArrayOutputStream = new ByteArrayOutputStream
    val thrown: Option[Throwable] =
      try
        Console.withOut(buffer)(body)
        None
      catch case error: Throwable => Some(error)
    (buffer.toString(StandardCharsets.UTF_8), thrown)

  private def lines(out: String): Seq[String] = out.split('\n').toSeq.filter(_.nonEmpty)

  private def without(files: Map[String, Seq[Byte]], skip: File): Map[String, Seq[Byte]] =
    files.filterNot(_._1 == skip.getAbsolutePath)

  private def stringArray(file: File, key: String): Seq[String] =
    val table = JToml.jToml.readFromString(Files.read(file))
    table.get(key).asArray.asScala.toSeq.map(_.asPrimitive.asString)

  private def snapshot(root: File): Map[String, Seq[Byte]] =
    def walk(file: File): Seq[File] =
      if !file.exists() then Seq.empty
      else if file.isDirectory then Option(file.listFiles).toSeq.flatten.flatMap(walk)
      else Seq(file)
    walk(root).map(file => file.getAbsolutePath -> bytes(file)).toMap

  private def bytes(file: File): Seq[Byte] =
    if file.isFile then JFiles.readAllBytes(file.toPath).toSeq else Seq.empty

  private def write(originals: File, path: String, content: String): File =
    val file: File = File(originals, path)
    file.getParentFile.mkdirs()
    Files.write(content, file)
    file

  private def writeBytes(originals: File, path: String, content: Array[Byte]): File =
    val file: File = File(originals, path)
    file.getParentFile.mkdirs()
    JFiles.write(file.toPath, content)
    file

  private def withOriginals(body: File => Unit): Unit =
    val root: File = JFiles.createTempDirectory("sync").toFile
    val originals: File = File(root, "originals")
    originals.mkdirs()
    try body(originals)
    finally deleteTree(root)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()

  private final class Fake(
    albums: Map[String, Set[String]],
    library: Set[String],
    failList: Option[String] = None,
    failRemove: Boolean = false
  ) extends Sync.Library:
    val listed: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer.empty
    val removed: scala.collection.mutable.ArrayBuffer[(String, Seq[String])] =
      scala.collection.mutable.ArrayBuffer.empty

    override def listAlbum(albumId: String): Set[String] =
      listed += albumId
      if failList.contains(albumId) then throw IllegalStateException("list failed")
      albums.getOrElse(albumId, Set.empty)

    override def libraryIds(ids: Seq[String]): Set[String] =
      ids.filter(library.contains).toSet

    override def removeFromAlbum(albumId: String, ids: Seq[String]): Unit =
      if failRemove then throw IllegalStateException("remove failed")
      removed += ((albumId, ids))
