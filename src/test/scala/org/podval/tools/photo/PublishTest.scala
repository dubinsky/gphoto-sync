package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files as JFiles
import java.time.LocalDate

class PublishTest extends AnyFlatSpec, Matchers:
  "publish" should "reject a year or the root without --recursive and write nothing" in :
    withOriginals: originals =>
      val jpeg: File = writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1, 2, 3))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake()
      var opened: Boolean = false
      val (yearOut, yearThrown) = capture:
        run(originals, fake, Some("2009"), recursive = false, onOpen = () => opened = true)
      yearThrown.get shouldBe a[IllegalArgumentException]
      yearThrown.get.getMessage shouldBe "year or root requires --recursive"
      yearOut should not include ("uploaded=")
      opened shouldBe false
      val (rootOut, rootThrown) = capture:
        run(originals, fake, None, recursive = false, onOpen = () => opened = true)
      rootThrown.get shouldBe a[IllegalArgumentException]
      rootThrown.get.getMessage shouldBe "year or root requires --recursive"
      rootOut should not include ("uploaded=")
      opened shouldBe false
      snapshot(originals) shouldBe before
      jpeg.isFile shouldBe true
      fake.uploads shouldBe empty

  it should "accept --recursive on a year" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009"), recursive = true))
      expectOk(thrown, out)
      fake.uploads should have size 1
      out should include ("uploaded=1")

  it should "upload one jpeg into an event and Best" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1, 2))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      write(originals, "Best.toml", "include = [\"2009/03/10 2412\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads should have size 1
      fake.batchCreates should have size 1
      fake.batchCreates.head should have size 1
      fake.adds should have size 2
      val id: String = metadata(originals, "2009/03/10/2412").`google-media-item-id`.getOrElse(fail("no media id"))
      fake.adds.flatMap(_._2) shouldBe Seq(id, id)
      fake.adds.map(_._1).distinct should have size 2
      val event: Album = Album.read(originals, File(originals, "2009/03/10/Party.toml"))
      event.googleAlbumId.nonEmpty shouldBe true
      event.publishedTitle shouldBe "2009/03/10/Party"
      val best: Album = Album.read(originals, File(originals, "Best.toml"))
      best.include.map(Include.logical) shouldBe Seq("2009/03/10 2412")
      best.googleAlbumId.nonEmpty shouldBe true
      best.publishedTitle shouldBe "Best"
      out should include ("uploaded=1 filed=2 skipped-excluded=0 skipped-external=0 skipped-exists=0 failed=0")

  it should "not upload a picture claimed by an external album" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val eventFile: File = write(originals, "2009/03/10/Party.toml", "external = true\ninclude = [\"all\"]\n")
      val eventBytes: Seq[Byte] = bytes(eventFile)
      val bestFile: File = write(originals, "Best.toml", "include = [\"2009/03/10 2412\"]\n")
      val bestBytes: Seq[Byte] = bytes(bestFile)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads shouldBe empty
      fake.batchCreates shouldBe empty
      fake.adds shouldBe empty
      bytes(eventFile) shouldBe eventBytes
      bytes(bestFile) shouldBe bestBytes
      File(originals, "2009/03/10/2412.toml").isFile shouldBe false
      out should include ("skipped-external=1")
      out should include ("uploaded=0")
      out should include ("filed=0")

  it should "upload a timestamped jpeg from the exiftool temp copy" in :
    withOriginals: originals =>
      val payload: Array[Byte] = Array[Byte](9, 8, 7, 6)
      val source: File = writeBytes(originals, "2009/03/10/2412.jpg", payload)
      write(originals, "2009/03/10/2412.toml", "timestamp = \"2009-03-10T19:00\"\n")
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      var argv: Seq[String] = Seq.empty
      val (out, thrown) = capture:
        run(
          originals,
          fake,
          Some("2009/03/10/2412"),
          exiftool = args =>
            argv = args
            Right(())
        )
      expectOk(thrown, out)
      argv.head shouldBe ExifOverride.exiftool
      fake.uploads should have size 1
      val uploaded: File = fake.uploads.head
      uploaded.getPath should not be source.getPath
      argv.lift(2) shouldBe Some(uploaded.getPath)
      JFiles.readAllBytes(source.toPath).toSeq shouldBe payload.toSeq
      metadata(originals, "2009/03/10/2412").timestamp shouldBe Some("2009-03-10T19:00")

  it should "upload a timestamped video from the exiftool temp copy" in :
    withOriginals: originals =>
      val payload: Array[Byte] = Array[Byte](5, 4, 3)
      val source: File = writeBytes(originals, "2009/03/10/1000.mov", payload)
      write(originals, "2009/03/10/1000.toml", "timestamp = \"2009-03-10T19:00\"\n")
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      var argv: Seq[String] = Seq.empty
      val (out, thrown) = capture:
        run(
          originals,
          fake,
          Some("2009/03/10/1000"),
          exiftool = args =>
            argv = args
            Right(())
        )
      expectOk(thrown, out)
      argv should contain (s"-CreateDate=2009-03-10T19:00")
      fake.uploads should have size 1
      val uploaded: File = fake.uploads.head
      uploaded.getPath should not be source.getPath
      uploaded.getName.endsWith(".mov") shouldBe true
      argv should contain (uploaded.getPath)
      JFiles.readAllBytes(source.toPath).toSeq shouldBe payload.toSeq

  it should "fail a video that has an orientation override and not upload it" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/1000.mov", Array[Byte](1, 2))
      write(originals, "2009/03/10/1000.toml", "orientation = \"Rotate 90 CW\"\n")
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      thrown.get shouldBe a[IllegalArgumentException]
      fake.uploads shouldBe empty
      fake.batchCreates shouldBe empty
      out should include ("2009/03/10/1000")
      out should include ("2009/03/10/Party")
      out should include ("failed=1")
      val sidecar: String = Files.read(File(originals, "2009/03/10/1000.toml"))
      sidecar should not include ("google-media-item-id")
      sidecar should not include ("already-in-google")

  it should "upload a video with no override as itself" in :
    withOriginals: originals =>
      val video: File = writeBytes(originals, "2009/03/10/1000.mov", Array[Byte](3, 4))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads.map(_.getPath).toSeq shouldBe Seq(video.getPath)
      fake.batchCreates should have size 1
      out should include ("uploaded=1")

  it should "file a stored media id without batchCreate" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(originals, "2009/03/10/2412.toml", "google-media-item-id = \"existing-id\"\n")
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads shouldBe empty
      fake.batchCreates shouldBe empty
      fake.adds should have size 1
      fake.adds.head._2 shouldBe Seq("existing-id")
      out should include ("skipped-exists=1")
      out should include ("uploaded=0")
      out should include ("filed=1")

  it should "not upload or file a picture already in Google" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val sidecar: File = write(originals, "2009/03/10/2412.toml", "already-in-google = true\n")
      val sidecarBytes: Seq[Byte] = bytes(sidecar)
      val album: File = write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val albumBytes: Seq[Byte] = bytes(album)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads shouldBe empty
      fake.batchCreates shouldBe empty
      fake.adds shouldBe empty
      bytes(sidecar) shouldBe sidecarBytes
      bytes(album) shouldBe albumBytes
      out should include ("skipped-exists=1")
      out should include ("filed=0")

  it should "not call Google or write files on a dry run" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1, 2, 3))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      write(originals, "Best.toml", "include = [\"2009/03/10 2412\"]\n")
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val fake: Fake = Fake()
      var opened: Boolean = false
      val (out, thrown) = capture:
        run(originals, fake, Some("2009/03/10"), dryRun = true, onOpen = () => opened = true)
      expectOk(thrown, out)
      opened shouldBe false
      fake.uploads shouldBe empty
      fake.batchCreates shouldBe empty
      fake.adds shouldBe empty
      fake.createdAlbums shouldBe empty
      snapshot(originals) shouldBe before
      out should include ("2009/03/10/2412 albums=2009/03/10/Party,Best")
      out should include ("2009/03/10/Party")
      out should include ("Best")
      out should not include ("google ")
      out should not include ("uploaded=")

  it should "not count an unclaimed picture as excluded" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1, 2))
      writeBytes(originals, "2009/03/10/1000.jpg", Array[Byte](3))
      write(originals, "2009/03/10/Party.toml", "include = [\"2009/03/10 2412\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads should have size 1
      out should include ("uploaded=1 filed=1 skipped-excluded=0 skipped-external=0 skipped-exists=0 failed=0")
      out should not include ("2009/03/10/1000")

  it should "count a picture excluded from its only album and not upload it" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(originals, "2009/03/10/Party.toml", "include = [\"all except 2412\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads shouldBe empty
      fake.adds shouldBe empty
      out should include ("skipped-excluded=1")
      out should include ("uploaded=0")
      out should not include ("no albums")

  it should "report no albums when nothing is claimed" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val fake: Fake = Fake()
      var opened: Boolean = false
      val (out, thrown) = capture:
        run(originals, fake, Some("2009/03/10"), onOpen = () => opened = true)
      expectOk(thrown, out)
      opened shouldBe false
      out should include ("no albums")
      out should not include ("uploaded=")
      fake.uploads shouldBe empty

  it should "fail when batchCreate returns no media item id" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val sidecar: File = write(originals, "2009/03/10/2412.toml", "timestamp = \"2009-03-10T19:00\"\n")
      val before: Seq[Byte] = bytes(sidecar)
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake(missingId = true)
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      thrown.get shouldBe a[IllegalArgumentException]
      bytes(sidecar) shouldBe before
      val text: String = Files.read(sidecar)
      text should not include ("google-media-item-id")
      text should not include ("already-in-google")
      out should include ("batchCreate returned no media item id")
      out should not include ("batchCreate returned no media item id:")
      out should include ("failed=1")
      fake.uploads should have size 1
      fake.batchCreates should have size 1

  it should "write the album id before uploading" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val album: File = write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      fake.onUpload = _ =>
        val event: Album = Album.read(originals, album)
        event.googleAlbumId.nonEmpty shouldBe true
        event.publishedTitle shouldBe "2009/03/10/Party"
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads should have size 1

  it should "not upload a gif or a raw file" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/1000.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/10/1000.cr2", Array[Byte](2))
      writeBytes(originals, "2009/03/10/1001.gif", Array[Byte](3))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads.map(_.getName).toSeq shouldBe Seq("1000.jpg")
      out should include ("uploaded=1")

  it should "select claim paths when the prefix has a leading slash" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/11/2413.jpg", Array[Byte](2))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      write(originals, "2009/03/11/Other.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("/2009/03/10")))
      expectOk(thrown, out)
      fake.uploads.map(_.getName).toSeq shouldBe Seq("2412.jpg")
      out should include ("uploaded=1")

  it should "fail a misnamed upload file and not publish the album" in :
    withOriginals: originals =>
      val img: File = writeBytes(originals, "2009/03/10/IMG_2412.jpg", Array[Byte](1))
      val upper: File = writeBytes(originals, "2009/03/10/2413.JPG", Array[Byte](2))
      val album: File = write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val before: Seq[Byte] = bytes(album)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      img.getName shouldBe "IMG_2412.jpg"
      upper.getName shouldBe "2413.JPG"
      File(originals, "2009/03/10/2412.jpg").isFile shouldBe false
      File(originals, "2009/03/10/2413.jpg").isFile shouldBe false
      bytes(album) shouldBe before
      fake.uploads shouldBe empty
      fake.createdAlbums shouldBe empty
      out should include ("2009/03/10/2412 2009/03/10/Party: IMG_2412.jpg is not the literal file")
      out should include ("2009/03/10/2413 2009/03/10/Party: 2413.JPG is not the literal file")
      out should include ("failed=2")

  it should "fail an already-literal non-digit name" in :
    withOriginals: originals =>
      val picture: File = writeBytes(originals, "2009/03/10/Screenshot_1.jpg", Array[Byte](1))
      val album: File = write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val before: Seq[Byte] = bytes(album)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      picture.getName shouldBe "Screenshot_1.jpg"
      bytes(album) shouldBe before
      fake.uploads shouldBe empty
      fake.createdAlbums shouldBe empty
      out should include ("2009/03/10/Screenshot_1 2009/03/10/Party: Screenshot_1 does not start with a digit")
      out should not include ("NOT RENAMED")
      out should not include ("WRONG NAME")
      out should not include ("is not the literal file")
      out should include ("failed=1")

  it should "fail a misnamed VID upload and not rename it" in :
    withOriginals: originals =>
      val vid: File = writeBytes(originals, "2009/03/10/VID_0063.MP4", Array[Byte](1))
      val album: File = write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val before: Seq[Byte] = bytes(album)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      vid.exists shouldBe true
      vid.getName shouldBe "VID_0063.MP4"
      File(originals, "2009/03/10/0063.mp4").isFile shouldBe false
      bytes(album) shouldBe before
      fake.uploads shouldBe empty
      fake.createdAlbums shouldBe empty
      out should include ("2009/03/10/0063 2009/03/10/Party: VID_0063.MP4 is not the literal file")
      out should include ("failed=1")

  it should "leave a raw gif thm or tif picture unpublished" in :
    withOriginals: originals =>
      val gif: File = writeBytes(originals, "2009/03/10/IMG_1001.gif", Array[Byte](1))
      writeBytes(originals, "2009/03/10/1002.cr2", Array[Byte](2))
      writeBytes(originals, "2009/03/10/1003.thm", Array[Byte](3))
      writeBytes(originals, "2009/03/10/1004.tif", Array[Byte](4))
      val album: File = write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val before: Seq[Byte] = bytes(album)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      gif.getName shouldBe "IMG_1001.gif"
      bytes(album) shouldBe before
      fake.uploads shouldBe empty
      fake.createdAlbums shouldBe empty
      out should include ("uploaded=0 filed=0 skipped-excluded=0 skipped-external=0 skipped-exists=0 failed=0")

  it should "write each batch of media ids before uploading the next files" in :
    withOriginals: originals =>
      val names: Seq[String] = (0 until 51).map(n => f"$n%04d")
      names.foreach(name => writeBytes(originals, s"2009/03/10/$name.jpg", Array[Byte](1)))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake(failCreateAfter = Some(1))
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      thrown.get shouldBe a[IllegalArgumentException]
      val first: Seq[String] = Seq.fill(50)("upload") ++ Seq("batchCreate", "owns")
      fake.events.toSeq shouldBe first ++ Seq("upload", "batchCreate")
      fake.batchCreates.map(_.length).toSeq shouldBe Seq(50, 1)
      names.take(50).foreach: name =>
        metadata(originals, s"2009/03/10/$name").`google-media-item-id`.nonEmpty shouldBe true
      File(originals, "2009/03/10/0050.toml").isFile shouldBe false
      out should include ("uploaded=50")
      out should include ("failed=1")
      out should include ("2009/03/10/0050")

  it should "include the batchCreate status when no media item id is returned" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val sidecar: File = write(originals, "2009/03/10/2412.toml", "timestamp = \"2009-03-10T19:00\"\n")
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake(missingId = true, statusMessage = Some("expired upload token"))
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      thrown.get shouldBe a[IllegalArgumentException]
      val text: String = Files.read(sidecar)
      text should not include ("google-media-item-id")
      text should not include ("already-in-google")
      out should include ("batchCreate returned no media item id: expired upload token")
      out should include ("failed=1")

  it should "mark a batchCreate id this client cannot see as already in Google" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val sidecar: File = write(originals, "2009/03/10/2412.toml", "timestamp = \"2009-03-10T19:00\"\n")
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake(unowned = true)
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      val metadata: Metadata = Metadata.read(sidecar)
      metadata.`google-media-item-id` shouldBe None
      metadata.`already-in-google` shouldBe Some(true)
      metadata.timestamp.nonEmpty shouldBe true
      fake.adds shouldBe empty
      fake.events.toSeq should contain ("owns")
      out should include ("uploaded=0")
      out should include ("skipped-exists=1")
      out should include ("failed=0")

  it should "publish a labeled month without --recursive" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/01/1000.jpg", Array[Byte](1))
      write(originals, "2009/03/Trip.toml", "include = [\"all\"]\n")
      writeBytes(originals, "2009/04/02/2000.jpg", Array[Byte](2))
      write(originals, "2009/04/album.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03"), recursive = false))
      expectOk(thrown, out)
      fake.createdAlbums.toSeq shouldBe Seq("2009/03/Trip")
      fake.uploads.map(_.getName).toSeq shouldBe Seq("1000.jpg")
      out should not include ("year or root requires --recursive")

  it should "rename to the path title when google-title is absent" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(
        originals,
        "2009/03/10/Party.toml",
        "include = [\"all\"]\ngoogle-album-id = \"album-party\"\npublished-title = \"Party\"\n"
      )
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.createdAlbums shouldBe empty
      fake.updatedAlbums.toSeq shouldBe Seq("album-party" -> "2009/03/10/Party")
      val event: Album = readAlbum(originals, "2009/03/10/Party")
      event.title shouldBe "2009/03/10/Party"
      event.publishedTitle shouldBe "2009/03/10/Party"
      event.googleTitle shouldBe ""

  it should "create an album at google-title and keep that title on the file" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\ngoogle-title = \"Party night\"\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.createdAlbums.toSeq shouldBe Seq("Party night")
      fake.updatedAlbums shouldBe empty
      val event: Album = readAlbum(originals, "2009/03/10/Party")
      event.title shouldBe "2009/03/10/Party"
      event.publishedTitle shouldBe "Party night"
      event.googleTitle shouldBe "Party night"
      out should include ("failed=0")

  it should "send a leading space to createAlbum and keep it on the file" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\ngoogle-title = \" Party night\"\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.createdAlbums.toSeq shouldBe Seq(" Party night")
      fake.updatedAlbums shouldBe empty
      val event: Album = readAlbum(originals, "2009/03/10/Party")
      event.googleTitle shouldBe " Party night"
      event.publishedTitle shouldBe " Party night"

  it should "rename a stored album to google-title" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(
        originals,
        "2009/03/10/Party.toml",
        "include = [\"all\"]\ngoogle-album-id = \"album-party\"\n" +
          "published-title = \"2009/03/10/Party\"\ngoogle-title = \"Party night\"\n"
      )
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.createdAlbums shouldBe empty
      fake.updatedAlbums.toSeq shouldBe Seq("album-party" -> "Party night")
      val event: Album = readAlbum(originals, "2009/03/10/Party")
      event.publishedTitle shouldBe "Party night"
      event.googleTitle shouldBe "Party night"
      event.googleAlbumId shouldBe "album-party"

  it should "not rename when published-title is already the desired title" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val album: File = write(
        originals,
        "2009/03/10/Party.toml",
        "include = [\"all\"]\ngoogle-album-id = \"album-party\"\n" +
          "published-title = \"Party night\"\ngoogle-title = \"Party night\"\n"
      )
      val before: Seq[Byte] = bytes(album)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.createdAlbums shouldBe empty
      fake.updatedAlbums shouldBe empty
      bytes(album) shouldBe before

  it should "rewrite an empty id even when published-title is already desired" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val album: File = write(
        originals,
        "2009/03/10/Party.toml",
        "include = [\"all\"]\npublished-title = \"Party night\"\ngoogle-title = \"Party night\"\n"
      )
      val before: Seq[Byte] = bytes(album)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.createdAlbums.toSeq shouldBe Seq("Party night")
      fake.updatedAlbums shouldBe empty
      bytes(album) should not be before
      val event: Album = readAlbum(originals, "2009/03/10/Party")
      event.googleAlbumId.nonEmpty shouldBe true
      event.publishedTitle shouldBe "Party night"
      event.googleTitle shouldBe "Party night"

  it should "adopt the remote album titled with google-title" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\ngoogle-title = \"Party night\"\n")
      val fake: Fake = Fake(initialRemote =
        Seq("album-path" -> "2009/03/10/Party", "album-night" -> "Party night")
      )
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.createdAlbums shouldBe empty
      fake.updatedAlbums shouldBe empty
      fake.remoteAlbums shouldBe Seq("album-path" -> "2009/03/10/Party", "album-night" -> "Party night")
      val event: Album = readAlbum(originals, "2009/03/10/Party")
      event.googleAlbumId shouldBe "album-night"
      event.publishedTitle shouldBe "Party night"
      event.title shouldBe "2009/03/10/Party"

  it should "fail every album that desires the same title and still create a different one" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/11/2413.jpg", Array[Byte](2))
      writeBytes(originals, "2009/03/12/2414.jpg", Array[Byte](3))
      val party: File = write(
        originals,
        "2009/03/10/Party.toml",
        "include = [\"all\"]\ngoogle-album-id = \"album-party\"\npublished-title = \"Old\"\ngoogle-title = \"Party night\"\n"
      )
      val other: File = write(
        originals,
        "2009/03/11/Other.toml",
        "include = [\"all\"]\ngoogle-title = \"Party night\"\n"
      )
      write(originals, "2009/03/12/Third.toml", "include = [\"all\"]\ngoogle-title = \"Third night\"\n")
      val partyBefore: Seq[Byte] = bytes(party)
      val otherBefore: Seq[Byte] = bytes(other)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      fake.createdAlbums.toSeq shouldBe Seq("Third night")
      fake.createdAlbums should not contain ("Party night")
      fake.updatedAlbums shouldBe empty
      bytes(party) shouldBe partyBefore
      bytes(other) shouldBe otherBefore
      out should include ("2009/03/10/Party")
      out should include ("2009/03/11/Other")
      out should include ("also desired")
      out should include ("failed=")

  it should "fail two stored albums that desire the same title without renaming" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/11/2413.jpg", Array[Byte](2))
      val party: File = write(
        originals,
        "2009/03/10/Party.toml",
        "include = [\"all\"]\ngoogle-album-id = \"album-party\"\npublished-title = \"Keep party\"\ngoogle-title = \"Party night\"\n"
      )
      val other: File = write(
        originals,
        "2009/03/11/Other.toml",
        "include = [\"all\"]\ngoogle-album-id = \"album-other\"\npublished-title = \"Keep other\"\ngoogle-title = \"Party night\"\n"
      )
      val partyBefore: Seq[Byte] = bytes(party)
      val otherBefore: Seq[Byte] = bytes(other)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      fake.createdAlbums shouldBe empty
      fake.updatedAlbums shouldBe empty
      bytes(party) shouldBe partyBefore
      bytes(other) shouldBe otherBefore
      out should include ("2009/03/10/Party")
      out should include ("2009/03/11/Other")
      out should include ("failed=")

  it should "not let an external google title block a client album" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\ngoogle-title = \"Party night\"\n")
      val shared: File = write(
        originals,
        "Shared.toml",
        "external = true\ngoogle-title = \"Party night\"\ninclude = [\"1999/01/01 0000\"]\n"
      )
      val sharedBefore: Seq[Byte] = bytes(shared)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.createdAlbums.toSeq shouldBe Seq("Party night")
      bytes(shared) shouldBe sharedBefore

  it should "keep publishing the other album when createAlbum throws" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/11/2413.jpg", Array[Byte](2))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\ngoogle-title = \"Party night\"\n")
      write(originals, "2009/03/11/Other.toml", "include = [\"all\"]\ngoogle-title = \"Other night\"\n")
      val fake: Fake = Fake(failCreateTitles = Set("Party night"))
      val (out, thrown) = capture(run(originals, fake, Some("2009/03")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      fake.createdAlbums.toSeq shouldBe Seq("Other night")
      out should include ("failed=")
      out should include ("create failed")

  it should "keep publishing the other album when updateAlbumTitle throws" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/11/2413.jpg", Array[Byte](2))
      val party: File = write(
        originals,
        "2009/03/10/Party.toml",
        "include = [\"all\"]\ngoogle-album-id = \"album-party\"\npublished-title = \"Old party\"\ngoogle-title = \"Party night\"\n"
      )
      write(
        originals,
        "2009/03/11/Other.toml",
        "include = [\"all\"]\ngoogle-album-id = \"album-other\"\npublished-title = \"Old other\"\ngoogle-title = \"Other night\"\n"
      )
      val partyBefore: Seq[Byte] = bytes(party)
      val fake: Fake = Fake(failUpdateTitles = Set("Party night"))
      val (out, thrown) = capture(run(originals, fake, Some("2009/03")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      fake.createdAlbums shouldBe empty
      fake.updatedAlbums.map(_._2).toSeq shouldBe Seq("Other night")
      bytes(party) shouldBe partyBefore
      readAlbum(originals, "2009/03/11/Other").publishedTitle shouldBe "Other night"
      out should include ("failed=")

  it should "not upload or rename an album owned by someone else" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val event: File = write(
        originals,
        "2009/03/10/Party.toml",
        "external = true\ninclude = [\"all\"]\ngoogle-title = \"Hawaii \"\nowner = \"Ada Owner\"\n"
      )
      val before: Seq[Byte] = bytes(event)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads shouldBe empty
      fake.createdAlbums shouldBe empty
      fake.updatedAlbums shouldBe empty
      bytes(event) shouldBe before
      out should include ("skipped-external=1")
      out should include ("uploaded=0")

  it should "refuse ensureOne for an album with an owner before any transport call" in :
    withOriginals: originals =>
      val file: File = write(
        originals,
        "2009/03/10/Party.toml",
        "external = true\ninclude = [\"all\"]\ngoogle-title = \"Hawaii \"\nowner = \"Ada Owner\"\n"
      )
      val before: Seq[Byte] = bytes(file)
      val album: Album = Album(
        title = "2009/03/10/Party",
        span = Some(Title.Span(LocalDate.of(2009, 3, 10), LocalDate.of(2009, 3, 10), Title.Grain.Day)),
        include = Seq(Include.Entry.All(Seq.empty)),
        external = true,
        googleAlbumId = "",
        publishedTitle = "",
        googleTitle = "Hawaii ",
        owner = Some("Ada Owner")
      )
      val fake: Fake = Fake()
      Publish.ensureOne(Album.Loaded(file, album), Seq.empty, Map.empty, fake) shouldBe
        Left("2009/03/10/Party is owned by someone else")
      fake.createdAlbums shouldBe empty
      fake.updatedAlbums shouldBe empty
      fake.uploads shouldBe empty
      bytes(file) shouldBe before

  it should "print a google line only under an open album whose title differs" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\ngoogle-title = \"Party night\"\n")
      write(originals, "Best.toml", "include = [\"2009/03/10 2412\"]\n")
      val fake: Fake = Fake()
      var opened: Boolean = false
      val (out, thrown) = capture:
        run(originals, fake, Some("2009/03/10"), dryRun = true, onOpen = () => opened = true)
      expectOk(thrown, out)
      opened shouldBe false
      fake.createdAlbums shouldBe empty
      out.trim.linesIterator.toSeq shouldBe Seq(
        "2009/03/10/2412 albums=2009/03/10/Party,Best",
        "2009/03/10/Party",
        "google \"2009/03/10/Party\" \"Party night\"",
        "Best"
      )
      out should not include ("uploaded=")

  it should "print collision lines and no google line on a dry run" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/11/2413.jpg", Array[Byte](2))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\ngoogle-title = \"Party night\"\n")
      write(originals, "2009/03/11/Other.toml", "include = [\"all\"]\ngoogle-title = \"Party night\"\n")
      val fake: Fake = Fake()
      var opened: Boolean = false
      val (out, thrown) = capture:
        run(originals, fake, Some("2009/03"), dryRun = true, onOpen = () => opened = true)
      expectOk(thrown, out)
      opened shouldBe false
      val lines: Seq[String] = out.trim.linesIterator.toSeq
      lines.take(2).foreach(_ should include ("albums="))
      lines.drop(2).foreach(_ should include ("also desired"))
      lines should not contain ("2009/03/10/Party")
      lines should not contain ("2009/03/11/Other")
      lines.exists(_.startsWith("google ")) shouldBe false
      out should include ("2009/03/10/2412 2009/03/10/Party:")
      out should include ("2009/03/11/2413 2009/03/11/Other:")
      out should not include ("uploaded=")

  it should "not add a list error to an album already blocked by a shared title" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/11/2413.jpg", Array[Byte](2))
      writeBytes(originals, "2009/03/12/2414.jpg", Array[Byte](3))
      write(
        originals,
        "2009/03/10/Party.toml",
        "include = [\"all\"]\ngoogle-album-id = \"album-party\"\npublished-title = \"Old\"\ngoogle-title = \"Party night\"\n"
      )
      write(originals, "2009/03/11/Other.toml", "include = [\"all\"]\ngoogle-title = \"Party night\"\n")
      write(originals, "2009/03/12/Third.toml", "include = [\"all\"]\ngoogle-title = \"Third night\"\n")
      val fake: Fake = Fake(failList = true)
      val (out, thrown) = capture(run(originals, fake, Some("2009/03")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      fake.createdAlbums shouldBe empty
      fake.updatedAlbums shouldBe empty
      val party: Seq[String] = out.linesIterator.filter(_.startsWith("2009/03/10/2412")).toSeq
      val other: Seq[String] = out.linesIterator.filter(_.startsWith("2009/03/11/2413")).toSeq
      val third: Seq[String] = out.linesIterator.filter(_.startsWith("2009/03/12/2414")).toSeq
      party should have size 1
      other should have size 1
      party.foreach: line =>
        line should include ("also desired")
        line should not include ("list failed")
      other.foreach: line =>
        line should include ("also desired")
        line should not include ("list failed")
      third shouldBe Seq("2009/03/12/2414 2009/03/12/Third: list failed")
      out should include ("failed=")

  it should "upload a still and reject the video until it is renamed with v" in :
    withOriginals: originals =>
      val still: File = writeBytes(originals, "2019/12/20/20191220_141955.jpg", Array[Byte](1))
      val video: File = writeBytes(originals, "2019/12/20/VID_20191220_141955.mp4", Array[Byte](2))
      write(originals, "2019/12/20/Party.toml", "include = [\"all\"]\n")
      val (scanOut, scanThrown) = capture:
        Archive.scan(originals).pictures.map(_.name).sorted shouldBe Seq(
          "20191220_141955",
          "20191220_141955v"
        )
      expectOk(scanThrown, scanOut)
      scanOut should include (s"$video: WRONG NAME; SHOULD BE 20191220_141955v.mp4; repair --names")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2019/12/20")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      fake.uploads.map(_.getName).toSeq shouldBe Seq(still.getName)
      video.getName shouldBe "VID_20191220_141955.mp4"
      File(originals, "2019/12/20/20191220_141955v.mp4").exists shouldBe false
      out should include (
        "2019/12/20/20191220_141955v 2019/12/20/Party: VID_20191220_141955.mp4 is not the literal file"
      )
      out should include ("failed=1")

  it should "not upload a video that still occupies the still's extension path" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.gif", Array[Byte](1))
      val video: File = writeBytes(originals, "2009/03/10/2412.mp4", Array[Byte](2))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10/2412")))
      expectOk(thrown, out)
      fake.uploads shouldBe empty
      video.getName shouldBe "2412.mp4"
      File(originals, "2009/03/10/2412.toml").isFile shouldBe false
      out should include ("uploaded=0")
      out should include ("failed=0")

    withOriginals: originals =>
      val still: File = writeBytes(originals, "2009/03/10/IMG_2412.jpg", Array[Byte](1))
      val video: File = writeBytes(originals, "2009/03/10/2412.mp4", Array[Byte](2))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10/2412")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      fake.uploads shouldBe empty
      still.getName shouldBe "IMG_2412.jpg"
      video.getName shouldBe "2412.mp4"
      File(originals, "2009/03/10/2412.jpg").isFile shouldBe false
      out should include ("2009/03/10/2412 2009/03/10/Party: IMG_2412.jpg is not the literal file")
      out should include ("failed=1")

    withOriginals: originals =>
      val still: File = writeBytes(originals, "2009/03/10/2412.JPG", Array[Byte](1))
      val video: File = writeBytes(originals, "2009/03/10/2412.mp4", Array[Byte](2))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10/2412")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      fake.uploads shouldBe empty
      still.getName shouldBe "2412.JPG"
      video.getName shouldBe "2412.mp4"
      File(originals, "2009/03/10/2412.jpg").isFile shouldBe false
      out should include ("2009/03/10/2412 2009/03/10/Party: 2412.JPG is not the literal file")
      out should include ("failed=1")

    withOriginals: originals =>
      val still: File = writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val video: File = writeBytes(originals, "2009/03/10/2412.mp4", Array[Byte](2))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10/2412")))
      expectOk(thrown, out)
      fake.uploads.map(_.getName).toSeq shouldBe Seq(still.getName)
      video.getName shouldBe "2412.mp4"
      out should include ("uploaded=1")
      out should include ("failed=0")

  it should "not upload either video when both would share a v stem" in :
    withOriginals: originals =>
      val still: File = writeBytes(originals, "2019/12/20/20191220_141955.jpg", Array[Byte](1))
      val mp4: File = writeBytes(originals, "2019/12/20/20191220_141955.mp4", Array[Byte](2))
      val mov: File = writeBytes(originals, "2019/12/20/20191220_141955.mov", Array[Byte](3))
      write(originals, "2019/12/20/Party.toml", "include = [\"all\"]\n")
      val (scanOut, scanThrown) = capture:
        Archive.scan(originals).pictures.map(_.name) shouldBe Seq("20191220_141955")
      expectOk(scanThrown, scanOut)
      scanOut should include ("20191220_141955v would have two normal files")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2019/12/20")))
      expectOk(thrown, out)
      fake.uploads.map(_.getName).toSeq shouldBe Seq(still.getName)
      mp4.getName shouldBe "20191220_141955.mp4"
      mov.getName shouldBe "20191220_141955.mov"
      out should include ("uploaded=1")
      out should include ("failed=0")
      out should not include ("is not the literal file")

  it should "fail publish when two normal files are already literal" in :
    withOriginals: originals =>
      writeBytes(originals, "2019/12/20/20191220_141955v.mp4", Array[Byte](1))
      writeBytes(originals, "2019/12/20/20191220_141955v.mov", Array[Byte](2))
      write(originals, "2019/12/20/Party.toml", "include = [\"all\"]\n")
      val (scanOut, scanThrown) = capture:
        val pictures: Seq[Archive.Picture] = Archive.scan(originals).pictures
        pictures.map(_.name) shouldBe Seq("20191220_141955v")
        pictures.head.extensions shouldBe Set("mp4", "mov")
      expectOk(scanThrown, scanOut)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2019/12/20")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      fake.uploads shouldBe empty
      out should include ("2019/12/20/20191220_141955v 2019/12/20/Party: too many normal extensions")
      out should include ("failed=1")

    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/10/2412.gif", Array[Byte](2))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      fake.uploads shouldBe empty
      out should include ("2009/03/10/2412 2009/03/10/Party: too many normal extensions")
      out should include ("failed=1")

  it should "upload the one literal normal and leave the other" in :
    withOriginals: originals =>
      val jpg: File = writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val gif: File = writeBytes(originals, "2009/03/10/IMG_2412.gif", Array[Byte](2))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads.map(_.getPath).toSeq shouldBe Seq(jpg.getPath)
      gif.getName shouldBe "IMG_2412.gif"
      out should include (s"$gif: NOT RENAMED; 2412 would have two normal files")
      out should include ("uploaded=1")
      out should include ("failed=0")

  it should "plan a Pixel motion still apart from the plain frame" in :
    withOriginals: originals =>
      writeBytes(originals, "2026/06/03/PXL_20260603_120000000.MP.jpg", Array[Byte](1))
      writeBytes(originals, "2026/06/03/PXL_20260603_120000000.jpg", Array[Byte](2))
      val video: File = writeBytes(originals, "2026/06/03/PXL_20260603_120000000.mp4", Array[Byte](3))
      write(originals, "2026/06/03/Party.toml", "include = [\"all\"]\n")
      val (scanOut, scanThrown) = capture:
        Archive.scan(originals).pictures.map(_.name).sorted shouldBe Seq(
          "20260603_120000000",
          "20260603_120000000.MP",
          "20260603_120000000v"
        )
      expectOk(scanThrown, scanOut)
      scanOut should not include ("20260603_120000000.MPv")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2026/06/03/20260603_120000000v")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      fake.uploads shouldBe empty
      video.getName shouldBe "PXL_20260603_120000000.mp4"
      out should include (
        "2026/06/03/20260603_120000000v 2026/06/03/Party: PXL_20260603_120000000.mp4 is not the literal file"
      )
      out should include ("failed=1")

  it should "leave an incoming video when the v name is taken" in :
    withOriginals: originals =>
      writeBytes(originals, "2019/12/20/20191220_141955.jpg", Array[Byte](1))
      val occupant: File = writeBytes(originals, "2019/12/20/20191220_141955v.mp4", Array[Byte](2))
      val incoming: File = writeBytes(originals, "2019/12/20/VID_20191220_141955.mp4", Array[Byte](3))
      write(originals, "2019/12/20/Party.toml", "include = [\"all\"]\n")
      val (scanOut, scanThrown) = capture:
        Archive.scan(originals).pictures.map(_.name).sorted shouldBe Seq(
          "20191220_141955",
          "20191220_141955v"
        )
      expectOk(scanThrown, scanOut)
      scanOut should include (s"$incoming: NOT RENAMED; 20191220_141955v.mp4 already exists")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2019/12/20")))
      expectOk(thrown, out)
      incoming.getName shouldBe "VID_20191220_141955.mp4"
      occupant.getName shouldBe "20191220_141955v.mp4"
      File(originals, "2019/12/20/20191220_141955vv.mp4").exists shouldBe false
      fake.uploads.map(_.getName) should not contain incoming.getName
      fake.uploads.map(_.getName) should contain (occupant.getName)
      out should include ("failed=0")

  it should "not make a picture from two jpegs and a prefixed raw" in :
    withOriginals: originals =>
      val first: File = writeBytes(originals, "2009/03/10/IMG_2412.jpg", Array[Byte](1))
      val second: File = writeBytes(originals, "2009/03/10/MVIMG_2412.jpg", Array[Byte](2))
      val raw: File = writeBytes(originals, "2009/03/10/IMG_2412.CR2", Array[Byte](3))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val (scanOut, scanThrown) = capture:
        Archive.scan(originals).pictures shouldBe empty
      expectOk(scanThrown, scanOut)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      first.getName shouldBe "IMG_2412.jpg"
      second.getName shouldBe "MVIMG_2412.jpg"
      raw.getName shouldBe "IMG_2412.CR2"
      fake.uploads shouldBe empty
      out should include ("no albums")
      out should include ("collides with")
      out should include ("is not one normal picture")

  it should "keep a raw on the jpeg stem when that stem stays one picture" in :
    withOriginals: originals =>
      val jpg: File = writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val raw: File = writeBytes(originals, "2009/03/10/IMG_2412.CR2", Array[Byte](2))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val (scanOut, scanThrown) = capture:
        val pictures: Seq[Archive.Picture] = Archive.scan(originals).pictures
        pictures.map(_.name) shouldBe Seq("2412")
        pictures.head.extensions shouldBe Set("jpg", "cr2")
      expectOk(scanThrown, scanOut)
      raw.getName shouldBe "IMG_2412.CR2"
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads.map(_.getPath).toSeq shouldBe Seq(jpg.getPath)
      raw.getName shouldBe "IMG_2412.CR2"
      out should include ("uploaded=1")
      out should include ("failed=0")

  it should "not publish a prefixed jpeg gif and raw as a picture" in :
    withOriginals: originals =>
      val jpg: File = writeBytes(originals, "2009/03/10/IMG_1.jpg", Array[Byte](1))
      val gif: File = writeBytes(originals, "2009/03/10/IMG_1.gif", Array[Byte](2))
      val raw: File = writeBytes(originals, "2009/03/10/IMG_1.CR2", Array[Byte](3))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val (scanOut, scanThrown) = capture:
        Archive.scan(originals).pictures shouldBe empty
      expectOk(scanThrown, scanOut)
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      jpg.getName shouldBe "IMG_1.jpg"
      gif.getName shouldBe "IMG_1.gif"
      raw.getName shouldBe "IMG_1.CR2"
      File(originals, "2009/03/10/1.cr2").exists shouldBe false
      fake.uploads shouldBe empty
      out should include ("no albums")
      out should include ("would have two normal files")
      out should include ("is not one normal picture")

  it should "scan a lone raw and omit a lone toml thm or tif" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/1002.cr2", Array[Byte](1))
      writeBytes(originals, "2009/03/10/1005.toml", Array[Byte]())
      writeBytes(originals, "2009/03/10/1003.thm", Array[Byte](2))
      writeBytes(originals, "2009/03/10/1004.tif", Array[Byte](3))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val (scanOut, scanThrown) = capture:
        Archive.scan(originals).pictures.map(_.name) shouldBe Seq("1002")
      expectOk(scanThrown, scanOut)
      scanOut should include ("1005 has no picture file")
      scanOut should include ("1003 has no picture file")
      scanOut should include ("1004 has no picture file")
      scanOut should not include ("no images")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads shouldBe empty
      out should include ("failed=0")
      out should not include ("no images")

  it should "fail a refused non-digit rename instead of staying local" in :
    withOriginals: originals =>
      val picture: File = writeBytes(originals, "2009/03/10/IMG_VID_1.jpg", Array[Byte](1))
      write(originals, "2009/03/10/Party.toml", "include = [\"all\"]\n")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage shouldBe "publish failed"
      picture.getName shouldBe "IMG_VID_1.jpg"
      File(originals, "2009/03/10/VID_1.jpg").exists shouldBe false
      fake.uploads shouldBe empty
      out should include ("2009/03/10/VID_1 2009/03/10/Party: IMG_VID_1.jpg is not the literal file")
      out should include ("failed=1")

  private def readAlbum(originals: File, title: String): Album =
    Album.read(originals, Album.fileFor(originals, title))

  private def run(
    originals: File,
    fake: Fake,
    prefix: Option[String],
    recursive: Boolean = false,
    dryRun: Boolean = false,
    exiftool: Seq[String] => Either[String, Unit] = _ => Right(()),
    onOpen: () => Unit = () => ()
  ): Unit =
    val tempDirectory: File = File(originals.getParentFile, "exif")
    tempDirectory.mkdirs()
    Publish.run(
      originals = originals,
      prefix = prefix,
      recursive = recursive,
      dryRun = dryRun,
      google = () =>
        onOpen()
        fake,
      exiftool = exiftool,
      tempDirectory = tempDirectory
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

  private def metadata(originals: File, path: String): Metadata =
    Metadata.read(File(originals, s"$path.toml"))

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
    val root: File = JFiles.createTempDirectory("publish").toFile
    val originals: File = File(root, "originals")
    originals.mkdirs()
    try body(originals)
    finally deleteTree(root)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()

  private final class Fake(
    missingId: Boolean = false,
    statusMessage: Option[String] = None,
    failCreateAfter: Option[Int] = None,
    unowned: Boolean = false,
    initialRemote: Seq[(String, String)] = Seq.empty,
    failList: Boolean = false,
    failCreateTitles: Set[String] = Set.empty,
    failUpdateTitles: Set[String] = Set.empty
  ) extends Publish.GoogleTransport:
    val uploads: scala.collection.mutable.ArrayBuffer[File] = scala.collection.mutable.ArrayBuffer.empty
    val events: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer.empty
    val batchCreates: scala.collection.mutable.ArrayBuffer[Seq[Publish.CreateItem]] =
      scala.collection.mutable.ArrayBuffer.empty
    val adds: scala.collection.mutable.ArrayBuffer[(String, Seq[String])] = scala.collection.mutable.ArrayBuffer.empty
    val createdAlbums: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer.empty
    val updatedAlbums: scala.collection.mutable.ArrayBuffer[(String, String)] =
      scala.collection.mutable.ArrayBuffer.empty
    var onUpload: File => Unit = _ => ()
    private var remote: Seq[(String, String)] = initialRemote
    private var next: Int = 0

    def remoteAlbums: Seq[(String, String)] = remote

    override def listAppAlbums(): Seq[(String, String)] =
      if failList then throw IllegalStateException("list failed")
      remote

    override def createAlbum(title: String): String =
      if failCreateTitles.contains(title) then throw IllegalStateException(s"create failed: $title")
      next += 1
      val id: String = s"album-$next"
      remote = remote :+ (id, title)
      createdAlbums += title
      id

    override def updateAlbumTitle(albumId: String, title: String): Unit =
      if failUpdateTitles.contains(title) then throw IllegalStateException(s"update failed: $title")
      updatedAlbums += ((albumId, title))
      remote = remote.map:
        case (id, _) if id == albumId => (id, title)
        case pair => pair

    override def upload(file: File, mimeType: String): String =
      onUpload(file)
      events += "upload"
      uploads += file
      s"token-${file.getName}"

    override def batchCreate(items: Seq[Publish.CreateItem]): Seq[Publish.Created] =
      events += "batchCreate"
      batchCreates += items
      if failCreateAfter.exists(limit => batchCreates.sizeIs > limit) then
        throw IllegalStateException("later batch failed")
      items.map: item =>
        val id: Option[String] = if missingId then None else Some(s"media-${item.uploadToken}")
        val status: Option[String] = if id.isDefined then None else statusMessage
        Publish.Created(item.uploadToken, id, status)

    override def owns(ids: Seq[String]): Set[String] =
      events += "owns"
      if unowned then Set.empty else ids.toSet

    override def batchAdd(albumId: String, mediaItemIds: Seq[String]): Unit =
      adds += ((albumId, mediaItemIds))
