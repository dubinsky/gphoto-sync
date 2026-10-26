package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files as JFiles
import java.time.{LocalDate, ZoneId}

class PublishTest extends AnyFlatSpec, Matchers:
  private val zone: ZoneId = ZoneId.of("America/New_York")

  "publish" should "reject a year or the root without --recursive and write nothing" in :
    withOriginals: originals =>
      val jpeg: File = writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1, 2, 3))
      write(originals, "2009/03/10/Party.toml", "from = 2009-03-10\n")
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
      write(originals, "2009/03/10/Party.toml", "")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009"), recursive = true))
      expectOk(thrown, out)
      fake.uploads should have size 1
      out should include ("uploaded=1")

  it should "upload one jpeg into an event and Best" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1, 2))
      write(originals, "2009/03/10/Party.toml", "from = 2009-03-10\n")
      write(originals, "Best.toml", "members = [\"2009/03/10/2412\"]\n")
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
      val event: Album.Event = Album.readEvent(
        File(originals, "2009/03/10/Party.toml"),
        "2009/03/10/Party",
        LocalDate.of(2009, 3, 10)
      )
      event.from match
        case Some(Album.When.Date(date)) => date.toString shouldBe "2009-03-10"
        case other => fail(s"from was $other")
      event.googleAlbumId.nonEmpty shouldBe true
      event.publishedTitle shouldBe "2009/03/10/Party"
      val best: Album.Curated = Album.readCurated(File(originals, "Best.toml"), "Best")
      best.members shouldBe Seq("2009/03/10/2412")
      best.googleAlbumId.nonEmpty shouldBe true
      best.publishedTitle shouldBe "Best"
      out should include ("uploaded=1 filed=2 skipped-excluded=0 skipped-external=0 skipped-exists=0 failed=0")

  it should "not upload a picture claimed by an external album" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val eventFile: File = write(originals, "2009/03/10/Party.toml", "external = true\n")
      val eventBytes: Seq[Byte] = bytes(eventFile)
      val bestFile: File = write(originals, "Best.toml", "members = [\"2009/03/10/2412\"]\n")
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
      write(originals, "2009/03/10/Party.toml", "")
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

  it should "fail a video that has an orientation override and not upload it" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/1000.mov", Array[Byte](1, 2))
      write(originals, "2009/03/10/1000.toml", "orientation = \"Rotate 90 CW\"\n")
      write(originals, "2009/03/10/Party.toml", "")
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
      write(originals, "2009/03/10/Party.toml", "")
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
      write(originals, "2009/03/10/Party.toml", "")
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
      val album: File = write(originals, "2009/03/10/Party.toml", "")
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
      write(originals, "2009/03/10/Party.toml", "from = 2009-03-10\n")
      write(originals, "Best.toml", "members = [\"2009/03/10/2412\"]\n")
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
      out should include ("2009/03/10/2412 dated=2009/03/10/Party curated=Best")
      out should include ("2009/03/10/Party")
      out should include ("Best")
      out should not include ("uploaded=")

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
      write(originals, "2009/03/10/Party.toml", "")
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
      val album: File = write(originals, "2009/03/10/Party.toml", "")
      val fake: Fake = Fake()
      fake.onUpload = _ =>
        val event: Album.Event = Album.readEvent(album, "2009/03/10/Party", LocalDate.of(2009, 3, 10))
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
      write(originals, "2009/03/10/Party.toml", "")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("2009/03/10")))
      expectOk(thrown, out)
      fake.uploads.map(_.getName).toSeq shouldBe Seq("1000.jpg")
      out should include ("uploaded=1")

  it should "select claim paths when the prefix has a leading slash" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/11/2413.jpg", Array[Byte](2))
      write(originals, "2009/03/10/Party.toml", "")
      write(originals, "2009/03/11/Other.toml", "")
      val fake: Fake = Fake()
      val (out, thrown) = capture(run(originals, fake, Some("/2009/03/10")))
      expectOk(thrown, out)
      fake.uploads.map(_.getName).toSeq shouldBe Seq("2412.jpg")
      out should include ("uploaded=1")

  it should "fail a misnamed upload file and not publish the album" in :
    withOriginals: originals =>
      val img: File = writeBytes(originals, "2009/03/10/IMG_2412.jpg", Array[Byte](1))
      val upper: File = writeBytes(originals, "2009/03/10/2413.JPG", Array[Byte](2))
      val album: File = write(originals, "2009/03/10/Party.toml", "")
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

  it should "leave a raw gif thm or tif picture unpublished" in :
    withOriginals: originals =>
      val gif: File = writeBytes(originals, "2009/03/10/IMG_1001.gif", Array[Byte](1))
      writeBytes(originals, "2009/03/10/1002.cr2", Array[Byte](2))
      writeBytes(originals, "2009/03/10/1003.thm", Array[Byte](3))
      writeBytes(originals, "2009/03/10/1004.tif", Array[Byte](4))
      val album: File = write(originals, "2009/03/10/Party.toml", "")
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
      write(originals, "2009/03/10/Party.toml", "")
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
      write(originals, "2009/03/10/Party.toml", "")
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
      write(originals, "2009/03/10/Party.toml", "")
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
      zone = zone,
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
    unowned: Boolean = false
  ) extends Publish.GoogleTransport:
    val uploads: scala.collection.mutable.ArrayBuffer[File] = scala.collection.mutable.ArrayBuffer.empty
    val events: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer.empty
    val batchCreates: scala.collection.mutable.ArrayBuffer[Seq[Publish.CreateItem]] =
      scala.collection.mutable.ArrayBuffer.empty
    val adds: scala.collection.mutable.ArrayBuffer[(String, Seq[String])] = scala.collection.mutable.ArrayBuffer.empty
    val createdAlbums: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer.empty
    var onUpload: File => Unit = _ => ()
    private var remote: Seq[(String, String)] = Seq.empty
    private var next: Int = 0

    override def listAppAlbums(): Seq[(String, String)] = remote

    override def createAlbum(title: String): String =
      next += 1
      val id: String = s"album-$next"
      remote = remote :+ (id, title)
      createdAlbums += title
      id

    override def updateAlbumTitle(albumId: String, title: String): Unit =
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
