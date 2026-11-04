package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.{FileAlreadyExistsException, Files as JFiles}
import java.time.{LocalDateTime, ZoneId}
import javax.imageio.ImageIO

class RepairTest extends AnyFlatSpec, Matchers:
  private val zone: ZoneId = ZoneId.of("America/New_York")

  "repair --names" should "rename IMG_ and .JPG" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, "2009/03/10/IMG_2412.JPG")
      picture.getParentFile.mkdirs()
      ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", picture)
      run(root, "2009/03/10", names = true)
      picture.exists shouldBe false
      File(originals, "2009/03/10/2412.jpg").isFile shouldBe true

  it should "rename a camera prefix and lowercase a known extension" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val renames: Seq[(String, String)] = Seq(
        "2009/03/10/VID_0063.MP4" -> "2009/03/10/0063.mp4",
        "2009/03/11/MVI_0063.MP4" -> "2009/03/11/0063.mp4",
        "2009/03/12/PXL_1111.jpg" -> "2009/03/12/1111.jpg",
        "2009/03/12/img_2222.JPG" -> "2009/03/12/2222.jpg",
        "2009/03/13/MVIMG_3333.jpg" -> "2009/03/13/3333.jpg",
        "2009/03/14/Pxl_20260603_120000000.MP.jpg" -> "2009/03/14/20260603_120000000.MP.jpg",
        "2009/03/10/2412.CR2" -> "2009/03/10/2412.cr2",
        "2009/03/10/2412.Jpg" -> "2009/03/10/2412.jpg"
      )
      val sources: Seq[File] = renames.map: (from, _) =>
        val file: File = File(originals, from)
        file.getParentFile.mkdirs()
        file.createNewFile()
        file
      run(root, "2009/03", names = true)
      sources.foreach(_.exists shouldBe false)
      renames.foreach: (_, to) =>
        File(originals, to).isFile shouldBe true

  it should "not rename JPEG" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, "2009/03/10/1000.JPEG")
      picture.getParentFile.mkdirs()
      picture.createNewFile()
      Pictures.repaired("1000", "JPEG") shouldBe ("1000", "jpeg")
      run(root, "2009/03/10", names = true)
      picture.exists shouldBe false
      File(originals, "2009/03/10/1000.jpeg").isFile shouldBe true
      File(originals, "2009/03/10/1000.jpg").exists shouldBe false
      File(originals, "2009/03/10/1000.JPEG").exists shouldBe false

  it should "not rename on a dry run" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, "2009/03/10/IMG_2412.JPG")
      picture.getParentFile.mkdirs()
      ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", picture)
      val out: String = capture(run(root, "2009/03/10", names = true, dryRun = true))
      picture.exists shouldBe true
      File(originals, "2009/03/10/2412.jpg").exists shouldBe false
      out should include ("moving")

  it should "print the video move and write nothing on a dry run" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val still: File = File(originals, "2009/03/10/20191220_141955.jpg")
      val video: File = File(originals, "2009/03/10/VID_20191220_141955.mp4")
      still.getParentFile.mkdirs()
      still.createNewFile()
      video.createNewFile()
      val destination: File = File(originals, "2009/03/10/20191220_141955v.mp4")
      val out: String = capture(run(root, "2009/03/10", names = true, dryRun = true))
      still.exists shouldBe true
      video.exists shouldBe true
      destination.exists shouldBe false
      out should include ("moving")
      out should include (video.getName)
      out should include ("20191220_141955v.mp4")
      out should not include (still.getName)

  "ls" should "not rename" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, "2009/03/10/IMG_2412.JPG")
      picture.getParentFile.mkdirs()
      ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", picture)
      val (_, result) = ListPictures(
        "2009/03/10",
        long = false,
        nameContains = None,
        hasExtension = None,
        property = None,
        timestampFromNameFormat = None,
        timestampFromName = false,
        timestampFromNameOnly = false,
        timestampsVary = false,
        hasMetadata = false,
        noNormal = false,
        noTimestamp = false,
        wrongTimestamp = false,
        wrongDay = false,
        wrong = false
      ).execute(Session(root))
      result match
        case Result.Ok(_, _) => ()
        case Result.Failed(text, _, message) => fail(s"$message\n$text")
        case Result.Usage(message) => fail(message)
      picture.exists shouldBe true
      File(originals, "2009/03/10/2412.jpg").exists shouldBe false

  "repair --raw" should "not generate from an image-only picture" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val heic: File = File(originals, "2009/03/10/1006.heic")
      heic.getParentFile.mkdirs()
      heic.createNewFile()
      var generated: Boolean = false
      run(root, "2009/03/10", raw = true, generate = _ => generated = true)
      generated shouldBe false
      heic.exists shouldBe true
      File(originals, "2009/03/10/1006.jpg").exists shouldBe false

  it should "still generate from raw when no normal image exists" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val nef: File = File(originals, "2009/03/10/1007.nef")
      nef.getParentFile.mkdirs()
      nef.createNewFile()
      var generated: Boolean = false
      run(root, "2009/03/10", raw = true, generate = picture =>
        picture.raw.map(_.name) shouldBe Some("nef")
        generated = true
      )
      generated shouldBe true

    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val heic: File = File(originals, "2009/03/10/1008.heic")
      val nef: File = File(originals, "2009/03/10/1008.nef")
      heic.getParentFile.mkdirs()
      heic.createNewFile()
      nef.createNewFile()
      var generated: Boolean = false
      run(root, "2009/03/10", raw = true, generate = picture =>
        picture.raw.map(_.name) shouldBe Some("nef")
        picture.image.map(_.name) shouldBe Some("heic")
        picture.normal shouldBe None
        generated = true
      )
      generated shouldBe true
      File(originals, "2009/03/10/1008.jpg").exists shouldBe false

  "repair --raw --dry-run" should "not call the generator" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val raw: File = File(originals, "2009/03/10/2412.cr2")
      raw.getParentFile.mkdirs()
      raw.createNewFile()
      var generated: Boolean = false
      run(root, "2009/03/10", raw = true, dryRun = true, generate = _ => generated = true)
      generated shouldBe false
      raw.exists shouldBe true

  "repair --timestamps --dry-run" should "not call the setter" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, "2009/03/10/20090310_120000.jpg")
      picture.getParentFile.mkdirs()
      ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", picture)
      var set: Boolean = false
      run(
        root,
        "2009/03/10",
        timestamps = true,
        dryRun = true,
        setTimestamp = (_, _, _) => set = true
      )
      set shouldBe false

  "repair --days" should "not move an evening jpeg with no offset" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, "2009/06/11/3404.jpg")
      copyFixture("/dates/evening-no-offset.jpg", picture)
      val out: String = capture(run(root, "2009/06/11", days = true))
      picture.isFile shouldBe true
      File(originals, "2009/06/12/3404.jpg").exists shouldBe false
      out should not include ("moving")

  it should "move an evening jpeg back from the next UTC day" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, "2009/06/12/3404.jpg")
      copyFixture("/dates/evening-no-offset.jpg", picture)
      val out: String = capture(run(root, "2009/06/12", days = true))
      picture.exists shouldBe false
      File(originals, "2009/06/11/3404.jpg").isFile shouldBe true
      File(originals, "2009/06/12").exists shouldBe false
      out should include ("/2009/06/11")

  it should "not let a compact filename override DateTimeOriginal" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, "2009/06/10/20090610_120000.jpg")
      copyFixture("/dates/evening-no-offset.jpg", picture)
      run(root, "2009/06/10", days = true)
      picture.exists shouldBe false
      File(originals, "2009/06/11/20090610_120000.jpg").isFile shouldBe true

  it should "move a counter-named file to the sidecar date and keep the embedded time" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, "2009/06/11/3404.jpg")
      copyFixture("/dates/evening-no-offset.jpg", picture)
      JFiles.writeString(File(originals, "2009/06/11/3404.toml").toPath, "date = 2012-04-04\n")
      val audit: Dates.Assessment = PictureAudit(root.parsePicture("2009/06/11/3404"))
      val clock: Dates.Clock = audit.clock.getOrElse(fail("no clock"))
      (clock.source eq Dates.Source.SidecarDate) shouldBe true
      clock.civil.isEqual(LocalDateTime.parse("2012-04-04T23:41:10.050")) shouldBe true
      clock.offset.isEmpty shouldBe true
      run(root, "2009/06/11", days = true)
      picture.exists shouldBe false
      File(originals, "2012/04/04/3404.jpg").isFile shouldBe true
      File(originals, "2012/04/04/3404.toml").isFile shouldBe true

  it should "not move or stamp when the sidecar date disagrees with the timestamp" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, "2009/06/11/3404.jpg")
      copyFixture("/dates/evening-no-offset.jpg", picture)
      JFiles.writeString(
        File(originals, "2009/06/11/3404.toml").toPath,
        "timestamp = \"2011-06-29T09:45:00\"\ndate = 2011-06-30\n"
      )
      var set: Boolean = false
      val out: String = capture(run(
        root,
        "2009/06/11",
        timestamps = true,
        days = true,
        setTimestamp = (_, _, _) => set = true
      ))
      set shouldBe false
      picture.isFile shouldBe true
      File(originals, "2011/06/29/3404.jpg").exists shouldBe false
      File(originals, "2011/06/30/3404.jpg").exists shouldBe false
      out should include (
        "DATE DISAGREES WITH TIMESTAMP /2009/06/11/3404 timestamp=2011-06-29T09:45 date=2011-06-30"
      )

  it should "not move or stamp an upload with no clock" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, ".upload/1000.jpg")
      picture.getParentFile.mkdirs()
      ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", picture)
      var set: Boolean = false
      val repairOut: String = capture(run(
        root,
        ".upload",
        timestamps = true,
        days = true,
        setTimestamp = (_, _, _) => set = true
      ))
      set shouldBe false
      picture.isFile shouldBe true
      repairOut should not include ("moving")
      val (_, result) = ListPictures(
        ".upload",
        long = true,
        nameContains = None,
        hasExtension = None,
        property = None,
        timestampFromNameFormat = None,
        timestampFromName = false,
        timestampFromNameOnly = false,
        timestampsVary = false,
        hasMetadata = false,
        noNormal = false,
        noTimestamp = false,
        wrongTimestamp = false,
        wrongDay = false,
        wrong = false
      ).execute(Session(root))
      result match
        case Result.Ok(text, _) =>
          text should include ("NO TIMESTAMP")
          text should not include ("source=FolderDay")
        case Result.Failed(text, _, message) => fail(s"$message\n$text")
        case Result.Usage(message) => fail(message)

  it should "file an inbox jpeg on the civil day and leave the curated include" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val best: String = "include = [\"2009/03/10 2412\"]\n"
      JFiles.writeString(File(originals, "Best.toml").toPath, best)
      val picture: File = File(originals, ".upload/3404.jpg")
      copyFixture("/dates/evening-no-offset.jpg", picture)
      val destination: File = File(originals, "2009/06/11/3404.jpg")
      val out: String = capture(run(root, ".upload", days = true))
      picture.exists shouldBe false
      destination.isFile shouldBe true
      File(originals, ".upload").isDirectory shouldBe true
      JFiles.readString(File(originals, "Best.toml").toPath) shouldBe best
      out should include ("moving /.upload/3404 to /2009/06/11")
      out should include (".upload/3404.jpg to ")
      out should include ("/2009/06/11/3404.jpg")
      out should not include ("keep")

  it should "print the inbox move and write nothing on a dry run" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, ".upload/3404.jpg")
      copyFixture("/dates/evening-no-offset.jpg", picture)
      val out: String = capture(run(root, ".upload", days = true, dryRun = true))
      picture.isFile shouldBe true
      File(originals, "2009/06/11/3404.jpg").exists shouldBe false
      out.linesIterator.map(_.trim).filter(_.nonEmpty).toSeq shouldBe
        Seq("moving /.upload/3404 to /2009/06/11")

  it should "keep an inbox picture when any destination file already exists" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, ".upload/3404.jpg")
      val sidecar: File = File(originals, ".upload/3404.toml")
      copyFixture("/dates/evening-no-offset.jpg", picture)
      JFiles.writeString(sidecar.toPath, "\n")
      val occupied: File = File(originals, "2009/06/11/3404.toml")
      occupied.getParentFile.mkdirs()
      occupied.createNewFile()
      def kept(dryRun: Boolean): String =
        val out: String = capture(run(root, ".upload", days = true, dryRun = dryRun))
        picture.isFile shouldBe true
        sidecar.isFile shouldBe true
        occupied.isFile shouldBe true
        File(originals, "2009/06/11/3404.jpg").exists shouldBe false
        File(originals, ".upload").isDirectory shouldBe true
        out
      val dry: String = kept(true)
      val real: String = kept(false)
      dry should include ("keep /.upload/3404 /2009/06/11 already exists")
      real should include ("keep /.upload/3404 /2009/06/11 already exists")
      dry should not include ("moving")
      real should not include ("moving")

  it should "put every extension back when the inbox link loses the name" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val first: File = File(originals, ".upload/3404.jpg")
      val sidecar: File = File(originals, ".upload/3404.toml")
      val second: File = File(originals, ".upload/3405.jpg")
      copyFixture("/dates/evening-no-offset.jpg", first)
      JFiles.writeString(sidecar.toPath, "\n")
      copyFixture("/dates/evening-no-offset.jpg", second)
      val firstLength: Long = first.length
      var calls: Int = 0
      val out: String = capture(run(
        root,
        ".upload",
        days = true,
        link = (partial, destination) =>
          calls += 1
          if calls == 2 then throw FileAlreadyExistsException(destination.getPath)
          else FileDownloads.linkPartial(partial, destination)
      ))
      calls shouldBe 3
      first.isFile shouldBe true
      first.length shouldBe firstLength
      sidecar.isFile shouldBe true
      second.exists shouldBe false
      File(originals, "2009/06/11/3405.jpg").isFile shouldBe true
      File(originals, "2009/06/11/3404.jpg").exists shouldBe false
      File(originals, "2009/06/11/3404.toml").exists shouldBe false
      val dayNames: Seq[String] =
        Option(File(originals, "2009/06/11").list).map(_.toSeq).getOrElse(Seq.empty)
      dayNames.exists(_.startsWith(".inbox-")) shouldBe false
      out should include ("keep /.upload/3404 /2009/06/11 already exists")
      out should include ("moving /.upload/3405 to /2009/06/11")
      out should include (".upload/3405.jpg to ")
      out should not include ("moving /.upload/3404")
      out should not include (".upload/3404.jpg to ")
      out should not include (".upload/3404.toml to ")

  it should "put every extension back and stop when the inbox link fails another way" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, ".upload/3404.jpg")
      val sidecar: File = File(originals, ".upload/3404.toml")
      copyFixture("/dates/evening-no-offset.jpg", picture)
      JFiles.writeString(sidecar.toPath, "\n")
      var calls: Int = 0
      val (out, thrown) = Transcript.collect(run(
        root,
        ".upload",
        days = true,
        link = (partial, destination) =>
          calls += 1
          if calls == 2 then throw RuntimeException("boom")
          else FileDownloads.linkPartial(partial, destination)
      ))
      calls shouldBe 2
      thrown.get.getMessage shouldBe "boom"
      picture.isFile shouldBe true
      sidecar.isFile shouldBe true
      File(originals, "2009/06/11/3404.jpg").exists shouldBe false
      File(originals, "2009/06/11/3404.toml").exists shouldBe false
      out should not include ("keep")
      out should not include ("moving")

  it should "print the disagreement line for an inbox sidecar and not move" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, ".upload/3404.jpg")
      copyFixture("/dates/evening-no-offset.jpg", picture)
      JFiles.writeString(
        File(originals, ".upload/3404.toml").toPath,
        "timestamp = \"2011-06-29T09:45:00\"\ndate = 2011-06-30\n"
      )
      val out: String = capture(run(root, ".upload", days = true))
      picture.isFile shouldBe true
      File(originals, "2011/06/29/3404.jpg").exists shouldBe false
      File(originals, "2011/06/30/3404.jpg").exists shouldBe false
      out should include (
        "DATE DISAGREES WITH TIMESTAMP /.upload/3404 timestamp=2011-06-29T09:45 date=2011-06-30"
      )
      out should not include ("moving")
      out should not include ("keep")

  it should "leave an orphan THM in .upload" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val thm: File = File(originals, ".upload/1003.THM")
      thm.getParentFile.mkdirs()
      thm.createNewFile()
      val out: String = capture(run(root, ".upload", names = true, days = true))
      thm.isFile shouldBe true
      out should include (s"$thm: NOT RENAMED; 1003 has no picture file")
      out should not include ("moving")

  it should "move an inbox partial back and then file it" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val partial: File = File(originals, "2010/01/02/.inbox-2412.jpg.partial")
      copyFixture("/dates/evening-no-offset.jpg", partial)
      File(originals, ".upload").mkdirs()
      val out: String = capture(run(root, ".upload", days = true))
      partial.exists shouldBe false
      File(originals, ".upload/2412.jpg").exists shouldBe false
      File(originals, "2009/06/11/2412.jpg").isFile shouldBe true
      File(originals, ".upload").isDirectory shouldBe true
      out should include ("moving /.upload/2412 to /2009/06/11")

  it should "leave an inbox partial in place on a dry run" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val partial: File = File(originals, "2010/01/02/.inbox-2412.jpg.partial")
      copyFixture("/dates/evening-no-offset.jpg", partial)
      File(originals, ".upload").mkdirs()
      val out: String = capture(run(root, ".upload", days = true, dryRun = true))
      partial.isFile shouldBe true
      File(originals, ".upload/2412.jpg").exists shouldBe false
      File(originals, "2009/06/11/2412.jpg").exists shouldBe false
      out should not include ("moving")

  it should "delete an inbox partial that is already the day file" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val dayFile: File = File(originals, "2009/06/11/2412.jpg")
      copyFixture("/dates/evening-no-offset.jpg", dayFile)
      val partial: File = File(dayFile.getParentFile, ".inbox-2412.jpg.partial")
      JFiles.createLink(partial.toPath, dayFile.toPath)
      File(originals, ".upload").mkdirs()
      val out: String = capture(run(root, "", days = true))
      partial.exists shouldBe false
      dayFile.isFile shouldBe true
      File(originals, ".upload/2412.jpg").exists shouldBe false
      out should not include ("moving")

  it should "refuse a leftover inbox partial that is not the day file" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val dayFile: File = File(originals, "2009/06/11/2412.jpg")
      copyFixture("/dates/evening-no-offset.jpg", dayFile)
      val partial: File = File(dayFile.getParentFile, ".inbox-2412.jpg.partial")
      JFiles.writeString(partial.toPath, "other")
      File(originals, ".upload").mkdirs()
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        run(root, ".upload", days = true)
      caught.getMessage shouldBe "leftover inbox partial: .inbox-2412.jpg.partial"
      partial.isFile shouldBe true
      dayFile.isFile shouldBe true
      File(originals, ".upload/2412.jpg").exists shouldBe false

  it should "refuse a leftover inbox partial when .upload already has the file" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val uploadFile: File = File(originals, ".upload/2412.jpg")
      copyFixture("/dates/evening-no-offset.jpg", uploadFile)
      val partial: File = File(originals, "2010/01/02/.inbox-2412.jpg.partial")
      partial.getParentFile.mkdirs()
      JFiles.writeString(partial.toPath, "other")
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy
        run(root, ".upload", days = true)
      caught.getMessage shouldBe "leftover inbox partial: .inbox-2412.jpg.partial"
      partial.isFile shouldBe true
      uploadFile.isFile shouldBe true
      File(originals, "2009/06/11/2412.jpg").exists shouldBe false

  it should "not restore an inbox partial when the path is a year" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val partial: File = File(originals, "2009/06/12/.inbox-2412.jpg.partial")
      copyFixture("/dates/evening-no-offset.jpg", partial)
      File(originals, ".upload").mkdirs()
      val out: String = capture(run(root, "2009", days = true))
      partial.isFile shouldBe true
      File(originals, ".upload/2412.jpg").exists shouldBe false
      out should not include ("moving")

  it should "throw when a day-folder destination exists and not print keep" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, "2009/06/12/3404.jpg")
      copyFixture("/dates/evening-no-offset.jpg", picture)
      val destination: File = File(originals, "2009/06/11/3404.jpg")
      destination.getParentFile.mkdirs()
      destination.createNewFile()
      val (out, thrown) = Transcript.collect(run(root, "2009/06/12", days = true))
      thrown.get shouldBe a [FileAlreadyExistsException]
      picture.isFile shouldBe true
      destination.isFile shouldBe true
      out should not include ("keep")

  private def run(
    root: Root,
    path: String,
    names: Boolean = false,
    raw: Boolean = false,
    timestamps: Boolean = false,
    days: Boolean = false,
    dryRun: Boolean = false,
    generate: Picture => Unit = _ => (),
    setTimestamp: (Picture, String, java.time.Instant) => Unit = (_, _, _) => (),
    link: (File, File) => Unit = FileDownloads.linkPartial
  ): Unit =
    Repair.run(
      root.parse(path),
      names = names,
      raw = raw,
      timestamps = timestamps,
      days = days,
      dryRun = dryRun,
      generate = generate,
      setTimestamp = setTimestamp,
      link = link
    )

  private def capture(body: => Unit): String =
    val (out, thrown) = Transcript.collect(body)
    thrown match
      case Some(error) => throw error
      case None => out

  private def rootFor(originals: File): Root =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(originals.toPath).toString
    Root(Configuration(rootPath, zone))

  private def withOriginals(body: File => Unit): Unit =
    val originals: File = JFiles.createTempDirectory("repair").toFile
    val runtime: File = JFiles.createTempDirectory("repair-lock").toFile
    try Album.withWriteLockDirectory(runtime)(body(originals))
    finally
      deleteTree(originals)
      deleteTree(runtime)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()

  private def copyFixture(name: String, to: File): Unit =
    to.getParentFile.mkdirs()
    val url = Option(getClass.getResource(name)).getOrElse(fail(s"missing $name"))
    JFiles.copy(File(url.toURI).toPath, to.toPath)
