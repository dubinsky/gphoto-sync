package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.Files as JFiles
import java.time.ZoneId
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

  private def run(
    root: Root,
    path: String,
    names: Boolean = false,
    raw: Boolean = false,
    timestamps: Boolean = false,
    days: Boolean = false,
    dryRun: Boolean = false,
    generate: Picture => Unit = _ => (),
    setTimestamp: (Picture, String, java.time.Instant) => Unit = (_, _, _) => ()
  ): Unit =
    Repair.run(
      root.parse(path),
      names = names,
      raw = raw,
      timestamps = timestamps,
      days = days,
      dryRun = dryRun,
      generate = generate,
      setTimestamp = setTimestamp
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
    try body(originals)
    finally deleteTree(originals)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
