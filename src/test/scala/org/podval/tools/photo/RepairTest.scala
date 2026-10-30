package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
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
      Pictures.repaired("1000", "JPEG") shouldBe ("1000", "JPEG")
      val thrown: IllegalArgumentException = intercept[IllegalArgumentException]:
        run(root, "2009/03/10", names = true)
      picture.exists shouldBe true
      File(originals, "2009/03/10/1000.jpg").exists shouldBe false
      thrown.getMessage should include ("illegal extension JPEG")
      thrown.getMessage should not include ("illegal extension jpeg")

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

  "ls" should "not rename" in :
    withOriginals: originals =>
      val root: Root = rootFor(originals)
      val picture: File = File(originals, "2009/03/10/IMG_2412.JPG")
      picture.getParentFile.mkdirs()
      ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", picture)
      capture:
        ListPictures(
          root.parse("2009/03/10"),
          long = false,
          nameContains = None,
          hasExtension = None,
          propertyAndValue = None,
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
        ).execute()
      picture.exists shouldBe true
      File(originals, "2009/03/10/2412.jpg").exists shouldBe false

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
    val buffer: ByteArrayOutputStream = ByteArrayOutputStream()
    Console.withOut(buffer)(body)
    buffer.toString(StandardCharsets.UTF_8)

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
