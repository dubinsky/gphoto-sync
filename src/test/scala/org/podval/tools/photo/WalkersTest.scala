package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files as JFiles
import java.nio.file.Path
import java.time.ZoneId

class WalkersTest extends AnyFlatSpec, Matchers:
  "Files.listDirectories" should "ignore a sibling .toml" in :
    withTemp: directory =>
      val month: Path = JFiles.createDirectory(directory.resolve("03"))
      JFiles.createFile(directory.resolve("03.toml"))
      Files.listDirectories(directory.toFile).map(_.getName) shouldBe Seq(month.getFileName.toString)

  it should "allow AGENTS.md and a dotfile beside a numeric child" in :
    withTemp: directory =>
      val month: Path = JFiles.createDirectory(directory.resolve("03"))
      JFiles.createFile(directory.resolve("AGENTS.md"))
      JFiles.createFile(directory.resolve(".keep"))
      Files.listDirectories(directory.toFile).map(_.getName) shouldBe Seq(month.getFileName.toString)

  it should "reject any other stray file" in :
    withTemp: directory =>
      JFiles.createDirectory(directory.resolve("03"))
      JFiles.createFile(directory.resolve("03.toml"))
      JFiles.createFile(directory.resolve("readme.txt"))
      an [IllegalArgumentException] should be thrownBy Files.listDirectories(directory.toFile)

  "Files.listFiles" should "reject a dot-directory in a picture directory" in :
    withTemp: directory =>
      JFiles.createDirectory(directory.resolve(".secret"))
      an [IllegalArgumentException] should be thrownBy Files.listFiles(directory.toFile)

  "Pictures.group" should "skip a non-digit .toml and keep a digit sidecar" in :
    withTemp: directory =>
      val jpg: File = touch(directory, "2412.jpg")
      val sidecar: File = touch(directory, "2412.toml")
      val album: File = touch(directory, "Purim Seuda.toml")
      Pictures.group(Seq(jpg, sidecar, album), NameMode.Leave) shouldBe Seq("2412" -> Set("jpg", "toml"))
      album.getName shouldBe "Purim Seuda.toml"
      sidecar.getName shouldBe "2412.toml"

  it should "not rename an album file when fixing names" in :
    withTemp: directory =>
      val album: File = touch(directory, "IMG_notes.toml")
      val picture: File = touch(directory, "IMG_2412.JPG")
      Pictures.group(Seq(album, picture), NameMode.Apply) shouldBe Seq("2412" -> Set("jpg"))
      album.exists shouldBe true
      album.getName shouldBe "IMG_notes.toml"
      picture.exists shouldBe false
      File(directory.toFile, "2412.jpg").exists shouldBe true

  it should "drop one case-insensitive camera prefix" in :
    val cases: Seq[(String, String, String)] = Seq(
      ("PXL_1111.jpg", "1111", "jpg"),
      ("img_2222.jpg", "2222", "jpg"),
      ("MVIMG_3333.jpg", "3333", "jpg"),
      ("Pxl_4444.JPG", "4444", "jpg"),
      ("MvImg_20260603_120000000.MP.jpg", "20260603_120000000.MP", "jpg")
    )
    cases.foreach: (from, stem, extension) =>
      withTemp: directory =>
        val picture: File = touch(directory, from)
        Pictures.group(Seq(picture), NameMode.Apply) shouldBe Seq(stem -> Set(extension))
        picture.exists shouldBe false
        File(directory.toFile, s"$stem.$extension").isFile shouldBe true

  it should "not match a hyphen or an unlisted prefix" in :
    withTemp: directory =>
      val names: Seq[String] = Seq(
        "IMG-1234.jpg",
        "VID-1234.jpg",
        "PANO_1234.jpg",
        "motion_photo_1234.jpg",
        "Screenshot_20191220.jpg"
      )
      val files: Seq[File] = names.map(touch(directory, _))
      val out: String = capture:
        Pictures.group(files, NameMode.Apply) shouldBe Seq(
          "IMG-1234" -> Set("jpg"),
          "PANO_1234" -> Set("jpg"),
          "Screenshot_20191220" -> Set("jpg"),
          "VID-1234" -> Set("jpg"),
          "motion_photo_1234" -> Set("jpg")
        )
      files.foreach(_.exists shouldBe true)
      out shouldBe ""

  it should "drop only one leading prefix" in :
    withTemp: directory =>
      val picture: File = touch(directory, "IMG_VID_1.jpg")
      val out: String = capture:
        Pictures.group(Seq(picture), NameMode.Apply) shouldBe Seq("VID_1" -> Set("jpg"))
      picture.exists shouldBe true
      picture.getName shouldBe "IMG_VID_1.jpg"
      File(directory.toFile, "VID_1.jpg").exists shouldBe false
      File(directory.toFile, "1.jpg").exists shouldBe false
      out should include (s"$picture: NOT RENAMED; VID_1.jpg does not start with a digit")
      out should not include ("moving")
      out should not include ("WRONG NAME")

    withTemp: directory =>
      val picture: File = touch(directory, "IMG_VID_1.jpg")
      val out: String = capture:
        Pictures.group(Seq(picture), NameMode.Preview) shouldBe Seq("VID_1" -> Set("jpg"))
      picture.getName shouldBe "IMG_VID_1.jpg"
      out should include (s"$picture: NOT RENAMED; VID_1.jpg does not start with a digit")
      out should not include ("moving")
      out should not include ("WRONG NAME")

    withTemp: directory =>
      val picture: File = touch(directory, "IMG_VID_1.jpg")
      val out: String = capture:
        Pictures.group(Seq(picture), NameMode.Leave) shouldBe Seq("VID_1" -> Set("jpg"))
      picture.getName shouldBe "IMG_VID_1.jpg"
      out should include (s"$picture: WRONG NAME; SHOULD BE VID_1.jpg; repair --names")
      out should include (s"$picture: NOT RENAMED; VID_1.jpg does not start with a digit")
      out should not include ("moving")

    withTemp: directory =>
      val picture: File = touch(directory, "img_AbC.jpg")
      val out: String = capture:
        Pictures.group(Seq(picture), NameMode.Apply) shouldBe Seq("AbC" -> Set("jpg"))
      picture.getName shouldBe "img_AbC.jpg"
      out should include (s"$picture: NOT RENAMED; AbC.jpg does not start with a digit")
      out should not include ("moving")

    withTemp: directory =>
      val picture: File = touch(directory, "Screenshot_1.jpg")
      val out: String = capture:
        Pictures.group(Seq(picture), NameMode.Leave) shouldBe Seq("Screenshot_1" -> Set("jpg"))
      picture.getName shouldBe "Screenshot_1.jpg"
      out should not include ("WRONG NAME")
      out should not include ("NOT RENAMED")

  it should "throw Non-digit name start from lsd" in :
    withDay("Screenshot_1.jpg"): (day, picture) =>
      val out: String = capture:
        val thrown: IllegalArgumentException = intercept[IllegalArgumentException](day.lsd(NameMode.Leave))
        thrown.getMessage should include ("Non-digit name start")
      picture.getName shouldBe "Screenshot_1.jpg"
      out should not include ("WRONG NAME")
      out should not include ("NOT RENAMED")

    withDay("IMG_.jpg"): (day, picture) =>
      val out: String = capture:
        val thrown: IllegalArgumentException = intercept[IllegalArgumentException](day.lsd(NameMode.Leave))
        thrown.getMessage should include ("Non-digit name start")
      picture.getName shouldBe "IMG_.jpg"
      File(picture.getParentFile, ".jpg").exists shouldBe false
      out should include ("NOT RENAMED")
      out should include ("WRONG NAME")
      out should not include ("moving")

  private def touch(directory: Path, name: String): File =
    JFiles.createFile(directory.resolve(name)).toFile

  private def withTemp(body: Path => Unit): Unit =
    val directory: Path = JFiles.createTempDirectory("walkers")
    try body(directory)
    finally deleteTree(directory.toFile)

  private def withDay(fileName: String)(body: (Pictures, File) => Unit): Unit =
    withTemp: directory =>
      val dayDir: Path = directory.resolve("2009").resolve("03").resolve("10")
      JFiles.createDirectories(dayDir)
      val picture: File = touch(dayDir, fileName)
      val home: File = File(System.getProperty("user.home"))
      val rootPath: String = home.toPath.relativize(directory).toString
      val root: Root = Root(Configuration(rootPath, ZoneId.of("America/New_York")))
      root.parse("2009/03/10") match
        case day: Pictures => body(day, picture)
        case other => fail(s"$other is not a picture directory")

  private def capture(body: => Unit): String =
    val buffer: ByteArrayOutputStream = ByteArrayOutputStream()
    Console.withOut(buffer)(body)
    buffer.toString(StandardCharsets.UTF_8)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
