package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
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

  it should "add v to a video that shares a repaired stem with a still" in :
    withTemp: directory =>
      val still: File = touch(directory, "20191220_141955.jpg")
      val video: File = touch(directory, "VID_20191220_141955.mp4")
      val out: String = capture:
        Pictures.group(Seq(still, video), NameMode.Leave) shouldBe Seq(
          "20191220_141955" -> Set("jpg"),
          "20191220_141955v" -> Set("mp4")
        )
      still.getName shouldBe "20191220_141955.jpg"
      video.getName shouldBe "VID_20191220_141955.mp4"
      out should include (s"$video: WRONG NAME; SHOULD BE 20191220_141955v.mp4; repair --names")
      out should not include ("NOT RENAMED")

    withTemp: directory =>
      val still: File = touch(directory, "20191220_141955.jpg")
      val video: File = touch(directory, "VID_20191220_141955.mp4")
      val destination: File = File(directory.toFile, "20191220_141955v.mp4")
      val out: String = capture:
        Pictures.group(Seq(still, video), NameMode.Preview) shouldBe Seq(
          "20191220_141955" -> Set("jpg"),
          "20191220_141955v" -> Set("mp4")
        )
      still.getName shouldBe "20191220_141955.jpg"
      video.getName shouldBe "VID_20191220_141955.mp4"
      destination.exists shouldBe false
      out should include (s"moving $video to $destination")
      out should not include (s"moving $still")

    withTemp: directory =>
      val still: File = touch(directory, "20191220_141955.jpg")
      val video: File = touch(directory, "VID_20191220_141955.mp4")
      val destination: File = File(directory.toFile, "20191220_141955v.mp4")
      capture:
        Pictures.group(Seq(still, video), NameMode.Apply) shouldBe Seq(
          "20191220_141955" -> Set("jpg"),
          "20191220_141955v" -> Set("mp4")
        )
      still.getName shouldBe "20191220_141955.jpg"
      video.exists shouldBe false
      destination.isFile shouldBe true
      File(directory.toFile, "20191220_141955vv.mp4").exists shouldBe false
      val again: String = capture:
        Pictures.group(Seq(still, destination), NameMode.Leave) shouldBe Seq(
          "20191220_141955" -> Set("jpg"),
          "20191220_141955v" -> Set("mp4")
        )
      again shouldBe ""

    withTemp: directory =>
      val still: File = touch(directory, "1.gif")
      val video: File = touch(directory, "1.mp4")
      Pictures.group(Seq(still, video), NameMode.Apply) shouldBe Seq(
        "1" -> Set("gif"),
        "1v" -> Set("mp4")
      )
      still.getName shouldBe "1.gif"
      File(directory.toFile, "1v.mp4").isFile shouldBe true

  it should "add v to m4v beside jpg and to mp4 beside jpeg" in :
    withTemp: directory =>
      val still: File = touch(directory, "1.jpg")
      val video: File = touch(directory, "1.m4v")
      Pictures.group(Seq(still, video), NameMode.Apply) shouldBe Seq(
        "1" -> Set("jpg"),
        "1v" -> Set("m4v")
      )
      still.getName shouldBe "1.jpg"
      File(directory.toFile, "1v.m4v").isFile shouldBe true

    withTemp: directory =>
      val still: File = touch(directory, "1.jpeg")
      val video: File = touch(directory, "1.mp4")
      Pictures.group(Seq(still, video), NameMode.Apply) shouldBe Seq(
        "1" -> Set("jpeg"),
        "1v" -> Set("mp4")
      )
      still.getName shouldBe "1.jpeg"
      still.exists shouldBe true
      File(directory.toFile, "1v.mp4").isFile shouldBe true

  it should "omit two videos that would share a v stem" in :
    withTemp: directory =>
      val still: File = touch(directory, "20191220_141955.jpg")
      val mp4: File = touch(directory, "20191220_141955.mp4")
      val mov: File = touch(directory, "20191220_141955.mov")
      val out: String = capture:
        Pictures.group(Seq(still, mp4, mov), NameMode.Apply) shouldBe Seq(
          "20191220_141955" -> Set("jpg")
        )
      still.getName shouldBe "20191220_141955.jpg"
      mp4.getName shouldBe "20191220_141955.mp4"
      mov.getName shouldBe "20191220_141955.mov"
      out should include (s"$mp4: NOT RENAMED; 20191220_141955v would have two normal files")
      out should include (s"$mov: NOT RENAMED; 20191220_141955v would have two normal files")
      out should not include ("moving")

  it should "return two normals that are already literal" in :
    withTemp: directory =>
      val mp4: File = touch(directory, "20191220_141955v.mp4")
      val mov: File = touch(directory, "20191220_141955v.mov")
      val out: String = capture:
        Pictures.group(Seq(mp4, mov), NameMode.Leave) shouldBe Seq(
          "20191220_141955v" -> Set("mov", "mp4")
        )
      out shouldBe ""
      mp4.exists shouldBe true
      mov.exists shouldBe true

    withDay("2412.jpg"): (day, jpg) =>
      touch(jpg.getParentFile.toPath, "2412.gif")
      val out: String = capture:
        val thrown: IllegalArgumentException = intercept[IllegalArgumentException](day.lsd(NameMode.Leave))
        thrown.getMessage should include ("too many normal extensions")
      jpg.getName shouldBe "2412.jpg"
      out should not include ("NOT RENAMED")

    withDay("20191220_141955v.mp4"): (day, mp4) =>
      touch(mp4.getParentFile.toPath, "20191220_141955v.mov")
      val thrown: IllegalArgumentException = intercept[IllegalArgumentException](day.lsd(NameMode.Leave))
      thrown.getMessage should include ("too many normal extensions")

  it should "keep a Pixel motion still off the plain stem" in :
    withTemp: directory =>
      val motion: File = touch(directory, "PXL_20260603_120000000.MP.jpg")
      val plain: File = touch(directory, "PXL_20260603_120000000.jpg")
      val video: File = touch(directory, "PXL_20260603_120000000.mp4")
      Pictures.group(Seq(motion, plain, video), NameMode.Apply) shouldBe Seq(
        "20260603_120000000" -> Set("jpg"),
        "20260603_120000000.MP" -> Set("jpg"),
        "20260603_120000000v" -> Set("mp4")
      )
      motion.exists shouldBe false
      File(directory.toFile, "20260603_120000000.MP.jpg").isFile shouldBe true
      File(directory.toFile, "20260603_120000000.jpg").isFile shouldBe true
      File(directory.toFile, "20260603_120000000v.mp4").isFile shouldBe true
      File(directory.toFile, "20260603_120000000.MPv.jpg").exists shouldBe false

  it should "not rename a video onto an occupied v name" in :
    withTemp: directory =>
      val still: File = touch(directory, "20191220_141955.jpg")
      val occupant: File = touch(directory, "20191220_141955v.mp4")
      val incoming: File = touch(directory, "VID_20191220_141955.mp4")
      val out: String = capture:
        Pictures.group(Seq(still, occupant, incoming), NameMode.Apply) shouldBe Seq(
          "20191220_141955" -> Set("jpg"),
          "20191220_141955v" -> Set("mp4")
        )
      still.getName shouldBe "20191220_141955.jpg"
      occupant.getName shouldBe "20191220_141955v.mp4"
      incoming.getName shouldBe "VID_20191220_141955.mp4"
      File(directory.toFile, "20191220_141955vv.mp4").exists shouldBe false
      out should include (s"$incoming: NOT RENAMED; 20191220_141955v.mp4 already exists")
      out should not include ("moving")

  it should "not move onto a v name whose file is leaving" in :
    withTemp: directory =>
      val still: File = touch(directory, "2412.jpg")
      val video: File = touch(directory, "2412.mp4")
      val otherStill: File = touch(directory, "2412v.jpg")
      val otherVideo: File = touch(directory, "2412v.mp4")
      val out: String = capture:
        Pictures.group(Seq(still, video, otherStill, otherVideo), NameMode.Apply) shouldBe Seq(
          "2412" -> Set("jpg"),
          "2412v" -> Set("jpg"),
          "2412vv" -> Set("mp4")
        )
      still.getName shouldBe "2412.jpg"
      video.getName shouldBe "2412.mp4"
      otherStill.getName shouldBe "2412v.jpg"
      otherVideo.exists shouldBe false
      File(directory.toFile, "2412vv.mp4").isFile shouldBe true
      File(directory.toFile, "2412vvv.mp4").exists shouldBe false
      out should include (s"$video: NOT RENAMED; 2412v.mp4 already exists")

  it should "not merge two jpegs or rename a prefixed raw onto that stem" in :
    withTemp: directory =>
      val first: File = touch(directory, "IMG_2412.jpg")
      val second: File = touch(directory, "MVIMG_2412.jpg")
      val raw: File = touch(directory, "IMG_2412.CR2")
      val out: String = capture:
        Pictures.group(Seq(first, second, raw), NameMode.Apply) shouldBe empty
      first.exists shouldBe true
      second.exists shouldBe true
      raw.getName shouldBe "IMG_2412.CR2"
      File(directory.toFile, "2412.jpg").exists shouldBe false
      File(directory.toFile, "2412.cr2").exists shouldBe false
      out should include (s"$first: NOT RENAMED; 2412.jpg collides with MVIMG_2412.jpg")
      out should include (s"$second: NOT RENAMED; 2412.jpg collides with IMG_2412.jpg")
      out should include (s"$raw: NOT RENAMED; 2412 is not one normal picture")

    withTemp: directory =>
      val occupant: File = touch(directory, "2412.jpg")
      val other: File = touch(directory, "IMG_2412.jpg")
      val raw: File = touch(directory, "IMG_2412.CR2")
      val out: String = capture:
        Pictures.group(Seq(occupant, other, raw), NameMode.Apply) shouldBe Seq("2412" -> Set("jpg"))
      occupant.getName shouldBe "2412.jpg"
      other.getName shouldBe "IMG_2412.jpg"
      raw.getName shouldBe "IMG_2412.CR2"
      File(directory.toFile, "2412.cr2").exists shouldBe false
      out should include (s"$other: NOT RENAMED; 2412.jpg already exists")
      out should include (s"$raw: NOT RENAMED; 2412 is not one normal picture")

  it should "rename a prefixed raw beside one occupant" in :
    withTemp: directory =>
      val jpg: File = touch(directory, "2412.jpg")
      val raw: File = touch(directory, "IMG_2412.CR2")
      Pictures.group(Seq(jpg, raw), NameMode.Apply) shouldBe Seq("2412" -> Set("cr2", "jpg"))
      jpg.getName shouldBe "2412.jpg"
      raw.exists shouldBe false
      File(directory.toFile, "2412.cr2").isFile shouldBe true

  it should "leave a jpeg a gif and a raw prefixed" in :
    withTemp: directory =>
      val jpg: File = touch(directory, "IMG_1.jpg")
      val gif: File = touch(directory, "IMG_1.gif")
      val raw: File = touch(directory, "IMG_1.CR2")
      val out: String = capture:
        Pictures.group(Seq(jpg, gif, raw), NameMode.Apply) shouldBe empty
      jpg.getName shouldBe "IMG_1.jpg"
      gif.getName shouldBe "IMG_1.gif"
      raw.getName shouldBe "IMG_1.CR2"
      File(directory.toFile, "1.jpg").exists shouldBe false
      File(directory.toFile, "1.gif").exists shouldBe false
      File(directory.toFile, "1.cr2").exists shouldBe false
      out should include (s"$jpg: NOT RENAMED; 1 would have two normal files")
      out should include (s"$gif: NOT RENAMED; 1 would have two normal files")
      out should include (s"$raw: NOT RENAMED; 1 is not one normal picture")

  it should "keep a lone heic and a jpg beside heic as one picture" in :
    withDay("1006.heic"): (day, heic) =>
      val out: String = capture:
        val pictures: Seq[Picture] = day.lsd(NameMode.Leave)
        pictures.map(_.name) shouldBe Seq("1006")
        pictures.head.image.map(_.name) shouldBe Some("heic")
        pictures.head.normal shouldBe None
        pictures.head.raw shouldBe None
      out should not include ("has no picture file")
      out should not include ("no images")
      heic.getName shouldBe "1006.heic"

    withDay("2412.jpg"): (day, jpg) =>
      val heic: File = touch(jpg.getParentFile.toPath, "2412.heic")
      val out: String = capture:
        val pictures: Seq[Picture] = day.lsd(NameMode.Leave)
        pictures.map(_.name) shouldBe Seq("2412")
        pictures.head.extensions.map(_.name) shouldBe Set("jpg", "heic")
        pictures.head.normal.map(_.name) shouldBe Some("jpg")
        pictures.head.image.map(_.name) shouldBe Some("heic")
      out should not include ("too many normal extensions")
      jpg.getName shouldBe "2412.jpg"
      heic.getName shouldBe "2412.heic"

  it should "reject two image extensions on one stem" in :
    withDay("2412.heic"): (day, heic) =>
      touch(heic.getParentFile.toPath, "2412.png")
      val thrown: IllegalArgumentException = intercept[IllegalArgumentException](day.lsd(NameMode.Leave))
      thrown.getMessage should include ("too many image extensions")

  it should "not give m4v a v suffix beside heic" in :
    withTemp: directory =>
      val heic: File = touch(directory, "1.heic")
      val video: File = touch(directory, "1.m4v")
      Pictures.group(Seq(heic, video), NameMode.Apply) shouldBe Seq("1" -> Set("heic", "m4v"))
      heic.getName shouldBe "1.heic"
      video.getName shouldBe "1.m4v"
      File(directory.toFile, "1v.m4v").exists shouldBe false

  it should "omit a lone toml thm or tif and keep a lone raw" in :
    withDay("1002.cr2"): (day, raw) =>
      val dir: Path = raw.getParentFile.toPath
      val toml: File = touch(dir, "1005.toml")
      val thm: File = touch(dir, "1003.thm")
      val tif: File = touch(dir, "1004.tif")
      val root: File = raw.getParentFile.getParentFile.getParentFile.getParentFile
      val out: String = capture:
        day.lsd(NameMode.Leave).map(_.name) shouldBe Seq("1002")
        Archive.scan(root).pictures.map(_.name) shouldBe Seq("1002")
      raw.getName shouldBe "1002.cr2"
      out should include (s"$toml: NOT RENAMED; 1005 has no picture file")
      out should include (s"$thm: NOT RENAMED; 1003 has no picture file")
      out should include (s"$tif: NOT RENAMED; 1004 has no picture file")
      out should not include ("no images")

  it should "still rename another file when one name collides" in :
    withTemp: directory =>
      val occupant: File = touch(directory, "2412.jpg")
      val blocked: File = touch(directory, "IMG_2412.jpg")
      val other: File = touch(directory, "IMG_1000.jpg")
      Pictures.group(Seq(occupant, blocked, other), NameMode.Apply).map(_._1) shouldBe Seq("1000", "2412")
      occupant.getName shouldBe "2412.jpg"
      blocked.getName shouldBe "IMG_2412.jpg"
      other.exists shouldBe false
      File(directory.toFile, "1000.jpg").isFile shouldBe true

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
    val (out, thrown) = Transcript.collect(body)
    thrown match
      case Some(error) => throw error
      case None => out

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
