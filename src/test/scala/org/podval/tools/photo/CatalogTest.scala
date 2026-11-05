package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NFiles}
import java.time.LocalDate

class CatalogTest extends AnyFlatSpec, Matchers:
  "Catalog" should "reject a file that starts with ## without parsing the rest" in:
    withTemp: dir =>
      val garbage: String = "## \nitem\t\t\tnot-a-date\t\t\nwrong column count\n"
      val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
        Catalog.read(write(dir, "catalog.txt", garbage))
      caught.getMessage shouldBe "catalog is not version 2"
      val raw: Array[Byte] = "## \n".getBytes(StandardCharsets.UTF_8) :+ 0xff.toByte
      val binary: File = writeBytes(dir, "binary.txt", raw)
      val binaryCaught: IllegalArgumentException = intercept[IllegalArgumentException]:
        Catalog.read(binary)
      binaryCaught.getMessage shouldBe "catalog is not version 2"

  it should "reject a second owner" in:
    withTemp: dir =>
      val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
        Catalog.read(catalog(dir, "album\ta\t0\tJune", "owner\ta\tAda", "owner\ta\tBea"))
      caught.getMessage shouldBe "second owner"

  it should "parse a shortfall whose name-failures are 0" in:
    withTemp: dir =>
      Catalog.read(
        catalog(
          dir,
          "album\ta\t5\tJune ",
          "error\ta\t2\t5\t0",
          "item\ta\tone.jpg\t2010-06-01\tk1\t-",
          "item\ta\ttwo.jpg\t-\tk2\tAda"
        )
      ) shouldBe Seq(
        Catalog.Album(
          id = "a",
          count = 5,
          title = "June ",
          owner = None,
          autoAdd = false,
          error = Some(Catalog.Error(got = 2, of = 5, nameFailures = 0)),
          items = Seq(
            Catalog.Item("one.jpg", Some(LocalDate.parse("2010-06-01")), "k1", None),
            Catalog.Item("two.jpg", None, "k2", Some("Ada"))
          ),
          unsets = Seq.empty
        )
      )

  it should "count an unset line as a name failure" in:
    withTemp: dir =>
      val album: Catalog.Album = Catalog
        .read(
          catalog(
            dir,
            "item\ta\tone.jpg\t2012-02-29\tk1\t-",
            "unset\ta\t2010-06-02\tk2\tBea",
            "album\ta\t4\tJune",
            "error\ta\t2\t4\t1"
          )
        )
        .head
      album.error shouldBe Some(Catalog.Error(got = 2, of = 4, nameFailures = 1))
      album.items.length shouldBe 1
      album.unsets shouldBe Seq(Catalog.Unset(Some(LocalDate.parse("2010-06-02")), "k2", Some("Bea")))
      album.error.get.nameFailures shouldBe album.unsets.length

  it should "require error.got to equal item lines plus unset lines" in:
    withTemp: dir =>
      val parsed: Catalog.Album = Catalog
        .read(
          catalog(
            dir,
            "album\ta\t4\tJune",
            "error\ta\t3\t4\t1",
            "item\ta\tone.jpg\t-\tk1\t-",
            "item\ta\ttwo.jpg\t-\tk2\t-",
            "unset\ta\t-\tk3\t-"
          )
        )
        .head
      parsed.error.get.got shouldBe parsed.items.length + parsed.unsets.length
      val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
        Catalog.read(
          catalog(
            dir,
            "album\ta\t4\tJune",
            "error\ta\t2\t4\t1",
            "item\ta\tone.jpg\t-\tk1\t-",
            "item\ta\ttwo.jpg\t-\tk2\t-",
            "unset\ta\t-\tk3\t-"
          )
        )
      caught.getMessage shouldBe "error.got does not equal item lines plus unset lines"
      val above: IllegalArgumentException = intercept[IllegalArgumentException]:
        Catalog.read(
          catalog(
            dir,
            "album\ta\t2\tJune",
            "error\ta\t3\t2\t0",
            "item\ta\tone.jpg\t-\tk1\t-",
            "item\ta\ttwo.jpg\t-\tk2\t-",
            "item\ta\tthree.jpg\t-\tk3\t-"
          )
        )
      above.getMessage shouldBe "error.got is greater than error.of"

  it should "reject a repeated album id and item key on item or unset" in:
    withTemp: dir =>
      val pairs: Seq[Seq[String]] = Seq(
        Seq("album\ta\t2\tJune", "item\ta\tone.jpg\t-\tk1\t-", "item\ta\ttwo.jpg\t-\tk1\t-"),
        Seq("album\ta\t0\tJune", "error\ta\t2\t0\t2", "unset\ta\t-\tk1\t-", "unset\ta\t-\tk1\t-"),
        Seq(
          "album\ta\t2\tJune",
          "error\ta\t2\t2\t1",
          "item\ta\tone.jpg\t-\tk1\t-",
          "unset\ta\t-\tk1\t-"
        )
      )
      pairs.foreach: lines =>
        val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
          Catalog.read(catalog(dir, lines*))
        caught.getMessage shouldBe "repeated item key"
      Catalog
        .read(
          catalog(
            dir,
            "album\ta\t1\tJune",
            "item\ta\tone.jpg\t-\tk1\t-",
            "album\tb\t1\tJuly",
            "item\tb\ttwo.jpg\t-\tk1\t-"
          )
        )
        .map(_.id) shouldBe Seq("a", "b")

  it should "reject an empty item filename" in:
    withTemp: dir =>
      val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
        Catalog.read(catalog(dir, "album\ta\t1\tJune", "item\ta\t\t2010-06-01\tk1\t-"))
      caught.getMessage shouldBe "empty item filename"
      val kept: Catalog.Item = Catalog
        .read(
          catalog(
            dir,
            "album\ta\t1\t June",
            "item\ta\t one.jpg \t-\tk1\t Ada "
          )
        )
        .head
        .items
        .head
      kept.filename shouldBe " one.jpg "
      kept.uploader shouldBe Some(" Ada ")

  it should "reject a missing file, a CR, a blank line, and a partial line" in:
    withTemp: dir =>
      val absent: File = File(dir, "catalog.txt")
      val missing: IllegalArgumentException = intercept[IllegalArgumentException]:
        Catalog.read(absent)
      missing.getMessage shouldBe "catalog is absent"
      absent.exists shouldBe false
      message(dir, "# gphoto-catalog 2\nalbum\ta\t0\tJune\r\n") shouldBe "carriage return"
      message(dir, "# gphoto-catalog 2\n\nalbum\ta\t0\tJune\n") shouldBe "blank line"
      message(dir, "# gphoto-catalog 2\nalbum\ta\t0\tJune") shouldBe "trailing partial line"
      val bom: Array[Byte] = Array(0xef.toByte, 0xbb.toByte, 0xbf.toByte) ++
        "# gphoto-catalog 2\n".getBytes(StandardCharsets.UTF_8)
      val bomCaught: IllegalArgumentException = intercept[IllegalArgumentException]:
        Catalog.read(writeBytes(dir, "bom.txt", bom))
      bomCaught.getMessage shouldBe "catalog is not version 2"

  it should "reject a bad integer, date, uploader, or owner label" in:
    withTemp: dir =>
      message(dir, "# gphoto-catalog 2\nalbum\ta\t01\tJune\n") shouldBe "leading zero"
      message(dir, "# gphoto-catalog 2\nalbum\ta\t2147483648\tJune\n") shouldBe "integer does not fit"
      message(dir, "# gphoto-catalog 2\nalbum\ta\t-1\tJune\n") shouldBe "not an integer"
      message(dir, "# gphoto-catalog 2\nalbum\ta\t1\tJune\nitem\ta\tone.jpg\t2011-02-29\tk1\t-\n") shouldBe "not a date"
      message(dir, "# gphoto-catalog 2\nalbum\ta\t1\tJune\nitem\ta\tone.jpg\t\tk1\t-\n") shouldBe "empty date"
      message(dir, "# gphoto-catalog 2\nalbum\ta\t1\tJune\nitem\ta\tone.jpg\t-\tk1\t\n") shouldBe "empty uploader"
      message(dir, "# gphoto-catalog 2\nalbum\ta\t0\tJune\nowner\ta\t\n") shouldBe "owner label is empty"
      message(dir, "# gphoto-catalog 2\nalbum\ta\t0\tJune\nowner\ta\t   \n") shouldBe "owner label is only whitespace"

  it should "keep an owner on an auto-add album and reject its items" in:
    withTemp: dir =>
      val album: Catalog.Album = Catalog
        .read(
          catalog(
            dir,
            "album\tb\t10\tAuto",
            "auto-add\tb",
            "owner\tb\tAda: Owner"
          )
        )
        .head
      album.owner shouldBe Some("Ada: Owner")
      album.autoAdd shouldBe true
      album.items shouldBe Seq.empty[Catalog.Item]
      val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
        Catalog.read(catalog(dir, "album\tb\t0\tAuto", "auto-add\tb", "item\tb\tone.jpg\t-\tk1\t-"))
      caught.getMessage shouldBe "auto-add has an item"

  it should "treat a count of 0 with no items as an empty album" in:
    withTemp: dir =>
      val album: Catalog.Album = Catalog.read(catalog(dir, "album\ta\t0\tJune")).head
      album.count shouldBe 0
      album.items shouldBe Seq.empty[Catalog.Item]
      val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
        Catalog.read(catalog(dir, "album\ta\t2\tJune", "item\ta\tone.jpg\t-\tk1\t-"))
      caught.getMessage shouldBe "item count does not equal count"
      val unset: IllegalArgumentException = intercept[IllegalArgumentException]:
        Catalog.read(catalog(dir, "album\ta\t0\tJune", "unset\ta\t-\tk1\t-"))
      unset.getMessage shouldBe "unset without an error"

  it should "reject a line whose album is missing and a second album, auto-add, or error" in:
    withTemp: dir =>
      Seq(
        "owner\ta\tAda",
        "auto-add\ta",
        "error\ta\t0\t0\t0",
        "unset\ta\t-\tk1\t-",
        "item\ta\tone.jpg\t-\tk1\t-"
      ).foreach: line =>
        val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
          Catalog.read(catalog(dir, line))
        caught.getMessage shouldBe "missing album"
      messageOf(dir, "album\ta\t0\tJune", "album\ta\t0\tJuly") shouldBe "second album"
      messageOf(dir, "album\ta\t0\tJune", "auto-add\ta", "auto-add\ta") shouldBe "second auto-add"
      messageOf(dir, "album\ta\t0\tJune", "error\ta\t0\t0\t0", "error\ta\t0\t0\t0") shouldBe "second error"
      messageOf(dir, "album\ta\t1\tJune", "error\ta\t1\t2\t0", "item\ta\tone.jpg\t-\tk1\t-") shouldBe
        "error.of does not equal count"
      messageOf(dir, "album\ta\t1\tJune", "error\ta\t1\t1\t1", "item\ta\tone.jpg\t-\tk1\t-") shouldBe
        "name-failures does not equal unset count"

  private def message(dir: File, text: String): String =
    val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
      Catalog.read(write(dir, "catalog.txt", text))
    caught.getMessage

  private def messageOf(dir: File, lines: String*): String =
    val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
      Catalog.read(catalog(dir, lines*))
    caught.getMessage

  private def catalog(dir: File, lines: String*): File =
    val body: String = if lines.isEmpty then "" else lines.mkString("\n") + "\n"
    write(dir, "catalog.txt", "# gphoto-catalog 2\n" + body)

  private def write(dir: File, name: String, text: String): File =
    writeBytes(dir, name, text.getBytes(StandardCharsets.UTF_8))

  private def writeBytes(dir: File, name: String, bytes: Array[Byte]): File =
    val file: File = File(dir, name)
    NFiles.write(file.toPath, bytes)
    file

  private def withTemp(body: File => Unit): Unit =
    val dir: File = NFiles.createTempDirectory("gphoto-catalog").toFile
    try body(dir)
    finally remove(dir)

  private def remove(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(remove))
    val _: Boolean = file.delete()
