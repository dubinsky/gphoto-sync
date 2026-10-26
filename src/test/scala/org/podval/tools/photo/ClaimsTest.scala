package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.nio.file.Files as JFiles
import java.time.ZoneId

class ClaimsTest extends AnyFlatSpec, Matchers:
  private val zone: ZoneId = ZoneId.of("America/New_York")

  "Claims" should "date a picture inside an event to the event, not the day" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10.toml", "")
      write(originals, "2009/03/10/Purim Seuda.toml", "")
      write(originals, "Best.toml", "members = [\"2009/03/10/2412\", \"2009/01/01/0000\"]\n")
      val picture: Claims.Resolved = one(originals, "2009/03/10/2412")
      picture.dated shouldBe Some("2009/03/10/Purim Seuda")
      picture.curated shouldBe Seq("Best")

  it should "date only unclaimed pictures to the month" in :
    withOriginals: originals =>
      write(originals, "2009/03.toml", "")
      touch(originals, "2009/03/01/1000.jpg")
      touch(originals, "2009/03/02/2000.jpg")
      write(originals, "2009/03/02.toml", "")
      touch(originals, "2009/03/03/3000.jpg")
      write(originals, "2009/03/03/Party.toml", "")
      one(originals, "2009/03/01/1000").dated shouldBe Some("2009/03")
      one(originals, "2009/03/02/2000").dated shouldBe Some("2009/03/02")
      one(originals, "2009/03/03/3000").dated shouldBe Some("2009/03/03/Party")

  it should "not claim a picture named in exclude" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10.toml", "")
      write(originals, "2009/03/10/Purim Seuda.toml", "exclude = [\"2412\"]\n")
      one(originals, "2009/03/10/2412").dated shouldBe Some("2009/03/10")

  it should "honor a time-bounded from" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/2412.toml", "timestamp = \"2009-03-10T19:00\"\n")
      touch(originals, "2009/03/10/1000.jpg")
      write(originals, "2009/03/10/Late.toml", "from = 2009-03-10T18:00\n")
      one(originals, "2009/03/10/2412").dated shouldBe Some("2009/03/10/Late")
      one(originals, "2009/03/10/1000").dated shouldBe None

  it should "return Left when two events claim one picture" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/One.toml", "")
      write(originals, "2009/03/10/Two.toml", "")
      Claims(originals, zone) match
        case Left(error) =>
          error should include ("2009/03/10/2412")
          error should include ("2009/03/10/One")
          error should include ("2009/03/10/Two")
        case Right(_) => fail("expected a conflict")

  it should "reject an unknown key" in :
    withOriginals: originals =>
      touch(originals, "2009/03/10/2412.jpg")
      write(originals, "2009/03/10/Party.toml", "nope = true\n")
      val caught: IllegalArgumentException = the [IllegalArgumentException] thrownBy Claims(originals, zone)
      caught.getMessage should include ("nope")

  private def one(originals: File, path: String): Claims.Resolved = Claims(originals, zone) match
    case Left(error) => fail(error)
    case Right(pictures) => pictures.find(_.path == path).getOrElse(fail(s"missing $path"))

  private def write(originals: File, path: String, content: String): Unit =
    val file: File = File(originals, path)
    file.getParentFile.mkdirs()
    Files.write(content, file)

  private def touch(originals: File, path: String): Unit = write(originals, path, "")

  private def withOriginals(body: File => Unit): Unit =
    val originals: File = JFiles.createTempDirectory("claims").toFile
    try body(originals)
    finally deleteTree(originals)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
