package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.nio.file.Files as JFiles
import java.time.ZoneId

class ListingTest extends AnyFlatSpec, Matchers:
  private val zone: ZoneId = ZoneId.of("America/New_York")

  "Root.lsd" should "prepend .upload and skip a dot-directory" in :
    withArchive: archive =>
      JFiles.createDirectory(archive.toPath.resolve("2009"))
      JFiles.createDirectory(archive.toPath.resolve(".gphoto-sync"))
      JFiles.createFile(archive.toPath.resolve("AGENTS.md"))
      JFiles.createFile(archive.toPath.resolve("Best.toml"))
      rootFor(archive).lsd.map(_.toString) shouldBe Seq("/.upload", "/2009")

  it should "list .upload once when the directory exists" in :
    withArchive: archive =>
      JFiles.createDirectory(archive.toPath.resolve("2009"))
      JFiles.createDirectory(archive.toPath.resolve(".upload"))
      rootFor(archive).lsd.map(_.toString) shouldBe Seq("/.upload", "/2009")

  "Year.lsd" should "skip a dot-directory and keep the month" in :
    withArchive: archive =>
      val year: File = JFiles.createDirectory(archive.toPath.resolve("2009")).toFile
      JFiles.createDirectory(year.toPath.resolve(".secret"))
      JFiles.createDirectory(year.toPath.resolve("03"))
      rootFor(archive).year(2009).lsd.map(_.toString) shouldBe Seq("/2009/03")

  "Month.lsd" should "skip a dot-directory and keep the day" in :
    withArchive: archive =>
      val year: File = JFiles.createDirectory(archive.toPath.resolve("2009")).toFile
      val month: File = JFiles.createDirectory(year.toPath.resolve("03")).toFile
      JFiles.createDirectory(month.toPath.resolve(".secret"))
      JFiles.createDirectory(month.toPath.resolve("01"))
      rootFor(archive).year(2009).month(3).lsd.map(_.toString) shouldBe Seq("/2009/03/01")

  it should "reject a visible non-numeric directory" in :
    withArchive: archive =>
      val year: File = JFiles.createDirectory(archive.toPath.resolve("2009")).toFile
      JFiles.createDirectory(year.toPath.resolve("foo"))
      an [NumberFormatException] should be thrownBy rootFor(archive).year(2009).lsd

  "Root.item" should "route .upload to the inbox when the directory is absent" in :
    withArchive: archive =>
      val inbox: Directory = rootFor(archive).item(".upload")
      inbox.name shouldBe ".upload"
      inbox shouldBe a [UploadDirectory]

  it should "reject .gphoto-sync as not a year" in :
    withArchive: archive =>
      val exception: IllegalArgumentException = intercept[IllegalArgumentException]:
        rootFor(archive).item(".gphoto-sync")
      exception.getMessage should include ("not a year")

  it should "reject the old Upload name" in :
    withArchive: archive =>
      an [NumberFormatException] should be thrownBy rootFor(archive).item("Upload")

  private def rootFor(archive: File): Root =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    Root(Configuration(rootPath, zone))

  private def withArchive(body: File => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("listing").toFile
    try body(archive)
    finally deleteTree(archive)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
