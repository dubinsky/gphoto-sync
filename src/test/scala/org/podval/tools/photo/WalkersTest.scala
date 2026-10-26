package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.nio.file.Files as JFiles
import java.nio.file.Path

class WalkersTest extends AnyFlatSpec, Matchers:
  "Files.listDirectories" should "ignore a sibling .toml" in :
    withTemp: directory =>
      val month: Path = JFiles.createDirectory(directory.resolve("03"))
      JFiles.createFile(directory.resolve("03.toml"))
      Files.listDirectories(directory.toFile).map(_.getName) shouldBe Seq(month.getFileName.toString)

  it should "reject any other stray file" in :
    withTemp: directory =>
      JFiles.createDirectory(directory.resolve("03"))
      JFiles.createFile(directory.resolve("03.toml"))
      JFiles.createFile(directory.resolve("readme.txt"))
      an [IllegalArgumentException] should be thrownBy Files.listDirectories(directory.toFile)

  "Pictures.group" should "skip a non-digit .toml and keep a digit sidecar" in :
    withTemp: directory =>
      val jpg: File = touch(directory, "2412.jpg")
      val sidecar: File = touch(directory, "2412.toml")
      val album: File = touch(directory, "Purim Seuda.toml")
      Pictures.group(Seq(jpg, sidecar, album), fix = false) shouldBe Seq("2412" -> Set("jpg", "toml"))
      album.getName shouldBe "Purim Seuda.toml"
      sidecar.getName shouldBe "2412.toml"

  it should "not rename an album file when fixing names" in :
    withTemp: directory =>
      val album: File = touch(directory, "IMG_notes.toml")
      val picture: File = touch(directory, "IMG_2412.JPG")
      Pictures.group(Seq(album, picture), fix = true) shouldBe Seq("2412" -> Set("jpg"))
      album.exists shouldBe true
      album.getName shouldBe "IMG_notes.toml"
      picture.exists shouldBe false
      File(directory.toFile, "2412.jpg").exists shouldBe true

  private def touch(directory: Path, name: String): File =
    JFiles.createFile(directory.resolve(name)).toFile

  private def withTemp(body: Path => Unit): Unit =
    val directory: Path = JFiles.createTempDirectory("walkers")
    try body(directory)
    finally deleteTree(directory.toFile)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
