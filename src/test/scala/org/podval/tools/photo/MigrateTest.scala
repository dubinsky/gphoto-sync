package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files as JFiles
import java.time.{LocalDate, ZoneId}

class MigrateTest extends AnyFlatSpec, Matchers:
  private val zone: ZoneId = ZoneId.of("America/New_York")

  "migrate" should "rewrite a month-word event and keep the folder-day claim" in :
    withOriginals: originals =>
      writeBytes(originals, "2011/03/07/0002.jpg", Array[Byte](1))
      writeBytes(originals, "2011/03/07/0017.jpg", Array[Byte](2))
      writeBytes(originals, "2011/03/07/0.jpg", Array[Byte](3))
      writeBytes(originals, "2011/03/27/0100.jpg", Array[Byte](4))
      writeBytes(originals, "2011/03/28/0100.jpg", Array[Byte](5))
      val old: File = write(
        originals,
        "2011/03/07/March April.toml",
        "external = true\nto = 2011-03-27\nname-from = \"0002\"\nname-to = \"7959\"\nexclude = [\"0017\"]\n"
      )
      val (out, thrown) = capture(Migrate.run(originals, zone, dryRun = false, checkWeights = false))
      expectOk(thrown, out)
      lines(out) shouldBe Seq(
        "title \"2011/03/07/March April\" -> \"2011/03/07-27\"",
        "file 2011/03/07/March April.toml -> 2011/03/07/to-27.toml"
      )
      old.isFile shouldBe false
      val file: File = File(originals, "2011/03/07/to-27.toml")
      val album: Album = Album.read(originals, file)
      album.title shouldBe "2011/03/07-27"
      album.external shouldBe true
      val got: Set[(LocalDate, String)] = expanded(originals, album)
      got.equals(Set(
        LocalDate.of(2011, 3, 7) -> "0002",
        LocalDate.of(2011, 3, 27) -> "0100"
      )) shouldBe true

  it should "write include by hand and copy google metadata" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(
        originals,
        "2009/03/10/Party.toml",
        "external = true\ngoogle-title = \"Hawaii \"\nowner = \"Ada Owner\"\n"
      )
      val (out, thrown) = capture(Migrate.run(originals, zone, dryRun = false, checkWeights = false))
      expectOk(thrown, out)
      out.trim shouldBe ""
      Files.read(File(originals, "2009/03/10/Party.toml")) shouldBe
        """external = true
          |google-title = "Hawaii "
          |owner = "Ada Owner"
          |include = [
          |  "all",
          |]
          |""".stripMargin

  it should "move a day file into the day folder" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/02/2000.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/02/2001.jpg", Array[Byte](2))
      val old: File = write(originals, "2009/03/02.toml", "external = true\nexclude = [\"2001\"]\n")
      val (out, thrown) = capture(Migrate.run(originals, zone, dryRun = false, checkWeights = false))
      expectOk(thrown, out)
      lines(out) shouldBe Seq("file 2009/03/02.toml -> 2009/03/02/album.toml")
      old.isFile shouldBe false
      val album: Album = Album.read(originals, File(originals, "2009/03/02/album.toml"))
      album.title shouldBe "2009/03/02"
      expanded(originals, album).equals(Set(LocalDate.of(2009, 3, 2) -> "2000")) shouldBe true

  it should "keep a titled month and move a plain month" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/01/1000.jpg", Array[Byte](1))
      writeBytes(originals, "2009/04/01/1000.jpg", Array[Byte](2))
      write(originals, "2009/03/Trip.toml", "external = true\n")
      val plain: File = write(originals, "2009/04.toml", "external = true\n")
      val (out, thrown) = capture(Migrate.run(originals, zone, dryRun = false, checkWeights = false))
      expectOk(thrown, out)
      lines(out) shouldBe Seq("file 2009/04.toml -> 2009/04/album.toml")
      plain.isFile shouldBe false
      val trip: Album = Album.read(originals, File(originals, "2009/03/Trip.toml"))
      trip.title shouldBe "2009/03/Trip"
      trip.include.map(Include.logical) shouldBe Seq("all")
      val month: Album = Album.read(originals, File(originals, "2009/04/album.toml"))
      month.title shouldBe "2009/04"
      expanded(originals, month).equals(Set(LocalDate.of(2009, 4, 1) -> "1000")) shouldBe true

  it should "fail on from or a date-time to and write nothing" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val good: String = "external = true\n"
      val bad: String = "external = true\nfrom = 2009-03-11\n"
      write(originals, "2009/03/10/Good.toml", good)
      write(originals, "2009/03/11/Bad.toml", bad)
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val (out, thrown) = capture(Migrate.run(originals, zone, dryRun = false, checkWeights = false))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage should include ("from is not a folder-day bound")
      out.trim shouldBe ""
      snapshot(originals) shouldBe before
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val text: String = "external = true\nto = 2009-03-10T18:00\n"
      write(originals, "2009/03/10/Party.toml", text)
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val (out, thrown) = capture(Migrate.run(originals, zone, dryRun = false, checkWeights = false))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage should include ("to must be a date")
      out.trim shouldBe ""
      snapshot(originals) shouldBe before

  it should "print the review on a dry run and write nothing" in :
    withOriginals: originals =>
      writeBytes(originals, "2010/07/19/0562.jpg", Array[Byte](1))
      write(originals, "2010/07/19/July August.toml", "external = true\nto = 2010-08-23\n")
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val (out, thrown) = capture(Migrate.run(originals, zone, dryRun = true, checkWeights = false))
      expectOk(thrown, out)
      lines(out) shouldBe Seq(
        "title \"2010/07/19/July August\" -> \"2010/07/19-08/23\"",
        "file 2010/07/19/July August.toml -> 2010/07/19/to-08-23.toml"
      )
      snapshot(originals) shouldBe before
      File(originals, "2010/07/19/to-08-23.toml").isFile shouldBe false

  it should "skip a file that already has include" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      write(
        originals,
        "2009/03/10/Party.toml",
        "external = true\ngoogle-title = \"Hawaii \"\nowner = \"Ada Owner\"\n"
      )
      val (firstOut, firstThrown) = capture(Migrate.run(originals, zone, dryRun = false, checkWeights = false))
      expectOk(firstThrown, firstOut)
      val written: Seq[Byte] = bytes(File(originals, "2009/03/10/Party.toml"))
      val (secondOut, secondThrown) = capture(Migrate.run(originals, zone, dryRun = false, checkWeights = false))
      expectOk(secondThrown, secondOut)
      secondOut.trim shouldBe ""
      bytes(File(originals, "2009/03/10/Party.toml")) shouldBe written

  it should "drop a millis clock that falls outside the folder day" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      writeBytes(originals, "2009/03/11/1236740400000.jpg", Array[Byte](2))
      write(originals, "2009/03/10/Party.toml", "external = true\n")
      val (out, thrown) = capture(Migrate.run(originals, zone, dryRun = false, checkWeights = false))
      expectOk(thrown, out)
      lines(out) shouldBe Seq("drop-time 2009/03/11/1236740400000 \"2009/03/10/Party\"")
      val album: Album = Album.read(originals, File(originals, "2009/03/10/Party.toml"))
      expanded(originals, album).equals(Set(LocalDate.of(2009, 3, 10) -> "2412")) shouldBe true

  it should "reject a curated file and write nothing" in :
    withOriginals: originals =>
      writeBytes(originals, "2009/03/10/2412.jpg", Array[Byte](1))
      val text: String = "members = [\"2009/03/10/2412\"]\n"
      write(originals, "Best.toml", text)
      val before: Map[String, Seq[Byte]] = snapshot(originals)
      val (out, thrown) = capture(Migrate.run(originals, zone, dryRun = false, checkWeights = false))
      thrown.get shouldBe a[IllegalArgumentException]
      thrown.get.getMessage should include ("curated albums are not migrated")
      out.trim shouldBe ""
      snapshot(originals) shouldBe before

  private def expanded(originals: File, album: Album): Set[(LocalDate, String)] =
    val universe: Map[LocalDate, Set[String]] = Archive.universe(Archive.pictures(originals))
    album.include.foldLeft(Set.empty[(LocalDate, String)]): (acc, entry) =>
      acc ++ Include.expand(album.span, entry, universe)

  private def lines(out: String): Seq[String] = out.split('\n').toSeq.filter(_.nonEmpty)

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
    val originals: File = JFiles.createTempDirectory("migrate").toFile
    try body(originals)
    finally deleteTree(originals)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
