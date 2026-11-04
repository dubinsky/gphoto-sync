package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import java.nio.file.Files as JFiles
import java.time.{LocalDate, ZoneId}
import javax.imageio.ImageIO

class ItemCommandTest extends AnyFlatSpec, Matchers:
  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")

  "item set date" should "store ISO, move every extension, and prune" in :
    withOriginals: archive =>
      val bytes: Array[Byte] = jpegBytes
      val day: File = File(archive, "2010/07/04")
      val jpg: File = picture(day, "2412.jpg", bytes)
      val raw: File = picture(day, "2412.cr2", bytes)
      picture(File(archive, "2011/02/03"), "IMG_9.JPG", bytes)
      write(archive, "2011/02/02/notes.txt", "stray\n")
      val root: File = sessionFor(archive).root.configuration.archive
      val (text, value) = ok(archive, "item", "set", "date=2010/08/01", "2010/07/04/2412")
      text.linesIterator.next() shouldBe "moving /2010/07/04/2412 to /2010/08/01"
      text.linesIterator.count(_ == "moving /2010/07/04/2412 to /2010/08/01") shouldBe 1
      text should not include ("WRONG NAME")
      text should not include ("NOT RENAMED")
      text should include (s"moving ${File(root, "2010/07/04/2412.jpg")} to ")
      text should include (s"moving ${File(root, "2010/07/04/2412.cr2")} to ")
      text should include ("/2010/08/01/2412: writing metadata file")
      text should include ("date = 2010-08-01")
      jpg.exists shouldBe false
      raw.exists shouldBe false
      day.exists shouldBe false
      File(archive, "2010/07").exists shouldBe false
      File(archive, "2010").isDirectory shouldBe true
      File(archive, "2010/08/01/2412.jpg").isFile shouldBe true
      File(archive, "2010/08/01/2412.cr2").isFile shouldBe true
      Metadata.read(File(archive, "2010/08/01/2412.toml")) shouldBe
        Metadata(date = Some(LocalDate.parse("2010-08-01")))
      value shouldBe setValue(false, "2010/08/01/2412", "date", Json.Str("2010-08-01"), Json.Str("2010/08/01/2412"))

  it should "accept an ISO day and keep the other sidecar keys" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      Files.write(
        Metadata.encode(Metadata(source = Some("scan"))),
        File(archive, "2010/07/04/2412.toml")
      )
      val (text, value) = ok(archive, "item", "set", "date=2010-08-01", "2010/07/04/2412")
      text.linesIterator.next() shouldBe "moving /2010/07/04/2412 to /2010/08/01"
      File(archive, "2010/07/04/2412.toml").exists shouldBe false
      val sidecar: File = File(archive, "2010/08/01/2412.toml")
      val metadata: Metadata = Metadata.read(sidecar)
      metadata shouldBe Metadata(source = Some("scan"), date = Some(LocalDate.parse("2010-08-01")))
      Files.read(sidecar) shouldBe Metadata.encode(metadata)
      value shouldBe setValue(false, "2010/08/01/2412", "date", Json.Str("2010-08-01"), Json.Str("2010/08/01/2412"))

  it should "stop the empty climb at the archive root" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      ok(archive, "item", "set", "date=2011/01/02", "2010/07/04/2412")
      File(archive, "2010").exists shouldBe false
      File(archive, "2011/01/02/2412.jpg").isFile shouldBe true
      archive.isDirectory shouldBe true
      archive.getParentFile.isDirectory shouldBe true

  it should "keep a directory that still holds an album file" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      write(archive, "2010/07/album.toml", "include = []\n")
      ok(archive, "item", "set", "date=2011/01/02", "2010/07/04/2412")
      File(archive, "2010/07/04").exists shouldBe false
      File(archive, "2010/07/album.toml").isFile shouldBe true
      File(archive, "2010").isDirectory shouldBe true

  it should "fail closed when a claiming dated album does not contain the new day" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      picture(File(archive, "2011/02/03"), "IMG_9.JPG", jpegBytes)
      write(archive, "2011/02/02/notes.txt", "stray\n")
      write(archive, "2010/07/album.toml", "include = [\n  'all',\n]\n")
      write(archive, "2010/07/Cape Cod.toml", "include = [\n  'all',\n]\n")
      write(archive, "Cape Cod.toml", "include = [\n  '2010/07/04 2412',\n]\n")
      val before: Map[String, (Seq[Byte], Long)] = snapshot(archive)
      val message: String = "2010/07/04/2412 is claimed by 2010/07, 2010/07/Cape Cod, which does not contain 2010-08-01"
      usage(archive, "item", "set", "date=2010/08/01", "2010/07/04/2412") shouldBe message
      snapshot(archive) shouldBe before
      usage(archive, "item", "set", "--dry-run", "date=2010/08/01", "2010/07/04/2412") shouldBe message
      snapshot(archive) shouldBe before
      File(archive, "2010/08").exists shouldBe false

  it should "rewrite a stem-specific include inside the span" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      val file: File = write(archive, "2010/07/album.toml", "include = [\n  '2010/07/04 2412',\n]\n")
      ok(archive, "item", "set", "date=2010/07/05", "2010/07/04/2412")
      val album: Album = Album.read(archive, file)
      album.claims(LocalDate.of(2010, 7, 5), "2412") shouldBe true
      album.claims(LocalDate.of(2010, 7, 4), "2412") shouldBe false
      Files.read(file) should include ("2010/07/05 2412")

  it should "keep day lines for a month of five stems" in :
    withOriginals: archive =>
      val day: File = File(archive, "2010/07/04")
      val bytes: Array[Byte] = jpegBytes
      Seq("2410", "2411", "2412", "2413", "2414").foreach(stem => picture(day, s"$stem.jpg", bytes))
      val file: File = write(archive, "2010/07/album.toml", "include = [\n  '2010/07/04 2410..2414',\n]\n")
      ok(archive, "item", "set", "date=2010/07/05", "2010/07/04/2412")
      val text: String = Files.read(file)
      text should not include ("all")
      val album: Album = Album.read(archive, file)
      album.claims(LocalDate.of(2010, 7, 5), "2412") shouldBe true
      album.claims(LocalDate.of(2010, 7, 4), "2412") shouldBe false
      album.claims(LocalDate.of(2010, 7, 4), "2410") shouldBe true
      File(archive, "2010/07/05/2412.jpg").isFile shouldBe true
      File(archive, "2010/07/04/2410.jpg").isFile shouldBe true

  it should "leave an all-only file unchanged" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      val file: File = write(archive, "2010/07/album.toml", "include = [\n  'all',\n]\n")
      val before: String = Files.read(file)
      ok(archive, "item", "set", "date=2010/07/05", "2010/07/04/2412")
      Files.read(file) shouldBe before
      File(archive, "2010/07/05/2412.jpg").isFile shouldBe true
      Album.read(archive, file).claims(LocalDate.of(2010, 7, 5), "2412") shouldBe true

  it should "keep an all line byte-for-byte beside a rewritten stem" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      val file: File = write(
        archive,
        "2010/07/album.toml",
        "# keep\ninclude = [\n  'all',\n  '2010/07/04 2412',\n]\n"
      )
      ok(archive, "item", "set", "date=2010/07/05", "2010/07/04/2412")
      val text: String = Files.read(file)
      text should startWith ("# keep\n")
      text should include ("  'all',\n")
      text should not include ("2010/07/04 2412")
      text should include ("2010/07/05 2412")

  it should "keep a whole-day line byte-for-byte beside a rewritten stem" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      val file: File = write(
        archive,
        "2010/07/album.toml",
        "include = [\n  '2010/07/04',\n  '2010/07/04 2412',\n]\n"
      )
      ok(archive, "item", "set", "date=2010/07/05", "2010/07/04/2412")
      val text: String = Files.read(file)
      text should include ("  '2010/07/04',\n")
      text should not include ("2010/07/04 2412")
      text should include ("2010/07/05 2412")

  it should "rewrite a curated include" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      val file: File = write(archive, "Best.toml", "# keep\ninclude = [\n  '2010/07/04 2412',\n]\n")
      ok(archive, "item", "set", "date=2010/08/01", "2010/07/04/2412")
      val text: String = Files.read(file)
      text should startWith ("# keep\n")
      text should include ("2010/08/01 2412")
      text should not include ("2010/07/04 2412")
      Album.read(archive, file).claims(LocalDate.of(2010, 8, 1), "2412") shouldBe true

  "item set" should "leave the files where they are for every key but a new date" in :
    withOriginals: archive =>
      val jpg: File = picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      val (sameText, sameValue) = ok(archive, "item", "set", "date=2010-07-04", "2010/07/04/2412")
      sameText should not include ("moving ")
      jpg.isFile shouldBe true
      sameValue shouldBe setValue(false, "2010/07/04/2412", "date", Json.Str("2010-07-04"), Json.Null)
      val (cleared, clearValue) = ok(archive, "item", "set", "date=", "2010/07/04/2412")
      cleared should include ("deleting metadata file")
      cleared should not include ("moving ")
      jpg.isFile shouldBe true
      File(archive, "2010/07/04/2412.toml").exists shouldBe false
      clearValue shouldBe setValue(false, "2010/07/04/2412", "date", Json.Null, Json.Null)
      val (sourceText, sourceValue) = ok(archive, "item", "set", "source=scan", "2010/07/04/2412")
      jpg.isFile shouldBe true
      File(archive, "2010/08").exists shouldBe false
      sourceText should include ("writing metadata file")
      sourceText should not include ("moving ")
      Files.read(File(archive, "2010/07/04/2412.toml")) shouldBe Metadata.encode(Metadata(source = Some("scan")))
      sourceValue shouldBe setValue(false, "2010/07/04/2412", "source", Json.Str("scan"), Json.Null)
      val (stampText, _) = ok(archive, "item", "set", "timestamp=2011-06-29T09:45:00", "2010/07/04/2412")
      stampText should not include ("moving ")
      jpg.isFile shouldBe true
      Metadata.read(File(archive, "2010/07/04/2412.toml")).timestamp shouldBe Some("2011-06-29T09:45:00")

  it should "write nothing on --dry-run" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      val before: Map[String, (Seq[Byte], Long)] = snapshot(archive)
      val (drySource, sourceValue) = ok(archive, "item", "set", "--dry-run", "source=scan", "2010/07/04/2412")
      drySource should include ("writing metadata file")
      drySource should include ("source = \"scan\"")
      snapshot(archive) shouldBe before
      sourceValue shouldBe setValue(true, "2010/07/04/2412", "source", Json.Str("scan"), Json.Null)
      Files.write(
        Metadata.encode(Metadata(date = Some(LocalDate.parse("2010-07-04")))),
        File(archive, "2010/07/04/2412.toml")
      )
      val withSidecar: Map[String, (Seq[Byte], Long)] = snapshot(archive)
      val (dryClear, clearValue) = ok(archive, "item", "set", "--dry-run", "date=", "2010/07/04/2412")
      dryClear should include ("deleting metadata file")
      snapshot(archive) shouldBe withSidecar
      clearValue shouldBe setValue(true, "2010/07/04/2412", "date", Json.Null, Json.Null)
      val (drySame, sameValue) = ok(archive, "item", "set", "--dry-run", "date=2010-07-04", "2010/07/04/2412")
      drySame should include ("writing metadata file")
      drySame should not include ("moving ")
      snapshot(archive) shouldBe withSidecar
      sameValue shouldBe setValue(true, "2010/07/04/2412", "date", Json.Str("2010-07-04"), Json.Null)
      val (dryMove, moveValue) = ok(archive, "item", "set", "--dry-run", "date=2010/08/01", "2010/07/04/2412")
      dryMove.linesIterator.next() shouldBe "moving /2010/07/04/2412 to /2010/08/01"
      snapshot(archive) shouldBe withSidecar
      File(archive, "2010/08").exists shouldBe false
      moveValue shouldBe setValue(true, "2010/07/04/2412", "date", Json.Str("2010-08-01"), Json.Null)

  it should "return the success object on the serve envelope" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      val session: Session = sessionFor(archive)
      val (_, line) = Cli.serveResponse(
        session,
        Serve.renderRequest(Some("i1"), List("item", "set", "bad-raw=true", "2010/07/04/2412"))
      )
      Serve.readValued(line) match
        case Right(called) =>
          called.response.ok shouldBe true
          called.response.id shouldBe Some("i1")
          called.value shouldBe setValue(false, "2010/07/04/2412", "bad-raw", Json.Bool(true), Json.Null)
        case Left(problem) =>
          fail(problem.message)

  it should "reject a bad date, an unknown property, a taken destination, and a non-date path" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      Cli.parse(List("item", "set", "date=2010/13/01", "2010/07/04/2412")) match
        case Cli.Run(_) => ()
        case other => fail(other.toString)
      usage(archive, "item", "set", "date=2010/13/01", "2010/07/04/2412") shouldBe "invalid date '2010/13/01'"
      usage(archive, "item", "set", "date=2010/7/4", "2010/07/04/2412") shouldBe "invalid date '2010/7/4'"
      usage(archive, "item", "set", "nope", "2010/07/04/2412") shouldBe "unknown property 'nope'"
      val notPicture: String = usage(archive, "item", "set", "nope", "2010")
      notPicture should not be "unknown property 'nope'"
      notPicture should include ("not a picture")
      picture(File(archive, "2010/08/01"), "2412.jpg", jpegBytes)
      usage(archive, "item", "set", "date=2010/08/01", "2010/07/04/2412") shouldBe "destination exists: 2010/08/01/2412"
      File(archive, "2010/07/04/2412.jpg").isFile shouldBe true
      val upload: File = File(archive, ".upload")
      picture(upload, "2412.jpg", jpegBytes)
      usage(archive, "item", "set", "date=2010/08/01", ".upload/2412") shouldBe "not a date-tree path"
      upload.isDirectory shouldBe true
      File(upload, "2412.jpg").isFile shouldBe true

  "item show" should "print the sidecar, the civil clock, and the claiming titles" in :
    withOriginals: archive =>
      val bytes: Array[Byte] = jpegBytes
      val day: File = File(archive, "2010/07/04")
      picture(day, "2412.jpg", bytes)
      picture(day, "2412.cr2", bytes)
      Files.write(
        Metadata.encode(Metadata(date = Some(LocalDate.parse("2010-07-04")), source = Some("scan"))),
        File(archive, "2010/07/04/2412.toml")
      )
      write(archive, "2010/07/album.toml", "include = [\n  'all',\n]\n")
      write(archive, "Best.toml", "include = [\n  '2010/07/04 2412',\n]\n")
      val (text, value) = ok(archive, "item", "show", "2010/07/04/2412")
      val lines: Seq[String] = text.linesIterator.toSeq
      lines.head shouldBe "/2010/07/04/2412"
      lines should contain ("extensions: cr2+jpg")
      lines should contain ("sidecar: source=scan, date=2010-07-04")
      lines should contain ("folder-day: 2010-07-04")
      lines.exists(_.startsWith("assessed: 2010-07-04")) shouldBe true
      lines.exists(_.contains("source=SidecarDate")) shouldBe true
      lines.exists(_.contains("SidecarTimestamp")) shouldBe false
      lines should contain ("albums: 2010/07,Best")
      value shouldBe Json.Obj(Seq(
        "path" -> Json.Str("2010/07/04/2412"),
        "extensions" -> Json.Arr(Seq(Json.Str("cr2"), Json.Str("jpg"))),
        "sidecar" -> Json.Obj(Seq(
          "source" -> Json.Str("scan"),
          "date" -> Json.Str("2010-07-04")
        )),
        "folderDay" -> Json.Str("2010-07-04"),
        "assessed" -> lines.find(_.startsWith("assessed: ")).get.stripPrefix("assessed: ").takeWhile(_ != ' ').pipe(Json.Str(_)),
        "albums" -> Json.Arr(Seq(Json.Str("2010/07"), Json.Str("Best")))
      ))

  it should "use SidecarTimestamp when timestamp and date agree" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      Files.write(
        Metadata.encode(Metadata(
          timestamp = Some("2010-07-04T15:30:00"),
          date = Some(LocalDate.parse("2010-07-04"))
        )),
        File(archive, "2010/07/04/2412.toml")
      )
      val (text, _) = ok(archive, "item", "show", "2010/07/04/2412")
      text should include ("source=SidecarTimestamp")
      text should not include ("SidecarDate")
      text should include ("2010-07-04")

  it should "print a dash when the picture has no sidecar and no assessed time" in :
    withOriginals: archive =>
      picture(File(archive, "2010/07/04"), "2412.jpg", jpegBytes)
      val (text, value) = ok(archive, "item", "show", "2010/07/04/2412")
      text should include ("sidecar: -")
      text should include ("assessed: -")
      text should include ("albums: -")
      value match
        case Json.Obj(fields) =>
          fields should contain ("sidecar" -> Json.Obj(Seq.empty))
          fields should contain ("assessed" -> Json.Null)
          fields should contain ("albums" -> Json.Arr(Seq.empty))
          fields should contain ("folderDay" -> Json.Str("2010-07-04"))
        case other =>
          fail(other.toString)

  it should "show an upload picture without a folder day" in :
    withOriginals: archive =>
      picture(File(archive, ".upload"), "2412.jpg", jpegBytes)
      val (text, value) = ok(archive, "item", "show", ".upload/2412")
      text.linesIterator.next() shouldBe "/.upload/2412"
      text should include ("folder-day: -")
      text should include ("albums: -")
      value match
        case Json.Obj(fields) =>
          fields should contain ("path" -> Json.Str(".upload/2412"))
          fields should contain ("folderDay" -> Json.Null)
          fields should contain ("albums" -> Json.Arr(Seq.empty))
        case other =>
          fail(other.toString)

  private def setValue(dryRun: Boolean, path: String, property: String, stored: Json, movedTo: Json): Json =
    Json.Obj(Seq(
      "dryRun" -> Json.Bool(dryRun),
      "path" -> Json.Str(path),
      "property" -> Json.Str(property),
      "value" -> stored,
      "movedTo" -> movedTo
    ))

  private def ok(archive: File, args: String*): (String, Json) =
    run(archive, args*) match
      case Result.Ok(text, value) => (text, value)
      case Result.Failed(text, _, message) => fail(s"$message\n$text")
      case Result.Usage(message) => fail(message)

  private def usage(archive: File, args: String*): String =
    run(archive, args*) match
      case Result.Usage(message) => message
      case other => fail(other.toString)

  private def run(archive: File, args: String*): Result =
    Cli.parse(args.toList) match
      case Cli.Run(parsed) => Cli.toOperation(parsed).execute(sessionFor(archive))._2
      case other => fail(other.toString)

  private def sessionFor(archive: File): Session =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    Session(Root(Configuration(rootPath, zone)))

  private def jpegBytes: Array[Byte] =
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", out)
    out.toByteArray

  private def picture(directory: File, name: String, bytes: Array[Byte]): File =
    directory.mkdirs()
    val file: File = File(directory, name)
    JFiles.write(file.toPath, bytes)
    file

  private def write(archive: File, path: String, content: String): File =
    val file: File = File(archive, path)
    file.getParentFile.mkdirs()
    Files.write(content, file)
    file

  private def snapshot(root: File): Map[String, (Seq[Byte], Long)] =
    def walk(file: File): Seq[(String, (Seq[Byte], Long))] =
      if file.isDirectory then Option(file.listFiles).toSeq.flatten.flatMap(walk)
      else
        val rel: String = root.toPath.relativize(file.toPath).toString
        Seq(rel -> (JFiles.readAllBytes(file.toPath).toSeq, file.lastModified))
    Option(root.listFiles).toSeq.flatten.flatMap(walk).toMap

  private def withOriginals(body: File => Unit): Unit =
    val originals: File = JFiles.createTempDirectory("item-cmd").toFile
    val runtime: File = JFiles.createTempDirectory("item-lock").toFile
    try Album.withWriteLockDirectory(runtime)(body(originals))
    finally
      deleteTree(originals)
      deleteTree(runtime)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()

  extension (text: String)
    private def pipe[A](f: String => A): A = f(text)
