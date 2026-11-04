package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.io.File
import java.nio.file.Files as JFiles
import java.time.ZoneId

class AlbumCommandTest extends AnyFlatSpec, Matchers:
  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")
  private val allFile: String = "include = [\n  'all',\n]\n"

  "album new" should "write all for a span and skip the later days" in :
    withOriginals: archive =>
      val (text, value) = ok(archive, "album", "new", "--span", "2010/07/04-18")
      text shouldBe
        """create 2010/07/04-18
          |2010/07/04/to-18.toml
          |""".stripMargin
      value shouldBe json("dryRun" -> Json.Bool(false), "title" -> Json.Str("2010/07/04-18"), "path" -> Json.Str("2010/07/04/to-18.toml"))
      Files.read(File(archive, "2010/07/04/to-18.toml")) shouldBe allFile
      File(archive, "2010/07/05").exists shouldBe false
      File(archive, "2010/07/18").exists shouldBe false
      val (dryText, dryValue) = ok(archive, "album", "new", "--dry-run", "--span", "2010/08")
      dryText shouldBe
        """create 2010/08
          |2010/08/album.toml
          |""".stripMargin
      dryValue shouldBe json("dryRun" -> Json.Bool(true), "title" -> Json.Str("2010/08"), "path" -> Json.Str("2010/08/album.toml"))
      File(archive, "2010/08").exists shouldBe false

  it should "write an empty curated file" in :
    withOriginals: archive =>
      val (text, value) = ok(archive, "album", "new", "--name", "Best")
      text shouldBe
        """create Best
          |Best.toml
          |""".stripMargin
      value shouldBe json("dryRun" -> Json.Bool(false), "title" -> Json.Str("Best"), "path" -> Json.Str("Best.toml"))
      val file: File = File(archive, "Best.toml")
      Files.read(file) shouldBe ""
      file.length shouldBe 0
      val (shown, showValue) = ok(archive, "album", "show", "Best")
      shown shouldBe
        """path: Best.toml
          |title: Best
          |span: -
          |name: Best
          |external: false
          |google-album-id: -
          |published-title: -
          |google-title: -
          |owner: -
          |include:
          |""".stripMargin
      shown should not include ("all")
      showValue shouldBe showJson(
        path = "Best.toml",
        title = "Best",
        span = Json.Null,
        name = Json.Str("Best"),
        external = false,
        include = Seq.empty
      )

  it should "write both a span and a name, and show an unnamed month" in :
    withOriginals: archive =>
      ok(archive, "album", "new", "--span", "2010/07", "--name", "Cape Cod")
      Files.read(File(archive, "2010/07/Cape Cod.toml")) shouldBe allFile
      val (monthText, monthValue) = ok(archive, "album", "new", "--span", "2010/08")
      monthText shouldBe
        """create 2010/08
          |2010/08/album.toml
          |""".stripMargin
      monthValue shouldBe json("dryRun" -> Json.Bool(false), "title" -> Json.Str("2010/08"), "path" -> Json.Str("2010/08/album.toml"))
      val (shown, showValue) = ok(archive, "album", "show", "2010/08")
      shown shouldBe
        """path: 2010/08/album.toml
          |title: 2010/08
          |span: 2010/08
          |name: -
          |external: false
          |google-album-id: -
          |published-title: -
          |google-title: -
          |owner: -
          |include:
          |  all
          |""".stripMargin
      showValue shouldBe showJson(
        path = "2010/08/album.toml",
        title = "2010/08",
        span = Json.Str("2010/08"),
        name = Json.Null,
        external = false,
        include = Seq("all")
      )
      ok(archive, "album", "new", "--span", "2010/07/19")
      val (canonical, _) = ok(archive, "album", "show", "2010/07/19-19")
      canonical should include ("title: 2010/07/19\n")
      canonical should include ("path: 2010/07/19/album.toml\n")

  it should "leave an existing file unchanged" in :
    withOriginals: archive =>
      val external: String = "external = true\n"
      val file: File = write(archive, "2010/07/album.toml", external)
      usage(archive, "album", "new", "--span", "2010/07") shouldBe "album already exists: 2010/07"
      Files.read(file) shouldBe external
      val day: File = write(archive, "2010/07/19/album.toml", allFile)
      usage(archive, "album", "new", "--span", "2010/07/19-19") shouldBe "album already exists: 2010/07/19"
      Files.read(day) shouldBe allFile
      usage(archive, "album", "new", "--name", "album") shouldBe "not an album name: album"
      usage(archive, "album", "new", "--span", "Best") shouldBe "not a span: Best"
      usage(archive, "album", "new", "--span", "2010/07/Cape Cod") shouldBe "not a span: 2010/07/Cape Cod"

  "album show" should "print a curated google title and owner" in :
    withOriginals: archive =>
      write(
        archive,
        "Best.toml",
        "external = true\ngoogle-title = \"Best Of\"\nowner = \"Ada\"\ninclude = [\n  '2009/03/10 2412',\n]\n"
      )
      val (text, value) = ok(archive, "album", "show", "Best")
      text shouldBe
        """path: Best.toml
          |title: Best
          |span: -
          |name: Best
          |external: true
          |google-album-id: -
          |published-title: -
          |google-title: Best Of
          |owner: Ada
          |include:
          |  2009/03/10 2412
          |""".stripMargin
      value shouldBe Json.Obj(Seq(
        "path" -> Json.Str("Best.toml"),
        "title" -> Json.Str("Best"),
        "span" -> Json.Null,
        "name" -> Json.Str("Best"),
        "external" -> Json.Bool(true),
        "googleAlbumId" -> Json.Str(""),
        "publishedTitle" -> Json.Str(""),
        "googleTitle" -> Json.Str("Best Of"),
        "owner" -> Json.Str("Ada"),
        "include" -> Json.Arr(Seq(Json.Str("2009/03/10 2412")))
      ))

  "album ls" should "list matching titles and no paths" in :
    withOriginals: archive =>
      write(archive, "2010/07/album.toml", allFile)
      write(archive, "Best.toml", "")
      val (text, value) = ok(archive, "album", "ls")
      text shouldBe "2010/07\nBest\n"
      text should not include (".toml")
      text should not include ("google")
      value shouldBe json("albums" -> Json.Arr(Seq(Json.Str("2010/07"), Json.Str("Best"))))
      val (month, monthValue) = ok(archive, "album", "ls", "2010")
      month shouldBe "2010/07\n"
      monthValue shouldBe json("albums" -> Json.Arr(Seq(Json.Str("2010/07"))))

  "album rm" should "refuse an external album and leave the file" in :
    withOriginals: archive =>
      val body: String = "external = true\n"
      val file: File = write(archive, "2010/07/album.toml", body)
      usage(archive, "album", "rm", "2010/07") shouldBe "refusing external album 2010/07"
      Files.read(file) shouldBe body
      file.isFile shouldBe true

  it should "delete a local album, leave the picture, and prune the empty day" in :
    withOriginals: archive =>
      val album: File = write(archive, "2010/07/04/album.toml", allFile)
      val picture: File = write(archive, "2010/07/05/2412.jpg", "")
      val (dryText, dryValue) = ok(archive, "album", "rm", "--dry-run", "2010/07/04")
      val expected: String =
        """delete 2010/07/04
          |2010/07/04/album.toml
          |rmdir 2010/07/04
          |""".stripMargin
      dryText shouldBe expected
      dryValue shouldBe removed(true, "2010/07/04", "2010/07/04")
      album.isFile shouldBe true
      File(archive, "2010/07/04").isDirectory shouldBe true
      val (text, value) = ok(archive, "album", "rm", "2010/07/04")
      text shouldBe expected
      value shouldBe removed(false, "2010/07/04", "2010/07/04")
      album.exists shouldBe false
      File(archive, "2010/07/04").exists shouldBe false
      picture.isFile shouldBe true
      File(archive, "2010/07").isDirectory shouldBe true
      archive.isDirectory shouldBe true
      usage(archive, "album", "rm", "Missing") shouldBe "no such album: Missing"
      usage(archive, "album", "rm", "2010/07-07") shouldBe "not an album title: 2010/07-07"

  it should "not remove the archive root" in :
    withOriginals: archive =>
      write(archive, "Best.toml", "")
      val (text, value) = ok(archive, "album", "rm", "Best")
      text shouldBe
        """delete Best
          |Best.toml
          |""".stripMargin
      text should not include ("rmdir")
      value shouldBe removed(false, "Best")
      archive.isDirectory shouldBe true
      File(archive, "Best.toml").exists shouldBe false

  "album mv" should "refuse an external album and an include line outside the new span" in :
    withOriginals: archive =>
      val external: String = "external = true\n"
      val externalFile: File = write(archive, "Hawaii.toml", external)
      usage(archive, "album", "mv", "Hawaii", "Somewhere") shouldBe "refusing external album Hawaii"
      Files.read(externalFile) shouldBe external
      File(archive, "Somewhere.toml").exists shouldBe false
      val outside: String = "include = [\n  '2010/07/04 2412',\n]\n"
      val source: File = write(archive, "2010/07/album.toml", outside)
      usage(archive, "album", "mv", "2010/07", "2010/08") shouldBe "include is outside 2010/08: 2010/07/04 2412"
      Files.read(source) shouldBe outside
      File(archive, "2010/08").exists shouldBe false
      val all: File = write(archive, "2010/06/album.toml", allFile)
      usage(archive, "album", "mv", "2010/06", "Best") shouldBe "include is outside Best: all"
      Files.read(all) shouldBe allFile
      File(archive, "Best.toml").exists shouldBe false

  it should "move the bytes and keep a comment and google-title" in :
    withOriginals: archive =>
      val body: String = "# keep\ngoogle-title = \"Kept\"\ninclude = [\n  'all',\n]\n"
      val source: File = write(archive, "2010/07/Cape Cod.toml", body)
      val (dryText, dryValue) = ok(archive, "album", "mv", "--dry-run", "2010/07/Cape Cod", "2010/08/Cape Cod")
      val expected: String =
        """move 2010/07/Cape Cod 2010/08/Cape Cod
          |2010/07/Cape Cod.toml 2010/08/Cape Cod.toml
          |rmdir 2010/07
          |""".stripMargin
      dryText shouldBe expected
      dryValue shouldBe json(
        "dryRun" -> Json.Bool(true),
        "from" -> Json.Str("2010/07/Cape Cod"),
        "to" -> Json.Str("2010/08/Cape Cod"),
        "path" -> Json.Str("2010/08/Cape Cod.toml")
      )
      source.isFile shouldBe true
      File(archive, "2010/08").exists shouldBe false
      val (text, value) = ok(archive, "album", "mv", "2010/07/Cape Cod", "2010/08/Cape Cod")
      text shouldBe expected
      value shouldBe json(
        "dryRun" -> Json.Bool(false),
        "from" -> Json.Str("2010/07/Cape Cod"),
        "to" -> Json.Str("2010/08/Cape Cod"),
        "path" -> Json.Str("2010/08/Cape Cod.toml")
      )
      source.exists shouldBe false
      File(archive, "2010/07").exists shouldBe false
      File(archive, "2010").isDirectory shouldBe true
      Files.read(File(archive, "2010/08/Cape Cod.toml")) shouldBe body
      archive.isDirectory shouldBe true

  "album include" should "recompress one album and leave the other" in :
    withOriginals: archive =>
      write(archive, "2010/07/04/2412.jpg", "")
      val month: File = write(archive, "2010/07/album.toml", "include = []\n")
      val otherBody: String = "google-title = \"Stay\"\n"
      val other: File = write(archive, "Best.toml", otherBody)
      val (included, includeValue) = ok(archive, "album", "include", "2010/07", "2010/07/04/2412")
      included shouldBe "include 2010/07 2010/07/04/2412\n"
      includeValue shouldBe editValue(false, "2010/07", Seq("all"), Seq("2010/07/04/2412"))
      Album.read(archive, month).include.map(Include.logical) shouldBe Seq("all")
      Files.read(other) shouldBe otherBody
      val (excluded, excludeValue) = ok(archive, "album", "exclude", "2010/07", "2010/07/04/2412")
      excluded shouldBe "exclude 2010/07 2010/07/04/2412\n"
      excludeValue shouldBe editValue(false, "2010/07", Seq.empty, Seq("2010/07/04/2412"))
      Files.read(month) shouldBe "include = []\n"
      Files.read(other) shouldBe otherBody
      File(archive, "2010/07/04/2412.jpg").isFile shouldBe true

  it should "refuse a picture outside the span and write nothing" in :
    withOriginals: archive =>
      write(archive, "2010/08/01/2412.jpg", "")
      val body: String = allFile
      val file: File = write(archive, "2010/07/album.toml", body)
      usage(archive, "album", "include", "2010/07", "2010/08/01/2412") shouldBe "2010/08/01/2412 is outside 2010/07"
      Files.read(file) shouldBe body

  it should "succeed with an empty transcript when the claim does not change" in :
    withOriginals: archive =>
      write(archive, "2010/07/04/1000.jpg", "")
      write(archive, "2010/07/04/2412.jpg", "")
      val body: String = "include = [\n  '2010/07/04 1000',\n]\n"
      val file: File = write(archive, "2010/07/album.toml", body)
      val (included, includeValue) = ok(archive, "album", "include", "2010/07", "2010/07/04/1000")
      included shouldBe ""
      includeValue shouldBe editValue(false, "2010/07", Seq("2010/07/04 1000"), Seq.empty)
      Files.read(file) shouldBe body
      val (excluded, excludeValue) = ok(archive, "album", "exclude", "2010/07", "2010/07/04/2412")
      excluded shouldBe ""
      excludeValue shouldBe editValue(false, "2010/07", Seq("2010/07/04 1000"), Seq.empty)
      Files.read(file) shouldBe body
      usage(archive, "album", "include", "2010/07", "2010/07/04/2412", "2010/07/04/2412") shouldBe
        "repeated picture 2010/07/04/2412"
      Files.read(file) shouldBe body

  it should "edit a curated album without a day check" in :
    withOriginals: archive =>
      write(archive, "2010/08/01/2412.jpg", "")
      val file: File = write(archive, "Best.toml", "")
      val (text, value) = ok(archive, "album", "include", "Best", "2010/08/01/2412")
      text shouldBe "include Best 2010/08/01/2412\n"
      value shouldBe editValue(false, "Best", Seq("2010/08/01 2412"), Seq("2010/08/01/2412"))
      Album.read(archive, file).include.map(Include.logical) shouldBe Seq("2010/08/01 2412")

  "album claims" should "match today's list text and the claims object" in :
    withOriginals: archive =>
      write(archive, "2009/03/10/2412.jpg", "")
      write(archive, "2009/03/11/1000.jpg", "")
      write(
        archive,
        "Best.toml",
        "google-title = \"Best Of\"\ninclude = [\n  '2009/03/10 2412',\n]\n"
      )
      write(archive, "2009/03/11/album.toml", "include = [\n  'all except 1000',\n]\n")
      val text: String =
        """album "Best" google="Best Of"
          |2009/03/10/2412 albums=Best
          |2009/03/11/1000 albums=-
          |""".stripMargin
      val (listed, value) = ok(archive, "album", "claims")
      listed shouldBe text
      listed shouldBe listText(archive, None)
      ok(archive, "album", "claims", "2009/03/10")._1 shouldBe listText(archive, Some("2009/03/10"))
      value shouldBe Json.Obj(Seq(
        "annotations" -> Json.Arr(Seq(Json.Obj(Seq(
          "title" -> Json.Str("Best"),
          "googleTitle" -> Json.Str("Best Of"),
          "owner" -> Json.Null
        )))),
        "pictures" -> Json.Arr(Seq(
          Json.Obj(Seq(
            "path" -> Json.Str("2009/03/10/2412"),
            "albums" -> Json.Arr(Seq(Json.Str("Best"))),
            "excludedOnly" -> Json.Bool(false)
          )),
          Json.Obj(Seq(
            "path" -> Json.Str("2009/03/11/1000"),
            "albums" -> Json.Arr(Seq.empty),
            "excludedOnly" -> Json.Bool(true)
          ))
        ))
      ))

  private def showJson(
    path: String,
    title: String,
    span: Json,
    name: Json,
    external: Boolean,
    include: Seq[String]
  ): Json = Json.Obj(Seq(
    "path" -> Json.Str(path),
    "title" -> Json.Str(title),
    "span" -> span,
    "name" -> name,
    "external" -> Json.Bool(external),
    "googleAlbumId" -> Json.Str(""),
    "publishedTitle" -> Json.Str(""),
    "googleTitle" -> Json.Str(""),
    "owner" -> Json.Null,
    "include" -> Json.Arr(include.map(Json.Str(_)))
  ))

  private def editValue(dryRun: Boolean, title: String, include: Seq[String], pictures: Seq[String]): Json =
    json(
      "dryRun" -> Json.Bool(dryRun),
      "title" -> Json.Str(title),
      "include" -> Json.Arr(include.map(Json.Str(_))),
      "pictures" -> Json.Arr(pictures.map(Json.Str(_)))
    )

  private def removed(dryRun: Boolean, title: String, dirs: String*): Json =
    json(
      "dryRun" -> Json.Bool(dryRun),
      "title" -> Json.Str(title),
      "removed" -> Json.Arr(dirs.map(Json.Str(_)))
    )

  private def json(fields: (String, Json)*): Json = Json.Obj(fields)

  private def listText(archive: File, path: Option[String]): String =
    ListAlbums(path).execute(sessionFor(archive))._2 match
      case Result.Ok(text, _) => text
      case other => fail(other.toString)

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

  private def write(archive: File, path: String, content: String): File =
    val file: File = File(archive, path)
    file.getParentFile.mkdirs()
    Files.write(content, file)
    file

  private def withOriginals(body: File => Unit): Unit =
    val originals: File = JFiles.createTempDirectory("album-cmd").toFile
    val runtime: File = JFiles.createTempDirectory("album-lock").toFile
    try Album.withWriteLockDirectory(runtime)(body(originals))
    finally
      deleteTree(originals)
      deleteTree(runtime)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
