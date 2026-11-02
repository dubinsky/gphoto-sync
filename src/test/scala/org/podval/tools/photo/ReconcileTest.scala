package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.io.{ByteArrayOutputStream, File}
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, StandardOpenOption}
import java.time.ZoneId

class ReconcileTest extends AnyFlatSpec, Matchers:
  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  @targetName("outcomeCanEqual")
  private given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")

  "reconcile flags" should "parse dry-run and delete, and reject a repeat" in :
    Cli.parse(List("reconcile")) shouldBe Cli.Run(Cli.ReconcileArgs(dryRun = false, delete = false))
    Cli.parse(List("reconcile", "--dry-run", "--delete")) shouldBe Cli.Run(Cli.ReconcileArgs(dryRun = true, delete = true))
    Cli.parse(List("reconcile", "--dry-run", "--dry-run")) shouldBe Cli.Usage("repeated option '--dry-run'")
    Cli.parse(List("reconcile", "--delete", "--delete")) shouldBe Cli.Usage("repeated option '--delete'")
    Cli.parse(List("reconcile", "--help")) match
      case Cli.Help(text) =>
        text should include ("--dry-run")
        text should include ("print the classes and write nothing")
        text should include ("--delete")
        text should include ("remove a toml whose link id is gone")
      case other =>
        fail(other.toString)

  "photo reconcile" should "link an external file by its working title and not write a second file" in :
    withArchive: archive =>
      val before: String = "external = true\n"
      writeToml(archive, "Picnic.toml", before)
      writeCatalog(archive, "album\tcid-work\t0\tPicnic")
      val session: Session = sessionFor(archive)
      val (back, result) = exec(session, "reconcile")
      val text: String = ok(result)
      text shouldBe "wrote .gphoto-sync/album-links.tsv\n"
      quiet(text, "cid-work")
      valueHas(result, "cid-work")
      readToml(archive, "Picnic.toml") shouldBe before
      links(archive) shouldBe "# gphoto-album-links 1\ncid-work\tPicnic.toml\n"
      File(archive, "Picnic.toml").isFile shouldBe true
      tomls(archive) shouldBe Seq("Picnic.toml")
      requests(archive).isFile shouldBe false
      partials(archive) shouldBe Seq.empty[String]
      back.catalogCache.isDefined shouldBe true

  it should "link an external file by google-title when that key is present" in :
    withArchive: archive =>
      val before: String =
        """external = true
          |google-title = "Island (Ada)"
          |owner = "Ada"
          |""".stripMargin
      writeToml(archive, "Island.toml", before)
      writeCatalog(
        archive,
        "album\tcid-google\t0\tIsland (Ada)",
        "owner\tcid-google\tAda"
      )
      val text: String = ok(exec(sessionFor(archive), "reconcile")._2)
      text shouldBe "wrote .gphoto-sync/album-links.tsv\n"
      quiet(text, "cid-google")
      readToml(archive, "Island.toml") shouldBe before
      File(archive, "Island (Ada).toml").exists shouldBe false
      links(archive) shouldBe "# gphoto-album-links 1\ncid-google\tIsland.toml\n"

  it should "keep the path on a rename and set google-title" in :
    withArchive: archive =>
      val before: String =
        """# keep
          |external = true
          |google-title = "Hawaii"
          |owner = "Ada"
          |""".stripMargin
      writeToml(archive, "Hawaii.toml", before)
      writeLinks(archive, "cid-hawaii" -> "Hawaii.toml")
      writeCatalog(
        archive,
        "album\tcid-hawaii\t0\tHawaii Trip",
        "owner\tcid-hawaii\tAda"
      )
      val text: String = ok(exec(sessionFor(archive), "reconcile")._2)
      text shouldBe
        """update Hawaii Trip
          |wrote Hawaii.toml
          |""".stripMargin
      quiet(text, "cid-hawaii")
      readToml(archive, "Hawaii.toml") shouldBe
        """# keep
          |external = true
          |google-title = "Hawaii Trip"
          |owner = "Ada"
          |""".stripMargin
      File(archive, "Hawaii Trip.toml").exists shouldBe false
      requests(archive).isFile shouldBe false
      links(archive) shouldBe "# gphoto-album-links 1\ncid-hawaii\tHawaii.toml\n"
      partials(archive) shouldBe Seq.empty[String]

  it should "set published-title on a client album in the same splice" in :
    withArchive: archive =>
      writeToml(
        archive,
        "Trip.toml",
        """# keep
          |google-album-id = "library-1"
          |published-title = "Trip"
          |""".stripMargin
      )
      writeLinks(archive, "cid-client" -> "Trip.toml")
      writeCatalog(archive, "album\tcid-client\t0\tTrip Renamed")
      val result: Result = exec(sessionFor(archive), "reconcile")._2
      val text: String = ok(result)
      text shouldBe
        """update Trip Renamed
          |wrote Trip.toml
          |""".stripMargin
      quiet(text, "cid-client", "library-1")
      valueHas(result, "cid-client")
      readToml(archive, "Trip.toml") shouldBe
        """# keep
          |google-title = "Trip Renamed"
          |google-album-id = "library-1"
          |published-title = "Trip Renamed"
          |""".stripMargin

  it should "omit google-title when a client title equals the path title" in :
    withArchive: archive =>
      writeToml(
        archive,
        "Trip.toml",
        """google-album-id = "library-1"
          |google-title = "Trip"
          |published-title = "Away"
          |""".stripMargin
      )
      writeLinks(archive, "cid-client" -> "Trip.toml")
      writeCatalog(archive, "album\tcid-client\t0\tTrip")
      val text: String = ok(exec(sessionFor(archive), "reconcile")._2)
      text shouldBe
        """update Trip
          |wrote Trip.toml
          |""".stripMargin
      readToml(archive, "Trip.toml") shouldBe
        """google-album-id = "library-1"
          |published-title = "Trip"
          |""".stripMargin

  it should "skip an owner that checkGoogle would reject and still record the link" in :
    withArchive: archive =>
      val before: String =
        """google-album-id = "library-9"
          |published-title = "Shared"
          |""".stripMargin
      writeToml(archive, "Shared.toml", before)
      writeCatalog(
        archive,
        "album\tcid-owner\t0\tShared",
        "owner\tcid-owner\tAda"
      )
      val result: Result = exec(sessionFor(archive), "reconcile")._2
      val text: String = ok(result)
      text shouldBe
        """skipped Shared owner
          |wrote .gphoto-sync/album-links.tsv
          |""".stripMargin
      quiet(text, "cid-owner")
      valueHas(result, "cid-owner")
      readToml(archive, "Shared.toml") shouldBe before
      tomls(archive) shouldBe Seq("Shared.toml")
      links(archive) shouldBe "# gphoto-album-links 1\ncid-owner\tShared.toml\n"

  it should "leave a pending file and an unlinked external file in place" in :
    withArchive: archive =>
      writeToml(archive, "Pending.toml", "")
      val extra: String = "external = true\n"
      writeToml(archive, "Extra.toml", extra)
      writeCatalog(archive)
      val text: String = ok(exec(sessionFor(archive), "reconcile")._2)
      text shouldBe
        """pending Pending.toml
          |skipped Extra.toml unlinked
          |""".stripMargin
      readToml(archive, "Pending.toml") shouldBe ""
      readToml(archive, "Extra.toml") shouldBe extra
      links(archive) shouldBe ""
      File(archive, ".gphoto-sync/album-links.tsv").isFile shouldBe false

  it should "report a gone link and keep the toml until --delete" in :
    withArchive: archive =>
      writeToml(
        archive,
        "Old.toml",
        """external = true
          |google-title = "Old Title"
          |""".stripMargin
      )
      mkdir(archive, "2010/07/19")
      touch(File(archive, "2010/07/19"), "1234.jpg")
      writeLinks(archive, "cid-old" -> "Old.toml")
      writeCatalog(archive)
      val kept: String = ok(exec(sessionFor(archive), "reconcile")._2)
      kept shouldBe "gone Old Title\n"
      quiet(kept, "cid-old")
      File(archive, "Old.toml").isFile shouldBe true
      File(archive, "2010/07/19/1234.jpg").isFile shouldBe true
      links(archive) shouldBe "# gphoto-album-links 1\ncid-old\tOld.toml\n"
      val deleted: String = ok(exec(sessionFor(archive), "reconcile", "--delete")._2)
      deleted shouldBe
        """gone Old Title
          |wrote Old.toml
          |wrote .gphoto-sync/album-links.tsv
          |""".stripMargin
      File(archive, "Old.toml").exists shouldBe false
      File(archive, "2010/07/19/1234.jpg").isFile shouldBe true
      links(archive) shouldBe ""

  it should "delete only the gone toml" in :
    withArchive: archive =>
      writeToml(
        archive,
        "Old.toml",
        """external = true
          |google-title = "Old Title"
          |""".stripMargin
      )
      writeToml(archive, "Pending.toml", "")
      val extra: String = "external = true\n"
      writeToml(archive, "Extra.toml", extra)
      mkdir(archive, "2010/07/19")
      touch(File(archive, "2010/07/19"), "1234.jpg")
      writeLinks(archive, "cid-old" -> "Old.toml")
      writeCatalog(archive)
      val text: String = ok(exec(sessionFor(archive), "reconcile", "--delete")._2)
      text shouldBe
        """pending Pending.toml
          |skipped Extra.toml unlinked
          |gone Old Title
          |wrote Old.toml
          |wrote .gphoto-sync/album-links.tsv
          |""".stripMargin
      quiet(text, "cid-old")
      File(archive, "Old.toml").exists shouldBe false
      File(archive, "Pending.toml").isFile shouldBe true
      readToml(archive, "Extra.toml") shouldBe extra
      File(archive, "2010/07/19/1234.jpg").isFile shouldBe true

  it should "write nothing on --dry-run and then create the external file" in :
    withArchive: archive =>
      writeCatalog(archive, "album\tcid-notes\t0\tNotes")
      val cursor: Session = sessionFor(archive).copy(style = PathStyle.Cursor)
      val (drySession, dryResult) = exec(cursor, "reconcile", "--dry-run")
      val dry: String = ok(dryResult)
      dry shouldBe "create Notes\n"
      quiet(dry, "cid-notes")
      valueHas(dryResult, "cid-notes")
      dry should not include ("wrote")
      File(archive, "Notes.toml").exists shouldBe false
      File(archive, ".gphoto-sync/album-links.tsv").isFile shouldBe false
      drySession.index.isEmpty shouldBe false
      val (realSession, realResult) = exec(cursor, "reconcile")
      val real: String = ok(realResult)
      real shouldBe
        """create Notes
          |wrote Notes.toml
          |wrote .gphoto-sync/album-links.tsv
          |""".stripMargin
      quiet(real, "cid-notes")
      readToml(archive, "Notes.toml") shouldBe "external = true\n"
      readToml(archive, "Notes.toml") should not include ("google-album-id")
      readToml(archive, "Notes.toml") should not include ("include")
      links(archive) shouldBe "# gphoto-album-links 1\ncid-notes\tNotes.toml\n"
      realSession.index.isEmpty shouldBe true
      realSession.catalogCache.isDefined shouldBe true
      requests(archive).isFile shouldBe false
      partials(archive) shouldBe Seq.empty[String]

  it should "still write a file when nothing matches, and skip unread names and a digit leftover" in :
    withArchive: archive =>
      mkdir(archive, "2010/07/19")
      touch(File(archive, "2010/07/19"), "1000.jpg")
      writeCatalog(
        archive,
        "album\tcid-pic\t1\tPicnic",
        "item\tcid-pic\t1000.jpg\t2010-07-19\tkey-pic\t-",
        "album\tcid-unread\t1\tBroken",
        "error\tcid-unread\t1\t1\t1",
        "unset\tcid-unread\t2010-07-19\tkey-unset\t-",
        "album\tcid-digit\t1\t19 Kislev",
        "item\tcid-digit\tpic.jpg\t2010-07-04\tkey-digit\t-",
        "album\tcid-auto\t0\tTravel",
        "auto-add\tcid-auto"
      )
      val text: String = ok(exec(sessionFor(archive), "reconcile")._2)
      text should include ("create Picnic\n")
      text should include ("skipped Broken unread names\n")
      text should include ("skipped 19 Kislev digit\n")
      text should include ("skipped Travel auto-add\n")
      quiet(text, "cid-pic", "key-pic", "cid-unread", "key-unset", "cid-digit", "key-digit", "cid-auto")
      val picnic: File = File(archive, "2010/07/19/Picnic.toml")
      picnic.isFile shouldBe true
      val read: Album = Album.read(archive, picnic)
      read.external shouldBe true
      read.googleAlbumId shouldBe ""
      read.claims(java.time.LocalDate.of(2010, 7, 19), "1000") shouldBe true
      File(archive, "19 Kislev.toml").exists shouldBe false
      File(archive, "Broken.toml").exists shouldBe false

  it should "reject a blank or duplicate link row without printing the id" in :
    withArchive: archive =>
      writeCatalog(archive)
      writeRaw(archive, ".gphoto-sync/album-links.tsv", "# gphoto-album-links 1\n\ncid-secret\tPicnic.toml\n")
      val blank: String = usage(sessionFor(archive), "reconcile")
      blank shouldBe "blank line"
      quiet(blank, "cid-secret")
      writeRaw(
        archive,
        ".gphoto-sync/album-links.tsv",
        "# gphoto-album-links 1\ncid-secret\tPicnic.toml\ncid-secret\tOther.toml\n"
      )
      val duplicate: String = usage(sessionFor(archive), "reconcile")
      duplicate shouldBe "duplicate album link"
      quiet(duplicate, "cid-secret")
      writeRaw(archive, ".gphoto-sync/album-links.tsv", "# gphoto-album-links 1\ncid-secret\t../Picnic.toml\n")
      usage(sessionFor(archive), "reconcile") shouldBe "album link path"
      File(archive, "Picnic.toml").exists shouldBe false

  it should "run in this process while serve holds its lock" in :
    withArchive: archive =>
      writeCatalog(archive)
      val session: Session = sessionFor(archive)
      val runtime: File = JFiles.createTempDirectory("reconcile-serve").toFile
      val channel: FileChannel = FileChannel.open(
        File(runtime, "serve.lock").toPath,
        StandardOpenOption.CREATE,
        StandardOpenOption.READ,
        StandardOpenOption.WRITE
      )
      val lock = channel.tryLock()
      lock should not be null
      try
        val parsed: Cli.Parsed = Cli.parse(List("reconcile")) match
          case Cli.Run(value) => value
          case other => fail(other.toString)
        val result: Result = Cli.executeOneShot(session, List("reconcile"), parsed, Right(runtime), waitMs = 250)
        ok(result) shouldBe ""
        result match
          case Result.Usage(message) =>
            message should not include ("a command is in progress")
            fail(message)
          case _ =>
            ()
      finally
        lock.release()
        channel.close()
        deleteTree(runtime)

  it should "stay in the shell and be rejected on the socket" in :
    withArchive: archive =>
      writeCatalog(archive)
      val session: Session = sessionFor(archive)
      val sent: scala.collection.mutable.ListBuffer[List[String]] = scala.collection.mutable.ListBuffer.empty
      val (code, out, err) = shell(
        session,
        Seq("reconcile"),
        Some((_, argv) =>
          sent += argv
          Result.Ok("sent\n", Json.Null)
        )
      )
      code shouldBe 0
      sent.isEmpty shouldBe true
      out shouldBe ""
      err should not include ("Photo Archive Tool:")
      var ran: Boolean = false
      val (_, line) = Cli.serveResponse(
        session,
        """{"argv":["reconcile","--dry-run"]}""",
        (_, _) =>
          ran = true
          (session, Result.Ok("ran\n", Json.Null))
      )
      ran shouldBe false
      line should include ("reconcile runs in the calling process")

  it should "skip an extra title that places on a free path and still update another album" in :
    withArchive: archive =>
      val hawaii: String =
        """# keep
          |external = true
          |google-title = "Hawaii"
          |owner = "Ada"
          |""".stripMargin
      writeToml(archive, "Hawaii.toml", hawaii)
      writeLinks(archive, "cid-hawaii" -> "Hawaii.toml")
      writeCatalog(
        archive,
        "album\tcid-hawaii\t0\tHawaii Trip",
        "owner\tcid-hawaii\tAda",
        "album\tcid-notes\t0\tNotes",
        "album\tcid-gap\t0\tNotes "
      )
      val text: String = ok(exec(sessionFor(archive), "reconcile")._2)
      text shouldBe
        """update Hawaii Trip
          |create Notes
          |skipped Notes  ambiguous title
          |wrote Hawaii.toml
          |wrote Notes.toml
          |wrote .gphoto-sync/album-links.tsv
          |""".stripMargin
      quiet(text, "cid-hawaii", "cid-notes", "cid-gap")
      readToml(archive, "Notes.toml") shouldBe "external = true\n"
      File(archive, "Notes .toml").exists shouldBe false
      readToml(archive, "Hawaii.toml") shouldBe
        """# keep
          |external = true
          |google-title = "Hawaii Trip"
          |owner = "Ada"
          |""".stripMargin
      links(archive) shouldBe
        """# gphoto-album-links 1
          |cid-hawaii	Hawaii.toml
          |cid-notes	Notes.toml
          |""".stripMargin

  it should "not give an existing path to the first of two titles that place there" in :
    withArchive: archive =>
      val before: String = "external = true\n"
      writeToml(archive, "2010/07/19/Picnic.toml", before)
      writeCatalog(
        archive,
        "album\tcid-pic\t1\tPicnic",
        "item\tcid-pic\t1000.jpg\t2010-07-19\tkey-pic\t-",
        "album\tcid-gap\t1\tPicnic ",
        "item\tcid-gap\t1000.jpg\t2010-07-19\tkey-gap\t-",
        "album\tcid-trip\t0\tTrip"
      )
      val text: String = ok(exec(sessionFor(archive), "reconcile")._2)
      text shouldBe
        """skipped Picnic ambiguous title
          |skipped Picnic  ambiguous title
          |create Trip
          |skipped 2010/07/19/Picnic.toml unlinked
          |wrote Trip.toml
          |wrote .gphoto-sync/album-links.tsv
          |""".stripMargin
      quiet(text, "cid-pic", "cid-gap", "cid-trip", "key-pic", "key-gap")
      readToml(archive, "2010/07/19/Picnic.toml") shouldBe before
      File(archive, "Picnic.toml").exists shouldBe false
      links(archive) shouldBe "# gphoto-album-links 1\ncid-trip\tTrip.toml\n"
      readToml(archive, "Trip.toml") shouldBe "external = true\n"

  it should "not store an empty catalog title on an owner file" in :
    withArchive: archive =>
      val before: String =
        """external = true
          |google-title = "Trip"
          |owner = "Ada"
          |""".stripMargin
      writeToml(archive, "Trip.toml", before)
      writeLinks(archive, "cid-empty" -> "Trip.toml")
      writeCatalog(
        archive,
        "album\tcid-empty\t0\t",
        "owner\tcid-empty\tAda"
      )
      val text: String = ok(exec(sessionFor(archive), "reconcile")._2)
      text shouldBe "skipped  owner\n"
      quiet(text, "cid-empty")
      readToml(archive, "Trip.toml") shouldBe before
      links(archive) shouldBe "# gphoto-album-links 1\ncid-empty\tTrip.toml\n"
      val again: String = ok(exec(sessionFor(archive), "reconcile")._2)
      again shouldBe text
      readToml(archive, "Trip.toml") shouldBe before

  it should "not set an empty published-title from an empty catalog title" in :
    withArchive: archive =>
      val before: String =
        """google-album-id = "library-1"
          |google-title = "Away"
          |published-title = "Away"
          |""".stripMargin
      writeToml(archive, "Trip.toml", before)
      writeLinks(archive, "cid-empty" -> "Trip.toml")
      writeCatalog(archive, "album\tcid-empty\t0\t")
      val text: String = ok(exec(sessionFor(archive), "reconcile")._2)
      text shouldBe "skipped  title\n"
      quiet(text, "cid-empty", "library-1")
      readToml(archive, "Trip.toml") shouldBe before
      links(archive) shouldBe "# gphoto-album-links 1\ncid-empty\tTrip.toml\n"

  it should "preview the link rows a real --delete would keep" in :
    withArchive: archive =>
      val before: String =
        """external = true
          |google-title = "Old Title"
          |""".stripMargin
      writeToml(archive, "Old.toml", before)
      mkdir(archive, "2010/07/19")
      touch(File(archive, "2010/07/19"), "1234.jpg")
      val link: String = "# gphoto-album-links 1\ncid-old\tOld.toml\n"
      writeLinks(archive, "cid-old" -> "Old.toml")
      writeCatalog(archive)
      val kept: Result = exec(sessionFor(archive), "reconcile", "--dry-run")._2
      ok(kept) shouldBe "gone Old Title\n"
      linkRows(kept) shouldBe "cid-old\tOld.toml"
      val preview: Result = exec(sessionFor(archive), "reconcile", "--dry-run", "--delete")._2
      val text: String = ok(preview)
      text shouldBe "gone Old Title\n"
      quiet(text, "cid-old")
      linkRows(preview) shouldBe ""
      readToml(archive, "Old.toml") shouldBe before
      File(archive, "2010/07/19/1234.jpg").isFile shouldBe true
      links(archive) shouldBe link
      val deleted: String = ok(exec(sessionFor(archive), "reconcile", "--delete")._2)
      deleted shouldBe
        """gone Old Title
          |wrote Old.toml
          |wrote .gphoto-sync/album-links.tsv
          |""".stripMargin
      File(archive, "Old.toml").exists shouldBe false
      File(archive, "2010/07/19/1234.jpg").isFile shouldBe true
      links(archive) shouldBe ""

  private def ok(result: Result): String = result match
    case Result.Ok(text, _) => text
    case other => fail(other.toString)

  private def valueHas(result: Result, id: String): Unit = result match
    case Result.Ok(_, value) => value.toString should include (id)
    case other => fail(other.toString)

  private def linkRows(result: Result): String = result match
    case Result.Ok(_, Json.Obj(fields)) =>
      fields.collectFirst:
        case ("links", Json.Arr(rows)) =>
          rows.flatMap:
            case Json.Obj(link) =>
              for
                id <- link.collectFirst { case ("id", Json.Str(value)) => value }
                path <- link.collectFirst { case ("path", Json.Str(value)) => value }
              yield s"$id\t$path"
            case _ =>
              None
          .mkString("\n")
      .getOrElse(fail("no links"))
    case other =>
      fail(other.toString)

  private def quiet(text: String, ids: String*): Unit =
    ids.foreach: id =>
      withClue(id):
        text should not include id

  private def usage(session: Session, args: String*): String =
    exec(session, args*)._2 match
      case Result.Usage(message) => message
      case other => fail(other.toString)

  private def exec(session: Session, args: String*): (Session, Result) =
    Cli.parse(args.toList) match
      case Cli.Run(parsed) =>
        Cli.toOperation(parsed).execute(session)
      case other =>
        fail(other.toString)

  private def shell(
    session: Session,
    lines: Seq[String],
    exchange: Option[(Session, List[String]) => Result]
  ): (Int, String, String) =
    val out = new ByteArrayOutputStream
    val err = new ByteArrayOutputStream
    val code: Int = Console.withOut(out):
      Console.withErr(err):
        Cli.shell(session, lines.iterator, exchange = exchange)
    (code, new String(out.toByteArray, StandardCharsets.UTF_8), new String(err.toByteArray, StandardCharsets.UTF_8))

  private def sessionFor(archive: File): Session = Session(rootFor(archive))

  private def rootFor(archive: File): Root =
    val home: File = File(System.getProperty("user.home"))
    Root(Configuration(home.toPath.relativize(archive.toPath).toString, zone))

  private def writeCatalog(archive: File, lines: String*): File =
    val directory: File = File(archive, ".gphoto-sync")
    directory.mkdirs()
    val file: File = File(directory, "catalog.txt")
    JFiles.writeString(file.toPath, (Seq("# gphoto-catalog 2") ++ lines).mkString("", "\n", "\n"))
    file

  private def writeLinks(archive: File, rows: (String, String)*): Unit =
    val body: String = rows.map((id, path) => s"$id\t$path\n").mkString
    writeRaw(archive, ".gphoto-sync/album-links.tsv", s"# gphoto-album-links 1\n$body")

  private def writeRaw(archive: File, path: String, text: String): Unit =
    val file: File = File(archive, path)
    file.getParentFile.mkdirs()
    JFiles.writeString(file.toPath, text)

  private def writeToml(archive: File, path: String, text: String): Unit =
    writeRaw(archive, path, text)

  private def readToml(archive: File, path: String): String =
    JFiles.readString(File(archive, path).toPath)

  private def links(archive: File): String =
    val file: File = File(archive, ".gphoto-sync/album-links.tsv")
    if !file.isFile then "" else JFiles.readString(file.toPath)

  private def requests(archive: File): File = File(archive, ".gphoto-sync/requests.tsv")

  private def partials(archive: File): Seq[String] =
    val dot: File = File(archive, ".gphoto-sync")
    if !dot.isDirectory then Seq.empty
    else Option(dot.list).getOrElse(Array.empty[String]).toSeq.filter(_.contains(".partial"))

  private def tomls(archive: File): Seq[String] =
    def walk(file: File): Seq[String] =
      if file.isDirectory && file.getName != ".gphoto-sync" then
        Option(file.listFiles).getOrElse(Array.empty[File]).toSeq.flatMap(walk)
      else if file.isFile && file.getName.endsWith(".toml") then
        Seq(archive.toPath.relativize(file.toPath).toString)
      else Seq.empty
    walk(archive).sorted

  private def mkdir(archive: File, path: String): Unit = File(archive, path).mkdirs()

  private def touch(directory: File, name: String): Unit =
    directory.mkdirs()
    JFiles.write(File(directory, name).toPath, Array.emptyByteArray)

  private def withArchive(body: File => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("reconcile").toFile
    try body(archive)
    finally deleteTree(archive)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
