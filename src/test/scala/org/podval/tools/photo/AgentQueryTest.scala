package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.io.File
import java.nio.file.{Files as JFiles}
import java.time.ZoneId

class AgentQueryTest extends AnyFlatSpec, Matchers:
  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  // strictEquality does not relate a sealed parent to its cases.
  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")

  "agent catalog" should "list albums without ids and cache a parsed file" in :
    withArchive: archive =>
      touch(File(archive, "2010/07/19"), "one.jpg")
      val file: File = writeCatalog(
        archive,
        "album\ta1\t1\tAlpha",
        "item\ta1\tone.jpg\t2010-07-19\tk1\t-",
        "album\ta2\t0\tAlpha",
        "album\ts1\t0\tAlpha ",
        "album\tb1\t0\tBeta",
        "owner\tb1\tAda Lovelace",
        "album\td1\t4\tDelta",
        "error\td1\t1\t4\t0",
        "item\td1\ttwo.jpg\t2010-07-19\tk2\t-",
        "album\te1\t2\tEpsilon",
        "error\te1\t2\t2\t1",
        "item\te1\tthree.jpg\t-\tk3\tBea",
        "unset\te1\t2010-07-20\tk4\tCara",
        "album\tc1\t0\tGamma",
        "auto-add\tc1"
      )
      val stamp: Long = 1_600_000_000_000L
      file.setLastModified(stamp) shouldBe true
      val session: Session = Session(rootFor(archive))
      val (listed, catalog) = exec(session, "agent", "catalog")
      catalog match
        case Result.Ok(text, value) =>
          text shouldBe
            """"Alpha" count=1 items=1 owned ambiguous
              |"Alpha" count=0 items=0 owned ambiguous
              |"Alpha " count=0 items=0 owned
              |"Beta" count=0 items=0 owner="Ada Lovelace"
              |"Delta" count=4 items=1 owned error got=1 of=4 name-failures=0
              |"Epsilon" count=2 items=1 owned error got=2 of=2 name-failures=1 unset=1
              |"Gamma" count=0 items=0 owned auto-add
              |""".stripMargin
          noIds(text, "a1", "a2", "s1", "b1", "c1", "d1", "e1", "k1", "k2", "k3", "k4")
          idsOf(value) shouldBe Seq("a1", "a2", "s1", "b1", "d1", "e1", "c1")
          val beta: Json = albumsOf(value).find(row => idOf(row) == "b1").get
          field(beta, "owner") shouldBe Json.Str("Ada Lovelace")
          field(beta, "owned") shouldBe Json.Bool(false)
          field(beta, "autoAdd") shouldBe Json.Bool(false)
          field(field(albumsOf(value).find(row => idOf(row) == "d1").get, "error"), "nameFailures") shouldBe
            Json.Long(0)
        case other =>
          fail(other.toString)
      listed.catalogCache.isDefined shouldBe true
      File(archive, ".gphoto-sync/requests.tsv").exists shouldBe false
      File(archive, ".gphoto-sync/results.tsv").exists shouldBe false

      val kept: String = JFiles.readString(file.toPath).replace("June", "June")
      JFiles.writeString(file.toPath, JFiles.readString(file.toPath).replace("Beta", "Zeta"))
      file.setLastModified(stamp) shouldBe true
      val (_, cached) = exec(listed, "agent", "catalog")
      cached match
        case Result.Ok(text, _) =>
          text should include ("""owner="Ada Lovelace"""")
          text should not include ("Zeta")
        case other =>
          fail(other.toString)
      file.setLastModified(stamp + 120_000L) shouldBe true
      val (_, refreshed) = exec(listed, "agent", "catalog")
      refreshed match
        case Result.Ok(text, _) => text should include ("\"Zeta\"")
        case other => fail(other.toString)
      kept should include ("Beta")

      val (same, items) = exec(listed, "agent", "items", "--id", "a1", "Alpha")
      items match
        case Result.Ok(text, value) =>
          text shouldBe "one.jpg 2010-07-19 -\n"
          noIds(text, "a1", "k1")
          field(value, "id") shouldBe Json.Str("a1")
          field(field(value, "items") match
            case Json.Arr(Seq(row)) => row
            case other => fail(other.toString)
          , "key") shouldBe Json.Str("k1")
        case other =>
          fail(other.toString)
      exec(same, "agent", "items", "Alpha ")._2 match
        case Result.Ok(text, value) =>
          text shouldBe ""
          field(value, "id") shouldBe Json.Str("s1")
        case other =>
          fail(other.toString)
      exec(same, "agent", "items", "Alpha")._2 shouldBe
        Result.Usage("more than one album titled \"Alpha\"")
      exec(same, "agent", "items", "--id", "a1", "Beta")._2 shouldBe
        Result.Usage("catalog title does not match")
      exec(same, "agent", "items", "Missing")._2 shouldBe
        Result.Usage("no album titled \"Missing\"")
      exec(same, "agent", "items", "Gamma")._2 match
        case Result.Ok(text, value) =>
          text shouldBe "auto-add\n"
          noIds(text, "c1")
          field(value, "id") shouldBe Json.Str("c1")
          field(value, "items") shouldBe Json.Arr(Seq.empty)
        case other =>
          fail(other.toString)
      exec(same, "agent", "items", "Epsilon")._2 match
        case Result.Ok(text, value) =>
          text shouldBe "three.jpg - Bea\nunset 2010-07-20 Cara\n"
          noIds(text, "e1", "k3", "k4")
          field(value, "id") shouldBe Json.Str("e1")
        case other =>
          fail(other.toString)

      val day: File = File(archive, "2010/07/19")
      val (skippedAdd, add) = exec(same, "agent", "match", "Gamma")
      add shouldBe Result.Ok("skipped auto-add\n", emptyMatch("c1"))
      add match
        case Result.Ok(text, _) => noIds(text, "c1")
        case other => fail(other.toString)
      skippedAdd.cached(day).isEmpty shouldBe true
      val (skippedNames, unread) = exec(skippedAdd, "agent", "match", "Epsilon")
      unread shouldBe Result.Ok("skipped unread names\n", emptyMatch("e1"))
      unread match
        case Result.Ok(text, _) => noIds(text, "e1", "k3", "k4", "three.jpg")
        case other => fail(other.toString)
      skippedNames.cached(day).isEmpty shouldBe true
      val before: Long = Extension.constructed
      val (matched, shortfall) = exec(skippedNames, "agent", "match", "Delta")
      Extension.constructed shouldBe before
      shortfall shouldBe Result.Ok(
        "matched 0 of 1\nunmatched 2010/07/19 two.jpg\n",
        matchValue("d1", Seq.empty, Seq("k2"))
      )
      shortfall match
        case Result.Ok(text, _) => noIds(text, "d1", "k2")
        case other => fail(other.toString)
      matched.cached(day).isDefined shouldBe true

  it should "reject a catalog that is not version 2 and not cache the failure" in :
    withArchive: archive =>
      val directory: File = File(archive, ".gphoto-sync")
      directory.mkdirs()
      val file: File = File(directory, "catalog.txt")
      JFiles.writeString(file.toPath, "## \n")
      val session: Session = Session(rootFor(archive))
      val (failed, bad) = exec(session, "agent", "catalog")
      bad shouldBe Result.Usage("catalog is not version 2")
      failed.catalogCache.isEmpty shouldBe true
      writeCatalog(archive, "album\ta1\t0\tJune")
      val (ok, good) = exec(failed, "agent", "catalog")
      good match
        case Result.Ok(text, value) =>
          text shouldBe "\"June\" count=0 items=0 owned\n"
          noIds(text, "a1")
          idsOf(value) shouldBe Seq("a1")
        case other =>
          fail(other.toString)
      ok.catalogCache.isDefined shouldBe true

  it should "not create a catalog file" in :
    withArchive: archive =>
      val (session, result) = exec(Session(rootFor(archive)), "agent", "catalog")
      result shouldBe Result.Usage("catalog is absent")
      session.catalogCache.isEmpty shouldBe true
      File(archive, ".gphoto-sync").exists shouldBe false

  "agent stems" should "read sidecar TOML and not EXIF" in :
    withArchive: archive =>
      val day: File = File(archive, "2010/08/12")
      touch(day, "2412.cr2")
      touch(day, "2412.jpg")
      touch(day, "2413.jpg")
      touch(day, "IMG_9.jpg")
      JFiles.writeString(
        File(day, "2413.toml").toPath,
        "bad-raw = true\nuploader = \"Ada\"\ngoogle-media-item-id = \"media-1\"\n"
      )
      val before: Long = Extension.constructed
      val (loaded, result) = exec(Session(rootFor(archive)), "agent", "stems", "2010/08/12")
      val (again, second) = exec(loaded, "agent", "stems", "2010/08/12")
      Extension.constructed shouldBe before
      val expected: String = "2412 cr2+jpg\n2413 jpg+toml\n9 jpg\n"
      result match
        case Result.Ok(text, value) =>
          text shouldBe expected
          text should not include ("media-1")
          text should not include ("Ada")
          text should not include ("bad-raw")
          field(value, "day") shouldBe Json.Str("2010/08/12")
          field(stemRow(value, "2413"), "metadata") shouldBe Json.Obj(Seq(
            "bad-raw" -> Json.Bool(true),
            "google-media-item-id" -> Json.Str("media-1"),
            "uploader" -> Json.Str("Ada")
          ))
          field(stemRow(value, "2412"), "extensions") shouldBe
            Json.Arr(Seq(Json.Str("cr2"), Json.Str("jpg")))
          field(stemRow(value, "2412"), "metadata") shouldBe Json.Obj(Seq.empty)
        case other =>
          fail(other.toString)
      second match
        case Result.Ok(text, _) => text shouldBe expected
        case other => fail(other.toString)
      again.cached(day).get.grouped.map(_._1) shouldBe Seq("2412", "2413", "9")
      File(day, "IMG_9.jpg").isFile shouldBe true
      File(day, "9.jpg").exists shouldBe false
      exec(again, "agent", "stems", "2010")._2 shouldBe Result.Usage("not a day")
      exec(again, "agent", "stems", "2010/08")._2 shouldBe Result.Usage("not a day")
      exec(again, "agent", "stems", ".upload")._2 shouldBe Result.Usage("not a day")
      exec(again, "agent", "stems", "2010/08/12/2412")._2 shouldBe Result.Usage("not a day")
      exec(again, "agent", "stems", "2010/02/31")._2 shouldBe Result.Usage("not a day")
      exec(again, "agent", "stems", "2010/02/29")._2 shouldBe Result.Usage("not a day")
      exec(again, "agent", "stems", "foo")._2 shouldBe Result.Usage("not a day")
      exec(again, "agent", "stems", "2010/13")._2 shouldBe Result.Usage("not a day")
      exec(again, "agent", "stems", "2010/01/32")._2 shouldBe Result.Usage("not a day")
      File(archive, ".gphoto-sync").exists shouldBe false

  "agent match" should "join catalog filenames to local stems and write nothing" in :
    withArchive: archive =>
      val day: File = File(archive, "2010/07/19")
      touch(day, "2412.cr2")
      JFiles.writeString(File(day, "2412.toml").toPath, "bad-raw = true\n")
      touch(day, "123.jpg")
      touch(day, "123v.mp4")
      touch(day, "2412.MP.jpg")
      touch(File(archive, "2010/07/18"), "8888.jpg")
      writeCatalog(
        archive,
        "album\tm1\t7\tTrip",
        "item\tm1\tIMG_2412.JPG\t2010-07-19\tk1\tAda",
        "item\tm1\t9999.jpg\t2010-07-20\tk2\t-",
        "item\tm1\t123.mp4\t2010-07-19\tk3\t-",
        "item\tm1\tIMG_2412.MP.jpg\t2010-07-19\tk4\t-",
        "item\tm1\tghost.jpg\t-\tk5\t-",
        "item\tm1\tVID_123.mp4\t2010-07-19\tk6\tBea",
        "item\tm1\t8888.jpg\t2010-07-19\tk7\t-"
      )
      val before: Long = Extension.constructed
      val (loaded, result) = exec(Session(rootFor(archive)), "agent", "match", "Trip")
      Extension.constructed shouldBe before
      val text: String =
        """matched 4 of 7
          |2010/07/19 IMG_2412.JPG 2412
          |2010/07/19 123.mp4 123v
          |2010/07/19 IMG_2412.MP.jpg 2412.MP
          |2010/07/19 VID_123.mp4 123v
          |unmatched 2010/07/20 9999.jpg
          |unmatched - ghost.jpg
          |unmatched 2010/07/19 8888.jpg
          |""".stripMargin
      result shouldBe Result.Ok(text, matchValue(
        "m1",
        Seq(
          "2010/07/19" -> "2412",
          "2010/07/19" -> "123v",
          "2010/07/19" -> "2412.MP",
          "2010/07/19" -> "123v"
        ),
        Seq("k2", "k5", "k7")
      ))
      result match
        case Result.Ok(shown, _) =>
          noIds(shown, "m1", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "Ada", "Bea")
        case other =>
          fail(other.toString)
      loaded.cached(day).isDefined shouldBe true
      loaded.cached(File(archive, "2010/07/18")).isEmpty shouldBe true
      loaded.cached(File(archive, "2010/07/20")).isEmpty shouldBe true
      File(day, "2412.cr2").isFile shouldBe true
      File(day, "2412.jpg").exists shouldBe false
      File(day, "123v.mp4").isFile shouldBe true
      Option(File(archive, ".gphoto-sync").list()).get.toSet shouldBe Set("catalog.txt")

  "agent match" should "join an m4v name to the v stem" in :
    withArchive: archive =>
      val day: File = File(archive, "2010/07/19")
      touch(day, "123.jpg")
      touch(day, "123v.m4v")
      writeCatalog(
        archive,
        "album\tm1\t1\tTrip",
        "item\tm1\t123.m4v\t2010-07-19\tk3\t-"
      )
      val (_, result) = exec(Session(rootFor(archive)), "agent", "match", "Trip")
      result shouldBe Result.Ok(
        """matched 1 of 1
          |2010/07/19 123.m4v 123v
          |""".stripMargin,
        matchValue("m1", Seq("2010/07/19" -> "123v"), Seq.empty)
      )
      File(day, "123v.m4v").isFile shouldBe true
      File(day, "123.m4v").exists shouldBe false

  "agent include" should "compress against every day in the span and write nothing" in :
    withArchive: archive =>
      val day: File = File(archive, "2010/07/19")
      touch(day, "2412.jpg")
      touch(day, "2413.jpg")
      touch(day, "2414.jpg")
      touch(File(archive, "2010/07/20"), "9999.jpg")
      val session: Session = Session(rootFor(archive))
      val (_, partial) = exec(session, "agent", "include", "--span", "2010/07", "2010/07/19/2412")
      partial shouldBe Result.Ok(
        "2010/07/19 2412\n",
        includeValue(Seq("2010/07/19 2412"), Seq("2010/07/19 2412"))
      )
      val (_, all) = exec(
        session,
        "agent", "include", "--span", "2010/07",
        "2010/07/19/2412", "2010/07/19/2413", "2010/07/19/2414", "2010/07/20/9999"
      )
      all shouldBe Result.Ok("all\n", includeValue(Seq("all"), Seq("all")))
      val (_, curated) = exec(
        session, "agent", "include", "2010/07/19/2412", "2010/07/19/2414"
      )
      curated shouldBe Result.Ok(
        "2010/07/19 2412\n2010/07/19 2414\n",
        includeValue(Seq("2010/07/19 2412", "2010/07/19 2414"), Seq("2010/07/19 2412", "2010/07/19 2414"))
      )
      exec(session, "agent", "include")._2 shouldBe
        Result.Ok("", includeValue(Seq.empty, Seq.empty))
      exec(session, "agent", "include", "--span", "2010/07", "2010/08/01/2412")._2 shouldBe
        Result.Usage("claimed day outside the span")
      exec(session, "agent", "include", "2010/07/19/a/b")._2 shouldBe
        Result.Usage("not a picture path: 2010/07/19/a/b")
      exec(session, "agent", "include", "--span", "Best")._2 shouldBe Result.Usage("not a span")
      exec(session, "agent", "include", "--span", "2010/07/Name")._2 shouldBe Result.Usage("not a span")
      File(archive, ".gphoto-sync").exists shouldBe false
      File(day, "album.toml").exists shouldBe false

  private def emptyMatch(id: String): Json = matchValue(id, Seq.empty, Seq.empty)

  private def matchValue(id: String, matched: Seq[(String, String)], unmatched: Seq[String]): Json =
    Json.Obj(Seq(
      "id" -> Json.Str(id),
      "matched" -> Json.Arr(matched.map(pair => Json.Obj(Seq(
        "day" -> Json.Str(pair._1),
        "stem" -> Json.Str(pair._2)
      )))),
      "unmatched" -> Json.Arr(unmatched.map(Json.Str.apply))
    ))

  private def includeValue(rendered: Seq[String], logical: Seq[String]): Json =
    Json.Obj(Seq(
      "rendered" -> Json.Arr(rendered.map(Json.Str.apply)),
      "logical" -> Json.Arr(logical.map(Json.Str.apply))
    ))

  private def albumsOf(value: Json): Seq[Json] = field(value, "albums") match
    case Json.Arr(rows) => rows
    case other => fail(other.toString)

  private def idsOf(value: Json): Seq[String] = albumsOf(value).map(idOf)

  private def idOf(value: Json): String = field(value, "id") match
    case Json.Str(id) => id
    case other => fail(other.toString)

  private def stemRow(value: Json, stem: String): Json =
    field(value, "stems") match
      case Json.Arr(rows) =>
        rows.find(row => field(row, "stem") == Json.Str(stem)).getOrElse(fail(stem))
      case other =>
        fail(other.toString)

  private def field(value: Json, name: String): Json = value match
    case Json.Obj(fields) =>
      fields.find(_._1 == name).map(_._2).getOrElse(fail(s"missing $name in $value"))
    case other =>
      fail(other.toString)

  private def noIds(text: String, ids: String*): Unit =
    ids.foreach: id =>
      withClue(id):
        text should not include id

  private def exec(session: Session, args: String*): (Session, Result) =
    Cli.parse(args.toList) match
      case Cli.Run(parsed) =>
        Cli.toOperation(parsed).execute(session)
      case other =>
        fail(other.toString)

  private def rootFor(archive: File): Root =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    Root(Configuration(rootPath, zone))

  private def writeCatalog(archive: File, lines: String*): File =
    val directory: File = File(archive, ".gphoto-sync")
    directory.mkdirs()
    val file: File = File(directory, "catalog.txt")
    JFiles.writeString(file.toPath, (Seq("# gphoto-catalog 2") ++ lines).mkString("", "\n", "\n"))
    file

  private def touch(directory: File, name: String): File =
    directory.mkdirs()
    val file: File = File(directory, name)
    JFiles.write(file.toPath, Array.emptyByteArray)
    file

  private def withArchive(body: File => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("agent-query").toFile
    try body(archive)
    finally deleteTree(archive)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
