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

  "agent catalog" should "list albums without ids and cache a parsed file" in:
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

      val kept: String = JFiles.readString(file.toPath).replace("June", "June")
      JFiles.writeString(file.toPath, JFiles.readString(file.toPath).replace("Beta", "Zeta"))
      file.setLastModified(stamp) shouldBe true
      val (_, cached) = exec(listed, "agent", "catalog")
      cached match
        case Result.Ok(text, _) =>
          text should include("""owner="Ada Lovelace"""")
          text should not include ("Zeta")
        case other =>
          fail(other.toString)
      file.setLastModified(stamp + 120_000L) shouldBe true
      val (_, refreshed) = exec(listed, "agent", "catalog")
      refreshed match
        case Result.Ok(text, _) => text should include("\"Zeta\"")
        case other              => fail(other.toString)
      kept should include("Beta")

      val (same, items) = exec(listed, "agent", "items", "--id", "a1", "Alpha")
      items match
        case Result.Ok(text, value) =>
          text shouldBe "one.jpg 2010-07-19 -\n"
          noIds(text, "a1", "k1")
          field(value, "id") shouldBe Json.Str("a1")
          field(
            field(value, "items") match
              case Json.Arr(Seq(row)) => row
              case other              => fail(other.toString)
            ,
            "key"
          ) shouldBe Json.Str("k1")
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
        case other              => fail(other.toString)
      skippedAdd.cached(day).isEmpty shouldBe true
      val (skippedNames, unread) = exec(skippedAdd, "agent", "match", "Epsilon")
      unread shouldBe Result.Ok("skipped unread names\n", emptyMatch("e1"))
      unread match
        case Result.Ok(text, _) => noIds(text, "e1", "k3", "k4", "three.jpg")
        case other              => fail(other.toString)
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
        case other              => fail(other.toString)
      matched.cached(day).isDefined shouldBe true

  it should "reject a catalog that is not version 2 and not cache the failure" in:
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

  it should "not create a catalog file" in:
    withArchive: archive =>
      val (session, result) = exec(Session(rootFor(archive)), "agent", "catalog")
      result shouldBe Result.Usage("catalog is absent")
      session.catalogCache.isEmpty shouldBe true
      File(archive, ".gphoto-sync").exists shouldBe false

  "agent stems" should "read sidecar TOML and not EXIF" in:
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
          field(stemRow(value, "2413"), "metadata") shouldBe Json.Obj(
            Seq(
              "bad-raw" -> Json.Bool(true),
              "google-media-item-id" -> Json.Str("media-1"),
              "uploader" -> Json.Str("Ada")
            )
          )
          field(stemRow(value, "2412"), "extensions") shouldBe
            Json.Arr(Seq(Json.Str("cr2"), Json.Str("jpg")))
          field(stemRow(value, "2412"), "metadata") shouldBe Json.Obj(Seq.empty)
        case other =>
          fail(other.toString)
      second match
        case Result.Ok(text, _) => text shouldBe expected
        case other              => fail(other.toString)
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

  "agent match" should "join catalog filenames to local stems and write nothing" in:
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
          |date-mismatch 2010/07/19 8888.jpg 2010/07/18 8888
          |""".stripMargin
      result shouldBe Result.Ok(
        text,
        matchValue(
          "m1",
          Seq(
            "2010/07/19" -> "2412",
            "2010/07/19" -> "123v",
            "2010/07/19" -> "2412.MP",
            "2010/07/19" -> "123v"
          ),
          Seq("k2", "k5"),
          Seq(("2010/07/19", "8888.jpg", "2010/07/18", "8888"))
        )
      )
      result match
        case Result.Ok(shown, _) =>
          noIds(shown, "m1", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "Ada", "Bea")
        case other =>
          fail(other.toString)
      loaded.cached(day).isDefined shouldBe true
      loaded.cached(File(archive, "2010/07/18")).isDefined shouldBe true
      loaded.cached(File(archive, "2010/07/20")).isEmpty shouldBe true
      File(day, "2412.cr2").isFile shouldBe true
      File(day, "2412.jpg").exists shouldBe false
      File(day, "123v.mp4").isFile shouldBe true
      Option(File(archive, ".gphoto-sync").list()).get.toSet shouldBe Set("catalog.txt")

  "agent match" should "join an m4v name to the v stem" in:
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

  "agent include" should "compress against every day in the span and write nothing" in:
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
        "agent",
        "include",
        "--span",
        "2010/07",
        "2010/07/19/2412",
        "2010/07/19/2413",
        "2010/07/19/2414",
        "2010/07/20/9999"
      )
      all shouldBe Result.Ok("all\n", includeValue(Seq("all"), Seq("all")))
      val (_, curated) = exec(
        session,
        "agent",
        "include",
        "2010/07/19/2412",
        "2010/07/19/2414"
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

  "agent drift" should "report a create and write nothing" in:
    withArchive: archive =>
      writeCatalog(archive, "album\tcid-notes\t0\tNotes")
      val (text, value) = unchanged(archive)(execOk(archive, "agent", "drift"))
      text shouldBe "create Notes\n"
      text should not include ("wrote")
      noIds(text, "cid-notes")
      value shouldBe driftValue("create", "Notes", "Notes.toml", None, Some("cid-notes"))
      File(archive, "Notes.toml").exists shouldBe false
      File(archive, ".gphoto-sync/album-links.tsv").exists shouldBe false

  it should "report an update and write nothing" in:
    withArchive: archive =>
      val toml: String =
        """# keep
          |external = true
          |google-title = "Hawaii"
          |owner = "Ada"
          |""".stripMargin
      writeRaw(archive, "Hawaii.toml", toml)
      writeRaw(archive, ".gphoto-sync/album-links.tsv", "# gphoto-album-links 1\ncid-hawaii\tHawaii.toml\n")
      writeCatalog(
        archive,
        "album\tcid-hawaii\t0\tHawaii Trip",
        "owner\tcid-hawaii\tAda"
      )
      val (text, value) = unchanged(archive)(execOk(archive, "agent", "drift"))
      text shouldBe "update Hawaii Trip\n"
      noIds(text, "cid-hawaii")
      value shouldBe driftValue("update", "Hawaii Trip", "Hawaii.toml", None, Some("cid-hawaii"))
      JFiles.readString(File(archive, "Hawaii.toml").toPath) shouldBe toml

  it should "report a gone link and write nothing" in:
    withArchive: archive =>
      writeRaw(
        archive,
        "Old.toml",
        """external = true
          |google-title = "Old Title"
          |""".stripMargin
      )
      writeRaw(archive, ".gphoto-sync/album-links.tsv", "# gphoto-album-links 1\ncid-old\tOld.toml\n")
      writeCatalog(archive)
      val (text, value) = unchanged(archive)(execOk(archive, "agent", "drift"))
      text shouldBe "gone Old Title\n"
      noIds(text, "cid-old")
      value shouldBe driftValue("gone", "Old Title", "Old.toml", None, Some("cid-old"))

  it should "report a pending file and write nothing" in:
    withArchive: archive =>
      writeRaw(archive, "Pending.toml", "external = false\n")
      writeCatalog(archive)
      val (text, value) = unchanged(archive)(execOk(archive, "agent", "drift"))
      text shouldBe "pending Pending.toml\n"
      value shouldBe driftValue("pending", "", "Pending.toml", None, None)

  it should "report a pending rename and write nothing" in:
    withArchive: archive =>
      val toml: String = "external = true\n"
      writeRaw(archive, "Best Of.toml", toml)
      writeCatalog(archive, "album\tcid-best\t0\tBest  Of")
      val (text, value) = unchanged(archive)(execOk(archive, "agent", "drift"))
      text shouldBe "pending-rename Best Of.toml\n"
      noIds(text, "cid-best")
      value shouldBe driftValue("pending-rename", "", "Best Of.toml", None, Some("cid-best"))

  it should "report a skipped album and write nothing" in:
    withArchive: archive =>
      writeCatalog(
        archive,
        "album\tcid-travel\t0\tTravel",
        "auto-add\tcid-travel"
      )
      val (text, value) = unchanged(archive)(execOk(archive, "agent", "drift"))
      text shouldBe "skipped Travel auto-add\n"
      noIds(text, "cid-travel")
      value shouldBe driftValue("skipped", "Travel", "", Some("auto-add"), Some("cid-travel"))
      File(archive, "Travel.toml").exists shouldBe false

  "agent missing" should "list absent items, including another uploader, and not a prefix hit" in:
    withArchive: archive =>
      val day: File = File(archive, "2010/07/19")
      touch(day, "2412.jpg")
      touch(day, "123.jpg")
      touch(day, "123v.mp4")
      touch(day, "ghost.jpg")
      touch(day, "5555.MP.jpg")
      touch(day, "kept.jpg")
      File(archive, "2010/07/20").mkdirs() shouldBe true
      writeCatalog(
        archive,
        "album\taid-trip\t7\tTrip",
        "item\taid-trip\t1000.jpg\t2010-07-19\tk-own\t-",
        "item\taid-trip\t2000.jpg\t2010-07-19\tk-ada\tAda",
        "item\taid-trip\tIMG_2412.JPG\t2010-07-19\tk-prefix\t-",
        "item\taid-trip\t123.mp4\t2010-07-19\tk-video\t-",
        "item\taid-trip\tghost.jpg\t-\tk-ghost\t-",
        "item\taid-trip\t5555.MP.jpg\t2010-07-19\tk-mp\t-",
        "item\taid-trip\t6666.PORTRAIT.jpg\t2010-07-19\tk-port\t-",
        "album\taid-eps\t2\tEpsilon",
        "error\taid-eps\t2\t2\t1",
        "item\taid-eps\tkept.jpg\t2010-07-19\tk-kept\t-",
        "unset\taid-eps\t2010-07-20\tk-unset\tCara",
        "album\taid-box\t0\tInbox",
        "auto-add\taid-box"
      )
      val beforeFiles: Map[String, String] = filesOf(archive)
      val before: Long = Extension.constructed
      val (loaded, result) = exec(Session(rootFor(archive)), "agent", "missing")
      Extension.constructed shouldBe before
      filesOf(archive) shouldBe beforeFiles
      val text: String =
        """missing Trip 2010-07-19 1000.jpg -
          |missing Trip 2010-07-19 2000.jpg Ada
          |missing Trip - ghost.jpg -
          |missing Trip 2010-07-19 6666.PORTRAIT.jpg -
          |""".stripMargin
      result shouldBe Result.Ok(
        text,
        Json.Obj(
          Seq(
            "missing" -> Json.Arr(
              Seq(
                missingRow("Trip", "2010-07-19", "1000.jpg", "-", "k-own"),
                missingRow("Trip", "2010-07-19", "2000.jpg", "Ada", "k-ada"),
                missingRow("Trip", "-", "ghost.jpg", "-", "k-ghost"),
                missingRow("Trip", "2010-07-19", "6666.PORTRAIT.jpg", "-", "k-port")
              )
            )
          )
        )
      )
      result match
        case Result.Ok(shown, value) =>
          noIds(
            shown,
            "aid-trip",
            "aid-eps",
            "aid-box",
            "k-own",
            "k-ada",
            "k-prefix",
            "k-video",
            "k-ghost",
            "k-mp",
            "k-port",
            "k-kept",
            "k-unset"
          )
          value.toString should not include ("aid-")
        case other =>
          fail(other.toString)
      loaded.cached(day).isDefined shouldBe true
      loaded.cached(File(archive, "2010/07/20")).isDefined shouldBe true
      File(archive, "2010/07/18").exists shouldBe false

  "agent years" should "list year names and skip dot directories" in:
    withArchive: archive =>
      val missing: File = File(archive, "absent")
      val (untouched, empty) = exec(Session(rootFor(missing)), "agent", "years")
      empty shouldBe Result.Ok("", Json.Obj(Seq("years" -> Json.Arr(Seq.empty))))
      untouched.index.isEmpty shouldBe true
      untouched.catalogCache.isEmpty shouldBe true

      val (bare, none) = exec(Session(rootFor(archive)), "agent", "years")
      none shouldBe Result.Ok("", Json.Obj(Seq("years" -> Json.Arr(Seq.empty))))
      bare.index.isEmpty shouldBe true

      File(archive, ".upload/card").getParentFile.mkdirs()
      touch(File(archive, ".upload"), "card.jpg")
      File(archive, ".secret").mkdirs()
      JFiles.writeString(File(archive, "AGENTS.md").toPath, "notes\n")
      JFiles.writeString(File(archive, "Best.toml").toPath, "include = [\"all\"]\n")
      touch(File(archive, "2010/08"), "2412.jpg")
      touch(File(archive, "2009/03"), "0001.jpg")
      val (listed, years) = exec(bare, "agent", "years")
      years shouldBe Result.Ok(
        "2009\n2010\n",
        Json.Obj(Seq("years" -> Json.Arr(Seq(Json.Str("2009"), Json.Str("2010")))))
      )
      listed.index.isEmpty shouldBe true
      listed.catalogCache.isEmpty shouldBe true
      listed.cached(File(archive, "2010")).isEmpty shouldBe true
      noRequests(archive)

      JFiles.writeString(File(archive, "readme.txt").toPath, "stray\n")
      exec(listed, "agent", "years")._2 match
        case Result.Usage(message) => message should include("readme.txt")
        case other                 => fail(other.toString)

  "agent months" should "list one year and reject anything else" in:
    withArchive: archive =>
      touch(File(archive, "2010/08"), "2412.jpg")
      touch(File(archive, "2010/07"), "0001.jpg")
      val session: Session = Session(rootFor(archive))
      val (listed, months) = exec(session, "agent", "months", "2010")
      listed.index.isEmpty shouldBe true
      listed.cached(File(archive, "2010")).isEmpty shouldBe true
      months shouldBe Result.Ok(
        "2010/07\n2010/08\n",
        Json.Obj(
          Seq(
            "year" -> Json.Str("2010"),
            "present" -> Json.Bool(true),
            "months" -> Json.Arr(Seq(Json.Str("2010/07"), Json.Str("2010/08")))
          )
        )
      )
      JFiles.write(File(archive, "2010/readme.txt").toPath, Array.emptyByteArray)
      exec(listed, "agent", "months", "2010")._2 match
        case Result.Usage(message) => message should include("readme.txt")
        case other                 => fail(other.toString)

      val (missing, absent) = exec(session, "agent", "months", "2099")
      absent shouldBe Result.Ok(
        "",
        Json.Obj(
          Seq(
            "year" -> Json.Str("2099"),
            "present" -> Json.Bool(false),
            "months" -> Json.Arr(Seq.empty)
          )
        )
      )
      missing.index.isEmpty shouldBe true
      missing.catalogCache.isEmpty shouldBe true
      JFiles.write(File(archive, "2011").toPath, Array.emptyByteArray)
      exec(session, "agent", "months", "2011")._2 match
        case Result.Usage(message) => message should include("is not a directory")
        case other                 => fail(other.toString)
      List("2010/07", "2010/07/19", "foo", ".upload", "2412v").foreach: path =>
        exec(session, "agent", "months", path)._2 shouldBe Result.Usage("not a year")
      noRequests(archive)

  "agent days" should "list one month and keep an impossible directory name" in:
    withArchive: archive =>
      touch(File(archive, "2010/07/20"), "0001.jpg")
      touch(File(archive, "2010/07/19"), "0002.jpg")
      File(archive, "2010/02/01").mkdirs()
      File(archive, "2010/02/31").mkdirs()
      val session: Session = Session(rootFor(archive))
      val (listed, days) = exec(session, "agent", "days", "2010/07")
      listed.index.isEmpty shouldBe true
      listed.cached(File(archive, "2010/07")).isEmpty shouldBe true
      days shouldBe Result.Ok(
        "2010/07/19\n2010/07/20\n",
        Json.Obj(
          Seq(
            "month" -> Json.Str("2010/07"),
            "present" -> Json.Bool(true),
            "days" -> Json.Arr(Seq(Json.Str("2010/07/19"), Json.Str("2010/07/20")))
          )
        )
      )
      exec(session, "agent", "days", "2010/02")._2 shouldBe Result.Ok(
        "2010/02/01\n2010/02/31\n",
        Json.Obj(
          Seq(
            "month" -> Json.Str("2010/02"),
            "present" -> Json.Bool(true),
            "days" -> Json.Arr(Seq(Json.Str("2010/02/01"), Json.Str("2010/02/31")))
          )
        )
      )
      val (missing, absent) = exec(session, "agent", "days", "2099/01")
      absent shouldBe Result.Ok(
        "",
        Json.Obj(
          Seq(
            "month" -> Json.Str("2099/01"),
            "present" -> Json.Bool(false),
            "days" -> Json.Arr(Seq.empty)
          )
        )
      )
      missing.index.isEmpty shouldBe true
      List("2010", "2010/07/19", "2010/02/31", "foo").foreach: path =>
        exec(session, "agent", "days", path)._2 shouldBe Result.Usage("not a month")
      noRequests(archive)

  "agent claims" should "list one day's titles and not scan an empty day" in:
    withArchive: archive =>
      val tifDay: File = File(archive, "2010/05/02")
      touch(tifDay, "2412.tif")
      val session: Session = Session(rootFor(archive))
      val (tifSession, tifClaims) = exec(session, "agent", "claims", "2010/05/02")
      tifClaims shouldBe Result.Ok(
        "",
        Json.Obj(
          Seq(
            "path" -> Json.Str("2010/05/02"),
            "present" -> Json.Bool(true),
            "claims" -> Json.Arr(Seq.empty)
          )
        )
      )
      tifSession.cached(tifDay).isDefined shouldBe true
      tifSession.cached(File(archive, "2010")).isEmpty shouldBe true
      tifSession.cached(archive).isEmpty shouldBe true
      tifSession.catalogCache.isEmpty shouldBe true
      exec(tifSession, "agent", "stems", "2010/05/02")._2 match
        case Result.Ok(text, value) =>
          text shouldBe ""
          field(value, "stems") shouldBe Json.Arr(Seq.empty)
        case other =>
          fail(other.toString)
      File(tifDay, "2412.tif").isFile shouldBe true

      val day: File = File(archive, "2010/06/02")
      touch(day, "2412.jpg")
      touch(day, "2413.jpg")
      touch(day, "2414.jpg")
      writeRaw(
        archive,
        "2010/06/02/Party.toml",
        "include = [\"2010/06/02 2412\"]\ngoogle-title = \"Secret Party\"\n"
      )
      writeRaw(
        archive,
        "2010/06/02/Except.toml",
        "include = [\"2010/06/02 2414 except 2414\"]\n"
      )
      writeRaw(archive, "Best.toml", "include = [\"2010/06/02 2412\"]\n")
      writeRaw(
        archive,
        "2010/07/19/to-08-23.toml",
        "include = [\"all\"]\ngoogle-title = \"July trip\"\n"
      )
      touch(File(archive, "2010/08/01"), "0002.jpg")
      writeCatalog(archive, "album\ta1\t0\tUnused")

      val (skipped, missing) = exec(session, "agent", "claims", "2099/01/01")
      missing shouldBe Result.Ok(
        "",
        Json.Obj(
          Seq(
            "path" -> Json.Str("2099/01/01"),
            "present" -> Json.Bool(false),
            "claims" -> Json.Arr(Seq.empty)
          )
        )
      )
      skipped.catalogCache.isEmpty shouldBe true
      skipped.index.isEmpty shouldBe true
      List("2010", "2010/06", "2010/06/02/2412", "2010/02/31").foreach: path =>
        exec(session, "agent", "claims", path)._2 shouldBe Result.Usage("not a day")

      val (loaded, claims) = exec(session, "agent", "claims", "2010/06/02")
      val text: String =
        """2010/06/02/2412 albums=2010/06/02/Party,Best
          |2010/06/02/2413 albums=-
          |2010/06/02/2414 albums=-
          |""".stripMargin
      claims shouldBe Result.Ok(
        text,
        Json.Obj(
          Seq(
            "path" -> Json.Str("2010/06/02"),
            "present" -> Json.Bool(true),
            "claims" -> Json.Arr(
              Seq(
                claimRow("2010/06/02/2412", "2412", Seq("2010/06/02/Party", "Best"), excludedOnly = false),
                claimRow("2010/06/02/2413", "2413", Seq.empty, excludedOnly = false),
                claimRow("2010/06/02/2414", "2414", Seq.empty, excludedOnly = true)
              )
            )
          )
        )
      )
      claims match
        case Result.Ok(shown, value) =>
          shown should not include ("Secret Party")
          shown should not include ("google=")
          value.toString should not include ("Secret Party")
          value.toString should not include ("google-title")
          value.toString should not include ("google-album-id")
        case other =>
          fail(other.toString)
      loaded.catalogCache.isEmpty shouldBe true

      val (_, later) = exec(loaded, "agent", "claims", "2010/08/01")
      later shouldBe Result.Ok(
        "2010/08/01/0002 albums=2010/07/19-08/23\n",
        Json.Obj(
          Seq(
            "path" -> Json.Str("2010/08/01"),
            "present" -> Json.Bool(true),
            "claims" -> Json.Arr(
              Seq(
                claimRow("2010/08/01/0002", "0002", Seq("2010/07/19-08/23"), excludedOnly = false)
              )
            )
          )
        )
      )
      later match
        case Result.Ok(shown, value) =>
          shown should not include ("July trip")
          value.toString should not include ("July trip")
        case other =>
          fail(other.toString)

      val root: Root = rootFor(archive)
      val serve: Session = Session(root, PathStyle.Serve, root)
      val (kept, served) = exec(serve, "agent", "claims", "2010/06/02")
      served match
        case Result.Ok(shown, _) => shown shouldBe text
        case other               => fail(other.toString)
      DirectoryIndex.cursor shouldBe None
      kept.cached(day).isDefined shouldBe true
      kept.catalogCache.isEmpty shouldBe true
      noRequests(archive)

  "agent locate" should "list catalog rows for one local stem and not read a catalog it cannot hit" in:
    withArchive: archive =>
      val day: File = File(archive, "2010/07/19")
      touch(day, "IMG_2412.jpg")
      touch(day, "2412.mp4")
      touch(day, "9999.jpg")
      JFiles.writeString(File(day, "2412.toml").toPath, "=\n")
      File(archive, "2010/07/20").mkdirs() shouldBe true
      File(archive, "2010/02/31").mkdirs() shouldBe true
      val session: Session = Session(rootFor(archive))

      val (missingDir, absent) = exec(session, "agent", "locate", "2099/01/01", "2412")
      absent shouldBe Result.Ok(
        "",
        locateValue("2099/01/01", "2412", directory = false, present = false, Seq.empty)
      )
      missingDir.catalogCache.isEmpty shouldBe true
      missingDir.index.isEmpty shouldBe true
      File(archive, ".gphoto-sync").exists shouldBe false

      val (rawSession, rawName) = exec(session, "agent", "locate", "2010/07/19", "IMG_2412")
      rawName shouldBe Result.Ok(
        "",
        locateValue("2010/07/19", "IMG_2412", directory = true, present = false, Seq.empty)
      )
      rawSession.catalogCache.isEmpty shouldBe true
      rawSession.cached(day).isDefined shouldBe true
      File(File(archive, ".gphoto-sync"), "catalog.txt").exists shouldBe false

      List("2010", "2010/07", "2010/07/19/2412", "2010/02/31").foreach: path =>
        exec(session, "agent", "locate", path, "2412")._2 shouldBe Result.Usage("not a day")
      exec(session, "agent", "locate", "2010/07/19", "2412")._2 shouldBe Result.Usage("catalog is absent")

      writeCatalog(
        archive,
        "album\taid-alpha\t4\tAlpha",
        "item\taid-alpha\tIMG_2412.jpg\t2010-07-19\tk-alpha\t-",
        "item\taid-alpha\tghost.jpg\t-\tk-ghost\t-",
        "item\taid-alpha\t1000.jpg\t2010-07-20\tk-other\t-",
        "item\taid-alpha\t2412.mp4\t2010-07-19\tk-video\t-",
        "album\taid-trip\t2\tBest Trip",
        "error\taid-trip\t2\t2\t1",
        "item\taid-trip\tIMG_2412.JPG\t2010-07-19\tk-trip\t-",
        "unset\taid-trip\t2010-07-20\tk-unset\tCara",
        "album\taid-beta\t1\tBeta",
        "item\taid-beta\t2412.jpg\t2010-07-19\tk-beta\tAda",
        "album\taid-box\t0\tInbox",
        "auto-add\taid-box"
      )
      val before: Long = Extension.constructed
      val beforeFiles: Map[String, String] = filesOf(archive)
      val (loaded, found) = exec(Session(rootFor(archive)), "agent", "locate", "2010/07/19", "2412")
      Extension.constructed shouldBe before
      filesOf(archive) shouldBe beforeFiles
      val text: String =
        """Alpha 2010-07-19 IMG_2412.jpg -
          |Best Trip 2010-07-19 IMG_2412.JPG -
          |Beta 2010-07-19 2412.jpg Ada
          |""".stripMargin
      val rows: Seq[Json] = Seq(
        locateRow("Alpha", "2010-07-19", "IMG_2412.jpg", "-", "aid-alpha", "k-alpha"),
        locateRow("Best Trip", "2010-07-19", "IMG_2412.JPG", "-", "aid-trip", "k-trip"),
        locateRow("Beta", "2010-07-19", "2412.jpg", "Ada", "aid-beta", "k-beta")
      )
      found shouldBe Result.Ok(
        text,
        locateValue("2010/07/19", "2412", directory = true, present = true, rows)
      )
      found match
        case Result.Ok(shown, value) =>
          noIds(
            shown,
            "aid-alpha",
            "aid-trip",
            "aid-beta",
            "aid-box",
            "k-alpha",
            "k-ghost",
            "k-other",
            "k-video",
            "k-trip",
            "k-unset",
            "k-beta"
          )
          shown should not include ("missing")
          shown should not include ("ambiguous")
          shown should not include ("matched")
          shown should not include ("\"")
          field(value, "ambiguous") shouldBe Json.Bool(true)
          value.toString should include("k-alpha")
          value.toString should not include ("k-ghost")
          value.toString should not include ("k-other")
          value.toString should not include ("k-video")
          value.toString should not include ("k-unset")
          value.toString should not include ("aid-box")
        case other =>
          fail(other.toString)
      loaded.catalogCache.isDefined shouldBe true
      loaded.cached(day).isDefined shouldBe true
      loaded.cached(File(archive, "2010/07/20")).isEmpty shouldBe true

      val (_, video) = exec(loaded, "agent", "locate", "2010/07/19", "2412v")
      video shouldBe Result.Ok(
        "Alpha 2010-07-19 2412.mp4 -\n",
        locateValue(
          "2010/07/19",
          "2412v",
          directory = true,
          present = true,
          Seq(locateRow("Alpha", "2010-07-19", "2412.mp4", "-", "aid-alpha", "k-video"))
        )
      )
      video match
        case Result.Ok(shown, value) =>
          noIds(shown, "aid-alpha", "k-video")
          field(value, "ambiguous") shouldBe Json.Bool(false)
        case other =>
          fail(other.toString)

      exec(loaded, "agent", "locate", "2010/07/19", "9999")._2 shouldBe Result.Ok(
        "",
        locateValue("2010/07/19", "9999", directory = true, present = true, Seq.empty)
      )

      val (stillCold, stillRaw) = exec(Session(rootFor(archive)), "agent", "locate", "2010/07/19", "IMG_2412")
      stillRaw shouldBe Result.Ok(
        "",
        locateValue("2010/07/19", "IMG_2412", directory = true, present = false, Seq.empty)
      )
      stillCold.catalogCache.isEmpty shouldBe true

      val (stillMissing, stillAbsent) = exec(Session(rootFor(archive)), "agent", "locate", "2099/01/01", "2412")
      stillAbsent shouldBe Result.Ok(
        "",
        locateValue("2099/01/01", "2412", directory = false, present = false, Seq.empty)
      )
      stillMissing.catalogCache.isEmpty shouldBe true
      stillMissing.index.isEmpty shouldBe true
      noRequests(archive)

  private def claimRow(path: String, stem: String, albums: Seq[String], excludedOnly: Boolean): Json =
    Json.Obj(
      Seq(
        "path" -> Json.Str(path),
        "stem" -> Json.Str(stem),
        "albums" -> Json.Arr(albums.map(Json.Str.apply)),
        "excludedOnly" -> Json.Bool(excludedOnly)
      )
    )

  private def noRequests(archive: File): Unit =
    File(archive, "requests.tsv").exists shouldBe false
    File(archive, "results.tsv").exists shouldBe false
    File(File(archive, ".gphoto-sync"), "requests.tsv").exists shouldBe false
    File(File(archive, ".gphoto-sync"), "results.tsv").exists shouldBe false

  private def emptyMatch(id: String): Json = matchValue(id, Seq.empty, Seq.empty)

  private def matchValue(
      id: String,
      matched: Seq[(String, String)],
      unmatched: Seq[String],
      mismatches: Seq[(String, String, String, String)] = Seq.empty
  ): Json =
    Json.Obj(
      Seq(
        "id" -> Json.Str(id),
        "matched" -> Json.Arr(
          matched.map(pair =>
            Json.Obj(
              Seq(
                "day" -> Json.Str(pair._1),
                "stem" -> Json.Str(pair._2)
              )
            )
          )
        ),
        "unmatched" -> Json.Arr(unmatched.map(Json.Str.apply)),
        "dateMismatches" -> Json.Arr(mismatches.map: (catalogDay, filename, archiveDay, stem) =>
          Json.Obj(
            Seq(
              "catalogDay" -> Json.Str(catalogDay),
              "filename" -> Json.Str(filename),
              "archiveDay" -> Json.Str(archiveDay),
              "stem" -> Json.Str(stem)
            )
          ))
      )
    )

  private def includeValue(rendered: Seq[String], logical: Seq[String]): Json =
    Json.Obj(
      Seq(
        "rendered" -> Json.Arr(rendered.map(Json.Str.apply)),
        "logical" -> Json.Arr(logical.map(Json.Str.apply))
      )
    )

  private def albumsOf(value: Json): Seq[Json] = field(value, "albums") match
    case Json.Arr(rows) => rows
    case other          => fail(other.toString)

  private def idsOf(value: Json): Seq[String] = albumsOf(value).map(idOf)

  private def idOf(value: Json): String = field(value, "id") match
    case Json.Str(id) => id
    case other        => fail(other.toString)

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

  private def driftValue(
      kind: String,
      title: String,
      path: String,
      reason: Option[String],
      id: Option[String]
  ): Json =
    Json.Obj(
      Seq(
        "actions" -> Json.Arr(
          Seq(
            Json.Obj(
              Seq(
                "class" -> Json.Str(kind),
                "title" -> Json.Str(title),
                "path" -> Json.Str(path),
                "reason" -> reason.fold(Json.Null)(Json.Str.apply),
                "id" -> id.fold(Json.Null)(Json.Str.apply)
              )
            )
          )
        )
      )
    )

  private def locateValue(
      day: String,
      stem: String,
      directory: Boolean,
      present: Boolean,
      matches: Seq[Json]
  ): Json =
    Json.Obj(
      Seq(
        "day" -> Json.Str(day),
        "stem" -> Json.Str(stem),
        "directory" -> Json.Bool(directory),
        "present" -> Json.Bool(present),
        "ambiguous" -> Json.Bool(matches.length > 1),
        "matches" -> Json.Arr(matches)
      )
    )

  private def locateRow(
      title: String,
      date: String,
      filename: String,
      uploader: String,
      id: String,
      key: String
  ): Json =
    Json.Obj(
      Seq(
        "title" -> Json.Str(title),
        "date" -> Json.Str(date),
        "filename" -> Json.Str(filename),
        "uploader" -> Json.Str(uploader),
        "id" -> Json.Str(id),
        "key" -> Json.Str(key)
      )
    )

  private def missingRow(title: String, date: String, filename: String, uploader: String, key: String): Json =
    Json.Obj(
      Seq(
        "title" -> Json.Str(title),
        "date" -> Json.Str(date),
        "filename" -> Json.Str(filename),
        "uploader" -> Json.Str(uploader),
        "key" -> Json.Str(key)
      )
    )

  private def execOk(archive: File, args: String*): (String, Json) =
    exec(Session(rootFor(archive)), args*)._2 match
      case Result.Ok(text, value) => (text, value)
      case other                  => fail(other.toString)

  private def unchanged(archive: File)(body: => (String, Json)): (String, Json) =
    val before: Map[String, String] = filesOf(archive)
    val result: (String, Json) = body
    filesOf(archive) shouldBe before
    result

  private def filesOf(archive: File): Map[String, String] =
    def walk(file: File): Seq[(String, String)] =
      if file.isDirectory then Option(file.listFiles).getOrElse(Array.empty[File]).toSeq.flatMap(walk)
      else Seq(archive.toPath.relativize(file.toPath).toString -> JFiles.readString(file.toPath))
    walk(archive).toMap

  private def writeRaw(archive: File, path: String, text: String): Unit =
    val file: File = File(archive, path)
    file.getParentFile.mkdirs()
    JFiles.writeString(file.toPath, text)

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
