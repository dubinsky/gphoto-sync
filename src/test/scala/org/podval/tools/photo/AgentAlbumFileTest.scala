package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import java.io.File
import java.nio.file.{Files as JFiles}
import java.time.LocalDate

class AgentAlbumFileTest extends AnyFlatSpec, AgentWriteSupport:
  "agent album-file" should "place leftover names from the catalog title" in :
    withArchive: archive =>
      mkdir(archive, "2010/02/28")
      mkdir(archive, "2019/02/03")
      mkdir(archive, "2011/03/15")
      mkdir(archive, "2011/04/02")
      mkdir(archive, "2009/12/25")
      mkdir(archive, "2009/07/04")
      mkdir(archive, "2010/02/03")
      touch(File(archive, "2009/12/25"), "2412.jpg")
      writeCatalog(
        archive,
        "album\talbPur\t1\t2010 February Purim Seuda",
        "item\talbPur\tpic.jpg\t2010-02-28\tkeyPur\t-",
        "album\talbGold\t1\t2019 Feb 3 Bar Mitzvah Gold",
        "item\talbGold\tgold.jpg\t2019-02-03\tkeyGold\t-",
        "album\talbMar\t1\t2011 March-April",
        "item\talbMar\tm.jpg\t2011-03-15\tkeyMar\t-",
        "album\talbTrip\t1\t2011 March-April Trip",
        "item\talbTrip\tt.jpg\t2011-04-02\tkeyTrip\t-",
        "album\talbDec\t1\t2009 December",
        "item\talbDec\t2412.jpg\t2009-12-25\tkeyDec\t-",
        "album\talbJul\t1\t2009 July",
        "item\talbJul\tj.jpg\t2009-07-04\tkeyJul\t-",
        "album\talbSpan\t2\t2010 February Purim",
        "item\talbSpan\ta.jpg\t2010-02-03\tkeyA\t-",
        "item\talbSpan\tb.jpg\t2010-02-05\tkeyB\t-"
      )
      val session: Session = sessionFor(archive)
      place(session, "2010 February Purim Seuda", "2010/02/28/Purim Seuda.toml", "2010/02/28/Purim Seuda")
      place(session, "2019 Feb 3 Bar Mitzvah Gold", "2019/02/03/Bar Mitzvah Gold.toml", "2019/02/03/Bar Mitzvah Gold")
      place(session, "2011 March-April", "2011/03/15/March April.toml", "2011/03/15/March April")
      place(session, "2011 March-April Trip", "2011/04/02/March April Trip.toml", "2011/04/02/March April Trip")
      place(session, "2009 July", "2009/07/04/album.toml", "2009/07/04")
      Album.read(archive, File(archive, "2009/07/04/album.toml")).include.isEmpty shouldBe true
      val (_, dec) = exec(session, "agent", "album-file", "2009 December")
      dec match
        case Result.Ok(text, value) =>
          noIds(text, "albDec", "keyDec")
          field(value, "pathTitle") shouldBe Json.Str("2009/12/25")
        case other =>
          fail(other.toString)
      val december: Album = Album.read(archive, File(archive, "2009/12/25/album.toml"))
      december.title shouldBe "2009/12/25"
      december.include.map(Include.render) shouldBe Seq("all")
      december.external shouldBe true
      val (_, span) = exec(session, "agent", "album-file", "2010 February Purim")
      span match
        case Result.Ok(text, _) =>
          noIds(text, "albSpan", "keyA", "keyB")
        case other =>
          fail(other.toString)
      val ranged: Album = Album.read(archive, File(archive, "2010/02/03/to-05 Purim.toml"))
      ranged.title shouldBe "2010/02/03-05/Purim"
      File(archive, "2010/02/05").exists shouldBe false
      requests(archive).exists shouldBe false
      partials(archive) shouldBe Seq.empty[String]

  it should "keep a day number that is not an item day and write no file" in :
    withArchive: archive =>
      writeCatalog(
        archive,
        "album\talbKis\t1\t19 Kislev",
        "item\talbKis\tpic.jpg\t2010-01-05\tkeyKis\t-",
        "album\talbGold\t1\t2019 Feb 3 Bar Mitzvah Gold",
        "item\talbGold\tgold.jpg\t2019-02-04\tkeyGold\t-"
      )
      val session: Session = sessionFor(archive)
      val kislev: String = usage(session, "agent", "album-file", "19 Kislev")
      kislev shouldBe "leftover starts with a digit"
      noIds(kislev, "albKis", "keyKis")
      val kept: String = usage(session, "agent", "album-file", "2019 Feb 3 Bar Mitzvah Gold")
      kept shouldBe "leftover starts with a digit"
      noIds(kept, "albGold", "keyGold")
      tomls(archive) shouldBe Seq.empty[String]
      File(archive, "2010").exists shouldBe false
      File(archive, "2019").exists shouldBe false
      partials(archive) shouldBe Seq.empty[String]
      requests(archive).exists shouldBe false

  it should "keep a parsed slash date as a day file and a parsed name" in :
    withArchive: archive =>
      mkdir(archive, "2010/07/19")
      mkdir(archive, "2010/07")
      mkdir(archive, "2010/08/01")
      writeCatalog(
        archive,
        "album\talbDay\t1\t2010/07/19",
        "item\talbDay\tpic.jpg\t2010-07-19\tkeyDay\t-",
        "album\talbName\t1\t2010/07/19/Purim",
        "item\talbName\tpic.jpg\t2010-07-19\tkeyName\t-",
        "album\talbPic\t1\t2010/07/19 Picnic",
        "item\talbPic\tpic.jpg\t2010-07-19\tkeyPic\t-",
        "album\talbExtra\t1\t2010/07/19/Purim Extra",
        "item\talbExtra\tpic.jpg\t2010-07-19\tkeyExtra\t-",
        "album\talbShore\t0\t2010/08/01 Shore",
        "album\talbSpan\t0\t2010/07-08"
      )
      val session: Session = sessionFor(archive)
      place(session, "2010/07/19", "2010/07/19/album.toml", "2010/07/19")
      place(session, "2010/07/19/Purim", "2010/07/19/Purim.toml", "2010/07/19/Purim")
      place(session, "2010/07/19 Picnic", "2010/07/19/Picnic.toml", "2010/07/19/Picnic")
      place(session, "2010/07/19/Purim Extra", "2010/07/19/Purim Extra.toml", "2010/07/19/Purim Extra")
      place(session, "2010/08/01 Shore", "2010/08/01/Shore.toml", "2010/08/01/Shore")
      File(archive, "Picnic.toml").exists shouldBe false
      place(session, "2010/07-08", "2010/07/to-08.toml", "2010/07-08")
      requests(archive).exists shouldBe false
      partials(archive) shouldBe Seq.empty[String]

  it should "strip a slash-date leftover and reject a name Album.read would misread" in :
    withArchive: archive =>
      mkdir(archive, "2010/07/19")
      writeCatalog(
        archive,
        "album\talbTo20\t1\t2010/07/19 to-20",
        "item\talbTo20\tpic.jpg\t2010-07-19\tkeyTo20\t-",
        "album\talbWord\t1\t2010/07/19 album",
        "item\talbWord\tpic.jpg\t2010-07-19\tkeyWord\t-",
        "album\talbMonthWord\t1\t2010/07/19 July",
        "item\talbMonthWord\tpic.jpg\t2010-07-19\tkeyMonthWord\t-",
        "album\talbDayPicnic\t1\t2010/07/19 19 Picnic",
        "item\talbDayPicnic\tpic.jpg\t2010-07-19\tkeyDayPicnic\t-"
      )
      val session: Session = sessionFor(archive)
      val to20: String = usage(session, "agent", "album-file", "2010/07/19 to-20")
      to20 shouldBe "leftover is not a name"
      noIds(to20, "albTo20", "keyTo20")
      val word: String = usage(session, "agent", "album-file", "2010/07/19 album")
      word shouldBe "leftover is not a name"
      noIds(word, "albWord", "keyWord")
      File(archive, "2010/07/19/to-20.toml").exists shouldBe false
      File(archive, "2010/07/19/album.toml").exists shouldBe false
      partials(archive) shouldBe Seq.empty[String]
      val (_, julyResult) = exec(session, "agent", "album-file", "2010/07/19 July")
      julyResult match
        case Result.Ok(text, value) =>
          noIds(text, "albMonthWord", "keyMonthWord")
          field(value, "path") shouldBe Json.Str("2010/07/19/album.toml")
          field(value, "pathTitle") shouldBe Json.Str("2010/07/19")
        case other =>
          fail(other.toString)
      val july: Album = Album.read(archive, File(archive, "2010/07/19/album.toml"))
      val julyParsed: Title.Parsed = Title.parse(july.title).getOrElse(fail(july.title))
      july.span.map(Title.renderSpan) shouldBe Some("2010/07/19")
      julyParsed.span.map(Title.renderSpan) shouldBe Some("2010/07/19")
      julyParsed.name.isEmpty shouldBe true
      val (_, picnicResult) = exec(session, "agent", "album-file", "2010/07/19 19 Picnic")
      picnicResult match
        case Result.Ok(text, value) =>
          noIds(text, "albDayPicnic", "keyDayPicnic")
          field(value, "path") shouldBe Json.Str("2010/07/19/Picnic.toml")
          field(value, "pathTitle") shouldBe Json.Str("2010/07/19/Picnic")
        case other =>
          fail(other.toString)
      val picnic: Album = Album.read(archive, File(archive, "2010/07/19/Picnic.toml"))
      val picnicParsed: Title.Parsed = Title.parse(picnic.title).getOrElse(fail(picnic.title))
      picnic.span.map(Title.renderSpan) shouldBe Some("2010/07/19")
      picnicParsed.span.map(Title.renderSpan) shouldBe Some("2010/07/19")
      picnicParsed.name shouldBe Some("Picnic")
      partials(archive) shouldBe Seq.empty[String]

  it should "use all for a month inside the cap and per-day lines when a pictured day has no match" in :
    withArchive: archive =>
      touch(File(archive, "2010/07/01"), "2412.jpg")
      touch(File(archive, "2010/07/01"), "9999.jpg")
      touch(File(archive, "2010/07/02"), "2413.jpg")
      touch(File(archive, "2010/08/01"), "2412.jpg")
      touch(File(archive, "2010/08/02"), "9998.jpg")
      writeCatalog(
        archive,
        "album\talbJul\t2\t2010/07",
        "item\talbJul\t2412.jpg\t2010-07-01\tkeyJul\t-",
        "item\talbJul\t2413.jpg\t2010-07-02\tkeyAug\t-",
        "album\talbAug\t1\t2010/08",
        "item\talbAug\t2412.jpg\t2010-08-01\tkeyAugDay\t-"
      )
      val session: Session = sessionFor(archive)
      val (_, passed) = exec(session, "agent", "album-file", "2010/07")
      passed match
        case Result.Ok(text, _) =>
          noIds(text, "albJul", "keyJul", "keyAug")
          text should not include ("skipped")
        case other =>
          fail(other.toString)
      val july: String = JFiles.readString(File(archive, "2010/07/album.toml").toPath)
      july should include ("'all'")
      july should not include ("9999")
      Album.read(archive, File(archive, "2010/07/album.toml")).span.map(Title.renderSpan) shouldBe Some("2010/07")
      val (_, failed) = exec(session, "agent", "album-file", "2010/08")
      failed match
        case Result.Ok(text, _) => noIds(text, "albAug", "keyAugDay")
        case other => fail(other.toString)
      val august: Album = Album.read(archive, File(archive, "2010/08/album.toml"))
      august.title shouldBe "2010/08"
      august.span.map(Title.renderSpan) shouldBe Some("2010/08")
      august.include.map(Include.render) shouldBe Seq("2010/08/01 2412")
      august.include.map(Include.render).foreach(line => line.startsWith("all") shouldBe false)
      requests(archive).exists shouldBe false

  it should "refuse a matched day outside a month title" in :
    withArchive: archive =>
      mkdir(archive, "2010/07")
      touch(File(archive, "2010/08/02"), "2412.jpg")
      writeCatalog(
        archive,
        "album\talbJul\t1\t2010/07",
        "item\talbJul\t2412.jpg\t2010-08-02\tkeyOut\t-"
      )
      val session: Session = sessionFor(archive)
      val message: String = usage(session, "agent", "album-file", "2010/07")
      message shouldBe "matched day outside the span"
      noIds(message, "albJul", "keyOut")
      File(archive, "2010/07/album.toml").exists shouldBe false
      requests(archive).exists shouldBe false
      partials(archive) shouldBe Seq.empty[String]

  it should "reject an include that does not expand to the matched set" in :
    val day: LocalDate = LocalDate.of(2010, 7, 1)
    val span: Title.Span = Title.Span(day, day, Title.Grain.Day)
    val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
      AgentAlbumFile.requireInclude(
        monthPassed = false,
        span = Some(span),
        include = Seq.empty,
        claimed = Set(day -> "2412"),
        universe = Map(day -> Set("2412"))
      )
    caught.getMessage shouldBe "include does not match"

  it should "skip auto-add and unread names without a file or a request" in :
    withArchive: archive =>
      val day: File = File(archive, "2010/07/19")
      touch(day, "2412.jpg")
      writeCatalog(
        archive,
        "album\talbAdd\t0\tGamma",
        "auto-add\talbAdd",
        "album\talbUnread\t1\tEpsilon",
        "error\talbUnread\t1\t1\t1",
        "unset\talbUnread\t2010-07-19\tkeyUnread\t-",
        "album\talbShort\t4\t2010/09",
        "error\talbShort\t1\t4\t0",
        "item\talbShort\tone.jpg\t2010-09-01\tkeyShort\t-"
      )
      mkdir(archive, "2010/09")
      val session: Session = sessionFor(archive)
      val (skippedAdd, add) = exec(session, "agent", "album-file", "Gamma")
      add shouldBe Result.Ok("skipped auto-add\n", AgentSupport.skipped(session, Catalog.Album(
        "albAdd", 0, "Gamma", None, true, None, Seq.empty, Seq.empty
      ), "auto-add")._2 match
        case Result.Ok(_, value) => value
        case other => fail(other.toString)
      )
      add match
        case Result.Ok(text, _) => noIds(text, "albAdd")
        case other => fail(other.toString)
      skippedAdd.cached(day).isEmpty shouldBe true
      val (skippedNames, unread) = exec(skippedAdd, "agent", "album-file", "Epsilon")
      unread match
        case Result.Ok(text, _) =>
          text shouldBe "skipped unread names\n"
          noIds(text, "albUnread", "keyUnread")
        case other =>
          fail(other.toString)
      skippedNames.cached(day).isEmpty shouldBe true
      val (_, short) = exec(skippedNames, "agent", "album-file", "2010/09")
      short match
        case Result.Ok(text, _) =>
          text should not include ("skipped")
          noIds(text, "albShort", "keyShort")
        case other =>
          fail(other.toString)
      File(archive, "2010/09/album.toml").isFile shouldBe true
      Album.read(archive, File(archive, "2010/09/album.toml")).include.isEmpty shouldBe true
      File(archive, "Gamma.toml").exists shouldBe false
      File(archive, "Epsilon.toml").exists shouldBe false
      requests(archive).exists shouldBe false
      partials(archive) shouldBe Seq.empty[String]

  it should "refuse an existing file unless replace, and a missing start directory" in :
    withArchive: archive =>
      JFiles.writeString(File(archive, "Best Of.toml").toPath, "keep-root\n")
      mkdir(archive, "2010/07")
      JFiles.writeString(File(archive, "2010/07/album.toml").toPath, "keep-month\n")
      mkdir(archive, "2010/02/28")
      JFiles.writeString(File(archive, "2010/02/28/album.toml").toPath, "keep-day\n")
      writeCatalog(
        archive,
        "album\talbRoot\t0\tBest Of",
        "album\talbMonth\t0\t2010/07",
        "album\talbDay\t1\t2010 February",
        "item\talbDay\tpic.jpg\t2010-02-28\tkeyDay\t-",
        "album\talbMiss\t0\t2013/04"
      )
      val session: Session = sessionFor(archive)
      val root: String = usage(session, "agent", "album-file", "Best Of")
      root shouldBe "file exists: Best Of.toml"
      noIds(root, "albRoot")
      JFiles.readString(File(archive, "Best Of.toml").toPath) shouldBe "keep-root\n"
      val month: String = usage(session, "agent", "album-file", "2010/07")
      month shouldBe "file exists: 2010/07/album.toml"
      noIds(month, "albMonth")
      JFiles.readString(File(archive, "2010/07/album.toml").toPath) shouldBe "keep-month\n"
      val day: String = usage(session, "agent", "album-file", "2010 February")
      day shouldBe "file exists: 2010/02/28/album.toml"
      noIds(day, "albDay", "keyDay")
      JFiles.readString(File(archive, "2010/02/28/album.toml").toPath) shouldBe "keep-day\n"
      val missing: String = usage(session, "agent", "album-file", "2013/04")
      missing shouldBe "no such directory"
      noIds(missing, "albMiss")
      File(archive, "2013").exists shouldBe false
      partials(archive) shouldBe Seq.empty[String]
      requests(archive).exists shouldBe false

  it should "replace an album file and keep the Google id and published title" in :
    withArchive: archive =>
      mkdir(archive, "2010/07")
      val dest: File = File(archive, "2010/07/album.toml")
      JFiles.writeString(
        dest.toPath,
        """external = false
          |google-album-id = "gid-keep"
          |published-title = "Published Keep"
          |google-title = "Old Title"
          |include = [
          |  "2010/07/01 9999",
          |]
          |""".stripMargin
      )
      writeCatalog(archive, "album\talbRep\t0\t2010/07")
      val (_, result) = exec(sessionFor(archive), "agent", "album-file", "--replace", "2010/07")
      result match
        case Result.Ok(text, value) =>
          noIds(text, "albRep")
          text should include ("rename no")
          text should not include ("appended")
          field(value, "albumId") shouldBe Json.Str("albRep")
          field(value, "requestId") shouldBe Json.Null
        case other =>
          fail(other.toString)
      val read: Album = Album.read(archive, dest)
      read.googleAlbumId shouldBe "gid-keep"
      read.publishedTitle shouldBe "Published Keep"
      read.googleTitle shouldBe ""
      read.include.isEmpty shouldBe true
      val text: String = JFiles.readString(dest.toPath)
      text should not include ("9999")
      text should not include ("Old Title")
      text shouldBe Album.renderFile(read)
      partials(archive) shouldBe Seq.empty[String]

  it should "replace an owner album without a google album id" in :
    withArchive: archive =>
      val dest: File = File(archive, "Friends.toml")
      JFiles.writeString(
        dest.toPath,
        """external = false
          |google-album-id = "gid-own"
          |published-title = "Published Own"
          |google-title = "Old"
          |""".stripMargin
      )
      writeCatalog(
        archive,
        "album\talbOwn\t0\tFriends ",
        "owner\talbOwn\tAda"
      )
      val (_, result) = exec(sessionFor(archive), "agent", "album-file", "--replace", "Friends ")
      result match
        case Result.Ok(text, _) =>
          text should include ("rename no")
          noIds(text, "albOwn")
        case other =>
          fail(other.toString)
      val read: Album = Album.read(archive, dest)
      read.owner shouldBe Some("Ada")
      read.googleTitle shouldBe "Friends "
      read.googleAlbumId shouldBe ""
      read.publishedTitle shouldBe ""
      read.external shouldBe true
      val text: String = JFiles.readString(dest.toPath)
      text should not include ("gid-own")
      text shouldBe Album.renderFile(read)

  it should "leave a file Album.read rejects in place" in :
    withArchive: archive =>
      mkdir(archive, "2010/07")
      val dest: File = File(archive, "2010/07/album.toml")
      val body: String = "mystery = true\n"
      JFiles.writeString(dest.toPath, body)
      writeCatalog(archive, "album\talbRep\t0\t2010/07")
      val session: Session = sessionFor(archive)
      val seen: File = File(session.root.configuration.archive, "2010/07/album.toml")
      val message: String = usage(session, "agent", "album-file", "--replace", "2010/07")
      message shouldBe s"$seen: unknown key 'mystery'"
      noIds(message, "albRep")
      JFiles.readString(dest.toPath) shouldBe body
      partials(archive) shouldBe Seq.empty[String]

  it should "store an owner and an exact google-title without a rename row" in :
    withArchive: archive =>
      writeCatalog(
        archive,
        "album\talbOwn\t0\tFriends ",
        "owner\talbOwn\tAda"
      )
      val session: Session = sessionFor(archive)
      val mismatch: String = usage(session, "agent", "album-file", "--google-title", "Other", "Friends ")
      mismatch shouldBe "google-title does not match \"Friends \" \"Other\""
      noIds(mismatch, "albOwn")
      File(archive, "Friends.toml").exists shouldBe false
      val (_, equal) = exec(session, "agent", "album-file", "--google-title", "Friends ", "Friends ")
      equal match
        case Result.Ok(text, value) =>
          text shouldBe
            """wrote Friends.toml
              |path-title Friends
              |catalog-title "Friends "
              |rename no
              |""".stripMargin
          noIds(text, "albOwn")
          field(value, "rename") shouldBe Json.Bool(false)
          field(value, "requestId") shouldBe Json.Null
          field(value, "albumId") shouldBe Json.Str("albOwn")
        case other =>
          fail(other.toString)
      JFiles.readString(File(archive, "Friends.toml").toPath) shouldBe
        """google-title = "Friends "
          |owner = "Ada"
          |""".stripMargin
      requests(archive).exists shouldBe false
      val replaced: String = JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath)
      JFiles.writeString(
        File(archive, ".gphoto-sync/catalog.txt").toPath,
        replaced.replace("Friends ", "Friends ")
      )
      // Owner with no flag still stores the exact catalog title.
      JFiles.delete(File(archive, "Friends.toml").toPath)
      val (_, plain) = exec(sessionFor(archive), "agent", "album-file", "Friends ")
      plain match
        case Result.Ok(text, _) =>
          text should include ("rename no")
          text should not include ("appended")
          noIds(text, "albOwn")
        case other =>
          fail(other.toString)
      JFiles.readString(File(archive, "Friends.toml").toPath) should include ("google-title = \"Friends \"")
      requests(archive).exists shouldBe false

  it should "print rename yes when the catalog title differs and append nothing" in :
    withArchive: archive =>
      mkdir(archive, "2010/07")
      writeCatalog(archive, "album\talb07\t0\t2010/07 ")
      val dir: File = File(archive, "2010/07")
      val primed: Session = sessionFor(archive).remember(dir, DirEntry(Vector.empty, Seq.empty, Seq.empty, None))
      val (drySession, dry) = exec(primed, "agent", "album-file", "--dry-run", "2010/07 ")
      dry match
        case Result.Ok(text, value) =>
          text shouldBe
            """rename yes
              |""".stripMargin
          noIds(text, "alb07")
          field(value, "path") shouldBe Json.Str("2010/07/album.toml")
          field(value, "pathTitle") shouldBe Json.Str("2010/07")
          field(value, "catalogTitle") shouldBe Json.Str("2010/07 ")
          field(value, "rename") shouldBe Json.Bool(true)
          field(value, "requestId") shouldBe Json.Null
          field(value, "albumId") shouldBe Json.Str("alb07")
        case other =>
          fail(other.toString)
      drySession.cached(dir).isDefined shouldBe true
      File(dir, "album.toml").exists shouldBe false
      requests(archive).exists shouldBe false
      val (quiet, suppressed) = exec(drySession, "agent", "album-file", "2010/07 ")
      suppressed match
        case Result.Ok(text, value) =>
          text shouldBe
            """wrote 2010/07/album.toml
              |path-title 2010/07
              |catalog-title "2010/07 "
              |rename yes
              |""".stripMargin
          noIds(text, "alb07")
          field(value, "requestId") shouldBe Json.Null
        case other =>
          fail(other.toString)
      quiet.cached(dir).isEmpty shouldBe true
      requests(archive).exists shouldBe false
      JFiles.delete(File(dir, "album.toml").toPath)
      val again: Session = sessionFor(archive).remember(dir, DirEntry(Vector.empty, Seq.empty, Seq.empty, None))
      val (written, result) = exec(again, "agent", "album-file", "2010/07 ")
      result match
        case Result.Ok(text, value) =>
          text shouldBe
            """wrote 2010/07/album.toml
              |path-title 2010/07
              |catalog-title "2010/07 "
              |rename yes
              |""".stripMargin
          noIds(text, "alb07")
          field(value, "requestId") shouldBe Json.Null
        case other =>
          fail(other.toString)
      written.cached(dir).isEmpty shouldBe true
      requests(archive).exists shouldBe false
      partials(archive) shouldBe Seq.empty[String]

  it should "collapse working-title spaces only on the path title" in :
    withArchive: archive =>
      writeCatalog(archive, "album\talbBest\t0\tBest  Of")
      val (_, result) = exec(sessionFor(archive), "agent", "album-file", "Best  Of")
      result match
        case Result.Ok(text, _) =>
          text should include ("path-title Best Of")
          text should include ("catalog-title \"Best  Of\"")
          text should include ("rename yes")
          text should not include ("appended")
          noIds(text, "albBest")
        case other =>
          fail(other.toString)
      Album.read(archive, File(archive, "Best Of.toml")).title shouldBe "Best Of"
      requests(archive).exists shouldBe false

  it should "keep a legacy requests file when the album file is written" in :
    withArchive: archive =>
      mkdir(archive, "2010/07")
      writeCatalog(archive, "album\talb07\t0\t2010/07 ")
      val bad: String = "nope\n"
      JFiles.writeString(requests(archive).toPath, bad)
      val dir: File = File(archive, "2010/07")
      val primed: Session = sessionFor(archive).remember(dir, DirEntry(Vector.empty, Seq.empty, Seq.empty, None))
      val (returned, result) = exec(primed, "agent", "album-file", "2010/07 ")
      result match
        case Result.Ok(text, value) =>
          text should include ("wrote 2010/07/album.toml")
          text should include ("rename yes")
          text should not include ("append failed")
          text should not include ("appended")
          noIds(text, "alb07")
          field(value, "requestId") shouldBe Json.Null
        case other =>
          fail(other.toString)
      returned.cached(dir).isEmpty shouldBe true
      File(dir, "album.toml").isFile shouldBe true
      JFiles.readString(requests(archive).toPath) shouldBe bad
      partials(archive) shouldBe Seq.empty[String]

  it should "not create a partial when the catalog check fails" in :
    withArchive: archive =>
      val message: String = usage(sessionFor(archive), "agent", "album-file", "2010/07")
      message shouldBe "catalog is absent"
      File(archive, ".gphoto-sync").exists shouldBe false
      val dot: File = File(archive, ".gphoto-sync")
      dot.mkdirs()
      JFiles.writeString(File(dot, "catalog.txt").toPath, "## \n")
      val bad: String = usage(sessionFor(archive), "agent", "album-file", "2010/07")
      bad shouldBe "catalog is not version 2"
      partials(archive) shouldBe Seq.empty[String]

  it should "reject a request that does not match the catalog" in :
    withArchive: archive =>
      writeFixture(archive)
      val session: Session = sessionFor(archive)
      def reject(message: String, args: String*): Unit =
        val text: String = usage(session, args*)
        withClue(args):
          text shouldBe message
          noIds(text, fixtureIds*)
          requests(archive).exists shouldBe false
      val previousAct = Refresh.openAct
      var opened = false
      Refresh.openAct = _ =>
        opened = true
        throw IllegalStateException("opened")
      try
        reject(
          "no album titled \"Missing\"",
          "agent", "rename-album", "--title", "Missing", "--current", "Missing", "--new", "July"
        )
        reject(
          "catalog title does not match",
          "agent", "rename-album", "--title", "2010/07 ", "--current", "2010/07 ", "--new", "July", "--id", "goneId"
        )
        reject(
          "album has an owner",
          "agent", "rename-album", "--title", "Owned Name", "--current", "Owned Name", "--new", "Next"
        )
        reject(
          "current does not match \"2010/07 \" \"2010/07\"",
          "agent", "rename-album", "--title", "2010/07 ", "--current", "2010/07", "--new", "July"
        )
        reject(
          "new title is empty",
          "agent", "rename-album", "--title", "2010/07 ", "--current", "2010/07 ", "--new", ""
        )
        reject(
          "new title is unchanged",
          "agent", "rename-album", "--title", "2010/07 ", "--current", "2010/07 ", "--new", "2010/07 "
        )
        reject("filename is not valid", "agent", "download-item", "--title", "Bad Names", "--item", "keyDot", "--current", ".")
        reject("filename is not valid", "agent", "download-item", "--title", "Bad Names", "--item", "keyDots", "--current", "..")
        reject(
          "filename is not valid",
          "agent", "download-item", "--title", "Bad Names", "--item", "keySlash", "--current", "a/b.jpg"
        )
        opened shouldBe false
      finally
        Refresh.openAct = previousAct
      requests(archive).exists shouldBe false
      reject(
        "no such item",
        "agent", "set-description", "--item", "goneId", "--current", "pic.jpg", "--new", "Hello"
      )
      reject(
        "no such item",
        "agent", "set-description", "--item", "goneId", "--current", "pic.jpg", "--new", "a\tb"
      )
      reject(
        "no such item",
        "agent", "set-description", "--item", "keyUnset", "--current", "pic.jpg", "--new", "Hello"
      )
      reject(
        "item key is not unique",
        "agent", "set-description", "--item", "keyShare", "--current", "pic.jpg", "--new", "Hello"
      )
      reject(
        "current does not match \"pic.jpg\" \"other.jpg\"",
        "agent", "set-description", "--item", "keyOnly", "--current", "other.jpg", "--new", "Hello"
      )
      reject(
        "new contains a tab or a newline",
        "agent", "set-description", "--item", "keyOnly", "--current", "pic.jpg", "--new", "a\tb"
      )
      reject(
        "new contains a tab or a newline",
        "agent", "set-description", "--item", "keyOnly", "--current", "pic.jpg", "--new", "a\nb"
      )
      reject(
        "new contains a tab or a newline",
        "agent", "set-description", "--item", "keyOnly", "--current", "pic.jpg", "--new", "a\rb"
      )
      reject("date is not valid", "agent", "set-date", "--item", "keyOnly", "--current", "pic.jpg", "--new", "2010-07-19 24:00")
      reject("date is not valid", "agent", "set-date", "--item", "keyOnly", "--current", "pic.jpg", "--new", "2010-07-19")
      reject("date is not valid", "agent", "set-date", "--item", "keyOnly", "--current", "pic.jpg", "--new", "2010-07-19 00:60")
      reject("date is not valid", "agent", "set-date", "--item", "keyOnly", "--current", "pic.jpg", "--new", "2010-02-31 09:05")
      reject("date is not valid", "agent", "set-date", "--item", "keyOnly", "--current", "pic.jpg", "--new", "2010-13-01 09:05")
      val download: Catalog.Album = Catalog.Album(
        "alb1", 1, "Title", None, false, None,
        Seq(Catalog.Item("pic.jpg", Some(LocalDate.of(2010, 7, 1)), "key1", None)),
        Seq.empty
      )
      intercept[IllegalArgumentException]:
        AgentChrome.acceptRequest(Seq(download), AgentChrome.Action.DownloadItem, "alb1", "key1", "pic.jpg", "nope")
      .getMessage shouldBe "new is not empty"
      intercept[IllegalArgumentException]:
        AgentChrome.acceptRequest(Seq(download), AgentChrome.Action.DeleteItem, "alb1", "key1", "pic.jpg", "nope")
      .getMessage shouldBe "new is not empty"
      intercept[IllegalArgumentException]:
        AgentChrome.acceptRequest(Seq(download), AgentChrome.Action.RenameAlbum, "gone", "", "Title", "Next")
      .getMessage shouldBe "no such album"

  it should "report a missing request flag before it reads the catalog" in :
    withArchive: archive =>
      val session: Session = sessionFor(archive)
      usage(session, "agent", "rename-album") shouldBe "missing title"
      usage(session, "agent", "rename-album", "--title", "T", "--new", "X") shouldBe "missing current"
      usage(session, "agent", "rename-album", "--title", "T", "--current", "Old") shouldBe "missing new"
      usage(session, "agent", "set-description") shouldBe "missing item"
      usage(session, "agent", "set-description", "--item", "keyOnly") shouldBe "missing current"
      usage(session, "agent", "set-date", "--item", "keyOnly", "--current", "pic.jpg") shouldBe "missing new"
      usage(session, "agent", "download-item") shouldBe "missing title"
      usage(session, "agent", "download-item", "--title", "Bad Names") shouldBe "missing item"
      usage(session, "agent", "download-item", "--title", "Bad Names", "--item", "keyDot") shouldBe "missing current"
      usage(session, "agent", "add-to-album") shouldBe "missing title"
      usage(session, "agent", "add-to-album", "--title", "Source Title") shouldBe "missing item"
      usage(session, "agent", "add-to-album", "--title", "Source Title", "--item", "keyPic") shouldBe "missing current"
      usage(session, "agent", "add-to-album", "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg") shouldBe
        "missing dest"
      usage(session, "agent", "remove-from-album") shouldBe "missing title"
      usage(session, "agent", "remove-from-album", "--title", "Source Title") shouldBe "missing item"
      usage(session, "agent", "remove-from-album", "--title", "Source Title", "--item", "keyPic") shouldBe "missing current"
      usage(session, "agent", "move-item") shouldBe "missing title"
      usage(session, "agent", "move-item", "--title", "Source Title") shouldBe "missing item"
      usage(session, "agent", "move-item", "--title", "Source Title", "--item", "keyPic") shouldBe "missing current"
      usage(session, "agent", "move-item", "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg") shouldBe
        "missing dest"
      usage(session, "agent", "delete-item") shouldBe "missing title"
      usage(session, "agent", "delete-item", "--title", "Source Title") shouldBe "missing item"
      usage(session, "agent", "delete-item", "--title", "Source Title", "--item", "keyPic") shouldBe "missing current"
      File(archive, ".gphoto-sync").exists shouldBe false


  private val fixtureIds: Seq[String] = Seq(
    "albTrail", "albOwn", "albSrc", "albDst", "albUp", "albShareA", "albShareB", "albOnly", "albBad", "albUnset",
    "keyOwn", "keyPic", "keyUp", "keyShare", "keyOnly", "keyDot", "keyDots", "keySlash", "keyUnset", "goneId"
  )


  private def place(session: Session, title: String, path: String, pathTitle: String): Unit =
    val (_, result) = exec(session, "agent", "album-file", title)
    result match
      case Result.Ok(text, value) =>
        field(value, "path") shouldBe Json.Str(path)
        field(value, "pathTitle") shouldBe Json.Str(pathTitle)
        text should include (s"wrote $path")
      case other =>
        fail(other.toString)
    val file: File = File(session.root.configuration.archive, path)
    val read: Album = Album.read(session.root.configuration.archive, file)
    read.title shouldBe pathTitle
    read.external shouldBe true


  // skipped's value is what match returns. Build the same album the catalog would.
  private def noIds(text: String, ids: String*): Unit =
    ids.foreach: id =>
      withClue(id):
        text should not include id


  private def mkdir(archive: File, path: String): Unit =
    File(archive, path).mkdirs()


  private def partials(archive: File): Seq[String] =
    val dot: File = File(archive, ".gphoto-sync")
    if !dot.isDirectory then Seq.empty
    else Option(dot.list).getOrElse(Array.empty[String]).toSeq.filter(_.startsWith("album-write-"))
