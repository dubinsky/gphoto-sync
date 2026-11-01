package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.io.File
import java.nio.file.{Files as JFiles}
import java.time.{LocalDate, ZoneId}

class AgentWriteTest extends AnyFlatSpec, Matchers:
  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")

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
      val (_, dec) = exec(session, "agent", "album-file", "--no-request", "2009 December")
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
      val (_, span) = exec(session, "agent", "album-file", "--no-request", "2010 February Purim")
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
      val (_, julyResult) = exec(session, "agent", "album-file", "--no-request", "2010/07/19 July")
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
      val (_, picnicResult) = exec(session, "agent", "album-file", "--no-request", "2010/07/19 19 Picnic")
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
      val (_, passed) = exec(session, "agent", "album-file", "--no-request", "2010/07")
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
      val (_, failed) = exec(session, "agent", "album-file", "--no-request", "2010/08")
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
      AgentCommands.requireInclude(
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
      add shouldBe Result.Ok("skipped auto-add\n", AgentCommands.skipped(session, Catalog.Album(
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
      val (_, short) = exec(skippedNames, "agent", "album-file", "--no-request", "2010/09")
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
        """external = true
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
        """external = true
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
        """external = true
          |google-title = "Friends "
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

  it should "append rename-album when the catalog title differs from the path title" in :
    withArchive: archive =>
      mkdir(archive, "2010/07")
      writeCatalog(archive, "album\talb07\t0\t2010/07 ")
      val dir: File = File(archive, "2010/07")
      val primed: Session = sessionFor(archive).remember(dir, DirEntry(Vector.empty, Seq.empty, Seq.empty, None))
      val (drySession, dry) = exec(primed, "agent", "album-file", "--dry-run", "2010/07 ")
      dry match
        case Result.Ok(text, value) =>
          text shouldBe
            """external = true
              |rename yes
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
      val (quiet, suppressed) = exec(drySession, "agent", "album-file", "--no-request", "2010/07 ")
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
              |appended r1 rename-album
              |""".stripMargin
          noIds(text, "alb07")
          field(value, "requestId") shouldBe Json.Str("r1")
        case other =>
          fail(other.toString)
      written.cached(dir).isEmpty shouldBe true
      JFiles.readString(requests(archive).toPath) shouldBe
        "# gphoto-requests 2\nr1\trename-album\talb07\t\t2010/07 \t2010/07\n"
      val (_, listed) = exec(written, "agent", "requests")
      listed match
        case Result.Ok(text, Json.Arr(Seq(row))) =>
          text shouldBe "r1 rename-album \"2010/07 \" \"2010/07\"\n"
          noIds(text, "alb07")
          field(row, "albumId") shouldBe Json.Str("alb07")
          field(row, "current") shouldBe Json.Str("2010/07 ")
          field(row, "new") shouldBe Json.Str("2010/07")
        case other =>
          fail(other.toString)
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
          text should include ("appended r1 rename-album")
          noIds(text, "albBest")
        case other =>
          fail(other.toString)
      Album.read(archive, File(archive, "Best Of.toml")).title shouldBe "Best Of"
      JFiles.readString(requests(archive).toPath) should include ("Best  Of\tBest Of\n")

  it should "keep the album file when the rename append fails" in :
    withArchive: archive =>
      mkdir(archive, "2010/07")
      writeCatalog(archive, "album\talb07\t0\t2010/07 ")
      val bad: String = "nope\n"
      JFiles.writeString(requests(archive).toPath, bad)
      val dir: File = File(archive, "2010/07")
      val primed: Session = sessionFor(archive).remember(dir, DirEntry(Vector.empty, Seq.empty, Seq.empty, None))
      val (returned, result) = exec(primed, "agent", "album-file", "2010/07 ")
      result match
        case Result.Failed(text, _, message) =>
          text should include ("wrote 2010/07/album.toml")
          text should include ("append failed")
          text should not include ("appended")
          noIds(text, "alb07")
          message shouldBe "requests are not version 2"
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
      reject("no such album", "agent", "request", "rename-album", "--album", "goneId", "--current", "2010/07 ", "--new", "July")
      reject(
        "album has an owner",
        "agent", "request", "rename-album", "--album", "albOwn", "--current", "Owned Name", "--new", "Next"
      )
      reject(
        "current does not match \"2010/07 \" \"2010/07\"",
        "agent", "request", "rename-album", "--album", "albTrail", "--current", "2010/07", "--new", "July"
      )
      reject(
        "new title is empty",
        "agent", "request", "rename-album", "--album", "albTrail", "--current", "2010/07 ", "--new", ""
      )
      reject(
        "new title is unchanged",
        "agent", "request", "rename-album", "--album", "albTrail", "--current", "2010/07 ", "--new", "2010/07 "
      )
      reject("no such album", "agent", "request", "move-item", "--album", "goneId", "--item", "keyPic", "--current", "pic.jpg", "--new", "albDst")
      reject(
        "no such destination",
        "agent", "request", "move-item", "--album", "albSrc", "--item", "keyPic", "--current", "pic.jpg", "--new", "goneId"
      )
      reject(
        "no such item",
        "agent", "request", "move-item", "--album", "albSrc", "--item", "keyUnset", "--current", "pic.jpg", "--new", "albDst"
      )
      reject(
        "current does not match \"pic.jpg\" \"other.jpg\"",
        "agent", "request", "move-item", "--album", "albSrc", "--item", "keyPic", "--current", "other.jpg", "--new", "albDst"
      )
      reject(
        "destination is the source album",
        "agent", "request", "move-item", "--album", "albSrc", "--item", "keyPic", "--current", "pic.jpg", "--new", "albSrc"
      )
      reject("no such item", "agent", "request", "set-description", "--item", "goneId", "--current", "pic.jpg")
      reject("no such item", "agent", "request", "set-description", "--item", "keyUnset", "--current", "pic.jpg")
      reject(
        "item key is not unique",
        "agent", "request", "set-description", "--item", "keyShare", "--current", "pic.jpg"
      )
      reject(
        "current does not match \"pic.jpg\" \"other.jpg\"",
        "agent", "request", "set-description", "--item", "keyOnly", "--current", "other.jpg"
      )
      reject("date is not valid", "agent", "request", "set-date", "--item", "keyOnly", "--current", "pic.jpg", "--new", "2010-07-19 24:00")
      reject("date is not valid", "agent", "request", "set-date", "--item", "keyOnly", "--current", "pic.jpg", "--new", "2010-07-19")
      reject("date is not valid", "agent", "request", "set-date", "--item", "keyOnly", "--current", "pic.jpg", "--new", "2010-07-19 00:60")
      reject("date is not valid", "agent", "request", "set-date", "--item", "keyOnly", "--current", "pic.jpg", "--new", "2010-02-31 09:05")
      reject("date is not valid", "agent", "request", "set-date", "--item", "keyOnly", "--current", "pic.jpg", "--new", "2010-13-01 09:05")
      reject("filename is not valid", "agent", "request", "download-item", "--album", "albBad", "--item", "keyDot", "--current", ".")
      reject("filename is not valid", "agent", "request", "download-item", "--album", "albBad", "--item", "keyDots", "--current", "..")
      reject("filename is not valid", "agent", "request", "download-item", "--album", "albBad", "--item", "keySlash", "--current", "a/b.jpg")
      reject(
        "album has an owner",
        "agent", "request", "delete-item", "--album", "albOwn", "--item", "keyOwn", "--current", "pic.jpg"
      )
      reject(
        "item has an uploader",
        "agent", "request", "delete-item", "--album", "albUp", "--item", "keyUp", "--current", "pic.jpg"
      )
      Seq("r0", "r00", "r000", "r001").foreach: id =>
        reject(
          "request id is not valid",
          "agent", "request", "rename-album", "--album", "albTrail", "--current", "2010/07 ", "--new", "July", "--id", id
        )
      val download: Catalog.Album = Catalog.Album(
        "alb1", 1, "Title", None, false, None,
        Seq(Catalog.Item("pic.jpg", Some(LocalDate.of(2010, 7, 1)), "key1", None)),
        Seq.empty
      )
      intercept[IllegalArgumentException]:
        AgentCommands.acceptRequest(Seq(download), Requests.Action.DownloadItem, "alb1", "key1", "pic.jpg", "nope")
      .getMessage shouldBe "new is not empty"
      intercept[IllegalArgumentException]:
        AgentCommands.acceptRequest(Seq(download), Requests.Action.DeleteItem, "alb1", "key1", "pic.jpg", "nope")
      .getMessage shouldBe "new is not empty"

  it should "append r1 and list titles without album ids or item keys" in :
    withArchive: archive =>
      writeFixture(archive)
      val session: Session = sessionFor(archive)
      val (_, rename) = exec(
        session, "agent", "request", "rename-album", "--album", "albTrail", "--current", "2010/07 ", "--new", "July"
      )
      rename match
        case Result.Ok(text, value) =>
          text shouldBe "appended r1 rename-album\n"
          noIds(text, fixtureIds*)
          field(value, "id") shouldBe Json.Str("r1")
          field(value, "albumId") shouldBe Json.Str("albTrail")
          field(value, "current") shouldBe Json.Str("2010/07 ")
        case other =>
          fail(other.toString)
      val before: String = JFiles.readString(requests(archive).toPath)
      val duplicate: String = usage(
        session, "agent", "request", "rename-album", "--album", "albTrail", "--current", "2010/07 ", "--new", "Later", "--id", "r1"
      )
      duplicate shouldBe "duplicate request id"
      noIds(duplicate, fixtureIds*)
      JFiles.readString(requests(archive).toPath) shouldBe before
      val (_, move) = exec(
        session, "agent", "request", "move-item",
        "--album", "albSrc", "--item", "keyPic", "--current", "pic.jpg", "--new", "albDst"
      )
      move match
        case Result.Ok(text, value) =>
          text shouldBe "appended r2 move-item\n"
          noIds(text, fixtureIds*)
          field(value, "itemKey") shouldBe Json.Str("keyPic")
          field(value, "albumId") shouldBe Json.Str("albSrc")
          field(value, "new") shouldBe Json.Str("albDst")
        case other =>
          fail(other.toString)
      Requests.append(requests(archive), Requests.Action.MoveItem, "albSrc", "keyPic", "pic.jpg", "goneId")
      val (_, described) = exec(
        session, "agent", "request", "set-description", "--item", "keyOnly", "--current", "pic.jpg"
      )
      described match
        case Result.Ok(text, value) =>
          text shouldBe "appended r4 set-description\n"
          noIds(text, fixtureIds*)
          field(value, "new") shouldBe Json.Str("")
          field(value, "itemKey") shouldBe Json.Str("keyOnly")
        case other =>
          fail(other.toString)
      val (_, dated) = exec(
        session, "agent", "request", "set-date", "--item", "keyOnly", "--current", "pic.jpg", "--new", "2010-07-19 09:05"
      )
      dated match
        case Result.Ok(text, _) =>
          text shouldBe "appended r5 set-date\n"
          noIds(text, fixtureIds*)
        case other =>
          fail(other.toString)
      val (_, download) = exec(
        session, "agent", "request", "download-item", "--album", "albSrc", "--item", "keyPic", "--current", "pic.jpg"
      )
      download match
        case Result.Ok(text, value) =>
          text shouldBe "appended r6 download-item\n"
          noIds(text, fixtureIds*)
          field(value, "itemKey") shouldBe Json.Str("keyPic")
        case other =>
          fail(other.toString)
      val (_, deleted) = exec(
        session, "agent", "request", "delete-item", "--album", "albSrc", "--item", "keyPic", "--current", "pic.jpg"
      )
      deleted match
        case Result.Ok(text, value) =>
          text shouldBe "appended r7 delete-item\n"
          noIds(text, fixtureIds*)
          field(value, "albumId") shouldBe Json.Str("albSrc")
        case other =>
          fail(other.toString)
      val (_, listed) = exec(session, "agent", "requests")
      listed match
        case Result.Ok(text, Json.Arr(rows)) =>
          text shouldBe
            """r1 rename-album "2010/07 " "July"
              |r2 move-item "pic.jpg" "Dest Title"
              |r3 move-item "pic.jpg" destination unknown
              |r4 set-description "pic.jpg" ""
              |r5 set-date "pic.jpg" "2010-07-19 09:05"
              |r6 download-item "pic.jpg"
              |r7 delete-item "pic.jpg"
              |""".stripMargin
          noIds(text, fixtureIds*)
          noIds(text, "goneId")
          rows.map(field(_, "id")) shouldBe Seq("r1", "r2", "r3", "r4", "r5", "r6", "r7").map(Json.Str.apply)
          field(rows(2), "new") shouldBe Json.Str("goneId")
        case other =>
          fail(other.toString)

  it should "print a result note unchanged" in :
    withArchive: archive =>
      val (_, absent) = exec(sessionFor(archive), "agent", "results")
      absent shouldBe Result.Ok("no results\n", Json.Arr(Seq.empty))
      File(archive, ".gphoto-sync").exists shouldBe false
      val (_, quiet) = exec(sessionFor(archive), "agent", "requests")
      quiet shouldBe Result.Ok("no requests\n", Json.Arr(Seq.empty))
      File(archive, ".gphoto-sync").exists shouldBe false
      val dot: File = File(archive, ".gphoto-sync")
      dot.mkdirs()
      JFiles.writeString(File(dot, "requests.tsv").toPath, "# gphoto-requests 2\n")
      val (_, emptyRows) = exec(sessionFor(archive), "agent", "requests")
      emptyRows shouldBe Result.Ok("", Json.Arr(Seq.empty))
      JFiles.writeString(
        File(dot, "results.tsv").toPath,
        "# gphoto-results 2\nr1\trename-album\tdone\thello \nr2\tmove-item\tfailed\t\n"
      )
      val (_, listed) = exec(sessionFor(archive), "agent", "results")
      listed match
        case Result.Ok(text, Json.Arr(Seq(first, second))) =>
          text shouldBe "r1 rename-album done hello \nr2 move-item failed\n"
          field(first, "note") shouldBe Json.Str("hello ")
          field(first, "status") shouldBe Json.Str("done")
          field(first, "action") shouldBe Json.Str("rename-album")
          field(second, "note") shouldBe Json.Str("")
          field(second, "status") shouldBe Json.Str("failed")
        case other =>
          fail(other.toString)

  it should "report a missing request flag before it reads the catalog" in :
    withArchive: archive =>
      val session: Session = sessionFor(archive)
      usage(session, "agent", "request", "rename-album") shouldBe "missing album"
      usage(session, "agent", "request", "rename-album", "--album", "albTrail", "--new", "X") shouldBe "missing current"
      usage(session, "agent", "request", "rename-album", "--album", "albTrail", "--current", "Old") shouldBe "missing new"
      usage(session, "agent", "request", "move-item", "--album", "albSrc", "--current", "pic.jpg", "--new", "albDst") shouldBe
        "missing item"
      usage(session, "agent", "request", "set-date", "--item", "keyOnly", "--current", "pic.jpg") shouldBe "missing new"
      usage(session, "agent", "request", "set-description", "--current", "pic.jpg") shouldBe "missing item"
      File(archive, ".gphoto-sync").exists shouldBe false

  private val fixtureIds: Seq[String] = Seq(
    "albTrail", "albOwn", "albSrc", "albDst", "albUp", "albShareA", "albShareB", "albOnly", "albBad", "albUnset",
    "keyOwn", "keyPic", "keyUp", "keyShare", "keyOnly", "keyDot", "keyDots", "keySlash", "keyUnset", "goneId"
  )

  private def writeFixture(archive: File): Unit =
    writeCatalog(
      archive,
      "album\talbTrail\t0\t2010/07 ",
      "album\talbOwn\t1\tOwned Name",
      "owner\talbOwn\tAda",
      "item\talbOwn\tpic.jpg\t2010-07-01\tkeyOwn\t-",
      "album\talbSrc\t1\tSource Title",
      "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
      "album\talbDst\t0\tDest Title",
      "album\talbUp\t1\tUploaded Name",
      "item\talbUp\tpic.jpg\t2010-07-02\tkeyUp\tBea",
      "album\talbShareA\t1\tAlpha Name",
      "item\talbShareA\tpic.jpg\t2010-07-03\tkeyShare\t-",
      "album\talbShareB\t1\tBeta Name",
      "item\talbShareB\tother.jpg\t2010-07-04\tkeyShare\t-",
      "album\talbOnly\t1\tOnly Name",
      "item\talbOnly\tpic.jpg\t2010-07-05\tkeyOnly\t-",
      "album\talbBad\t3\tBad Names",
      "item\talbBad\t.\t2010-07-06\tkeyDot\t-",
      "item\talbBad\t..\t2010-07-07\tkeyDots\t-",
      "item\talbBad\ta/b.jpg\t2010-07-08\tkeySlash\t-",
      "album\talbUnset\t1\tUnset Name",
      "error\talbUnset\t1\t1\t1",
      "unset\talbUnset\t2010-07-09\tkeyUnset\t-"
    )

  private def place(session: Session, title: String, path: String, pathTitle: String): Unit =
    val (_, result) = exec(session, "agent", "album-file", "--no-request", title)
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

  private def usage(session: Session, args: String*): String =
    exec(session, args*)._2 match
      case Result.Usage(message) => message
      case other => fail(other.toString)

  private def field(value: Json, name: String): Json = value match
    case Json.Obj(fields) =>
      fields.find(_._1 == name).map(_._2).getOrElse(fail(s"missing $name in $value"))
    case other =>
      fail(other.toString)

  private def exec(session: Session, args: String*): (Session, Result) =
    Cli.parse(args.toList) match
      case Cli.Run(parsed) =>
        Cli.toOperation(parsed).execute(session)
      case other =>
        fail(other.toString)

  private def sessionFor(archive: File): Session = Session(rootFor(archive))

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

  private def mkdir(archive: File, path: String): Unit =
    File(archive, path).mkdirs()

  private def touch(directory: File, name: String): Unit =
    directory.mkdirs()
    JFiles.write(File(directory, name).toPath, Array.emptyByteArray)

  private def requests(archive: File): File = File(archive, ".gphoto-sync/requests.tsv")

  private def partials(archive: File): Seq[String] =
    val dot: File = File(archive, ".gphoto-sync")
    if !dot.isDirectory then Seq.empty
    else Option(dot.list).getOrElse(Array.empty[String]).toSeq.filter(_.startsWith("album-write-"))

  private def tomls(file: File): Seq[String] =
    if !file.exists then Seq.empty
    else if file.isDirectory then Option(file.listFiles).getOrElse(Array.empty[File]).toSeq.flatMap(tomls)
    else if file.getName.endsWith(".toml") then Seq(file.getPath)
    else Seq.empty

  private def withArchive(body: File => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("agent-write").toFile
    try body(archive)
    finally deleteTree(archive)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
