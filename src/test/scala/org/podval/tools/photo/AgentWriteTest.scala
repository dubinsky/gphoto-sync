package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.io.{ByteArrayOutputStream, File}
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, StandardOpenOption}
import java.time.{LocalDate, ZoneId}

class AgentWriteTest extends AnyFlatSpec, Matchers:
  @targetName("outcomeCanEqual")
  private given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

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
        opened shouldBe false
      finally
        Refresh.openAct = previousAct
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
          "agent", "request", "move-item", "--album", "albSrc", "--item", "keyPic",
          "--current", "pic.jpg", "--new", "albDst", "--id", id
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
      intercept[IllegalArgumentException]:
        AgentCommands.acceptRequest(Seq(download), Requests.Action.RenameAlbum, "gone", "", "Title", "Next")
      .getMessage shouldBe "no such album"

  it should "append r1 and list titles without album ids or item keys" in :
    withArchive: archive =>
      writeFixture(archive)
      val session: Session = sessionFor(archive)
      val seeded: Requests.Row = Requests.append(
        requests(archive), Requests.Action.RenameAlbum, "albTrail", "", "2010/07 ", "July"
      )
      seeded.id shouldBe "r1"
      val before: String = JFiles.readString(requests(archive).toPath)
      val duplicate: String = usage(
        session, "agent", "request", "move-item",
        "--album", "albSrc", "--item", "keyPic", "--current", "pic.jpg", "--new", "albDst", "--id", "r1"
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
      val beforeLegacy: String = JFiles.readString(requests(archive).toPath)
      Cli.parse(List(
        "agent", "request", "set-description", "--item", "keyOnly", "--current", "pic.jpg", "--new", "Hello"
      )) shouldBe Cli.Usage("use photo agent set-description")
      Cli.parse(List(
        "agent", "request", "set-date", "--item", "keyOnly", "--current", "pic.jpg", "--new", "2010-07-19 09:05"
      )) shouldBe Cli.Usage("use photo agent set-date")
      JFiles.readString(requests(archive).toPath) shouldBe beforeLegacy
      Requests.append(requests(archive), Requests.Action.SetDescription, "", "keyOnly", "pic.jpg", "")
      Requests.append(requests(archive), Requests.Action.SetDate, "", "keyOnly", "pic.jpg", "2010-07-19 09:05")
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
      Cli.parse(List("agent", "request", "rename-album")) shouldBe Cli.Usage("use photo agent rename-album")
      Cli.parse(List("agent", "request", "rename-album", "--help")) shouldBe
        Cli.Usage("use photo agent rename-album")
      Cli.parse(List(
        "agent", "request", "rename-album", "--album", "albTrail", "--current", "Old", "--new", "X"
      )) shouldBe Cli.Usage("use photo agent rename-album")
      usage(session, "agent", "rename-album") shouldBe "missing title"
      usage(session, "agent", "rename-album", "--title", "T", "--new", "X") shouldBe "missing current"
      usage(session, "agent", "rename-album", "--title", "T", "--current", "Old") shouldBe "missing new"
      usage(session, "agent", "request", "move-item", "--album", "albSrc", "--current", "pic.jpg", "--new", "albDst") shouldBe
        "missing item"
      Cli.parse(List("agent", "request", "set-description", "--current", "pic.jpg")) shouldBe
        Cli.Usage("use photo agent set-description")
      Cli.parse(List("agent", "request", "set-date", "--item", "keyOnly", "--current", "pic.jpg")) shouldBe
        Cli.Usage("use photo agent set-date")
      usage(session, "agent", "set-description") shouldBe "missing item"
      usage(session, "agent", "set-description", "--item", "keyOnly") shouldBe "missing current"
      usage(session, "agent", "set-date", "--item", "keyOnly", "--current", "pic.jpg") shouldBe "missing new"
      File(archive, ".gphoto-sync").exists shouldBe false

  "rename-album" should "leave the catalog unchanged when the step throws" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, opened) =>
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake((album, updated) =>
            throw IllegalStateException(s"step missed AF1QipLEAK $updated AF1QipOTHER")
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session, "agent", "rename-album", "--title", "2010/07 ", "--current", "2010/07 ", "--new", "July"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            Cli.processCode(result) shouldBe 1
            text shouldBe
              """failed rename-album
                |album "2010/07 "
                |step More options, then Edit album, Rename album, or Rename
                |recovery one-click
                |catalog unchanged
                |step missed 2010/07  July 2010/07 
                |""".stripMargin
            text should not include ("AF1Qip")
            text should not include ("alb07")
            field(value, "recovery") shouldBe Json.Str("one-click")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "albumId") shouldBe Json.Str("alb07")
            field(value, "itemKey") shouldBe Json.Str("")
            field(value, "title") shouldBe Json.Str("2010/07 ")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        places.directory.exists shouldBe false
        closed shouldBe true
        released(places)

  it should "retitle only that album line when the live title is clean" in :
    withArchive: archive =>
      writePair(archive)
      withAct(archive): (session, places, opened) =>
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => ()):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session, "agent", "rename-album", "--title", "2010/07 ", "--current", "2010/07 ", "--new", "July"
        )
        result match
          case Result.Ok(text, value) =>
            text shouldBe "rename-album \"July\"\n"
            text should not include ("alb07")
            text should not include ("albKeep")
            Cli.processCode(result) shouldBe 0
            field(value, "action") shouldBe Json.Str("rename-album")
            field(value, "title") shouldBe Json.Str("July")
            field(value, "catalog") shouldBe Json.Str("updated")
            field(value, "albumId") shouldBe Json.Str("alb07")
            field(value, "itemKey") shouldBe Json.Str("")
            value match
              case Json.Obj(fields) =>
                fields.exists(_._1 == "step") shouldBe false
                fields.exists(_._1 == "recovery") shouldBe false
              case other =>
                fail(other.toString)
          case other =>
            fail(other.toString)
        val written: String = JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath)
        written shouldBe
          """# gphoto-catalog 2
            |album	alb07	1	July
            |item	alb07	pic.jpg	2010-07-01	keyPic	-
            |album	albKeep	0	Keep Title
            |""".stripMargin
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        places.directory.exists shouldBe false
        File(places.directory, "status.txt").exists shouldBe false
        File(places.directory, "albums.tsv").exists shouldBe false
        File(places.directory, "parts").exists shouldBe false
        closed shouldBe true
        released(places)

  it should "leave the partial when the retitle check fails" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => ())
        val (_, result) = exec(
          session, "agent", "rename-album", "--title", "2010/07 ", "--current", "2010/07 ", "--new", "July\nX"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery refresh-only")
            text should include ("catalog unchanged")
            text should not include ("album set")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "catalog") shouldBe Json.Str("unchanged")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").isFile shouldBe true
        places.directory.exists shouldBe false

  it should "report already running without opening the browser" in :
    withArchive: archive =>
      writePair(archive)
      withAct(archive): (session, places, opened) =>
        places.directory.mkdirs()
        JFiles.writeString(File(places.directory, "status.txt").toPath, "phase=items\nAF1QipBusy\n")
        Refresh.claim(places.lock) match
          case Refresh.Claim.Owned(channel, lock) =>
            try
              Refresh.openAct = _ => new RenameFake((_, _) => fail("opened while busy"))
              val (_, result) = exec(
                session, "agent", "rename-album", "--title", "2010/07 ", "--current", "2010/07 ", "--new", "July"
              )
              result match
                case Result.Failed(text, value, message) =>
                  message shouldBe Refresh.AlreadyRunning
                  Cli.processCode(result) shouldBe 2
                  text should startWith ("already running\n")
                  text should include ("phase=items")
                  text should not include ("AF1Qip")
                  value shouldBe Json.Null
                case other =>
                  fail(other.toString)
              opened() shouldBe false
              File(places.directory, "albums.tsv").exists shouldBe false
            finally
              Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  it should "strip every AF1Qip token on stderr and stay in the caller" in :
    withArchive: archive =>
      writeCatalog(archive, "album\talb07\t0\tOther")
      PlaywrightPhotosUi.redactSecrets("AF1QipAAA and AF1QipBBB_9", "2010/07 ") shouldBe "2010/07  and 2010/07 "
      PlaywrightPhotosUi.redactSecrets("AF1QipAAA and AF1QipBBB_9", "") shouldBe "album and album"
      withAct(archive): (session, _, opened) =>
        Refresh.openAct = _ => new RenameFake((_, _) => fail("accept opened"))
        val (_, out, err) = shellOf(
          session,
          "agent rename-album --title \"AF1QipAAA AF1QipBBB\" --current x --new y"
        )
        out shouldBe ""
        err should include ("Photo Archive Tool:")
        err should include ("no album titled \"album album\"")
        err should not include ("AF1Qip")
        opened() shouldBe false
        var ran = false
        val (_, line) = Cli.serveResponse(
          session,
          """{"argv":["agent","rename-album","--title","2010/07 ","--current","2010/07 ","--new","July"]}""",
          (_, _) =>
            ran = true
            (session, Result.Ok("ran\n", Json.Null))
        )
        ran shouldBe false
        line should include ("rename-album runs in the calling process")
        Refresh.openAct = _ => new RenameFake((_, _) => ())
        val sent = scala.collection.mutable.ListBuffer.empty[List[String]]
        val (code, shown, errors) = shellOf(
          session,
          "agent rename-album --title Other --current Other --new July",
          Some((_, argv) =>
            sent += argv
            Result.Ok("sent\n", Json.Null)
          )
        )
        code shouldBe 0
        sent.isEmpty shouldBe true
        shown shouldBe "rename-album \"July\"\n"
        errors should not include ("Photo Archive Tool:")
        errors should not include ("AF1Qip")

  it should "run in the one-shot process while serve holds its lock" in :
    withArchive: archive =>
      writePair(archive)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => ())
        val argv: List[String] = List(
          "agent", "rename-album", "--title", "2010/07 ", "--current", "2010/07 ", "--new", "July"
        )
        val runtime: File = JFiles.createTempDirectory("rename-serve").toFile
        val channel: FileChannel = FileChannel.open(
          File(runtime, "serve.lock").toPath,
          StandardOpenOption.CREATE,
          StandardOpenOption.READ,
          StandardOpenOption.WRITE
        )
        val lock = channel.tryLock()
        lock should not be null
        try
          Cli.parse(argv) match
            case Cli.Run(parsed) =>
              Cli.executeOneShot(session, argv, parsed, Right(runtime), waitMs = 200) match
                case Result.Ok(text, _) =>
                  text shouldBe "rename-album \"July\"\n"
                case Result.Usage(message) =>
                  message should not include ("a command is in progress")
                  fail(message)
                case other =>
                  fail(other.toString)
            case other =>
              fail(other.toString)
        finally
          lock.release()
          channel.close()
          deleteTree(runtime)

  "set-description" should "leave the catalog unchanged when the step throws" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDescription = (_, text) =>
            throw IllegalStateException(s"step missed AF1QipLEAK $text AF1QipOTHER")
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session, "agent", "set-description", "--item", "keyPic", "--current", "pic.jpg", "--new", "Hello"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            Cli.processCode(result) shouldBe 1
            text shouldBe
              """failed set-description
                |filename "pic.jpg"
                |step Open info, or the i key, then Add a description
                |recovery one-click
                |catalog unchanged
                |step missed 2010/07  Hello 2010/07 
                |""".stripMargin
            text should not include ("AF1Qip")
            text should not include ("alb07")
            text should not include ("keyPic")
            field(value, "recovery") shouldBe Json.Str("one-click")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "albumId") shouldBe Json.Str("alb07")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
            field(value, "title") shouldBe Json.Str("2010/07 ")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        places.directory.exists shouldBe false
        closed shouldBe true
        released(places)

  it should "stay refresh-only when the panel does not show the description" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDescription = (_, _) =>
            throw new Refresh.ActMiss("panel does not show the description", confirmed = true)
          )
        val (_, result) = exec(
          session, "agent", "set-description", "--item", "keyPic", "--current", "pic.jpg", "--new", "Hello"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery refresh-only")
            text should include ("catalog unchanged")
            text should include ("panel does not show the description")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        places.directory.exists shouldBe false

  it should "leave the catalog byte-identical when the description is set" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        val seen = scala.collection.mutable.ListBuffer.empty[String]
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDescription = (item, text) =>
            item.filename shouldBe "pic.jpg"
            seen += text
          ):
            override def close(): Unit = closed = true
        val (kept, result) = exec(
          session, "agent", "set-description", "--item", "keyPic", "--current", "pic.jpg", "--new", "Hello"
        )
        result match
          case Result.Ok(text, value) =>
            text shouldBe "set-description \"pic.jpg\"\n"
            text should not include ("alb07")
            text should not include ("keyPic")
            Cli.processCode(result) shouldBe 0
            field(value, "action") shouldBe Json.Str("set-description")
            field(value, "title") shouldBe Json.Str("2010/07 ")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "albumId") shouldBe Json.Str("alb07")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
            value match
              case Json.Obj(fields) =>
                fields.exists(_._1 == "step") shouldBe false
                fields.exists(_._1 == "recovery") shouldBe false
              case other =>
                fail(other.toString)
          case other =>
            fail(other.toString)
        kept.catalogCache.isDefined shouldBe true
        val (cleared, empty) = exec(
          kept, "agent", "set-description", "--item", "keyPic", "--current", "pic.jpg", "--new", ""
        )
        empty match
          case Result.Ok(text, value) =>
            text shouldBe "set-description \"pic.jpg\"\n"
            field(value, "catalog") shouldBe Json.Str("unchanged")
          case other =>
            fail(other.toString)
        cleared.catalogCache.isDefined shouldBe true
        seen.toSeq shouldBe Seq("Hello", "")
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        places.directory.exists shouldBe false
        closed shouldBe true
        released(places)

  "set-date" should "write only that item's date when the read shows the civil day" in :
    withArchive: archive =>
      writeDated(archive)
      withAct(archive): (session, places, _) =>
        var seen: Option[String] = None
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDate = (item, local) =>
            item.filename shouldBe "pic.jpg"
            item.key shouldBe "keyPic"
            seen = Some(local)
          ):
            override def close(): Unit = closed = true
        val (next, result) = exec(
          session, "agent", "set-date", "--item", "keyPic", "--current", "pic.jpg", "--new", "2010-07-19 09:05"
        )
        result match
          case Result.Ok(text, value) =>
            text shouldBe "set-date \"pic.jpg\"\n"
            text should not include ("alb07")
            text should not include ("keyPic")
            text should not include ("09:05")
            field(value, "action") shouldBe Json.Str("set-date")
            field(value, "title") shouldBe Json.Str("2010/07 ")
            field(value, "catalog") shouldBe Json.Str("updated")
            field(value, "albumId") shouldBe Json.Str("alb07")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        seen shouldBe Some("2010-07-19 09:05")
        next.catalogCache.isEmpty shouldBe true
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe
          """# gphoto-catalog 2
            |album	alb07	2	2010/07 
            |item	alb07	pic.jpg	2010-07-19	keyPic	-
            |item	alb07	other.jpg	2010-07-02	keyOther	-
            |album	albKeep	1	Keep Title
            |owner	albKeep	Ada
            |item	albKeep	keep.jpg	2010-08-01	keyKeep	-
            |""".stripMargin
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        places.directory.exists shouldBe false
        closed shouldBe true
        released(places)

  it should "leave the old bytes when the read shows a different day" in :
    withArchive: archive =>
      val catalog: String = writeDated(archive)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDate = (_, _) =>
            throw new Refresh.ActMiss("read date differs", confirmed = true)
          )
        val (_, result) = exec(
          session, "agent", "set-date", "--item", "keyPic", "--current", "pic.jpg", "--new", "2010-07-19 09:05"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("filename \"pic.jpg\"")
            text should include ("step Date taken, then Save, Done, or OK")
            text should include ("recovery refresh-only")
            text should include ("catalog unchanged")
            text should include ("read date differs")
            text should not include ("keyPic")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        places.directory.exists shouldBe false

  it should "write nothing when the date step misses before confirm" in :
    withArchive: archive =>
      val catalog: String = writeDated(archive)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDate = (_, _) =>
            throw new Refresh.ActMiss("Year is absent", confirmed = false)
          )
        val (_, result) = exec(
          session, "agent", "set-date", "--item", "keyPic", "--current", "pic.jpg", "--new", "2010-07-19 09:05"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery one-click")
            text should include ("catalog unchanged")
            text should include ("Year is absent")
            field(value, "recovery") shouldBe Json.Str("one-click")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false

  it should "stay refresh-only when the catalog read throws after confirm" in :
    withArchive: archive =>
      val catalog: String = writeDated(archive)
      val live: File = File(archive, ".gphoto-sync/catalog.txt")
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDate = (_, _) =>
            live.setReadable(false, false)
          )
        try
          val (_, result) = exec(
            session, "agent", "set-date", "--item", "keyPic", "--current", "pic.jpg", "--new", "2010-07-19 09:05"
          )
          result match
            case Result.Failed(text, value, message) =>
              message shouldBe Refresh.MutationFailed
              text should include ("recovery refresh-only")
              text should include ("catalog unchanged")
              field(value, "recovery") shouldBe Json.Str("refresh-only")
            case other =>
              fail(other.toString)
          live.setReadable(true, false)
          JFiles.readString(live.toPath) shouldBe catalog
          File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
          File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        finally
          live.setReadable(true, false)

  it should "answer both edits from the calling process" in :
    withArchive: archive =>
      writePair(archive)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => fail("accept opened"))
        val (_, described) = Cli.serveResponse(
          session,
          """{"argv":["agent","set-description","--item","keyPic","--current","pic.jpg","--new","Hello"]}""",
          (_, _) => fail("set-description was forwarded")
        )
        described should include ("set-description runs in the calling process")
        val (_, dated) = Cli.serveResponse(
          session,
          """{"argv":["agent","set-date","--item","keyPic","--current","pic.jpg","--new","2010-07-19 09:05"]}""",
          (_, _) => fail("set-date was forwarded")
        )
        dated should include ("set-date runs in the calling process")
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDescription = (_, text) =>
            if text.contains("AF1Qip") then throw IllegalStateException(s"step missed $text")
          )
        val sent = scala.collection.mutable.ListBuffer.empty[List[String]]
        val (code, shown, errors) = shellOf(
          session,
          "agent set-description --item keyPic --current pic.jpg --new Hello",
          Some((_, argv) =>
            sent += argv
            Result.Ok("sent\n", Json.Null)
          )
        )
        code shouldBe 0
        sent.isEmpty shouldBe true
        shown shouldBe "set-description \"pic.jpg\"\n"
        errors should not include ("Photo Archive Tool:")
        errors should not include ("mutation failed")
        errors should not include ("AF1Qip")
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDescription = (_, _) =>
            throw IllegalStateException("step missed AF1QipLEAK")
          )
        val (failed, out, err) = shellOf(
          session,
          "agent set-description --item keyPic --current pic.jpg --new Hello"
        )
        failed shouldBe 0
        out should include ("failed set-description")
        out should include ("recovery one-click")
        out should not include ("AF1Qip")
        out should not include ("mutation failed")
        err should not include ("mutation failed")
        err should not include ("Photo Archive Tool:")
        err should not include ("AF1Qip")

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

  private def writePair(archive: File): String =
    val file: File = writeCatalog(
      archive,
      "album\talb07\t1\t2010/07 ",
      "item\talb07\tpic.jpg\t2010-07-01\tkeyPic\t-",
      "album\talbKeep\t0\tKeep Title"
    )
    JFiles.readString(file.toPath)

  private def writeDated(archive: File): String =
    val file: File = writeCatalog(
      archive,
      "album\talb07\t2\t2010/07 ",
      "item\talb07\tpic.jpg\t2010-07-01\tkeyPic\t-",
      "item\talb07\tother.jpg\t2010-07-02\tkeyOther\t-",
      "album\talbKeep\t1\tKeep Title",
      "owner\talbKeep\tAda",
      "item\talbKeep\tkeep.jpg\t2010-08-01\tkeyKeep\t-"
    )
    JFiles.readString(file.toPath)

  private class RenameFake(
    effect: (Catalog.Album, String) => Unit,
    onDescription: (Catalog.Item, String) => Unit = (_, _) => (),
    onDate: (Catalog.Item, String) => Unit = (_, _) => ()
  ) extends PhotosAct:
    def albumCards(): Seq[AlbumCard] = Seq.empty
    def readOwner(card: AlbumCard): Option[String] = None
    def readAlbum(card: AlbumCard): Catalog.Album = throw IllegalStateException("read")
    def close(): Unit = ()
    def renameAlbum(album: Catalog.Album, updated: String): Unit = effect(album, updated)
    def setDescription(item: Catalog.Item, text: String): Unit = onDescription(item, text)
    def setDate(item: Catalog.Item, local: String): Unit = onDate(item, local)

  private def withAct(archive: File)(body: (Session, Refresh.Places, () => Boolean) => Unit): Unit =
    val parent: File = JFiles.createTempDirectory("rename-lock").toFile
    val places: Refresh.Places = Refresh.Places(File(parent, "gphoto-sync-refresh"), File(parent, "lock"))
    val previousPlaces = Refresh.placesOf
    val previousAct = Refresh.openAct
    var opened = false
    Refresh.placesOf = () => places
    Refresh.openAct = notice =>
      opened = true
      previousAct(notice)
    try body(sessionFor(archive), places, () => opened)
    finally
      Refresh.placesOf = previousPlaces
      Refresh.openAct = previousAct
      deleteTree(parent)

  private def released(places: Refresh.Places): Unit =
    Refresh.claim(places.lock) match
      case Refresh.Claim.Owned(channel, lock) =>
        Refresh.release(channel, lock)
      case Refresh.Claim.Busy =>
        fail("lock still held")

  private def shellOf(
    session: Session,
    line: String,
    exchange: Option[(Session, List[String]) => Result] = None
  ): (Int, String, String) =
    val out = new ByteArrayOutputStream
    val err = new ByteArrayOutputStream
    val code: Int = Console.withOut(out):
      Console.withErr(err):
        Cli.shell(
          session,
          Iterator(line),
          (current, parsed) => Cli.toOperation(parsed).execute(current),
          exchange
        )
    (code, new String(out.toByteArray, StandardCharsets.UTF_8), new String(err.toByteArray, StandardCharsets.UTF_8))

  private def withArchive(body: File => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("agent-write").toFile
    try body(archive)
    finally deleteTree(archive)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
