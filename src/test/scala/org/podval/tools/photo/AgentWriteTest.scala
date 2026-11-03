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
        reject("filename is not valid", "agent", "download-item", "--title", "Bad Names", "--item", "keyDot", "--current", ".")
        reject("filename is not valid", "agent", "download-item", "--title", "Bad Names", "--item", "keyDots", "--current", "..")
        reject(
          "filename is not valid",
          "agent", "download-item", "--title", "Bad Names", "--item", "keySlash", "--current", "a/b.jpg"
        )
        opened shouldBe false
      finally
        Refresh.openAct = previousAct
      Cli.parse(List(
        "agent", "request", "move-item", "--album", "goneId", "--item", "keyPic",
        "--current", "pic.jpg", "--new", "albDst"
      )) shouldBe Cli.Usage("use photo agent move-item")
      Cli.parse(List("agent", "request", "move-item", "--help")) shouldBe
        Cli.Usage("use photo agent move-item")
      Cli.parse(List(
        "agent", "request", "delete-item", "--album", "albOwn", "--item", "keyOwn", "--current", "pic.jpg"
      )) shouldBe Cli.Usage("use photo agent delete-item")
      Cli.parse(List("agent", "request", "delete-item", "--help")) shouldBe
        Cli.Usage("use photo agent delete-item")
      Cli.requestCommandNames shouldBe Nil
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
        AgentCommands.acceptRequest(Seq(download), AgentCommands.Action.DownloadItem, "alb1", "key1", "pic.jpg", "nope")
      .getMessage shouldBe "new is not empty"
      intercept[IllegalArgumentException]:
        AgentCommands.acceptRequest(Seq(download), AgentCommands.Action.DeleteItem, "alb1", "key1", "pic.jpg", "nope")
      .getMessage shouldBe "new is not empty"
      intercept[IllegalArgumentException]:
        AgentCommands.acceptRequest(Seq(download), AgentCommands.Action.RenameAlbum, "gone", "", "Title", "Next")
      .getMessage shouldBe "no such album"

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
      Cli.parse(List(
        "agent", "request", "move-item", "--album", "albSrc", "--current", "pic.jpg", "--new", "albDst"
      )) shouldBe Cli.Usage("use photo agent move-item")
      Cli.parse(List("agent", "request", "set-description", "--current", "pic.jpg")) shouldBe
        Cli.Usage("use photo agent set-description")
      Cli.parse(List("agent", "request", "set-date", "--item", "keyOnly", "--current", "pic.jpg")) shouldBe
        Cli.Usage("use photo agent set-date")
      Cli.parse(List(
        "agent", "request", "download-item", "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg"
      )) shouldBe Cli.Usage("use photo agent download-item")
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
      Cli.parse(List(
        "agent", "request", "delete-item", "--album", "albSrc", "--item", "keyPic", "--current", "pic.jpg"
      )) shouldBe Cli.Usage("use photo agent delete-item")
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
        val (_, downloaded) = Cli.serveResponse(
          session,
          """{"argv":["agent","download-item","--title","2010/07 ","--item","keyPic","--current","pic.jpg"]}""",
          (_, _) => fail("download-item was forwarded")
        )
        downloaded should include ("download-item runs in the calling process")
        val (_, added) = Cli.serveResponse(
          session,
          """{"argv":["agent","add-to-album","--title","2010/07 ","--item","keyPic","--current","pic.jpg","--dest","Keep Title"]}""",
          (_, _) => fail("add-to-album was forwarded")
        )
        added should include ("add-to-album runs in the calling process")
        val (_, removed) = Cli.serveResponse(
          session,
          """{"argv":["agent","remove-from-album","--title","2010/07 ","--item","keyPic","--current","pic.jpg"]}""",
          (_, _) => fail("remove-from-album was forwarded")
        )
        removed should include ("remove-from-album runs in the calling process")
        val (_, moved) = Cli.serveResponse(
          session,
          """{"argv":["agent","move-item","--title","2010/07 ","--item","keyPic","--current","pic.jpg","--dest","Keep Title"]}""",
          (_, _) => fail("move-item was forwarded")
        )
        moved should include ("move-item runs in the calling process")
        val (_, deleted) = Cli.serveResponse(
          session,
          """{"argv":["agent","delete-item","--title","2010/07 ","--item","keyPic","--current","pic.jpg"]}""",
          (_, _) => fail("delete-item was forwarded")
        )
        deleted should include ("delete-item runs in the calling process")
        val (_, uploaded) = Cli.serveResponse(
          session,
          """{"argv":["agent","upload-item","--dest","Keep Title","2010/07/01/pic.jpg"]}""",
          (_, _) => fail("upload-item was forwarded")
        )
        uploaded should include ("upload-item runs in the calling process")
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

  "download-item" should "keep a staged file and not open Chrome" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talb07\t1\t2010/07 ",
        "item\talb07\tIMG_pic.JPG\t2010-07-01\tkeyPic\t-",
        "album\talbKeep\t1\tKeep Title",
        "item\talbKeep\tIMG_pic.JPG\t2010-07-01\tkeyKeep\t-"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      val staged: File = File(archive, ".gphoto-sync/.download/2010/07/01/IMG_pic.JPG")
      staged.getParentFile.mkdirs()
      val body: Array[Byte] = Array[Byte](1, 2, 3, 4)
      JFiles.write(staged.toPath, body)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onDownload = (_, _) => fail("download called"))
        val (_, first) = exec(
          session, "agent", "download-item", "--title", "2010/07 ", "--item", "keyPic", "--current", "IMG_pic.JPG"
        )
        first match
          case Result.Ok(text, value) =>
            text shouldBe "download-item \"IMG_pic.JPG\"\n"
            text should not include ("alb07")
            text should not include ("keyPic")
            Cli.processCode(first) shouldBe 0
            field(value, "action") shouldBe Json.Str("download-item")
            field(value, "title") shouldBe Json.Str("2010/07 ")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "albumId") shouldBe Json.Str("alb07")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        val (_, second) = exec(
          session, "agent", "download-item", "--title", "Keep Title", "--item", "keyKeep", "--current", "IMG_pic.JPG"
        )
        second match
          case Result.Ok(text, value) =>
            text shouldBe "download-item \"IMG_pic.JPG\"\n"
            text should not include ("albKeep")
            text should not include ("keyKeep")
            field(value, "albumId") shouldBe Json.Str("albKeep")
          case other =>
            fail(other.toString)
        JFiles.readAllBytes(staged.toPath).sameElements(body) shouldBe true
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        requests(archive).exists shouldBe false
        places.directory.exists shouldBe false
        released(places)

  it should "keep an undated staged file" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talb07\t1\tLoose",
        "item\talb07\tIMG_pic.JPG\t-\tkeyPic\t-"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      val staged: File = File(archive, ".gphoto-sync/.download/undated/IMG_pic.JPG")
      staged.getParentFile.mkdirs()
      val body: Array[Byte] = Array[Byte](5, 6)
      JFiles.write(staged.toPath, body)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onDownload = (_, _) => fail("download called"))
        val (_, result) = exec(
          session, "agent", "download-item", "--title", "Loose", "--item", "keyPic", "--current", "IMG_pic.JPG"
        )
        result match
          case Result.Ok(text, _) =>
            text shouldBe "download-item \"IMG_pic.JPG\"\n"
          case other =>
            fail(other.toString)
        JFiles.readAllBytes(staged.toPath).sameElements(body) shouldBe true
        JFiles.readString(catalogFile.toPath) shouldBe catalog

  it should "use that album's item when the key is on two albums" in :
    withArchive: archive =>
      writeFixture(archive)
      val staged: File = File(archive, ".gphoto-sync/.download/2010/07/03/pic.jpg")
      staged.getParentFile.mkdirs()
      val body: Array[Byte] = Array[Byte](7)
      JFiles.write(staged.toPath, body)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onDownload = (_, _) => fail("download called"))
        val (_, result) = exec(
          session, "agent", "download-item", "--title", "Alpha Name", "--item", "keyShare", "--current", "pic.jpg"
        )
        result match
          case Result.Ok(text, value) =>
            text shouldBe "download-item \"pic.jpg\"\n"
            field(value, "albumId") shouldBe Json.Str("albShareA")
            field(value, "itemKey") shouldBe Json.Str("keyShare")
          case other =>
            fail(other.toString)
        JFiles.readAllBytes(staged.toPath).sameElements(body) shouldBe true

  it should "write the staging file and leave the catalog unchanged" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        var closed = false
        var seen: Option[File] = None
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDownload = (item, dest) =>
            item.filename shouldBe "pic.jpg"
            item.key shouldBe "keyPic"
            seen = Some(dest)
            JFiles.write(dest.toPath, Array[Byte](9, 8, 7))
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session, "agent", "download-item", "--title", "2010/07 ", "--item", "keyPic", "--current", "pic.jpg"
        )
        result match
          case Result.Ok(text, value) =>
            text shouldBe "download-item \"pic.jpg\"\n"
            text should not include ("alb07")
            text should not include ("keyPic")
            field(value, "action") shouldBe Json.Str("download-item")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "albumId") shouldBe Json.Str("alb07")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        val written: File = seen.getOrElse(fail("download was not called"))
        val staged: File = File(session.root.configuration.archive, ".gphoto-sync/.download/2010/07/01/pic.jpg")
        written.getCanonicalPath shouldBe staged.getCanonicalPath
        JFiles.readAllBytes(written.toPath).sameElements(Array[Byte](9, 8, 7)) shouldBe true
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        requests(archive).exists shouldBe false
        places.directory.exists shouldBe false
        File(places.directory, "status.txt").exists shouldBe false
        File(places.directory, "albums.tsv").exists shouldBe false
        File(places.directory, "parts").exists shouldBe false
        closed shouldBe true
        released(places)

  it should "report one-click when the download writes nothing" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDownload = (_, _) => ()):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session, "agent", "download-item", "--title", "2010/07 ", "--item", "keyPic", "--current", "pic.jpg"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            Cli.processCode(result) shouldBe 1
            text shouldBe
              """failed download-item
                |filename "pic.jpg"
                |step More options, then Download
                |recovery one-click
                |catalog unchanged
                |staged file is absent
                |""".stripMargin
            text should not include ("refresh-only")
            text should not include ("alb07")
            text should not include ("keyPic")
            field(value, "action") shouldBe Json.Str("download-item")
            field(value, "title") shouldBe Json.Str("2010/07 ")
            field(value, "step") shouldBe Json.Str("More options, then Download")
            field(value, "recovery") shouldBe Json.Str("one-click")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "albumId") shouldBe Json.Str("alb07")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        File(archive, ".gphoto-sync/.download/2010/07/01/pic.jpg").exists shouldBe false
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        places.directory.exists shouldBe false
        closed shouldBe true
        released(places)

  it should "report one-click when Download is absent" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDownload = (_, _) =>
            throw new Refresh.ActMiss("Download is absent AF1QipLEAK", confirmed = false)
          )
        val (_, result) = exec(
          session, "agent", "download-item", "--title", "2010/07 ", "--item", "keyPic", "--current", "pic.jpg"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery one-click")
            text should not include ("refresh-only")
            text should include ("Download is absent")
            text should not include ("AF1Qip")
            field(value, "recovery") shouldBe Json.Str("one-click")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog

  it should "report already running without opening the browser" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        places.directory.mkdirs()
        JFiles.writeString(File(places.directory, "status.txt").toPath, "phase=items\nAF1QipBusy\n")
        Refresh.claim(places.lock) match
          case Refresh.Claim.Owned(channel, lock) =>
            try
              Refresh.openAct = _ => new RenameFake((_, _) => fail("opened while busy"))
              val (_, result) = exec(
                session, "agent", "download-item", "--title", "2010/07 ", "--item", "keyPic", "--current", "pic.jpg"
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
              JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
              File(archive, ".gphoto-sync/.download/2010/07/01/pic.jpg").exists shouldBe false
            finally
              Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  "confirmedAdd" should "accept only one new key whose filename is current" in :
    val current: String = "pic.jpg"
    val hit: Catalog.Item = Catalog.Item(current, Some(LocalDate.of(2011, 1, 2)), "keyNew", Some("Bea"))
    val wrong: Catalog.Item = Catalog.Item("other.jpg", None, "keyBad", None)
    val blank: Catalog.Item = Catalog.Item("", None, "keyBlank", None)
    def shown(before: Set[String], after: Seq[String], named: Map[String, Catalog.Item]): String =
      AgentCommands.confirmedAdd(before, after, named, current) match
        case Some(item) => s"${item.key} ${item.filename}"
        case None => "none"
    shown(Set("old"), Seq("old", "keyNew"), Map("keyNew" -> hit)) shouldBe "keyNew pic.jpg"
    shown(Set("old"), Seq("old", "keyNew"), Map.empty) shouldBe "none"
    shown(Set("old"), Seq("old", "keyBlank"), Map("keyBlank" -> blank)) shouldBe "none"
    shown(Set.empty, Seq("keyNew", "keyBad"), Map("keyNew" -> hit, "keyBad" -> wrong)) shouldBe "none"
    shown(Set("old"), Seq("old", "keyBad"), Map("keyBad" -> wrong)) shouldBe "none"

  "add-to-album" should "refuse an owner line before Chrome opens" in :
    rejectPut(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title",
        "owner\talbDst\tAda"
      ),
      Seq("agent", "add-to-album", "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"),
      "destination has an owner"
    )

  it should "refuse auto-add before Chrome opens" in :
    rejectPut(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title",
        "auto-add\talbDst"
      ),
      Seq("agent", "add-to-album", "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"),
      "destination is auto-add"
    )

  it should "refuse unread names before Chrome opens" in :
    rejectPut(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t2\tDest Title",
        "error\talbDst\t1\t2\t1",
        "unset\talbDst\t2010-07-09\tkeyUnset\t-"
      ),
      Seq("agent", "add-to-album", "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"),
      "destination has unread names"
    )

  it should "refuse an excepted stem before Chrome opens" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      val day: File = File(archive, "2010/07/01")
      day.mkdirs()
      JFiles.write(File(day, "pic.jpg").toPath, Array.emptyByteArray)
      JFiles.writeString(File(archive, "Dest.toml").toPath, "include = [\"2010/07/01 except pic\"]\n")
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onPut = (_, _, _) => fail("opened"))
        usage(
          session, "agent", "add-to-album",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest"
        ) shouldBe "destination excepts \"pic\""
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "refuse the source album before Chrome opens" in :
    rejectPut(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-"
      ),
      Seq("agent", "add-to-album", "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Source Title"),
      "destination is the source album"
    )

  it should "refuse an ambiguous destination before Chrome opens" in :
    rejectPut(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title",
        "album\talbDst2\t0\tDest Title"
      ),
      Seq("agent", "add-to-album", "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"),
      "more than one album titled \"Dest Title\""
    )

  it should "not refuse an except when the local file is absent" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      JFiles.writeString(File(archive, "Dest.toml").toPath, "include = [\"2010/07/01 except pic\"]\n")
      withAct(archive): (session, places, _) =>
        var called = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onPut = (_, _, _) =>
            called = true
            throw new Refresh.ActMiss("Add to album is absent", confirmed = false)
          )
        val (_, result) = exec(
          session, "agent", "add-to-album",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest"
        )
        called shouldBe true
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery one-click")
            text should not include ("refresh-only")
            field(value, "recovery") shouldBe Json.Str("one-click")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "fail one-click on New album and click nothing" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tNew album"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onPut = (_, _, _) => fail("opened"))
        val (_, result) = exec(
          session, "agent", "add-to-album",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "New album"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery one-click")
            text should include ("New album is not a destination")
            text should not include ("refresh-only")
            field(value, "recovery") shouldBe Json.Str("one-click")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false
        released(places)

  it should "write the confirmed destination item and adjust an error line" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\tBea",
        "album\talbDst\t1\tDest Title",
        "error\talbDst\t1\t1\t0",
        "item\talbDst\tkeep.jpg\t2010-07-02\tkeyKeep\tAda",
        "album\talbKeep\t0\tKeep Title"
      )
      withAct(archive): (session, places, _) =>
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onPut = (item, destination, current) =>
            current shouldBe "pic.jpg"
            item.key shouldBe "keyPic"
            destination.title shouldBe "Dest Title"
            Catalog.Item("ignored.jpg", Some(LocalDate.of(2011, 1, 2)), "keyNew", None)
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session, "agent", "add-to-album",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"
        )
        result match
          case Result.Ok(text, value) =>
            text shouldBe "add-to-album \"pic.jpg\" \"Dest Title\"\n"
            text should not include ("albDst")
            text should not include ("keyNew")
            text should not include ("keyPic")
            field(value, "action") shouldBe Json.Str("add-to-album")
            field(value, "title") shouldBe Json.Str("Dest Title")
            field(value, "catalog") shouldBe Json.Str("updated")
            field(value, "albumId") shouldBe Json.Str("albDst")
            field(value, "itemKey") shouldBe Json.Str("keyNew")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalogBody(
          "album\talbSrc\t1\tSource Title",
          "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\tBea",
          "album\talbDst\t2\tDest Title",
          "error\talbDst\t2\t2\t0",
          "item\talbDst\tkeep.jpg\t2010-07-02\tkeyKeep\tAda",
          "item\talbDst\tpic.jpg\t2011-01-02\tkeyNew\tBea",
          "album\talbKeep\t0\tKeep Title"
        )
        JFiles.readString(catalogFile.toPath) should not include ("unset")
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        requests(archive).exists shouldBe false
        places.directory.exists shouldBe false
        File(places.directory, "status.txt").exists shouldBe false
        File(places.directory, "albums.tsv").exists shouldBe false
        File(places.directory, "parts").exists shouldBe false
        closed shouldBe true
        released(places)

  it should "write no error line when the destination has none" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\tBea",
        "album\talbDst\t0\tDest Title",
        "album\talbKeep\t0\tKeep Title"
      )
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onPut = (_, _, _) =>
            Catalog.Item("pic.jpg", Some(LocalDate.of(2011, 1, 2)), "keyNew", None)
          )
        val (_, result) = exec(
          session, "agent", "add-to-album",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"
        )
        result match
          case Result.Ok(text, _) =>
            text shouldBe "add-to-album \"pic.jpg\" \"Dest Title\"\n"
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalogBody(
          "album\talbSrc\t1\tSource Title",
          "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\tBea",
          "album\talbDst\t1\tDest Title",
          "item\talbDst\tpic.jpg\t2011-01-02\tkeyNew\tBea",
          "album\talbKeep\t0\tKeep Title"
        )

  it should "stay refresh-only when the live read is not the clean key" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onPut = (_, _, _) =>
            throw new Refresh.ActMiss("live key differs", confirmed = true)
          )
        val (_, result) = exec(
          session, "agent", "add-to-album",
          "--title", "2010/07 ", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Keep Title"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery refresh-only")
            text should include ("catalog unchanged")
            text should include ("live key differs")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "albumId") shouldBe Json.Str("alb07")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) should not include ("unset")
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        places.directory.exists shouldBe false

  it should "report one-click when Add to album is absent" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onPut = (_, _, _) =>
            throw new Refresh.ActMiss("Add to album is absent AF1QipLEAK", confirmed = false)
          )
        val (_, result) = exec(
          session, "agent", "add-to-album",
          "--title", "2010/07 ", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Keep Title"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text shouldBe
              """failed add-to-album
                |filename "pic.jpg"
                |step More options, then Add to album, then the existing album
                |recovery one-click
                |catalog unchanged
                |Add to album is absent 2010/07 
                |""".stripMargin
            text should not include ("AF1Qip")
            text should not include ("refresh-only")
            field(value, "step") shouldBe Json.Str("More options, then Add to album, then the existing album")
            field(value, "recovery") shouldBe Json.Str("one-click")
            field(value, "albumId") shouldBe Json.Str("alb07")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog

  it should "leave the catalog unchanged when the new key is already there" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t1\tDest Title",
        "item\talbDst\tkeep.jpg\t2010-07-02\tkeyNew\t-"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onPut = (_, _, _) =>
            Catalog.Item("pic.jpg", None, "keyNew", None)
          )
        val (_, result) = exec(
          session, "agent", "add-to-album",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"
        )
        result match
          case Result.Failed(text, _, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery refresh-only")
            text should include ("repeated item key")
            text should include ("catalog unchanged")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false

  it should "stay refresh-only when the catalog read throws after confirm" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onPut = (_, _, _) =>
            catalogFile.setReadable(false, false)
            Catalog.Item("pic.jpg", None, "keyNew", None)
          )
        try
          val (_, result) = exec(
            session, "agent", "add-to-album",
            "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"
          )
          result match
            case Result.Failed(text, value, message) =>
              message shouldBe Refresh.MutationFailed
              text should include ("recovery refresh-only")
              text should include ("catalog unchanged")
              field(value, "recovery") shouldBe Json.Str("refresh-only")
            case other =>
              fail(other.toString)
          catalogFile.setReadable(true, false)
          JFiles.readString(catalogFile.toPath) shouldBe catalog
          File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        finally
          catalogFile.setReadable(true, false)

  it should "report already running without opening the browser" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        places.directory.mkdirs()
        JFiles.writeString(File(places.directory, "status.txt").toPath, "phase=items\nAF1QipBusy\n")
        Refresh.claim(places.lock) match
          case Refresh.Claim.Owned(channel, lock) =>
            try
              Refresh.openAct = _ => new RenameFake((_, _) => fail("opened while busy"))
              val (_, result) = exec(
                session, "agent", "add-to-album",
                "--title", "2010/07 ", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Keep Title"
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
              JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
            finally
              Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  "remove-from-album" should "drop the item and adjust an error line" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t2\tSource Title",
        "owner\talbSrc\tAda",
        "error\talbSrc\t2\t2\t1",
        "unset\talbSrc\t2010-07-09\tkeyUnset\t-",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\tBea",
        "album\talbKeep\t0\tKeep Title"
      )
      withAct(archive): (session, places, _) =>
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onTake = (album, item) =>
            album.id shouldBe "albSrc"
            item.key shouldBe "keyPic"
            item.uploader shouldBe Some("Bea")
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session, "agent", "remove-from-album",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg"
        )
        result match
          case Result.Ok(text, value) =>
            text shouldBe "remove-from-album \"pic.jpg\"\n"
            text should not include ("albSrc")
            text should not include ("keyPic")
            field(value, "action") shouldBe Json.Str("remove-from-album")
            field(value, "title") shouldBe Json.Str("Source Title")
            field(value, "catalog") shouldBe Json.Str("updated")
            field(value, "albumId") shouldBe Json.Str("albSrc")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalogBody(
          "album\talbSrc\t1\tSource Title",
          "owner\talbSrc\tAda",
          "error\talbSrc\t1\t1\t1",
          "unset\talbSrc\t2010-07-09\tkeyUnset\t-",
          "album\talbKeep\t0\tKeep Title"
        )
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        requests(archive).exists shouldBe false
        places.directory.exists shouldBe false
        closed shouldBe true
        released(places)

  it should "report one-click and click nothing when the dialog is trash" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onTake = (_, _) =>
            throw new Refresh.ActMiss("trash dialog", confirmed = false)
          )
        val (_, result) = exec(
          session, "agent", "remove-from-album",
          "--title", "2010/07 ", "--item", "keyPic", "--current", "pic.jpg"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text shouldBe
              """failed remove-from-album
                |filename "pic.jpg"
                |step More options, then Remove from album, then Remove
                |recovery one-click
                |catalog unchanged
                |trash dialog
                |""".stripMargin
            text should not include ("refresh-only")
            field(value, "step") shouldBe Json.Str("More options, then Remove from album, then Remove")
            field(value, "recovery") shouldBe Json.Str("one-click")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "stay refresh-only when the source key is still present" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onTake = (_, _) =>
            throw new Refresh.ActMiss("source key is present", confirmed = true)
          )
        val (_, result) = exec(
          session, "agent", "remove-from-album",
          "--title", "2010/07 ", "--item", "keyPic", "--current", "pic.jpg"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery refresh-only")
            text should include ("catalog unchanged")
            text should include ("source key is present")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false

  "move-item" should "refuse an owner line before Chrome opens" in :
    refuseMove(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title",
        "owner\talbDst\tAda"
      ),
      "destination has an owner"
    )

  it should "refuse auto-add before Chrome opens" in :
    refuseMove(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title",
        "auto-add\talbDst"
      ),
      "destination is auto-add"
    )

  it should "refuse unread names before Chrome opens" in :
    refuseMove(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t2\tDest Title",
        "error\talbDst\t1\t2\t1",
        "unset\talbDst\t2010-07-09\tkeyUnset\t-"
      ),
      "destination has unread names"
    )

  it should "refuse an excepted stem before Chrome opens" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      val day: File = File(archive, "2010/07/01")
      day.mkdirs()
      JFiles.write(File(day, "pic.jpg").toPath, Array.emptyByteArray)
      JFiles.writeString(File(archive, "Dest.toml").toPath, "include = [\"2010/07/01 except pic\"]\n")
      withAct(archive): (session, places, _) =>
        var called = false
        Refresh.openAct = _ =>
          called = true
          new RenameFake((_, _) => (), onPut = (_, _, _) => fail("put"), onTake = (_, _) => fail("take"))
        usage(
          session, "agent", "move-item",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest"
        ) shouldBe "destination excepts \"pic\""
        called shouldBe false
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "move the item in one session when both halves are clean" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t2\tSource Title",
        "owner\talbSrc\tAda",
        "error\talbSrc\t2\t2\t1",
        "unset\talbSrc\t2010-07-09\tkeyUnset\t-",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\tBea",
        "album\talbDst\t1\tDest Title",
        "error\talbDst\t1\t1\t0",
        "item\talbDst\tstay.jpg\t2010-07-03\tkeyStay\tAda",
        "album\talbKeep\t1\tKeep Title",
        "item\talbKeep\tother.jpg\t2010-08-01\tkeyKeep\t-",
        "owner\talbKeep\tZoe"
      )
      withAct(archive): (session, places, _) =>
        var opened = 0
        var closed = false
        var order = ""
        Refresh.openAct = _ =>
          opened += 1
          new RenameFake((_, _) => (), onPut = (item, destination, current) =>
            order = order + "put"
            current shouldBe "pic.jpg"
            item.key shouldBe "keyPic"
            item.uploader shouldBe Some("Bea")
            destination.title shouldBe "Dest Title"
            Catalog.Item("ignored.jpg", Some(LocalDate.of(2011, 1, 2)), "keyNew", None)
          , onTake = (album, item) =>
            order = order + "take"
            album.id shouldBe "albSrc"
            item.key shouldBe "keyPic"
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session, "agent", "move-item",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"
        )
        opened shouldBe 1
        order shouldBe "puttake"
        closed shouldBe true
        result match
          case Result.Ok(text, value) =>
            text shouldBe "move-item \"pic.jpg\" \"Source Title\" \"Dest Title\"\n"
            text should not include ("albSrc")
            text should not include ("albDst")
            text should not include ("keyPic")
            text should not include ("keyNew")
            field(value, "action") shouldBe Json.Str("move-item")
            field(value, "title") shouldBe Json.Str("Source Title")
            field(value, "destination") shouldBe Json.Str("Dest Title")
            field(value, "catalog") shouldBe Json.Str("updated")
            field(value, "albumId") shouldBe Json.Str("albSrc")
            field(value, "destinationId") shouldBe Json.Str("albDst")
            field(value, "itemKey") shouldBe Json.Str("keyNew")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalogBody(
          "album\talbSrc\t1\tSource Title",
          "owner\talbSrc\tAda",
          "error\talbSrc\t1\t1\t1",
          "unset\talbSrc\t2010-07-09\tkeyUnset\t-",
          "album\talbDst\t2\tDest Title",
          "error\talbDst\t2\t2\t0",
          "item\talbDst\tstay.jpg\t2010-07-03\tkeyStay\tAda",
          "item\talbDst\tpic.jpg\t2011-01-02\tkeyNew\tBea",
          "album\talbKeep\t1\tKeep Title",
          "item\talbKeep\tother.jpg\t2010-08-01\tkeyKeep\t-",
          "owner\talbKeep\tZoe"
        )
        JFiles.readString(catalogFile.toPath) should not include ("ignored.jpg")
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        requests(archive).exists shouldBe false
        places.directory.exists shouldBe false
        released(places)

  it should "leave the catalog unchanged when remove fails after a clean add" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\tBea",
        "album\talbDst\t0\tDest Title",
        "album\talbKeep\t0\tKeep Title"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onPut = (_, _, _) =>
            Catalog.Item("pic.jpg", Some(LocalDate.of(2011, 1, 2)), "keyNew", None)
          , onTake = (_, _) =>
            throw new Refresh.ActMiss("trash dialog", confirmed = false)
          )
        val (_, result) = exec(
          session, "agent", "move-item",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("\"Source Title\"")
            text should include ("\"Dest Title\"")
            text should include ("recovery refresh-only")
            text should include ("catalog unchanged")
            text should include ("trash dialog")
            text should not include ("one-click")
            text should not include ("keyNew")
            text should not include ("albDst")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "step") shouldBe Json.Str("More options, then Remove from album, then Remove")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        JFiles.readString(catalogFile.toPath) should not include ("keyNew")
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false

  it should "not remove when the add misses before confirm" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        var took = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onPut = (_, _, _) =>
            throw new Refresh.ActMiss("Add to album is absent", confirmed = false)
          , onTake = (_, _) =>
            took = true
          )
        val (_, result) = exec(
          session, "agent", "move-item",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"
        )
        took shouldBe false
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text shouldBe
              """failed move-item
                |filename "pic.jpg"
                |album "Source Title"
                |destination "Dest Title"
                |step More options, then Add to album, then the existing album
                |recovery one-click
                |catalog unchanged
                |Add to album is absent
                |""".stripMargin
            text should include ("\"Source Title\"")
            text should include ("\"Dest Title\"")
            text should not include ("refresh-only")
            field(value, "recovery") shouldBe Json.Str("one-click")
            field(value, "albumId") shouldBe Json.Str("albSrc")
            field(value, "destinationId") shouldBe Json.Str("albDst")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "not remove when the add confirm is not a clean key" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, _, _) =>
        var took = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onPut = (_, _, _) =>
            throw new Refresh.ActMiss("live key differs", confirmed = true)
          , onTake = (_, _) =>
            took = true
          )
        val (_, result) = exec(
          session, "agent", "move-item",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"
        )
        took shouldBe false
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("\"Source Title\"")
            text should include ("\"Dest Title\"")
            text should include ("recovery refresh-only")
            text should include ("catalog unchanged")
            text should include ("live key differs")
            text should not include ("one-click")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "step") shouldBe Json.Str("More options, then Add to album, then the existing album")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false

  it should "stay refresh-only when the catalog read throws after both halves" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, _, _) =>
        var took = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onPut = (_, _, _) =>
            Catalog.Item("pic.jpg", None, "keyNew", None)
          , onTake = (_, _) =>
            took = true
            catalogFile.setReadable(false, false)
          )
        try
          val (_, result) = exec(
            session, "agent", "move-item",
            "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"
          )
          took shouldBe true
          result match
            case Result.Failed(text, value, message) =>
              message shouldBe Refresh.MutationFailed
              text should include ("\"Source Title\"")
              text should include ("\"Dest Title\"")
              text should include ("recovery refresh-only")
              text should include ("catalog unchanged")
              text should not include ("one-click")
              field(value, "recovery") shouldBe Json.Str("refresh-only")
            case other =>
              fail(other.toString)
          catalogFile.setReadable(true, false)
          JFiles.readString(catalogFile.toPath) shouldBe catalog
          File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        finally
          catalogFile.setReadable(true, false)

  it should "report already running without opening the browser" in :
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        places.directory.mkdirs()
        JFiles.writeString(File(places.directory, "status.txt").toPath, "phase=items\nAF1QipBusy\n")
        Refresh.claim(places.lock) match
          case Refresh.Claim.Owned(channel, lock) =>
            try
              Refresh.openAct = _ => new RenameFake((_, _) => fail("opened while busy"))
              val (_, result) = exec(
                session, "agent", "move-item",
                "--title", "2010/07 ", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Keep Title"
              )
              result match
                case Result.Failed(text, value, message) =>
                  message shouldBe Refresh.AlreadyRunning
                  message shouldBe "already running"
                  Cli.processCode(result) shouldBe 2
                  text should startWith ("already running\n")
                  text should include ("phase=items")
                  text should not include ("AF1Qip")
                  value shouldBe Json.Null
                case other =>
                  fail(other.toString)
              JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
            finally
              Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  "delete-item" should "refuse no owner line or an uploader before Chrome opens" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbOwn\t1\tOwned Name",
        "owner\talbOwn\tAda",
        "item\talbOwn\tpic.jpg\t2010-07-01\tkeyOwn\t-",
        "album\talbUp\t1\tUploaded Name",
        "item\talbUp\tpic.jpg\t2010-07-02\tkeyUp\tBea",
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        var called = false
        Refresh.openAct = _ =>
          called = true
          new RenameFake((_, _) => (), onTrash = (_, _) => fail("trash"))
        val owner: String = usage(
          session, "agent", "delete-item", "--title", "Owned Name", "--item", "keyOwn", "--current", "pic.jpg"
        )
        owner shouldBe "no owner line"
        owner should not include ("owned")
        val uploader: String = usage(
          session, "agent", "delete-item", "--title", "Uploaded Name", "--item", "keyUp", "--current", "pic.jpg"
        )
        uploader shouldBe "uploader is not -"
        usage(
          session, "agent", "delete-item", "--title", "Source Title", "--item", "keyPic", "--current", "other.jpg"
        ) shouldBe "current does not match \"pic.jpg\" \"other.jpg\""
        called shouldBe false
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/.download").exists shouldBe false
        places.directory.exists shouldBe false

  it should "remove the item line when the key is already gone and not record a trash click" in :
    withArchive: archive =>
      val catalogFile: File = deleteCatalog(archive)
      withAct(archive): (session, places, _) =>
        var clicks: List[String] = Nil
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onPut = (_, _, _) => fail("put"), onTake = (_, _) => fail("take"), onTrash = (album, item) =>
            album.id shouldBe "albSrc"
            item.key shouldBe "keyPic"
            item.uploader shouldBe None
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session, "agent", "delete-item",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg"
        )
        clicks shouldBe Nil
        closed shouldBe true
        result match
          case Result.Ok(text, value) =>
            text shouldBe "delete-item \"pic.jpg\"\n"
            text should not include ("albSrc")
            text should not include ("keyPic")
            field(value, "action") shouldBe Json.Str("delete-item")
            field(value, "catalog") shouldBe Json.Str("updated")
            field(value, "albumId") shouldBe Json.Str("albSrc")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe deletedBody
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        File(archive, ".gphoto-sync/.download").exists shouldBe false
        File(archive, ".gphoto-sync/results.tsv").exists shouldBe false
        requests(archive).exists shouldBe false
        File(archive, "2010").exists shouldBe false
        places.directory.exists shouldBe false
        released(places)

  it should "remove the item line when the confirm leaves the key gone" in :
    withArchive: archive =>
      val catalogFile: File = deleteCatalog(archive)
      withAct(archive): (session, _, _) =>
        var clicks: List[String] = Nil
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onTrash = (_, _) =>
            clicks = clicks :+ "trash"
          )
        val (_, result) = exec(
          session, "agent", "delete-item",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg"
        )
        clicks shouldBe List("trash")
        result match
          case Result.Ok(text, _) =>
            text shouldBe "delete-item \"pic.jpg\"\n"
            text should not include ("albSrc")
            text should not include ("keyPic")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe deletedBody
        File(archive, ".gphoto-sync/.download").exists shouldBe false
        File(archive, ".gphoto-sync/results.tsv").exists shouldBe false

  it should "report one-click and publish nothing when the control is Delete forever" in :
    withArchive: archive =>
      val catalogFile: File = deleteCatalog(archive)
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        var clicks: List[String] = Nil
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onTrash = (_, _) =>
            throw new Refresh.ActMiss("Delete forever", confirmed = false)
          )
        val (_, result) = exec(
          session, "agent", "delete-item",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg"
        )
        clicks shouldBe Nil
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            message shouldBe "mutation failed"
            text shouldBe
              """failed delete-item
                |filename "pic.jpg"
                |step Move to trash
                |recovery one-click
                |catalog unchanged
                |Delete forever
                |""".stripMargin
            text should not include ("refresh-only")
            field(value, "step") shouldBe Json.Str("Move to trash")
            field(value, "recovery") shouldBe Json.Str("one-click")
            field(value, "catalog") shouldBe Json.Str("unchanged")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/.download").exists shouldBe false
        places.directory.exists shouldBe false
        val (code, out, err) = shellOf(
          session,
          "agent delete-item --title \"Source Title\" --item keyPic --current pic.jpg"
        )
        code shouldBe 0
        out should include ("Delete forever")
        out should include ("recovery one-click")
        out should not include ("albSrc")
        out should not include ("keyPic")
        err should not include ("mutation failed")
        err should not include ("Photo Archive Tool:")
        JFiles.readString(catalogFile.toPath) shouldBe catalog

  it should "stay refresh-only when the key is still present after the confirm" in :
    withArchive: archive =>
      val catalogFile: File = deleteCatalog(archive)
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onTrash = (_, _) =>
            throw new Refresh.ActMiss("source key is present", confirmed = true)
          )
        val (_, result) = exec(
          session, "agent", "delete-item",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery refresh-only")
            text should include ("catalog unchanged")
            text should include ("source key is present")
            text should not include ("one-click")
            field(value, "step") shouldBe Json.Str("Move to trash")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/.download").exists shouldBe false

  it should "stay refresh-only when the catalog read throws after a clean return" in :
    withArchive: archive =>
      val catalogFile: File = deleteCatalog(archive)
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onTrash = (_, _) =>
            catalogFile.setReadable(false, false)
          )
        try
          val (_, result) = exec(
            session, "agent", "delete-item",
            "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg"
          )
          result match
            case Result.Failed(text, value, message) =>
              message shouldBe Refresh.MutationFailed
              text should include ("recovery refresh-only")
              text should include ("catalog unchanged")
              text should not include ("one-click")
              field(value, "recovery") shouldBe Json.Str("refresh-only")
            case other =>
              fail(other.toString)
          catalogFile.setReadable(true, false)
          JFiles.readString(catalogFile.toPath) shouldBe catalog
          File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        finally
          catalogFile.setReadable(true, false)

  it should "report already running without opening the browser" in :
    withArchive: archive =>
      val catalogFile: File = deleteCatalog(archive)
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        places.directory.mkdirs()
        JFiles.writeString(File(places.directory, "status.txt").toPath, "phase=items\nAF1QipBusy\n")
        Refresh.claim(places.lock) match
          case Refresh.Claim.Owned(channel, lock) =>
            try
              Refresh.openAct = _ => new RenameFake((_, _) => fail("opened while busy"))
              val (_, result) = exec(
                session, "agent", "delete-item",
                "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg"
              )
              result match
                case Result.Failed(text, value, message) =>
                  message shouldBe Refresh.AlreadyRunning
                  message shouldBe "already running"
                  Cli.processCode(result) shouldBe 2
                  text should startWith ("already running\n")
                  text should include ("phase=items")
                  text should not include ("AF1Qip")
                  value shouldBe Json.Null
                case other =>
                  fail(other.toString)
              JFiles.readString(catalogFile.toPath) shouldBe catalog
              File(archive, ".gphoto-sync/.download").exists shouldBe false
            finally
              Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  "upload-item" should "refuse a path before Chrome opens" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      val outside: File = JFiles.createTempFile("upload-outside", ".jpg").toFile
      val sibling: File = File(archive.getParentFile, archive.getName + "-extra/2010/07/01/pic.jpg")
      sibling.getParentFile.mkdirs()
      JFiles.write(sibling.toPath, Array.emptyByteArray)
      touch(File(archive, "2010/02/31"), "pic.jpg")
      touch(File(archive, "2010/07/01"), "pic.png")
      touch(File(archive, "2010/07/01"), "pic")
      touch(archive, "notes.jpg")
      withAct(archive): (session, places, _) =>
        var called = false
        Refresh.openAct = _ =>
          called = true
          new RenameFake((_, _) => (), onUpload = (_, _, _) => fail("opened"))
        Cli.parse(List("agent", "upload-item", "--dest", "Dest Title")) shouldBe Cli.Usage("missing path")
        usage(session, "agent", "upload-item", "2010/07/01/pic.jpg") shouldBe "missing dest"
        usage(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/missing.jpg") shouldBe
          "no such file"
        usage(session, "agent", "upload-item", "--dest", "Dest Title", outside.getCanonicalPath) shouldBe
          "path is outside the archive"
        usage(session, "agent", "upload-item", "--dest", "Dest Title", sibling.getCanonicalPath) shouldBe
          "path is outside the archive"
        usage(session, "agent", "upload-item", "--dest", "Dest Title", "notes.jpg") shouldBe "not a picture path"
        usage(session, "agent", "upload-item", "--dest", "Dest Title", "2010/02/31/pic.jpg") shouldBe
          "not a picture path"
        usage(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/pic.png") shouldBe
          "extension is not uploadable"
        usage(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/pic") shouldBe
          "extension is not uploadable"
        called shouldBe false
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false
      outside.delete()

  it should "refuse a catalog item, a sidecar mark, or a destination rule before Chrome opens" in :
    uploadRefuse(
      Seq(
        "album\talbOther\t1\tOther",
        "item\talbOther\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title"
      ),
      "catalog item exists"
    )
    uploadRefuse(
      Seq(
        "album\talbOther\t1\tOther",
        "item\talbOther\tIMG_pic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title"
      ),
      "catalog item exists"
    )
    uploadRefuse(
      Seq(
        "album\talbOther\t1\tOther",
        "item\talbOther\tpic.cr2\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title"
      ),
      "catalog item exists"
    )
    // A non-digit toml in a day folder is an album file, so the sidecar stem starts with a digit.
    uploadRefuse(
      Seq("album\talbDst\t0\tDest Title"),
      "already in google",
      relative = "2010/07/01/2412.jpg",
      prepare = archive =>
        JFiles.writeString(
          File(archive, "2010/07/01/2412.toml").toPath,
          Metadata.encode(Metadata(`google-media-item-id` = Some("media-1")))
        )
    )
    uploadRefuse(
      Seq("album\talbDst\t0\tDest Title"),
      "already in google",
      relative = "2010/07/01/2412.jpg",
      prepare = archive =>
        JFiles.writeString(
          File(archive, "2010/07/01/2412.toml").toPath,
          Metadata.encode(Metadata(`already-in-google` = Some(true)))
        )
    )
    uploadRefuse(
      Seq(
        "album\talbDst\t0\tDest Title",
        "owner\talbDst\tAda"
      ),
      "destination has an owner"
    )
    uploadRefuse(
      Seq(
        "album\talbDst\t0\tDest Title",
        "auto-add\talbDst"
      ),
      "destination is auto-add"
    )
    uploadRefuse(
      Seq(
        "album\talbDst\t2\tDest Title",
        "error\talbDst\t1\t2\t1",
        "unset\talbDst\t2010-07-09\tkeyUnset\t-"
      ),
      "destination has unread names"
    )
    uploadRefuse(
      Seq(
        "album\talbOne\t0\tDest Title",
        "album\talbTwo\t0\tDest Title"
      ),
      "more than one album titled \"Dest Title\""
    )
    uploadRefuse(
      Seq(
        "album\talbDst\t0\tDest Title",
        "album\talbOther\t0\tOther"
      ),
      "catalog title does not match",
      args = Seq("--dest-id", "albOther")
    )
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, "2010/07/01"), "pic.jpg")
      JFiles.writeString(File(archive, "Dest.toml").toPath, "include = [\"2010/07/01 except pic\"]\n")
      withAct(archive): (session, places, _) =>
        var called = false
        Refresh.openAct = _ =>
          called = true
          new RenameFake((_, _) => (), onUpload = (_, _, _) => fail("opened"))
        usage(session, "agent", "upload-item", "--dest", "Dest", "2010/07/01/pic.jpg") shouldBe
          "destination excepts \"pic\""
        called shouldBe false
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "resolve a video beside a still to the v stem" in :
    uploadRefuse(
      Seq(
        "album\talbOther\t1\tOther",
        "item\talbOther\tVID_1000.mp4\t2010-07-02\tkeyVid\t-",
        "album\talbDst\t0\tDest Title"
      ),
      "catalog item exists",
      prepare = archive => touch(File(archive, "2010/07/02"), "1000.jpg"),
      relative = "2010/07/02/VID_1000.mp4"
    )
    uploadRefuse(
      Seq("album\talbDst\t0\tDest Title"),
      "already in google",
      relative = "2010/07/02/VID_1000.mp4",
      prepare = archive =>
        touch(File(archive, "2010/07/02"), "1000.jpg")
        JFiles.writeString(
          File(archive, "2010/07/02/1000v.toml").toPath,
          Metadata.encode(Metadata(`google-media-item-id` = Some("media-v")))
        )
    )
    openedUpload(
      Seq(
        "album\talbOther\t1\tOther",
        "item\talbOther\t1000.jpg\t2010-07-02\tkeyStill\t-",
        "album\talbDst\t0\tDest Title"
      ),
      relative = "2010/07/02/VID_1000.mp4",
      prepare = archive => touch(File(archive, "2010/07/02"), "1000.jpg")
    )
    openedUpload(
      Seq("album\talbDst\t0\tDest Title"),
      relative = "2010/07/02/VID_1000.mp4",
      prepare = archive =>
        touch(File(archive, "2010/07/02"), "1000.jpg")
        JFiles.writeString(
          File(archive, "2010/07/02/1000.toml").toPath,
          Metadata.encode(Metadata(`google-media-item-id` = Some("media-still")))
        )
    )
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, "2010/07/02"), "1000.jpg")
      touch(File(archive, "2010/07/02"), "VID_1000.mp4")
      JFiles.writeString(File(archive, "Dest.toml").toPath, "include = [\"2010/07/02 except 1000v\"]\n")
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onUpload = (_, _, _) => fail("opened"))
        usage(session, "agent", "upload-item", "--dest", "Dest", "2010/07/02/VID_1000.mp4") shouldBe
          "destination excepts \"1000v\""
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false
    withArchive: archive =>
      writeCatalog(archive, "album\talbDst\t0\tDest")
      touch(File(archive, "2010/07/02"), "1000.jpg")
      touch(File(archive, "2010/07/02"), "VID_1000.mp4")
      JFiles.writeString(File(archive, "Dest.toml").toPath, "include = [\"2010/07/02 except 1000\"]\n")
      withAct(archive): (session, places, _) =>
        var called = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onUpload = (_, _, current) =>
            called = true
            current shouldBe "VID_1000.mp4"
            throw new Refresh.ActMiss("Add photos is absent", confirmed = false)
          )
        val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest", "2010/07/02/VID_1000.mp4")
        called shouldBe true
        result match
          case Result.Failed(_, _, message) =>
            message shouldBe Refresh.MutationFailed
          case other =>
            fail(other.toString)
        places.directory.exists shouldBe false

  it should "open for a file the catalog does not already have" in :
    openedUpload(Seq(
      "album\talbOther\t1\tOther",
      "item\talbOther\tpic.jpg\t2010-07-02\tkeyPic\t-",
      "album\talbDst\t0\tDest Title"
    ))
    openedUpload(Seq(
      "album\talbOther\t1\tOther",
      "item\talbOther\tother.jpg\t2010-07-01\tkeyOther\t-",
      "album\talbDst\t0\tDest Title"
    ))
    openedUpload(Seq(
      "album\talbOther\t1\tOther",
      "item\talbOther\tIMG_IMG_pic.jpg\t2010-07-01\tkeyPic\t-",
      "album\talbDst\t0\tDest Title"
    ))
    openedUpload(
      Seq("album\talbDst\t0\tDest Title"),
      relative = "2010/07/01/2412.jpg",
      prepare = archive =>
        JFiles.writeString(
          File(archive, "2010/07/01/2412.toml").toPath,
          Metadata.encode(Metadata(`already-in-google` = Some(false), `google-media-item-id` = Some("")))
        )
    )
    openedUpload(Seq("album\talbDst\t0\tDest Title"), relative = "2010/07/01/pic.JPG")
    openedUpload(Seq("album\talbDst\t0\tDest Title"), relative = "2010/07/01/clip.3gp")

  it should "write the confirmed item from the archive name and the read" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbDst\t1\tDest Title",
        "error\talbDst\t1\t1\t0",
        "item\talbDst\tkeep.jpg\t2010-07-02\tkeyKeep\tAda",
        "album\talbKeep\t0\tKeep Title"
      )
      touch(File(archive, "2010/07/01"), "pic.jpg")
      withAct(archive): (session, places, _) =>
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onUpload = (destination, file, current) =>
            current shouldBe "pic.jpg"
            file.getName shouldBe "pic.jpg"
            destination.title shouldBe "Dest Title"
            Catalog.Item("ignored.jpg", Some(LocalDate.of(2011, 1, 2)), "keyNew", Some("Ida"))
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/pic.jpg")
        result match
          case Result.Ok(text, value) =>
            text shouldBe "upload-item \"pic.jpg\" \"Dest Title\"\n"
            text should not include ("albDst")
            text should not include ("keyNew")
            field(value, "action") shouldBe Json.Str("upload-item")
            field(value, "title") shouldBe Json.Str("Dest Title")
            field(value, "catalog") shouldBe Json.Str("updated")
            field(value, "albumId") shouldBe Json.Str("albDst")
            field(value, "itemKey") shouldBe Json.Str("keyNew")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalogBody(
          "album\talbDst\t2\tDest Title",
          "error\talbDst\t2\t2\t0",
          "item\talbDst\tkeep.jpg\t2010-07-02\tkeyKeep\tAda",
          "item\talbDst\tpic.jpg\t2011-01-02\tkeyNew\tIda",
          "album\talbKeep\t0\tKeep Title"
        )
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        places.directory.exists shouldBe false
        File(places.directory, "status.txt").exists shouldBe false
        closed shouldBe true
        released(places)

  it should "accept an absolute path and write no error line when the read has no date" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest Title")
      val picture: File = File(archive, "2010/07/01/pic.jpg")
      picture.getParentFile.mkdirs()
      JFiles.write(picture.toPath, Array.emptyByteArray)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onUpload = (_, _, current) =>
            current shouldBe "pic.jpg"
            Catalog.Item("ignored.jpg", None, "keyNew", None)
          )
        val (_, result) = exec(
          session, "agent", "upload-item", "--dest", "Dest Title", picture.getCanonicalPath
        )
        result match
          case Result.Ok(text, _) =>
            text shouldBe "upload-item \"pic.jpg\" \"Dest Title\"\n"
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalogBody(
          "album\talbDst\t1\tDest Title",
          "item\talbDst\tpic.jpg\t-\tkeyNew\t-"
        )
        JFiles.readString(catalogFile.toPath) should not include ("error")
        JFiles.readString(catalogFile.toPath) should not include ("unset")
        places.directory.exists shouldBe false
        released(places)

  it should "fail one-click when the chooser was not reached" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, "2010/07/01"), "pic.jpg")
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onUpload = (_, _, _) =>
            throw new Refresh.ActMiss("Add photos is absent", confirmed = false)
          )
        val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/pic.jpg")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("failed upload-item")
            text should include ("filename \"pic.jpg\"")
            text should include ("step Add photos or Select photos, then Select from computer")
            text should include ("recovery one-click")
            text should include ("Add photos is absent")
            text should not include ("refresh-only")
            text should not include ("albDst")
            field(value, "action") shouldBe Json.Str("upload-item")
            field(value, "title") shouldBe Json.Str("Dest Title")
            field(value, "recovery") shouldBe Json.Str("one-click")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "itemKey") shouldBe Json.Str("")
            field(value, "albumId") shouldBe Json.Str("albDst")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false
        released(places)

  it should "stay refresh-only when the file was chosen and the read is not clean" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, "2010/07/01"), "pic.jpg")
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onUpload = (_, _, _) =>
            throw new Refresh.ActMiss("live key differs", confirmed = true)
          )
        val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/pic.jpg")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery refresh-only")
            text should include ("catalog unchanged")
            text should not include ("one-click")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "stay one-click when the upload throws before the file is chosen" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, "2010/07/01"), "pic.jpg")
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onUpload = (_, _, _) =>
            throw IllegalStateException("chooser missed")
          )
        val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/pic.jpg")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery one-click")
            text should include ("chooser missed")
            text should not include ("refresh-only")
            field(value, "recovery") shouldBe Json.Str("one-click")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog

  it should "stay refresh-only when the catalog write fails after the file was chosen" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, "2010/07/01"), "pic.jpg")
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onUpload = (_, _, _) =>
            catalogFile.setReadable(false, false)
            Catalog.Item("pic.jpg", Some(LocalDate.of(2011, 1, 2)), "keyNew", None)
          )
        try
          val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/pic.jpg")
          result match
            case Result.Failed(text, value, message) =>
              message shouldBe Refresh.MutationFailed
              text should include ("recovery refresh-only")
              text should include ("catalog unchanged")
              field(value, "recovery") shouldBe Json.Str("refresh-only")
            case other =>
              fail(other.toString)
          catalogFile.setReadable(true, false)
          JFiles.readString(catalogFile.toPath) shouldBe catalog
          File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        finally
          catalogFile.setReadable(true, false)

  it should "report already running without opening the browser" in :
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, "2010/07/01"), "pic.jpg")
      withAct(archive): (session, places, _) =>
        places.directory.mkdirs()
        JFiles.writeString(File(places.directory, "status.txt").toPath, "phase=items\nAF1QipBusy\n")
        Refresh.claim(places.lock) match
          case Refresh.Claim.Owned(channel, lock) =>
            try
              Refresh.openAct = _ => new RenameFake((_, _) => fail("opened while busy"))
              val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/pic.jpg")
              result match
                case Result.Failed(text, value, message) =>
                  message shouldBe "already running"
                  Cli.processCode(result) shouldBe 2
                  text should not include ("AF1Qip")
                  value shouldBe Json.Null
                case other =>
                  fail(other.toString)
              JFiles.readString(catalogFile.toPath) shouldBe catalog
            finally
              Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  private def uploadRefuse(
    lines: Seq[String],
    message: String,
    prepare: File => Unit = _ => (),
    args: Seq[String] = Seq.empty,
    relative: String = "2010/07/01/pic.jpg"
  ): Unit =
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, lines*)
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, relative).getParentFile, File(relative).getName)
      prepare(archive)
      withAct(archive): (session, places, _) =>
        var called = false
        Refresh.openAct = _ =>
          called = true
          new RenameFake((_, _) => (), onUpload = (_, _, _) => fail("opened"))
        val argv: Seq[String] =
          Seq("agent", "upload-item", "--dest", "Dest Title") ++ args ++ Seq(relative)
        usage(session, argv*) shouldBe message
        called shouldBe false
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  private def openedUpload(
    lines: Seq[String],
    relative: String = "2010/07/01/pic.jpg",
    prepare: File => Unit = _ => ()
  ): Unit =
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, lines*)
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, relative).getParentFile, File(relative).getName)
      prepare(archive)
      withAct(archive): (session, places, _) =>
        var called = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onUpload = (_, _, _) =>
            called = true
            throw new Refresh.ActMiss("Add photos is absent", confirmed = false)
          )
        val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest Title", relative)
        called shouldBe true
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include ("recovery one-click")
            field(value, "recovery") shouldBe Json.Str("one-click")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  private def deleteCatalog(archive: File): File =
    writeCatalog(
      archive,
      "album\talbSrc\t2\tSource Title",
      "error\talbSrc\t2\t2\t1",
      "unset\talbSrc\t2010-07-09\tkeyUnset\t-",
      "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
      "album\talbKeep\t1\tKeep Title",
      "item\talbKeep\tother.jpg\t2010-08-01\tkeyKeep\t-",
      "owner\talbKeep\tZoe"
    )

  private def deletedBody: String =
    catalogBody(
      "album\talbSrc\t1\tSource Title",
      "error\talbSrc\t1\t1\t1",
      "unset\talbSrc\t2010-07-09\tkeyUnset\t-",
      "album\talbKeep\t1\tKeep Title",
      "item\talbKeep\tother.jpg\t2010-08-01\tkeyKeep\t-",
      "owner\talbKeep\tZoe"
    )

  private def catalogBody(lines: String*): String =
    (Seq("# gphoto-catalog 2") ++ lines).mkString("", "\n", "\n")

  private def refuseMove(lines: Seq[String], message: String): Unit =
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, lines*)
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        var called = false
        Refresh.openAct = _ =>
          called = true
          new RenameFake((_, _) => (), onPut = (_, _, _) => fail("put"), onTake = (_, _) => fail("take"))
        usage(
          session, "agent", "move-item",
          "--title", "Source Title", "--item", "keyPic", "--current", "pic.jpg", "--dest", "Dest Title"
        ) shouldBe message
        called shouldBe false
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  private def rejectPut(lines: Seq[String], args: Seq[String], message: String): Unit =
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, lines*)
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onPut = (_, _, _) => fail("opened"))
        usage(session, args*) shouldBe message
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

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
    onDate: (Catalog.Item, String) => Unit = (_, _) => (),
    onDownload: (Catalog.Item, File) => Unit = (_, _) => (),
    onPut: (Catalog.Item, Catalog.Album, String) => Catalog.Item = (_, _, _) =>
      throw IllegalStateException("put"),
    onTake: (Catalog.Album, Catalog.Item) => Unit = (_, _) => (),
    onTrash: (Catalog.Album, Catalog.Item) => Unit = (_, _) => (),
    onUpload: (Catalog.Album, File, String) => Catalog.Item = (_, _, _) =>
      throw IllegalStateException("upload")
  ) extends PhotosAct:
    def albumCards(): Seq[AlbumCard] = Seq.empty
    def readOwner(card: AlbumCard): Option[String] = None
    def readAlbum(card: AlbumCard): Catalog.Album = throw IllegalStateException("read")
    def close(): Unit = ()
    def renameAlbum(album: Catalog.Album, updated: String): Unit = effect(album, updated)
    def setDescription(item: Catalog.Item, text: String): Unit = onDescription(item, text)
    def setDate(item: Catalog.Item, local: String): Unit = onDate(item, local)
    def downloadItem(item: Catalog.Item, destination: File): Unit = onDownload(item, destination)
    def putInAlbum(item: Catalog.Item, destination: Catalog.Album, current: String): Catalog.Item =
      onPut(item, destination, current)
    def takeOut(album: Catalog.Album, item: Catalog.Item): Unit = onTake(album, item)
    def trash(album: Catalog.Album, item: Catalog.Item): Unit = onTrash(album, item)
    def uploadItem(destination: Catalog.Album, file: File, current: String): Catalog.Item =
      onUpload(destination, file, current)

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
