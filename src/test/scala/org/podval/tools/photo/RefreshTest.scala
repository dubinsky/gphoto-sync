package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.io.{ByteArrayOutputStream, File}
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NFiles, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import java.time.{LocalDate, ZoneId}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

class RefreshTest extends AnyFlatSpec, Matchers:
  @targetName("outcomeCanEqual")
  private given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  private val defaultPlaces: () => Refresh.Places = Refresh.placesOf
  private val day: LocalDate = LocalDate.parse("2010-06-01")
  private val statusKeys: Set[String] = Set(
    "heartbeat", "phase", "seen", "pass", "need", "done", "total", "error"
  )

  "needsAnother" should "stop on an ok read, take three shorts, and stop a name-failure at two" in :
    import Refresh.ReadKind.*
    Refresh.needsAnother(Seq.empty) shouldBe true
    Refresh.needsAnother(Seq(Ok)) shouldBe false
    Refresh.needsAnother(Seq(Short, Ok)) shouldBe false
    Refresh.needsAnother(Seq(Short)) shouldBe true
    Refresh.needsAnother(Seq(Short, Short)) shouldBe true
    Refresh.needsAnother(Seq(Short, Short, Short)) shouldBe false
    Refresh.needsAnother(Seq(Other)) shouldBe true
    Refresh.needsAnother(Seq(Other, Short)) shouldBe false
    Refresh.needsAnother(Seq(Other, Other)) shouldBe false

  "refresh flags" should "parse incremental, full, repeated album, list, and allow-shrink" in :
    Cli.parse(List("refresh")) shouldBe Cli.Run(refreshArgs())
    Cli.parse(List("refresh", "--full")) shouldBe Cli.Run(refreshArgs(mode = RefreshPlan.Mode.Full))
    Cli.parse(List("refresh", "--album", "A", "--album=B")) shouldBe Cli.Run(refreshArgs(
      mode = RefreshPlan.Mode.Album(Seq("A", "B"))
    ))
    Cli.parse(List("refresh", "--list", "--allow-shrink", "--full")) shouldBe Cli.Run(refreshArgs(
      mode = RefreshPlan.Mode.Full,
      list = true,
      allowShrink = true
    ))
    Cli.parse(List("refresh", "--help")) match
      case Cli.Help(text) =>
        text shouldBe
          """refresh  refresh the Google Photos catalog from Chrome
            |
            |Positionals:
            |none
            |
            |Flags:
            |--full  boolean  item-read every album that is not auto-add
            |--album  repeatable  exact title; may be repeated
            |--list  boolean  print the diff and write nothing
            |--allow-shrink  boolean  skip only the rows < 250 floor""".stripMargin
      case other =>
        fail(other.toString)

  it should "reject full with album, a repeated full, and a missing album value" in :
    Cli.parse(List("refresh", "--full", "--album", "A")) shouldBe
      Cli.Usage("cannot combine '--full' and '--album'")
    Cli.parse(List("refresh", "--album", "A", "--full")) shouldBe
      Cli.Usage("cannot combine '--full' and '--album'")
    Cli.parse(List("refresh", "--full", "--full")) shouldBe Cli.Usage("repeated option '--full'")
    Cli.parse(List("refresh", "--album")) shouldBe Cli.Usage("missing value for '--album'")
    Cli.parse(List("refresh", "--album=")) shouldBe Cli.Usage("missing value for '--album'")

  "places" should "keep the lock beside the work directory" in :
    val places: Refresh.Places = defaultPlaces()
    places.directory.getName shouldBe "gphoto-sync-refresh"
    places.lock.getName shouldBe "gphoto-sync-refresh.lock"
    places.lock.getParentFile.getAbsolutePath shouldBe places.directory.getParentFile.getAbsolutePath
    places.lock.toPath.normalize.startsWith(places.directory.toPath.normalize) shouldBe false

  "a refresh" should "run in this process while serve holds its lock" in :
    withPlaces: (archive, session, places) =>
      val runtime: File = NFiles.createTempDirectory("refresh-serve").toFile
      val channel: FileChannel = FileChannel.open(
        File(runtime, "serve.lock").toPath,
        StandardOpenOption.CREATE,
        StandardOpenOption.READ,
        StandardOpenOption.WRITE
      )
      val lock = channel.tryLock()
      lock should not be null
      val previousOpen = Refresh.openUi
      val previousPlaces = Refresh.placesOf
      val ui = new Fake(Seq.empty)
      Refresh.placesOf = () => places
      Refresh.openUi = _ => ui
      try
        val parsed: Cli.Parsed = parsedOf("refresh", "--allow-shrink", "--list")
        val result: Result = Cli.executeOneShot(
          session,
          List("refresh", "--allow-shrink", "--list"),
          parsed,
          Right(runtime),
          waitMs = 250
        )
        result match
          case Result.Ok(text, _) =>
            text shouldBe "kept 0\n"
          case Result.Usage(message) =>
            message should not include ("a command is in progress")
            fail(message)
          case other =>
            fail(other.toString)
        ui.cardsCalls shouldBe 1
        ui.albumCalls shouldBe 0
        ui.ownerCalls shouldBe 0
      finally
        Refresh.openUi = previousOpen
        Refresh.placesOf = previousPlaces
        lock.release()
        channel.close()
        deleteTree(runtime)

  it should "reject refresh on the socket before execute" in :
    withPlaces: (_, session, _) =>
      var ran: Boolean = false
      val (_, line) = Cli.serveResponse(
        session,
        """{"argv":["refresh","--list"]}""",
        (_, _) =>
          ran = true
          (session, Result.Ok("ran\n", Json.Null))
      )
      ran shouldBe false
      line should include ("refresh runs in the calling process")

  it should "stay in the shell and print an overlap without a usage line" in :
    withPlaces: (_, session, places) =>
      val sent = scala.collection.mutable.ListBuffer.empty[List[String]]
      val ui = new Fake(Seq.empty)
      val (code, out, err) = shell(
        session,
        Seq("refresh --list --allow-shrink"),
        (current, parsed) => parsed match
          case args: Cli.RefreshArgs =>
            Refresh.run(current, args, _ => ui, places, () => ())
          case _ =>
            fail("refresh did not stay local")
        ,
        Some((_, argv) =>
          sent += argv
          Result.Ok("sent\n", Json.Null)
        )
      )
      code shouldBe 0
      sent.isEmpty shouldBe true
      out shouldBe "kept 0\n"
      err should not include ("Photo Archive Tool:")
      val overlap: Result = Result.Failed(
        "already running\nphase=scroll\n",
        Json.Null,
        Refresh.AlreadyRunning
      )
      Cli.processCode(overlap) shouldBe 2
      Cli.processCode(Result.Usage("rows < 250")) shouldBe 1
      val (again, shown, errors) = shell(
        session,
        Seq("refresh --list"),
        (_, _) => (session, overlap),
        Some((_, _) =>
          sent += List("remote")
          Result.Ok("sent\n", Json.Null)
        )
      )
      again shouldBe 0
      shown shouldBe "already running\nphase=scroll\n"
      errors should not include ("Photo Archive Tool:")
      sent.isEmpty shouldBe true

  it should "list full and album verbs without calling the item reader" in :
    withPlaces: (archive, session, places) =>
      writeCatalog(archive, autoLine("aa", 2, "Auto", "Ada") ++ Seq(
        albumLine("kk", 1, "Keep"),
        itemLine("kk", "keep.jpg", "kk1"),
        albumLine("cc", 1, "Changed"),
        itemLine("cc", "old.jpg", "cc1"),
        albumLine("gg", 0, "Gone")
      ))
      val cards: Seq[AlbumCard] = Seq(
        card("aa", "Auto", "2"),
        card("kk", "Keep", "1"),
        card("cc", "Changed", "3")
      )
      val full = new Fake(cards)
      val listed: Result = run(session, places, refreshArgs(
        mode = RefreshPlan.Mode.Full,
        list = true,
        allowShrink = true
      ), full)._2
      okText(listed) shouldBe
        """auto-add Auto 2
          |would-read Keep 1
          |reread Changed 1 3
          |would-read Changed 3
          |gone Gone
          |""".stripMargin
      full.albumCalls shouldBe 0
      full.ownerCalls shouldBe 0
      full.closed shouldBe true
      val named = new Fake(cards)
      val albumList: Result = run(session, places, refreshArgs(
        mode = RefreshPlan.Mode.Album(Seq("Auto", "Keep", "Changed")),
        list = true,
        allowShrink = true
      ), named)._2
      okText(albumList) shouldBe
        """owner-only Auto 2
          |would-read Keep 1
          |reread Changed 1 3
          |would-read Changed 3
          |""".stripMargin
      named.albumCalls shouldBe 0
      named.ownerCalls shouldBe 0
      retitle(archive, session, places)

  it should "print an incremental count diff and item-read only new and changed albums" in :
    withPlaces: (archive, session, places) =>
      writeCatalog(archive, Seq(
        albumLine("kk", 1, "Keep"),
        itemLine("kk", "keep.jpg", "kk1"),
        albumLine("cc", 1, "Changed"),
        itemLine("cc", "old.jpg", "cc1")
      ) ++ autoLine("aa", 2, "Auto", "Old") ++ Seq(albumLine("gg", 0, "Gone")))
      val cards: Seq[AlbumCard] = Seq(
        card("kk", "Keep", "1"),
        card("cc", "Changed", "3"),
        card("aa", "Auto", "4"),
        card("nn", "New", "1")
      )
      val listed = new Fake(cards)
      val preview: Result = run(
        session,
        places,
        refreshArgs(list = true, allowShrink = true),
        listed
      )._2
      okText(preview) shouldBe
        """reread Changed 1 3
          |reread Auto 2 4
          |new New 1
          |gone Gone
          |kept 1
          |""".stripMargin
      preview match
        case Result.Ok(text, _) =>
          text should not include ("would-read")
          text should not include ("owner-only")
          text should not include ("nn")
        case other =>
          fail(other.toString)
      listed.albumCalls shouldBe 0
      listed.ownerCalls shouldBe 0
      val publishing = new Fake(cards, ownerOf = _ => Some("Ada"))
      val (next, published) = run(session, places, refreshArgs(allowShrink = true), publishing)
      okText(published) shouldBe okText(preview)
      publishing.albumTitles.toSeq shouldBe Seq("New", "Changed")
      publishing.ownerTitles.toSeq shouldBe Seq("Auto")
      val albums: Seq[Catalog.Album] = Catalog.read(catalogFile(archive))
      albums.map(_.title) shouldBe Seq("Keep", "Changed", "Auto", "New")
      val auto: Catalog.Album = albums.find(_.title == "Auto").get
      auto.autoAdd shouldBe true
      auto.owner shouldBe Some("Ada")
      auto.items.isEmpty shouldBe true
      auto.count shouldBe 4
      albums.find(_.title == "New").get.items.length shouldBe 1
      next.catalogCache.isEmpty shouldBe true
      places.directory.exists shouldBe false
      places.lock.isFile shouldBe true

  it should "owner-read a stored auto-add album and item-read the others on a full pass" in :
    withPlaces: (archive, session, places) =>
      writeCatalog(archive, Seq(
        albumLine("sm", 1, "Small"),
        itemLine("sm", "s.jpg", "ks")
      ) ++ autoLine("aa", 3, "Auto", "Old") ++ Seq(
        albumLine("bg", 5, "Big"),
        itemLine("bg", "b1.jpg", "kb1"),
        itemLine("bg", "b2.jpg", "kb2"),
        itemLine("bg", "b3.jpg", "kb3"),
        itemLine("bg", "b4.jpg", "kb4"),
        itemLine("bg", "b5.jpg", "kb5")
      ))
      val ui = new Fake(Seq(
        card("bg", "Big", "5"),
        card("aa", "Auto", "3"),
        card("sm", "Small", "1")
      ), ownerOf = _ => Some("Ada"))
      val text: String = okText(run(session, places, refreshArgs(
        mode = RefreshPlan.Mode.Full,
        allowShrink = true
      ), ui)._2)
      text shouldBe "kept 3\n"
      ui.albumTitles.toSeq shouldBe Seq("Small", "Big")
      ui.ownerTitles.toSeq shouldBe Seq("Auto")

  it should "owner-read one named auto-add album and not item-read it" in :
    withPlaces: (archive, session, places) =>
      writeCatalog(archive, autoLine("aa", 2, "Auto", "Old") ++ Seq(albumLine("kk", 0, "Keep")))
      val ui = new Fake(Seq(card("aa", "Auto", "2"), card("kk", "Keep", "0")), ownerOf = _ => Some("Ada"))
      val text: String = okText(run(session, places, refreshArgs(
        mode = RefreshPlan.Mode.Album(Seq("Auto"))
      ), ui)._2)
      text shouldBe "kept 1\n"
      ui.albumCalls shouldBe 0
      ui.ownerCalls shouldBe 1
      val auto: Catalog.Album = Catalog.read(catalogFile(archive)).find(_.title == "Auto").get
      auto.owner shouldBe Some("Ada")
      auto.autoAdd shouldBe true
      auto.items.isEmpty shouldBe true

  it should "keep a new album's human line to the title and the count" in :
    withPlaces: (_, session, places) =>
      val ui = new Fake(Seq(card("hideme", "Hello", "1")))
      val result: Result = run(session, places, refreshArgs(list = true, allowShrink = true), ui)._2
      result match
        case Result.Ok(text, value) =>
          text shouldBe "new Hello 1\nkept 0\n"
          text should not include ("hideme")
          value.toString should include ("hideme")
        case other =>
          fail(other.toString)
      assertStatus(places, "hideme")
      PosixFilePermissions.toString(NFiles.getPosixFilePermissions(places.directory.toPath)) shouldBe "rwx------"
      PosixFilePermissions.toString(NFiles.getPosixFilePermissions(places.lock.toPath)) shouldBe "rw-------"
      File(places.directory, "albums.tsv").exists shouldBe false

  it should "fail a short list, an empty count, and a bad row without writing the album list" in :
    withPlaces: (_, session, places) =>
      val short: Result = run(session, places, refreshArgs(mode = RefreshPlan.Mode.Full), new Fake(Seq(
        card("one", "Only", "0")
      )))._2
      usage(short) shouldBe "rows < 250"
      File(places.directory, "albums.tsv").exists shouldBe false
      status(places)("phase") shouldBe "error"
      status(places)("error") shouldBe "list"
      val empty: Result = run(session, places, refreshArgs(allowShrink = true), new Fake(Seq(
        card("one", "Only", "")
      )))._2
      usage(empty) shouldBe "empty count"
      val bad: Result = run(session, places, refreshArgs(allowShrink = true), new Fake(Seq(
        card("one", "Only", "nope")
      )))._2
      usage(bad) shouldBe "bad row"

  it should "skip the floor for album mode and for allow-shrink" in :
    withPlaces: (_, session, places) =>
      val named: Result = run(session, places, refreshArgs(
        mode = RefreshPlan.Mode.Album(Seq("Only")),
        list = true
      ), new Fake(Seq(card("one", "Only", "0"))))._2
      okText(named) shouldBe "new Only 0\nwould-read Only 0\n"
      val allowed: Result = run(session, places, refreshArgs(list = true, allowShrink = true), new Fake(Seq(
        card("one", "Only", "0")
      )))._2
      okText(allowed) shouldBe "new Only 0\nkept 0\n"

  it should "name an ambiguous title and a missing card without an id" in :
    withPlaces: (_, session, places) =>
      val cards: Seq[AlbumCard] = Seq(card("ida", "Twin", "0"), card("idb", "Twin", "1"))
      val ambiguous: Result = run(session, places, refreshArgs(
        mode = RefreshPlan.Mode.Album(Seq("Twin")),
        list = true,
        allowShrink = true
      ), new Fake(cards))._2
      usage(ambiguous) shouldBe "ambiguous title"
      val missing: Result = run(session, places, refreshArgs(
        mode = RefreshPlan.Mode.Album(Seq("Missing")),
        list = true,
        allowShrink = true
      ), new Fake(cards))._2
      usage(missing) shouldBe "named card is missing"

  it should "read an ok album once and keep the earlier shortfall when the keys match" in :
    withPlaces: (archive, session, places) =>
      var pauses: Int = 0
      var calls: Int = 0
      val ui = new Fake(Seq(card("pic", "Pic", "2")), albumOf = card =>
        calls += 1
        shortAlbum(card, Seq("same"), LocalDate.parse(s"2010-06-0$calls"))
      )
      val text: String = okText(run(session, places, refreshArgs(allowShrink = true), ui, () => pauses += 1)._2)
      text shouldBe "new Pic 2\nkept 0\n"
      calls shouldBe 3
      pauses shouldBe 1
      Catalog.read(catalogFile(archive)).head.items.head.date.map(_.toString) shouldBe Some(day.toString)

  it should "keep the shortfall with the most item keys" in :
    withPlaces: (archive, session, places) =>
      val sets: Seq[Seq[String]] = Seq(Seq("a"), Seq("a", "b"), Seq("a"))
      var calls: Int = 0
      val ui = new Fake(Seq(card("pic", "Pic", "3")), albumOf = card =>
        val keys: Seq[String] = sets(calls)
        calls += 1
        shortAlbum(card, keys, day)
      )
      run(session, places, refreshArgs(allowShrink = true), ui)
      calls shouldBe 3
      Catalog.read(catalogFile(archive)).head.items.map(_.key).toSet shouldBe Set("a", "b")

  it should "repeat a name-failure once and keep the later ok read" in :
    withPlaces: (archive, session, places) =>
      var calls: Int = 0
      var picReads: Int = 0
      var pauses: Int = 0
      val ui = new Fake(Seq(card("pic", "Pic", "1"), card("two", "Two", "1")), albumOf = card =>
        calls += 1
        if card.title == "Pic" then
          picReads += 1
          if picReads == 1 then nameFailure(card) else okAlbum(card)
        else okAlbum(card)
      )
      run(session, places, refreshArgs(allowShrink = true), ui, () => pauses += 1)
      calls shouldBe 3
      pauses shouldBe 2
      val pic: Catalog.Album = Catalog.read(catalogFile(archive)).find(_.title == "Pic").get
      pic.error shouldBe None
      pic.items.map(_.key) shouldBe Seq("k1")
      pic.unsets.isEmpty shouldBe true

  it should "restore a tab or newline in a filename and count it as a name-failure" in :
    val tab: String = "bad\u0001tname.jpg"
    val nl: String = "bad\u0001nname.jpg"
    val cr: String = "bad\u0001rname.jpg"
    val album: Catalog.Album = Refresh.parseBody(
      s"""album\tpic\t4\tPic
         |item\tpic\tgood.jpg\t2010-06-01\tk1\t-
         |item\tpic\t$tab\t2010-06-01\tk2\t-
         |item\tpic\t$nl\t2010-06-01\tk3\t-
         |item\tpic\t$cr\t2010-06-01\tk4\t-
         |error\tpic\t4\t4\t0
         |""".stripMargin
    )
    album.items.map(_.filename) shouldBe Seq("good.jpg", "bad\tname.jpg", "bad\nname.jpg", "bad\rname.jpg")
    album.error.map(_.nameFailures) shouldBe Some(3)
    album.unsets.isEmpty shouldBe true

  it should "keep the other items when a filename contains a tab and fail the check" in :
    withPlaces: (archive, session, places) =>
      val catalog: File = writeCatalog(archive, Seq(albumLine("stay", 0, "Stay")))
      val before: Array[Byte] = NFiles.readAllBytes(catalog.toPath)
      var calls: Int = 0
      val ui = new Fake(Seq(card("newid", "New", "2")), albumOf = _ =>
        calls += 1
        Catalog.Album(
          "newid",
          2,
          "New",
          None,
          false,
          Some(Catalog.Error(2, 2, 1)),
          Seq(
            Catalog.Item("good.jpg", Some(day), "k1", None),
            Catalog.Item("bad\tname.jpg", Some(day), "k2", None)
          ),
          Seq.empty
        )
      )
      val failed: Result = run(session, places, refreshArgs(allowShrink = true), ui)._2
      usage(failed) shouldBe "wrong column count"
      calls shouldBe 2
      java.util.Arrays.equals(NFiles.readAllBytes(catalog.toPath), before) shouldBe true
      status(places)("phase") shouldBe "error"
      status(places)("error") shouldBe "check"
      val part: String = NFiles.readString(File(File(places.directory, "parts"), "newid").toPath, StandardCharsets.UTF_8)
      part.startsWith("# part\t") shouldBe true
      part.contains("# FAILED") shouldBe false
      part should include ("good.jpg")
      val partial: String = NFiles.readString(
        File(File(archive, ".gphoto-sync"), "catalog.txt.partial").toPath,
        StandardCharsets.UTF_8
      )
      partial should include ("good.jpg")
      partial should include ("bad\tname.jpg")
      partial.contains("bad name.jpg") shouldBe false

  it should "record four thrown reads as a failed part and scroll on the next run" in :
    withPlaces: (archive, session, places) =>
      val catalog: File = writeCatalog(archive, Seq(albumLine("stay", 0, "Stay")))
      val before: Array[Byte] = NFiles.readAllBytes(catalog.toPath)
      var calls: Int = 0
      val cached: Session = session.copy(catalogCache = Some(CatalogCache(4, 5, Seq.empty)))
      val ui = new Fake(Seq(card("newid", "New", "1")), albumOf = _ =>
        calls += 1
        throw IllegalArgumentException("bad filename")
      )
      val (returned, failed) = run(cached, places, refreshArgs(allowShrink = true), ui)
      usage(failed) shouldBe "item count does not equal count"
      calls shouldBe 4
      returned.catalogCache.isDefined shouldBe true
      returned.catalogCache.get.size shouldBe 4
      java.util.Arrays.equals(NFiles.readAllBytes(catalog.toPath), before) shouldBe true
      status(places)("phase") shouldBe "error"
      status(places)("error") shouldBe "check"
      val part: String = NFiles.readString(File(File(places.directory, "parts"), "newid").toPath, StandardCharsets.UTF_8)
      part.startsWith("# part\t") shouldBe true
      part should include ("# FAILED")
      part should include ("bad filename")
      File(File(archive, ".gphoto-sync"), "catalog.txt.partial").isFile shouldBe true
      val again = new Fake(Seq(card("newid", "New", "1")))
      val (next, ok) = run(returned, places, refreshArgs(allowShrink = true), again)
      okText(ok) should include ("new New 1")
      again.cardsCalls shouldBe 1
      again.albumCalls shouldBe 1
      next.catalogCache.isEmpty shouldBe true
      places.directory.exists shouldBe false
      places.lock.isFile shouldBe true

  it should "keep phase items when a resumed run is asked to sign in and nothing is item-read" in :
    withPlaces: (archive, session, places) =>
      val catalog: File = writeCatalog(archive, autoLine("aa", 2, "Auto", "Old"))
      val before: Array[Byte] = NFiles.readAllBytes(catalog.toPath)
      seed(places, "items", Seq("Auto\t2\taa\t\t[]"))
      var notices: Int = 0
      val ui = new Fake(Seq.empty, ownerOf = _ => throw IllegalStateException("nope"))
      val failed: Result = Refresh.run(
        session,
        refreshArgs(),
        notice =>
          notices += 1
          notice(Refresh.Notice("signin", Seq.empty))
          ui
        ,
        places,
        () => ()
      )._2
      usage(failed) shouldBe "owner read failed"
      notices shouldBe 1
      ui.albumCalls shouldBe 0
      ui.ownerCalls shouldBe 4
      status(places)("phase") shouldBe "items"
      NFiles.readString(File(places.directory, "status.txt").toPath, StandardCharsets.UTF_8).contains("signin") shouldBe false
      java.util.Arrays.equals(NFiles.readAllBytes(catalog.toPath), before) shouldBe true
      val again = new Fake(Seq.empty, onCards = () => fail("scrolled"), ownerOf = _ => Some("Old"))
      val text: String = okText(Refresh.run(session, refreshArgs(), _ => again, places, () => ())._2)
      text shouldBe "kept 1\n"
      again.cardsCalls shouldBe 0
      again.albumCalls shouldBe 0
      again.ownerCalls shouldBe 1

  it should "retry an owner four times and leave the phase on items" in :
    withPlaces: (archive, session, places) =>
      val catalog: File = writeCatalog(archive, autoLine("aaid", 2, "Auto", "Old"))
      val before: Array[Byte] = NFiles.readAllBytes(catalog.toPath)
      var calls: Int = 0
      val ui = new Fake(Seq(card("aaid", "Auto", "2")), ownerOf = _ =>
        calls += 1
        throw IllegalStateException("nope")
      )
      val failed: Result = run(session, places, refreshArgs(allowShrink = true), ui)._2
      usage(failed) shouldBe "owner read failed"
      calls shouldBe 4
      ui.albumCalls shouldBe 0
      status(places)("phase") shouldBe "items"
      java.util.Arrays.equals(NFiles.readAllBytes(catalog.toPath), before) shouldBe true
      assertStatus(places, "aaid")

  it should "resume a completed album list and not reread a winner" in :
    withPlaces: (archive, session, places) =>
      seed(places, "items", Seq("Pic\t1\tpic\t\t"))
      seedPart(places, "pic",
        """album	pic	1	Pic
          |item	pic	one.jpg	2010-06-01	k1	-
          |""".stripMargin)
      val ui = new Fake(Seq.empty, onCards = () => fail("scrolled"))
      val text: String = okText(run(session, places, refreshArgs(), ui)._2)
      text shouldBe "new Pic 1\nkept 0\n"
      ui.cardsCalls shouldBe 0
      ui.albumCalls shouldBe 0
      Catalog.read(catalogFile(archive)).head.items.map(_.key) shouldBe Seq("k1")

  it should "retry a failed part without scrolling" in :
    withPlaces: (archive, session, places) =>
      seed(places, "items", Seq("Pic\t1\tpic\t\t"))
      seedPart(places, "pic", "# FAILED\tboom\n")
      val ui = new Fake(Seq(card("pic", "Pic", "1")), onCards = () => fail("scrolled"))
      run(session, places, refreshArgs(), ui)
      ui.cardsCalls shouldBe 0
      ui.albumCalls shouldBe 1
      Catalog.read(catalogFile(archive)).head.items.length shouldBe 1

  it should "scroll again when the phase is not items or the album list is missing" in :
    withPlaces: (_, session, places) =>
      seed(places, "scroll", Seq("nope"))
      val marker: File = File(places.directory, "marker")
      NFiles.writeString(marker.toPath, "keep\n", StandardCharsets.UTF_8)
      val scrolled = new Fake(Seq(card("one", "Only", "0")))
      val text: String = okText(run(session, places, refreshArgs(list = true, allowShrink = true), scrolled)._2)
      text shouldBe "new Only 0\nkept 0\n"
      scrolled.cardsCalls shouldBe 1
      marker.exists shouldBe false
      val parent: File = places.directory.getParentFile
      val again: Refresh.Places = Refresh.Places(
        File(parent, "gphoto-sync-refresh"),
        File(parent, "gphoto-sync-refresh.lock")
      )
      seed(again, "items", Seq.empty)
      val leftover: File = File(again.directory, "marker")
      NFiles.writeString(leftover.toPath, "keep\n", StandardCharsets.UTF_8)
      NFiles.deleteIfExists(File(again.directory, "albums.tsv").toPath)
      val fresh = new Fake(Seq(card("one", "Only", "0")))
      run(session, places, refreshArgs(list = true, allowShrink = true), fresh)
      fresh.cardsCalls shouldBe 1
      leftover.exists shouldBe false

  it should "list a saved album list without deleting it or opening the ui" in :
    withPlaces: (_, session, places) =>
      seed(places, "items", Seq("Only\t0\tone\t\t"))
      val marker: File = File(places.directory, "marker")
      NFiles.writeString(marker.toPath, "keep\n", StandardCharsets.UTF_8)
      var opened: Boolean = false
      val result: Result = Refresh.run(
        session,
        refreshArgs(list = true),
        _ =>
          opened = true
          new Fake(Seq.empty)
        ,
        places,
        () => ()
      )._2
      okText(result) shouldBe "new Only 0\nkept 0\n"
      opened shouldBe false
      marker.isFile shouldBe true
      places.directory.isDirectory shouldBe true

  it should "leave a bad catalog and a lock inside the work directory unwiped" in :
    withPlaces: (archive, session, places) =>
      val directory: File = File(archive, ".gphoto-sync")
      directory.mkdirs()
      NFiles.writeString(File(directory, "catalog.txt").toPath, "nope\n", StandardCharsets.UTF_8)
      seed(places, "items", Seq("Only\t0\tone\t\t"))
      val marker: File = File(places.directory, "marker")
      NFiles.writeString(marker.toPath, "keep\n", StandardCharsets.UTF_8)
      var opened: Boolean = false
      val bad: Result = Refresh.run(
        session,
        refreshArgs(allowShrink = true, list = true),
        _ =>
          opened = true
          new Fake(Seq.empty)
        ,
        places,
        () => ()
      )._2
      usage(bad) shouldBe "catalog is not version 2"
      opened shouldBe false
      marker.isFile shouldBe true
      val work: File = File(places.directory.getParentFile, "inside")
      work.mkdirs()
      val kept: File = File(work, "marker")
      NFiles.writeString(kept.toPath, "keep\n", StandardCharsets.UTF_8)
      val inside: Refresh.Places = Refresh.Places(work, File(work, "gphoto-sync-refresh.lock"))
      val locked: Result = Refresh.run(session, refreshArgs(list = true, allowShrink = true), _ =>
        new Fake(Seq.empty)
      , inside, () => ())._2
      usage(locked) shouldBe "refresh lock is inside the work directory"
      NFiles.readString(kept.toPath, StandardCharsets.UTF_8) shouldBe "keep\n"

  it should "leave day-folder pictures in place when an album is dropped" in :
    withPlaces: (archive, session, places) =>
      val picture: File = File(archive, "2010/07/19/pic.jpg")
      picture.getParentFile.mkdirs()
      NFiles.writeString(picture.toPath, "pixels", StandardCharsets.UTF_8)
      writeCatalog(archive, Seq(albumLine("gg", 0, "Gone")))
      val cached: Session = session.copy(catalogCache = Some(CatalogCache(9, 8, Seq.empty)))
      val (next, result) = run(cached, places, refreshArgs(allowShrink = true), new Fake(Seq.empty))
      okText(result) shouldBe "gone Gone\nkept 0\n"
      NFiles.readString(picture.toPath, StandardCharsets.UTF_8) shouldBe "pixels"
      next.catalogCache.isEmpty shouldBe true
      val failed: Session = session.copy(catalogCache = Some(CatalogCache(3, 4, Seq.empty)))
      val (kept, floor) = run(failed, places, refreshArgs(), new Fake(Seq(card("one", "Only", "0"))))
      usage(floor) shouldBe "rows < 250"
      kept.catalogCache.get.size shouldBe 3

  it should "tell a second refresh in this process that one is already running" in :
    withPlaces: (_, session, places) =>
      val started = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val firstUi = new Fake(Seq(card("scrollid", "Scroll", "0")), onCards = () =>
        started.countDown()
        if !release.await(30, TimeUnit.SECONDS) then throw IllegalStateException("release timed out")
      )
      val holder = new AtomicReference[(Session, Result)]
      val thread = new Thread(() =>
        holder.set(run(session, places, refreshArgs(list = true, allowShrink = true), firstUi))
      , "refresh-holder")
      thread.setDaemon(true)
      thread.start()
      try
        if !started.await(10, TimeUnit.SECONDS) then fail("first refresh did not start")
        val secondUi = new Fake(Seq(card("scrollid", "Scroll", "0")), onCards = () => fail("second refresh scrolled"))
        val second: Result = run(session, places, refreshArgs(list = true, allowShrink = true), secondUi)._2
        second match
          case Result.Failed(text, _, message) =>
            text.split("\n", -1).head shouldBe "already running"
            text should include ("phase=scroll")
            message shouldBe Refresh.AlreadyRunning
            Cli.processCode(second) shouldBe 2
            text should not include ("scrollid")
          case other =>
            fail(other.toString)
        secondUi.cardsCalls shouldBe 0
      finally
        release.countDown()
        thread.join(10000)
      holder.get()._2 match
        case Result.Ok(text, _) =>
          text shouldBe "new Scroll 0\nkept 0\n"
        case other =>
          fail(other.toString)
      val third = new Fake(Seq(card("scrollid", "Scroll", "0")))
      val after: Result = run(session, places, refreshArgs(list = true, allowShrink = true), third)._2
      after match
        case Result.Ok(_, _) =>
          third.cardsCalls shouldBe 1
        case Result.Failed(_, _, message) if message == Refresh.AlreadyRunning =>
          fail("lock stayed held")
        case other =>
          fail(other.toString)

  it should "release the lock when the album list throws" in :
    withPlaces: (_, session, places) =>
      val boom = new Fake(Seq.empty, onCards = () => throw IllegalStateException("boom"))
      usage(run(session, places, refreshArgs(list = true, allowShrink = true), boom)._2) shouldBe "boom"
      val again = new Fake(Seq.empty)
      okText(run(session, places, refreshArgs(list = true, allowShrink = true), again)._2) shouldBe "kept 0\n"
      again.cardsCalls shouldBe 1

  private def retitle(archive: File, session: Session, places: Refresh.Places): Unit =
    val ui = new Fake(Seq(card("tt", "NewName", "1")))
    writeCatalog(archive, Seq(albumLine("tt", 1, "OldName"), itemLine("tt", "one.jpg", "kt")))
    val text: String = okText(run(session, places, refreshArgs(
      mode = RefreshPlan.Mode.Album(Seq("NewName")),
      list = true,
      allowShrink = true
    ), ui)._2)
    text shouldBe "retitle NewName\nwould-read NewName 1\n"
    text should not include ("gone")
    ui.albumCalls shouldBe 0
    ui.ownerCalls shouldBe 0

  private def refreshArgs(
    mode: RefreshPlan.Mode = RefreshPlan.Mode.Incremental,
    list: Boolean = false,
    allowShrink: Boolean = false
  ): Cli.RefreshArgs = Cli.RefreshArgs(mode, list, allowShrink)

  private def parsedOf(argv: String*): Cli.Parsed = Cli.parse(argv.toList) match
    case Cli.Run(parsed) => parsed
    case other => fail(other.toString)

  private def okAlbum(card: AlbumCard, date: LocalDate = day): Catalog.Album =
    val count: Int = card.countText.toInt
    val items: Seq[Catalog.Item] = (1 to count).map(i => Catalog.Item(s"f$i.jpg", Some(date), s"k$i", None))
    Catalog.Album(card.id, count, card.title, None, false, None, items, Seq.empty)

  private def shortAlbum(card: AlbumCard, keys: Seq[String], date: LocalDate): Catalog.Album =
    val count: Int = card.countText.toInt
    val items: Seq[Catalog.Item] = keys.map(key => Catalog.Item(s"$key.jpg", Some(date), key, None))
    Catalog.Album(
      card.id,
      count,
      card.title,
      None,
      false,
      Some(Catalog.Error(items.length, count, 0)),
      items,
      Seq.empty
    )

  private def nameFailure(card: AlbumCard): Catalog.Album =
    val count: Int = card.countText.toInt
    Catalog.Album(
      card.id,
      count,
      card.title,
      None,
      false,
      Some(Catalog.Error(1, count, 1)),
      Seq.empty,
      Seq(Catalog.Unset(Some(day), "missing", None))
    )

  private final class Fake(
    cards: Seq[AlbumCard],
    ownerOf: AlbumCard => Option[String] = _ => None,
    albumOf: AlbumCard => Catalog.Album = card => okAlbum(card),
    onCards: () => Unit = () => ()
  ) extends PhotosUi:
    var cardsCalls: Int = 0
    var ownerCalls: Int = 0
    var albumCalls: Int = 0
    val albumTitles: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer.empty
    val ownerTitles: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer.empty
    var closed: Boolean = false

    def albumCards(): Seq[AlbumCard] =
      cardsCalls += 1
      onCards()
      cards

    def readOwner(card: AlbumCard): Option[String] =
      ownerCalls += 1
      ownerTitles += card.title
      ownerOf(card)

    def readAlbum(card: AlbumCard): Catalog.Album =
      albumCalls += 1
      albumTitles += card.title
      albumOf(card)

    def close(): Unit = closed = true

  private def run(
    session: Session,
    places: Refresh.Places,
    args: Cli.RefreshArgs,
    ui: PhotosUi,
    pause: () => Unit = () => ()
  ): (Session, Result) =
    Refresh.run(session, args, _ => ui, places, pause)

  private def card(id: String, title: String, count: String): AlbumCard =
    AlbumCard(id, title, count, "", "[]")

  private def albumLine(id: String, count: Int, title: String): String =
    s"album\t$id\t$count\t$title"

  private def itemLine(id: String, filename: String, key: String): String =
    s"item\t$id\t$filename\t2010-06-01\t$key\t-"

  private def autoLine(id: String, count: Int, title: String, owner: String): Seq[String] =
    Seq(albumLine(id, count, title), s"owner\t$id\t$owner", s"auto-add\t$id")

  private def writeCatalog(archive: File, lines: Seq[String]): File =
    val directory: File = File(archive, ".gphoto-sync")
    directory.mkdirs()
    val file: File = catalogFile(archive)
    NFiles.writeString(file.toPath, (Seq("# gphoto-catalog 2") ++ lines).mkString("", "\n", "\n"), StandardCharsets.UTF_8)
    file

  private def catalogFile(archive: File): File = File(File(archive, ".gphoto-sync"), "catalog.txt")

  private def seed(places: Refresh.Places, phase: String, lines: Seq[String]): Unit =
    places.directory.mkdirs()
    NFiles.writeString(
      File(places.directory, "status.txt").toPath,
      s"heartbeat=1\nphase=$phase\n",
      StandardCharsets.UTF_8
    )
    if lines.nonEmpty then
      NFiles.writeString(
        File(places.directory, "albums.tsv").toPath,
        lines.map(_ + "\n").mkString,
        StandardCharsets.UTF_8
      )

  private def seedPart(places: Refresh.Places, id: String, body: String): Unit =
    val parts: File = File(places.directory, "parts")
    parts.mkdirs()
    NFiles.writeString(File(parts, id).toPath, s"# part\t$id\n$body", StandardCharsets.UTF_8)

  private def status(places: Refresh.Places): Map[String, String] =
    val file: File = File(places.directory, "status.txt")
    if !file.isFile then Map.empty
    else
      NFiles.readString(file.toPath, StandardCharsets.UTF_8).split("\n", -1).toSeq.filter(_.nonEmpty).flatMap: line =>
        val (key, rest) = line.span(_ != '=')
        if key.nonEmpty && rest.startsWith("=") then Some(key -> rest.drop(1)) else None
      .toMap

  private def assertStatus(places: Refresh.Places, id: String): Unit =
    val file: File = File(places.directory, "status.txt")
    val text: String = if file.isFile then NFiles.readString(file.toPath, StandardCharsets.UTF_8) else ""
    text should not include (id)
    status(places).keys.foreach(key => statusKeys should contain (key))

  private def okText(result: Result): String = result match
    case Result.Ok(text, _) => text
    case other => fail(other.toString)

  private def usage(result: Result): String = result match
    case Result.Usage(message) => message
    case other => fail(other.toString)

  private def shell(
    session: Session,
    lines: Seq[String],
    executeCommand: (Session, Cli.Parsed) => (Session, Result),
    exchange: Option[(Session, List[String]) => Result]
  ): (Int, String, String) =
    val out = new ByteArrayOutputStream
    val err = new ByteArrayOutputStream
    val code: Int = Console.withOut(out):
      Console.withErr(err):
        Cli.shell(session, lines.iterator, executeCommand, exchange)
    (code, new String(out.toByteArray, StandardCharsets.UTF_8), new String(err.toByteArray, StandardCharsets.UTF_8))

  private def withPlaces(body: (File, Session, Refresh.Places) => Unit): Unit =
    val parent: File = NFiles.createTempDirectory("gphoto-refresh-test").toFile
    val archive: File = File(parent, "archive")
    archive.mkdirs()
    val places: Refresh.Places = Refresh.Places(
      File(parent, "gphoto-sync-refresh"),
      File(parent, "gphoto-sync-refresh.lock")
    )
    try body(archive, sessionFor(archive), places)
    finally deleteTree(parent)

  private def sessionFor(archive: File): Session =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    Session(Root(Configuration(rootPath, ZoneId.of("America/New_York"))))

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).toSeq.flatten.foreach(deleteTree)
    if file.exists then NFiles.deleteIfExists(file.toPath)
