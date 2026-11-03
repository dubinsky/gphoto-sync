package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, StandardCopyOption}
import java.time.{LocalDate, ZoneId}

class ApplyRequestsTest extends AnyFlatSpec, Matchers:
  @targetName("outcomeCanEqual")
  private given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")

  "apply-requests" should "print a plan without Chrome, the lock, or blocked" in :
    withArchive: archive =>
      writeCatalog(archive, planCatalog*)
      val requests: File = writeRequests(archive, planRows*)
      val before: String = read(requests)
      val staged: File = File(archive, ".gphoto-sync/.download/2010/07/03")
      staged.mkdirs()
      JFiles.write(File(staged, "have.jpg").toPath, Array[Byte](1))
      withSeams(archive): (session, places) =>
        var opened: Int = 0
        Refresh.openAct = _ =>
          opened += 1
          new Clicker()
        val text: String = shown(exec(session, "agent", "apply-requests"), planHidden*)
        text shouldBe planText
        opened shouldBe 0
        places.lock.exists shouldBe false
        read(requests) shouldBe before
        outcomes(archive).exists shouldBe false
        results(archive).exists shouldBe false

  it should "keep a needs-agent move after another album changes the catalog" in :
    withArchive: archive =>
      val catalog: File = writeCatalog(
        archive,
        "album\tidSrc\t1\tSource Title",
        "item\tidSrc\tpic.jpg\t2010-07-03\tkeyMove\t-",
        "album\tidDst\t0\tDest Title",
        "album\tidOther\t0\tOther Title"
      )
      val requests: File = writeRequests(
        archive,
        request("r1", "move-item", "idSrc", "keyMove", "pic.jpg", "idDst")
      )
      val beforeRequests: String = read(requests)
      val seeded: File = writeOutcomes(archive, "r1\tneeds-agent\t1700000000000")
      val beforeOutcomes: String = read(seeded)
      val rewritten: String = read(catalog).replace("Other Title", "Published Title")
      JFiles.writeString(catalog.toPath, rewritten, StandardCharsets.UTF_8)
      catalog.setLastModified(System.currentTimeMillis() + 5000) shouldBe true
      val albums: Seq[Catalog.Album] = Catalog.read(catalog)
      albums.find(_.id == "idSrc").get.items.exists(_.key == "keyMove") shouldBe true
      albums.find(_.id == "idDst").get.items.exists(_.filename == "pic.jpg") shouldBe false
      albums.find(_.id == "idOther").get.title shouldBe "Published Title"
      withSeams(archive): (session, places) =>
        var opened: Int = 0
        Refresh.openAct = _ =>
          opened += 1
          new Clicker()
        val text: String = shown(exec(session, "agent", "apply-requests"), "idSrc", "idDst", "keyMove", "idOther")
        text shouldBe "r1 move-item needs-agent \"pic.jpg\"\n"
        val applied: String = shown(
          exec(session, "agent", "apply-requests", "--apply"),
          "idSrc", "idDst", "keyMove", "idOther"
        )
        applied shouldBe text
        opened shouldBe 0
        places.lock.exists shouldBe false
        read(requests) shouldBe beforeRequests
        read(seeded) shouldBe beforeOutcomes
        results(archive).exists shouldBe false
        // Same catalog is click when that outcome file is not beside the requests.
        val aside: File = File(seeded.getParentFile, "requests.tsv.outcomes.aside")
        JFiles.move(seeded.toPath, aside.toPath)
        try
          val click: String = shown(exec(session, "agent", "apply-requests"), "idSrc", "idDst", "keyMove")
          click shouldBe "r1 move-item click \"pic.jpg\"\n"
          opened shouldBe 0
        finally JFiles.move(aside.toPath, seeded.toPath)
        read(seeded) shouldBe beforeOutcomes

  it should "append needs-agent when swap restores the old catalog and not click the later row" in :
    withArchive: archive =>
      val catalog: File = writeCatalog(
        archive,
        "album\tidSrc\t1\tSource Title",
        "item\tidSrc\tpic.jpg\t2010-07-03\tkeyMove\t-",
        "album\tidDst\t0\tDest Title",
        "album\tidRen\t0\tOld Title"
      )
      val beforeCatalog: String = read(catalog)
      val requests: File = writeRequests(
        archive,
        request("r1", "move-item", "idSrc", "keyMove", "pic.jpg", "idDst"),
        request("r2", "rename-album", "idRen", "", "Old Title", "July")
      )
      val beforeRequests: String = read(requests)
      val seeded: File = writeOutcomes(archive, "r9\tdone\t1700000000000")
      val beforeOutcomes: String = read(seeded)
      withSeams(archive): (session, places) =>
        val real: (File, File) => Unit = RefreshPlan.moveReplacing
        var swapped: Boolean = false
        RefreshPlan.moveReplacing = (from, to) =>
          if from.getName == "catalog.txt.partial" && to.getName == "catalog.txt" then
            swapped = true
            throw IllegalStateException("second rename")
          else real(from, to)
        var opened: Int = 0
        var put: Boolean = false
        var took: Boolean = false
        Refresh.openAct = _ =>
          opened += 1
          new Clicker(
            onRename = (_, _) => throw AssertionError("later rename"),
            onPut = (_, _, _) =>
              put = true
              Catalog.Item("pic.jpg", Some(LocalDate.of(2010, 7, 3)), "keyMove", None),
            onTake = (_, _) => took = true
          )
        val result: Result = exec(session, "agent", "apply-requests", "--apply")
        Cli.processCode(result) shouldBe 0
        shown(result, "idSrc", "idDst", "idRen", "keyMove") shouldBe
          "r1 move-item needs-agent \"pic.jpg\"\n"
        put shouldBe true
        took shouldBe true
        swapped shouldBe true
        opened shouldBe 1
        read(catalog) shouldBe beforeCatalog
        read(requests) shouldBe beforeRequests
        results(archive).exists shouldBe false
        val after: String = read(seeded)
        after.startsWith(beforeOutcomes) shouldBe true
        after should include ("r1\tneeds-agent\t")
        Catalog.read(catalog).find(_.id == "idSrc").get.items.exists(_.key == "keyMove") shouldBe true
        Refresh.openAct = _ =>
          opened += 1
          new Clicker(onRename = (_, _) => throw AssertionError("later rename"))
        shown(exec(session, "agent", "apply-requests"), "idSrc", "idDst", "idRen", "keyMove") shouldBe
          """r1 move-item needs-agent "pic.jpg"
            |r2 rename-album click "Old Title"
            |""".stripMargin
        opened shouldBe 1
        read(seeded).startsWith(beforeOutcomes) shouldBe true

  it should "fail when the needs-agent append throws and leave that id with no line" in :
    withArchive: archive =>
      val catalog: File = writeCatalog(archive, "album\tidRen\t0\tOld Title")
      val beforeCatalog: String = read(catalog)
      val requests: File = writeRequests(
        archive,
        request("r1", "rename-album", "idRen", "", "Old Title", "July")
      )
      val beforeRequests: String = read(requests)
      withSeams(archive): (session, places) =>
        var opened: Int = 0
        var held: Boolean = false
        Refresh.openAct = _ =>
          opened += 1
          new Clicker(onRename = (_, _) => throw Refresh.ActMiss("selector", confirmed = false))
        Outcomes.beforeWrite = () =>
          Refresh.claim(places.lock) match
            case Refresh.Claim.Busy =>
              held = true
              throw IllegalStateException("append failed")
            case Refresh.Claim.Owned(channel, lock) =>
              Refresh.release(channel, lock)
              throw AssertionError("refresh lock was not held")
        val result: Result = exec(session, "agent", "apply-requests", "--apply")
        result shouldBe Result.Usage("append failed")
        Cli.processCode(result) shouldBe 1
        held shouldBe true
        opened shouldBe 1
        outcomes(archive).exists shouldBe false
        read(catalog) shouldBe beforeCatalog
        read(requests) shouldBe beforeRequests
        results(archive).exists shouldBe false
        Refresh.openAct = _ =>
          opened += 1
          new Clicker()
        shown(exec(session, "agent", "apply-requests"), "idRen") shouldBe
          "r1 rename-album click \"Old Title\"\n"
        opened shouldBe 1

  it should "not click a done description" in :
    withArchive: archive =>
      writeCatalog(
        archive,
        "album\tidDesc\t1\tDesc Title",
        "item\tidDesc\tpic.jpg\t-\tkeyDesc\t-"
      )
      val requests: File = writeRequests(
        archive,
        request("r1", "set-description", "", "keyDesc", "pic.jpg", "Hello")
      )
      val beforeRequests: String = read(requests)
      withSeams(archive): (session, places) =>
        var opened: Int = 0
        Refresh.openAct = _ =>
          opened += 1
          new Clicker()
        shown(exec(session, "agent", "apply-requests"), "idDesc", "keyDesc") shouldBe
          "r1 set-description click \"pic.jpg\"\n"
        val seeded: File = writeOutcomes(archive, "r1\tdone\t1700000000000")
        val beforeOutcomes: String = read(seeded)
        val planned: String = shown(exec(session, "agent", "apply-requests"), "idDesc", "keyDesc")
        planned shouldBe "r1 set-description done \"pic.jpg\"\n"
        val applied: Result = exec(session, "agent", "apply-requests", "--apply")
        Cli.processCode(applied) shouldBe 0
        shown(applied, "idDesc", "keyDesc") shouldBe planned
        opened shouldBe 0
        places.lock.exists shouldBe false
        read(seeded) shouldBe beforeOutcomes
        read(requests) shouldBe beforeRequests
        results(archive).exists shouldBe false

  it should "append done for a successful description and not click it again" in :
    withArchive: archive =>
      val catalog: File = writeCatalog(
        archive,
        "album\tidDesc\t1\tDesc Title",
        "item\tidDesc\tpic.jpg\t-\tkeyDesc\t-"
      )
      val beforeCatalog: String = read(catalog)
      val requests: File = writeRequests(
        archive,
        request("r1", "set-description", "", "keyDesc", "pic.jpg", "Hello")
      )
      val beforeRequests: String = read(requests)
      withSeams(archive): (session, places) =>
        var opened: Int = 0
        var described: Boolean = false
        var held: Boolean = false
        Refresh.openAct = _ =>
          opened += 1
          new Clicker(onDescription = (_, _) => described = true)
        Outcomes.beforeWrite = () =>
          Refresh.claim(places.lock) match
            case Refresh.Claim.Busy =>
              held = true
            case Refresh.Claim.Owned(channel, lock) =>
              Refresh.release(channel, lock)
              throw AssertionError("refresh lock was not held")
        val applied: Result = exec(session, "agent", "apply-requests", "--apply")
        Cli.processCode(applied) shouldBe 0
        shown(applied, "idDesc", "keyDesc") shouldBe "r1 set-description done \"pic.jpg\"\n"
        described shouldBe true
        held shouldBe true
        opened shouldBe 1
        read(catalog) shouldBe beforeCatalog
        read(requests) shouldBe beforeRequests
        results(archive).exists shouldBe false
        val line: String = read(outcomes(archive))
        line.startsWith("# gphoto-request-outcomes 1\nr1\tdone\t") shouldBe true
        Refresh.openAct = _ =>
          opened += 1
          new Clicker()
        shown(exec(session, "agent", "apply-requests", "--apply"), "idDesc", "keyDesc") shouldBe
          "r1 set-description done \"pic.jpg\"\n"
        opened shouldBe 1
        read(outcomes(archive)) shouldBe line

  it should "skip an accept failure and still click the next row" in :
    withArchive: archive =>
      writeCatalog(
        archive,
        "album\tidSrc\t1\tSource Title",
        "item\tidSrc\tpic.jpg\t-\tkeyMove\t-",
        "album\tidOwn\t0\tOwned Dest",
        "owner\tidOwn\tAda",
        "album\tidRen\t0\tOld Title"
      )
      val requests: File = writeRequests(
        archive,
        request("r1", "move-item", "idSrc", "keyMove", "pic.jpg", "idOwn"),
        request("r2", "rename-album", "idRen", "", "Old Title", "July")
      )
      val beforeRequests: String = read(requests)
      withSeams(archive): (session, _) =>
        var opened: Int = 0
        var renamed: Boolean = false
        Refresh.openAct = _ =>
          opened += 1
          new Clicker(onRename = (_, _) => renamed = true)
        val result: Result = exec(session, "agent", "apply-requests", "--apply")
        Cli.processCode(result) shouldBe 0
        shown(result, "idSrc", "idOwn", "idRen", "keyMove") shouldBe
          """r1 move-item skip "pic.jpg"
            |r2 rename-album satisfied "July"
            |""".stripMargin
        opened shouldBe 1
        renamed shouldBe true
        outcomes(archive).exists shouldBe false
        read(requests) shouldBe beforeRequests
        results(archive).exists shouldBe false
        Catalog.read(catalog(archive)).find(_.id == "idRen").get.title shouldBe "July"

  it should "treat a missing destination as click and then skip without Chrome" in :
    withArchive: archive =>
      writeCatalog(
        archive,
        "album\tidSrc\t1\tSource Title",
        "item\tidSrc\tpic.jpg\t-\tkeyMove\t-",
        "album\tidRen\t0\tOld Title"
      )
      val requests: File = writeRequests(
        archive,
        request("r1", "move-item", "idSrc", "keyMove", "pic.jpg", "idMissing"),
        request("r2", "rename-album", "idRen", "", "Old Title", "July")
      )
      val beforeRequests: String = read(requests)
      withSeams(archive): (session, places) =>
        var opened: Int = 0
        Refresh.openAct = _ =>
          opened += 1
          new Clicker(onRename = (_, _) => ())
        shown(exec(session, "agent", "apply-requests"), "idSrc", "idMissing", "idRen", "keyMove") shouldBe
          """r1 move-item click "pic.jpg"
            |r2 rename-album click "Old Title"
            |""".stripMargin
        places.lock.exists shouldBe false
        shown(exec(session, "agent", "apply-requests", "--apply"), "idSrc", "idMissing", "idRen", "keyMove") shouldBe
          """r1 move-item skip "pic.jpg"
            |r2 rename-album satisfied "July"
            |""".stripMargin
        opened shouldBe 1
        outcomes(archive).exists shouldBe false
        read(requests) shouldBe beforeRequests
        results(archive).exists shouldBe false

  it should "classify unset, same-album, and date edges without clicking" in :
    withArchive: archive =>
      writeCatalog(
        archive,
        "album\tidU\t1\tUnset Title",
        "error\tidU\t1\t1\t1",
        "unset\tidU\t-\tkeyUnset\t-",
        "album\tidSrc\t1\tSource Title",
        "item\tidSrc\tpic.jpg\t-\tkeyMove\t-",
        "album\tidDate\t1\tDate Title",
        "item\tidDate\tpic.jpg\t2010-07-03\tkeyDate\t-",
        "album\tidDate2\t1\tDate Id",
        "item\tidDate2\tpic.jpg\t2010-01-01\tkeyDate2\t-",
        "album\tidDot\t1\tDot Title",
        "item\tidDot\t.\t-\tkeyDot\t-",
        "album\tidDots\t1\tDots Title",
        "item\tidDots\t..\t-\tkeyDots\t-"
      )
      val requests: File = writeRequests(
        archive,
        request("r1", "delete-item", "idU", "keyUnset", "pic.jpg", ""),
        request("r2", "move-item", "idSrc", "keyMove", "pic.jpg", "idSrc"),
        request("r3", "set-date", "", "keyDate", "other.jpg", "2010-07-03 09:05"),
        request("r4", "set-date", "idDate2", "keyDate2", "pic.jpg", "2010-07-03 09:05"),
        request("r5", "download-item", "idDot", "keyDot", ".", ""),
        request("r6", "download-item", "idDots", "keyDots", "..", "")
      )
      val before: String = read(requests)
      withSeams(archive): (session, places) =>
        var opened: Int = 0
        Refresh.openAct = _ =>
          opened += 1
          new Clicker()
        shown(
          exec(session, "agent", "apply-requests"),
          "idU", "keyUnset", "idSrc", "keyMove", "idDate", "keyDate", "idDate2", "keyDate2", "idDot", "keyDot",
          "idDots", "keyDots"
        ) shouldBe
          """r1 delete-item skip "pic.jpg"
            |r2 move-item skip "pic.jpg"
            |r3 set-date satisfied "other.jpg"
            |r4 set-date skip "pic.jpg"
            |r5 download-item skip "."
            |r6 download-item skip ".."
            |""".stripMargin
        opened shouldBe 0
        places.lock.exists shouldBe false
        read(requests) shouldBe before
        outcomes(archive).exists shouldBe false

  it should "reject a bad outcomes file before Chrome even when nothing would click" in :
    val cases: Seq[(String, String, Boolean)] = Seq(
      ("", "outcomes are not version 1", false),
      ("# gphoto-request-outcomes 1\nAF1QipXXX\tneeds-agent\t1\n", "request id is not valid", false),
      ("# gphoto-request-outcomes 1\nr1\tneeds-agent\t1\nr1\tdone\t2\n", "duplicate request id", true),
      ("# gphoto-request-outcomes 1\nr1\tneeds-agent\n", "wrong column count", true)
    )
    cases.foreach: (body, message, withRow) =>
      withArchive: archive =>
        if withRow then
          writeCatalog(archive, "album\tidOld\t0\tOld Title")
          writeRequests(archive, request("r1", "rename-album", "idOld", "", "Old Title", "July"))
        val file: File = outcomes(archive)
        file.getParentFile.mkdirs()
        JFiles.writeString(file.toPath, body, StandardCharsets.UTF_8)
        val before: String = if requests(archive).isFile then read(requests(archive)) else ""
        withSeams(archive): (session, places) =>
          var opened: Int = 0
          Refresh.openAct = _ =>
            opened += 1
            new Clicker()
          usage(exec(session, "agent", "apply-requests")) shouldBe message
          usage(exec(session, "agent", "apply-requests", "--apply")) shouldBe message
          opened shouldBe 0
          places.lock.exists shouldBe false
          if withRow then read(requests(archive)) shouldBe before
          else requests(archive).exists shouldBe false
          results(archive).exists shouldBe false

  it should "print nothing when the requests file is absent" in :
    withArchive: archive =>
      withSeams(archive): (session, places) =>
        var opened: Int = 0
        Refresh.openAct = _ =>
          opened += 1
          new Clicker()
        exec(session, "agent", "apply-requests") shouldBe Result.Ok("", Json.Arr(Seq.empty))
        exec(session, "agent", "apply-requests", "--apply") shouldBe Result.Ok("", Json.Arr(Seq.empty))
        opened shouldBe 0
        places.lock.exists shouldBe false
        File(archive, ".gphoto-sync").exists shouldBe false

  it should "stay in the calling process and out of mcp tools" in :
    Cli.parse(List("agent", "apply-requests")) shouldBe Cli.Run(Cli.ApplyRequestsArgs(perform = false))
    Cli.parse(List("agent", "apply-requests", "--apply")) shouldBe Cli.Run(Cli.ApplyRequestsArgs(perform = true))
    Cli.mcpTools.exists(_.name == "apply-requests") shouldBe false
    Cli.parse(List("--help")) match
      case Cli.Help(text) =>
        text.linesIterator.exists(_.startsWith("apply-requests")) shouldBe false
      case other =>
        fail(other.toString)
    withArchive: archive =>
      val session: Session = sessionFor(archive)
      var ran: Boolean = false
      val (_, line) = Cli.serveResponse(
        session,
        """{"argv":["agent","apply-requests"]}""",
        (_, _) =>
          ran = true
          (session, Result.Ok("ran\n", Json.Null))
      )
      ran shouldBe false
      line should include ("apply-requests runs in the calling process")

  private val planHidden: Seq[String] = Seq(
    "idOld", "idNew", "idOther", "idMissing", "idSrc", "idDst", "idGone", "idHas", "idPartial",
    "idDate", "idDate2", "idDate3", "idDesc", "idDesc2", "idDesc3", "idDown", "idHave", "idSlash",
    "idDelGone", "idDel", "idUp", "idMis", "idOwn", "idDup",
    "keyMove", "keyHas", "keyPartial", "keyNope", "keyDate", "keyDate2", "keyDate3",
    "keyDesc", "keyDesc2", "keyDesc3", "keyDown", "keyHave", "keySlash", "keyGone",
    "keyDel", "keyUp", "keyMis", "keyOwn", "keyDup1", "keyDup2"
  )

  private val planCatalog: Seq[String] = Seq(
    "album\tidOld\t0\tOld Title",
    "album\tidNew\t0\tNew Title",
    "album\tidOther\t0\tOther Title",
    "album\tidSrc\t1\tSource Title",
    "item\tidSrc\tpic.jpg\t2010-07-03\tkeyMove\t-",
    "album\tidDst\t0\tDest Title",
    "album\tidGone\t0\tGone Title",
    "album\tidHas\t1\tHas Title",
    "item\tidHas\tpic.jpg\t2010-07-05\tkeyHas\t-",
    "album\tidPartial\t1\tPartial Title",
    "item\tidPartial\tpic.jpg\t2010-07-04\tkeyPartial\t-",
    "album\tidDate\t1\tDate Title",
    "item\tidDate\tpic.jpg\t2010-07-03\tkeyDate\t-",
    "album\tidDate2\t1\tDate Click",
    "item\tidDate2\tpic.jpg\t2010-01-01\tkeyDate2\t-",
    "album\tidDate3\t1\tDate Bad",
    "item\tidDate3\tpic.jpg\t2010-01-02\tkeyDate3\t-",
    "album\tidDesc\t1\tDesc Title",
    "item\tidDesc\tpic.jpg\t-\tkeyDesc\t-",
    "album\tidDesc2\t1\tDesc Bad",
    "item\tidDesc2\tother.jpg\t-\tkeyDesc2\t-",
    "album\tidDesc3\t1\tDesc Id",
    "item\tidDesc3\tpic.jpg\t-\tkeyDesc3\t-",
    "album\tidDown\t1\tDown Title",
    "item\tidDown\tpic.jpg\t2010-07-03\tkeyDown\t-",
    "album\tidHave\t1\tHave Title",
    "item\tidHave\thave.jpg\t2010-07-03\tkeyHave\t-",
    "album\tidSlash\t1\tSlash Title",
    "item\tidSlash\ta/b.jpg\t2010-07-03\tkeySlash\t-",
    "album\tidDelGone\t0\tDel Gone",
    "album\tidDel\t1\tDel Title",
    "item\tidDel\tpic.jpg\t-\tkeyDel\t-",
    "album\tidUp\t1\tUp Title",
    "item\tidUp\tup.jpg\t-\tkeyUp\tAda",
    "album\tidMis\t1\tMis Title",
    "item\tidMis\treal.jpg\t-\tkeyMis\t-",
    "album\tidOwn\t1\tOwn Title",
    "owner\tidOwn\tAda",
    "item\tidOwn\tpic.jpg\t-\tkeyOwn\t-",
    "album\tidDup\t2\tDup Title",
    "item\tidDup\tpic.jpg\t-\tkeyDup1\t-",
    "item\tidDup\tpic.jpg\t-\tkeyDup2\t-"
  )

  private val planRows: Seq[String] = Seq(
    request("r1", "rename-album", "idOld", "", "Old Title", "New Title"),
    request("r2", "rename-album", "idNew", "", "Old Title", "New Title"),
    request("r3", "rename-album", "idOther", "", "Old Title", "New Title"),
    request("r4", "rename-album", "idMissing", "", "Old Title", "New Title"),
    request("r5", "move-item", "idSrc", "keyMove", "pic.jpg", "idDst"),
    request("r6", "move-item", "idGone", "keyHas", "pic.jpg", "idHas"),
    request("r7", "move-item", "idPartial", "keyPartial", "pic.jpg", "idHas"),
    request("r8", "move-item", "idGone", "keyNope", "pic.jpg", "idDst"),
    request("r9", "set-date", "", "keyDate", "pic.jpg", "2010-07-03 09:05"),
    request("r10", "set-date", "", "keyDate2", "pic.jpg", "2010-07-03 09:05"),
    request("r11", "set-date", "", "keyDate3", "nope.jpg", "2010-08-01 09:05"),
    request("r12", "set-date", "", "keyMissing", "pic.jpg", "2010-07-03 09:05"),
    request("r13", "set-description", "", "keyDesc", "pic.jpg", "Hello"),
    request("r14", "set-description", "", "keyDesc2", "nope.jpg", "Hello"),
    request("r15", "set-description", "idDesc3", "keyDesc3", "pic.jpg", "Hello"),
    request("r16", "download-item", "idDown", "keyDown", "pic.jpg", ""),
    request("r17", "download-item", "idHave", "keyHave", "have.jpg", ""),
    request("r18", "download-item", "idSlash", "keySlash", "a/b.jpg", ""),
    request("r19", "delete-item", "idDelGone", "keyGone", "pic.jpg", ""),
    request("r20", "delete-item", "idDel", "keyDel", "pic.jpg", ""),
    request("r21", "delete-item", "idUp", "keyUp", "up.jpg", ""),
    request("r22", "delete-item", "idMis", "keyMis", "other.jpg", ""),
    request("r23", "delete-item", "idOwn", "keyOwn", "pic.jpg", ""),
    request("r24", "move-item", "idGone", "keyDup1", "pic.jpg", "idDup")
  )

  private val planText: String =
    """r1 rename-album click "Old Title"
      |r2 rename-album satisfied "New Title"
      |r3 rename-album skip "Other Title"
      |r4 rename-album skip "Old Title"
      |r5 move-item click "pic.jpg"
      |r6 move-item satisfied "pic.jpg"
      |r7 move-item skip "pic.jpg"
      |r8 move-item skip "pic.jpg"
      |r9 set-date satisfied "pic.jpg"
      |r10 set-date click "pic.jpg"
      |r11 set-date skip "nope.jpg"
      |r12 set-date skip "pic.jpg"
      |r13 set-description click "pic.jpg"
      |r14 set-description skip "nope.jpg"
      |r15 set-description skip "pic.jpg"
      |r16 download-item click "pic.jpg"
      |r17 download-item satisfied "have.jpg"
      |r18 download-item skip "a/b.jpg"
      |r19 delete-item satisfied "pic.jpg"
      |r20 delete-item click "pic.jpg"
      |r21 delete-item skip "up.jpg"
      |r22 delete-item skip "other.jpg"
      |r23 delete-item skip "pic.jpg"
      |r24 move-item skip "pic.jpg"
      |""".stripMargin

  private def shown(result: Result, hidden: String*): String = result match
    case Result.Ok(text, Json.Arr(rows)) =>
      text should not include "blocked"
      val lines: Seq[String] =
        if text.isEmpty then Seq.empty
        else
          text.endsWith("\n") shouldBe true
          text.dropRight(1).split("\n", -1).toSeq
      rows.length shouldBe lines.length
      rows.foreach:
        case Json.Obj(fields) =>
          fields.map(_._1) shouldBe Seq("id", "action", "status", "label")
        case other =>
          fail(other.toString)
      hidden.foreach: token =>
        withClue(token):
          text should not include token
      text
    case other =>
      fail(other.toString)

  private def usage(result: Result): String = result match
    case Result.Usage(message) => message
    case other => fail(other.toString)

  private def exec(session: Session, args: String*): Result =
    Cli.parse(args.toList) match
      case Cli.Run(parsed) =>
        Cli.toOperation(parsed).execute(session)._2
      case other =>
        fail(other.toString)

  private def sessionFor(archive: File): Session = Session(rootFor(archive))

  private def rootFor(archive: File): Root =
    val home: File = File(System.getProperty("user.home"))
    Root(Configuration(home.toPath.relativize(archive.toPath).toString, zone))

  private def request(
    id: String,
    action: String,
    albumId: String,
    itemKey: String,
    current: String,
    updated: String
  ): String =
    Seq(id, action, albumId, itemKey, current, updated).mkString("\t")

  private def writeCatalog(archive: File, lines: String*): File =
    writeLines(catalog(archive), "# gphoto-catalog 2", lines)

  private def writeRequests(archive: File, lines: String*): File =
    writeLines(requests(archive), "# gphoto-requests 2", lines)

  private def writeOutcomes(archive: File, lines: String*): File =
    writeLines(outcomes(archive), "# gphoto-request-outcomes 1", lines)

  private def writeLines(file: File, header: String, lines: Seq[String]): File =
    file.getParentFile.mkdirs()
    JFiles.writeString(file.toPath, (header +: lines).mkString("", "\n", "\n"), StandardCharsets.UTF_8)
    file

  private def catalog(archive: File): File = File(archive, ".gphoto-sync/catalog.txt")
  private def requests(archive: File): File = File(archive, ".gphoto-sync/requests.tsv")
  private def outcomes(archive: File): File = File(archive, ".gphoto-sync/requests.tsv.outcomes")
  private def results(archive: File): File = File(archive, ".gphoto-sync/results.tsv")

  private def read(file: File): String = JFiles.readString(file.toPath, StandardCharsets.UTF_8)

  private def withArchive(body: File => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("apply-requests").toFile
    try body(archive)
    finally deleteTree(archive)

  private def withSeams(archive: File)(body: (Session, Refresh.Places) => Unit): Unit =
    val parent: File = JFiles.createTempDirectory("apply-lock").toFile
    val places: Refresh.Places = Refresh.Places(File(parent, "work"), File(parent, "lock"))
    val previousPlaces: () => Refresh.Places = Refresh.placesOf
    val previousAct: (Refresh.Notice => Unit) => PhotosAct = Refresh.openAct
    val previousMove: (File, File) => Unit = RefreshPlan.moveReplacing
    val previousBefore: () => Unit = Outcomes.beforeWrite
    val previousAfter: Result => Unit = AgentCommands.afterHeld
    var failed: Boolean = false
    Refresh.placesOf = () => places
    Refresh.openAct = _ => throw AssertionError("chrome")
    RefreshPlan.moveReplacing = (from, to) =>
      JFiles.move(from.toPath, to.toPath, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    Outcomes.beforeWrite = () => ()
    try body(sessionFor(archive), places)
    catch
      case error: Throwable =>
        failed = true
        throw error
    finally
      var leaked: Boolean = false
      if !failed && places.lock.isFile then
        Refresh.claim(places.lock) match
          case Refresh.Claim.Owned(channel, lock) =>
            Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            leaked = true
      Refresh.placesOf = previousPlaces
      Refresh.openAct = previousAct
      RefreshPlan.moveReplacing = previousMove
      Outcomes.beforeWrite = previousBefore
      AgentCommands.afterHeld = previousAfter
      if leaked then fail("lock still held")
      deleteTree(parent)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    if file.exists then JFiles.deleteIfExists(file.toPath)

  private class Clicker(
    onRename: (Catalog.Album, String) => Unit = (_, _) => throw AssertionError("rename"),
    onDescription: (Catalog.Item, String) => Unit = (_, _) => throw AssertionError("description"),
    onDate: (Catalog.Item, String) => Unit = (_, _) => throw AssertionError("date"),
    onDownload: (Catalog.Item, File) => Unit = (_, _) => throw AssertionError("download"),
    onPut: (Catalog.Item, Catalog.Album, String) => Catalog.Item = (_, _, _) => throw AssertionError("put"),
    onTake: (Catalog.Album, Catalog.Item) => Unit = (_, _) => throw AssertionError("take"),
    onTrash: (Catalog.Album, Catalog.Item) => Unit = (_, _) => throw AssertionError("trash")
  ) extends PhotosAct:
    def albumCards(): Seq[AlbumCard] = throw AssertionError("cards")
    def readOwner(card: AlbumCard): Option[String] = throw AssertionError("owner")
    def readAlbum(card: AlbumCard): Catalog.Album = throw AssertionError("read")
    def close(): Unit = ()
    def renameAlbum(album: Catalog.Album, updated: String): Unit = onRename(album, updated)
    def setDescription(item: Catalog.Item, text: String): Unit = onDescription(item, text)
    def setDate(item: Catalog.Item, local: String): Unit = onDate(item, local)
    def downloadItem(item: Catalog.Item, destination: File): Unit = onDownload(item, destination)
    def putInAlbum(item: Catalog.Item, destination: Catalog.Album, current: String): Catalog.Item =
      onPut(item, destination, current)
    def takeOut(album: Catalog.Album, item: Catalog.Item): Unit = onTake(album, item)
    def trash(album: Catalog.Album, item: Catalog.Item): Unit = onTrash(album, item)
