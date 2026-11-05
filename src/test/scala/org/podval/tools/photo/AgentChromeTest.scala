package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import java.io.{ByteArrayOutputStream, File}
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, StandardOpenOption}
import java.time.LocalDate
import org.podval.tools.photo.chrome.{AgentChrome, AlbumCard, PhotosAct, PlaywrightPhotosUi}

class AgentChromeTest extends AnyFlatSpec, AgentWriteSupport:
  "rename-album" should "leave the catalog unchanged when the step throws" in:
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
          session,
          "agent",
          "rename-album",
          "--title",
          "2010/07 ",
          "--current",
          "2010/07 ",
          "--new",
          "July"
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

  it should "retitle only that album line when the live title is clean" in:
    withArchive: archive =>
      writePair(archive)
      withAct(archive): (session, places, opened) =>
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => ()):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session,
          "agent",
          "rename-album",
          "--title",
          "2010/07 ",
          "--current",
          "2010/07 ",
          "--new",
          "July"
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

  it should "leave the partial when the retitle check fails" in:
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => ())
        val (_, result) = exec(
          session,
          "agent",
          "rename-album",
          "--title",
          "2010/07 ",
          "--current",
          "2010/07 ",
          "--new",
          "July\nX"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery refresh-only")
            text should include("catalog unchanged")
            text should not include ("album set")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "catalog") shouldBe Json.Str("unchanged")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").isFile shouldBe true
        places.directory.exists shouldBe false

  it should "report already running without opening the browser" in:
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
                session,
                "agent",
                "rename-album",
                "--title",
                "2010/07 ",
                "--current",
                "2010/07 ",
                "--new",
                "July"
              )
              result match
                case Result.Failed(text, value, message) =>
                  message shouldBe Refresh.AlreadyRunning
                  Cli.processCode(result) shouldBe 2
                  text should startWith("already running\n")
                  text should include("phase=items")
                  text should not include ("AF1Qip")
                  value shouldBe Json.Null
                case other =>
                  fail(other.toString)
              opened() shouldBe false
              File(places.directory, "albums.tsv").exists shouldBe false
            finally Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  it should "strip every AF1Qip token on stderr and stay in the caller" in:
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
        err should include("Photo Archive Tool:")
        err should include("no album titled \"album album\"")
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
        line should include("rename-album runs in the calling process")
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

  it should "run in the one-shot process while serve holds its lock" in:
    withArchive: archive =>
      writePair(archive)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => ())
        val argv: List[String] = List(
          "agent",
          "rename-album",
          "--title",
          "2010/07 ",
          "--current",
          "2010/07 ",
          "--new",
          "July"
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

  "set-description" should "leave the catalog unchanged when the step throws" in:
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onDescription = (_, text) => throw IllegalStateException(s"step missed AF1QipLEAK $text AF1QipOTHER")
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session,
          "agent",
          "set-description",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--new",
          "Hello"
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

  it should "stay refresh-only when the panel does not show the description" in:
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onDescription = (_, _) => throw new Refresh.ActMiss("panel does not show the description", confirmed = true)
          )
        val (_, result) = exec(
          session,
          "agent",
          "set-description",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--new",
          "Hello"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery refresh-only")
            text should include("catalog unchanged")
            text should include("panel does not show the description")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        places.directory.exists shouldBe false

  it should "leave the catalog byte-identical when the description is set" in:
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        val seen = scala.collection.mutable.ListBuffer.empty[String]
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onDescription = (item, text) =>
              item.filename shouldBe "pic.jpg"
              seen += text
          ):
            override def close(): Unit = closed = true
        val (kept, result) = exec(
          session,
          "agent",
          "set-description",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--new",
          "Hello"
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
          kept,
          "agent",
          "set-description",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--new",
          ""
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

  "set-date" should "write only that item's date when the read shows the civil day" in:
    withArchive: archive =>
      writeDated(archive)
      withAct(archive): (session, places, _) =>
        var seen: Option[String] = None
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onDate = (item, local) =>
              item.filename shouldBe "pic.jpg"
              item.key shouldBe "keyPic"
              seen = Some(local)
          ):
            override def close(): Unit = closed = true
        val (next, result) = exec(
          session,
          "agent",
          "set-date",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--new",
          "2010-07-19 09:05"
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

  it should "leave the old bytes when the read shows a different day" in:
    withArchive: archive =>
      val catalog: String = writeDated(archive)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onDate = (_, _) => throw new Refresh.ActMiss("read date differs", confirmed = true)
          )
        val (_, result) = exec(
          session,
          "agent",
          "set-date",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--new",
          "2010-07-19 09:05"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("filename \"pic.jpg\"")
            text should include("step Date taken, then Save, Done, or OK")
            text should include("recovery refresh-only")
            text should include("catalog unchanged")
            text should include("read date differs")
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

  it should "write nothing when the date step misses before confirm" in:
    withArchive: archive =>
      val catalog: String = writeDated(archive)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onDate = (_, _) => throw new Refresh.ActMiss("Year is absent", confirmed = false)
          )
        val (_, result) = exec(
          session,
          "agent",
          "set-date",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--new",
          "2010-07-19 09:05"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery one-click")
            text should include("catalog unchanged")
            text should include("Year is absent")
            field(value, "recovery") shouldBe Json.Str("one-click")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false

  it should "stay refresh-only when the catalog read throws after confirm" in:
    withArchive: archive =>
      val catalog: String = writeDated(archive)
      val live: File = File(archive, ".gphoto-sync/catalog.txt")
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onDate = (_, _) => live.setReadable(false, false))
        try
          val (_, result) = exec(
            session,
            "agent",
            "set-date",
            "--item",
            "keyPic",
            "--current",
            "pic.jpg",
            "--new",
            "2010-07-19 09:05"
          )
          result match
            case Result.Failed(text, value, message) =>
              message shouldBe Refresh.MutationFailed
              text should include("recovery refresh-only")
              text should include("catalog unchanged")
              field(value, "recovery") shouldBe Json.Str("refresh-only")
            case other =>
              fail(other.toString)
          live.setReadable(true, false)
          JFiles.readString(live.toPath) shouldBe catalog
          File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
          File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        finally live.setReadable(true, false)

  it should "answer both edits from the calling process" in:
    withArchive: archive =>
      writePair(archive)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => fail("accept opened"))
        val (_, described) = Cli.serveResponse(
          session,
          """{"argv":["agent","set-description","--item","keyPic","--current","pic.jpg","--new","Hello"]}""",
          (_, _) => fail("set-description was forwarded")
        )
        described should include("set-description runs in the calling process")
        val (_, dated) = Cli.serveResponse(
          session,
          """{"argv":["agent","set-date","--item","keyPic","--current","pic.jpg","--new","2010-07-19 09:05"]}""",
          (_, _) => fail("set-date was forwarded")
        )
        dated should include("set-date runs in the calling process")
        val (_, downloaded) = Cli.serveResponse(
          session,
          """{"argv":["agent","download-item","--title","2010/07 ","--item","keyPic","--current","pic.jpg"]}""",
          (_, _) => fail("download-item was forwarded")
        )
        downloaded should include("download-item runs in the calling process")
        val (_, added) = Cli.serveResponse(
          session,
          """{"argv":["agent","add-to-album","--title","2010/07 ","--item","keyPic","--current","pic.jpg","--dest","Keep Title"]}""",
          (_, _) => fail("add-to-album was forwarded")
        )
        added should include("add-to-album runs in the calling process")
        val (_, removed) = Cli.serveResponse(
          session,
          """{"argv":["agent","remove-from-album","--title","2010/07 ","--item","keyPic","--current","pic.jpg"]}""",
          (_, _) => fail("remove-from-album was forwarded")
        )
        removed should include("remove-from-album runs in the calling process")
        val (_, moved) = Cli.serveResponse(
          session,
          """{"argv":["agent","move-item","--title","2010/07 ","--item","keyPic","--current","pic.jpg","--dest","Keep Title"]}""",
          (_, _) => fail("move-item was forwarded")
        )
        moved should include("move-item runs in the calling process")
        val (_, deleted) = Cli.serveResponse(
          session,
          """{"argv":["agent","delete-item","--title","2010/07 ","--item","keyPic","--current","pic.jpg"]}""",
          (_, _) => fail("delete-item was forwarded")
        )
        deleted should include("delete-item runs in the calling process")
        val (_, uploaded) = Cli.serveResponse(
          session,
          """{"argv":["agent","upload-item","--dest","Keep Title","2010/07/01/pic.jpg"]}""",
          (_, _) => fail("upload-item was forwarded")
        )
        uploaded should include("upload-item runs in the calling process")
        val (_, created) = Cli.serveResponse(
          session,
          """{"argv":["agent","create-album","--title","Probe"]}""",
          (_, _) => fail("create-album was forwarded")
        )
        created should include("create-album runs in the calling process")
        val (_, deletedAlbum) = Cli.serveResponse(
          session,
          """{"argv":["agent","delete-album","--title","Probe"]}""",
          (_, _) => fail("delete-album was forwarded")
        )
        deletedAlbum should include("delete-album runs in the calling process")
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onDescription =
              (_, text) => if text.contains("AF1Qip") then throw IllegalStateException(s"step missed $text")
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
          new RenameFake((_, _) => (), onDescription = (_, _) => throw IllegalStateException("step missed AF1QipLEAK"))
        val (failed, out, err) = shellOf(
          session,
          "agent set-description --item keyPic --current pic.jpg --new Hello"
        )
        failed shouldBe 0
        out should include("failed set-description")
        out should include("recovery one-click")
        out should not include ("AF1Qip")
        out should not include ("mutation failed")
        err should not include ("mutation failed")
        err should not include ("Photo Archive Tool:")
        err should not include ("AF1Qip")

  "download-item" should "keep a staged file and not open Chrome" in:
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
          session,
          "agent",
          "download-item",
          "--title",
          "2010/07 ",
          "--item",
          "keyPic",
          "--current",
          "IMG_pic.JPG"
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
          session,
          "agent",
          "download-item",
          "--title",
          "Keep Title",
          "--item",
          "keyKeep",
          "--current",
          "IMG_pic.JPG"
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

  it should "keep an undated staged file" in:
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
          session,
          "agent",
          "download-item",
          "--title",
          "Loose",
          "--item",
          "keyPic",
          "--current",
          "IMG_pic.JPG"
        )
        result match
          case Result.Ok(text, _) =>
            text shouldBe "download-item \"IMG_pic.JPG\"\n"
          case other =>
            fail(other.toString)
        JFiles.readAllBytes(staged.toPath).sameElements(body) shouldBe true
        JFiles.readString(catalogFile.toPath) shouldBe catalog

  it should "use that album's item when the key is on two albums" in:
    withArchive: archive =>
      writeFixture(archive)
      val staged: File = File(archive, ".gphoto-sync/.download/2010/07/03/pic.jpg")
      staged.getParentFile.mkdirs()
      val body: Array[Byte] = Array[Byte](7)
      JFiles.write(staged.toPath, body)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onDownload = (_, _) => fail("download called"))
        val (_, result) = exec(
          session,
          "agent",
          "download-item",
          "--title",
          "Alpha Name",
          "--item",
          "keyShare",
          "--current",
          "pic.jpg"
        )
        result match
          case Result.Ok(text, value) =>
            text shouldBe "download-item \"pic.jpg\"\n"
            field(value, "albumId") shouldBe Json.Str("albShareA")
            field(value, "itemKey") shouldBe Json.Str("keyShare")
          case other =>
            fail(other.toString)
        JFiles.readAllBytes(staged.toPath).sameElements(body) shouldBe true

  it should "write the staging file and leave the catalog unchanged" in:
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        var closed = false
        var seen: Option[File] = None
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onDownload = (item, dest) =>
              item.filename shouldBe "pic.jpg"
              item.key shouldBe "keyPic"
              seen = Some(dest)
              JFiles.write(dest.toPath, Array[Byte](9, 8, 7))
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session,
          "agent",
          "download-item",
          "--title",
          "2010/07 ",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg"
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

  it should "report one-click when the download writes nothing" in:
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onDownload = (_, _) => ()):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session,
          "agent",
          "download-item",
          "--title",
          "2010/07 ",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg"
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

  it should "report one-click when Download is absent" in:
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onDownload = (_, _) => throw new Refresh.ActMiss("Download is absent AF1QipLEAK", confirmed = false)
          )
        val (_, result) = exec(
          session,
          "agent",
          "download-item",
          "--title",
          "2010/07 ",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery one-click")
            text should not include ("refresh-only")
            text should include("Download is absent")
            text should not include ("AF1Qip")
            field(value, "recovery") shouldBe Json.Str("one-click")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog

  it should "report already running without opening the browser" in:
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
                session,
                "agent",
                "download-item",
                "--title",
                "2010/07 ",
                "--item",
                "keyPic",
                "--current",
                "pic.jpg"
              )
              result match
                case Result.Failed(text, value, message) =>
                  message shouldBe Refresh.AlreadyRunning
                  Cli.processCode(result) shouldBe 2
                  text should startWith("already running\n")
                  text should include("phase=items")
                  text should not include ("AF1Qip")
                  value shouldBe Json.Null
                case other =>
                  fail(other.toString)
              JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
              File(archive, ".gphoto-sync/.download/2010/07/01/pic.jpg").exists shouldBe false
            finally Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  "confirmedAdd" should "accept only one new key whose filename is current" in:
    val current: String = "pic.jpg"
    val hit: Catalog.Item = Catalog.Item(current, Some(LocalDate.of(2011, 1, 2)), "keyNew", Some("Bea"))
    val wrong: Catalog.Item = Catalog.Item("other.jpg", None, "keyBad", None)
    val blank: Catalog.Item = Catalog.Item("", None, "keyBlank", None)
    def shown(before: Set[String], after: Seq[String], named: Map[String, Catalog.Item]): String =
      AgentChrome.confirmedAdd(before, after, named, current) match
        case Some(item) => s"${item.key} ${item.filename}"
        case None       => "none"
    shown(Set("old"), Seq("old", "keyNew"), Map("keyNew" -> hit)) shouldBe "keyNew pic.jpg"
    shown(Set("old"), Seq("old", "keyNew"), Map.empty) shouldBe "none"
    shown(Set("old"), Seq("old", "keyBlank"), Map("keyBlank" -> blank)) shouldBe "none"
    shown(Set.empty, Seq("keyNew", "keyBad"), Map("keyNew" -> hit, "keyBad" -> wrong)) shouldBe "none"
    shown(Set("old"), Seq("old", "keyBad"), Map("keyBad" -> wrong)) shouldBe "none"

  "add-to-album" should "refuse an owner line before Chrome opens" in:
    rejectPut(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title",
        "owner\talbDst\tAda"
      ),
      Seq(
        "agent",
        "add-to-album",
        "--title",
        "Source Title",
        "--item",
        "keyPic",
        "--current",
        "pic.jpg",
        "--dest",
        "Dest Title"
      ),
      "destination has an owner"
    )

  it should "refuse auto-add before Chrome opens" in:
    rejectPut(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title",
        "auto-add\talbDst"
      ),
      Seq(
        "agent",
        "add-to-album",
        "--title",
        "Source Title",
        "--item",
        "keyPic",
        "--current",
        "pic.jpg",
        "--dest",
        "Dest Title"
      ),
      "destination is auto-add"
    )

  it should "refuse unread names before Chrome opens" in:
    rejectPut(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t2\tDest Title",
        "error\talbDst\t1\t2\t1",
        "unset\talbDst\t2010-07-09\tkeyUnset\t-"
      ),
      Seq(
        "agent",
        "add-to-album",
        "--title",
        "Source Title",
        "--item",
        "keyPic",
        "--current",
        "pic.jpg",
        "--dest",
        "Dest Title"
      ),
      "destination has unread names"
    )

  it should "refuse an excepted stem before Chrome opens" in:
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
          session,
          "agent",
          "add-to-album",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Dest"
        ) shouldBe "destination excepts \"pic\""
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "refuse the source album before Chrome opens" in:
    rejectPut(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-"
      ),
      Seq(
        "agent",
        "add-to-album",
        "--title",
        "Source Title",
        "--item",
        "keyPic",
        "--current",
        "pic.jpg",
        "--dest",
        "Source Title"
      ),
      "destination is the source album"
    )

  it should "refuse an ambiguous destination before Chrome opens" in:
    rejectPut(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title",
        "album\talbDst2\t0\tDest Title"
      ),
      Seq(
        "agent",
        "add-to-album",
        "--title",
        "Source Title",
        "--item",
        "keyPic",
        "--current",
        "pic.jpg",
        "--dest",
        "Dest Title"
      ),
      "more than one album titled \"Dest Title\""
    )

  it should "not refuse an except when the local file is absent" in:
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
          new RenameFake(
            (_, _) => (),
            onPut = (_, _, _) =>
              called = true
              throw new Refresh.ActMiss("Add to album is absent", confirmed = false)
          )
        val (_, result) = exec(
          session,
          "agent",
          "add-to-album",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Dest"
        )
        called shouldBe true
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery one-click")
            text should not include ("refresh-only")
            field(value, "recovery") shouldBe Json.Str("one-click")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "fail one-click on New album and click nothing" in:
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
          session,
          "agent",
          "add-to-album",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "New album"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery one-click")
            text should include("New album is not a destination")
            text should not include ("refresh-only")
            field(value, "recovery") shouldBe Json.Str("one-click")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false
        released(places)

  it should "write the confirmed destination item and adjust an error line" in:
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
          new RenameFake(
            (_, _) => (),
            onPut = (item, destination, current) =>
              current shouldBe "pic.jpg"
              item.key shouldBe "keyPic"
              destination.title shouldBe "Dest Title"
              Catalog.Item("ignored.jpg", Some(LocalDate.of(2011, 1, 2)), "keyNew", None)
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session,
          "agent",
          "add-to-album",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Dest Title"
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

  it should "write no error line when the destination has none" in:
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
          new RenameFake(
            (_, _) => (),
            onPut = (_, _, _) => Catalog.Item("pic.jpg", Some(LocalDate.of(2011, 1, 2)), "keyNew", None)
          )
        val (_, result) = exec(
          session,
          "agent",
          "add-to-album",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Dest Title"
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

  it should "stay refresh-only when the live read is not the clean key" in:
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onPut = (_, _, _) => throw new Refresh.ActMiss("live key differs", confirmed = true)
          )
        val (_, result) = exec(
          session,
          "agent",
          "add-to-album",
          "--title",
          "2010/07 ",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Keep Title"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery refresh-only")
            text should include("catalog unchanged")
            text should include("live key differs")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "albumId") shouldBe Json.Str("alb07")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) should not include ("unset")
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        places.directory.exists shouldBe false

  it should "report one-click when Add to album is absent" in:
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onPut = (_, _, _) => throw new Refresh.ActMiss("Add to album is absent AF1QipLEAK", confirmed = false)
          )
        val (_, result) = exec(
          session,
          "agent",
          "add-to-album",
          "--title",
          "2010/07 ",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Keep Title"
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

  it should "leave the catalog unchanged when the new key is already there" in:
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
        Refresh.openAct =
          _ => new RenameFake((_, _) => (), onPut = (_, _, _) => Catalog.Item("pic.jpg", None, "keyNew", None))
        val (_, result) = exec(
          session,
          "agent",
          "add-to-album",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Dest Title"
        )
        result match
          case Result.Failed(text, _, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery refresh-only")
            text should include("repeated item key")
            text should include("catalog unchanged")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false

  it should "stay refresh-only when the catalog read throws after confirm" in:
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
          new RenameFake(
            (_, _) => (),
            onPut = (_, _, _) =>
              catalogFile.setReadable(false, false)
              Catalog.Item("pic.jpg", None, "keyNew", None)
          )
        try
          val (_, result) = exec(
            session,
            "agent",
            "add-to-album",
            "--title",
            "Source Title",
            "--item",
            "keyPic",
            "--current",
            "pic.jpg",
            "--dest",
            "Dest Title"
          )
          result match
            case Result.Failed(text, value, message) =>
              message shouldBe Refresh.MutationFailed
              text should include("recovery refresh-only")
              text should include("catalog unchanged")
              field(value, "recovery") shouldBe Json.Str("refresh-only")
            case other =>
              fail(other.toString)
          catalogFile.setReadable(true, false)
          JFiles.readString(catalogFile.toPath) shouldBe catalog
          File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        finally catalogFile.setReadable(true, false)

  it should "report already running without opening the browser" in:
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
                session,
                "agent",
                "add-to-album",
                "--title",
                "2010/07 ",
                "--item",
                "keyPic",
                "--current",
                "pic.jpg",
                "--dest",
                "Keep Title"
              )
              result match
                case Result.Failed(text, value, message) =>
                  message shouldBe Refresh.AlreadyRunning
                  Cli.processCode(result) shouldBe 2
                  text should startWith("already running\n")
                  text should include("phase=items")
                  text should not include ("AF1Qip")
                  value shouldBe Json.Null
                case other =>
                  fail(other.toString)
              JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
            finally Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  "remove-from-album" should "drop the item and adjust an error line" in:
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
          new RenameFake(
            (_, _) => (),
            onTake = (album, item) =>
              album.id shouldBe "albSrc"
              item.key shouldBe "keyPic"
              item.uploader shouldBe Some("Bea")
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session,
          "agent",
          "remove-from-album",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg"
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

  it should "report one-click and click nothing when the dialog is trash" in:
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake((_, _) => (), onTake = (_, _) => throw new Refresh.ActMiss("trash dialog", confirmed = false))
        val (_, result) = exec(
          session,
          "agent",
          "remove-from-album",
          "--title",
          "2010/07 ",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg"
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

  it should "stay refresh-only when the source key is still present" in:
    withArchive: archive =>
      val catalog: String = writePair(archive)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onTake = (_, _) => throw new Refresh.ActMiss("source key is present", confirmed = true)
          )
        val (_, result) = exec(
          session,
          "agent",
          "remove-from-album",
          "--title",
          "2010/07 ",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery refresh-only")
            text should include("catalog unchanged")
            text should include("source key is present")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
          case other =>
            fail(other.toString)
        JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false

  "move-item" should "refuse an owner line before Chrome opens" in:
    refuseMove(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title",
        "owner\talbDst\tAda"
      ),
      "destination has an owner"
    )

  it should "refuse auto-add before Chrome opens" in:
    refuseMove(
      Seq(
        "album\talbSrc\t1\tSource Title",
        "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title",
        "auto-add\talbDst"
      ),
      "destination is auto-add"
    )

  it should "refuse unread names before Chrome opens" in:
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

  it should "refuse an excepted stem before Chrome opens" in:
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
          session,
          "agent",
          "move-item",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Dest"
        ) shouldBe "destination excepts \"pic\""
        called shouldBe false
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "move the item in one session when both halves are clean" in:
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
          new RenameFake(
            (_, _) => (),
            onPut = (item, destination, current) =>
              order = order + "put"
              current shouldBe "pic.jpg"
              item.key shouldBe "keyPic"
              item.uploader shouldBe Some("Bea")
              destination.title shouldBe "Dest Title"
              Catalog.Item("ignored.jpg", Some(LocalDate.of(2011, 1, 2)), "keyNew", None)
            ,
            onTake = (album, item) =>
              order = order + "take"
              album.id shouldBe "albSrc"
              item.key shouldBe "keyPic"
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session,
          "agent",
          "move-item",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Dest Title"
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

  it should "leave the catalog unchanged when remove fails after a clean add" in:
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
          new RenameFake(
            (_, _) => (),
            onPut = (_, _, _) => Catalog.Item("pic.jpg", Some(LocalDate.of(2011, 1, 2)), "keyNew", None),
            onTake = (_, _) => throw new Refresh.ActMiss("trash dialog", confirmed = false)
          )
        val (_, result) = exec(
          session,
          "agent",
          "move-item",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Dest Title"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("\"Source Title\"")
            text should include("\"Dest Title\"")
            text should include("recovery refresh-only")
            text should include("catalog unchanged")
            text should include("trash dialog")
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

  it should "not remove when the add misses before confirm" in:
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
          new RenameFake(
            (_, _) => (),
            onPut = (_, _, _) => throw new Refresh.ActMiss("Add to album is absent", confirmed = false),
            onTake = (_, _) => took = true
          )
        val (_, result) = exec(
          session,
          "agent",
          "move-item",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Dest Title"
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
            text should include("\"Source Title\"")
            text should include("\"Dest Title\"")
            text should not include ("refresh-only")
            field(value, "recovery") shouldBe Json.Str("one-click")
            field(value, "albumId") shouldBe Json.Str("albSrc")
            field(value, "destinationId") shouldBe Json.Str("albDst")
            field(value, "itemKey") shouldBe Json.Str("keyPic")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "not remove when the add confirm is not a clean key" in:
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
          new RenameFake(
            (_, _) => (),
            onPut = (_, _, _) => throw new Refresh.ActMiss("live key differs", confirmed = true),
            onTake = (_, _) => took = true
          )
        val (_, result) = exec(
          session,
          "agent",
          "move-item",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Dest Title"
        )
        took shouldBe false
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("\"Source Title\"")
            text should include("\"Dest Title\"")
            text should include("recovery refresh-only")
            text should include("catalog unchanged")
            text should include("live key differs")
            text should not include ("one-click")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "step") shouldBe Json.Str("More options, then Add to album, then the existing album")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false

  it should "stay refresh-only when the catalog read throws after both halves" in:
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
          new RenameFake(
            (_, _) => (),
            onPut = (_, _, _) => Catalog.Item("pic.jpg", None, "keyNew", None),
            onTake = (_, _) =>
              took = true
              catalogFile.setReadable(false, false)
          )
        try
          val (_, result) = exec(
            session,
            "agent",
            "move-item",
            "--title",
            "Source Title",
            "--item",
            "keyPic",
            "--current",
            "pic.jpg",
            "--dest",
            "Dest Title"
          )
          took shouldBe true
          result match
            case Result.Failed(text, value, message) =>
              message shouldBe Refresh.MutationFailed
              text should include("\"Source Title\"")
              text should include("\"Dest Title\"")
              text should include("recovery refresh-only")
              text should include("catalog unchanged")
              text should not include ("one-click")
              field(value, "recovery") shouldBe Json.Str("refresh-only")
            case other =>
              fail(other.toString)
          catalogFile.setReadable(true, false)
          JFiles.readString(catalogFile.toPath) shouldBe catalog
          File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        finally catalogFile.setReadable(true, false)

  it should "report already running without opening the browser" in:
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
                session,
                "agent",
                "move-item",
                "--title",
                "2010/07 ",
                "--item",
                "keyPic",
                "--current",
                "pic.jpg",
                "--dest",
                "Keep Title"
              )
              result match
                case Result.Failed(text, value, message) =>
                  message shouldBe Refresh.AlreadyRunning
                  message shouldBe "already running"
                  Cli.processCode(result) shouldBe 2
                  text should startWith("already running\n")
                  text should include("phase=items")
                  text should not include ("AF1Qip")
                  value shouldBe Json.Null
                case other =>
                  fail(other.toString)
              JFiles.readString(File(archive, ".gphoto-sync/catalog.txt").toPath) shouldBe catalog
            finally Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  "delete-item" should "refuse no owner line or an uploader before Chrome opens" in:
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
          session,
          "agent",
          "delete-item",
          "--title",
          "Owned Name",
          "--item",
          "keyOwn",
          "--current",
          "pic.jpg"
        )
        owner shouldBe "no owner line"
        owner should not include ("owned")
        val uploader: String = usage(
          session,
          "agent",
          "delete-item",
          "--title",
          "Uploaded Name",
          "--item",
          "keyUp",
          "--current",
          "pic.jpg"
        )
        uploader shouldBe "uploader is not -"
        usage(
          session,
          "agent",
          "delete-item",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "other.jpg"
        ) shouldBe "current does not match \"pic.jpg\" \"other.jpg\""
        called shouldBe false
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/.download").exists shouldBe false
        places.directory.exists shouldBe false

  it should "remove the item line when the key is already gone and not record a trash click" in:
    withArchive: archive =>
      val catalogFile: File = deleteCatalog(archive)
      withAct(archive): (session, places, _) =>
        var clicks: List[String] = Nil
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onPut = (_, _, _) => fail("put"),
            onTake = (_, _) => fail("take"),
            onTrash = (album, item) =>
              album.id shouldBe "albSrc"
              item.key shouldBe "keyPic"
              item.uploader shouldBe None
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(
          session,
          "agent",
          "delete-item",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg"
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

  it should "remove the item line when the confirm leaves the key gone" in:
    withArchive: archive =>
      val catalogFile: File = deleteCatalog(archive)
      withAct(archive): (session, _, _) =>
        var clicks: List[String] = Nil
        Refresh.openAct = _ => new RenameFake((_, _) => (), onTrash = (_, _) => clicks = clicks :+ "trash")
        val (_, result) = exec(
          session,
          "agent",
          "delete-item",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg"
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

  it should "report one-click and publish nothing when the control is Delete forever" in:
    withArchive: archive =>
      val catalogFile: File = deleteCatalog(archive)
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        var clicks: List[String] = Nil
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onTrash = (_, _) => throw new Refresh.ActMiss("Delete forever", confirmed = false)
          )
        val (_, result) = exec(
          session,
          "agent",
          "delete-item",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg"
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
        out should include("Delete forever")
        out should include("recovery one-click")
        out should not include ("albSrc")
        out should not include ("keyPic")
        err should not include ("mutation failed")
        err should not include ("Photo Archive Tool:")
        JFiles.readString(catalogFile.toPath) shouldBe catalog

  it should "stay refresh-only when the key is still present after the confirm" in:
    withArchive: archive =>
      val catalogFile: File = deleteCatalog(archive)
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onTrash = (_, _) => throw new Refresh.ActMiss("source key is present", confirmed = true)
          )
        val (_, result) = exec(
          session,
          "agent",
          "delete-item",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg"
        )
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery refresh-only")
            text should include("catalog unchanged")
            text should include("source key is present")
            text should not include ("one-click")
            field(value, "step") shouldBe Json.Str("Move to trash")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/.download").exists shouldBe false

  it should "stay refresh-only when the catalog read throws after a clean return" in:
    withArchive: archive =>
      val catalogFile: File = deleteCatalog(archive)
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onTrash = (_, _) => catalogFile.setReadable(false, false))
        try
          val (_, result) = exec(
            session,
            "agent",
            "delete-item",
            "--title",
            "Source Title",
            "--item",
            "keyPic",
            "--current",
            "pic.jpg"
          )
          result match
            case Result.Failed(text, value, message) =>
              message shouldBe Refresh.MutationFailed
              text should include("recovery refresh-only")
              text should include("catalog unchanged")
              text should not include ("one-click")
              field(value, "recovery") shouldBe Json.Str("refresh-only")
            case other =>
              fail(other.toString)
          catalogFile.setReadable(true, false)
          JFiles.readString(catalogFile.toPath) shouldBe catalog
          File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        finally catalogFile.setReadable(true, false)

  it should "report already running without opening the browser" in:
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
                session,
                "agent",
                "delete-item",
                "--title",
                "Source Title",
                "--item",
                "keyPic",
                "--current",
                "pic.jpg"
              )
              result match
                case Result.Failed(text, value, message) =>
                  message shouldBe Refresh.AlreadyRunning
                  message shouldBe "already running"
                  Cli.processCode(result) shouldBe 2
                  text should startWith("already running\n")
                  text should include("phase=items")
                  text should not include ("AF1Qip")
                  value shouldBe Json.Null
                case other =>
                  fail(other.toString)
              JFiles.readString(catalogFile.toPath) shouldBe catalog
              File(archive, ".gphoto-sync/.download").exists shouldBe false
            finally Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  "upload-item" should "refuse a path before Chrome opens" in:
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

  it should "refuse a catalog item, a sidecar mark, or a destination rule before Chrome opens" in:
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

  it should "resolve a video beside a still to the v stem" in:
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
          new RenameFake(
            (_, _) => (),
            onUpload = (_, _, current) =>
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

  it should "open for a file the catalog does not already have" in:
    openedUpload(
      Seq(
        "album\talbOther\t1\tOther",
        "item\talbOther\tpic.jpg\t2010-07-02\tkeyPic\t-",
        "album\talbDst\t0\tDest Title"
      )
    )
    openedUpload(
      Seq(
        "album\talbOther\t1\tOther",
        "item\talbOther\tother.jpg\t2010-07-01\tkeyOther\t-",
        "album\talbDst\t0\tDest Title"
      )
    )
    openedUpload(
      Seq(
        "album\talbOther\t1\tOther",
        "item\talbOther\tIMG_IMG_pic.jpg\t2010-07-01\tkeyPic\t-",
        "album\talbDst\t0\tDest Title"
      )
    )
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

  it should "write the confirmed item from the archive name and the read" in:
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
          new RenameFake(
            (_, _) => (),
            onUpload = (destination, file, current) =>
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

  it should "accept an absolute path and write no error line when the read has no date" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest Title")
      val picture: File = File(archive, "2010/07/01/pic.jpg")
      picture.getParentFile.mkdirs()
      JFiles.write(picture.toPath, Array.emptyByteArray)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onUpload = (_, _, current) =>
              current shouldBe "pic.jpg"
              Catalog.Item("ignored.jpg", None, "keyNew", None)
          )
        val (_, result) = exec(
          session,
          "agent",
          "upload-item",
          "--dest",
          "Dest Title",
          picture.getCanonicalPath
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

  it should "fail one-click when the chooser was not reached" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, "2010/07/01"), "pic.jpg")
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onUpload = (_, _, _) => throw new Refresh.ActMiss("Add photos is absent", confirmed = false)
          )
        val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/pic.jpg")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("failed upload-item")
            text should include("filename \"pic.jpg\"")
            text should include("step Add photos or Select photos, then Select from computer")
            text should include("recovery one-click")
            text should include("Add photos is absent")
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

  it should "stay refresh-only when the file was chosen and the read is not clean" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, "2010/07/01"), "pic.jpg")
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onUpload = (_, _, _) => throw new Refresh.ActMiss("live key differs", confirmed = true)
          )
        val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/pic.jpg")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery refresh-only")
            text should include("catalog unchanged")
            text should not include ("one-click")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "stay one-click when the upload throws before the file is chosen" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, "2010/07/01"), "pic.jpg")
      withAct(archive): (session, _, _) =>
        Refresh.openAct =
          _ => new RenameFake((_, _) => (), onUpload = (_, _, _) => throw IllegalStateException("chooser missed"))
        val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/pic.jpg")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery one-click")
            text should include("chooser missed")
            text should not include ("refresh-only")
            field(value, "recovery") shouldBe Json.Str("one-click")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog

  it should "stay refresh-only when the catalog write fails after the file was chosen" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDst\t0\tDest Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      touch(File(archive, "2010/07/01"), "pic.jpg")
      withAct(archive): (session, _, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onUpload = (_, _, _) =>
              catalogFile.setReadable(false, false)
              Catalog.Item("pic.jpg", Some(LocalDate.of(2011, 1, 2)), "keyNew", None)
          )
        try
          val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest Title", "2010/07/01/pic.jpg")
          result match
            case Result.Failed(text, value, message) =>
              message shouldBe Refresh.MutationFailed
              text should include("recovery refresh-only")
              text should include("catalog unchanged")
              field(value, "recovery") shouldBe Json.Str("refresh-only")
            case other =>
              fail(other.toString)
          catalogFile.setReadable(true, false)
          JFiles.readString(catalogFile.toPath) shouldBe catalog
          File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        finally catalogFile.setReadable(true, false)

  it should "report already running without opening the browser" in:
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
            finally Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  "create-album" should "refuse a title before Chrome opens" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbKeep\t0\tKeep Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        var called = false
        Refresh.openAct = _ =>
          called = true
          new RenameFake((_, _) => (), onCreate = _ => fail("opened"))
        usage(session, "agent", "create-album") shouldBe "missing title"
        usage(session, "agent", "create-album", "--title", "") shouldBe "empty title"
        usage(session, "agent", "create-album", "--title", "Pro\tbe") shouldBe "title has a tab or newline"
        usage(session, "agent", "create-album", "--title", "Pro\nbe") shouldBe "title has a tab or newline"
        usage(session, "agent", "create-album", "--title", "Pro\rbe") shouldBe "title has a tab or newline"
        called shouldBe false
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false
        tomls(archive) shouldBe Seq.empty[String]
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbOld\t0\tProbe")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onCreate = _ => fail("opened"))
        usage(session, "agent", "create-album", "--title", "Probe") shouldBe
          "album titled \"Probe\" already exists"
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbOne\t0\tProbe",
        "album\talbTwo\t0\tProbe"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onCreate = _ => fail("opened"))
        usage(session, "agent", "create-album", "--title", "Probe") shouldBe
          "more than one album titled \"Probe\""
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "copy the other block and append the new card" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbOld\t1\tKeep Title",
        "owner\talbOld\tAda",
        "item\talbOld\tkeep.jpg\t2010-08-01\tkeyKeep\t-"
      )
      withAct(archive): (session, places, _) =>
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onCreate = title =>
              title shouldBe "Probe"
              (
                Seq(
                  AlbumCard("albOld", "Renamed", "9", "x", "y"),
                  AlbumCard("albNew", "Probe", "0", "auth", "leaf")
                ),
                Some(blankAlbum("albNew", 0, "Probe"))
              )
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(session, "agent", "create-album", "--title", "Probe")
        result match
          case Result.Ok(text, value) =>
            text shouldBe "create-album \"Probe\"\n"
            text should not include ("albNew")
            text should not include ("albOld")
            field(value, "action") shouldBe Json.Str("create-album")
            field(value, "title") shouldBe Json.Str("Probe")
            field(value, "catalog") shouldBe Json.Str("updated")
            field(value, "albumId") shouldBe Json.Str("albNew")
            field(value, "itemKey") shouldBe Json.Str("")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalogBody(
          "album\talbOld\t1\tKeep Title",
          "owner\talbOld\tAda",
          "item\talbOld\tkeep.jpg\t2010-08-01\tkeyKeep\t-",
          "album\talbNew\t0\tProbe"
        )
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        places.directory.exists shouldBe false
        File(places.directory, "status.txt").exists shouldBe false
        File(places.directory, "albums.tsv").exists shouldBe false
        File(places.directory, "parts").exists shouldBe false
        tomls(archive) shouldBe Seq.empty[String]
        closed shouldBe true
        released(places)

  it should "fail one-click when Create album was not clicked" in:
    createMiss(confirmed = false, detail = "Create album is absent", recovery = "one-click")

  it should "stay refresh-only when the click already happened" in:
    createMiss(confirmed = true, detail = "Done is absent", recovery = "refresh-only")

  it should "stay one-click when create throws before it returns" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbOld\t0\tKeep Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct =
          _ => new RenameFake((_, _) => (), onCreate = _ => throw IllegalStateException("editor missed"))
        val (_, result) = exec(session, "agent", "create-album", "--title", "Probe")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery one-click")
            text should include("editor missed")
            text should not include ("refresh-only")
            text should not include ("albOld")
            field(value, "recovery") shouldBe Json.Str("one-click")
            field(value, "albumId") shouldBe Json.Str("")
            field(value, "itemKey") shouldBe Json.Str("")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false
        tomls(archive) shouldBe Seq.empty[String]

  it should "stay refresh-only when the live card is missing" in:
    createCards(
      Seq(AlbumCard("albOld", "Keep Title", "9", "", "")),
      None,
      "named card is missing"
    )

  it should "stay refresh-only when the live title is ambiguous" in:
    createCards(
      Seq(
        AlbumCard("albNew", "Probe", "0", "", ""),
        AlbumCard("albOther", "Probe", "0", "", "")
      ),
      None,
      "ambiguous title"
    )

  it should "leave the live catalog when the read count disagrees" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbOld\t0\tKeep Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onCreate = _ =>
              (
                Seq(
                  AlbumCard("albOld", "Keep Title", "0", "", ""),
                  AlbumCard("albNew", "Probe", "0", "", "")
                ),
                Some(
                  blankAlbum(
                    "albNew",
                    0,
                    "Probe",
                    Seq(Catalog.Item("pic.jpg", None, "keyPic", None))
                  )
                )
              )
          )
        val (_, result) = exec(session, "agent", "create-album", "--title", "Probe")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery refresh-only")
            text should include("catalog unchanged")
            text should include("item count does not equal count")
            text should not include ("albNew")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "albumId") shouldBe Json.Str("")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").isFile shouldBe true
        places.directory.exists shouldBe false
        released(places)

  it should "report already running without opening the browser" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbOld\t0\tKeep Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        places.directory.mkdirs()
        JFiles.writeString(File(places.directory, "status.txt").toPath, "phase=items\nAF1QipBusy\n")
        Refresh.claim(places.lock) match
          case Refresh.Claim.Owned(channel, lock) =>
            try
              Refresh.openAct = _ => new RenameFake((_, _) => fail("opened while busy"))
              val (_, result) = exec(session, "agent", "create-album", "--title", "Probe")
              result match
                case Result.Failed(text, value, message) =>
                  message shouldBe "already running"
                  Cli.processCode(result) shouldBe 2
                  text should not include ("AF1Qip")
                  value shouldBe Json.Null
                case other =>
                  fail(other.toString)
              JFiles.readString(catalogFile.toPath) shouldBe catalog
            finally Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  "delete-album" should "refuse before Chrome opens" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbOld\t0\tKeep Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        var called = false
        Refresh.openAct = _ =>
          called = true
          new RenameFake((_, _) => (), onDelete = _ => fail("opened"))
        usage(session, "agent", "delete-album") shouldBe "missing title"
        usage(session, "agent", "delete-album", "--title", "Missing") shouldBe
          "no album titled \"Missing\""
        called shouldBe false
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbOld\t0\tKeep Title",
        "owner\talbOld\tAda"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onDelete = _ => fail("opened"))
        usage(session, "agent", "delete-album", "--title", "Keep Title") shouldBe "no owner line"
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbOld\t0\tKeep Title",
        "auto-add\talbOld"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onDelete = _ => fail("opened"))
        usage(session, "agent", "delete-album", "--title", "Keep Title") shouldBe "album is auto-add"
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false
    withArchive: archive =>
      val catalogFile: File = writeCatalog(
        archive,
        "album\talbOne\t0\tKeep Title",
        "album\talbTwo\t0\tKeep Title"
      )
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onDelete = _ => fail("opened"))
        usage(session, "agent", "delete-album", "--title", "Keep Title") shouldBe
          "more than one album titled \"Keep Title\""
        usage(session, "agent", "delete-album", "--title", "Keep Title", "--id", "albMissing") shouldBe
          "catalog title does not match"
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "copy the other blocks when the sweep is the stored ids minus this one" in:
    withArchive: archive =>
      val lines: Seq[String] = survivorLines ++ deletedLines
      val catalogFile: File = writeCatalog(archive, lines*)
      withAct(archive): (session, places, _) =>
        var closed = false
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onDelete = album =>
              album.id shouldBe "albDel"
              album.title shouldBe "Gone Title"
              mutatedCards(survivorIds)
          ):
            override def close(): Unit = closed = true
        val (_, result) = exec(session, "agent", "delete-album", "--title", "Gone Title")
        result match
          case Result.Ok(text, value) =>
            text shouldBe "delete-album \"Gone Title\"\n"
            text should not include ("albDel")
            text should not include ("alb001")
            field(value, "action") shouldBe Json.Str("delete-album")
            field(value, "title") shouldBe Json.Str("Gone Title")
            field(value, "catalog") shouldBe Json.Str("updated")
            field(value, "albumId") shouldBe Json.Str("albDel")
            field(value, "itemKey") shouldBe Json.Str("")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalogBody(survivorLines*)
        JFiles.readString(catalogFile.toPath) should not include ("albDel")
        JFiles.readString(catalogFile.toPath) should include("owner\talb001\tAda")
        JFiles.readString(catalogFile.toPath) should include("Title alb001")
        JFiles.readString(catalogFile.toPath) should not include ("Nope")
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/catalog.txt.previous").exists shouldBe false
        places.directory.exists shouldBe false
        File(places.directory, "status.txt").exists shouldBe false
        File(places.directory, "albums.tsv").exists shouldBe false
        tomls(archive) shouldBe Seq.empty[String]
        closed shouldBe true
        released(places)

  it should "publish nothing when a second stored id is missing" in:
    deleteSweep(
      survivorLines ++ deletedLines ++ Seq("album\talbMiss\t0\tMissed Title"),
      mutatedCards(survivorIds),
      "album set"
    )

  it should "publish nothing when the sweep is under 250" in:
    deleteSweep(
      Seq(
        "album\talbKeep\t1\tKeep Title",
        "owner\talbKeep\tAda",
        "item\talbKeep\tkeep.jpg\t2010-08-01\tkeyKeep\t-"
      ) ++ deletedLines,
      Seq(AlbumCard("albKeep", "Nope", "9", "", "")),
      "rows < 250"
    )

  it should "publish nothing when the sweep has an extra card" in:
    deleteSweep(
      survivorLines ++ deletedLines,
      mutatedCards(survivorIds) :+ AlbumCard("albExtra", "Extra", "1", "", ""),
      "album set"
    )

  it should "publish nothing when the deleted id is still listed" in:
    deleteSweep(
      survivorLines ++ deletedLines,
      mutatedCards(survivorIds) :+ AlbumCard("albDel", "Gone Title", "2", "", ""),
      "album set"
    )

  it should "fail one-click when Delete was not clicked" in:
    deleteMiss(confirmed = false, detail = "Delete album is absent", recovery = "one-click")

  it should "stay refresh-only when the delete click already happened" in:
    deleteMiss(
      confirmed = true,
      detail = "rows < 250",
      recovery = "refresh-only",
      cards = Some(
        Seq(
          AlbumCard("albKeep", "Keep Title", "0", "", "")
        )
      )
    )

  it should "stay one-click when delete throws before it returns" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDel\t0\tGone Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onDelete = _ => throw IllegalStateException("menu missed"))
        val (_, result) = exec(session, "agent", "delete-album", "--title", "Gone Title")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery one-click")
            text should include("menu missed")
            text should not include ("refresh-only")
            text should not include ("albDel")
            field(value, "recovery") shouldBe Json.Str("one-click")
            field(value, "albumId") shouldBe Json.Str("")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false

  it should "report already running without opening the browser" in:
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDel\t0\tGone Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        places.directory.mkdirs()
        JFiles.writeString(File(places.directory, "status.txt").toPath, "phase=items\nAF1QipBusy\n")
        Refresh.claim(places.lock) match
          case Refresh.Claim.Owned(channel, lock) =>
            try
              Refresh.openAct = _ => new RenameFake((_, _) => fail("opened while busy"))
              val (_, result) = exec(session, "agent", "delete-album", "--title", "Gone Title")
              result match
                case Result.Failed(text, value, message) =>
                  message shouldBe "already running"
                  Cli.processCode(result) shouldBe 2
                  text should not include ("AF1Qip")
                  value shouldBe Json.Null
                case other =>
                  fail(other.toString)
              JFiles.readString(catalogFile.toPath) shouldBe catalog
            finally Refresh.release(channel, lock)
          case Refresh.Claim.Busy =>
            fail("lock was already held")

  private def deleteMiss(
    confirmed: Boolean,
    detail: String,
    recovery: String,
    cards: Option[Seq[AlbumCard]] = None
  ): Unit =
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbDel\t0\tGone Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ =>
          new RenameFake(
            (_, _) => (),
            onDelete = _ =>
              cards match
                case Some(found) => found
                case None        => throw new Refresh.ActMiss(detail, confirmed)
          )
        val (_, result) = exec(session, "agent", "delete-album", "--title", "Gone Title")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("failed delete-album")
            text should include("album \"Gone Title\"")
            text should include("step More options, then Delete album, then Delete")
            text should include(s"recovery $recovery")
            text should include("catalog unchanged")
            text should include(detail)
            text should not include ("albDel")
            field(value, "action") shouldBe Json.Str("delete-album")
            field(value, "recovery") shouldBe Json.Str(recovery)
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "albumId") shouldBe Json.Str("")
            field(value, "itemKey") shouldBe Json.Str("")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        places.directory.exists shouldBe false
        released(places)

  private def deleteSweep(lines: Seq[String], cards: Seq[AlbumCard], detail: String): Unit =
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, lines*)
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onDelete = _ => cards)
        val (_, result) = exec(session, "agent", "delete-album", "--title", "Gone Title")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery refresh-only")
            text should include("catalog unchanged")
            text should include(detail)
            text should not include ("albDel")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "albumId") shouldBe Json.Str("")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        places.directory.exists shouldBe false
        released(places)

  private val survivorIds: Seq[String] = (1 to 250).map(i => f"alb$i%03d")

  private def survivorLines: Seq[String] =
    val head: Seq[String] = Seq(
      "album\talb001\t1\tTitle alb001",
      "owner\talb001\tAda",
      "item\talb001\tkeep.jpg\t2010-08-01\tkeyKeep\t-"
    )
    val rest: Seq[String] = survivorIds.drop(1).map(id => s"album\t$id\t0\tTitle $id")
    head ++ rest

  private def deletedLines: Seq[String] = Seq(
    "album\talbDel\t2\tGone Title",
    "item\talbDel\tpic.jpg\t2010-07-01\tkeyPic\t-",
    "item\talbDel\tother.jpg\t2010-07-02\tkeyOther\t-"
  )

  private def mutatedCards(ids: Seq[String]): Seq[AlbumCard] =
    ids.map(id => AlbumCard(id, "Nope", "9", "auth", "leaf"))

  private def createMiss(confirmed: Boolean, detail: String, recovery: String): Unit =
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbOld\t0\tKeep Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct =
          _ => new RenameFake((_, _) => (), onCreate = _ => throw new Refresh.ActMiss(detail, confirmed))
        val (_, result) = exec(session, "agent", "create-album", "--title", "Probe")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("failed create-album")
            text should include("album \"Probe\"")
            text should include("step Create album, then Edit album name, Done")
            text should include(s"recovery $recovery")
            text should include("catalog unchanged")
            text should include(detail)
            text should not include ("albOld")
            field(value, "action") shouldBe Json.Str("create-album")
            field(value, "title") shouldBe Json.Str("Probe")
            field(value, "recovery") shouldBe Json.Str(recovery)
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "albumId") shouldBe Json.Str("")
            field(value, "itemKey") shouldBe Json.Str("")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        places.directory.exists shouldBe false
        tomls(archive) shouldBe Seq.empty[String]
        released(places)

  private def createCards(
    cards: Seq[AlbumCard],
    read: Option[Catalog.Album],
    detail: String
  ): Unit =
    withArchive: archive =>
      val catalogFile: File = writeCatalog(archive, "album\talbOld\t0\tKeep Title")
      val catalog: String = JFiles.readString(catalogFile.toPath)
      withAct(archive): (session, places, _) =>
        Refresh.openAct = _ => new RenameFake((_, _) => (), onCreate = _ => (cards, read))
        val (_, result) = exec(session, "agent", "create-album", "--title", "Probe")
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery refresh-only")
            text should include("catalog unchanged")
            text should include(detail)
            text should not include ("one-click")
            text should not include ("albOld")
            field(value, "recovery") shouldBe Json.Str("refresh-only")
            field(value, "catalog") shouldBe Json.Str("unchanged")
            field(value, "albumId") shouldBe Json.Str("")
          case other =>
            fail(other.toString)
        JFiles.readString(catalogFile.toPath) shouldBe catalog
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        places.directory.exists shouldBe false
        released(places)

  private def blankAlbum(
    id: String,
    count: Int,
    title: String,
    items: Seq[Catalog.Item] = Seq.empty
  ): Catalog.Album =
    Catalog.Album(id, count, title, None, false, None, items, Seq.empty)

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
          new RenameFake(
            (_, _) => (),
            onUpload = (_, _, _) =>
              called = true
              throw new Refresh.ActMiss("Add photos is absent", confirmed = false)
          )
        val (_, result) = exec(session, "agent", "upload-item", "--dest", "Dest Title", relative)
        called shouldBe true
        result match
          case Result.Failed(text, value, message) =>
            message shouldBe Refresh.MutationFailed
            text should include("recovery one-click")
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
          session,
          "agent",
          "move-item",
          "--title",
          "Source Title",
          "--item",
          "keyPic",
          "--current",
          "pic.jpg",
          "--dest",
          "Dest Title"
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
    onPut: (Catalog.Item, Catalog.Album, String) => Catalog.Item = (_, _, _) => throw IllegalStateException("put"),
    onTake: (Catalog.Album, Catalog.Item) => Unit = (_, _) => (),
    onTrash: (Catalog.Album, Catalog.Item) => Unit = (_, _) => (),
    onUpload: (Catalog.Album, File, String) => Catalog.Item = (_, _, _) => throw IllegalStateException("upload"),
    onCreate: String => (Seq[AlbumCard], Option[Catalog.Album]) = _ => throw IllegalStateException("create"),
    onDelete: Catalog.Album => Seq[AlbumCard] = _ => throw IllegalStateException("delete-album")
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
    def createAlbum(title: String): (Seq[AlbumCard], Option[Catalog.Album]) = onCreate(title)
    def deleteAlbum(album: Catalog.Album): Seq[AlbumCard] = onDelete(album)

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
