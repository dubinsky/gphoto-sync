package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import scala.collection.mutable.ListBuffer
import java.io.{File, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles}
import java.time.{LocalDate, ZoneId}

class PushTest extends AnyFlatSpec, Matchers:
  @targetName("outcomeCanEqual")
  private given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

  @targetName("operationCanEqual")
  private given [A <: Operation, B <: Operation] => CanEqual[A, B] = CanEqual.derived

  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")

  "push" should "parse dry-run, stay out of MCP, and run in the caller" in :
    Cli.parse(List("push")) shouldBe Cli.Run(Cli.PushArgs(dryRun = false))
    Cli.parse(List("push", "--dry-run")) shouldBe Cli.Run(Cli.PushArgs(dryRun = true))
    Cli.parse(List("push", "--dry-run=false")) shouldBe Cli.Run(Cli.PushArgs(dryRun = false))
    Cli.parse(List("push", "--dry-run", "--dry-run")) shouldBe Cli.Usage("repeated option '--dry-run'")
    Cli.parse(List("agent", "push")) shouldBe Cli.Usage("unknown command 'push'")
    Cli.toOperation(Cli.PushArgs(dryRun = true)) shouldBe Push(dryRun = true)
    Cli.mcpTools.exists(_.name == "push") shouldBe false
    withArchive: archive =>
      val (_, served) = Cli.serveResponse(
        sessionFor(archive),
        """{"argv":["push","--dry-run"]}""",
        (_, _) => fail("push was forwarded")
      )
      served should include ("push runs in the calling process")

  it should "print remove, then add, then upload, and skip albums it must not edit" in :
    withArchive: archive =>
      writeLibrary(archive)
      val before: String = read(archive, ".gphoto-sync/catalog.txt")
      val toml: String = read(archive, "2010/07/album.toml")
      withPlaces(archive): (session, places, opens) =>
        Refresh.claim(places.lock) match
          case Refresh.Claim.Owned(channel, lock) =>
            val text: String =
              try ok(run(session, "push", "--dry-run"))
              finally Refresh.release(channel, lock)
            text shouldBe
              """remove "July Trip" "9999.jpg"
                |add "July Trip" "IMG_2412.jpg" from "Source"
                |upload "Best" "2010/07/02/1000.jpg"
                |""".stripMargin
            text should not include ("alb")
            text should not include ("key")
            opens() shouldBe 0
          case Refresh.Claim.Busy =>
            fail("lock busy")
        read(archive, ".gphoto-sync/catalog.txt") shouldBe before
        read(archive, "2010/07/album.toml") shouldBe toml
        File(archive, ".gphoto-sync/status.txt").exists shouldBe false
        File(archive, ".gphoto-sync/albums.tsv").exists shouldBe false

  it should "add one shared key from the first album and upload a prefixed file" in :
    withArchive: archive =>
      write(archive, "2010/07/album.toml", external("2010/07/01 2412"))
      write(archive, "2010/07/01/2412.jpg", "pic")
      write(archive, "Best.toml", external("2010/07/02 1000"))
      write(archive, "2010/07/02/IMG_1000.jpg", "pic")
      writeCatalog(
        archive,
        "album\talbA\t1\tSource A",
        "item\talbA\t2412.jpg\t2010-07-01\tkeySame\t-",
        "album\talbB\t1\tSource B",
        "item\talbB\t2412.jpg\t2010-07-01\tkeySame\t-",
        "album\talbJuly\t0\tJuly Trip",
        "album\talbBest\t0\tBest"
      )
      writeLinks(
        archive,
        "albJuly\t2010/07/album.toml",
        "albBest\tBest.toml"
      )
      withPlaces(archive): (session, _, opens) =>
        ok(run(session, "push", "--dry-run")) shouldBe
          """add "July Trip" "2412.jpg" from "Source A"
            |upload "Best" "2010/07/02/IMG_1000.jpg"
            |""".stripMargin
        opens() shouldBe 0

  it should "leave a desired pair that is already in the album" in :
    withArchive: archive =>
      write(archive, "2010/07/album.toml", external("2010/07/01 2412"))
      write(archive, "2010/07/01/2412.jpg", "pic")
      writeCatalog(
        archive,
        "album\talbSrc\t1\tSource",
        "item\talbSrc\t2412.jpg\t2010-07-01\tkey2412\t-",
        "album\talbJuly\t1\tJuly Trip",
        "item\talbJuly\t2412.jpg\t2010-07-01\tkey2412\t-"
      )
      writeLinks(archive, "albJuly\t2010/07/album.toml")
      withPlaces(archive): (session, _, opens) =>
        ok(run(session, "push", "--dry-run")) shouldBe ""
        opens() shouldBe 0

  it should "publish remove, then add, then upload through one browser" in :
    withArchive: archive =>
      writeRun(archive)
      val toml: String = read(archive, "2010/07/album.toml")
      val calls: ListBuffer[String] = ListBuffer.empty
      withPlaces(archive, act(calls)): (session, _, opens) =>
        val text: String = ok(run(session, "push"))
        text shouldBe
          """remove "July Trip" "9999.jpg"
            |add "July Trip" "IMG_2412.jpg" from "Source"
            |upload "Best" "2010/07/02/1000.jpg"
            |""".stripMargin
        opens() shouldBe 1
        calls.toSeq shouldBe Seq("take 9999.jpg", "add IMG_2412.jpg", "upload 1000.jpg")
        val albums: Seq[Catalog.Album] = Catalog.read(File(archive, ".gphoto-sync/catalog.txt"))
        albums.find(_.id == "albJuly").get.items.map(_.filename) shouldBe Seq("extra.jpg", "IMG_2412.jpg")
        albums.find(_.id == "albJuly").get.count shouldBe 2
        albums.find(_.id == "albSrc").get.items.map(_.filename) shouldBe Seq("IMG_2412.jpg")
        albums.find(_.id == "albBest").get.items.map(_.filename) shouldBe Seq("1000.jpg")
        albums.find(_.id == "albBest").get.count shouldBe 1
        read(archive, "2010/07/album.toml") shouldBe toml
        read(archive, "2010/07/album.toml") should not include ("google-album-id")
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false
        File(archive, ".gphoto-sync/status.txt").exists shouldBe false
        File(archive, ".gphoto-sync/albums.tsv").exists shouldBe false
        File(archive, ".gphoto-sync/parts").exists shouldBe false

  it should "stop after a one-click miss and leave that action unpublished" in :
    withArchive: archive =>
      writeRun(archive)
      val calls: ListBuffer[String] = ListBuffer.empty
      val failing = act(calls, onPut = (_, _, _) => throw Refresh.ActMiss("destination album is absent", false))
      withPlaces(archive, failing): (session, _, opens) =>
        val failed: Result = run(session, "push")
        val text: String = failure(failed)
        text shouldBe
          """remove "July Trip" "9999.jpg"
            |failed push
            |add "July Trip" "IMG_2412.jpg" from "Source"
            |step More options, then Add to album, then the existing album
            |recovery one-click
            |catalog updated
            |destination album is absent
            |""".stripMargin
        text should not include ("albJuly")
        text should not include ("key2412")
        opens() shouldBe 1
        calls.toSeq shouldBe Seq("take 9999.jpg", "add IMG_2412.jpg")
        val albums: Seq[Catalog.Album] = Catalog.read(File(archive, ".gphoto-sync/catalog.txt"))
        albums.find(_.id == "albJuly").get.items.map(_.filename) shouldBe Seq("extra.jpg")
        albums.find(_.id == "albJuly").get.count shouldBe 1
        albums.find(_.id == "albBest").get.count shouldBe 0
        File(archive, ".gphoto-sync/catalog.txt.partial").exists shouldBe false

  it should "leave the catalog byte-identical when the first confirm already ran" in :
    withArchive: archive =>
      writeRun(archive)
      val before: String = read(archive, ".gphoto-sync/catalog.txt")
      val calls: ListBuffer[String] = ListBuffer.empty
      val failing = act(calls, onTake = (_, _) => throw Refresh.ActMiss("source key is present", true))
      withPlaces(archive, failing): (session, _, _) =>
        failure(run(session, "push")) shouldBe
          """failed push
            |remove "July Trip" "9999.jpg"
            |step More options, then Remove from album, then Remove
            |recovery refresh-only
            |catalog unchanged
            |source key is present
            |""".stripMargin
        calls.toSeq shouldBe Seq("take 9999.jpg")
        read(archive, ".gphoto-sync/catalog.txt") shouldBe before

  it should "not open a second upload after the file is already a catalog item" in :
    withArchive: archive =>
      write(archive, "2010/07/album.toml", external("2010/07/03 3000"))
      write(archive, "Best.toml", external("2010/07/03 3000"))
      write(archive, "2010/07/03/3000.jpg", "pic")
      writeCatalog(
        archive,
        "album\talbJuly\t0\tJuly Trip",
        "album\talbBest\t0\tBest"
      )
      writeLinks(archive, "albJuly\t2010/07/album.toml", "albBest\tBest.toml")
      val calls: ListBuffer[String] = ListBuffer.empty
      withPlaces(archive, act(calls, uploadDate = "2010-07-03")): (session, _, opens) =>
        val text: String = failure(run(session, "push"))
        text should include ("""upload "July Trip" "2010/07/03/3000.jpg"""")
        text should include ("failed push")
        text should include ("""upload "Best" "2010/07/03/3000.jpg"""")
        text should include ("recovery one-click")
        text should include ("catalog updated")
        text should include ("catalog item exists")
        opens() shouldBe 1
        calls.toSeq shouldBe Seq("upload 3000.jpg")
        val albums: Seq[Catalog.Album] = Catalog.read(File(archive, ".gphoto-sync/catalog.txt"))
        albums.find(_.id == "albJuly").get.items.map(_.filename) shouldBe Seq("3000.jpg")
        albums.find(_.id == "albBest").get.items shouldBe Seq.empty[Catalog.Item]

  it should "report already running and not open Chrome" in :
    withArchive: archive =>
      writeRun(archive)
      val before: String = read(archive, ".gphoto-sync/catalog.txt")
      withPlaces(archive): (session, places, opens) =>
        Refresh.claim(places.lock) match
          case Refresh.Claim.Owned(channel, lock) =>
            try
              run(session, "push") match
                case Result.Failed(text, _, message) =>
                  message shouldBe Refresh.AlreadyRunning
                  text should include ("already running")
                case other =>
                  fail(other.toString)
            finally Refresh.release(channel, lock)
            opens() shouldBe 0
            read(archive, ".gphoto-sync/catalog.txt") shouldBe before
          case Refresh.Claim.Busy =>
            fail("lock busy")

  it should "refuse a missing catalog before Chrome opens" in :
    withArchive: archive =>
      withPlaces(archive): (session, _, opens) =>
        run(session, "push") shouldBe Result.Usage("catalog is absent")
        opens() shouldBe 0

  it should "upload a video beside its still without using the still's catalog row" in :
    withArchive: archive =>
      write(archive, "Best.toml", external("2010/07/02 1000v"))
      write(archive, "2010/07/02/1000.jpg", "pic")
      write(archive, "2010/07/02/1000.toml", "google-media-item-id = \"library-id\"\n")
      write(archive, "2010/07/02/VID_1000.mp4", "vid")
      writeCatalog(
        archive,
        "album\talbSrc\t1\tSource",
        "item\talbSrc\t1000.jpg\t2010-07-02\tkeyStill\t-",
        "album\talbBest\t0\tBest"
      )
      writeLinks(archive, "albBest\tBest.toml")
      val calls: ListBuffer[String] = ListBuffer.empty
      withPlaces(archive, act(calls)): (session, _, opens) =>
        ok(run(session, "push")) shouldBe
          """upload "Best" "2010/07/02/VID_1000.mp4"
            |""".stripMargin
        opens() shouldBe 1
        calls.toSeq shouldBe Seq("upload VID_1000.mp4")
        val albums: Seq[Catalog.Album] = Catalog.read(File(archive, ".gphoto-sync/catalog.txt"))
        albums.find(_.id == "albBest").get.items.map(_.filename) shouldBe Seq("VID_1000.mp4")
        albums.find(_.id == "albSrc").get.items.map(_.filename) shouldBe Seq("1000.jpg")

  private def external(include: String): String =
    s"external = true\ninclude = ['$include']\n"

  private def writeLibrary(archive: File): Unit =
    write(archive, "2010/07/album.toml", external("2010/07/01 except 9999"))
    write(archive, "2010/07/01/2412.jpg", "pic")
    write(archive, "2010/07/01/5555.jpg", "pic")
    write(archive, "2010/07/01/8888.jpg", "pic")
    write(archive, "2010/07/01/8888.toml", "already-in-google = true\n")
    write(archive, "2010/07/01/7777.cr2", "raw")
    write(archive, "2010/07/01/6666.jpg", "pic")
    write(archive, "2010/07/01/6666.toml", "google-media-item-id = \"library-id\"\n")
    write(archive, "2010/07/02/1000.jpg", "pic")
    write(archive, "Best.toml", external("2010/07/02 1000"))
    write(archive, "2010/06/album.toml", external("2010/06/01 4141"))
    write(archive, "2010/06/01/4141.jpg", "pic")
    write(archive, "2010/08/album.toml", "external = true\ngoogle-title = \"Owned\"\nowner = \"Ada\"\ninclude = ['2010/08/01 4242']\n")
    write(archive, "2010/08/01/4242.jpg", "pic")
    write(archive, "2010/09/album.toml", "include = ['2010/09/01 4343']\n")
    write(archive, "2010/09/01/4343.jpg", "pic")
    write(archive, "2010/10/album.toml", external("2010/10/01 4444"))
    write(archive, "2010/10/01/4444.jpg", "pic")
    write(archive, "2010/11/album.toml", external("2010/11/01 4545"))
    write(archive, "2010/11/01/4545.jpg", "pic")
    write(archive, "2010/12/album.toml", external("2010/12/01 4646"))
    write(archive, "2010/12/01/4646.jpg", "pic")
    writeCatalog(
      archive,
      "album\talbJuly\t2\tJuly Trip",
      "item\talbJuly\t9999.jpg\t2010-07-01\tkey9999\t-",
      "item\talbJuly\t5555.jpg\t2010-07-01\tkey5555\t-",
      "album\talbSrc\t1\tSource",
      "item\talbSrc\tIMG_2412.jpg\t2010-07-01\tkey2412\t-",
      "album\talbBest\t0\tBest",
      "album\talbOwn\t0\tOwned",
      "album\talbApp\t0\tApp",
      "album\talbAuto\t0\tAuto",
      "auto-add\talbAuto",
      "album\talbUnread\t1\tUnread",
      "error\talbUnread\t1\t1\t1",
      "unset\talbUnread\t2010-12-01\tkeyUnset\t-"
    )
    writeLinks(
      archive,
      "missing\t2010/06/album.toml",
      "albJuly\t2010/07/album.toml",
      "albOwn\t2010/08/album.toml",
      "albApp\t2010/09/album.toml",
      "albAuto\t2010/11/album.toml",
      "albUnread\t2010/12/album.toml",
      "albBest\tBest.toml"
    )

  private def writeRun(archive: File): Unit =
    write(archive, "2010/07/album.toml", external("2010/07/01 except 9999"))
    write(archive, "2010/07/01/2412.jpg", "pic")
    write(archive, "2010/07/02/1000.jpg", "pic")
    write(archive, "Best.toml", external("2010/07/02 1000"))
    writeCatalog(
      archive,
      "album\talbJuly\t2\tJuly Trip",
      "item\talbJuly\t9999.jpg\t2010-07-01\tkey9999\t-",
      "item\talbJuly\textra.jpg\t2010-08-01\tkeyExtra\t-",
      "album\talbSrc\t1\tSource",
      "item\talbSrc\tIMG_2412.jpg\t2010-07-01\tkey2412\t-",
      "album\talbBest\t0\tBest"
    )
    writeLinks(
      archive,
      "albJuly\t2010/07/album.toml",
      "albBest\tBest.toml"
    )

  private def act(
    calls: ListBuffer[String],
    onTake: (Catalog.Album, Catalog.Item) => Unit = (_, _) => (),
    onPut: (Catalog.Item, Catalog.Album, String) => Catalog.Item = (item, _, _) => item,
    uploadDate: String = "2010-07-02"
  ): () => PhotosAct = () => new PhotosAct:
    def albumCards(): Seq[AlbumCard] = Seq.empty
    def readOwner(card: AlbumCard): Option[String] = None
    def readAlbum(card: AlbumCard): Catalog.Album = fail("read")
    def close(): Unit = ()
    def renameAlbum(album: Catalog.Album, updated: String): Unit = fail("rename")
    def setDescription(item: Catalog.Item, text: String): Unit = fail("description")
    def setDate(item: Catalog.Item, local: String): Unit = fail("date")
    def downloadItem(item: Catalog.Item, destination: File): Unit = fail("download")
    def putInAlbum(item: Catalog.Item, destination: Catalog.Album, current: String): Catalog.Item =
      calls += s"add $current"
      onPut(item, destination, current)
    def takeOut(album: Catalog.Album, item: Catalog.Item): Unit =
      calls += s"take ${item.filename}"
      onTake(album, item)
    def trash(album: Catalog.Album, item: Catalog.Item): Unit = fail("trash")
    def uploadItem(destination: Catalog.Album, file: File, current: String): Catalog.Item =
      calls += s"upload $current"
      Catalog.Item(current, Some(LocalDate.parse(uploadDate)), "keyUp", None)
    def createAlbum(title: String): (Seq[AlbumCard], Option[Catalog.Album]) = fail("create")
    def deleteAlbum(album: Catalog.Album): Seq[AlbumCard] = fail("delete-album")

  private def withPlaces(
    archive: File,
    act: () => PhotosAct = () => fail("opened")
  )(body: (Session, Refresh.Places, () => Int) => Unit): Unit =
    val parent: File = JFiles.createTempDirectory("push-lock").toFile
    val places: Refresh.Places = Refresh.Places(File(parent, "work"), File(parent, "lock"))
    val previousPlaces: () => Refresh.Places = Refresh.placesOf
    val previousAct: (Refresh.Notice => Unit) => PhotosAct = Refresh.openAct
    var opens: Int = 0
    Refresh.placesOf = () => places
    Refresh.openAct = _ =>
      opens += 1
      act()
    try body(sessionFor(archive), places, () => opens)
    finally
      Refresh.placesOf = previousPlaces
      Refresh.openAct = previousAct
      deleteTree(parent)

  private def ok(result: Result): String = result match
    case Result.Ok(text, value) =>
      value shouldBe Json.Null
      text
    case other =>
      fail(other.toString)

  private def failure(result: Result): String = result match
    case Result.Failed(text, value, message) =>
      value shouldBe Json.Null
      message shouldBe Refresh.MutationFailed
      text
    case other =>
      fail(other.toString)

  private def run(session: Session, args: String*): Result =
    Cli.parse(args.toList) match
      case Cli.Run(parsed) => Cli.toOperation(parsed).execute(session)._2
      case other => fail(other.toString)

  private def sessionFor(archive: File): Session = Session(rootFor(archive))

  private def rootFor(archive: File): Root =
    val home: File = File(System.getProperty("user.home"))
    Root(Configuration(home.toPath.relativize(archive.toPath).toString, zone))

  private def writeCatalog(archive: File, lines: String*): Unit =
    write(archive, ".gphoto-sync/catalog.txt", (Seq("# gphoto-catalog 2") ++ lines).mkString("", "\n", "\n"))

  private def writeLinks(archive: File, lines: String*): Unit =
    write(archive, ".gphoto-sync/album-links.tsv", (Seq("# gphoto-album-links 1") ++ lines).mkString("", "\n", "\n"))

  private def write(root: File, relative: String, text: String): Unit =
    val file: File = File(root, relative)
    file.getParentFile.mkdirs()
    JFiles.writeString(file.toPath, text)

  private def read(root: File, relative: String): String =
    JFiles.readString(File(root, relative).toPath)

  private def withArchive(body: File => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("push").toFile
    try body(archive)
    finally deleteTree(archive)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
