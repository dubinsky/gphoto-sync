package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{FileAlreadyExistsException, Files as JFiles, StandardOpenOption}
import java.time.ZoneId
import javax.imageio.ImageIO

class FileDownloadsTest extends AnyFlatSpec, Matchers:
  @targetName("outcomeCanEqual")
  private given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

  @targetName("operationCanEqual")
  private given [A <: Operation, B <: Operation] => CanEqual[A, B] = CanEqual.derived

  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")

  "file" should "parse --dry-run once" in:
    Cli.parse(List("file")) shouldBe Cli.Run(Cli.FileArgs(dryRun = false))
    Cli.parse(List("file", "--dry-run")) shouldBe Cli.Run(Cli.FileArgs(dryRun = true))
    Cli.parse(List("file", "--dry-run=false")) shouldBe Cli.Run(Cli.FileArgs(dryRun = false))
    Cli.parse(List("file", "--dry-run", "--dry-run")) shouldBe Cli.Usage("repeated option '--dry-run'")
    Cli.parse(List("file", "2010")) shouldBe Cli.Usage("extra argument '2010'")
    Cli.toOperation(Cli.FileArgs(dryRun = true)) shouldBe FileDownloads(dryRun = true)
    Cli.parse(List("file", "--help")) match
      case Cli.Help(text) =>
        text shouldBe
          """file  file staged downloads into civil day folders
            |
            |Positionals:
            |none
            |
            |Flags:
            |--dry-run  boolean  print the actions and write nothing""".stripMargin
      case other =>
        fail(other.toString)

  it should "file a dated download, suffix a video, and leave undated, collisions, and a non-digit stem" in:
    withArchive: archive =>
      write(archive, "2010/07/19/123.jpg", "original")
      write(archive, "2010/08/01/20191220_141955.jpg", "still")
      write(archive, "2010/08/02/50.jpg", "keep-me")
      val inbox: File = File(File(archive, ".gphoto-sync"), ".download")
      write(inbox, "2010/07/19/IMG_2412.JPG", "dated")
      write(inbox, "2010/07/19/IMG_123.jpg", "staged-collision")
      write(inbox, "2010/07/19/PHOTO.JPG", "photo")
      write(inbox, "2010/07/20/100.jpg", "plain")
      write(inbox, "2010/07/20/IMG_100.jpg", "prefixed")
      write(inbox, "2010/08/01/VID_20191220_141955.mp4", "video")
      write(inbox, "2010/08/02/50.jpg", "do-not-copy")
      write(inbox, "undated/IMG_9.jpg", "undated")

      val text: String = okText(FileDownloads(dryRun = false).execute(sessionFor(archive))._2)
      text shouldBe
        """keep 2010/07/19/IMG_123.jpg 123.jpg already exists
          |file 2010/07/19 IMG_2412.JPG 2412.jpg
          |wrote 2010/07/19/2412.jpg
          |keep 2010/07/19/PHOTO.JPG PHOTO.jpg does not start with a digit
          |file 2010/07/20 100.jpg 100.jpg
          |wrote 2010/07/20/100.jpg
          |keep 2010/07/20/IMG_100.jpg 100.jpg already exists
          |file 2010/08/01 VID_20191220_141955.mp4 20191220_141955v.mp4
          |wrote 2010/08/01/20191220_141955v.mp4
          |keep 2010/08/02/50.jpg 50.jpg already exists
          |keep undated/IMG_9.jpg undated
          |""".stripMargin

      read(archive, "2010/07/19/123.jpg") shouldBe "original"
      read(archive, "2010/07/19/2412.jpg") shouldBe "dated"
      read(archive, "2010/07/20/100.jpg") shouldBe "plain"
      read(archive, "2010/08/01/20191220_141955.jpg") shouldBe "still"
      read(archive, "2010/08/01/20191220_141955v.mp4") shouldBe "video"
      read(archive, "2010/08/02/50.jpg") shouldBe "keep-me"
      File(archive, "2010/07/19/IMG_2412.JPG").exists shouldBe false
      File(archive, "2010/08/01/20191220_141955.mp4").exists shouldBe false
      namesUnder(archive).filter(name => name.startsWith(".") || name.contains("partial")) shouldBe Set.empty[String]

      read(inbox, "2010/07/19/IMG_123.jpg") shouldBe "staged-collision"
      read(inbox, "2010/07/19/PHOTO.JPG") shouldBe "photo"
      read(inbox, "2010/07/20/IMG_100.jpg") shouldBe "prefixed"
      read(inbox, "2010/08/02/50.jpg") shouldBe "do-not-copy"
      read(inbox, "undated/IMG_9.jpg") shouldBe "undated"
      File(inbox, "2010/07/19/IMG_2412.JPG").exists shouldBe false
      File(inbox, "2010/07/19/2412.jpg").exists shouldBe false
      File(inbox, "2010/07/20/100.jpg").exists shouldBe false
      File(inbox, "2010/08/01/VID_20191220_141955.mp4").exists shouldBe false
      File(inbox, "undated/9.jpg").exists shouldBe false
      filesUnder(inbox) shouldBe Set(
        "2010/07/19/IMG_123.jpg",
        "2010/07/19/PHOTO.JPG",
        "2010/07/20/IMG_100.jpg",
        "2010/08/02/50.jpg",
        "undated/IMG_9.jpg"
      )
      filesUnder(archive) shouldBe Set(
        "2010/07/19/123.jpg",
        "2010/07/19/2412.jpg",
        "2010/07/20/100.jpg",
        "2010/08/01/20191220_141955.jpg",
        "2010/08/01/20191220_141955v.mp4",
        "2010/08/02/50.jpg"
      )

  it should "write nothing on a dry run and leave a warm index" in:
    withArchive: archive =>
      val day: File = File(archive, "2010/06/01")
      jpeg(day, "1000.jpg")
      val inbox: File = File(File(archive, ".gphoto-sync"), ".download")
      write(inbox, "2010/09/01/IMG_5555.JPG", "dry")
      val root: Root = rootFor(archive)
      val cursor: Session = Session(root, PathStyle.Cursor, root)
      val warmed: Session = run(cursor, "ls", "2010/06/01")._1
      warmed.index.isEmpty shouldBe false

      val (dry, dryResult) = FileDownloads(dryRun = true).execute(warmed)
      okText(dryResult) shouldBe "file 2010/09/01 IMG_5555.JPG 5555.jpg\n"
      dry.index.get(day).get should be theSameInstanceAs warmed.index.get(day).get
      read(inbox, "2010/09/01/IMG_5555.JPG") shouldBe "dry"
      File(archive, "2010/09/01").exists shouldBe false

      val (filed, filedResult) = FileDownloads(dryRun = false).execute(dry)
      okText(filedResult) shouldBe
        """file 2010/09/01 IMG_5555.JPG 5555.jpg
          |wrote 2010/09/01/5555.jpg
          |""".stripMargin
      filed.index.isEmpty shouldBe true
      read(archive, "2010/09/01/5555.jpg") shouldBe "dry"
      File(inbox, "2010/09/01/IMG_5555.JPG").exists shouldBe false

  it should "run in this process and be rejected on the socket" in:
    withArchive: archive =>
      val inbox: File = File(File(archive, ".gphoto-sync"), ".download")
      write(inbox, "2010/09/02/IMG_1.JPG", "local")
      val session: Session = sessionFor(archive)
      val runtime: File = JFiles.createTempDirectory("file-serve").toFile
      val channel: FileChannel = FileChannel.open(
        File(runtime, "serve.lock").toPath,
        StandardOpenOption.CREATE,
        StandardOpenOption.READ,
        StandardOpenOption.WRITE
      )
      val lock = channel.tryLock()
      lock should not be null
      try
        val parsed: Cli.Parsed = Cli.parse(List("file")) match
          case Cli.Run(value) => value
          case other          => fail(other.toString)
        val result: Result = Cli.executeOneShot(
          session,
          List("file"),
          parsed,
          Right(runtime),
          waitMs = 250
        )
        result match
          case Result.Ok(text, _) =>
            text shouldBe
              """file 2010/09/02 IMG_1.JPG 1.jpg
                |wrote 2010/09/02/1.jpg
                |""".stripMargin
          case Result.Usage(message) =>
            message should not include ("a command is in progress")
            fail(message)
          case other =>
            fail(other.toString)
        read(archive, "2010/09/02/1.jpg") shouldBe "local"
      finally
        lock.release()
        channel.close()
        deleteTree(runtime)

      var ran: Boolean = false
      val (_, line) = Cli.serveResponse(
        session,
        """{"argv":["file","--dry-run"]}""",
        (_, _) =>
          ran = true
          (session, Result.Ok("ran\n", Json.Null))
      )
      ran shouldBe false
      line should include("file runs in the calling process")

      val sent = scala.collection.mutable.ListBuffer.empty[List[String]]
      write(inbox, "2010/09/03/IMG_2.JPG", "shell")
      val (code, out, err) = shell(
        session,
        Seq("file --dry-run"),
        (current, parsed) => Cli.toOperation(parsed).execute(current),
        Some((_, argv) =>
          sent += argv
          Result.Ok("sent\n", Json.Null)
        )
      )
      code shouldBe 0
      sent.isEmpty shouldBe true
      out shouldBe "file 2010/09/03 IMG_2.JPG 2.jpg\n"
      err should not include ("Photo Archive Tool:")
      read(inbox, "2010/09/03/IMG_2.JPG") shouldBe "shell"
      File(archive, "2010/09/03").exists shouldBe false

  it should "ignore a dotfile partial in Pictures.plan" in:
    val dir: File = JFiles.createTempDirectory("plan-partial").toFile
    try
      val picture: File = File(dir, "IMG_4.JPG")
      val partial: File = File(dir, ".file-left.partial")
      JFiles.writeString(picture.toPath, "pic")
      JFiles.writeString(partial.toPath, "partial")
      Pictures.plan(Seq(partial, picture)).map(_.finalName) shouldBe Seq("4.jpg")
      Pictures.plan(Seq(partial)).map(_.finalName) shouldBe Seq.empty[String]
    finally deleteTree(dir)

  it should "delete a leftover partial on a real run and leave it on a dry run" in:
    withArchive: archive =>
      val day: File = File(archive, "2010/10/02")
      day.mkdirs()
      val partial: File = File(day, ".file-old.partial")
      JFiles.writeString(partial.toPath, "old")
      val inbox: File = File(File(archive, ".gphoto-sync"), ".download")
      write(inbox, "2010/10/02/IMG_8.JPG", "fresh")
      okText(FileDownloads(dryRun = true).execute(sessionFor(archive))._2) shouldBe
        "file 2010/10/02 IMG_8.JPG 8.jpg\n"
      read(day, ".file-old.partial") shouldBe "old"
      File(day, "8.jpg").exists shouldBe false

      okText(FileDownloads(dryRun = false).execute(sessionFor(archive))._2) shouldBe
        """file 2010/10/02 IMG_8.JPG 8.jpg
          |wrote 2010/10/02/8.jpg
          |""".stripMargin
      read(archive, "2010/10/02/8.jpg") shouldBe "fresh"
      partial.exists shouldBe false
      namesUnder(archive).filter(name => name.startsWith(".") || name.contains("partial")) shouldBe Set.empty[String]

  it should "keep a staged non-picture and still file another day" in:
    withArchive: archive =>
      val inbox: File = File(File(archive, ".gphoto-sync"), ".download")
      write(inbox, "2010/12/03/notes.txt", "nope")
      write(inbox, "2010/12/04/IMG_3.JPG", "yes")
      val text: String = okText(FileDownloads(dryRun = false).execute(sessionFor(archive))._2)
      text should include("keep 2010/12/03/notes.txt ")
      text should include("illegal extension txt")
      text should include("file 2010/12/04 IMG_3.JPG 3.jpg")
      text should include("wrote 2010/12/04/3.jpg")
      read(inbox, "2010/12/03/notes.txt") shouldBe "nope"
      read(archive, "2010/12/04/3.jpg") shouldBe "yes"
      File(archive, "2010/12/03").exists shouldBe false

  it should "keep a day's downloads when a stray day file makes plan throw, and file other days" in:
    withArchive: archive =>
      write(archive, "2010/12/01/notes.txt", "stray")
      val inbox: File = File(File(archive, ".gphoto-sync"), ".download")
      write(inbox, "2010/12/01/IMG_5.JPG", "blocked")
      write(inbox, "2010/12/02/IMG_6.JPG", "other")
      val text: String = okText(FileDownloads(dryRun = false).execute(sessionFor(archive))._2)
      text should include("keep 2010/12/01/IMG_5.JPG ")
      text should include("illegal extension txt")
      text should include("file 2010/12/02 IMG_6.JPG 6.jpg")
      text should include("wrote 2010/12/02/6.jpg")
      read(inbox, "2010/12/01/IMG_5.JPG") shouldBe "blocked"
      read(archive, "2010/12/01/notes.txt") shouldBe "stray"
      read(archive, "2010/12/02/6.jpg") shouldBe "other"
      File(archive, "2010/12/01/5.jpg").exists shouldBe false

  it should "suffix a video staged with its still into an empty day" in:
    withArchive: archive =>
      val inbox: File = File(File(archive, ".gphoto-sync"), ".download")
      write(inbox, "2010/10/01/IMG_100.jpg", "still")
      write(inbox, "2010/10/01/VID_100.mp4", "video")
      val lines: String =
        """file 2010/10/01 IMG_100.jpg 100.jpg
          |file 2010/10/01 VID_100.mp4 100v.mp4
          |""".stripMargin
      okText(FileDownloads(dryRun = true).execute(sessionFor(archive))._2) shouldBe lines
      File(archive, "2010/10/01").exists shouldBe false
      read(inbox, "2010/10/01/IMG_100.jpg") shouldBe "still"
      read(inbox, "2010/10/01/VID_100.mp4") shouldBe "video"

      okText(FileDownloads(dryRun = false).execute(sessionFor(archive))._2) shouldBe
        """file 2010/10/01 IMG_100.jpg 100.jpg
          |wrote 2010/10/01/100.jpg
          |file 2010/10/01 VID_100.mp4 100v.mp4
          |wrote 2010/10/01/100v.mp4
          |""".stripMargin
      read(archive, "2010/10/01/100.jpg") shouldBe "still"
      read(archive, "2010/10/01/100v.mp4") shouldBe "video"
      File(archive, "2010/10/01/100.mp4").exists shouldBe false
      File(inbox, "2010/10/01/IMG_100.jpg").exists shouldBe false
      File(inbox, "2010/10/01/VID_100.mp4").exists shouldBe false

  it should "not file a stem that exists only on an adjacent day" in:
    withArchive: archive =>
      write(archive, "2010/07/18/2412.jpg", "previous")
      write(archive, "2010/07/20/9.jpg", "earlier")
      write(archive, "2010/07/22/9.jpg", "later")
      val inbox: File = File(File(archive, ".gphoto-sync"), ".download")
      write(inbox, "2010/07/19/IMG_2412.JPG", "staged")
      write(inbox, "2010/07/21/IMG_9.JPG", "both")
      val lines: String =
        """date-mismatch 2010/07/19 IMG_2412.JPG 2010/07/18 2412
          |date-mismatch 2010/07/21 IMG_9.JPG 2010/07/20 9
          |date-mismatch 2010/07/21 IMG_9.JPG 2010/07/22 9
          |""".stripMargin
      okText(FileDownloads(dryRun = true).execute(sessionFor(archive))._2) shouldBe lines
      File(archive, "2010/07/19").exists shouldBe false
      File(archive, "2010/07/21").exists shouldBe false
      read(inbox, "2010/07/19/IMG_2412.JPG") shouldBe "staged"
      read(inbox, "2010/07/21/IMG_9.JPG") shouldBe "both"

      val text: String = okText(FileDownloads(dryRun = false).execute(sessionFor(archive))._2)
      text shouldBe lines
      text should not include ("wrote")
      text should not include ("file ")
      File(archive, "2010/07/19").exists shouldBe false
      File(archive, "2010/07/21").exists shouldBe false
      read(inbox, "2010/07/19/IMG_2412.JPG") shouldBe "staged"
      read(inbox, "2010/07/21/IMG_9.JPG") shouldBe "both"
      read(archive, "2010/07/18/2412.jpg") shouldBe "previous"
      read(archive, "2010/07/20/9.jpg") shouldBe "earlier"
      read(archive, "2010/07/22/9.jpg") shouldBe "later"

  it should "not replace an existing destination via createLink" in:
    val dir: File = JFiles.createTempDirectory("file-link").toFile
    try
      val destination: File = File(dir, "7.jpg")
      val partial: File = File(dir, ".file-test.partial")
      JFiles.writeString(destination.toPath, "OLD")
      JFiles.writeString(partial.toPath, "NEW")
      an[FileAlreadyExistsException] should be thrownBy FileDownloads.linkPartial(partial, destination)
      JFiles.readString(destination.toPath) shouldBe "OLD"
      JFiles.readString(partial.toPath) shouldBe "NEW"
    finally deleteTree(dir)

  private def okText(result: Result): String = result match
    case Result.Ok(text, _) => text
    case other              => fail(other.toString)

  private def run(session: Session, args: String*): (Session, Result) =
    Cli.parse(args.toList) match
      case Cli.Run(parsed) => Cli.toOperation(parsed).execute(session)
      case other           => fail(other.toString)

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

  private def write(root: File, relative: String, text: String): Unit =
    val file: File = File(root, relative)
    file.getParentFile.mkdirs()
    JFiles.writeString(file.toPath, text)

  private def read(root: File, relative: String): String =
    JFiles.readString(File(root, relative).toPath)

  private def jpeg(directory: File, name: String): File =
    directory.mkdirs()
    val file: File = File(directory, name)
    ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", file)
    file

  private def filesUnder(root: File): Set[String] =
    walkFiles(root).map(file => Album.relative(root, file)).filterNot(_.startsWith(".gphoto-sync/")).toSet

  private def namesUnder(root: File): Set[String] =
    walkFiles(root).map(_.getName).toSet

  private def walkFiles(directory: File): Seq[File] =
    if !directory.isDirectory then Seq.empty
    else
      val children: Seq[File] = Option(directory.listFiles).toSeq.flatten
      val (dirs, files) = children.partition(_.isDirectory)
      files ++ dirs.flatMap(walkFiles)

  private def sessionFor(archive: File): Session = Session(rootFor(archive))

  private def rootFor(archive: File): Root =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    Root(Configuration(rootPath, zone))

  private def withArchive(body: File => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("file-downloads").toFile
    try body(archive)
    finally deleteTree(archive)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).toSeq.flatten.foreach(deleteTree)
    if file.exists then JFiles.deleteIfExists(file.toPath)
