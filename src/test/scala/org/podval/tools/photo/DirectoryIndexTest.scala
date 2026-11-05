package org.podval.tools.photo

import com.drew.imaging.ImageProcessingException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import scala.collection.mutable.ListBuffer
import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files as JFiles
import java.nio.file.attribute.FileTime
import java.time.{Instant, ZoneId}
import javax.imageio.ImageIO

class DirectoryIndexTest extends AnyFlatSpec, Matchers:
  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")
  private val august: String = "2010/08/12"
  private val july: String = "2010/07/19"
  private val misnamed: String = "2010/07/20"

  "Pictures.grouped" should "not enact and not print" in:
    withArchive: archive =>
      val file: File = jpeg(File(archive, july), "VID_1.jpg")
      val (text, thrown) = Transcript.collect:
        Pictures.grouped(Seq(file)) shouldBe Seq("1" -> Set("jpg"))
      thrown shouldBe None
      text shouldBe ""
      file.getName shouldBe "VID_1.jpg"

  "a shell ls" should "match a cold run and not construct Extension on the second command" in:
    withArchive: archive =>
      jpeg(File(archive, august), "1000.jpg")
      jpeg(File(archive, august), "20100812_013216.jpg")
      jpeg(File(archive, july), "2412.jpg")
      jpeg(File(archive, misnamed), "VID_1.jpg")
      val root: Root = rootFor(archive)
      val cursor: Session = Session(root, PathStyle.Cursor, root)

      val plain: String = cold(root, "ls", august)
      val long: String = cold(root, "ls", "--long", august)
      val only: String = cold(root, "ls", "--timestamp-from-name-only", august)
      plain should not be long
      only should not be plain
      only should include("timestampFromName=")
      only should not include ("1000")

      val first: Seq[String] = Seq(s"ls $august", s"ls --long $august")
      val (code1, out1, deltas1) = shell(cursor, first :+ "exit")
      code1 shouldBe 0
      out1 shouldBe plain + long
      deltas1 should have size 2
      deltas1.head should be > 0L
      deltas1(1) shouldBe 0L

      val second: Seq[String] = Seq(s"ls --long $august", s"ls $august")
      val (code2, out2, deltas2) = shell(cursor, second :+ "exit")
      code2 shouldBe 0
      out2 shouldBe long + plain
      deltas2(1) shouldBe 0L

      val third: Seq[String] = Seq(
        s"ls --timestamp-from-name-only $august",
        s"ls --timestamp-from-name-only $august"
      )
      val (code3, out3, deltas3) = shell(cursor, third :+ "exit")
      code3 shouldBe 0
      out3 shouldBe only + only
      deltas3(1) shouldBe 0L

      val (namedCode, namedOut, namedDeltas, namedTexts) =
        shellTexts(cursor, Seq(s"ls $misnamed", s"ls $misnamed", "exit"))
      namedCode shouldBe 0
      val oneShotMisnamed: String = cold(root, "ls", misnamed)
      oneShotMisnamed.linesIterator.count(_.contains("WRONG NAME")) shouldBe 1
      namedTexts.head shouldBe oneShotMisnamed
      namedTexts(1).linesIterator.exists(_.contains("WRONG NAME")) shouldBe false
      namedTexts(1) shouldBe namedTexts.head.linesIterator.filterNot(_.contains("WRONG NAME")).mkString("", "\n", "\n")
      namedDeltas(1) shouldBe 0L
      namedOut shouldBe namedTexts.mkString

      val warmed: Session = runOk(cursor, "ls", july)._1
      val (narrowed, narrowText) = runOk(warmed, "ls", "--name-contains", "07", july)
      val before: Long = Extension.constructed
      val again: String = runOk(narrowed, "ls", "--name-contains", "07", july)._2
      Extension.constructed shouldBe before
      narrowText shouldBe ""
      again shouldBe ""
      again shouldBe cold(root, "ls", "--name-contains", "07", july)

      val (augustListed, augustText) = runOk(cursor, "ls", august)
      augustListed.index.get(File(archive, august)).get.listing.get.map(_.stem) shouldBe
        Seq("1000", "20100812_013216")
      augustText.linesIterator.filter(_.startsWith(s"/$august/")).map(_.takeWhile(_ != ' ')).toSeq shouldBe
        Seq(s"/$august/20100812_013216", s"/$august/1000")

  it should "not store a listing when the extractor rejects the file" in:
    withArchive: archive =>
      val relative: String = "2010/08/13"
      val directory: File = File(archive, relative)
      directory.mkdirs()
      File(directory, "1000.jpg").createNewFile() shouldBe true
      val root: Root = rootFor(archive)
      val cursor: Session = Session(root, PathStyle.Cursor, root)

      cold(root, "ls", relative) shouldBe s"/$relative/1000 jpg\n"
      intercept[ImageProcessingException]:
        run(Session(root), "ls", "--long", relative)

      intercept[ImageProcessingException]:
        shell(cursor, Seq(s"ls $relative", "exit"))
      cursor.index.get(directory).flatMap(_.listing) shouldBe None

      intercept[ImageProcessingException]:
        shell(cursor, Seq(s"ls --long $relative", "exit"))

  it should "keep a filtered ls of a misnamed sidecar in the shell" in:
    withArchive: archive =>
      val day: String = "2010/07/21"
      jpeg(File(archive, day), "VID_1.jpg")
      write(archive, s"$day/VID_1.toml", "source = \"camera\"\n")
      val root: Root = rootFor(archive)
      val cursor: Session = Session(root, PathStyle.Cursor, root)
      val (code, _, _, texts) = shellTexts(
        cursor,
        Seq(
          s"ls --name-contains zz $day",
          "pwd",
          "exit"
        )
      )
      code shouldBe 0
      texts should have size 2
      texts.head should include("WRONG NAME")
      texts.head shouldBe cold(root, "ls", "--name-contains", "zz", day)
      texts(1) shouldBe "/\n"

  it should "not fill on cd or on a one-shot ls" in:
    withArchive: archive =>
      jpeg(File(archive, july), "2412.jpg")
      JFiles.createDirectory(archive.toPath.resolve(".gphoto-sync"))
      JFiles.createDirectory(archive.toPath.resolve(".upload"))
      val root: Root = rootFor(archive)
      val cursor: Session = Session(root, PathStyle.Cursor, root)
      var afterCd: Session = cursor
      val (code, _, _) = shell(
        cursor,
        Seq(s"cd $july", "exit"),
        (current, parsed) =>
          val result: (Session, Result) = Cli.toOperation(parsed).execute(current)
          afterCd = result._1
          result
      )
      code shouldBe 0
      afterCd.index.isEmpty shouldBe true
      afterCd.cursor.name shouldBe "19"

      val (oneShot, text) = runOk(Session(root), "ls", july)
      text shouldBe "/2010/07/19/2412 jpg\n"
      oneShot.index.isEmpty shouldBe true

  "the directory index" should "reuse a month only when its own children match and a day only when its files match" in:
    withArchive: archive =>
      jpeg(File(archive, july), "2412.jpg")
      jpeg(File(archive, august), "1000.jpg")
      write(archive, "2010/07/album.toml", "include = [\"all\"]\n")
      write(archive, "2010/08/album.toml", "include = [\"all\"]\n")
      write(archive, "Best.toml", "include = [\"2010/07/19 2412\"]\n")
      JFiles.createDirectory(archive.toPath.resolve(".gphoto-sync"))
      JFiles.createDirectory(archive.toPath.resolve(".upload"))
      val root: Root = rootFor(archive)
      val cursor: Session = Session(root, PathStyle.Cursor, root)

      val (filled, albumsText) = runOk(cursor, "album", "claims")
      albumsText shouldBe cold(root, "album", "claims")
      filled.index.get(archive).get.albums.map(_.album.title) shouldBe Seq("Best")
      val year: DirEntry = filled.index.get(File(archive, "2010")).get
      year.grouped.map(_._1) shouldBe Seq("07", "08")
      year.albums.isEmpty shouldBe true
      year.listing shouldBe None
      val julyMonth: Album.Loaded = filled.index.get(File(archive, "2010/07")).get.albums.head
      julyMonth.album.title shouldBe "2010/07"
      filled.index.get(File(archive, july)).get.listing shouldBe None
      filled.index.get(File(archive, july)).get.grouped.map(_._1) shouldBe Seq("2412")
      filled.index.entries.keys.exists(key => key.endsWith("/.upload") || key.endsWith("/.gphoto-sync")) shouldBe false

      val (listed, longText) = runOk(filled, "ls", "--long", july)
      val dayEntry: DirEntry = listed.index.get(File(archive, july)).get
      dayEntry.listing.get.map(_.stem) shouldBe Seq("2412")
      listed.index.get(File(archive, "2010/07")).get.albums.head should be theSameInstanceAs julyMonth

      val shifted: Instant = Instant.now.plusSeconds(3600)
      JFiles.setLastModifiedTime(File(archive, s"$july/2412.jpg").toPath, FileTime.from(shifted))
      val (reread, longAgain) = runOk(listed, "ls", "--long", july)
      longAgain should not be longText
      longAgain shouldBe cold(root, "ls", "--long", july)
      reread.index.get(File(archive, "2010/07")).get.albums.head should be theSameInstanceAs julyMonth
      reread.index.get(File(archive, "2010")).get should be theSameInstanceAs listed.index
        .get(File(archive, "2010"))
        .get

      write(archive, "2010/07/album.toml", "include = []\n")
      JFiles.setLastModifiedTime(
        File(archive, "2010/07/album.toml").toPath,
        FileTime.from(Instant.now.plusSeconds(7200))
      )
      val (reloaded, albumsAgain) = runOk(reread, "album", "claims")
      albumsAgain shouldBe cold(root, "album", "claims")
      albumsAgain should not include ("2412 albums=2010/07")
      (reloaded.index.get(File(archive, "2010/07")).get.albums.head eq julyMonth) shouldBe false
      reloaded.index.get(File(archive, july)).get.grouped.map(_._1) shouldBe Seq("2412")

  "drops" should "follow the write and leave a dry run cached" in:
    withArchive: archive =>
      jpeg(File(archive, july), "2412.jpg")
      jpeg(File(archive, july), "1000.jpg")
      jpeg(File(archive, august), "1000.jpg")
      val root: Root = rootFor(archive)
      val cursor: Session = Session(root, PathStyle.Cursor, root)
      val warmed: Session = runOk(runOk(cursor, "ls", july)._1, "ls", august)._1
      warmed.index.get(File(archive, july)).get.listing.isDefined shouldBe true
      warmed.index.get(File(archive, august)).get.listing.isDefined shouldBe true

      val (dryNames, _) = runOk(warmed, "repair", "--names", "--dry-run", july)
      dryNames.index.get(File(archive, july)).get should be theSameInstanceAs warmed.index.get(File(archive, july)).get
      val (dryRaw, _) = runOk(dryNames, "repair", "--raw", "--dry-run", july)
      dryRaw.index.get(File(archive, august)).get should be theSameInstanceAs warmed.index
        .get(File(archive, august))
        .get
      val (dryTimestamps, _) = runOk(dryRaw, "repair", "--timestamps", "--dry-run", july)
      dryTimestamps should be theSameInstanceAs dryRaw
      val (dryDays, _) = runOk(dryTimestamps, "repair", "--days", "--dry-run", july)
      dryDays.index.get(File(archive, july)).isDefined shouldBe true

      val (usage, usageResult) = run(dryDays, "item", "set", "nope", s"$july/2412")
      usageResult match
        case Result.Usage(_) => usage should be theSameInstanceAs dryDays
        case other           => fail(other.toString)

      val (clearedDay, _) = runOk(usage, "item", "set", "source=camera", s"$july/2412")
      clearedDay.index.get(File(archive, july)) shouldBe None
      clearedDay.index.get(File(archive, august)).get.listing.isDefined shouldBe true

      val refilled: Session = runOk(clearedDay, "ls", july)._1
      val (named, _) = runOk(refilled, "repair", "--names", july)
      named.index.get(File(archive, july)) shouldBe None
      named.index.get(File(archive, august)).isDefined shouldBe true

      val refilledJuly: Session = runOk(named, "ls", july)._1
      val (stamped, _) = runOk(refilledJuly, "repair", "--timestamps", july)
      stamped.index.get(File(archive, july)) shouldBe None
      stamped.index.get(File(archive, august)).isDefined shouldBe true

      val both: Session = runOk(runOk(stamped, "ls", july)._1, "ls", august)._1
      val (moved, _) = runOk(both, "repair", "--days", july)
      moved.index.isEmpty shouldBe true

    withArchive: archive =>
      jpeg(File(archive, july), "2412.jpg")
      write(archive, "2010/07/album.toml", "include = [\"all\"]\n")
      val root: Root = rootFor(archive)
      val cursor: Session = Session(root, PathStyle.Cursor, root)
      val warmed: Session = runOk(cursor, "ls", july)._1
      val (dryPublish, dryPublishText) = runOk(warmed, "publish", "--dry-run", "2010/07")
      dryPublishText shouldBe cold(root, "publish", "--dry-run", "2010/07")
      dryPublish.index.get(File(archive, july)).get.listing.isDefined shouldBe true
      val (drySync, drySyncText) = runOk(dryPublish, "sync", "--dry-run", "2010/07")
      drySyncText shouldBe "no albums\n"
      drySync.index.get(File(archive, "2010/07")).isDefined shouldBe true

      val (synced, syncText) = runOk(drySync, "sync", "2010/07")
      syncText shouldBe "no albums\n"
      synced.index.isEmpty shouldBe true

    withArchive: archive =>
      jpeg(File(archive, july), "2412.jpg")
      val root: Root = rootFor(archive)
      val cursor: Session = Session(root, PathStyle.Cursor, root)
      val warmed: Session = runOk(cursor, "ls", july)._1
      val (published, publishText) = runOk(warmed, "publish", "--recursive")
      publishText shouldBe "no albums\n"
      published.index.isEmpty shouldBe true
      cold(root, "publish", "--recursive") shouldBe "no albums\n"

  private def cold(root: Root, args: String*): String =
    val (returned, text) = runOk(Session(root), args*)
    returned.index.isEmpty shouldBe true
    text

  private def runOk(session: Session, args: String*): (Session, String) =
    run(session, args*) match
      case (returned, Result.Ok(text, _))       => (returned, text)
      case (_, Result.Failed(text, _, message)) => fail(s"$message\n$text")
      case (_, Result.Usage(message))           => fail(message)

  private def run(session: Session, args: String*): (Session, Result) =
    Cli.parse(args.toList) match
      case Cli.Run(parsed) => Cli.toOperation(parsed).execute(session)
      case other           => fail(other.toString)

  private def shell(session: Session, lines: Seq[String]): (Int, String, Seq[Long]) =
    val (code, out, deltas, _) = shellTexts(session, lines)
    (code, out, deltas)

  private def shell(
      session: Session,
      lines: Seq[String],
      executeCommand: (Session, Cli.Parsed) => (Session, Result)
  ): (Int, String, String) =
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    val err: ByteArrayOutputStream = new ByteArrayOutputStream
    val code: Int = Console.withOut(out):
      Console.withErr(err):
        Cli.shell(session, lines.iterator, executeCommand)
    (code, text(out), text(err))

  private def shellTexts(session: Session, lines: Seq[String]): (Int, String, Seq[Long], Seq[String]) =
    val deltas: ListBuffer[Long] = ListBuffer.empty
    val texts: ListBuffer[String] = ListBuffer.empty
    val (code, out, _) = shell(
      session,
      lines,
      (current, parsed) =>
        val before: Long = Extension.constructed
        val result: (Session, Result) = Cli.toOperation(parsed).execute(current)
        deltas += Extension.constructed - before
        result._2 match
          case Result.Ok(text, _)        => texts += text
          case Result.Failed(text, _, _) => texts += text
          case Result.Usage(_)           => ()
        result
    )
    (code, out, deltas.toSeq, texts.toSeq)

  private def rootFor(archive: File): Root =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    Root(Configuration(rootPath, zone))

  private def jpeg(directory: File, name: String): File =
    directory.mkdirs()
    val file: File = File(directory, name)
    ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", file)
    file

  private def write(archive: File, relative: String, text: String): Unit =
    val file: File = File(archive, relative)
    file.getParentFile.mkdirs()
    JFiles.writeString(file.toPath, text)

  private def text(bytes: ByteArrayOutputStream): String =
    new String(bytes.toByteArray, StandardCharsets.UTF_8)

  private def withArchive(body: File => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("index").toFile
    try body(archive)
    finally deleteTree(archive)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
