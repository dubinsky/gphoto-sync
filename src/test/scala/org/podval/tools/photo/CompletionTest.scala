package org.podval.tools.photo

import org.jline.reader.{CompletingParsedLine, Parser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.nio.file.Files as JFiles
import java.time.ZoneId

class CompletionTest extends AnyFlatSpec, Matchers:
  private val shellNames: List[String] = List(
    "ls", "repair", "item", "album", "publish", "sync", "push", "refresh", "reconcile", "file",
    "web", "card", "login", "status", "logout", "help", "agent", "cd", "pwd", "exit"
  )

  private val itemVerbs: List[String] = List("show", "set", "month", "into")

  private val albumVerbs: List[String] = List(
    "ls", "show", "claims", "new", "rm", "mv", "include", "exclude", "merge"
  )

  private val agentVerbs: List[String] = List(
    "stems", "years", "months", "days", "claims", "catalog", "items", "match", "locate", "include",
    "album-file", "rename-album", "set-description", "set-date", "download-item", "add-to-album",
    "remove-from-album", "move-item", "delete-item", "upload-item", "create-album", "delete-album",
    "drift", "missing"
  )

  private val lsShortAndLong: List[String] = List(
    "--long", "-l",
    "--recursive", "-R",
    "--name-contains", "-n",
    "--has-extension", "-e",
    "--property",
    "--timestamp-from-name-format",
    "--timestamp-from-name",
    "--timestamp-from-name-only",
    "--timestamps-vary",
    "--has-metadata",
    "--no-normal",
    "--no-timestamp",
    "--wrong-timestamp",
    "--wrong-day",
    "--wrong"
  )

  "completion" should "offer shell commands in help order, and not the retired or front commands" in :
    withSession: session =>
      at(session, "") shouldBe shellNames
      at(session, "al") shouldBe List("album")
      val names: List[String] = at(session, "")
      names should contain ("web")
      names should contain ("card")
      List("lsd", "set-metadata", "albums", "shell", "serve", "mcp").foreach(name => names should not contain name)

  it should "offer item, album, and agent verbs, and card copy only" in :
    withSession: session =>
      at(session, "item ") shouldBe itemVerbs
      at(session, "item mo") shouldBe List("month")
      at(session, "album ") shouldBe albumVerbs
      at(session, "album mer") shouldBe List("merge")
      at(session, "item in") shouldBe List("into")
      at(session, "agent ") shouldBe agentVerbs
      at(session, "agent re") shouldBe List("rename-album", "remove-from-album")
      at(session, "agent ") should not contain ("request")
      at(session, "agent request ") shouldBe Nil
      at(session, "agent request rename-album") shouldBe Nil
      at(session, "card ") shouldBe List("copy")
      at(session, "card c") shouldBe List("copy")
      at(session, "card d") shouldBe Nil

  it should "offer unset flags in long form plus the short aliases" in :
    withSession: session =>
      at(session, "ls -") shouldBe lsShortAndLong
      at(session, "ls --") shouldBe lsShortAndLong.filter(_.startsWith("--"))
      at(session, "ls --long -") shouldBe lsShortAndLong.filter(flag => flag != "--long" && flag != "-l")
      at(session, "ls -l --") should not contain ("--long")
      at(session, "ls -R --") should not contain ("--recursive")
      at(session, "ls --name-contains ") shouldBe Nil
      at(session, "ls --name-contains -- ") shouldBe Nil
      at(session, "ls --name-contains -- 2010") shouldBe Nil
      at(session, "ls --name-contains -- 2010 ") shouldBe List("2010", "2011")
      at(session, "agent include --span -- ") shouldBe Nil
      at(session, "ls --name-contains x ") shouldBe List("2010", "2011")
      at(session, "card -") shouldBe List("--dry-run")
      at(session, "card copy -") shouldBe List("--dry-run")
      at(session, "album new -") shouldBe List("--dry-run", "--span", "--name")
      at(session, "album new --span ") shouldBe Nil
      at(session, "refresh --album Best -") should contain ("--album")
      at(session, "refresh --full -") should not contain ("--full")
      at(session, "item set -") shouldBe List("--dry-run")

  it should "list one date-tree level and skip upload, dot metadata, and extensions" in :
    withSession: session =>
      at(session, "ls ") shouldBe List("2010", "2011")
      at(session, "ls ") should not contain (".upload")
      at(session, "ls ") should not contain (".gphoto-sync")
      at(session, "ls 2010/") shouldBe List("2010/07", "2010/11")
      at(session, "ls 2010/0") shouldBe List("2010/07")
      at(session, "ls 2010/07/") shouldBe List("2010/07/19")
      at(session, "ls 2010/07/19/") shouldBe List("2010/07/19/2412", "2010/07/19/2413")
      at(session, "item show 2010/07/19/") shouldBe List("2010/07/19/2412", "2010/07/19/2413")
      at(session, "ls .upload/") shouldBe Nil
      at(session, "ls .upload/9") shouldBe Nil
      at(session, "ls .gphoto-sync/") shouldBe Nil
      at(session, "cd ") shouldBe List("2010", "2011")
      val year: Directory = session.parse("2010") match
        case directory: Directory => directory
        case other => fail(other.toString)
      at(session.copy(cursor = year), "ls ") shouldBe List("07", "11")
      at(session.copy(cursor = year), "ls /2011/") shouldBe List("/2011/01")
      at(session, "album ls ") shouldBe List("2010", "2011")
      at(session, "item month ") shouldBe List("2010", "2011")
      at(session, "agent include ") shouldBe List("2010", "2011")

  it should "offer archive titles as the raw title" in :
    withSession: session =>
      val titles: List[String] = List(
        "Best",
        "Cape Cod",
        "Say \"Hi\"",
        "a\\b",
        "2010/07/Cape Cod",
        "2010/07",
        "2010/07/19"
      )
      at(session, "album show ") shouldBe titles
      at(session, "album rm ") shouldBe titles
      at(session, "album merge ") shouldBe titles
      at(session, "album merge Best ") shouldBe titles
      at(session, "item into ") shouldBe titles
      at(session, "item into Best ") shouldBe List("2010", "2011")
      at(session, "album mv ") shouldBe titles

  it should "escape a quote or a backslash as the token splitLine reads" in :
    List("Say \"Hi\"", "a\\b", "Cape Cod").foreach: title =>
      val token: String = readerEscape(title)
      Cli.splitLine(token) shouldBe Right(List(title))
      val quotedAgain: String = readerEscape(token)
      Cli.splitLine(quotedAgain) should not be Right(List(title))

  it should "offer item set property names with a trailing equals" in :
    withSession: session =>
      at(session, "item set ") shouldBe List(
        "source=", "timestamp=", "date=", "bad-raw=", "orientation=",
        "google-media-item-id=", "already-in-google=", "uploader="
      )
      at(session, "item set da") shouldBe List("date=")
      at(session, "item set --dry-run up") shouldBe List("uploader=")
      at(session, "item set date= ") shouldBe List("2010", "2011")

  private def readerEscape(title: String): String =
    Cli.shellParser.parse("", 0, Parser.ParseContext.COMPLETE) match
      case completing: CompletingParsedLine => completing.escape(title, true).toString
      case other => fail(other.getClass.getName)

  private def at(session: Session, line: String): List[String] =
    val words: List[String] =
      if line.isEmpty then List("")
      else if line.endsWith(" ") then wordsOf(line.trim) :+ ""
      else wordsOf(line)
    Completion.candidates(session, words, words.length - 1)

  private def wordsOf(line: String): List[String] = Cli.splitLine(line) match
    case Right(words) => words
    case Left(message) => fail(message)

  private def withSession(body: Session => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("completion").toFile
    try
      write(archive, "2010/07/19/2412.jpg")
      write(archive, "2010/07/19/2413.jpg")
      write(archive, "2010/07/19/2412.toml")
      write(archive, "2010/07/19/album.toml")
      write(archive, "2010/11/02/1000.jpg")
      write(archive, "2011/01/02/1000.jpg")
      write(archive, ".upload/9999.jpg")
      File(archive, ".gphoto-sync").mkdirs() shouldBe true
      write(archive, "Best.toml")
      write(archive, "Cape Cod.toml")
      write(archive, "Say \"Hi\".toml")
      write(archive, "a\\b.toml")
      write(archive, "2010/07/Cape Cod.toml")
      write(archive, "2010/07/album.toml")
      body(sessionFor(archive))
    finally deleteTree(archive)

  private def write(archive: File, relative: String): Unit =
    val file: File = File(archive, relative)
    file.getParentFile.mkdirs()
    file.createNewFile() shouldBe true

  private def sessionFor(archive: File): Session =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    val root: Root = Root(Configuration(rootPath, ZoneId.of("America/New_York")))
    Session(root, PathStyle.Cursor, root)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
