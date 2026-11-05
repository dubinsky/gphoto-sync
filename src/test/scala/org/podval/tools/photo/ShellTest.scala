package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files as JFiles
import java.time.ZoneId
import javax.imageio.ImageIO

class ShellTest extends AnyFlatSpec, Matchers:
  @targetName("outcomeCanEqual")
  private given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  "splitLine" should "group quotes and escapes" in:
    Cli.splitLine("ls  07") shouldBe Right(List("ls", "07"))
    Cli.splitLine("ls \"a b\" c") shouldBe Right(List("ls", "a b", "c"))
    Cli.splitLine("cd my\\ dir") shouldBe Right(List("cd", "my dir"))
    Cli.splitLine("a\\nb") shouldBe Right(List("anb"))
    Cli.splitLine("\"a\\\\b\\\"c\"") shouldBe Right(List("a\\b\"c"))
    Cli.splitLine("\"a\\nb\"") shouldBe Right(List("a\\nb"))
    Cli.splitLine("\"a\\ b\"") shouldBe Right(List("a\\ b"))
    Cli.splitLine("ab\"c d\"e") shouldBe Right(List("abc de"))
    Cli.splitLine("--name-contains=\"foo bar\"") shouldBe Right(List("--name-contains=foo bar"))
    Cli.splitLine("\"abc") shouldBe Left("unclosed quote")
    Cli.splitLine("\"abc\\") shouldBe Left("unclosed quote")
    Cli.splitLine("a\\") shouldBe Left("trailing backslash")
    Cli.splitLine("") shouldBe Right(Nil)
    Cli.splitLine(" \t ") shouldBe Right(Nil)

  "the shell help" should "list help, agent, cd, pwd, and exit, and not shell" in:
    Cli.parse(List("--help"), Cli.Front.Shell) match
      case Cli.Help(text) =>
        text.linesIterator.drop(3).map(_.takeWhile(_ != ' ')).toList shouldBe List(
          "ls",
          "repair",
          "item",
          "album",
          "publish",
          "sync",
          "push",
          "refresh",
          "reconcile",
          "file",
          "web",
          "card",
          "login",
          "status",
          "logout",
          "help",
          "agent",
          "cd",
          "pwd",
          "exit"
        )
      case other => fail(other.toString)

  "a shell" should "cd, pwd, and keep the cursor across usage" in:
    prepared: (_, session) =>
      val lines: Seq[String] = Seq(
        "pwd",
        "cd 2010/07/19",
        "pwd",
        "cd",
        "pwd",
        "cd /",
        "pwd",
        "cd 2010/07/19",
        "cd ..",
        "pwd",
        "cd ../..",
        "pwd",
        "cd ..",
        "pwd",
        "cd 2099",
        "pwd",
        "cd 2010/07/19/2412",
        "pwd",
        "cd \"2010/07/19\"",
        "pwd",
        "cd 2010/07/19/9999",
        "pwd",
        "cd 2010/07/19/2412.jpg",
        "pwd",
        "cd /",
        "cd 2010/07/\\19",
        "pwd",
        "\"oops",
        "pwd",
        "",
        "pwd",
        "exit"
      )
      val (code, out, err) = run(session, lines)
      code shouldBe 0
      out shouldBe
        """/
          |2010/07/19
          |/
          |/
          |2010/07
          |/
          |/
          |/
          |/
          |2010/07/19
          |2010/07/19
          |2010/07/19
          |2010/07/19
          |2010/07/19
          |2010/07/19
          |""".stripMargin
      err should include("Photo Archive Tool: .. above the archive root")
      err should include("Photo Archive Tool: no such directory")
      err should include("Photo Archive Tool: not a directory")
      err should include("Photo Archive Tool: unclosed quote")
      err should include("> ")
      err should include("2010/07> ")
      err should include("2010/07/19> ")
      out should not include ("> ")
      out should not include ("Photo Archive Tool")

  it should "list relative to the cursor and treat a leading slash as the archive root" in:
    prepared: (archive, session) =>
      val (code, out, err) = run(
        session,
        Seq(
          "cd 2010",
          "ls 07",
          "ls \"07\"",
          "cd /2011",
          "ls /2010/07",
          "ls 01",
          "cd /2011/01",
          "ls ..",
          "ls /2010",
          "exit"
        )
      )
      code shouldBe 0
      out shouldBe
        """2010/07/19
          |2010/07/19
          |2010/07/19
          |2011/01/02
          |2011/01
          |2010/07
          |""".stripMargin
      err should include("2010> ")
      err should include("2011> ")
      err should include("2011/01> ")
      out should not include ("> ")

      val root: Root = session.root
      val oneShot: Session = Session(root, PathStyle.OneShot, root)
      val julyDays: Json = Json.Obj(
        Seq(
          "kind" -> Json.Str("directories"),
          "entries" -> Json.Arr(
            Seq(
              Json.Obj(Seq("path" -> Json.Str("2010/07/19"), "items" -> Json.Long(1)))
            )
          )
        )
      )
      execute(oneShot, "ls", "/2010/07") shouldBe execute(oneShot, "ls", "2010/07")
      execute(oneShot, "ls", "2010/07") shouldBe Result.Ok("2010/07/19\n", julyDays)
      execute(oneShot, "ls", "..") match
        case Result.Usage(message) => message should include("not a year")
        case other                 => fail(other.toString)
      execute(oneShot, "ls", ".") match
        case Result.Usage(message) => message should include("not a year")
        case other                 => fail(other.toString)
      execute(oneShot, "ls", "2010/..") match
        case Result.Usage(message) => message should include("not a month")
        case other                 => fail(other.toString)

      val year: Directory = session.parse("2010") match
        case directory: Directory => directory
        case other                => fail(other.toString)
      val oneShotAtYear: Session = Session(root, PathStyle.OneShot, year)
      execute(oneShotAtYear, "ls", "07") match
        case Result.Usage(_) => ()
        case other           => fail(other.toString)
      val cursorAtYear: Session = Session(root, PathStyle.Cursor, year)
      execute(cursorAtYear, "ls", "07") shouldBe Result.Ok("2010/07/19\n", julyDays)
      execute(cursorAtYear, "ls", "/2010/07") shouldBe Result.Ok("2010/07/19\n", julyDays)
      cursorAtYear.parse("07").toString shouldBe "/2010/07"

      File(archive, ".gphoto-sync").exists shouldBe false
      File(archive, ".upload").exists shouldBe false
      File(archive, ".download").exists shouldBe false

  it should "keep a cursor returned with Failed and ignore one returned with Usage" in:
    prepared: (_, session) =>
      val year: Directory = session.parse("2010") match
        case directory: Directory => directory
        case other                => fail(other.toString)
      val executeCommand = (current: Session, parsed: Cli.Parsed) =>
        parsed match
          case ls: Cli.Ls if ls.path.contains("move-fail") =>
            (current.copy(cursor = year), Result.Failed("partial\n", Json.Null, "boom"))
          case ls: Cli.Ls if ls.path.contains("usage-move") =>
            (current.copy(cursor = current.root), Result.Usage("nope"))
          case other =>
            Cli.toOperation(other).execute(current)
      val (code, out, err) = run(
        session,
        Seq(
          "ls move-fail",
          "pwd",
          "ls usage-move",
          "pwd",
          "exit"
        ),
        executeCommand
      )
      code shouldBe 0
      out shouldBe "partial\n2010\n2010\n"
      err should include("Photo Archive Tool: boom")
      err should include("Photo Archive Tool: nope")
      err should include("2010> ")
      out should not include ("> ")

  it should "exit 0 on EOF and on exit" in:
    prepared: (_, session) =>
      val (eofCode, eofOut, eofErr) = run(session, Seq("pwd"))
      eofCode shouldBe 0
      eofOut shouldBe "/\n"
      eofErr should include("> ")
      eofOut should not include ("> ")

      val (exitCode, exitOut, _) = run(session, Seq("exit", "pwd"))
      exitCode shouldBe 0
      exitOut shouldBe ""

  it should "reject a nested shell and keep going" in:
    prepared: (_, session) =>
      val (code, out, err) = run(session, Seq("shell", "shell --help", "pwd", "exit"))
      code shouldBe 0
      out shouldBe "/\n"
      err should include("Photo Archive Tool: already running")

  it should "print help on stdout and not leave" in:
    prepared: (_, session) =>
      val (code, out, err) = run(session, Seq("help", "help cd", "pwd", "exit"))
      code shouldBe 0
      out should startWith("Photo Archive Tool 0.0.1")
      out should include("cd  change the cursor; only available in the shell")
      out should include("pwd  print the cursor; only available in the shell")
      out should include("exit  leave the session; only available in the shell")
      out.linesIterator.exists(_.startsWith("shell")) shouldBe false
      out.linesIterator.exists(_.startsWith("agent")) shouldBe true
      out.linesIterator.exists(_.startsWith("serve")) shouldBe false
      out should endWith("/\n")
      err should include("> ")
      out should not include ("> ")

  it should "send archive-root paths and keep cd, pwd, and exit local" in:
    prepared: (_, session) =>
      var sent: List[List[String]] = Nil
      val exchange = (_: Session, argv: List[String]) =>
        sent = sent :+ argv
        Result.Ok("", Json.Null)
      val (code, out, _) = run(
        session,
        Seq(
          "cd 2010",
          "album ls",
          "album claims",
          "ls",
          "ls 07",
          "ls --long 07",
          "album ls 07",
          "pwd",
          "album claims 07",
          "publish",
          "item set uploader 07/19/2412",
          "agent include 2009/03/10/2412",
          "exit",
          "ls 07"
        ),
        exchange = Some(exchange)
      )
      code shouldBe 0
      sent shouldBe List(
        List("album", "ls"),
        List("album", "claims"),
        List("ls", "2010"),
        List("ls", "2010/07"),
        List("ls", "--long", "2010/07"),
        List("album", "ls", "2010/07"),
        List("album", "claims", "2010/07"),
        List("publish"),
        List("item", "set", "uploader", "2010/07/19/2412"),
        List("agent", "include", "2009/03/10/2412")
      )
      out shouldBe "2010\n"

  it should "rewrite a relative item show to an archive-root path" in:
    prepared: (_, session) =>
      var sent: List[List[String]] = Nil
      val exchange = (_: Session, argv: List[String]) =>
        sent = sent :+ argv
        Result.Ok("", Json.Null)
      val (code, _, _) = run(
        session,
        Seq("cd 2010/07/19", "item show 2412"),
        exchange = Some(exchange)
      )
      code shouldBe 0
      sent shouldBe List(List("item", "show", "2010/07/19/2412"))

  private def execute(session: Session, args: String*): Result =
    Cli.parse(args.toList) match
      case Cli.Run(parsed) =>
        Cli.toOperation(parsed).execute(session)._2
      case other =>
        fail(other.toString)

  private def run(
    session: Session,
    lines: Seq[String],
    executeCommand: (Session, Cli.Parsed) => (Session, Result) = (current, parsed) =>
      Cli.toOperation(parsed).execute(current),
    exchange: Option[(Session, List[String]) => Result] = None
  ): (Int, String, String) =
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    val err: ByteArrayOutputStream = new ByteArrayOutputStream
    val code: Int = Console.withOut(out):
      Console.withErr(err):
        Cli.shell(session, lines.iterator, executeCommand, exchange)
    (code, text(out), text(err))

  private def text(bytes: ByteArrayOutputStream): String =
    new String(bytes.toByteArray, StandardCharsets.UTF_8)

  private def prepared(body: (File, Session) => Unit): Unit =
    withArchive: archive =>
      touch(File(archive, "2010/07/19"), "2412.jpg")
      touch(File(archive, "2011/01/02"), "1000.jpg")
      body(archive, sessionFor(archive))

  private def sessionFor(archive: File, style: PathStyle = PathStyle.Cursor): Session =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    val root: Root = Root(Configuration(rootPath, ZoneId.of("America/New_York")))
    Session(root, style, root)

  private def touch(directory: File, name: String): Unit =
    directory.mkdirs() shouldBe true
    ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", File(directory, name)) shouldBe true

  private def withArchive(body: File => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("shell").toFile
    try body(archive)
    finally deleteTree(archive)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
