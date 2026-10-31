package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles}
import java.time.ZoneId

class ServeTest extends AnyFlatSpec, Matchers:
  private val pwdRoot: String = """{"ok":true,"text":"/\n","value":null}"""
  private val pwd2010: String = """{"ok":true,"text":"2010\n","value":null}"""
  private val emptyOk: String = """{"ok":true,"text":"","value":null}"""
  private val malformed: String = """{"ok":false,"error":"Photo Archive Tool: malformed request"}"""

  "serve" should "answer pwd and move the cursor from the archive root" in :
    prepared: (archive, session) =>
      val (code, out, err) = run(session, Seq(
        """{"argv":["pwd"]}""",
        """{"argv":["cd","2010"]}""",
        """{"argv":["pwd"]}""",
        """{"argv":["cd","/"]}""",
        """{"argv":["pwd"]}""",
        """{"argv":["cd","/2010"]}""",
        """{"argv":["pwd"]}"""
      ))
      code shouldBe 0
      err shouldBe ""
      responses(out) shouldBe List(pwdRoot, emptyOk, pwd2010, emptyOk, pwdRoot, emptyOk, pwd2010)
      File(archive, ".gphoto-sync").exists shouldBe false

  it should "keep the session on usage and replace it on failure" in :
    prepared: (_, session) =>
      val year: Directory = session.parse("2010") match
        case directory: Directory => directory
        case other => fail(other.toString)
      val (code, out, err) = run(
        session,
        Seq(
          """{"argv":["cd","2010"]}""",
          """{"argv":["agent"]}""",
          """{"argv":["pwd"]}""",
          """{"argv":["ls","usage-move"]}""",
          """{"argv":["pwd"]}""",
          """{"argv":["cd","/"]}""",
          """{"argv":["ls","move-fail"]}""",
          """{"argv":["pwd"]}"""
        ),
        (current, parsed) => parsed match
          case ls: Cli.Ls if ls.path == "usage-move" =>
            (current.copy(cursor = current.root), Result.Usage("nope"))
          case ls: Cli.Ls if ls.path == "move-fail" =>
            (current.copy(cursor = year), Result.Failed("partial\n", Json.Null, "publish failed"))
          case other =>
            Cli.toOperation(other).execute(current)
      )
      code shouldBe 0
      err shouldBe ""
      responses(out) shouldBe List(
        emptyOk,
        """{"ok":false,"error":"Photo Archive Tool: missing agent command"}""",
        pwd2010,
        """{"ok":false,"error":"Photo Archive Tool: nope"}""",
        pwd2010,
        emptyOk,
        """{"ok":false,"text":"partial\n","error":"Photo Archive Tool: publish failed","value":null}""",
        pwd2010
      )

  it should "answer exit and not read the next line" in :
    prepared: (_, session) =>
      var reads: Int = 0
      val lines: Iterator[String] = Seq(
        """{"id":"1","argv":["exit"]}""",
        """{"argv":["pwd"]}"""
      ).iterator.map: line =>
        reads += 1
        line
      val (code, out, err) = runIterator(session, lines)
      code shouldBe 0
      err shouldBe ""
      reads shouldBe 1
      responses(out) shouldBe List("""{"id":"1","ok":true,"text":"","value":null}""")

  it should "return 0 on EOF without an exit" in :
    prepared: (_, session) =>
      val (code, out, err) = run(session, Seq("""{"argv":["pwd"]}"""))
      code shouldBe 0
      err shouldBe ""
      responses(out) shouldBe List(pwdRoot)
      val (emptyCode, emptyOut, emptyErr) = run(session, Seq.empty)
      emptyCode shouldBe 0
      emptyOut shouldBe ""
      emptyErr shouldBe ""

  it should "reject a blank line and still run the next command" in :
    prepared: (_, session) =>
      val (code, out, err) = run(session, Seq(
        "",
        " \t ",
        """{"argv":["pwd"]}"""
      ))
      code shouldBe 0
      err shouldBe ""
      responses(out) shouldBe List(
        """{"ok":false,"error":"Photo Archive Tool: empty request"}""",
        """{"ok":false,"error":"Photo Archive Tool: empty request"}""",
        pwdRoot
      )

  it should "reject a malformed request and keep the session" in :
    prepared: (_, session) =>
      val (code, out, err) = run(session, Seq(
        """{"argv":["cd","2010"]}""",
        """{"argv":["pwd"],"extra":1}""",
        """{"id":"n","argv":[{"a":true}]}""",
        """{"id":"num","argv":[1]}""",
        """{"id":"m"}""",
        "[]",
        "42",
        """"pwd"""",
        """{"id":1,"argv":["pwd"]}""",
        """{"id":"s","argv":"pwd"}""",
        """{"id":"1","argv":["\q"]}""",
        """{"id":"t","argv":["pwd"]} true""",
        """{"argv":["pwd"]}"""
      ))
      code shouldBe 0
      err shouldBe ""
      responses(out) shouldBe List(
        emptyOk,
        malformed,
        withId("n", malformed),
        withId("num", malformed),
        withId("m", malformed),
        malformed,
        malformed,
        malformed,
        malformed,
        withId("s", malformed),
        withId("1", malformed),
        withId("t", malformed),
        pwd2010
      )

  it should "omit a missing id and echo a string id" in :
    prepared: (_, session) =>
      val (code, out, err) = run(session, Seq(
        """{"argv":["pwd"]}  """,
        """{ "id" : "1" , "argv" : [ "pwd" ] }""",
        """{"id":"a\/b\"c\\d\n\t\r","argv":["pwd"]}"""
      ))
      code shouldBe 0
      err shouldBe ""
      responses(out) shouldBe List(
        pwdRoot,
        """{"id":"1","ok":true,"text":"/\n","value":null}""",
        """{"id":"a/b\"c\\d\n\t\r","ok":true,"text":"/\n","value":null}"""
      )

  it should "reject a flag after the album positional" in :
    prepared: (_, session) =>
      val (code, out, err) = run(session, Seq(
        """{"argv":["cd","2010"]}""",
        """{"argv":["agent","items","2010/07","--id","a1"]}""",
        """{"argv":["pwd"]}"""
      ))
      code shouldBe 0
      err shouldBe ""
      responses(out) shouldBe List(
        emptyOk,
        """{"ok":false,"error":"Photo Archive Tool: extra argument '--id'"}""",
        pwd2010
      )

  it should "put an agent id in value and not in text" in :
    prepared: (archive, session) =>
      writeCatalog(
        archive,
        "album\ta1\t1\t2010/07",
        "item\ta1\tone.jpg\t2010-07-19\tk1\t-"
      )
      val (code, out, err) = run(session, Seq(
        """{"argv":["agent","items","--id","a1","2010/07"]}"""
      ))
      val line: String =
        """{"ok":true,"text":"one.jpg 2010-07-19 -\n","value":{"id":"a1","error":null,"items":[{"filename":"one.jpg","date":"2010-07-19","key":"k1","uploader":null}],"unsets":[]}}"""
      val textField: String = """"text":"one.jpg 2010-07-19 -\n""""
      code shouldBe 0
      err shouldBe ""
      responses(out) shouldBe List(line)
      textField should not include ("a1")
      line should include (""""id":"a1"""")
      Option(File(archive, ".gphoto-sync").list()).get.toSet shouldBe Set("catalog.txt")

  it should "answer help as one JSON line" in :
    prepared: (_, session) =>
      val help: String = Cli.parse(List("help"), Cli.Front.Serve) match
        case Cli.Help(text) => text
        case other => fail(other.toString)
      help.linesIterator.drop(3).map(_.takeWhile(_ != ' ')).toList shouldBe List(
        "lsd", "ls", "repair", "set-metadata", "albums", "publish", "sync",
        "login", "status", "logout", "help", "agent", "cd", "pwd", "exit"
      )
      help should not include ("shell  start a session")
      help should not include ("serve  answer one JSON command per line")
      val (code, out, err) = run(session, Seq("""{"argv":["help"]}"""))
      code shouldBe 0
      err shouldBe ""
      out.count(_ == '\n') shouldBe 1
      responses(out) shouldBe List(Serve.render(None, ok = true, Some(help + "\n"), None, Some(Json.Null)))

  it should "reject shell and serve inside serve" in :
    prepared: (_, session) =>
      val (code, out, err) = run(session, Seq(
        """{"argv":["shell"]}""",
        """{"argv":["serve"]}""",
        """{"id":"s","argv":["shell","--help"]}""",
        """{"argv":["pwd"]}"""
      ))
      code shouldBe 0
      err shouldBe ""
      responses(out) shouldBe List(
        """{"ok":false,"error":"Photo Archive Tool: already running"}""",
        """{"ok":false,"error":"Photo Archive Tool: already running"}""",
        """{"id":"s","ok":false,"error":"Photo Archive Tool: already running"}""",
        pwdRoot
      )

  it should "let an unexpected exception leave serve" in :
    prepared: (_, session) =>
      val out: ByteArrayOutputStream = new ByteArrayOutputStream
      val err: ByteArrayOutputStream = new ByteArrayOutputStream
      val thrown: RuntimeException = the [RuntimeException] thrownBy
        Console.withOut(out):
          Console.withErr(err):
            Cli.serve(session, Iterator("""{"argv":["pwd"]}"""), (_, _) => throw RuntimeException("boom"))
      thrown.getMessage shouldBe "boom"
      text(out) shouldBe ""
      text(err) shouldBe ""

  "the response writer" should "keep order and escape controls" in :
    Serve.write(Json.Obj(Seq(
      "n" -> Json.Long(0),
      "neg" -> Json.Long(-2),
      "ok" -> Json.Bool(true),
      "no" -> Json.Bool(false),
      "z" -> Json.Null,
      "s" -> Json.Str("a\"b\\c\n\t\r"),
      "a" -> Json.Arr(Seq(Json.Long(1), Json.Str("x")))
    ))) shouldBe """{"n":0,"neg":-2,"ok":true,"no":false,"z":null,"s":"a\"b\\c\n\t\r","a":[1,"x"]}"""

  private def withId(id: String, line: String): String =
    """{"id":"""" + id + "\"," + line.drop(1)

  private def responses(text: String): List[String] =
    val parts: Seq[String] = text.split("\n", -1).toSeq
    parts.last shouldBe ""
    parts.dropRight(1).toList

  private def run(session: Session, lines: Seq[String]): (Int, String, String) =
    run(session, lines, (current, parsed) => Cli.toOperation(parsed).execute(current))

  private def run(
    session: Session,
    lines: Seq[String],
    executeCommand: (Session, Cli.Parsed) => (Session, Result)
  ): (Int, String, String) =
    runIterator(session, lines.iterator, executeCommand)

  private def runIterator(
    session: Session,
    lines: Iterator[String],
    executeCommand: (Session, Cli.Parsed) => (Session, Result) = (current, parsed) =>
      Cli.toOperation(parsed).execute(current)
  ): (Int, String, String) =
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    val err: ByteArrayOutputStream = new ByteArrayOutputStream
    val code: Int = Console.withOut(out):
      Console.withErr(err):
        Cli.serve(session, lines, executeCommand)
    (code, text(out), text(err))

  private def text(bytes: ByteArrayOutputStream): String =
    new String(bytes.toByteArray, StandardCharsets.UTF_8)

  private def sessionFor(archive: File): Session =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    val root: Root = Root(Configuration(rootPath, ZoneId.of("America/New_York")))
    Session(root, PathStyle.Cursor, root)

  private def writeCatalog(archive: File, lines: String*): Unit =
    val directory: File = File(archive, ".gphoto-sync")
    directory.mkdirs() shouldBe true
    val file: File = File(directory, "catalog.txt")
    JFiles.writeString(file.toPath, (Seq("# gphoto-catalog 2") ++ lines).mkString("", "\n", "\n"))

  private def prepared(body: (File, Session) => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("serve").toFile
    try
      File(archive, "2010").mkdirs() shouldBe true
      body(archive, sessionFor(archive))
    finally deleteTree(archive)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
