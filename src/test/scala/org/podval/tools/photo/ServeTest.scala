package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, File}
import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException, ServerSocketChannel, SocketChannel}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, StandardOpenOption}
import java.nio.file.attribute.{BasicFileAttributes, FileTime, PosixFilePermissions}
import java.time.ZoneId
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import javax.imageio.ImageIO

class ServeTest extends AnyFlatSpec, Matchers:
  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  @targetName("attemptCanEqual")
  private given [A <: Serve.Attempt, B <: Serve.Attempt] => CanEqual[A, B] = CanEqual.derived

  @targetName("responseCanEqual")
  private given [A <: Serve.Response, B <: Serve.Response] => CanEqual[A, B] = CanEqual.derived

  // strictEquality does not relate a sealed parent to its cases.
  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  @targetName("calledCanEqual")
  private given [A <: Serve.Called, B <: Serve.Called] => CanEqual[A, B] = CanEqual.derived

  "a response" should "require archive and ignore value" in :
    val accept: String = Serve.render(None, ok = true, None, None, None, "/archive")
    accept shouldBe """{"ok":true,"archive":"/archive"}"""
    Serve.readResponse(accept) shouldBe Right(Serve.Response(None, true, None, None, "/archive"))
    val command: String = Serve.render(
      Some("1"),
      ok = true,
      Some("/2010\n"),
      None,
      Some(Json.Null),
      "/archive"
    )
    command shouldBe """{"id":"1","ok":true,"archive":"/archive","text":"/2010\n","value":null}"""
    Serve.readResponse(command) shouldBe Right(Serve.Response(Some("1"), true, Some("/2010\n"), None, "/archive"))
    Serve.readResponse("""{"ok":true,"archive":"/a","value":{"id":"a1","n":[1,true,null]}}""") shouldBe
      Right(Serve.Response(None, true, None, None, "/a"))
    Serve.readResponse("""{"ok":true}""") match
      case Left(problem) => problem.message shouldBe "malformed request"
      case Right(other) => fail(other.toString)
    Serve.readResponse("""{"ok":true,"archive":"/a","nope":1}""") match
      case Left(problem) => problem.message shouldBe "malformed request"
      case Right(other) => fail(other.toString)
    Serve.read("""{"argv":["lsd","2010"],"archive":"/tmp/a"}""") match
      case Left(problem) => problem.message shouldBe "malformed request"
      case Right(other) => fail(other.toString)

  it should "retain a nested value that readResponse drops" in :
    val line: String = """{"ok":true,"archive":"/a","value":{"id":"a1","n":[1,true,null]}}"""
    val response: Serve.Response = Serve.Response(None, true, None, None, "/a")
    val value: Json = Json.Obj(Seq(
      "id" -> Json.Str("a1"),
      "n" -> Json.Arr(Seq(Json.Long(1), Json.Bool(true), Json.Null))
    ))
    Serve.readResponse(line) shouldBe Right(response)
    Serve.readValued(line) shouldBe Right(Serve.Called(response, value))

  it should "reject a fraction, an exponent, and a long outside range only when retaining" in :
    val response: Serve.Response = Serve.Response(None, true, None, None, "/a")
    def valued(token: String): String = s"""{"ok":true,"archive":"/a","value":$token}"""
    List("1.5", "1e2", "9223372036854775808", "-9223372036854775809").foreach: token =>
      Serve.readResponse(valued(token)) shouldBe Right(response)
      Serve.readValued(valued(token)) match
        case Left(problem) => problem.message shouldBe "malformed request"
        case Right(other) => fail(other.toString)
      wholeJson(token) match
        case Left(_) => ()
        case Right(json) => fail(json.toString)
    val maxLine: String = valued("9223372036854775807")
    val minLine: String = valued("-9223372036854775808")
    Serve.readResponse(maxLine) shouldBe Right(response)
    Serve.readResponse(minLine) shouldBe Right(response)
    Serve.readValued(maxLine) shouldBe Right(Serve.Called(response, Json.Long(Long.MaxValue)))
    Serve.readValued(minLine) shouldBe Right(Serve.Called(response, Json.Long(Long.MinValue)))
    wholeJson("9223372036854775807") shouldBe Right(Json.Long(Long.MaxValue))
    wholeJson("-9223372036854775808") shouldBe Right(Json.Long(Long.MinValue))
    wholeJson("true trailing") match
      case Left(_) => ()
      case Right(json) => fail(json.toString)
    wholeJson("  true \n") shouldBe Right(Json.Bool(true))

  "serve" should "treat cd, pwd, and exit as usage and keep reading" in :
    prepared: (_, session) =>
      respond(session, Seq(
        """{"argv":["cd","2010"]}""",
        """{"argv":["pwd"]}""",
        """{"argv":["exit"]}""",
        """{"argv":["lsd","2010"]}"""
      )) shouldBe List(
        usage(session, "cd is only available in the shell"),
        usage(session, "pwd is only available in the shell"),
        usage(session, "exit is only available in the shell"),
        lsdOk(session)
      )

  it should "reset the cursor after a command" in :
    prepared: (_, session) =>
      var cursors: List[String] = Nil
      val lines: List[String] = respond(
        session,
        Seq("""{"argv":["ls","2010"]}""", """{"argv":["lsd","2010"]}"""),
        (current, parsed) =>
          cursors = cursors :+ Session.display(current.cursor)
          parsed match
            case _: Cli.Ls =>
              current.catalogCache.isDefined shouldBe false
              val year: Directory = current.parse("2010") match
                case directory: Directory => directory
                case other => fail(other.toString)
              val marked: Session =
                current.copy(cursor = year, catalogCache = Some(CatalogCache(1L, 2L, Seq.empty[Catalog.Album])))
              (marked, Result.Ok("moved\n", Json.Null))
            case _ =>
              val cache: CatalogCache =
                current.catalogCache.getOrElse(fail("continued session dropped catalogCache"))
              cache.size shouldBe 1L
              cache.mtimeMillis shouldBe 2L
              (current, Result.Ok("next\n", Json.Null))
      )
      cursors shouldBe List("", "")
      lines shouldBe List(
        rendered(session, ok = true, text = Some("moved\n"), value = Some(Json.Null)),
        rendered(session, ok = true, text = Some("next\n"), value = Some(Json.Null))
      )

  it should "return Step.Stop for serve stop" in :
    prepared: (_, session) =>
      Cli.serveResponse(session, """{"id":"1","argv":["serve","stop"]}""") match
        case (Cli.Step.Stop, line) =>
          line shouldBe rendered(session, ok = true, id = Some("1"))
        case other =>
          fail(other.toString)

  it should "reject a blank line and still run the next command" in :
    prepared: (_, session) =>
      respond(session, Seq(
        "",
        " \t ",
        """{"argv":["lsd","2010"]}"""
      )) shouldBe List(
        usage(session, "empty request"),
        usage(session, "empty request"),
        lsdOk(session)
      )

  it should "reject a malformed request and keep the session" in :
    prepared: (_, session) =>
      val malformed: String = usage(session, "malformed request")
      respond(session, Seq(
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
        """{"argv":["lsd","2010"]}"""
      )) shouldBe List(
        usage(session, "cd is only available in the shell"),
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
        lsdOk(session)
      )

  it should "omit a missing id and echo a string id" in :
    prepared: (_, session) =>
      respond(session, Seq(
        """{"argv":["lsd","2010"]}  """,
        """{ "id" : "1" , "argv" : [ "lsd", "2010" ] }""",
        """{"id":"a\/b\"c\\d\n\t\r","argv":["lsd","2010"]}"""
      )) shouldBe List(
        lsdOk(session),
        rendered(session, ok = true, text = Some("/2010\n"), value = Some(Json.Null), id = Some("1")),
        rendered(
          session,
          ok = true,
          text = Some("/2010\n"),
          value = Some(Json.Null),
          id = Some("a/b\"c\\d\n\t\r")
        )
      )

  it should "reject a flag after the album positional" in :
    prepared: (_, session) =>
      respond(session, Seq(
        """{"argv":["agent","items","2010/07","--id","a1"]}""",
        """{"argv":["lsd","2010"]}"""
      )) shouldBe List(
        usage(session, "extra argument '--id'"),
        lsdOk(session)
      )

  it should "put an agent id in value and not in text" in :
    prepared: (archive, session) =>
      writeCatalog(
        archive,
        "album\ta1\t1\t2010/07",
        "item\ta1\tone.jpg\t2010-07-19\tk1\t-"
      )
      val (_, line) = Cli.serveResponse(session, """{"argv":["agent","items","--id","a1","2010/07"]}""")
      val value: Json = Json.Obj(Seq(
        "id" -> Json.Str("a1"),
        "error" -> Json.Null,
        "items" -> Json.Arr(Seq(Json.Obj(Seq(
          "filename" -> Json.Str("one.jpg"),
          "date" -> Json.Str("2010-07-19"),
          "key" -> Json.Str("k1"),
          "uploader" -> Json.Null
        )))),
        "unsets" -> Json.Arr(Seq.empty)
      ))
      val textField: String = """"text":"one.jpg 2010-07-19 -\n""""
      line shouldBe rendered(
        session,
        ok = true,
        text = Some("one.jpg 2010-07-19 -\n"),
        value = Some(value)
      )
      textField should not include ("a1")
      line should include (""""id":"a1"""")
      Option(File(archive, ".gphoto-sync").list()).get.toSet shouldBe Set("catalog.txt")

  it should "answer help as one JSON line" in :
    prepared: (_, session) =>
      val help: String = Cli.parse(List("help"), Cli.Front.Serve) match
        case Cli.Help(text) => text
        case other => fail(other.toString)
      help.linesIterator.drop(3).map(_.takeWhile(_ != ' ')).toList shouldBe List(
        "lsd", "ls", "repair", "set-metadata", "albums", "publish", "sync", "push", "refresh", "reconcile", "file",
        "web", "card", "login", "status", "logout", "help", "agent"
      )
      help should not include ("shell  start a session")
      help should not include ("serve  answer one JSON command per line")
      help should not include ("cd  ")
      val (_, line) = Cli.serveResponse(session, """{"argv":["help"]}""")
      line.exists(_ == '\n') shouldBe false
      line shouldBe rendered(session, ok = true, text = Some(help + "\n"), value = Some(Json.Null))

  it should "refuse web on the socket" in :
    prepared: (_, session) =>
      val (_, line) = Cli.serveResponse(
        session,
        """{"argv":["web","--port","9"]}""",
        (_, _) => fail("web was forwarded")
      )
      line shouldBe usage(session, "web runs in the calling process")
      line should include ("\"ok\":false")

  it should "reject shell and serve inside serve" in :
    prepared: (_, session) =>
      respond(session, Seq(
        """{"argv":["shell"]}""",
        """{"argv":["serve"]}""",
        """{"id":"s","argv":["shell","--help"]}""",
        """{"argv":["lsd","2010"]}"""
      )) shouldBe List(
        usage(session, "already running"),
        usage(session, "already running"),
        usage(session, "already running", id = Some("s")),
        lsdOk(session)
      )

  it should "escape a backspace so the echoed id round-trips" in :
    prepared: (_, session) =>
      val (_, line) = Cli.serveResponse(session, """{"id":"a\b","argv":["lsd","2010"]}""")
      line shouldBe rendered(
        session,
        ok = true,
        text = Some("/2010\n"),
        value = Some(Json.Null),
        id = Some("a\b")
      )
      line should not include ("\b")
      Serve.read(line) match
        case Left(problem) =>
          problem.message shouldBe "malformed request"
          problem.id shouldBe Some("a\b")
        case Right(other) =>
          fail(other.toString)

  it should "let an unexpected exception leave serveResponse" in :
    prepared: (_, session) =>
      val thrown: RuntimeException = the [RuntimeException] thrownBy
        Cli.serveResponse(
          session,
          """{"argv":["lsd","2010"]}""",
          (_, _) => throw RuntimeException("boom")
        )
      thrown.getMessage shouldBe "boom"

  "the response writer" should "keep order and escape controls" in :
    Serve.write(Json.Obj(Seq(
      "n" -> Json.Long(0),
      "neg" -> Json.Long(-2),
      "ok" -> Json.Bool(true),
      "no" -> Json.Bool(false),
      "z" -> Json.Null,
      "s" -> Json.Str("a\"b\\c\n\t\r\b\f\u0001"),
      "a" -> Json.Arr(Seq(Json.Long(1), Json.Str("x")))
    ))) shouldBe """{"n":0,"neg":-2,"ok":true,"no":false,"z":null,"s":"a\"b\\c\n\t\r\b\f\u0001","a":[1,"x"]}"""
    val round: String = "a\fb\u0001"
    val line: String = Serve.render(None, ok = true, Some(round), None, None, "/archive")
    Serve.readResponse(line) shouldBe Right(Serve.Response(None, true, Some(round), None, "/archive"))

  "the socket" should "bind under the runtime directory and answer before the request" in :
    prepared: (_, session) =>
      val runtime: File = tempRuntime()
      try
        JFiles.setPosixFilePermissions(runtime.toPath, PosixFilePermissions.fromString("rwxr-xr-x"))
        withServer(session, runtime): running =>
          perms(runtime) shouldBe "rwx------"
          perms(File(runtime, "serve.sock")) shouldBe "rw-------"
          running.err should include (s"bound ${File(runtime, "serve.sock").getAbsolutePath}")
          val channel: SocketChannel = connect(runtime)
          try
            Serve.readResponse(readSocket(channel)) shouldBe
              Right(Serve.Response(None, true, None, None, Serve.archiveOf(session)))
            writeSocket(channel, Serve.renderRequest(None, List("lsd", "2010")))
            readSocket(channel) shouldBe lsdOk(session)
          finally channel.close()
      finally deleteTree(runtime)

  it should "reuse a directory index across connections" in :
    withPicture: (_, session) =>
      val runtime: File = tempRuntime()
      try
        withServer(session, runtime): _ =>
          val first: Long = Extension.constructed
          val listed: Serve.Response = command(runtime, session, List("ls", "2010/07/19"))
          listed.ok shouldBe true
          listed.text.getOrElse("") should include ("2412")
          val filled: Long = Extension.constructed
          filled should be > first
          command(runtime, session, List("ls", "2010/07/19"))
          Extension.constructed shouldBe filled
      finally deleteTree(runtime)

  it should "replace a stale socket after the lock is free" in :
    prepared: (_, session) =>
      val runtime: File = tempRuntime()
      try
        val running: Running = start(session, runtime)
        val socket: File = File(runtime, "serve.sock")
        val key: Object = JFiles.readAttributes(socket.toPath, classOf[BasicFileAttributes]).fileKey
        running.crash()
        JFiles.exists(socket.toPath) shouldBe true
        val again: Running = start(session, runtime)
        try
          val replaced: Object = JFiles.readAttributes(socket.toPath, classOf[BasicFileAttributes]).fileKey
          java.util.Objects.equals(replaced, key) shouldBe false
          command(runtime, session, List("lsd", "2010")).ok shouldBe true
        finally again.finish()
      finally deleteTree(runtime)

  it should "refuse a second serve without unlinking" in :
    prepared: (_, session) =>
      val runtime: File = tempRuntime()
      try
        withServer(session, runtime): _ =>
          val socket: File = File(runtime, "serve.sock")
          val lock: File = File(runtime, "serve.lock")
          val key: Object = JFiles.readAttributes(socket.toPath, classOf[BasicFileAttributes]).fileKey
          Serve.disarmOwner()
          val err: ByteArrayOutputStream = new ByteArrayOutputStream
          val code: Int = Console.withErr(err)(Serve.listen(session, runtime))
          code shouldBe 0
          text(err) shouldBe "already running\n"
          java.util.Objects.equals(
            JFiles.readAttributes(socket.toPath, classOf[BasicFileAttributes]).fileKey,
            key
          ) shouldBe true
          childLock(lock, socket) shouldBe "held\n"
          JFiles.exists(socket.toPath) shouldBe true
          command(runtime, session, List("lsd", "2010")).ok shouldBe true
      finally deleteTree(runtime)

  it should "not run cold while the lock is held and the socket is quiet" in :
    prepared: (_, session) =>
      val runtime: File = tempRuntime()
      val channel: FileChannel = FileChannel.open(
        File(runtime, "serve.lock").toPath,
        StandardOpenOption.CREATE,
        StandardOpenOption.READ,
        StandardOpenOption.WRITE
      )
      val lock: FileLock = channel.tryLock()
      lock should not be null
      try
        val result: Result = Cli.executeOneShot(
          session.copy(style = PathStyle.OneShot),
          List("lsd", "2010"),
          lsdParsed,
          Right(runtime),
          waitMs = 250
        )
        result match
          case Result.Usage(message) =>
            message should include ("a command is in progress")
            message should include ("photo serve stop runs only between commands")
            message should include (s"fuser ${File(runtime, "serve.lock").getAbsolutePath}")
          case other =>
            fail(other.toString)
        childLock(File(runtime, "serve.lock"), File(runtime, "serve.sock")) shouldBe "held\n"
        File(runtime, "serve.sock").exists shouldBe false
      finally
        lock.release()
        channel.close()
        deleteTree(runtime)

  it should "time out the accept line and not the command response" in :
    prepared: (_, session) =>
      val runtime: File = tempRuntime()
      try
        val quiet: Stall = stall(runtime, delayMs = 800, respond = false, Serve.archiveOf(session))
        try
          val attempt: Serve.Attempt = Serve.client(
            runtime,
            List("lsd", "2010"),
            Serve.archiveOf(session),
            waitMs = 200
          )
          attempt match
            case Serve.Attempt.Failed(message) =>
              message should include ("a command is in progress")
            case other =>
              fail(other.toString)
        finally quiet.close()
        deleteTree(runtime)
        val again: File = tempRuntime()
        try
          val slow: Stall = stall(again, delayMs = 800, respond = true, Serve.archiveOf(session))
          try
            Serve.client(again, List("lsd", "2010"), Serve.archiveOf(session), waitMs = 200) match
              case Serve.Attempt.Done(response) =>
                response.ok shouldBe true
                response.text shouldBe Some("late\n")
              case other =>
                fail(other.toString)
          finally slow.close()
        finally deleteTree(again)
      finally deleteTree(runtime)

  "a one-shot" should "run cold when the lock is free and not spawn" in :
    prepared: (_, session) =>
      val oneShot: Session = session.copy(style = PathStyle.OneShot)
      Cli.executeOneShot(oneShot, List("lsd", "2010"), lsdParsed, Left("XDG_RUNTIME_DIR is unset")) shouldBe
        Result.Ok("/2010\n", Json.Null)
      val runtime: File = tempRuntime()
      try
        Cli.executeOneShot(oneShot, List("lsd", "2010"), lsdParsed, Right(runtime), waitMs = 200) shouldBe
          Result.Ok("/2010\n", Json.Null)
        File(runtime, "serve.sock").exists shouldBe false
        File(runtime, "serve.lock").exists shouldBe false
        val stale: ServerSocketChannel = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
        try
          stale.bind(UnixDomainSocketAddress.of(File(runtime, "serve.sock").toPath))
        finally stale.close()
        File(runtime, "serve.sock").exists shouldBe true
        Cli.executeOneShot(oneShot, List("lsd", "2010"), lsdParsed, Right(runtime), waitMs = 200) shouldBe
          Result.Ok("/2010\n", Json.Null)
        File(runtime, "serve.sock").exists shouldBe true
        Cli.executeStop(Right(runtime)) shouldBe Left("serve is not running")
      finally deleteTree(runtime)
      Cli.executeStop(Left("XDG_RUNTIME_DIR is unset")) shouldBe Left("serve is not running")

  it should "use the socket when the archive matches" in :
    withPicture: (_, session) =>
      val runtime: File = tempRuntime()
      try
        withServer(session, runtime): _ =>
          val oneShot: Session = session.copy(style = PathStyle.OneShot)
          val before: Long = Extension.constructed
          val parsed: Cli.Parsed = Cli.parse(List("ls", "2010/07/19")) match
            case Cli.Run(value) => value
            case other => fail(other.toString)
          val result: Result = Cli.executeOneShot(
            oneShot,
            List("ls", "2010/07/19"),
            parsed,
            Right(runtime),
            waitMs = 2000
          )
          result match
            case Result.Ok(text, _) => text should include ("2412")
            case other => fail(other.toString)
          val filled: Long = Extension.constructed
          filled should be > before
          command(runtime, session, List("ls", "2010/07/19"))
          Extension.constructed shouldBe filled
      finally deleteTree(runtime)

  it should "name both archives on a mismatch and still stop" in :
    prepared: (_, session) =>
      val other: File = JFiles.createTempDirectory("serve-other").toFile
      File(other, "2010").mkdirs() shouldBe true
      val client: Session = sessionFor(other, PathStyle.OneShot)
      val runtime: File = tempRuntime()
      try
        val running: Running = start(session, runtime)
        try
          val result: Result = Cli.executeOneShot(
            client,
            List("lsd", "2010"),
            lsdParsed,
            Right(runtime),
            waitMs = 2000
          )
          result match
            case Result.Usage(message) =>
              message should include (Serve.archiveOf(session))
              message should include (Serve.archiveOf(client))
              message should include ("run photo serve stop")
            case other =>
              fail(other.toString)
          Cli.executeStop(Right(runtime), waitMs = 2000) match
            case Right(response) =>
              response.ok shouldBe true
              response.archive shouldBe Serve.archiveOf(session)
            case Left(message) =>
              fail(message)
          running.awaitExit()
          File(runtime, "serve.sock").exists shouldBe false
          Serve.client(runtime, List("lsd", "2010"), Serve.archiveOf(session), waitMs = 200) shouldBe
            Serve.Attempt.Absent
          Cli.executeStop(Right(runtime), waitMs = 200) shouldBe Left("serve is not running")
        finally running.finish()
      finally
        deleteTree(runtime)
        deleteTree(other)

  "a shell" should "keep a mismatched command off the socket and leave serve running" in :
    prepared: (_, session) =>
      val other: File = JFiles.createTempDirectory("serve-shell").toFile
      File(other, "2010").mkdirs() shouldBe true
      val shellSession: Session = sessionFor(other, PathStyle.Cursor)
      val runtime: File = tempRuntime()
      try
        withServer(session, runtime): _ =>
          val (code, out, err) = shell(
            shellSession,
            Seq("lsd 2010", "pwd", "exit"),
            Some((current, argv) => Cli.remoteResult(runtime, current, argv))
          )
          code shouldBe 0
          out shouldBe "/\n"
          err should include (Serve.archiveOf(session))
          err should include (Serve.archiveOf(shellSession))
          err should include ("run photo serve stop")
          command(runtime, session, List("help")).ok shouldBe true
      finally
        deleteTree(runtime)
        deleteTree(other)

  "two ensures" should "let one bind and the other attach" in :
    prepared: (_, session) =>
      val runtime: File = tempRuntime()
      val threads: ConcurrentLinkedQueue[Thread] = new ConcurrentLinkedQueue
      try
        val gate: CountDownLatch = new CountDownLatch(1)
        val done: CountDownLatch = new CountDownLatch(2)
        val results: ConcurrentLinkedQueue[Either[String, Unit]] = new ConcurrentLinkedQueue
        val listens: AtomicInteger = new AtomicInteger
        def spawn(): Either[String, Unit] =
          listens.incrementAndGet()
          val thread: Thread = new Thread(() =>
            val err: ByteArrayOutputStream = new ByteArrayOutputStream
            Console.withErr(err)(Serve.listen(session, runtime))
            ()
          )
          thread.setDaemon(true)
          threads.add(thread)
          thread.start()
          Right(())
        def worker(): Unit =
          gate.await()
          results.add(Serve.ensure(runtime, () => spawn(), waitMs = 3000))
          done.countDown()
        val first: Thread = new Thread(() => worker())
        val second: Thread = new Thread(() => worker())
        first.setDaemon(true)
        second.setDaemon(true)
        first.start()
        second.start()
        gate.countDown()
        done.await(10, TimeUnit.SECONDS) shouldBe true
        val drained: List[Either[String, Unit]] = drain(results)
        drained should have size 2
        drained.foreach:
          case Right(()) => ()
          case Left(message) => fail(message)
        listens.get should (be >= 1 and be <= 2)
        command(runtime, session, List("lsd", "2010")).ok shouldBe true
        val err: ByteArrayOutputStream = new ByteArrayOutputStream
        val code: Int = Console.withErr(err)(Serve.listen(session, runtime))
        code shouldBe 0
        text(err) shouldBe "already running\n"
        command(runtime, session, List("help")).ok shouldBe true
      finally
        Serve.client(runtime, List("serve", "stop"), Serve.archiveOf(session), stop = true, waitMs = 2000)
        threads.forEach(thread => thread.join(5000))
        deleteTree(runtime)

  it should "report a spawn that never accepts" in :
    val runtime: File = tempRuntime()
    try
      Serve.ensure(runtime, () => Right(()), waitMs = 200) shouldBe Left("serve did not start")
    finally deleteTree(runtime)

  "call" should "return the agent object and still drop it in toResult" in :
    prepared: (archive, session) =>
      writeCatalog(
        archive,
        "album\ta1\t1\t2010/07",
        "item\ta1\tone.jpg\t2010-07-19\tk1\t-"
      )
      val runtime: File = tempRuntime()
      try
        withServer(session, runtime): _ =>
          val value: Json = Json.Obj(Seq(
            "id" -> Json.Str("a1"),
            "error" -> Json.Null,
            "items" -> Json.Arr(Seq(Json.Obj(Seq(
              "filename" -> Json.Str("one.jpg"),
              "date" -> Json.Str("2010-07-19"),
              "key" -> Json.Str("k1"),
              "uploader" -> Json.Null
            )))),
            "unsets" -> Json.Arr(Seq.empty)
          ))
          Serve.call(
            runtime,
            List("agent", "items", "--id", "a1", "2010/07"),
            Serve.archiveOf(session),
            waitMs = 2000
          ) match
            case Right(called) =>
              called.value shouldBe value
              called.response.text shouldBe Some("one.jpg 2010-07-19 -\n")
              Serve.toResult(called.response) shouldBe Result.Ok("one.jpg 2010-07-19 -\n", Json.Null)
            case Left(message) =>
              fail(message)
      finally deleteTree(runtime)

  it should "name both archives, send no request, and still run on the server archive" in :
    prepared: (_, session) =>
      val other: File = JFiles.createTempDirectory("serve-other").toFile
      File(other, "2010").mkdirs() shouldBe true
      val client: Session = sessionFor(other)
      val runtime: File = tempRuntime()
      try
        val bytes: AtomicInteger = new AtomicInteger
        val eof: CountDownLatch = new CountDownLatch(1)
        val quiet: Stall = stallBytes(runtime, Serve.archiveOf(session), bytes, eof)
        try
          val result: Either[String, Serve.Called] = Serve.call(
            runtime,
            List("lsd", "2010"),
            Serve.archiveOf(client),
            waitMs = 2000
          )
          eof.await(5, TimeUnit.SECONDS) shouldBe true
          bytes.get shouldBe 0
          result shouldBe Left(
            s"serve archive is ${Serve.archiveOf(session)}; this archive is ${Serve.archiveOf(client)}; run photo serve stop"
          )
        finally quiet.close()
        withServer(session, runtime): _ =>
          Serve.call(runtime, List("lsd", "2010"), Serve.archiveOf(session), waitMs = 2000) match
            case Right(called) =>
              called.response.ok shouldBe true
              called.response.text shouldBe Some("/2010\n")
            case Left(message) =>
              fail(message)
      finally
        deleteTree(runtime)
        deleteTree(other)

  it should "report serve is not running only after the lock is free" in :
    prepared: (_, session) =>
      val runtime: File = tempRuntime()
      try
        withServer(session, runtime)(_ => ())
        File(runtime, "serve.sock").exists shouldBe false
        val lockFile: File = File(runtime, "serve.lock")
        val channel: FileChannel = FileChannel.open(
          lockFile.toPath,
          StandardOpenOption.READ,
          StandardOpenOption.WRITE
        )
        var closeChannel: Boolean = true
        try
          val lock: FileLock =
            try channel.tryLock()
            catch case _: OverlappingFileLockException =>
              closeChannel = false
              fail("lock still held")
          lock should not be null
          lock.release()
        finally if closeChannel then channel.close()
        Serve.call(runtime, List("lsd", "2010"), Serve.archiveOf(session), waitMs = 200) shouldBe
          Left("serve is not running")
      finally deleteTree(runtime)

  it should "stay busy when the socket is deleted under a live listen" in :
    prepared: (archive, session) =>
      writeCatalog(archive)
      val sentinel: File = File(File(archive, ".gphoto-sync"), "catalog.txt")
      val runtime: File = tempRuntime()
      try
        val running: Running = start(session, runtime)
        try
          val socket: File = File(runtime, "serve.sock")
          JFiles.delete(socket.toPath)
          socket.exists shouldBe false
          val epoch: FileTime = FileTime.fromMillis(0)
          JFiles.setAttribute(sentinel.toPath, "basic:lastAccessTime", epoch)
          val before: FileTime =
            JFiles.getAttribute(sentinel.toPath, "basic:lastAccessTime").asInstanceOf[FileTime]
          val result: Either[String, Serve.Called] = Serve.call(
            runtime,
            List("agent", "catalog"),
            Serve.archiveOf(session),
            waitMs = 200
          )
          val lock: String = File(runtime, "serve.lock").getAbsolutePath
          result shouldBe Left(
            s"a command is in progress; photo serve stop runs only between commands; the JVM holding $lock is that process (fuser $lock)"
          )
          val after: FileTime =
            JFiles.getAttribute(sentinel.toPath, "basic:lastAccessTime").asInstanceOf[FileTime]
          after.toMillis shouldBe before.toMillis
        finally running.crash()
      finally deleteTree(runtime)

  private def drain[A](queue: ConcurrentLinkedQueue[A]): List[A] =
    Iterator.continually(Option(queue.poll())).takeWhile(_.isDefined).flatten.toList

  // skip is private. A whole line fails when anything remains after that same whitespace.
  private def wholeJson(line: String): Either[String, Json] =
    Serve.readJson(line) match
      case Left(message) => Left(message)
      case Right((json, rest)) if rest.forall(isSkip) => Right(json)
      case Right(_) => Left("suffix")

  private def isSkip(ch: Char): Boolean =
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'

  private def lsdParsed: Cli.Parsed = Cli.parse(List("lsd", "2010")) match
    case Cli.Run(parsed) => parsed
    case other => fail(other.toString)

  private def rendered(
    session: Session,
    ok: Boolean,
    text: Option[String] = None,
    error: Option[String] = None,
    value: Option[Json] = None,
    id: Option[String] = None
  ): String =
    Serve.render(id, ok, text, error, value, Serve.archiveOf(session))

  private def usage(session: Session, message: String, id: Option[String] = None): String =
    rendered(session, ok = false, error = Some(s"Photo Archive Tool: $message"), id = id)

  private def lsdOk(session: Session): String =
    rendered(session, ok = true, text = Some("/2010\n"), value = Some(Json.Null))

  private def withId(id: String, line: String): String =
    """{"id":"""" + id + "\"," + line.drop(1)

  private def respond(
    session: Session,
    lines: Seq[String],
    executeCommand: (Session, Cli.Parsed) => (Session, Result) = (current, parsed) =>
      Cli.toOperation(parsed).execute(current)
  ): List[String] =
    val (_, rendered) =
      lines.foldLeft((session, List.empty[String])):
        case ((current, acc), line) =>
          val (step, text) = Cli.serveResponse(current, line, executeCommand)
          step match
            case Cli.Step.Continue(next) =>
              (next, acc :+ text)
            case Cli.Step.Stop =>
              fail("serve stop is one response, not a batch")
    rendered

  private def shell(
    session: Session,
    lines: Seq[String],
    exchange: Option[(Session, List[String]) => Result]
  ): (Int, String, String) =
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    val err: ByteArrayOutputStream = new ByteArrayOutputStream
    val code: Int = Console.withOut(out):
      Console.withErr(err):
        Cli.shell(session, lines.iterator, exchange = exchange)
    (code, text(out), text(err))

  private def text(bytes: ByteArrayOutputStream): String =
    new String(bytes.toByteArray, StandardCharsets.UTF_8)

  private def sessionFor(archive: File, style: PathStyle = PathStyle.Serve): Session =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    val root: Root = Root(Configuration(rootPath, ZoneId.of("America/New_York")))
    Session(root, style, root)

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

  private def withPicture(body: (File, Session) => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("serve").toFile
    try
      jpeg(File(archive, "2010/07/19"), "2412.jpg")
      body(archive, sessionFor(archive))
    finally deleteTree(archive)

  private def jpeg(directory: File, name: String): Unit =
    directory.mkdirs() shouldBe true
    ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB), "jpg", File(directory, name)) shouldBe true

  private def tempRuntime(): File = JFiles.createTempDirectory("serve-rt").toFile

  private def perms(file: File): String =
    PosixFilePermissions.toString(JFiles.getPosixFilePermissions(file.toPath))

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()

  private def connect(runtime: File): SocketChannel =
    val channel: SocketChannel = SocketChannel.open(StandardProtocolFamily.UNIX)
    channel.connect(UnixDomainSocketAddress.of(File(runtime, "serve.sock").toPath))
    channel

  private def writeSocket(channel: SocketChannel, line: String): Unit =
    val buf: ByteBuffer = ByteBuffer.wrap((line + "\n").getBytes(StandardCharsets.UTF_8))
    while buf.hasRemaining do channel.write(buf)

  private def readSocket(channel: SocketChannel): String =
    val raw: ByteArrayOutputStream = new ByteArrayOutputStream
    val chunk: ByteBuffer = ByteBuffer.allocate(1024)
    var done: Boolean = false
    while !done do
      chunk.clear()
      val n: Int = channel.read(chunk)
      if n < 0 then fail("eof")
      chunk.flip()
      while chunk.hasRemaining && !done do
        val b: Int = chunk.get().toInt & 0xff
        if b == '\n' then done = true else raw.write(b)
    raw.toString(StandardCharsets.UTF_8)

  private def command(runtime: File, session: Session, argv: List[String]): Serve.Response =
    Serve.client(runtime, argv, Serve.archiveOf(session), waitMs = 2000) match
      case Serve.Attempt.Done(response) => response
      case other => fail(other.toString)

  private def childLock(lock: File, socket: File): String =
    val process: java.lang.ProcessBuilder = new java.lang.ProcessBuilder(
      System.getProperty("java.home") + "/bin/java",
      "-cp",
      System.getProperty("java.class.path"),
      "org.podval.tools.photo.LockProbe",
      lock.getAbsolutePath,
      socket.getAbsolutePath
    )
    process.redirectErrorStream(true)
    val proc: Process = process.start()
    val finished: Boolean = proc.waitFor(10, TimeUnit.SECONDS)
    val output: String = new String(proc.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    finished shouldBe true
    proc.exitValue shouldBe 0
    output

  private final class Running(
    val errBytes: ByteArrayOutputStream,
    holder: AtomicReference[ServerSocketChannel],
    stopped: CountDownLatch,
    runtime: File,
    session: Session
  ):
    def err: String = text(errBytes)

    def crash(): Unit =
      val server: ServerSocketChannel = holder.get
      server should not be null
      server.close()
      awaitExit()

    def awaitExit(): Unit =
      stopped.await(5, TimeUnit.SECONDS) shouldBe true

    def finish(): Unit =
      if stopped.getCount > 0 then
        Serve.client(runtime, List("serve", "stop"), Serve.archiveOf(session), stop = true, waitMs = 2000)
        awaitExit()

  private def start(session: Session, runtime: File): Running =
    val started: CountDownLatch = new CountDownLatch(1)
    val stopped: CountDownLatch = new CountDownLatch(1)
    val errBytes: ByteArrayOutputStream = new ByteArrayOutputStream
    val holder: AtomicReference[ServerSocketChannel] = new AtomicReference
    val thread: Thread = new Thread(() =>
      try
        Console.withErr(errBytes):
          Serve.listen(session, runtime, server =>
            holder.set(server)
            started.countDown()
          )
      finally
        started.countDown()
        stopped.countDown()
      ()
    )
    thread.setDaemon(true)
    thread.start()
    started.await(5, TimeUnit.SECONDS) shouldBe true
    holder.get should not be null
    Running(errBytes, holder, stopped, runtime, session)

  private def withServer(session: Session, runtime: File)(body: Running => Unit): Unit =
    val running: Running = start(session, runtime)
    try body(running)
    finally running.finish()

  private final class Stall(server: ServerSocketChannel, thread: Thread):
    def close(): Unit =
      server.close()
      thread.join(5000)

  private def stall(runtime: File, delayMs: Long, respond: Boolean, archive: String): Stall =
    JFiles.createDirectories(runtime.toPath)
    val server: ServerSocketChannel = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    server.bind(UnixDomainSocketAddress.of(File(runtime, "serve.sock").toPath))
    val thread: Thread = new Thread(() =>
      var conn: SocketChannel = null
      try
        conn = server.accept()
        if respond then
          writeSocket(conn, Serve.render(None, ok = true, None, None, None, archive))
          readSocket(conn)
        Thread.sleep(delayMs)
        if respond then
          writeSocket(
            conn,
            Serve.render(None, ok = true, Some("late\n"), None, Some(Json.Null), archive)
          )
      catch
        case _: java.io.IOException => ()
        case _: InterruptedException => ()
      finally if conn != null then conn.close()
    )
    thread.setDaemon(true)
    thread.start()
    Stall(server, thread)

  private def stallBytes(runtime: File, archive: String, bytes: AtomicInteger, eof: CountDownLatch): Stall =
    JFiles.createDirectories(runtime.toPath)
    val server: ServerSocketChannel = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    server.bind(UnixDomainSocketAddress.of(File(runtime, "serve.sock").toPath))
    val thread: Thread = new Thread(() =>
      var conn: SocketChannel = null
      try
        conn = server.accept()
        writeSocket(conn, Serve.render(None, ok = true, None, None, None, archive))
        val chunk: ByteBuffer = ByteBuffer.allocate(1024)
        var done: Boolean = false
        while !done do
          chunk.clear()
          val n: Int = conn.read(chunk)
          if n < 0 then done = true
          else bytes.addAndGet(n)
      catch
        case _: java.io.IOException => ()
        case _: InterruptedException => ()
      finally
        if conn != null then conn.close()
        eof.countDown()
    )
    thread.setDaemon(true)
    thread.start()
    Stall(server, thread)

// A second JVM. tryLock in this process cannot see whether the kernel lock survived.
object LockProbe:
  def main(args: Array[String]): Unit =
    val lockFile: File = File(args(0))
    val socketFile: File = File(args(1))
    val channel: FileChannel = FileChannel.open(
      lockFile.toPath,
      StandardOpenOption.READ,
      StandardOpenOption.WRITE
    )
    val acquired: Boolean =
      try
        val lock: FileLock =
          try channel.tryLock()
          catch case _: java.nio.channels.OverlappingFileLockException => null
        if lock == null then false
        else
          JFiles.deleteIfExists(socketFile.toPath)
          lock.release()
          true
      finally channel.close()
    println(if acquired then "acquired" else "held")
