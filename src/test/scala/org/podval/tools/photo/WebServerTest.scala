package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import scala.annotation.targetName
import java.io.{ByteArrayOutputStream, File}
import java.net.InetSocketAddress
import java.net.http.HttpResponse
import java.nio.channels.FileChannel
import java.nio.file.{Files as JFiles, StandardOpenOption}

class WebServerTest extends AnyFlatSpec, WebTestSupport:
  @targetName("outcomeCanEqual")
  private given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

  @targetName("operationCanEqual")
  private given [A <: Operation, B <: Operation] => CanEqual[A, B] = CanEqual.derived

  "web" should "parse a loopback port and stay out of the operation until the port is legal" in:
    Cli.parse(List("web")) shouldBe Cli.Run(Cli.WebArgs(Web.defaultPort))
    Cli.parse(List("web", "--port", "8766")) shouldBe Cli.Run(Cli.WebArgs(8766))
    Cli.parse(List("web", "--port=65535")) shouldBe Cli.Run(Cli.WebArgs(65535))
    Cli.parse(List("web", "--port", "0")) shouldBe Cli.Usage("port must be 1 to 65535")
    Cli.parse(List("web", "--port", "65536")) shouldBe Cli.Usage("port must be 1 to 65535")
    Cli.parse(List("web", "--port", "nope")) shouldBe Cli.Usage("port must be 1 to 65535")
    Cli.parse(List("web", "--port", "+8765")) shouldBe Cli.Usage("port must be 1 to 65535")
    Cli.parse(List("web", "--port", "0000000000008765")) shouldBe Cli.Run(Cli.WebArgs(8765))
    Cli.parse(List("web", "extra")) shouldBe Cli.Usage("extra argument 'extra'")
    Cli.parse(List("web", "--port", "8766", "--port", "8767")) shouldBe Cli.Usage("repeated option '--port'")
    Cli.toOperation(Cli.WebArgs(8766)) shouldBe Web(8766)
    Mcp.mcpTools.exists(_.name == "web") shouldBe false

  it should "serve the shell and refuse a second start or a taken port" in:
    val runtime: File = JFiles.createTempDirectory("web-rt").toFile
    val port: Int = freePort()
    try
      val (out, err, ended) = running(runtime, port):
        assert(Web.handlerRuntime.exists(_ eq runtime))
        File(runtime, "serve.lock").exists shouldBe false
        perms(runtime) shouldBe "rwx------"
        perms(File(runtime, "web.lock")) shouldBe "rw-------"
        val page: HttpResponse[String] = get(port, "/")
        page.statusCode shouldBe 200
        page.headers.firstValue("content-type").orElse("") shouldBe "text/html; charset=utf-8"
        page.headers.firstValue("access-control-allow-origin").isEmpty shouldBe true
        page.body should include("<h1>Photo archive</h1>")
        page.body should include("/app.js")
        val script: HttpResponse[String] = get(port, "/app.js")
        script.statusCode shouldBe 200
        script.headers.firstValue("content-type").orElse("") shouldBe "text/javascript; charset=utf-8"
        script.body should include("/api/tree")
        script.body should include("/api/day")
        script.body should include("/api/album")
        script.body should include("/api/thumb")
        script.body should include("/api/file")
        script.body should include("/api/toggle")
        script.body should include("/api/albums")
        script.body should include("event.key === \"x\"")
        script.body should include("\"desired\"")
        script.body should include("\"observed\"")
        val style: HttpResponse[String] = get(port, "/app.css")
        style.statusCode shouldBe 200
        style.headers.firstValue("content-type").orElse("") shouldBe "text/css; charset=utf-8"
        style.body should include("font-family")
        val missing: HttpResponse[String] = get(port, "/missing")
        missing.statusCode shouldBe 404
        missing.body shouldBe """{"error":"not found"}"""
        val options: HttpResponse[String] = send(port, "/", "OPTIONS")
        options.statusCode shouldBe 405
        options.body shouldBe """{"error":"method not allowed"}"""
        options.headers.firstValue("access-control-allow-origin").isEmpty shouldBe true
        Web.start(runtime, freePort()) shouldBe Left("photo web is already running")
      out shouldBe s"http://$loopback:$port/\n"
      err shouldBe s"bound $loopback:$port\n"
      ended shouldBe Right(())
      assert(Web.handlerRuntime.isEmpty)
    finally deleteTree(runtime)

  it should "release the lock when the port is taken and reject port 0 before binding" in:
    val runtime: File = JFiles.createTempDirectory("web-taken").toFile
    val taken: Int = freePort()
    val socket = new java.net.ServerSocket()
    try
      Web.start(runtime, 0) shouldBe Left("port must be 1 to 65535")
      File(runtime, "web.lock").exists shouldBe false
      socket.setReuseAddress(true)
      socket.bind(new InetSocketAddress(loopback, taken))
      val out = new ByteArrayOutputStream
      val err = new ByteArrayOutputStream
      val failed: Either[String, Unit] = Console.withOut(out):
        Console.withErr(err):
          Web.start(runtime, taken)
      failed shouldBe Left(s"address already in use: $loopback:$taken")
      text(out) shouldBe ""
      text(err) shouldBe ""
      assert(Web.handlerRuntime.isEmpty)
      socket.close()
      val port: Int = freePort()
      val (_, _, ended) = running(runtime, port)(())
      ended shouldBe Right(())
    finally
      socket.close()
      deleteTree(runtime)

  it should "report a bind failure that is not a taken port" in:
    val runtime: File = JFiles.createTempDirectory("web-denied").toFile
    val probe = new java.net.ServerSocket()
    try
      val denied: Option[String] =
        try
          probe.bind(new InetSocketAddress(loopback, 80))
          None
        catch
          case caught: java.net.BindException =>
            Some(Option(caught.getMessage).getOrElse(""))
      denied match
        case Some(message) =>
          val out = new ByteArrayOutputStream
          val err = new ByteArrayOutputStream
          val failed: Either[String, Unit] = Console.withOut(out):
            Console.withErr(err):
              Web.start(runtime, 80)
          if message.contains("Address already in use") then
            failed shouldBe Left(s"address already in use: $loopback:80")
          else failed shouldBe Left(message)
          text(out) shouldBe ""
          text(err) shouldBe ""
          assert(Web.handlerRuntime.isEmpty)
          val port: Int = freePort()
          val (_, _, ended) = running(runtime, port)(())
          ended shouldBe Right(())
        case None =>
          succeed
    finally
      probe.close()
      deleteTree(runtime)

  it should "keep another holder of web.lock when this JVM already has it" in:
    val runtime: File = JFiles.createTempDirectory("web-overlap").toFile
    val port: Int = freePort()
    Serve.prepare(runtime)
    val lockFile: File = File(runtime, "web.lock")
    val channel: FileChannel = FileChannel.open(
      lockFile.toPath,
      StandardOpenOption.CREATE,
      StandardOpenOption.READ,
      StandardOpenOption.WRITE
    )
    val lock = channel.tryLock()
    try
      assert(lock != null)
      Web.start(runtime, port) shouldBe Left("photo web is already running")
      lock.isValid shouldBe true
      File(runtime, "serve.lock").exists shouldBe false
    finally
      if lock != null && lock.isValid then lock.release()
      channel.close()
      deleteTree(runtime)
