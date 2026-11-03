package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.io.{ByteArrayOutputStream, File}
import java.net.{InetSocketAddress, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import java.time.Duration
import java.util.concurrent.atomic.AtomicReference

class WebTest extends AnyFlatSpec, Matchers:
  @targetName("outcomeCanEqual")
  private given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

  @targetName("operationCanEqual")
  private given [A <: Operation, B <: Operation] => CanEqual[A, B] = CanEqual.derived

  @targetName("rejectedCanEqual")
  private given CanEqual[Web.Rejected, Web.Rejected] = CanEqual.derived

  private val loopback: String = "127.0.0.1"
  private val escape: String = "path escapes the archive"
  private val client: HttpClient =
    HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(2)).build()

  "web" should "parse a loopback port and stay out of the operation until the port is legal" in :
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
    Cli.mcpTools.exists(_.name == "web") shouldBe false

  it should "serve the shell and refuse a second start or a taken port" in :
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
        page.body should include ("<h1>Photo archive</h1>")
        page.body should include ("/app.js")
        val script: HttpResponse[String] = get(port, "/app.js")
        script.statusCode shouldBe 200
        script.headers.firstValue("content-type").orElse("") shouldBe "text/javascript; charset=utf-8"
        script.body should include ("Photo archive")
        script.body should not include ("fetch")
        script.body should not include ("/api")
        val style: HttpResponse[String] = get(port, "/app.css")
        style.statusCode shouldBe 200
        style.headers.firstValue("content-type").orElse("") shouldBe "text/css; charset=utf-8"
        style.body should include ("font-family")
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

  it should "release the lock when the port is taken and reject port 0 before binding" in :
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

  it should "report a bind failure that is not a taken port" in :
    val runtime: File = JFiles.createTempDirectory("web-denied").toFile
    val probe = new java.net.ServerSocket()
    try
      val denied: Option[String] =
        try
          probe.bind(new InetSocketAddress(loopback, 80))
          None
        catch case caught: java.net.BindException =>
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
          else
            failed shouldBe Left(message)
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

  it should "keep another holder of web.lock when this JVM already has it" in :
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

  it should "reject a path that leaves the archive" in :
    val archive: File = JFiles.createTempDirectory("web-archive").toFile
    val outside: File = JFiles.createTempDirectory("web-outside").toFile
    try
      val secret: File = File(outside, "secret.jpg")
      JFiles.writeString(secret.toPath, "nope")
      val day: File = File(archive, "2010/07/04")
      day.mkdirs() shouldBe true
      JFiles.writeString(File(day, "1000.jpg").toPath, "yes")
      JFiles.createSymbolicLink(File(day, "2412.jpg").toPath, secret.toPath)
      JFiles.createSymbolicLink(File(day, "gone.jpg").toPath, File(outside, "missing.jpg").toPath)
      JFiles.createSymbolicLink(File(archive, "2011").toPath, outside.toPath)
      val loop: File = File(archive, "loop")
      JFiles.createSymbolicLink(loop.toPath, loop.toPath)
      val linkA: File = File(archive, "link-a")
      val linkB: File = File(archive, "link-b")
      JFiles.createSymbolicLink(linkA.toPath, linkB.toPath)
      JFiles.createSymbolicLink(linkB.toPath, linkA.toPath)

      rejected(archive, "../x")
      rejected(archive, "2010/07/04/../../x")
      rejected(archive, "%2e%2e")
      rejected(archive, "%2E%2E")
      rejected(archive, "/etc/passwd")
      rejected(archive, "%2Fetc%2Fpasswd")
      rejected(archive, "2010\\07")
      rejected(archive, "2010/./07")
      rejected(archive, "2010//07")
      rejected(archive, "2010/07/04/a\u0000b")
      rejected(archive, "2010/07/04/2412.jpg")
      rejected(archive, "2011")
      rejected(archive, "2011/secret.jpg")
      rejected(archive, "2011/missing.jpg")
      rejected(archive, "2010/07/04/1000.jpg/extra")
      rejected(archive, "loop")
      rejected(archive, "loop/child")
      rejected(archive, "link-a/child")
      Web.reject(archive, "2010/07/04/gone.jpg") shouldBe Left(Web.Rejected(404, "no such picture"))
      Web.reject(archive, "2010/07/04/9999.jpg") shouldBe Left(Web.Rejected(404, "no such picture"))
      Web.reject(archive, "%252e%252e") shouldBe Left(Web.Rejected(404, "no such picture"))
      Web.reject(archive, "2010/07/04/1000.jpg") match
        case Right(path) =>
          Web.contained(archive.toPath.toRealPath(), path) shouldBe true
        case Left(other) =>
          fail(other.toString)

      val root: java.nio.file.Path = java.nio.file.Path.of("/home/dub/Pictures")
      Web.contained(root, java.nio.file.Path.of("/home/dub/Pictures-other")) shouldBe false
      Web.contained(root, java.nio.file.Path.of("/home/dub/Pictures/2010")) shouldBe true
      Web.contained(root, root) shouldBe true
    finally
      deleteTree(archive)
      deleteTree(outside)

  private def rejected(archive: File, raw: String): Unit =
    Web.reject(archive, raw) shouldBe Left(Web.Rejected(400, escape))

  private def running(runtime: File, port: Int)(body: => Unit): (String, String, Either[String, Unit]) =
    val out = new ByteArrayOutputStream
    val err = new ByteArrayOutputStream
    val result = new AtomicReference[Either[String, Unit]](null)
    val thread = new Thread(() =>
      Console.withOut(out):
        Console.withErr(err):
          result.set(Web.start(runtime, port))
    , "web-test")
    thread.setDaemon(true)
    thread.start()
    try
      waitFor(out, s"http://$loopback:$port/")
      body
    finally
      Web.stop()
      thread.join(5000)
    thread.isAlive shouldBe false
    (text(out), text(err), Option(result.get).getOrElse(Left("no result")))

  private def waitFor(out: ByteArrayOutputStream, needle: String): Unit =
    val deadline: Long = System.nanoTime() + 5000000000L
    def loop(): Unit =
      if text(out).contains(needle) then ()
      else if System.nanoTime() >= deadline then fail(s"not bound: ${text(out)}")
      else
        Thread.sleep(20)
        loop()
    loop()

  private def get(port: Int, path: String): HttpResponse[String] = send(port, path, "GET")

  private def send(port: Int, path: String, method: String): HttpResponse[String] =
    val request: HttpRequest = HttpRequest.newBuilder(URI.create(s"http://$loopback:$port$path"))
      .timeout(Duration.ofSeconds(2))
      .method(method, HttpRequest.BodyPublishers.noBody())
      .build()
    client.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))

  private def freePort(): Int =
    val socket = new java.net.ServerSocket(0)
    try socket.getLocalPort
    finally socket.close()

  private def perms(file: File): String =
    PosixFilePermissions.toString(JFiles.getPosixFilePermissions(file.toPath))

  private def text(bytes: ByteArrayOutputStream): String =
    new String(bytes.toByteArray, StandardCharsets.UTF_8)

  private def deleteTree(file: File): Unit =
    if JFiles.isSymbolicLink(file.toPath) then file.delete()
    else
      if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
      file.delete()
