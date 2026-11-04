package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import javax.imageio.ImageIO
import java.net.{InetSocketAddress, URI, URLEncoder}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import java.time.{Duration, Instant, ZoneId}
import java.util.concurrent.atomic.AtomicReference

class WebTest extends AnyFlatSpec, Matchers:
  @targetName("outcomeCanEqual")
  private given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

  @targetName("operationCanEqual")
  private given [A <: Operation, B <: Operation] => CanEqual[A, B] = CanEqual.derived

  @targetName("rejectedCanEqual")
  private given CanEqual[Web.Rejected, Web.Rejected] = CanEqual.derived

  // strictEquality does not relate a sealed parent to its cases.
  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

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

  it should "disagree only when observed is in or out and desired differs" in :
    Web.disagree(desired = true, "in") shouldBe false
    Web.disagree(desired = false, "in") shouldBe true
    Web.disagree(desired = true, "out") shouldBe true
    Web.disagree(desired = false, "out") shouldBe false
    Web.disagree(desired = true, "unknown") shouldBe false
    Web.disagree(desired = false, "unknown") shouldBe false

  it should "serve the date tree and one day without a catalog id" in :
    val archive: File = JFiles.createTempDirectory("web-tree").toFile
    val runtime: File = JFiles.createTempDirectory("web-tree-rt").toFile
    val port: Int = freePort()
    try
      touch(File(archive, ".upload"), "inbox.jpg")
      touch(File(archive, "2010/07/04"), "1000.cr2")
      touch(File(archive, "2010/07/04"), "1001.cr2")
      touch(File(archive, "2010/07/04"), "1001.jpg")
      touch(File(archive, "2010/07/04"), "1002.gif")
      touch(File(archive, "2010/07/04"), "1003.png")
      touch(File(archive, "2010/07/04"), "1004.webp")
      touch(File(archive, "2010/07/04"), "1005.mov")
      touch(File(archive, "2010/07/04"), "1006.m4v")
      touch(File(archive, "2010/07/04"), "1007.heic")
      File(archive, "2010/07/05").mkdirs() shouldBe true
      File(archive, "2010/08").mkdirs() shouldBe true
      File(archive, "2011").mkdirs() shouldBe true
      val portraits: String =
        """external = true
          |include = [
          |  '2010/07/04 1001',
          |  '2010/07/04 1005',
          |]
          |""".stripMargin
      val cape: String =
        """external = true
          |include = [
          |  'all',
          |]
          |""".stripMargin
      writeText(File(archive, "Portraits.toml"), portraits)
      writeText(File(archive, "Unlisted.toml"), "external = true\n")
      writeText(File(archive, "2010/07/album.toml"), "include = [\n  'all',\n]\n")
      writeText(File(archive, "2010/07/Zoo.toml"), "")
      writeText(File(archive, "2010/07/04/to-05 Cape Cod.toml"), cape)
      val catalog: File = writeCatalog(archive, "album\tAF1QipWebFixture\t0\tUnused")
      val before: String = readText(File(archive, "Portraits.toml"))
      val (_, _, ended) = running(runtime, port):
        Web.useSession(sessionFor(archive))
        val root = json(get(port, "/api/tree"))
        root shouldBe Json.Obj(Seq(
          "at" -> Json.Str(""),
          "kind" -> Json.Str("years"),
          "entries" -> Json.Arr(Seq(
            entry("2010", "2010", None),
            entry("2011", "2011", None)
          )),
          "albums" -> Json.Arr(Seq.empty),
          "curated" -> Json.Arr(Seq(
            albumLink("Portraits", "Portraits.toml"),
            albumLink("Unlisted", "Unlisted.toml")
          )),
          "catalog" -> catalogValue(catalog, present = true, readable = true)
        ))
        textOf(get(port, "/api/tree")) should not include ("AF1Qip")
        textOf(get(port, "/api/tree")) should not include (".upload")
        Web.sessionNow.get.catalogCache shouldBe None
        json(get(port, "/api/tree?at=")) shouldBe root
        val year = json(getq(port, "/api/tree", "at", "2010"))
        year shouldBe Json.Obj(Seq(
          "at" -> Json.Str("2010"),
          "kind" -> Json.Str("months"),
          "entries" -> Json.Arr(Seq(
            entry("07", "2010/07", None),
            entry("08", "2010/08", None)
          )),
          "albums" -> Json.Arr(Seq.empty),
          "curated" -> Json.Arr(Seq.empty),
          "catalog" -> catalogValue(catalog, present = true, readable = true)
        ))
        val month = json(getq(port, "/api/tree", "at", "2010/07"))
        field(month, "kind") shouldBe Json.Str("days")
        field(month, "entries") shouldBe Json.Arr(Seq(
          entry("04", "2010/07/04", Some(8)),
          entry("05", "2010/07/05", Some(0))
        ))
        field(month, "albums") shouldBe Json.Arr(Seq(
          albumLink("2010/07", "2010/07/album.toml"),
          albumLink("2010/07/Zoo", "2010/07/Zoo.toml")
        ))
        field(month, "curated") shouldBe Json.Arr(Seq.empty)
        error(getq(port, "/api/tree", "at", "2010/07/04"), 400, "not a branch")
        error(getq(port, "/api/tree", "at", ".."), 400, escape)
        error(get(port, "/api/tree?at=%2e%2e"), 400, escape)
        error(get(port, "/api/tree?at=%252e%252e"), 400, "not a branch")
        error(get(port, "/api/day"), 400, "not a day")
        error(getq(port, "/api/day", "day", "2010/07/32"), 400, "not a day")
        error(getq(port, "/api/day", "day", "2010/07/06"), 404, "no such day")
        error(send(port, "/api/tree", "OPTIONS"), 405, "method not allowed")
        error(send(port, "/api/day", "POST"), 405, "method not allowed")
        val dayResponse: HttpResponse[String] = getq(port, "/api/day", "day", "2010/07/04")
        dayResponse.statusCode shouldBe 200
        dayResponse.headers.firstValue("content-type").orElse("") shouldBe "application/json; charset=utf-8"
        dayResponse.headers.firstValue("access-control-allow-origin").isEmpty shouldBe true
        val day = json(dayResponse)
        dayResponse.body should not include ("AF1Qip")
        field(day, "day") shouldBe Json.Str("2010/07/04")
        field(day, "curated") shouldBe Json.Arr(Seq(Json.Str("Portraits"), Json.Str("Unlisted")))
        field(day, "albums") shouldBe Json.Arr(Seq(
          albumLink("2010/07/04-05/Cape Cod", "2010/07/04/to-05 Cape Cod.toml")
        ))
        field(day, "catalog") shouldBe catalogValue(catalog, present = true, readable = true)
        val stems: Seq[Json] = arr(field(day, "stems"))
        stems.map(stem => str(field(stem, "stem"))) shouldBe Seq(
          "1000", "1001", "1002", "1003", "1004", "1005", "1006", "1007"
        )
        stemAt(stems, "1000") shouldBe stemJson("1000", Seq("cr2"), "preview", uploadable = false, Seq.empty)
        stemAt(stems, "1001") shouldBe stemJson("1001", Seq("cr2", "jpg"), "jpeg", uploadable = true, Seq("Portraits"))
        stemAt(stems, "1002") shouldBe stemJson("1002", Seq("gif"), "gif", uploadable = false, Seq.empty)
        stemAt(stems, "1003") shouldBe stemJson("1003", Seq("png"), "png", uploadable = false, Seq.empty)
        stemAt(stems, "1004") shouldBe stemJson("1004", Seq("webp"), "preview", uploadable = false, Seq.empty)
        stemAt(stems, "1005") shouldBe stemJson("1005", Seq("mov"), "video", uploadable = true, Seq("Portraits"))
        stemAt(stems, "1006") shouldBe stemJson("1006", Seq("m4v"), "video", uploadable = false, Seq.empty)
        stemAt(stems, "1007") shouldBe stemJson("1007", Seq("heic"), "preview", uploadable = false, Seq.empty)
        Web.sessionNow.get.catalogCache.isDefined shouldBe true
        Web.sessionNow.get.cached(File(archive, "2010/07/04")) shouldBe None
        val capeResponse: HttpResponse[String] = getq(port, "/api/album", "title", "2010/07/04-05/Cape Cod")
        capeResponse.statusCode shouldBe 200
        capeResponse.body should not include ("AF1Qip")
        capeResponse.body should not include ("no such day")
        val capeJson: Json = json(capeResponse)
        field(capeJson, "title") shouldBe Json.Str("2010/07/04-05/Cape Cod")
        field(capeJson, "span") shouldBe Json.Str("2010/07/04-05")
        field(capeJson, "name") shouldBe Json.Str("Cape Cod")
        field(capeJson, "curated") shouldBe Json.Bool(false)
        field(capeJson, "include") shouldBe Json.Arr(Seq(Json.Str("all")))
        field(capeJson, "membership") shouldBe membership(joined = false, ambiguous = false, "unknown", Some("unjoined"))
        arr(field(capeJson, "tiles")).map(tile => str(field(tile, "stem"))) shouldBe Seq(
          "1000", "1001", "1002", "1003", "1004", "1005", "1006", "1007"
        )
        arr(field(capeJson, "tiles")).foreach: tile =>
          field(tile, "desired") shouldBe Json.Bool(true)
          field(tile, "observed") shouldBe Json.Str("unknown")
        field(capeJson, "unmatched") shouldBe Json.Arr(Seq.empty)
        Web.sessionNow.get.cached(File(archive, "2010/07/04")) shouldBe None
        readText(File(archive, "Portraits.toml")) shouldBe before
        readText(catalog) shouldBe "# gphoto-catalog 2\nalbum\tAF1QipWebFixture\t0\tUnused\n"
      ended shouldBe Right(())
    finally
      deleteTree(archive)
      deleteTree(runtime)

  it should "report a stray file without its path" in :
    val archive: File = JFiles.createTempDirectory("web-stray").toFile
    val runtime: File = JFiles.createTempDirectory("web-stray-rt").toFile
    val port: Int = freePort()
    try
      writeText(File(archive, "NOTES.txt"), "nope")
      val (_, _, ended) = running(runtime, port):
        Web.useSession(sessionFor(archive))
        val response: HttpResponse[String] = get(port, "/api/tree")
        error(response, 500, "stray file")
        response.body should not include ("NOTES.txt")
      ended shouldBe Right(())
    finally
      deleteTree(archive)
      deleteTree(runtime)

  it should "serve observed membership without leaking a catalog id" in :
    val archive: File = JFiles.createTempDirectory("web-album").toFile
    val runtime: File = JFiles.createTempDirectory("web-album-rt").toFile
    val port: Int = freePort()
    val albumFile: File = File(archive, "2010/07/04/to-05 Trip.toml")
    try
      touch(File(archive, "2010/07/04"), "2412.jpg")
      touch(File(archive, "2010/07/04"), "1000.jpg")
      touch(File(archive, "2010/07/05"), "1000.jpg")
      val albumText: String =
        """external = true
          |google-title = "Trip"
          |owner = "Ada"
          |include = [
          |  '2010/07/04 2412',
          |]
          |""".stripMargin
      writeText(albumFile, albumText)
      writeText(File(archive, "2010/07/19/album.toml"), "external = true\n")
      val (_, _, ended) = running(runtime, port):
        Web.useSession(sessionFor(archive))
        val missing: Json = album(port, "2010/07/04-05/Trip")
        field(missing, "membership") shouldBe membership(joined = false, ambiguous = false, "unknown", Some("missing"))
        arr(field(missing, "tiles")).foreach(tile => field(tile, "observed") shouldBe Json.Str("unknown"))
        field(missing, "unmatched") shouldBe Json.Arr(Seq.empty)
        field(missing, "owner") shouldBe Json.Str("Ada")
        field(missing, "external") shouldBe Json.Bool(true)
        field(missing, "path") shouldBe Json.Str("2010/07/04/to-05 Trip.toml")
        field(missing, "include") shouldBe Json.Arr(Seq(Json.Str("2010/07/04 2412")))
        writeText(catalogFile(archive), "not a header\nAF1QipLeak\n")
        val unreadable: HttpResponse[String] = getq(port, "/api/album", "title", "2010/07/04-05/Trip")
        unreadable.body should not include ("AF1Qip")
        unreadable.body should not include ("catalog is not version 2")
        field(json(unreadable), "membership") shouldBe
          membership(joined = false, ambiguous = false, "unknown", Some("unreadable"))
        field(json(unreadable), "catalog") shouldBe catalogValue(catalogFile(archive), present = true, readable = false)
        writeCatalog(archive, "album\tAF1QipWebFixture\t0\t2010/07/04-05/Trip")
        val unjoined: HttpResponse[String] = getq(port, "/api/album", "title", "2010/07/04-05/Trip")
        unjoined.body should not include ("AF1Qip")
        field(json(unjoined), "membership") shouldBe
          membership(joined = false, ambiguous = false, "unknown", Some("unjoined"))
        writeCatalog(
          archive,
          "album\tAF1QipOne\t0\tTrip",
          "album\tAF1QipTwo\t0\tTrip"
        )
        field(album(port, "2010/07/04-05/Trip"), "membership") shouldBe
          membership(joined = false, ambiguous = true, "unknown", Some("ambiguous"))
        writeCatalog(
          archive,
          "album\tAF1QipWebFixture\t3\tTrip",
          "item\tAF1QipWebFixture\t2412.jpg\t2010-07-04\tAF1QipHit\t-",
          "item\tAF1QipWebFixture\tIMG_9999.JPG\t2010-07-04\tAF1QipMiss\t-",
          "item\tAF1QipWebFixture\tLOST.JPG\t-\tAF1QipNone\t-"
        )
        val known: HttpResponse[String] = getq(port, "/api/album", "title", "2010/07/04-05/Trip")
        known.body should not include ("AF1Qip")
        val knownJson: Json = json(known)
        field(knownJson, "membership") shouldBe membership(joined = true, ambiguous = false, "known", None)
        tilesOf(knownJson) shouldBe Seq(
          ("2010/07/04", "1000", false, "out"),
          ("2010/07/04", "2412", true, "in"),
          ("2010/07/05", "1000", false, "out")
        )
        field(knownJson, "unmatched") shouldBe Json.Arr(Seq(
          Json.Obj(Seq("filename" -> Json.Str("IMG_9999.JPG"), "day" -> Json.Str("2010/07/04"))),
          Json.Obj(Seq("filename" -> Json.Str("LOST.JPG"), "day" -> Json.Null))
        ))
        Web.sessionNow.get.cached(File(archive, "2010/07/04")).isDefined shouldBe true
        writeCatalog(
          archive,
          "album\tAF1QipWebFixture\t1\tTrip",
          "item\tAF1QipWebFixture\t2412.jpg\t2010-07-03\tAF1QipHit\t-"
        )
        val mismatch: Json = album(port, "2010/07/04-05/Trip")
        textOf(getq(port, "/api/album", "title", "2010/07/04-05/Trip")) should not include ("AF1Qip")
        field(mismatch, "unmatched") shouldBe Json.Arr(Seq.empty)
        tilesOf(mismatch).find(_._2 == "2412").get._4 shouldBe "out"
        writeCatalog(
          archive,
          "album\tAF1QipWebFixture\t4\tTrip",
          "error\tAF1QipWebFixture\t1\t4\t0",
          "item\tAF1QipWebFixture\t2412.jpg\t2010-07-04\tAF1QipHit\t-"
        )
        val errored: Json = album(port, "2010/07/04-05/Trip")
        field(errored, "membership") shouldBe membership(joined = true, ambiguous = false, "unknown", Some("error"))
        field(errored, "unmatched") shouldBe Json.Arr(Seq.empty)
        tilesOf(errored).foreach(tile => tile._4 shouldBe "unknown")
        writeCatalog(archive, "album\tAF1QipWebFixture\t2\tTrip", "auto-add\tAF1QipWebFixture")
        field(album(port, "2010/07/04-05/Trip"), "membership") shouldBe
          membership(joined = true, ambiguous = false, "unknown", Some("auto-add"))
        writeCatalog(
          archive,
          "album\tAF1QipOther\t0\tTrip",
          "album\tAF1QipWebFixture\t1\tOther Name",
          "item\tAF1QipWebFixture\t2412.jpg\t2010-07-04\tAF1QipHit\t-"
        )
        writeLinks(archive, "AF1QipWebFixture\t2010/07/04/to-05 Trip.toml")
        val linked: HttpResponse[String] = getq(port, "/api/album", "title", "2010/07/04-05/Trip")
        linked.body should not include ("AF1Qip")
        field(json(linked), "membership") shouldBe membership(joined = true, ambiguous = false, "known", None)
        tilesOf(json(linked)).find(_._2 == "2412").get._4 shouldBe "in"
        writeLinks(archive, "AF1QipMissing\t2010/07/04/to-05 Trip.toml")
        field(album(port, "2010/07/04-05/Trip"), "membership") shouldBe
          membership(joined = false, ambiguous = false, "unknown", Some("link-absent"))
        field(album(port, "2010/07/19-19"), "title") shouldBe Json.Str("2010/07/19")
        error(getq(port, "/api/album", "title", "2010/07/04-03"), 400, "not an album title")
        error(get(port, "/api/album"), 400, "not an album title")
        error(getq(port, "/api/album", "title", "Missing"), 404, "no such album: Missing")
        readText(albumFile) shouldBe albumText
      ended shouldBe Right(())
    finally
      deleteTree(archive)
      deleteTree(runtime)

  it should "skip a missing day in a span and reject a directory symlink that leaves the archive" in :
    val archive: File = JFiles.createTempDirectory("web-gap").toFile
    val outside: File = JFiles.createTempDirectory("web-gap-out").toFile
    val runtime: File = JFiles.createTempDirectory("web-gap-rt").toFile
    val port: Int = freePort()
    try
      touch(File(archive, "2010/07/04"), "1000.jpg")
      touch(File(archive, "2010/07/04"), "1001.jpg")
      touch(File(archive, "2010/07/05"), "1000.jpg")
      touch(outside, "9998.jpg")
      val portraits: String =
        """include = [
          |  '2010/07/04-05 1000',
          |]
          |""".stripMargin
      writeText(File(archive, "Portraits.toml"), portraits)
      writeText(
        File(archive, "2010/07/04/to-05 Gap.toml"),
        "include = [\n  'all',\n]\n"
      )
      val (_, _, ended) = running(runtime, port):
        Web.useSession(sessionFor(archive))
        val both: Json = album(port, "Portraits")
        field(both, "curated") shouldBe Json.Bool(true)
        field(both, "span") shouldBe Json.Null
        field(both, "name") shouldBe Json.Str("Portraits")
        tilesOf(both).map(tile => (tile._1, tile._2)) shouldBe Seq(
          ("2010/07/04", "1000"),
          ("2010/07/05", "1000")
        )
        Web.sessionNow.get.cached(File(archive, "2010/07/04")) shouldBe None
        deleteTree(File(archive, "2010/07/05"))
        val gap: HttpResponse[String] = getq(port, "/api/album", "title", "Portraits")
        gap.statusCode shouldBe 200
        gap.body should not include ("no such day")
        tilesOf(json(gap)).map(tile => (tile._1, tile._2)) shouldBe Seq(("2010/07/04", "1000"))
        val dated: HttpResponse[String] = getq(port, "/api/album", "title", "2010/07/04-05/Gap")
        dated.statusCode shouldBe 200
        dated.body should not include ("no such day")
        tilesOf(json(dated)).map(tile => (tile._1, tile._2)) shouldBe Seq(
          ("2010/07/04", "1000"),
          ("2010/07/04", "1001")
        )
        JFiles.createSymbolicLink(File(archive, "2010/07/05").toPath, outside.toPath)
        val escaped: HttpResponse[String] = getq(port, "/api/album", "title", "Portraits")
        error(escaped, 400, escape)
        escaped.body should not include ("9998")
        val datedEscaped: HttpResponse[String] = getq(port, "/api/album", "title", "2010/07/04-05/Gap")
        error(datedEscaped, 400, escape)
        datedEscaped.body should not include ("9998")
        val month: HttpResponse[String] = getq(port, "/api/tree", "at", "2010/07")
        error(month, 400, escape)
        month.body should not include ("9998")
        val day: HttpResponse[String] = getq(port, "/api/day", "day", "2010/07/05")
        error(day, 400, escape)
        day.body should not include ("9998")
      ended shouldBe Right(())
      val linked: File = JFiles.createTempDirectory("web-link-arch").toFile
      try
        JFiles.createSymbolicLink(File(linked, "2010").toPath, outside.toPath)
        File(outside, "07/04").mkdirs() shouldBe true
        touch(File(outside, "07/04"), "secret-stem.jpg")
        val (_, _, linkEnded) = running(runtime, port):
          Web.useSession(sessionFor(linked))
          val listed: HttpResponse[String] = get(port, "/api/tree")
          listed.statusCode shouldBe 200
          listed.body should include ("\"2010\"")
          listed.body should not include ("secret-stem")
          val opened: HttpResponse[String] = getq(port, "/api/tree", "at", "2010")
          error(opened, 400, escape)
          opened.body should not include ("secret-stem")
          val day: HttpResponse[String] = getq(port, "/api/day", "day", "2010/07/04")
          error(day, 400, escape)
          day.body should not include ("secret-stem")
        linkEnded shouldBe Right(())
      finally deleteTree(linked)
    finally
      deleteTree(archive)
      deleteTree(outside)
      deleteTree(runtime)

  it should "serve a scaled thumbnail, a placeholder, and the original file bytes" in :
    val archive: File = JFiles.createTempDirectory("web-thumb-archive").toFile
    val outside: File = JFiles.createTempDirectory("web-thumb-outside").toFile
    val runtime: File = JFiles.createTempDirectory("web-thumb-rt").toFile
    val cacheHome: File = JFiles.createTempDirectory("web-thumb-home").toFile
    val port: Int = freePort()
    val savedHome: String = System.getProperty("user.home")
    val day: File = File(archive, "2010/07/04")
    val jpeg: File = File(day, "1001.jpg")
    val raw: File = File(day, "1000.cr2")
    val video: File = File(day, "1005.mov")
    val secret: File = File(outside, "secret-stem.jpg")
    val placeholder: Array[Byte] =
      val in = getClass.getClassLoader.getResourceAsStream("org/podval/tools/photo/web/placeholder.jpg")
      try in.readAllBytes() finally in.close()
    def filesIn(dir: File): Seq[File] =
      if !dir.exists then Seq.empty
      else
        Option(dir.listFiles).toSeq.flatten.flatMap: child =>
          if JFiles.isSymbolicLink(child.toPath) then Seq.empty
          else if child.isDirectory then filesIn(child)
          else if child.isFile && !child.getName.startsWith(".") then Seq(child)
          else Seq.empty
    def kindOf(stem: String): String =
      val body = json(getq(port, "/api/day", "day", "2010/07/04"))
      str(field(stemAt(arr(field(body, "stems")), stem), "kind"))
    try
      (placeholder(0) & 0xff) shouldBe 0xff
      (placeholder(1) & 0xff) shouldBe 0xd8
      writeJpeg(jpeg, 320, 240)
      val original: Array[Byte] = JFiles.readAllBytes(jpeg.toPath)
      val originalMtime: Long = jpeg.lastModified
      JFiles.writeString(raw.toPath, "not a raw preview")
      JFiles.writeString(video.toPath, "not a video")
      val rawBytes: Array[Byte] = JFiles.readAllBytes(raw.toPath)
      val videoBytes: Array[Byte] = JFiles.readAllBytes(video.toPath)
      JFiles.writeString(secret.toPath, "OUTSIDE-SECRET-BYTES")
      JFiles.createSymbolicLink(File(day, "2412.jpg").toPath, secret.toPath)
      JFiles.createSymbolicLink(File(day, "1009.jpg").toPath, File(day, "missing-target.jpg").toPath)
      JFiles.createSymbolicLink(File(archive, "2011").toPath, outside.toPath)
      writeText(File(archive, ".gphoto-sync/catalog.txt"), "CATALOG-SECRET\n")
      // A day listing reads album toml. The bytes still must not come back from /api/file.
      writeText(File(day, "album.toml"), "google-title = \"TOML-SECRET\"\n")
      System.setProperty("user.home", cacheHome.getAbsolutePath)
      val cacheRoot: File = File(cacheHome, ".cache/org.podval.tools.photo/thumbs")
      val (_, err, ended) = running(runtime, port):
        Web.useSession(sessionAt(archive))
        error(get(port, "/api/thumb"), 400, "not a picture path")
        error(get(port, "/api/thumb?path=%2e%2e"), 400, escape)
        error(get(port, "/api/file?path=%2e%2e"), 400, escape)
        error(get(port, "/api/thumb?path=%252e%252e"), 400, "not a picture path")
        val catalog: HttpResponse[String] = getq(port, "/api/file", "path", ".gphoto-sync/catalog.txt")
        error(catalog, 400, "not a picture path")
        catalog.body should not include ("CATALOG-SECRET")
        val toml: HttpResponse[String] = getq(port, "/api/file", "path", "2010/07/04/album.toml")
        error(toml, 400, "not a picture path")
        toml.body should not include ("TOML-SECRET")
        val escapedFile: HttpResponse[String] = getq(port, "/api/thumb", "path", "2010/07/04/2412")
        error(escapedFile, 400, escape)
        escapedFile.body should not include ("OUTSIDE-SECRET-BYTES")
        val escapedBytes: HttpResponse[String] = getq(port, "/api/file", "path", "2010/07/04/2412")
        error(escapedBytes, 400, escape)
        escapedBytes.body should not include ("OUTSIDE-SECRET-BYTES")
        val escapedDir: HttpResponse[String] = getq(port, "/api/thumb", "path", "2011/07/04/secret-stem")
        error(escapedDir, 400, escape)
        escapedDir.body should not include ("secret-stem")
        escapedDir.body should not include ("OUTSIDE-SECRET-BYTES")
        error(getq(port, "/api/thumb", "path", "2010/07/04/1009"), 404, "no such picture")
        error(getq(port, "/api/file", "path", "2010/07/04/1009"), 404, "no such picture")
        error(getq(port, "/api/thumb", "path", "2010/07/04/9999"), 404, "no such picture")
        error(send(port, "/api/thumb", "POST"), 405, "method not allowed")
        val full: HttpResponse[Array[Byte]] = getBytes(port, s"/api/file?path=${enc("2010/07/04/1001")}")
        full.statusCode shouldBe 200
        full.headers.firstValue("content-type").orElse("") shouldBe "image/jpeg"
        full.headers.firstValue("x-photo-thumb").isEmpty shouldBe true
        full.body.sameElements(original) shouldBe true
        filesIn(cacheRoot).isEmpty shouldBe true
        kindOf("1000") shouldBe "preview"
        val missed: HttpResponse[Array[Byte]] = getBytes(port, s"/api/thumb?path=${enc("2010/07/04/1000")}")
        missed.statusCode shouldBe 200
        missed.headers.firstValue("content-type").orElse("") shouldBe "image/jpeg"
        missed.headers.firstValue("x-photo-thumb").orElse("") shouldBe "placeholder"
        missed.headers.firstValue("cache-control").orElse("") shouldBe "private, no-store"
        missed.body.sameElements(placeholder) shouldBe true
        kindOf("1000") shouldBe "preview"
        val clip: HttpResponse[Array[Byte]] = getBytes(port, s"/api/thumb?path=${enc("2010/07/04/1005")}")
        clip.statusCode shouldBe 200
        clip.headers.firstValue("x-photo-thumb").orElse("") shouldBe "placeholder"
        clip.body.sameElements(placeholder) shouldBe true
        error(getq(port, "/api/file", "path", "2010/07/04/1000"), 404, "no displayable image")
        error(getq(port, "/api/file", "path", "2010/07/04/1005"), 404, "no displayable image")
        filesIn(cacheRoot).isEmpty shouldBe true
        val hit: HttpResponse[Array[Byte]] = getBytes(port, s"/api/thumb?path=${enc("2010/07/04/1001")}")
        hit.statusCode shouldBe 200
        hit.headers.firstValue("content-type").orElse("") shouldBe "image/jpeg"
        hit.headers.firstValue("x-photo-thumb").orElse("") shouldBe "hit"
        hit.headers.firstValue("cache-control").orElse("") shouldBe "private, no-store"
        val tile: Array[Byte] = hit.body
        (tile(0) & 0xff) shouldBe 0xff
        (tile(1) & 0xff) shouldBe 0xd8
        tile.sameElements(original) shouldBe false
        tile.sameElements(placeholder) shouldBe false
        val scaled: BufferedImage = ImageIO.read(new ByteArrayInputStream(tile))
        (scaled eq null) shouldBe false
        scaled.getWidth should be <= 256
        scaled.getHeight should be <= 256
        scaled.getWidth should be < 320
        val stored: Seq[File] = filesIn(cacheRoot)
        stored.length shouldBe 1
        val cacheFile: File = stored.head
        cacheFile.getParentFile.getName should fullyMatch regex "[0-9a-f]{2}"
        cacheFile.getName should fullyMatch regex "[0-9a-f]{62}\\.jpg"
        JFiles.readAllBytes(cacheFile.toPath).sameElements(tile) shouldBe true
        perms(File(cacheHome, ".cache/org.podval.tools.photo")) shouldBe "rwx------"
        perms(cacheRoot) shouldBe "rwx------"
        perms(cacheFile.getParentFile) shouldBe "rwx------"
        perms(cacheFile) shouldBe "rw-------"
        val stamped: Long = cacheFile.lastModified
        val again: HttpResponse[Array[Byte]] = getBytes(port, s"/api/thumb?path=${enc("2010/07/04/1001")}")
        again.statusCode shouldBe 200
        again.headers.firstValue("x-photo-thumb").orElse("") shouldBe "hit"
        again.body.sameElements(tile) shouldBe true
        filesIn(cacheRoot).map(_.getAbsolutePath) shouldBe Seq(cacheFile.getAbsolutePath)
        cacheFile.lastModified shouldBe stamped
        JFiles.readAllBytes(jpeg.toPath).sameElements(original) shouldBe true
        jpeg.lastModified shouldBe originalMtime
        JFiles.readAllBytes(raw.toPath).sameElements(rawBytes) shouldBe true
        JFiles.readAllBytes(video.toPath).sameElements(videoBytes) shouldBe true
        JFiles.readAllBytes(secret.toPath).sameElements("OUTSIDE-SECRET-BYTES".getBytes(StandardCharsets.UTF_8)) shouldBe true
      err should not include ("2010/07/04")
      err should not include ("1001")
      ended shouldBe Right(())
    finally
      System.setProperty("user.home", savedHome)
      deleteTree(archive)
      deleteTree(outside)
      deleteTree(runtime)
      deleteTree(cacheHome)

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

  private def getBytes(port: Int, path: String): HttpResponse[Array[Byte]] =
    val request: HttpRequest = HttpRequest.newBuilder(URI.create(s"http://$loopback:$port$path"))
      .timeout(Duration.ofSeconds(30))
      .GET()
      .build()
    client.send(request, HttpResponse.BodyHandlers.ofByteArray())

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

  private def textOf(response: HttpResponse[String]): String = response.body

  private def getq(port: Int, path: String, key: String, value: String): HttpResponse[String] =
    get(port, s"$path?$key=${enc(value)}")

  private def enc(text: String): String =
    URLEncoder.encode(text, StandardCharsets.UTF_8).replace("+", "%20")

  private def json(response: HttpResponse[String]): Json =
    Serve.readJson(response.body) match
      case Right((value, rest)) if rest.isEmpty => value
      case other => fail(s"$other from ${response.body}")

  private def field(value: Json, name: String): Json = value match
    case Json.Obj(items) =>
      items.collectFirst { case (key, item) if key == name => item }.getOrElse(fail(s"no $name in $value"))
    case other =>
      fail(other.toString)

  private def arr(value: Json): Seq[Json] = value match
    case Json.Arr(values) => values
    case other => fail(other.toString)

  private def str(value: Json): String = value match
    case Json.Str(text) => text
    case other => fail(other.toString)

  private def error(response: HttpResponse[String], status: Int, message: String): Unit =
    response.statusCode shouldBe status
    response.body shouldBe s"""{"error":"$message"}"""

  private def entry(name: String, path: String, count: Option[Int]): Json =
    val fields: Seq[(String, Json)] = Seq("name" -> Json.Str(name), "path" -> Json.Str(path))
    Json.Obj(count.fold(fields)(value => fields :+ ("count" -> Json.Long(value))))

  private def albumLink(title: String, path: String): Json =
    Json.Obj(Seq("title" -> Json.Str(title), "path" -> Json.Str(path)))

  private def stemJson(
    stem: String,
    extensions: Seq[String],
    kind: String,
    uploadable: Boolean,
    curated: Seq[String]
  ): Json =
    Json.Obj(Seq(
      "stem" -> Json.Str(stem),
      "path" -> Json.Str(s"2010/07/04/$stem"),
      "extensions" -> Json.Arr(extensions.map(Json.Str.apply)),
      "kind" -> Json.Str(kind),
      "uploadable" -> Json.Bool(uploadable),
      "curated" -> Json.Arr(curated.map(Json.Str.apply))
    ))

  private def stemAt(stems: Seq[Json], stem: String): Json =
    stems.find(item => field(item, "stem") == Json.Str(stem)).getOrElse(fail(stem))

  private def membership(joined: Boolean, ambiguous: Boolean, observed: String, reason: Option[String]): Json =
    Json.Obj(Seq(
      "joined" -> Json.Bool(joined),
      "ambiguous" -> Json.Bool(ambiguous),
      "observed" -> Json.Str(observed),
      "reason" -> reason.fold(Json.Null)(Json.Str.apply)
    ))

  private def tilesOf(value: Json): Seq[(String, String, Boolean, String)] =
    arr(field(value, "tiles")).map: tile =>
      (
        str(field(tile, "day")),
        str(field(tile, "stem")),
        field(tile, "desired") == Json.Bool(true),
        str(field(tile, "observed"))
      )

  private def album(port: Int, title: String): Json =
    val response: HttpResponse[String] = getq(port, "/api/album", "title", title)
    response.statusCode shouldBe 200
    response.body should not include ("AF1Qip")
    json(response)

  private def sessionFor(archive: File): Session =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    Session(Root(Configuration(rootPath, ZoneId.of("America/New_York"))))

  // Configuration reads HOME, not user.home. The thumb cache is the user.home property.
  private def sessionAt(archive: File): Session =
    val home: File = File(System.getenv("HOME"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    Session(Root(Configuration(rootPath, ZoneId.of("America/New_York"))))

  private def writeJpeg(file: File, width: Int, height: Int): Unit =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val graphics = image.createGraphics()
    try
      graphics.setColor(Color.BLUE)
      graphics.fillRect(0, 0, width, height)
      graphics.setColor(Color.ORANGE)
      graphics.fillRect(0, 0, width / 2, height / 2)
    finally graphics.dispose()
    file.getParentFile.mkdirs()
    ImageIO.write(image, "jpg", file) shouldBe true

  private def touch(directory: File, name: String): Unit =
    if !directory.isDirectory then directory.mkdirs() shouldBe true
    JFiles.writeString(File(directory, name).toPath, name)

  private def writeText(file: File, text: String): Unit =
    file.getParentFile.mkdirs()
    JFiles.writeString(file.toPath, text)

  private def readText(file: File): String = JFiles.readString(file.toPath)

  private def catalogFile(archive: File): File =
    File(File(archive, ".gphoto-sync"), "catalog.txt")

  private def writeCatalog(archive: File, lines: String*): File =
    val file: File = catalogFile(archive)
    val text: String = ("# gphoto-catalog 2" +: lines).mkString("", "\n", "\n")
    writeText(file, text)
    file.setLastModified(file.lastModified + 2000L)
    file

  private def writeLinks(archive: File, rows: String*): Unit =
    val text: String = ("# gphoto-album-links 1" +: rows).mkString("", "\n", "\n")
    writeText(File(File(archive, ".gphoto-sync"), "album-links.tsv"), text)

  private def catalogValue(file: File, present: Boolean, readable: Boolean): Json =
    val mtime: Json =
      if file.isFile then Json.Str(Instant.ofEpochMilli(file.lastModified).toString) else Json.Null
    Json.Obj(Seq(
      "present" -> Json.Bool(present),
      "readable" -> Json.Bool(readable),
      "mtime" -> mtime,
      "zone" -> Json.Str("America/New_York")
    ))

  private def deleteTree(file: File): Unit =
    if JFiles.isSymbolicLink(file.toPath) then file.delete()
    else
      if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
      file.delete()
