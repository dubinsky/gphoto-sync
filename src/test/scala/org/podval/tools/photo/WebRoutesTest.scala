package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import scala.annotation.targetName
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, File}
import javax.imageio.ImageIO
import java.net.{URI, URLEncoder}
import java.net.http.{HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.Files as JFiles
import java.time.{Duration, Instant, LocalDate, ZoneId}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

class WebRoutesTest extends AnyFlatSpec, WebTestSupport:
  @targetName("rejectedCanEqual")
  private given CanEqual[Web.Rejected, Web.Rejected] = CanEqual.derived

  // strictEquality does not relate a sealed parent to its cases.
  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  private val escape: String = "path escapes the archive"

  behavior of "web"

  it should "reject a path that leaves the archive" in:
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
      WebRoutes.reject(archive, "2010/07/04/gone.jpg") shouldBe Left(Web.Rejected(404, "no such picture"))
      WebRoutes.reject(archive, "2010/07/04/9999.jpg") shouldBe Left(Web.Rejected(404, "no such picture"))
      WebRoutes.reject(archive, "%252e%252e") shouldBe Left(Web.Rejected(404, "no such picture"))
      WebRoutes.reject(archive, "2010/07/04/1000.jpg") match
        case Right(path) =>
          WebRoutes.contained(archive.toPath.toRealPath(), path) shouldBe true
        case Left(other) =>
          fail(other.toString)

      val root: java.nio.file.Path = java.nio.file.Path.of("/home/dub/Pictures")
      WebRoutes.contained(root, java.nio.file.Path.of("/home/dub/Pictures-other")) shouldBe false
      WebRoutes.contained(root, java.nio.file.Path.of("/home/dub/Pictures/2010")) shouldBe true
      WebRoutes.contained(root, root) shouldBe true
    finally
      deleteTree(archive)
      deleteTree(outside)

  it should "disagree only when observed is in or out and desired differs" in:
    Web.disagree(desired = true, "in") shouldBe false
    Web.disagree(desired = false, "in") shouldBe true
    Web.disagree(desired = true, "out") shouldBe true
    Web.disagree(desired = false, "out") shouldBe false
    Web.disagree(desired = true, "unknown") shouldBe false
    Web.disagree(desired = false, "unknown") shouldBe false

  it should "serve the date tree and one day without a catalog id" in:
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
        root shouldBe Json.Obj(
          Seq(
            "at" -> Json.Str(""),
            "kind" -> Json.Str("years"),
            "entries" -> Json.Arr(
              Seq(
                entry("2010", "2010", None),
                entry("2011", "2011", None)
              )
            ),
            "albums" -> Json.Arr(Seq.empty),
            "curated" -> Json.Arr(
              Seq(
                albumLink("Portraits", "Portraits.toml"),
                albumLink("Unlisted", "Unlisted.toml")
              )
            ),
            "catalog" -> catalogValue(catalog, present = true, readable = true)
          )
        )
        textOf(get(port, "/api/tree")) should not include ("AF1Qip")
        textOf(get(port, "/api/tree")) should not include (".upload")
        Web.sessionNow.get.catalogCache shouldBe None
        json(get(port, "/api/tree?at=")) shouldBe root
        val year = json(getq(port, "/api/tree", "at", "2010"))
        year shouldBe Json.Obj(
          Seq(
            "at" -> Json.Str("2010"),
            "kind" -> Json.Str("months"),
            "entries" -> Json.Arr(
              Seq(
                entry("07", "2010/07", None),
                entry("08", "2010/08", None)
              )
            ),
            "albums" -> Json.Arr(Seq.empty),
            "curated" -> Json.Arr(Seq.empty),
            "catalog" -> catalogValue(catalog, present = true, readable = true)
          )
        )
        val month = json(getq(port, "/api/tree", "at", "2010/07"))
        field(month, "kind") shouldBe Json.Str("days")
        field(month, "entries") shouldBe Json.Arr(
          Seq(
            entry("04", "2010/07/04", Some(8)),
            entry("05", "2010/07/05", Some(0))
          )
        )
        field(month, "albums") shouldBe Json.Arr(
          Seq(
            albumLink("2010/07", "2010/07/album.toml"),
            albumLink("2010/07/Zoo", "2010/07/Zoo.toml")
          )
        )
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
        field(day, "albums") shouldBe Json.Arr(
          Seq(
            albumLink("2010/07/04-05/Cape Cod", "2010/07/04/to-05 Cape Cod.toml")
          )
        )
        field(day, "catalog") shouldBe catalogValue(catalog, present = true, readable = true)
        val stems: Seq[Json] = arr(field(day, "stems"))
        stems.map(stem => str(field(stem, "stem"))) shouldBe Seq(
          "1000",
          "1001",
          "1002",
          "1003",
          "1004",
          "1005",
          "1006",
          "1007"
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
        field(capeJson, "membership") shouldBe membership(
          joined = false,
          ambiguous = false,
          "unknown",
          Some("unjoined")
        )
        arr(field(capeJson, "tiles")).map(tile => str(field(tile, "stem"))) shouldBe Seq(
          "1000",
          "1001",
          "1002",
          "1003",
          "1004",
          "1005",
          "1006",
          "1007"
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

  it should "report a stray file without its path" in:
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

  it should "serve observed membership without leaking a catalog id" in:
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
        field(knownJson, "unmatched") shouldBe Json.Arr(
          Seq(
            Json.Obj(Seq("filename" -> Json.Str("IMG_9999.JPG"), "day" -> Json.Str("2010/07/04"))),
            Json.Obj(Seq("filename" -> Json.Str("LOST.JPG"), "day" -> Json.Null))
          )
        )
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

  it should "skip a missing day in a span and reject a directory symlink that leaves the archive" in:
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
          listed.body should include("\"2010\"")
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

  it should "serve a scaled thumbnail, a placeholder, and the original file bytes" in:
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
      try in.readAllBytes()
      finally in.close()
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
        JFiles
          .readAllBytes(secret.toPath)
          .sameElements("OUTSIDE-SECRET-BYTES".getBytes(StandardCharsets.UTF_8)) shouldBe true
      val logged: String = err.linesIterator.filterNot(_.startsWith("bound ")).mkString("\n")
      logged should not include ("2010/07/04")
      logged should not include ("1001")
      ended shouldBe Right(())
    finally
      System.setProperty("user.home", savedHome)
      deleteTree(archive)
      deleteTree(outside)
      deleteTree(runtime)
      deleteTree(cacheHome)

  it should "toggle include lines and reject a path the file does not claim" in:
    val archive: File = JFiles.createTempDirectory("web-toggle").toFile
    val outside: File = JFiles.createTempDirectory("web-toggle-out").toFile
    val runtime: File = JFiles.createTempDirectory("web-toggle-rt").toFile
    val port: Int = freePort()
    val xdgExisted: Boolean = xdgWriteLock.exists(_.isFile)
    val kept: String =
      "# keep\n" +
        "external = true\n" +
        "include = [\n" +
        "  'all',\n" +
        "]\n"
    val outsideText: String =
      "# stay\n" +
        "external = true\n" +
        "include = [\n" +
        "  'all',\n" +
        "]\n"
    val mixedText: String =
      "include = [\n" +
        "  'all',\n" +
        "  '2010/07/04 1000',\n" +
        "]\n"
    val linesText: String =
      "include = [\n" +
        "  '2010/07/04 1000..1002',\n" +
        "]\n"
    val portraits: String =
      "include = [\n" +
        "  '2010/07/04-05 1000',\n" +
        "]\n"
    val gapText: String =
      "include = [\n" +
        "  '2010/09/06-07 1000',\n" +
        "]\n"
    val linkText: String =
      "include = [\n" +
        "  '2010/09/03-04 1000',\n" +
        "]\n"
    val badText: String = "include = [\"2009/03/10 2412\",\n\"2009/01/01 0000\"]\n"
    val outsideFile: File = File(archive, "2010/07/04/Outside.toml")
    val mixedFile: File = File(archive, "2010/07/04/Mixed.toml")
    val linesFile: File = File(archive, "2010/07/04/Lines.toml")
    val shotFile: File = File(archive, "Shot.toml")
    val badFile: File = File(archive, "Bad.toml")
    val linkFile: File = File(archive, "Link.toml")
    val datedLink: File = File(archive, "2010/09/03/to-04.toml")
    try
      touch(File(archive, "2010/07/04"), "1000.jpg")
      touch(File(archive, "2010/07/04"), "1001.jpg")
      touch(File(archive, "2010/07/04"), "1002.jpg")
      touch(File(archive, "2010/07/05"), "1000.jpg")
      touch(File(archive, "2010/08/01"), "1000.jpg")
      touch(File(archive, "2010/08/02"), "1000.jpg")
      touch(File(archive, "2010/09/01"), "1000.jpg")
      touch(File(archive, "2010/09/01"), "1001.jpg")
      touch(File(archive, "2010/09/03"), "1000.jpg")
      touch(File(archive, "2010/09/06"), "1000.jpg")
      touch(File(archive, "2010/11/02"), "2412.jpg")
      touch(File(archive, "2010/11/03"), "1000.jpg")
      touch(File(archive, "2009/03/10"), "2412.jpg")
      touch(outside, "9998.jpg")
      writeText(File(archive, "2010/07/04/album.toml"), kept)
      writeText(outsideFile, outsideText)
      writeText(mixedFile, mixedText)
      writeText(linesFile, linesText)
      writeText(File(archive, "2010/09/01/album.toml"), "# day\n")
      writeText(shotFile, "")
      writeText(File(archive, "2010/11/03/album.toml"), "")
      writeText(File(archive, "2010/08/01/to-02.toml"), "include = [\n  'all',\n]\n")
      writeText(File(archive, "Portraits.toml"), portraits)
      writeText(File(archive, "Gap.toml"), gapText)
      writeText(File(archive, "2010/09/06/to-07.toml"), "include = [\n  'all',\n]\n")
      writeText(linkFile, linkText)
      writeText(datedLink, "include = [\n  'all',\n]\n")
      writeText(badFile, badText)
      val (_, _, ended) = running(runtime, port):
        Web.useSession(sessionFor(archive))
        def flipped(
          title: String,
          path: String,
          desired: Boolean,
          include: Seq[String],
          contentType: Option[String] = Some("application/json")
        ): HttpResponse[String] =
          val response: HttpResponse[String] =
            postJson(port, "/api/toggle", s"""{"title":"$title","path":"$path"}""", contentType)
          response.statusCode shouldBe 200
          val parsed: Json = json(response)
          field(parsed, "title") shouldBe Json.Str(title)
          field(parsed, "path") shouldBe Json.Str(path)
          field(parsed, "desired") shouldBe Json.Bool(desired)
          arr(field(parsed, "include")).map(str) shouldBe include
          response
        flipped(
          "2010/07/04",
          "2010/07/04/1001",
          desired = false,
          Seq("all except 1001"),
          Some("Application/JSON; charset=utf-8")
        )
        val dropped: String = readText(File(archive, "2010/07/04/album.toml"))
        dropped should startWith("# keep\nexternal = true\n")
        dropped should include("all except 1001")
        flipped("2010/07/04/Lines", "2010/07/04/1001", desired = false, Seq("2010/07/04 1000", "2010/07/04 1002"))
        readText(linesFile) should not include ("all except")
        flipped("2010/09/01", "2010/09/01/1000", desired = true, Seq("2010/09/01 1000"))
        val twoStems: String = readText(File(archive, "2010/09/01/album.toml"))
        twoStems should startWith("# day\n")
        twoStems should not include ("'all'")
        flipped("Shot", "2010/11/02/2412", desired = true, Seq("2010/11/02 2412"))
        readText(shotFile) should not include ("'all'")
        flipped("2010/11/03", "2010/11/03/1000", desired = true, Seq("all"))
        flipped("2010/08/01-02", "2010/08/01/1000", desired = false, Seq("2010/08/02 1000"))
        readText(File(archive, "2010/08/01/to-02.toml")) should not include ("all except")
        tilesOf(album(port, "Portraits")).map(tile => (tile._1, tile._2)) shouldBe Seq(
          ("2010/07/04", "1000"),
          ("2010/07/05", "1000")
        )
        flipped("Portraits", "2010/07/04/1000", desired = false, Seq("2010/07/05 1000"))
        Album.read(archive, File(archive, "Portraits.toml")).claims(LocalDate.of(2010, 7, 5), "1000") shouldBe true
        tilesOf(album(port, "Portraits")).map(tile => (tile._1, tile._2)) shouldBe Seq(("2010/07/05", "1000"))
        val gap: HttpResponse[String] = getq(port, "/api/album", "title", "Gap")
        gap.statusCode shouldBe 200
        gap.body should not include ("no such day")
        tilesOf(json(gap)).map(tile => (tile._1, tile._2)) shouldBe Seq(("2010/09/06", "1000"))
        flipped("Gap", "2010/09/06/1000", desired = false, Seq.empty)
        val datedGap: HttpResponse[String] = getq(port, "/api/album", "title", "2010/09/06-07")
        datedGap.statusCode shouldBe 200
        datedGap.body should not include ("no such day")
        flipped("2010/09/06-07", "2010/09/06/1000", desired = false, Seq.empty)
        val stayed: String = readText(outsideFile)
        error(
          postJson(port, "/api/toggle", """{"title":"2010/07/04/Outside","path":"2010/07/08/1000"}"""),
          400,
          "2010/07/08/1000 is outside 2010/07/04/Outside"
        )
        readText(outsideFile) shouldBe stayed
        val unspliced: HttpResponse[String] =
          postJson(port, "/api/toggle", """{"title":"Bad","path":"2009/03/10/2412"}""")
        unspliced.statusCode shouldBe 500
        unspliced.body should include("include array does not end on its own line")
        readText(badFile) shouldBe badText
        error(
          postJson(port, "/api/toggle", """{"title":"2010/07/04-03","path":"2010/07/04/1000"}"""),
          400,
          "not an album title: 2010/07/04-03"
        )
        error(
          postJson(port, "/api/toggle", """{"title":"2010/02/31","path":"2010/07/04/1000"}"""),
          400,
          "not an album title: 2010/02/31"
        )
        error(
          postJson(port, "/api/toggle", """{"title":"Gone","path":"2010/07/04/1000"}"""),
          404,
          "no such album: Gone"
        )
        error(
          postJson(port, "/api/toggle", """{"title":"2010/07/04","path":"nope"}"""),
          400,
          "not a picture path: nope"
        )
        error(
          postJson(port, "/api/toggle", """{"title":"2010/07/04","path":"2010/07/04"}"""),
          400,
          "not a picture path: 2010/07/04"
        )
        error(
          postJson(port, "/api/toggle", """{"title":"2010/07/04","path":"2010/02/31/1000"}"""),
          400,
          "not a picture path: 2010/02/31/1000"
        )
        error(
          postJson(port, "/api/toggle", """{"title":"2010/07/04","path":"2010/07/04/9999"}"""),
          400,
          "no such picture: 2010/07/04/9999"
        )
        error(
          postJson(port, "/api/toggle", """{"title":"2010/07/04/Mixed","path":"2010/07/04/1000"}"""),
          400,
          "include mixes all with other lines"
        )
        readText(mixedFile) shouldBe mixedText
        error(postJson(port, "/api/toggle", "{", None), 415, "unsupported media type")
        error(postJson(port, "/api/toggle", "{", Some("text/plain")), 415, "unsupported media type")
        error(postJson(port, "/api/toggle", "{"), 400, "not json")
        error(postJson(port, "/api/toggle", "[]"), 400, "not json")
        error(get(port, "/api/toggle"), 405, "method not allowed")
        JFiles.createSymbolicLink(File(archive, "2010/09/04").toPath, outside.toPath)
        val linkedBytes: String = readText(linkFile)
        val datedBytes: String = readText(datedLink)
        val curatedEscape: HttpResponse[String] =
          postJson(port, "/api/toggle", """{"title":"Link","path":"2010/09/03/1000"}""")
        error(curatedEscape, 400, escape)
        curatedEscape.body should not include ("9998")
        readText(linkFile) shouldBe linkedBytes
        val datedEscape: HttpResponse[String] =
          postJson(port, "/api/toggle", """{"title":"2010/09/03-04","path":"2010/09/03/1000"}""")
        error(datedEscape, 400, escape)
        datedEscape.body should not include ("9998")
        readText(datedLink) shouldBe datedBytes
        val shotText: String = readText(shotFile)
        val held = new CountDownLatch(1)
        val release = new CountDownLatch(1)
        val holder = new Thread(
          () => {
            Album.withWriteLockDirectory(runtime):
              Album.withWriteLock:
                held.countDown()
                release.await(8, TimeUnit.SECONDS)
                ()
          },
          "album-hold"
        )
        holder.setDaemon(true)
        holder.start()
        try
          assert(held.await(5, TimeUnit.SECONDS))
          error(
            postJson(port, "/api/toggle", """{"title":"Shot","path":"2010/11/02/2412"}"""),
            500,
            "album file is busy"
          )
          readText(shotFile) shouldBe shotText
        finally
          release.countDown()
          holder.join(5000)
        holder.isAlive shouldBe false
        assertRuntimeLock(runtime, xdgExisted)
      ended shouldBe Right(())
    finally
      deleteTree(archive)
      deleteTree(outside)
      deleteTree(runtime)

  it should "splice a stale include that waited, and re-read a toggle that waited" in:
    val archive: File = JFiles.createTempDirectory("web-overlap").toFile
    val runtime: File = JFiles.createTempDirectory("web-overlap-rt").toFile
    val port: Int = freePort()
    val xdgExisted: Boolean = xdgWriteLock.exists(_.isFile)
    val day: LocalDate = LocalDate.of(2010, 7, 4)
    val file: File = File(archive, "2010/07/04/album.toml")
    val original: String =
      "include = [\n" +
        "  '2010/07/04 1000',\n" +
        "  '2010/07/04 1001',\n" +
        "]\n"
    val universe: Map[LocalDate, Set[String]] = Map(day -> Set("1000", "1001"))
    Album.afterCompute = () => ()
    try
      touch(File(archive, "2010/07/04"), "1000.jpg")
      touch(File(archive, "2010/07/04"), "1001.jpg")
      writeText(file, original)
      val loaded: Album = Album.read(archive, file)
      val stale: Seq[Include.Entry] = Include.toggle(loaded.include, loaded.span, day -> "1000", universe)
      val (_, _, ended) = running(runtime, port):
        Web.useSession(sessionFor(archive))
        def claims(stem: String): Boolean = Album.read(archive, file).claims(day, stem)
        def overlap(wait: => Unit): Unit =
          val entered = new CountDownLatch(1)
          val queued = new AtomicBoolean(false)
          val failed = new AtomicReference[Throwable](null)
          val store = new Thread(
            () => {
              try
                Album.withWriteLockDirectory(runtime):
                  Album.afterCompute = () =>
                    Album.afterCompute = () => ()
                    entered.countDown()
                    val deadline: Long = System.nanoTime() + 3000000000L
                    def loop(): Unit =
                      if Album.writeWaiters > 0 then queued.set(true)
                      else if System.nanoTime() >= deadline then throw IllegalStateException("waiter not queued")
                      else
                        Thread.sleep(20)
                        loop()
                    loop()
                  Album.storeRemovals(file, archive, Seq(day -> "1001"), universe)
              catch
                case error: Throwable =>
                  Album.afterCompute = () => ()
                  entered.countDown()
                  failed.set(error)
            },
            "album-store"
          )
          store.setDaemon(true)
          store.start()
          try
            assert(entered.await(5, TimeUnit.SECONDS))
            if failed.get != null then throw failed.get
            wait
          finally store.join(15000)
          store.isAlive shouldBe false
          if failed.get != null then throw failed.get
          queued.get shouldBe true
        writeText(file, original)
        overlap:
          val waiterFailed = new AtomicReference[Throwable](null)
          val waiter = new Thread(
            () => {
              try Album.withWriteLockDirectory(runtime)(Album.putInclude(file, stale))
              catch case error: Throwable => waiterFailed.set(error)
            },
            "album-stale"
          )
          waiter.setDaemon(true)
          waiter.start()
          waiter.join(15000)
          waiter.isAlive shouldBe false
          if waiterFailed.get != null then throw waiterFailed.get
        Album.read(archive, file).include.map(Include.logical) shouldBe Seq("2010/07/04 1001")
        claims("1001") shouldBe true
        claims("1000") shouldBe false
        writeText(file, original)
        val correct = new AtomicReference[HttpResponse[String]](null)
        overlap:
          correct.set(postJson(port, "/api/toggle", """{"title":"2010/07/04","path":"2010/07/04/1000"}"""))
        val reread: HttpResponse[String] = correct.get
        reread.statusCode shouldBe 200
        field(json(reread), "desired") shouldBe Json.Bool(false)
        field(json(reread), "include") shouldBe Json.Arr(Seq.empty[Json])
        claims("1000") shouldBe false
        claims("1001") shouldBe false
        readText(file) should include("include = []")
        writeText(file, original)
        Album.afterCompute = () => ()
        Album.withWriteLockDirectory(runtime):
          Album.storeRemovals(file, archive, Seq(day -> "1001"), universe)
        val sequential: HttpResponse[String] =
          postJson(port, "/api/toggle", """{"title":"2010/07/04","path":"2010/07/04/1000"}""")
        sequential.statusCode shouldBe 200
        field(json(sequential), "desired") shouldBe Json.Bool(false)
        field(json(sequential), "include") shouldBe Json.Arr(Seq.empty[Json])
        claims("1000") shouldBe false
        claims("1001") shouldBe false
        assertRuntimeLock(runtime, xdgExisted)
      ended shouldBe Right(())
    finally
      Album.afterCompute = () => ()
      deleteTree(archive)
      deleteTree(runtime)

  it should "seed a new album from a catalog match or from all and write nothing on failure" in:
    val archive: File = JFiles.createTempDirectory("web-create").toFile
    val runtime: File = JFiles.createTempDirectory("web-create-rt").toFile
    val port: Int = freePort()
    val xdgExisted: Boolean = xdgWriteLock.exists(_.isFile)
    val existing: File = File(archive, "2010/07/04/album.toml")
    val existingText: String = "# kept\ninclude = [\n  'all',\n]\n"
    try
      touch(File(archive, "2010/07/04"), "2412.jpg")
      touch(File(archive, "2010/07/04"), "1000.jpg")
      touch(File(archive, "2010/07/05"), "1000.jpg")
      writeText(existing, existingText)
      File(archive, "2010/07/20").mkdirs() shouldBe true
      File(archive, "2010/07/21").mkdirs() shouldBe true
      val (_, _, ended) = running(runtime, port):
        Web.useSession(sessionFor(archive))
        def tomlTexts: Map[String, String] =
          def walk(dir: File): Seq[File] =
            Option(dir.listFiles).toSeq.flatten.flatMap: child =>
              if JFiles.isSymbolicLink(child.toPath) then Seq.empty
              else if child.isDirectory then walk(child)
              else if child.getName.endsWith(".toml") then Seq(child)
              else Seq.empty
          walk(archive).map(file => archive.toPath.relativize(file.toPath).toString -> readText(file)).toMap
        def rejectCreate(
          body: String,
          status: Int,
          message: String,
          contentType: Option[String] = Some("application/json")
        ): Unit =
          val before: Map[String, String] = tomlTexts
          val response: HttpResponse[String] = postJson(port, "/api/albums", body, contentType)
          error(response, status, message)
          response.body should not include ("AF1Qip")
          tomlTexts shouldBe before
        def created(body: String): Json =
          val response: HttpResponse[String] = postJson(port, "/api/albums", body)
          response.statusCode shouldBe 201
          response.body should not include ("AF1Qip")
          json(response)
        rejectCreate("""{"start":"2010-07-04","end":"2010-07-04"}""", 400, "not a day")
        rejectCreate("""{"start":"2010/07/05","end":"2010/07/04"}""", 400, "not a span")
        rejectCreate(
          """{"start":"2010/07/04","end":"2010/07/04","name":"bad/name"}""",
          400,
          "not an album name: bad/name"
        )
        rejectCreate("""{"start":"2010/07/04","end":"2010/07/04"}""", 415, "unsupported media type", None)
        rejectCreate("""{"start":"2010/07/04","end":"2010/07/04"}""", 415, "unsupported media type", Some("text/plain"))
        rejectCreate("[]", 400, "not json")
        rejectCreate("{", 400, "not json")
        rejectCreate("""{"start":"2011/01/02","end":"2011/01/02","name":"Nope"}""", 400, "no such directory")
        File(archive, "2011").exists shouldBe false
        rejectCreate("""{"start":"2010/07/04","end":"2010/07/04"}""", 409, "album already exists: 2010/07/04")
        readText(existing) shouldBe existingText
        writeCatalog(
          archive,
          "album\tAF1QipWebFixture\t1\t2010/07/04-18/Cape Cod",
          "owner\tAF1QipWebFixture\tAda",
          "item\tAF1QipWebFixture\t2412.jpg\t2010-07-04\tAF1QipHit\t-"
        )
        val cape: Json = created("""{"start":"2010/07/04","end":"2010/07/18","name":"Cape Cod"}""")
        field(cape, "title") shouldBe Json.Str("2010/07/04-18/Cape Cod")
        field(cape, "path") shouldBe Json.Str("2010/07/04/to-18 Cape Cod.toml")
        field(cape, "external") shouldBe Json.Bool(true)
        field(cape, "seed") shouldBe Json.Str("matched")
        val capeFile: File = File(archive, "2010/07/04/to-18 Cape Cod.toml")
        val capeText: String = readText(capeFile)
        capeText shouldBe
          """google-title = "2010/07/04-18/Cape Cod"
            |owner = "Ada"
            |include = [
            |  '2010/07/04 2412',
            |]
            |""".stripMargin
        capeText should not include ("google-album-id")
        capeText should not include ("published-title")
        capeText should not include ("AF1Qip")
        perms(capeFile) shouldBe "rw-------"
        File(archive, "2010/07/18").exists shouldBe false
        File(File(archive, ".gphoto-sync"), "album-links.tsv").exists shouldBe false
        rejectCreate(
          """{"start":"2010/07/04","end":"2010/07/18","name":"Cape Cod"}""",
          409,
          "album already exists: 2010/07/04-18/Cape Cod"
        )
        readText(capeFile) shouldBe capeText
        writeCatalog(
          archive,
          "album\tAF1QipWebFixture\t1\t2010/07/04/Plain",
          "item\tAF1QipWebFixture\t1000.jpg\t2010-07-04\tAF1QipPlain\t-"
        )
        val plain: Json = created("""{"start":"2010/07/04","end":"2010/07/04","name":"Plain"}""")
        field(plain, "external") shouldBe Json.Bool(true)
        field(plain, "seed") shouldBe Json.Str("matched")
        val plainText: String = readText(File(archive, "2010/07/04/Plain.toml"))
        plainText shouldBe
          """include = [
            |  '2010/07/04 1000',
            |]
            |""".stripMargin
        plainText should not include ("google-title")
        plainText should not include ("google-album-id")
        writeCatalog(archive, "album\tAF1QipWebFixture\t0\t2010/07/04/Empty")
        val empty: Json = created("""{"start":"2010/07/04","end":"2010/07/04","name":"Empty"}""")
        field(empty, "external") shouldBe Json.Bool(true)
        field(empty, "seed") shouldBe Json.Str("matched")
        readText(File(archive, "2010/07/04/Empty.toml")) shouldBe ""
        writeCatalog(archive, "album\tAF1QipWebFixture\t0\tSomeone Else")
        val local: Json = created("""{"start":"2010/07/04","end":"2010/07/04","name":"Local"}""")
        field(local, "external") shouldBe Json.Bool(false)
        field(local, "seed") shouldBe Json.Str("all")
        readText(File(archive, "2010/07/04/Local.toml")) shouldBe
          "external = false\ninclude = [\n  'all',\n]\n"
        catalogFile(archive).delete() shouldBe true
        val absent: Json = created("""{"start":"2010/07/04","end":"2010/07/04","name":"Absent"}""")
        field(absent, "external") shouldBe Json.Bool(false)
        field(absent, "seed") shouldBe Json.Str("all")
        readText(File(archive, "2010/07/04/Absent.toml")) should include("external = false")
        val unnamed: Json = created("""{"start":"2010/07/20","end":"2010/07/20","name":""}""")
        field(unnamed, "title") shouldBe Json.Str("2010/07/20")
        field(unnamed, "path") shouldBe Json.Str("2010/07/20/album.toml")
        field(unnamed, "external") shouldBe Json.Bool(false)
        field(unnamed, "seed") shouldBe Json.Str("all")
        val omitted: Json = created("""{"start":"2010/07/21","end":"2010/07/21"}""")
        field(omitted, "title") shouldBe Json.Str("2010/07/21")
        field(omitted, "path") shouldBe Json.Str("2010/07/21/album.toml")
        field(omitted, "seed") shouldBe Json.Str("all")
        writeText(catalogFile(archive), "not a header\n")
        rejectCreate("""{"start":"2010/07/04","end":"2010/07/04","name":"Unread"}""", 400, "catalog is unreadable")
        File(archive, "2010/07/04/Unread.toml").exists shouldBe false
        writeCatalog(
          archive,
          "album\tAF1QipOne\t0\t2010/07/04/Dup",
          "album\tAF1QipTwo\t0\t2010/07/04/Dup"
        )
        rejectCreate("""{"start":"2010/07/04","end":"2010/07/04","name":"Dup"}""", 400, "ambiguous catalog title")
        File(archive, "2010/07/04/Dup.toml").exists shouldBe false
        writeCatalog(
          archive,
          "album\tAF1QipWebFixture\t1\t2010/07/04/Broken",
          "error\tAF1QipWebFixture\t1\t1\t0",
          "item\tAF1QipWebFixture\t2412.jpg\t2010-07-04\tAF1QipHit\t-"
        )
        rejectCreate(
          """{"start":"2010/07/04","end":"2010/07/04","name":"Broken"}""",
          400,
          "catalog album is not readable"
        )
        File(archive, "2010/07/04/Broken.toml").exists shouldBe false
        writeCatalog(
          archive,
          "album\tAF1QipWebFixture\t0\t2010/07/04/Auto",
          "auto-add\tAF1QipWebFixture"
        )
        rejectCreate(
          """{"start":"2010/07/04","end":"2010/07/04","name":"Auto"}""",
          400,
          "catalog album is not readable"
        )
        File(archive, "2010/07/04/Auto.toml").exists shouldBe false
        writeCatalog(
          archive,
          "album\tAF1QipWebFixture\t1\t2010/07/04/Wide",
          "item\tAF1QipWebFixture\t1000.jpg\t2010-07-05\tAF1QipOut\t-"
        )
        rejectCreate("""{"start":"2010/07/04","end":"2010/07/04","name":"Wide"}""", 400, "matched day outside the span")
        File(archive, "2010/07/04/Wide.toml").exists shouldBe false
        def partialNames(dir: File): Seq[String] =
          Option(dir.listFiles).toSeq.flatten.flatMap: child =>
            if JFiles.isSymbolicLink(child.toPath) then Seq.empty
            else if child.isDirectory then partialNames(child)
            else if child.getName.startsWith(".") && child.getName.endsWith(".partial") then Seq(child.getName)
            else Seq.empty
        partialNames(archive) shouldBe Seq.empty[String]
        assertRuntimeLock(runtime, xdgExisted)
      ended shouldBe Right(())
    finally
      deleteTree(archive)
      deleteTree(runtime)

  private def rejected(archive: File, raw: String): Unit =
    WebRoutes.reject(archive, raw) shouldBe Left(Web.Rejected(400, escape))

  private def getBytes(port: Int, path: String): HttpResponse[Array[Byte]] =
    val request: HttpRequest = HttpRequest
      .newBuilder(URI.create(s"http://$loopback:$port$path"))
      .timeout(Duration.ofSeconds(30))
      .GET()
      .build()
    client.send(request, HttpResponse.BodyHandlers.ofByteArray())

  private def postJson(
    port: Int,
    path: String,
    body: String,
    contentType: Option[String] = Some("application/json")
  ): HttpResponse[String] =
    val builder: HttpRequest.Builder = HttpRequest
      .newBuilder(URI.create(s"http://$loopback:$port$path"))
      .timeout(Duration.ofSeconds(15))
      .POST(HttpRequest.BodyPublishers.ofString(body, StandardCharsets.UTF_8))
    contentType.foreach(value => builder.header("Content-Type", value))
    client.send(builder.build(), HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))

  private def xdgWriteLock: Option[File] =
    Option(System.getenv("XDG_RUNTIME_DIR"))
      .map(_.trim)
      .filter(_.nonEmpty)
      .map: dir =>
        File(File(dir, "org.podval.tools.photo"), "album-write.lock")

  private def assertRuntimeLock(runtime: File, xdgExisted: Boolean): Unit =
    perms(File(runtime, "album-write.lock")) shouldBe "rw-------"
    if !xdgExisted then xdgWriteLock.foreach(file => file.isFile shouldBe false)

  private def textOf(response: HttpResponse[String]): String = response.body

  private def getq(port: Int, path: String, key: String, value: String): HttpResponse[String] =
    get(port, s"$path?$key=${enc(value)}")

  private def enc(text: String): String =
    URLEncoder.encode(text, StandardCharsets.UTF_8).replace("+", "%20")

  private def json(response: HttpResponse[String]): Json =
    Serve.readJson(response.body) match
      case Right((value, rest)) if rest.isEmpty => value
      case other                                => fail(s"$other from ${response.body}")

  private def field(value: Json, name: String): Json = value match
    case Json.Obj(items) =>
      items.collectFirst { case (key, item) if key == name => item }.getOrElse(fail(s"no $name in $value"))
    case other =>
      fail(other.toString)

  private def arr(value: Json): Seq[Json] = value match
    case Json.Arr(values) => values
    case other            => fail(other.toString)

  private def str(value: Json): String = value match
    case Json.Str(text) => text
    case other          => fail(other.toString)

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
    Json.Obj(
      Seq(
        "stem" -> Json.Str(stem),
        "path" -> Json.Str(s"2010/07/04/$stem"),
        "extensions" -> Json.Arr(extensions.map(Json.Str.apply)),
        "kind" -> Json.Str(kind),
        "uploadable" -> Json.Bool(uploadable),
        "curated" -> Json.Arr(curated.map(Json.Str.apply))
      )
    )

  private def stemAt(stems: Seq[Json], stem: String): Json =
    stems.find(item => field(item, "stem") == Json.Str(stem)).getOrElse(fail(stem))

  private def membership(joined: Boolean, ambiguous: Boolean, observed: String, reason: Option[String]): Json =
    Json.Obj(
      Seq(
        "joined" -> Json.Bool(joined),
        "ambiguous" -> Json.Bool(ambiguous),
        "observed" -> Json.Str(observed),
        "reason" -> reason.fold(Json.Null)(Json.Str.apply)
      )
    )

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
    Json.Obj(
      Seq(
        "present" -> Json.Bool(present),
        "readable" -> Json.Bool(readable),
        "mtime" -> mtime,
        "zone" -> Json.Str("America/New_York")
      )
    )
