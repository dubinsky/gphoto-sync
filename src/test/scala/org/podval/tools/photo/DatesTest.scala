package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.time.{Instant, LocalDate, LocalDateTime, ZoneId, ZoneOffset}

class DatesTest extends AnyFlatSpec, Matchers:
  private val zone: ZoneId = ZoneId.of("America/New_York")

  "fromFileName" should "label each filename clock" in :
    labeled("2010-09-14-19-49-2", "2010-09-14T19:49:02") shouldBe "full"
    labeled("20120812_013216", "2012-08-12T01:32:16") shouldBe "compact"
    val millis: (Instant, String) = Dates.fromFileName("1300000000000", zone, None).getOrElse(fail("no clock"))
    millis._1.toEpochMilli shouldBe 1300000000000L
    millis._2 shouldBe "millis"
    labeled("013216", "2012-08-12T01:32:16", Some(LocalDate.of(2012, 8, 12))) shouldBe "short"

  it should "strip one trailing _1" in :
    labeled("20120812_013216_1", "2012-08-12T01:32:16") shouldBe "compact"

  it should "drop one trailing v before _1 without rewriting the stem" in :
    val plain: Long = clock("20191220_141955")
    val burst: Long = clock("20191220_141955_1")
    clock("20191220_141955v") shouldBe plain
    clock("20191220_141955_1v") shouldBe burst
    labeled("20191220_141955v", "2019-12-20T14:19:55") shouldBe "compact"
    labeled("20191220_141955_1v", "2019-12-20T14:19:55") shouldBe "compact"
    Dates.fromFileName("20191220_141955_2", zone, None) shouldBe None
    Dates.fromFileName("20191220_141955_2v", zone, None) shouldBe None

  it should "skip a short name when no date is supplied" in :
    Dates.fromFileName("013216", zone, None).isEmpty shouldBe true

  private def clock(name: String): Long =
    Dates.fromFileName(name, zone, None).getOrElse(fail(s"no clock in $name"))._1.toEpochMilli

  private def labeled(name: String, clock: String, shortDate: Option[LocalDate] = None): String =
    val found: (Instant, String) = Dates.fromFileName(name, zone, shortDate).getOrElse(fail(s"no clock in $name"))
    found._1.toEpochMilli shouldBe LocalDateTime.parse(clock).toInstant(ZoneOffset.UTC).toEpochMilli
    found._2
