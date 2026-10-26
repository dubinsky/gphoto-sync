package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.time.ZoneId

class ConfigurationTest extends AnyFlatSpec, Matchers:
  private val fixture: String =
    """byYearDirectoryName = "originals"
      |rootPath = "Pictures"
      |uploadDirectoryName = "Upload"
      |zoneId = "America/New_York"
      |""".stripMargin

  "Configuration" should "round-trip the four required keys" in :
    val expected: Configuration = Configuration(
      rootPath = "Pictures",
      byYearDirectoryName = "originals",
      uploadDirectoryName = "Upload",
      zoneId = ZoneId.of("America/New_York")
    )
    val decoded: Configuration = Configuration.decode(fixture)
    decoded shouldBe expected
    Configuration.encode(decoded) shouldBe fixture

  it should "reject a missing zoneId" in :
    val toml: String = fixture.linesIterator.filterNot(_.startsWith("zoneId")).mkString("\n")
    val exception: IllegalArgumentException = intercept[IllegalArgumentException]:
      Configuration.decode(toml)
    exception.getMessage should include ("zoneId")

  it should "ignore an unknown key and drop it on encode" in :
    val decoded: Configuration = Configuration.decode(s"${fixture}extra = 1\n")
    decoded shouldBe Configuration(
      rootPath = "Pictures",
      byYearDirectoryName = "originals",
      uploadDirectoryName = "Upload",
      zoneId = ZoneId.of("America/New_York")
    )
    Configuration.encode(decoded) should not include "extra"

  it should "reject a zoneId that is not a zone" in :
    val exception: IllegalArgumentException = intercept[IllegalArgumentException]:
      Configuration.decode(fixture.replace("America/New_York", "Not/AZone"))
    exception.getMessage should include ("zoneId")
