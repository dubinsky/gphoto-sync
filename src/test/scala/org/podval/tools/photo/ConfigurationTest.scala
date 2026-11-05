package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.time.ZoneId

class ConfigurationTest extends AnyFlatSpec, Matchers:
  private val fixture: String =
    """rootPath = "Pictures/originals"
      |zoneId = "America/New_York"
      |""".stripMargin

  "Configuration" should "round-trip the two required keys" in:
    val expected: Configuration = Configuration(
      rootPath = "Pictures/originals",
      zoneId = ZoneId.of("America/New_York")
    )
    val decoded: Configuration = Configuration.decode(fixture)
    decoded shouldBe expected
    val encoded: String = Configuration.encode(decoded)
    encoded shouldBe fixture
    encoded should not include "byYearDirectoryName"
    encoded should not include "uploadDirectoryName"

  it should "reject a missing zoneId" in:
    val toml: String = fixture.linesIterator.filterNot(_.startsWith("zoneId")).mkString("\n")
    val exception: IllegalArgumentException = intercept[IllegalArgumentException]:
      Configuration.decode(toml)
    exception.getMessage should include("zoneId")

  it should "ignore an unknown key and drop it on encode" in:
    val decoded: Configuration = Configuration.decode(s"${fixture}extra = 1\n")
    decoded shouldBe Configuration(
      rootPath = "Pictures/originals",
      zoneId = ZoneId.of("America/New_York")
    )
    Configuration.encode(decoded) should not include "extra"

  it should "reject a zoneId that is not a zone" in:
    val exception: IllegalArgumentException = intercept[IllegalArgumentException]:
      Configuration.decode(fixture.replace("America/New_York", "Not/AZone"))
    exception.getMessage should include("zoneId")

  it should "reject byYearDirectoryName" in:
    rejectRetired(s"""${fixture}byYearDirectoryName = "originals"\n""", "byYearDirectoryName")

  it should "reject uploadDirectoryName" in:
    rejectRetired(s"""${fixture}uploadDirectoryName = "Upload"\n""", "uploadDirectoryName")

  it should "reject both retired keys" in:
    val toml: String =
      s"""${fixture}byYearDirectoryName = "originals"
         |uploadDirectoryName = "Upload"
         |""".stripMargin
    rejectRetired(toml, "byYearDirectoryName and uploadDirectoryName")

  private def rejectRetired(toml: String, keys: String): Unit =
    val exception: IllegalArgumentException = intercept[IllegalArgumentException]:
      Configuration.decode(toml)
    exception.getMessage should include(keys)
    exception.getMessage should include("rootPath must be the archive")
