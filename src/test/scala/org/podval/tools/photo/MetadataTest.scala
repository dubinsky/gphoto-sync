package org.podval.tools.photo

import org.podval.tools.toml.TomlCodec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.time.LocalDate

class MetadataTest extends AnyFlatSpec, Matchers:
  private val newKeys: Seq[String] = Seq("orientation", "google-media-item-id", "already-in-google")

  "Metadata" should "decode existing sidecar shapes and encode them without the new keys" in :
    assertLegacy("bad-raw = true", Metadata(`bad-raw` = Some(true)))
    assertLegacy("""timestamp = "2011-06-29T09:45:00"""", Metadata(timestamp = Some("2011-06-29T09:45:00")))
    assertLegacy("date = 2012-04-04", Metadata(date = Some(LocalDate.parse("2012-04-04"))))
    assertLegacy("""source = "scan"""", Metadata(source = Some("scan")))

  it should "round-trip the new optional fields" in :
    val metadata: Metadata = Metadata(
      orientation = Some("Rotate 90 CW"),
      `google-media-item-id` = Some("media-item-1"),
      `already-in-google` = Some(true)
    )
    decode(Metadata.encode(metadata)) shouldBe metadata

  it should "omit the new keys when they are absent" in :
    val encoded: String = Metadata.encode(Metadata())
    newKeys.foreach(key => encoded should not include key)

  private def assertLegacy(toml: String, expected: Metadata): Unit =
    val decoded: Metadata = decode(toml)
    decoded shouldBe expected
    val encoded: String = Metadata.encode(decoded)
    newKeys.foreach(key => encoded should not include key)
    encoded.trim shouldBe toml
    decode(encoded) shouldBe expected

  private def decode(toml: String): Metadata = TomlCodec.decode[Metadata](toml) match
    case Right(value) => value
    case Left(error) => fail(error.toString)
