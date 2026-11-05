package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.time.LocalDate

class MetadataTest extends AnyFlatSpec, Matchers:
  private val newKeys: Seq[String] = Seq("orientation", "google-media-item-id", "already-in-google", "uploader")

  "Metadata" should "decode existing sidecar shapes and encode them without the new keys" in:
    assertLegacy("bad-raw = true", Metadata(`bad-raw` = Some(true)))
    assertLegacy("""timestamp = "2011-06-29T09:45:00"""", Metadata(timestamp = Some("2011-06-29T09:45:00")))
    assertLegacy("date = 2012-04-04", Metadata(date = Some(LocalDate.parse("2012-04-04"))))
    assertLegacy("""source = "scan"""", Metadata(source = Some("scan")))

  it should "round-trip the new optional fields" in:
    val metadata: Metadata = Metadata(
      orientation = Some("Rotate 90 CW"),
      `google-media-item-id` = Some("media-item-1"),
      `already-in-google` = Some(true)
    )
    decode(Metadata.encode(metadata)) shouldBe metadata

  it should "omit absent keys" in:
    val encoded: String = Metadata.encode(Metadata())
    Property.all.map(_.name).foreach(key => encoded should not include key)

  it should "round-trip a fully populated value" in:
    val metadata: Metadata = Metadata(
      source = Some("scan"),
      timestamp = Some("2011-06-29T09:45:00"),
      date = Some(LocalDate.parse("2012-04-04")),
      `bad-raw` = Some(true),
      orientation = Some("Rotate 90 CW"),
      `google-media-item-id` = Some("media-item-1"),
      `already-in-google` = Some(true),
      uploader = Some(" Ada ")
    )
    val encoded: String = Metadata.encode(metadata)
    Property.all.map(_.name).foreach(name => encoded should include(name))
    decode(encoded) shouldBe metadata

  it should "round-trip a set uploader and reject a blank or controlled one" in:
    val metadata: Metadata = Metadata(uploader = Some("Ada Owner"))
    decode(Metadata.encode(metadata)) shouldBe metadata
    val blank: IllegalArgumentException = the[IllegalArgumentException] thrownBy decode("uploader = \"   \"")
    blank.getMessage shouldBe "key 'uploader' must be a non-empty display name"
    // jtoml rejects a basic empty string that is not followed by a newline.
    val empty: IllegalArgumentException = the[IllegalArgumentException] thrownBy decode("uploader = \"\"\n")
    empty.getMessage shouldBe "key 'uploader' must be a non-empty display name"
    val notString: IllegalArgumentException = the[IllegalArgumentException] thrownBy decode("uploader = true")
    notString.getMessage shouldBe "key 'uploader' must be a string"
    Seq("\\t", "\\r", "\\n").foreach: escape =>
      val caught: IllegalArgumentException =
        the[IllegalArgumentException] thrownBy decode(s"uploader = \"Ada${escape}Owner\"")
      caught.getMessage shouldBe "key 'uploader' must not contain a tab, CR, or LF"

  it should "ignore an unknown key and drop it on write" in:
    val decoded: Metadata = decode("nope = true\nbad-raw = true")
    decoded shouldBe Metadata(`bad-raw` = Some(true))
    val encoded: String = Metadata.encode(decoded)
    encoded.trim shouldBe "bad-raw = true"
    encoded should not include "nope"

  it should "encode false" in:
    val encoded: String = Metadata.encode(Metadata(`already-in-google` = Some(false)))
    encoded should include("already-in-google = false")

  it should "reject a string where a boolean is required" in:
    val caught: IllegalArgumentException = the[IllegalArgumentException] thrownBy decode("""bad-raw = "false"""")
    caught.getMessage should include("bad-raw")

  private def assertLegacy(toml: String, expected: Metadata): Unit =
    val decoded: Metadata = decode(toml)
    decoded shouldBe expected
    val encoded: String = Metadata.encode(decoded)
    newKeys.foreach(key => encoded should not include key)
    encoded.trim shouldBe toml
    decode(encoded) shouldBe expected

  private def decode(toml: String): Metadata = Metadata.decode(toml)
