package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExtensionTest extends AnyFlatSpec, Matchers:
  "Extension" should "identify the new still, video, and TIFF raw descriptors" in :
    check(Extension.JPEG, "jpeg", Dates.MetadataExtractor, normal = true, video = false, raw = false, image = false)
    check(Extension.M4V, "m4v", Dates.FFProbeZoned, normal = true, video = true, raw = false, image = false)
    check(Extension.NEF, "nef", Dates.MetadataExtractor, normal = false, video = false, raw = true, image = false)
    check(Extension.ORF, "orf", Dates.MetadataExtractor, normal = false, video = false, raw = true, image = false)
    check(Extension.ARW, "arw", Dates.MetadataExtractor, normal = false, video = false, raw = true, image = false)
    check(Extension.RW2, "rw2", Dates.MetadataExtractor, normal = false, video = false, raw = true, image = false)
    check(Extension.RWL, "rwl", Dates.MetadataExtractor, normal = false, video = false, raw = true, image = false)
    check(Extension.SRW, "srw", Dates.MetadataExtractor, normal = false, video = false, raw = true, image = false)
    check(Extension.CR3, "cr3", Dates.DCRaw, normal = false, video = false, raw = true, image = false)
    (Extension.normal -- Extension.video).map(_.name) shouldBe Set("gif", "jpeg", "jpg")
    Extension.all.exists(_.name == "dng") shouldBe false
    Extension.all.exists(_.name == "mkv") shouldBe false

  it should "count heic, heif, avif, png, and webp as images" in :
    check(Extension.HEIC, "heic", Dates.MetadataExtractor, normal = false, video = false, raw = false, image = true)
    check(Extension.HEIF, "heif", Dates.MetadataExtractor, normal = false, video = false, raw = false, image = true)
    check(Extension.AVIF, "avif", Dates.MetadataExtractor, normal = false, video = false, raw = false, image = true)
    check(Extension.PNG, "png", Dates.MetadataExtractor, normal = false, video = false, raw = false, image = true)
    check(Extension.WEBP, "webp", Dates.MetadataExtractor, normal = false, video = false, raw = false, image = true)
    Extension.image.map(_.name) shouldBe Set("heic", "heif", "avif", "png", "webp")
    Seq("heic", "heif", "avif", "png", "webp").foreach: name =>
      Publish.uploadable should not contain name
    Extension.all.exists(_.name == "dng") shouldBe false
    Extension.all.exists(_.name == "mkv") shouldBe false

  private def check(
    descriptor: Extension.Descriptor,
    name: String,
    dater: Dates.FromFileMetadata,
    normal: Boolean,
    video: Boolean,
    raw: Boolean,
    image: Boolean
  ): Unit =
    descriptor.name shouldBe name
    (descriptor.dater eq dater) shouldBe true
    Extension.normal.contains(descriptor) shouldBe normal
    Extension.video.contains(descriptor) shouldBe video
    Extension.raw.contains(descriptor) shouldBe raw
    Extension.image.contains(descriptor) shouldBe image
    Extension.all.contains(descriptor) shouldBe true
