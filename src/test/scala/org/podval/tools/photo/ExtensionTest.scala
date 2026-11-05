package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExtensionTest extends AnyFlatSpec, Matchers:
  "Extension" should "identify the new still, video, and TIFF raw descriptors" in:
    check(Extension.JPEG, "jpeg", Dates.StillExif, normal = true, video = false, raw = false, image = false)
    check(Extension.M4V, "m4v", Dates.FFProbe, normal = true, video = true, raw = false, image = false)
    check(Extension.MOV, "mov", Dates.Mov, normal = true, video = true, raw = false, image = false)
    check(Extension.MP4, "mp4", Dates.FFProbe, normal = true, video = true, raw = false, image = false)
    check(Extension.TGP, "3gp", Dates.FFProbe, normal = true, video = true, raw = false, image = false)
    check(Extension.AVI, "avi", Dates.FFProbeAVI, normal = true, video = true, raw = false, image = false)
    check(Extension.NEF, "nef", Dates.StillExif, normal = false, video = false, raw = true, image = false)
    check(Extension.ORF, "orf", Dates.StillExif, normal = false, video = false, raw = true, image = false)
    check(Extension.ARW, "arw", Dates.StillExif, normal = false, video = false, raw = true, image = false)
    check(Extension.RW2, "rw2", Dates.StillExif, normal = false, video = false, raw = true, image = false)
    check(Extension.RWL, "rwl", Dates.StillExif, normal = false, video = false, raw = true, image = false)
    check(Extension.SRW, "srw", Dates.StillExif, normal = false, video = false, raw = true, image = false)
    check(Extension.CR2, "cr2", Dates.StillExif, normal = false, video = false, raw = true, image = false)
    check(Extension.CR3, "cr3", Dates.StillExif, normal = false, video = false, raw = true, image = false)
    check(Extension.CRW, "crw", Dates.DCRaw, normal = false, video = false, raw = true, image = false)
    check(Extension.THM, "thm", Dates.StillExif, normal = false, video = false, raw = false, image = false)
    check(Extension.TIF, "tif", Dates.StillExif, normal = false, video = false, raw = false, image = false)
    Extension.votes(Extension.THM) shouldBe false
    Extension.votes(Extension.TIF) shouldBe false
    Extension.votes(Extension.JPEG) shouldBe true
    Extension.votes(Extension.MOV) shouldBe true
    (Extension.normal -- Extension.video).map(_.name) shouldBe Set("gif", "jpeg", "jpg")
    Extension.all.exists(_.name == "dng") shouldBe false
    Extension.all.exists(_.name == "mkv") shouldBe false

  it should "count heic, heif, avif, png, and webp as images" in:
    check(Extension.HEIC, "heic", Dates.StillExif, normal = false, video = false, raw = false, image = true)
    check(Extension.HEIF, "heif", Dates.StillExif, normal = false, video = false, raw = false, image = true)
    check(Extension.AVIF, "avif", Dates.StillExif, normal = false, video = false, raw = false, image = true)
    check(Extension.PNG, "png", Dates.StillExif, normal = false, video = false, raw = false, image = true)
    check(Extension.WEBP, "webp", Dates.StillExif, normal = false, video = false, raw = false, image = true)
    Extension.image.map(_.name) shouldBe Set("heic", "heif", "avif", "png", "webp")
    Extension.all.exists(_.name == "dng") shouldBe false
    Extension.all.exists(_.name == "mkv") shouldBe false

  private def check(
    descriptor: Extension.Descriptor,
    name: String,
    dater: Dates.Reader,
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
