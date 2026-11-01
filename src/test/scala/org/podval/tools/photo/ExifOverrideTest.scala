package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File

class ExifOverrideTest extends AnyFlatSpec, Matchers:
  private val source: File = File("/tmp/gphoto-sync-exif-override/photo.jpg")
  private val stamp: String = "2011-06-29T09:45:00"
  private val orientation: String = "Rotate 90 CW"
  private val exiftool: String = "/usr/bin/vendor_perl/exiftool"

  "ExifOverride" should "build a JPEG argv for a jpeg suffix timestamp" in :
    val file: File = File("/tmp/gphoto-sync-exif-override/photo.jpeg")
    ExifOverride(file, "jpeg", Metadata(timestamp = Some(stamp))) match
      case Right((copy, Some(argv))) =>
        argv.head shouldBe exiftool
        has(argv, "-o")
        has(argv, copy.getPath)
        has(argv, s"-DateTimeOriginal=$stamp")
        has(argv, file.getPath)
        argv.exists(_.startsWith("-Orientation")) shouldBe false
        argv.exists(_.startsWith("-CreateDate")) shouldBe false
        copy.getName.endsWith(".jpg") shouldBe true
        copy.getName.endsWith(".jpeg") shouldBe false
        copy.getPath should not be file.getPath
      case other => fail(s"expected a JPEG temp copy, got $other")

  it should "build a JPEG argv for a timestamp only" in :
    val prepared: (File, Seq[String]) = jpeg(Metadata(timestamp = Some(stamp)))
    val copy: File = prepared._1
    val argv: Seq[String] = prepared._2
    argv.head shouldBe exiftool
    has(argv, "-o")
    has(argv, copy.getPath)
    has(argv, s"-DateTimeOriginal=$stamp")
    has(argv, source.getPath)
    argv.exists(_.startsWith("-Orientation")) shouldBe false
    copy.getPath should not be source.getPath

  it should "build a JPEG argv for an orientation only" in :
    val argv: Seq[String] = jpeg(Metadata(orientation = Some(orientation)))._2
    has(argv, s"-Orientation=$orientation")
    argv.exists(_.startsWith("-DateTimeOriginal")) shouldBe false

  it should "include both JPEG flags when timestamp and orientation are set" in :
    val argv: Seq[String] = jpeg(Metadata(timestamp = Some(stamp), orientation = Some(orientation)))._2
    has(argv, s"-DateTimeOriginal=$stamp")
    has(argv, s"-Orientation=$orientation")

  it should "keep a JPEG with no override on the original file and no argv" in :
    assertUnchanged(source, "jpg", Metadata())

  it should "keep a video with no override on the original file and no argv" in :
    val video: File = File("/tmp/gphoto-sync-exif-override/clip.mov")
    assertUnchanged(video, "mov", Metadata())

  it should "build a video argv for a timestamp only" in :
    val video: File = File("/tmp/gphoto-sync-exif-override/clip.mp4")
    ExifOverride(video, "mp4", Metadata(timestamp = Some(stamp))) match
      case Right((copy, Some(argv))) =>
        argv.head shouldBe exiftool
        has(argv, "-o")
        has(argv, copy.getPath)
        has(argv, s"-CreateDate=$stamp")
        has(argv, video.getPath)
        argv.exists(_.startsWith("-DateTimeOriginal")) shouldBe false
        argv.exists(_.startsWith("-Orientation")) shouldBe false
        argv.exists(_.startsWith("-Rotation")) shouldBe false
        copy.getName.endsWith(".mp4") shouldBe true
        copy.getPath should not be video.getPath
      case other => fail(s"expected a video temp copy, got $other")

  it should "reject a video orientation override" in :
    assertRejected("mov", Metadata(orientation = Some(orientation)))
    assertRejected("mp4", Metadata(timestamp = Some(stamp), orientation = Some(orientation)))

  it should "reject a non-jpeg override" in :
    Seq("gif", "tif", "thm", "cr2").foreach(extension => assertRejected(extension, Metadata(timestamp = Some(stamp))))

  private def has(argv: Seq[String], argument: String): Unit =
    argv.exists(_ == argument) shouldBe true

  private def jpeg(metadata: Metadata): (File, Seq[String]) =
    ExifOverride(source, "jpg", metadata) match
      case Right((copy, Some(argv))) =>
        argv.sliding(2).exists(_ == Seq("-o", copy.getPath)) shouldBe true
        has(argv, source.getPath)
        copy.getPath should not be source.getPath
        (copy, argv)
      case other => fail(s"expected a JPEG temp copy, got $other")

  private def assertUnchanged(file: File, extension: String, metadata: Metadata): Unit =
    ExifOverride(file, extension, metadata) match
      case Right((result, argv)) =>
        result.getPath shouldBe file.getPath
        argv.isDefined shouldBe false
      case Left(message) => fail(message)

  private def assertRejected(extension: String, metadata: Metadata): Unit =
    val file: File = File(s"/tmp/gphoto-sync-exif-override/clip.$extension")
    ExifOverride(file, extension, metadata) match
      case Left(message) =>
        message.contains(extension) shouldBe true
        message.contains(exiftool) shouldBe false
      case Right(value) => fail(s"expected an error for $extension, got $value")
