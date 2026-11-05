package org.podval.tools.photo

import java.io.File
import java.util.UUID

// Argv for a later exiftool copy. Does not run exiftool or modify the source.
object ExifOverride:
  val exiftool: String = "/usr/bin/vendor_perl/exiftool"

  private val videos: Set[String] = Set(
    Extension.MOV.name,
    Extension.MP4.name,
    Extension.AVI.name,
    Extension.TGP.name
  )

  def apply(
    source: File,
    extension: String,
    metadata: Metadata,
    tempDirectory: File = File(System.getProperty("java.io.tmpdir"))
  ): Either[String, (File, Option[Seq[String]])] =
    val hasOverride: Boolean = metadata.timestamp.isDefined || metadata.orientation.isDefined
    if !hasOverride then Right((source, None))
    else
      extension match
        case Extension.JPG.name | Extension.JPEG.name =>
          val copy: File = tempJpeg(tempDirectory)
          Right((copy, Some(argv(source, copy, metadata))))
        case name if videos.contains(name) =>
          video(source, name, metadata, tempDirectory)
        case name =>
          Left(s"metadata override is not supported for $name")

  private def tempJpeg(directory: File): File =
    File(directory, s"${UUID.randomUUID}.jpg")

  // 2026-10-26 spike on this account. QuickTime CreateDate written from the sidecar string
  // 2001-02-03T04:05:06 came back as creationTime 2001-02-03 04:05:06 UTC, the same instant as a
  // JPEG DateTimeOriginal of that string. Keys:CreationDate with a UTC offset came back five hours
  // later and did not finish processing. Rotation=90 never produced width or height, and the API
  // returns no orientation. A video orientation override stays a failure.
  private def video(
    source: File,
    extension: String,
    metadata: Metadata,
    tempDirectory: File
  ): Either[String, (File, Option[Seq[String]])] =
    if metadata.orientation.isDefined then Left(s"orientation override is not supported for video $extension")
    else
      val copy: File = File(tempDirectory, s"${UUID.randomUUID}.$extension")
      Right((copy, Some(videoArgv(source, copy, metadata))))

  private def videoArgv(source: File, copy: File, metadata: Metadata): Seq[String] =
    Seq(exiftool, "-o", copy.getPath) ++
      metadata.timestamp.toSeq.map(timestamp => s"-CreateDate=$timestamp") ++
      Seq(source.getPath)

  private def argv(source: File, copy: File, metadata: Metadata): Seq[String] =
    Seq(exiftool, "-o", copy.getPath) ++
      metadata.timestamp.toSeq.map(timestamp => s"-DateTimeOriginal=$timestamp") ++
      metadata.orientation.toSeq.map(orientation => s"-Orientation=$orientation") ++
      Seq(source.getPath)
