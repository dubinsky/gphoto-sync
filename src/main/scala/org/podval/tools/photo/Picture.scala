package org.podval.tools.photo

import java.io.File
import java.time.ZoneId

final class Picture(
    override val parent: Pictures,
    override val name: String,
    val hasMetadata: Boolean,
    val normal: Option[Extension.Descriptor],
    val raw: Option[Extension.Descriptor],
    val image: Option[Extension.Descriptor],
    thumbnail: Option[Extension.Descriptor],
    aux: Option[Extension.Descriptor]
) extends Item:
  require(normal.isDefined || raw.isDefined || image.isDefined, s"no images for $this")

  def zone: ZoneId = root.configuration.zoneId

  // Note: image files only, not the metadata
  val extensions: Set[Extension] = Set(normal, raw, image, thumbnail, aux).flatten.map(Extension(this, _))

  def file(extension: String): File = File(parent.directory, s"$name.$extension")

  def metadataFile: File = file(Metadata.extension)

  lazy val metadata: Metadata = Option.when(hasMetadata)(Metadata.read(metadataFile)).getOrElse(Metadata.default)

  // Sidecar date is a short-name input here. Claims passes only the folder day.
  lazy val timestampFromName: Option[(Dates.Clock, String)] =
    Dates.fromFileName(name, zone, metadata.date.orElse(parent.day.map(_.date)))
