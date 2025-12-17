package org.podval.tools.photo

import java.io.File
import java.time.{Instant, ZoneId}

final class Picture(
  override val parent: Pictures,
  override val name: String,
  val hasMetadata: Boolean,
  val normal: Option[Extension.Descriptor],
  val raw: Option[Extension.Descriptor],
  thumbnail: Option[Extension.Descriptor],
  aux: Option[Extension.Descriptor]
) extends Item:
  require(normal.isDefined || raw.isDefined, s"no images for $this")

  def zone: ZoneId = root.configuration.zoneId

  // Note: image files only, not the metadata
  val extensions: Set[Extension] = Set(normal, raw, thumbnail, aux).flatten.map(Extension(this, _))

  def file(extension: String): File = File(parent.directory, s"$name.$extension")

  def metadataFile: File = file(Metadata.extension)

  lazy val metadata: Metadata = Option.when(hasMetadata)(Metadata.read(metadataFile)).getOrElse(Metadata.default)

  lazy val timestampFromName: Option[(Instant, String)] =
    val name: String = if !this.name.endsWith("_1") then this.name else this.name.substring(0, this.name.length - "_1".length)
              Dates.NameFull   .get(name, zone).map(_ -> "full"   )
      .orElse(Dates.NameCompact.get(name, zone).map(_ -> "compact"))
      .orElse(Dates.NameMillis .get(name, zone).map(_ -> "millis" ))
      .orElse(metadata.date.orElse(parent.day.map(_.date)).flatMap(Dates.NameShort.get(name, zone, _)).map(_ -> "short"))
