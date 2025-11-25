package org.podval.tools.photo

import java.io.File
import java.time.{Instant, ZoneId}

final class Picture private(
  day: Day,
  override val name: String,
  val hasMetadata: Boolean,
  val normal: Option[Extension.Descriptor],
  val raw: Option[Extension.Descriptor],
  thumbnail: Option[Extension.Descriptor],
  aux: Option[Extension.Descriptor]
) extends Sub[Day, String, Picture]:
  require(normal.isDefined || raw.isDefined, s"no images for $this")

  override def parent: Day = day

  def root: Root = parent.parent.parent.parent

  def zone: ZoneId = root.zone

  // Note: image files only, not the metadata
  val extensions: Set[Extension] = Set(normal, raw, thumbnail, aux).flatten.map(Extension(this, _))

  def file(extension: String): File = File(parent.directory, s"$name.$extension")

  def metadataFile: File = file(Metadata.extension)

  lazy val metadata: Metadata = Option.when(hasMetadata)(Metadata.parse(metadataFile)).getOrElse(Metadata.default)

  lazy val timestampFromName: Option[(Instant, String)] =
    val name: String = if !this.name.endsWith("_1") then this.name else this.name.substring(0, this.name.length - "_1".length)
              Dates.NameFull   .get(name, zone     ).map(_ -> "full"   )
      .orElse(Dates.NameCompact.get(name, zone     ).map(_ -> "compact"))
      .orElse(Dates.NameShort  .get(name, zone, day).map(_ -> "short"  ))
      .orElse(Dates.NameMillis .get(name, zone     ).map(_ -> "millis" ))

object Picture:
  def apply(
    day: Day,
    name: String,
    extensions: Set[String]
  ): Picture =
    val nameFull: String = s"$day/$name"

    require(name.head.isDigit, s"Non-digit name start: $nameFull")
    require(extensions.nonEmpty, s"No files for $nameFull")

    extensions.foreach(Picture.verifyExtension(nameFull, _))

    def noMoreThanOne(from: Set[Extension.Descriptor], what: String): Option[Extension.Descriptor] =
      val all: Set[Extension.Descriptor] = from.filter(extension => extensions.contains(extension.name))
      require(all.size <= 1, s"too many $what extensions for $nameFull: $all")
      all.headOption

    new Picture(
      day,
      name,
      hasMetadata = extensions.contains(Metadata.extension),
      normal    = noMoreThanOne(extensionsNormal   , "normal"   ),
      raw       = noMoreThanOne(extensionsRaw      , "raw"      ),
      thumbnail = noMoreThanOne(extensionsThumbnail, "thumbnail"),
      aux       = noMoreThanOne(extensionsAux      , "aux"      )
    )

  private val extensionsNormal: Set[Extension.Descriptor] = Set(
    Extension.JPG,
    Extension.GIF,
    Extension.AVI,
    Extension.MOV,
    Extension.MP4,
    Extension.TGP
  )

  private val extensionsRaw: Set[Extension.Descriptor] = Set(
    Extension.CRW,
    Extension.CR2,
    Extension.CR3
  )

  private val extensionsThumbnail: Set[Extension.Descriptor] = Set(
    Extension.THM
  )

  private val extensionsAux: Set[Extension.Descriptor] = Set(
    Extension.TIF
  )

  private val extensions: Set[String] = (
    extensionsNormal ++
    extensionsRaw ++
    extensionsThumbnail ++
    extensionsAux
  ).map(_.name) ++ Set(
    Metadata.extension
  )

  def verifyExtension(name: String, extension: String): Unit =
    if !extensions.contains(extension) then throw IllegalArgumentException(s"$name: illegal extension $extension")
