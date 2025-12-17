package org.podval.tools.photo

import java.time.Instant

final class Extension(val picture: Picture, val descriptor: Extension.Descriptor):
  def name: String = descriptor.name
  
  override def toString: String = s"$picture.$name"
  
  lazy val current: Instant = Dates.FromFile.get(picture, name)

  lazy val fromMetadata: Option[Instant] = descriptor.dater.get(picture, name)

object Extension:
  sealed class Descriptor(
    val name: String,
    val dater: Dates.FromFileMetadata
  ):
    override def toString: String = name
  
  object CRW extends Descriptor("crw", Dates.DCRaw)
  object CR2 extends Descriptor("cr2", Dates.MetadataExtractor)
  object CR3 extends Descriptor("cr3", Dates.DCRaw)
  
  object JPG extends Descriptor("jpg", Dates.MetadataExtractor)
  object GIF extends Descriptor("gif", Dates.MetadataExtractor)
  object TIF extends Descriptor("tif", Dates.MetadataExtractor)
  object THM extends Descriptor("thm", Dates.MetadataExtractor)

  object MOV extends Descriptor("mov", Dates.FFProbeZoned)
  object MP4 extends Descriptor("mp4", Dates.FFProbeZoned)
  object AVI extends Descriptor("avi", Dates.FFProbeAVI)
  object TGP extends Descriptor("3gp", Dates.FFProbeZoned)

  val normal: Set[Descriptor] = Set(
    Extension.JPG,
    Extension.GIF,
    Extension.AVI,
    Extension.MOV,
    Extension.MP4,
    Extension.TGP
  )

  val raw: Set[Descriptor] = Set(
    Extension.CRW,
    Extension.CR2,
    Extension.CR3
  )

  val thumbnail: Set[Descriptor] = Set(
    Extension.THM
  )

  val aux: Set[Descriptor] = Set(
    Extension.TIF
  )

  val all: Set[Descriptor] = (
    normal ++
    raw ++
    thumbnail ++
    aux
  )
