package org.podval.tools.photo

import java.time.Instant

final class Extension(val picture: Picture, val descriptor: Extension.Descriptor):
  Extension.constructed += 1

  def name: String = descriptor.name
  
  override def toString: String = s"$picture.$name"
  
  lazy val current: Instant = Dates.FromFile.get(picture, name)

  lazy val fromMetadata: Option[Instant] = descriptor.dater.get(picture, name)

object Extension:
  private[photo] var constructed: Long = 0

  sealed class Descriptor(
    val name: String,
    val dater: Dates.FromFileMetadata
  ):
    override def toString: String = name
  
  object CRW extends Descriptor("crw", Dates.DCRaw)
  object CR2 extends Descriptor("cr2", Dates.MetadataExtractor)
  object CR3 extends Descriptor("cr3", Dates.DCRaw)
  object NEF extends Descriptor("nef", Dates.MetadataExtractor)
  object ORF extends Descriptor("orf", Dates.MetadataExtractor)
  object ARW extends Descriptor("arw", Dates.MetadataExtractor)
  object RW2 extends Descriptor("rw2", Dates.MetadataExtractor)
  object RWL extends Descriptor("rwl", Dates.MetadataExtractor)
  object SRW extends Descriptor("srw", Dates.MetadataExtractor)

  object JPG extends Descriptor("jpg", Dates.MetadataExtractor)
  object JPEG extends Descriptor("jpeg", Dates.MetadataExtractor)
  object GIF extends Descriptor("gif", Dates.MetadataExtractor)
  object TIF extends Descriptor("tif", Dates.MetadataExtractor)
  object THM extends Descriptor("thm", Dates.MetadataExtractor)

  object MOV extends Descriptor("mov", Dates.FFProbeZoned)
  object MP4 extends Descriptor("mp4", Dates.FFProbeZoned)
  object M4V extends Descriptor("m4v", Dates.FFProbeZoned)
  object AVI extends Descriptor("avi", Dates.FFProbeAVI)
  object TGP extends Descriptor("3gp", Dates.FFProbeZoned)

  val video: Set[Descriptor] = Set(
    Extension.MOV,
    Extension.MP4,
    Extension.M4V,
    Extension.AVI,
    Extension.TGP
  )

  val normal: Set[Descriptor] = Set(
    Extension.JPG,
    Extension.JPEG,
    Extension.GIF
  ) ++ video

  val raw: Set[Descriptor] = Set(
    Extension.CRW,
    Extension.CR2,
    Extension.CR3,
    Extension.NEF,
    Extension.ORF,
    Extension.ARW,
    Extension.RW2,
    Extension.RWL,
    Extension.SRW
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
