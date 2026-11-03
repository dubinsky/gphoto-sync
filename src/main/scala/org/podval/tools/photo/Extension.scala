package org.podval.tools.photo

import java.time.Instant

final class Extension(val picture: Picture, val descriptor: Extension.Descriptor):
  Extension.constructed += 1

  def name: String = descriptor.name
  
  override def toString: String = s"$picture.$name"
  
  lazy val current: Instant = Dates.FromFile.get(picture, name)

  lazy val read: Option[Dates.Clock] =
    descriptor.dater.read(
      name,
      picture.file(name),
      Dates.usable(picture.timestampFromName.map(_._1)),
      picture.zone
    )

  def votes: Boolean = Extension.votes(descriptor)

object Extension:
  private[photo] var constructed: Long = 0

  sealed class Descriptor(
    val name: String,
    val dater: Dates.Reader
  ):
    override def toString: String = name

  // Thumbnail and .tif are read and do not supply the day.
  def votes(descriptor: Descriptor): Boolean =
    !thumbnail.contains(descriptor) && !aux.contains(descriptor)

  def votes(name: String): Boolean =
    all.find(_.name == name).exists(votes)

  object CRW extends Descriptor("crw", Dates.DCRaw)
  object CR2 extends Descriptor("cr2", Dates.StillExif)
  object CR3 extends Descriptor("cr3", Dates.StillExif)
  object NEF extends Descriptor("nef", Dates.StillExif)
  object ORF extends Descriptor("orf", Dates.StillExif)
  object ARW extends Descriptor("arw", Dates.StillExif)
  object RW2 extends Descriptor("rw2", Dates.StillExif)
  object RWL extends Descriptor("rwl", Dates.StillExif)
  object SRW extends Descriptor("srw", Dates.StillExif)

  object JPG extends Descriptor("jpg", Dates.StillExif)
  object JPEG extends Descriptor("jpeg", Dates.StillExif)
  object GIF extends Descriptor("gif", Dates.StillExif)
  object TIF extends Descriptor("tif", Dates.StillExif)
  object THM extends Descriptor("thm", Dates.StillExif)

  object HEIC extends Descriptor("heic", Dates.StillExif)
  object HEIF extends Descriptor("heif", Dates.StillExif)
  object AVIF extends Descriptor("avif", Dates.StillExif)
  object PNG extends Descriptor("png", Dates.StillExif)
  object WEBP extends Descriptor("webp", Dates.StillExif)

  object MOV extends Descriptor("mov", Dates.Mov)
  object MP4 extends Descriptor("mp4", Dates.FFProbe)
  object M4V extends Descriptor("m4v", Dates.FFProbe)
  object AVI extends Descriptor("avi", Dates.FFProbeAVI)
  object TGP extends Descriptor("3gp", Dates.FFProbe)

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

  val image: Set[Descriptor] = Set(
    Extension.HEIC,
    Extension.HEIF,
    Extension.AVIF,
    Extension.PNG,
    Extension.WEBP
  )

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
    image ++
    raw ++
    thumbnail ++
    aux
  )
