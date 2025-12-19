package org.podval.tools.photo

import scala.sys.process.{Process, ProcessBuilder}
import java.io.File

sealed trait RawConverter:
  protected def process(rawFile: File, file: File): ProcessBuilder
  
  final def generate(picture: Picture): Unit = process(
    picture.file(picture.raw.get.name), 
    picture.file(RawConverter.extension)
  ).!

object RawConverter:
  val extension: String = Extension.JPG.name
  
  // Note: on cr3 files, return code of 'dcraw' is non-zero for reasons unknown,
  // but it does print the data, so...
  object DCRaw extends RawConverter:
    override protected def process(rawFile: File, file: File): ProcessBuilder =
      Process(s"dcraw -c $rawFile").#|(Process(s"convert - $file"))

  // Note: coder for crw (dng, digital negative) is not installed by default...
  object ImageMagic extends RawConverter:
    override protected def process(rawFile: File, file: File): ProcessBuilder =
      Process(s"magick $rawFile $file")
