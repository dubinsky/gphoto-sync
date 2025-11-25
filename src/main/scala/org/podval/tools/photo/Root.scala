package org.podval.tools.photo

import java.io.File
import java.time.ZoneId

final class Root(
  directoryPath: String,
  val zone: ZoneId
) extends Directory[Root, Int, Year] with DirectoryOfNumbered[Root, Year, Int, Month]:
  override def name: String = ""

  override def toString: String = name

  override protected def getDirectory: File = File(directoryPath)

  override def numberedItemCompanion: NumberedCompanion[Root, Year, Int, Month] = Year

  def parse(path: String): Path =
    val segments: Array[String] = path.split("/").filter(_.nonEmpty)

    def toInt(index: Int) =
      try segments(index).toInt catch
        case e: NumberFormatException =>
          println(s"'${segments(index)}' is not a number")
          throw e

    segments.length match
      case 0 => this
      case 1 => this.item(toInt(0))
      case 2 => this.item(toInt(0)).item(toInt(1))
      case 3 => this.item(toInt(0)).item(toInt(1)).item(toInt(2))
      case 4 => this.item(toInt(0)).item(toInt(1)).item(toInt(2)).item(segments(3))
      case _ => throw IllegalArgumentException(s"'$path' is not a valid path")

  def parsePicture(path: String): Picture = parse(path) match
    case picture: Picture => picture
    case path => throw IllegalArgumentException(s"'$path' is not a picture")

object Root:
  lazy val get: Root =
    val home: String = Option(System.getenv("HOME")).getOrElse(throw IllegalArgumentException(s"no HOME environment variable"))

    // TODO get out of the configuration!
    Root(
      s"$home/Pictures/originals",
      ZoneId.of("America/New_York")
    )
