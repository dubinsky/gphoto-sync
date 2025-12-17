package org.podval.tools.photo

import java.io.File

trait Directory extends Path:
  protected def getDirectory: File

  final lazy val directory: File =
    val result: File = getDirectory
    require(!result.exists || result.isDirectory, s"'$result' is not a directory")
    result

  def lsd: Seq[Directory]

  def item(name: String): Path

  final def item(segments: List[String]): Path = segments match
    case Nil => this
    case segment :: segments => item(segment) match
      case directory: Directory => directory.item(segments)
      case item => segments match
        case Nil => item
        case segments => throw IllegalArgumentException(s"Item does not have sub-items: $item.item($segments)")
