package org.podval.tools.photo

final case class ListDirectories(path: Path, isLong: Boolean) extends Operation:
  override def execute(): Unit = lsd(path)

  private def lsd(path: Path): Unit = path match
    case directory: DirectoryOfNumbered[?, ?, ?, ?] =>
      println(directory.toString)
      if isLong then directory.lsd.foreach(lsd)

    case _ => ()
