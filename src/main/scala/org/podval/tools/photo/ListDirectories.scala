package org.podval.tools.photo

final case class ListDirectories(path: Path, isLong: Boolean) extends Operation:
  override def execute(): Unit = lsd(path, listItemDirectories = true)

  private def lsd(path: Path, listItemDirectories: Boolean): Unit = path match
    case directory: Directory =>
      println(directory.toString)
      if listItemDirectories then directory.lsd.foreach(lsd(_, isLong))

    case _ => ()
