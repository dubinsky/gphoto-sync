package org.podval.tools.photo

final case class ListDirectories(path: String, isLong: Boolean) extends Operation:
  override def execute(session: Session): (Session, Result) =
    Operation.run(session):
      lsd(session.parse(path), listItemDirectories = true)

  private def lsd(path: Path, listItemDirectories: Boolean): Unit = path match
    case directory: Directory =>
      Transcript.println(directory.toString)
      if listItemDirectories then directory.lsd.foreach(lsd(_, isLong))

    case _ => ()
