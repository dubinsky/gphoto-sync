package org.podval.tools.photo

final case class Session(root: Root)

object Session:
  def start: Session = Session(Root(Configuration.read))
