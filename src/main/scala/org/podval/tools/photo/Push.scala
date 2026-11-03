package org.podval.tools.photo

private[photo] final case class Push(dryRun: Boolean) extends Operation:
  def execute(session: Session): (Session, Result) =
    AgentCommands.push(session, dryRun)
