package org.podval.tools.photo

trait Operation:
  def execute(session: Session): (Session, Result)

object Operation:
  // `value` is by-name so the body can assign it. Ok and Failed read it after the body.
  // Usage returns the session passed in. Ok and Failed return index edits made by the body.
  // end only restores the caller; it does not drop.
  def run(session: Session, value: => Json = Json.Null)(body: => Unit): (Session, Result) =
    val previous: Option[DirectoryIndex.Frame] = DirectoryIndex.begin(session)
    try
      val (text, thrown) = Transcript.collect(body)
      val edited: Session = DirectoryIndex.now
      thrown match
        case None =>
          (edited, Result.Ok(text, value))
        case Some(error: (IllegalArgumentException | IllegalStateException)) =>
          val message: String = messageOf(error)
          if text.isEmpty then (session, Result.Usage(message))
          else (edited, Result.Failed(text, value, message))
        case Some(error) =>
          throw error
    finally DirectoryIndex.end(previous)

  def messageOf(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)

// Lines of the command that is running. enact and scan have no other channel.
private[photo] object Transcript:
  private val current = new ThreadLocal[StringBuilder]

  def println(line: String): Unit =
    Option(current.get).foreach: buffer =>
      buffer.append(line)
      buffer.append('\n')

  def collect(body: => Unit): (String, Option[Throwable]) =
    val buffer = new StringBuilder
    current.set(buffer)
    val thrown: Option[Throwable] =
      try
        body
        None
      catch case error: Throwable => Some(error)
      finally current.remove()
    (buffer.toString, thrown)
