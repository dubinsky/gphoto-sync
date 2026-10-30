package org.podval.tools.photo

trait Operation:
  def execute(session: Session): (Session, Result)

object Operation:
  def run(session: Session)(body: => Unit): (Session, Result) =
    Transcript.bracket: transcript =>
      try
        body
        (session, Result.Ok(transcript.text, Json.Null))
      catch
        case error: (IllegalArgumentException | IllegalStateException) =>
          val message: String = messageOf(error)
          if transcript.isEmpty then (session, Result.Usage(message))
          else (session, Result.Failed(transcript.text, Json.Null, message))

  def messageOf(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)

// Lines of the command that is running. enact and scan have no other channel.
private[photo] final class Transcript:
  private val buffer = new StringBuilder

  def println(line: String): Unit =
    buffer.append(line)
    buffer.append('\n')

  def text: String = buffer.toString

  def isEmpty: Boolean = buffer.isEmpty

private[photo] object Transcript:
  private val current = new ThreadLocal[Transcript]

  def println(line: String): Unit =
    Option(current.get).foreach(_.println(line))

  def collect(body: => Unit): (String, Option[Throwable]) =
    bracket: transcript =>
      val thrown: Option[Throwable] =
        try
          body
          None
        catch case error: Throwable => Some(error)
      (transcript.text, thrown)

  def text(body: => Unit): String =
    val (out, thrown) = collect(body)
    thrown match
      case Some(error) => throw error
      case None => out

  def bracket[A](body: Transcript => A): A =
    val previous: Option[Transcript] = Option(current.get)
    val transcript: Transcript = previous.getOrElse(new Transcript)
    if previous.isEmpty then current.set(transcript)
    try body(transcript)
    finally if previous.isEmpty then current.remove()
