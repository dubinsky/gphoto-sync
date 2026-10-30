package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.time.ZoneId

class ResultTest extends AnyFlatSpec, Matchers:
  // strictEquality does not relate a sealed parent to its cases.
  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  private def session: Session =
    Session(Root(Configuration("Pictures/originals", ZoneId.of("America/New_York"))))

  "a command" should "return stdout with the failure when it already wrote" in :
    val current: Session = session
    val (kept, result) = Operation.run(current):
      Transcript.println("uploaded=0 failed=1")
      throw IllegalArgumentException("publish failed")
    kept should be theSameInstanceAs current
    result match
      case Result.Failed(text, Json.Null, message) =>
        text shouldBe "uploaded=0 failed=1\n"
        message shouldBe "publish failed"
      case other => fail(other.toString)

  it should "return usage when nothing was written" in :
    val current: Session = session
    val (kept, result) = Operation.run(current):
      throw IllegalArgumentException("year or root requires --recursive")
    kept should be theSameInstanceAs current
    result shouldBe Result.Usage("year or root requires --recursive")

  it should "use the class name when the message is missing" in :
    val (_, blank) = Operation.run(session)(throw IllegalArgumentException(""))
    blank shouldBe Result.Usage("IllegalArgumentException")
    val (_, numbered) = Operation.run(session)(throw NumberFormatException())
    numbered shouldBe Result.Usage("NumberFormatException")
    val (_, stated) = Operation.run(session)(throw IllegalStateException("cannot bind the loopback listener"))
    stated shouldBe Result.Usage("cannot bind the loopback listener")

  it should "keep a NumberFormatException message" in :
    val (_, result) = Operation.run(session):
      val _: Int = "Upload".toInt
    result match
      case Result.Usage(message) =>
        message should include ("Upload")
        message should not be "NumberFormatException"
      case other => fail(other.toString)

  it should "store a JSON long" in :
    Json.Long(1L).value shouldBe 1L

  it should "let any other exception abort and not keep its lines" in :
    an [RuntimeException] should be thrownBy Operation.run(session):
      Transcript.println("partial")
      throw RuntimeException("boom")
    val (_, after) = Operation.run(session)(())
    after shouldBe Result.Ok("", Json.Null)
