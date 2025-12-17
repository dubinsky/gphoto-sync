package org.podval.tools.toml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.prelude.NonEmptyMap
import zio.schema.Schema

class TomlCodecTest extends AnyFlatSpec, Matchers:
  "TestCodec" should "work" in :
    final case class Test(
      stringOption: Option[String],
      booleanOption: Option[Boolean],
      string: String,
      boolean: Boolean,
      nested: Nested,
      map: Map[String, Boolean],
      nonEmptyMap: NonEmptyMap[String, Int],
      set: Set[String],
      list: List[Nested],
      e1: E,
      e2: E
    )
    final case class Nested(
      boolean: Boolean,
      int: Int
    )
    sealed trait E
    object E:
      final case class C1(s: String) extends E
      case object C2 extends E

    val test: Test = Test(
      stringOption = None,
      booleanOption = Some(true),
      string = "str",
      boolean = false,
      nested = Nested(boolean = true, int = 0),
      map = Map(
        "3" -> true,
        "4" -> false
      ),
      nonEmptyMap = NonEmptyMap(
        "key" -> 42
      ),
      set = Set(
        "str"
      ),
      list = List(
        Nested(boolean = true, int = 1),
        Nested(boolean = false, int = 2)
      ),
      e1 = E.C1("blah"),
      e2 = E.C2
    )

    given Schema[Test] = zio.schema.DeriveSchema.gen
    val encoded: String = TomlCodec.encode(test)
    println(encoded)
    val decodeResult = TomlCodec.decode(encoded)
    if decodeResult.isLeft then println(decodeResult)
    decodeResult.isRight shouldBe true
    val decoded: Test = decodeResult.getOrElse(throw IllegalStateException("can not happen"))
    decoded shouldEqual test
