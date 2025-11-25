package org.podval.tools.toml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.schema.Schema

class TomlCodecTest extends AnyFlatSpec, Matchers:
  "TestCodec" should "handle basic structure" in:
    final case class Test(
      a: Option[String],
      b: Option[Boolean],
      c: String,
      d: Boolean
    )

    val test: Test = Test(
      a = None,
      b = Some(true),
      c = "str",
      d = false
    )

    given Schema[Test] = zio.schema.DeriveSchema.gen
    val encoded: String = TomlCodec.encode(test)
    val decodeResult = TomlCodec.decode(encoded)
    decodeResult.isRight shouldBe true
    val decoded: Test = decodeResult.getOrElse(throw IllegalStateException("can not happen"))
    decoded shouldEqual test

  it should "handle nested tables" in :
    final case class Nested(
      a: Boolean
    )
    final case class Test(
      a: String,
      b: Nested
    )

    val test: Test = Test(
      a = "str",
      b = Nested(a = true)
    )

    given Schema[Test] = zio.schema.DeriveSchema.gen
    val encoded: String = TomlCodec.encode(test)
    val decodeResult = TomlCodec.decode(encoded)
    decodeResult.isRight shouldBe true
    val decoded: Test = decodeResult.getOrElse(throw IllegalStateException("can not happen"))
    decoded shouldEqual test

  it should "handle arrays" in :
    final case class Nested(
      a: Boolean,
      i: Int
    )
    final case class Test(
      map: Map[String, Boolean],
      a: Set[String],
      b: List[Nested]
    )

    val test: Test = Test(
      map = Map("3" -> true, "4" -> false),
      a = Set("str"),
      b = List(Nested(a = true, i = 1), Nested(a = false, i = 2))
    )

    given Schema[Test] = zio.schema.DeriveSchema.gen
    val encoded: String = TomlCodec.encode(test)
    val decodeResult = TomlCodec.decode(encoded)
    decodeResult.isRight shouldBe true
    val decoded: Test = decodeResult.getOrElse(throw IllegalStateException("can not happen"))
    decoded shouldEqual test
