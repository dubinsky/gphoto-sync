package org.podval.tools.toml

import zio.Chunk
import zio.schema.{Schema, StandardType}
import zio.schema.codec.{BinaryCodec, DecodeError}
import zio.stream.ZPipeline

object TomlCodec:
  given [A] => (schema: Schema[A]) => BinaryCodec[A] = new BinaryCodec[A]:
    override def decode(bytes: Chunk[Byte]): Either[DecodeError, A] = TomlDecoder.decode(bytes, schema)

    override def encode(value: A): Chunk[Byte] = TomlEncoder.encode(value, schema)

    // I do not need to stream TOML :)
    override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] = ???
    override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] = ???

  def encode[A: Schema](value: A): String = summon[BinaryCodec[A]].encode(value).asString

  def decode[A: Schema](string: String): Either[DecodeError, A] = summon[BinaryCodec[A]].decode(Chunk.fromArray(string.getBytes))

  def eager[A](schema: Schema[A]): Schema[A] = schema match
    case lzy: Schema.Lazy[?] => lzy.schema
    case schema => schema

  private given CanEqual[StandardType[?], StandardType[?]] = CanEqual.derived

  def isString(schema: Schema[?]): Boolean = schema match
    case Schema.Primitive(StandardType.StringType, _) => true
    case _ => false

  val caseNameKey: String = "__CASE_NAME__"