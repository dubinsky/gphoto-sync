package org.podval.tools.toml

import zio.Chunk
import zio.schema.{Schema, StandardType}
import zio.schema.codec.{BinaryCodec, DecodeError}
import zio.stream.ZPipeline

// TODO handle Toml encoding/decoding configuration
// TODO take care of streaming
// TODO encode and decode tuples to and from Toml tables?
// TODO decode Toml arrays to records?
// TODO is there some miraculous way to preserve comments for the round trip (annotations)?
// TODO productize the codec (rewrite in Scala 2 etc.) and submit a pull request to ZIO Schema?
/*
 My attempt to implement a subset of the TOML specification https://toml.io/en/v1.0.0 using ZIO Schema.

 Motivation: I started off with the https://github.com/indoorvivants/toml-scala library;
 turns out, it can *parse* a string into a case class but is not able to *write* a case class to string
 (see https://github.com/indoorvivants/toml-scala/issues/10):
 to write TOML using it, one has to hand-convert it to the `toml-scala` AST;
 I do not want to do that ;)

 There is no official TOML codec for ZIO Schema, so I had to make my own.

 I *really* do not want to deal directly with the TOML syntax;
 like ZIO Schema codecs for other formats do, I want to delegate reading and writing
 to a well-maintained library that supports current version of TOML;
 my choices seem to be (including the libraries listed in https://github.com/toml-lang/toml/wiki#implementations):

 - https://github.com/indoorvivants/toml-scala
 - https://github.com/FasterXML/jackson-dataformats-text
 - https://github.com/TheElectronWill/Night-Config
 - https://github.com/tomlj/tomlj
 - https://github.com/WasabiThumb/jtoml

 Let's try WasabiThumb/jtoml: it claims latest TOML compatibility, compares itself favorably to a number of other
 TOML implementation and exposes a document model which can be parsed into and written from.
*/
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
  