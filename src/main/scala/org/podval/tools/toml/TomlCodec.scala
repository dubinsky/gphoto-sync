package org.podval.tools.toml

import zio.Chunk
import zio.schema.Schema
import zio.schema.codec.{BinaryCodec, DecodeError}
import zio.stream.ZPipeline

/*
 My attempt to implement a subset of the TOML specification https://toml.io/en/v1.0.0 using ZIO Schema.

 Motivation: I started offf with the https://github.com/indoorvivants/toml-scala library;
 turns out, it can *parse* a string into a case class -
 but does not seem to be able to *write* a case class to string
 (see https://github.com/indoorvivants/toml-scala/issues/10).

 To write TOML using it, one has to hand-convert it to the `toml-scala` AST:
    val pairs: List[Pair] = List(
      metadata.source.map(source => Pair("source", Str(source))),
      metadata.timestamp.map(timestamp => Pair("timestamp", Str(timestamp))),
      Option.when(metadata.`bad-raw`.contains(true))(Pair("bad-raw", Bool(true))),
    )
      .flatten

    toml.Toml.generate(Root(pairs))

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

 Note: inspired by
 - zio.schema.codec.ThriftCodec
 - https://github.com/indoorvivants/toml-scala
*/
object TomlCodec:
  def encode[A: Schema](value: A): String = summon[BinaryCodec[A]].encode(value).asString
  def decode[A: Schema](string: String): Either[DecodeError, A] = summon[BinaryCodec[A]].decode(Chunk.fromArray(string.getBytes))

  given [A] => (schema: Schema[A]) => BinaryCodec[A] = new BinaryCodec[A]:
    override def decode(bytes: Chunk[Byte]): Either[DecodeError, A] = TomlDecoder.decode(bytes, schema)

    override def encode(value: A): Chunk[Byte] = TomlEncoder.encode(value, schema)

    // I do not need to stream TOML :)
    override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] = ???
    override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] = ???

  def eager[A](schema: Schema[A]): Schema[A] = schema match
    case lzy: Schema.Lazy[?] => lzy.schema
    case schema => schema

//  @tailrec
//  def emptyValue[A](schema: Schema[A]): Option[A] = schema match
//    case Schema.Lazy(s) => emptyValue(s())
//    case Schema.Optional(_, _) => Some(None)
//    case Schema.Sequence(_, fromChunk, _, _, _) => Some(fromChunk(Chunk.empty))
//    case Schema.Primitive(StandardType.UnitType, _) => Some(())
//    case _ => None
