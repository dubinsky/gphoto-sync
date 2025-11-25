package org.podval.tools.toml

import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.array.TomlArray
import io.github.wasabithumb.jtoml.value.table.TomlTable
import zio.{Chunk, Unsafe}
import zio.schema.Schema
import zio.schema.codec.DecodeError
import scala.jdk.CollectionConverters.MapHasAsScala

// TODO add path
// TODO add error converters
object DirectTomlDecoder:
  def decode[A](bytes: Chunk[Byte], schema: Schema[A]): Either[DecodeError, A] =
    Right(decodeRecord(
      path = Chunk.empty,
      TomlCodec.fromBytes(bytes),
      schema.asInstanceOf[Schema.Record[A]]
    ))

  type Path = Chunk[String]

  private def decodeValue[A](path: Path, value: TomlValue, schema: Schema[A]): A =
    schema match
      case enumeration: Schema.Enum[?] =>
        decodeEnumeration(value, enumeration)
      case primitive: Schema.Primitive[?] =>
        TomlCodec.decodePrimitive(value.asPrimitive, primitive.standardType)
      case record: Schema.Record[?] =>
        decodeRecord(path :+ "record", value.asTable, record)
      case map: Schema.Map[String, ?] =>println
        map.fromChunk(decodeMap(path :+ "map", value.asTable, map.valueSchema))
      case map: Schema.NonEmptyMap[String, ?] =>
        map.fromChunk(decodeMap(path :+ "map", value.asTable, map.valueSchema))
      case set: Schema.Set[?] =>
        decodeArray(path :+ "set", value.asArray, set.elementSchema).toSet
      case sequence: Schema.Sequence[?, ?, ?] =>
        sequence.fromChunk(decodeArray(path :+ "sequence", value.asArray, sequence.elementSchema))
      case sequence: Schema.NonEmptySequence[?, ?, ?] =>
        sequence.fromChunk(decodeArray(path :+ "sequence", value.asArray, sequence.elementSchema))


  private def decodeRecord[R](path: Path, table: TomlTable, record: Schema.Record[R]): R =
    val values: Chunk[Any] = record.fields.map((field: Schema.Field[R, ?]) =>
      val fieldName: String = field.name
      val value: Option[TomlValue] = Option(table.get(fieldName))
      val fieldValue: Any = TomlCodec.eager(field.schema) match
        case optional: Schema.Optional[?] => value.map(decodeValue(path :+ s"field:$fieldName" :+ "Some", _, optional.schema))
        case schema: Schema[?] => decodeValue(path :+ s"field:$fieldName", value.get, schema)

      fieldValue // TODO? TomlCodec.emptyValue(fieldSchema)
    )

    Unsafe.unsafe: _ ?=>
      record.construct(values) match
        case Left(message) => throw IllegalArgumentException(message) // TODO fail(context, message)
        case Right(value) => value

  private def decodeEnumeration[Z](value: TomlValue, schema: Schema.Enum[Z]): Z =
    schema.caseOf(value.asPrimitive.asString).get.asInstanceOf[Schema.Case[Z, Unit]].construct(())

  private def decodeArray[E](path: Path, array: TomlArray, elementSchemaRaw: Schema[E]): Chunk[E] =
    val elementSchema: Schema[E] = TomlCodec.eager(elementSchemaRaw)
    Chunk.fromArray(array.toArray).map(decodeValue(path :+ "[element", _, elementSchema))

  private def decodeMap[V](path: Path, map: TomlTable, valueSchema: Schema[V]): Chunk[(String, V)] =
    Chunk.fromIterable(map.toMap.asScala.toSeq.map((key, value) => key.toString -> decodeValue(path :+ s"element $key", value, valueSchema)))
