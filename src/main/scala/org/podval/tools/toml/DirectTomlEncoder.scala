package org.podval.tools.toml

import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.array.TomlArray
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import zio.Chunk
import zio.schema.Schema

object DirectTomlEncoder:
  def encode[A](value: A, schema: Schema[A]): Chunk[Byte] =
    TomlCodec.toBytes(encodeRecord(value, schema.asInstanceOf[Schema.Record[A]]))

  private def encodeValue[A](value: A, schema: Schema[A]): Option[TomlValue] = schema match
    case optional: Schema.Optional[?] => encodeOptional(value.asInstanceOf[Option[A]], optional.asInstanceOf[Schema.Optional[A]].schema)
    case enumeration: Schema.Enum[?] => Some(encodeEnum(value, enumeration.cases))
    case primitive: Schema.Primitive[?] => Some(TomlCodec.encodePrimitive(primitive.standardType, value))
    case record: Schema.Record[?] => Some(encodeRecord(value, record))
    case map: Schema.Map[?, ?] => Some(encodeMap(value.asInstanceOf[Map[?, ?]], map.valueSchema))
    case map: Schema.NonEmptyMap[?, ?] => Some(encodeMap(value.asInstanceOf[Map[?, ?]], map.valueSchema))
    case set: Schema.Set[?] => Some(encodeArray(value.asInstanceOf[IterableOnce[?]], set.elementSchema))
    case sequence: Schema.Sequence[?, ?, ?] => Some(encodeArray(value.asInstanceOf[IterableOnce[?]], sequence.elementSchema))
    case sequence: Schema.NonEmptySequence[?, ?, ?] => Some(encodeArray(value.asInstanceOf[IterableOnce[?]], sequence.elementSchema))

  private def encodeOptional[A](value: Option[A], schema: Schema[A]): Option[TomlValue] = value match
    case None => None
    case Some(value: A) => encodeValue(value, schema)

  private def encodeRecord[R](value: R, record: Schema.Record[R]): TomlTable =
    val result: TomlTable = TomlTable.create

    record.fields.foreach((field: Schema.Field[R, ?]) =>
      encodeValue(field.get(value), TomlCodec.eager(field.schema)).foreach(result.put(field.name, _))
    )

    result

  private def encodeMap[K, V](map: Map[K, V], schemaRaw: Schema[?]): TomlTable =
    val schema: Schema[V] = TomlCodec.eager(schemaRaw.asInstanceOf[Schema[V]])

    val result: TomlTable = TomlTable.create

    map.foreach( (key, value) =>
      encodeValue(value, schema).foreach(result.put(key.toString, _))
    )

    result

  private def encodeEnum[Z](value: Z, cases: Chunk[Schema.Case[Z, ?]]): TomlPrimitive =
    TomlPrimitive.of(cases.find(_.isCase(value)).get.caseName)

  private def encodeArray[A](value: IterableOnce[A], schemaRaw: Schema[?]): TomlArray =
    val schema: Schema[A] = TomlCodec.eager(schemaRaw.asInstanceOf[Schema[A]])
    val result: TomlArray = TomlArray.create
    value.iterator.foreach((element: A) => encodeValue(element, schema).foreach(result.add))
    result
