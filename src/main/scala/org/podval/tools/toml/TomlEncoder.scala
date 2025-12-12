package org.podval.tools.toml

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.array.TomlArray
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import zio.Chunk
import zio.schema.{Schema, StandardType}
import scala.util.control.NonFatal
import java.math.{BigDecimal, BigInteger}
import java.nio.charset.StandardCharsets
import java.time.{DayOfWeek, Duration, Instant, LocalDate, LocalDateTime, LocalTime, Month, MonthDay, OffsetDateTime,
  OffsetTime, Period, Year, YearMonth, ZoneId, ZoneOffset, ZonedDateTime}
import java.util.{Currency, UUID}

object TomlEncoder:
  def encode[A](value: A, schema: Schema[A]): Chunk[Byte] =
    val result: TomlTable = encodeRecord(value, schema.asInstanceOf[Schema.Record[A]])
    Chunk.fromArray(JToml.jToml.writeToString(result).getBytes(StandardCharsets.UTF_8))

  private def run[A](a: => A): A =
    try a catch
      case NonFatal(e) => fail(e.getMessage)

  private def fail(message: String): Nothing =
    throw IllegalArgumentException(s"Encoding error: $message")
    
  private def encodeValue[A](value: A, schema: Schema[A]): TomlValue = schema match
    case enumeration: Schema.Enum[?] =>
      encodeEnum(value, enumeration.cases)
    case primitive: Schema.Primitive[?] =>
      encodePrimitive(value, primitive.standardType)
    case record: Schema.Record[?] =>
      encodeRecord(value, record)
    case map: Schema.Map[?, ?] =>
      encodeMap(value, map.keySchema, map.valueSchema, map)
    case map: Schema.NonEmptyMap[?, ?] =>
      encodeMap(value, map.keySchema, map.valueSchema, map)
    case set: Schema.Set[?] =>
      encodeArray(value, set.elementSchema)
    case sequence: Schema.Sequence[?, ?, ?] =>
      encodeArray(value, sequence.elementSchema)
    case sequence: Schema.NonEmptySequence[?, ?, ?] =>
      encodeArray(value, sequence.elementSchema)
    case schema =>
      fail(s"Unsupported: $schema")

  private def encodeRecord[R](value: R, record: Schema.Record[R]): TomlTable =
    val result: TomlTable = TomlTable.create

    record.fields.foreach((field: Schema.Field[R, ?]) =>
      val fieldValue = field.get(value)
      val encoded: Option[TomlValue] = TomlCodec.eager(field.schema) match
        case optional: Schema.Optional[?] =>
          run(fieldValue.asInstanceOf[Option[?]].map(encodeValue(_, optional.asInstanceOf[Schema.Optional[Any]].schema)))
        case schema: Schema[?] =>
          Some(encodeValue(fieldValue, schema))
          
      encoded.foreach(result.put(field.name, _))
    )

    result

  private def encodeEnum[Z](value: Z, cases: Chunk[Schema.Case[Z, ?]]): TomlValue = run {
    val enumCase: Schema.Case[Z, ?] = cases.find(_.isCase(value)).get
    // TODO encode the value using enumCase.schema
    TomlPrimitive.of(enumCase.caseName)
  }

  private def encodeMap[Col, K, V](
    value: Col,
    keySchema: Schema[K],
    valueSchema: Schema[V],
    collection: Schema.Collection[Col, (K, V)]
  ): TomlTable =
    if !TomlCodec.isString(keySchema)
    then fail(s"Map key must be a String, not a $keySchema")
    
    val result: TomlTable = TomlTable.create
    run(collection.toChunk(value).foreach( (key, value) => result.put(key.toString, encodeValue(value, TomlCodec.eager(valueSchema)))))
    result

  private def encodeArray[E](value: Any, schemaRaw: Schema[?]): TomlArray =
    val schema: Schema[E] = TomlCodec.eager(schemaRaw.asInstanceOf[Schema[E]])
    val result: TomlArray = TomlArray.create
    run(value.asInstanceOf[IterableOnce[E]].iterator.foreach((element: E) => result.add(encodeValue(element, schema))))
    result

  private given CanEqual[StandardType[?], StandardType[?]] = CanEqual.derived

  private def encodePrimitive[A](value: A, typ: StandardType[A]): TomlPrimitive = typ match
    case StandardType.UnitType           => fail("Unit is not supported")
    case StandardType.StringType         => run(TomlPrimitive.of(value.asInstanceOf[String        ]))
    case StandardType.BoolType           => run(TomlPrimitive.of(value.asInstanceOf[Boolean       ]))
    case StandardType.ByteType           => run(TomlPrimitive.of(value.asInstanceOf[Byte          ]))
    case StandardType.ShortType          => run(TomlPrimitive.of(value.asInstanceOf[Short         ]))
    case StandardType.IntType            => run(TomlPrimitive.of(value.asInstanceOf[Int           ]))
    case StandardType.LongType           => run(TomlPrimitive.of(value.asInstanceOf[Long          ]))
    case StandardType.FloatType          => run(TomlPrimitive.of(value.asInstanceOf[Float         ]))
    case StandardType.DoubleType         => run(TomlPrimitive.of(value.asInstanceOf[Double        ]))
    case StandardType.BinaryType         => fail("Binary is not supported") // TODO  Chunk[Byte] => base64?
    case StandardType.CharType           => run(TomlPrimitive.of(value.asInstanceOf[Char          ]))
    case StandardType.UUIDType           => run(TomlPrimitive.of(value.asInstanceOf[UUID          ].toString))
    case StandardType.CurrencyType       => run(TomlPrimitive.of(value.asInstanceOf[Currency      ].toString))
    case StandardType.BigDecimalType     => run(TomlPrimitive.of(value.asInstanceOf[BigDecimal    ].toString))
    case StandardType.BigIntegerType     => run(TomlPrimitive.of(value.asInstanceOf[BigInteger    ].toString))
    case StandardType.DayOfWeekType      => run(TomlPrimitive.of(value.asInstanceOf[DayOfWeek     ].toString))
    case StandardType.MonthType          => run(TomlPrimitive.of(value.asInstanceOf[Month         ].toString))
    case StandardType.MonthDayType       => run(TomlPrimitive.of(value.asInstanceOf[MonthDay      ].toString))
    case StandardType.PeriodType         => run(TomlPrimitive.of(value.asInstanceOf[Period        ].toString))
    case StandardType.YearType           => run(TomlPrimitive.of(value.asInstanceOf[Year          ].toString))
    case StandardType.YearMonthType      => run(TomlPrimitive.of(value.asInstanceOf[YearMonth     ].toString))
    case StandardType.ZoneIdType         => run(TomlPrimitive.of(value.asInstanceOf[ZoneId        ].toString))
    case StandardType.ZoneOffsetType     => run(TomlPrimitive.of(value.asInstanceOf[ZoneOffset    ].toString))
    case StandardType.DurationType       => run(TomlPrimitive.of(value.asInstanceOf[Duration      ].toString))
    case StandardType.InstantType        => run(TomlPrimitive.of(value.asInstanceOf[Instant       ].toString))
    case StandardType.LocalDateType      => run(TomlPrimitive.of(value.asInstanceOf[LocalDate     ]))
    case StandardType.LocalTimeType      => run(TomlPrimitive.of(value.asInstanceOf[LocalTime     ]))
    case StandardType.LocalDateTimeType  => run(TomlPrimitive.of(value.asInstanceOf[LocalDateTime ]))
    case StandardType.OffsetTimeType     => run(TomlPrimitive.of(value.asInstanceOf[OffsetTime    ].toString))
    case StandardType.OffsetDateTimeType => run(TomlPrimitive.of(value.asInstanceOf[OffsetDateTime]))
    case StandardType.ZonedDateTimeType  => run(TomlPrimitive.of(value.asInstanceOf[ZonedDateTime ].toString)) // TODO do better?
