package org.podval.tools.toml

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.array.TomlArray
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import zio.Chunk
import zio.schema.{Schema, StandardType}
import java.math.{BigDecimal, BigInteger}
import java.nio.charset.StandardCharsets
import java.time.{DayOfWeek, Duration, Instant, LocalDate, LocalDateTime, LocalTime, Month, MonthDay, OffsetDateTime,
  OffsetTime, Period, Year, YearMonth, ZoneId, ZoneOffset, ZonedDateTime}
import java.util.{Currency, UUID}

object TomlEncoder:
  def encode[A](value: A, schema: Schema[A]): Chunk[Byte] =
    val result: TomlTable = encodeRecord(value, schema.asInstanceOf[Schema.Record[A]])
    Chunk.fromArray(JToml.jToml.writeToString(result).getBytes(StandardCharsets.UTF_8))

  private def encodeValue[A](value: A, schema: Schema[A]): TomlValue = schema match
    case enumeration: Schema.Enum[?] =>
      encodeEnum(value, enumeration.cases)
    case primitive: Schema.Primitive[?] => 
      encodePrimitive(value, primitive.standardType)
    case record: Schema.Record[?] => 
      encodeRecord(value, record)
    case map: Schema.Map[?, ?] =>
      encodeMap(value, map.valueSchema)
    case map: Schema.NonEmptyMap[?, ?] =>
      encodeMap(value, map.valueSchema)
    case set: Schema.Set[?] => 
      encodeArray(value, set.elementSchema)
    case sequence: Schema.Sequence[?, ?, ?] => 
      encodeArray(value, sequence.elementSchema)
    case sequence: Schema.NonEmptySequence[?, ?, ?] => 
      encodeArray(value, sequence.elementSchema)
  
  private def encodeRecord[R](value: R, record: Schema.Record[R]): TomlTable =
    val result: TomlTable = TomlTable.create

    record.fields.foreach((field: Schema.Field[R, ?]) =>
      val fieldValue = field.get(value)
      val encoded = TomlCodec.eager(field.schema) match
        case optional: Schema.Optional[?] =>
          fieldValue.asInstanceOf[Option[?]].map(encodeValue(_, optional.asInstanceOf[Schema.Optional[Any]].schema))
        case schema: Schema[?] => 
          Some(encodeValue(fieldValue, schema))
      encoded.foreach(result.put(field.name, _))
    )

    result

  private def encodeEnum[Z](value: Z, cases: Chunk[Schema.Case[Z, ?]]): TomlPrimitive =
    TomlPrimitive.of(cases.find(_.isCase(value)).get.caseName)

  private def encodeMap[K, V](map: Any, schemaRaw: Schema[?]): TomlTable =
    val schema: Schema[V] = TomlCodec.eager(schemaRaw.asInstanceOf[Schema[V]])
    val result: TomlTable = TomlTable.create
    map.asInstanceOf[Map[K, V]].foreach( (key, value) => result.put(key.toString, encodeValue(value, schema)))
    result
  
  private def encodeArray[E](value: Any, schemaRaw: Schema[?]): TomlArray =
    val schema: Schema[E] = TomlCodec.eager(schemaRaw.asInstanceOf[Schema[E]])
    val result: TomlArray = TomlArray.create
    value.asInstanceOf[IterableOnce[E]].iterator.foreach((element: E) => result.add(encodeValue(element, schema)))
    result

  private given CanEqual[StandardType[?], StandardType[?]] = CanEqual.derived

  private def encodePrimitive[A](value: A, typ: StandardType[A]): TomlPrimitive = (typ, value) match
    case (StandardType.UnitType          , value: Unit          ) => ??? // TODO fail
    case (StandardType.StringType        , value: String        ) => TomlPrimitive.of(value)
    case (StandardType.BoolType          , value: Boolean       ) => TomlPrimitive.of(value)
    case (StandardType.ByteType          , value: Byte          ) => TomlPrimitive.of(value)
    case (StandardType.ShortType         , value: Short         ) => TomlPrimitive.of(value)
    case (StandardType.IntType           , value: Int           ) => TomlPrimitive.of(value)
    case (StandardType.LongType          , value: Long          ) => TomlPrimitive.of(value)
    case (StandardType.FloatType         , value: Float         ) => TomlPrimitive.of(value)
    case (StandardType.DoubleType        , value: Double        ) => TomlPrimitive.of(value)
    case (StandardType.BinaryType        , value: Chunk[Byte]   ) => ??? // TODO  base64?
    case (StandardType.CharType          , value: Char          ) => TomlPrimitive.of(value)
    case (StandardType.UUIDType          , value: UUID          ) => TomlPrimitive.of(value.toString)
    case (StandardType.CurrencyType      , value: Currency      ) => TomlPrimitive.of(value.toString)
    case (StandardType.BigDecimalType    , value: BigDecimal    ) => TomlPrimitive.of(value.toString)
    case (StandardType.BigIntegerType    , value: BigInteger    ) => TomlPrimitive.of(value.toString)
    case (StandardType.DayOfWeekType     , value: DayOfWeek     ) => TomlPrimitive.of(value.toString)
    case (StandardType.MonthType         , value: Month         ) => TomlPrimitive.of(value.toString)
    case (StandardType.MonthDayType      , value: MonthDay      ) => TomlPrimitive.of(value.toString)
    case (StandardType.PeriodType        , value: Period        ) => TomlPrimitive.of(value.toString)
    case (StandardType.YearType          , value: Year          ) => TomlPrimitive.of(value.toString)
    case (StandardType.YearMonthType     , value: YearMonth     ) => TomlPrimitive.of(value.toString)
    case (StandardType.ZoneIdType        , value: ZoneId        ) => TomlPrimitive.of(value.toString)
    case (StandardType.ZoneOffsetType    , value: ZoneOffset    ) => TomlPrimitive.of(value.toString)
    case (StandardType.DurationType      , value: Duration      ) => TomlPrimitive.of(value.toString)
    case (StandardType.InstantType       , value: Instant       ) => TomlPrimitive.of(value.toString)
    case (StandardType.LocalDateType     , value: LocalDate     ) => TomlPrimitive.of(value)
    case (StandardType.LocalTimeType     , value: LocalTime     ) => TomlPrimitive.of(value)
    case (StandardType.LocalDateTimeType , value: LocalDateTime ) => TomlPrimitive.of(value)
    case (StandardType.OffsetTimeType    , value: OffsetTime    ) => TomlPrimitive.of(value.toString)
    case (StandardType.OffsetDateTimeType, value: OffsetDateTime) => TomlPrimitive.of(value)
    case (StandardType.ZonedDateTimeType , value: ZonedDateTime ) => TomlPrimitive.of(value.toString) // TODO do better?
