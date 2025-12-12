package org.podval.tools.toml

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.array.TomlArray
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import zio.{Chunk, Unsafe}
import zio.schema.{Schema, StandardType}
import zio.schema.codec.DecodeError
import scala.jdk.CollectionConverters.MapHasAsScala
import java.math.{BigDecimal, BigInteger}
import java.time.{DayOfWeek, Duration, Instant, LocalDate, LocalDateTime, LocalTime, Month, MonthDay, OffsetDateTime,
  OffsetTime, Period, Year, YearMonth, ZoneId, ZoneOffset, ZonedDateTime}
import java.util.{Currency, UUID}

// TODO add fail() and error converters
object TomlDecoder:
  def decode[A](bytes: Chunk[Byte], schema: Schema[A]): Either[DecodeError, A] =
    Right(decodeRecord(
      path = Chunk.empty,
      table = JToml.jToml().readFromString(bytes.asString),
      record = schema.asInstanceOf[Schema.Record[A]]
    ))

  type Path = Chunk[String]

  private def decodeValue[A](path: Path, value: TomlValue, schema: Schema[A]): A =
    schema match
      case enumeration: Schema.Enum[?] =>
        decodeEnum(value, enumeration)
      case primitive: Schema.Primitive[?] =>
        decodePrimitive(value.asPrimitive, primitive.standardType)
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

  private def decodeEnum[Z](value: TomlValue, schema: Schema.Enum[Z]): Z =
    schema.caseOf(value.asPrimitive.asString).get.asInstanceOf[Schema.Case[Z, Unit]].construct(())

  private def decodeMap[V](path: Path, map: TomlTable, valueSchema: Schema[V]): Chunk[(String, V)] =
    Chunk.fromIterable(map.toMap.asScala.toSeq.map((key, value) => key.toString -> decodeValue(path :+ s"element $key", value, valueSchema)))

  private def decodeArray[E](path: Path, array: TomlArray, elementSchemaRaw: Schema[E]): Chunk[E] =
    val elementSchema: Schema[E] = TomlCodec.eager(elementSchemaRaw)
    Chunk.fromArray(array.toArray).map(decodeValue(path :+ "[element", _, elementSchema))

  private given CanEqual[StandardType[?], StandardType[?]] = CanEqual.derived

  private def decodePrimitive[A](value: TomlPrimitive, typ: StandardType[A]): A = typ match
    case StandardType.UnitType           => () // TODO fail
    case StandardType.StringType         => value.asString
    case StandardType.BoolType           => value.asBoolean
    case StandardType.ByteType           => value.asInteger.toByte
    case StandardType.ShortType          => value.asInteger.toShort
    case StandardType.IntType            => value.asInteger
    case StandardType.LongType           => value.asLong
    case StandardType.FloatType          => value.asFloat
    case StandardType.DoubleType         => value.asDouble
    case StandardType.BinaryType         => ??? // TODO base64?
    case StandardType.CharType           => value.asInteger.toChar
    case StandardType.UUIDType           => UUID.fromString(value.asString)
    case StandardType.CurrencyType       => Currency.getInstance(value.asString)
    case StandardType.BigDecimalType     => BigDecimal(value.asString)
    case StandardType.BigIntegerType     => BigInteger(value.asString)
    case StandardType.DayOfWeekType      => DayOfWeek.valueOf(value.asString)
    case StandardType.MonthType          => Month.valueOf(value.asString)
    case StandardType.MonthDayType       => MonthDay.parse(value.asString)
    case StandardType.PeriodType         => Period.parse(value.asString)
    case StandardType.YearType           => Year.parse(value.asString)
    case StandardType.YearMonthType      => YearMonth.parse(value.asString)
    case StandardType.ZoneIdType         => ZoneId.of(value.asString)
    case StandardType.ZoneOffsetType     => ZoneOffset.of(value.asString)
    case StandardType.DurationType       => Duration.parse(value.asString)
    case StandardType.InstantType        => Instant.parse(value.asString)
    case StandardType.LocalDateType      => value.asLocalDate
    case StandardType.LocalTimeType      => value.asLocalTime
    case StandardType.LocalDateTimeType  => value.asLocalDateTime
    case StandardType.OffsetTimeType     => OffsetTime.parse(value.asString)
    case StandardType.OffsetDateTimeType => value.asOffsetDateTime
    case StandardType.ZonedDateTimeType  => ZonedDateTime.parse(value.asString) // TODO do better?
