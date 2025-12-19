package org.podval.tools.toml

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.key.TomlKey
import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.array.TomlArray
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import zio.{Chunk, ChunkBuilder, Unsafe}
import zio.schema.{Schema, StandardType}
import zio.schema.codec.DecodeError
import scala.jdk.CollectionConverters.MapHasAsScala
import java.math.{BigDecimal, BigInteger}
import java.time.{DayOfWeek, Duration, Instant, Month, MonthDay, OffsetTime, Period, Year, YearMonth, ZoneId,
  ZoneOffset, ZonedDateTime}
import java.util.{Currency, UUID}
import scala.annotation.tailrec
import scala.util.control.NonFatal

object TomlDecoder:
  private type Result[A] = Either[DecodeError, A]

  type Path = Chunk[String]

  private def run[A](path: Path, a: => A): Result[A] =
    try Right(a) catch
      case NonFatal(e) =>
        Left(DecodeError.MalformedFieldWithPath(path, e.getMessage)) // TODO Cause

  private def fail(path: Path, failure: String): Result[Nothing] =
    Left(DecodeError.MalformedFieldWithPath(path, failure))

  def decode[A](bytes: Chunk[Byte], schema: Schema[A]): Result[A] =
   run(Chunk.empty, JToml.jToml.readFromString(bytes.asString))
     .flatMap(asRecord(Chunk.empty :+ "topLevelRecord", schema, _))

  private def asRecord[A](
    path: Path,
    schema: Schema[A],
    table: TomlTable
  ): Result[A] = schema match
    case record: Schema.Record[A] => decodeRecord(path, table, record)
    case schema => fail(path, s"Must be a record, not $schema")

  private def decodeValue[A](path: Path, value: TomlValue, schema: Schema[A]): Result[A] = schema match
    case enumeration: Schema.Enum[?] =>
      decodeEnum(path, value, enumeration)
    case primitive: Schema.Primitive[?] =>
      decodePrimitive(path, value.asPrimitive, primitive.standardType)
    case record: Schema.Record[?] =>
      decodeRecord(path :+ "record", value.asTable, record)
    case map: Schema.Map[?, ?] =>
      decodeMap(path :+ "map", value.asTable, map.keySchema, map.valueSchema, map)
    case map: Schema.NonEmptyMap[?, ?] =>
      decodeMap(path :+ "nonEmptyMap", value.asTable, map.keySchema, map.valueSchema, map)
    case set: Schema.Set[?] =>
      decodeArray(path :+ "set", value.asArray, set.elementSchema, set)
    case sequence: Schema.Sequence[?, ?, ?] =>
      decodeArray(path :+ "sequence", value.asArray, sequence.elementSchema, sequence)
    case sequence: Schema.NonEmptySequence[?, ?, ?] =>
      decodeArray(path :+ "nonEmptySequence", value.asArray, sequence.elementSchema, sequence)
    case schema =>
      fail(path, s"Unsupported schema $schema")

  private def decodeRecord[R](path: Path, table: TomlTable, record: Schema.Record[R]): Result[R] = build(
    record.fields,
    (field: Schema.Field[R, ?]) =>
      val fieldName: String = field.name
      val fieldPath: Path = path :+ s"field:$fieldName"
      (Option(table.get(fieldName)), TomlCodec.eager(field.schema)) match
        case (None       , optional: Schema.Optional[?]) => Right(None)
        case (None       , schema  : Schema         [?]) => fail(fieldPath, "Missing non-optional value")
        case (Some(value), optional: Schema.Optional[?]) => decodeValue(fieldPath :+ "Some", value, optional.schema).map(Some(_))
        case (Some(value), schema  : Schema         [?]) => decodeValue(fieldPath          , value,          schema),
    (chunk: Chunk[Any]) => Unsafe.unsafe: _ ?=>
      record.construct(chunk) match
        case Left(error: String) => fail(path, error)
        case Right(result) => Right(result)
  )

  private def decodeMap[Col, K, V](
    path: Path,
    map: TomlTable,
    keySchema: Schema[K],
    valueSchema: Schema[V],
    collection: Schema.Collection[Col, (K, V)]
  ): Result[Col] =
    if !TomlCodec.isString(keySchema)
    then fail(path, s"Map key must be a String, not a $keySchema") else build(
      map.toMap.asScala.toList,
      (key: TomlKey, value: TomlValue) => decodeValue(path :+ s"key:$key", value, valueSchema).map(key.toString -> _),
      (chunk: Chunk[(String, V)]) => run(path, collection.asInstanceOf[Schema.Collection[Col, (String, V)]].fromChunk(chunk))
    )

  private def decodeArray[Col, E](
    path: Path,
    array: TomlArray,
    elementSchemaRaw: Schema[E],
    collection: Schema.Collection[Col, E]
  ): Result[Col] =
    val elementSchema: Schema[E] = TomlCodec.eager(elementSchemaRaw)

    build(
      array.toArray.toList.zipWithIndex.map((value, index) => (index, value)),
      (index: Int, element: TomlValue) => decodeValue(path :+ s"index:$index", element, elementSchema),
      (chunk: Chunk[E]) => run(path, collection.fromChunk(chunk))
    )

  private def build[E, V, R](
    elements: Seq[E],
    decode: E => Result[V],
    construct: Chunk[V] => Result[R]
  ): Result[R] =
    @tailrec
    def build(elements: Seq[E], result: ChunkBuilder[V]): Result[R] =
      if elements.isEmpty
      then construct(result.result)
      else decode(elements.head) match
        case Left(error) => Left(error)
        case Right(value) => build(elements.tail, result += value)

    build(elements, ChunkBuilder.make[V]())

  private def decodeEnum[Z](path: Path, value: TomlValue, schema: Schema.Enum[Z]): Result[Z] =
    val (caseNameValue: TomlValue, table: TomlTable) = if value.isPrimitive then (value, emptyTable) else
      val table: TomlTable = value.asTable
      val caseNameValue: TomlValue = Option(table.get(TomlCodec.caseNameKey)).get
      table.remove(TomlCodec.caseNameKey)
      (caseNameValue, table)

    val caseName: String = caseNameValue.asPrimitive.asString
    val enumCase: Schema.Case[Z, ?] = schema.caseOf(caseName).get
    asRecord(
      path :+ s"enumCaseRecord:$caseName",
      enumCase.schema,
      table
    )
      .map(enumCase.construct)

  private val emptyTable: TomlTable = TomlTable.create

  private given CanEqual[StandardType[?], StandardType[?]] = CanEqual.derived

  private def decodePrimitive[A](path: Path, value: TomlPrimitive, typ: StandardType[A]): Result[A] =
    def run(a: => A): Result[A] = this.run(path, a)
    typ match
      case StandardType.UnitType           => fail(path, "Unit is not supported")
      case StandardType.StringType         => run(value.asString)
      case StandardType.BoolType           => run(value.asBoolean)
      case StandardType.ByteType           => run(value.asInteger.toByte)
      case StandardType.ShortType          => run(value.asInteger.toShort)
      case StandardType.IntType            => run(value.asInteger)
      case StandardType.LongType           => run(value.asLong)
      case StandardType.FloatType          => run(value.asFloat)
      case StandardType.DoubleType         => run(value.asDouble)
      case StandardType.BinaryType         => fail(path, "Binary is not supported") // TODO base64?
      case StandardType.CharType           => run(value.asInteger.toChar)
      case StandardType.UUIDType           => run(UUID.fromString(value.asString))
      case StandardType.CurrencyType       => run(Currency.getInstance(value.asString))
      case StandardType.BigDecimalType     => run(BigDecimal(value.asString))
      case StandardType.BigIntegerType     => run(BigInteger(value.asString))
      case StandardType.DayOfWeekType      => run(DayOfWeek.valueOf(value.asString))
      case StandardType.MonthType          => run(Month.valueOf(value.asString))
      case StandardType.MonthDayType       => run(MonthDay.parse(value.asString))
      case StandardType.PeriodType         => run(Period.parse(value.asString))
      case StandardType.YearType           => run(Year.parse(value.asString))
      case StandardType.YearMonthType      => run(YearMonth.parse(value.asString))
      case StandardType.ZoneIdType         => run(ZoneId.of(value.asString))
      case StandardType.ZoneOffsetType     => run(ZoneOffset.of(value.asString))
      case StandardType.DurationType       => run(Duration.parse(value.asString))
      case StandardType.InstantType        => run(Instant.parse(value.asString))
      case StandardType.LocalDateType      => run(value.asLocalDate)
      case StandardType.LocalTimeType      => run(value.asLocalTime)
      case StandardType.LocalDateTimeType  => run(value.asLocalDateTime)
      case StandardType.OffsetTimeType     => run(OffsetTime.parse(value.asString))
      case StandardType.OffsetDateTimeType => run(value.asOffsetDateTime)
      case StandardType.ZonedDateTimeType  => run(ZonedDateTime.parse(value.asString)) // TODO do better?
