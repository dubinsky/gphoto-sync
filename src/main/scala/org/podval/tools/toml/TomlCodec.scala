package org.podval.tools.toml

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.document.TomlDocument
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import zio.{Cause, Chunk}
import zio.schema.MutableSchemaBasedValueBuilder.CreateValueFromSchemaError
import zio.schema.{Schema, StandardType}
import zio.schema.codec.{BinaryCodec, DecodeError}
import zio.stream.ZPipeline
import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal
import java.math.{BigDecimal, BigInteger}
import java.time.{DayOfWeek, Duration, Instant, LocalDate, LocalDateTime, LocalTime, Month, MonthDay, OffsetDateTime, 
  OffsetTime, Period, Year, YearMonth, ZoneId, ZoneOffset, ZonedDateTime}
import java.util.{Currency, UUID}
import scala.annotation.tailrec

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
    override def decode(bytes: Chunk[Byte]): Either[DecodeError, A] = DirectTomlDecoder.decode(bytes, schema)

    override def encode(value: A): Chunk[Byte] = DirectTomlEncoder.encode(value, schema)

    // I do not need to stream TOML :)
    override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] = ???
    override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] = ???

//    private def decodeChunk(chunk: Chunk[Byte]): Either[DecodeError, A] =
//      if chunk.isEmpty
//      then Left(DecodeError.EmptyContent("No bytes to decode"))
//      else
//        try
//          Right(MutableSchemaBasedValueBuilderTomlDecoder(chunk).create(schema).asInstanceOf[A])
//        catch
//          case error: CreateValueFromSchemaError[MutableSchemaBasedValueBuilderTomlDecoder.Context] =>
//            error.cause match
//              case error: DecodeError => Left(error)
//              case _ => Left(DecodeError.ReadErrorWithPath(error.context.path, Cause.fail(error.cause), error.cause.getMessage))
//          case NonFatal(err) => Left(DecodeError.ReadError(Cause.fail(err), err.getMessage))
//      // TODO To ensure that a @nowarn annotation actually suppresses a warning, enable -Xlint:unused or -Wunused:nowarn.
//      //: @nowarn


  def toBytes(result: TomlTable): Chunk[Byte] =
    Chunk.fromArray(JToml.jToml.writeToString(result).getBytes(StandardCharsets.UTF_8))

  def fromBytes(bytes: Chunk[Byte]): TomlDocument = JToml.jToml().readFromString(bytes.asString)

  def eager[A](schema: Schema[A]): Schema[A] = schema match
    case lzy: Schema.Lazy[?] => lzy.schema
    case schema => schema

  private given CanEqual[StandardType[?], StandardType[?]] = CanEqual.derived

  @tailrec
  def emptyValue[A](schema: Schema[A]): Option[A] = schema match
    case Schema.Lazy(s) => emptyValue(s())
    case Schema.Optional(_, _) => Some(None)
    case Schema.Sequence(_, fromChunk, _, _, _) => Some(fromChunk(Chunk.empty))
    case Schema.Primitive(StandardType.UnitType, _) => Some(())
    case _ => None

  def encodePrimitive[A](typ: StandardType[A], value: A): TomlPrimitive =
    given CanEqual[StandardType[A], StandardType[A]] = CanEqual.derived

    (typ, value) match
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

  def decodePrimitive[A](value: TomlPrimitive, typ: StandardType[A]): A = typ match
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
    