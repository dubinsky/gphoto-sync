package org.podval.tools.toml

import io.github.wasabithumb.jtoml.document.TomlDocument
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import zio.{Chunk, Unsafe}
import zio.schema.MutableSchemaBasedValueBuilder.ReadingFieldResult
import zio.schema.codec.DecodeError
import zio.schema.{Fallback, MutableSchemaBasedValueBuilder, Schema, StandardType}

object MutableSchemaBasedValueBuilderTomlDecoder:
  final case class Context(
    path: Chunk[String],
    table: TomlTable,
    key: Option[String]
  )

final class MutableSchemaBasedValueBuilderTomlDecoder(chunk: Chunk[Byte])
  extends MutableSchemaBasedValueBuilder[Any, MutableSchemaBasedValueBuilderTomlDecoder.Context]:
  import MutableSchemaBasedValueBuilderTomlDecoder.Context
  given CanEqual[StandardType[?], StandardType[?]] = CanEqual.derived

  private val doc: TomlDocument = TomlCodec.fromBytes(chunk)

  override protected def createPrimitive(context: Context, typ: StandardType[?]): Any =
    // TODO convert errors to calls to fail()
    val value: TomlPrimitive = context.table.get(context.key.get).asPrimitive
    TomlCodec.decodePrimitive(value, typ)

  override protected def startCreatingRecord(context: Context, record: Schema.Record[?]): Context =
    context.key match
      case None => context
      case Some(key) => context.copy(table = context.table.get(key).asTable)

  override protected def startReadingField(context: Context, record: Schema.Record[?], index: Int): ReadingFieldResult[Context] =
    if index >= record.fields.size
    then
      ReadingFieldResult.Finished()
    else
      val fieldName: String = record.fields(index).name
      ReadingFieldResult.ReadField(context.copy(path = context.path :+ s"field:$fieldName", key = Some(fieldName)), index)

  // TODO look at ProtobufCodec too
  override protected def createRecord(
    context: Context,
    record: Schema.Record[?],
    values: Chunk[(Int, Any)]
  ): Any =
    val allValues: Chunk[Any] =
      if record.fields.isEmpty
      then Chunk.empty
      else
        val valuesMap: Map[Int, Any] = values.toMap
        record.fields.zipWithIndex.map:
          case (field, idx) => valuesMap.get(idx).orElse(TomlCodec.emptyValue(field.schema)) match
            case Some(value) => value
            case None =>
              if (field.optional || field.transient) && field.defaultValue.isDefined
              then field.defaultValue.get
              else fail(context, s"Missing value")

    Unsafe.unsafe: _ ?=>
      record.construct(allValues) match
        case Left(message) => fail(context, message)
        case Right(value)  => value

  override protected def startCreatingEnum(context: Context, cases: Chunk[Schema.Case[?, ?]]): (Context, Int) = ???
  override protected def createEnum(context: Context, cases: Chunk[Schema.Case[?, ?]], index: Int, value: Any): Any = ???
  override protected def startCreatingSequence(context: Context, schema: Schema.Sequence[?, ?, ?]): Option[Context] = ???
  override protected def startCreatingOneSequenceElement(context: Context, schema: Schema.Sequence[?, ?, ?]): Context = ???
  override protected def createSequence(context: Context, schema: Schema.Sequence[?, ?, ?], values: Chunk[Any]): Any = ???
  override protected def startCreatingDictionary(context: Context, schema: Schema.Map[?, ?]): Option[Context] = ???
  override protected def startCreatingOneDictionaryElement(context: Context, schema: Schema.Map[?, ?]): Context = ???
  override protected def startCreatingOneDictionaryValue(context: Context, schema: Schema.Map[?, ?]): Context = ???
  override protected def createDictionary(context: Context, schema: Schema.Map[?, ?], values: Chunk[(Any, Any)]): Any = ???
  override protected def startCreatingSet(context: Context, schema: Schema.Set[?]): Option[Context] = ???
  override protected def startCreatingOneSetElement(context: Context, schema: Schema.Set[?]): Context = ???
  override protected def createSet(context: Context, schema: Schema.Set[?], values: Chunk[Any]): Any = ???

  override protected def startCreatingOptional(context: Context, schema: Schema.Optional[?]): Option[Context] =
    if !context.table.contains(context.key.get)
    then None
    else Some(context.copy(path = context.path :+ "Some"))

  override protected def createOptional(context: Context, schema: Schema.Optional[?], value: Option[Any]): Any = value

  override protected def startCreatingEither(context: Context, schema: Schema.Either[?, ?]): Either[Context, Context] = ???
  override protected def createEither(context: Context, schema: Schema.Either[?, ?], value: Either[Any, Any]): Any = ???
  override protected def startCreatingFallback(context: Context, schema: Schema.Fallback[?, ?]): Fallback[Context, Context] = ???
  override protected def startReadingRightFallback(context: Context, schema: Schema.Fallback[?, ?]): Context = ???
  override protected def createFallback(context: Context, schema: Schema.Fallback[?, ?], value: Fallback[Any, Any]): Any = ???
  override protected def startCreatingTuple(context: Context, schema: Schema.Tuple2[?, ?]): Context = ???
  override protected def startReadingSecondTupleElement(context: Context, schema: Schema.Tuple2[?, ?]): Context = ???
  override protected def createTuple(context: Context, schema: Schema.Tuple2[?, ?], left: Any, right: Any): Any = ???
  override protected def createDynamic(context: Context): Option[Any] = ???
  override protected def transform(context: Context, value: Any, f: Any => Either[String, Any], schema: Schema[?]): Any = ???

  override protected def fail(context: Context, message: String): Any =
    throw DecodeError.MalformedFieldWithPath(context.path, message)

  override protected def finishedCreatingOneSequenceElement(context: Context, index: Int): Boolean = ???
  override protected def finishedCreatingOneDictionaryElement(context: Context, schema: Schema.Map[?, ?], index: Int): Boolean = ???
  override protected def finishedCreatingOneSetElement(context: Context, schema: Schema.Set[?], index: Int): Boolean = ???

  override protected val initialContext: Context = Context(
    path = Chunk.empty,
    table = doc,
    key = None
  )
