package org.podval.tools.toml

import io.github.wasabithumb.jtoml.key.TomlKey
import io.github.wasabithumb.jtoml.value.array.TomlArray
import io.github.wasabithumb.jtoml.value.table.TomlTable
import zio.Chunk
import zio.schema.{DynamicValue, Fallback, MutableSchemaBasedValueProcessor, Schema, StandardType}
import scala.collection.immutable.ListMap

/*
  Attempt to derive TomlEncoder from ' MutableSchemaBasedValueProcessor[Unit, TomlEncoder.Context]
  following examples from ZIO Schema - ThriftCodec and ProtoBufCodec -  
  ran into difficulty: although methods to handle most flavours of Schema come in triples
  (startProcessingX, contextForX, processX),
  there is no contextForRecord,
  so to use this approach to handle Records inside Collections
  requires keeping a shadow stack of "real" states inside the TomlEncoder instance
  and manipulating it in convoluted ways.
    
  Let's try encoding the value directly, following the example of ZIO Schema AvroCodec :)  
    
  For reference, here are all the MutableSchemaBasedValueProcessor methods:
    
  processPrimitive
  processDynamic

  startProcessingOption / contextForOption / processOption
  startProcessingTuple / contextForTuple / processTuple
  startProcessingEnum / contextForEnumConstructor / processEnum [name discrepancy]
  startProcessingEither / contextForEither / processEither
  startProcessingSequence / contextForSequence / processSequence
  startProcessingSet / contextForSet / processSet
  startProcessingFallback / contextForFallback / processFallback
  startProcessingDictionary / contextForMap / processDictionary [name discrepancy]
  startProcessingRecord / _ / processRecord
    TODO add to ZIO Schema
    protected contextForRecord(context: Context, schema: Schema.Record[?]): Context
    - or (ab)use contextForRecordField() somehow?
  contextForRecordField
 */
object MutableSchemaBasedValueProcessorTomlEncoder:
  sealed trait Context
  object Context:
    final case class Table(table: TomlTable) extends Context
    final case class Key(table: TomlTable, key: String) extends Context
    final case class Array(array: TomlArray) extends Context


final class MutableSchemaBasedValueProcessorTomlEncoder
  extends MutableSchemaBasedValueProcessor[Unit, MutableSchemaBasedValueProcessorTomlEncoder.Context]:
  import MutableSchemaBasedValueProcessorTomlEncoder.Context

  private val result: TomlTable = TomlTable.create()

  def encode[A](value: A, schema: Schema[A]): Chunk[Byte] =
    process(schema, value)
    TomlCodec.toBytes(result)

  override protected def fail(context: Context, message: String): Unit = ???

  override protected val initialContext: Context = Context.Table(table = result)

  override protected def processPrimitive(context: Context, value: Any, typ: StandardType[Any]): Unit =
    context match
      case Context.Key(table, key) => table.put(TomlKey.parse(key), TomlCodec.encodePrimitive(typ, value))
      case Context.Array(array) => array.add(TomlCodec.encodePrimitive(typ, value))
  
  override protected def processDynamic(context: Context, value: DynamicValue): Option[Unit] = None // not supported

  // Option
  override protected def startProcessingOption(context: Context, schema: Schema.Optional[?]): Unit = ()
  override protected def contextForOption(context: Context, o: Option[Unit]): Context = context
  override protected def processOption(context: Context, schema: Schema.Optional[?], value: Option[Unit]): Unit = ()

  // Tuple
  override protected def startProcessingTuple(context: Context, schema: Schema.Tuple2[?, ?]): Unit = ()
  override protected def contextForTuple(context: Context, index: Int): Context = ???
  override protected def processTuple(context: Context, schema: Schema.Tuple2[?, ?], left: Unit, right: Unit): Unit = ???

  // Enum
  override protected def startProcessingEnum(context: Context, schema: Schema.Enum[?]): Unit = ()
  override protected def contextForEnumConstructor(context: Context, index: Int, c: Schema.Case[?, ?]): Context = ???
  override protected def processEnum(context: Context, schema: Schema.Enum[?], tuple: (String, Unit)): Unit = ???

  // Either
  override protected def startProcessingEither(context: Context, schema: Schema.Either[?, ?]): Unit = ()
  override protected def contextForEither(context: Context, e: Either[Unit, Unit]): Context = ???
  override protected def processEither(context: Context, schema: Schema.Either[?, ?], value: Either[Unit, Unit]): Unit = ???

  // Sequence
  override protected def startProcessingSequence(context: Context, schema: Schema.Sequence[?, ?, ?], size: Int): Unit = ()
  override protected def contextForSequence(context: Context, schema: Schema.Sequence[?, ?, ?], index: Int): Context = context
  override protected def processSequence(context: Context, schema: Schema.Sequence[?, ?, ?], value: Chunk[Unit]): Unit = ???

  // Set
  override protected def startProcessingSet(context: Context, schema: Schema.Set[?], size: Int): Unit = ()
  override protected def contextForSet(context: Context, schema: Schema.Set[?], index: Int): Context = context
  override protected def processSet(context: Context, schema: Schema.Set[?], value: Set[Unit]): Unit = ()

  // Fallback
  override protected def startProcessingFallback(context: Context, schema: Schema.Fallback[?, ?]): Unit = ()
  override protected def contextForFallback(context: Context, e: Fallback[Unit, Unit]): Context = ???
  override protected def processFallback(context: Context, schema: Schema.Fallback[?, ?], value: Fallback[Unit, Unit]): Unit = ???

  // Map
  override protected def startProcessingDictionary(context: Context, schema: Schema.Map[?, ?], size: Int): Unit = ()
  override protected def contextForMap(context: Context, schema: Schema.Map[?, ?], index: Int): Context = ???
  override protected def processDictionary(context: Context, schema: Schema.Map[?, ?], value: Chunk[(Unit, Unit)]): Unit = ???

  // Record
  override protected def startProcessingRecord(context: Context, schema: Schema.Record[?]): Unit = ()
  // Note: there is no contextForRecord
  override protected def processRecord(context: Context, schema: Schema.Record[?], value: ListMap[String, Unit]): Unit = ()

  override protected def contextForRecordField(context: Context, index: Int, field: Schema.Field[?, ?]): Context =
    context match
      case Context.Table(table) =>
        TomlCodec.eager(field.schema) match
          case _: Schema.Primitive[?] | _: Schema.Optional[?] => Context.Key(table, field.name)
          case _: Schema.Record[?] | _: Schema.Map[?, ?] | _: Schema.NonEmptyMap[?, ?] => // TODO tuples?
            val nestedTable: TomlTable = TomlTable.create
            table.put(field.name, nestedTable)
            Context.Table(nestedTable)
          case _: Schema.Set[?] | _: Schema.Sequence[?, ?, ?] | _: Schema.NonEmptySequence[?, ?, ?] =>
            val array: TomlArray = TomlArray.create
            table.put(field.name, array)
            Context.Array(array)
          case _: Schema.Enum[?] =>
            ??? // TODO
          case _ =>
            ??? // not supported
