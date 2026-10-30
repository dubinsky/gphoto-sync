package org.podval.tools.photo

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.except.TomlException
import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import java.io.File
import java.time.LocalDate

final case class Metadata(
  source: Option[String] = None,
  timestamp: Option[String] = None, // TODO turn into LocalDateTime - a coercion seems to be missing though?
  date: Option[LocalDate] = None,
  `bad-raw`: Option[Boolean] = None,
  orientation: Option[String] = None,
  `google-media-item-id`: Option[String] = None,
  `already-in-google`: Option[Boolean] = None,
  uploader: Option[String] = None
) derives CanEqual:
  override def toString: String = Property.all
    .flatMap(property => property.get(this).map(value => s"${property.name}=$value"))
    .mkString(", ")

object Metadata:
  val extension: String = "toml"

  val default: Metadata = Metadata()

  def read(file: File): Metadata =
    try decode(Files.read(file))
    catch case error: (IllegalArgumentException | TomlException) =>
      throw IllegalArgumentException(s"Error parsing TOML file '$file': ${error.getMessage}", error)

  private[photo] def decode(text: String): Metadata =
    val table: TomlTable =
      if text.isBlank then TomlTable.create()
      else JToml.jToml.readFromString(text)
    Metadata(
      source = optionalString(table, "source"),
      timestamp = optionalString(table, "timestamp"),
      date = optionalDate(table, "date"),
      `bad-raw` = optionalBoolean(table, "bad-raw"),
      orientation = optionalString(table, "orientation"),
      `google-media-item-id` = optionalString(table, "google-media-item-id"),
      `already-in-google` = optionalBoolean(table, "already-in-google"),
      uploader = optionalString(table, "uploader").map(parseUploader)
    )

  def encode(metadata: Metadata): String =
    val table: TomlTable = TomlTable.create()
    metadata.source.foreach(value => table.put("source", value))
    metadata.timestamp.foreach(value => table.put("timestamp", value))
    metadata.date.foreach(value => table.put("date", value))
    metadata.`bad-raw`.foreach(value => table.put("bad-raw", value))
    metadata.orientation.foreach(value => table.put("orientation", value))
    metadata.`google-media-item-id`.foreach(value => table.put("google-media-item-id", value))
    metadata.`already-in-google`.foreach(value => table.put("already-in-google", value))
    metadata.uploader.foreach(value => table.put("uploader", value))
    JToml.jToml.writeToString(table)

  // String.trim counts tab, CR, and LF as whitespace, so those are rejected first.
  private[photo] def parseUploader(value: String): String =
    if value.exists(ch => ch == '\t' || ch == '\r' || ch == '\n') then
      reject("uploader", "must not contain a tab, CR, or LF")
    else if value.trim.isEmpty then
      reject("uploader", "must be a non-empty display name")
    else value

  private def optionalString(table: TomlTable, key: String): Option[String] =
    Option(table.get(key)).map: value =>
      val primitive: TomlPrimitive = primitiveOf(key, value, "must be a string")
      if primitive.isString then primitive.asString
      else reject(key, "must be a string")

  private def optionalBoolean(table: TomlTable, key: String): Option[Boolean] =
    Option(table.get(key)).map: value =>
      val primitive: TomlPrimitive = primitiveOf(key, value, "must be a boolean")
      if primitive.isBoolean then primitive.asBoolean
      else reject(key, "must be a boolean")

  private def optionalDate(table: TomlTable, key: String): Option[LocalDate] =
    Option(table.get(key)).map: value =>
      val primitive: TomlPrimitive = primitiveOf(key, value, "must be a local date")
      if primitive.isLocalDate then primitive.asLocalDate
      else reject(key, "must be a local date")

  // as* coerces the wrong primitive (a string "false" becomes true). Check first.
  private def primitiveOf(key: String, value: TomlValue, expected: String): TomlPrimitive =
    if value.isPrimitive then value.asPrimitive
    else reject(key, expected)

  private def reject(key: String, message: String): Nothing =
    throw IllegalArgumentException(s"key '$key' $message")
