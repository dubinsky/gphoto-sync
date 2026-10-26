package org.podval.tools.photo

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.key.TomlKey
import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import scala.jdk.CollectionConverters.{IteratorHasAsScala, ListHasAsScala, SetHasAsScala}
import java.io.File
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeParseException

// Metadata's codec accepts unknown keys. Album files must reject them.
sealed trait Album:
  def title: String
  def exclude: Set[String]
  def external: Boolean
  def googleAlbumId: String
  def publishedTitle: String

object Album:
  final case class Event(
    title: String,
    pathDay: LocalDate,
    from: Option[When],
    to: Option[When],
    nameFrom: Option[String],
    nameTo: Option[String],
    exclude: Set[String],
    external: Boolean,
    googleAlbumId: String,
    publishedTitle: String
  ) extends Album

  final case class Day(
    title: String,
    exclude: Set[String],
    external: Boolean,
    googleAlbumId: String,
    publishedTitle: String
  ) extends Album

  final case class Month(
    title: String,
    exclude: Set[String],
    external: Boolean,
    googleAlbumId: String,
    publishedTitle: String
  ) extends Album

  final case class Curated(
    title: String,
    members: Seq[String],
    exclude: Set[String],
    external: Boolean,
    googleAlbumId: String,
    publishedTitle: String
  ) extends Album

  sealed trait When
  object When:
    final case class Date(value: LocalDate) extends When
    final case class DateTime(value: LocalDateTime) extends When

  def relative(originals: File, file: File): String =
    originals.toPath.relativize(file.toPath).iterator.asScala.map(_.toString).mkString("/")

  def title(originals: File, file: File): String = relative(originals, file).stripSuffix(".toml")

  def readEvent(file: File, title: String, pathDay: LocalDate): Event =
    val values: Map[String, TomlValue] = checked(file, eventKeys)
    val common: Common = commonOf(file, values)
    Event(
      title,
      pathDay,
      from = when(file, values, "from"),
      to = when(file, values, "to"),
      nameFrom = optionalString(file, values, "name-from"),
      nameTo = optionalString(file, values, "name-to"),
      exclude = common.exclude,
      external = common.external,
      googleAlbumId = common.googleAlbumId,
      publishedTitle = common.publishedTitle
    )

  def readDay(file: File, title: String): Day =
    val common: Common = commonOf(file, checked(file, folderKeys))
    Day(title, common.exclude, common.external, common.googleAlbumId, common.publishedTitle)

  def readMonth(file: File, title: String): Month =
    val common: Common = commonOf(file, checked(file, folderKeys))
    Month(title, common.exclude, common.external, common.googleAlbumId, common.publishedTitle)

  def readCurated(file: File, title: String): Curated =
    val values: Map[String, TomlValue] = checked(file, curatedKeys)
    val common: Common = commonOf(file, values)
    Curated(
      title,
      members = stringSeq(file, values, "members"),
      exclude = common.exclude,
      external = common.external,
      googleAlbumId = common.googleAlbumId,
      publishedTitle = common.publishedTitle
    )

  private final case class Common(
    exclude: Set[String],
    external: Boolean,
    googleAlbumId: String,
    publishedTitle: String
  )

  private val folderKeys: Set[String] = Set("exclude", "external", "google-album-id", "published-title")
  private val eventKeys: Set[String] = folderKeys ++ Set("from", "to", "name-from", "name-to")
  private val curatedKeys: Set[String] = folderKeys + "members"

  private def commonOf(file: File, values: Map[String, TomlValue]): Common = Common(
    exclude = stringSeq(file, values, "exclude").toSet,
    external = boolean(file, values, "external", default = false),
    googleAlbumId = string(file, values, "google-album-id", default = ""),
    publishedTitle = string(file, values, "published-title", default = "")
  )

  private def checked(file: File, allowed: Set[String]): Map[String, TomlValue] =
    val entries: Seq[(String, TomlValue)] = readEntries(file)
    val unknown: Seq[String] = entries.map(_._1).filterNot(allowed.contains)
    if unknown.isEmpty then entries.toMap
    else throw IllegalArgumentException(s"$file: unknown key '${unknown.mkString(", ")}'")

  private def readEntries(file: File): Seq[(String, TomlValue)] =
    val text: String = Files.read(file)
    if text.isBlank then Seq.empty
    else
      val table: TomlTable = JToml.jToml.readFromString(text)
      table.keys(false).asScala.toSeq.flatMap: key =>
        Option(table.get(key)).map(keyName(key) -> _)

  private def keyName(key: TomlKey): String =
    if key.size == 1 then key.get(0) else key.toString

  private def optionalString(file: File, values: Map[String, TomlValue], key: String): Option[String] =
    values.get(key).map(asString(file, key, _))

  private def string(file: File, values: Map[String, TomlValue], key: String, default: String): String =
    optionalString(file, values, key).getOrElse(default)

  private def boolean(file: File, values: Map[String, TomlValue], key: String, default: Boolean): Boolean =
    values.get(key).fold(default): value =>
      val primitive: TomlPrimitive = primitiveOf(file, key, value)
      if primitive.isBoolean then primitive.asBoolean
      else reject(file, key, "must be a boolean")

  private def stringSeq(file: File, values: Map[String, TomlValue], key: String): Seq[String] =
    values.get(key).fold(Seq.empty): value =>
      if !value.isArray then reject(file, key, "must be an array of strings")
      else value.asArray.asScala.toSeq.map(asString(file, key, _))

  private def when(file: File, values: Map[String, TomlValue], key: String): Option[When] =
    values.get(key).map: value =>
      val primitive: TomlPrimitive = primitiveOf(file, key, value)
      if primitive.isLocalDate then When.Date(primitive.asLocalDate)
      else if primitive.isLocalDateTime then When.DateTime(primitive.asLocalDateTime)
      else if primitive.isString then whenString(file, key, primitive.asString)
      else reject(file, key, "must be a date or a date-time")

  private def whenString(file: File, key: String, text: String): When =
    try When.DateTime(LocalDateTime.parse(text))
    catch case _: DateTimeParseException =>
      try When.Date(LocalDate.parse(text))
      catch case _: DateTimeParseException =>
        reject(file, key, s"must be a date or a date-time, not '$text'")

  private def asString(file: File, key: String, value: TomlValue): String =
    val primitive: TomlPrimitive = primitiveOf(file, key, value)
    if primitive.isString then primitive.asString
    else reject(file, key, "must be a string")

  private def primitiveOf(file: File, key: String, value: TomlValue): TomlPrimitive =
    if value.isPrimitive then value.asPrimitive
    else reject(file, key, "must be a primitive")

  private def reject(file: File, key: String, message: String): Nothing =
    throw IllegalArgumentException(s"$file: key '$key' $message")
