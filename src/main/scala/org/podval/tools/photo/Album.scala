package org.podval.tools.photo

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.key.TomlKey
import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import scala.annotation.tailrec
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

  // Curated albums are the .toml files directly in the originals directory, as Claims loads them.
  // Only `members` changes; exclude is picture names, and the other keys are left as written.
  def rewriteMember(originals: File, from: String, to: String): Unit =
    if from != to then
      val changes: Seq[(File, Seq[String])] = curatedFiles(originals).flatMap: file =>
        val curated: Curated = readCurated(file, title(originals, file))
        Option.when(curated.members.contains(from))(file -> moveMember(curated.members, from, to))
      changes.foreach: (file, members) =>
        Files.write(replaceMembers(file, Files.read(file), members), file)

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

  private def curatedFiles(originals: File): Seq[File] =
    Option(originals.listFiles).map(_.toSeq).getOrElse(Seq.empty)
      .filter(file => file.isFile && file.getName.endsWith(".toml"))
      .sortBy(_.getName)

  private def moveMember(members: Seq[String], from: String, to: String): Seq[String] =
    members.foldLeft(Seq.empty[String]): (acc, member) =>
      val path: String = if member == from then to else member
      if path == to && acc.contains(to) then acc else acc :+ path

  private def replaceMembers(file: File, text: String, members: Seq[String]): String =
    val (before, array) = findMembers(file, text, text)
    val after: String = skipNested(file, array.substring(1), List(Nest.Array))
    before + members.map(quote).mkString("[", ", ", "]") + after

  private def quote(value: String): String =
    val body: String = value.flatMap:
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case c if c.isControl => f"\\u${c.toInt}%04x"
      case c => c.toString
    s"\"$body\""

  private enum Nest:
    case Array, Table

  @tailrec
  private def findMembers(file: File, original: String, rest: String): (String, String) =
    val ready: String = skipTop(rest)
    if ready.isEmpty then fail(file, "members array not found")
    else
      val (key, afterKey) = readKey(file, ready)
      val value: String = skipTop(expectEquals(file, afterKey))
      if key == "members" then
        if !value.startsWith("[") then fail(file, "members must be an array")
        else (original.substring(0, original.length - value.length), value)
      else findMembers(file, original, skipValue(file, value))

  private def readKey(file: File, rest: String): (String, String) =
    if rest.startsWith("\"") || rest.startsWith("'") then takeQuoted(file, rest)
    else
      val (key, after) = rest.span(c => c.isLetterOrDigit || c == '_' || c == '-' || c == '.')
      if key.isEmpty then fail(file, s"expected a key near '${rest.take(24)}'")
      else (key, after)

  private def expectEquals(file: File, rest: String): String =
    val after: String = skipSpaces(rest)
    if after.startsWith("=") then after.substring(1) else fail(file, "expected '='")

  private def skipValue(file: File, rest: String): String =
    if rest.isEmpty then fail(file, "missing value")
    else if rest.startsWith("\"") || rest.startsWith("'") then takeQuoted(file, rest)._2
    else if rest.startsWith("[") then skipNested(file, rest.substring(1), List(Nest.Array))
    else if rest.startsWith("{") then skipNested(file, rest.substring(1), List(Nest.Table))
    else rest.dropWhile(c => c != '\n' && c != '#')

  @tailrec
  private def skipNested(file: File, rest: String, stack: List[Nest]): String =
    if rest.isEmpty then fail(file, "unclosed members array")
    else if rest.startsWith("\"") || rest.startsWith("'") then skipNested(file, takeQuoted(file, rest)._2, stack)
    else if rest.head == '#' then skipNested(file, rest.dropWhile(_ != '\n'), stack)
    else rest.head match
      case '[' => skipNested(file, rest.substring(1), Nest.Array :: stack)
      case '{' => skipNested(file, rest.substring(1), Nest.Table :: stack)
      case ']' => stack match
        case Nest.Array :: Nil => rest.substring(1)
        case Nest.Array :: tail => skipNested(file, rest.substring(1), tail)
        case _ => fail(file, "unexpected ']'")
      case '}' => stack match
        case Nest.Table :: Nil => rest.substring(1)
        case Nest.Table :: tail => skipNested(file, rest.substring(1), tail)
        case _ => fail(file, "unexpected '}'")
      case _ => skipNested(file, rest.substring(1), stack)

  @tailrec
  private def skipTop(rest: String): String =
    if rest.isEmpty then rest
    else if rest.head == ' ' || rest.head == '\t' || rest.head == '\n' || rest.head == '\r' then
      skipTop(rest.substring(1))
    else if rest.head == '#' then
      val line: String = rest.dropWhile(_ != '\n')
      skipTop(if line.startsWith("\n") then line.substring(1) else "")
    else rest

  private def skipSpaces(rest: String): String =
    rest.dropWhile(c => c == ' ' || c == '\t' || c == '\n' || c == '\r')

  private def takeQuoted(file: File, rest: String): (String, String) =
    if rest.startsWith("\"\"\"") then takeMulti(file, rest.substring(3), "\"\"\"", escapes = true, "", false)
    else if rest.startsWith("\"") then takeBasic(file, rest.substring(1), "", false)
    else if rest.startsWith("'''") then takeMulti(file, rest.substring(3), "'''", escapes = false, "", false)
    else if rest.startsWith("'") then takeLiteral(file, rest.substring(1), "")
    else fail(file, "expected a string")

  @tailrec
  private def takeBasic(file: File, rest: String, acc: String, escape: Boolean): (String, String) =
    if rest.isEmpty then fail(file, "unclosed string")
    else if escape then takeBasic(file, rest.substring(1), acc + rest.head, false)
    else if rest.head == '\\' then takeBasic(file, rest.substring(1), acc, true)
    else if rest.head == '"' then (acc, rest.substring(1))
    else takeBasic(file, rest.substring(1), acc + rest.head, false)

  @tailrec
  private def takeLiteral(file: File, rest: String, acc: String): (String, String) =
    if rest.isEmpty then fail(file, "unclosed string")
    else if rest.head == '\'' then (acc, rest.substring(1))
    else takeLiteral(file, rest.substring(1), acc + rest.head)

  @tailrec
  private def takeMulti(
    file: File,
    rest: String,
    close: String,
    escapes: Boolean,
    acc: String,
    escape: Boolean
  ): (String, String) =
    if rest.isEmpty then fail(file, "unclosed string")
    else if escape then takeMulti(file, rest.substring(1), close, escapes, acc + rest.head, false)
    else if escapes && rest.head == '\\' then takeMulti(file, rest.substring(1), close, escapes, acc, true)
    else if rest.startsWith(close) then (acc, rest.substring(close.length))
    else takeMulti(file, rest.substring(1), close, escapes, acc + rest.head, false)

  private def fail(file: File, message: String): Nothing =
    throw IllegalArgumentException(s"$file: $message")
