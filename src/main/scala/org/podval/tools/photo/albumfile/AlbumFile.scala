package org.podval.tools.photo.albumfile

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.key.TomlKey
import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import org.podval.tools.photo.{Album, AlbumRole, Files, Include, Publication, Title}
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}
import java.io.File

private[photo] object AlbumFile:
  private[photo] def read(
    originals: File,
    file: File
  ): (String, Option[Title.Span], Seq[Include.Entry], AlbumRole) =
    val parsed: Title.Parsed = Title
      .fromLocation(directoryOf(originals, file), stemOf(file))
      .getOrElse(
        throw IllegalArgumentException(s"$file: not an album file")
      )
    val values: Map[String, TomlValue] = checked(file, keys)
    val common: Common = commonOf(file, values)
    val span: Option[Title.Span] = parsed.span
    val include: Seq[Include.Entry] = stringSeq(file, values, "include").map: text =>
      val entry: Include.Entry =
        try Include.parse(text)
        catch
          case error: IllegalArgumentException =>
            reject(file, "include", Album.messageOf(error))
      if Include.inside(span, entry) then entry
      else reject(file, "include", s"'$text' is outside the album")
    val googleTitle: Option[String] = Option.when(common.googleTitle.nonEmpty)(common.googleTitle)
    val role: AlbumRole =
      if common.external then AlbumRole.External(googleTitle, common.owner)
      else if common.googleAlbumId.nonEmpty then
        AlbumRole.Client(googleTitle, Publication.Published(common.googleAlbumId, common.publishedTitle))
      else
        AlbumRole.Client(
          googleTitle,
          Publication.Pending(Option.when(common.publishedTitle.nonEmpty)(common.publishedTitle))
        )
    (parsed.title, span, include, role)

  // Hand-written order.
  private[photo] def renderFile(album: Album): String =
    def googleLine(googleTitle: Option[String]): Seq[String] =
      googleTitle.filter(_.nonEmpty).map(text => s"google-title = ${Album.quote(text)}").toSeq
    val head: Seq[String] = album.role match
      case AlbumRole.External(googleTitle, owner) =>
        googleLine(googleTitle) ++ owner.map(name => s"owner = ${Album.quote(name)}").toSeq
      case AlbumRole.Client(googleTitle, Publication.Pending(_)) =>
        Seq("external = false") ++ googleLine(googleTitle)
      case AlbumRole.Client(googleTitle, Publication.Published(id, published)) =>
        Seq("external = false") ++ googleLine(googleTitle) ++ Seq(
          s"google-album-id = ${Album.quote(id)}",
          s"published-title = ${Album.quote(published)}"
        )
    val lines: Seq[String] =
      head ++ (if album.include.isEmpty then Seq.empty else includeLines(album.include))
    if lines.isEmpty then "" else lines.mkString("", "\n", "\n")

  private[photo] def quoteInclude(text: String): String =
    if text.indexOf('\'') >= 0 then Album.quote(text) else "'" + text + "'"

  private[photo] def renderedInclude(file: File, include: Seq[Include.Entry]): String =
    val text: String = Files.read(file)
    val lines: Seq[TextLine] = splitLines(text)
    val block: Seq[TextLine] =
      if include.isEmpty then Seq(TextLine("include = []", ""))
      else blockLines(includeLines(include))
    val next: Seq[TextLine] = findInclude(file, lines) match
      case Some((from, to))             => replaceSpan(lines, from, to, block)
      case None if includePresent(text) =>
        throw IllegalArgumentException(s"$file: include array does not end on its own line")
      case None => appendBlock(lines, block)
    joinLines(next)

  // jtoml sorts keys and drops comments, so a title change is one assignment line.
  // googleTitle None leaves that key. Some(None) removes it. Some(Some(text)) sets it.
  // publishedTitle None leaves that key.
  private[photo] def spliceTitles(
    file: File,
    original: String,
    googleTitle: Option[Option[String]],
    publishedTitle: Option[String]
  ): String =
    val titled: Seq[TextLine] = googleTitle match
      case None        => splitLines(original)
      case Some(value) => assign(file, splitLines(original), "google-title", value)
    val next: Seq[TextLine] = publishedTitle match
      case None       => titled
      case Some(text) => assign(file, titled, "published-title", Some(text))
    joinLines(next)

  private[photo] def blankGoogle(id: String, title: String): String =
    s"external = false\ngoogle-album-id = ${Album.quote(id)}\npublished-title = ${Album.quote(title)}\n"

  private[photo] def spliceGoogleText(
    file: File,
    text: String,
    googleAlbumId: String,
    publishedTitle: String
  ): String =
    val table: TomlTable = JToml.jToml.readFromString(text)
    val values: Map[String, String] = Map(
      "google-album-id" -> googleAlbumId,
      "published-title" -> publishedTitle
    )
    val present: Set[String] = values.keySet.filter(key => hasKey(table, key))
    joinLines(spliceGoogle(file, splitLines(text), values, present))

  private[photo] def includePresent(text: String): Boolean =
    !text.isBlank && hasKey(JToml.jToml.readFromString(text), "include")

  // None removes a present line. Leaving published-title alone is the None branch in spliceTitles.
  private def assign(
    file: File,
    lines: Seq[TextLine],
    key: String,
    value: Option[String]
  ): Seq[TextLine] =
    val at: Seq[Int] = lines.zipWithIndex.collect:
      case (line, index) if singleAssignment(line.text, key) => index
    if at.sizeIs > 1 then failGoogle(file, s"$key is not a single line")
    (at.headOption, value) match
      case (None, None) =>
        lines
      case (Some(index), None) =>
        lines.patch(index, Seq.empty, 1)
      case (Some(index), Some(text)) =>
        lines.updated(index, lines(index).copy(text = s"$key = ${Album.quote(text)}"))
      case (None, Some(text)) =>
        insertAssignment(lines, key, TextLine(s"$key = ${Album.quote(text)}", "\n"))

  private def insertAssignment(lines: Seq[TextLine], key: String, added: TextLine): Seq[TextLine] =
    val beforeKey: Set[String] = key match
      case "google-title"    => Set("owner", "google-album-id", "published-title", "include")
      case "published-title" => Set("include")
      case _                 => Set.empty
    val pivot: Option[Int] = lines.zipWithIndex.collectFirst:
      case (line, index) if beforeKey.exists(name => singleAssignment(line.text, name)) => index
    pivot match
      case Some(index) =>
        val (before, after) = lines.splitAt(index)
        val base: Seq[TextLine] = before match
          case prefix :+ last if last.ending.isEmpty && last.text.nonEmpty =>
            prefix :+ last.copy(ending = "\n")
          case _ => before
        base ++ Seq(added) ++ after
      case None =>
        appendBlock(lines, Seq(added))

  private final case class Common(
    external: Boolean,
    googleAlbumId: String,
    publishedTitle: String,
    googleTitle: String,
    owner: Option[String]
  )

  private val keys: Set[String] =
    Set("external", "google-album-id", "published-title", "google-title", "owner", "include")

  private def commonOf(file: File, values: Map[String, TomlValue]): Common =
    val common: Common = readCommon(file, values)
    checkGoogle(file, common)
    common

  private def readCommon(file: File, values: Map[String, TomlValue]): Common = Common(
    external = boolean(file, values, "external", default = true),
    googleAlbumId = string(file, values, "google-album-id", default = ""),
    publishedTitle = string(file, values, "published-title", default = ""),
    googleTitle = string(file, values, "google-title", default = ""),
    owner = optionalString(file, values, "owner")
  )

  // Spaces are significant. A title of only spaces is a title. Tab, CR, and LF are not.
  private def checkGoogle(file: File, common: Common): Unit =
    rejectControl(file, "google-title", common.googleTitle)
    common.owner.foreach(name => rejectControl(file, "owner", name))
    common.owner.foreach: name =>
      if name.trim.isEmpty then failGoogle(file, "owner must be a non-empty display name")
      else if !common.external then failGoogle(file, "owner requires external = true")
      else if common.googleAlbumId.nonEmpty then failGoogle(file, "owner must not be combined with google-album-id")
      else if common.googleTitle.isEmpty then failGoogle(file, "owner requires a non-empty google-title")
    if common.googleAlbumId.nonEmpty && common.external then
      failGoogle(file, "google-album-id requires external = false")

  private[photo] def rejectControl(file: File, key: String, text: String): Unit =
    if text.exists(ch => ch == '\t' || ch == '\r' || ch == '\n') then
      reject(file, key, "must not contain a tab, CR, or LF")

  private def failGoogle(file: File, message: String): Nothing =
    throw IllegalArgumentException(s"$file: $message")

  private def checked(file: File, allowed: Set[String]): Map[String, TomlValue] =
    parsed(file, allowed)._2

  private def parsed(file: File, allowed: Set[String]): (TomlTable, Map[String, TomlValue]) =
    val text: String = Files.read(file)
    val table: TomlTable = if text.isBlank then TomlTable.create() else JToml.jToml.readFromString(text)
    val entries: Seq[(String, TomlValue)] = table
      .keys(false)
      .asScala
      .toSeq
      .flatMap: key =>
        Option(table.get(key)).map(keyName(key) -> _)
    val unknown: Seq[String] = entries.map(_._1).filterNot(allowed.contains)
    if unknown.isEmpty then (table, entries.toMap)
    else throw IllegalArgumentException(s"$file: unknown key '${unknown.mkString(", ")}'")

  private def keyName(key: TomlKey): String =
    if key.size == 1 then key.get(0) else key.toString

  private def optionalString(file: File, values: Map[String, TomlValue], key: String): Option[String] =
    values.get(key).map(asString(file, key, _))

  private def string(file: File, values: Map[String, TomlValue], key: String, default: String): String =
    optionalString(file, values, key).getOrElse(default)

  private def boolean(file: File, values: Map[String, TomlValue], key: String, default: Boolean): Boolean =
    values
      .get(key)
      .fold(default): value =>
        val primitive: TomlPrimitive = primitiveOf(file, key, value)
        if primitive.isBoolean then primitive.asBoolean
        else reject(file, key, "must be a boolean")

  private def stringSeq(file: File, values: Map[String, TomlValue], key: String): Seq[String] =
    values
      .get(key)
      .fold(Seq.empty): value =>
        if !value.isArray then reject(file, key, "must be an array of strings")
        else value.asArray.asScala.toSeq.map(asString(file, key, _))

  private def asString(file: File, key: String, value: TomlValue): String =
    val primitive: TomlPrimitive = primitiveOf(file, key, value)
    if primitive.isString then primitive.asString
    else reject(file, key, "must be a string")

  private def primitiveOf(file: File, key: String, value: TomlValue): TomlPrimitive =
    if value.isPrimitive then value.asPrimitive
    else reject(file, key, "must be a primitive")

  private def reject(file: File, key: String, message: String): Nothing =
    throw IllegalArgumentException(s"$file: key '$key' $message")

  private def directoryOf(originals: File, file: File): String =
    val directory: String = Album.relative(originals, file.getParentFile)
    if directory == "." then "" else directory

  private def stemOf(file: File): String =
    val (name, extension) = Files.nameAndExtension(file)
    if extension.contains("toml") then name
    else throw IllegalArgumentException(s"$file: not an album file")

  private def includeLines(include: Seq[Include.Entry]): Seq[String] =
    Seq("include = [") ++ include.map(entry => s"  ${quoteInclude(Include.render(entry))},") ++ Seq("]")

  private def hasKey(table: TomlTable, name: String): Boolean =
    table.keys(false).asScala.exists(key => keyName(key) == name)

  private final case class TextLine(text: String, ending: String)

  private def splitLines(text: String): Seq[TextLine] =
    def loop(rest: String): Seq[TextLine] =
      if rest.isEmpty then Seq.empty
      else
        val (body, afterBody) = rest.span(ch => ch != '\n' && ch != '\r')
        val (ending, after) =
          if afterBody.startsWith("\r\n") then ("\r\n", afterBody.substring(2))
          else if afterBody.startsWith("\n") then ("\n", afterBody.substring(1))
          else if afterBody.startsWith("\r") then ("\r", afterBody.substring(1))
          else ("", afterBody)
        TextLine(body, ending) +: loop(after)
    loop(text)

  private def joinLines(lines: Seq[TextLine]): String =
    lines.map(line => line.text + line.ending).mkString

  private def blockLines(bodies: Seq[String]): Seq[TextLine] = bodies match
    case Seq()        => Seq.empty
    case Seq(last)    => Seq(TextLine(last, ""))
    case head +: tail => TextLine(head, "\n") +: blockLines(tail)

  private def appendBlock(lines: Seq[TextLine], block: Seq[TextLine]): Seq[TextLine] =
    val base: Seq[TextLine] = lines match
      case prefix :+ last if last.ending.isEmpty && last.text.nonEmpty =>
        prefix :+ last.copy(ending = "\n")
      case _ => lines
    val ended: Seq[TextLine] = block match
      case prefix :+ last if last.ending.isEmpty => prefix :+ last.copy(ending = "\n")
      case _                                     => block
    base ++ ended

  private def replaceSpan(lines: Seq[TextLine], from: Int, to: Int, block: Seq[TextLine]): Seq[TextLine] =
    val (before, rest) = lines.splitAt(from)
    val (span, after) = rest.splitAt(to - from + 1)
    val ending: String = span.lastOption.map(_.ending).getOrElse("")
    val fitted: Seq[TextLine] = block match
      case Seq()          => Seq.empty
      case prefix :+ last => prefix :+ last.copy(ending = ending)
    before ++ fitted ++ after

  private def spliceGoogle(
    file: File,
    lines: Seq[TextLine],
    values: Map[String, String],
    present: Set[String]
  ): Seq[TextLine] =
    val slots: Map[String, Int] = present
      .map: key =>
        val found: Seq[Int] = lines.zipWithIndex.collect:
          case (line, index) if singleAssignment(line.text, key) => index
        if found.sizeIs != 1 then failGoogle(file, s"$key is not a single line")
        key -> found.head
      .toMap
    val missing: Seq[String] = Seq("google-album-id", "published-title").filterNot(present.contains)
    val includeAt: Option[Int] = lines.zipWithIndex.collectFirst:
      case (line, index) if assignmentRest(line.text.trim, "include").isDefined => index
    val replaced: Seq[TextLine] = lines.zipWithIndex.map: (line, index) =>
      slots
        .collectFirst:
          case (key, at) if at == index => line.copy(text = s"$key = ${Album.quote(values(key))}")
        .getOrElse(line)
    val added: Seq[TextLine] = missing.map(key => TextLine(s"$key = ${Album.quote(values(key))}", "\n"))
    includeAt match
      case Some(index) =>
        val (before, after) = replaced.splitAt(index)
        before ++ added ++ after
      case None =>
        appendBlock(replaced, added)

  private def findInclude(file: File, lines: Seq[TextLine]): Option[(Int, Int)] =
    val opens: Seq[Int] = lines.zipWithIndex.collect:
      case (line, index) if assignmentRest(line.text.trim, "include").isDefined => index
    opens match
      case Seq()      => None
      case Seq(index) => Some(includeBounds(file, lines, index))
      case _          => throw IllegalArgumentException(s"$file: include is not a single array")

  private def includeBounds(file: File, lines: Seq[TextLine], index: Int): (Int, Int) =
    val value: String = assignmentRest(lines(index).text.trim, "include").getOrElse("")
    if value == "[" then
      lines.zipWithIndex
        .drop(index + 1)
        .collectFirst:
          case (line, end) if line.text.trim == "]" => index -> end
        .getOrElse(throw IllegalArgumentException(s"$file: include array does not end on its own line"))
    else if value.startsWith("[") && arrayClosed(value) then index -> index
    else throw IllegalArgumentException(s"$file: include array does not end on its own line")

  private def isTomlWs(ch: Char): Boolean = ch == ' ' || ch == '\t'

  private def singleAssignment(line: String, key: String): Boolean =
    assignmentRest(line.trim, key).exists(valueClosed)

  private def assignmentRest(trimmed: String, key: String): Option[String] =
    keyRest(trimmed, key).flatMap: afterKey =>
      val after: String = afterKey.dropWhile(isTomlWs)
      if after.startsWith("=") then Some(after.substring(1).dropWhile(isTomlWs)) else None

  // A bare key, or a basic or literal quoted key. Space and tab may surround '='.
  private def keyRest(trimmed: String, key: String): Option[String] =
    if trimmed.startsWith("\"") && !trimmed.startsWith("\"\"\"") then
      takeBasic(trimmed.substring(1)).flatMap: (decoded, rest) =>
        if decoded == key then Some(rest) else None
    else if trimmed.startsWith("'") && !trimmed.startsWith("'''") then
      takeLiteral(trimmed.substring(1)).flatMap: (decoded, rest) =>
        if decoded == key then Some(rest) else None
    else if trimmed.startsWith(key) then
      val rest: String = trimmed.substring(key.length)
      if rest.isEmpty || isTomlWs(rest.head) || rest.startsWith("=") then Some(rest) else None
    else None

  // An escaped delimiter does not finish a multiline string. A trailing '\' continues the line.
  private def valueClosed(value: String): Boolean =
    if value.startsWith("\"\"\"") then skipMlBasic(value.substring(3)).exists(tailAfterValue)
    else if value.startsWith("'''") then skipMlLiteral(value.substring(3)).exists(tailAfterValue)
    else true

  private def tailAfterValue(rest: String): Boolean =
    val tail: String = rest.dropWhile(isTomlWs)
    tail.isEmpty || tail.startsWith("#")

  private def arrayClosed(value: String): Boolean =
    closedArray(value).exists(tailAfterValue)

  private def closedArray(value: String): Option[String] =
    if !value.startsWith("[") then None else scanValues(value.substring(1), 1)

  private def scanValues(rest: String, depth: Int): Option[String] =
    if depth == 0 then Some(rest)
    else if rest.isEmpty then None
    else if rest.startsWith("\"\"\"") then skipMlBasic(rest.substring(3)).flatMap(next => scanValues(next, depth))
    else if rest.startsWith("'''") then skipMlLiteral(rest.substring(3)).flatMap(next => scanValues(next, depth))
    else if rest.startsWith("\"") then skipBasic(rest.substring(1)).flatMap(next => scanValues(next, depth))
    else if rest.startsWith("'") then skipLiteral(rest.substring(1)).flatMap(next => scanValues(next, depth))
    else if rest.startsWith("#") then None
    else
      rest.head match
        case '[' => scanValues(rest.substring(1), depth + 1)
        case ']' => scanValues(rest.substring(1), depth - 1)
        case _   => scanValues(rest.substring(1), depth)

  private def skipMlBasic(rest: String): Option[String] =
    if rest.isEmpty then None
    else if rest.startsWith("\\") then if rest.length == 1 then None else skipMlBasic(rest.substring(2))
    else if rest.startsWith("\"\"\"") then Some(rest.substring(3))
    else skipMlBasic(rest.substring(1))

  private def skipMlLiteral(rest: String): Option[String] =
    val at: Int = rest.indexOf("'''")
    if at < 0 then None else Some(rest.substring(at + 3))

  private def skipBasic(rest: String): Option[String] = takeBasic(rest).map((_, after) => after)

  private def takeBasic(rest: String): Option[(String, String)] =
    def loop(rest: String, acc: String): Option[(String, String)] =
      if rest.isEmpty then None
      else if rest.startsWith("\\") then
        if rest.length == 1 then None
        else loop(rest.substring(2), acc + unescapeBasic(rest.substring(1, 2)))
      else if rest.startsWith("\"") then Some((acc, rest.substring(1)))
      else loop(rest.substring(1), acc + rest.substring(0, 1))
    loop(rest, "")

  private def unescapeBasic(ch: String): String = ch match
    case "b"   => "\b"
    case "t"   => "\t"
    case "n"   => "\n"
    case "f"   => "\f"
    case "r"   => "\r"
    case "\""  => "\""
    case "\\"  => "\\"
    case other => other

  private def skipLiteral(rest: String): Option[String] = takeLiteral(rest).map((_, after) => after)

  private def takeLiteral(rest: String): Option[(String, String)] =
    def loop(rest: String, acc: String): Option[(String, String)] =
      if rest.isEmpty then None
      else if rest.startsWith("'") then Some((acc, rest.substring(1)))
      else loop(rest.substring(1), acc + rest.substring(0, 1))
    loop(rest, "")
