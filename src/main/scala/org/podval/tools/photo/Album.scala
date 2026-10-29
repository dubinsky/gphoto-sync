package org.podval.tools.photo

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.key.TomlKey
import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.array.TomlArray
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import scala.jdk.CollectionConverters.{IteratorHasAsScala, ListHasAsScala, SetHasAsScala}
import java.io.File
import java.time.LocalDate

// One album. The title is not the path: the file sits in the start folder, and a span's end is `to-` in the name.
final case class Album(
  title: String,
  span: Option[Title.Span],
  include: Seq[Include.Entry],
  external: Boolean,
  googleAlbumId: String,
  publishedTitle: String,
  googleTitle: String,
  owner: Option[String]
):
  def desiredGoogleTitle: String =
    if googleTitle.nonEmpty then googleTitle else title

  def startDir: String = span.fold(title)(_.startDir)

  def claims(day: LocalDate, stem: String): Boolean =
    include.exists(entry => Include.claims(span, entry, day, stem))

  def positive(day: LocalDate, stem: String): Boolean =
    include.exists(entry => Include.positive(span, entry, day, stem))

object Album:
  def relative(originals: File, file: File): String =
    originals.toPath.relativize(file.toPath).iterator.asScala.map(_.toString).mkString("/")

  def fileFor(originals: File, title: String): File =
    Title.parse(title) match
      case Some(parsed) => Title.file(originals, parsed)
      case None => throw IllegalArgumentException(s"not an album title: $title")

  def read(originals: File, file: File): Album =
    val parsed: Title.Parsed = Title.fromLocation(directoryOf(originals, file), stemOf(file)).getOrElse(
      throw IllegalArgumentException(s"$file: not an album file")
    )
    val values: Map[String, TomlValue] = checked(file, keys)
    val common: Common = commonOf(file, values)
    val span: Option[Title.Span] = parsed.span
    val include: Seq[Include.Entry] = stringSeq(file, values, "include").map: text =>
      val entry: Include.Entry =
        try Include.parse(text)
        catch case error: IllegalArgumentException =>
          reject(file, "include", Album.messageOf(error))
      if Include.inside(span, entry) then entry
      else reject(file, "include", s"'$text' is outside the album")
    Album(
      title = parsed.title,
      span = span,
      include = include,
      external = common.external,
      googleAlbumId = common.googleAlbumId,
      publishedTitle = common.publishedTitle,
      googleTitle = common.googleTitle,
      owner = common.owner
    )

  // Hand-written order. jtoml is only for storeGoogle and the sync rewrite.
  def renderFile(album: Album): String =
    val lines: Seq[String] = Seq(
      Option.when(album.external)(Seq("external = true")).getOrElse(Seq.empty),
      Option.when(album.googleTitle.nonEmpty)(Seq(s"google-title = ${quote(album.googleTitle)}")).getOrElse(Seq.empty),
      album.owner.map(name => s"owner = ${quote(name)}").toSeq,
      if album.googleAlbumId.isEmpty then Seq.empty
      else Seq(
        s"google-album-id = ${quote(album.googleAlbumId)}",
        s"published-title = ${quote(album.publishedTitle)}"
      ),
      if album.include.isEmpty then Seq.empty
      else Seq("include = [") ++ album.include.map(entry => s"  ${quote(Include.render(entry))},") ++ Seq("]")
    ).flatten
    if lines.isEmpty then "" else lines.mkString("", "\n", "\n")

  private[photo] final case class Loaded(file: File, album: Album)

  private[photo] def needsRecursive(prefix: Option[String]): Boolean =
    val segments: Seq[String] = prefix.filter(_.nonEmpty).toSeq.flatMap(_.split('/').filter(_.nonEmpty))
    segments.isEmpty || (segments.sizeIs == 1 && isNumber(segments.head, 4))

  def pictureMatches(path: String, prefix: Option[String]): Boolean =
    prefix.filter(_.nonEmpty).forall(selected => path == selected || path.startsWith(s"$selected/"))

  def albumMatches(startDir: String, title: String, prefix: Option[String]): Boolean =
    prefix.filter(_.nonEmpty).forall: selected =>
      val titleHit: Boolean = title == selected || title.startsWith(s"$selected/")
      val startHit: Boolean = startDir == selected || startDir.startsWith(s"$selected/")
      titleHit || startHit

  private[photo] def messageOf(error: Exception): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)

  def quote(text: String): String =
    "\"" + text.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  // Same JToml table the reader accepted. Do not hand-write TOML.
  def storeGoogle(file: File, googleAlbumId: String, publishedTitle: String): Unit =
    val text: String = Files.read(file)
    val table: TomlTable =
      if text.isBlank then TomlTable.create()
      else JToml.jToml.readFromString(text)
    table.put("google-album-id", googleAlbumId)
    table.put("published-title", publishedTitle)
    Files.write(JToml.jToml.writeToString(table), file)

  // Recompress each include entry that claims one of these stems. An empty album writes include = [].
  def storeRemovals(
    file: File,
    originals: File,
    removals: Seq[(LocalDate, String)],
    universe: Map[LocalDate, Set[String]]
  ): Unit =
    val album: Album = read(originals, file)
    val next: Seq[Include.Entry] = removals.foldLeft(album.include): (include, pair) =>
      Include.drop(include, album.span, pair, universe).sortBy(Include.logical)
    if !sameEntries(next, album.include) then putInclude(file, next)

  // Curated include paths, before any picture moves. A bad curated file fails first.
  def rewriteInclude(originals: File, from: String, to: String): Unit =
    if from != to then
      val fromPair: (LocalDate, String) = splitPath(from)
      val toPair: (LocalDate, String) = splitPath(to)
      val universe: Map[LocalDate, Set[String]] = Archive.universe(Archive.pictures(originals))
      val loaded: Seq[(File, Album)] = curatedFiles(originals).map(file => file -> read(originals, file))
      val changes: Seq[(File, Seq[Include.Entry])] = loaded.flatMap: (file, album) =>
        val next: Seq[Include.Entry] = Include.relocate(album.include, album.span, fromPair, toPair, universe)
        Option.when(!sameEntries(next, album.include))(file -> next)
      changes.foreach: (file, include) =>
        putInclude(file, include)

  def splitPath(path: String): (LocalDate, String) =
    path.split('/').filter(_.nonEmpty).toSeq match
      case Seq(year, month, day, stem) if isNumber(year, 4) && isNumber(month, 2) && isNumber(day, 2) =>
        (LocalDate.of(year.toInt, month.toInt, day.toInt), stem)
      case _ =>
        throw IllegalArgumentException(s"not a picture path: $path")

  private def sameEntries(left: Seq[Include.Entry], right: Seq[Include.Entry]): Boolean =
    left.map(Include.render).equals(right.map(Include.render))

  private def directoryOf(originals: File, file: File): String =
    val directory: String = relative(originals, file.getParentFile)
    if directory == "." then "" else directory

  private def stemOf(file: File): String =
    val (name, extension) = Files.nameAndExtension(file)
    if extension.contains("toml") then name
    else throw IllegalArgumentException(s"$file: not an album file")

  private def putInclude(file: File, include: Seq[Include.Entry]): Unit =
    val text: String = Files.read(file)
    val table: TomlTable =
      if text.isBlank then TomlTable.create()
      else JToml.jToml.readFromString(text)
    table.put("include", tomlArray(include.map(Include.render)))
    Files.write(JToml.jToml.writeToString(table), file)

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
    external = boolean(file, values, "external", default = false),
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

  private def rejectControl(file: File, key: String, text: String): Unit =
    if text.exists(ch => ch == '\t' || ch == '\r' || ch == '\n') then
      reject(file, key, "must not contain a tab, CR, or LF")

  private def failGoogle(file: File, message: String): Nothing =
    throw IllegalArgumentException(s"$file: $message")

  private def checked(file: File, allowed: Set[String]): Map[String, TomlValue] =
    parsed(file, allowed)._2

  private def parsed(file: File, allowed: Set[String]): (TomlTable, Map[String, TomlValue]) =
    val text: String = Files.read(file)
    val table: TomlTable = if text.isBlank then TomlTable.create() else JToml.jToml.readFromString(text)
    val entries: Seq[(String, TomlValue)] = table.keys(false).asScala.toSeq.flatMap: key =>
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
    values.get(key).fold(default): value =>
      val primitive: TomlPrimitive = primitiveOf(file, key, value)
      if primitive.isBoolean then primitive.asBoolean
      else reject(file, key, "must be a boolean")

  private def stringSeq(file: File, values: Map[String, TomlValue], key: String): Seq[String] =
    values.get(key).fold(Seq.empty): value =>
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

  private def curatedFiles(originals: File): Seq[File] =
    Option(originals.listFiles).map(_.toSeq).getOrElse(Seq.empty)
      .filter(file => file.isFile && file.getName.endsWith(".toml"))
      .sortBy(_.getName)

  private def isNumber(name: String, length: Int): Boolean =
    name.length == length && name.forall(_.isDigit)

  private def tomlArray(values: Seq[String]): TomlArray =
    val array: TomlArray = TomlArray.create()
    values.foreach(value => array.add(TomlPrimitive.of(value)))
    array
