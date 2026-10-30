package org.podval.tools.photo

import java.io.File
import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.file.{Files as NFiles, NoSuchFileException}
import java.time.LocalDate
import java.time.format.DateTimeParseException

// The first line is compared as bytes so a rejected version does not decode or parse the rest.
private[photo] object TextLines:
  def read(file: File, header: String, versionError: String): String =
    fromBytes(NFiles.readAllBytes(file.toPath), header, versionError)

  def fromBytes(bytes: Array[Byte], header: String, versionError: String): String =
    val first: Array[Byte] = bytes.takeWhile(_ != '\n'.toByte)
    if !first.sameElements(header.getBytes(StandardCharsets.UTF_8)) then
      throw IllegalArgumentException(versionError)
    val text: String = decode(bytes)
    if text.contains('\r') then throw IllegalArgumentException("carriage return")
    if !text.endsWith("\n") then throw IllegalArgumentException("trailing partial line")
    text

  def dataLines(text: String): Seq[String] =
    val (_, rest) = text.span(_ != '\n')
    val body: String = if rest.startsWith("\n") then rest.drop(1) else ""
    if body.isEmpty then Seq.empty
    else body.dropRight(1).split("\n", -1).toSeq

  def columns(line: String, count: Int): Seq[String] =
    val parts: Seq[String] = line.split("\t", -1).toSeq
    if parts.length != count then throw IllegalArgumentException("wrong column count")
    parts

  private def decode(bytes: Array[Byte]): String =
    try
      StandardCharsets.UTF_8.newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
        .decode(ByteBuffer.wrap(bytes))
        .toString
    catch case _: CharacterCodingException =>
      throw IllegalArgumentException("not utf-8")

object Catalog:
  final case class Error(got: Int, of: Int, nameFailures: Int) derives CanEqual
  final case class Item(
    filename: String,
    date: Option[LocalDate],
    key: String,
    uploader: Option[String]
  ) derives CanEqual
  final case class Unset(
    date: Option[LocalDate],
    key: String,
    uploader: Option[String]
  ) derives CanEqual
  final case class Album(
    id: String,
    count: Int,
    title: String,
    owner: Option[String],
    autoAdd: Boolean,
    error: Option[Error],
    items: Seq[Item],
    unsets: Seq[Unset]
  ) derives CanEqual

  private val Header: String = "# gphoto-catalog 2"

  def read(file: File): Seq[Album] =
    if !file.isFile then throw IllegalArgumentException("catalog is absent")
    try assemble(TextLines.dataLines(TextLines.read(file, Header, "catalog is not version 2")).map(parseLine))
    catch case _: NoSuchFileException => throw IllegalArgumentException("catalog is absent")

  private sealed trait Line
  private final case class AlbumLine(id: String, count: Int, title: String) extends Line
  private final case class OwnerLine(id: String, label: String) extends Line
  private final case class AutoAddLine(id: String) extends Line
  private final case class ErrorLine(id: String, error: Error) extends Line
  private final case class ItemLine(id: String, item: Item) extends Line
  private final case class UnsetLine(id: String, unset: Unset) extends Line

  private final case class Draft(
    album: Option[(Int, String)] = None,
    owner: Option[String] = None,
    autoAdd: Boolean = false,
    error: Option[Error] = None,
    items: Seq[Item] = Seq.empty,
    unsets: Seq[Unset] = Seq.empty
  )

  private final case class Build(order: Seq[String], drafts: Map[String, Draft]):
    def update(id: String)(f: Draft => Draft): Build =
      val draft: Draft = drafts.getOrElse(id, Draft())
      val nextOrder: Seq[String] = if drafts.contains(id) then order else order :+ id
      copy(order = nextOrder, drafts = drafts.updated(id, f(draft)))

  private def assemble(lines: Seq[Line]): Seq[Album] =
    val build: Build = lines.foldLeft(Build(Seq.empty, Map.empty)): (acc, line) =>
      add(acc, line)
    build.order.map(id => finish(id, build.drafts(id)))

  private def add(build: Build, line: Line): Build = line match
    case AlbumLine(id, count, title) =>
      build.update(id): draft =>
        if draft.album.isDefined then throw IllegalArgumentException("second album")
        draft.copy(album = Some(count -> title))
    case OwnerLine(id, label) =>
      build.update(id): draft =>
        if draft.owner.isDefined then throw IllegalArgumentException("second owner")
        draft.copy(owner = Some(label))
    case AutoAddLine(id) =>
      build.update(id): draft =>
        if draft.autoAdd then throw IllegalArgumentException("second auto-add")
        draft.copy(autoAdd = true)
    case ErrorLine(id, error) =>
      build.update(id): draft =>
        if draft.error.isDefined then throw IllegalArgumentException("second error")
        draft.copy(error = Some(error))
    case ItemLine(id, item) =>
      build.update(id): draft =>
        if taken(draft, item.key) then throw IllegalArgumentException("repeated item key")
        draft.copy(items = draft.items :+ item)
    case UnsetLine(id, unset) =>
      build.update(id): draft =>
        if taken(draft, unset.key) then throw IllegalArgumentException("repeated item key")
        draft.copy(unsets = draft.unsets :+ unset)

  private def taken(draft: Draft, key: String): Boolean =
    draft.items.exists(_.key == key) || draft.unsets.exists(_.key == key)

  private def finish(id: String, draft: Draft): Album =
    draft.album match
      case None =>
        throw IllegalArgumentException("missing album")
      case Some((count, title)) =>
        if draft.autoAdd then
          if draft.items.nonEmpty then throw IllegalArgumentException("auto-add has an item")
          if draft.unsets.nonEmpty then throw IllegalArgumentException("auto-add has an unset")
          if draft.error.isDefined then throw IllegalArgumentException("auto-add has an error")
        else draft.error match
          case Some(error) =>
            if error.of != count then throw IllegalArgumentException("error.of does not equal count")
            val got: Int = draft.items.length + draft.unsets.length
            if error.got != got then
              throw IllegalArgumentException("error.got does not equal item lines plus unset lines")
            if error.got > error.of then throw IllegalArgumentException("error.got is greater than error.of")
            if error.nameFailures != draft.unsets.length then
              throw IllegalArgumentException("name-failures does not equal unset count")
          case None =>
            if draft.unsets.nonEmpty then throw IllegalArgumentException("unset without an error")
            if draft.items.length != count then throw IllegalArgumentException("item count does not equal count")
        Album(id, count, title, draft.owner, draft.autoAdd, draft.error, draft.items, draft.unsets)

  private def parseLine(line: String): Line =
    if line.isEmpty then throw IllegalArgumentException("blank line")
    line.split("\t", -1).toSeq match
      case Seq("album", id, count, title) =>
        AlbumLine(id, parseInt(count), title)
      case Seq("owner", id, label) =>
        OwnerLine(id, parseOwner(label))
      case Seq("auto-add", id) =>
        AutoAddLine(id)
      case Seq("error", id, got, of, failures) =>
        ErrorLine(id, Error(parseInt(got), parseInt(of), parseInt(failures)))
      case Seq("unset", id, date, key, uploader) =>
        UnsetLine(id, Unset(parseDate(date), key, parseUploader(uploader)))
      case Seq("item", id, filename, date, key, uploader) =>
        if filename.isEmpty then throw IllegalArgumentException("empty item filename")
        ItemLine(id, Item(filename, parseDate(date), key, parseUploader(uploader)))
      case Seq("album", _*) => throw IllegalArgumentException("wrong column count")
      case Seq("owner", _*) => throw IllegalArgumentException("wrong column count")
      case Seq("auto-add", _*) => throw IllegalArgumentException("wrong column count")
      case Seq("error", _*) => throw IllegalArgumentException("wrong column count")
      case Seq("unset", _*) => throw IllegalArgumentException("wrong column count")
      case Seq("item", _*) => throw IllegalArgumentException("wrong column count")
      case _ => throw IllegalArgumentException("unknown catalog line")

  private def parseInt(text: String): Int =
    if text.isEmpty || text.exists(ch => ch < '0' || ch > '9') then throw IllegalArgumentException("not an integer")
    if text.length > 1 && text.startsWith("0") then throw IllegalArgumentException("leading zero")
    try Integer.parseInt(text)
    catch case _: NumberFormatException => throw IllegalArgumentException("integer does not fit")

  private def parseDate(text: String): Option[LocalDate] =
    if text == "-" then None
    else if text.isEmpty then throw IllegalArgumentException("empty date")
    else
      try
        val date: LocalDate = LocalDate.parse(text)
        if date.toString != text then throw IllegalArgumentException("not a date")
        Some(date)
      catch case _: DateTimeParseException =>
        throw IllegalArgumentException("not a date")

  private def parseUploader(text: String): Option[String] =
    if text == "-" then None
    else if text.isEmpty then throw IllegalArgumentException("empty uploader")
    else Some(text)

  private def parseOwner(text: String): String =
    if text.isEmpty then throw IllegalArgumentException("owner label is empty")
    else if text.forall(_.isWhitespace) then throw IllegalArgumentException("owner label is only whitespace")
    else text
