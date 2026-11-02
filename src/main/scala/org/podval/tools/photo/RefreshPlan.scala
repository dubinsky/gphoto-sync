package org.podval.tools.photo

import java.io.File
import java.nio.file.{Files as NFiles, StandardCopyOption}
import java.time.LocalDate

object RefreshPlan:
  final case class Card(id: String, count: Int, title: String) derives CanEqual

  enum Mode derives CanEqual:
    case Incremental
    case Full
    case Album(titles: Seq[String])

  enum Action derives CanEqual:
    case Add(card: Card)
    case CountChange(stored: Catalog.Album, card: Card)
    case Retitle(stored: Catalog.Album, card: Card)
    case Keep(stored: Catalog.Album, card: Card)
    case Drop(stored: Catalog.Album)

  final case class Diff(
    cards: Seq[Card],
    stored: Seq[Catalog.Album],
    actions: Seq[Action],
    itemReads: Set[String],
    named: Set[String],
    mode: Mode
  ) derives CanEqual

  final case class Prepared(diff: Diff, albums: Seq[Catalog.Album], text: String) derives CanEqual

  // Raw albums-list rows are title, count, id, auth, leaves. read.mjs refuses any other width.
  private val Columns: Int = 5
  // Backstop only. Not a comparison with the stored album count. 250 is accepted.
  private val Floor: Int = 250
  private val Header: String = "# gphoto-catalog 2"
  private val LiveName: String = "catalog.txt"
  private val PartialName: String = "catalog.txt.partial"
  private val PreviousName: String = "catalog.txt.previous"
  private val Bom: Array[Byte] = Array(0xef.toByte, 0xbb.toByte, 0xbf.toByte)

  def diff(
    lines: Seq[String],
    stored: Seq[Catalog.Album],
    mode: Mode,
    allowShrink: Boolean
  ): Either[String, Diff] =
    guard(lines, useFloor(mode, allowShrink)).flatMap: cards =>
      index(stored).flatMap: storedById =>
        resolve(mode, cards, stored).map: named =>
          val actions: Seq[Action] = classify(cards, stored, storedById, mode, named)
          Diff(cards, stored, actions, itemReads(cards, storedById, actions, mode, named), named, mode)

  def splice(found: Diff, reads: Map[String, Catalog.Album] = Map.empty, source: String = ""): Prepared =
    val cardById: Map[String, Card] = found.cards.map(card => card.id -> card).toMap
    val storedIds: Set[String] = found.stored.map(_.id).toSet
    val blocks: Map[String, Seq[String]] = blocksOf(source)
    val survivors: Seq[(Catalog.Album, Seq[String])] = found.stored.flatMap: album =>
      val copyWhole: Boolean = found.mode match
        case Mode.Album(_) => !found.named.contains(album.id)
        case _ => false
      if copyWhole then Some(album -> copiedLines(album, blocks))
      else cardById.get(album.id).map: card =>
        val readItems: Boolean = found.itemReads.contains(album.id)
        val next: Catalog.Album = rewrite(album, card, readItems, reads)
        next -> keptLines(album, next, readItems, blocks)
    val appended: Seq[(Catalog.Album, Seq[String])] = found.cards.flatMap: card =>
      val wanted: Boolean = found.mode match
        case Mode.Album(_) => found.named.contains(card.id)
        case _ => true
      if !storedIds.contains(card.id) && wanted then
        val next: Catalog.Album = taken(card, reads.get(card.id))
        Some(next -> recordLines(next))
      else None
    val rows: Seq[(Catalog.Album, Seq[String])] = survivors ++ appended
    Prepared(found, rows.map(_._1), renderLines(rows.map(_._2)))

  def plan(
    lines: Seq[String],
    stored: Seq[Catalog.Album],
    mode: Mode,
    allowShrink: Boolean,
    reads: Map[String, Catalog.Album] = Map.empty,
    source: String = ""
  ): Either[String, Prepared] =
    diff(lines, stored, mode, allowShrink).map(found => splice(found, reads, source))

  def publish(
    directory: File,
    prepared: Prepared,
    move: (File, File) => Unit = moveReplacing
  ): Either[String, Unit] =
    val partial: File = File(directory, PartialName)
    val live: File = File(directory, LiveName)
    val previous: File = File(directory, PreviousName)
    Files.write(prepared.text, partial)
    check(partial, prepared).flatMap: _ =>
      swap(live, partial, previous, move)

  def run(
    directory: File,
    lines: Seq[String],
    stored: Seq[Catalog.Album],
    mode: Mode,
    allowShrink: Boolean,
    reads: Map[String, Catalog.Album] = Map.empty,
    source: String = ""
  ): Either[String, Prepared] =
    plan(lines, stored, mode, allowShrink, reads, source).flatMap: prepared =>
      publish(directory, prepared).map(_ => prepared)

  private[photo] def check(file: File, prepared: Prepared): Either[String, Unit] =
    val bytes: Array[Byte] = NFiles.readAllBytes(file.toPath)
    bytesOk(bytes).flatMap: _ =>
      readCatalog(file).flatMap: albums =>
        idsMatch(albums, prepared.diff)

  // A title edit is not a card sweep. idsMatch would reject a catalog that did not come from albums.tsv.
  private[photo] def checkRetitle(file: File): Either[String, Unit] =
    val bytes: Array[Byte] = NFiles.readAllBytes(file.toPath)
    bytesOk(bytes).flatMap(_ => readCatalog(file).map(_ => ()))

  def render(albums: Seq[Catalog.Album]): String =
    renderLines(albums.map(recordLines))

  private def useFloor(mode: Mode, allowShrink: Boolean): Boolean = mode match
    case Mode.Album(_) => false
    case _ => !allowShrink

  // A trailing newline is not a row. The list reader drops empty lines before the shape check.
  private def guard(lines: Seq[String], floor: Boolean): Either[String, Seq[Card]] =
    val fields: Seq[Seq[String]] = lines.filter(_.nonEmpty).map(_.split("\t", -1).toSeq)
    if fields.exists(_.length != Columns) then Left("bad row")
    else if fields.exists(row => row.drop(1).headOption.contains("")) then Left("empty count")
    else cardsOf(fields).flatMap: cards =>
      if floor && cards.length < Floor then Left("rows < 250")
      else Right(cards)

  private def cardsOf(fields: Seq[Seq[String]]): Either[String, Seq[Card]] =
    fields.foldLeft[Either[String, (Set[String], Seq[Card])]](Right((Set.empty, Seq.empty))):
      case (Left(message), _) => Left(message)
      case (Right((seen, acc)), Seq(title, count, id, _, _)) =>
        parseCount(count) match
          case None => Left("bad row")
          case Some(_) if id.isEmpty => Left("bad row")
          case Some(_) if seen.contains(id) => Left("repeated card id")
          case Some(n) => Right((seen + id, acc :+ Card(id, n, title)))
      case (Right(_), _) => Left("bad row")
    .map(_._2)

  private def parseCount(text: String): Option[Int] =
    if text.isEmpty || text.exists(ch => ch < '0' || ch > '9') then None
    else if text.length > 1 && text.startsWith("0") then None
    else
      try Some(Integer.parseInt(text))
      catch case _: NumberFormatException => None

  private def index(albums: Seq[Catalog.Album]): Either[String, Map[String, Catalog.Album]] =
    albums.foldLeft[Either[String, Map[String, Catalog.Album]]](Right(Map.empty)):
      case (Left(message), _) => Left(message)
      case (Right(acc), album) if acc.contains(album.id) => Left("repeated album id")
      case (Right(acc), album) => Right(acc.updated(album.id, album))

  private def resolve(mode: Mode, cards: Seq[Card], stored: Seq[Catalog.Album]): Either[String, Set[String]] =
    mode match
      case Mode.Album(titles) => namedIds(cards, stored, titles)
      case _ => Right(Set.empty)

  private def namedIds(cards: Seq[Card], stored: Seq[Catalog.Album], titles: Seq[String]): Either[String, Set[String]] =
    titles.foldLeft[Either[String, Set[String]]](Right(Set.empty)):
      case (Left(message), _) => Left(message)
      case (Right(acc), title) =>
        val onCards: Seq[Card] = cards.filter(_.title == title)
        val onStored: Seq[Catalog.Album] = stored.filter(_.title == title)
        if onCards.drop(1).nonEmpty || onStored.drop(1).nonEmpty then Left("ambiguous title")
        else if onCards.isEmpty then Left("named card is missing")
        else Right(acc + onCards.head.id)

  private def classify(
    cards: Seq[Card],
    stored: Seq[Catalog.Album],
    storedById: Map[String, Catalog.Album],
    mode: Mode,
    named: Set[String]
  ): Seq[Action] =
    val cardIds: Set[String] = cards.map(_.id).toSet
    val relevant: Seq[Card] = mode match
      case Mode.Album(_) => cards.filter(card => named.contains(card.id))
      case _ => cards
    val listed: Seq[Action] = relevant.map(card => action(card, storedById.get(card.id)))
    val drops: Seq[Action] = mode match
      case Mode.Album(_) => Seq.empty
      case _ => stored.filterNot(album => cardIds.contains(album.id)).map(Action.Drop.apply)
    listed ++ drops

  private def action(card: Card, stored: Option[Catalog.Album]): Action = stored match
    case None => Action.Add(card)
    case Some(album) if album.count != card.count => Action.CountChange(album, card)
    case Some(album) if album.title != card.title => Action.Retitle(album, card)
    case Some(album) => Action.Keep(album, card)

  private def itemReads(
    cards: Seq[Card],
    storedById: Map[String, Catalog.Album],
    actions: Seq[Action],
    mode: Mode,
    named: Set[String]
  ): Set[String] =
    def readable(id: String): Boolean = !storedById.get(id).exists(_.autoAdd)
    mode match
      case Mode.Full => cards.map(_.id).filter(readable).toSet
      case Mode.Album(_) => named.filter(readable)
      case Mode.Incremental =>
        actions.collect:
          case Action.Add(card) if readable(card.id) => card.id
          case Action.CountChange(album, card) if !album.autoAdd => card.id
        .toSet

  private def rewrite(
    stored: Catalog.Album,
    card: Card,
    readItems: Boolean,
    reads: Map[String, Catalog.Album]
  ): Catalog.Album =
    if stored.autoAdd then carried(stored, card, reads)
    else if readItems then taken(card, reads.get(card.id))
    else if card.title != stored.title then stored.copy(title = card.title)
    else stored

  private def carried(stored: Catalog.Album, card: Card, reads: Map[String, Catalog.Album]): Catalog.Album =
    val owner: Option[String] = reads.get(card.id).fold(stored.owner)(_.owner)
    stored.copy(
      count = card.count,
      title = card.title,
      owner = owner,
      autoAdd = true,
      error = None,
      items = Seq.empty,
      unsets = Seq.empty
    )

  private def taken(card: Card, read: Option[Catalog.Album]): Catalog.Album =
    Catalog.Album(
      id = card.id,
      count = card.count,
      title = card.title,
      owner = read.fold(Option.empty[String])(_.owner),
      autoAdd = false,
      error = read.flatMap(_.error),
      items = read.fold(Seq.empty[Catalog.Item])(_.items),
      unsets = read.fold(Seq.empty[Catalog.Unset])(_.unsets)
    )

  private def recordLines(album: Catalog.Album): Seq[String] =
    val owner: Seq[String] = album.owner.map(label => s"owner\t${album.id}\t${plain(label)}").toSeq
    val auto: Seq[String] = if album.autoAdd then Seq(s"auto-add\t${album.id}") else Seq.empty
    val error: Seq[String] = album.error.toSeq.map: err =>
      s"error\t${album.id}\t${err.got}\t${err.of}\t${err.nameFailures}"
    val unsets: Seq[String] = album.unsets.map: unset =>
      s"unset\t${album.id}\t${dateText(unset.date)}\t${unset.key}\t${person(unset.uploader)}"
    val items: Seq[String] = album.items.map: item =>
      s"item\t${album.id}\t${item.filename}\t${dateText(item.date)}\t${item.key}\t${person(item.uploader)}"
    Seq(s"album\t${album.id}\t${album.count}\t${album.title}") ++ owner ++ auto ++ error ++ unsets ++ items

  // Group the source file by the id column. Not a second catalog parse: order inside a block stays as written.
  private[photo] def blocksOf(source: String): Map[String, Seq[String]] =
    val raw: Seq[String] = source.split("\n", -1).toSeq
    val body: Seq[String] = raw.dropWhile(_ == Header)
    val lines: Seq[String] = if body.lastOption.contains("") then body.dropRight(1) else body
    lines.foldLeft(Map.empty[String, Seq[String]]): (acc, line) =>
      line.split("\t", -1).toSeq match
        case _ +: id +: _ if id.nonEmpty =>
          acc.updated(id, acc.getOrElse(id, Seq.empty) :+ line)
        case _ => acc

  private[photo] def copiedLines(album: Catalog.Album, blocks: Map[String, Seq[String]]): Seq[String] =
    blocks.getOrElse(album.id, recordLines(album))

  private[photo] def keptLines(
    stored: Catalog.Album,
    next: Catalog.Album,
    readItems: Boolean,
    blocks: Map[String, Seq[String]]
  ): Seq[String] =
    val block: Seq[String] = blocks.getOrElse(stored.id, Seq.empty)
    if readItems || block.isEmpty || next.copy(title = stored.title) != stored then recordLines(next)
    else if next.title != stored.title then retitle(block, next.title)
    else block

  private[photo] def retitle(lines: Seq[String], title: String): Seq[String] =
    lines.map: line =>
      val fields: Seq[String] = line.split("\t", -1).toSeq
      if fields.headOption.contains("album") && fields.length >= 4 then
        fields.take(3).mkString("\t") + "\t" + title
      else line

  private[photo] def renderLines(groups: Seq[Seq[String]]): String =
    Header + "\n" + groups.flatten.map(_ + "\n").mkString

  // A CR would fail the byte check. Tab and newline, including CR, become one space. No other trim.
  private def plain(text: String): String =
    text.map:
      case '\t' | '\n' | '\r' => ' '
      case ch => ch

  private def dateText(date: Option[LocalDate]): String = date.fold("-")(_.toString)

  private def person(text: Option[String]): String = text.fold("-")(plain)

  private def bytesOk(bytes: Array[Byte]): Either[String, Unit] =
    if bytes.length >= Bom.length && bytes.take(Bom.length).sameElements(Bom) then Left("bom")
    else if bytes.contains('\r'.toByte) then Left("carriage return")
    else if bytes.isEmpty || bytes.last != '\n'.toByte then Left("trailing partial line")
    else Right(())

  private def readCatalog(file: File): Either[String, Seq[Catalog.Album]] =
    try Right(Catalog.read(file))
    catch case e: IllegalArgumentException => Left(e.getMessage)

  private def idsMatch(albums: Seq[Catalog.Album], found: Diff): Either[String, Unit] =
    val got: Set[String] = albums.map(_.id).toSet
    val ok: Boolean = found.mode match
      case Mode.Album(_) => found.named.subsetOf(got)
      case _ => got == found.cards.map(_.id).toSet
    if ok then Right(()) else Left("album set")

  private[photo] def swap(live: File, partial: File, previous: File): Either[String, Unit] =
    swap(live, partial, previous, moveReplacing)

  // previous exists only between the two renames. A failed second rename puts the live file back.
  private[photo] def swap(live: File, partial: File, previous: File, move: (File, File) => Unit): Either[String, Unit] =
    val hadLive: Boolean = live.isFile
    try
      if hadLive then move(live, previous)
      try
        move(partial, live)
        forget(previous)
        Right(())
      catch case failed: Exception =>
        restore(live, previous, hadLive, move) match
          case Right(()) => Left(renameMessage(failed))
          case Left(during) => Left(s"${renameMessage(failed)}; $during")
    catch case failed: Exception =>
      Left(renameMessage(failed))

  private def restore(
    live: File,
    previous: File,
    hadLive: Boolean,
    move: (File, File) => Unit
  ): Either[String, Unit] =
    if hadLive && !live.isFile && previous.isFile then
      try
        move(previous, live)
        Right(())
      catch case failed: Exception =>
        Left(s"catalog.txt is missing: ${renameMessage(failed)}")
    else if hadLive && !live.isFile then
      Left("catalog.txt is missing")
    else Right(())

  private def forget(previous: File): Unit =
    try if previous.exists then NFiles.delete(previous.toPath)
    catch case _: Exception => ()

  private def renameMessage(failed: Exception): String =
    Option(failed.getMessage).filter(_.nonEmpty).getOrElse("rename failed")

  private val moveReplacing: (File, File) => Unit = (from, to) =>
    NFiles.move(
      from.toPath,
      to.toPath,
      StandardCopyOption.ATOMIC_MOVE,
      StandardCopyOption.REPLACE_EXISTING
    )
