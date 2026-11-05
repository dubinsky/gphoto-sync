package org.podval.tools.photo

import java.time.LocalDate

// One include entry. Logical text has no quotes. The file quotes a stem only when a bare token would be wrong.
object Include:
  enum StemSpec:
    case One(stem: String)
    case Range(from: String, to: String)

  enum Entry:
    case All(except: Seq[String])
    case Dates(span: Title.Span, stems: Option[StemSpec], except: Seq[String])

  def parse(text: String): Entry =
    parseEntry(text).getOrElse(throw IllegalArgumentException(s"not an include entry: $text"))

  def logical(entry: Entry): String = entry match
    case Entry.All(except) =>
      if except.isEmpty then "all" else s"all except ${except.mkString(" ")}"
    case Entry.Dates(span, stems, except) =>
      val spec: String = stems match
        case None                           => ""
        case Some(StemSpec.One(stem))       => s" $stem"
        case Some(StemSpec.Range(from, to)) => s" $from..$to"
      val tail: String = if except.isEmpty then "" else s" except ${except.mkString(" ")}"
      s"${Title.renderSpan(span)}$spec$tail"

  def render(entry: Entry): String = entry match
    case Entry.All(except) =>
      if except.isEmpty then "all" else s"all except ${except.map(quoteStem).mkString(" ")}"
    case Entry.Dates(span, stems, except) =>
      val spec: String = stems match
        case None                           => ""
        case Some(StemSpec.One(stem))       => s" ${quoteStem(stem)}"
        case Some(StemSpec.Range(from, to)) => s" ${quoteStem(from)}..${quoteStem(to)}"
      val tail: String = if except.isEmpty then "" else s" except ${except.map(quoteStem).mkString(" ")}"
      s"${Title.renderSpan(span)}$spec$tail"

  def needsQuote(stem: String): Boolean =
    stem.exists(ch => ch.isWhitespace || ch == '"' || ch == '\\') || stem.contains("..") || stem == "except"

  def quoteStem(stem: String): String =
    if needsQuote(stem) then "\"" + stem.replace("\\", "\\\\").replace("\"", "\\\"") + "\"" else stem

  def positive(albumSpan: Option[Title.Span], entry: Entry, day: LocalDate, stem: String): Boolean = entry match
    case Entry.All(_) =>
      albumSpan.exists(_.contains(day))
    case Entry.Dates(span, spec, _) =>
      span.contains(day) && spec.forall(matches(_, stem))

  def claims(albumSpan: Option[Title.Span], entry: Entry, day: LocalDate, stem: String): Boolean =
    positive(albumSpan, entry, day, stem) && !exceptOf(entry).contains(stem)

  def matches(spec: StemSpec, stem: String): Boolean = spec match
    case StemSpec.One(one)        => stem == one
    case StemSpec.Range(from, to) =>
      stem.length == from.length && stem.length == to.length && stem >= from && stem <= to

  def inside(albumSpan: Option[Title.Span], entry: Entry): Boolean = entry match
    case Entry.All(_) =>
      albumSpan.isDefined
    case Entry.Dates(span, _, _) =>
      albumSpan.forall(outer => !span.start.isBefore(outer.start) && !span.end.isAfter(outer.end))

  // Per-day logical length, and the `all except` length when that candidate exists.
  // A full `all` has no except length. Newlines are not part of either length.
  private[photo] def weights(
    albumSpan: Option[Title.Span],
    claimed: Set[(LocalDate, String)],
    universe: Map[LocalDate, Set[String]]
  ): (Int, Option[Int]) =
    val prepared: Prepared = prepare(albumSpan, claimed, universe)
    (lengthSum(prepared.perDay), prepared.allExcept.map(entry => logical(entry).length))

  // Claimed pairs against the scanned universe. `all` is legal only when albumSpan is defined.
  // Equal logical length keeps the per-day lines. Newlines are not part of either length.
  // allowAll false skips both all candidates even when the claimed set is the whole span.
  def compress(
    albumSpan: Option[Title.Span],
    claimed: Set[(LocalDate, String)],
    universe: Map[LocalDate, Set[String]],
    allowAll: Boolean = true
  ): Seq[Entry] =
    val prepared: Prepared = prepare(albumSpan, claimed, universe, allowAll)
    prepared.all
      .map(entry => Seq(entry))
      .orElse:
        prepared.allExcept
          .filter(entry => logical(entry).length < lengthSum(prepared.perDay))
          .map(entry => Seq(entry))
      .getOrElse(prepared.perDay)

  private final case class Prepared(perDay: Seq[Entry], all: Option[Entry], allExcept: Option[Entry])

  private def prepare(
    albumSpan: Option[Title.Span],
    claimed: Set[(LocalDate, String)],
    universe: Map[LocalDate, Set[String]],
    allowAll: Boolean = true
  ): Prepared =
    val widened: Map[LocalDate, Set[String]] = withClaimed(universe, claimed)
    val perDay: Seq[Entry] = perDayLines(claimed, widened)
    if !allowAll then Prepared(perDay, None, None)
    else
      albumSpan match
        case None =>
          Prepared(perDay, None, None)
        case Some(span) =>
          val pictures: Set[(LocalDate, String)] = pairsIn(span, widened)
          if claimed.equals(pictures) then Prepared(perDay, Some(Entry.All(Seq.empty)), None)
          else
            val excepted: Option[Entry] = uniformExcept(pictures, pictures -- claimed).map(stems => Entry.All(stems))
            Prepared(perDay, None, excepted)

  def expand(
    albumSpan: Option[Title.Span],
    entry: Entry,
    universe: Map[LocalDate, Set[String]]
  ): Set[(LocalDate, String)] =
    val span: Option[Title.Span] = entry match
      case Entry.All(_)             => albumSpan
      case Entry.Dates(value, _, _) => Some(value)
    val banned: Set[String] = exceptOf(entry).toSet
    span.fold(Set.empty): dates =>
      pairsIn(dates, universe).filter: (day, stem) =>
        claims(albumSpan, entry, day, stem) && !banned.contains(stem)

  def drop(
    include: Seq[Entry],
    albumSpan: Option[Title.Span],
    pair: (LocalDate, String),
    universe: Map[LocalDate, Set[String]]
  ): Seq[Entry] =
    include.flatMap: entry =>
      if !claims(albumSpan, entry, pair._1, pair._2) then Seq(entry)
      else
        val remaining: Set[(LocalDate, String)] = expand(albumSpan, entry, universe) - pair
        if remaining.isEmpty then Seq.empty
        else compress(albumSpan, remaining, universe)

  // Curated moves do not invent `all`. Dated albums are not rewritten: their date follows the folder.
  def relocate(
    include: Seq[Entry],
    albumSpan: Option[Title.Span],
    from: (LocalDate, String),
    to: (LocalDate, String),
    universe: Map[LocalDate, Set[String]]
  ): Seq[Entry] =
    if !include.exists(entry => claims(albumSpan, entry, from._1, from._2)) then include
    else
      val dropped: Seq[Entry] = drop(include, albumSpan, from, universe).sortBy(logical)
      if dropped.exists(entry => claims(albumSpan, entry, to._1, to._2)) then dropped
      else add(dropped, albumSpan, to, universe)

  def lengthSum(entries: Seq[Entry]): Int = entries.map(entry => logical(entry).length).sum

  // Album-level all is only an empty include or a lone Entry.All on add, and only a lone Entry.All on remove.
  // A lone Entry.Dates uses compress(None). An empty after does not call compress: empty equals empty would be all.
  def toggle(
    include: Seq[Entry],
    albumSpan: Option[Title.Span],
    pair: (LocalDate, String),
    universe: Map[LocalDate, Set[String]]
  ): Seq[Entry] =
    val (alls, others) = include.partition:
      case Entry.All(_)         => true
      case Entry.Dates(_, _, _) => false
    if alls.nonEmpty && others.nonEmpty then throw IllegalArgumentException("include mixes all with other lines")
    val widened: Map[LocalDate, Set[String]] =
      universe.updated(pair._1, universe.getOrElse(pair._1, Set.empty) + pair._2)
    val on: Boolean = include.exists(entry => claims(albumSpan, entry, pair._1, pair._2))
    val before: Set[(LocalDate, String)] =
      include.flatMap(entry => expand(albumSpan, entry, widened)).toSet
    if on then removeOne(include, albumSpan, pair, widened, before - pair)
    else addOne(include, albumSpan, pair, widened, before + pair)

  private def removeOne(
    include: Seq[Entry],
    albumSpan: Option[Title.Span],
    pair: (LocalDate, String),
    universe: Map[LocalDate, Set[String]],
    after: Set[(LocalDate, String)]
  ): Seq[Entry] =
    if after.isEmpty then Seq.empty
    else
      include match
        case Seq(Entry.All(_)) =>
          compress(albumSpan, after, universe)
        case _ =>
          spliceTouched(include, entry => claims(albumSpan, entry, pair._1, pair._2)): hit =>
            val pairs: Set[(LocalDate, String)] =
              hit.flatMap(entry => expand(albumSpan, entry, universe)).toSet - pair
            if pairs.isEmpty then Seq.empty else compress(None, pairs, universe)

  private def addOne(
    include: Seq[Entry],
    albumSpan: Option[Title.Span],
    pair: (LocalDate, String),
    universe: Map[LocalDate, Set[String]],
    after: Set[(LocalDate, String)]
  ): Seq[Entry] =
    include match
      case Seq() =>
        compress(albumSpan, after, universe)
      case Seq(Entry.All(_)) =>
        compress(albumSpan, after, universe)
      case _ =>
        spliceTouched(include, entry => expand(albumSpan, entry, universe).exists(item => item._1.equals(pair._1))):
          hit =>
            // compress(None) so a subset cannot become album-level all.
            // expand of the whole entry keeps a multi-day line's other days.
            val pairs: Set[(LocalDate, String)] =
              hit.flatMap(entry => expand(albumSpan, entry, universe)).toSet + pair
            compress(None, pairs, universe)

  // Untouched entries stay in order. The touched group is recompressed at the first touched index.
  private def spliceTouched(
    include: Seq[Entry],
    touched: Entry => Boolean
  )(replaced: Seq[Entry] => Seq[Entry]): Seq[Entry] =
    val (prefix, tail) = include.span(entry => !touched(entry))
    val (hit, kept) = tail.partition(touched)
    prefix ++ replaced(hit) ++ kept

  private[photo] def add(
    include: Seq[Entry],
    albumSpan: Option[Title.Span],
    pair: (LocalDate, String),
    universe: Map[LocalDate, Set[String]]
  ): Seq[Entry] =
    val widened: Map[LocalDate, Set[String]] =
      universe.updated(pair._1, universe.getOrElse(pair._1, Set.empty) + pair._2)
    val (hit, rest) = include.partition: entry =>
      expand(albumSpan, entry, widened).exists(item => item._1.equals(pair._1))
    val dayClaims: Set[(LocalDate, String)] =
      hit.flatMap(entry => expand(albumSpan, entry, widened)).filter(item => item._1.equals(pair._1)).toSet + pair
    (rest ++ compress(albumSpan, dayClaims, widened)).sortBy(logical)

  private def exceptOf(entry: Entry): Seq[String] = entry match
    case Entry.All(except)         => except
    case Entry.Dates(_, _, except) => except

  private def withClaimed(
    universe: Map[LocalDate, Set[String]],
    claimed: Set[(LocalDate, String)]
  ): Map[LocalDate, Set[String]] =
    claimed.foldLeft(universe): (acc, pair) =>
      acc.updated(pair._1, acc.getOrElse(pair._1, Set.empty) + pair._2)

  private[photo] def pairsIn(span: Title.Span, universe: Map[LocalDate, Set[String]]): Set[(LocalDate, String)] =
    universe.foldLeft(Set.empty[(LocalDate, String)]): (acc, pair) =>
      if span.contains(pair._1) then acc ++ pair._2.map(pair._1 -> _) else acc

  // Each stem in the set is missing on every day it occurs, and every miss is one of those stems.
  private def uniformExcept(
    pictures: Set[(LocalDate, String)],
    missing: Set[(LocalDate, String)]
  ): Option[Seq[String]] =
    if missing.isEmpty then None
    else
      val stems: Set[String] = missing.map(_._2)
      val uniform: Boolean = stems.forall: stem =>
        pictures.filter(_._2 == stem).forall(missing.contains)
      Option.when(uniform)(stems.toSeq.sorted)

  private def perDayLines(
    claimed: Set[(LocalDate, String)],
    universe: Map[LocalDate, Set[String]]
  ): Seq[Entry] =
    claimed
      .map(_._1)
      .toSeq
      .sorted
      .flatMap: day =>
        val claimedStems: Set[String] = claimed.filter(item => item._1.equals(day)).map(_._2)
        val stems: Set[String] = universe.getOrElse(day, Set.empty)
        byWidth(day, claimedStems, stems)
      .sortBy(logical)

  private def byWidth(day: LocalDate, claimed: Set[String], universe: Set[String]): Seq[Entry] =
    universe
      .groupBy(_.length)
      .toSeq
      .sortBy(_._1)
      .flatMap: (_, stems) =>
        val sorted: Seq[String] = stems.toSeq.sorted
        val runLines: Seq[Entry] = runs(sorted, claimed).map(run => runLine(day, run))
        exceptAlternative(day, sorted, claimed)
          .filter(line => logical(line).length < lengthSum(runLines))
          .map(Seq(_))
          .getOrElse(runLines)

  private def runs(sorted: Seq[String], claimed: Set[String]): Seq[Seq[String]] =
    def loop(rest: Seq[String], acc: Seq[Seq[String]]): Seq[Seq[String]] =
      val skipped: Seq[String] = rest.dropWhile(stem => !claimed.contains(stem))
      if skipped.isEmpty then acc
      else
        val (run, tail) = skipped.span(claimed.contains)
        loop(tail, acc :+ run)
    loop(sorted, Seq.empty)

  private def runLine(day: LocalDate, run: Seq[String]): Entry =
    val spec: StemSpec = if run.sizeIs == 1 then StemSpec.One(run.head) else StemSpec.Range(run.head, run.last)
    Entry.Dates(Title.Span(day, day, Title.Grain.Day), Some(spec), Seq.empty)

  private def exceptAlternative(day: LocalDate, sorted: Seq[String], claimed: Set[String]): Option[Entry] =
    val kept: Seq[String] = sorted.filter(claimed.contains)
    if kept.sizeIs <= 1 then None
    else
      val first: String = kept.head
      val last: String = kept.last
      val between: Seq[String] = sorted.dropWhile(_ != first).takeWhile(_ != last).drop(1)
      val holes: Seq[String] = between.filterNot(claimed.contains)
      Option.when(holes.nonEmpty):
        Entry.Dates(
          Title.Span(day, day, Title.Grain.Day),
          Some(StemSpec.Range(first, last)),
          holes.sorted
        )

  private def parseEntry(text: String): Option[Entry] =
    if text == "all" then Some(Entry.All(Seq.empty))
    else if text.startsWith("all except ") then readExcepts(text.drop("all except ".length)).map(Entry.All.apply)
    else if text.startsWith("all") then None
    else
      Title
        .takeDate(text)
        .flatMap: (span, rest) =>
          readBody(rest).flatMap: (stems, except) =>
            Option.when(except.forall(stem => exceptOk(stems, stem)))(Entry.Dates(span, stems, except))

  private def readBody(rest: String): Option[(Option[StemSpec], Seq[String])] =
    if rest.isEmpty then Some(None -> Seq.empty)
    else if rest.startsWith(" except ") then readExcepts(rest.drop(" except ".length)).map(None -> _)
    else if rest.startsWith(" ") then
      readStem(rest.drop(1)).flatMap: (from, after) =>
        val spec: Option[(StemSpec, String)] =
          if after.startsWith("..") then
            readStem(after.drop(2)).flatMap: (to, left) =>
              Option.when(from.length == to.length && from <= to)(StemSpec.Range(from, to) -> left)
          else Some(StemSpec.One(from) -> after)
        spec.flatMap: (stems, left) =>
          if left.isEmpty then Some(Some(stems) -> Seq.empty)
          else if left.startsWith(" except ") then
            readExcepts(left.drop(" except ".length)).map(except => Some(stems) -> except)
          else None
    else None

  private def exceptOk(stems: Option[StemSpec], stem: String): Boolean = stems match
    case None                        => true
    case Some(StemSpec.One(one))     => stem == one
    case Some(range: StemSpec.Range) => matches(range, stem)

  private def readExcepts(text: String): Option[Seq[String]] =
    def loop(rest: String, acc: Seq[String]): Option[Seq[String]] =
      readStem(rest).flatMap: (stem, left) =>
        if left.isEmpty then Some(acc :+ stem)
        else if left.startsWith(" ") && !left.startsWith("  ") then loop(left.drop(1), acc :+ stem)
        else None
    Option.when(text.nonEmpty && !text.startsWith(" "))(()).flatMap(_ => loop(text, Seq.empty))

  private def readStem(text: String): Option[(String, String)] =
    if text.startsWith("\"") then quotedStem(text) else bareStem(text)

  private def bareStem(text: String): Option[(String, String)] =
    val (token, rest) = text.span(ch => !ch.isWhitespace && ch != '"' && ch != '\\')
    val (stem, marker) = cutBeforeDots(token)
    Option.when(stem.nonEmpty && stem != "except")(stem -> (marker + rest))

  private def cutBeforeDots(token: String): (String, String) =
    token.indexOf("..") match
      case -1    => (token, "")
      case index => (token.take(index), token.drop(index))

  private def quotedStem(text: String): Option[(String, String)] =
    Option.when(text.startsWith("\""))(()).flatMap(_ => decodeQuoted(text.drop(1), ""))

  private def decodeQuoted(rest: String, body: String): Option[(String, String)] =
    val (plain, tail) = rest.span(ch => ch != '"' && ch != '\\' && ch != '\n' && ch != '\r')
    tail.take(2) match
      case "\\\""                                                => decodeQuoted(tail.drop(2), body + plain + "\"")
      case "\\\\"                                                => decodeQuoted(tail.drop(2), body + plain + "\\")
      case _ if tail.startsWith("\"") && (body + plain).nonEmpty =>
        Some((body + plain, tail.drop(1)))
      case _ =>
        None
