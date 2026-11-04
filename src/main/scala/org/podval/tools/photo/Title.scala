package org.podval.tools.photo

import java.io.File
import java.time.{DateTimeException, LocalDate, YearMonth}

// A title is a date span plus an optional name. The file is not that string:
// a slash in the end date would be a directory.
object Title:
  enum Grain:
    case Day, Month

  final case class Span(start: LocalDate, end: LocalDate, grain: Grain):
    def contains(day: LocalDate): Boolean = !day.isBefore(start) && !day.isAfter(end)

    def startDir: String = grain match
      case Grain.Month => monthText(YearMonth.from(start))
      case Grain.Day => dayText(start)

  final case class Parsed(span: Option[Span], name: Option[String]):
    def title: String = span match
      case None => name.getOrElse(throw IllegalArgumentException("curated album has no name"))
      case Some(value) =>
        val date: String = renderSpan(value)
        name.fold(date)(text => s"$date/$text")

    def startDir: String = span.fold(title)(_.startDir)

  def parse(text: String): Option[Parsed] =
    takeDay(text, allowName = true)
      .filter((parsed, rest) => rest.isEmpty)
      .map(_._1)
      .orElse(takeMonth(text, allowName = true).filter((_, rest) => rest.isEmpty).map(_._1))
      .orElse(curated(text))

  // Include lines carry a date and no name. The remainder starts the stem.
  def takeDate(text: String): Option[(Span, String)] =
    takeDay(text, allowName = false).map((parsed, rest) => parsed.span.get -> rest)
      .orElse(takeMonth(text, allowName = false).map((parsed, rest) => parsed.span.get -> rest))

  def canonical(text: String): Option[String] = parse(text).map(_.title)

  // The directory is the start folder. Grain comes from that directory, not from the to- token.
  // A to- end that is not strictly later than the start is not a file name.
  def fromLocation(directory: String, stem: String): Option[Parsed] =
    val segments: Seq[String] = directory.split('/').filter(_.nonEmpty).toSeq
    segments match
      case Seq() =>
        curated(stem)
      case Seq(year, month) if isNumber(year, 4) && isMonthNumber(month) =>
        yearMonth(year.toInt, month.toInt).flatMap(start => monthFile(start, stem))
      case Seq(year, month, day) if isNumber(year, 4) && isMonthNumber(month) && isDayNumber(day) =>
        civil(year.toInt, month.toInt, day.toInt).flatMap(start => dayFile(start, stem))
      case _ =>
        None

  def file(originals: File, parsed: Parsed): File =
    val stem: String = fileStem(parsed)
    parsed.span match
      case None =>
        File(originals, s"$stem.toml")
      case Some(span) =>
        File(File(originals, span.startDir), s"$stem.toml")

  private def monthFile(start: YearMonth, stem: String): Option[Parsed] =
    if stem == "album" then Some(Parsed(Some(monthSpan(start, start)), None))
    else if stem.startsWith("to-") then monthTo(start, stem.drop(3))
    else Option.when(legalName(stem))(Parsed(Some(monthSpan(start, start)), Some(stem)))

  private def dayFile(start: LocalDate, stem: String): Option[Parsed] =
    if stem == "album" then Some(Parsed(Some(Span(start, start, Grain.Day)), None))
    else if stem.startsWith("to-") then dayTo(start, stem.drop(3))
    else Option.when(legalName(stem))(Parsed(Some(Span(start, start, Grain.Day)), Some(stem)))

  private def monthTo(start: YearMonth, rest: String): Option[Parsed] =
    splitToken(rest).flatMap: (token, name) =>
      endMonth(start, token).map(end => Parsed(Some(monthSpan(start, end)), name))

  private def dayTo(start: LocalDate, rest: String): Option[Parsed] =
    splitToken(rest).flatMap: (token, name) =>
      endDay(start, token).map(end => Parsed(Some(Span(start, end, Grain.Day)), name))

  private def splitToken(rest: String): Option[(String, Option[String])] =
    rest.indexOf(' ') match
      case -1 =>
        Option.when(rest.nonEmpty)(rest -> None)
      case space =>
        val token: String = rest.take(space)
        val name: String = rest.drop(space + 1)
        Option.when(token.nonEmpty && legalName(name))(token -> Some(name))

  // Longer token first. The token must be consumed. The end must be strictly later.
  private def endMonth(start: YearMonth, token: String): Option[YearMonth] =
    yearMonthToken(token).filter((end, left) => left.isEmpty && end.isAfter(start)).map(_._1)
      .orElse:
        takeDigits(token, 2).flatMap: (month, left) =>
          Option.when(left.isEmpty)(()).flatMap(_ => yearMonth(start.getYear, month).filter(_.isAfter(start)))

  private def endDay(start: LocalDate, token: String): Option[LocalDate] =
    fullDayToken(token).filter((end, left) => left.isEmpty && end.isAfter(start)).map(_._1)
      .orElse(monthDayToken(start.getYear, token).filter((end, left) => left.isEmpty && end.isAfter(start)).map(_._1))
      .orElse:
        takeDigits(token, 2).flatMap: (day, left) =>
          Option.when(left.isEmpty)(()).flatMap: _ =>
            civil(start.getYear, start.getMonthValue, day).filter(_.isAfter(start))

  private def yearMonthToken(text: String): Option[(YearMonth, String)] =
    for
      (year, restYear) <- takeDigits(text, 4)
      rest <- hyphen(restYear)
      (month, left) <- takeDigits(rest, 2)
      value <- yearMonth(year, month)
    yield value -> left

  private def fullDayToken(text: String): Option[(LocalDate, String)] =
    for
      (year, restYear) <- takeDigits(text, 4)
      restMonth <- hyphen(restYear)
      (month, restDay) <- takeDigits(restMonth, 2)
      rest <- hyphen(restDay)
      (day, left) <- takeDigits(rest, 2)
      date <- civil(year, month, day)
    yield date -> left

  private def monthDayToken(year: Int, text: String): Option[(LocalDate, String)] =
    for
      (month, restDay) <- takeDigits(text, 2)
      rest <- hyphen(restDay)
      (day, left) <- takeDigits(rest, 2)
      date <- civil(year, month, day)
    yield date -> left

  private def monthSpan(start: YearMonth, end: YearMonth): Span =
    Span(start.atDay(1), end.atEndOfMonth, Grain.Month)

  private def hyphen(text: String): Option[String] = Option.when(text.startsWith("-"))(text.drop(1))

  private def isMonthNumber(name: String): Boolean = DateTree.isMonth(name)

  private def isDayNumber(name: String): Boolean = DateTree.isDay(name)

  private def curated(text: String): Option[Parsed] =
    Option.when(legalName(text) && !isNumber(text, 4))(Parsed(None, Some(text)))

  private def takeDay(text: String, allowName: Boolean): Option[(Parsed, String)] =
    for
      (year, restYear) <- takeDigits(text, 4)
      restMonth <- slash(restYear)
      (month, restDaySlash) <- takeDigits(restMonth, 2)
      restDay <- slash(restDaySlash)
      (day, rest) <- takeDigits(restDay, 2)
      start <- civil(year, month, day)
      (end, left) <- dayEnd(start, rest)
      nameRest <- finish(left, allowName)
    yield Parsed(Some(Span(start, end, Grain.Day)), nameRest._1) -> nameRest._2

  private def takeMonth(text: String, allowName: Boolean): Option[(Parsed, String)] =
    for
      (year, restYear) <- takeDigits(text, 4)
      restMonth <- slash(restYear)
      (month, rest) <- takeDigits(restMonth, 2)
      start <- yearMonth(year, month)
      (end, left) <- monthEnd(start, rest)
      nameRest <- finish(left, allowName)
    yield
      val span: Span = Span(start.atDay(1), end.atEndOfMonth, Grain.Month)
      Parsed(Some(span), nameRest._1) -> nameRest._2

  // Longer end first. An equal civil day is accepted and later rendered as one day.
  private def dayEnd(start: LocalDate, rest: String): Option[(LocalDate, String)] =
    if rest.isEmpty || rest.startsWith("/") || rest.startsWith(" ") then Some(start -> rest)
    else if rest.startsWith("-") then
      val tail: String = rest.drop(1)
      fullDate(tail).filter((end, _) => !end.isBefore(start))
        .orElse(monthDay(start.getYear, tail).filter((end, _) => !end.isBefore(start)))
        .orElse(sameMonthDay(start, tail).filter((end, _) => !end.isBefore(start)))
    else None

  private def monthEnd(start: YearMonth, rest: String): Option[(YearMonth, String)] =
    if rest.isEmpty || rest.startsWith("/") || rest.startsWith(" ") then Some(start -> rest)
    else if rest.startsWith("-") then
      val tail: String = rest.drop(1)
      fullMonth(tail).filter((end, _) => end.isAfter(start))
        .orElse(sameYearMonth(start, tail).filter((end, _) => end.isAfter(start)))
    else None

  private def fullDate(text: String): Option[(LocalDate, String)] =
    for
      (year, restYear) <- takeDigits(text, 4)
      restMonth <- slash(restYear)
      (month, restDaySlash) <- takeDigits(restMonth, 2)
      restDay <- slash(restDaySlash)
      (day, rest) <- takeDigits(restDay, 2)
      date <- civil(year, month, day)
    yield date -> rest

  private def monthDay(year: Int, text: String): Option[(LocalDate, String)] =
    for
      (month, restDay) <- takeDigits(text, 2)
      rest <- slash(restDay)
      (day, left) <- takeDigits(rest, 2)
      date <- civil(year, month, day)
    yield date -> left

  private def sameMonthDay(start: LocalDate, text: String): Option[(LocalDate, String)] =
    takeDigits(text, 2).flatMap: (day, rest) =>
      civil(start.getYear, start.getMonthValue, day).map(_ -> rest)

  private def fullMonth(text: String): Option[(YearMonth, String)] =
    for
      (year, restMonth) <- takeDigits(text, 4)
      rest <- slash(restMonth)
      (month, left) <- takeDigits(rest, 2)
      value <- yearMonth(year, month)
    yield value -> left

  private def sameYearMonth(start: YearMonth, text: String): Option[(YearMonth, String)] =
    takeDigits(text, 2).flatMap: (month, rest) =>
      yearMonth(start.getYear, month).map(_ -> rest)

  // A name is optional only when the caller is parsing a title. Include stops at the date.
  private def finish(rest: String, allowName: Boolean): Option[(Option[String], String)] =
    if rest.isEmpty || rest.startsWith(" ") then Some(None -> rest)
    else if allowName && rest.startsWith("/") && legalName(rest.drop(1)) then Some(Some(rest.drop(1)) -> "")
    else None

  def legalName(name: String): Boolean =
    name.nonEmpty &&
      !isNumber(name, 2) &&
      !name.head.isDigit &&
      !name.startsWith("to-") &&
      name != "album" &&
      !name.contains('/') &&
      !name.exists(ch => ch == '\t' || ch == '\r' || ch == '\n')

  def renderSpan(span: Span): String = span.grain match
    case Grain.Month =>
      val start: String = monthText(YearMonth.from(span.start))
      val end: YearMonth = YearMonth.from(span.end)
      val startMonth: YearMonth = YearMonth.from(span.start)
      if end.equals(startMonth) then start
      else if end.getYear == startMonth.getYear then s"$start-${f"${end.getMonthValue}%02d"}"
      else s"$start-${monthText(end)}"
    case Grain.Day =>
      val start: String = dayText(span.start)
      if span.end.equals(span.start) then start
      else if span.end.getYear == span.start.getYear && span.end.getMonthValue == span.start.getMonthValue then
        s"$start-${f"${span.end.getDayOfMonth}%02d"}"
      else if span.end.getYear == span.start.getYear then
        s"$start-${f"${span.end.getMonthValue}%02d"}/${f"${span.end.getDayOfMonth}%02d"}"
      else s"$start-${dayText(span.end)}"

  def fileStem(parsed: Parsed): String = parsed.span match
    case None => parsed.name.get
    case Some(span) =>
      val end: String = endToken(span)
      val named: String = parsed.name.fold("")(name => s" $name")
      if end.isEmpty then parsed.name.getOrElse("album") else s"to-$end$named"

  // Empty when the span is exactly the start folder. A to- filename must be strictly later.
  def endToken(span: Span): String = span.grain match
    case Grain.Month =>
      val start: YearMonth = YearMonth.from(span.start)
      val end: YearMonth = YearMonth.from(span.end)
      if end.equals(start) then ""
      else if end.getYear == start.getYear then f"${end.getMonthValue}%02d"
      else s"${f"${end.getYear}%04d"}-${f"${end.getMonthValue}%02d"}"
    case Grain.Day =>
      if span.end.equals(span.start) then ""
      else if span.end.getYear == span.start.getYear && span.end.getMonthValue == span.start.getMonthValue then
        f"${span.end.getDayOfMonth}%02d"
      else if span.end.getYear == span.start.getYear then
        s"${f"${span.end.getMonthValue}%02d"}-${f"${span.end.getDayOfMonth}%02d"}"
      else dayText(span.end).replace('/', '-')

  def monthText(value: YearMonth): String = f"${value.getYear}%04d/${value.getMonthValue}%02d"

  def dayText(value: LocalDate): String =
    f"${value.getYear}%04d/${value.getMonthValue}%02d/${value.getDayOfMonth}%02d"

  private def slash(text: String): Option[String] = Option.when(text.startsWith("/"))(text.drop(1))

  private def takeDigits(text: String, n: Int): Option[(Int, String)] =
    val (head, rest) = text.splitAt(n)
    Option.when(head.length == n && head.forall(_.isDigit))(head.toInt -> rest)

  private def civil(year: Int, month: Int, day: Int): Option[LocalDate] =
    try Some(LocalDate.of(year, month, day))
    catch case _: DateTimeException => None

  private def yearMonth(year: Int, month: Int): Option[YearMonth] =
    try Some(YearMonth.of(year, month))
    catch case _: DateTimeException => None

  private def isNumber(name: String, length: Int): Boolean = DateTree.isNumber(name, length)
