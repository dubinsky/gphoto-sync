package org.podval.tools.photo

import scala.annotation.tailrec

private[photo] object Serve:
  private[photo] final case class Request(id: Option[String], argv: List[String])
  private[photo] final case class Problem(id: Option[String], message: String)

  private val malformed: String = "malformed request"
  private val emptyRequest: String = "empty request"

  // A broken value keeps a string id already read from this object. A syntax error before that id does not.
  private[photo] def read(line: String): Either[Problem, Request] =
    if line.forall(_.isWhitespace) then Left(Problem(None, emptyRequest))
    else
      val left: String = skip(line)
      if !left.startsWith("{") then Left(Problem(None, malformed))
      else fields(left.drop(1), Seen(), take = true, allowEmpty = true) match
        case Walk.Failed(seen) =>
          Left(Problem(echoed(seen), malformed))
        case Walk.Done(seen, rest) =>
          if skip(rest).nonEmpty then Left(Problem(echoed(seen), malformed))
          else request(seen)

  private[photo] def render(
    id: Option[String],
    ok: Boolean,
    text: Option[String],
    error: Option[String],
    value: Option[Json]
  ): String =
    val fields: Seq[(String, Json)] =
      id.toSeq.map(text => "id" -> Json.Str(text)) ++
        Seq("ok" -> Json.Bool(ok)) ++
        text.toSeq.map(text => "text" -> Json.Str(text)) ++
        error.toSeq.map(text => "error" -> Json.Str(text)) ++
        value.toSeq.map(json => "value" -> json)
    write(Json.Obj(fields))

  private[photo] def write(json: Json): String = json match
    case Json.Null => "null"
    case Json.Bool(value) => value.toString
    case Json.Long(value) => value.toString
    case Json.Str(value) => quote(value)
    case Json.Arr(values) => values.map(write).mkString("[", ",", "]")
    case Json.Obj(fields) =>
      fields.map((key, item) => quote(key) + ":" + write(item)).mkString("{", ",", "}")

  private def quote(value: String): String =
    val body: StringBuilder = value.foldLeft(new StringBuilder): (acc, ch) =>
      ch match
        case '\\' => acc.append("\\\\")
        case '"' => acc.append("\\\"")
        case '\b' => acc.append("\\b")
        case '\f' => acc.append("\\f")
        case '\n' => acc.append("\\n")
        case '\r' => acc.append("\\r")
        case '\t' => acc.append("\\t")
        case other if other < ' ' =>
          acc.append("\\u00")
          acc.append(hexDigit(other.toInt / 16))
          acc.append(hexDigit(other.toInt % 16))
        case other =>
          acc.append(other)
    "\"" + body.toString + "\""

  private def hexDigit(n: Int): Char =
    if n < 10 then ('0' + n).toChar else ('a' + (n - 10)).toChar

  // Only a string id and a string argv are kept. Every other JSON value is skipped or rejected.
  private final case class Seen(
    id: Option[String] = None,
    ids: Int = 0,
    argv: Option[List[String]] = None,
    argvs: Int = 0,
    other: Boolean = false
  )

  private enum Walk:
    case Done(seen: Seen, rest: String)
    case Failed(seen: Seen)

  private enum ArrayRead:
    case Strings(values: List[String], rest: String)
    case Mixed(rest: String)
    case Bad

  private def echoed(seen: Seen): Option[String] =
    if seen.ids == 1 then seen.id else None

  private def request(seen: Seen): Either[Problem, Request] =
    val id: Option[String] = echoed(seen)
    val idOk: Boolean = seen.ids == 0 || id.isDefined
    if seen.other || !idOk || seen.argvs != 1 then Left(Problem(id, malformed))
    else seen.argv match
      case Some(argv) => Right(Request(id, argv))
      case None => Left(Problem(id, malformed))

  // allowEmpty is true only before the first field, so a trailing comma is not an empty object.
  @tailrec
  private def fields(rest: String, seen: Seen, take: Boolean, allowEmpty: Boolean): Walk =
    val left: String = skip(rest)
    if allowEmpty && left.startsWith("}") then Walk.Done(seen, left.drop(1))
    else parseKey(left) match
      case None =>
        Walk.Failed(seen)
      case Some((key, afterKey)) =>
        val afterColon: String = skip(afterKey)
        if !afterColon.startsWith(":") then Walk.Failed(seen)
        else accept(key, afterColon.drop(1), seen, take) match
          case None =>
            Walk.Failed(seen)
          case Some((next, afterValue)) =>
            val more: String = skip(afterValue)
            if more.startsWith("}") then Walk.Done(next, more.drop(1))
            else if more.startsWith(",") then fields(more.drop(1), next, take, allowEmpty = false)
            else Walk.Failed(next)

  private def accept(key: String, rest: String, seen: Seen, take: Boolean): Option[(Seen, String)] =
    if !take then skipValue(rest).map(more => (seen, more))
    else key match
      case "id" => takeId(rest, seen)
      case "argv" => takeArgv(rest, seen)
      case _ => skipValue(rest).map(more => (seen.copy(other = true), more))

  private def takeId(rest: String, seen: Seen): Option[(Seen, String)] =
    val left: String = skip(rest)
    if left.startsWith("\"") then
      parseString(left.drop(1), "") match
        case Left(_) => None
        case Right((text, more)) => Some((seen.copy(id = Some(text), ids = seen.ids + 1), more))
    else skipValue(left).map(more => (seen.copy(ids = seen.ids + 1), more))

  private def takeArgv(rest: String, seen: Seen): Option[(Seen, String)] =
    val left: String = skip(rest)
    if !left.startsWith("[") then skipValue(left).map(more => (seen.copy(argvs = seen.argvs + 1), more))
    else elements(left.drop(1), Nil, mixed = false, allowEmpty = true) match
      case ArrayRead.Bad => None
      case ArrayRead.Strings(values, more) =>
        Some((seen.copy(argv = Some(values), argvs = seen.argvs + 1), more))
      case ArrayRead.Mixed(more) =>
        Some((seen.copy(argvs = seen.argvs + 1), more))

  private def skipValue(rest: String): Option[String] =
    val left: String = skip(rest)
    if left.isEmpty then None
    else if left.startsWith("{") then
      fields(left.drop(1), Seen(), take = false, allowEmpty = true) match
        case Walk.Done(_, more) => Some(more)
        case Walk.Failed(_) => None
    else if left.startsWith("[") then
      elements(left.drop(1), Nil, mixed = false, allowEmpty = true) match
        case ArrayRead.Bad => None
        case ArrayRead.Strings(_, more) => Some(more)
        case ArrayRead.Mixed(more) => Some(more)
    else if left.startsWith("\"") then
      parseString(left.drop(1), "") match
        case Right((_, more)) => Some(more)
        case Left(_) => None
    else if left.startsWith("true") then Some(left.drop(4))
    else if left.startsWith("false") then Some(left.drop(5))
    else if left.startsWith("null") then Some(left.drop(4))
    else if left.startsWith("-") || left.head.isDigit then parseNumber(left)
    else None

  @tailrec
  private def elements(
    rest: String,
    acc: List[String],
    mixed: Boolean,
    allowEmpty: Boolean
  ): ArrayRead =
    val left: String = skip(rest)
    if allowEmpty && left.startsWith("]") then
      if mixed then ArrayRead.Mixed(left.drop(1)) else ArrayRead.Strings(acc.reverse, left.drop(1))
    else element(left) match
      case None =>
        ArrayRead.Bad
      case Some((text, after)) =>
        val nextMixed: Boolean = mixed || text.isEmpty
        val next: List[String] = text match
          case Some(value) if !nextMixed => value :: acc
          case _ => Nil
        val more: String = skip(after)
        if more.startsWith("]") then
          if nextMixed then ArrayRead.Mixed(more.drop(1)) else ArrayRead.Strings(next.reverse, more.drop(1))
        else if more.startsWith(",") then elements(more.drop(1), next, nextMixed, allowEmpty = false)
        else ArrayRead.Bad

  private def element(rest: String): Option[(Option[String], String)] =
    val left: String = skip(rest)
    if left.startsWith("\"") then
      parseString(left.drop(1), "") match
        case Right((text, more)) => Some((Some(text), more))
        case Left(_) => None
    else skipValue(left).map(more => (None, more))

  private def parseKey(rest: String): Option[(String, String)] =
    val left: String = skip(rest)
    if !left.startsWith("\"") then None
    else parseString(left.drop(1), "") match
      case Right((text, more)) => Some((text, more))
      case Left(_) => None

  private def parseNumber(rest: String): Option[String] =
    val body: String = if rest.startsWith("-") then rest.drop(1) else rest
    integer(body).flatMap(fraction).flatMap(exponent)

  private def integer(rest: String): Option[String] =
    if rest.isEmpty || !rest.head.isDigit then None
    else if rest.head == '0' then
      if rest.drop(1).headOption.exists(_.isDigit) then None
      else Some(rest.drop(1))
    else digits(rest)

  private def fraction(rest: String): Option[String] =
    if !rest.startsWith(".") then Some(rest)
    else digits(rest.drop(1))

  private def exponent(rest: String): Option[String] =
    if !rest.startsWith("e") && !rest.startsWith("E") then Some(rest)
    else
      val afterMark: String = rest.drop(1)
      val afterSign: String =
        if afterMark.startsWith("+") || afterMark.startsWith("-") then afterMark.drop(1) else afterMark
      digits(afterSign)

  private def digits(rest: String): Option[String] =
    val (taken, more) = rest.span(_.isDigit)
    if taken.isEmpty then None else Some(more)

  @tailrec
  private def parseString(rest: String, acc: String): Either[String, (String, String)] =
    if rest.isEmpty then Left(malformed)
    else
      val (bare, more) = rest.span(ch => ch != '"' && ch != '\\')
      if bare.exists(_ < ' ') then Left(malformed)
      else if more.isEmpty then Left(malformed)
      else if more.head == '"' then Right((acc + bare, more.drop(1)))
      else
        val escaped: String = more.drop(1)
        if escaped.isEmpty then Left(malformed)
        else escape(escaped.head) match
          case None => Left(malformed)
          case Some(decoded) => parseString(escaped.drop(1), acc + bare + decoded)

  private def escape(ch: Char): Option[String] = ch match
    case '\\' => Some("\\")
    case '"' => Some("\"")
    case '/' => Some("/")
    case 'n' => Some("\n")
    case 't' => Some("\t")
    case 'r' => Some("\r")
    case 'b' => Some("\b")
    case _ => None

  private def skip(rest: String): String =
    rest.dropWhile(ch => ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')
