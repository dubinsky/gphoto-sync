package org.podval.tools.photo

import scala.annotation.tailrec

// NDJSON for the serve front. The Json enum stays in Result.scala; this object only reads requests and writes responses.
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
      if left.startsWith("{") then parseTop(left.drop(1), Nil)
      else Left(Problem(None, malformed))

  @tailrec
  private def parseTop(rest: String, acc: List[(String, Node)]): Either[Problem, Request] =
    val left: String = skip(rest)
    if left.startsWith("}") && acc.isEmpty then finish(Nil, left.drop(1))
    else parseKey(left) match
      case Left(_) =>
        Left(Problem(idOf(acc), malformed))
      case Right((key, afterKey)) =>
        val afterColon: String = skip(afterKey)
        if !afterColon.startsWith(":") then Left(Problem(idOf(acc), malformed))
        else parseValue(afterColon.drop(1)) match
          case Left(_) =>
            Left(Problem(idOf(acc), malformed))
          case Right((value, afterValue)) =>
            val more: String = skip(afterValue)
            val next: List[(String, Node)] = (key, value) :: acc
            if more.startsWith("}") then finish(next.reverse, more.drop(1))
            else if more.startsWith(",") then parseTop(more.drop(1), next)
            else Left(Problem(idOf(next), malformed))

  private def finish(fields: List[(String, Node)], rest: String): Either[Problem, Request] =
    val id: Option[String] = idOf(fields)
    if skip(rest).nonEmpty then Left(Problem(id, malformed))
    else interpret(Node.Obj(fields), id)

  private def idOf(fields: List[(String, Node)]): Option[String] =
    fields.filter(_._1 == "id") match
      case (_, Node.Str(value)) :: Nil => Some(value)
      case _ => None

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
        case '\n' => acc.append("\\n")
        case '\r' => acc.append("\\r")
        case '\t' => acc.append("\\t")
        case other => acc.append(other)
    "\"" + body.toString + "\""

  private enum Node:
    case Null
    case Bool(value: Boolean)
    case Num
    case Str(value: String)
    case Arr(values: List[Node])
    case Obj(fields: List[(String, Node)])

  private def interpret(node: Node, id: Option[String]): Either[Problem, Request] = node match
    case Node.Obj(fields) =>
      val argvFields: List[(String, Node)] = fields.filter(_._1 == "argv")
      val idCount: Int = fields.count(_._1 == "id")
      val other: Boolean = fields.exists(field => field._1 != "id" && field._1 != "argv")
      val idShapeOk: Boolean = idCount == 0 || id.isDefined
      if other || !idShapeOk || argvFields.length != 1 then Left(Problem(id, malformed))
      else argvFields.head._2 match
        case Node.Arr(values) =>
          strings(values) match
            case Some(argv) => Right(Request(id, argv))
            case None => Left(Problem(id, malformed))
        case _ =>
          Left(Problem(id, malformed))
    case _ =>
      Left(Problem(None, malformed))

  private def strings(values: List[Node]): Option[List[String]] =
    @tailrec
    def loop(rest: List[Node], acc: List[String]): Option[List[String]] = rest match
      case Nil => Some(acc.reverse)
      case Node.Str(value) :: tail => loop(tail, value :: acc)
      case _ => None
    loop(values, Nil)

  private def skip(rest: String): String =
    rest.dropWhile(ch => ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')

  private def parseValue(rest: String): Either[String, (Node, String)] =
    val left: String = skip(rest)
    if left.isEmpty then Left(malformed)
    else if left.startsWith("{") then parseObject(left.drop(1))
    else if left.startsWith("[") then parseArray(left.drop(1))
    else if left.startsWith("\"") then parseString(left.drop(1), "").map((text, more) => (Node.Str(text), more))
    else if left.startsWith("true") then Right((Node.Bool(true), left.drop(4)))
    else if left.startsWith("false") then Right((Node.Bool(false), left.drop(5)))
    else if left.startsWith("null") then Right((Node.Null, left.drop(4)))
    else if left.startsWith("-") || left.head.isDigit then parseNumber(left)
    else Left(malformed)

  private def parseNumber(rest: String): Either[String, (Node, String)] =
    val body: String = if rest.startsWith("-") then rest.drop(1) else rest
    integer(body).flatMap: afterInt =>
      fraction(afterInt).flatMap: afterFrac =>
        exponent(afterFrac).map(afterExp => (Node.Num, afterExp))

  private def integer(rest: String): Either[String, String] =
    if rest.isEmpty || !rest.head.isDigit then Left(malformed)
    else if rest.head == '0' then
      if rest.drop(1).headOption.exists(_.isDigit) then Left(malformed)
      else Right(rest.drop(1))
    else digits(rest)

  private def fraction(rest: String): Either[String, String] =
    if !rest.startsWith(".") then Right(rest)
    else digits(rest.drop(1))

  private def exponent(rest: String): Either[String, String] =
    if !rest.startsWith("e") && !rest.startsWith("E") then Right(rest)
    else
      val afterMark: String = rest.drop(1)
      val afterSign: String =
        if afterMark.startsWith("+") || afterMark.startsWith("-") then afterMark.drop(1) else afterMark
      digits(afterSign)

  private def digits(rest: String): Either[String, String] =
    val (taken, more) = rest.span(_.isDigit)
    if taken.isEmpty then Left(malformed) else Right(more)

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

  private def parseObject(rest: String): Either[String, (Node, String)] =
    val left: String = skip(rest)
    if left.startsWith("}") then Right((Node.Obj(Nil), left.drop(1)))
    else parseFields(left, Nil)

  @tailrec
  private def parseFields(rest: String, acc: List[(String, Node)]): Either[String, (Node, String)] =
    parseKey(rest) match
      case Left(message) => Left(message)
      case Right((key, afterKey)) =>
        val afterColon: String = skip(afterKey)
        if !afterColon.startsWith(":") then Left(malformed)
        else parseValue(afterColon.drop(1)) match
          case Left(message) => Left(message)
          case Right((value, afterValue)) =>
            val more: String = skip(afterValue)
            val next: List[(String, Node)] = (key, value) :: acc
            if more.startsWith("}") then Right((Node.Obj(next.reverse), more.drop(1)))
            else if more.startsWith(",") then parseFields(more.drop(1), next)
            else Left(malformed)

  private def parseKey(rest: String): Either[String, (String, String)] =
    val left: String = skip(rest)
    if left.startsWith("\"") then parseString(left.drop(1), "")
    else Left(malformed)

  private def parseArray(rest: String): Either[String, (Node, String)] =
    val left: String = skip(rest)
    if left.startsWith("]") then Right((Node.Arr(Nil), left.drop(1)))
    else parseElements(left, Nil)

  @tailrec
  private def parseElements(rest: String, acc: List[Node]): Either[String, (Node, String)] =
    parseValue(rest) match
      case Left(message) => Left(message)
      case Right((value, after)) =>
        val more: String = skip(after)
        val next: List[Node] = value :: acc
        if more.startsWith("]") then Right((Node.Arr(next.reverse), more.drop(1)))
        else if more.startsWith(",") then parseElements(more.drop(1), next)
        else Left(malformed)
