package org.podval.tools.photo

import scala.annotation.tailrec
import java.io.{ByteArrayOutputStream, File, IOException}
import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{
  ClosedChannelException,
  FileChannel,
  FileLock,
  OverlappingFileLockException,
  Selector,
  ServerSocketChannel,
  SocketChannel
}
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption
import java.nio.file.attribute.PosixFilePermissions

private[photo] object Serve:
  private[photo] final case class Request(id: Option[String], argv: List[String])
  private[photo] final case class Problem(id: Option[String], message: String)
  private[photo] final case class Response(
    id: Option[String],
    ok: Boolean,
    text: Option[String],
    error: Option[String],
    archive: String
  )

  private[photo] final case class Called(response: Response, value: Json)

  private[photo] enum Attempt:
    case Absent
    case Failed(message: String)
    case Done(response: Response)

  private enum Reply:
    case Absent
    case Failed(message: String)
    case Done(called: Called)

  private val acceptWaitMs: Long = 5000
  private val socketName: String = "serve.sock"
  private val lockName: String = "serve.lock"
  private val directoryMode: String = "rwx------"
  private val fileMode: String = "rw-------"

  private val malformed: String = "malformed request"
  private val emptyRequest: String = "empty request"

  // A broken value keeps a string id already read from this object. A syntax error before that id does not.
  private[photo] def read(line: String): Either[Problem, Request] =
    if line.forall(_.isWhitespace) then Left(Problem(None, emptyRequest))
    else
      val left: String = skip(line)
      if !left.startsWith("{") then Left(Problem(None, malformed))
      else fields(left.drop(1), Seen(), Take.Request, allowEmpty = true) match
        case Walk.Failed(seen) =>
          Left(Problem(echoed(seen), malformed))
        case Walk.Done(seen, rest) =>
          if skip(rest).nonEmpty then Left(Problem(echoed(seen), malformed))
          else request(seen)

  private[photo] def readResponse(line: String): Either[Problem, Response] =
    if line.forall(_.isWhitespace) then Left(Problem(None, malformed))
    else
      val left: String = skip(line)
      if !left.startsWith("{") then Left(Problem(None, malformed))
      else fields(left.drop(1), Seen(), Take.Response, allowEmpty = true) match
        case Walk.Failed(seen) =>
          Left(Problem(echoed(seen), malformed))
        case Walk.Done(seen, rest) =>
          if skip(rest).nonEmpty then Left(Problem(echoed(seen), malformed))
          else responseOf(seen)

  // Same envelope as readResponse. value is readJson, so nested keys are not envelope keys.
  private[photo] def readValued(line: String): Either[Problem, Called] =
    if line.forall(_.isWhitespace) then Left(Problem(None, malformed))
    else
      val left: String = skip(line)
      if !left.startsWith("{") then Left(Problem(None, malformed))
      else fields(left.drop(1), Seen(), Take.Response, allowEmpty = true, retain = true) match
        case Walk.Failed(seen) =>
          Left(Problem(echoed(seen), malformed))
        case Walk.Done(seen, rest) =>
          if skip(rest).nonEmpty then Left(Problem(echoed(seen), malformed))
          else responseOf(seen).map(response => Called(response, seen.json.getOrElse(Json.Null)))

  private[photo] def render(
    id: Option[String],
    ok: Boolean,
    text: Option[String],
    error: Option[String],
    value: Option[Json],
    archive: String
  ): String =
    val fields: Seq[(String, Json)] =
      id.toSeq.map(text => "id" -> Json.Str(text)) ++
        Seq("ok" -> Json.Bool(ok), "archive" -> Json.Str(archive)) ++
        text.toSeq.map(text => "text" -> Json.Str(text)) ++
        error.toSeq.map(text => "error" -> Json.Str(text)) ++
        value.toSeq.map(json => "value" -> json)
    write(Json.Obj(fields))

  private[photo] def renderRequest(id: Option[String], argv: List[String]): String =
    val fields: Seq[(String, Json)] =
      Seq("argv" -> Json.Arr(argv.map(text => Json.Str(text)))) ++
        id.toSeq.map(text => "id" -> Json.Str(text))
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
    ok: Option[Boolean] = None,
    oks: Int = 0,
    archive: Option[String] = None,
    archives: Int = 0,
    text: Option[String] = None,
    texts: Int = 0,
    error: Option[String] = None,
    errors: Int = 0,
    values: Int = 0,
    json: Option[Json] = None,
    other: Boolean = false
  )

  private enum Take:
    case Request, Response, Skip

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

  private def responseOf(seen: Seen): Either[Problem, Response] =
    val id: Option[String] = echoed(seen)
    val idOk: Boolean = seen.ids == 0 || id.isDefined
    val textOk: Boolean = seen.texts == 0 || seen.text.isDefined
    val errorOk: Boolean = seen.errors == 0 || seen.error.isDefined
    if seen.other || !idOk || !textOk || !errorOk || seen.oks != 1 || seen.archives != 1 || seen.values > 1 then
      Left(Problem(id, malformed))
    else (seen.ok, seen.archive) match
      case (Some(ok), Some(archive)) =>
        Right(Response(id, ok, seen.text, seen.error, archive))
      case _ =>
        Left(Problem(id, malformed))

  // allowEmpty is true only before the first field, so a trailing comma is not an empty object.
  @tailrec
  private def fields(
    rest: String,
    seen: Seen,
    take: Take,
    allowEmpty: Boolean,
    retain: Boolean = false
  ): Walk =
    val left: String = skip(rest)
    if allowEmpty && left.startsWith("}") then Walk.Done(seen, left.drop(1))
    else parseKey(left) match
      case None =>
        Walk.Failed(seen)
      case Some((key, afterKey)) =>
        val afterColon: String = skip(afterKey)
        if !afterColon.startsWith(":") then Walk.Failed(seen)
        else accept(key, afterColon.drop(1), seen, take, retain) match
          case None =>
            Walk.Failed(seen)
          case Some((next, afterValue)) =>
            val more: String = skip(afterValue)
            if more.startsWith("}") then Walk.Done(next, more.drop(1))
            else if more.startsWith(",") then fields(more.drop(1), next, take, allowEmpty = false, retain)
            else Walk.Failed(next)

  private def accept(
    key: String,
    rest: String,
    seen: Seen,
    take: Take,
    retain: Boolean
  ): Option[(Seen, String)] =
    take match
      case Take.Skip =>
        skipValue(rest).map(more => (seen, more))
      case Take.Request =>
        key match
          case "id" => takeId(rest, seen)
          case "argv" => takeArgv(rest, seen)
          case _ => skipValue(rest).map(more => (seen.copy(other = true), more))
      case Take.Response =>
        key match
          case "id" => takeId(rest, seen)
          case "ok" => takeOk(rest, seen)
          case "archive" => takeArchive(rest, seen)
          case "text" => takeOptionalString(rest, seen, error = false)
          case "error" => takeOptionalString(rest, seen, error = true)
          case "value" =>
            if retain then takeJson(rest, seen)
            else skipValue(rest).map(more => (seen.copy(values = seen.values + 1), more))
          case _ => skipValue(rest).map(more => (seen.copy(other = true), more))

  private def takeOk(rest: String, seen: Seen): Option[(Seen, String)] =
    val left: String = skip(rest)
    if left.startsWith("true") then Some((seen.copy(ok = Some(true), oks = seen.oks + 1), left.drop(4)))
    else if left.startsWith("false") then Some((seen.copy(ok = Some(false), oks = seen.oks + 1), left.drop(5)))
    else skipValue(left).map(more => (seen.copy(oks = seen.oks + 1), more))

  private def takeArchive(rest: String, seen: Seen): Option[(Seen, String)] =
    val left: String = skip(rest)
    if left.startsWith("\"") then
      parseString(left.drop(1), "") match
        case Left(_) => None
        case Right((text, more)) =>
          Some((seen.copy(archive = Some(text), archives = seen.archives + 1), more))
    else skipValue(left).map(more => (seen.copy(archives = seen.archives + 1), more))

  private def takeOptionalString(rest: String, seen: Seen, error: Boolean): Option[(Seen, String)] =
    val left: String = skip(rest)
    if left.startsWith("\"") then
      parseString(left.drop(1), "") match
        case Left(_) => None
        case Right((text, more)) =>
          val next: Seen =
            if error then seen.copy(error = Some(text), errors = seen.errors + 1)
            else seen.copy(text = Some(text), texts = seen.texts + 1)
          Some((next, more))
    else
      skipValue(left).map: more =>
        val next: Seen = if error then seen.copy(errors = seen.errors + 1) else seen.copy(texts = seen.texts + 1)
        (next, more)

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

  private def takeJson(rest: String, seen: Seen): Option[(Seen, String)] =
    readJson(rest) match
      case Right((json, more)) =>
        Some((seen.copy(json = Some(json), values = seen.values + 1), more))
      case Left(_) =>
        None

  private def skipValue(rest: String): Option[String] =
    val left: String = skip(rest)
    if left.isEmpty then None
    else if left.startsWith("{") then
      fields(left.drop(1), Seen(), Take.Skip, allowEmpty = true) match
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
        escape(escaped) match
          case None => Left(malformed)
          case Some((decoded, restOf)) => parseString(restOf, acc + bare + decoded)

  // quote emits \f and \u00XX. A response the shell reads has to accept those.
  private def escape(rest: String): Option[(String, String)] =
    if rest.isEmpty then None
    else rest.head match
      case '\\' => Some(("\\", rest.drop(1)))
      case '"' => Some(("\"", rest.drop(1)))
      case '/' => Some(("/", rest.drop(1)))
      case 'n' => Some(("\n", rest.drop(1)))
      case 't' => Some(("\t", rest.drop(1)))
      case 'r' => Some(("\r", rest.drop(1)))
      case 'b' => Some(("\b", rest.drop(1)))
      case 'f' => Some(("\f", rest.drop(1)))
      case 'u' =>
        val hex: String = rest.drop(1).take(4)
        if hex.length == 4 && hex.forall(isHex) then
          Some((Integer.parseInt(hex, 16).toChar.toString, rest.drop(5)))
        else None
      case _ => None

  private def isHex(ch: Char): Boolean =
    ch.isDigit || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')

  private def skip(rest: String): String =
    rest.dropWhile(ch => ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r')

  // Fraction, exponent, and an integer outside Long fail here.
  // parseNumber still only skips those tokens, so the shell can drop them.
  private[photo] def readJson(text: String): Either[String, (Json, String)] =
    val left: String = skip(text)
    if left.startsWith("null") then Right((Json.Null, left.drop(4)))
    else if left.startsWith("true") then Right((Json.Bool(true), left.drop(4)))
    else if left.startsWith("false") then Right((Json.Bool(false), left.drop(5)))
    else if left.startsWith("\"") then
      parseString(left.drop(1), "") match
        case Left(message) => Left(message)
        case Right((value, more)) => Right((Json.Str(value), more))
    else if left.startsWith("{") then readObject(left.drop(1), Nil, allowEmpty = true)
    else if left.startsWith("[") then readArray(left.drop(1), Nil, allowEmpty = true)
    else if left.startsWith("-") || left.headOption.exists(_.isDigit) then readLong(left)
    else Left(malformed)

  @tailrec
  private def readObject(
    rest: String,
    acc: List[(String, Json)],
    allowEmpty: Boolean
  ): Either[String, (Json, String)] =
    val left: String = skip(rest)
    if allowEmpty && left.startsWith("}") then Right((Json.Obj(acc.reverse), left.drop(1)))
    else parseKey(left) match
      case None =>
        Left(malformed)
      case Some((key, afterKey)) =>
        val afterColon: String = skip(afterKey)
        if !afterColon.startsWith(":") then Left(malformed)
        else readJson(afterColon.drop(1)) match
          case Left(message) =>
            Left(message)
          case Right((value, afterValue)) =>
            val more: String = skip(afterValue)
            val next: List[(String, Json)] = (key, value) :: acc
            if more.startsWith("}") then Right((Json.Obj(next.reverse), more.drop(1)))
            else if more.startsWith(",") then readObject(more.drop(1), next, allowEmpty = false)
            else Left(malformed)

  @tailrec
  private def readArray(
    rest: String,
    acc: List[Json],
    allowEmpty: Boolean
  ): Either[String, (Json, String)] =
    val left: String = skip(rest)
    if allowEmpty && left.startsWith("]") then Right((Json.Arr(acc.reverse), left.drop(1)))
    else readJson(left) match
      case Left(message) =>
        Left(message)
      case Right((value, afterValue)) =>
        val more: String = skip(afterValue)
        val next: List[Json] = value :: acc
        if more.startsWith("]") then Right((Json.Arr(next.reverse), more.drop(1)))
        else if more.startsWith(",") then readArray(more.drop(1), next, allowEmpty = false)
        else Left(malformed)

  private def readLong(rest: String): Either[String, (Json, String)] =
    val (negative, body) = if rest.startsWith("-") then (true, rest.drop(1)) else (false, rest)
    val (digits, more) = body.span(_.isDigit)
    if digits.isEmpty || (digits.startsWith("0") && digits.length > 1) then Left(malformed)
    else if more.startsWith(".") || more.startsWith("e") || more.startsWith("E") then Left(malformed)
    else if !inLongRange(negative, digits) then Left(malformed)
    else
      val token: String = if negative then "-" + digits else digits
      Right((Json.Long(token.toLong), more))

  private def inLongRange(negative: Boolean, digits: String): Boolean =
    val limit: String = if negative then "9223372036854775808" else "9223372036854775807"
    digits.length < limit.length || (digits.length == limit.length && digits <= limit)

  private[photo] def archiveOf(session: Session): String =
    DirectoryIndex.key(session.root.configuration.archive)

  private[photo] def toResult(response: Response): Result =
    def plain(error: String): String = error.stripPrefix("Photo Archive Tool: ")
    if response.ok then Result.Ok(response.text.getOrElse(""), Json.Null)
    else response.text match
      case Some(text) =>
        Result.Failed(text, Json.Null, plain(response.error.getOrElse("failed")))
      case None =>
        Result.Usage(plain(response.error.getOrElse("failed")))

  private[photo] def runtimeDirectory(): Either[String, File] =
    Option(System.getenv("XDG_RUNTIME_DIR")).map(_.trim).filter(_.nonEmpty) match
      case None => Left("XDG_RUNTIME_DIR is unset")
      case Some(dir) => Right(File(File(dir), "org.podval.tools.photo"))

  // Tests pass a temp directory. Production passes $XDG_RUNTIME_DIR/org.podval.tools.photo.
  private[photo] def listen(
    session: Session,
    runtime: File,
    onBound: ServerSocketChannel => Unit = _ => ()
  ): Int =
    prepare(runtime)
    val lockFile: File = File(runtime, lockName)
    val socketFile: File = File(runtime, socketName)
    if heldByThisJvm(lockFile) then
      Console.err.println("already running")
      0
    else
      val channel: FileChannel = FileChannel.open(
        lockFile.toPath,
        StandardOpenOption.CREATE,
        StandardOpenOption.READ,
        StandardOpenOption.WRITE
      )
      claim(channel) match
        case LockClaim.SameJvm =>
          pin(channel)
          Console.err.println("already running")
          0
        case LockClaim.OtherJvm =>
          channel.close()
          Console.err.println("already running")
          0
        case LockClaim.Owned(lock) =>
          takeOwner(lockFile, channel)
          var server: ServerSocketChannel = null
          try
            try
              java.nio.file.Files.deleteIfExists(socketFile.toPath)
              server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
              server.bind(UnixDomainSocketAddress.of(socketFile.toPath))
              java.nio.file.Files.setPosixFilePermissions(
                socketFile.toPath,
                PosixFilePermissions.fromString(fileMode)
              )
            catch case error: IOException =>
              Console.err.println(Option(error.getMessage).getOrElse("bind failed"))
              return 1
            Console.err.println(s"bound ${socketFile.getAbsolutePath}")
            onBound(server)
            acceptLoop(server, parked(session), socketFile)
          finally
            if server != null && server.isOpen then server.close()
            lock.release()
            dropOwner(channel)
            channel.close()

  private[photo] def ensure(directory: File): Either[String, Unit] =
    ensure(directory, () => spawnServe(directory), acceptWaitMs)

  private[photo] def ensure(
    directory: File,
    spawn: () => Either[String, Unit],
    waitMs: Long
  ): Either[String, Unit] =
    if canConnect(directory) then Right(())
    else if lockHeld(directory) then
      if waitUntilConnect(directory, waitMs) then Right(()) else Left(busyMessage(directory))
    else spawn() match
      case Left(message) => Left(message)
      case Right(_) =>
        if waitUntilConnect(directory, waitMs) then Right(()) else Left("serve did not start")

  private[photo] def client(
    runtime: File,
    argv: List[String],
    archive: String,
    stop: Boolean = false,
    waitMs: Long = acceptWaitMs
  ): Attempt =
    exchangeOrWait(runtime, argv, archive, stop, waitMs, dropValue) match
      case Reply.Absent if stop =>
        Attempt.Failed("serve is not running")
      case Reply.Absent =>
        Attempt.Absent
      case Reply.Failed(message) =>
        Attempt.Failed(message)
      case Reply.Done(called) =>
        Attempt.Done(called.response)

  private[photo] def call(
    runtime: File,
    argv: List[String],
    archive: String,
    waitMs: Long = acceptWaitMs
  ): Either[String, Called] =
    exchangeOrWait(runtime, argv, archive, stop = false, waitMs, readValued) match
      case Reply.Absent =>
        Left("serve is not running")
      case Reply.Failed(message) =>
        Left(message)
      case Reply.Done(called) =>
        Right(called)

  private def dropValue(body: String): Either[Problem, Called] =
    readResponse(body).map(response => Called(response, Json.Null))

  private def parked(session: Session): Session =
    session.copy(cursor = session.root, style = PathStyle.Serve)

  private enum Line:
    case Got(text: String)
    case Eof
    case TimedOut

  @tailrec
  private def acceptLoop(server: ServerSocketChannel, session: Session, socketFile: File): Int =
    if !server.isOpen then 0
    else
      val accepted: Option[SocketChannel] =
        try Option(server.accept())
        catch case _: ClosedChannelException => None
      accepted match
        case None =>
          0
        case Some(conn) =>
          val step: Cli.Step =
            try handle(conn, session)
            catch case _: IOException => Cli.Step.Continue(session)
            finally conn.close()
          step match
            case Cli.Step.Stop =>
              Console.err.println("stopping")
              java.nio.file.Files.deleteIfExists(socketFile.toPath)
              0
            case Cli.Step.Continue(next) =>
              acceptLoop(server, next, socketFile)

  private def handle(conn: SocketChannel, session: Session): Cli.Step =
    writeLine(conn, render(None, ok = true, None, None, None, archiveOf(session)))
    readLine(conn, None) match
      case Line.Got(text) =>
        val (step, response) = Cli.serveResponse(session, text)
        writeLine(conn, response)
        step
      case Line.Eof | Line.TimedOut =>
        Cli.Step.Continue(session)

  private def exchangeOrWait(
    runtime: File,
    argv: List[String],
    archive: String,
    stop: Boolean,
    waitMs: Long,
    readBody: String => Either[Problem, Called]
  ): Reply =
    def once(): Reply = exchange(runtime, argv, archive, stop, waitMs, readBody)
    once() match
      case Reply.Absent if lockHeld(runtime) =>
        if waitUntilConnect(runtime, waitMs) then
          once() match
            case Reply.Absent => Reply.Failed(busyMessage(runtime))
            case other => other
        else Reply.Failed(busyMessage(runtime))
      case other =>
        other

  private def exchange(
    runtime: File,
    argv: List[String],
    archive: String,
    stop: Boolean,
    waitMs: Long,
    readBody: String => Either[Problem, Called]
  ): Reply =
    openChannel(File(runtime, socketName)) match
      case None =>
        Reply.Absent
      case Some(channel) =>
        try talk(channel, runtime, argv, archive, stop, waitMs, readBody)
        catch case _: IOException => Reply.Failed("serve closed the connection")
        finally channel.close()

  private def talk(
    channel: SocketChannel,
    runtime: File,
    argv: List[String],
    archive: String,
    stop: Boolean,
    waitMs: Long,
    readBody: String => Either[Problem, Called]
  ): Reply =
    readLine(channel, Some(waitMs)) match
      case Line.TimedOut | Line.Eof =>
        Reply.Failed(busyMessage(runtime))
      case Line.Got(text) =>
        readResponse(text) match
          case Left(_) =>
            Reply.Failed(malformed)
          case Right(accept) if !stop && accept.archive != archive =>
            Reply.Failed(mismatchMessage(accept.archive, archive))
          case Right(_) =>
            writeLine(channel, renderRequest(None, argv))
            readLine(channel, None) match
              case Line.Got(body) =>
                readBody(body) match
                  case Left(_) => Reply.Failed(malformed)
                  case Right(called) => Reply.Done(called)
              case Line.Eof =>
                Reply.Failed("serve closed the connection")
              case Line.TimedOut =>
                Reply.Failed(busyMessage(runtime))

  private def openChannel(socket: File): Option[SocketChannel] =
    val channel: SocketChannel =
      try SocketChannel.open(StandardProtocolFamily.UNIX)
      catch case _: IOException => return None
    try
      channel.connect(UnixDomainSocketAddress.of(socket.toPath))
      Some(channel)
    catch case _: IOException =>
      channel.close()
      None

  private def canConnect(runtime: File): Boolean =
    val socket: File = File(runtime, socketName)
    if !socket.exists then false
    else openChannel(socket) match
      case None => false
      case Some(channel) =>
        channel.close()
        true

  private def waitUntilConnect(runtime: File, waitMs: Long): Boolean =
    val deadline: Long = System.nanoTime() + waitMs * 1000000L
    def loop(): Boolean =
      if canConnect(runtime) then true
      else if System.nanoTime() >= deadline then false
      else
        Thread.sleep(50)
        loop()
    loop()

  // A missing lock file means no server has created it. Do not create one from a client.
  private def lockHeld(runtime: File): Boolean =
    val file: File = File(runtime, lockName)
    if heldByThisJvm(file) then true
    else if !file.isFile then false
    else
      val channel: FileChannel =
        try FileChannel.open(file.toPath, StandardOpenOption.READ, StandardOpenOption.WRITE)
        catch case _: IOException => return false
      var closeChannel: Boolean = true
      try
        val lock: FileLock =
          try channel.tryLock(0L, Long.MaxValue, true)
          catch case _: OverlappingFileLockException =>
            closeChannel = false
            pin(channel)
            return true
        if lock == null then true
        else
          lock.release()
          false
      finally
        if closeChannel then channel.close()

  // Closing any descriptor for this file drops every fcntl lock this process holds.
  // A null tryLock is another process, so that channel is safe to close. Overlap is this JVM.
  private enum LockClaim:
    case Owned(lock: FileLock)
    case SameJvm
    case OtherJvm

  private def claim(channel: FileChannel): LockClaim =
    try
      val lock: FileLock = channel.tryLock()
      if lock == null then LockClaim.OtherJvm else LockClaim.Owned(lock)
    catch case _: OverlappingFileLockException => LockClaim.SameJvm

  private var ownerChannel: FileChannel = null
  private var ownerPath: String = null
  // Strong refs. A collected channel is closed, and that close releases the owner's lock.
  private var pinned: List[FileChannel] = Nil

  private def heldByThisJvm(lockFile: File): Boolean = synchronized {
    ownerChannel != null && ownerChannel.isOpen && ownerPath == lockFile.getAbsolutePath
  }

  private def takeOwner(lockFile: File, channel: FileChannel): Unit = synchronized {
    ownerChannel = channel
    ownerPath = lockFile.getAbsolutePath
  }

  private def dropOwner(channel: FileChannel): Unit = synchronized {
    if ownerChannel eq channel then
      ownerChannel = null
      ownerPath = null
  }

  private def pin(channel: FileChannel): Unit = synchronized {
    pinned = channel :: pinned
  }

  // The note is not the lock. Tests drop it so the next open hits OverlappingFileLockException.
  private[photo] def disarmOwner(): Unit = synchronized {
    ownerChannel = null
    ownerPath = null
  }

  private def prepare(runtime: File): Unit =
    val path = runtime.toPath
    java.nio.file.Files.createDirectories(path)
    java.nio.file.Files.setPosixFilePermissions(path, PosixFilePermissions.fromString(directoryMode))

  private def busyMessage(runtime: File): String =
    val lock: String = File(runtime, lockName).getAbsolutePath
    s"a command is in progress; photo serve stop runs only between commands; the JVM holding $lock is that process (fuser $lock)"

  private def mismatchMessage(server: String, local: String): String =
    s"serve archive is $server; this archive is $local; run photo serve stop"

  // The child recomputes the directory as $XDG_RUNTIME_DIR/org.podval.tools.photo.
  private def spawnServe(runtime: File): Either[String, Unit] =
    if !setsidOnPath then Left("setsid is missing")
    else
      val parent: File = runtime.getParentFile
      if parent == null then Left("runtime directory has no parent")
      else
        val javaHome: String = System.getProperty("java.home")
        val classPath: String = System.getProperty("java.class.path")
        val process: java.lang.ProcessBuilder = new java.lang.ProcessBuilder(
          "setsid",
          javaHome + "/bin/java",
          "-cp",
          classPath,
          "org.podval.tools.photo.Cli",
          "serve"
        )
        process.environment().put("XDG_RUNTIME_DIR", parent.getAbsolutePath)
        process.redirectInput(java.lang.ProcessBuilder.Redirect.from(new File("/dev/null")))
        process.redirectOutput(java.lang.ProcessBuilder.Redirect.DISCARD)
        process.redirectError(java.lang.ProcessBuilder.Redirect.appendTo(serveLog()))
        try
          process.start()
          Right(())
        catch case error: IOException =>
          Left(Option(error.getMessage).getOrElse("setsid is missing"))

  private def setsidOnPath: Boolean =
    val path: String = Option(System.getenv("PATH")).getOrElse("")
    path.split(File.pathSeparator).exists(dir => File(dir, "setsid").canExecute)

  private def serveLog(): File =
    val base: File = Option(System.getenv("XDG_STATE_HOME")).map(_.trim).filter(_.nonEmpty) match
      case Some(dir) => File(dir)
      case None => File(File(System.getProperty("user.home"), ".local"), "state")
    val dir: File = File(base, "org.podval.tools.photo")
    java.nio.file.Files.createDirectories(dir.toPath)
    java.nio.file.Files.setPosixFilePermissions(dir.toPath, PosixFilePermissions.fromString(directoryMode))
    val file: File = File(dir, "serve.log")
    val perms = PosixFilePermissions.fromString(fileMode)
    if !java.nio.file.Files.exists(file.toPath) then
      java.nio.file.Files.createFile(file.toPath, PosixFilePermissions.asFileAttribute(perms))
    else java.nio.file.Files.setPosixFilePermissions(file.toPath, perms)
    file

  private def writeLine(channel: SocketChannel, text: String): Unit =
    val buf: ByteBuffer = ByteBuffer.wrap((text + "\n").getBytes(StandardCharsets.UTF_8))
    while buf.hasRemaining do channel.write(buf)

  private def readLine(channel: SocketChannel, timeoutMs: Option[Long]): Line =
    timeoutMs match
      case None => readBlocking(channel)
      case Some(ms) => readTimed(channel, ms)

  private def readBlocking(channel: SocketChannel): Line =
    val raw: ByteArrayOutputStream = new ByteArrayOutputStream
    val chunk: ByteBuffer = ByteBuffer.allocate(1024)
    var done: Boolean = false
    var eof: Boolean = false
    while !done && !eof do
      chunk.clear()
      val n: Int = channel.read(chunk)
      if n < 0 then eof = true
      else done = takeBytes(chunk, raw)
    if !done && raw.size == 0 then Line.Eof else Line.Got(raw.toString(StandardCharsets.UTF_8))

  private def readTimed(channel: SocketChannel, timeoutMs: Long): Line =
    val wasBlocking: Boolean = channel.isBlocking
    channel.configureBlocking(false)
    val selector: Selector = Selector.open()
    try
      channel.register(selector, java.nio.channels.SelectionKey.OP_READ)
      val raw: ByteArrayOutputStream = new ByteArrayOutputStream
      val chunk: ByteBuffer = ByteBuffer.allocate(1024)
      val deadline: Long = System.nanoTime() + timeoutMs * 1000000L
      var done: Boolean = false
      var eof: Boolean = false
      var timedOut: Boolean = false
      while !done && !eof && !timedOut do
        chunk.clear()
        val n: Int = channel.read(chunk)
        if n > 0 then done = takeBytes(chunk, raw)
        else if n < 0 then eof = true
        else
          val remaining: Long = deadline - System.nanoTime()
          if remaining <= 0 then timedOut = true
          else
            val selected: Int = selector.select(Math.max(remaining / 1000000L, 1L))
            // A key left in the selected set makes the next select return immediately.
            selector.selectedKeys().clear()
            if selected == 0 && System.nanoTime() >= deadline then timedOut = true
      if timedOut && !done then Line.TimedOut
      else if !done && raw.size == 0 then Line.Eof
      else Line.Got(raw.toString(StandardCharsets.UTF_8))
    finally
      selector.close()
      if channel.isOpen && wasBlocking then channel.configureBlocking(true)

  // Returns true once a newline has been stored out of the chunk.
  private def takeBytes(chunk: ByteBuffer, raw: ByteArrayOutputStream): Boolean =
    chunk.flip()
    var done: Boolean = false
    while chunk.hasRemaining && !done do
      val b: Int = chunk.get().toInt & 0xff
      if b == '\n' then done = true else raw.write(b)
    done
