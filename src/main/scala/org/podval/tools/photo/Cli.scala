package org.podval.tools.photo

import scala.annotation.tailrec
import java.io.File

object Cli:
  private[photo] sealed trait Outcome
  private[photo] final case class Help(text: String) extends Outcome
  private[photo] final case class Usage(message: String) extends Outcome

  private[photo] sealed trait Parsed
  private[photo] final case class Lsd(directory: String, long: Boolean) extends Parsed
  private[photo] final case class Ls(
    path: String,
    long: Boolean,
    nameContains: Option[String],
    hasExtension: Option[String],
    property: Option[String],
    timestampFromNameFormat: Option[String],
    timestampFromName: Boolean,
    timestampFromNameOnly: Boolean,
    timestampsVary: Boolean,
    hasMetadata: Boolean,
    noNormal: Boolean,
    noTimestamp: Boolean,
    wrongTimestamp: Boolean,
    wrongDay: Boolean,
    wrong: Boolean
  ) extends Parsed
  private[photo] final case class SetMetadataArgs(property: String, picture: String) extends Parsed
  private[photo] final case class Albums(path: Option[String]) extends Parsed
  private[photo] final case class PublishArgs(path: Option[String], recursive: Boolean, dryRun: Boolean) extends Parsed
  private[photo] final case class SyncArgs(path: Option[String], recursive: Boolean, dryRun: Boolean) extends Parsed
  private[photo] final case class RepairArgs(
    path: String,
    names: Boolean,
    raw: Boolean,
    timestamps: Boolean,
    days: Boolean,
    dryRun: Boolean
  ) extends Parsed
  private[photo] case object LoginArgs extends Parsed
  private[photo] case object StatusArgs extends Parsed
  private[photo] case object LogoutArgs extends Parsed
  private[photo] final case class CdArgs(path: Option[String]) extends Parsed
  private[photo] case object PwdArgs extends Parsed
  private[photo] case object ExitArgs extends Parsed
  private[photo] final case class AgentStemsArgs(day: String) extends Parsed
  private[photo] case object AgentCatalogArgs extends Parsed
  private[photo] final case class AgentItemsArgs(id: Option[String], album: String) extends Parsed
  private[photo] final case class AgentMatchArgs(id: Option[String], album: String) extends Parsed
  private[photo] final case class AgentIncludeArgs(span: Option[String], paths: Seq[String]) extends Parsed
  private[photo] final case class AgentAlbumFileArgs(
    id: Option[String],
    dryRun: Boolean,
    replace: Boolean,
    googleTitle: Option[String],
    noRequest: Boolean,
    album: String
  ) extends Parsed
  private[photo] final case class AgentRequestArgs(
    action: String,
    album: Option[String],
    item: Option[String],
    current: Option[String],
    updated: Option[String],
    id: Option[String]
  ) extends Parsed
  private[photo] case object AgentRequestsArgs extends Parsed
  private[photo] case object AgentResultsArgs extends Parsed

  private[photo] final case class Run(parsed: Parsed) extends Outcome
  private[photo] case object ShellFront extends Outcome
  private[photo] case object ServeFront extends Outcome
  private[photo] case object ServeStop extends Outcome

  private[photo] enum Front:
    case OneShot, Shell, Serve

  def main(args: Array[String]): Unit =
    parse(args.toList) match
      case Help(text) =>
        println(text)
      case Usage(message) =>
        System.err.println(usageLine(message))
        sys.exit(1)
      case ShellFront =>
        val session: Session = Session.open(PathStyle.Cursor)
        Serve.runtimeDirectory() match
          case Left(message) =>
            System.err.println(usageLine(message))
            sys.exit(1)
          case Right(runtime) =>
            Serve.ensure(runtime) match
              case Left(message) =>
                System.err.println(usageLine(message))
                sys.exit(1)
              case Right(_) =>
                val code: Int = shell(
                  session,
                  stdinLines,
                  exchange = Some((current, argv) => remoteResult(runtime, current, argv))
                )
                if code != 0 then sys.exit(code)
      case ServeFront =>
        Serve.runtimeDirectory() match
          case Left(message) =>
            System.err.println(usageLine(message))
            sys.exit(1)
          case Right(runtime) =>
            val code: Int = Serve.listen(Session.open(PathStyle.Serve), runtime)
            if code != 0 then sys.exit(code)
      case ServeStop =>
        try
          val archive: String = Serve.archiveOf(Session.open(PathStyle.OneShot))
          executeStop(Serve.runtimeDirectory(), archive) match
            case Left(message) =>
              System.err.println(usageLine(message))
              sys.exit(1)
            case Right(response) =>
              println(response.archive)
              if !response.ok then
                response.error.foreach(System.err.println)
                sys.exit(1)
        catch
          case error: (IllegalArgumentException | IllegalStateException) =>
            System.err.println(usageLine(Operation.messageOf(error)))
            sys.exit(1)
      case Run(parsed) =>
        try
          val session: Session = Session.start
          val result: Result = executeOneShot(session, args.toList, parsed, Serve.runtimeDirectory())
          render(result)
        catch
          case error: (IllegalArgumentException | IllegalStateException) =>
            System.err.println(usageLine(Operation.messageOf(error)))
            sys.exit(1)

  private def stdinLines: Iterator[String] =
    Iterator.continually(scala.io.StdIn.readLine()).takeWhile(_ != null)

  private def render(result: Result): Unit = result match
    case Result.Ok(text, _) =>
      print(text)
    case Result.Failed(text, _, message) =>
      print(text)
      System.err.println(usageLine(message))
      sys.exit(1)
    case Result.Usage(message) =>
      System.err.println(usageLine(message))
      sys.exit(1)

  private[photo] enum Step:
    case Continue(session: Session)
    case Stop

  private[photo] def shell(
    session: Session,
    lines: Iterator[String],
    run: (Session, Parsed) => (Session, Result) = (session, parsed) => toOperation(parsed).execute(session),
    exchange: Option[(Session, List[String]) => Result] = None
  ): Int =
    @tailrec
    def loop(current: Session): Int =
      Console.err.print(current.prompt)
      Console.err.flush()
      if !lines.hasNext then 0
      else step(current, lines.next(), run, exchange) match
        case Step.Stop => 0
        case Step.Continue(next) => loop(next)
    loop(session)

  private[photo] def serve(
    session: Session,
    lines: Iterator[String],
    run: (Session, Parsed) => (Session, Result) = (session, parsed) => toOperation(parsed).execute(session)
  ): Int =
    @tailrec
    def loop(current: Session): Int =
      if !lines.hasNext then 0
      else
        val (step, rendered) = serveResponse(current, lines.next(), run)
        println(rendered)
        step match
          case Step.Stop => 0
          case Step.Continue(next) => loop(next)
    loop(session)

  // The socket writes this line on the connection. The line handler prints it.
  private[photo] def serveResponse(
    session: Session,
    line: String,
    run: (Session, Parsed) => (Session, Result) = (session, parsed) => toOperation(parsed).execute(session)
  ): (Step, String) =
    val archive: String = Serve.archiveOf(session)
    def reply(
      id: Option[String],
      ok: Boolean,
      text: Option[String],
      error: Option[String],
      value: Option[Json]
    ): String =
      Serve.render(id, ok, text, error, value, archive)

    def usage(id: Option[String], message: String): (Step, String) =
      (Step.Continue(parked(session)), reply(id, ok = false, None, Some(usageLine(message)), None))

    Serve.read(line) match
      case Left(problem) =>
        usage(problem.id, problem.message)
      case Right(request) =>
        parse(request.argv, Front.Serve) match
          case Help(text) =>
            // The shell prints help with println, so the human text ends with a newline.
            (Step.Continue(parked(session)), reply(request.id, ok = true, Some(text + "\n"), None, Some(Json.Null)))
          case Usage(message) =>
            usage(request.id, message)
          case ShellFront | ServeFront =>
            usage(request.id, "already running")
          case ServeStop =>
            (Step.Stop, reply(request.id, ok = true, None, None, None))
          case Run(parsed) =>
            val (returned, result) = run(session, parsed)
            result match
              case Result.Ok(text, value) =>
                (Step.Continue(parked(returned)), reply(request.id, ok = true, Some(text), None, Some(value)))
              case Result.Failed(text, value, message) =>
                (
                  Step.Continue(parked(returned)),
                  reply(request.id, ok = false, Some(text), Some(usageLine(message)), Some(value))
                )
              case Result.Usage(message) =>
                usage(request.id, message)

  // Serve parses from the archive root. The cursor field is unused, so put it back.
  private def parked(session: Session): Session =
    session.copy(cursor = session.root, style = PathStyle.Serve)

  private def step(
    session: Session,
    line: String,
    run: (Session, Parsed) => (Session, Result),
    exchange: Option[(Session, List[String]) => Result]
  ): Step =
    splitLine(line) match
      case Left(message) =>
        Console.err.println(usageLine(message))
        Step.Continue(session)
      case Right(Nil) =>
        Step.Continue(session)
      case Right(args) =>
        parse(args, Front.Shell) match
          case Help(text) =>
            println(text)
            Step.Continue(session)
          case Usage(message) =>
            Console.err.println(usageLine(message))
            Step.Continue(session)
          case ShellFront | ServeFront | ServeStop =>
            Console.err.println(usageLine("already running"))
            Step.Continue(session)
          case Run(ExitArgs) =>
            Step.Stop
          case Run(parsed) if exchange.isDefined && !staysLocal(parsed) =>
            remoteStep(session, args, exchange.get)
          case Run(parsed) =>
            val (returned, result) = run(session, parsed)
            show(session, returned, result, keepReturned = true)

  private def staysLocal(parsed: Parsed): Boolean = parsed match
    case CdArgs(_) | PwdArgs | ExitArgs => true
    case _ => false

  private def remoteStep(
    session: Session,
    args: List[String],
    exchange: (Session, List[String]) => Result
  ): Step =
    val argv: List[String] =
      try rewritePaths(session, args)
      catch case error: IllegalArgumentException =>
        Console.err.println(usageLine(Operation.messageOf(error)))
        return Step.Continue(session)
    show(session, session, exchange(session, argv), keepReturned = false)

  private def show(session: Session, returned: Session, result: Result, keepReturned: Boolean): Step =
    result match
      case Result.Ok(text, _) =>
        print(text)
        Step.Continue(if keepReturned then returned else session)
      case Result.Failed(text, _, message) =>
        print(text)
        Console.err.println(usageLine(message))
        Step.Continue(if keepReturned then returned else session)
      case Result.Usage(message) =>
        Console.err.println(usageLine(message))
        Step.Continue(session)

  // Path positionals are marked on the command. Flags stay as typed.
  private def rewritePaths(session: Session, args: List[String]): List[String] =
    locate(args) match
      case None =>
        args
      case Some((command, commandArgs, prefix)) =>
        readFlags(command, commandArgs, Values()) match
          case Left(_) =>
            args
          case Right((_, positionals)) =>
            val flags: List[String] = commandArgs.take(commandArgs.length - positionals.length)
            prefix ++ flags ++ rewritePositionals(session, command.positionals, positionals)

  private def locate(args: List[String]): Option[(Command, List[String], List[String])] = args match
    case "agent" :: "request" :: name :: rest =>
      requestCommands.find(_.name == name).map(command => (command, rest, List("agent", "request", name)))
    case "agent" :: name :: rest =>
      agentCommands.find(_.name == name).map(command => (command, rest, List("agent", name)))
    case name :: rest =>
      registry.find(_.name == name).map(command => (command, rest, List(name)))
    case Nil =>
      None

  private def rewritePositionals(session: Session, expected: List[Positional], args: List[String]): List[String] =
    (expected, args) match
      case (Nil, rest) =>
        rest
      case (Rest(_, path) :: _, rest) =>
        if path then rest.map(token => rootForm(session, token)) else rest
      case (_, Nil) =>
        Nil
      case (positional :: moreExpected, token :: moreArgs) =>
        val rewritten: String = if positional.path then rootForm(session, token) else token
        rewritten :: rewritePositionals(session, moreExpected, moreArgs)

  private def rootForm(session: Session, token: String): String =
    val text: String = session.parse(token).toString.stripPrefix("/")
    if text.isEmpty then "/" else text

  private[photo] def remoteResult(runtime: File, session: Session, argv: List[String]): Result =
    Serve.client(runtime, argv, Serve.archiveOf(session)) match
      case Serve.Attempt.Absent => Result.Usage("serve is not running")
      case Serve.Attempt.Failed(message) => Result.Usage(message)
      case Serve.Attempt.Done(response) => Serve.toResult(response)

  private[photo] def executeOneShot(
    session: Session,
    argv: List[String],
    parsed: Parsed,
    runtime: Either[String, File],
    waitMs: Long = 5000
  ): Result =
    def cold: Result = toOperation(parsed).execute(session)._2
    runtime match
      case Left(_) =>
        cold
      case Right(dir) =>
        Serve.client(dir, argv, Serve.archiveOf(session), waitMs = waitMs) match
          case Serve.Attempt.Absent => cold
          case Serve.Attempt.Failed(message) => Result.Usage(message)
          case Serve.Attempt.Done(response) => Serve.toResult(response)

  private[photo] def executeStop(
    runtime: Either[String, File],
    archive: String,
    waitMs: Long = 5000
  ): Either[String, Serve.Response] =
    runtime match
      case Left(_) =>
        Left("serve is not running")
      case Right(dir) =>
        Serve.client(dir, List("serve", "stop"), archive, stop = true, waitMs = waitMs) match
          case Serve.Attempt.Done(response) => Right(response)
          case Serve.Attempt.Absent => Left("serve is not running")
          case Serve.Attempt.Failed(message) => Left(message)

  // Inside quotes only \\ and \" are escapes. Any other backslash stays in the token.
  private[photo] def splitLine(line: String): Either[String, List[String]] =
    collect(line, Nil)

  @tailrec
  private def collect(rest: String, acc: List[String]): Either[String, List[String]] =
    val left: String = rest.dropWhile(_.isWhitespace)
    if left.isEmpty then Right(acc.reverse)
    else readToken(left, "") match
      case Left(message) => Left(message)
      case Right((token, more)) => collect(more, token :: acc)

  private def readToken(rest: String, acc: String): Either[String, (String, String)] =
    if rest.isEmpty || rest.head.isWhitespace then Right((acc, rest))
    else rest.head match
      case '\\' =>
        val more: String = rest.drop(1)
        if more.isEmpty then Left("trailing backslash")
        else readToken(more.drop(1), acc + more.take(1))
      case '"' =>
        readQuoted(rest.drop(1), acc) match
          case Left(message) => Left(message)
          case Right((quoted, more)) => readToken(more, quoted)
      case _ =>
        val (bare, more) = rest.span(ch => !ch.isWhitespace && ch != '\\' && ch != '"')
        readToken(more, acc + bare)

  private def readQuoted(rest: String, acc: String): Either[String, (String, String)] =
    if rest.isEmpty then Left("unclosed quote")
    else
      val (bare, more) = rest.span(ch => ch != '"' && ch != '\\')
      val soFar: String = acc + bare
      if more.isEmpty then Left("unclosed quote")
      else if more.head == '"' then Right((soFar, more.drop(1)))
      else
        val escaped: String = more.drop(1)
        if escaped.isEmpty then Left("unclosed quote")
        else
          val taken: String = escaped.head match
            case '\\' => "\\"
            case '"' => "\""
            case other => s"\\$other"
          readQuoted(escaped.drop(1), soFar + taken)

  private[photo] def parse(args: List[String], front: Front = Front.OneShot): Outcome = args match
    case Nil =>
      Usage("missing command")
    case ("--help" | "-h") :: _ =>
      Help(topHelp(front))
    case "shell" :: rest =>
      front match
        case Front.Shell | Front.Serve =>
          Usage("already running")
        case Front.OneShot =>
          rest match
            case Nil =>
              ShellFront
            case extra :: _ =>
              Usage(s"extra argument '$extra'")
    case "serve" :: rest =>
      front match
        case Front.Serve =>
          rest match
            case "stop" :: Nil =>
              ServeStop
            case _ =>
              Usage("already running")
        case Front.OneShot =>
          rest match
            case Nil =>
              ServeFront
            case "stop" :: Nil =>
              ServeStop
            case "stop" :: extra :: _ =>
              Usage(s"extra argument '$extra'")
            case extra :: _ =>
              Usage(s"extra argument '$extra'")
        case Front.Shell =>
          lookup("serve", rest, front)
    case "agent" :: rest =>
      parseAgent(rest, front)
    case name :: rest =>
      lookup(name, rest, front)

  private def lookup(name: String, rest: List[String], front: Front): Outcome =
    registry.find(_.name == name) match
      case None => Usage(s"unknown command '$name'")
      case Some(command) => parseCommand(command, rest, front)

  private[photo] def toOperation(parsed: Parsed): Operation = parsed match
    case Lsd(directory, long) =>
      ListDirectories(directory, isLong = long)
    case ls: Ls =>
      ListPictures(
        ls.path,
        ls.long,
        ls.nameContains,
        ls.hasExtension,
        ls.property,
        ls.timestampFromNameFormat,
        ls.timestampFromName,
        ls.timestampFromNameOnly,
        ls.timestampsVary,
        ls.hasMetadata,
        ls.noNormal,
        ls.noTimestamp,
        ls.wrongTimestamp,
        ls.wrongDay,
        ls.wrong
      )
    case SetMetadataArgs(property, picture) =>
      SetMetadata(picture, property)
    case Albums(path) =>
      ListAlbums(path)
    case PublishArgs(path, recursive, dryRun) =>
      Publish(path, recursive = recursive, dryRun = dryRun)
    case SyncArgs(path, recursive, dryRun) =>
      Sync(path, recursive = recursive, dryRun = dryRun)
    case LoginArgs => Login
    case StatusArgs => Status
    case LogoutArgs => Logout
    case RepairArgs(path, names, raw, timestamps, days, dryRun) =>
      Repair(path, names, raw, timestamps, days, dryRun)
    case CdArgs(path) =>
      Cd(path)
    case PwdArgs =>
      Pwd
    case ExitArgs =>
      Exit
    case AgentStemsArgs(day) =>
      AgentStems(day)
    case AgentCatalogArgs =>
      AgentCatalog
    case AgentItemsArgs(id, album) =>
      AgentItems(id, album)
    case AgentMatchArgs(id, album) =>
      AgentMatch(id, album)
    case AgentIncludeArgs(span, paths) =>
      AgentInclude(span, paths)
    case AgentAlbumFileArgs(id, dryRun, replace, googleTitle, noRequest, album) =>
      AgentAlbumFile(id, dryRun, replace, googleTitle, noRequest, album)
    case AgentRequestArgs(action, album, item, current, updated, id) =>
      AgentRequest(Requests.Action.fromWire(action), album, item, current, updated, id)
    case AgentRequestsArgs =>
      AgentRequests
    case AgentResultsArgs =>
      AgentResults

  private def finish(command: Command, values: Values, positionals: List[String], front: Front): Outcome =
    if command.name == "help" then helpText(positionals, front)
    else command.build(values, positionals) match
      case RepairArgs(_, names, raw, timestamps, days, _) if !names && !raw && !timestamps && !days =>
        Usage("missing repair action")
      case other =>
        Run(other)

  private def helpText(positionals: List[String], front: Front): Outcome = positionals match
    case Nil =>
      Help(topHelp(front))
    case name :: Nil if name == "agent" =>
      Help(agentHelp)
    case name :: Nil =>
      registry.find(_.name == name) match
        case None => Usage(s"unknown command '$name'")
        case Some(found) => Help(commandHelp(found))
    case extra :: _ =>
      Usage(s"extra argument '$extra'")

  private[photo] def usageLine(message: String): String = s"Photo Archive Tool: $message"

  private[photo] def parsePropertyAndValue(string: String): Property.AndValue[?] =
    val (name: String, value: Option[String]) = Files.splitOnLast(string, '=')
    Property.AndValue(Property.forName(name), value)

  private def parseCommand(command: Command, args: List[String], front: Front): Outcome =
    readFlags(command, args, Values()) match
      case Left(outcome) => outcome
      case Right((values, positionals)) =>
        checkPositionals(command.positionals, positionals) match
          case Some(message) => Usage(message)
          case None => finish(command, values, positionals, front)

  private def readFlags(
    command: Command,
    args: List[String],
    values: Values
  ): Either[Outcome, (Values, List[String])] = args match
    case Nil =>
      Right((values, Nil))
    case token :: rest =>
      splitFlag(token) match
        case None =>
          Right((values, args))
        case Some((true, "", None)) =>
          Right((values, rest))
        case _ if token == "--help" || token == "-h" =>
          Left(Help(commandHelp(command)))
        case Some((isLong, name, attached)) =>
          if !isLong && name.length != 1 then
            Left(Usage(s"unknown option '$token'"))
          else findFlag(command.flags, isLong = isLong, name = name) match
            case None =>
              Left(Usage(s"unknown option '$token'"))
            case Some(flag) if values.has(flag.name) =>
              Left(Usage(s"repeated option '--${flag.name}'"))
            case Some(flag) =>
              readValue(command, flag, attached, rest, values)

  private def readValue(
    command: Command,
    flag: Flag,
    attached: Option[String],
    rest: List[String],
    values: Values
  ): Either[Outcome, (Values, List[String])] = flag.kind match
    case BooleanKind =>
      attached match
        case None =>
          readFlags(command, rest, values.setBool(flag.name, true))
        case Some(raw) =>
          parseBool(raw) match
            case Some(value) => readFlags(command, rest, values.setBool(flag.name, value))
            case None => Left(Usage(s"invalid value '$raw' for '--${flag.name}'"))
    case TextKind =>
      attached match
        case Some("") =>
          Left(Usage(s"missing value for '--${flag.name}'"))
        case Some(raw) =>
          readFlags(command, rest, values.setText(flag.name, raw))
        case None =>
          rest match
            case Nil =>
              Left(Usage(s"missing value for '--${flag.name}'"))
            case "--" :: Nil =>
              Left(Usage(s"missing value for '--${flag.name}'"))
            // "--" after a text option supplies the value. A bare "--" still ends flags.
            case "--" :: value :: more =>
              readFlags(command, more, values.setText(flag.name, value))
            case value :: _ if value.startsWith("-") =>
              Left(Usage(s"value for '--${flag.name}' looks like a flag: '$value'"))
            case value :: more =>
              readFlags(command, more, values.setText(flag.name, value))

  private def splitFlag(token: String): Option[(Boolean, String, Option[String])] =
    val (long: Boolean, body: String) =
      if token.startsWith("--") then (true, token.drop(2))
      else if token.startsWith("-") then (false, token.drop(1))
      else (false, "")
    if !token.startsWith("-") then None
    else
      val (name: String, rest: String) = body.span(_ != '=')
      val value: Option[String] = if rest.isEmpty then None else Some(rest.drop(1))
      Some((long, name, value))

  private def findFlag(flags: List[Flag], isLong: Boolean, name: String): Option[Flag] =
    if isLong then flags.find(_.name == name)
    else flags.find(_.alias.contains(name))

  private def parseBool(raw: String): Option[Boolean] =
    if raw.equalsIgnoreCase("true") then Some(true)
    else if raw.equalsIgnoreCase("false") then Some(false)
    else None

  private def checkPositionals(expected: List[Positional], args: List[String]): Option[String] =
    (expected, args) match
      case (Nil, Nil) => None
      case (Nil, extra :: _) => Some(s"extra argument '$extra'")
      // Rest has no "too many". A flag after a positional is still an extra argument.
      case (Rest(_, _) :: _, rest) =>
        rest.find(_.startsWith("-")).map(token => s"extra argument '$token'")
      case (Required(name, _) :: _, Nil) => Some(s"missing $name")
      case (Optional(_, _) :: _, Nil) => None
      case (_ :: restExpected, _ :: restArgs) => checkPositionals(restExpected, restArgs)

  private def bool(values: Values, name: String): Boolean = values.bools.getOrElse(name, false)

  private def topHelp(front: Front): String =
    val header: List[String] = List(
      "Photo Archive Tool 0.0.1",
      "manipulates photo archive",
      ""
    )
    val sessionCommands: List[Command] = front match
      case Front.Shell =>
        List(cdCommand, pwdCommand, exitCommand)
      case Front.OneShot | Front.Serve =>
        Nil
    val frontLines: List[String] = front match
      case Front.OneShot =>
        List(shellLine, serveLine)
      case Front.Shell | Front.Serve =>
        Nil
    val lines: List[String] =
      (commands ++ List(helpCommand)).map(lineOf) ++
        frontLines ++
        List(agentLine) ++
        sessionCommands.map(lineOf)
    (header ++ lines).mkString("\n")

  private def lineOf(command: Command): String = s"${command.name}  ${command.help}"

  private def parseAgent(args: List[String], front: Front): Outcome = args match
    case Nil =>
      Usage("missing agent command")
    case ("--help" | "-h") :: _ =>
      Help(agentHelp)
    case "request" :: rest =>
      parseRequest(rest, front)
    case name :: rest =>
      agentCommands.find(_.name == name) match
        case None => Usage(s"unknown command '$name'")
        case Some(command) => parseCommand(command, rest, front)

  private def parseRequest(args: List[String], front: Front): Outcome = args match
    case Nil =>
      Usage("missing request action")
    case ("--help" | "-h") :: _ =>
      Help(requestHelp)
    case name :: rest =>
      requestCommands.find(_.name == name) match
        case None => Usage(s"unknown command '$name'")
        case Some(command) => parseCommand(command, rest, front)

  private def agentHelp: String =
    val lines: List[String] = agentCommands.flatMap: command =>
      if command.name == "album-file" then List(lineOf(command), requestLine) else List(lineOf(command))
    (List(agentLine, "") ++ lines).mkString("\n")

  private def requestHelp: String =
    (List(requestLine, "") ++ requestCommands.map(lineOf)).mkString("\n")

  private def commandHelp(command: Command): String =
    val positionals: String =
      if command.positionals.isEmpty then "none"
      else command.positionals.map(describePositional).mkString("\n")
    val flags: String =
      if command.flags.isEmpty then "none"
      else command.flags.map(describeFlag).mkString("\n")
    List(
      s"${command.name}  ${command.help}",
      "",
      "Positionals:",
      positionals,
      "",
      "Flags:",
      flags
    ).mkString("\n")

  private def describePositional(positional: Positional): String = positional match
    case Required(name, _) => s"$name  required"
    case Optional(name, _) => s"$name  optional"
    case Rest(name, _) => s"$name  rest"

  private def describeFlag(flag: Flag): String =
    val names: String = flag.alias match
      case Some(alias) => s"--${flag.name}, -$alias"
      case None => s"--${flag.name}"
    val kind: String = flag.kind match
      case BooleanKind => "boolean"
      case TextKind => "text"
    s"$names  $kind  ${flag.description}"

  private sealed trait Positional:
    def path: Boolean
  private final case class Required(name: String, path: Boolean = false) extends Positional
  private final case class Optional(name: String, path: Boolean = false) extends Positional
  private final case class Rest(name: String, path: Boolean = false) extends Positional

  private sealed trait FlagKind
  private case object BooleanKind extends FlagKind
  private case object TextKind extends FlagKind

  private final case class Flag(name: String, alias: Option[String], kind: FlagKind, description: String)

  private final case class Values(
    bools: Map[String, Boolean] = Map.empty,
    texts: Map[String, String] = Map.empty
  ):
    def has(name: String): Boolean = bools.contains(name) || texts.contains(name)
    def setBool(name: String, value: Boolean): Values = copy(bools = bools.updated(name, value))
    def setText(name: String, value: String): Values = copy(texts = texts.updated(name, value))

  private final case class Command(
    name: String,
    help: String,
    positionals: List[Positional],
    flags: List[Flag],
    build: (Values, List[String]) => Parsed
  )

  private val lsdLongFlag: Flag = Flag("long", Some("l"), BooleanKind, "list nested directories")
  private val lsLongFlag: Flag = Flag("long", Some("l"), BooleanKind, "show per-file timestamps and day mismatches")

  private val lsFlags: List[Flag] = List(
    lsLongFlag,
    Flag("name-contains", Some("n"), TextKind, "keep pictures whose name contains this text"),
    Flag("has-extension", Some("e"), TextKind, "keep pictures that have this extension"),
    Flag("property", None, TextKind, "keep pictures whose sidecar property matches"),
    Flag("timestamp-from-name-format", None, TextKind, "keep pictures whose filename clock uses this format"),
    Flag("timestamp-from-name", None, BooleanKind, "keep pictures whose filename encodes a time"),
    Flag("timestamp-from-name-only", None, BooleanKind, "keep pictures with a filename clock and no file metadata time"),
    Flag("timestamps-vary", None, BooleanKind, "keep pictures whose file times differ by more than an hour"),
    Flag("has-metadata", None, BooleanKind, "keep pictures that have a sidecar"),
    Flag("no-normal", None, BooleanKind, "keep pictures with no normal image"),
    Flag("no-timestamp", None, BooleanKind, "keep pictures with no assessed timestamp"),
    Flag("wrong-timestamp", None, BooleanKind, "keep pictures whose file mtime differs from the assessed timestamp"),
    Flag("wrong-day", None, BooleanKind, "keep pictures filed on a different day than the assessed timestamp"),
    Flag("wrong", None, BooleanKind, "keep pictures with no normal image, no timestamp, a wrong timestamp, or a wrong day")
  )

  private val repairFlags: List[Flag] = List(
    Flag(
      "names",
      None,
      BooleanKind,
      "rename one MVIMG_, PXL_, IMG_, VID_, or MVI_ prefix, add v when a video shares a stem with a still, and lowercase known extensions"
    ),
    Flag("raw", None, BooleanKind, "make a JPEG from RAW with dcraw when no normal image exists"),
    Flag("timestamps", None, BooleanKind, "set file mtimes from the assessed instant"),
    Flag("days", None, BooleanKind, "move a picture into the assessed day folder"),
    Flag("dry-run", None, BooleanKind, "print those actions and write nothing")
  )

  private val commands: List[Command] = List(
    Command(
      name = "lsd",
      help = "list directories",
      positionals = List(Required("directory", path = true)),
      flags = List(lsdLongFlag),
      build = (values, args) => Lsd(args.head, bool(values, "long"))
    ),
    Command(
      name = "ls",
      help = "list pictures",
      positionals = List(Required("path", path = true)),
      flags = lsFlags,
      build = (values, args) => Ls(
        path = args.head,
        long = bool(values, "long"),
        nameContains = values.texts.get("name-contains"),
        hasExtension = values.texts.get("has-extension"),
        property = values.texts.get("property"),
        timestampFromNameFormat = values.texts.get("timestamp-from-name-format"),
        timestampFromName = bool(values, "timestamp-from-name"),
        timestampFromNameOnly = bool(values, "timestamp-from-name-only"),
        timestampsVary = bool(values, "timestamps-vary"),
        hasMetadata = bool(values, "has-metadata"),
        noNormal = bool(values, "no-normal"),
        noTimestamp = bool(values, "no-timestamp"),
        wrongTimestamp = bool(values, "wrong-timestamp"),
        wrongDay = bool(values, "wrong-day"),
        wrong = bool(values, "wrong")
      )
    ),
    Command(
      name = "repair",
      help = "repair picture names, raw files, timestamps, or day folders",
      positionals = List(Required("path", path = true)),
      flags = repairFlags,
      build = (values, args) => RepairArgs(
        path = args.head,
        names = bool(values, "names"),
        raw = bool(values, "raw"),
        timestamps = bool(values, "timestamps"),
        days = bool(values, "days"),
        dryRun = bool(values, "dry-run")
      )
    ),
    Command(
      name = "set-metadata",
      help = "set metadata property",
      positionals = List(Required("property"), Required("picture", path = true)),
      flags = Nil,
      build = (_, args) => SetMetadataArgs(args.head, args.drop(1).head)
    ),
    Command(
      name = "albums",
      help = "list album claims",
      positionals = List(Optional("path", path = true)),
      flags = Nil,
      build = (_, args) => Albums(args.headOption)
    ),
    Command(
      name = "publish",
      help = "publish albums to Google Photos",
      positionals = List(Optional("path", path = true)),
      flags = List(
        Flag("recursive", None, BooleanKind, "required for a year or the archive root"),
        Flag("dry-run", None, BooleanKind, "print the local plan and do not call Google")
      ),
      build = (values, args) => PublishArgs(
        path = args.headOption,
        recursive = bool(values, "recursive"),
        dryRun = bool(values, "dry-run")
      )
    ),
    Command(
      name = "sync",
      help = "sync album removals into local except lists",
      positionals = List(Optional("path", path = true)),
      flags = List(
        Flag("recursive", None, BooleanKind, "required for a year or the archive root"),
        Flag("dry-run", None, BooleanKind, "read Google, print except and remove lines, and do not write")
      ),
      build = (values, args) => SyncArgs(
        path = args.headOption,
        recursive = bool(values, "recursive"),
        dryRun = bool(values, "dry-run")
      )
    ),
    Command(
      name = "login",
      help = "log in to Google Photos",
      positionals = Nil,
      flags = Nil,
      build = (_, _) => LoginArgs
    ),
    Command(
      name = "status",
      help = "show Google Photos login status",
      positionals = Nil,
      flags = Nil,
      build = (_, _) => StatusArgs
    ),
    Command(
      name = "logout",
      help = "log out of Google Photos",
      positionals = Nil,
      flags = Nil,
      build = (_, _) => LogoutArgs
    )
  )

  private val albumIdFlag: Flag = Flag("id", None, TextKind, "catalog album id")

  private val agentCommands: List[Command] = List(
    Command(
      name = "stems",
      help = "list one day's grouped stems",
      positionals = List(Required("day", path = true)),
      flags = Nil,
      build = (_, args) => AgentStemsArgs(args.head)
    ),
    Command(
      name = "catalog",
      help = "list catalog albums",
      positionals = Nil,
      flags = Nil,
      build = (_, _) => AgentCatalogArgs
    ),
    Command(
      name = "items",
      help = "list catalog items for one album",
      positionals = List(Required("album")),
      flags = List(albumIdFlag),
      build = (values, args) => AgentItemsArgs(values.texts.get("id"), args.head)
    ),
    Command(
      name = "match",
      help = "match catalog items to local stems",
      positionals = List(Required("album")),
      flags = List(albumIdFlag),
      build = (values, args) => AgentMatchArgs(values.texts.get("id"), args.head)
    ),
    Command(
      name = "include",
      help = "compress include lines",
      positionals = List(Rest("year/month/day/stem")),
      flags = List(Flag("span", None, TextKind, "title span; absent means a curated album")),
      build = (values, args) => AgentIncludeArgs(values.texts.get("span"), args)
    ),
    Command(
      name = "album-file",
      help = "write an album file",
      positionals = List(Required("album")),
      flags = List(
        albumIdFlag,
        Flag("dry-run", None, BooleanKind, "print the album file and write nothing"),
        Flag("replace", None, BooleanKind, "overwrite an existing album file"),
        Flag("google-title", None, TextKind, "exact google-title"),
        Flag("no-request", None, BooleanKind, "do not append rename-album")
      ),
      build = (values, args) => AgentAlbumFileArgs(
        id = values.texts.get("id"),
        dryRun = bool(values, "dry-run"),
        replace = bool(values, "replace"),
        googleTitle = values.texts.get("google-title"),
        noRequest = bool(values, "no-request"),
        album = args.head
      )
    ),
    Command(
      name = "requests",
      help = "list request rows",
      positionals = Nil,
      flags = Nil,
      build = (_, _) => AgentRequestsArgs
    ),
    Command(
      name = "results",
      help = "list result rows",
      positionals = Nil,
      flags = Nil,
      build = (_, _) => AgentResultsArgs
    )
  )

  private val requestIdFlag: Flag = Flag("id", None, TextKind, "request id")

  private def requestCommand(
    name: String,
    help: String,
    flags: List[Flag],
    build: Values => AgentRequestArgs
  ): Command =
    Command(name, help, Nil, flags, (values, _) => build(values))

  private val requestCommands: List[Command] = List(
    requestCommand(
      "rename-album",
      "rename an owned album",
      List(
        Flag("album", None, TextKind, "album id"),
        Flag("current", None, TextKind, "current title"),
        Flag("new", None, TextKind, "new title"),
        requestIdFlag
      ),
      values => AgentRequestArgs(
        "rename-album",
        values.texts.get("album"),
        None,
        values.texts.get("current"),
        values.texts.get("new"),
        values.texts.get("id")
      )
    ),
    requestCommand(
      "move-item",
      "move an item to another album",
      List(
        Flag("album", None, TextKind, "album id"),
        Flag("item", None, TextKind, "item key"),
        Flag("current", None, TextKind, "current filename"),
        Flag("new", None, TextKind, "destination album id"),
        requestIdFlag
      ),
      values => AgentRequestArgs(
        "move-item",
        values.texts.get("album"),
        values.texts.get("item"),
        values.texts.get("current"),
        values.texts.get("new"),
        values.texts.get("id")
      )
    ),
    requestCommand(
      "set-description",
      "set an item description",
      List(
        Flag("item", None, TextKind, "item key"),
        Flag("current", None, TextKind, "current filename"),
        Flag("new", None, TextKind, "new description"),
        requestIdFlag
      ),
      values => AgentRequestArgs(
        "set-description",
        None,
        values.texts.get("item"),
        values.texts.get("current"),
        values.texts.get("new"),
        values.texts.get("id")
      )
    ),
    requestCommand(
      "set-date",
      "set an item date",
      List(
        Flag("item", None, TextKind, "item key"),
        Flag("current", None, TextKind, "current filename"),
        Flag("new", None, TextKind, "new local date"),
        requestIdFlag
      ),
      values => AgentRequestArgs(
        "set-date",
        None,
        values.texts.get("item"),
        values.texts.get("current"),
        values.texts.get("new"),
        values.texts.get("id")
      )
    ),
    requestCommand(
      "download-item",
      "download an item",
      List(
        Flag("album", None, TextKind, "album id"),
        Flag("item", None, TextKind, "item key"),
        Flag("current", None, TextKind, "current filename"),
        requestIdFlag
      ),
      values => AgentRequestArgs(
        "download-item",
        values.texts.get("album"),
        values.texts.get("item"),
        values.texts.get("current"),
        None,
        values.texts.get("id")
      )
    ),
    requestCommand(
      "delete-item",
      "delete an item",
      List(
        Flag("album", None, TextKind, "album id"),
        Flag("item", None, TextKind, "item key"),
        Flag("current", None, TextKind, "current filename"),
        requestIdFlag
      ),
      values => AgentRequestArgs(
        "delete-item",
        values.texts.get("album"),
        values.texts.get("item"),
        values.texts.get("current"),
        None,
        values.texts.get("id")
      )
    )
  )

  private val helpCommand: Command = Command(
    name = "help",
    help = "print command help",
    positionals = List(Optional("command")),
    flags = Nil,
    build = (_, _) => throw IllegalStateException("help is printed by the front")
  )

  private val cdCommand: Command = Command(
    name = "cd",
    help = "change the cursor; only available in the shell",
    positionals = List(Optional("directory")),
    flags = Nil,
    build = (_, args) => CdArgs(args.headOption)
  )

  private val pwdCommand: Command = Command(
    name = "pwd",
    help = "print the cursor; only available in the shell",
    positionals = Nil,
    flags = Nil,
    build = (_, _) => PwdArgs
  )

  private val exitCommand: Command = Command(
    name = "exit",
    help = "leave the session; only available in the shell",
    positionals = Nil,
    flags = Nil,
    build = (_, _) => ExitArgs
  )

  private val registry: List[Command] =
    commands ++ List(helpCommand, cdCommand, pwdCommand, exitCommand)

  private val shellLine: String = "shell  start a session"

  private val serveLine: String = "serve  listen on the per-user Unix socket"

  private val agentLine: String = "agent  query the catalog and the archive"

  private val requestLine: String = "request  append one request row"

  private def cursorOnly(session: Session, name: String): Option[Result] = session.style match
    case PathStyle.Cursor =>
      None
    case PathStyle.OneShot | PathStyle.Serve =>
      Some(Result.Usage(s"$name is only available in the shell"))

  private final case class Cd(path: Option[String]) extends Operation:
    override def execute(session: Session): (Session, Result) =
      cursorOnly(session, "cd") match
        case Some(result) =>
          (session, result)
        case None =>
          try
            val target: Path = path.fold(session.root)(session.parse)
            target match
              case directory: Directory if directory.directory.isDirectory =>
                (session.copy(cursor = directory), Result.Ok("", Json.Null))
              case _: Directory =>
                (session, Result.Usage("no such directory"))
              case _ =>
                (session, Result.Usage("not a directory"))
          catch
            case error: IllegalArgumentException =>
              (session, Result.Usage(Operation.messageOf(error)))
            case _: NoSuchElementException =>
              (session, Result.Usage("no such directory"))

  private object Pwd extends Operation:
    override def execute(session: Session): (Session, Result) =
      cursorOnly(session, "pwd") match
        case Some(result) =>
          (session, result)
        case None =>
          (session, Result.Ok(session.pwdLine, Json.Null))

  private object Exit extends Operation:
    override def execute(session: Session): (Session, Result) =
      cursorOnly(session, "exit") match
        case Some(result) =>
          (session, result)
        case None =>
          (session, Result.Ok("", Json.Null))
