package org.podval.tools.photo

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.ListHasAsScala
import org.jline.reader.{
  Candidate,
  Completer,
  CompletingParsedLine,
  EndOfFileException,
  LineReader,
  LineReaderBuilder,
  ParsedLine,
  Parser,
  UserInterruptException
}
import org.jline.reader.impl.DefaultParser
import org.jline.terminal.TerminalBuilder

object Shell:
  // doComplete deletes the raw word, opening quote included, then writes this escape.
  // titleToken is that whole replacement. DefaultParser.escape would quote the token again.
  private[photo] val shellParser: Parser = new DefaultParser:
    override def parse(line: String, cursor: Int, context: Parser.ParseContext): ParsedLine =
      super.parse(line, cursor, context) match
        case completing: CompletingParsedLine =>
          new CompletingParsedLine:
            override def escape(candidate: CharSequence, complete: Boolean): CharSequence =
              // complete is DefaultParser's request to append a closing quote.
              // titleToken already closed one, and this string replaces the raw word.
              Completion.titleToken(candidate.toString)
            override def rawWordCursor(): Int = completing.rawWordCursor()
            override def rawWordLength(): Int = completing.rawWordLength()
            override def word(): String = completing.word()
            override def wordCursor(): Int = completing.wordCursor()
            override def wordIndex(): Int = completing.wordIndex()
            override def words(): java.util.List[String] = completing.words()
            override def line(): String = completing.line()
            override def cursor(): Int = completing.cursor()
        case other =>
          other

  private[photo] def shell(
    session: Session,
    lines: Iterator[String],
    run: (Session, Cli.Parsed) => (Session, Result) = (session, parsed) => Cli.toOperation(parsed).execute(session),
    exchange: Option[(Session, List[String]) => Result] = None,
    interactive: Boolean = false
  ): Int =
    // Assigned before each readLine. Tab runs inside that read, so cd is already visible.
    var live: Session = session
    val reader: Option[LineReader] =
      if !interactive then None
      else
        try
          val terminal = TerminalBuilder.builder().system(true).jna(true).build()
          val sessionNow: () => Session = () => live
          val completer: Completer = (_, line, found) =>
            val words: List[String] = line.words.asScala.toList
            Completion
              .candidates(sessionNow(), words, line.wordIndex)
              .foreach: text =>
                found.add(new Candidate(text))
          Some(
            LineReaderBuilder.builder().terminal(terminal).parser(shellParser).completer(completer).build()
          )
        catch
          case _: Exception =>
            None
    try
      @tailrec
      def loop(current: Session): Int =
        live = current
        val line: Option[String] = reader match
          case None =>
            Console.err.print(current.prompt)
            Console.err.flush()
            if lines.hasNext then Some(lines.next()) else None
          case Some(lineReader) =>
            try Option(lineReader.readLine(current.prompt))
            catch
              case _: EndOfFileException     => None
              case _: UserInterruptException => Some("")
        line match
          case None =>
            0
          case Some(text) =>
            step(current, text, run, exchange) match
              case Cli.Step.Stop           => 0
              case Cli.Step.Continue(next) => loop(next)
      loop(session)
    finally
      reader.foreach: lineReader =>
        try lineReader.getTerminal.close()
        catch case _: java.io.IOException => ()

  private def step(
    session: Session,
    line: String,
    run: (Session, Cli.Parsed) => (Session, Result),
    exchange: Option[(Session, List[String]) => Result]
  ): Cli.Step =
    splitLine(line) match
      case Left(message) =>
        Console.err.println(Cli.redacted(Cli.usageLine(message)))
        Cli.Step.Continue(session)
      case Right(Nil) =>
        Cli.Step.Continue(session)
      case Right(args) =>
        Cli.parse(args, Cli.Front.Shell) match
          case Cli.Help(text) =>
            println(text)
            Cli.Step.Continue(session)
          case Cli.Usage(message) =>
            Console.err.println(Cli.redacted(Cli.usageLine(message)))
            Cli.Step.Continue(session)
          case Cli.ShellFront | Cli.ServeFront | Cli.ServeStop | Cli.McpFront =>
            Console.err.println(Cli.redacted(Cli.usageLine("already running")))
            Cli.Step.Continue(session)
          case Cli.Run(Cli.ExitArgs) =>
            Cli.Step.Stop
          case Cli.Run(parsed) if exchange.isDefined && !Cli.staysLocal(parsed) && !Cli.runsInCaller(parsed) =>
            remoteStep(session, args, exchange.get)
          case Cli.Run(parsed) =>
            val (returned, result) = run(session, parsed)
            show(session, returned, result, keepReturned = true)

  private def remoteStep(
    session: Session,
    args: List[String],
    exchange: (Session, List[String]) => Result
  ): Cli.Step =
    val argv: List[String] =
      try rewritePaths(session, args)
      catch
        case error: IllegalArgumentException =>
          Console.err.println(Cli.redacted(Cli.usageLine(Operation.messageOf(error))))
          return Cli.Step.Continue(session)
    show(session, session, exchange(session, argv), keepReturned = false)

  private def show(session: Session, returned: Session, result: Result, keepReturned: Boolean): Cli.Step =
    result match
      case Result.Ok(text, _) =>
        print(Cli.redacted(text))
        Cli.Step.Continue(if keepReturned then returned else session)
      case Result.Failed(text, _, message) if Cli.quietFailure(message) =>
        print(Cli.redacted(text))
        Cli.Step.Continue(if keepReturned then returned else session)
      case Result.Failed(text, _, message) =>
        print(Cli.redacted(text))
        Console.err.println(Cli.redacted(Cli.usageLine(message)))
        Cli.Step.Continue(if keepReturned then returned else session)
      case Result.Usage(message) =>
        Console.err.println(Cli.redacted(Cli.usageLine(message)))
        Cli.Step.Continue(session)

  // Path positionals are marked on the command. Flags stay as typed.
  private def rewritePaths(session: Session, args: List[String]): List[String] =
    locate(args) match
      case None =>
        args
      case Some((command, commandArgs, prefix)) =>
        Cli.readFlags(command, commandArgs, Cli.Values()) match
          case Left(_) =>
            args
          case Right((_, positionals)) =>
            val flags: List[String] = commandArgs.take(commandArgs.length - positionals.length)
            val rewrittenPositionals: List[String] =
              rewritePositionals(session, command.positionals, positionals)
            if prefix == List("ls") && positionals.isEmpty then prefix ++ flags ++ List(rootForm(session, "."))
            else prefix ++ flags ++ rewrittenPositionals

  private def locate(args: List[String]): Option[(Cli.Command, List[String], List[String])] = args match
    case "agent" :: name :: rest =>
      Cli.agentCommands.find(_.name == name).map(command => (command, rest, List("agent", name)))
    case "item" :: name :: rest =>
      Cli.itemCommands.find(_.name == name).map(command => (command, rest, List("item", name)))
    case "album" :: name :: rest =>
      Cli.albumCommands.find(_.name == name).map(command => (command, rest, List("album", name)))
    case name :: rest =>
      Cli.registry.find(_.name == name).map(command => (command, rest, List(name)))
    case Nil =>
      None

  private def rewritePositionals(session: Session, expected: List[Cli.Positional], args: List[String]): List[String] =
    (expected, args) match
      case (Nil, rest) =>
        rest
      case (Cli.Rest(_, path, _) :: _, rest) =>
        if path then rest.map(token => rootForm(session, token)) else rest
      case (_, Nil) =>
        Nil
      case (positional :: moreExpected, token :: moreArgs) =>
        val rewritten: String = if positional.path then rootForm(session, token) else token
        rewritten :: rewritePositionals(session, moreExpected, moreArgs)

  private def rootForm(session: Session, token: String): String =
    val text: String = session.parse(token).toString.stripPrefix("/")
    if text.isEmpty then "/" else text

  // Inside quotes only \\ and \" are escapes. Any other backslash stays in the token.
  private[photo] def splitLine(line: String): Either[String, List[String]] =
    collect(line, Nil)

  @tailrec
  private def collect(rest: String, acc: List[String]): Either[String, List[String]] =
    val left: String = rest.dropWhile(_.isWhitespace)
    if left.isEmpty then Right(acc.reverse)
    else
      readToken(left, "") match
        case Left(message)        => Left(message)
        case Right((token, more)) => collect(more, token :: acc)

  private def readToken(rest: String, acc: String): Either[String, (String, String)] =
    if rest.isEmpty || rest.head.isWhitespace then Right((acc, rest))
    else
      rest.head match
        case '\\' =>
          val more: String = rest.drop(1)
          if more.isEmpty then Left("trailing backslash")
          else readToken(more.drop(1), acc + more.take(1))
        case '"' =>
          readQuoted(rest.drop(1), acc) match
            case Left(message)         => Left(message)
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
            case '\\'  => "\\"
            case '"'   => "\""
            case other => s"\\$other"
          readQuoted(escaped.drop(1), soFar + taken)

  private def cursorOnly(session: Session, name: String): Option[Result] = session.style match
    case PathStyle.Cursor =>
      None
    case PathStyle.OneShot | PathStyle.Serve =>
      Some(Result.Usage(s"$name is only available in the shell"))

  private[photo] final case class Cd(path: Option[String]) extends Operation:
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

  private[photo] object Pwd extends Operation:
    override def execute(session: Session): (Session, Result) =
      cursorOnly(session, "pwd") match
        case Some(result) =>
          (session, result)
        case None =>
          (session, Result.Ok(session.pwdLine, Json.Null))

  private[photo] object Exit extends Operation:
    override def execute(session: Session): (Session, Result) =
      cursorOnly(session, "exit") match
        case Some(result) =>
          (session, result)
        case None =>
          (session, Result.Ok("", Json.Null))
