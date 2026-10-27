package org.podval.tools.photo

object Cli:
  private[photo] sealed trait Outcome
  private[photo] final case class Help(text: String) extends Outcome
  private[photo] final case class Usage(message: String) extends Outcome

  private[photo] sealed trait Parsed
  private[photo] final case class Lsd(directory: String, long: Boolean) extends Parsed
  private[photo] final case class Ls(
    path: String,
    long: Boolean,
    fix: Boolean,
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
  private[photo] case object LoginArgs extends Parsed
  private[photo] case object StatusArgs extends Parsed
  private[photo] case object LogoutArgs extends Parsed

  private[photo] final case class Run(parsed: Parsed) extends Outcome

  def main(args: Array[String]): Unit =
    parse(args.toList) match
      case Help(text) =>
        println(text)
      case Usage(message) =>
        System.err.println(usageLine(message))
        sys.exit(1)
      case Run(parsed) =>
        toOperation(parsed).execute()

  private[photo] def parse(args: List[String]): Outcome = args match
    case Nil =>
      Usage("missing command")
    case ("--help" | "-h") :: _ =>
      Help(topHelp)
    case name :: rest =>
      commands.find(_.name == name) match
        case None => Usage(s"unknown command '$name'")
        case Some(command) => parseCommand(command, rest)

  private[photo] def toOperation(parsed: Parsed): Operation = parsed match
    case Lsd(directory, long) =>
      ListDirectories(Root.get.parse(directory), isLong = long)
    case ls: Ls =>
      ListPictures(
        Root.get.parse(ls.path),
        ls.long,
        ls.fix,
        ls.nameContains,
        ls.hasExtension,
        ls.property.map(parsePropertyAndValue),
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
      SetMetadata(Root.get.parsePicture(picture), parsePropertyAndValue(property))
    case Albums(path) =>
      ListAlbums(path.map(Root.get.parse))
    case PublishArgs(path, recursive, dryRun) =>
      Publish(path.map(Root.get.parse), recursive = recursive, dryRun = dryRun)
    case SyncArgs(path, recursive, dryRun) =>
      Sync(path.map(Root.get.parse), recursive = recursive, dryRun = dryRun)
    case LoginArgs => Login
    case StatusArgs => Status
    case LogoutArgs => Logout

  private[photo] def usageLine(message: String): String = s"Photo Archive Tool: $message"

  private[photo] def parsePropertyAndValue(string: String): Property.AndValue[?] =
    val (name: String, value: Option[String]) = Files.splitOnLast(string, '=')
    Property.AndValue(Property.forName(name), value)

  private def parseCommand(command: Command, args: List[String]): Outcome =
    readFlags(command, args, Values()) match
      case Left(outcome) => outcome
      case Right((values, positionals)) =>
        checkPositionals(command.positionals, positionals) match
          case Some(message) => Usage(message)
          case None => Run(command.build(values, positionals))

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
      case (Required(name) :: _, Nil) => Some(s"missing $name")
      case (Optional(_) :: _, Nil) => None
      case (_ :: restExpected, _ :: restArgs) => checkPositionals(restExpected, restArgs)

  private def bool(values: Values, name: String): Boolean = values.bools.getOrElse(name, false)

  private def topHelp: String =
    val header: List[String] = List(
      "Photo Archive Tool 0.0.1",
      "manipulates photo archive",
      ""
    )
    (header ++ commands.map(command => s"${command.name}  ${command.help}")).mkString("\n")

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
    case Required(name) => s"$name  required"
    case Optional(name) => s"$name  optional"

  private def describeFlag(flag: Flag): String =
    val names: String = flag.alias match
      case Some(alias) => s"--${flag.name}, -$alias"
      case None => s"--${flag.name}"
    val kind: String = flag.kind match
      case BooleanKind => "boolean"
      case TextKind => "text"
    s"$names  $kind"

  private sealed trait Positional
  private final case class Required(name: String) extends Positional
  private final case class Optional(name: String) extends Positional

  private sealed trait FlagKind
  private case object BooleanKind extends FlagKind
  private case object TextKind extends FlagKind

  private final case class Flag(name: String, alias: Option[String], kind: FlagKind)

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

  private val longFlag: Flag = Flag("long", Some("l"), BooleanKind)
  private val recursiveFlag: Flag = Flag("recursive", None, BooleanKind)
  private val dryRunFlag: Flag = Flag("dry-run", None, BooleanKind)

  private val lsFlags: List[Flag] = List(
    longFlag,
    Flag("fix", None, BooleanKind),
    Flag("name-contains", Some("n"), TextKind),
    Flag("has-extension", Some("e"), TextKind),
    Flag("property", None, TextKind),
    Flag("timestamp-from-name-format", None, TextKind),
    Flag("timestamp-from-name", None, BooleanKind),
    Flag("timestamp-from-name-only", None, BooleanKind),
    Flag("timestamps-vary", None, BooleanKind),
    Flag("has-metadata", None, BooleanKind),
    Flag("no-normal", None, BooleanKind),
    Flag("no-timestamp", None, BooleanKind),
    Flag("wrong-timestamp", None, BooleanKind),
    Flag("wrong-day", None, BooleanKind),
    Flag("wrong", None, BooleanKind)
  )

  private val commands: List[Command] = List(
    Command(
      name = "lsd",
      help = "list directories",
      positionals = List(Required("directory")),
      flags = List(longFlag),
      build = (values, args) => Lsd(args.head, bool(values, "long"))
    ),
    Command(
      name = "ls",
      help = "list pictures",
      positionals = List(Required("path")),
      flags = lsFlags,
      build = (values, args) => Ls(
        path = args.head,
        long = bool(values, "long"),
        fix = bool(values, "fix"),
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
      name = "set-metadata",
      help = "set metadata property",
      positionals = List(Required("property"), Required("picture")),
      flags = Nil,
      build = (_, args) => SetMetadataArgs(args.head, args.drop(1).head)
    ),
    Command(
      name = "albums",
      help = "list album claims",
      positionals = List(Optional("path")),
      flags = Nil,
      build = (_, args) => Albums(args.headOption)
    ),
    Command(
      name = "publish",
      help = "publish albums to Google Photos",
      positionals = List(Optional("path")),
      flags = List(recursiveFlag, dryRunFlag),
      build = (values, args) => PublishArgs(
        path = args.headOption,
        recursive = bool(values, "recursive"),
        dryRun = bool(values, "dry-run")
      )
    ),
    Command(
      name = "sync",
      help = "sync album removals into local exclude lists",
      positionals = List(Optional("path")),
      flags = List(recursiveFlag, dryRunFlag),
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
