package org.podval.tools.photo

import scala.annotation.tailrec

object Completion:
  private[photo] def candidates(session: Session, words: List[String], wordIndex: Int): List[String] =
    if words.isEmpty then Cli.shellCommandNames
    else if wordIndex < 0 || wordIndex >= words.length then Nil
    else
      val current: String = words(wordIndex)
      val before: List[String] = words.take(wordIndex)
      before match
        case Nil =>
          if current.startsWith("-") then Nil else prefixed(Cli.shellCommandNames, current)
        case "item" :: Nil =>
          if current.startsWith("-") then Nil else prefixed(Cli.itemCommands.map(_.name), current)
        case "album" :: Nil =>
          if current.startsWith("-") then Nil else prefixed(Cli.albumCommands.map(_.name), current)
        case "agent" :: Nil =>
          if current.startsWith("-") then Nil else prefixed(Cli.agentCommands.map(_.name), current)
        case "card" :: Nil =>
          if current.startsWith("-") then flagCandidates(Cli.discoverCommand, Set.empty, current)
          else prefixed(List("copy"), current)
        case "agent" :: "request" :: _ =>
          Nil
        case _ =>
          commandOf(before) match
            case None =>
              Nil
            case Some((command, args)) =>
              argumentCandidates(session, command, args, current)

  private def commandOf(before: List[String]): Option[(Cli.Command, List[String])] = before match
    case "item" :: verb :: args =>
      Cli.itemCommands.find(_.name == verb).map(command => (command, args))
    case "album" :: verb :: args =>
      Cli.albumCommands.find(_.name == verb).map(command => (command, args))
    case "agent" :: "request" :: _ =>
      None
    case "agent" :: verb :: args =>
      Cli.agentCommands.find(_.name == verb).map(command => (command, args))
    case "card" :: "copy" :: args =>
      Some((Cli.copyCommand, args))
    case "card" :: arg :: _ if !arg.startsWith("-") =>
      None
    case "card" :: args =>
      Some((Cli.discoverCommand, args))
    case name :: args =>
      Cli.registry.find(_.name == name).map(command => (command, args))
    case Nil =>
      None

  private def argumentCandidates(
    session: Session,
    command: Cli.Command,
    args: List[String],
    current: String
  ): List[String] =
    val (set, count, eating) = consume(command, args)
    if current.startsWith("-") then flagCandidates(command, set, current)
    else if eating then Nil
    else positionalAt(command.positionals, count) match
      case Some(positional) => positionalCandidates(session, positional, current)
      case None => Nil

  private def flagCandidates(command: Cli.Command, set: Set[String], prefix: String): List[String] =
    val names: List[String] = command.flags.flatMap: flag =>
      val repeatable: Boolean = flag.kind match
        case Cli.RepeatKind => true
        case Cli.BooleanKind | Cli.TextKind => false
      if set.contains(flag.name) && !repeatable then Nil
      else
        val long: String = s"--${flag.name}"
        flag.alias.fold(List(long))(alias => List(long, s"-$alias"))
    prefixed(names, prefix)

  // eating is true when the last token is a text flag that still needs its value.
  private def consume(command: Cli.Command, args: List[String]): (Set[String], Int, Boolean) =
    @tailrec
    def loop(rest: List[String], set: Set[String], count: Int): (Set[String], Int, Boolean) =
      rest match
        case Nil =>
          (set, count, false)
        case "--" :: more =>
          (set, count + more.length, false)
        case token :: more =>
          Cli.splitFlag(token) match
            case Some((isLong, name, attached)) =>
              Cli.findFlag(command.flags, isLong = isLong, name = name) match
                case Some(flag) =>
                  val takesNext: Boolean = flag.kind match
                    case Cli.TextKind | Cli.RepeatKind => attached.isEmpty
                    case Cli.BooleanKind => false
                  if !takesNext then loop(more, set + flag.name, count)
                  else more match
                    case Nil =>
                      (set + flag.name, count, true)
                    case _ :: tail =>
                      loop(tail, set + flag.name, count)
                case None =>
                  loop(more, set, count)
            case None =>
              loop(more, set, count + 1)
    loop(args, Set.empty, 0)

  private def positionalAt(positionals: List[Cli.Positional], index: Int): Option[Cli.Positional] =
    positionals match
      case Nil =>
        None
      case (rest: Cli.Rest) :: Nil =>
        Some(rest)
      case _ :: tail if index > 0 =>
        positionalAt(tail, index - 1)
      case head :: _ =>
        Some(head)

  private def positionalCandidates(session: Session, positional: Cli.Positional, prefix: String): List[String] =
    if isProperty(positional) then propertyCandidates(prefix)
    else if isTitle(positional) then titleCandidates(session, prefix)
    else if isDatePath(positional) then dateCandidates(session, prefix)
    else Nil

  private def isProperty(positional: Cli.Positional): Boolean = positional match
    case Cli.Required("property", _) => true
    case _ => false

  private def isTitle(positional: Cli.Positional): Boolean = positional match
    case Cli.Required(name, _) => titleName(name)
    case Cli.Optional(name, _) => titleName(name)
    case _ => false

  private def titleName(name: String): Boolean =
    name == "title" || name == "old" || name == "new" || name == "first" || name == "second"

  private def isDatePath(positional: Cli.Positional): Boolean = positional match
    case Cli.Required("directory", _) | Cli.Optional("directory", _) =>
      true
    case Cli.Rest(name, path, _) =>
      path || name.startsWith("year/month/day")
    case other =>
      other.path

  private def propertyCandidates(prefix: String): List[String] =
    prefixed(Property.all.map(property => s"${property.name}="), prefix)

  private def titleCandidates(session: Session, prefix: String): List[String] =
    val titles: Seq[String] =
      try Archive.scan(session.root.configuration.archive).albums.map(_.album.title)
      catch case _: Exception => Seq.empty
    titles.iterator.filter(title => title.startsWith(prefix) || titleToken(title).startsWith(prefix)).map(titleToken).toList

  // Inside quotes splitLine only treats \\ and \" as escapes. Anything else would change the title.
  private def titleToken(title: String): String =
    if title.exists(ch => ch.isWhitespace || ch == '"' || ch == '\\') then
      val body: String = title.flatMap:
        case '\\' => "\\\\"
        case '"' => "\\\""
        case ch => ch.toString
      s"\"$body\""
    else title

  private def dateCandidates(session: Session, token: String): List[String] =
    val slash: Int = token.lastIndexOf('/')
    val parent: String =
      if slash < 0 then ""
      else if slash == 0 then "/"
      else token.take(slash)
    val names: Seq[String] =
      try childNames(session.parse(parent))
      catch case _: Exception => Seq.empty
    val combined: Seq[String] =
      if parent.isEmpty then names
      else if parent == "/" then names.map(name => s"/$name")
      else names.map(name => s"$parent/$name")
    prefixed(combined, token)

  private def childNames(path: Path): Seq[String] = path match
    case _: UploadDirectory =>
      Nil
    case root: Root =>
      Year.lsd(root).map(_.name)
    case day: Day =>
      Pictures.grouped(Files.listFiles(day.directory)).map(_._1).filter(stem => stem.nonEmpty && stem.head.isDigit)
    case directory: Directory =>
      directory.lsd.map(_.name)
    case _ =>
      Nil

  private def prefixed(options: Seq[String], prefix: String): List[String] =
    options.iterator.filter(_.startsWith(prefix)).toList
