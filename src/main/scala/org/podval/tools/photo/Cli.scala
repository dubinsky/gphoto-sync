package org.podval.tools.photo

import zio.ZIO
import zio.cli.{Args, CliApp, Command, HelpDoc, Options, ZIOCliDefault}
import zio.cli.HelpDoc.Span.text

object Cli extends ZIOCliDefault:
  private sealed trait Operation
  private final case class ListDirectories(path: Path, isLong: Boolean) extends Operation
  private final case class ListPictures(path: Path, filter: Filter, isLong: Boolean) extends Operation
  private final case class Fix(path: Path, filter: Filter, dryRun: Boolean) extends Operation
  private final case class SetMetadata(picture: Picture, dryRun: Boolean, update: Metadata.Update) extends Operation

  // TODO config!
  private val rootPath: String = "/home/dub/Pictures/originals"
  private val root: Root = Root(rootPath)

  private val path: Args[Path] = Args.text("path").map(root.parse)

  private val long: Options[Boolean] = Options.boolean("long").alias("l")
  private val dryRun: Options[Boolean] = Options.boolean("dry-run").alias("d")

  private val filter: Options[(
    Option[String],
    Option[String],
    Boolean,
    Boolean,
    Boolean,
    Boolean,
    Boolean,
    Boolean,
    Option[String],
    Option[String],
    Boolean
  )] =
    Options.text("name").alias("n").optional ++
    Options.text("extension").alias("e").optional ++
    Options.boolean("normal-missing") ++
    Options.boolean("timestamp-missing") ++
    Options.boolean("setting-timestamp") ++
    Options.boolean("wrong-day") ++
    Options.boolean("needs-fixing") ++
    Options.boolean("has-metadata") ++
    Options.text("source").alias("s").optional ++
    Options.text("timestamp").alias("t").optional ++
    Options.boolean("bad-raw").alias("b")

  private val set: Options[(
    Option[String],
    Option[String],
    Boolean
  )] =
    Options.text("source").alias("s").optional ++
    Options.text("timestamp").alias("t").optional ++
    Options.boolean("bad-raw").alias("b")

  private val lsd: Command[ListDirectories] = Command(
    name = "lsd",
    options = long,
    args = Args.text("directory").map(root.parse)
  )
    .withHelp(HelpDoc.p("list directories"))
    .map((long, path) => ListDirectories(path, isLong = long))

  private val ls: Command[ListPictures] = Command(
    name = "ls",
    options = filter ++ long,
    args = path
  )
    .withHelp(HelpDoc.p("list pictures"))
    .map((filterAndLong, path) => ListPictures(path, filter = filterAndLong.init, isLong = filterAndLong.last))

  private val fix: Command[Fix] = Command(
    name = "fix",
    options = filter ++ dryRun,
    args = path
  )
    .withHelp(HelpDoc.p("fix picture issues"))
    .map((filterAndDryRun, path) => Fix(path, filter = filterAndDryRun.init, dryRun = filterAndDryRun.last))

  private val setMetadata: Command[SetMetadata] = Command(
    name = "set-metadata",
    options = set ++ dryRun,
    args = Args.text("picture").map(root.parsePicture)
  )
    .withHelp(HelpDoc.p("list directories"))
    .map((setAndDryRun, path) => SetMetadata(path, dryRun = setAndDryRun.last, update = setAndDryRun.init))

  override val cliApp: CliApp[Any, Any, Unit] = CliApp.make[Any, Any, Operation, Unit](
    name = "Photo Archive Tool",
    version = "0.0.1",
    summary = text("manipulates photo archive"),
    command = Command("photo").subcommands(
      lsd,
      ls,
      fix,
      setMetadata
    )
  ) {
    case ListDirectories(path, isLong) =>
      ZIO.succeed(path.lsd(isLong))

    case ListPictures(path, filter, isLong) =>
      ZIO.succeed(path.forEachPicture(filter, _.list(filter, isLong)))

    case Fix(path, filter, dryRun) =>
      ZIO.succeed(path.forEachPicture(filter, _.fix(dryRun)))

    case SetMetadata(picture, dryRun, update) =>
      ZIO.succeed(picture.updateMetadata(update, dryRun))
  }
