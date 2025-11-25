package org.podval.tools.photo

import zio.ZIO
import zio.cli.{Args, CliApp, Command, HelpDoc, Options, ZIOCliDefault}
import zio.cli.HelpDoc.Span.text

object Cli extends ZIOCliDefault:
  private val long: Options[Boolean] = Options.boolean("long").alias("l")
  
  private val lsd: Command[ListDirectories] = Command(
    name = "lsd",
    options = long,
    args = Args.text("directory").map(Root.get.parse)
  )
    .withHelp(HelpDoc.p("list directories"))
    .map((long, path) => ListDirectories(path, isLong = long))

  private val setMetadata: Command[SetMetadata] = Command(
    name = "set-metadata",
    args = Args.text("property") ++ Args.text("picture").map(Root.get.parsePicture)
  )
    .withHelp(HelpDoc.p("set metadata property"))
    .map((property, picture) => SetMetadata(picture, Metadata.Property(property)))

  private val ls: Command[ListPictures] = Command(
    name = "ls",
    args = Args.text("path").map(Root.get.parse),
    options =
      long ++
      Options.boolean("fix") ++
      Options.text("name-contains").alias("n").optional ++
      Options.text("has-extension").alias("e").optional ++
      Options.text("property").optional ++
      Options.text("timestamp-from-name-format").optional ++
      Options.boolean("timestamp-from-name") ++
      Options.boolean("timestamp-from-name-only") ++
      Options.boolean("timestamps-vary") ++
      Options.boolean("has-metadata") ++
      Options.boolean("no-normal") ++
      Options.boolean("no-timestamp") ++
      Options.boolean("wrong-timestamp") ++
      Options.boolean("wrong-day") ++
      Options.boolean("wrong")
  )
    .withHelp(HelpDoc.p("list pictures"))
    .map { case (
      (
        long,
        fix,
        nameContains,
        hasExtension,
        property,
        timestampFromNameFormat,
        timestampFromName,
        timestampFromNameOnly,
        timestampsVary,
        hasMetadata,
        noNormal,
        noTimestamp,
        wrongTimestamp,
        wrongDay,
        wrong
      ),
      path
    ) =>
      ListPictures(
        path,
        long,
        fix,
        nameContains,
        hasExtension,
        property.map(Metadata.Property.apply),
        timestampFromNameFormat,
        timestampFromName,
        timestampFromNameOnly,
        timestampsVary,
        hasMetadata,
        noNormal,
        noTimestamp,
        wrongTimestamp,
        wrongDay,
        wrong
      )
    }

  override val cliApp: CliApp[Any, Any, Unit] = CliApp.make[Any, Any, Operation, Unit](
    name = "Photo Archive Tool",
    version = "0.0.1",
    summary = text("manipulates photo archive"),
    command = Command("photo").subcommands(
      lsd,
      ls,
      setMetadata
    )
  )(
    (operation: Operation) => ZIO.succeed(operation.execute())
  )
