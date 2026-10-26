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

  private val login: Command[Login.type] = Command("login")
    .withHelp(HelpDoc.p("log in to Google Photos"))
    .as(Login)

  private val status: Command[Status.type] = Command("status")
    .withHelp(HelpDoc.p("show Google Photos login status"))
    .as(Status)

  private val logout: Command[Logout.type] = Command("logout")
    .withHelp(HelpDoc.p("log out of Google Photos"))
    .as(Logout)

  private val setMetadata: Command[SetMetadata] = Command(
    name = "set-metadata",
    args = Args.text("property") ++ Args.text("picture").map(Root.get.parsePicture)
  )
    .withHelp(HelpDoc.p("set metadata property"))
    .map((property, picture) => SetMetadata(picture, parsePropertyAndValue(property)))

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
        property.map(parsePropertyAndValue),
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

  private val albums: Command[ListAlbums] = Command(
    name = "albums",
    args = Args.text("path").atMost(1).map(_.headOption.map(Root.get.parse))
  )
    .withHelp(HelpDoc.p("list album claims"))
    .map(ListAlbums(_))

  private val publish: Command[Publish] = Command(
    name = "publish",
    args = Args.text("path").atMost(1).map(_.headOption.map(Root.get.parse)),
    options = Options.boolean("recursive") ++ Options.boolean("dry-run")
  )
    .withHelp(HelpDoc.p("publish albums to Google Photos"))
    .map { case ((recursive, dryRun), path) =>
      Publish(path, recursive = recursive, dryRun = dryRun)
    }

  private val sync: Command[Sync] = Command(
    name = "sync",
    args = Args.text("path").atMost(1).map(_.headOption.map(Root.get.parse)),
    options = Options.boolean("recursive") ++ Options.boolean("dry-run")
  )
    .withHelp(HelpDoc.p("sync album removals into local exclude lists"))
    .map { case ((recursive, dryRun), path) =>
      Sync(path, recursive = recursive, dryRun = dryRun)
    }

  override val cliApp: CliApp[Any, Any, Unit] = CliApp.make[Any, Any, Operation, Unit](
    name = "Photo Archive Tool",
    version = "0.0.1",
    summary = text("manipulates photo archive"),
    command = Command("photo").subcommands(
      lsd,
      ls,
      setMetadata,
      albums,
      publish,
      sync,
      login,
      status,
      logout
    )
  )(
    (operation: Operation) => ZIO.succeed(operation.execute())
  )
  
  private def parsePropertyAndValue(string: String): Property.AndValue[?] =
    val (name: String, value: Option[String]) = Files.splitOnLast(string, '=')
    Property.AndValue(Property.forName(name), value)

