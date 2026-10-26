package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.time.LocalDate

class CliTest extends AnyFlatSpec, Matchers:
  // strictEquality does not relate a sealed parent to its cases.
  @targetName("outcomeCanEqual")
  private given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

  @targetName("operationCanEqual")
  private given [A <: Operation, B <: Operation] => CanEqual[A, B] = CanEqual.derived

  "parse" should "accept every subcommand" in :
    Cli.parse(List("lsd", "2009")) shouldBe Cli.Run(Cli.Lsd("2009", long = false))
    Cli.parse(List("lsd", "--long", "2009")) shouldBe Cli.Run(Cli.Lsd("2009", long = true))
    Cli.parse(List("lsd", "-l", "2009")) shouldBe Cli.Run(Cli.Lsd("2009", long = true))
    Cli.parse(List("lsd", "--long", "true")) shouldBe Cli.Run(Cli.Lsd("true", long = true))
    Cli.parse(List("lsd", "-l=false", "2009")) shouldBe Cli.Run(Cli.Lsd("2009", long = false))
    Cli.parse(List("lsd", "--long=false", "2009")) shouldBe Cli.Run(Cli.Lsd("2009", long = false))
    Cli.parse(List("lsd", "--long=TRUE", "2009")) shouldBe Cli.Run(Cli.Lsd("2009", long = true))

    Cli.parse(List("ls", "2009")) shouldBe ls("2009")
    Cli.parse(List("ls", "--long", "false")) shouldBe ls(path = "false", long = true)
    Cli.parse(List("ls", "--long=false", "2009")) shouldBe ls("2009", long = false)
    Cli.parse(List("ls", "--fix=false", "2009")) shouldBe ls("2009")
    Cli.parse(List(
      "ls",
      "--long",
      "--fix",
      "--name-contains", "mid",
      "--has-extension", "jpg",
      "--property", "bad-raw",
      "--timestamp-from-name-format", "yyyy",
      "--timestamp-from-name",
      "--timestamp-from-name-only",
      "--timestamps-vary",
      "--has-metadata",
      "--no-normal",
      "--no-timestamp",
      "--wrong-timestamp",
      "--wrong-day",
      "--wrong",
      "2009/03"
    )) shouldBe ls(
      path = "2009/03",
      long = true,
      fix = true,
      nameContains = Some("mid"),
      hasExtension = Some("jpg"),
      property = Some("bad-raw"),
      timestampFromNameFormat = Some("yyyy"),
      timestampFromName = true,
      timestampFromNameOnly = true,
      timestampsVary = true,
      hasMetadata = true,
      noNormal = true,
      noTimestamp = true,
      wrongTimestamp = true,
      wrongDay = true,
      wrong = true
    )
    Cli.parse(List("ls", "--name-contains=mid", "2009")) shouldBe ls("2009", nameContains = Some("mid"))
    Cli.parse(List("ls", "--name-contains=-hidden", "2009")) shouldBe ls("2009", nameContains = Some("-hidden"))
    Cli.parse(List("ls", "-n", "mid", "2009")) shouldBe ls("2009", nameContains = Some("mid"))
    Cli.parse(List("ls", "-n=mid", "2009")) shouldBe ls("2009", nameContains = Some("mid"))
    Cli.parse(List("ls", "-e", "jpg", "2009")) shouldBe ls("2009", hasExtension = Some("jpg"))
    Cli.parse(List("ls", "-e=jpg", "2009")) shouldBe ls("2009", hasExtension = Some("jpg"))
    Cli.parse(List("ls", "--has-extension=jpg", "2009")) shouldBe ls("2009", hasExtension = Some("jpg"))
    Cli.parse(List("ls", "--property=already-in-google=false", "2009")) shouldBe
      ls("2009", property = Some("already-in-google=false"))
    Cli.parse(List("ls", "--name-contains", "--", "-hidden", "2009")) shouldBe
      ls("2009", nameContains = Some("-hidden"))
    Cli.parse(List("ls", "--", "--fix")) shouldBe ls("--fix")

    Cli.parse(List("set-metadata", "bad-raw", "2009/03/10/2412.jpg")) shouldBe
      Cli.Run(Cli.SetMetadataArgs("bad-raw", "2009/03/10/2412.jpg"))
    Cli.parse(List("set-metadata", "bad-raw=true", "2009/03/10/2412.jpg")) shouldBe
      Cli.Run(Cli.SetMetadataArgs("bad-raw=true", "2009/03/10/2412.jpg"))

    Cli.parse(List("albums")) shouldBe Cli.Run(Cli.Albums(None))
    Cli.parse(List("albums", "2009")) shouldBe Cli.Run(Cli.Albums(Some("2009")))

    Cli.parse(List("publish")) shouldBe publish(None, recursive = false, dryRun = false)
    Cli.parse(List("publish", "2009")) shouldBe publish(Some("2009"), recursive = false, dryRun = false)
    Cli.parse(List("publish", "--recursive", "--dry-run")) shouldBe
      publish(None, recursive = true, dryRun = true)
    Cli.parse(List("publish", "--recursive", "--dry-run", "2009")) shouldBe
      publish(Some("2009"), recursive = true, dryRun = true)
    Cli.parse(List("publish", "--dry-run=false")) shouldBe publish(None, recursive = false, dryRun = false)
    Cli.parse(List("publish", "--recursive", "--dry-run=false", "2009")) shouldBe
      publish(Some("2009"), recursive = true, dryRun = false)

    Cli.parse(List("sync")) shouldBe sync(None, recursive = false, dryRun = false)
    Cli.parse(List("sync", "2009")) shouldBe sync(Some("2009"), recursive = false, dryRun = false)
    Cli.parse(List("sync", "--recursive", "--dry-run")) shouldBe sync(None, recursive = true, dryRun = true)
    Cli.parse(List("sync", "--recursive", "--dry-run", "2009")) shouldBe
      sync(Some("2009"), recursive = true, dryRun = true)
    Cli.parse(List("sync", "--dry-run=false")) shouldBe sync(None, recursive = false, dryRun = false)
    Cli.parse(List("sync", "--recursive", "--dry-run=FALSE", "2009")) shouldBe
      sync(Some("2009"), recursive = true, dryRun = false)

    Cli.parse(List("login")) shouldBe Cli.Run(Cli.LoginArgs)
    Cli.parse(List("status")) shouldBe Cli.Run(Cli.StatusArgs)
    Cli.parse(List("logout")) shouldBe Cli.Run(Cli.LogoutArgs)

  it should "print help and not an operation" in :
    val top: String = helpOf("--help")
    top shouldBe
      """Photo Archive Tool 0.0.1
        |manipulates photo archive
        |
        |lsd  list directories
        |ls  list pictures
        |set-metadata  set metadata property
        |albums  list album claims
        |publish  publish albums to Google Photos
        |sync  sync album removals into local exclude lists
        |login  log in to Google Photos
        |status  show Google Photos login status
        |logout  log out of Google Photos""".stripMargin
    helpOf("-h") shouldBe top
    helpOf("--help", "ls") shouldBe top

    helpOf("lsd", "--help") shouldBe
      """lsd  list directories
        |
        |Positionals:
        |directory  required
        |
        |Flags:
        |--long, -l  boolean""".stripMargin
    helpOf("lsd", "-h") shouldBe helpOf("lsd", "--help")

    helpOf("ls", "--help") shouldBe
      """ls  list pictures
        |
        |Positionals:
        |path  required
        |
        |Flags:
        |--long, -l  boolean
        |--fix  boolean
        |--name-contains, -n  text
        |--has-extension, -e  text
        |--property  text
        |--timestamp-from-name-format  text
        |--timestamp-from-name  boolean
        |--timestamp-from-name-only  boolean
        |--timestamps-vary  boolean
        |--has-metadata  boolean
        |--no-normal  boolean
        |--no-timestamp  boolean
        |--wrong-timestamp  boolean
        |--wrong-day  boolean
        |--wrong  boolean""".stripMargin
    helpOf("ls", "-h") shouldBe helpOf("ls", "--help")
    helpOf("ls", "--long", "--help") shouldBe helpOf("ls", "--help")

    helpOf("set-metadata", "--help") shouldBe
      """set-metadata  set metadata property
        |
        |Positionals:
        |property  required
        |picture  required
        |
        |Flags:
        |none""".stripMargin
    helpOf("set-metadata", "-h") shouldBe helpOf("set-metadata", "--help")

    helpOf("albums", "--help") shouldBe
      """albums  list album claims
        |
        |Positionals:
        |path  optional
        |
        |Flags:
        |none""".stripMargin
    helpOf("albums", "-h") shouldBe helpOf("albums", "--help")

    helpOf("publish", "--help") shouldBe
      """publish  publish albums to Google Photos
        |
        |Positionals:
        |path  optional
        |
        |Flags:
        |--recursive  boolean
        |--dry-run  boolean""".stripMargin
    helpOf("publish", "-h") shouldBe helpOf("publish", "--help")
    helpOf("publish", "--recursive", "--help") shouldBe helpOf("publish", "--help")

    helpOf("sync", "--help") shouldBe
      """sync  sync album removals into local exclude lists
        |
        |Positionals:
        |path  optional
        |
        |Flags:
        |--recursive  boolean
        |--dry-run  boolean""".stripMargin
    helpOf("sync", "-h") shouldBe helpOf("sync", "--help")

    helpOf("login", "--help") shouldBe
      """login  log in to Google Photos
        |
        |Positionals:
        |none
        |
        |Flags:
        |none""".stripMargin
    helpOf("login", "-h") shouldBe helpOf("login", "--help")

    helpOf("status", "--help") shouldBe
      """status  show Google Photos login status
        |
        |Positionals:
        |none
        |
        |Flags:
        |none""".stripMargin
    helpOf("status", "-h") shouldBe helpOf("status", "--help")

    helpOf("logout", "--help") shouldBe
      """logout  log out of Google Photos
        |
        |Positionals:
        |none
        |
        |Flags:
        |none""".stripMargin
    helpOf("logout", "-h") shouldBe helpOf("logout", "--help")

  it should "reject usage errors" in :
    val errors: List[(List[String], String)] = List(
      List() -> "missing command",
      List("photo") -> "unknown command 'photo'",
      List("LS") -> "unknown command 'LS'",
      List("Lsd") -> "unknown command 'Lsd'",
      List("lsd", "--bogus") -> "unknown option '--bogus'",
      List("albums", "--bogus") -> "unknown option '--bogus'",
      List("ls", "--dry-run", "2009") -> "unknown option '--dry-run'",
      List("login", "--long") -> "unknown option '--long'",
      List("ls", "-abc", "2009") -> "unknown option '-abc'",
      List("ls", "-nfoo", "2009") -> "unknown option '-nfoo'",
      List("lsd", "--Long", "2009") -> "unknown option '--Long'",
      List("ls", "--timestamp-from", "2009") -> "unknown option '--timestamp-from'",
      List("lsd") -> "missing directory",
      List("ls") -> "missing path",
      List("ls", "--name-contains", "--", "-hidden") -> "missing path",
      List("set-metadata") -> "missing property",
      List("set-metadata", "bad-raw") -> "missing picture",
      List("lsd", "2009", "extra") -> "extra argument 'extra'",
      List("login", "extra") -> "extra argument 'extra'",
      List("albums", "2009", "2010") -> "extra argument '2010'",
      List("ls", "2009/03/10", "--fix") -> "extra argument '--fix'",
      List("ls", "path", "--help") -> "extra argument '--help'",
      List("ls", "path", "-h") -> "extra argument '-h'",
      List("publish", "2009", "--recursive") -> "extra argument '--recursive'",
      List("lsd", "--long", "--long", "2009") -> "repeated option '--long'",
      List("lsd", "--long", "-l", "2009") -> "repeated option '--long'",
      List("lsd", "-l", "--long", "2009") -> "repeated option '--long'",
      List("publish", "--dry-run=yes") -> "invalid value 'yes' for '--dry-run'",
      List("lsd", "--long=1", "2009") -> "invalid value '1' for '--long'",
      List("lsd", "--long=on", "2009") -> "invalid value 'on' for '--long'",
      List("lsd", "--long=", "2009") -> "invalid value '' for '--long'",
      List("ls", "--name-contains") -> "missing value for '--name-contains'",
      List("ls", "--name-contains=") -> "missing value for '--name-contains'",
      List("ls", "-n") -> "missing value for '--name-contains'",
      List("ls", "--name-contains", "--") -> "missing value for '--name-contains'",
      List("ls", "--name-contains", "-hidden") -> "value for '--name-contains' looks like a flag: '-hidden'",
      List("ls", "-n", "-hidden", "2009") -> "value for '--name-contains' looks like a flag: '-hidden'"
    )
    errors.foreach: (args, message) =>
      withClue(args):
        Cli.parse(args) shouldBe Cli.Usage(message)

  "usageLine" should "prefix the program name" in :
    Cli.usageLine("missing command") shouldBe "Photo Archive Tool: missing command"
    Cli.usageLine("unknown option '--bogus'") shouldBe "Photo Archive Tool: unknown option '--bogus'"

  "toOperation" should "build the operations that do not read the archive" in :
    Cli.toOperation(Cli.LoginArgs) shouldBe Login
    Cli.toOperation(Cli.StatusArgs) shouldBe Status
    Cli.toOperation(Cli.LogoutArgs) shouldBe Logout
    Cli.toOperation(Cli.Albums(None)) shouldBe ListAlbums(None)
    Cli.toOperation(Cli.PublishArgs(None, recursive = true, dryRun = false)) shouldBe
      Publish(None, recursive = true, dryRun = false)
    Cli.toOperation(Cli.SyncArgs(None, recursive = false, dryRun = true)) shouldBe
      Sync(None, recursive = false, dryRun = true)

  "parsePropertyAndValue" should "split on the last equals and use a real property name" in :
    Files.splitOnLast("bad-raw", '=') shouldBe ("bad-raw", None)
    Files.splitOnLast("bad-raw=true", '=') shouldBe ("bad-raw", Some("true"))
    Files.splitOnLast("a=b=c", '=') shouldBe ("a=b", Some("c"))

    Cli.parsePropertyAndValue("source=scan").set(Metadata()) shouldBe Metadata(source = Some("scan"))
    Cli.parsePropertyAndValue("timestamp=2011-06-29T09:45:00").set(Metadata()) shouldBe
      Metadata(timestamp = Some("2011-06-29T09:45:00"))
    Cli.parsePropertyAndValue("date=2012-04-04").set(Metadata()) shouldBe
      Metadata(date = Some(LocalDate.parse("2012-04-04")))
    Cli.parsePropertyAndValue("bad-raw=true").set(Metadata()) shouldBe Metadata(`bad-raw` = Some(true))
    Cli.parsePropertyAndValue("bad-raw").set(Metadata(`bad-raw` = Some(true))) shouldBe Metadata()
    Cli.parsePropertyAndValue("orientation=Rotate 90 CW").set(Metadata()) shouldBe
      Metadata(orientation = Some("Rotate 90 CW"))
    Cli.parsePropertyAndValue("google-media-item-id=media-item-1").set(Metadata()) shouldBe
      Metadata(`google-media-item-id` = Some("media-item-1"))
    Cli.parsePropertyAndValue("already-in-google=false").set(Metadata()) shouldBe
      Metadata(`already-in-google` = Some(false))

    a [NoSuchElementException] should be thrownBy Cli.parsePropertyAndValue("nope")
    a [NoSuchElementException] should be thrownBy Cli.parsePropertyAndValue("source=a=b")

  private def helpOf(args: String*): String = Cli.parse(args.toList) match
    case Cli.Help(text) => text
    case other => fail(s"expected help for ${args.toList}, got $other")

  private def ls(
    path: String,
    long: Boolean = false,
    fix: Boolean = false,
    nameContains: Option[String] = None,
    hasExtension: Option[String] = None,
    property: Option[String] = None,
    timestampFromNameFormat: Option[String] = None,
    timestampFromName: Boolean = false,
    timestampFromNameOnly: Boolean = false,
    timestampsVary: Boolean = false,
    hasMetadata: Boolean = false,
    noNormal: Boolean = false,
    noTimestamp: Boolean = false,
    wrongTimestamp: Boolean = false,
    wrongDay: Boolean = false,
    wrong: Boolean = false
  ): Cli.Outcome = Cli.Run(Cli.Ls(
    path,
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
  ))

  private def publish(path: Option[String], recursive: Boolean, dryRun: Boolean): Cli.Outcome =
    Cli.Run(Cli.PublishArgs(path, recursive, dryRun))

  private def sync(path: Option[String], recursive: Boolean, dryRun: Boolean): Cli.Outcome =
    Cli.Run(Cli.SyncArgs(path, recursive, dryRun))
