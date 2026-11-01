package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.time.{LocalDate, ZoneId}

class CliTest extends AnyFlatSpec, Matchers:
  // strictEquality does not relate a sealed parent to its cases.
  @targetName("outcomeCanEqual")
  private given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

  @targetName("operationCanEqual")
  private given [A <: Operation, B <: Operation] => CanEqual[A, B] = CanEqual.derived

  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

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
    Cli.parse(List(
      "ls",
      "--long",
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

    Cli.parse(List("repair", "--names", "2009")) shouldBe
      repair("2009", names = true)
    Cli.parse(List("repair", "--raw", "--timestamps", "--days", "--dry-run", "2009/03")) shouldBe
      repair("2009/03", raw = true, timestamps = true, days = true, dryRun = true)
    Cli.parse(List("repair", "--names=false", "--raw", "2009")) shouldBe
      repair("2009", raw = true)

    Cli.parse(List("login")) shouldBe Cli.Run(Cli.LoginArgs)
    Cli.parse(List("status")) shouldBe Cli.Run(Cli.StatusArgs)
    Cli.parse(List("logout")) shouldBe Cli.Run(Cli.LogoutArgs)

    Cli.parse(List("agent", "stems", "2010/07/19")) shouldBe Cli.Run(Cli.AgentStemsArgs("2010/07/19"))
    Cli.parse(List("agent", "catalog")) shouldBe Cli.Run(Cli.AgentCatalogArgs)
    Cli.parse(List("agent", "items", "--id", "a1", "2010/07")) shouldBe
      Cli.Run(Cli.AgentItemsArgs(Some("a1"), "2010/07"))
    Cli.parse(List("agent", "items", "2010/07")) shouldBe Cli.Run(Cli.AgentItemsArgs(None, "2010/07"))
    Cli.parse(List("agent", "match", "--id", "a1", "2010/07")) shouldBe
      Cli.Run(Cli.AgentMatchArgs(Some("a1"), "2010/07"))
    Cli.parse(List("agent", "match", "2010/07")) shouldBe Cli.Run(Cli.AgentMatchArgs(None, "2010/07"))
    Cli.parse(List(
      "agent", "include", "--span", "2010/07", "2010/07/19/2412", "2010/07/20/2413"
    )) shouldBe Cli.Run(Cli.AgentIncludeArgs(Some("2010/07"), Seq("2010/07/19/2412", "2010/07/20/2413")))
    Cli.parse(List("agent", "include", "2010/07/19/2412")) shouldBe
      Cli.Run(Cli.AgentIncludeArgs(None, Seq("2010/07/19/2412")))
    Cli.parse(List(
      "agent", "album-file", "--id", "a1", "--dry-run", "--replace",
      "--google-title", "Exact", "--no-request", "2010/07"
    )) shouldBe Cli.Run(Cli.AgentAlbumFileArgs(
      Some("a1"), dryRun = true, replace = true, Some("Exact"), noRequest = true, "2010/07"
    ))
    Cli.parse(List(
      "agent", "request", "rename-album", "--album", "a1", "--current", "Old", "--new", "New", "--id", "r1"
    )) shouldBe Cli.Run(Cli.AgentRequestArgs("rename-album", Some("a1"), None, Some("Old"), Some("New"), Some("r1")))
    Cli.parse(List(
      "agent", "request", "move-item", "--album", "a1", "--item", "k1",
      "--current", "a.jpg", "--new", "a2", "--id", "r1"
    )) shouldBe Cli.Run(Cli.AgentRequestArgs(
      "move-item", Some("a1"), Some("k1"), Some("a.jpg"), Some("a2"), Some("r1")
    ))
    Cli.parse(List(
      "agent", "request", "set-description", "--item", "k1", "--current", "a.jpg", "--new", "Hello", "--id", "r1"
    )) shouldBe Cli.Run(Cli.AgentRequestArgs(
      "set-description", None, Some("k1"), Some("a.jpg"), Some("Hello"), Some("r1")
    ))
    Cli.parse(List(
      "agent", "request", "set-date", "--item", "k1", "--current", "a.jpg",
      "--new", "2010-07-19 00:00", "--id", "r1"
    )) shouldBe Cli.Run(Cli.AgentRequestArgs(
      "set-date", None, Some("k1"), Some("a.jpg"), Some("2010-07-19 00:00"), Some("r1")
    ))
    Cli.parse(List(
      "agent", "request", "download-item", "--album", "a1", "--item", "k1", "--current", "a.jpg", "--id", "r1"
    )) shouldBe Cli.Run(Cli.AgentRequestArgs(
      "download-item", Some("a1"), Some("k1"), Some("a.jpg"), None, Some("r1")
    ))
    Cli.parse(List(
      "agent", "request", "delete-item", "--album", "a1", "--item", "k1", "--current", "a.jpg", "--id", "r1"
    )) shouldBe Cli.Run(Cli.AgentRequestArgs(
      "delete-item", Some("a1"), Some("k1"), Some("a.jpg"), None, Some("r1")
    ))
    Cli.parse(List("agent", "requests")) shouldBe Cli.Run(Cli.AgentRequestsArgs)
    Cli.parse(List("agent", "results")) shouldBe Cli.Run(Cli.AgentResultsArgs)

  it should "print help and not an operation" in :
    val top: String = helpOf("--help")
    top shouldBe
      """Photo Archive Tool 0.0.1
        |manipulates photo archive
        |
        |lsd  list directories
        |ls  list pictures
        |repair  repair picture names, raw files, timestamps, or day folders
        |set-metadata  set metadata property
        |albums  list album claims
        |publish  publish albums to Google Photos
        |sync  sync album removals into local except lists
        |login  log in to Google Photos
        |status  show Google Photos login status
        |logout  log out of Google Photos
        |help  print command help
        |shell  start a session
        |serve  listen on the per-user Unix socket
        |agent  query the catalog and the archive""".stripMargin
    helpOf("-h") shouldBe top
    helpOf("--help", "ls") shouldBe top

    helpOf("lsd", "--help") shouldBe
      """lsd  list directories
        |
        |Positionals:
        |directory  required
        |
        |Flags:
        |--long, -l  boolean  list nested directories""".stripMargin
    helpOf("lsd", "-h") shouldBe helpOf("lsd", "--help")

    helpOf("ls", "--help") shouldBe
      """ls  list pictures
        |
        |Positionals:
        |path  required
        |
        |Flags:
        |--long, -l  boolean  show per-file timestamps and day mismatches
        |--name-contains, -n  text  keep pictures whose name contains this text
        |--has-extension, -e  text  keep pictures that have this extension
        |--property  text  keep pictures whose sidecar property matches
        |--timestamp-from-name-format  text  keep pictures whose filename clock uses this format
        |--timestamp-from-name  boolean  keep pictures whose filename encodes a time
        |--timestamp-from-name-only  boolean  keep pictures with a filename clock and no file metadata time
        |--timestamps-vary  boolean  keep pictures whose file times differ by more than an hour
        |--has-metadata  boolean  keep pictures that have a sidecar
        |--no-normal  boolean  keep pictures with no normal image
        |--no-timestamp  boolean  keep pictures with no assessed timestamp
        |--wrong-timestamp  boolean  keep pictures whose file mtime differs from the assessed timestamp
        |--wrong-day  boolean  keep pictures filed on a different day than the assessed timestamp
        |--wrong  boolean  keep pictures with no normal image, no timestamp, a wrong timestamp, or a wrong day""".stripMargin
    helpOf("ls", "-h") shouldBe helpOf("ls", "--help")
    helpOf("ls", "--long", "--help") shouldBe helpOf("ls", "--help")

    helpOf("repair", "--help") shouldBe
      """repair  repair picture names, raw files, timestamps, or day folders
        |
        |Positionals:
        |path  required
        |
        |Flags:
        |--names  boolean  rename one MVIMG_, PXL_, IMG_, VID_, or MVI_ prefix, add v when a video shares a stem with a still, and lowercase known extensions
        |--raw  boolean  make a JPEG from RAW with dcraw when no normal image exists
        |--timestamps  boolean  set file mtimes from the assessed instant
        |--days  boolean  move a picture into the assessed day folder
        |--dry-run  boolean  print those actions and write nothing""".stripMargin
    helpOf("repair", "-h") shouldBe helpOf("repair", "--help")

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
        |--recursive  boolean  required for a year or the archive root
        |--dry-run  boolean  print the local plan and do not call Google""".stripMargin
    helpOf("publish", "-h") shouldBe helpOf("publish", "--help")
    helpOf("publish", "--recursive", "--help") shouldBe helpOf("publish", "--help")

    helpOf("sync", "--help") shouldBe
      """sync  sync album removals into local except lists
        |
        |Positionals:
        |path  optional
        |
        |Flags:
        |--recursive  boolean  required for a year or the archive root
        |--dry-run  boolean  read Google, print except and remove lines, and do not write""".stripMargin
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

    helpOf("help") shouldBe top
    helpOf("help", "ls") shouldBe helpOf("ls", "--help")
    helpOf("help", "--", "ls") shouldBe helpOf("ls", "--help")
    helpOf("help", "--help") shouldBe
      """help  print command help
        |
        |Positionals:
        |command  optional
        |
        |Flags:
        |none""".stripMargin
    val where: String = "only available in the shell"
    helpOf("cd", "--help") should include (where)
    helpOf("pwd", "--help") should include (where)
    helpOf("exit", "--help") should include (where)
    helpOf("cd", "--help") shouldBe
      """cd  change the cursor; only available in the shell
        |
        |Positionals:
        |directory  optional
        |
        |Flags:
        |none""".stripMargin
    top.linesIterator.exists(_.startsWith("cd")) shouldBe false
    top.linesIterator.exists(_.startsWith("pwd")) shouldBe false
    top.linesIterator.exists(_.startsWith("exit")) shouldBe false
    top.linesIterator.exists(_.startsWith("agent")) shouldBe true
    top.linesIterator.exists(_.startsWith("serve")) shouldBe true
    top.linesIterator.exists(_.startsWith("album-file")) shouldBe false

    val agent: String = helpOf("agent", "--help")
    agent shouldBe
      """agent  query the catalog and the archive
        |
        |stems  list one day's grouped stems
        |catalog  list catalog albums
        |items  list catalog items for one album
        |match  match catalog items to local stems
        |include  compress include lines
        |album-file  write an album file
        |request  append one request row
        |requests  list request rows
        |results  list result rows""".stripMargin
    helpOf("agent", "-h") shouldBe agent
    helpOf("help", "agent") shouldBe agent
    agent should not include ("rename-album")
    agent should not include ("move-item")
    agent should not include ("set-description")
    agent should not include ("set-date")
    agent should not include ("download-item")
    agent should not include ("delete-item")
    helpOf("agent", "request", "--help") shouldBe
      """request  append one request row
        |
        |rename-album  rename an owned album
        |move-item  move an item to another album
        |set-description  set an item description
        |set-date  set an item date
        |download-item  download an item
        |delete-item  delete an item""".stripMargin
    helpOf("agent", "request", "rename-album", "--help") shouldBe
      """rename-album  rename an owned album
        |
        |Positionals:
        |none
        |
        |Flags:
        |--album  text  album id
        |--current  text  current title
        |--new  text  new title
        |--id  text  request id""".stripMargin
    helpOf("agent", "album-file", "--help") should include ("--id  text  catalog album id")
    helpOf("agent", "include", "--help") shouldBe
      """include  compress include lines
        |
        |Positionals:
        |year/month/day/stem  rest
        |
        |Flags:
        |--span  text  title span; absent means a curated album""".stripMargin
    helpOf("agent", "items", "--help") shouldBe
      """items  list catalog items for one album
        |
        |Positionals:
        |album  required
        |
        |Flags:
        |--id  text  catalog album id""".stripMargin

  it should "reject usage errors" in :
    val errors: List[(List[String], String)] = List(
      List() -> "missing command",
      List("photo") -> "unknown command 'photo'",
      List("LS") -> "unknown command 'LS'",
      List("Lsd") -> "unknown command 'Lsd'",
      List("lsd", "--bogus") -> "unknown option '--bogus'",
      List("albums", "--bogus") -> "unknown option '--bogus'",
      List("ls", "--dry-run", "2009") -> "unknown option '--dry-run'",
      List("ls", "--fix", "2009") -> "unknown option '--fix'",
      List("repair") -> "missing path",
      List("repair", "2009") -> "missing repair action",
      List("repair", "--dry-run", "2009") -> "missing repair action",
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
      List("ls", "-n", "-hidden", "2009") -> "value for '--name-contains' looks like a flag: '-hidden'",
      List("help", "ls", "extra") -> "extra argument 'extra'",
      List("help", "nope") -> "unknown command 'nope'",
      List("cd", "--long") -> "unknown option '--long'",
      List("pwd", "x") -> "extra argument 'x'",
      List("exit", "now") -> "extra argument 'now'",
      List("shell", "pwd") -> "extra argument 'pwd'",
      List("shell", "--help") -> "extra argument '--help'",
      List("serve", "pwd") -> "extra argument 'pwd'",
      List("serve", "--help") -> "extra argument '--help'",
      List("agent") -> "missing agent command",
      List("agent", "--id", "x", "items", "2010/07") -> "unknown command '--id'",
      List("agent", "items", "2010/07", "--id", "a1") -> "extra argument '--id'",
      List("agent", "match", "2010/07", "--id", "a1") -> "extra argument '--id'",
      List("agent", "include", "2010/07/19/2412", "--span", "2010/07") -> "extra argument '--span'",
      List("agent", "stems", "2010/07/19", "--long") -> "extra argument '--long'",
      List("agent", "stems") -> "missing day",
      List("agent", "items") -> "missing album",
      List("agent", "match") -> "missing album",
      List("agent", "catalog", "June") -> "extra argument 'June'",
      List("agent", "nope") -> "unknown command 'nope'",
      List("agent", "album-file", "2010/07", "--replace") -> "extra argument '--replace'",
      List("agent", "request") -> "missing request action",
      List("agent", "request", "--album", "a1", "rename-album") -> "unknown command '--album'",
      List("agent", "requests", "x") -> "extra argument 'x'",
      List("agent", "results", "x") -> "extra argument 'x'"
    )
    errors.foreach: (args, message) =>
      withClue(args):
        Cli.parse(args) shouldBe Cli.Usage(message)

  "one-shot" should "refuse cd, pwd, and exit" in :
    val root: Root = Root(Configuration("Pictures/originals", ZoneId.of("America/New_York")))
    val session: Session = Session(root)
    def run(args: String*): Result = Cli.parse(args.toList) match
      case Cli.Run(parsed) =>
        val (kept, result) = Cli.toOperation(parsed).execute(session)
        kept should be theSameInstanceAs session
        result
      case other => fail(other.toString)

    run("cd") shouldBe Result.Usage("cd is only available in the shell")
    run("cd", "2010") shouldBe Result.Usage("cd is only available in the shell")
    run("cd", "/") shouldBe Result.Usage("cd is only available in the shell")
    run("pwd") shouldBe Result.Usage("pwd is only available in the shell")
    run("exit") shouldBe Result.Usage("exit is only available in the shell")
    Cli.parse(List("shell")) shouldBe Cli.ShellFront
    Cli.parse(List("serve")) shouldBe Cli.ServeFront
    Cli.parse(List("shell"), Cli.Front.Shell) shouldBe Cli.Usage("already running")
    Cli.parse(List("shell", "--help"), Cli.Front.Shell) shouldBe Cli.Usage("already running")
    Cli.parse(List("serve"), Cli.Front.Shell) shouldBe Cli.Usage("unknown command 'serve'")
    Cli.parse(List("serve", "--help"), Cli.Front.Shell) shouldBe Cli.Usage("unknown command 'serve'")
    Cli.parse(List("shell"), Cli.Front.Serve) shouldBe Cli.Usage("already running")
    Cli.parse(List("shell", "--help"), Cli.Front.Serve) shouldBe Cli.Usage("already running")
    Cli.parse(List("serve"), Cli.Front.Serve) shouldBe Cli.Usage("already running")
    Cli.parse(List("serve", "--help"), Cli.Front.Serve) shouldBe Cli.Usage("already running")
    Cli.parse(List("serve", "stop")) shouldBe Cli.ServeStop
    Cli.parse(List("serve", "stop", "now")) shouldBe Cli.Usage("extra argument 'now'")
    Cli.parse(List("serve", "stop"), Cli.Front.Serve) shouldBe Cli.ServeStop
    Cli.parse(List("serve", "stop", "now"), Cli.Front.Serve) shouldBe Cli.Usage("already running")
    Cli.parse(List("serve", "stop"), Cli.Front.Shell) shouldBe Cli.Usage("unknown command 'serve'")

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
    Cli.parsePropertyAndValue("uploader=Ada").set(Metadata()) shouldBe Metadata(uploader = Some("Ada"))
    Cli.parsePropertyAndValue("uploader= Ada ").set(Metadata()) shouldBe Metadata(uploader = Some(" Ada "))
    Cli.parsePropertyAndValue("uploader").set(Metadata(uploader = Some("Ada"))) shouldBe Metadata()
    val blank: IllegalArgumentException = the [IllegalArgumentException] thrownBy
      Cli.parsePropertyAndValue("uploader=   ").set(Metadata())
    blank.getMessage shouldBe "key 'uploader' must be a non-empty display name"
    val tab: IllegalArgumentException = the [IllegalArgumentException] thrownBy
      Cli.parsePropertyAndValue("uploader=Ada\t").set(Metadata())
    tab.getMessage shouldBe "key 'uploader' must not contain a tab, CR, or LF"

    an [IllegalArgumentException] should be thrownBy Cli.parsePropertyAndValue("nope")
    an [IllegalArgumentException] should be thrownBy Cli.parsePropertyAndValue("source=a=b")

  private def helpOf(args: String*): String = Cli.parse(args.toList) match
    case Cli.Help(text) => text
    case other => fail(s"expected help for ${args.toList}, got $other")

  private def ls(
    path: String,
    long: Boolean = false,
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

  private def repair(
    path: String,
    names: Boolean = false,
    raw: Boolean = false,
    timestamps: Boolean = false,
    days: Boolean = false,
    dryRun: Boolean = false
  ): Cli.Outcome = Cli.Run(Cli.RepairArgs(path, names, raw, timestamps, days, dryRun))

  private def publish(path: Option[String], recursive: Boolean, dryRun: Boolean): Cli.Outcome =
    Cli.Run(Cli.PublishArgs(path, recursive, dryRun))

  private def sync(path: Option[String], recursive: Boolean, dryRun: Boolean): Cli.Outcome =
    Cli.Run(Cli.SyncArgs(path, recursive, dryRun))
