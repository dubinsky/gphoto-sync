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

  "card" should "parse discover and copy without joining the registry" in:
    Cli.parse(List("card")) shouldBe Cli.Run(Cli.CardDiscover(dryRun = false))
    Cli.parse(List("card", "--dry-run")) shouldBe Cli.Run(Cli.CardDiscover(dryRun = true))
    Cli.parse(List("card", "--dry-run=false")) shouldBe Cli.Run(Cli.CardDiscover(dryRun = false))
    Cli.parse(List("card", "copy")) shouldBe Cli.Run(Cli.CardCopy(dryRun = false, path = None))
    Cli.parse(List("card", "copy", "--dry-run", "/tmp/card")) shouldBe
      Cli.Run(Cli.CardCopy(dryRun = true, path = Some("/tmp/card")))
    Cli.parse(List("card", "--dry-run", "--dry-run")) shouldBe Cli.Usage("repeated option '--dry-run'")
    Cli.parse(List("card", "copy", "a", "b")) shouldBe Cli.Usage("extra argument 'b'")
    Cli.parse(List("card", "eject")) shouldBe Cli.Usage("unknown command 'eject'")
    Cli.parse(List("help", "card", "copy")) shouldBe Cli.Usage("extra argument 'copy'")
    Cli.toOperation(Cli.CardDiscover(dryRun = true)) shouldBe DiscoverCard(dryRun = true)
    Cli.toOperation(Cli.CardCopy(dryRun = false, path = Some("/tmp/card"))) shouldBe
      CopyCard(dryRun = false, path = Some("/tmp/card"))
    Cli.inProcessWrite(Cli.CardDiscover(dryRun = false)) shouldBe false
    Cli.inProcessWrite(Cli.CardCopy(dryRun = true, path = None)) shouldBe false

  "parse" should "accept every subcommand" in:
    Cli.parse(List("ls")) shouldBe ls()
    Cli.parse(List("ls", "2009")) shouldBe ls(Some("2009"))
    Cli.parse(List("ls", "-R", "2009")) shouldBe ls(Some("2009"), recursive = true)
    Cli.parse(List("ls", "--recursive", "2009")) shouldBe ls(Some("2009"), recursive = true)
    Cli.parse(List("ls", "--recursive=false", "2009")) shouldBe ls(Some("2009"))
    Cli.parse(List("ls", "--long", "false")) shouldBe ls(path = Some("false"), long = true)
    Cli.parse(List("ls", "--long=false", "2009")) shouldBe ls(Some("2009"), long = false)
    Cli.parse(
      List(
        "ls",
        "--long",
        "--name-contains",
        "mid",
        "--has-extension",
        "jpg",
        "--property",
        "bad-raw",
        "--timestamp-from-name-format",
        "yyyy",
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
      )
    ) shouldBe ls(
      path = Some("2009/03"),
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
    Cli.parse(List("ls", "--name-contains=mid", "2009")) shouldBe ls(Some("2009"), nameContains = Some("mid"))
    Cli.parse(List("ls", "--name-contains=-hidden", "2009")) shouldBe ls(Some("2009"), nameContains = Some("-hidden"))
    Cli.parse(List("ls", "-n", "mid", "2009")) shouldBe ls(Some("2009"), nameContains = Some("mid"))
    Cli.parse(List("ls", "-n=mid", "2009")) shouldBe ls(Some("2009"), nameContains = Some("mid"))
    Cli.parse(List("ls", "-e", "jpg", "2009")) shouldBe ls(Some("2009"), hasExtension = Some("jpg"))
    Cli.parse(List("ls", "-e=jpg", "2009")) shouldBe ls(Some("2009"), hasExtension = Some("jpg"))
    Cli.parse(List("ls", "--has-extension=jpg", "2009")) shouldBe ls(Some("2009"), hasExtension = Some("jpg"))
    Cli.parse(List("ls", "--property=already-in-google=false", "2009")) shouldBe
      ls(Some("2009"), property = Some("already-in-google=false"))
    Cli.parse(List("ls", "--name-contains", "--", "-hidden", "2009")) shouldBe
      ls(Some("2009"), nameContains = Some("-hidden"))
    Cli.parse(List("ls", "--", "--fix")) shouldBe ls(Some("--fix"))
    Cli.parse(List("ls", "--name-contains", "--", "-hidden")) shouldBe
      ls(nameContains = Some("-hidden"))

    Cli.parse(List("item", "set", "bad-raw", "2009/03/10/2412.jpg")) shouldBe
      Cli.Run(Cli.ItemSetArgs(dryRun = false, "bad-raw", "2009/03/10/2412.jpg"))
    Cli.parse(List("item", "set", "bad-raw=true", "2009/03/10/2412.jpg")) shouldBe
      Cli.Run(Cli.ItemSetArgs(dryRun = false, "bad-raw=true", "2009/03/10/2412.jpg"))
    Cli.parse(List("item", "set", "--dry-run", "date=2010/08/01", "2010/07/04/2412")) shouldBe
      Cli.Run(Cli.ItemSetArgs(dryRun = true, "date=2010/08/01", "2010/07/04/2412"))
    Cli.parse(List("item", "set", "--dry-run=false", "source=scan", "2010/07/04/2412")) shouldBe
      Cli.Run(Cli.ItemSetArgs(dryRun = false, "source=scan", "2010/07/04/2412"))
    Cli.parse(List("item", "show", "2010/07/04/2412")) shouldBe
      Cli.Run(Cli.ItemShowArgs("2010/07/04/2412"))
    Cli.parse(List("item", "month", "--dry-run", "2010/07/04/2412")) shouldBe
      Cli.Run(Cli.ItemMonthArgs(dryRun = true, Seq("2010/07/04/2412")))
    Cli.parse(List("item", "into", "--dry-run", "Best", "2010/07/04/2412")) shouldBe
      Cli.Run(Cli.ItemIntoArgs(dryRun = true, "Best", Seq("2010/07/04/2412")))
    Cli.inProcessWrite(Cli.ItemShowArgs("2010/07/04/2412")) shouldBe false
    Cli.inProcessWrite(Cli.ItemSetArgs(dryRun = true, "date=", "2010/07/04/2412")) shouldBe false
    Cli.inProcessWrite(Cli.ItemMonthArgs(dryRun = false, Seq("2010/07/04/2412"))) shouldBe false
    Cli.inProcessWrite(Cli.ItemIntoArgs(dryRun = true, "Best", Seq("2010/07/04/2412"))) shouldBe false

    Cli.parse(List("album", "ls")) shouldBe Cli.Run(Cli.AlbumLsArgs(None))
    Cli.parse(List("album", "ls", "2009")) shouldBe Cli.Run(Cli.AlbumLsArgs(Some("2009")))
    Cli.parse(List("album", "show", "Best")) shouldBe Cli.Run(Cli.AlbumShowArgs("Best"))
    Cli.parse(List("album", "claims")) shouldBe Cli.Run(Cli.AlbumClaimsArgs(None))
    Cli.parse(List("album", "claims", "2009")) shouldBe Cli.Run(Cli.AlbumClaimsArgs(Some("2009")))
    Cli.parse(List("album", "new", "--span", "2010/07")) shouldBe
      Cli.Run(Cli.AlbumNewArgs(dryRun = false, Some("2010/07"), None))
    Cli.parse(List("album", "new", "--name", "Cape Cod")) shouldBe
      Cli.Run(Cli.AlbumNewArgs(dryRun = false, None, Some("Cape Cod")))
    Cli.parse(List("album", "new", "--dry-run", "--span", "2010/07", "--name", "Cape Cod")) shouldBe
      Cli.Run(Cli.AlbumNewArgs(dryRun = true, Some("2010/07"), Some("Cape Cod")))
    Cli.parse(List("album", "rm", "--dry-run", "2010/07")) shouldBe
      Cli.Run(Cli.AlbumRmArgs(dryRun = true, "2010/07"))
    Cli.parse(List("album", "mv", "--dry-run", "2010/07", "2010/08")) shouldBe
      Cli.Run(Cli.AlbumMvArgs(dryRun = true, "2010/07", "2010/08"))
    Cli.parse(List("album", "include", "--dry-run", "2010/07", "2010/07/04/2412")) shouldBe
      Cli.Run(Cli.AlbumIncludeArgs(dryRun = true, "2010/07", Seq("2010/07/04/2412")))
    Cli.parse(List("album", "exclude", "2010/07", "2010/07/04/2412", "2010/07/05/1000")) shouldBe
      Cli.Run(Cli.AlbumExcludeArgs(dryRun = false, "2010/07", Seq("2010/07/04/2412", "2010/07/05/1000")))
    Cli.parse(List("album", "merge", "--dry-run", "2010/07", "2010/08")) shouldBe
      Cli.Run(Cli.AlbumMergeArgs(dryRun = true, "2010/07", "2010/08"))
    Cli.inProcessWrite(Cli.AlbumLsArgs(None)) shouldBe false
    Cli.inProcessWrite(Cli.AlbumShowArgs("Best")) shouldBe false
    Cli.inProcessWrite(Cli.AlbumClaimsArgs(None)) shouldBe false
    Cli.inProcessWrite(Cli.AlbumNewArgs(dryRun = true, Some("2010/07"), None)) shouldBe false
    Cli.inProcessWrite(Cli.AlbumRmArgs(dryRun = false, "2010/07")) shouldBe false
    Cli.inProcessWrite(Cli.AlbumMvArgs(dryRun = false, "2010/07", "2010/08")) shouldBe false
    Cli.inProcessWrite(Cli.AlbumIncludeArgs(dryRun = false, "2010/07", Seq("2010/07/04/2412"))) shouldBe false
    Cli.inProcessWrite(Cli.AlbumExcludeArgs(dryRun = true, "2010/07", Seq("2010/07/04/2412"))) shouldBe false
    Cli.inProcessWrite(Cli.AlbumMergeArgs(dryRun = true, "2010/07", "2010/08")) shouldBe false

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
    Cli.parse(List("agent", "years")) shouldBe Cli.Run(Cli.AgentYearsArgs)
    Cli.parse(List("agent", "months", "2010")) shouldBe Cli.Run(Cli.AgentMonthsArgs("2010"))
    Cli.parse(List("agent", "days", "2010/07")) shouldBe Cli.Run(Cli.AgentDaysArgs("2010/07"))
    Cli.parse(List("agent", "claims", "2010/07/19")) shouldBe Cli.Run(Cli.AgentClaimsArgs("2010/07/19"))
    Cli.parse(List("agent", "catalog")) shouldBe Cli.Run(Cli.AgentCatalogArgs)
    Cli.parse(List("agent", "items", "--id", "a1", "2010/07")) shouldBe
      Cli.Run(Cli.AgentItemsArgs(Some("a1"), "2010/07"))
    Cli.parse(List("agent", "items", "2010/07")) shouldBe Cli.Run(Cli.AgentItemsArgs(None, "2010/07"))
    Cli.parse(List("agent", "match", "--id", "a1", "2010/07")) shouldBe
      Cli.Run(Cli.AgentMatchArgs(Some("a1"), "2010/07"))
    Cli.parse(List("agent", "match", "2010/07")) shouldBe Cli.Run(Cli.AgentMatchArgs(None, "2010/07"))
    Cli.parse(List("agent", "locate", "2010/07/19", "2412")) shouldBe
      Cli.Run(Cli.AgentLocateArgs("2010/07/19", "2412"))
    Cli.parse(
      List(
        "agent",
        "include",
        "--span",
        "2010/07",
        "2010/07/19/2412",
        "2010/07/20/2413"
      )
    ) shouldBe Cli.Run(Cli.AgentIncludeArgs(Some("2010/07"), Seq("2010/07/19/2412", "2010/07/20/2413")))
    Cli.parse(List("agent", "include", "2010/07/19/2412")) shouldBe
      Cli.Run(Cli.AgentIncludeArgs(None, Seq("2010/07/19/2412")))
    Cli.parse(
      List(
        "agent",
        "album-file",
        "--id",
        "a1",
        "--dry-run",
        "--replace",
        "--google-title",
        "Exact",
        "2010/07"
      )
    ) shouldBe Cli.Run(
      Cli.AgentAlbumFileArgs(
        Some("a1"),
        dryRun = true,
        replace = true,
        Some("Exact"),
        "2010/07"
      )
    )
    Cli.parse(
      List(
        "agent",
        "rename-album",
        "--title",
        "2010/07 ",
        "--current",
        "2010/07 ",
        "--new",
        "July",
        "--id",
        "a1"
      )
    ) shouldBe Cli.Run(Cli.RenameAlbumArgs(Some("2010/07 "), Some("2010/07 "), Some("July"), Some("a1")))
    Cli.parse(
      List(
        "agent",
        "request",
        "rename-album",
        "--album",
        "a1",
        "--current",
        "Old",
        "--new",
        "New",
        "--id",
        "r1"
      )
    ) shouldBe Cli.Usage("unknown command 'request'")
    Cli.parse(List("agent", "request", "rename-album", "--help")) shouldBe
      Cli.Usage("unknown command 'request'")
    Cli.parse(
      List(
        "agent",
        "move-item",
        "--title",
        "Trip",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--dest",
        "Other",
        "--id",
        "a1",
        "--dest-id",
        "a2"
      )
    ) shouldBe Cli.Run(
      Cli.MoveItemArgs(
        Some("Trip"),
        Some("k1"),
        Some("a.jpg"),
        Some("Other"),
        Some("a1"),
        Some("a2")
      )
    )
    Cli.parse(
      List(
        "agent",
        "request",
        "move-item",
        "--album",
        "a1",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--new",
        "a2",
        "--id",
        "r1"
      )
    ) shouldBe Cli.Usage("unknown command 'request'")
    Cli.parse(List("agent", "request", "move-item", "--help")) shouldBe
      Cli.Usage("unknown command 'request'")
    Cli.parse(
      List(
        "agent",
        "delete-item",
        "--title",
        "Trip",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--id",
        "a1"
      )
    ) shouldBe Cli.Run(Cli.DeleteItemArgs(Some("Trip"), Some("k1"), Some("a.jpg"), Some("a1")))
    Cli.parse(
      List(
        "agent",
        "request",
        "delete-item",
        "--album",
        "a1",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--id",
        "r1"
      )
    ) shouldBe Cli.Usage("unknown command 'request'")
    Cli.parse(List("agent", "request", "delete-item", "--help")) shouldBe
      Cli.Usage("unknown command 'request'")
    Cli.parse(
      List(
        "agent",
        "upload-item",
        "--dest",
        "Trip",
        "--dest-id",
        "a2",
        "2010/07/01/a.jpg"
      )
    ) shouldBe Cli.Run(Cli.UploadItemArgs(Some("2010/07/01/a.jpg"), Some("Trip"), Some("a2")))
    Cli.parse(List("agent", "request", "upload-item", "2010/07/01/a.jpg")) shouldBe
      Cli.Usage("unknown command 'request'")
    Cli.parse(List("agent", "request", "upload-item", "--help")) shouldBe
      Cli.Usage("unknown command 'request'")
    Cli.parse(List("agent", "create-album", "--title", "Probe")) shouldBe
      Cli.Run(Cli.CreateAlbumArgs(Some("Probe")))
    Cli.parse(List("agent", "delete-album", "--title", "Probe", "--id", "a1")) shouldBe
      Cli.Run(Cli.DeleteAlbumArgs(Some("Probe"), Some("a1")))
    Cli.parse(
      List(
        "agent",
        "set-description",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--new",
        "Hello"
      )
    ) shouldBe Cli.Run(Cli.SetDescriptionArgs(Some("k1"), Some("a.jpg"), Some("Hello")))
    Cli.parse(
      List(
        "agent",
        "set-description",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--new",
        ""
      )
    ) shouldBe Cli.Run(Cli.SetDescriptionArgs(Some("k1"), Some("a.jpg"), Some("")))
    Cli.parse(
      List(
        "agent",
        "set-date",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--new",
        "2010-07-19 00:00"
      )
    ) shouldBe Cli.Run(Cli.SetDateArgs(Some("k1"), Some("a.jpg"), Some("2010-07-19 00:00")))
    Cli.parse(
      List(
        "agent",
        "request",
        "set-description",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--new",
        "Hello",
        "--id",
        "r1"
      )
    ) shouldBe Cli.Usage("unknown command 'request'")
    Cli.parse(List("agent", "request", "set-description", "--help")) shouldBe
      Cli.Usage("unknown command 'request'")
    Cli.parse(
      List(
        "agent",
        "request",
        "set-date",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--new",
        "2010-07-19 00:00",
        "--id",
        "r1"
      )
    ) shouldBe Cli.Usage("unknown command 'request'")
    Cli.parse(List("agent", "request", "set-date", "--help")) shouldBe
      Cli.Usage("unknown command 'request'")
    Cli.parse(
      List(
        "agent",
        "download-item",
        "--title",
        "Trip",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--id",
        "a1"
      )
    ) shouldBe Cli.Run(Cli.DownloadItemArgs(Some("Trip"), Some("k1"), Some("a.jpg"), Some("a1")))
    Cli.parse(
      List(
        "agent",
        "request",
        "download-item",
        "--album",
        "a1",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--id",
        "r1"
      )
    ) shouldBe Cli.Usage("unknown command 'request'")
    Cli.parse(List("agent", "request", "download-item", "--help")) shouldBe
      Cli.Usage("unknown command 'request'")
    Cli.parse(
      List(
        "agent",
        "add-to-album",
        "--title",
        "Trip",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--dest",
        "Other",
        "--id",
        "a1",
        "--dest-id",
        "a2"
      )
    ) shouldBe Cli.Run(
      Cli.AddToAlbumArgs(
        Some("Trip"),
        Some("k1"),
        Some("a.jpg"),
        Some("Other"),
        Some("a1"),
        Some("a2")
      )
    )
    Cli.parse(
      List(
        "agent",
        "remove-from-album",
        "--title",
        "Trip",
        "--item",
        "k1",
        "--current",
        "a.jpg",
        "--id",
        "a1"
      )
    ) shouldBe Cli.Run(Cli.RemoveFromAlbumArgs(Some("Trip"), Some("k1"), Some("a.jpg"), Some("a1")))
    Cli.parse(List("agent", "request", "add-to-album")) shouldBe
      Cli.Usage("unknown command 'request'")
    Cli.parse(List("agent", "request", "remove-from-album")) shouldBe
      Cli.Usage("unknown command 'request'")
    Cli.parse(List("agent", "requests")) shouldBe Cli.Usage("unknown command 'requests'")
    Cli.parse(List("agent", "results")) shouldBe Cli.Usage("unknown command 'results'")
    Cli.parse(List("agent", "apply-requests")) shouldBe Cli.Usage("unknown command 'apply-requests'")
    Cli.parse(List("agent", "drift")) shouldBe Cli.Run(Cli.AgentDriftArgs)
    Cli.parse(List("agent", "missing")) shouldBe Cli.Run(Cli.AgentMissingArgs)

  it should "print help and not an operation" in:
    val top: String = helpOf("--help")
    top shouldBe
      """Photo Archive Tool 0.0.1
        |manipulates photo archive
        |
        |ls  list the date tree
        |repair  repair picture names, raw files, timestamps, or day folders
        |item  show or change a picture
        |album  list and edit local album files
        |publish  publish albums to Google Photos
        |sync  sync album removals into local except lists
        |push  push external albums
        |refresh  refresh the Google Photos catalog from Chrome
        |reconcile  link catalog albums to local album files
        |file  file staged downloads into civil day folders
        |web  edit album membership on 127.0.0.1
        |card  find a camera card or copy it into .upload
        |login  log in to Google Photos
        |status  show Google Photos login status
        |logout  log out of Google Photos
        |help  print command help
        |shell  start a session
        |serve  listen on the per-user Unix socket
        |mcp  expose agent commands and local archive writes on MCP stdio
        |agent  query the catalog and the archive""".stripMargin
    helpOf("-h") shouldBe top
    helpOf("--help", "ls") shouldBe top

    helpOf("ls", "--help") shouldBe
      """ls  list the date tree
        |
        |Positionals:
        |path  optional
        |
        |Flags:
        |--long, -l  boolean  counts on a directory, or the long item line on a day or a picture
        |--recursive, -R  boolean  list items under the path
        |--name-contains, -n  text  keep pictures whose name contains this text
        |--has-extension, -e  text  keep pictures that have this extension
        |--property  text  keep pictures whose sidecar property matches
        |--timestamp-from-name-format  text  keep pictures whose filename clock uses this format
        |--timestamp-from-name  boolean  keep pictures whose filename encodes a time
        |--timestamp-from-name-only  boolean  keep pictures with a filename clock and no usable voting capture time
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

    val item: String =
      """item  show or change a picture
        |
        |show  show extensions, sidecar, folder day, assessed civil time, and claiming albums
        |set  set one sidecar property; date= moves the picture
        |month  exclude the picture from every other dated album that claims it, then include it in YYYY/MM, creating that album when absent
        |into  include the picture in the album; when that album has a span, also exclude it from every other span album""".stripMargin
    helpOf("item", "--help") shouldBe item
    helpOf("item", "-h") shouldBe item
    helpOf("help", "item") shouldBe item
    helpOf("item", "show", "--help") shouldBe
      """show  show extensions, sidecar, folder day, assessed civil time, and claiming albums
        |
        |Positionals:
        |picture  required
        |
        |Flags:
        |none""".stripMargin
    helpOf("help", "item", "show") shouldBe helpOf("item", "show", "--help")
    helpOf("item", "set", "--help") shouldBe
      """set  set one sidecar property; date= moves the picture
        |
        |Positionals:
        |property  required
        |picture  required
        |
        |Flags:
        |--dry-run  boolean  print the actions and write nothing""".stripMargin
    helpOf("item", "set", "-h") shouldBe helpOf("item", "set", "--help")

    val album: String =
      """album  list and edit local album files
        |
        |ls  list album titles
        |show  show one album file
        |claims  list album claims
        |new  create a local album
        |rm  delete an album file and leave the pictures
        |mv  change an album title and move its file
        |include  add pictures to one album and recompress include
        |exclude  remove pictures from one album and recompress include
        |merge  merge two adjoining month albums""".stripMargin
    helpOf("album", "--help") shouldBe album
    helpOf("album", "-h") shouldBe album
    helpOf("help", "album") shouldBe album
    helpOf("album", "ls", "--help") shouldBe
      """ls  list album titles
        |
        |Positionals:
        |path  optional
        |
        |Flags:
        |none""".stripMargin
    helpOf("help", "album", "new") shouldBe
      """new  create a local album
        |
        |Positionals:
        |none
        |
        |Flags:
        |--dry-run  boolean  print the actions and write nothing
        |--span  text  date span with no name
        |--name  text  album name""".stripMargin
    helpOf("album", "new", "--help") shouldBe helpOf("help", "album", "new")

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

    helpOf("push", "--help") shouldBe
      """push  push external albums
        |
        |Positionals:
        |none
        |
        |Flags:
        |--dry-run  boolean  print the plan and do not open Chrome""".stripMargin
    helpOf("push", "-h") shouldBe helpOf("push", "--help")

    helpOf("web", "--help") shouldBe
      """web  edit album membership on 127.0.0.1
        |
        |Positionals:
        |none
        |
        |Flags:
        |--port  text  loopback port; default 8765""".stripMargin
    helpOf("web", "-h") shouldBe helpOf("web", "--help")
    helpOf("help", "web") shouldBe helpOf("web", "--help")

    helpOf("help", "card") shouldBe
      """card  find a camera card or copy it into .upload
        |
        |No subcommand prints one card and does not copy.
        |--dry-run prints that card and does not mount.
        |A filed stem is claimed when Include.claims matches that folder day.
        |include = all does that inside the album span.
        |include = all except does not claim an excepted stem.
        |No include line is written.
        |Unclaimed pictures are omitted from photo publish.
        |copy  copy DCIM picture files into .upload""".stripMargin
    helpOf("card", "--help") shouldBe helpOf("help", "card")
    helpOf("card", "--dry-run", "--help") shouldBe
      """card  print one camera card
        |
        |Positionals:
        |none
        |
        |Flags:
        |--dry-run  boolean  print the device and do not mount""".stripMargin
    helpOf("card", "copy", "--help") shouldBe
      """copy  copy DCIM picture files into .upload
        |
        |Positionals:
        |path  optional
        |
        |Flags:
        |--dry-run  boolean  print copy and keep lines and write nothing""".stripMargin

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
    helpOf("cd", "--help") should include(where)
    helpOf("pwd", "--help") should include(where)
    helpOf("exit", "--help") should include(where)
    helpOf("cd", "--help") shouldBe
      """cd  change the cursor; only available in the shell
        |
        |Positionals:
        |directory  optional
        |
        |Flags:
        |none""".stripMargin
    top.linesIterator.exists(_.startsWith("cd ")) shouldBe false
    top.linesIterator.exists(_.startsWith("pwd")) shouldBe false
    top.linesIterator.exists(_.startsWith("exit")) shouldBe false
    top.linesIterator.exists(_.startsWith("agent")) shouldBe true
    top.linesIterator.exists(_.startsWith("serve")) shouldBe true
    top.linesIterator.exists(_.startsWith("album-file")) shouldBe false
    top.linesIterator.exists(_.startsWith("rename-album")) shouldBe false

    val agent: String = helpOf("agent", "--help")
    agent shouldBe
      """agent  query the catalog and the archive
        |
        |stems  list one day's grouped stems
        |years  list archive years
        |months  list the months of one year
        |days  list the days of one month
        |claims  list album claims for one day
        |catalog  list catalog albums
        |items  list catalog items for one album
        |match  match catalog items to local stems
        |locate  list catalog rows that match one local stem
        |include  compress include lines
        |album-file  write an album file
        |rename-album  rename an album
        |set-description  set an item description
        |set-date  set an item date
        |download-item  download an item
        |add-to-album  add an item to an album
        |remove-from-album  remove an item from an album
        |move-item  move an item to another album
        |delete-item  delete an item
        |upload-item  upload one archive file into an album
        |create-album  create an album
        |delete-album  delete an album
        |drift  report create, update, gone, pending, pending-rename, and skipped
        |missing  list catalog items absent from the day folder""".stripMargin
    helpOf("agent", "-h") shouldBe agent
    helpOf("help", "agent") shouldBe agent
    agent should include("rename-album  rename an album")
    agent should include("set-description  set an item description")
    agent should include("set-date  set an item date")
    agent should include("download-item  download an item")
    agent should include("add-to-album  add an item to an album")
    agent should include("remove-from-album  remove an item from an album")
    agent should include("move-item  move an item to another album")
    agent should include("delete-item  delete an item")
    agent should include("upload-item  upload one archive file into an album")
    agent should include("create-album  create an album")
    agent should include("delete-album  delete an album")
    helpOf("agent", "rename-album", "--help") shouldBe
      """rename-album  rename an album
        |
        |Positionals:
        |none
        |
        |Flags:
        |--title  text  album title
        |--current  text  current title
        |--new  text  new title
        |--id  text  catalog album id""".stripMargin
    helpOf("agent", "set-description", "--help") shouldBe
      """set-description  set an item description
        |
        |Positionals:
        |none
        |
        |Flags:
        |--item  text  item key
        |--current  text  current filename
        |--new  text  new description""".stripMargin
    helpOf("agent", "set-date", "--help") shouldBe
      """set-date  set an item date
        |
        |Positionals:
        |none
        |
        |Flags:
        |--item  text  item key
        |--current  text  current filename
        |--new  text  new local date""".stripMargin
    helpOf("agent", "download-item", "--help") shouldBe
      """download-item  download an item
        |
        |Positionals:
        |none
        |
        |Flags:
        |--title  text  album title
        |--item  text  item key
        |--current  text  current filename
        |--id  text  catalog album id""".stripMargin
    helpOf("agent", "add-to-album", "--help") shouldBe
      """add-to-album  add an item to an album
        |
        |Positionals:
        |none
        |
        |Flags:
        |--title  text  album title
        |--item  text  item key
        |--current  text  current filename
        |--dest  text  destination album title
        |--id  text  catalog album id
        |--dest-id  text  destination album id""".stripMargin
    helpOf("agent", "remove-from-album", "--help") shouldBe
      """remove-from-album  remove an item from an album
        |
        |Positionals:
        |none
        |
        |Flags:
        |--title  text  album title
        |--item  text  item key
        |--current  text  current filename
        |--id  text  catalog album id""".stripMargin
    helpOf("agent", "move-item", "--help") shouldBe
      """move-item  move an item to another album
        |
        |Positionals:
        |none
        |
        |Flags:
        |--title  text  album title
        |--item  text  item key
        |--current  text  current filename
        |--dest  text  destination album title
        |--id  text  catalog album id
        |--dest-id  text  destination album id""".stripMargin
    helpOf("agent", "delete-item", "--help") shouldBe
      """delete-item  delete an item
        |
        |Positionals:
        |none
        |
        |Flags:
        |--title  text  album title
        |--item  text  item key
        |--current  text  current filename
        |--id  text  catalog album id""".stripMargin
    helpOf("agent", "upload-item", "--help") shouldBe
      """upload-item  upload one archive file into an album
        |
        |Positionals:
        |path  required
        |
        |Flags:
        |--dest  text  destination album title
        |--dest-id  text  destination album id""".stripMargin
    helpOf("agent", "create-album", "--help") shouldBe
      """create-album  create an album
        |
        |Positionals:
        |none
        |
        |Flags:
        |--title  text  album title""".stripMargin
    helpOf("agent", "delete-album", "--help") shouldBe
      """delete-album  delete an album
        |
        |Positionals:
        |none
        |
        |Flags:
        |--title  text  album title
        |--id  text  catalog album id""".stripMargin
    helpOf("agent", "album-file", "--help") should include("--id  text  catalog album id")
    helpOf("agent", "album-file", "--help") should not include ("--no-request")
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
    helpOf("agent", "locate", "--help") shouldBe
      """locate  list catalog rows that match one local stem
        |
        |Positionals:
        |day  required
        |stem  required
        |
        |Flags:
        |none""".stripMargin

  it should "reject usage errors" in:
    val errors: List[(List[String], String)] = List(
      List() -> "missing command",
      List("photo") -> "unknown command 'photo'",
      List("LS") -> "unknown command 'LS'",
      List("Lsd") -> "unknown command 'Lsd'",
      List("lsd") -> "unknown command 'lsd'",
      List("lsd", "2009") -> "unknown command 'lsd'",
      List("lsd", "--bogus") -> "unknown command 'lsd'",
      List("albums") -> "unknown command 'albums'",
      List("albums", "--bogus") -> "unknown command 'albums'",
      List("albums", "2009", "2010") -> "unknown command 'albums'",
      List("album") -> "missing album command",
      List("album", "foo") -> "unknown command 'foo'",
      List("album", "new") -> "missing span or name",
      List("album", "new", "--span", "2010/07", "extra") -> "extra argument 'extra'",
      List("album", "rm") -> "missing title",
      List("album", "rm", "--dry-run") -> "missing title",
      List("album", "mv", "2010/07") -> "missing new",
      List("album", "include", "2010/07") -> "missing picture",
      List("album", "rm", "2010/07", "--dry-run") -> "extra argument '--dry-run'",
      List("album", "include", "2010/07", "2010/07/04/2412", "--dry-run") -> "extra argument '--dry-run'",
      List("help", "album", "new", "extra") -> "extra argument 'extra'",
      List("help", "album", "nope") -> "unknown command 'nope'",
      List("ls", "--dry-run", "2009") -> "unknown option '--dry-run'",
      List("ls", "--fix", "2009") -> "unknown option '--fix'",
      List("repair") -> "missing path",
      List("repair", "2009") -> "missing repair action",
      List("repair", "--dry-run", "2009") -> "missing repair action",
      List("login", "--long") -> "unknown option '--long'",
      List("ls", "-abc", "2009") -> "unknown option '-abc'",
      List("ls", "-nfoo", "2009") -> "unknown option '-nfoo'",
      List("ls", "--Long", "2009") -> "unknown option '--Long'",
      List("ls", "--timestamp-from", "2009") -> "unknown option '--timestamp-from'",
      List("set-metadata") -> "unknown command 'set-metadata'",
      List("item") -> "missing item command",
      List("item", "nope") -> "unknown command 'nope'",
      List("item", "show") -> "missing picture",
      List("item", "set") -> "missing property",
      List("item", "set", "date=") -> "missing picture",
      List("item", "month") -> "missing picture",
      List("item", "into") -> "missing title",
      List("item", "into", "Best") -> "missing picture",
      List("item", "month", "2010/07/04/2412", "--dry-run") -> "extra argument '--dry-run'",
      List("album", "merge", "2010/07") -> "missing second",
      List("album", "merge", "2010/07", "2010/08", "--dry-run") -> "extra argument '--dry-run'",
      List("item", "show", "--dry-run", "2010/07/04/2412") -> "unknown option '--dry-run'",
      List("item", "set", "date=2010/08/01", "2010/07/04/2412", "--dry-run") -> "extra argument '--dry-run'",
      List("help", "item", "show", "extra") -> "extra argument 'extra'",
      List("login", "extra") -> "extra argument 'extra'",

      List("ls", "2009/03/10", "--fix") -> "extra argument '--fix'",
      List("ls", "path", "--help") -> "extra argument '--help'",
      List("ls", "path", "-h") -> "extra argument '-h'",
      List("publish", "2009", "--recursive") -> "extra argument '--recursive'",
      List("ls", "--long", "--long", "2009") -> "repeated option '--long'",
      List("ls", "--long", "-l", "2009") -> "repeated option '--long'",
      List("ls", "-l", "--long", "2009") -> "repeated option '--long'",
      List("publish", "--dry-run=yes") -> "invalid value 'yes' for '--dry-run'",
      List("ls", "--long=1", "2009") -> "invalid value '1' for '--long'",
      List("ls", "--long=on", "2009") -> "invalid value 'on' for '--long'",
      List("ls", "--long=", "2009") -> "invalid value '' for '--long'",
      List("ls", "--recursive=no") -> "invalid value 'no' for '--recursive'",
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
      List("agent", "months") -> "missing year",
      List("agent", "days") -> "missing month",
      List("agent", "claims") -> "missing path",
      List("agent", "locate") -> "missing day",
      List("agent", "locate", "2010/07/19") -> "missing stem",
      List("agent", "locate", "--id", "a1", "2010/07/19", "2412") -> "unknown option '--id'",
      List("agent", "years", "2010") -> "extra argument '2010'",
      List("agent", "items") -> "missing album",
      List("agent", "match") -> "missing album",
      List("agent", "catalog", "June") -> "extra argument 'June'",
      List("agent", "nope") -> "unknown command 'nope'",
      List("agent", "album-file", "2010/07", "--replace") -> "extra argument '--replace'",
      List("agent", "request") -> "unknown command 'request'",
      List("agent", "request", "--help") -> "unknown command 'request'",
      List("agent", "request", "-h") -> "unknown command 'request'",
      List("agent", "request", "rename-album") -> "unknown command 'request'",
      List("agent", "request", "rename-album", "--help") -> "unknown command 'request'",
      List("agent", "request", "set-description") -> "unknown command 'request'",
      List("agent", "request", "set-description", "--help") -> "unknown command 'request'",
      List("agent", "request", "set-date") -> "unknown command 'request'",
      List("agent", "request", "set-date", "--help") -> "unknown command 'request'",
      List("agent", "request", "download-item") -> "unknown command 'request'",
      List("agent", "request", "download-item", "--help") -> "unknown command 'request'",
      List("agent", "request", "move-item") -> "unknown command 'request'",
      List("agent", "request", "move-item", "--help") -> "unknown command 'request'",
      List("agent", "request", "delete-item") -> "unknown command 'request'",
      List("agent", "request", "delete-item", "--help") -> "unknown command 'request'",
      List("agent", "request", "upload-item") -> "unknown command 'request'",
      List("agent", "request", "upload-item", "--help") -> "unknown command 'request'",
      List("agent", "request", "--album", "a1", "rename-album") -> "unknown command 'request'",
      List("agent", "request", "create-album") -> "unknown command 'request'",
      List("agent", "request", "create-album", "--title", "Probe") -> "unknown command 'request'",
      List("agent", "request", "delete-album") -> "unknown command 'request'",
      List("agent", "request", "delete-album", "--help") -> "unknown command 'request'",
      List("help", "agent", "request") -> "extra argument 'request'",
      List("agent", "create-album", "--title") -> "missing value for '--title'",
      List("agent", "album-file", "--no-request", "2010/07") -> "unknown option '--no-request'",
      List("agent", "requests", "x") -> "unknown command 'requests'",
      List("agent", "results", "x") -> "unknown command 'results'",
      List("agent", "apply-requests") -> "unknown command 'apply-requests'"
    )
    errors.foreach: (args, message) =>
      withClue(args):
        Cli.parse(args) shouldBe Cli.Usage(message)

  "one-shot" should "refuse cd, pwd, and exit" in:
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
    Cli.parse(List("mcp")) shouldBe Cli.McpFront
    Cli.parse(List("mcp", "--help")) shouldBe Cli.Usage("extra argument '--help'")
    Cli.parse(List("mcp", "extra")) shouldBe Cli.Usage("extra argument 'extra'")
    Cli.parse(List("mcp"), Cli.Front.Shell) shouldBe Cli.Usage("already running")
    Cli.parse(List("mcp", "--help"), Cli.Front.Shell) shouldBe Cli.Usage("already running")
    Cli.parse(List("mcp"), Cli.Front.Serve) shouldBe Cli.Usage("already running")
    Cli.parse(List("mcp", "--help"), Cli.Front.Serve) shouldBe Cli.Usage("already running")

  "usageLine" should "prefix the program name" in:
    Cli.usageLine("missing command") shouldBe "Photo Archive Tool: missing command"
    Cli.usageLine("unknown option '--bogus'") shouldBe "Photo Archive Tool: unknown option '--bogus'"

  "toOperation" should "build the operations that do not read the archive" in:
    Cli.toOperation(Cli.LoginArgs) shouldBe Login
    Cli.toOperation(Cli.StatusArgs) shouldBe Status
    Cli.toOperation(Cli.LogoutArgs) shouldBe Logout
    Cli.toOperation(Cli.AlbumClaimsArgs(None)) shouldBe ListAlbums(None)
    Cli.toOperation(Cli.AlbumLsArgs(Some("2009"))) shouldBe AlbumLs(Some("2009"))
    Cli.toOperation(Cli.AlbumShowArgs("Best")) shouldBe AlbumShow("Best")
    Cli.toOperation(Cli.AlbumNewArgs(dryRun = true, Some("2010/07"), Some("Cape Cod"))) shouldBe
      AlbumNew(dryRun = true, Some("2010/07"), Some("Cape Cod"))
    Cli.toOperation(Cli.AlbumRmArgs(dryRun = false, "2010/07")) shouldBe AlbumRm(dryRun = false, "2010/07")
    Cli.toOperation(Cli.AlbumMvArgs(dryRun = false, "2010/07", "2010/08")) shouldBe
      AlbumMv(dryRun = false, "2010/07", "2010/08")
    Cli.toOperation(Cli.AlbumIncludeArgs(dryRun = false, "2010/07", Seq("2010/07/04/2412"))) shouldBe
      AlbumEdit(dryRun = false, exclude = false, "2010/07", Seq("2010/07/04/2412"))
    Cli.toOperation(Cli.AlbumExcludeArgs(dryRun = true, "Best", Seq("2010/07/04/2412"))) shouldBe
      AlbumEdit(dryRun = true, exclude = true, "Best", Seq("2010/07/04/2412"))
    Cli.toOperation(Cli.AlbumMergeArgs(dryRun = true, "2010/07", "2010/08")) shouldBe
      AlbumMerge(dryRun = true, "2010/07", "2010/08")
    Cli.toOperation(Cli.ItemMonthArgs(dryRun = false, Seq("2010/07/04/2412"))) shouldBe
      ItemMonth(dryRun = false, Seq("2010/07/04/2412"))
    Cli.toOperation(Cli.ItemIntoArgs(dryRun = true, "Best", Seq("2010/07/04/2412"))) shouldBe
      ItemInto(dryRun = true, "Best", Seq("2010/07/04/2412"))
    Cli.toOperation(Cli.PublishArgs(None, recursive = true, dryRun = false)) shouldBe
      Publish(None, recursive = true, dryRun = false)
    Cli.toOperation(Cli.SyncArgs(None, recursive = false, dryRun = true)) shouldBe
      Sync(None, recursive = false, dryRun = true)

  "parsePropertyAndValue" should "split on the last equals and use a real property name" in:
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
    val blank: IllegalArgumentException = the[IllegalArgumentException] thrownBy
      Cli.parsePropertyAndValue("uploader=   ").set(Metadata())
    blank.getMessage shouldBe "key 'uploader' must be a non-empty display name"
    val tab: IllegalArgumentException = the[IllegalArgumentException] thrownBy
      Cli.parsePropertyAndValue("uploader=Ada\t").set(Metadata())
    tab.getMessage shouldBe "key 'uploader' must not contain a tab, CR, or LF"

    an[IllegalArgumentException] should be thrownBy Cli.parsePropertyAndValue("nope")
    an[IllegalArgumentException] should be thrownBy Cli.parsePropertyAndValue("source=a=b")

  private def helpOf(args: String*): String = Cli.parse(args.toList) match
    case Cli.Help(text) => text
    case other          => fail(s"expected help for ${args.toList}, got $other")

  private def ls(
    path: Option[String] = None,
    long: Boolean = false,
    recursive: Boolean = false,
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
  ): Cli.Outcome = Cli.Run(
    Cli.Ls(
      path,
      long,
      recursive,
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
    )
  )

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

// Subset the read tools advertise. No $ref. A null in a null|object union skips properties.
private[photo] def outputMismatch(schema: Json, value: Json): Option[String] =
  def mismatch(current: Json, sample: Json, path: String): Option[String] = current match
    case Json.Obj(fields) =>
      typeNames(fields) match
        case Left(message) => Some(s"$path $message")
        case Right(names)  =>
          names.find(name => accepts(name, sample)) match
            case None =>
              Some(s"$path is not ${names.mkString(" or ")}: $sample")
            case Some("object") =>
              objectMismatch(fields, sample, path)
            case Some("array") =>
              arrayMismatch(fields, sample, path)
            case Some(_) =>
              None
    case _ =>
      Some(s"$path schema is not an object")

  def typeNames(fields: Seq[(String, Json)]): Either[String, Seq[String]] =
    fields.collectFirst { case ("type", typed) => typed } match
      case Some(Json.Str(name))                   => Right(Seq(name))
      case Some(Json.Arr(items)) if items.isEmpty => Left("type union is empty")
      case Some(Json.Arr(items))                  =>
        items.foldLeft[Either[String, Seq[String]]](Right(Seq.empty)):
          case (Left(message), _)           => Left(message)
          case (Right(acc), Json.Str(name)) => Right(acc :+ name)
          case (Right(_), _)                => Left("type union is not strings")
      case Some(_) => Left("type is not a string or array")
      case None    => Left("has no type")

  def accepts(name: String, sample: Json): Boolean = (name, sample) match
    case ("string", Json.Str(_))   => true
    case ("integer", Json.Long(_)) => true
    case ("boolean", Json.Bool(_)) => true
    case ("array", Json.Arr(_))    => true
    case ("object", Json.Obj(_))   => true
    case ("null", Json.Null)       => true
    case _                         => false

  def objectMismatch(fields: Seq[(String, Json)], sample: Json, path: String): Option[String] =
    sample match
      case Json.Obj(props) =>
        propertiesOf(fields, path) match
          case Left(message)     => Some(message)
          case Right(properties) =>
            closedOf(fields, path) match
              case Left(message) => Some(message)
              case Right(closed) =>
                requiredOf(fields, path) match
                  case Left(message)   => Some(message)
                  case Right(required) =>
                    val unknown: Option[String] =
                      if !closed then None
                      else
                        props.collectFirst:
                          case (key, _) if !properties.exists(_._1 == key) => s"$path.$key is not allowed"
                    val missing: Option[String] = required.collectFirst:
                      case name if !props.exists(_._1 == name) => s"$path.$name is required"
                    val nested: Option[String] = props.foldLeft[Option[String]](None):
                      case (Some(message), _)   => Some(message)
                      case (None, (key, child)) =>
                        properties.find(_._1 == key) match
                          case None                   => None
                          case Some((_, childSchema)) => mismatch(childSchema, child, s"$path.$key")
                    unknown.orElse(missing).orElse(nested)
      case _ =>
        Some(s"$path is not an object")

  def propertiesOf(fields: Seq[(String, Json)], path: String): Either[String, Seq[(String, Json)]] =
    fields.collectFirst { case ("properties", value) => value } match
      case Some(Json.Obj(nested)) => Right(nested)
      case Some(_)                => Left(s"$path properties is not an object")
      case None                   => Left(s"$path has no properties")

  def closedOf(fields: Seq[(String, Json)], path: String): Either[String, Boolean] =
    fields.collectFirst { case ("additionalProperties", value) => value } match
      case None                     => Right(false)
      case Some(Json.Bool(allowed)) => Right(!allowed)
      case Some(_)                  => Left(s"$path additionalProperties is not a boolean")

  def requiredOf(fields: Seq[(String, Json)], path: String): Either[String, Seq[String]] =
    fields.collectFirst { case ("required", value) => value } match
      case None                  => Right(Seq.empty)
      case Some(Json.Arr(items)) =>
        items.foldLeft[Either[String, Seq[String]]](Right(Seq.empty)):
          case (Left(message), _)           => Left(message)
          case (Right(acc), Json.Str(name)) => Right(acc :+ name)
          case (Right(_), _)                => Left(s"$path required entry is not a string")
      case Some(_) =>
        Left(s"$path required is not an array")

  def arrayMismatch(fields: Seq[(String, Json)], sample: Json, path: String): Option[String] =
    sample match
      case Json.Arr(items) =>
        fields.collectFirst { case ("items", itemSchema) => itemSchema } match
          case None             => Some(s"$path has no items")
          case Some(itemSchema) =>
            items.zipWithIndex.foldLeft[Option[String]](None):
              case (Some(message), _)    => Some(message)
              case (None, (item, index)) => mismatch(itemSchema, item, s"$path[$index]")
      case _ =>
        Some(s"$path is not an array")

  mismatch(schema, value, "$")
