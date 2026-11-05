package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NFiles, StandardCopyOption}
import java.time.LocalDate

class RefreshPlanTest extends AnyFlatSpec, Matchers:
  private val Day: Option[LocalDate] = Some(LocalDate.parse("2010-06-01"))

  "RefreshPlan" should "carry auto-add by id and not item-read it" in:
    val stored: Catalog.Album = album("aa", 10, "Auto", autoAdd = true, owner = Some("Ada: Owner"))
    val fresh: Catalog.Album = album("nu", 1, "New", autoAdd = true, items = Seq(item("one.jpg", "k-new")))
    val reads: Map[String, Catalog.Album] = Map(
      "aa" -> album("aa", 99, "ignored", items = Seq(item("nope.jpg", "k-no")), owner = Some("Bea")),
      "nu" -> fresh
    )
    val prepared: RefreshPlan.Prepared = accepted(
      Seq(line("aa", "12", "Auto 2"), line("nu", "1", "New")),
      Seq(stored),
      RefreshPlan.Mode.Incremental,
      allowShrink = true,
      reads
    )
    prepared.diff.itemReads shouldBe Set("nu")
    val countChanges: Seq[String] = prepared.diff.actions.collect:
      case RefreshPlan.Action.CountChange(_, card) => card.id
    countChanges shouldBe Seq("aa")
    val carried: Catalog.Album = prepared.albums.head
    carried.autoAdd shouldBe true
    carried.count shouldBe 12
    carried.title shouldBe "Auto 2"
    carried.owner shouldBe Some("Bea")
    carried.items shouldBe Seq.empty[Catalog.Item]
    carried.unsets shouldBe Seq.empty[Catalog.Unset]
    carried.error shouldBe None
    val added: Catalog.Album = prepared.albums(1)
    added.autoAdd shouldBe false
    added.items shouldBe fresh.items
    added.count shouldBe 1
    roundTrip(prepared)

  it should "copy an error line through and not re-read a same-count album" in:
    // Shortfall order is album, then the item, then the error. A keep must not rebuild that block.
    val keepBlock: Seq[String] = Seq(
      "album\ter\t4\tJune ",
      "item\ter\t one.jpg \t2010-06-01\tk-i\t Ada ",
      "error\ter\t1\t4\t0"
    )
    val retitleBlock: Seq[String] = Seq(
      "item\trt\tstay.jpg\t2010-06-01\tk-stay\t-",
      "album\trt\t1\tOld"
    )
    val source: String = catalog(keepBlock ++ retitleBlock ++ Seq("album\tgone\t0\tGone"))
    val stored: Seq[Catalog.Album] = readBack(source)
    val kept: Catalog.Album = stored.head
    val renamed: Catalog.Album = stored(1)
    val dropped: Catalog.Album = stored(2)
    val reads: Map[String, Catalog.Album] = Map(
      "er" -> album("er", 4, "June ", items = Seq(item("swap.jpg", "k-swap"))),
      "rt" -> album("rt", 1, "New", items = Seq(item("other.jpg", "k-other")))
    )
    val prepared: RefreshPlan.Prepared = accepted(
      Seq(line("er", "4", "June "), line("rt", "1", "New")),
      stored,
      RefreshPlan.Mode.Incremental,
      allowShrink = true,
      reads,
      source
    )
    prepared.diff.itemReads shouldBe Set.empty[String]
    prepared.diff.actions shouldBe Seq(
      RefreshPlan.Action.Keep(kept, RefreshPlan.Card("er", 4, "June ")),
      RefreshPlan.Action.Retitle(renamed, RefreshPlan.Card("rt", 1, "New")),
      RefreshPlan.Action.Drop(dropped)
    )
    prepared.albums.map(_.id) shouldBe Seq("er", "rt")
    prepared.albums.head shouldBe kept
    prepared.albums(1) shouldBe renamed.copy(title = "New")
    prepared.text shouldBe catalog(
      keepBlock ++ Seq(
        "item\trt\tstay.jpg\t2010-06-01\tk-stay\t-",
        "album\trt\t1\tNew"
      )
    )
    roundTrip(prepared)

  it should "keep survivor order and append new albums in card order" in:
    val stored: Seq[Catalog.Album] = Seq(album("b", 0, "B"), album("gone", 0, "G"), album("a", 0, "A"))
    val prepared: RefreshPlan.Prepared = accepted(
      Seq(line("a", "0", "A"), line("z", "0", "Z"), line("b", "0", "B"), line("m", "0", "M")),
      stored,
      RefreshPlan.Mode.Incremental,
      allowShrink = true
    )
    prepared.albums.map(_.id) shouldBe Seq("b", "a", "z", "m")

  it should "replace only named albums" in:
    val stayBlock: Seq[String] = Seq(
      "album\ta\t4\tA",
      "item\ta\ta.jpg\t2010-06-01\tk-a\t-",
      "error\ta\t1\t4\t0"
    )
    val absentBlock: Seq[String] = Seq(
      "album\tz\t2\tZed",
      "item\tz\tz.jpg\t2010-06-01\tk-z\t-",
      "error\tz\t1\t2\t0"
    )
    val source: String = catalog(
      stayBlock ++ Seq(
        "album\tb\t1\tB",
        "item\tb\tb.jpg\t2010-06-01\tk-b\t-"
      ) ++ absentBlock
    )
    val stored: Seq[Catalog.Album] = readBack(source)
    val prepared: RefreshPlan.Prepared = accepted(
      Seq(line("b", "0", "B2"), line("c", "0", "C"), line("e", "0", "E")),
      stored,
      RefreshPlan.Mode.Album(Seq("B2", "C")),
      allowShrink = false,
      source = source
    )
    prepared.diff.actions.collect { case RefreshPlan.Action.Drop(album) => album.id } shouldBe
      Seq.empty[String]
    prepared.diff.itemReads shouldBe Set("b", "c")
    prepared.albums.map(_.id) shouldBe Seq("a", "b", "z", "c")
    prepared.albums.head shouldBe stored.head
    prepared.albums(2) shouldBe stored(2)
    prepared.albums(1).title shouldBe "B2"
    prepared.albums(1).count shouldBe 0
    prepared.albums(1).items shouldBe Seq.empty[Catalog.Item]
    prepared.albums(3) shouldBe album("c", 0, "C")
    prepared.text shouldBe catalog(stayBlock ++ Seq("album\tb\t0\tB2") ++ absentBlock ++ Seq("album\tc\t0\tC"))

  it should "re-read a non-auto-add album when the card count changes" in:
    val stored: Catalog.Album = album("a", 1, "A", items = Seq(item("old.jpg", "k-old")))
    val read: Catalog.Album = album(
      "a",
      9,
      "ignored",
      owner = Some("Ada"),
      items = Seq(item("one.jpg", "k-1"), item("two.jpg", "k-2"))
    )
    val prepared: RefreshPlan.Prepared = accepted(
      Seq(line("a", "2", "A")),
      Seq(stored),
      RefreshPlan.Mode.Incremental,
      allowShrink = true,
      Map("a" -> read)
    )
    prepared.diff.itemReads shouldBe Set("a")
    prepared.diff.actions shouldBe Seq(RefreshPlan.Action.CountChange(stored, RefreshPlan.Card("a", 2, "A")))
    prepared.albums shouldBe Seq(read.copy(count = 2, title = "A"))
    roundTrip(prepared)

  it should "item-read every non-auto-add album on a full pass" in:
    val kept: Catalog.Album = album("k", 1, "K", items = Seq(item("old.jpg", "k-old")))
    val auto: Catalog.Album = album("aa", 3, "Auto", autoAdd = true, owner = Some("Ada"))
    val read: Catalog.Album = album("k", 1, "K", items = Seq(item("new.jpg", "k-new")))
    val prepared: RefreshPlan.Prepared = accepted(
      Seq(line("k", "1", "K"), line("aa", "4", "Auto")),
      Seq(kept, auto),
      RefreshPlan.Mode.Full,
      allowShrink = true,
      Map(
        "k" -> read,
        "aa" -> album("aa", 4, "Auto", owner = Some("Bea"), items = Seq(item("nope.jpg", "k-no")))
      )
    )
    prepared.diff.itemReads shouldBe Set("k")
    prepared.albums.head.items shouldBe read.items
    prepared.albums(1).autoAdd shouldBe true
    prepared.albums(1).count shouldBe 4
    prepared.albums(1).items shouldBe Seq.empty[Catalog.Item]
    prepared.albums(1).owner shouldBe Some("Bea")

  it should "turn a tab or newline in an owner or uploader into one space" in:
    val stored: Catalog.Album = album(
      "a",
      1,
      "A",
      owner = Some(" Ada\tBee\n"),
      items = Seq(Catalog.Item("a.jpg", None, "k-a", Some("Ann\rBob")))
    )
    val prepared: RefreshPlan.Prepared = accepted(
      Seq(line("a", "1", "A")),
      Seq(stored),
      RefreshPlan.Mode.Incremental,
      allowShrink = true
    )
    val back: Catalog.Album = readBack(prepared.text).head
    back.owner shouldBe Some(" Ada Bee ")
    back.items.head.uploader shouldBe Some("Ann Bob")
    back.items.head.filename shouldBe "a.jpg"

  it should "refuse an empty count, a bad row, a short list, and a duplicate id" in:
    withTemp: dir =>
      write(dir, "catalog.txt", "sentinel\n")
      refused(dir, Seq("only-two\tfields"), "bad row")
      refused(dir, Seq(line("a", "", "A")), "empty count")
      refused(dir, Seq.tabulate(249)(i => line(s"n$i", "0", s"T$i")), "rows < 250")
      refused(dir, Seq(line("a", "0", "A"), line("a", "0", "A2")), "repeated card id")
      refused(dir, Seq(line("a", "01", "A")), "bad row")
      NFiles.readString(File(dir, "catalog.txt").toPath) shouldBe "sentinel\n"
      File(dir, "catalog.txt.partial").exists shouldBe false

  it should "let --allow-shrink skip only the row floor" in:
    val stored: Seq[Catalog.Album] = Seq.tabulate(20)(i => album(s"old$i", 0, s"Old$i"))
    val short: Seq[String] = Seq.tabulate(249)(i => line(s"n$i", "0", s"T$i"))
    RefreshPlan.diff(short, stored, RefreshPlan.Mode.Incremental, allowShrink = false) shouldBe
      Left("rows < 250")
    val shrunk: RefreshPlan.Diff =
      RefreshPlan.diff(short, stored, RefreshPlan.Mode.Incremental, allowShrink = true).fold(fail(_), identity)
    shrunk.actions.collect { case RefreshPlan.Action.Drop(_) => () }.length shouldBe 20
    RefreshPlan.diff(short :+ "bad", stored, RefreshPlan.Mode.Incremental, allowShrink = true) shouldBe
      Left("bad row")
    RefreshPlan.diff(
      short.updated(0, line("n0", "", "T0")),
      stored,
      RefreshPlan.Mode.Incremental,
      allowShrink = true
    ) shouldBe Left("empty count")
    val enough: Seq[String] = Seq.tabulate(250)(i => line(s"n$i", "0", s"T$i"))
    RefreshPlan.diff(enough, stored, RefreshPlan.Mode.Incremental, allowShrink = false).isRight shouldBe true

  it should "not use the row floor for --album" in:
    RefreshPlan
      .diff(Seq(line("a", "0", "A")), Seq.empty, RefreshPlan.Mode.Album(Seq("A")), allowShrink = false)
      .isRight shouldBe true
    RefreshPlan.diff(Seq(line("a", "", "A")), Seq.empty, RefreshPlan.Mode.Album(Seq("A")), allowShrink = false) shouldBe
      Left("empty count")
    RefreshPlan.diff(Seq("bad"), Seq.empty, RefreshPlan.Mode.Album(Seq("A")), allowShrink = true) shouldBe
      Left("bad row")
    RefreshPlan.diff(
      Seq(line("a", "1", "Same"), line("b", "1", "Other")),
      Seq.empty,
      RefreshPlan.Mode.Album(Seq("Missing")),
      allowShrink = false
    ) shouldBe Left("named card is missing")
    RefreshPlan.diff(
      Seq(line("a", "1", "Same"), line("b", "1", "Same")),
      Seq.empty,
      RefreshPlan.Mode.Album(Seq("Same")),
      allowShrink = false
    ) shouldBe Left("ambiguous title")
    RefreshPlan.diff(
      Seq(line("card", "1", "Same")),
      Seq(album("one", 0, "Same"), album("two", 0, "Same")),
      RefreshPlan.Mode.Album(Seq("Same")),
      allowShrink = false
    ) shouldBe Left("ambiguous title")

  it should "publish by rename and leave the live file when a check fails" in:
    withTemp: dir =>
      val live: File = write(dir, "catalog.txt", "sentinel\n")
      write(dir, "catalog.txt.previous", "old-previous\n")
      val prepared: RefreshPlan.Prepared = accepted(
        Seq(line("a", "0", "A")),
        Seq.empty,
        RefreshPlan.Mode.Incremental,
        allowShrink = true
      )
      RefreshPlan.publish(dir, prepared) shouldBe Right(())
      NFiles.readString(live.toPath) shouldBe prepared.text
      Catalog.read(live) shouldBe prepared.albums
      File(dir, "catalog.txt.partial").exists shouldBe false
      File(dir, "catalog.txt.previous").exists shouldBe false

      write(dir, "catalog.txt", "sentinel\n")
      val failed: Either[String, RefreshPlan.Prepared] = RefreshPlan.run(
        dir,
        Seq(line("a", "3", "A")),
        Seq.empty,
        RefreshPlan.Mode.Incremental,
        allowShrink = true
      )
      failed shouldBe Left("item count does not equal count")
      NFiles.readString(live.toPath) shouldBe "sentinel\n"
      File(dir, "catalog.txt.partial").exists shouldBe true
      File(dir, "catalog.txt.previous").exists shouldBe false

  it should "put the previous file back when the second rename fails" in:
    withTemp: dir =>
      val live: File = write(dir, "catalog.txt", "sentinel\n")
      val prepared: RefreshPlan.Prepared = accepted(
        Seq(line("a", "0", "A")),
        Seq.empty,
        RefreshPlan.Mode.Incremental,
        allowShrink = true
      )
      var step: Int = 0
      val move: (File, File) => Unit = (from, to) =>
        step += 1
        if step == 2 then throw java.io.IOException("second rename")
        if to.exists then NFiles.delete(to.toPath)
        NFiles.move(from.toPath, to.toPath, StandardCopyOption.ATOMIC_MOVE)
      RefreshPlan.publish(dir, prepared, move) shouldBe Left("second rename")
      NFiles.readString(live.toPath) shouldBe "sentinel\n"
      File(dir, "catalog.txt.partial").exists shouldBe true
      File(dir, "catalog.txt.previous").exists shouldBe false
      NFiles.readString(File(dir, "catalog.txt.partial").toPath) shouldBe prepared.text

  it should "report a missing catalog when the restore move fails" in:
    withTemp: dir =>
      val live: File = write(dir, "catalog.txt", "sentinel\n")
      val prepared: RefreshPlan.Prepared = accepted(
        Seq(line("a", "0", "A")),
        Seq.empty,
        RefreshPlan.Mode.Incremental,
        allowShrink = true
      )
      var step: Int = 0
      val move: (File, File) => Unit = (from, to) =>
        step += 1
        if step == 2 then throw java.io.IOException("second rename")
        if step == 3 then throw java.io.IOException("restore failed")
        if to.exists then NFiles.delete(to.toPath)
        NFiles.move(from.toPath, to.toPath, StandardCopyOption.ATOMIC_MOVE)
      RefreshPlan.publish(dir, prepared, move) shouldBe
        Left("second rename; catalog.txt is missing: restore failed")
      live.exists shouldBe false
      NFiles.readString(File(dir, "catalog.txt.previous").toPath) shouldBe "sentinel\n"
      File(dir, "catalog.txt.partial").exists shouldBe true

  it should "reject a mark, a CR, and a short line before rename" in:
    withTemp: dir =>
      val prepared: RefreshPlan.Prepared = accepted(
        Seq(line("a", "0", "A")),
        Seq.empty,
        RefreshPlan.Mode.Incremental,
        allowShrink = true
      )
      write(dir, "catalog.txt", "sentinel\n")
      val marked: File = write(dir, "marked.txt", "# gphoto-catalog 2\n## retired\n")
      RefreshPlan.check(marked, prepared) shouldBe Left("unknown catalog line")
      val cr: File = write(dir, "cr.txt", "# gphoto-catalog 2\nalbum\ta\t0\tA\r\n")
      RefreshPlan.check(cr, prepared) shouldBe Left("carriage return")
      val bom: Array[Byte] = Array(0xef.toByte, 0xbb.toByte, 0xbf.toByte) ++
        prepared.text.getBytes(StandardCharsets.UTF_8)
      RefreshPlan.check(writeBytes(dir, "bom.txt", bom), prepared) shouldBe Left("bom")
      val partial: File = write(dir, "short.txt", "# gphoto-catalog 2\nalbum\ta\t0\tA")
      RefreshPlan.check(partial, prepared) shouldBe Left("trailing partial line")
      val tabbed: Catalog.Album = album("a", 1, "A", items = Seq(Catalog.Item("a\tb.jpg", None, "k", None)))
      val bad: RefreshPlan.Prepared = accepted(
        Seq(line("a", "1", "A")),
        Seq.empty,
        RefreshPlan.Mode.Incremental,
        allowShrink = true,
        Map("a" -> tabbed)
      )
      RefreshPlan.publish(dir, bad) shouldBe Left("wrong column count")
      NFiles.readString(File(dir, "catalog.txt").toPath) shouldBe "sentinel\n"

  it should "treat a count of 0 with no items as complete" in:
    val prepared: RefreshPlan.Prepared = accepted(
      Seq(line("a", "0", "A")),
      Seq.empty,
      RefreshPlan.Mode.Incremental,
      allowShrink = true
    )
    prepared.albums shouldBe Seq(album("a", 0, "A"))
    roundTrip(prepared)

  it should "require the card id set on a full pass and only the named ids on --album" in:
    withTemp: dir =>
      val wide: RefreshPlan.Prepared = accepted(
        Seq(line("a", "0", "A"), line("b", "0", "B")),
        Seq.empty,
        RefreshPlan.Mode.Incremental,
        allowShrink = true
      )
      val onlyA: File = write(dir, "only-a.txt", RefreshPlan.render(Seq(album("a", 0, "A"))))
      RefreshPlan.check(onlyA, wide) shouldBe Left("album set")
      val named: RefreshPlan.Prepared = accepted(
        Seq(line("a", "0", "A"), line("b", "0", "B")),
        Seq(album("a", 0, "A"), album("z", 0, "Z")),
        RefreshPlan.Mode.Album(Seq("A")),
        allowShrink = false
      )
      RefreshPlan.check(onlyA, named) shouldBe Right(())

  private def accepted(
    lines: Seq[String],
    stored: Seq[Catalog.Album],
    mode: RefreshPlan.Mode,
    allowShrink: Boolean,
    reads: Map[String, Catalog.Album] = Map.empty,
    source: String = ""
  ): RefreshPlan.Prepared =
    RefreshPlan.plan(lines, stored, mode, allowShrink, reads, source).fold(message => fail(message), identity)

  private def catalog(lines: Seq[String]): String =
    (Seq("# gphoto-catalog 2") ++ lines).mkString("\n") + "\n"

  private def refused(dir: File, lines: Seq[String], message: String): Unit =
    RefreshPlan.run(dir, lines, Seq.empty, RefreshPlan.Mode.Incremental, allowShrink = false) shouldBe
      Left(message)

  private def roundTrip(prepared: RefreshPlan.Prepared): Unit =
    readBack(prepared.text) shouldBe prepared.albums

  private def readBack(text: String): Seq[Catalog.Album] =
    val dir: File = NFiles.createTempDirectory("gphoto-refresh").toFile
    try Catalog.read(write(dir, "catalog.txt", text))
    finally remove(dir)

  private def line(id: String, count: String, title: String): String =
    s"$title\t$count\t$id\t\t[]"

  private def item(filename: String, key: String): Catalog.Item =
    Catalog.Item(filename, Day, key, None)

  private def album(
    id: String,
    count: Int,
    title: String,
    owner: Option[String] = None,
    autoAdd: Boolean = false,
    error: Option[Catalog.Error] = None,
    items: Seq[Catalog.Item] = Seq.empty,
    unsets: Seq[Catalog.Unset] = Seq.empty
  ): Catalog.Album =
    Catalog.Album(id, count, title, owner, autoAdd, error, items, unsets)

  private def write(dir: File, name: String, text: String): File =
    writeBytes(dir, name, text.getBytes(StandardCharsets.UTF_8))

  private def writeBytes(dir: File, name: String, bytes: Array[Byte]): File =
    val file: File = File(dir, name)
    NFiles.write(file.toPath, bytes)
    file

  private def withTemp(body: File => Unit): Unit =
    val dir: File = NFiles.createTempDirectory("gphoto-refresh").toFile
    try body(dir)
    finally remove(dir)

  private def remove(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(remove))
    val _: Boolean = file.delete()
