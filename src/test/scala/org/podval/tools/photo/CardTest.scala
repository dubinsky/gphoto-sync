package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import scala.collection.mutable.ArrayBuffer
import java.io.File
import java.nio.file.{Files as JFiles, LinkOption}
import java.nio.file.attribute.FileTime
import java.time.ZoneId

class CardTest extends AnyFlatSpec, Matchers:
  @targetName("resultCanEqual")
  private given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  @targetName("jsonCanEqual")
  private given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")

  private val mountArgs: Seq[String] =
    Seq("udisksctl", "mount", "-b", "/dev/sdb1", "-o", "ro", "--no-user-interaction")

  "card" should "parse fixture lsblk JSON and not call the mount runner unless one card is unmounted" in :
    val none = probe(Seq(doc(nvme)))
    none.mounts shouldBe Seq.empty[Seq[String]]
    none.text shouldBe ""
    failure(none.thrown) shouldBe "no memory card"

    val unmounted = canon("[]", fstype = "\"EXFAT\"")
    val dry = probe(Seq(unmounted), dryRun = true)
    dry.mounts shouldBe Seq.empty[Seq[String]]
    dry.text shouldBe line("/dev/sdb1", "CANON", "unmounted", "exfat", "unknown")
    dry.thrown.isEmpty shouldBe true
    dry.root.isEmpty shouldBe true

    withTemp: mounted =>
      JFiles.createDirectories(File(mounted, "dcim").toPath)
      val found = probe(Seq(canon(mounts(mounted), fstype = "\"vfat\"")))
      found.mounts shouldBe Seq.empty[Seq[String]]
      found.text shouldBe line("/dev/sdb1", "CANON", mounted.getPath, "vfat", "yes")
      found.thrown.isEmpty shouldBe true
      found.root.map(_.getPath) shouldBe Some(mounted.getPath)

    withTemp: empty =>
      val missing = probe(Seq(canon(mounts(empty), fstype = "\"exfat\"", label = "\"\"")))
      missing.mounts shouldBe Seq.empty[Seq[String]]
      missing.text shouldBe line("/dev/sdb1", "", empty.getPath, "exfat", "no")
      failure(missing.thrown) shouldBe "no memory card"

    val two = probe(Seq(doc(
      volume("/dev/sdc1", label = "\"B\""),
      volume("/dev/sdb1", label = "\"A\"")
    )))
    two.mounts shouldBe Seq.empty[Seq[String]]
    two.text shouldBe
      line("/dev/sdb1", "A", "unmounted", "exfat", "unknown") +
        line("/dev/sdc1", "B", "unmounted", "exfat", "unknown")
    failure(two.thrown) shouldBe "more than one memory card"

    withTemp: yes =>
      JFiles.createDirectories(File(yes, "DCIM").toPath)
      val both = probe(Seq(doc(
        volume("/dev/sdc1"),
        volume("/dev/sdb1", mountpoints = mounts(yes), fstype = "\"vfat\"")
      )))
      both.mounts shouldBe Seq.empty[Seq[String]]
      both.text shouldBe
        line("/dev/sdb1", "CANON", yes.getPath, "vfat", "yes") +
          line("/dev/sdc1", "CANON", "unmounted", "exfat", "unknown")
      failure(both.thrown) shouldBe "more than one memory card"

    probe(Seq(doc(volume("/dev/sdb1", rm = "1"))), dryRun = true).thrown.isEmpty shouldBe true
    probe(Seq(doc(volume("/dev/sdb1", rm = "\"1\""))), dryRun = true).thrown.isEmpty shouldBe true
    val disk = probe(Seq(doc(volume("/dev/sdb", fstype = "\"exfat\"", nodeType = "disk"))), dryRun = true)
    disk.text shouldBe line("/dev/sdb", "CANON", "unmounted", "exfat", "unknown")
    disk.thrown.isEmpty shouldBe true

    val multi = probe(Seq(doc(volume("/dev/sdb1", mountpoints = """["/a","/b"]"""))))
    multi.mounts shouldBe Seq.empty[Seq[String]]
    multi.text shouldBe ""
    failure(multi.thrown) shouldBe "more than one mountpoint: /dev/sdb1"

    withTemp: other =>
      JFiles.createDirectories(File(other, "DCIM").toPath)
      val mixed = probe(Seq(doc(
        volume("/dev/sdb1", mountpoints = """["/a","/b"]"""),
        volume("/dev/sdc1", mountpoints = mounts(other), fstype = "\"vfat\"")
      )))
      mixed.mounts shouldBe Seq.empty[Seq[String]]
      mixed.text shouldBe line("/dev/sdc1", "CANON", other.getPath, "vfat", "yes")
      failure(mixed.thrown) shouldBe "more than one mountpoint: /dev/sdb1"

    val broken = probe(Seq(canon("[]", label = jsonString("A\nB"))))
    broken.mounts shouldBe Seq.empty[Seq[String]]
    broken.text shouldBe ""
    failure(broken.thrown) shouldBe "label or mountpoint is not one line: /dev/sdb1"

    val quoted = "MY \"CARD\""
    val spaces = probe(Seq(canon("[]", label = jsonString(quoted))), dryRun = true)
    spaces.text shouldBe line("/dev/sdb1", quoted, "unmounted", "exfat", "unknown")
    spaces.thrown.isEmpty shouldBe true

    withTemp: root =>
      val target = File(root, "real")
      JFiles.createDirectories(target.toPath)
      JFiles.createSymbolicLink(File(root, "DCIM").toPath, target.toPath)
      val link = probe(Seq(canon(mounts(root))))
      link.mounts shouldBe Seq.empty[Seq[String]]
      link.text shouldBe line("/dev/sdb1", "CANON", root.getPath, "exfat", "no")
      failure(link.thrown) shouldBe "no memory card"

    withTemp: root =>
      JFiles.createDirectories(File(root, "DCIM").toPath)
      val singular =
        s"""{"blockdevices":[{"name":"sdb1","path":"/dev/sdb1","rm":true,"type":"disk","fstype":"vfat","label":null,"mountpoint":${jsonString(root.getPath)}}]}
           |""".stripMargin
      val found = probe(Seq(singular))
      found.mounts shouldBe Seq.empty[Seq[String]]
      found.text shouldBe line("/dev/sdb1", "", root.getPath, "vfat", "yes")
      found.thrown.isEmpty shouldBe true

    failure(probe(Seq("nope")).thrown) shouldBe "lsblk output was not JSON"
    failure(probe(Seq("""{"blockdevices":{}}""")).thrown) shouldBe "lsblk output was not JSON"
    failure(probe(Seq("""{"nope":[]}""")).thrown) shouldBe "lsblk output was not JSON"
    failure(probe(Seq("""{"blockdevices":[]} trailing""")).thrown) shouldBe "lsblk output was not JSON"
    val failed = probe(Seq.empty)
    failed.mounts shouldBe Seq.empty[Seq[String]]
    failed.text shouldBe ""
    failure(failed.thrown) shouldBe "lsblk failed"

  it should "mount one unmounted card and read the mountpoint from the second document" in :
    withTemp: root =>
      JFiles.createDirectories(File(root, "DCIM").toPath)
      val found = probe(Seq(canon("[]"), canon(mounts(root), fstype = "\"vfat\"")))
      found.mounts shouldBe Seq(mountArgs)
      found.text shouldBe line("/dev/sdb1", "CANON", root.getPath, "vfat", "yes")
      found.thrown.isEmpty shouldBe true
      found.root.map(_.getPath) shouldBe Some(root.getPath)

      val raced = probe(
        Seq(canon("[]"), canon(mounts(root), fstype = "\"vfat\"")),
        mountResult = (1, "nope\n")
      )
      raced.mounts shouldBe Seq(mountArgs)
      raced.thrown.isEmpty shouldBe true
      raced.text shouldBe line("/dev/sdb1", "CANON", root.getPath, "vfat", "yes")

    withTemp: empty =>
      val still = probe(Seq(canon("[]"), canon("[]")), mountResult = (1, ""))
      still.mounts shouldBe Seq(mountArgs)
      still.text shouldBe ""
      failure(still.thrown) shouldBe "mount failed: /dev/sdb1"

      val denied = probe(Seq(canon("[]"), canon("[]")), mountResult = (1, "denied\nmore\n"))
      failure(denied.thrown) shouldBe "mount failed: /dev/sdb1: denied"

    withTemp: other =>
      val first = doc(volume("/dev/sda1", mountpoints = mounts(other)), volume("/dev/sdb1"))
      val second = doc(volume("/dev/sda1", mountpoints = mounts(other)), volume("/dev/sdb1"))
      val found = probe(Seq(first, second), mountResult = (1, "denied\r\nmore\n"))
      found.mounts shouldBe Seq(mountArgs)
      found.text shouldBe line("/dev/sda1", "CANON", other.getPath, "exfat", "no")
      failure(found.thrown) shouldBe "mount failed: /dev/sdb1: denied"
      found.text should not include ("/dev/sdb1")

    var calls: Int = 0
    val again = Transcript.collect:
      Card.discover(
        dryRun = false,
        lsblk = () =>
          calls += 1
          if calls == 1 then Right(canon("[]")) else Left("x"),
        mount = _ => (0, "")
      )
    calls shouldBe 2
    again._1 shouldBe ""
    failure(again._2) shouldBe "lsblk failed"

  "card copy" should "copy DCIM picture files flat into .upload and leave the card" in :
    withTemp: archive =>
      withTemp: card =>
        val source: File = write(card, "DCIM/100CANON/IMG_0001.JPG", "same-bytes")
        write(card, "DCIM/IMG_0001.JPG", "same-bytes")
        write(card, "DCIM/100CANON/IMG_0002.JPG", "longer-bytes")
        write(card, "DCIM/100CANON/IMG_0003.THM", "thumb")
        write(card, "DCIM/100CANON/IMG_0004.TIF", "aux")
        write(card, "DCIM/100CANON/DSC_0001.JPG", "dsc")
        write(card, "DCIM/100CANON/NOTE.TXT", "note")
        write(card, "DCIM/100CANON/README", "readme")
        write(card, "DCIM/100CANON/MISC/X.JPG", "nested-bytes")
        write(card, "DCIM/100CANON/._IMG_0001.JPG", "apple")
        write(card, "DCIM/.DS_Store", "store")
        val link = File(card, "DCIM/100CANON/LINK.JPG")
        JFiles.createSymbolicLink(link.toPath, File(card, "missing-target").toPath)
        val stamp: FileTime = FileTime.fromMillis(1_700_000_000_000L)
        JFiles.setLastModifiedTime(source.toPath, stamp)
        val upload: File = File(archive, ".upload")
        write(upload, ".card-left.partial", "leftover")
        write(upload, ".file-keep.partial", "file-partial")
        write(upload, ".inbox-keep.partial", "inbox-partial")
        write(upload, "IMG_0002.JPG", "short")
        val session: Session = sessionFor(archive)
        session.root.configuration.uploadDirectory.getCanonicalPath shouldBe upload.getCanonicalPath

        val first: String =
          """keep DCIM/100CANON/DSC_0001.JPG DSC_0001.JPG does not start with a digit
            |copy DCIM/100CANON/IMG_0001.JPG .upload/IMG_0001.JPG
            |keep DCIM/100CANON/IMG_0002.JPG size differs
            |copy DCIM/100CANON/IMG_0003.THM .upload/IMG_0003.THM
            |copy DCIM/100CANON/IMG_0004.TIF .upload/IMG_0004.TIF
            |keep DCIM/100CANON/LINK.JPG not a regular file
            |keep DCIM/100CANON/MISC nested
            |keep DCIM/100CANON/NOTE.TXT illegal extension txt
            |keep DCIM/100CANON/README no extension
            |keep DCIM/IMG_0001.JPG duplicate name
            |""".stripMargin
        runCopy(session, card, dryRun = false) shouldBe (first, Json.Null)
        read(File(upload, "IMG_0001.JPG")) shouldBe "same-bytes"
        read(File(upload, "IMG_0003.THM")) shouldBe "thumb"
        read(File(upload, "IMG_0004.TIF")) shouldBe "aux"
        read(File(upload, "IMG_0002.JPG")) shouldBe "short"
        read(source) shouldBe "same-bytes"
        read(File(card, "DCIM/100CANON/IMG_0002.JPG")) shouldBe "longer-bytes"
        JFiles.getLastModifiedTime(File(upload, "IMG_0001.JPG").toPath).toMillis shouldBe stamp.toMillis
        JFiles.getLastModifiedTime(source.toPath).toMillis shouldBe stamp.toMillis
        File(upload, "DSC_0001.JPG").exists shouldBe false
        File(upload, "NOTE.TXT").exists shouldBe false
        File(upload, "README").exists shouldBe false
        File(upload, "._IMG_0001.JPG").exists shouldBe false
        File(upload, ".card-left.partial").exists shouldBe false
        read(File(upload, ".file-keep.partial")) shouldBe "file-partial"
        read(File(upload, ".inbox-keep.partial")) shouldBe "inbox-partial"
        partialNames(upload) shouldBe Seq(".file-keep.partial", ".inbox-keep.partial")
        val copiedAt: Long = JFiles.getLastModifiedTime(File(upload, "IMG_0001.JPG").toPath).toMillis

        val second: String = first
          .replace("copy DCIM/100CANON/IMG_0001.JPG .upload/IMG_0001.JPG", "keep DCIM/100CANON/IMG_0001.JPG already copied")
          .replace("copy DCIM/100CANON/IMG_0003.THM .upload/IMG_0003.THM", "keep DCIM/100CANON/IMG_0003.THM already copied")
          .replace("copy DCIM/100CANON/IMG_0004.TIF .upload/IMG_0004.TIF", "keep DCIM/100CANON/IMG_0004.TIF already copied")
        runCopy(session, card, dryRun = false) shouldBe (second, Json.Null)
        JFiles.getLastModifiedTime(File(upload, "IMG_0001.JPG").toPath).toMillis shouldBe copiedAt

  it should "print copy lines without creating .upload, and delete only .card partials on a real keep" in :
    withTemp: archive =>
      withTemp: card =>
        write(card, "DCIM/100CANON/IMG_0001.JPG", "pic")
        val session: Session = sessionFor(archive)
        val text: String =
          """copy DCIM/100CANON/IMG_0001.JPG .upload/IMG_0001.JPG
            |""".stripMargin
        runCopy(session, card, dryRun = true) shouldBe (text, Json.Null)
        File(archive, ".upload").exists shouldBe false
        read(File(card, "DCIM/100CANON/IMG_0001.JPG")) shouldBe "pic"

    withTemp: archive =>
      withTemp: card =>
        write(card, "DCIM/100CANON/NOTE.TXT", "note")
        val session: Session = sessionFor(archive)
        val text: String =
          """keep DCIM/100CANON/NOTE.TXT illegal extension txt
            |""".stripMargin
        runCopy(session, card, dryRun = false) shouldBe (text, Json.Null)
        File(archive, ".upload").exists shouldBe false

    withTemp: archive =>
      withTemp: card =>
        write(card, "DCIM/100CANON/NOTE.TXT", "note")
        val upload: File = File(archive, ".upload")
        write(upload, ".card-left.partial", "leftover")
        write(upload, ".file-keep.partial", "stay")
        val session: Session = sessionFor(archive)
        runCopy(session, card, dryRun = true)._1 should include ("illegal extension txt")
        read(File(upload, ".card-left.partial")) shouldBe "leftover"
        runCopy(session, card, dryRun = false)._1 should include ("illegal extension txt")
        File(upload, ".card-left.partial").exists shouldBe false
        read(File(upload, ".file-keep.partial")) shouldBe "stay"
        File(upload, "NOTE.TXT").exists shouldBe false
        upload.isDirectory shouldBe true

  it should "reject a path that is not one real DCIM root, and a non-directory .upload" in :
    withTemp: archive =>
      withTemp: card =>
        val session: Session = sessionFor(archive)
        JFiles.createDirectories(File(card, "DCIM").toPath)
        JFiles.createDirectories(File(card, "dcim").toPath)
        usage(session, card.getPath) shouldBe "more than one DCIM directory"

        val linked: File = JFiles.createTempDirectory("card-link").toFile
        try
          JFiles.createDirectories(File(linked, "real").toPath)
          JFiles.createSymbolicLink(File(linked, "DCIM").toPath, File(linked, "real").toPath)
          usage(session, linked.getPath) shouldBe "no DCIM directory"
        finally deleteTree(linked)

        val dcim: File = JFiles.createTempDirectory("card-dcim").toFile
        try
          write(dcim, "IMG_0001.JPG", "pic")
          usage(session, dcim.getPath) shouldBe "no DCIM directory"
        finally deleteTree(dcim)

        val file: File = write(archive, "not-dir", "x")
        usage(session, file.getPath) shouldBe s"not a directory: ${file.getPath}"

        write(archive, ".upload", "nope")
        val root: File = JFiles.createTempDirectory("card-ok").toFile
        try
          write(root, "DCIM/100CANON/IMG_0001.JPG", "pic")
          usage(session, root.getPath) shouldBe ".upload is not a directory"
          read(File(archive, ".upload")) shouldBe "nope"
        finally deleteTree(root)

  it should "mount on copy --dry-run without a path and write nothing" in :
    withTemp: archive =>
      withTemp: card =>
        write(card, "DCIM/100CANON/IMG_0001.JPG", "pic")
        val session: Session = sessionFor(archive)
        val docs: Iterator[String] = Seq(canon("[]"), canon(mounts(card), fstype = "\"vfat\"")).iterator
        val mountsSeen = ArrayBuffer.empty[Seq[String]]
        val (text, thrown) = Transcript.collect:
          Card.copy(
            session,
            dryRun = true,
            path = None,
            lsblk = () => if docs.hasNext then Right(docs.next()) else Left("missing"),
            mount = argv =>
              mountsSeen += argv
              (0, "")
          )
        thrown.isEmpty shouldBe true
        mountsSeen.toSeq shouldBe Seq(mountArgs)
        text shouldBe
          line("/dev/sdb1", "CANON", card.getPath, "vfat", "yes") +
            "copy DCIM/100CANON/IMG_0001.JPG .upload/IMG_0001.JPG\n"
        File(archive, ".upload").exists shouldBe false
        read(File(card, "DCIM/100CANON/IMG_0001.JPG")) shouldBe "pic"

  it should "tell serve that card runs in the calling process" in :
    withTemp: archive =>
      val session: Session = sessionFor(archive)
      var ran: Boolean = false
      val (_, response) = Cli.serveResponse(
        session,
        """{"argv":["card"]}""",
        (_, _) =>
          ran = true
          (session, Result.Ok("ran\n", Json.Null))
      )
      ran shouldBe false
      response should include ("card runs in the calling process")

  private final case class Probe(
    text: String,
    thrown: Option[Throwable],
    mounts: Seq[Seq[String]],
    root: Option[File]
  )

  private def probe(
    documents: Seq[String],
    dryRun: Boolean = false,
    mountResult: (Int, String) = (0, "")
  ): Probe =
    val docs: Iterator[String] = documents.iterator
    val seen = ArrayBuffer.empty[Seq[String]]
    var root: Option[File] = None
    val (text, thrown) = Transcript.collect:
      root = Card.discover(
        dryRun,
        lsblk = () => if docs.hasNext then Right(docs.next()) else Left("missing"),
        mount = argv =>
          seen += argv
          mountResult
      )
    Probe(text, thrown, seen.toSeq, root)

  private def failure(thrown: Option[Throwable]): String = thrown match
    case Some(error: IllegalArgumentException) => error.getMessage
    case Some(error) => fail(error.toString)
    case None => fail("expected failure")

  private def line(device: String, label: String, mount: String, fstype: String, dcim: String): String =
    s"card device=$device label=${Album.quote(label)} mountpoint=${Album.quote(mount)} fstype=$fstype dcim=$dcim\n"

  private def runCopy(session: Session, card: File, dryRun: Boolean): (String, Json) =
    CopyCard(dryRun, Some(card.getPath)).execute(session)._2 match
      case Result.Ok(text, value) => (text, value)
      case other => fail(other.toString)

  private def usage(session: Session, path: String): String =
    CopyCard(dryRun = false, Some(path)).execute(session)._2 match
      case Result.Usage(message) => message
      case other => fail(other.toString)

  private def sessionFor(archive: File): Session =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    Session(Root(Configuration(rootPath, zone)))

  private def write(root: File, relative: String, text: String): File =
    val file: File = File(root, relative)
    file.getParentFile.mkdirs()
    JFiles.writeString(file.toPath, text)
    file

  private def read(file: File): String = JFiles.readString(file.toPath)

  private def partialNames(upload: File): Seq[String] =
    Option(upload.listFiles).toSeq.flatten.map(_.getName).filter(_.endsWith(".partial")).sorted

  private def withTemp(body: File => Unit): Unit =
    val dir: File = JFiles.createTempDirectory("card").toFile
    try body(dir)
    finally deleteTree(dir)

  private def deleteTree(file: File): Unit =
    if JFiles.isDirectory(file.toPath, LinkOption.NOFOLLOW_LINKS) then
      Option(file.listFiles).toSeq.flatten.foreach(deleteTree)
    if file.exists || JFiles.isSymbolicLink(file.toPath) then JFiles.deleteIfExists(file.toPath)

  private def jsonString(value: String): String =
    val escaped: String = value.flatMap:
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case ch => ch.toString
    "\"" + escaped + "\""

  private def mounts(dir: File): String = s"[${jsonString(dir.getPath)}]"

  private def doc(nodes: String*): String =
    s"""{"blockdevices":[${nodes.mkString(",")}]}
"""

  private def volume(
    path: String,
    rm: String = "true",
    fstype: String = "\"exfat\"",
    label: String = "\"CANON\"",
    mountpoints: String = "[]",
    children: Seq[String] = Nil,
    nodeType: String = "part"
  ): String =
    val nested: String = if children.isEmpty then "" else s""","children":[${children.mkString(",")}]"""
    s"""{"name":${jsonString(path.stripPrefix("/dev/"))},"path":${jsonString(path)},"rm":$rm,"type":${jsonString(nodeType)},"fstype":$fstype,"label":$label,"mountpoints":$mountpoints$nested}"""

  private def nvme: String =
    volume(
      "/dev/nvme0n1",
      rm = "false",
      fstype = "null",
      label = "null",
      nodeType = "disk",
      children = Seq(volume(
        "/dev/nvme0n1p2",
        rm = "false",
        fstype = "null",
        label = "null",
        mountpoints = """["/"]""",
        children = Seq(volume(
          "/dev/mapper/root",
          rm = "false",
          fstype = "\"ext4\"",
          label = "null",
          nodeType = "crypt",
          mountpoints = """["/"]"""
        ))
      ))
    )

  private def canon(
    mountpoints: String,
    fstype: String = "\"exfat\"",
    label: String = "\"CANON\"",
    rm: String = "true"
  ): String =
    doc(
      nvme,
      volume(
        "/dev/sdb",
        fstype = "null",
        label = "null",
        nodeType = "disk",
        children = Seq(volume("/dev/sdb1", rm = rm, fstype = fstype, label = label, mountpoints = mountpoints))
      )
    )
