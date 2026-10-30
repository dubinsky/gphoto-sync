package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NFiles, Path, StandardOpenOption}
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

class RequestsTest extends AnyFlatSpec, Matchers:
  private val header: String = "# gphoto-requests 2"

  "Requests" should "not create a requests file before the first row" in :
    withTemp: dir =>
      val file: File = File(dir, "requests.tsv")
      Requests.read(file) shouldBe Seq.empty[Requests.Row]
      file.exists shouldBe false
      val rejected: IllegalArgumentException = intercept[IllegalArgumentException]:
        Requests.append(file, Requests.Action.RenameAlbum, "a", "", "Old", "New", Some("r0"))
      rejected.getMessage shouldBe "request id is not valid"
      file.exists shouldBe false

  it should "require the requests header" in :
    withTemp: dir =>
      val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
        Requests.read(write(dir, "requests.tsv", "## \nr0\trename-album\ta\t\tOld\tNew\n"))
      caught.getMessage shouldBe "requests are not version 2"
      val missing: IllegalArgumentException = intercept[IllegalArgumentException]:
        Requests.read(write(dir, "bare.tsv", "r1\trename-album\ta\t\tOld\tNew\n"))
      missing.getMessage shouldBe "requests are not version 2"

  it should "accept r1 and reject r0, r00, r000, and r001" in :
    withTemp: dir =>
      val file: File = requests(dir, "r1\trename-album\ta\t\tOld title\tNew title")
      Requests.read(file).map(_.id) shouldBe Seq("r1")
      Seq("r0", "r00", "r000", "r001", "r01").foreach: id =>
        val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
          Requests.read(requests(dir, s"$id\trename-album\ta\t\tOld\tNew"))
        caught.getMessage shouldBe "request id is not valid"
        val before: Seq[Byte] = NFiles.readAllBytes(file.toPath).toSeq
        val appendCaught: IllegalArgumentException = intercept[IllegalArgumentException]:
          Requests.append(file, Requests.Action.DeleteItem, "a", "k1", "one.jpg", "", Some(id))
        appendCaught.getMessage shouldBe "request id is not valid"
        NFiles.readAllBytes(file.toPath).toSeq shouldBe before

  it should "skip a blank line and reject a partial line or a CR" in :
    withTemp: dir =>
      val file: File = requests(
        dir,
        "r1\tmove-item\ta\tk1\tone.jpg\tb",
        "",
        "r2\tset-description\t\tk1\tone.jpg\t"
      )
      val rows: Seq[Requests.Row] = Requests.read(file)
      rows.map(_.id) shouldBe Seq("r1", "r2")
      rows(1).albumId shouldBe ""
      rows(1).`new` shouldBe ""
      val partial: IllegalArgumentException = intercept[IllegalArgumentException]:
        Requests.read(write(dir, "partial.tsv", s"$header\nr1\trename-album\ta\t\tOld\tNew"))
      partial.getMessage shouldBe "trailing partial line"
      val cr: IllegalArgumentException = intercept[IllegalArgumentException]:
        Requests.read(write(dir, "cr.tsv", s"$header\nr1\trename-album\ta\t\tOld\tNew\r\n"))
      cr.getMessage shouldBe "carriage return"
      val duplicate: IllegalArgumentException = intercept[IllegalArgumentException]:
        Requests.read(requests(dir, "r1\trename-album\ta\t\tOld\tNew", "r1\tdelete-item\ta\tk1\tone.jpg\t"))
      duplicate.getMessage shouldBe "duplicate request id"

  it should "assign the next id and keep a caller id that is free" in :
    withTemp: dir =>
      val file: File = File(dir, "requests.tsv")
      val first: Requests.Row = Requests.append(file, Requests.Action.RenameAlbum, "a", "", "Old", "New")
      first.id shouldBe "r1"
      text(file) shouldBe s"$header\nr1\trename-album\ta\t\tOld\tNew\n"
      val supplied: Requests.Row =
        Requests.append(file, Requests.Action.DownloadItem, "a", "k1", "one.jpg", "", Some("r10"))
      supplied.id shouldBe "r10"
      val third: Requests.Row = Requests.append(file, Requests.Action.DeleteItem, "a", "k1", "one.jpg", "")
      third.id shouldBe "r11"
      val gapped: File = requests(dir, "r1\trename-album\ta\t\tOld\tNew", "r3\tdelete-item\ta\tk1\tone.jpg\t")
      Requests.append(gapped, Requests.Action.MoveItem, "a", "k1", "one.jpg", "b").id shouldBe "r4"
      val before: Seq[Byte] = NFiles.readAllBytes(file.toPath).toSeq
      val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
        Requests.append(file, Requests.Action.SetDate, "", "k1", "one.jpg", "2010-06-01 09:30", Some("r1"))
      caught.getMessage shouldBe "duplicate request id"
      NFiles.readAllBytes(file.toPath).toSeq shouldBe before

  it should "round-trip every action" in :
    withTemp: dir =>
      val rows: Seq[Requests.Row] = Requests.read(requests(
        dir,
        "r1\trename-album\ta\t\tOld\tNew",
        "r2\tmove-item\ta\tk1\tone.jpg\tb",
        "r3\tset-description\t\tk1\tone.jpg\t",
        "r4\tset-date\t\tk1\tone.jpg\t2010-06-01 09:30",
        "r5\tdownload-item\ta\tk1\tone.jpg\t",
        "r6\tdelete-item\ta\tk1\tone.jpg\t"
      ))
      rows.map(_.action) shouldBe Seq(
        Requests.Action.RenameAlbum,
        Requests.Action.MoveItem,
        Requests.Action.SetDescription,
        Requests.Action.SetDate,
        Requests.Action.DownloadItem,
        Requests.Action.DeleteItem
      )
      rows(2).`new` shouldBe ""
      rows(3).`new` shouldBe "2010-06-01 09:30"

  it should "take the existing-file lock and append one row" in :
    withTemp: dir =>
      resetProbes()
      val file: File = File(dir, "requests.tsv")
      Requests.append(file, Requests.Action.RenameAlbum, "a", "", "Old", "New")
      val size: Long = NFiles.size(file.toPath)
      val channel: FileChannel = FileChannel.open(file.toPath, StandardOpenOption.READ, StandardOpenOption.WRITE)
      val lock = channel.lock()
      val overlapped: CountDownLatch = CountDownLatch(1)
      Requests.onOverlap = () => overlapped.countDown()
      val errors: ConcurrentLinkedQueue[Throwable] = ConcurrentLinkedQueue()
      val thread: Thread = new Thread(() =>
        try Requests.append(file, Requests.Action.MoveItem, "a", "k1", "one.jpg", "b")
        catch case error: Throwable => errors.add(error)
        ()
      )
      try
        thread.start()
        overlapped.await(5, TimeUnit.SECONDS) shouldBe true
        NFiles.size(file.toPath) shouldBe size
        lock.release()
        thread.join(5000)
        thread.isAlive shouldBe false
      finally
        resetProbes()
        if lock.isValid then lock.release()
        channel.close()
      if !errors.isEmpty then fail(String.valueOf(errors.peek))
      Requests.read(file).map(_.id) shouldBe Seq("r1", "r2")

  it should "make a CREATE_NEW loser take that same exclusive lock" in :
    withTemp: dir =>
      resetProbes()
      val file: File = File(dir, "requests.tsv")
      val planted: String = s"$header\nr1\trename-album\ta\t\tOld\tNew\n"
      var sawLock: String = ""
      Requests.beforeCreateNew = () =>
        NFiles.writeString(file.toPath, planted, StandardCharsets.UTF_8)
      Requests.afterLock = () =>
        sawLock = NFiles.readString(Path.of("/proc/locks"))
      val row: Requests.Row =
        try Requests.append(file, Requests.Action.DownloadItem, "a", "k1", "one.jpg", "")
        finally resetProbes()
      row.id shouldBe "r2"
      val held: Boolean = sawLock.split('\n').exists: line =>
        line.contains("POSIX") && line.contains("WRITE") && line.contains(":" + fileInode(file) + " ")
      held shouldBe true
      text(file) shouldBe planted + "r2\tdownload-item\ta\tk1\tone.jpg\t\n"
      text(file).split("\n").count(_ == header) shouldBe 1

  it should "initialize an empty file or a header prefix and reject any other bytes" in :
    withTemp: dir =>
      val empty: File = File(dir, "empty.tsv")
      NFiles.createFile(empty.toPath)
      val created: Requests.Row = Requests.append(empty, Requests.Action.RenameAlbum, "a", "", "Old", "New")
      created.id shouldBe "r1"
      text(empty) shouldBe s"$header\nr1\trename-album\ta\t\tOld\tNew\n"
      text(empty).split("\n").count(_ == header) shouldBe 1

      val prefix: File = write(dir, "prefix.tsv", header.take(8))
      val repaired: Requests.Row =
        Requests.append(prefix, Requests.Action.DownloadItem, "a", "k1", "one.jpg", "kept-whole")
      repaired.id shouldBe "r1"
      text(prefix) shouldBe s"$header\nr1\tdownload-item\ta\tk1\tone.jpg\tkept-whole\n"
      text(prefix).split("\n").count(_ == header) shouldBe 1

      val unfinished: File = write(dir, "unfinished.tsv", header)
      Requests.append(unfinished, Requests.Action.MoveItem, "a", "k1", "one.jpg", "b").id shouldBe "r1"
      text(unfinished) shouldBe s"$header\nr1\tmove-item\ta\tk1\tone.jpg\tb\n"

      val finished: File = write(dir, "finished.tsv", s"$header\n")
      Requests.append(finished, Requests.Action.SetDescription, "", "k1", "one.jpg", "cap").id shouldBe "r1"
      text(finished) shouldBe s"$header\nr1\tset-description\t\tk1\tone.jpg\tcap\n"
      text(finished).split("\n").count(_ == header) shouldBe 1

      val bare: File = write(dir, "bare.tsv", "r1\trename-album\ta\t\tOld\tNew\n")
      val bareBefore: Seq[Byte] = NFiles.readAllBytes(bare.toPath).toSeq
      val bareError: IllegalArgumentException = intercept[IllegalArgumentException]:
        Requests.append(bare, Requests.Action.DownloadItem, "a", "k1", "one.jpg", "kept-whole")
      bareError.getMessage shouldBe "requests are not version 2"
      NFiles.readAllBytes(bare.toPath).toSeq shouldBe bareBefore

      val foreign: File = write(dir, "foreign.tsv", "## \nr1\trename-album\ta\t\tOld\tNew\n")
      val before: Seq[Byte] = NFiles.readAllBytes(foreign.toPath).toSeq
      val version: IllegalArgumentException = intercept[IllegalArgumentException]:
        Requests.append(foreign, Requests.Action.MoveItem, "a", "k1", "one.jpg", "b")
      version.getMessage shouldBe "requests are not version 2"
      NFiles.readAllBytes(foreign.toPath).toSeq shouldBe before

      val same: File = write(dir, "same.tsv", "r1\tdelete-item\ta\tk1\tone.jpg\t\n")
      val sameBefore: Seq[Byte] = NFiles.readAllBytes(same.toPath).toSeq
      val notParsed: IllegalArgumentException = intercept[IllegalArgumentException]:
        Requests.append(same, Requests.Action.DeleteItem, "a", "k1", "one.jpg", "", Some("r1"))
      notParsed.getMessage shouldBe "requests are not version 2"
      NFiles.readAllBytes(same.toPath).toSeq shouldBe sameBefore

  it should "not let a read during an append observe a partial row" in :
    withTemp: dir =>
      resetProbes()
      val file: File = File(dir, "requests.tsv")
      Requests.append(file, Requests.Action.RenameAlbum, "a", "", "Old", "New")
      val size: Long = NFiles.size(file.toPath)
      val release: CountDownLatch = CountDownLatch(1)
      val inLock: CountDownLatch = CountDownLatch(1)
      val finished: CountDownLatch = CountDownLatch(1)
      val errors: ConcurrentLinkedQueue[Throwable] = ConcurrentLinkedQueue()
      val found = new java.util.concurrent.atomic.AtomicReference[Seq[String]]()
      Requests.afterLock = () =>
        inLock.countDown()
        release.await()
      val writer: Thread = new Thread(() =>
        try Requests.append(file, Requests.Action.MoveItem, "a", "k1", "one.jpg", "kept-whole")
        catch case error: Throwable => errors.add(error)
        ()
      )
      val reader: Thread = new Thread(() =>
        try found.set(Requests.read(file).map(_.`new`))
        catch case error: Throwable => errors.add(error)
        finally finished.countDown()
        ()
      )
      try
        writer.start()
        inLock.await(5, TimeUnit.SECONDS) shouldBe true
        reader.start()
        waitUntilBlocked(reader) shouldBe true
        finished.getCount shouldBe 1
        NFiles.size(file.toPath) shouldBe size
        release.countDown()
        writer.join(5000)
        finished.await(5, TimeUnit.SECONDS) shouldBe true
        writer.isAlive shouldBe false
      finally
        release.countDown()
        resetProbes()
      if !errors.isEmpty then fail(String.valueOf(errors.peek))
      found.get shouldBe Seq("New", "kept-whole")

  it should "give two overlapping appends one header and distinct ids" in :
    withTemp: dir =>
      resetProbes()
      val file: File = File(dir, "requests.tsv")
      val n: Int = 8
      val start: CountDownLatch = CountDownLatch(1)
      val done: CountDownLatch = CountDownLatch(n)
      val errors: ConcurrentLinkedQueue[Throwable] = ConcurrentLinkedQueue()
      val threads: Seq[Thread] = (1 to n).toSeq.map: _ =>
        new Thread(() =>
          try
            start.await()
            Requests.append(file, Requests.Action.RenameAlbum, "a", "", "Old", "New")
          catch case error: Throwable =>
            errors.add(error)
          finally done.countDown()
          ()
        )
      threads.foreach(_.start())
      start.countDown()
      done.await(10, TimeUnit.SECONDS) shouldBe true
      threads.foreach: thread =>
        thread.join(5000)
        thread.isAlive shouldBe false
      if !errors.isEmpty then fail(String.valueOf(errors.peek))
      Requests.read(file).map(_.id) shouldBe (1 to n).map(i => s"r$i")
      text(file).split("\n").count(_ == header) shouldBe 1

  private def waitUntilBlocked(thread: Thread): Boolean =
    val deadline: Long = System.nanoTime() + 5000000000L
    var waiting: Boolean = false
    while System.nanoTime() < deadline && !waiting do
      val state: Thread.State = thread.getState
      waiting = (state eq Thread.State.BLOCKED) || (state eq Thread.State.WAITING) ||
        (state eq Thread.State.TIMED_WAITING)
      if !waiting then Thread.sleep(1)
    waiting

  private def fileInode(file: File): String =
    String.valueOf(NFiles.getAttribute(file.toPath, "unix:ino"))

  private def resetProbes(): Unit =
    Requests.beforeCreateNew = () => ()
    Requests.afterLock = () => ()
    Requests.onOverlap = () => ()

  private def requests(dir: File, lines: String*): File =
    write(dir, "requests.tsv", (header +: lines).mkString("\n") + "\n")

  private def text(file: File): String = NFiles.readString(file.toPath, StandardCharsets.UTF_8)

  private def write(dir: File, name: String, text: String): File =
    val file: File = File(dir, name)
    NFiles.writeString(file.toPath, text, StandardCharsets.UTF_8)
    file

  private def withTemp(body: File => Unit): Unit =
    val dir: File = NFiles.createTempDirectory("gphoto-requests").toFile
    try body(dir)
    finally remove(dir)

  private def remove(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(remove))
    val _: Boolean = file.delete()
