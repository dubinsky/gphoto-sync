package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NFiles}

class ResultsTest extends AnyFlatSpec, Matchers:
  private val header: String = "# gphoto-results 2"

  "Results" should "mean no rows when the file is absent and not create it" in :
    withTemp: dir =>
      val file: File = File(dir, "results.tsv")
      Results.read(file) shouldBe Seq.empty[Results.Row]
      file.exists shouldBe false

  it should "require the results header" in :
    withTemp: dir =>
      val caught: IllegalArgumentException = intercept[IllegalArgumentException]:
        Results.read(write(dir, "results.tsv", "## \nr1\trename-album\tdone\tgarbage\n"))
      caught.getMessage shouldBe "results are not version 2"
      val missing: IllegalArgumentException = intercept[IllegalArgumentException]:
        Results.read(write(dir, "bare.tsv", "r1\trename-album\tdone\t\n"))
      missing.getMessage shouldBe "results are not version 2"

  it should "keep the note unchanged for done, failed, and skipped" in :
    withTemp: dir =>
      val rows: Seq[Results.Row] = Results.read(results(
        dir,
        "r1\trename-album\tdone\t",
        "",
        "r2\tmove-item\tfailed\tkept as written",
        "r3\tdelete-item\tskipped\tmentions a and k1 on purpose"
      ))
      rows.map(_.status) shouldBe Seq(Results.Status.Done, Results.Status.Failed, Results.Status.Skipped)
      rows.map(_.note) shouldBe Seq("", "kept as written", "mentions a and k1 on purpose")
      rows.map(_.action) shouldBe Seq(
        Requests.Action.RenameAlbum,
        Requests.Action.MoveItem,
        Requests.Action.DeleteItem
      )

  it should "reject a bad status, a partial line, and a CR" in :
    withTemp: dir =>
      val status: IllegalArgumentException = intercept[IllegalArgumentException]:
        Results.read(results(dir, "r1\trename-album\tDone\t"))
      status.getMessage shouldBe "unknown result status"
      val id: IllegalArgumentException = intercept[IllegalArgumentException]:
        Results.read(results(dir, "r0\trename-album\tdone\t"))
      id.getMessage shouldBe "request id is not valid"
      val partial: IllegalArgumentException = intercept[IllegalArgumentException]:
        Results.read(write(dir, "partial.tsv", s"$header\nr1\trename-album\tdone\t"))
      partial.getMessage shouldBe "trailing partial line"
      val cr: IllegalArgumentException = intercept[IllegalArgumentException]:
        Results.read(write(dir, "cr.tsv", s"$header\nr1\trename-album\tdone\tok\r\n"))
      cr.getMessage shouldBe "carriage return"
      val columns: IllegalArgumentException = intercept[IllegalArgumentException]:
        Results.read(results(dir, "r1\trename-album\tdone"))
      columns.getMessage shouldBe "wrong column count"

  private def results(dir: File, lines: String*): File =
    write(dir, "results.tsv", (header +: lines).mkString("\n") + "\n")

  private def write(dir: File, name: String, text: String): File =
    val file: File = File(dir, name)
    NFiles.writeString(file.toPath, text, StandardCharsets.UTF_8)
    file

  private def withTemp(body: File => Unit): Unit =
    val dir: File = NFiles.createTempDirectory("gphoto-results").toFile
    try body(dir)
    finally remove(dir)

  private def remove(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(remove))
    val _: Boolean = file.delete()
