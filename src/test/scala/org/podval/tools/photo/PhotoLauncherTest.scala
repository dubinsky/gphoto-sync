package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.concurrent.TimeUnit

class PhotoLauncherTest extends AnyFlatSpec, Matchers:
  private def projectDirectory: File = new File(System.getProperty("user.dir"))

  "bin/photo" should "omit --args when there are no photo arguments" in:
    succeed(projectDirectory, "bin/photo") shouldBe header

  it should "quote mcp" in:
    succeed(projectDirectory, "bin/photo", "mcp") shouldBe header :+ "--args='mcp'"

  it should "quote agent and catalog from another directory" in:
    succeed(
      new File(System.getProperty("java.io.tmpdir")),
      Path.of("bin/photo").toRealPath().toString,
      "agent",
      "catalog"
    ) shouldBe header :+ "--args='agent' 'catalog'"

  it should "keep a space inside quotes" in:
    succeed(projectDirectory, "bin/photo", "Best of show") shouldBe header :+ "--args='Best of show'"

  it should "quote an apostrophe" in:
    succeed(projectDirectory, "bin/photo", "it's") shouldBe header :+ "--args='it'\"'\"'s'"

  it should "keep double quotes inside an argument" in:
    succeed(projectDirectory, "bin/photo", "say \"hi\"") shouldBe header :+ "--args='say \"hi\"'"

  it should "keep a leading dash" in:
    succeed(projectDirectory, "bin/photo", "--flag=false") shouldBe header :+ "--args='--flag=false'"

  it should "refuse an empty argument" in:
    val (exit, stdout, stderr) = launch(projectDirectory, "bin/photo", "")
    exit shouldBe 1
    stdout shouldBe ""
    stderr shouldBe "photo: empty argument cannot be passed through gradle run --args\n"

  private def header: List[String] = List(
    Path.of("gradlew").toRealPath().toString,
    "--project-dir=" + Path.of("bin").toRealPath().getParent
  )

  private def succeed(cwd: File, command: String, args: String*): List[String] =
    val (exit, stdout, stderr) = launch(cwd, command, args*)
    exit shouldBe 0
    stderr shouldBe ""
    stdout.linesIterator.toList

  private def launch(cwd: File, command: String, args: String*): (Int, String, String) =
    val argv: Seq[String] = command +: args
    val process: ProcessBuilder = new ProcessBuilder(argv*)
    process.directory(cwd)
    process.environment.put("PHOTO_LAUNCHER_PRINT_ARGS", "1")
    val proc: Process = process.start()
    val finished: Boolean = proc.waitFor(10, TimeUnit.SECONDS)
    val stdout: String = new String(proc.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val stderr: String = new String(proc.getErrorStream.readAllBytes(), StandardCharsets.UTF_8)
    if !finished then proc.destroyForcibly()
    finished shouldBe true
    (proc.exitValue, stdout, stderr)
