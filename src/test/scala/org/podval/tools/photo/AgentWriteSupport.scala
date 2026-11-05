package org.podval.tools.photo

import org.scalatest.matchers.should.Matchers
import scala.annotation.targetName
import java.io.File
import java.nio.file.{Files as JFiles}
import java.time.ZoneId

private[photo] trait AgentWriteSupport extends Matchers:
  @targetName("outcomeCanEqual")
  private[photo] given [A <: Cli.Outcome, B <: Cli.Outcome] => CanEqual[A, B] = CanEqual.derived

  @targetName("resultCanEqual")
  private[photo] given [A <: Result, B <: Result] => CanEqual[A, B] = CanEqual.derived

  @targetName("jsonCanEqual")
  private[photo] given [A <: Json, B <: Json] => CanEqual[A, B] = CanEqual.derived

  private val zone: ZoneId = ZoneId.of("America/New_York")

  private[photo] def writeFixture(archive: File): Unit =
    writeCatalog(
      archive,
      "album\talbTrail\t0\t2010/07 ",
      "album\talbOwn\t1\tOwned Name",
      "owner\talbOwn\tAda",
      "item\talbOwn\tpic.jpg\t2010-07-01\tkeyOwn\t-",
      "album\talbSrc\t1\tSource Title",
      "item\talbSrc\tpic.jpg\t2010-07-01\tkeyPic\t-",
      "album\talbDst\t0\tDest Title",
      "album\talbUp\t1\tUploaded Name",
      "item\talbUp\tpic.jpg\t2010-07-02\tkeyUp\tBea",
      "album\talbShareA\t1\tAlpha Name",
      "item\talbShareA\tpic.jpg\t2010-07-03\tkeyShare\t-",
      "album\talbShareB\t1\tBeta Name",
      "item\talbShareB\tother.jpg\t2010-07-04\tkeyShare\t-",
      "album\talbOnly\t1\tOnly Name",
      "item\talbOnly\tpic.jpg\t2010-07-05\tkeyOnly\t-",
      "album\talbBad\t3\tBad Names",
      "item\talbBad\t.\t2010-07-06\tkeyDot\t-",
      "item\talbBad\t..\t2010-07-07\tkeyDots\t-",
      "item\talbBad\ta/b.jpg\t2010-07-08\tkeySlash\t-",
      "album\talbUnset\t1\tUnset Name",
      "error\talbUnset\t1\t1\t1",
      "unset\talbUnset\t2010-07-09\tkeyUnset\t-"
    )

  private[photo] def usage(session: Session, args: String*): String =
    exec(session, args*)._2 match
      case Result.Usage(message) => message
      case other                 => fail(other.toString)

  private[photo] def field(value: Json, name: String): Json = value match
    case Json.Obj(fields) =>
      fields.find(_._1 == name).map(_._2).getOrElse(fail(s"missing $name in $value"))
    case other =>
      fail(other.toString)

  private[photo] def exec(session: Session, args: String*): (Session, Result) =
    Cli.parse(args.toList) match
      case Cli.Run(parsed) =>
        Cli.toOperation(parsed).execute(session)
      case other =>
        fail(other.toString)

  private[photo] def sessionFor(archive: File): Session = Session(rootFor(archive))

  private def rootFor(archive: File): Root =
    val home: File = File(System.getProperty("user.home"))
    val rootPath: String = home.toPath.relativize(archive.toPath).toString
    Root(Configuration(rootPath, zone))

  private[photo] def writeCatalog(archive: File, lines: String*): File =
    val directory: File = File(archive, ".gphoto-sync")
    directory.mkdirs()
    val file: File = File(directory, "catalog.txt")
    JFiles.writeString(file.toPath, (Seq("# gphoto-catalog 2") ++ lines).mkString("", "\n", "\n"))
    file

  private[photo] def touch(directory: File, name: String): Unit =
    directory.mkdirs()
    JFiles.write(File(directory, name).toPath, Array.emptyByteArray)

  private[photo] def requests(archive: File): File = File(archive, ".gphoto-sync/requests.tsv")

  private[photo] def tomls(file: File): Seq[String] =
    if !file.exists then Seq.empty
    else if file.isDirectory then Option(file.listFiles).getOrElse(Array.empty[File]).toSeq.flatMap(tomls)
    else if file.getName.endsWith(".toml") then Seq(file.getPath)
    else Seq.empty

  private[photo] def withArchive(body: File => Unit): Unit =
    val archive: File = JFiles.createTempDirectory("agent-write").toFile
    try body(archive)
    finally deleteTree(archive)

  private[photo] def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()
