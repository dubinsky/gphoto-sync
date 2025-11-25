package org.podval.tools.photo

import java.util.{Date, TimeZone}
import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.Metadata
import com.drew.metadata.exif.ExifSubIFDDirectory
import java.io.File
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZonedDateTime, ZoneId}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import scala.sys.process.Process

object Check:
  private val raw: Set[String] = Set("crw", "cr2", "cr3")
  private def isRaw(extension: String): Boolean = raw.contains(extension)

  private val extensions: Set[String] = raw ++ Set("jpg", "thm", "avi", "mov", "mp4", "3gp")

  private val rootPath: String = "/home/dub/Pictures/originals"

  private val NY: ZoneId = ZoneId.of("America/New_York")
  private val NY1: TimeZone = TimeZone.getTimeZone(NY)

  private def in(file: File, set: Set[String]): Boolean = set.exists(file.getPath.endsWith)

  private val wrongDate: Set[String] = Set(
    "2011/2011-06/2011-06-29/1715.mov"
  )

  private val corrupted: Set[String] = Set(
    "2003/2003-04/2003-04-29/1866.crw",
    "2003/2003-12/2003-12-05/2974.crw",
    "2004/2004-11/2004-11-27/3697.crw",
    "2005/2005-04/2005-04-28/4196.crw",
    "2005/2005-04/2005-04-28/4236.crw",
    "2005/2005-04/2005-04-28/4262.crw",
    "2005/2005-11/2005-11-05/4594.crw",
    "2006/2006-01/2006-01-07/4780.crw",
    "2006/2006-03/2006-03-13/5011.crw",
    "2006/2006-11/2006-11-22/5711.crw",
    "2006/2006-12/2006-12-18/5916.crw",
    "2007/2007-06/2007-06-19/6211.crw",
    "2008/2008-04/2008-04-23/7057.crw"
  )

  private val dryRun: Boolean = true

  def main(args: Array[String]): Unit =
    check()
    //checkYear(2004)

  private def check(): Unit =
    for yearDirectory: File <- listDirectories(File(rootPath))
    do checkYear(yearDirectory.getName.toInt, yearDirectory)

  private def checkYear(year: Int): Unit =
    checkYear(year, File(f"$rootPath/$year"))

  private def checkYear(year: Int, directory: File): Unit =
    require(year >= 2001)
    for monthDirectory <- listDirectories(directory)
    do checkMonth(year, getNumber(directory, monthDirectory), monthDirectory)

  private def checkMonth(year: Int, month: Int): Unit =
    checkMonth(year, month, File(f"$rootPath/$year/$year-$month%02d"))

  private def checkMonth(year: Int, month: Int, directory: File): Unit =
    require(month >= 1 && month <= 12)
    for dayDirectory <- listDirectories(directory)
    do checkDay(year, month, getNumber(directory, dayDirectory), dayDirectory)

  private def checkDay(year: Int, month: Int, day: Int): Unit =
    checkDay(year, month, day, File(f"$rootPath/$year/$year-$month%02d/$year-$month%02d-$day%02d"))

  private def checkDay(year: Int, month: Int, day: Int, directory: File): Unit =
    require(day >= 1 && day <= 31)

    for file <- listFiles(directory) do
      val (name: String, extension: String) = nameAndExtension(file)
      def sibling(extension: String): File = File(directory, s"$name.$extension")

      if !extensions.contains(extension) then println(s"Unknown extension: $file")
      if !name.head.isDigit              then println(s"Non-digit name start: $file")

      if isRaw(extension) then
        val jpg: File = sibling("jpg")
        if !jpg.exists && !in(file, corrupted) then
          println(s"Raw only: $file; generating JPG")
          if !dryRun then Process(s"convert $file $jpg").!

      val originalDateOpt: Option[ZonedDateTime] = getOriginalDateFromExtractor(file)
        .orElse {
          val thm: File = sibling("thm")
          if thm.exists then getOriginalDateFromExtractor(thm) else None
        }
        .orElse(if !isRaw(extension) then None else getOriginalDateFromDcraw(file, extension))
        .orElse(parseFileName(dropSuffix(name), compactFormat))
        .orElse(parseFileName(year, month, day, dropSuffix(name), shortFormat))
        .orElse(parseFileName(name, dashedFormat))
        .orElse {
          if extension != "jpg" then None else
            val fromRaw: Set[ZonedDateTime] = for
              rawExtension: String <- raw
              rawSibling: File = sibling(rawExtension)
              if rawSibling.exists
            yield getLastModified(rawSibling)
            fromRaw.headOption
        }

      if originalDateOpt.isEmpty then println(s"Can't determine original date: $file")

      if originalDateOpt.isDefined then
        val originalDate: ZonedDateTime = originalDateOpt.get
        val lastModified: ZonedDateTime = getLastModified(file)
        val distance: Long = getDistance(originalDate, lastModified)
        if (distance != 0) && !in(file, wrongDate) then
          println(s"Wrong last modified: $file - $lastModified, but should be $originalDate (${distance/1000})")
          if !dryRun then file.setLastModified(getMillis(originalDate))

        if !isDefaultDate(lastModified) then
          if      lastModified.getYear       != year  then println(s"Wrong year : $file")
          else if lastModified.getMonthValue != month then println(s"Wrong month: $file")
          else if lastModified.getDayOfMonth != day   then println(s"Wrong day  : $file")

  private def list(directory: File): (Seq[File], Seq[File]) =
    directory.listFiles().toIndexedSeq.partition(_.isDirectory)

  private def listDirectories(directory: File): Seq[File] =
    val (directories: Seq[File], files: Seq[File]) = list(directory)
    require(files.isEmpty, files)
    directories

  private def listFiles(directory: File): Seq[File] =
    val (directories: Seq[File], files: Seq[File]) = list(directory)
    require(directories.isEmpty, directories)
    files

  private def nameAndExtension(file: File): (String, String) =
    val name: String = file.getName
    val dot: Int = name.lastIndexOf('.')
    (name.substring(0, dot), name.substring(dot+1))

  private def getNumber(directory: File, file: File): Int =
    val name: String = file.getName
    require(name.startsWith(directory.getName + "-"))
    val str: String = name.substring(name.lastIndexOf('-')+1)
    require(str.length == 2)
    str.toInt

  private def dropSuffix(name: String): String = if name.endsWith("_1") then name.substring(0, name.length-2) else name

  private def getLastModified(file: File): ZonedDateTime =
    val lastModifiedMillis: Long = file.lastModified()
    Instant.ofEpochMilli(lastModifiedMillis).atZone(NY)

  private def getMillis(date: ZonedDateTime): Long = date.toInstant.toEpochMilli

  private def getDistance(from: ZonedDateTime, to: ZonedDateTime): Long =
    Math.abs(getMillis(from) - getMillis(to))

  private def getOriginalDateFromExtractor(file: File): Option[ZonedDateTime] =
    val metadata: Metadata = ImageMetadataReader.readMetadata(file)
    for
      exif: ExifSubIFDDirectory <- Option(metadata.getFirstDirectoryOfType(classOf[ExifSubIFDDirectory]))
      result: Date <- Option(exif.getDateOriginal(NY1))
    yield result.toInstant.atZone(NY)

  private val compactFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss") // 20120812_013216
  private val dashedFormat : DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-M-d-H-m-s" ) // 2010-09-14-19-49-2

  private def parseFileName(name: String, format: DateTimeFormatter): Option[ZonedDateTime] =
    try Some(LocalDateTime.parse(name, format).atZone(NY))
    catch case e: DateTimeParseException => None

  private val shortFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("HHmmss") // 013216

  private def parseFileName(year: Int, month: Int, day: Int, name: String, format: DateTimeFormatter): Option[ZonedDateTime] =
    try Some(LocalDate.of(year, month, day).atTime(LocalTime.parse(name, format)).atZone(NY))
    catch case e: DateTimeParseException => None

  private val timestampPrefix: String = "Timestamp:"
  private val timestampFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("MMM dd HH:mm:ss yyyy") // Nov  7 03:53:52 2001

  private def getOriginalDateFromDcraw(file: File, extension: String): Option[ZonedDateTime] = Process(s"dcraw -i -v $file")
    .!!
    .split('\n')
    .find(_.startsWith(timestampPrefix))
    .map(_.substring(timestampPrefix.length+4)) // drop day of the week too
    .map(_.trim)
    .map(string => if string(4) == ' ' then string.updated(4, '0') else string) // pad
    .map(LocalDateTime.parse(_, timestampFormat))
    .map(_.atZone(NY))

  // some photos were taken with no time set on the camera...
  private def isDefaultDate(date: ZonedDateTime): Boolean =
    (date.getYear == 2000 && date.getMonthValue == 1 && date.getDayOfMonth == 1) ||
    (date.getYear == 1980 && date.getMonthValue == 1 && date.getDayOfMonth == 1)
