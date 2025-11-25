package org.podval.tools.photo

import scala.sys.process.Process
import java.io.File
import java.time.{Instant, LocalDateTime, ZonedDateTime}

final class Picture private(
  val day: Day,
  override val name: String,
  hasMetadata: Boolean,
  normal: Option[String],
  raw: Option[String],
  thumbnail: Option[String],
  aux: Option[String]
) extends Sub[Day, String, Picture]:
  require(normal.isDefined || raw.isDefined || thumbnail.isDefined, s"no images for $this")

  override def parent: Day = day

  def file(extension: String): File = File(day.directory, s"$name.$extension")

  private def metadataFile: File = file(Picture.extensionMetadata)

  private def isNormalMissing: Boolean = raw.isDefined && normal.isEmpty

  // Note: image files only, not the metadata
  private val extensions: Set[Extension] = Set(normal, raw, thumbnail, aux)
    .flatten
    .map(Extension(this, _))

  private def extensionNames: Set[String] = extensions.map(_.extension)

  private lazy val metadata: Option[Metadata] = Option.when(hasMetadata)(Metadata.parse(metadataFile))

  def updateMetadata(u: Metadata.Update, dryRun: Boolean): Unit =
     val metadata: Metadata = this.metadata.getOrElse(Metadata.default)
     val metadataUpdated: Metadata = metadata.copy(
       source = u.source.orElse(metadata.source),
       timestamp = u.timestamp.orElse(metadata.timestamp),
       `bad-raw` = Option.when(u.badRaw || metadata.`bad-raw`.contains(true))(true)
     )

     if metadataUpdated == Metadata.default then
       Util.execute(
         dryRun,
         s"$this: deleting metadata file",
         Files.delete(metadataFile)
       )
     else
       val string: String = Metadata.unparse(metadataUpdated)
       Util.execute(
         dryRun,
         s"$this: writing metadata file\n$string",
         Files.write(string, metadataFile)
       )

  private lazy val timestampOverride: Option[Instant] = metadata
    .flatMap(_.timestamp)
    .map(timestamp => LocalDateTime.parse(timestamp).toInstant(Util.UTC))

  private lazy val timestamp: Option[Instant] =
    val result: Option[Instant] = timestampOverride.orElse(Extension.earliest(extensions.map(_.original)))
    result.foreach(timestamp => extensions.foreach(_.maybeSet(timestamp)))
    result

  private def isTimestampMissing: Boolean = timestamp.isEmpty

  private def isSettingTimestamp: Boolean =
    timestamp.nonEmpty && extensions.exists(_.isSetting)

  private def dayFromTimestamp: Option[Day] = timestamp.flatMap: timestamp =>
    val timestampZoned: ZonedDateTime = timestamp.atZone(Util.UTC)
    val result = root
      .item(timestampZoned.getYear)
      .item(timestampZoned.getMonthValue)
      .item(timestampZoned.getDayOfMonth)
    Option.when(!Extension.isDefaultDate(timestampZoned) && day.toString != result.toString)(result)

  private def isDayWrong: Boolean = dayFromTimestamp.isDefined

  private def needsFixing: Boolean = isNormalMissing || isTimestampMissing || isSettingTimestamp || isDayWrong

  def fix(dryRun: Boolean): Unit =
    // convert raw to 'jpg'
    if isNormalMissing then
      if !metadata.flatMap(_.`bad-raw`).contains(true)
      then println(s"$this: NOT generating 'jpg' from raw file marked as bad") else Util.execute(
        dryRun,
        s"$this: generating 'jpg' from raw file",
        Picture.dcraw(file(raw.get), file("jpg"))
      )

    if isTimestampMissing || isSettingTimestamp || isDayWrong then println(toStringLong)

    if isSettingTimestamp then extensions.foreach(_.set(dryRun))

    dayFromTimestamp.foreach: dayFromTimestamp =>
      Util.execute(
        dryRun,
        s"moving $this to $dayFromTimestamp",
        {
          Files.mkDirs(dayFromTimestamp.directory)
          (extensionNames ++ Option.when(hasMetadata)(Picture.extensionMetadata).toSet)
            .map(file)
            .foreach(Files.move(_, dayFromTimestamp.directory))
          Files.deleteEmptyDirectories(day.directory)
        }
      )

  override def toString: String =
    s"$day/$name " ++
    extensionNames.mkString("+") ++
    (if !isNormalMissing then "" else " NO NORMAL IMAGE ") ++
    metadata.fold("")(metadata => s" [$metadata]")

  private def toStringLong: String =
    toString ++
    timestamp.fold(" NO TIMESTAMP")(timestamp => s" timestamp=$timestamp") ++
    dayFromTimestamp.fold("")(dayFromTimestamp => s" WRONG DAY; SHOULD BE: $dayFromTimestamp") ++
    "\n" ++
    extensions.map(_.toString).mkString("\n")

  override def lsd(isLong: Boolean): Unit = ()

  def list(filter: Filter, isLong: Boolean): Unit =
    val forceLong: Boolean =
      (filter.timestampMissing || filter.settingTimestamp || filter.wrongDay || filter.needsFixing) &&
      (isTimestampMissing || isSettingTimestamp || isDayWrong)
    println(if isLong || forceLong then toStringLong else toString)

  override def forEachPicture(filter: Filter, action: Picture => Unit): Unit =
    if passes(filter) then action(this)

  private def passes(f: Filter): Boolean =
    def filter(pattern: String, f: Metadata => Option[String]): Boolean =
      val value: Option[String] = metadata.flatMap(f)
      (pattern == "*" && value.isDefined) || value.contains(pattern)

    (f.name.isEmpty || name.contains(f.name.get)) &&
    (f.extension.isEmpty || extensionNames.contains(f.extension.get)) &&
    (!f.normalMissing || isNormalMissing) &&
    (!f.timestampMissing || isTimestampMissing) &&
    (!f.settingTimestamp || isSettingTimestamp) &&
    (!f.wrongDay || isDayWrong) &&
    (!f.needsFixing || needsFixing) &&
    (!f.hasMetadata || hasMetadata) &&
    (f.source.isEmpty || filter(f.source.get, _.source)) &&
    (f.timestamp.isEmpty || filter(f.timestamp.get, _.timestamp)) &&
    (!f.badRaw || metadata.flatMap(_.`bad-raw`).contains(true))

object Picture:
  def apply(
    day: Day,
    name: String,
    extensions: Set[String]
  ): Picture =
    val nameFull: String = s"$day/$name"

    require(name.head.isDigit, s"Non-digit name start: $nameFull")

    require(extensions.nonEmpty, s"No files for $nameFull")
    extensions.foreach(extension => require(Picture.extensions.contains(extension), s"Unknown extension: $nameFull.$extension"))

    def noMoreThanOne(from: Set[String], what: String): Option[String] =
      val all: Set[String] = extensions.intersect(from)
      require(all.size <= 1, s"too many $what extensions for $nameFull: $all")
      all.headOption

    new Picture(
      day,
      name,
      hasMetadata = noMoreThanOne(extensionsMetadata , "metadata").nonEmpty,
      normal    = noMoreThanOne(extensionsNormal   , "normal"   ),
      raw       = noMoreThanOne(extensionsRaw      , "raw"      ),
      thumbnail = noMoreThanOne(extensionsThumbnail, "thumbnail"),
      aux       = noMoreThanOne(extensionsAux      , "aux"      )
    )

  private val extensionMetadata: String = "toml"

  private val extensionsNormal: Set[String] = Set("jpg", "gif", "avi", "mov", "mp4", "3gp")
  private val extensionsRaw: Set[String] = Set("crw", "cr2", "cr3")
  private val extensionsThumbnail: Set[String] = Set("thm")
  private val extensionsAux: Set[String] = Set("tif")
  private val extensionsMetadata: Set[String] = Set(extensionMetadata)

  private val extensions: Set[String] =
    extensionsNormal ++
    extensionsRaw ++
    extensionsThumbnail ++
    extensionsAux ++
    extensionsMetadata

  private def dcraw(rawFile: File, jpgFile: File): Unit =
    Process(s"dcraw -c $rawFile").#|(Process(s"convert - $jpgFile")).!

  private def imageMagic(rawFile: File, jpgFile: File): Unit =
    // TODO coder for crw (dng, digital negative) is not installed by default...
    Process(s"magick $rawFile $jpgFile").!
