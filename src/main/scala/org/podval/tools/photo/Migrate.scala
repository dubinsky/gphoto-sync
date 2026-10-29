package org.podval.tools.photo

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.key.TomlKey
import io.github.wasabithumb.jtoml.value.TomlValue
import io.github.wasabithumb.jtoml.value.primitive.TomlPrimitive
import io.github.wasabithumb.jtoml.value.table.TomlTable
import scala.jdk.CollectionConverters.{IteratorHasAsScala, ListHasAsScala, SetHasAsScala}
import java.io.File
import java.nio.file.Files as JFiles
import java.time.format.DateTimeParseException
import java.time.{DateTimeException, LocalDate, LocalDateTime, LocalTime, YearMonth, ZoneId, ZoneOffset}

// One-time reader for the old album keys. Delete this with the migrate-albums command after the archive run.
object Migrate:
  final case class Drop(path: String, title: String)

  final case class Rewritten(
    oldTitle: String,
    newTitle: String,
    oldFile: File,
    newFile: File,
    oldRelative: String,
    newRelative: String,
    text: String,
    span: Title.Span,
    claimed: Set[(LocalDate, String)],
    drops: Seq[Drop]
  )

  final case class Plan(rewritten: Seq[Rewritten]):
    def lines: Seq[String] = rewritten.flatMap: item =>
      val titleLine: Option[String] =
        Option.when(item.oldTitle != item.newTitle)(s"""title "${item.oldTitle}" -> "${item.newTitle}"""")
      val fileLine: Option[String] =
        Option.when(item.oldRelative != item.newRelative)(s"file ${item.oldRelative} -> ${item.newRelative}")
      titleLine.toSeq ++ fileLine.toSeq ++ item.drops.map(drop => s"""drop-time ${drop.path} "${drop.title}"""")

  def plan(originals: File, zone: ZoneId): Plan =
    val pictures: Seq[Archive.Picture] = Archive.pictures(originals)
    val universe: Map[LocalDate, Set[String]] = Archive.universe(pictures)
    val built: Plan = build(originals, zone, pictures, universe)
    verify(built, universe)
    built

  def run(originals: File, zone: ZoneId, dryRun: Boolean, checkWeights: Boolean): Plan =
    val pictures: Seq[Archive.Picture] = Archive.pictures(originals)
    val universe: Map[LocalDate, Set[String]] = Archive.universe(pictures)
    val built: Plan = build(originals, zone, pictures, universe)
    if checkWeights then requireWeights(originals, built, universe)
    verify(built, universe)
    built.lines.foreach(println)
    if !dryRun && built.rewritten.nonEmpty then
      writeAll(built)
      verifyDisk(originals, built, universe)
    built

  private val capeOld: String = "2010/07/04/Cape Cod.toml"
  private val capeNew: String = "2010/07/04/to-18 Cape Cod.toml"
  private val capeTitle: String = "2010/07/04/Cape Cod"
  private val weddingRelative: String = "2012/09/11/Wedding Yisroel Noah.toml"
  private val weddingTitle: String = "2012/09/11/Wedding Yisroel Noah"

  private enum WeightState:
    case Missing, Done, Check

  // Both albums are still old, or both are already migrated. A missing file fails before any write.
  private def requireWeights(
    originals: File,
    built: Plan,
    universe: Map[LocalDate, Set[String]]
  ): Unit =
    val cape: WeightState = weightState(originals, capeOld, capeNew)
    val wedding: WeightState = weightState(originals, weddingRelative, weddingRelative)
    (cape, wedding) match
      case (WeightState.Done, WeightState.Done) =>
        ()
      case (WeightState.Check, WeightState.Check) =>
        requireOne(built, universe, capeTitle, allExcept = 434, perDay = 429)
        requireOne(built, universe, weddingTitle, allExcept = 1825, perDay = 1829)
      case (WeightState.Missing, _) =>
        throw IllegalArgumentException(s"missing $capeOld")
      case (_, WeightState.Missing) =>
        throw IllegalArgumentException(s"missing $weddingRelative")
      case _ =>
        throw IllegalArgumentException("Cape Cod and Wedding Yisroel Noah must be migrated together")

  private def requireOne(
    built: Plan,
    universe: Map[LocalDate, Set[String]],
    title: String,
    allExcept: Int,
    perDay: Int
  ): Unit =
    val item: Rewritten = built.rewritten.find(_.oldTitle == title).getOrElse(
      throw IllegalArgumentException(s"missing $title")
    )
    val weights: (Int, Option[Int]) = Include.weights(Some(item.span), item.claimed, universe)
    val gotPerDay: Int = weights._1
    val gotExcept: Option[Int] = weights._2
    val exceptLength: Int = gotExcept.getOrElse(-1)
    if gotPerDay != perDay || exceptLength != allExcept then
      throw IllegalArgumentException(
        s"$title all-except $exceptLength per-day $gotPerDay, expected all-except $allExcept per-day $perDay"
      )

  private def weightState(originals: File, oldRelative: String, newRelative: String): WeightState =
    val oldFile: File = File(originals, oldRelative)
    val newFile: File = File(originals, newRelative)
    if oldFile.isFile && !hasInclude(oldFile) then WeightState.Check
    else if oldFile.isFile && hasInclude(oldFile) then WeightState.Done
    else if !oldFile.isFile && newFile.isFile && hasInclude(newFile) then WeightState.Done
    else WeightState.Missing

  private def build(
    originals: File,
    zone: ZoneId,
    pictures: Seq[Archive.Picture],
    universe: Map[LocalDate, Set[String]]
  ): Plan =
    val olds: Seq[Old] = collect(originals)
    val rewritten: Seq[Rewritten] = olds.map(old => rewrite(originals, zone, pictures, universe, old))
    val plan: Plan = Plan(rewritten)
    distinctTargets(plan)
    plan

  private def rewrite(
    originals: File,
    zone: ZoneId,
    pictures: Seq[Archive.Picture],
    universe: Map[LocalDate, Set[String]],
    old: Old
  ): Rewritten =
    val parsed: Title.Parsed = target(old)
    val claimed: Set[(LocalDate, String)] = folderClaims(old, pictures)
    val include: Seq[Include.Entry] = Include.compress(parsed.span, claimed, universe)
    val album: Album = Album(
      title = parsed.title,
      span = parsed.span,
      include = include,
      external = old.external,
      googleAlbumId = old.googleAlbumId,
      publishedTitle = old.publishedTitle,
      googleTitle = old.googleTitle,
      owner = old.owner
    )
    val newFile: File = Title.file(originals, parsed)
    Rewritten(
      oldTitle = old.title,
      newTitle = parsed.title,
      oldFile = old.file,
      newFile = newFile,
      oldRelative = Album.relative(originals, old.file),
      newRelative = Album.relative(originals, newFile),
      text = textOf(album),
      span = parsed.span.getOrElse(throw IllegalArgumentException(s"${old.file}: migrated album has no span")),
      claimed = claimed,
      drops = dropTime(old, zone, pictures, claimed)
    )

  private def textOf(album: Album): String =
    val rendered: String = Album.renderFile(album)
    if album.include.nonEmpty then rendered
    else if rendered.isEmpty then "include = []\n"
    else rendered + "include = []\n"

  private def dropTime(
    old: Old,
    zone: ZoneId,
    pictures: Seq[Archive.Picture],
    claimed: Set[(LocalDate, String)]
  ): Seq[Drop] =
    val timed: Set[(LocalDate, String)] = timeClaims(old, zone, pictures)
    (timed -- claimed).toSeq
      .sortBy(pair => s"${Title.dayText(pair._1)}/${pair._2}")
      .map(pair => Drop(s"${Title.dayText(pair._1)}/${pair._2}", old.title))

  private def distinctTargets(plan: Plan): Unit =
    val dupes: Seq[String] = plan.rewritten.groupBy(_.newRelative).collect:
      case (path, items) if items.sizeIs > 1 => path
    .toSeq
    if dupes.nonEmpty then
      throw IllegalArgumentException(s"two album files would occupy ${dupes.mkString(", ")}")
    val byOld: Map[String, Rewritten] = plan.rewritten.map(item => item.oldRelative -> item).toMap
    plan.rewritten.foreach: item =>
      byOld.get(item.newRelative).foreach: other =>
        if other.oldRelative != item.oldRelative then
          throw IllegalArgumentException(s"${item.newRelative}: two album files would occupy one path")

  private def verify(plan: Plan, universe: Map[LocalDate, Set[String]]): Unit =
    val temp: File = JFiles.createTempDirectory("migrate-albums").toFile
    try plan.rewritten.foreach: item =>
      val dest: File = File(temp, item.newRelative)
      dest.getParentFile.mkdirs()
      Files.write(item.text, dest)
      val read: Album = Album.read(temp, dest)
      val got: Set[(LocalDate, String)] = expanded(read, universe)
      if !got.equals(item.claimed) then
        throw IllegalArgumentException(s"${item.oldRelative}: parsed expansion does not match the old claim")
    finally deleteTree(temp)

  private def verifyDisk(originals: File, plan: Plan, universe: Map[LocalDate, Set[String]]): Unit =
    plan.rewritten.foreach: item =>
      val read: Album = Album.read(originals, item.newFile)
      val got: Set[(LocalDate, String)] = expanded(read, universe)
      if !got.equals(item.claimed) then
        throw IllegalArgumentException(s"${item.newFile}: read-back does not match the old claim")

  private def expanded(album: Album, universe: Map[LocalDate, Set[String]]): Set[(LocalDate, String)] =
    album.include.foldLeft(Set.empty[(LocalDate, String)]): (acc, entry) =>
      acc ++ Include.expand(album.span, entry, universe)

  private def writeAll(plan: Plan): Unit =
    val kept: Set[String] = plan.rewritten.map(_.newRelative).toSet
    plan.rewritten.foreach: item =>
      item.newFile.getParentFile.mkdirs()
      Files.write(item.text, item.newFile)
    plan.rewritten.foreach: item =>
      if !kept.contains(item.oldRelative) then item.oldFile.delete()

  private enum Kind:
    case Event(day: LocalDate, name: String)
    case Day(day: LocalDate)
    case Month(month: YearMonth, name: Option[String])

  private final case class Old(
    file: File,
    title: String,
    kind: Kind,
    exclude: Set[String],
    nameFrom: Option[String],
    nameTo: Option[String],
    to: Option[LocalDate],
    external: Boolean,
    googleAlbumId: String,
    publishedTitle: String,
    googleTitle: String,
    owner: Option[String]
  )

  private def target(old: Old): Title.Parsed = old.kind match
    case Kind.Event(day, name) =>
      val end: LocalDate = old.to.getOrElse(day)
      if end.isBefore(day) then throw IllegalArgumentException(s"${old.file}: to is before the path day")
      val kept: Option[String] = Option.unless(monthName(name))(name)
      kept.foreach: text =>
        if !Title.legalName(text) then throw IllegalArgumentException(s"${old.file}: not an album name: $text")
      Title.Parsed(Some(Title.Span(day, end, Title.Grain.Day)), kept)
    case Kind.Day(day) =>
      Title.Parsed(Some(Title.Span(day, day, Title.Grain.Day)), None)
    case Kind.Month(month, name) =>
      val kept: Option[String] = name.filterNot(monthName)
      kept.foreach: text =>
        if !Title.legalName(text) then throw IllegalArgumentException(s"${old.file}: not an album name: $text")
      val span: Title.Span = Title.Span(month.atDay(1), month.atEndOfMonth, Title.Grain.Month)
      Title.Parsed(Some(span), kept)

  private def folderClaims(old: Old, pictures: Seq[Archive.Picture]): Set[(LocalDate, String)] =
    pictures.foldLeft(Set.empty[(LocalDate, String)]): (acc, picture) =>
      if folderHit(old, picture) then acc + (picture.folderDay -> picture.name) else acc

  private def folderHit(old: Old, picture: Archive.Picture): Boolean =
    !old.exclude.contains(picture.name) && (old.kind match
      case Kind.Event(day, _) =>
        val end: LocalDate = old.to.getOrElse(day)
        !picture.folderDay.isBefore(day) && !picture.folderDay.isAfter(end) && inBounds(old, picture.name)
      case Kind.Day(day) =>
        picture.folderDay.equals(day)
      case Kind.Month(month, _) =>
        YearMonth.from(picture.folderDay).equals(month)
    )

  private def timeClaims(old: Old, zone: ZoneId, pictures: Seq[Archive.Picture]): Set[(LocalDate, String)] =
    old.kind match
      case Kind.Event(day, _) =>
        val start: LocalDateTime = day.atStartOfDay
        val end: LocalDateTime = old.to.getOrElse(day).atTime(LocalTime.MAX)
        pictures.foldLeft(Set.empty[(LocalDate, String)]): (acc, picture) =>
          val time: LocalDateTime = pictureTime(picture, zone)
          val hit: Boolean =
            !time.isBefore(start) && !time.isAfter(end) &&
              inBounds(old, picture.name) && !old.exclude.contains(picture.name)
          if hit then acc + (picture.folderDay -> picture.name) else acc
      case _ =>
        Set.empty

  // Same clock the old claims used. Metadata time wins. Millis is an epoch instant in the archive zone.
  private def pictureTime(picture: Archive.Picture, zone: ZoneId): LocalDateTime =
    picture.metadata.timestamp.map(LocalDateTime.parse)
      .orElse:
        Dates.fromFileName(picture.name, zone, Some(picture.folderDay)).map:
          case (instant, "millis") => instant.atZone(zone).toLocalDateTime
          case (instant, _) => LocalDateTime.ofInstant(instant, ZoneOffset.UTC)
      .getOrElse(picture.folderDay.atStartOfDay)

  private def inBounds(old: Old, name: String): Boolean =
    old.nameFrom.forall(bound => name >= bound) && old.nameTo.forall(bound => name <= bound)

  private val monthWords: Set[String] = Set(
    "jan", "january", "feb", "february", "mar", "march", "apr", "april", "may",
    "jun", "june", "jul", "july", "aug", "august", "sep", "sept", "september",
    "oct", "october", "nov", "november", "dec", "december"
  )

  private def monthName(stem: String): Boolean =
    val words: Seq[String] = stem.split("\\s+").toSeq.filter(_.nonEmpty)
    words.nonEmpty && words.forall(word => monthWords.contains(word.toLowerCase))

  private def collect(originals: File): Seq[Old] =
    rootToml(originals)
    childDirectories(originals).filter(dir => isNumber(dir.getName, 4)).flatMap: yearDir =>
      collectYear(originals, yearDir)

  private def rootToml(originals: File): Unit =
    tomlFiles(originals).foreach: file =>
      if !hasInclude(file) then throw IllegalArgumentException(s"$file: curated albums are not migrated")

  private def collectYear(originals: File, yearDir: File): Seq[Old] =
    val year: Int = yearDir.getName.toInt
    val plain: Seq[Old] = tomlFiles(yearDir).map: file =>
      val stem: String = stemOf(file)
      if isMonth(stem) then readPlainMonth(file, year, stem.toInt)
      else throw IllegalArgumentException(s"$file: album files do not live in a year directory")
    val nested: Seq[Old] = childDirectories(yearDir).filter(dir => isMonth(dir.getName)).flatMap: monthDir =>
      collectMonth(originals, year, monthDir)
    plain ++ nested

  private def collectMonth(originals: File, year: Int, monthDir: File): Seq[Old] =
    val month: Int = monthDir.getName.toInt
    val here: Seq[Old] = tomlFiles(monthDir).flatMap: file =>
      monthFile(file, year, month)
    val days: Seq[Old] = childDirectories(monthDir).filter(dir => isDay(dir.getName)).flatMap: dayDir =>
      collectDay(year, month, dayDir)
    here ++ days

  private def monthFile(file: File, year: Int, month: Int): Option[Old] =
    if hasInclude(file) then None
    else
      val stem: String = stemOf(file)
      if newName(stem) then throw IllegalArgumentException(s"$file: new album file is missing include")
      else if isDay(stem) then Some(readDay(file, civil(year, month, stem.toInt)))
      else if stem.nonEmpty && !stem.head.isDigit then
        Some(readMonth(file, YearMonth.of(year, month), Some(stem)))
      else throw IllegalArgumentException(s"$file: not an album file")

  private def collectDay(year: Int, month: Int, dayDir: File): Seq[Old] =
    val day: LocalDate = civil(year, month, dayDir.getName.toInt)
    dayFiles(dayDir).flatMap: file =>
      if hasInclude(file) then None
      else if newName(stemOf(file)) then throw IllegalArgumentException(s"$file: new album file is missing include")
      else Some(readEvent(file, day))

  private def readEvent(file: File, day: LocalDate): Old =
    val values: Map[String, TomlValue] = checked(file, eventKeys)
    if values.contains("from") then throw IllegalArgumentException(s"$file: from is not a folder-day bound")
    val common: Common = readCommon(file, values)
    val name: String = stemOf(file)
    Old(
      file,
      title = s"${Title.dayText(day)}/$name",
      kind = Kind.Event(day, name),
      exclude = common.exclude,
      nameFrom = optionalString(file, values, "name-from"),
      nameTo = optionalString(file, values, "name-to"),
      to = readTo(file, values),
      external = common.external,
      googleAlbumId = common.googleAlbumId,
      publishedTitle = common.publishedTitle,
      googleTitle = common.googleTitle,
      owner = common.owner
    )

  private def readDay(file: File, day: LocalDate): Old =
    val common: Common = readCommon(file, checked(file, folderKeys))
    Old(
      file,
      title = Title.dayText(day),
      kind = Kind.Day(day),
      exclude = common.exclude,
      nameFrom = None,
      nameTo = None,
      to = None,
      external = common.external,
      googleAlbumId = common.googleAlbumId,
      publishedTitle = common.publishedTitle,
      googleTitle = common.googleTitle,
      owner = common.owner
    )

  private def readMonth(file: File, month: YearMonth, name: Option[String]): Old =
    val common: Common = readCommon(file, checked(file, folderKeys))
    val title: String = name.fold(Title.monthText(month))(text => s"${Title.monthText(month)}/$text")
    Old(
      file,
      title = title,
      kind = Kind.Month(month, name),
      exclude = common.exclude,
      nameFrom = None,
      nameTo = None,
      to = None,
      external = common.external,
      googleAlbumId = common.googleAlbumId,
      publishedTitle = common.publishedTitle,
      googleTitle = common.googleTitle,
      owner = common.owner
    )

  private def readPlainMonth(file: File, year: Int, month: Int): Old =
    if hasInclude(file) then throw IllegalArgumentException(s"$file: album files do not live in a year directory")
    else readMonth(file, YearMonth.of(year, month), None)

  private final case class Common(
    exclude: Set[String],
    external: Boolean,
    googleAlbumId: String,
    publishedTitle: String,
    googleTitle: String,
    owner: Option[String]
  )

  private val folderKeys: Set[String] =
    Set("exclude", "external", "google-album-id", "published-title", "google-title", "owner")
  private val eventKeys: Set[String] = folderKeys ++ Set("from", "to", "name-from", "name-to")

  private def readCommon(file: File, values: Map[String, TomlValue]): Common =
    val common: Common = Common(
      exclude = stringSeq(file, values, "exclude").toSet,
      external = boolean(file, values, "external", default = false),
      googleAlbumId = string(file, values, "google-album-id", default = ""),
      publishedTitle = string(file, values, "published-title", default = ""),
      googleTitle = string(file, values, "google-title", default = ""),
      owner = optionalString(file, values, "owner")
    )
    if common.googleAlbumId.isEmpty && common.publishedTitle.nonEmpty then
      throw IllegalArgumentException(s"$file: published-title requires google-album-id")
    common

  private def readTo(file: File, values: Map[String, TomlValue]): Option[LocalDate] =
    values.get("to").map: value =>
      val primitive: TomlPrimitive = primitiveOf(file, "to", value)
      if primitive.isLocalDate then primitive.asLocalDate
      else if primitive.isLocalDateTime then dateOnly(file)
      else if primitive.isString then dateString(file, primitive.asString)
      else reject(file, "to", "must be a date")

  private def dateString(file: File, text: String): LocalDate =
    try LocalDate.parse(text)
    catch case _: DateTimeParseException =>
      try
        LocalDateTime.parse(text)
        dateOnly(file)
      catch case _: DateTimeParseException =>
        reject(file, "to", s"must be a date, not '$text'")

  private def dateOnly(file: File): Nothing =
    throw IllegalArgumentException(s"$file: to must be a date")

  private def hasInclude(file: File): Boolean =
    entries(tableOf(file)).exists(_._1 == "include")

  private def newName(stem: String): Boolean = stem == "album" || stem.startsWith("to-")

  private def checked(file: File, allowed: Set[String]): Map[String, TomlValue] =
    val found: Seq[(String, TomlValue)] = entries(tableOf(file))
    val unknown: Seq[String] = found.map(_._1).filterNot(allowed.contains)
    if unknown.isEmpty then found.toMap
    else throw IllegalArgumentException(s"$file: unknown key '${unknown.mkString(", ")}'")

  private def tableOf(file: File): TomlTable =
    val text: String = Files.read(file)
    if text.isBlank then TomlTable.create() else JToml.jToml.readFromString(text)

  private def entries(table: TomlTable): Seq[(String, TomlValue)] =
    table.keys(false).asScala.toSeq.flatMap: key =>
      Option(table.get(key)).map(keyName(key) -> _)

  private def keyName(key: TomlKey): String =
    if key.size == 1 then key.get(0) else key.toString

  private def optionalString(file: File, values: Map[String, TomlValue], key: String): Option[String] =
    values.get(key).map(asString(file, key, _))

  private def string(file: File, values: Map[String, TomlValue], key: String, default: String): String =
    optionalString(file, values, key).getOrElse(default)

  private def boolean(file: File, values: Map[String, TomlValue], key: String, default: Boolean): Boolean =
    values.get(key).fold(default): value =>
      val primitive: TomlPrimitive = primitiveOf(file, key, value)
      if primitive.isBoolean then primitive.asBoolean
      else reject(file, key, "must be a boolean")

  private def stringSeq(file: File, values: Map[String, TomlValue], key: String): Seq[String] =
    values.get(key).fold(Seq.empty): value =>
      if !value.isArray then reject(file, key, "must be an array of strings")
      else value.asArray.asScala.toSeq.map(asString(file, key, _))

  private def asString(file: File, key: String, value: TomlValue): String =
    val primitive: TomlPrimitive = primitiveOf(file, key, value)
    if primitive.isString then primitive.asString
    else reject(file, key, "must be a string")

  private def primitiveOf(file: File, key: String, value: TomlValue): TomlPrimitive =
    if value.isPrimitive then value.asPrimitive
    else reject(file, key, "must be a primitive")

  private def reject(file: File, key: String, message: String): Nothing =
    throw IllegalArgumentException(s"$file: key '$key' $message")

  private def dayFiles(directory: File): Seq[File] =
    children(directory).filter: file =>
      val (name, extension) = Files.nameAndExtension(file)
      file.isFile && extension.contains("toml") && !name.headOption.exists(_.isDigit)

  private def tomlFiles(directory: File): Seq[File] =
    children(directory).filter(file => file.isFile && file.getName.endsWith(".toml"))

  private def childDirectories(directory: File): Seq[File] =
    children(directory).filter(_.isDirectory)

  private def children(directory: File): Seq[File] =
    Option(directory.listFiles).map(_.toSeq).getOrElse(Seq.empty).sortBy(_.getName)

  private def stemOf(file: File): String = Files.nameAndExtension(file)._1

  private def civil(year: Int, month: Int, day: Int): LocalDate =
    try LocalDate.of(year, month, day)
    catch case _: DateTimeException =>
      throw IllegalArgumentException(s"not a date: $year-$month-$day")

  private def isMonth(name: String): Boolean = isNumber(name, 2) && name.toInt >= 1 && name.toInt <= 12

  private def isDay(name: String): Boolean = isNumber(name, 2) && name.toInt >= 1 && name.toInt <= 31

  private def isNumber(name: String, length: Int): Boolean =
    name.length == length && name.forall(_.isDigit)

  private def deleteTree(file: File): Unit =
    if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
    file.delete()

final case class MigrateAlbums(dryRun: Boolean) extends Operation:
  override def execute(): Unit =
    val root: Root = Root.get
    Migrate.run(
      originals = root.configuration.byYearDirectory,
      zone = root.configuration.zoneId,
      dryRun = dryRun,
      checkWeights = true
    )
