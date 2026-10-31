package org.podval.tools.photo

import java.io.File
import java.time.{DateTimeException, LocalDate}

private[photo] final class AgentStems(dayText: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val day: Day = AgentCommands.parseDay(session, dayText)
      val (loaded, grouped, sidecars) = AgentCommands.loadGrouped(session, day, readSidecars = true)
      val rows: Seq[String] = grouped.map: (stem, extensions) =>
        s"$stem ${extensions.toSeq.sorted.mkString("+")}"
      val value: Json = Json.Obj(Seq(
        "day" -> Json.Str(Title.dayText(day.date)),
        "stems" -> Json.Arr(grouped.map: (stem, extensions) =>
          Json.Obj(Seq(
            "stem" -> Json.Str(stem),
            "extensions" -> Json.Arr(extensions.toSeq.sorted.map(Json.Str.apply)),
            "metadata" -> AgentCommands.metadataValue(sidecars.getOrElse(stem, Metadata.default))
          ))
        )
      ))
      (loaded, Result.Ok(AgentCommands.textOf(rows), value))

private[photo] object AgentCatalog extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val (loaded, albums) = session.loadCatalog
      val counts: Map[String, Int] = albums.groupBy(_.title).view.mapValues(_.length).toMap
      val sorted: Seq[Catalog.Album] = albums.sortBy(album => (album.title, album.id))
      val lines: Seq[String] = sorted.map: album =>
        AgentCommands.catalogLine(album, counts.getOrElse(album.title, 0) > 1)
      val value: Json = Json.Obj(Seq(
        "albums" -> Json.Arr(sorted.map: album =>
          AgentCommands.albumValue(album, counts.getOrElse(album.title, 0) > 1)
        )
      ))
      (loaded, Result.Ok(AgentCommands.textOf(lines), value))

private[photo] final class AgentItems(id: Option[String], title: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        val album: Catalog.Album = AgentCommands.select(albums, title, id)
        val text: String =
          if album.autoAdd then "auto-add\n"
          else AgentCommands.textOf(AgentCommands.itemLines(album))
        (loaded, Result.Ok(text, AgentCommands.itemsValue(album)))

private[photo] final class AgentMatch(id: Option[String], title: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        val album: Catalog.Album = AgentCommands.select(albums, title, id)
        if album.autoAdd then AgentCommands.skipped(loaded, album, "auto-add")
        else if album.error.exists(_.nameFailures > 0) || album.unsets.nonEmpty then
          AgentCommands.skipped(loaded, album, "unread names")
        else AgentCommands.matchItems(loaded, album)

private[photo] final class AgentInclude(spanText: Option[String], paths: Seq[String]) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val span: Option[Title.Span] = spanText.map(AgentCommands.parseSpan)
      val claimedPairs: Seq[(LocalDate, String)] = paths.map(AgentCommands.picturePath)
      span.foreach: dates =>
        if claimedPairs.exists(pair => !dates.contains(pair._1)) then
          throw IllegalArgumentException("claimed day outside the span")
      val claimed: Set[(LocalDate, String)] = claimedPairs.toSet
      val days: Seq[LocalDate] = span.fold(claimedPairs.map(_._1).distinct)(AgentCommands.eachDay)
      val (loaded, universe) = days.foldLeft((session, Map.empty[LocalDate, Set[String]])): (acc, date) =>
        val (current, soFar) = acc
        val day: Day = AgentCommands.civilDay(current.root, date)
        if !day.directory.isDirectory then (current, soFar)
        else
          val (next, grouped, _) = AgentCommands.loadGrouped(current, day)
          val stems: Set[String] = grouped.map(_._1).toSet
          (next, if stems.isEmpty then soFar else soFar.updated(date, stems))
      val entries: Seq[Include.Entry] = Include.compress(span, claimed, universe)
      val value: Json = Json.Obj(Seq(
        "rendered" -> Json.Arr(entries.map(entry => Json.Str(Include.render(entry)))),
        "logical" -> Json.Arr(entries.map(entry => Json.Str(Include.logical(entry))))
      ))
      (loaded, Result.Ok(AgentCommands.textOf(entries.map(Include.render)), value))

private[photo] object AgentCommands:
  // Usage keeps the session passed in. A later failure keeps a catalog that already parsed.
  def catching(session: Session)(body: => (Session, Result)): (Session, Result) =
    try body
    catch
      case error: (IllegalArgumentException | IllegalStateException) =>
        (session, Result.Usage(Operation.messageOf(error)))

  // Day's own require accepts 1..31. The civil date rejects February 31 and a non-leap February 29.
  // A bad segment is NumberFormatException or a failed year, month, or day require. Both are not a day.
  def parseDay(session: Session, text: String): Day =
    try session.parse(text) match
      case day: Day =>
        day.date
        day
      case _ =>
        throw IllegalArgumentException("not a day")
    catch
      case _: (NoSuchElementException | DateTimeException | IllegalArgumentException) =>
        throw IllegalArgumentException("not a day")

  def select(albums: Seq[Catalog.Album], title: String, id: Option[String]): Catalog.Album =
    id match
      case Some(wanted) =>
        albums.find(_.id == wanted) match
          case Some(album) if album.title == title => album
          case _ => throw IllegalArgumentException("catalog title does not match")
      case None =>
        albums.filter(_.title == title) match
          case Seq(album) => album
          case Seq() => throw IllegalArgumentException(s"no album titled ${Album.quote(title)}")
          case _ => throw IllegalArgumentException(s"more than one album titled ${Album.quote(title)}")

  def catalogLine(album: Catalog.Album, ambiguous: Boolean): String =
    val flags: Seq[String] = Seq(
      Some(album.owner.fold("owned")(label => s"owner=${Album.quote(label)}")),
      Option.when(album.autoAdd)("auto-add"),
      album.error.map(error => s"error got=${error.got} of=${error.of} name-failures=${error.nameFailures}"),
      Option.when(album.unsets.nonEmpty)(s"unset=${album.unsets.length}"),
      Option.when(ambiguous)("ambiguous")
    ).flatten
    s"${Album.quote(album.title)} count=${album.count} items=${album.items.length} ${flags.mkString(" ")}"

  def albumValue(album: Catalog.Album, ambiguous: Boolean): Json =
    Json.Obj(Seq(
      "id" -> Json.Str(album.id),
      "title" -> Json.Str(album.title),
      "count" -> Json.Long(album.count),
      "owned" -> Json.Bool(album.owner.isEmpty),
      "owner" -> album.owner.fold(Json.Null)(Json.Str.apply),
      "autoAdd" -> Json.Bool(album.autoAdd),
      "error" -> album.error.fold(Json.Null)(errorValue),
      "unset" -> Json.Long(album.unsets.length),
      "items" -> Json.Long(album.items.length),
      "ambiguous" -> Json.Bool(ambiguous)
    ))

  def itemLines(album: Catalog.Album): Seq[String] =
    album.items.map(item => s"${item.filename} ${catalogDate(item.date)} ${showUploader(item.uploader)}") ++
      album.unsets.map(unset => s"unset ${catalogDate(unset.date)} ${showUploader(unset.uploader)}")

  def itemsValue(album: Catalog.Album): Json =
    Json.Obj(Seq(
      "id" -> Json.Str(album.id),
      "error" -> album.error.fold(Json.Null)(errorValue),
      "items" -> Json.Arr(album.items.map(itemValue)),
      "unsets" -> Json.Arr(album.unsets.map(unsetValue))
    ))

  // Skip is a result. Do not read a day, and do not treat unread names as unmatched files.
  def skipped(session: Session, album: Catalog.Album, reason: String): (Session, Result) =
    (session, Result.Ok(s"skipped $reason\n", matchValue(album, Seq.empty, Seq.empty)))

  def matchItems(session: Session, album: Catalog.Album): (Session, Result) =
    val (loaded, hits, misses) = album.items.foldLeft(
      (session, Seq.empty[(LocalDate, String, String)], Seq.empty[(Option[LocalDate], String, String)])
    ): (acc, item) =>
      val (current, matched, unmatched) = acc
      // Catalog names are not files. nameAndExtension is this same last-dot split.
      val (stem, extension) = Files.splitOnLast(item.filename, '.')
      item.date match
        case None =>
          (current, matched, unmatched :+ (None, item.filename, item.key))
        case Some(date) =>
          val (next, grouped, _) = loadGrouped(current, civilDay(current.root, date))
          resolve(grouped.toMap, stem, extension) match
            case Some(local) =>
              (next, matched :+ (date, item.filename, local), unmatched)
            case None =>
              (next, matched, unmatched :+ (Some(date), item.filename, item.key))
    val rows: Seq[String] =
      Seq(s"matched ${hits.length} of ${album.items.length}") ++
        hits.map(hit => s"${Title.dayText(hit._1)} ${hit._2} ${hit._3}") ++
        misses.map(miss => s"unmatched ${miss._1.fold("-")(Title.dayText)} ${miss._2}")
    val value: Json = matchValue(
      album,
      hits.map(hit => hit._1 -> hit._3),
      misses.map(miss => miss._3)
    )
    (loaded, Result.Ok(textOf(rows), value))

  def parseSpan(text: String): Title.Span =
    Title.parse(text) match
      case Some(Title.Parsed(Some(span), None)) => span
      case _ => throw IllegalArgumentException("not a span")

  def picturePath(path: String): (LocalDate, String) =
    try Album.splitPath(path)
    catch case _: DateTimeException =>
      throw IllegalArgumentException(s"not a picture path: $path")

  def eachDay(span: Title.Span): List[LocalDate] =
    Iterator.iterate(span.start)(_.plusDays(1)).takeWhile(day => !day.isAfter(span.end)).toList

  def textOf(rows: Seq[String]): String =
    if rows.isEmpty then "" else rows.mkString("", "\n", "\n")

  // Grouped rows are names. The listing would run EXIF, so the sidecar is read here.
  def sidecar(planned: Seq[Pictures.Planned], directory: File, stem: String): Metadata =
    planned.find(row => !row.omitted && row.stem == stem && row.extension == Metadata.extension) match
      case None =>
        Metadata.default
      case Some(row) =>
        val corrected: File = File(directory, s"$stem.${Metadata.extension}")
        val file: File = if corrected.isFile then corrected else row.file
        if file.isFile then Metadata.read(file) else Metadata.default

  def metadataValue(metadata: Metadata): Json =
    Json.Obj(Seq(
      metadata.source.map(value => "source" -> Json.Str(value)),
      metadata.timestamp.map(value => "timestamp" -> Json.Str(value)),
      metadata.date.map(date => "date" -> Json.Str(date.toString)),
      metadata.`bad-raw`.map(value => "bad-raw" -> Json.Bool(value)),
      metadata.orientation.map(value => "orientation" -> Json.Str(value)),
      metadata.`google-media-item-id`.map(value => "google-media-item-id" -> Json.Str(value)),
      metadata.`already-in-google`.map(value => "already-in-google" -> Json.Bool(value)),
      metadata.uploader.map(value => "uploader" -> Json.Str(value))
    ).flatten)

  // readSidecars parses TOML on a miss and on a grouped hit that never stored it.
  // A hit that already has the map does not list the day again.
  def loadGrouped(
    session: Session,
    day: Day,
    readSidecars: Boolean = false
  ): (Session, Seq[(String, Set[String])], Map[String, Metadata]) =
    if !day.directory.isDirectory then (session, Seq.empty, Map.empty)
    else
      val directory: File = day.directory
      val fingerprint: Vector[FileStamp] = DirectoryIndex.fingerprint(directory, filesOnly = true)
      session.cached(directory).filter(_.fingerprint == fingerprint) match
        case Some(entry) =>
          entry.sidecars match
            case Some(known) =>
              (session, entry.grouped, known)
            case None if readSidecars =>
              val files: Seq[File] = Files.listFiles(directory)
              val known: Map[String, Metadata] = sidecarMap(directory, entry.grouped, Pictures.plan(files))
              (session.remember(directory, entry.copy(sidecars = Some(known))), entry.grouped, known)
            case None =>
              (session, entry.grouped, Map.empty)
        case None =>
          val files: Seq[File] = Files.listFiles(directory)
          val grouped: Seq[(String, Set[String])] = Pictures.grouped(files)
          val albums: Seq[Album.Loaded] = Archive.dayAlbums(day.root.configuration.archive, directory)
          val known: Option[Map[String, Metadata]] =
            if readSidecars then Some(sidecarMap(directory, grouped, Pictures.plan(files))) else None
          val next: Session = session.remember(directory, DirEntry(fingerprint, grouped, albums, None, known))
          (next, grouped, known.getOrElse(Map.empty))

  private def sidecarMap(
    directory: File,
    grouped: Seq[(String, Set[String])],
    planned: Seq[Pictures.Planned]
  ): Map[String, Metadata] =
    grouped.map(row => row._1 -> sidecar(planned, directory, row._1)).toMap

  private val videoExtensions: Set[String] = Set("mp4", "mov", "avi", "3gp")

  private def resolve(
    grouped: Map[String, Set[String]],
    stem: String,
    extension: Option[String]
  ): Option[String] =
    hit(grouped, stem, extension).orElse:
      val stripped: String = Pictures.dropOnePrefix(stem)
      if stripped == stem then None else hit(grouped, stripped, extension)

  private def hit(
    grouped: Map[String, Set[String]],
    stem: String,
    extension: Option[String]
  ): Option[String] =
    grouped.get(stem).map: extensions =>
      val v: String = stem + "v"
      val useV: Boolean =
        extension.exists(isVideo) && !extensions.exists(isVideo) && grouped.get(v).exists(_.exists(isVideo))
      if useV then v else stem

  private def isVideo(extension: String): Boolean =
    videoExtensions.contains(extension.toLowerCase(java.util.Locale.ROOT))

  def civilDay(root: Root, date: LocalDate): Day =
    root.year(date.getYear).month(date.getMonthValue).day(date.getDayOfMonth)

  private def catalogDate(date: Option[LocalDate]): String = date.fold("-")(_.toString)

  private def showUploader(uploader: Option[String]): String = uploader.getOrElse("-")

  private def errorValue(error: Catalog.Error): Json =
    Json.Obj(Seq(
      "got" -> Json.Long(error.got),
      "of" -> Json.Long(error.of),
      "nameFailures" -> Json.Long(error.nameFailures)
    ))

  private def itemValue(item: Catalog.Item): Json =
    Json.Obj(Seq(
      "filename" -> Json.Str(item.filename),
      "date" -> item.date.fold(Json.Null)(date => Json.Str(date.toString)),
      "key" -> Json.Str(item.key),
      "uploader" -> item.uploader.fold(Json.Null)(Json.Str.apply)
    ))

  private def unsetValue(unset: Catalog.Unset): Json =
    Json.Obj(Seq(
      "date" -> unset.date.fold(Json.Null)(date => Json.Str(date.toString)),
      "key" -> Json.Str(unset.key),
      "uploader" -> unset.uploader.fold(Json.Null)(Json.Str.apply)
    ))

  private def matchValue(
    album: Catalog.Album,
    hits: Seq[(LocalDate, String)],
    misses: Seq[String]
  ): Json =
    Json.Obj(Seq(
      "id" -> Json.Str(album.id),
      "matched" -> Json.Arr(hits.map: (day, stem) =>
        Json.Obj(Seq(
          "day" -> Json.Str(Title.dayText(day)),
          "stem" -> Json.Str(stem)
        ))
      ),
      "unmatched" -> Json.Arr(misses.map(Json.Str.apply))
    ))
