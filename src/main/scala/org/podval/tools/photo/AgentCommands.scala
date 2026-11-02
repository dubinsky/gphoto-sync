package org.podval.tools.photo

import java.io.File
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, StandardCopyOption, StandardOpenOption}
import java.time.{DateTimeException, LocalDate, YearMonth}
import java.util.UUID

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

private[photo] final class AgentAlbumFile(
  id: Option[String],
  dryRun: Boolean,
  replace: Boolean,
  googleTitle: Option[String],
  noRequest: Boolean,
  title: String
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        val album: Catalog.Album = AgentCommands.select(albums, title, id)
        if album.autoAdd then AgentCommands.skipped(loaded, album, "auto-add")
        else if album.error.exists(_.nameFailures > 0) || album.unsets.nonEmpty then
          AgentCommands.skipped(loaded, album, "unread names")
        else AgentCommands.writeAlbum(loaded, album, dryRun, replace, googleTitle, noRequest)

private[photo] final class AgentRequest(
  action: Requests.Action,
  album: Option[String],
  item: Option[String],
  current: Option[String],
  updated: Option[String],
  id: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      id.foreach(Requests.requireId)
      val fields: (String, String, String, String) =
        AgentCommands.requestFields(action, album, item, current, updated)
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        AgentCommands.acceptRequest(albums, action, fields._1, fields._2, fields._3, fields._4)
        val file: File = AgentCommands.requestsFile(loaded)
        val row: Requests.Row = Requests.append(file, action, fields._1, fields._2, fields._3, fields._4, id)
        (loaded, Result.Ok(s"appended ${row.id} ${row.action.wire}\n", AgentCommands.requestValue(row)))

private[photo] object AgentRequests extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.listRequests(session)

private[photo] object AgentResults extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.listResults(session)

private[photo] object AgentDrift extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      AgentCommands.drift(session)

private[photo] object AgentMissing extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val (loaded, albums) = session.loadCatalog
      AgentCommands.listMissing(loaded, albums)

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
      val (loaded, universe) = AgentCommands.scanDays(session, days)
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
    val (loaded, hits, misses) = gather(session, album)
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

  // Same lookup as agent match. Album-file does not keep a second copy.
  def gather(
    session: Session,
    album: Catalog.Album
  ): (Session, Seq[(LocalDate, String, String)], Seq[(Option[LocalDate], String, String)]) =
    album.items.foldLeft(
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
    Extension.video.exists(_.name == extension.toLowerCase(java.util.Locale.ROOT))

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

  def writeAlbum(
    session: Session,
    album: Catalog.Album,
    dryRun: Boolean,
    replace: Boolean,
    googleFlag: Option[String],
    noRequest: Boolean
  ): (Session, Result) =
    album.owner.foreach: _ =>
      googleFlag.foreach: flag =>
        if flag != album.title then
          throw IllegalArgumentException(
            s"google-title does not match ${Album.quote(album.title)} ${Album.quote(flag)}"
          )
    val archive: File = session.root.configuration.archive
    val (parsed, monthFile) = place(album)
    val dest: File = Title.file(archive, parsed)
    if !dest.getParentFile.isDirectory then throw IllegalArgumentException("no such directory")
    val relative: String = Album.relative(archive, dest)
    if dest.isFile && !replace then throw IllegalArgumentException(s"file exists: $relative")
    // checkGoogle rejects an owner together with a google album id.
    val (keptId, keptPublished) =
      if replace && dest.isFile then
        val old: Album = Album.read(archive, dest)
        if album.owner.isDefined then ("", "") else (old.googleAlbumId, old.publishedTitle)
      else
        ("", "")
    val (matched, hits, _) = gather(session, album)
    val claimed: Set[(LocalDate, String)] = hits.map(hit => hit._1 -> hit._3).toSet
    // Album.read rejects an include day outside the span. Fail before any partial.
    parsed.span.foreach: span =>
      if claimed.exists(pair => !span.contains(pair._1)) then
        throw IllegalArgumentException("matched day outside the span")
    val days: Seq[LocalDate] = parsed.span match
      case Some(span) => eachDay(span)
      case None => album.items.flatMap(_.date).distinct.sorted
    val (scanned, universe) = scanDays(matched, days)
    val monthPassed: Boolean =
      monthFile && monthAllowsAll(parsed.span.get, claimed, universe)
    // Zero matches omit include. all would claim pictures added later.
    val include: Seq[Include.Entry] =
      if claimed.isEmpty then Seq.empty
      else if monthPassed then Seq(Include.Entry.All(Seq.empty))
      else if monthFile then Include.compress(None, claimed, universe)
      else Include.compress(parsed.span, claimed, universe)
    requireInclude(monthPassed, parsed.span, include, claimed, universe)
    val (googleTitle, owner) = album.owner match
      case Some(label) =>
        (googleFlag.getOrElse(album.title), Some(label))
      case None =>
        (googleFlag.getOrElse(""), None)
    val pathTitle: String = parsed.title
    val willRename: Boolean = album.owner.isEmpty && googleFlag.isEmpty && album.title != pathTitle
    val written: Album = Album(
      title = pathTitle,
      span = parsed.span,
      include = include,
      external = true,
      googleAlbumId = keptId,
      publishedTitle = keptPublished,
      googleTitle = googleTitle,
      owner = owner
    )
    val renameWord: String = if willRename then "yes" else "no"
    val value: Json = albumValueOf(album.id, relative, pathTitle, album.title, willRename, None)
    if dryRun then
      (scanned, Result.Ok(Album.renderFile(written) + s"rename $renameWord\n", value))
    else
      commitFile(archive, dest, Album.renderFile(written))
      val dropped: Session = scanned.drop(dest.getParentFile)
      val summary: String =
        s"""wrote $relative
           |path-title $pathTitle
           |catalog-title ${Album.quote(album.title)}
           |rename $renameWord
           |""".stripMargin
      if !willRename || noRequest then (dropped, Result.Ok(summary, value))
      else
        try
          val row: Requests.Row = Requests.append(
            requestsFile(dropped),
            Requests.Action.RenameAlbum,
            album.id,
            "",
            album.title,
            pathTitle
          )
          val appended: String = summary + s"appended ${row.id} rename-album\n"
          (dropped, Result.Ok(appended, albumValueOf(album.id, relative, pathTitle, album.title, true, Some(row.id))))
        catch
          case error: Exception =>
            (
              dropped,
              Result.Failed(summary + "append failed\n", value, Operation.messageOf(error))
            )

  // Called before the partial is created. A bad include throws and nothing is written.
  def requireInclude(
    monthPassed: Boolean,
    span: Option[Title.Span],
    include: Seq[Include.Entry],
    claimed: Set[(LocalDate, String)],
    universe: Map[LocalDate, Set[String]]
  ): Unit =
    val got: Set[(LocalDate, String)] =
      include.foldLeft(Set.empty[(LocalDate, String)]): (acc, entry) =>
        acc ++ Include.expand(span, entry, universe)
    val ok: Boolean =
      if monthPassed then
        val local: Set[(LocalDate, String)] = Include.pairsIn(span.get, universe)
        got.equals(local) && monthAllowsAll(span.get, claimed, universe)
      else
        got.equals(claimed)
    if !ok then throw IllegalArgumentException("include does not match")

  def requestFields(
    action: Requests.Action,
    album: Option[String],
    item: Option[String],
    current: Option[String],
    updated: Option[String]
  ): (String, String, String, String) =
    def text(value: Option[String], name: String): String =
      value.getOrElse(throw IllegalArgumentException(s"missing $name"))
    action match
      case Requests.Action.RenameAlbum =>
        (text(album, "album"), "", text(current, "current"), text(updated, "new"))
      case Requests.Action.MoveItem =>
        (text(album, "album"), text(item, "item"), text(current, "current"), text(updated, "new"))
      case Requests.Action.SetDescription =>
        ("", text(item, "item"), text(current, "current"), updated.getOrElse(""))
      case Requests.Action.SetDate =>
        ("", text(item, "item"), text(current, "current"), text(updated, "new"))
      case Requests.Action.DownloadItem | Requests.Action.DeleteItem =>
        (text(album, "album"), text(item, "item"), text(current, "current"), "")

  def acceptRequest(
    albums: Seq[Catalog.Album],
    action: Requests.Action,
    albumId: String,
    itemKey: String,
    current: String,
    updated: String
  ): Unit = action match
    case Requests.Action.RenameAlbum =>
      val album: Catalog.Album = albums.find(_.id == albumId).getOrElse(
        throw IllegalArgumentException("no such album")
      )
      if album.owner.isDefined then throw IllegalArgumentException("album has an owner")
      requireCurrent(album.title, current)
      if updated.isEmpty then throw IllegalArgumentException("new title is empty")
      if updated == current then throw IllegalArgumentException("new title is unchanged")
    case Requests.Action.MoveItem =>
      val source: Catalog.Album = albums.find(_.id == albumId).getOrElse(
        throw IllegalArgumentException("no such album")
      )
      if !albums.exists(_.id == updated) then throw IllegalArgumentException("no such destination")
      val item: Catalog.Item = source.items.find(_.key == itemKey).getOrElse(
        throw IllegalArgumentException("no such item")
      )
      requireCurrent(item.filename, current)
      if updated == albumId then throw IllegalArgumentException("destination is the source album")
    case Requests.Action.SetDescription =>
      val item: Catalog.Item = uniqueItem(albums, itemKey)
      requireCurrent(item.filename, current)
    case Requests.Action.SetDate =>
      val item: Catalog.Item = uniqueItem(albums, itemKey)
      requireCurrent(item.filename, current)
      requireDate(updated)
    case Requests.Action.DownloadItem =>
      val album: Catalog.Album = albums.find(_.id == albumId).getOrElse(
        throw IllegalArgumentException("no such album")
      )
      val item: Catalog.Item = album.items.find(_.key == itemKey).getOrElse(
        throw IllegalArgumentException("no such item")
      )
      requireCurrent(item.filename, current)
      if updated.nonEmpty then throw IllegalArgumentException("new is not empty")
      requireFilename(item.filename)
    case Requests.Action.DeleteItem =>
      val album: Catalog.Album = albums.find(_.id == albumId).getOrElse(
        throw IllegalArgumentException("no such album")
      )
      val item: Catalog.Item = album.items.find(_.key == itemKey).getOrElse(
        throw IllegalArgumentException("no such item")
      )
      requireCurrent(item.filename, current)
      if updated.nonEmpty then throw IllegalArgumentException("new is not empty")
      if album.owner.isDefined then throw IllegalArgumentException("album has an owner")
      if item.uploader.isDefined then throw IllegalArgumentException("item has an uploader")

  def listRequests(session: Session): (Session, Result) =
    catching(session):
      val file: File = requestsFile(session)
      if !file.isFile then (session, Result.Ok("no requests\n", Json.Arr(Seq.empty)))
      else
        val rows: Seq[Requests.Row] = Requests.read(file)
        if rows.isEmpty then (session, Result.Ok("", Json.Arr(Seq.empty)))
        else
          val (loaded, albums) =
            if rows.exists(_.action == Requests.Action.MoveItem) then session.loadCatalog
            else (session, Seq.empty)
          catching(loaded):
            val lines: Seq[String] = rows.map(row => requestText(row, albums))
            (loaded, Result.Ok(textOf(lines), Json.Arr(rows.map(requestValue))))

  def listResults(session: Session): (Session, Result) =
    catching(session):
      val file: File = resultsFile(session)
      if !file.isFile then (session, Result.Ok("no results\n", Json.Arr(Seq.empty)))
      else
        val rows: Seq[Results.Row] = Results.read(file)
        val lines: Seq[String] = rows.map: row =>
          val note: String = if row.note.isEmpty then "" else s" ${row.note}"
          s"${row.id} ${row.action.wire} ${row.status.wire}$note"
        val value: Json = Json.Arr(rows.map: row =>
          Json.Obj(Seq(
            "id" -> Json.Str(row.id),
            "action" -> Json.Str(row.action.wire),
            "status" -> Json.Str(row.status.wire),
            "note" -> Json.Str(row.note)
          ))
        )
        (session, Result.Ok(textOf(lines), value))

  // Same classes reconcile prints. dry-run builds no write touches, and the link file is not published.
  def drift(session: Session): (Session, Result) =
    val archive: File = session.root.configuration.archive
    val (loaded, catalog) = session.loadCatalog
    val found: Seq[Album.Loaded] = Archive.scan(archive).albums
    val existing: Seq[AlbumLinks.Row] = AlbumLinks.read(archive)
    val (ready, plan) = Reconcile.classify(
      loaded,
      archive,
      catalog,
      found,
      existing,
      Cli.ReconcileArgs(dryRun = true, delete = false)
    )
    (ready, Result.Ok(textOf(plan.actions.map(Reconcile.line)), Reconcile.actionsValue(plan.actions)))

  // gather calls resolve. A date of - is not a day, so that item is not looked up.
  // Auto-add is skipped here. Unset lines are not items.
  def listMissing(session: Session, albums: Seq[Catalog.Album]): (Session, Result) =
    val (ready, rows) = albums.foldLeft((session, Seq.empty[(Catalog.Album, Catalog.Item)])): (acc, album) =>
      if album.autoAdd then acc
      else
        val (current, soFar) = acc
        val (next, _, misses) = gather(current, album)
        val wanted: Set[String] = misses.map(_._3).toSet
        val added: Seq[(Catalog.Album, Catalog.Item)] = album.items.collect:
          case item if wanted.contains(item.key) => album -> item
        (next, soFar ++ added)
    val lines: Seq[String] = rows.map: (album, item) =>
      s"missing ${album.title} ${catalogDate(item.date)} ${item.filename} ${showUploader(item.uploader)}"
    val value: Json = Json.Obj(Seq(
      "missing" -> Json.Arr(rows.map: (album, item) =>
        Json.Obj(Seq(
          "title" -> Json.Str(album.title),
          "date" -> Json.Str(catalogDate(item.date)),
          "filename" -> Json.Str(item.filename),
          "uploader" -> Json.Str(showUploader(item.uploader)),
          "key" -> Json.Str(item.key)
        ))
      )
    ))
    (ready, Result.Ok(textOf(lines), value))

  def requestsFile(session: Session): File =
    File(File(session.root.configuration.archive, ".gphoto-sync"), "requests.tsv")

  def resultsFile(session: Session): File =
    File(File(session.root.configuration.archive, ".gphoto-sync"), "results.tsv")

  def requestValue(row: Requests.Row): Json =
    Json.Obj(Seq(
      "id" -> Json.Str(row.id),
      "action" -> Json.Str(row.action.wire),
      "albumId" -> Json.Str(row.albumId),
      "itemKey" -> Json.Str(row.itemKey),
      "current" -> Json.Str(row.current),
      "new" -> Json.Str(row.`new`)
    ))

  private def requestText(row: Requests.Row, albums: Seq[Catalog.Album]): String = row.action match
    case Requests.Action.RenameAlbum =>
      s"${row.id} rename-album ${Album.quote(row.current)} ${Album.quote(row.`new`)}"
    case Requests.Action.MoveItem =>
      val dest: String = albums.find(_.id == row.`new`).fold("destination unknown")(album => Album.quote(album.title))
      s"${row.id} move-item ${Album.quote(row.current)} $dest"
    case Requests.Action.SetDescription | Requests.Action.SetDate =>
      s"${row.id} ${row.action.wire} ${Album.quote(row.current)} ${Album.quote(row.`new`)}"
    case Requests.Action.DownloadItem | Requests.Action.DeleteItem =>
      s"${row.id} ${row.action.wire} ${Album.quote(row.current)}"

  private def albumValueOf(
    albumId: String,
    path: String,
    pathTitle: String,
    catalogTitle: String,
    rename: Boolean,
    requestId: Option[String]
  ): Json =
    Json.Obj(Seq(
      "albumId" -> Json.Str(albumId),
      "path" -> Json.Str(path),
      "pathTitle" -> Json.Str(pathTitle),
      "catalogTitle" -> Json.Str(catalogTitle),
      "rename" -> Json.Bool(rename),
      "requestId" -> requestId.fold(Json.Null)(Json.Str.apply)
    ))

  private def commitFile(archive: File, dest: File, text: String): Unit =
    val dot: File = File(archive, ".gphoto-sync")
    dot.mkdirs()
    val partial: File = File(dot, s"album-write-${UUID.randomUUID()}.partial")
    var renamed: Boolean = false
    try
      writeForced(partial, text)
      JFiles.move(
        partial.toPath,
        dest.toPath,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )
      renamed = true
    finally
      if !renamed then JFiles.deleteIfExists(partial.toPath)

  private def writeForced(file: File, text: String): Unit =
    val channel: FileChannel = FileChannel.open(
      file.toPath,
      StandardOpenOption.CREATE_NEW,
      StandardOpenOption.WRITE
    )
    try
      val bytes: ByteBuffer = ByteBuffer.wrap(text.getBytes(StandardCharsets.UTF_8))
      while bytes.hasRemaining do channel.write(bytes)
      channel.force(true)
    finally
      channel.close()

  private[photo] def scanDays(
    session: Session,
    days: Seq[LocalDate]
  ): (Session, Map[LocalDate, Set[String]]) =
    days.foldLeft((session, Map.empty[LocalDate, Set[String]])): (acc, date) =>
      val (current, soFar) = acc
      val (next, grouped, _) = loadGrouped(current, civilDay(current.root, date))
      val stems: Set[String] = grouped.map(_._1).toSet
      (next, if stems.isEmpty then soFar else soFar.updated(date, stems))

  private[photo] def place(album: Catalog.Album): (Title.Parsed, Boolean) =
    val working: String = album.title.split(' ').filter(_.nonEmpty).mkString(" ")
    Title.parse(working) match
      case Some(parsed) if singleMonth(parsed) =>
        (parsed, true)
      case Some(parsed) =>
        (parsedDays(album, parsed), false)
      case None =>
        val tokens: Seq[String] = tokenize(working)
        // A slash date plus more words is not a root file. A name inside that token stays.
        tokens.headOption.flatMap(token => Title.parse(token).filter(_.span.isDefined)) match
          case Some(lead) =>
            // The slash token is already out. Strip an item year, an item day, and one month from the name only.
            val tail: String = tokens.drop(1).mkString(" ")
            val raw: String = lead.name.fold(tail)(label => if tail.isEmpty then label else s"$label $tail")
            val name: String = leftoverName(raw, album)
            if name.headOption.exists(_.isDigit) then throw IllegalArgumentException("leftover starts with a digit")
            if name.nonEmpty && !Title.legalName(name) then throw IllegalArgumentException("leftover is not a name")
            (parsedDays(album, Title.Parsed(lead.span, Option.when(name.nonEmpty)(name))), false)
          case None =>
            val name: String = leftoverName(working, album)
            if name.headOption.exists(_.isDigit) then throw IllegalArgumentException("leftover starts with a digit")
            val parsed: Title.Parsed =
              if hasYearOrMonth(working) then datedTitle(album, name)
              else if name.isEmpty then throw IllegalArgumentException("curated album has no name")
              else Title.Parsed(None, Some(name))
            (parsed, false)

  private def singleMonth(parsed: Title.Parsed): Boolean =
    parsed.span.exists: span =>
      val oneMonth: Boolean = YearMonth.from(span.start).equals(YearMonth.from(span.end))
      span.grain match
        case Title.Grain.Month => oneMonth
        case Title.Grain.Day => false

  // Item days when the catalog has them. Otherwise keep the span Title.parse already accepted.
  private def parsedDays(album: Catalog.Album, parsed: Title.Parsed): Title.Parsed =
    val dates: Seq[LocalDate] = album.items.flatMap(_.date)
    if dates.isEmpty then parsed
    else Title.Parsed(Some(Title.Span(dates.min, dates.max, Title.Grain.Day)), parsed.name)

  private def datedTitle(album: Catalog.Album, name: String): Title.Parsed =
    val dates: Seq[LocalDate] = album.items.flatMap(_.date)
    if dates.isEmpty then throw IllegalArgumentException("no such directory")
    val span: Title.Span = Title.Span(dates.min, dates.max, Title.Grain.Day)
    Title.Parsed(Some(span), Option.when(name.nonEmpty)(name))

  private def hasYearOrMonth(working: String): Boolean =
    val tokens: Seq[String] = tokenize(working)
    tokens.exists(isFourDigit) || tokens.exists(isMonth)

  private def leftoverName(working: String, album: Catalog.Album): String =
    val dates: Seq[LocalDate] = album.items.flatMap(_.date)
    val years: Set[Int] = dates.map(_.getYear).toSet
    val days: Set[Int] = dates.map(_.getDayOfMonth).toSet
    val tokens: Seq[String] = tokenize(working)
      .filterNot(token => isItemYear(token, years))
      .filterNot(token => isItemDay(token, days))
    val months: Seq[String] = tokens.filter(isMonth)
    val kept: Seq[String] =
      if months.sizeIs == 1 && tokens.exists(token => !isMonth(token)) then tokens.filterNot(isMonth)
      else if months.sizeIs == 1 then Seq.empty
      else tokens
    kept.mkString(" ")

  private def tokenize(working: String): Seq[String] =
    working.split(' ').toSeq.filter(_.nonEmpty).flatMap(splitMonthHyphen)

  private def splitMonthHyphen(token: String): Seq[String] =
    val parts: Seq[String] = token.split('-').toSeq.filter(_.nonEmpty)
    if parts.lengthIs > 1 && parts.forall(isMonth) then parts else Seq(token)

  private def isFourDigit(token: String): Boolean =
    token.length == 4 && token.forall(_.isDigit)

  private def isItemYear(token: String, years: Set[Int]): Boolean =
    isFourDigit(token) && years.contains(token.toInt)

  private def isItemDay(token: String, days: Set[Int]): Boolean =
    token.nonEmpty && token.length < 4 && token.forall(_.isDigit) && days.contains(token.toInt)

  private val monthWords: Set[String] = Set(
    "january", "february", "march", "april", "may", "june", "july", "august",
    "september", "october", "november", "december",
    "jan", "feb", "mar", "apr", "jun", "jul", "aug", "sep", "sept", "oct", "nov", "dec"
  )

  private def isMonth(token: String): Boolean =
    monthWords.contains(token.toLowerCase(java.util.Locale.ROOT))

  private[photo] def monthAllowsAll(
    span: Title.Span,
    claimed: Set[(LocalDate, String)],
    universe: Map[LocalDate, Set[String]]
  ): Boolean =
    val local: Set[(LocalDate, String)] = Include.pairsIn(span, universe)
    val missing: Int = local.count(pair => !claimed.contains(pair))
    val cap: Int = math.max(5, (local.size * 2) / 100)
    val every: Boolean = universe.keys.forall(day => span.contains(day) && claimed.exists(_._1.equals(day)))
    local.nonEmpty && every && missing <= cap && claimed.subsetOf(local)

  private def uniqueItem(albums: Seq[Catalog.Album], key: String): Catalog.Item =
    albums.flatMap(album => album.items.filter(_.key == key)) match
      case Seq(item) => item
      case Seq() => throw IllegalArgumentException("no such item")
      case _ => throw IllegalArgumentException("item key is not unique")

  private def requireCurrent(actual: String, supplied: String): Unit =
    if actual != supplied then
      throw IllegalArgumentException(
        s"current does not match ${Album.quote(actual)} ${Album.quote(supplied)}"
      )

  private def requireDate(text: String): Unit =
    val ok: Boolean = text match
      case datePattern(year, month, day, hour, minute) =>
        hour.toInt <= 23 && minute.toInt <= 59 && civilDate(year.toInt, month.toInt, day.toInt)
      case _ =>
        false
    if !ok then throw IllegalArgumentException("date is not valid")

  private def civilDate(year: Int, month: Int, day: Int): Boolean =
    try
      LocalDate.of(year, month, day)
      true
    catch case _: DateTimeException => false

  private val datePattern = """(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})""".r

  private def requireFilename(name: String): Unit =
    if name == "." || name == ".." || name.contains('/') then
      throw IllegalArgumentException("filename is not valid")
