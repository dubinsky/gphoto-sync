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

private[photo] object AgentYears extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val years: Seq[String] = Year.lsd(session.root).map(year => year.toString.stripPrefix("/"))
      val value: Json = Json.Obj(Seq("years" -> Json.Arr(years.map(Json.Str.apply))))
      (session, Result.Ok(AgentCommands.textOf(years), value))

private[photo] final class AgentMonths(yearText: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val year: Year =
        try session.parse(yearText) match
          case found: Year => found
          case _ => throw IllegalArgumentException("not a year")
        catch
          case _: (NoSuchElementException | DateTimeException | IllegalArgumentException) =>
            throw IllegalArgumentException("not a year")
      val present: Boolean = year.directory.isDirectory
      val months: Seq[String] =
        if present then year.lsd.map(month => month.toString.stripPrefix("/")) else Seq.empty
      val value: Json = Json.Obj(Seq(
        "year" -> Json.Str(year.toString.stripPrefix("/")),
        "present" -> Json.Bool(present),
        "months" -> Json.Arr(months.map(Json.Str.apply))
      ))
      (session, Result.Ok(AgentCommands.textOf(months), value))

private[photo] final class AgentDays(monthText: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val month: Month =
        try session.parse(monthText) match
          case found: Month => found
          case _ => throw IllegalArgumentException("not a month")
        catch
          case _: (NoSuchElementException | DateTimeException | IllegalArgumentException) =>
            throw IllegalArgumentException("not a month")
      val present: Boolean = month.directory.isDirectory
      val days: Seq[String] =
        if present then month.lsd.map(day => day.toString.stripPrefix("/")) else Seq.empty
      val value: Json = Json.Obj(Seq(
        "month" -> Json.Str(month.toString.stripPrefix("/")),
        "present" -> Json.Bool(present),
        "days" -> Json.Arr(days.map(Json.Str.apply))
      ))
      (session, Result.Ok(AgentCommands.textOf(days), value))

private[photo] final class AgentClaims(dayText: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val day: Day = AgentCommands.parseDay(session, dayText)
      if !day.directory.isDirectory then
        (session, Result.Ok("", claimsValue(day, present = false, Seq.empty)))
      else
        val (loaded, grouped, _) = AgentCommands.loadGrouped(session, day, readSidecars = false)
        val pictureNames: Set[String] = (Extension.normal ++ Extension.raw ++ Extension.image).map(_.name)
        if !grouped.exists((_, extensions) => extensions.exists(pictureNames.contains)) then
          (loaded, Result.Ok("", claimsValue(day, present = true, Seq.empty)))
        else
          // end restores the caller. A frame left current makes a later scan trust this session.
          val previous: Option[DirectoryIndex.Frame] = DirectoryIndex.begin(loaded)
          val scanned: (Session, Archive.Scan) =
            try
              val found: Archive.Scan = Archive.scan(loaded.root.configuration.archive)
              (DirectoryIndex.now, found)
            finally
              DirectoryIndex.end(previous)
          val (edited, scan) = scanned
          val resolved: Seq[Claims.Resolved] = Claims.resolve(Archive.Scan(
            scan.albums,
            scan.pictures.filter(_.folderDay.isEqual(day.date))
          ))
          (edited, Result.Ok(AgentCommands.textOf(resolved.map(_.line)), claimsValue(day, present = true, resolved)))

  private def claimsValue(day: Day, present: Boolean, claims: Seq[Claims.Resolved]): Json =
    Json.Obj(Seq(
      "path" -> Json.Str(Title.dayText(day.date)),
      "present" -> Json.Bool(present),
      "claims" -> Json.Arr(claims.map: claim =>
        val stem: String = Files.splitOnLast(claim.path, '/')._2.getOrElse(claim.path)
        Json.Obj(Seq(
          "path" -> Json.Str(claim.path),
          "stem" -> Json.Str(stem),
          "albums" -> Json.Arr(claim.albums.map(Json.Str.apply)),
          "excludedOnly" -> Json.Bool(claim.excludedOnly)
        ))
      )
    ))

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

private[photo] final class AgentLocate(dayText: String, stem: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      AgentCommands.locate(session, dayText, stem)

private[photo] final class AgentAlbumFile(
  id: Option[String],
  dryRun: Boolean,
  replace: Boolean,
  googleTitle: Option[String],
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
        else AgentCommands.writeAlbum(loaded, album, dryRun, replace, googleTitle)

private[photo] final class AgentRename(
  title: Option[String],
  current: Option[String],
  updated: Option[String],
  id: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val wantedTitle: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val wantedNew: String = updated.getOrElse(throw IllegalArgumentException("missing new"))
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        val album: Catalog.Album = AgentCommands.select(albums, wantedTitle, id)
        AgentCommands.acceptRequest(
          albums,
          AgentCommands.Action.RenameAlbum,
          album.id,
          "",
          wantedCurrent,
          wantedNew
        )
        AgentCommands.renameAlbum(loaded, album, wantedNew)

private[photo] final class AgentSetDescription(
  item: Option[String],
  current: Option[String],
  updated: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val text: String = updated.getOrElse(throw IllegalArgumentException("missing new"))
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        AgentCommands.acceptRequest(albums, AgentCommands.Action.SetDescription, "", key, wantedCurrent, text)
        if text.exists(ch => ch == '\t' || ch == '\n' || ch == '\r') then
          throw IllegalArgumentException("new contains a tab or a newline")
        val album: Catalog.Album = AgentCommands.locateItem(albums, key)._1
        AgentCommands.setDescription(loaded, album, key, wantedCurrent, text)

private[photo] final class AgentSetDate(
  item: Option[String],
  current: Option[String],
  updated: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val local: String = updated.getOrElse(throw IllegalArgumentException("missing new"))
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        AgentCommands.acceptRequest(albums, AgentCommands.Action.SetDate, "", key, wantedCurrent, local)
        val album: Catalog.Album = AgentCommands.locateItem(albums, key)._1
        AgentCommands.setDate(loaded, album, key, wantedCurrent, local)

private[photo] final class AgentDownload(
  title: Option[String],
  item: Option[String],
  current: Option[String],
  id: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val wantedTitle: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        val album: Catalog.Album = AgentCommands.select(albums, wantedTitle, id)
        AgentCommands.acceptRequest(albums, AgentCommands.Action.DownloadItem, album.id, key, wantedCurrent, "")
        AgentCommands.downloadItem(loaded, album, key, wantedCurrent)

private[photo] final class AgentAdd(
  title: Option[String],
  item: Option[String],
  current: Option[String],
  dest: Option[String],
  id: Option[String],
  destId: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val wantedTitle: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val destTitle: String = dest.getOrElse(throw IllegalArgumentException("missing dest"))
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        AgentCommands.putInAlbum(loaded, albums, wantedTitle, id, key, wantedCurrent, destTitle, destId)

private[photo] final class AgentRemove(
  title: Option[String],
  item: Option[String],
  current: Option[String],
  id: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val wantedTitle: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        AgentCommands.takeOut(loaded, albums, wantedTitle, id, key, wantedCurrent)

private[photo] final class AgentMove(
  title: Option[String],
  item: Option[String],
  current: Option[String],
  dest: Option[String],
  id: Option[String],
  destId: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val wantedTitle: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val destTitle: String = dest.getOrElse(throw IllegalArgumentException("missing dest"))
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        AgentCommands.moveItem(loaded, albums, wantedTitle, id, key, wantedCurrent, destTitle, destId)

private[photo] final class AgentDelete(
  title: Option[String],
  item: Option[String],
  current: Option[String],
  id: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val wantedTitle: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        AgentCommands.deleteItem(loaded, albums, wantedTitle, id, key, wantedCurrent)

private[photo] final class AgentUpload(
  path: Option[String],
  dest: Option[String],
  destId: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val wantedPath: String = path.getOrElse(throw IllegalArgumentException("missing path"))
      val destTitle: String = dest.getOrElse(throw IllegalArgumentException("missing dest"))
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        AgentCommands.uploadItem(loaded, albums, wantedPath, destTitle, destId)

private[photo] final class AgentCreate(title: Option[String]) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val wanted: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        AgentCommands.createAlbum(loaded, albums, wanted)

private[photo] final class AgentDeleteAlbum(title: Option[String], id: Option[String]) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentCommands.catching(session):
      val wanted: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val (loaded, albums) = session.loadCatalog
      AgentCommands.catching(loaded):
        AgentCommands.deleteAlbum(loaded, albums, wanted, id)

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
  enum Action derives CanEqual:
    case RenameAlbum, SetDescription, SetDate, DownloadItem, DeleteItem

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
    (session, Result.Ok(s"skipped $reason\n", matchValue(album, Seq.empty, Seq.empty, Seq.empty)))

  def matchItems(session: Session, album: Catalog.Album): (Session, Result) =
    val (loaded, hits, nonHits) = gather(session, album)
    val rows: Seq[String] =
      Seq(s"matched ${hits.length} of ${album.items.length}") ++
        hits.map(hit => s"${Title.dayText(hit._1)} ${hit._2} ${hit._3}") ++
        nonHits.map:
          case NonHit.Miss(date, filename, _) =>
            s"unmatched ${date.fold("-")(Title.dayText)} $filename"
          case NonHit.Mismatch(catalogDay, filename, archiveDay, stem) =>
            s"date-mismatch ${Title.dayText(catalogDay)} $filename ${Title.dayText(archiveDay)} $stem"
    val value: Json = matchValue(
      album,
      hits.map(hit => hit._1 -> hit._3),
      nonHits.collect { case NonHit.Miss(_, _, key) => key },
      nonHits.collect { case mismatch: NonHit.Mismatch => mismatch }
    )
    (loaded, Result.Ok(textOf(rows), value))

  // Match skips an unread album before gather. This walk does not. A date of - is not a row.
  def locate(session: Session, dayText: String, stem: String): (Session, Result) =
    val day: Day = parseDay(session, dayText)
    val shown: String = Title.dayText(day.date)
    if !day.directory.isDirectory then
      (session, Result.Ok("", locateValue(shown, stem, directory = false, present = false, Seq.empty)))
    else
      val (loaded, grouped, _) = loadGrouped(session, day, readSidecars = false)
      val byStem: Map[String, Set[String]] = grouped.toMap
      if !byStem.contains(stem) then
        (loaded, Result.Ok("", locateValue(shown, stem, directory = true, present = false, Seq.empty)))
      else
        val (cataloged, albums) = loaded.loadCatalog
        val rows: Seq[(Catalog.Album, Catalog.Item, LocalDate)] = albums.flatMap: album =>
          album.items.flatMap: item =>
            item.date match
              case Some(date) if date.isEqual(day.date) =>
                val (name, extension) = Files.splitOnLast(item.filename, '.')
                if resolve(byStem, name, extension).contains(stem) then Some((album, item, date)) else None
              case _ =>
                None
        val lines: Seq[String] = rows.map: (album, item, date) =>
          s"${album.title} ${date.toString} ${item.filename} ${showUploader(item.uploader)}"
        (cataloged, Result.Ok(textOf(lines), locateValue(shown, stem, directory = true, present = true, rows)))

  // A miss is not a hit. An adjacent hit is neither, so album-file does not keep a second copy.
  enum NonHit:
    case Miss(date: Option[LocalDate], filename: String, key: String)
    case Mismatch(catalogDay: LocalDate, filename: String, archiveDay: LocalDate, stem: String)

  def gather(
    session: Session,
    album: Catalog.Album
  ): (Session, Seq[(LocalDate, String, String)], Seq[NonHit]) =
    album.items.foldLeft(
      (session, Seq.empty[(LocalDate, String, String)], Seq.empty[NonHit])
    ): (acc, item) =>
      val (current, matched, nonHits) = acc
      // Catalog names are not files. nameAndExtension is this same last-dot split.
      val (stem, extension) = Files.splitOnLast(item.filename, '.')
      item.date match
        case None =>
          (current, matched, nonHits :+ NonHit.Miss(None, item.filename, item.key))
        case Some(date) =>
          val (next, grouped, _) = loadGrouped(current, civilDay(current.root, date))
          resolve(grouped.toMap, stem, extension) match
            case Some(local) =>
              (next, matched :+ (date, item.filename, local), nonHits)
            case None =>
              val (after, found) = Seq(date.minusDays(1), date.plusDays(1)).foldLeft(
                (next, Seq.empty[(LocalDate, String)])
              ): (dayAcc, neighbor) =>
                val (daySession, soFar) = dayAcc
                val (loaded, neighborGrouped, _) =
                  loadGrouped(daySession, civilDay(daySession.root, neighbor))
                val hitStem: Option[String] = resolve(neighborGrouped.toMap, stem, extension)
                (loaded, hitStem.fold(soFar)(local => soFar :+ (neighbor, local)))
              if found.isEmpty then
                (after, matched, nonHits :+ NonHit.Miss(Some(date), item.filename, item.key))
              else
                val mismatches: Seq[NonHit] = found.map: (archiveDay, local) =>
                  NonHit.Mismatch(date, item.filename, archiveDay, local)
                (after, matched, nonHits ++ mismatches)

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

  private[photo] def resolve(
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

  private def locateValue(
    day: String,
    stem: String,
    directory: Boolean,
    present: Boolean,
    rows: Seq[(Catalog.Album, Catalog.Item, LocalDate)]
  ): Json =
    Json.Obj(Seq(
      "day" -> Json.Str(day),
      "stem" -> Json.Str(stem),
      "directory" -> Json.Bool(directory),
      "present" -> Json.Bool(present),
      "ambiguous" -> Json.Bool(rows.length > 1),
      "matches" -> Json.Arr(rows.map: (album, item, date) =>
        Json.Obj(Seq(
          "title" -> Json.Str(album.title),
          "date" -> Json.Str(date.toString),
          "filename" -> Json.Str(item.filename),
          "uploader" -> Json.Str(showUploader(item.uploader)),
          "id" -> Json.Str(album.id),
          "key" -> Json.Str(item.key)
        ))
      )
    ))

  private def matchValue(
    album: Catalog.Album,
    hits: Seq[(LocalDate, String)],
    misses: Seq[String],
    mismatches: Seq[NonHit.Mismatch]
  ): Json =
    Json.Obj(Seq(
      "id" -> Json.Str(album.id),
      "matched" -> Json.Arr(hits.map: (day, stem) =>
        Json.Obj(Seq(
          "day" -> Json.Str(Title.dayText(day)),
          "stem" -> Json.Str(stem)
        ))
      ),
      "unmatched" -> Json.Arr(misses.map(Json.Str.apply)),
      "dateMismatches" -> Json.Arr(mismatches.map: mismatch =>
        Json.Obj(Seq(
          "catalogDay" -> Json.Str(Title.dayText(mismatch.catalogDay)),
          "filename" -> Json.Str(mismatch.filename),
          "archiveDay" -> Json.Str(Title.dayText(mismatch.archiveDay)),
          "stem" -> Json.Str(mismatch.stem)
        ))
      )
    ))

  private val RenameStep: String = "More options, then Edit album, Rename album, or Rename"
  private val DescriptionStep: String = "Open info, or the i key, then Add a description"
  private val DateStep: String = "Date taken, then Save, Done, or OK"
  private val DownloadStep: String = "More options, then Download"
  private val AddStep: String = "More options, then Add to album, then the existing album"
  private val RemoveStep: String = "More options, then Remove from album, then Remove"
  private val TrashStep: String = "Move to trash"
  private val UploadStep: String = "Add photos or Select photos, then Select from computer"
  private val CreateStep: String = "Create album, then Edit album name, Done"
  private val DeleteAlbumStep: String = "More options, then Delete album, then Delete"

  def renameAlbum(session: Session, album: Catalog.Album, updated: String): (Session, Result) =
    withLock(session, album.title)(heldRename(session, album, updated))

  def setDescription(
    session: Session,
    album: Catalog.Album,
    key: String,
    current: String,
    text: String
  ): (Session, Result) =
    withLock(session, album.title)(heldDescription(session, key, current, text))

  def setDate(
    session: Session,
    album: Catalog.Album,
    key: String,
    current: String,
    local: String
  ): (Session, Result) =
    withLock(session, album.title)(heldDate(session, key, current, local))

  def downloadItem(
    session: Session,
    album: Catalog.Album,
    key: String,
    current: String
  ): (Session, Result) =
    val item: Catalog.Item = album.items.find(_.key == key).getOrElse(
      throw IllegalArgumentException("no such item")
    )
    if stagingFile(session, item).exists then
      itemOk(session, album, item, "download-item", "unchanged")
    else
      withLock(session, album.title)(heldDownload(session, album.id, key, current))

  def putInAlbum(
    session: Session,
    albums: Seq[Catalog.Album],
    title: String,
    id: Option[String],
    key: String,
    current: String,
    destTitle: String,
    destId: Option[String]
  ): (Session, Result) =
    val album: Catalog.Album = select(albums, title, id)
    val destination: Catalog.Album = select(albums, destTitle, destId)
    val item: Catalog.Item = album.items.find(_.key == key).getOrElse(
      throw IllegalArgumentException("no such item")
    )
    requireCurrent(item.filename, current)
    if album.id == destination.id then throw IllegalArgumentException("destination is the source album")
    acceptPut(session, destination, item)
    withLock(session, album.title)(heldPut(session, album.id, key, current, destination.id))

  def takeOut(
    session: Session,
    albums: Seq[Catalog.Album],
    title: String,
    id: Option[String],
    key: String,
    current: String
  ): (Session, Result) =
    val album: Catalog.Album = select(albums, title, id)
    val item: Catalog.Item = album.items.find(_.key == key).getOrElse(
      throw IllegalArgumentException("no such item")
    )
    requireCurrent(item.filename, current)
    withLock(session, album.title)(heldTake(session, album.id, key, current))

  def moveItem(
    session: Session,
    albums: Seq[Catalog.Album],
    title: String,
    id: Option[String],
    key: String,
    current: String,
    destTitle: String,
    destId: Option[String]
  ): (Session, Result) =
    val album: Catalog.Album = select(albums, title, id)
    val destination: Catalog.Album = select(albums, destTitle, destId)
    val item: Catalog.Item = album.items.find(_.key == key).getOrElse(
      throw IllegalArgumentException("no such item")
    )
    requireCurrent(item.filename, current)
    if album.id == destination.id then throw IllegalArgumentException("destination is the source album")
    acceptPut(session, destination, item)
    withLock(session, album.title)(heldMove(session, album.id, key, current, destination.id))

  def deleteItem(
    session: Session,
    albums: Seq[Catalog.Album],
    title: String,
    id: Option[String],
    key: String,
    current: String
  ): (Session, Result) =
    val album: Catalog.Album = select(albums, title, id)
    val item: Catalog.Item = album.items.find(_.key == key).getOrElse(
      throw IllegalArgumentException("no such item")
    )
    requireCurrent(item.filename, current)
    acceptDelete(album, item)
    withLock(session, album.title)(heldDelete(session, album.id, key, current))

  def uploadItem(
    session: Session,
    albums: Seq[Catalog.Album],
    pathText: String,
    destTitle: String,
    destId: Option[String]
  ): (Session, Result) =
    val (_, relative) = uploadFile(session, pathText)
    val (day, stem, _, extension) = uploadName(relative)
    val destination: Catalog.Album = select(albums, destTitle, destId)
    val loaded: Session = refuseUpload(session, albums, destination, day, stem, extension)
    withLock(loaded, destination.title)(heldUpload(loaded, pathText, destination.id))

  def createAlbum(session: Session, albums: Seq[Catalog.Album], title: String): (Session, Result) =
    acceptCreate(title, albums)
    withLock(session, title)(heldCreate(session, title))

  def deleteAlbum(
    session: Session,
    albums: Seq[Catalog.Album],
    title: String,
    id: Option[String]
  ): (Session, Result) =
    val album: Catalog.Album = select(albums, title, id)
    acceptAlbumDelete(album)
    withLock(session, album.title)(heldDeleteAlbum(session, album.id))

  // claim is not reentrant, and each row has to publish before the next click.
  // One push holds the lock and the browser. The public commands still take both themselves.
  private[photo] def push(session: Session, dryRun: Boolean): (Session, Result) =
    val planned: Either[(Session, Result), (Session, Seq[PushAction])] =
      try Right(planPush(session))
      catch case error: IllegalArgumentException =>
        Left((session, Result.Usage(Operation.messageOf(error))))
    planned match
      case Left(done) => done
      case Right((ready, actions)) =>
        if dryRun || actions.isEmpty then (ready, Result.Ok(pushText(actions), Json.Null))
        else withLock(ready, actions.head.shownTitle)(runPush(ready, actions))

  private def withLock(session: Session, title: String)(body: => (Session, Result)): (Session, Result) =
    val places: Refresh.Places = Refresh.placesOf()
    Refresh.claim(places.lock) match
      case Refresh.Claim.Busy =>
        val (kept, result) = Refresh.busy(session, places)
        result match
          case Result.Failed(text, value, message) =>
            (kept, Result.Failed(PlaywrightPhotosUi.redactSecrets(text, title), value, message))
          case other =>
            (kept, other)
      case Refresh.Claim.Owned(channel, lock) =>
        try body
        finally Refresh.release(channel, lock)

  private def heldRename(
    session: Session,
    album: Catalog.Album,
    updated: String
  ): (Session, Result) =
    val accepted: Either[(Session, Result), Catalog.Album] =
      try Right(reread(session, album, updated))
      catch
        case error: (IllegalArgumentException | IllegalStateException) =>
          Left((session, Result.Usage(Operation.messageOf(error))))
    accepted match
      case Left(done) => done
      case Right(fresh) =>
        var ui: PhotosAct = null
        try
          val launched: Either[(Session, Result), PhotosAct] =
            try
              // A notice would write status.txt and drop a crashed refresh's resume phase.
              Right(Refresh.openAct(_ => ()))
            catch
              case error: Exception =>
                Left(renameFailed(session, fresh, "one-click", Operation.messageOf(error)))
          launched match
            case Left(done) => done
            case Right(opened) =>
              ui = opened
              try
                ui.renameAlbum(fresh, updated)
                publishTitle(session, fresh, updated) match
                  case Left(detail) => renameFailed(session, fresh, "refresh-only", detail)
                  case Right(next) => renameOk(next, fresh, updated)
              catch
                case miss: Refresh.ActMiss =>
                  val recovery: String = if miss.confirmed then "refresh-only" else "one-click"
                  val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("rename failed")
                  renameFailed(session, fresh, recovery, detail)
                case error: Exception =>
                  renameFailed(session, fresh, "one-click", Operation.messageOf(error))
        finally
          if ui != null then
            try ui.close()
            catch case _: Exception => ()

  private def reread(session: Session, album: Catalog.Album, updated: String): Catalog.Album =
    val file: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    val albums: Seq[Catalog.Album] = Catalog.read(file)
    val fresh: Catalog.Album = albums.find(_.id == album.id).getOrElse(
      throw IllegalArgumentException("no such album")
    )
    acceptRequest(albums, Action.RenameAlbum, fresh.id, "", album.title, updated)
    fresh

  private def publishTitle(session: Session, album: Catalog.Album, updated: String): Either[String, Session] =
    val directory: File = File(session.root.configuration.archive, ".gphoto-sync")
    val live: File = File(directory, "catalog.txt")
    val partial: File = File(directory, "catalog.txt.partial")
    val previous: File = File(directory, "catalog.txt.previous")
    val source: String = JFiles.readString(live.toPath, StandardCharsets.UTF_8)
    val albums: Seq[Catalog.Album] = Catalog.read(live)
    val blocks: Map[String, Seq[String]] = RefreshPlan.blocksOf(source)
    val groups: Either[String, Seq[Seq[String]]] =
      albums.foldLeft[Either[String, Seq[Seq[String]]]](Right(Seq.empty)):
        case (Left(message), _) => Left(message)
        case (Right(acc), next) =>
          if next.id == album.id then
            blocks.get(album.id) match
              case Some(block) => Right(acc :+ RefreshPlan.retitle(block, updated))
              case None => Left("missing album block")
          else Right(acc :+ RefreshPlan.copiedLines(next, blocks))
    groups.flatMap: lines =>
      try
        Files.write(RefreshPlan.renderLines(lines), partial)
        Right(())
      catch case error: Exception =>
        Left(Operation.messageOf(error))
    .flatMap(_ => RefreshPlan.checkRetitle(partial))
    .flatMap(_ => RefreshPlan.swap(live, partial, previous))
    .map(_ => session.copy(catalogCache = None))

  private def renameOk(session: Session, album: Catalog.Album, updated: String): (Session, Result) =
    val text: String = PlaywrightPhotosUi.redactSecrets(s"rename-album ${Album.quote(updated)}\n", updated)
    val value: Json = Json.Obj(Seq(
      "action" -> Json.Str("rename-album"),
      "title" -> Json.Str(updated),
      "catalog" -> Json.Str("updated"),
      "albumId" -> Json.Str(album.id),
      "itemKey" -> Json.Str("")
    ))
    (session, Result.Ok(text, value))

  private def renameFailed(
    session: Session,
    album: Catalog.Album,
    recovery: String,
    detail: String
  ): (Session, Result) =
    val shown: String = collapsedText(detail, "rename failed")
    val text: String =
      s"""failed rename-album
         |album ${Album.quote(album.title)}
         |step $RenameStep
         |recovery $recovery
         |catalog unchanged
         |$shown
         |""".stripMargin
    val value: Json = Json.Obj(Seq(
      "action" -> Json.Str("rename-album"),
      "title" -> Json.Str(album.title),
      "step" -> Json.Str(RenameStep),
      "recovery" -> Json.Str(recovery),
      "catalog" -> Json.Str("unchanged"),
      "albumId" -> Json.Str(album.id),
      "itemKey" -> Json.Str("")
    ))
    (session, Result.Failed(PlaywrightPhotosUi.redactSecrets(text, album.title), value, Refresh.MutationFailed))

  private def heldDescription(
    session: Session,
    key: String,
    current: String,
    text: String
  ): (Session, Result) =
    val (album, item) = rereadItem(session, Action.SetDescription, key, current, text)
    var ui: PhotosAct = null
    try
      // A notice would write status.txt and drop a crashed refresh's resume phase.
      ui = Refresh.openAct(_ => ())
      ui.setDescription(item, text)
      itemOk(session, album, item, "set-description", "unchanged")
    catch
      case miss: Refresh.ActMiss =>
        val recovery: String = if miss.confirmed then "refresh-only" else "one-click"
        val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("set-description failed")
        itemFailed(session, album, item, "set-description", DescriptionStep, recovery, detail)
      case error: Exception =>
        itemFailed(session, album, item, "set-description", DescriptionStep, "one-click", Operation.messageOf(error))
    finally
      if ui != null then
        try ui.close()
        catch case _: Exception => ()

  private def heldDate(
    session: Session,
    key: String,
    current: String,
    local: String
  ): (Session, Result) =
    val (album, item) = rereadItem(session, Action.SetDate, key, current, local)
    val day: LocalDate = LocalDate.parse(local.take(10))
    var ui: PhotosAct = null
    try
      // A notice would write status.txt and drop a crashed refresh's resume phase.
      ui = Refresh.openAct(_ => ())
      ui.setDate(item, local)
      publishDate(session, album, item, day) match
        case Left(detail) =>
          itemFailed(session, album, item, "set-date", DateStep, "refresh-only", detail)
        case Right(next) =>
          itemOk(next, album, item, "set-date", "updated")
    catch
      case miss: Refresh.ActMiss =>
        val recovery: String = if miss.confirmed then "refresh-only" else "one-click"
        val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("set-date failed")
        itemFailed(session, album, item, "set-date", DateStep, recovery, detail)
      case error: Exception =>
        itemFailed(session, album, item, "set-date", DateStep, "one-click", Operation.messageOf(error))
    finally
      if ui != null then
        try ui.close()
        catch case _: Exception => ()

  private def heldDownload(
    session: Session,
    albumId: String,
    key: String,
    current: String
  ): (Session, Result) =
    val (album, item) = rereadDownload(session, albumId, key, current)
    val dest: File = stagingFile(session, item)
    // The file may have appeared while this command waited for the lock.
    if dest.exists then itemOk(session, album, item, "download-item", "unchanged")
    else
      var ui: PhotosAct = null
      try
        // saveAs writes the catalog filename and does not create this directory.
        Files.mkDirs(dest.getParentFile)
        // A notice would write status.txt and drop a crashed refresh's resume phase.
        ui = Refresh.openAct(_ => ())
        ui.downloadItem(item, dest)
        if dest.isFile then itemOk(session, album, item, "download-item", "unchanged")
        else
          // Download does not change Google Photos, so a missing file after the click is one-click.
          itemFailed(session, album, item, "download-item", DownloadStep, "one-click", "staged file is absent")
      catch
        case miss: Refresh.ActMiss =>
          val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("download-item failed")
          itemFailed(session, album, item, "download-item", DownloadStep, "one-click", detail)
        case error: Exception =>
          itemFailed(session, album, item, "download-item", DownloadStep, "one-click", Operation.messageOf(error))
      finally
        if ui != null then
          try ui.close()
          catch case _: Exception => ()

  private def rereadDownload(
    session: Session,
    albumId: String,
    key: String,
    current: String
  ): (Catalog.Album, Catalog.Item) =
    val file: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    val albums: Seq[Catalog.Album] = Catalog.read(file)
    val album: Catalog.Album = albums.find(_.id == albumId).getOrElse(
      throw IllegalArgumentException("no such album")
    )
    acceptRequest(albums, Action.DownloadItem, album.id, key, current, "")
    val item: Catalog.Item = album.items.find(_.key == key).getOrElse(
      throw IllegalArgumentException("no such item")
    )
    (album, item)

  private[photo] def stagingFile(session: Session, item: Catalog.Item): File =
    val day: String = item.date.fold("undated")(date => date.toString.replace('-', '/'))
    val inbox: File = File(File(session.root.configuration.archive, ".gphoto-sync"), ".download")
    File(File(inbox, day), item.filename)

  private def rereadItem(
    session: Session,
    action: Action,
    key: String,
    current: String,
    updated: String
  ): (Catalog.Album, Catalog.Item) =
    val file: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    val albums: Seq[Catalog.Album] = Catalog.read(file)
    val found: (Catalog.Album, Catalog.Item) = locateItem(albums, key)
    acceptRequest(albums, action, "", key, current, updated)
    found

  private def publishDate(
    session: Session,
    album: Catalog.Album,
    item: Catalog.Item,
    day: LocalDate
  ): Either[String, Session] =
    try
      val directory: File = File(session.root.configuration.archive, ".gphoto-sync")
      val live: File = File(directory, "catalog.txt")
      val partial: File = File(directory, "catalog.txt.partial")
      val previous: File = File(directory, "catalog.txt.previous")
      val source: String = JFiles.readString(live.toPath, StandardCharsets.UTF_8)
      val albums: Seq[Catalog.Album] = Catalog.read(live)
      val blocks: Map[String, Seq[String]] = RefreshPlan.blocksOf(source)
      val groups: Either[String, Seq[Seq[String]]] =
        albums.foldLeft[Either[String, Seq[Seq[String]]]](Right(Seq.empty)):
          case (Left(message), _) => Left(message)
          case (Right(acc), next) =>
            if next.id == album.id then
              if !blocks.contains(album.id) then Left("missing album block")
              else if !next.items.exists(_.key == item.key) then Left("no such item")
              else
                val revised: Catalog.Album = next.copy(items = next.items.map: found =>
                  if found.key == item.key then found.copy(date = Some(day)) else found
                )
                Right(acc :+ RefreshPlan.recordLines(revised))
            else Right(acc :+ RefreshPlan.copiedLines(next, blocks))
      groups.flatMap: lines =>
        try
          Files.write(RefreshPlan.renderLines(lines), partial)
          Right(())
        catch case error: Exception =>
          Left(Operation.messageOf(error))
      .flatMap(_ => RefreshPlan.checkRetitle(partial))
      .flatMap(_ => RefreshPlan.swap(live, partial, previous))
      .map(_ => session.copy(catalogCache = None))
    catch
      case error: Exception =>
        Left(Operation.messageOf(error))

  private def heldPut(
    session: Session,
    sourceId: String,
    key: String,
    current: String,
    destinationId: String
  ): (Session, Result) =
    val (source, item, destination) = rereadAdd(session, sourceId, key, current, destinationId)
    var ui: PhotosAct = null
    try
      // That option creates an album, so Chrome stays closed.
      if destination.title == "New album" then
        throw Refresh.ActMiss("New album is not a destination", confirmed = false)
      // A notice would write status.txt and drop a crashed refresh's resume phase.
      ui = Refresh.openAct(_ => ())
      commitPut(ui, session, item, destination, current) match
        case Right(done) =>
          addOk(done.session, destination, current, done.key, "add-to-album")
        case Left(message) =>
          itemFailed(session, source, item, "add-to-album", AddStep, "refresh-only", message)
    catch
      case miss: Refresh.ActMiss =>
        val recovery: String = if miss.confirmed then "refresh-only" else "one-click"
        val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("add-to-album failed")
        itemFailed(session, source, item, "add-to-album", AddStep, recovery, detail)
      case error: Exception =>
        itemFailed(session, source, item, "add-to-album", AddStep, "one-click", Operation.messageOf(error))
    finally
      if ui != null then
        try ui.close()
        catch case _: Exception => ()

  private def heldTake(
    session: Session,
    albumId: String,
    key: String,
    current: String
  ): (Session, Result) =
    val (album, item) = rereadTake(session, albumId, key, current)
    var ui: PhotosAct = null
    try
      // A notice would write status.txt and drop a crashed refresh's resume phase.
      ui = Refresh.openAct(_ => ())
      commitTake(ui, session, album, item) match
        case Right(next) =>
          itemOk(next, album, item, "remove-from-album", "updated")
        case Left(message) =>
          itemFailed(session, album, item, "remove-from-album", RemoveStep, "refresh-only", message)
    catch
      case miss: Refresh.ActMiss =>
        val recovery: String = if miss.confirmed then "refresh-only" else "one-click"
        val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("remove-from-album failed")
        itemFailed(session, album, item, "remove-from-album", RemoveStep, recovery, detail)
      case error: Exception =>
        itemFailed(session, album, item, "remove-from-album", RemoveStep, "one-click", Operation.messageOf(error))
    finally
      if ui != null then
        try ui.close()
        catch case _: Exception => ()

  private def heldMove(
    session: Session,
    sourceId: String,
    key: String,
    current: String,
    destinationId: String
  ): (Session, Result) =
    val (source, item, destination) = rereadAdd(session, sourceId, key, current, destinationId)
    var ui: PhotosAct = null
    try
      // That option creates an album, so Chrome stays closed.
      if destination.title == "New album" then
        throw Refresh.ActMiss("New album is not a destination", confirmed = false)
      // A notice would write status.txt and drop a crashed refresh's resume phase.
      ui = Refresh.openAct(_ => ())
      val read: Catalog.Item = ui.putInAlbum(item, destination, current)
      val line: Catalog.Item = Catalog.Item(
        current,
        read.date.orElse(item.date),
        read.key,
        read.uploader.orElse(item.uploader)
      )
      try
        ui.takeOut(source, item)
        publishAlbum(session, Seq(
          destination.id -> (dest => revisedAdd(dest, line)),
          source.id -> (next => revisedTake(next, item))
        )) match
          case Right(next) =>
            moveOk(next, source, destination, current, line.key)
          case Left(message) =>
            moveFailed(session, source, destination, item, RemoveStep, "refresh-only", message)
      catch
        // The add confirm already ran. A miss on the remove is not one-click.
        case miss: Refresh.ActMiss =>
          val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("move-item failed")
          moveFailed(session, source, destination, item, RemoveStep, "refresh-only", detail)
        case error: Exception =>
          moveFailed(session, source, destination, item, RemoveStep, "refresh-only", Operation.messageOf(error))
    catch
      case miss: Refresh.ActMiss =>
        val recovery: String = if miss.confirmed then "refresh-only" else "one-click"
        val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("move-item failed")
        moveFailed(session, source, destination, item, AddStep, recovery, detail)
      case error: Exception =>
        moveFailed(session, source, destination, item, AddStep, "one-click", Operation.messageOf(error))
    finally
      if ui != null then
        try ui.close()
        catch case _: Exception => ()

  private def heldDelete(
    session: Session,
    albumId: String,
    key: String,
    current: String
  ): (Session, Result) =
    val (album, item) = rereadDelete(session, albumId, key, current)
    var ui: PhotosAct = null
    try
      // A notice would write status.txt and drop a crashed refresh's resume phase.
      ui = Refresh.openAct(_ => ())
      ui.trash(album, item)
      publishAlbum(session, Seq(album.id -> (next => revisedTake(next, item)))) match
        case Right(next) =>
          itemOk(next, album, item, "delete-item", "updated")
        case Left(message) =>
          itemFailed(session, album, item, "delete-item", TrashStep, "refresh-only", message)
    catch
      case miss: Refresh.ActMiss =>
        val recovery: String = if miss.confirmed then "refresh-only" else "one-click"
        val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("delete-item failed")
        itemFailed(session, album, item, "delete-item", TrashStep, recovery, detail)
      case error: Exception =>
        itemFailed(session, album, item, "delete-item", TrashStep, "one-click", Operation.messageOf(error))
    finally
      if ui != null then
        try ui.close()
        catch case _: Exception => ()

  // The reread stays outside the catch. A refusal here is usage, and Chrome stays closed.
  private def heldCreate(session: Session, title: String): (Session, Result) =
    rereadCreate(session, title)
    var ui: PhotosAct = null
    var confirmed = false
    try
      // A notice would write status.txt and drop a crashed refresh's resume phase.
      ui = Refresh.openAct(_ => ())
      val (cards, read) = ui.createAlbum(title)
      confirmed = true
      val directory: File = File(session.root.configuration.archive, ".gphoto-sync")
      val live: File = File(directory, "catalog.txt")
      val source: String = JFiles.readString(live.toPath, StandardCharsets.UTF_8)
      val stored: Seq[Catalog.Album] = Catalog.read(live)
      val lines: Seq[String] = cards.map(Refresh.lineOf)
      val reads: Map[String, Catalog.Album] = read.toSeq.map(album => album.id -> album).toMap
      RefreshPlan.plan(
        lines,
        stored,
        RefreshPlan.Mode.Album(Seq(title)),
        allowShrink = false,
        reads,
        source
      ) match
        case Left(message) =>
          createFailed(session, title, "refresh-only", message)
        case Right(prepared) =>
          RefreshPlan.publish(directory, prepared) match
            case Left(message) =>
              createFailed(session, title, "refresh-only", message)
            case Right(()) =>
              val id: String = prepared.albums.find(_.title == title).fold("")(_.id)
              createOk(session.copy(catalogCache = None), title, id)
    catch
      case miss: Refresh.ActMiss =>
        val recovery: String = if miss.confirmed then "refresh-only" else "one-click"
        val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("create-album failed")
        createFailed(session, title, recovery, detail)
      case error: Exception =>
        val recovery: String = if confirmed then "refresh-only" else "one-click"
        createFailed(session, title, recovery, Operation.messageOf(error))
    finally
      if ui != null then
        try ui.close()
        catch case _: Exception => ()

  private def rereadCreate(session: Session, title: String): Unit =
    val catalog: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    acceptCreate(title, Catalog.read(catalog))

  // The reread stays outside the catch. A refusal here is usage, and Chrome stays closed.
  private def heldDeleteAlbum(session: Session, albumId: String): (Session, Result) =
    val album: Catalog.Album = rereadAlbumDelete(session, albumId)
    var ui: PhotosAct = null
    var confirmed = false
    try
      // A notice would write status.txt and drop a crashed refresh's resume phase.
      ui = Refresh.openAct(_ => ())
      val cards: Seq[AlbumCard] = ui.deleteAlbum(album)
      confirmed = true
      omitAlbum(session, album.id, cards) match
        case Left(message) =>
          deleteAlbumFailed(session, album, "refresh-only", message)
        case Right(next) =>
          deleteAlbumOk(next, album)
    catch
      case miss: Refresh.ActMiss =>
        val recovery: String = if miss.confirmed then "refresh-only" else "one-click"
        val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("delete-album failed")
        deleteAlbumFailed(session, album, recovery, detail)
      case error: Exception =>
        val recovery: String = if confirmed then "refresh-only" else "one-click"
        deleteAlbumFailed(session, album, recovery, Operation.messageOf(error))
    finally
      if ui != null then
        try ui.close()
        catch case _: Exception => ()

  private def rereadAlbumDelete(session: Session, albumId: String): Catalog.Album =
    val catalog: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    val album: Catalog.Album = Catalog.read(catalog).find(_.id == albumId).getOrElse(
      throw IllegalArgumentException("no such album")
    )
    acceptAlbumDelete(album)
    album

  private def acceptAlbumDelete(album: Catalog.Album): Unit =
    if album.owner.isDefined then throw IllegalArgumentException("no owner line")
    if album.autoAdd then throw IllegalArgumentException("album is auto-add")

  // A short list can be a truncated scroll. 250 cards is the refresh backstop, and the set has to be
  // exactly every stored id except this one. Counts and titles on those cards are not a rewrite.
  private def omitAlbum(session: Session, deletedId: String, cards: Seq[AlbumCard]): Either[String, Session] =
    try
      val directory: File = File(session.root.configuration.archive, ".gphoto-sync")
      val live: File = File(directory, "catalog.txt")
      val partial: File = File(directory, "catalog.txt.partial")
      val previous: File = File(directory, "catalog.txt.previous")
      val source: String = JFiles.readString(live.toPath, StandardCharsets.UTF_8)
      val stored: Seq[Catalog.Album] = Catalog.read(live)
      if cards.length < RefreshPlan.Floor then Left("rows < 250")
      else
        val cardIds: Set[String] = cards.map(_.id).toSet
        val wanted: Set[String] = stored.map(_.id).toSet - deletedId
        if cardIds != wanted then Left("album set")
        else
          val blocks: Map[String, Seq[String]] = RefreshPlan.blocksOf(source)
          val survivors: Seq[Catalog.Album] = stored.filter(_.id != deletedId)
          val groups: Either[String, Seq[Seq[String]]] =
            survivors.foldLeft[Either[String, Seq[Seq[String]]]](Right(Seq.empty)):
              case (Left(message), _) => Left(message)
              case (Right(acc), album) =>
                blocks.get(album.id) match
                  case Some(lines) => Right(acc :+ lines)
                  case None => Left("missing album block")
          groups.flatMap: lines =>
            try
              Files.write(RefreshPlan.renderLines(lines), partial)
              Right(())
            catch case error: Exception =>
              Left(Operation.messageOf(error))
          .flatMap(_ => omitCheck(partial, deletedId, survivors, blocks))
          .flatMap(_ => RefreshPlan.swap(live, partial, previous))
          .map(_ => session.copy(catalogCache = None))
    catch
      case error: Exception =>
        Left(Operation.messageOf(error))

  private def omitCheck(
    partial: File,
    deletedId: String,
    survivors: Seq[Catalog.Album],
    blocks: Map[String, Seq[String]]
  ): Either[String, Unit] =
    RefreshPlan.checkRetitle(partial).flatMap: _ =>
      val written: String = JFiles.readString(partial.toPath, StandardCharsets.UTF_8)
      val again: Map[String, Seq[String]] = RefreshPlan.blocksOf(written)
      val read: Seq[Catalog.Album] = Catalog.read(partial)
      if read.exists(_.id == deletedId) then Left("deleted id remains")
      else if read.map(_.id).toSet != survivors.map(_.id).toSet then Left("album set")
      else if survivors.exists(album => again.get(album.id) != blocks.get(album.id)) then Left("block differs")
      else Right(())

  private def acceptCreate(title: String, albums: Seq[Catalog.Album]): Unit =
    if title.isEmpty then throw IllegalArgumentException("empty title")
    else if title.exists(ch => ch == '\t' || ch == '\n' || ch == '\r') then
      throw IllegalArgumentException("title has a tab or newline")
    else albums.filter(_.title == title) match
      case Seq() => ()
      case Seq(_) => throw IllegalArgumentException(s"album titled ${Album.quote(title)} already exists")
      case _ => throw IllegalArgumentException(s"more than one album titled ${Album.quote(title)}")

  // The reread stays outside the catch. A refusal here is usage, and Chrome stays closed.
  private def heldUpload(session: Session, pathText: String, destinationId: String): (Session, Result) =
    val (destination, file, name) = rereadUpload(session, pathText, destinationId)
    var ui: PhotosAct = null
    try
      // A notice would write status.txt and drop a crashed refresh's resume phase.
      ui = Refresh.openAct(_ => ())
      commitUpload(ui, session, destination, file, name) match
        case Right(done) =>
          addOk(done.session, destination, name, done.key, "upload-item")
        case Left(message) =>
          uploadFailed(session, destination, name, "refresh-only", message)
    catch
      case miss: Refresh.ActMiss =>
        val recovery: String = if miss.confirmed then "refresh-only" else "one-click"
        val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("upload-item failed")
        uploadFailed(session, destination, name, recovery, detail)
      case error: Exception =>
        uploadFailed(session, destination, name, "one-click", Operation.messageOf(error))
    finally
      if ui != null then
        try ui.close()
        catch case _: Exception => ()

  private final case class Committed(session: Session, key: String)

  private def commitPut(
    ui: PhotosAct,
    session: Session,
    item: Catalog.Item,
    destination: Catalog.Album,
    current: String
  ): Either[String, Committed] =
    val read: Catalog.Item = ui.putInAlbum(item, destination, current)
    val line: Catalog.Item = Catalog.Item(
      current,
      read.date.orElse(item.date),
      read.key,
      read.uploader.orElse(item.uploader)
    )
    publishAlbum(session, Seq(destination.id -> (dest => revisedAdd(dest, line)))).map: next =>
      Committed(next, line.key)

  private def commitTake(
    ui: PhotosAct,
    session: Session,
    album: Catalog.Album,
    item: Catalog.Item
  ): Either[String, Session] =
    ui.takeOut(album, item)
    publishAlbum(session, Seq(album.id -> (next => revisedTake(next, item))))

  private def commitUpload(
    ui: PhotosAct,
    session: Session,
    destination: Catalog.Album,
    file: File,
    name: String
  ): Either[String, Committed] =
    val read: Catalog.Item = ui.uploadItem(destination, file, name)
    val line: Catalog.Item = Catalog.Item(name, read.date, read.key, read.uploader)
    publishAlbum(session, Seq(destination.id -> (dest => revisedAdd(dest, line)))).map: next =>
      Committed(next, line.key)

  private enum PushAction:
    case Remove(albumId: String, key: String, current: String, albumTitle: String)
    case Add(
      sourceId: String,
      key: String,
      current: String,
      destinationId: String,
      albumTitle: String,
      sourceTitle: String
    )
    case Upload(destinationId: String, pathText: String, albumTitle: String)

    def line: String = this match
      case Remove(_, _, current, albumTitle) =>
        s"remove ${Album.quote(albumTitle)} ${Album.quote(current)}"
      case Add(_, _, current, _, albumTitle, sourceTitle) =>
        s"add ${Album.quote(albumTitle)} ${Album.quote(current)} from ${Album.quote(sourceTitle)}"
      case Upload(_, pathText, albumTitle) =>
        s"upload ${Album.quote(albumTitle)} ${Album.quote(pathText)}"

    def stepName: String = this match
      case _: Remove => RemoveStep
      case _: Add => AddStep
      case _: Upload => UploadStep

    def shownTitle: String = this match
      case Remove(_, _, _, albumTitle) => albumTitle
      case Add(_, _, _, _, albumTitle, _) => albumTitle
      case Upload(_, _, albumTitle) => albumTitle

  private final case class PushStop(
    line: String,
    step: String,
    recovery: String,
    detail: String,
    title: String
  )

  private final case class PushRow(path: String, fileAlbum: Album, catalog: Catalog.Album)

  private final case class PushDay(
    directory: File,
    grouped: Map[String, Set[String]],
    sidecars: Map[String, Metadata]
  )

  // A later album is not replanned after an earlier publish.
  private def planPush(session: Session): (Session, Seq[PushAction]) =
    val archive: File = session.root.configuration.archive
    val catalogFile: File = File(File(archive, ".gphoto-sync"), "catalog.txt")
    val stored: Seq[Catalog.Album] = Catalog.read(catalogFile)
    val links: Map[String, String] = AlbumLinks.read(archive).map(row => row.path -> row.id).toMap
    val chosen: Seq[PushRow] = Archive.scan(archive).albums.flatMap: loaded =>
      val path: String = Album.relative(archive, loaded.file)
      // No link means there is no album page to open. Owner, auto-add, and unread names are the
      // item-command refusals, so one such album does not fail the others.
      if !loaded.album.external || loaded.album.owner.isDefined then None
      else links.get(path).flatMap: id =>
        stored.find(_.id == id).filterNot(pushSkipped).map(album => PushRow(path, loaded.album, album))
    val ordered: Seq[PushRow] =
      chosen.sortBy(row => (utf8Key(row.path), utf8Key(row.catalog.title)))
    ordered.foldLeft((session, Seq.empty[PushAction])):
      case ((current, acc), row) =>
        val (next, planned) = planAlbum(current, row, stored)
        (next, acc ++ planned)

  private def pushSkipped(album: Catalog.Album): Boolean =
    album.owner.isDefined ||
      album.autoAdd ||
      album.error.exists(_.nameFailures > 0) ||
      album.unsets.nonEmpty

  private def planAlbum(
    session: Session,
    row: PushRow,
    stored: Seq[Catalog.Album]
  ): (Session, Seq[PushAction]) =
    val (ready, facts) = pushDays(row.fileAlbum).foldLeft((session, Map.empty[LocalDate, PushDay])):
      case ((current, acc), day) =>
        val civil: Day = civilDay(current.root, day)
        val (next, grouped, sidecars) = loadGrouped(current, civil, readSidecars = true)
        (next, acc.updated(day, PushDay(civil.directory, grouped.toMap, sidecars)))
    val universe: Map[LocalDate, Set[String]] =
      facts.iterator.map((day, fact) => day -> fact.grouped.keySet).toMap
    val desired: Set[(LocalDate, String)] =
      row.fileAlbum.include.foldLeft(Set.empty[(LocalDate, String)]): (acc, entry) =>
        acc ++ Include.expand(row.fileAlbum.span, entry, universe)
    val hits: Map[(LocalDate, String), Seq[(Catalog.Album, Catalog.Item)]] =
      stored.foldLeft(Map.empty[(LocalDate, String), Seq[(Catalog.Album, Catalog.Item)]]): (acc, album) =>
        album.items.foldLeft(acc): (inner, item) =>
          pushStem(facts, item) match
            case None => inner
            case Some(pair) =>
              inner.updated(pair, inner.getOrElse(pair, Seq.empty) :+ (album -> item))
    val removes: Seq[PushAction] = pushRemoves(row, facts)
    val adds: Seq[PushAction] = pushAdds(row, desired, hits)
    val uploads: Seq[PushAction] = pushUploads(ready, row, desired, facts, hits)
    (ready, removes ++ adds ++ uploads)

  private def pushRemoves(row: PushRow, facts: Map[LocalDate, PushDay]): Seq[PushAction] =
    row.catalog.items.flatMap: item =>
      val (raw, _) = Files.splitOnLast(item.filename, '.')
      val stem: String = pushStem(facts, item).map(_._2).getOrElse(Pictures.dropOnePrefix(raw))
      val excepted: Boolean =
        item.date.exists(day => row.fileAlbum.positive(day, stem) && !row.fileAlbum.claims(day, stem))
      Option.when(excepted)(item)
    .sortBy(item => (utf8Key(item.filename), utf8Key(item.key)))
    .map: item =>
      PushAction.Remove(row.catalog.id, item.key, item.filename, row.catalog.title)

  private def pushAdds(
    row: PushRow,
    desired: Set[(LocalDate, String)],
    hits: Map[(LocalDate, String), Seq[(Catalog.Album, Catalog.Item)]]
  ): Seq[PushAction] =
    desired.toSeq.flatMap: pair =>
      val holders: Seq[(Catalog.Album, Catalog.Item)] = hits.getOrElse(pair, Seq.empty)
      if holders.exists(_._1.id == row.catalog.id) then Seq.empty
      else
        holders.map(_._2.key).distinct.flatMap: key =>
          holders.find((album, item) => item.key == key && album.id != row.catalog.id).map: (album, item) =>
            PushAction.Add(
              album.id,
              item.key,
              item.filename,
              row.catalog.id,
              row.catalog.title,
              album.title
            )
    .sortBy:
      case action: PushAction.Add =>
        (utf8Key(action.current), utf8Key(action.sourceTitle), utf8Key(action.key))
      case _ =>
        (Seq.empty[Int], Seq.empty[Int], Seq.empty[Int])

  private def pushUploads(
    session: Session,
    row: PushRow,
    desired: Set[(LocalDate, String)],
    facts: Map[LocalDate, PushDay],
    hits: Map[(LocalDate, String), Seq[(Catalog.Album, Catalog.Item)]]
  ): Seq[PushAction] =
    val archive: File = session.root.configuration.archive
    desired.toSeq.flatMap: (day, stem) =>
      if hits.contains(day -> stem) then None
      else facts.get(day).flatMap: fact =>
        val metadata: Metadata = fact.sidecars.getOrElse(stem, Metadata.default)
        // The Library API already recorded this file. A second upload would be another library copy.
        val marked: Boolean =
          metadata.`google-media-item-id`.exists(_.nonEmpty) || metadata.`already-in-google`.contains(true)
        if marked then None
        else pushFile(archive, fact.directory, stem).map: relative =>
          PushAction.Upload(row.catalog.id, relative, row.catalog.title)
    .sortBy:
      case action: PushAction.Upload => utf8Key(action.pathText)
      case _ => Seq.empty

  private def pushFile(archive: File, directory: File, stem: String): Option[String] =
    val planned: Seq[Pictures.Planned] = Pictures.plan(Files.listFiles(directory)).filter: row =>
      !row.omitted && row.stem == stem && Publish.uploadable.contains(row.extension)
    val literal: Option[Pictures.Planned] =
      Publish.uploadable.flatMap(ext => planned.find(row => row.literal && row.extension == ext)).headOption
    val fallback: Option[Pictures.Planned] =
      Publish.uploadable.flatMap(ext => planned.find(_.extension == ext)).headOption
    literal.orElse(fallback).map(row => Album.relative(archive, row.file))

  private def pushStem(
    facts: Map[LocalDate, PushDay],
    item: Catalog.Item
  ): Option[(LocalDate, String)] =
    item.date.flatMap: day =>
      facts.get(day).flatMap: fact =>
        val (stem, extension) = Files.splitOnLast(item.filename, '.')
        resolve(fact.grouped, stem, extension).map(local => day -> local)

  private def pushDays(album: Album): Seq[LocalDate] =
    album.span match
      case Some(span) => eachDay(span)
      case None => album.include.flatMap(pushEntryDays).distinct.sorted

  private def pushEntryDays(entry: Include.Entry): Seq[LocalDate] = entry match
    case Include.Entry.All(_) => Seq.empty
    case Include.Entry.Dates(span, _, _) => eachDay(span)

  // Unsigned UTF-8 bytes. Java String order is UTF-16, which disagrees past U+007F.
  private given Ordering[Seq[Int]] = scala.math.Ordering.Implicits.seqOrdering

  private def utf8Key(text: String): Seq[Int] =
    text.getBytes(StandardCharsets.UTF_8).toSeq.map(byte => byte & 0xff)

  private def runPush(session: Session, actions: Seq[PushAction]): (Session, Result) =
    var ui: PhotosAct = null
    try
      // A notice would write status.txt and drop a crashed refresh's resume phase.
      ui = Refresh.openAct(_ => ())
      val opened: PhotosAct = ui
      val outcome: Either[(Session, Result), (Session, Seq[String])] =
        actions.foldLeft[Either[(Session, Result), (Session, Seq[String])]](Right(session -> Seq.empty)):
          case (left @ Left(_), _) => left
          case (Right((current, done)), action) =>
            applyPush(opened, current, action) match
              case Right((next, line)) => Right(next -> (done :+ line))
              case Left(stop) => Left(pushFailed(current, done, stop))
      outcome match
        case Left(done) => done
        case Right((current, done)) =>
          (current, Result.Ok(pushText(done, actions.head.shownTitle), Json.Null))
    catch
      case miss: Refresh.ActMiss =>
        val recovery: String = if miss.confirmed then "refresh-only" else "one-click"
        val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("push failed")
        val head: PushAction = actions.head
        pushFailed(session, Seq.empty, PushStop(head.line, head.stepName, recovery, detail, head.shownTitle))
      case error: Exception =>
        val head: PushAction = actions.head
        pushFailed(
          session,
          Seq.empty,
          PushStop(head.line, head.stepName, "one-click", Operation.messageOf(error), head.shownTitle)
        )
    finally
      if ui != null then
        try ui.close()
        catch case _: Exception => ()

  private def applyPush(
    ui: PhotosAct,
    session: Session,
    action: PushAction
  ): Either[PushStop, (Session, String)] =
    try
      action match
        case remove: PushAction.Remove =>
          val (album, item) = rereadTake(session, remove.albumId, remove.key, remove.current)
          commitTake(ui, session, album, item) match
            case Right(next) => Right(next -> remove.line)
            case Left(message) => Left(stopped(remove, "refresh-only", message))
        case add: PushAction.Add =>
          val (_, item, destination) = rereadAdd(session, add.sourceId, add.key, add.current, add.destinationId)
          if destination.title == "New album" then
            throw Refresh.ActMiss("New album is not a destination", confirmed = false)
          commitPut(ui, session, item, destination, add.current) match
            case Right(done) => Right(done.session -> add.line)
            case Left(message) => Left(stopped(add, "refresh-only", message))
        case upload: PushAction.Upload =>
          val (destination, file, name) = rereadUpload(session, upload.pathText, upload.destinationId)
          commitUpload(ui, session, destination, file, name) match
            case Right(done) => Right(done.session -> upload.line)
            case Left(message) => Left(stopped(upload, "refresh-only", message))
    catch
      case miss: Refresh.ActMiss =>
        val recovery: String = if miss.confirmed then "refresh-only" else "one-click"
        val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse("push failed")
        Left(stopped(action, recovery, detail))
      case error: Exception =>
        Left(stopped(action, "one-click", Operation.messageOf(error)))

  private def stopped(action: PushAction, recovery: String, detail: String): PushStop =
    PushStop(action.line, action.stepName, recovery, detail, action.shownTitle)

  private def pushFailed(session: Session, done: Seq[String], stop: PushStop): (Session, Result) =
    val shown: String = collapsedText(stop.detail, "push failed")
    val catalog: String = if done.nonEmpty then "updated" else "unchanged"
    val rows: Seq[String] = done ++ Seq(
      "failed push",
      stop.line,
      s"step ${stop.step}",
      s"recovery ${stop.recovery}",
      s"catalog $catalog",
      shown
    ).filter(_.nonEmpty)
    val text: String = PlaywrightPhotosUi.redactSecrets(rows.mkString("", "\n", "\n"), stop.title)
    (session, Result.Failed(text, Json.Null, Refresh.MutationFailed))

  private def pushText(actions: Seq[PushAction]): String =
    pushText(actions.map(_.line), actions.headOption.fold("")(_.shownTitle))

  private def pushText(lines: Seq[String], title: String): String =
    PlaywrightPhotosUi.redactSecrets(textOf(lines), title)

  private def rereadAdd(
    session: Session,
    sourceId: String,
    key: String,
    current: String,
    destinationId: String
  ): (Catalog.Album, Catalog.Item, Catalog.Album) =
    val file: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    val albums: Seq[Catalog.Album] = Catalog.read(file)
    val source: Catalog.Album = albums.find(_.id == sourceId).getOrElse(
      throw IllegalArgumentException("no such album")
    )
    val destination: Catalog.Album = albums.find(_.id == destinationId).getOrElse(
      throw IllegalArgumentException("no such album")
    )
    val item: Catalog.Item = source.items.find(_.key == key).getOrElse(
      throw IllegalArgumentException("no such item")
    )
    requireCurrent(item.filename, current)
    if source.id == destination.id then throw IllegalArgumentException("destination is the source album")
    acceptPut(session, destination, item)
    (source, item, destination)

  private def rereadTake(
    session: Session,
    albumId: String,
    key: String,
    current: String
  ): (Catalog.Album, Catalog.Item) =
    val file: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    val albums: Seq[Catalog.Album] = Catalog.read(file)
    val album: Catalog.Album = albums.find(_.id == albumId).getOrElse(
      throw IllegalArgumentException("no such album")
    )
    val item: Catalog.Item = album.items.find(_.key == key).getOrElse(
      throw IllegalArgumentException("no such item")
    )
    requireCurrent(item.filename, current)
    (album, item)

  private def rereadDelete(
    session: Session,
    albumId: String,
    key: String,
    current: String
  ): (Catalog.Album, Catalog.Item) =
    val file: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    val albums: Seq[Catalog.Album] = Catalog.read(file)
    val album: Catalog.Album = albums.find(_.id == albumId).getOrElse(
      throw IllegalArgumentException("no such album")
    )
    val item: Catalog.Item = album.items.find(_.key == key).getOrElse(
      throw IllegalArgumentException("no such item")
    )
    requireCurrent(item.filename, current)
    acceptDelete(album, item)
    (album, item)

  private def publishAlbum(
    session: Session,
    revisions: Seq[(String, Catalog.Album => Catalog.Album)]
  ): Either[String, Session] =
    try
      val directory: File = File(session.root.configuration.archive, ".gphoto-sync")
      val live: File = File(directory, "catalog.txt")
      val partial: File = File(directory, "catalog.txt.partial")
      val previous: File = File(directory, "catalog.txt.previous")
      val source: String = JFiles.readString(live.toPath, StandardCharsets.UTF_8)
      val albums: Seq[Catalog.Album] = Catalog.read(live)
      if revisions.exists((id, _) => !albums.exists(_.id == id)) then Left("no such album")
      else
        val blocks: Map[String, Seq[String]] = RefreshPlan.blocksOf(source)
        val groups: Either[String, Seq[Seq[String]]] =
          albums.foldLeft[Either[String, Seq[Seq[String]]]](Right(Seq.empty)):
            case (Left(message), _) => Left(message)
            case (Right(acc), next) =>
              revisions.find(_._1 == next.id) match
                case Some((_, revise)) =>
                  if !blocks.contains(next.id) then Left("missing album block")
                  else Right(acc :+ RefreshPlan.recordLines(revise(next)))
                case None =>
                  Right(acc :+ RefreshPlan.copiedLines(next, blocks))
        groups.flatMap: lines =>
          try
            Files.write(RefreshPlan.renderLines(lines), partial)
            Right(())
          catch case error: Exception =>
            Left(Operation.messageOf(error))
        .flatMap(_ => RefreshPlan.checkRetitle(partial))
        .flatMap(_ => RefreshPlan.swap(live, partial, previous))
        .map(_ => session.copy(catalogCache = None))
    catch
      case error: Exception =>
        Left(Operation.messageOf(error))

  private def revisedAdd(next: Catalog.Album, line: Catalog.Item): Catalog.Album =
    if next.items.exists(_.key == line.key) || next.unsets.exists(_.key == line.key) then
      throw IllegalArgumentException("repeated item key")
    val count: Int = next.count + 1
    next.copy(
      count = count,
      error = next.error.map(err => err.copy(of = count, got = err.got + 1)),
      items = next.items :+ line
    )

  private def revisedTake(next: Catalog.Album, item: Catalog.Item): Catalog.Album =
    if !next.items.exists(_.key == item.key) then throw IllegalArgumentException("no such item")
    val count: Int = next.count - 1
    next.copy(
      count = count,
      error = next.error.map(err => err.copy(of = count, got = err.got - 1)),
      items = next.items.filterNot(_.key == item.key)
    )

  private def acceptDelete(album: Catalog.Album, item: Catalog.Item): Unit =
    if album.owner.isDefined then throw IllegalArgumentException("no owner line")
    if item.uploader.isDefined then throw IllegalArgumentException("uploader is not -")

  private def acceptPut(session: Session, destination: Catalog.Album, item: Catalog.Item): Unit =
    acceptDestination(session, destination, knownLocal(session, item))

  private def acceptDestination(
    session: Session,
    destination: Catalog.Album,
    known: Option[(LocalDate, String)]
  ): Unit =
    if destination.owner.isDefined then throw IllegalArgumentException("destination has an owner")
    if destination.autoAdd then throw IllegalArgumentException("destination is auto-add")
    if destination.error.exists(_.nameFailures > 0) || destination.unsets.nonEmpty then
      throw IllegalArgumentException("destination has unread names")
    known.foreach: (day, stem) =>
      albumFile(session, destination).foreach: file =>
        val album: Album = Album.read(session.root.configuration.archive, file)
        // Same test Sync uses for an except.
        if album.positive(day, stem) && !album.claims(day, stem) then
          throw IllegalArgumentException(s"destination excepts ${Album.quote(stem)}")

  private def rereadUpload(
    session: Session,
    pathText: String,
    destinationId: String
  ): (Catalog.Album, File, String) =
    val catalog: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    val albums: Seq[Catalog.Album] = Catalog.read(catalog)
    val destination: Catalog.Album = albums.find(_.id == destinationId).getOrElse(
      throw IllegalArgumentException("no such album")
    )
    val (file, relative) = uploadFile(session, pathText)
    val (day, stem, name, extension) = uploadName(relative)
    refuseUpload(session, albums, destination, day, stem, extension)
    (destination, file, name)

  private def refuseUpload(
    session: Session,
    albums: Seq[Catalog.Album],
    destination: Catalog.Album,
    day: LocalDate,
    stem: String,
    extension: Option[String]
  ): Session =
    val (loaded, grouped, sidecars) = loadGrouped(session, civilDay(session.root, day), readSidecars = true)
    val groupedMap: Map[String, Set[String]] = grouped.toMap
    // dropOnePrefix of VID_1000.mp4 is 1000, the still beside it. The grouped stem is 1000v.
    val groupedStem: String = resolve(groupedMap, stem, extension).getOrElse(stem)
    val matched: Boolean = albums.exists: album =>
      album.items.exists: item =>
        item.date.contains(day) && {
          val (itemStem, itemExt) = Files.splitOnLast(item.filename, '.')
          resolve(groupedMap, itemStem, itemExt).contains(groupedStem)
        }
    if matched then throw IllegalArgumentException("catalog item exists")
    sidecars.get(groupedStem).foreach: metadata =>
      val marked: Boolean =
        metadata.`google-media-item-id`.exists(_.nonEmpty) || metadata.`already-in-google`.contains(true)
      if marked then throw IllegalArgumentException("already in google")
    acceptDestination(loaded, destination, Some(day -> groupedStem))
    loaded

  private def uploadFile(session: Session, text: String): (File, String) =
    val archive: File = session.root.configuration.archive.getCanonicalFile
    val raw: File = if text.startsWith("/") then File(text) else File(archive, text)
    if !raw.isFile then throw IllegalArgumentException("no such file")
    val file: File = raw.getCanonicalFile
    val root: java.nio.file.Path = archive.toPath
    val placed: java.nio.file.Path = file.toPath
    // startsWith is a path prefix, so an archive named ".../originals" does not accept ".../originals-extra".
    if !placed.startsWith(root) || placed.getNameCount <= root.getNameCount then
      throw IllegalArgumentException("path is outside the archive")
    (file, Album.relative(archive, file))

  private def uploadName(relative: String): (LocalDate, String, String, Option[String]) =
    val (date, name) =
      try picturePath(relative)
      catch case _: IllegalArgumentException =>
        throw IllegalArgumentException("not a picture path")
    val (rawStem, extension) = Files.splitOnLast(name, '.')
    val lowered: Option[String] = extension.map(_.toLowerCase(java.util.Locale.ROOT)).filter(_.nonEmpty)
    if rawStem.isEmpty then throw IllegalArgumentException("not a picture path")
    else if !lowered.exists(Publish.uploadable.contains) then
      throw IllegalArgumentException("extension is not uploadable")
    // Prefix only. refuseUpload resolves the grouped stem, including a trailing v beside a still.
    (date, Pictures.dropOnePrefix(rawStem), name, lowered)

  private def albumFile(session: Session, destination: Catalog.Album): Option[File] =
    val archive: File = session.root.configuration.archive
    Title.parse(destination.title).map(parsed => Title.file(archive, parsed)).filter(_.isFile)

  private def knownLocal(session: Session, item: Catalog.Item): Option[(LocalDate, String)] =
    val (stem, extension) = Files.splitOnLast(item.filename, '.')
    item.date.flatMap: date =>
      val (_, grouped, _) = loadGrouped(session, civilDay(session.root, date))
      resolve(grouped.toMap, stem, extension).map(local => (date, local))

  private[photo] def confirmedAdd(
    before: Set[String],
    after: Seq[String],
    named: Map[String, Catalog.Item],
    current: String
  ): Option[Catalog.Item] =
    val added: Seq[String] = after.filterNot(before.contains)
    if added.lengthIs != 1 then None
    else named.get(added.head).filter(_.filename == current)

  private def addOk(
    session: Session,
    destination: Catalog.Album,
    filename: String,
    key: String,
    action: String
  ): (Session, Result) =
    val text: String = PlaywrightPhotosUi.redactSecrets(
      s"$action ${Album.quote(filename)} ${Album.quote(destination.title)}\n",
      destination.title
    )
    val value: Json = Json.Obj(Seq(
      "action" -> Json.Str(action),
      "title" -> Json.Str(destination.title),
      "catalog" -> Json.Str("updated"),
      "albumId" -> Json.Str(destination.id),
      "itemKey" -> Json.Str(key)
    ))
    (session, Result.Ok(text, value))

  private def uploadFailed(
    session: Session,
    destination: Catalog.Album,
    filename: String,
    recovery: String,
    detail: String
  ): (Session, Result) =
    itemFailed(
      session,
      destination,
      Catalog.Item(filename, None, "", None),
      "upload-item",
      UploadStep,
      recovery,
      detail
    )

  private def createOk(session: Session, title: String, id: String): (Session, Result) =
    val text: String = PlaywrightPhotosUi.redactSecrets(s"create-album ${Album.quote(title)}\n", title)
    val value: Json = Json.Obj(Seq(
      "action" -> Json.Str("create-album"),
      "title" -> Json.Str(title),
      "catalog" -> Json.Str("updated"),
      "albumId" -> Json.Str(id),
      "itemKey" -> Json.Str("")
    ))
    (session, Result.Ok(text, value))

  private def createFailed(
    session: Session,
    title: String,
    recovery: String,
    detail: String
  ): (Session, Result) =
    val shown: String = collapsedText(detail, "create-album failed")
    val text: String =
      s"""failed create-album
         |album ${Album.quote(title)}
         |step $CreateStep
         |recovery $recovery
         |catalog unchanged
         |$shown
         |""".stripMargin
    val value: Json = Json.Obj(Seq(
      "action" -> Json.Str("create-album"),
      "title" -> Json.Str(title),
      "step" -> Json.Str(CreateStep),
      "recovery" -> Json.Str(recovery),
      "catalog" -> Json.Str("unchanged"),
      "albumId" -> Json.Str(""),
      "itemKey" -> Json.Str("")
    ))
    (session, Result.Failed(PlaywrightPhotosUi.redactSecrets(text, title), value, Refresh.MutationFailed))

  private def deleteAlbumOk(session: Session, album: Catalog.Album): (Session, Result) =
    val text: String = PlaywrightPhotosUi.redactSecrets(s"delete-album ${Album.quote(album.title)}\n", album.title)
    val value: Json = Json.Obj(Seq(
      "action" -> Json.Str("delete-album"),
      "title" -> Json.Str(album.title),
      "catalog" -> Json.Str("updated"),
      "albumId" -> Json.Str(album.id),
      "itemKey" -> Json.Str("")
    ))
    (session, Result.Ok(text, value))

  private def deleteAlbumFailed(
    session: Session,
    album: Catalog.Album,
    recovery: String,
    detail: String
  ): (Session, Result) =
    val shown: String = collapsedText(detail, "delete-album failed")
    val text: String =
      s"""failed delete-album
         |album ${Album.quote(album.title)}
         |step $DeleteAlbumStep
         |recovery $recovery
         |catalog unchanged
         |$shown
         |""".stripMargin
    val value: Json = Json.Obj(Seq(
      "action" -> Json.Str("delete-album"),
      "title" -> Json.Str(album.title),
      "step" -> Json.Str(DeleteAlbumStep),
      "recovery" -> Json.Str(recovery),
      "catalog" -> Json.Str("unchanged"),
      "albumId" -> Json.Str(""),
      "itemKey" -> Json.Str("")
    ))
    (session, Result.Failed(PlaywrightPhotosUi.redactSecrets(text, album.title), value, Refresh.MutationFailed))

  private def moveOk(
    session: Session,
    source: Catalog.Album,
    destination: Catalog.Album,
    filename: String,
    key: String
  ): (Session, Result) =
    val text: String = PlaywrightPhotosUi.redactSecrets(
      s"move-item ${Album.quote(filename)} ${Album.quote(source.title)} ${Album.quote(destination.title)}\n",
      source.title
    )
    val value: Json = Json.Obj(Seq(
      "action" -> Json.Str("move-item"),
      "title" -> Json.Str(source.title),
      "destination" -> Json.Str(destination.title),
      "catalog" -> Json.Str("updated"),
      "albumId" -> Json.Str(source.id),
      "destinationId" -> Json.Str(destination.id),
      "itemKey" -> Json.Str(key)
    ))
    (session, Result.Ok(text, value))

  private def moveFailed(
    session: Session,
    source: Catalog.Album,
    destination: Catalog.Album,
    item: Catalog.Item,
    step: String,
    recovery: String,
    detail: String
  ): (Session, Result) =
    val shown: String = collapsedText(detail, "move-item failed")
    val text: String =
      s"""failed move-item
         |filename ${Album.quote(item.filename)}
         |album ${Album.quote(source.title)}
         |destination ${Album.quote(destination.title)}
         |step $step
         |recovery $recovery
         |catalog unchanged
         |$shown
         |""".stripMargin
    val value: Json = Json.Obj(Seq(
      "action" -> Json.Str("move-item"),
      "title" -> Json.Str(source.title),
      "destination" -> Json.Str(destination.title),
      "step" -> Json.Str(step),
      "recovery" -> Json.Str(recovery),
      "catalog" -> Json.Str("unchanged"),
      "albumId" -> Json.Str(source.id),
      "destinationId" -> Json.Str(destination.id),
      "itemKey" -> Json.Str(item.key)
    ))
    (session, Result.Failed(PlaywrightPhotosUi.redactSecrets(text, source.title), value, Refresh.MutationFailed))

  private def itemOk(
    session: Session,
    album: Catalog.Album,
    item: Catalog.Item,
    action: String,
    catalog: String
  ): (Session, Result) =
    val text: String = PlaywrightPhotosUi.redactSecrets(s"$action ${Album.quote(item.filename)}\n", album.title)
    val value: Json = Json.Obj(Seq(
      "action" -> Json.Str(action),
      "title" -> Json.Str(album.title),
      "catalog" -> Json.Str(catalog),
      "albumId" -> Json.Str(album.id),
      "itemKey" -> Json.Str(item.key)
    ))
    (session, Result.Ok(text, value))

  private def itemFailed(
    session: Session,
    album: Catalog.Album,
    item: Catalog.Item,
    action: String,
    step: String,
    recovery: String,
    detail: String
  ): (Session, Result) =
    val shown: String = collapsedText(detail, s"$action failed")
    val text: String =
      s"""failed $action
         |filename ${Album.quote(item.filename)}
         |step $step
         |recovery $recovery
         |catalog unchanged
         |$shown
         |""".stripMargin
    val value: Json = Json.Obj(Seq(
      "action" -> Json.Str(action),
      "title" -> Json.Str(album.title),
      "step" -> Json.Str(step),
      "recovery" -> Json.Str(recovery),
      "catalog" -> Json.Str("unchanged"),
      "albumId" -> Json.Str(album.id),
      "itemKey" -> Json.Str(item.key)
    ))
    (session, Result.Failed(PlaywrightPhotosUi.redactSecrets(text, album.title), value, Refresh.MutationFailed))

  private def collapsedText(detail: String, fallback: String): String =
    val message: String = detail.replace('\t', ' ').replace('\r', ' ').replace('\n', ' ')
    if message.isEmpty then fallback else message

  def writeAlbum(
    session: Session,
    album: Catalog.Album,
    dryRun: Boolean,
    replace: Boolean,
    googleFlag: Option[String]
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
      external = keptId.isEmpty,
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
      (dropped, Result.Ok(summary, value))

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

  def acceptRequest(
    albums: Seq[Catalog.Album],
    action: Action,
    albumId: String,
    itemKey: String,
    current: String,
    updated: String
  ): Unit = action match
    case Action.RenameAlbum =>
      val album: Catalog.Album = albums.find(_.id == albumId).getOrElse(
        throw IllegalArgumentException("no such album")
      )
      if album.owner.isDefined then throw IllegalArgumentException("album has an owner")
      requireCurrent(album.title, current)
      if updated.isEmpty then throw IllegalArgumentException("new title is empty")
      if updated == current then throw IllegalArgumentException("new title is unchanged")
    case Action.SetDescription =>
      val item: Catalog.Item = uniqueItem(albums, itemKey)
      requireCurrent(item.filename, current)
    case Action.SetDate =>
      val item: Catalog.Item = uniqueItem(albums, itemKey)
      requireCurrent(item.filename, current)
      requireDate(updated)
    case Action.DownloadItem =>
      val album: Catalog.Album = albums.find(_.id == albumId).getOrElse(
        throw IllegalArgumentException("no such album")
      )
      val item: Catalog.Item = album.items.find(_.key == itemKey).getOrElse(
        throw IllegalArgumentException("no such item")
      )
      requireCurrent(item.filename, current)
      if updated.nonEmpty then throw IllegalArgumentException("new is not empty")
      requireFilename(item.filename)
    case Action.DeleteItem =>
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
        val (next, _, nonHits) = gather(current, album)
        val wanted: Set[String] = nonHits.collect { case NonHit.Miss(_, _, key) => key }.toSet
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

  private[photo] def locateItem(albums: Seq[Catalog.Album], key: String): (Catalog.Album, Catalog.Item) =
    albums.flatMap(album => album.items.filter(_.key == key).map(album -> _)) match
      case Seq(found) => found
      case Seq() => throw IllegalArgumentException("no such item")
      case _ => throw IllegalArgumentException("item key is not unique")

  private def uniqueItem(albums: Seq[Catalog.Album], key: String): Catalog.Item =
    locateItem(albums, key)._2

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
