package org.podval.tools.photo

import java.io.File
import java.time.{DateTimeException, LocalDate}

private[photo] final class AgentStems(dayText: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val day: Day = AgentSupport.parseDay(session, dayText)
      val (loaded, grouped, sidecars) = AgentSupport.loadGrouped(session, day, readSidecars = true)
      val rows: Seq[String] = grouped.map: (stem, extensions) =>
        s"$stem ${extensions.toSeq.sorted.mkString("+")}"
      val value: Json = Json.Obj(Seq(
        "day" -> Json.Str(Title.dayText(day.date)),
        "stems" -> Json.Arr(grouped.map: (stem, extensions) =>
          Json.Obj(Seq(
            "stem" -> Json.Str(stem),
            "extensions" -> Json.Arr(extensions.toSeq.sorted.map(Json.Str.apply)),
            "metadata" -> AgentSupport.metadataValue(sidecars.getOrElse(stem, Metadata.default))
          ))
        )
      ))
      (loaded, Result.Ok(AgentSupport.textOf(rows), value))


private[photo] object AgentYears extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val years: Seq[String] = Year.lsd(session.root).map(year => year.toString.stripPrefix("/"))
      val value: Json = Json.Obj(Seq("years" -> Json.Arr(years.map(Json.Str.apply))))
      (session, Result.Ok(AgentSupport.textOf(years), value))


private[photo] final class AgentMonths(yearText: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
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
      (session, Result.Ok(AgentSupport.textOf(months), value))


private[photo] final class AgentDays(monthText: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
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
      (session, Result.Ok(AgentSupport.textOf(days), value))


private[photo] final class AgentClaims(dayText: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val day: Day = AgentSupport.parseDay(session, dayText)
      if !day.directory.isDirectory then
        (session, Result.Ok("", claimsValue(day, present = false, Seq.empty)))
      else
        val (loaded, grouped, _) = AgentSupport.loadGrouped(session, day, readSidecars = false)
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
          (edited, Result.Ok(AgentSupport.textOf(resolved.map(_.line)), claimsValue(day, present = true, resolved)))

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
    AgentSupport.catching(session):
      val (loaded, albums) = session.loadCatalog
      val counts: Map[String, Int] = albums.groupBy(_.title).view.mapValues(_.length).toMap
      val sorted: Seq[Catalog.Album] = albums.sortBy(album => (album.title, album.id))
      val lines: Seq[String] = sorted.map: album =>
        AgentSupport.catalogLine(album, counts.getOrElse(album.title, 0) > 1)
      val value: Json = Json.Obj(Seq(
        "albums" -> Json.Arr(sorted.map: album =>
          AgentSupport.albumValue(album, counts.getOrElse(album.title, 0) > 1)
        )
      ))
      (loaded, Result.Ok(AgentSupport.textOf(lines), value))


private[photo] final class AgentItems(id: Option[String], title: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        val album: Catalog.Album = AgentSupport.select(albums, title, id)
        val text: String =
          if album.autoAdd then "auto-add\n"
          else AgentSupport.textOf(AgentSupport.itemLines(album))
        (loaded, Result.Ok(text, AgentSupport.itemsValue(album)))


private[photo] final class AgentMatch(id: Option[String], title: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        val album: Catalog.Album = AgentSupport.select(albums, title, id)
        if album.autoAdd then AgentSupport.skipped(loaded, album, "auto-add")
        else if album.error.exists(_.nameFailures > 0) || album.unsets.nonEmpty then
          AgentSupport.skipped(loaded, album, "unread names")
        else AgentQueries.matchItems(loaded, album)


private[photo] final class AgentLocate(dayText: String, stem: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      AgentQueries.locate(session, dayText, stem)


private[photo] object AgentDrift extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      AgentQueries.drift(session)


private[photo] object AgentMissing extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val (loaded, albums) = session.loadCatalog
      AgentQueries.listMissing(loaded, albums)


private[photo] final class AgentInclude(spanText: Option[String], paths: Seq[String]) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val span: Option[Title.Span] = spanText.map(AgentSupport.parseSpan)
      val claimedPairs: Seq[(LocalDate, String)] = paths.map(AgentSupport.picturePath)
      span.foreach: dates =>
        if claimedPairs.exists(pair => !dates.contains(pair._1)) then
          throw IllegalArgumentException("claimed day outside the span")
      val claimed: Set[(LocalDate, String)] = claimedPairs.toSet
      val days: Seq[LocalDate] = span.fold(claimedPairs.map(_._1).distinct)(AgentSupport.eachDay)
      val (loaded, universe) = AgentSupport.scanDays(session, days)
      val entries: Seq[Include.Entry] = Include.compress(span, claimed, universe)
      val value: Json = Json.Obj(Seq(
        "rendered" -> Json.Arr(entries.map(entry => Json.Str(Include.render(entry)))),
        "logical" -> Json.Arr(entries.map(entry => Json.Str(Include.logical(entry))))
      ))
      (loaded, Result.Ok(AgentSupport.textOf(entries.map(Include.render)), value))

private[photo] object AgentQueries:

  def matchItems(session: Session, album: Catalog.Album): (Session, Result) =
    val (loaded, hits, nonHits) = AgentSupport.gather(session, album)
    val rows: Seq[String] =
      Seq(s"matched ${hits.length} of ${album.items.length}") ++
        hits.map(hit => s"${Title.dayText(hit._1)} ${hit._2} ${hit._3}") ++
        nonHits.map:
          case AgentSupport.NonHit.Miss(date, filename, _) =>
            s"unmatched ${date.fold("-")(Title.dayText)} $filename"
          case AgentSupport.NonHit.Mismatch(catalogDay, filename, archiveDay, stem) =>
            s"date-mismatch ${Title.dayText(catalogDay)} $filename ${Title.dayText(archiveDay)} $stem"
    val value: Json = AgentSupport.matchValue(
      album,
      hits.map(hit => hit._1 -> hit._3),
      nonHits.collect { case AgentSupport.NonHit.Miss(_, _, key) => key },
      nonHits.collect { case mismatch: AgentSupport.NonHit.Mismatch => mismatch }
    )
    (loaded, Result.Ok(AgentSupport.textOf(rows), value))

  // Match skips an unread album before gather. This walk does not. A date of - is not a row.
  def locate(session: Session, dayText: String, stem: String): (Session, Result) =
    val day: Day = AgentSupport.parseDay(session, dayText)
    val shown: String = Title.dayText(day.date)
    if !day.directory.isDirectory then
      (session, Result.Ok("", locateValue(shown, stem, directory = false, present = false, Seq.empty)))
    else
      val (loaded, grouped, _) = AgentSupport.loadGrouped(session, day, readSidecars = false)
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
                if AgentSupport.resolve(byStem, name, extension).contains(stem) then Some((album, item, date)) else None
              case _ =>
                None
        val lines: Seq[String] = rows.map: (album, item, date) =>
          s"${album.title} ${date.toString} ${item.filename} ${AgentSupport.showUploader(item.uploader)}"
        (cataloged, Result.Ok(AgentSupport.textOf(lines), locateValue(shown, stem, directory = true, present = true, rows)))

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
          "uploader" -> Json.Str(AgentSupport.showUploader(item.uploader)),
          "id" -> Json.Str(album.id),
          "key" -> Json.Str(item.key)
        ))
      )
    ))

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
    (ready, Result.Ok(AgentSupport.textOf(plan.actions.map(Reconcile.line)), Reconcile.actionsValue(plan.actions)))

  // gather calls resolve. A date of - is not a day, so that item is not looked up.
  // Auto-add is skipped here. Unset lines are not items.
  def listMissing(session: Session, albums: Seq[Catalog.Album]): (Session, Result) =
    val (ready, rows) = albums.foldLeft((session, Seq.empty[(Catalog.Album, Catalog.Item)])): (acc, album) =>
      if album.autoAdd then acc
      else
        val (current, soFar) = acc
        val (next, _, nonHits) = AgentSupport.gather(current, album)
        val wanted: Set[String] = nonHits.collect { case AgentSupport.NonHit.Miss(_, _, key) => key }.toSet
        val added: Seq[(Catalog.Album, Catalog.Item)] = album.items.collect:
          case item if wanted.contains(item.key) => album -> item
        (next, soFar ++ added)
    val lines: Seq[String] = rows.map: (album, item) =>
      s"missing ${album.title} ${AgentSupport.catalogDate(item.date)} ${item.filename} ${AgentSupport.showUploader(item.uploader)}"
    val value: Json = Json.Obj(Seq(
      "missing" -> Json.Arr(rows.map: (album, item) =>
        Json.Obj(Seq(
          "title" -> Json.Str(album.title),
          "date" -> Json.Str(AgentSupport.catalogDate(item.date)),
          "filename" -> Json.Str(item.filename),
          "uploader" -> Json.Str(AgentSupport.showUploader(item.uploader)),
          "key" -> Json.Str(item.key)
        ))
      )
    ))
    (ready, Result.Ok(AgentSupport.textOf(lines), value))
