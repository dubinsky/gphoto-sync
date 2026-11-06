package org.podval.tools.photo

import java.io.File
import java.time.{DateTimeException, LocalDate}

private[photo] object AgentSupport:

  // Usage keeps the session passed in. A later failure keeps a catalog that already parsed.
  def catching(session: Session)(body: => (Session, Result)): (Session, Result) =
    try body
    catch
      case error: (IllegalArgumentException | IllegalStateException) =>
        (session, Result.Usage(Operation.messageOf(error)))

  // Day's own require accepts 1..31. The civil date rejects February 31 and a non-leap February 29.
  // A bad segment is NumberFormatException or a failed year, month, or day require. Both are not a day.
  def parseDay(session: Session, text: String): Day =
    try
      session.parse(text) match
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
          case _                                   => throw IllegalArgumentException("catalog title does not match")
      case None =>
        albums.filter(_.title == title) match
          case Seq(album) => album
          case Seq()      => throw IllegalArgumentException(s"no album titled ${Album.quote(title)}")
          case _          => throw IllegalArgumentException(s"more than one album titled ${Album.quote(title)}")

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
    Json.Obj(
      Seq(
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
      )
    )

  def itemLines(album: Catalog.Album): Seq[String] =
    album.items.map(item => s"${item.filename} ${catalogDate(item.date)} ${showUploader(item.uploader)}") ++
      album.unsets.map(unset => s"unset ${catalogDate(unset.date)} ${showUploader(unset.uploader)}")

  def itemsValue(album: Catalog.Album): Json =
    Json.Obj(
      Seq(
        "id" -> Json.Str(album.id),
        "error" -> album.error.fold(Json.Null)(errorValue),
        "items" -> Json.Arr(album.items.map(itemValue)),
        "unsets" -> Json.Arr(album.unsets.map(unsetValue))
      )
    )

  // Skip is a result. Do not read a day, and do not treat unread names as unmatched files.
  def skipped(session: Session, album: Catalog.Album, reason: String): (Session, Result) =
    (session, Result.Ok(s"skipped $reason\n", matchValue(album, Seq.empty, Seq.empty, Seq.empty)))

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
              if found.isEmpty then (after, matched, nonHits :+ NonHit.Miss(Some(date), item.filename, item.key))
              else
                val mismatches: Seq[NonHit] = found.map: (archiveDay, local) =>
                  NonHit.Mismatch(date, item.filename, archiveDay, local)
                (after, matched, nonHits ++ mismatches)

  def parseSpan(text: String): Title.Span =
    Title.parse(text) match
      case Some(Title.Parsed(Some(span), None)) => span
      case _                                    => throw IllegalArgumentException("not a span")

  def picturePath(path: String): (LocalDate, String) =
    try Album.splitPath(path)
    catch
      case _: DateTimeException =>
        throw IllegalArgumentException(s"not a picture path: $path")

  def eachDay(span: Title.Span): List[LocalDate] =
    Iterator.iterate(span.start)(_.plusDays(1)).takeWhile(day => !day.isAfter(span.end)).toList

  def textOf(rows: Seq[String]): String =
    if rows.isEmpty then "" else rows.mkString("", "\n", "\n")

  // Grouped rows are names. The listing would run EXIF, so the sidecar is read here.
  private def sidecar(planned: Seq[Pictures.Planned], directory: File, stem: String): Metadata =
    planned.find(row => !row.omitted && row.stem == stem && row.extension == Metadata.extension) match
      case None =>
        Metadata.default
      case Some(row) =>
        val corrected: File = File(directory, s"$stem.${Metadata.extension}")
        val file: File = if corrected.isFile then corrected else row.file
        if file.isFile then Metadata.read(file) else Metadata.default

  def metadataValue(metadata: Metadata): Json =
    Json.Obj(
      Seq(
        metadata.source.map(value => "source" -> Json.Str(value)),
        metadata.timestamp.map(value => "timestamp" -> Json.Str(value)),
        metadata.date.map(date => "date" -> Json.Str(date.toString)),
        metadata.`bad-raw`.map(value => "bad-raw" -> Json.Bool(value)),
        metadata.orientation.map(value => "orientation" -> Json.Str(value)),
        metadata.`google-media-item-id`.map(value => "google-media-item-id" -> Json.Str(value)),
        metadata.`already-in-google`.map(value => "already-in-google" -> Json.Bool(value)),
        metadata.uploader.map(value => "uploader" -> Json.Str(value))
      ).flatten
    )

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
    grouped
      .get(stem)
      .map: extensions =>
        val v: String = stem + "v"
        val useV: Boolean =
          extension.exists(isVideo) && !extensions.exists(isVideo) && grouped.get(v).exists(_.exists(isVideo))
        if useV then v else stem

  private def isVideo(extension: String): Boolean =
    Extension.video.exists(_.name == extension.toLowerCase(java.util.Locale.ROOT))

  def civilDay(root: Root, date: LocalDate): Day =
    root.year(date.getYear).month(date.getMonthValue).day(date.getDayOfMonth)

  private[photo] def catalogDate(date: Option[LocalDate]): String = date.fold("-")(_.toString)

  private[photo] def showUploader(uploader: Option[String]): String = uploader.getOrElse("-")

  private def errorValue(error: Catalog.Error): Json =
    Json.Obj(
      Seq(
        "got" -> Json.Long(error.got),
        "of" -> Json.Long(error.of),
        "nameFailures" -> Json.Long(error.nameFailures)
      )
    )

  private def itemValue(item: Catalog.Item): Json =
    Json.Obj(
      Seq(
        "filename" -> Json.Str(item.filename),
        "date" -> item.date.fold(Json.Null)(date => Json.Str(date.toString)),
        "key" -> Json.Str(item.key),
        "uploader" -> item.uploader.fold(Json.Null)(Json.Str.apply)
      )
    )

  private def unsetValue(unset: Catalog.Unset): Json =
    Json.Obj(
      Seq(
        "date" -> unset.date.fold(Json.Null)(date => Json.Str(date.toString)),
        "key" -> Json.Str(unset.key),
        "uploader" -> unset.uploader.fold(Json.Null)(Json.Str.apply)
      )
    )

  private[photo] def matchValue(
    album: Catalog.Album,
    hits: Seq[(LocalDate, String)],
    misses: Seq[String],
    mismatches: Seq[NonHit.Mismatch]
  ): Json =
    Json.Obj(
      Seq(
        "id" -> Json.Str(album.id),
        "matched" -> Json.Arr(hits.map: (day, stem) =>
          Json.Obj(
            Seq(
              "day" -> Json.Str(Title.dayText(day)),
              "stem" -> Json.Str(stem)
            )
          )),
        "unmatched" -> Json.Arr(misses.map(Json.Str.apply)),
        "dateMismatches" -> Json.Arr(mismatches.map: mismatch =>
          Json.Obj(
            Seq(
              "catalogDay" -> Json.Str(Title.dayText(mismatch.catalogDay)),
              "filename" -> Json.Str(mismatch.filename),
              "archiveDay" -> Json.Str(Title.dayText(mismatch.archiveDay)),
              "stem" -> Json.Str(mismatch.stem)
            )
          ))
      )
    )

  private[photo] def withLock(session: Session, title: String)(body: => (Session, Result)): (Session, Result) =
    val places: Refresh.Places = Refresh.placesOf()
    Refresh.claim(places.lock) match
      case Refresh.Claim.Busy =>
        val (kept, result) = Refresh.busy(session, places)
        result match
          case Result.Failed(text, value, message) =>
            (kept, Result.Failed(Redaction.redactSecrets(text, title), value, message))
          case other =>
            (kept, other)
      case Refresh.Claim.Owned(channel, lock) =>
        try body
        finally Refresh.release(channel, lock)

  private[photo] def scanDays(
    session: Session,
    days: Seq[LocalDate]
  ): (Session, Map[LocalDate, Set[String]]) =
    days.foldLeft((session, Map.empty[LocalDate, Set[String]])): (acc, date) =>
      val (current, soFar) = acc
      val (next, grouped, _) = loadGrouped(current, civilDay(current.root, date))
      val stems: Set[String] = grouped.map(_._1).toSet
      (next, if stems.isEmpty then soFar else soFar.updated(date, stems))
