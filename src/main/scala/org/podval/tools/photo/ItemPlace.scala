package org.podval.tools.photo

import java.io.File
import java.time.{LocalDate, YearMonth}

private object ItemPlace:
  private final case class Pic(path: String, day: LocalDate, stem: String, month: YearMonth)

  private final case class Working(
    file: File,
    album: Album,
    current: Seq[Include.Entry],
    drops: Seq[(LocalDate, String)],
    adds: Seq[(LocalDate, String)]
  )

  private final case class Created(file: File, album: Album)

  private final case class Spoken(
    lines: Seq[String],
    created: Seq[String],
    excluded: Seq[(String, String)],
    included: Seq[(String, String)]
  ):
    def addCreate(title: String): Spoken =
      copy(lines = lines :+ s"create $title", created = created :+ title)

    def addPicture(excludes: Seq[(String, String)], include: Option[(String, String)]): Spoken =
      val more: Seq[String] =
        excludes.map((title, path) => s"exclude $title $path") ++
          include.toSeq.map((title, path) => s"include $title $path")
      copy(
        lines = lines ++ more,
        excluded = excluded ++ excludes,
        included = included ++ include.toSeq
      )

  private val unspoken: Spoken = Spoken(Seq.empty, Seq.empty, Seq.empty, Seq.empty)

  def month(session: Session, dryRun: Boolean, pictures: Seq[String]): Json =
    val originals: File = session.root.configuration.archive
    val pics: Seq[Pic] = dated(session, pictures)
    val universe: Map[LocalDate, Set[String]] = Archive.universe(Archive.pictures(originals))
    val loaded: Map[String, Working] = load(originals)
    val dropped: Map[String, Working] = pics.foldLeft(loaded): (acc, pic) =>
      val destTitle: String = monthParsed(pic.month).title
      dropPair(
        acc,
        pic.day -> pic.stem,
        universe,
        working => working.album.span.isDefined && working.album.title != destTitle
      )
    val months: Seq[YearMonth] =
      pics.map(_.month).distinct.sortBy(month => (month.getYear, month.getMonthValue))
    val (ready, creates, spoken) = months.foldLeft((dropped, Seq.empty[Created], unspoken)):
      case ((current, made, acc), month) =>
        val parsed: Title.Parsed = monthParsed(month)
        val destTitle: String = parsed.title
        val destFile: File = Title.file(originals, parsed)
        val monthPics: Seq[Pic] = pics.filter(pic => pic.month.equals(month))
        val other: Working => Boolean = working => working.album.span.isDefined && working.album.title != destTitle
        current.get(DirectoryIndex.key(destFile)) match
          case None =>
            val claimed: Set[(LocalDate, String)] = monthPics.map(pic => pic.day -> pic.stem).toSet
            val include: Seq[Include.Entry] =
              Include.compress(parsed.span, claimed, universe, allowAll = false)
            val album: Album = Album.pending(destTitle, parsed.span, include)
            val spokenPics: Spoken = monthPics.foldLeft(acc.addCreate(destTitle)): (soFar, pic) =>
              val after: Boolean = holds(include, parsed.span, pic.day, pic.stem)
              soFar.addPicture(excludePairs(current.values, pic, other), Option.when(after)(destTitle -> pic.path))
            (current, made :+ Created(destFile, album), spokenPics)
          case Some(dest) =>
            val updated: Working = monthPics.foldLeft(dest)((cur, pic) => addTo(cur, pic, universe))
            val spokenPics: Spoken = monthPics.foldLeft(acc): (soFar, pic) =>
              val before: Boolean = dest.album.claims(pic.day, pic.stem)
              val after: Boolean = holds(updated.current, updated.album.span, pic.day, pic.stem)
              soFar.addPicture(
                excludePairs(current.values, pic, other),
                Option.when(before != after)(destTitle -> pic.path)
              )
            (current.updated(DirectoryIndex.key(destFile), updated), made, spokenPics)
    commit(originals, dryRun, ready.values, creates, spoken)

  def into(session: Session, dryRun: Boolean, title: String, pictures: Seq[String]): Json =
    val originals: File = session.root.configuration.archive
    val (destFile, destAlbum) = AlbumCommand.load(originals, title)
    val resolved: Seq[Picture] = pictures.map(session.parsePicture)
    AlbumCommand.rejectRepeated(resolved)
    destAlbum.span.foreach: span =>
      resolved.foreach: picture =>
        val path: String = AlbumCommand.pathOf(picture)
        if !picture.parent.day.map(_.date).exists(span.contains) then
          throw IllegalArgumentException(s"$path is outside ${destAlbum.title}")
    val pics: Seq[Pic] = resolved.map: picture =>
      val day: LocalDate = picture.parent.day
        .map(_.date)
        .getOrElse(
          throw IllegalArgumentException("not a date-tree path")
        )
      Pic(AlbumCommand.pathOf(picture), day, picture.name, YearMonth.from(day))
    val universe: Map[LocalDate, Set[String]] = Archive.universe(Archive.pictures(originals))
    val destKey: String = DirectoryIndex.key(destFile)
    val scanned: Map[String, Working] = load(originals)
    val files: Map[String, Working] =
      if scanned.contains(destKey) then scanned
      else scanned.updated(destKey, Working(destFile, destAlbum, destAlbum.include, Seq.empty, Seq.empty))
    val dropped: Map[String, Working] =
      if destAlbum.span.isEmpty then files
      else
        pics.foldLeft(files): (acc, pic) =>
          dropPair(
            acc,
            pic.day -> pic.stem,
            universe,
            working => working.album.span.isDefined && DirectoryIndex.key(working.file) != destKey
          )
    val base: Working = dropped.getOrElse(
      destKey,
      Working(destFile, destAlbum, destAlbum.include, Seq.empty, Seq.empty)
    )
    val dest: Working = pics.foldLeft(base)((cur, pic) => addTo(cur, pic, universe))
    val ready: Map[String, Working] = dropped.updated(destKey, dest)
    val other: Working => Boolean = working =>
      working.album.span.isDefined && DirectoryIndex.key(working.file) != destKey
    val spoken: Spoken = pics.foldLeft(unspoken): (acc, pic) =>
      val before: Boolean = dest.album.claims(pic.day, pic.stem)
      val after: Boolean = holds(dest.current, dest.album.span, pic.day, pic.stem)
      acc.addPicture(excludePairs(ready.values, pic, other), Option.when(before != after)(dest.album.title -> pic.path))
    commit(originals, dryRun, ready.values, Seq.empty, spoken)

  private def dated(session: Session, pictures: Seq[String]): Seq[Pic] =
    val resolved: Seq[Picture] = pictures.map(session.parsePicture)
    AlbumCommand.rejectRepeated(resolved)
    resolved.map: picture =>
      val day: LocalDate = picture.parent.day
        .map(_.date)
        .getOrElse(
          throw IllegalArgumentException("not a date-tree path")
        )
      Pic(AlbumCommand.pathOf(picture), day, picture.name, YearMonth.from(day))

  private def monthParsed(month: YearMonth): Title.Parsed =
    Title.Parsed(Some(Title.Span(month.atDay(1), month.atEndOfMonth, Title.Grain.Month)), None)

  private def load(originals: File): Map[String, Working] =
    Archive
      .scan(originals)
      .albums
      .foldLeft(Map.empty[String, Working]): (acc, loaded) =>
        acc.updated(
          DirectoryIndex.key(loaded.file),
          Working(loaded.file, loaded.album, loaded.album.include, Seq.empty, Seq.empty)
        )

  private def dropPair(
    files: Map[String, Working],
    pair: (LocalDate, String),
    universe: Map[LocalDate, Set[String]],
    other: Working => Boolean
  ): Map[String, Working] =
    files.foldLeft(files):
      case (acc, (key, working)) =>
        val hit: Boolean = other(working) && holds(working.current, working.album.span, pair._1, pair._2)
        if !hit then acc
        else
          val next: Seq[Include.Entry] = Include.drop(working.current, working.album.span, pair, universe)
          acc.updated(key, working.copy(current = next, drops = working.drops :+ pair))

  private def addTo(working: Working, pic: Pic, universe: Map[LocalDate, Set[String]]): Working =
    val pair: (LocalDate, String) = pic.day -> pic.stem
    if holds(working.current, working.album.span, pair._1, pair._2) then working
    else
      working.copy(
        current = AlbumCommand.addCovering(working.current, working.album.span, pair, universe),
        adds = working.adds :+ pair
      )

  private def holds(include: Seq[Include.Entry], span: Option[Title.Span], day: LocalDate, stem: String): Boolean =
    include.exists(entry => Include.claims(span, entry, day, stem))

  private def excludeTitles(files: Iterable[Working], pic: Pic, other: Working => Boolean): Seq[String] =
    val titles: Seq[String] = files.toSeq
      .filter(other)
      .flatMap: working =>
        val before: Boolean = working.album.claims(pic.day, pic.stem)
        val after: Boolean = holds(working.current, working.album.span, pic.day, pic.stem)
        if before && !after then Some(working.album.title) else None
    titles.sorted

  private def excludePairs(
    files: Iterable[Working],
    pic: Pic,
    other: Working => Boolean
  ): Seq[(String, String)] =
    excludeTitles(files, pic, other).map(title => title -> pic.path)

  private def replay(
    fresh: Album,
    working: Working,
    universe: Map[LocalDate, Set[String]]
  ): Seq[Include.Entry] =
    val dropped: Seq[Include.Entry] = working.drops.foldLeft(fresh.include): (cur, pair) =>
      Include.drop(cur, fresh.span, pair, universe)
    working.adds.foldLeft(dropped): (cur, pair) =>
      if holds(cur, fresh.span, pair._1, pair._2) then cur
      else AlbumCommand.addCovering(cur, fresh.span, pair, universe)

  // The file can change before this lock. Replay on what is there now.
  private def commit(
    originals: File,
    dryRun: Boolean,
    files: Iterable[Working],
    creates: Seq[Created],
    spoken: Spoken
  ): Json =
    val changed: Seq[Working] =
      files.filter(working => !AlbumCommand.same(working.album.include, working.current)).toSeq
    changed.foreach(working => Album.renderedInclude(working.file, working.current))
    if !dryRun && (changed.nonEmpty || creates.nonEmpty) then
      Album.withWriteLock:
        val again: Map[LocalDate, Set[String]] = Archive.universe(Archive.pictures(originals))
        val ready: Seq[(File, Seq[Include.Entry])] = changed.map: working =>
          val fresh: Album = Album.read(originals, working.file)
          val next: Seq[Include.Entry] =
            if AlbumCommand.same(fresh.include, working.album.include) then working.current
            else replay(fresh, working, again)
          Album.renderedInclude(working.file, next)
          working.file -> next
        creates.foreach: created =>
          Files.mkDirs(created.file.getParentFile)
          Album.create(originals, created.album)
        ready.foreach:
          case (file, next) => Album.putInclude(file, next)
    spoken.lines.foreach(Transcript.println)
    Json.Obj(
      Seq(
        "dryRun" -> Json.Bool(dryRun),
        "created" -> AlbumCommand.strings(spoken.created),
        "excluded" -> hits(spoken.excluded),
        "included" -> hits(spoken.included)
      )
    )

  private def hits(pairs: Seq[(String, String)]): Json =
    Json.Arr(pairs.map: (title, path) =>
      Json.Obj(Seq("title" -> Json.Str(title), "path" -> Json.Str(path))))

final case class ItemMonth(dryRun: Boolean, pictures: Seq[String]) extends Operation:
  override def execute(session: Session): (Session, Result) =
    var produced: Json = Json.Null
    Operation.run(session, produced):
      produced = ItemPlace.month(session, dryRun, pictures)

final case class ItemInto(dryRun: Boolean, title: String, pictures: Seq[String]) extends Operation:
  override def execute(session: Session): (Session, Result) =
    var produced: Json = Json.Null
    Operation.run(session, produced):
      produced = ItemPlace.into(session, dryRun, title, pictures)
