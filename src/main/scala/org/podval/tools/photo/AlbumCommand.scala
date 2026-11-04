package org.podval.tools.photo

import java.io.File
import java.time.LocalDate

private object AlbumCommand:
  def load(originals: File, title: String): (File, Album) =
    Title.parse(title) match
      case None =>
        throw IllegalArgumentException(s"not an album title: $title")
      case Some(parsed) =>
        val file: File = Title.file(originals, parsed)
        if !file.isFile then throw IllegalArgumentException(s"no such album: ${parsed.title}")
        else (file, Album.read(originals, file))

  def pathOf(picture: Picture): String = picture.toString.stripPrefix("/")

  def rejectRepeated(pictures: Seq[Picture]): Unit =
    val _: Set[String] = pictures.foldLeft(Set.empty[String]): (seen, picture) =>
      val path: String = pathOf(picture)
      if seen.contains(path) then throw IllegalArgumentException(s"repeated picture $path")
      else seen + path

  def same(left: Seq[Include.Entry], right: Seq[Include.Entry]): Boolean =
    left.map(Include.render) == right.map(Include.render)

  def edit(
    include: Seq[Include.Entry],
    span: Option[Title.Span],
    universe: Map[LocalDate, Set[String]],
    pictures: Seq[Picture],
    exclude: Boolean
  ): (Seq[Include.Entry], Seq[String]) =
    // Include.add keeps only pairs on the hit day. A multi-day line would lose its other days.
    def addCovering(
      current: Seq[Include.Entry],
      albumSpan: Option[Title.Span],
      pair: (LocalDate, String),
      scanned: Map[LocalDate, Set[String]]
    ): Seq[Include.Entry] =
      val widened: Map[LocalDate, Set[String]] =
        scanned.updated(pair._1, scanned.getOrElse(pair._1, Set.empty) + pair._2)
      def covers(entry: Include.Entry): Boolean =
        Include.expand(albumSpan, entry, widened).exists(item => item._1.equals(pair._1))
      val (prefix, tail) = current.span(entry => !covers(entry))
      val (hit, kept) = tail.partition(covers)
      val pairs: Set[(LocalDate, String)] =
        hit.flatMap(entry => Include.expand(albumSpan, entry, widened)).toSet + pair
      val first: Seq[Include.Entry] = Include.compress(albumSpan, pairs, widened)
      def isAll(entry: Include.Entry): Boolean = entry match
        case Include.Entry.All(_) => true
        case Include.Entry.Dates(_, _, _) => false
      val touched: Seq[Include.Entry] =
        if (prefix.nonEmpty || kept.nonEmpty) && first.exists(isAll) then
          Include.compress(None, pairs, widened)
        else first
      prefix ++ touched ++ kept

    pictures.foldLeft((include, Seq.empty[String])):
      case ((current, changed), picture) =>
        val day: LocalDate = picture.parent.day.map(_.date).getOrElse(
          throw IllegalArgumentException("not a date-tree path")
        )
        val stem: String = picture.name
        val path: String = pathOf(picture)
        val before: Boolean = current.exists(entry => Include.claims(span, entry, day, stem))
        val next: Seq[Include.Entry] =
          if !exclude && before then current
          else if exclude then Include.drop(current, span, (day, stem), universe)
          else addCovering(current, span, (day, stem), universe)
        val after: Boolean = next.exists(entry => Include.claims(span, entry, day, stem))
        (next, if before == after then changed else changed :+ path)

  // Parent chain only, matching deleteEmptyDirectories. An empty sibling is not removed.
  // `keep` is the destination parent: it and its ancestors stay, including before mkDirs.
  def removedDirectories(start: File, stop: File, gone: File, keep: Option[File]): Seq[File] =
    def key(file: File): String = DirectoryIndex.key(file)
    def keeps(directory: File): Boolean = keep.exists: held =>
      val heldKey: String = key(held)
      val dirKey: String = key(directory)
      dirKey == heldKey || heldKey.startsWith(dirKey + File.separator)
    def emptyEnough(directory: File, absent: Set[String]): Boolean =
      val children: Seq[File] = Option(directory.listFiles).map(_.toSeq).getOrElse(Seq.empty)
      children.forall(child => absent.contains(key(child)))
    def climb(directory: File, absent: Set[String]): List[File] =
      if key(directory) == key(stop) || directory.getParentFile == null then Nil
      else if keeps(directory) then Nil
      else if directory.isDirectory && emptyEnough(directory, absent) then
        directory :: climb(directory.getParentFile, absent + key(directory))
      else Nil
    climb(start, Set(key(gone)))

  def strings(values: Seq[String]): Json = Json.Arr(values.map(Json.Str(_)))

final case class AlbumLs(path: Option[String]) extends Operation:
  override def execute(session: Session): (Session, Result) =
    var produced: Json = Json.Null
    Operation.run(session, produced):
      val parsed: Option[Path] = path.map(session.parse)
      val root: Root = parsed.fold(session.root)(_.root)
      val prefix: Option[String] = parsed.map(_.toString.stripPrefix("/")).filter(_.nonEmpty)
      val titles: Seq[String] = Archive.scan(root.configuration.archive).albums
        .map(_.album)
        .filter(album => Album.albumMatches(album.startDir, album.title, prefix))
        .map(_.title)
        .sorted
      titles.foreach(Transcript.println)
      produced = Json.Obj(Seq("albums" -> AlbumCommand.strings(titles)))

final case class AlbumShow(title: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    var produced: Json = Json.Null
    Operation.run(session, produced):
      val originals: File = session.root.configuration.archive
      val (file, album) = AlbumCommand.load(originals, title)
      val spanText: Option[String] = album.span.map(Title.renderSpan)
      val nameText: Option[String] = Title.parse(album.title).flatMap(_.name)
      val include: Seq[String] = album.include.map(Include.logical)
      def dash(text: String): String = if text.isEmpty then "-" else text
      Transcript.println(s"path: ${Album.relative(originals, file)}")
      Transcript.println(s"title: ${album.title}")
      Transcript.println(s"span: ${spanText.getOrElse("-")}")
      Transcript.println(s"name: ${nameText.getOrElse("-")}")
      Transcript.println(s"external: ${album.external}")
      Transcript.println(s"google-album-id: ${dash(album.googleAlbumId)}")
      Transcript.println(s"published-title: ${dash(album.publishedTitle)}")
      Transcript.println(s"google-title: ${dash(album.googleTitle)}")
      Transcript.println(s"owner: ${album.owner.getOrElse("-")}")
      Transcript.println("include:")
      include.foreach(line => Transcript.println(s"  $line"))
      produced = Json.Obj(Seq(
        "path" -> Json.Str(Album.relative(originals, file)),
        "title" -> Json.Str(album.title),
        "span" -> spanText.fold(Json.Null)(Json.Str(_)),
        "name" -> nameText.fold(Json.Null)(Json.Str(_)),
        "external" -> Json.Bool(album.external),
        "googleAlbumId" -> Json.Str(album.googleAlbumId),
        "publishedTitle" -> Json.Str(album.publishedTitle),
        "googleTitle" -> Json.Str(album.googleTitle),
        "owner" -> album.owner.fold(Json.Null)(Json.Str(_)),
        "include" -> AlbumCommand.strings(include)
      ))

final case class AlbumNew(dryRun: Boolean, span: Option[String], name: Option[String]) extends Operation:
  override def execute(session: Session): (Session, Result) =
    var produced: Json = Json.Null
    Operation.run(session, produced):
      val parsedSpan: Option[Title.Parsed] = span.map(parseSpan)
      val albumName: Option[String] = name.map(parseName)
      val parsed: Title.Parsed = (parsedSpan, albumName) match
        case (None, None) =>
          throw IllegalArgumentException("missing span or name")
        case (Some(value), None) =>
          value
        case (None, Some(text)) =>
          Title.Parsed(None, Some(text))
        case (Some(value), Some(text)) =>
          value.copy(name = Some(text))
      val originals: File = session.root.configuration.archive
      val file: File = Title.file(originals, parsed)
      if file.exists then throw IllegalArgumentException(s"album already exists: ${parsed.title}")
      val relative: String = Album.relative(originals, file)
      val include: Seq[Include.Entry] =
        if parsed.span.isDefined then Seq(Include.Entry.All(Seq.empty)) else Seq.empty
      val album: Album = Album(
        title = parsed.title,
        span = parsed.span,
        include = include,
        external = false,
        googleAlbumId = "",
        publishedTitle = "",
        googleTitle = "",
        owner = None
      )
      Transcript.println(s"create ${parsed.title}")
      Transcript.println(relative)
      if !dryRun then
        Files.mkDirs(file.getParentFile)
        Album.create(originals, album)
      produced = Json.Obj(Seq(
        "dryRun" -> Json.Bool(dryRun),
        "title" -> Json.Str(parsed.title),
        "path" -> Json.Str(relative)
      ))

  private def parseSpan(text: String): Title.Parsed =
    Title.parse(text) match
      case Some(parsed) if parsed.span.isDefined && parsed.name.isEmpty =>
        parsed
      case _ =>
        throw IllegalArgumentException(s"not a span: $text")

  private def parseName(text: String): String =
    if Title.legalName(text) then text
    else throw IllegalArgumentException(s"not an album name: $text")

final case class AlbumRm(dryRun: Boolean, title: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    var produced: Json = Json.Null
    Operation.run(session, produced):
      val originals: File = session.root.configuration.archive
      val (file, album) = AlbumCommand.load(originals, title)
      if album.external then throw IllegalArgumentException(s"refusing external album ${album.title}")
      val removed: Seq[File] =
        AlbumCommand.removedDirectories(file.getParentFile, originals, file, None)
      Transcript.println(s"delete ${album.title}")
      Transcript.println(Album.relative(originals, file))
      removed.foreach(dir => Transcript.println(s"rmdir ${Album.relative(originals, dir)}"))
      if !dryRun then
        Album.withWriteLock:
          Files.delete(file)
        Files.deleteEmptyDirectories(file.getParentFile, originals)
      produced = Json.Obj(Seq(
        "dryRun" -> Json.Bool(dryRun),
        "title" -> Json.Str(album.title),
        "removed" -> AlbumCommand.strings(removed.map(dir => Album.relative(originals, dir)))
      ))

final case class AlbumMv(dryRun: Boolean, from: String, to: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    var produced: Json = Json.Null
    Operation.run(session, produced):
      val originals: File = session.root.configuration.archive
      val (file, album) = AlbumCommand.load(originals, from)
      if album.external then throw IllegalArgumentException(s"refusing external album ${album.title}")
      val parsed: Title.Parsed = Title.parse(to).getOrElse(
        throw IllegalArgumentException(s"not an album title: $to")
      )
      val dest: File = Title.file(originals, parsed)
      if dest.exists then throw IllegalArgumentException(s"album already exists: ${parsed.title}")
      album.include.foreach: entry =>
        if !Include.inside(parsed.span, entry) then
          throw IllegalArgumentException(s"include is outside ${parsed.title}: ${Include.logical(entry)}")
      val oldParent: File = file.getParentFile
      val newParent: File = dest.getParentFile
      val sameParent: Boolean = DirectoryIndex.key(oldParent) == DirectoryIndex.key(newParent)
      val removed: Seq[File] =
        if sameParent then Seq.empty
        else AlbumCommand.removedDirectories(oldParent, originals, file, Some(newParent))
      if !dryRun then
        // Web.toggle can rewrite include before this lock is taken.
        Album.withWriteLock:
          val fresh: Album = Album.read(originals, file)
          if fresh.external then throw IllegalArgumentException(s"refusing external album ${fresh.title}")
          fresh.include.foreach: entry =>
            if !Include.inside(parsed.span, entry) then
              throw IllegalArgumentException(s"include is outside ${parsed.title}: ${Include.logical(entry)}")
          if dest.exists then throw IllegalArgumentException(s"album already exists: ${parsed.title}")
          Files.mkDirs(newParent)
          Files.move(file, dest)
      Transcript.println(s"move ${album.title} ${parsed.title}")
      Transcript.println(s"${Album.relative(originals, file)} ${Album.relative(originals, dest)}")
      removed.foreach(dir => Transcript.println(s"rmdir ${Album.relative(originals, dir)}"))
      if !dryRun && !sameParent then Files.deleteEmptyDirectories(oldParent, originals)
      produced = Json.Obj(Seq(
        "dryRun" -> Json.Bool(dryRun),
        "from" -> Json.Str(album.title),
        "to" -> Json.Str(parsed.title),
        "path" -> Json.Str(Album.relative(originals, dest))
      ))

final case class AlbumEdit(
  dryRun: Boolean,
  exclude: Boolean,
  title: String,
  pictures: Seq[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    var produced: Json = Json.Null
    Operation.run(session, produced):
      val originals: File = session.root.configuration.archive
      val (file, album) = AlbumCommand.load(originals, title)
      val resolved: Seq[Picture] = pictures.map(session.parsePicture)
      AlbumCommand.rejectRepeated(resolved)
      album.span.foreach: span =>
        resolved.foreach: picture =>
          val path: String = AlbumCommand.pathOf(picture)
          val inside: Boolean = picture.parent.day.map(_.date).exists(span.contains)
          if !inside then throw IllegalArgumentException(s"$path is outside ${album.title}")
      val universe: Map[LocalDate, Set[String]] = Archive.universe(Archive.pictures(originals))
      val (planned, changed) =
        AlbumCommand.edit(album.include, album.span, universe, resolved, exclude)
      val unchanged: Boolean = AlbumCommand.same(planned, album.include)
      val verb: String = if exclude then "exclude" else "include"
      val (shown, paths) =
        if unchanged then (album.include, Seq.empty)
        else
          Album.renderedInclude(file, planned)
          if dryRun then
            changed.foreach(path => Transcript.println(s"$verb ${album.title} $path"))
            (planned, changed)
          else
            Album.withWriteLock:
              val fresh: Album = Album.read(originals, file)
              val (next, againChanged) =
                if AlbumCommand.same(fresh.include, album.include) then (planned, changed)
                else
                  val again: Map[LocalDate, Set[String]] = Archive.universe(Archive.pictures(originals))
                  AlbumCommand.edit(fresh.include, fresh.span, again, resolved, exclude)
              Album.renderedInclude(file, next)
              Album.putInclude(file, next)
              againChanged.foreach(path => Transcript.println(s"$verb ${album.title} $path"))
              (next, againChanged)
      produced = Json.Obj(Seq(
        "dryRun" -> Json.Bool(dryRun),
        "title" -> Json.Str(album.title),
        "include" -> AlbumCommand.strings(shown.map(Include.logical)),
        "pictures" -> AlbumCommand.strings(paths)
      ))
