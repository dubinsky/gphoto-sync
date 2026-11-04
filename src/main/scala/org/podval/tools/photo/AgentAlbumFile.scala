package org.podval.tools.photo

import java.io.File
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, StandardCopyOption, StandardOpenOption}
import java.time.{LocalDate, YearMonth}
import java.util.UUID


private[photo] final class AgentAlbumFile(
  id: Option[String],
  dryRun: Boolean,
  replace: Boolean,
  googleTitle: Option[String],
  title: String
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        val album: Catalog.Album = AgentSupport.select(albums, title, id)
        if album.autoAdd then AgentSupport.skipped(loaded, album, "auto-add")
        else if album.error.exists(_.nameFailures > 0) || album.unsets.nonEmpty then
          AgentSupport.skipped(loaded, album, "unread names")
        else AgentAlbumFile.writeAlbum(loaded, album, dryRun, replace, googleTitle)

private[photo] object AgentAlbumFile:

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
    val (matched, hits, _) = AgentSupport.gather(session, album)
    val claimed: Set[(LocalDate, String)] = hits.map(hit => hit._1 -> hit._3).toSet
    // Album.read rejects an include day outside the span. Fail before any partial.
    parsed.span.foreach: span =>
      if claimed.exists(pair => !span.contains(pair._1)) then
        throw IllegalArgumentException("matched day outside the span")
    val days: Seq[LocalDate] = parsed.span match
      case Some(span) => AgentSupport.eachDay(span)
      case None => album.items.flatMap(_.date).distinct.sorted
    val (scanned, universe) = AgentSupport.scanDays(matched, days)
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
    val stored: Option[String] = Option.when(googleTitle.nonEmpty)(googleTitle)
    val written: Album =
      if keptId.isEmpty then Album.external(pathTitle, parsed.span, include, stored, owner)
      else Album.published(pathTitle, parsed.span, include, keptId, keptPublished, stored)
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
