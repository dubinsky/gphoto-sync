package org.podval.tools.photo

import scala.collection.mutable
import java.io.File
import java.nio.file.{Files as NFiles}
import java.time.{LocalDate, YearMonth}

// Links catalog albums to album files in this process. Not a loop over album-file:
// a rename keeps the path.
final class Reconcile(args: Cli.ReconcileArgs) extends Operation:
  def execute(session: Session): (Session, Result) =
    // Filled before a write so a failure still carries the plan. Read after the body.
    var planned: Json = Json.Null
    Operation.run(session, planned):
      Reconcile.run(session, args)(planned = _)

object Reconcile:

  enum Kind(val wire: String) derives CanEqual:
    case Create extends Kind("create")
    case Update extends Kind("update")
    case Gone extends Kind("gone")
    case Pending extends Kind("pending")
    case PendingRename extends Kind("pending-rename")
    case Skipped extends Kind("skipped")

  private[photo] final case class Action(
    kind: Kind,
    label: String,
    path: String,
    reason: String,
    id: Option[String]
  )

  private[photo] enum Touch:
    case CreateFile(dest: File, text: String)
    case EditTitles(file: File, googleTitle: Option[Option[String]], publishedTitle: Option[String])
    case EditInclude(file: File, include: Seq[Include.Entry])
    case RemoveFile(file: File)

  private[photo] final case class Plan(actions: Seq[Action], links: Seq[AlbumLinks.Row], touches: Seq[Touch])

  private final case class Placed(
    album: Catalog.Album,
    parsed: Title.Parsed,
    monthFile: Boolean,
    dest: File,
    rel: String
  )

  private final case class MatchSet(
    include: Seq[Include.Entry],
    claimed: Set[(LocalDate, String)],
    universe: Map[LocalDate, Set[String]]
  )

  private enum Prepared:
    case Ready(matched: MatchSet)
    case Refuse(reason: String)

  private def run(session: Session, args: Cli.ReconcileArgs)(use: Json => Unit): Unit =
    val archive: File = session.root.configuration.archive
    val (catalogSession, catalog) = session.loadCatalog
    DirectoryIndex.replace(catalogSession)
    val existing: Seq[AlbumLinks.Row] = AlbumLinks.read(archive)
    // scan, not isAlbumFile: a digit-leading day sidecar is already not an album.
    val found: Seq[Album.Loaded] = Archive.scan(archive).albums
    val (ready, plan) = classify(DirectoryIndex.now, archive, catalog, found, existing, args)
    DirectoryIndex.replace(ready)
    use(jsonOf(plan))
    plan.actions.foreach(action => Transcript.println(line(action)))
    // A dry run writes nothing. dropAll only after a toml or the link file changes.
    if !args.dryRun && perform(archive, plan) then
      // One parent drop would leave the other directories warm.
      DirectoryIndex.dropAll()

  private[photo] def line(action: Action): String = action.kind match
    case Kind.Pending => s"pending ${action.path}"
    case Kind.PendingRename => s"pending-rename ${action.path}"
    case Kind.Skipped => s"skipped ${action.label} ${action.reason}"
    case Kind.Gone => s"gone ${action.label}"
    case Kind.Create => s"create ${action.label}"
    case Kind.Update => s"update ${action.label}"

  // Drift calls this and does not perform the plan.
  private[photo] def classify(
    start: Session,
    archive: File,
    catalog: Seq[Catalog.Album],
    found: Seq[Album.Loaded],
    existing: Seq[AlbumLinks.Row],
    args: Cli.ReconcileArgs
  ): (Session, Plan) =
    val relOf: Album.Loaded => String = row => Album.relative(archive, row.file)
    val byRel: Map[String, Album.Loaded] = found.map(row => relOf(row) -> row).toMap
    val catalogById: Map[String, Catalog.Album] = catalog.map(album => album.id -> album).toMap
    val titleCount: Map[String, Int] = catalog.groupBy(_.title).view.mapValues(_.length).toMap

    // A pending file is never a link target, even when a stale row names it.
    val pinned: Map[String, Album.Loaded] = existing.flatMap: link =>
      if !catalogById.contains(link.id) then None
      else byRel.get(link.path).filter(row => !pending(row.album)).map(link.id -> _)
    .toMap
    val pinnedPaths: Set[String] = pinned.values.map(relOf).toSet

    val open: Seq[Catalog.Album] = catalog.filter: album =>
      !pinned.contains(album.id) && hard(album).isEmpty && titleCount.getOrElse(album.title, 0) == 1

    val taken: mutable.Set[String] = mutable.Set.empty
    taken ++= pinnedPaths
    val assigned: mutable.Map[String, Album.Loaded] = mutable.Map.empty
    assigned ++= pinned
    val ambiguous: mutable.Set[String] = mutable.Set.empty

    def pool(rows: Seq[Album.Loaded]): Seq[Album.Loaded] =
      rows.filter(row => !pending(row.album) && !taken.contains(relOf(row)))

    def take(matches: Map[String, Album.Loaded], crowded: Set[String]): Unit =
      matches.foreach: (id, row) =>
        assigned(id) = row
        taken += relOf(row)
      ambiguous ++= crowded

    val googlePool: Seq[Album.Loaded] =
      pool(found).filter(row => row.album.external && row.album.googleTitle.nonEmpty)
    val (googleHits, googleCrowded) = pair(open, googlePool, relOf, (album, row) => row.album.googleTitle == album.title)
    take(googleHits, googleCrowded)

    val workingPool: Seq[Album.Loaded] =
      pool(found).filter(row => row.album.external && row.album.googleTitle.isEmpty)
    val workingOpen: Seq[Catalog.Album] = open.filter(album => !assigned.contains(album.id) && !ambiguous.contains(album.id))
    val (workingHits, workingCrowded) =
      pair(workingOpen, workingPool, relOf, (album, row) => row.album.title == working(album.title))
    take(workingHits, workingCrowded)

    val clientPool: Seq[Album.Loaded] = pool(found).filter(row => client(row.album))
    val clientOpen: Seq[Catalog.Album] =
      open.filter(album => !assigned.contains(album.id) && !ambiguous.contains(album.id))
    val (clientHits, clientCrowded) = pair(clientOpen, clientPool, relOf, (album, row) => clientHit(album, row.album))
    take(clientHits, clientCrowded)

    val creates: mutable.Map[String, (Title.Parsed, Boolean, File, String)] = mutable.Map.empty
    val refused: mutable.Map[String, String] = mutable.Map.empty
    val placed: mutable.ListBuffer[Placed] = mutable.ListBuffer.empty
    catalog.foreach: album =>
      val blocked: Boolean =
        assigned.contains(album.id) || ambiguous.contains(album.id) || hard(album).isDefined ||
          titleCount.getOrElse(album.title, 0) > 1
      if !blocked then
        try
          val (parsed, monthFile) = AgentCommands.place(album)
          val dest: File = Title.file(archive, parsed)
          val rel: String = Album.relative(archive, dest)
          if !dest.getParentFile.isDirectory then refused(album.id) = "no such directory"
          else placed += Placed(album, parsed, monthFile, dest, rel)
        catch
          case error: IllegalArgumentException =>
            refused(album.id) = placeReason(Operation.messageOf(error))

    // Titles that are not exact duplicates can still place to one path. The extra is not a second link row.
    // An existing path is not given to the first album.
    placed.toSeq.groupBy(_.rel).foreach: (_, group) =>
      val chosen: Placed = group.head
      if group.sizeIs > 1 && (chosen.dest.exists || taken.contains(chosen.rel)) then
        group.foreach(row => refused(row.album.id) = "ambiguous title")
      else if group.sizeIs > 1 then
        creates(chosen.album.id) = (chosen.parsed, chosen.monthFile, chosen.dest, chosen.rel)
        group.drop(1).foreach(row => refused(row.album.id) = "ambiguous title")
      else if chosen.dest.exists then
        byRel.get(chosen.rel) match
          case Some(row) if pending(row.album) || taken.contains(chosen.rel) =>
            refused(chosen.album.id) = "path exists"
          case Some(row) if agrees(chosen.album, row.album) =>
            assigned(chosen.album.id) = row
            taken += chosen.rel
          case Some(_) =>
            refused(chosen.album.id) = "working title"
          case None =>
            refused(chosen.album.id) = "path exists"
      else creates(chosen.album.id) = (chosen.parsed, chosen.monthFile, chosen.dest, chosen.rel)

    var session: Session = start
    val actions: mutable.ListBuffer[Action] = mutable.ListBuffer.empty
    val touches: mutable.ListBuffer[Touch] = mutable.ListBuffer.empty
    val links: mutable.ListBuffer[AlbumLinks.Row] = mutable.ListBuffer.empty
    val linkedPaths: mutable.Set[String] = mutable.Set.empty
    val renamePaths: mutable.Set[String] = mutable.Set.empty

    def keep(id: String, rel: String): Unit =
      if links.exists(row => row.id == id || row.path == rel) then
        throw IllegalStateException("duplicate album link")
      links += AlbumLinks.Row(id, rel)
      linkedPaths += rel

    catalog.foreach: album =>
      hard(album) match
        case Some(reason) =>
          actions += skipped(album.title, "", reason, Some(album.id))
          pinned.get(album.id).foreach(row => keep(album.id, relOf(row)))
        case None =>
          assigned.get(album.id) match
            case Some(row) =>
              val rel: String = relOf(row)
              // Date span and a rejected owner leave the bytes, including include.
              if dateSpanMoved(archive, album, row.album, rel) then
                actions += skipped(album.title, rel, "date span", Some(album.id))
                keep(album.id, rel)
              else if ownerBlocked(album, row.album) then
                actions += skipped(album.title, rel, "owner", Some(album.id))
                keep(album.id, rel)
              else if album.title.isEmpty && storesTitle(album, row.album) then
                val reason: String = if row.album.owner.isDefined || album.owner.isDefined then "owner" else "title"
                actions += skipped(album.title, rel, reason, Some(album.id))
                keep(album.id, rel)
              else if pendingRename(album, row.album) then
                actions += Action(Kind.PendingRename, "", rel, "", Some(album.id))
                if pinned.contains(album.id) then keep(album.id, rel)
                else renamePaths += rel
              else
                val (next, prepared) = prepare(session, album, row.album.span, monthFile(row.album))
                session = next
                prepared match
                  case Prepared.Refuse(reason) =>
                    actions += skipped(album.title, rel, reason, Some(album.id))
                    keep(album.id, rel)
                  case Prepared.Ready(matched) =>
                    edit(album, row, matched) match
                      case Edit.Pure =>
                        keep(album.id, rel)
                      case Edit.Skip(reason) =>
                        actions += skipped(album.title, rel, reason, Some(album.id))
                        keep(album.id, rel)
                      case Edit.Write(fileTouches) =>
                        actions += Action(Kind.Update, album.title, rel, "", Some(album.id))
                        if !args.dryRun then touches ++= fileTouches
                        keep(album.id, rel)
            case None =>
              creates.get(album.id) match
                case Some((parsed, monthFlag, dest, rel)) =>
                  val (next, prepared) = prepare(session, album, parsed.span, monthFlag)
                  session = next
                  prepared match
                    case Prepared.Refuse(reason) =>
                      actions += skipped(album.title, "", reason, Some(album.id))
                    case Prepared.Ready(matched) =>
                      fresh(album, parsed, matched) match
                        case Left(reason) =>
                          actions += skipped(album.title, "", reason, Some(album.id))
                        case Right(text) =>
                          actions += Action(Kind.Create, album.title, rel, "", Some(album.id))
                          if !args.dryRun then touches += Touch.CreateFile(dest, text)
                          keep(album.id, rel)
                case None =>
                  val reason: String = refused.getOrElse(album.id, "ambiguous title")
                  actions += skipped(album.title, "", reason, Some(album.id))

    val pendingActions: Seq[Action] = found.filter(row => pending(row.album)).sortBy(relOf).map: row =>
      Action(Kind.Pending, "", relOf(row), "", None)
    // A stale id still names this path. Reporting it gone would delete the file under --delete.
    existing.filter(link => renamePaths.contains(link.path) && !linkedPaths.contains(link.path)).foreach: link =>
      keep(link.id, link.path)
    val goneLinks: Seq[AlbumLinks.Row] = existing.filter: link =>
      !catalogById.contains(link.id) &&
        byRel.get(link.path).exists(row => !pending(row.album) && !linkedPaths.contains(link.path)) &&
        !renamePaths.contains(link.path)
    val goneActions: Seq[Action] = goneLinks.map: link =>
      val row: Album.Loaded = byRel(link.path)
      val label: String = if row.album.googleTitle.nonEmpty then row.album.googleTitle else row.album.title
      Action(Kind.Gone, label, relOf(row), "", Some(link.id))
    // Link rows follow --delete even on a dry run. Only the removal and the write wait for a real run.
    if args.delete then
      if !args.dryRun then
        goneLinks.foreach(link => touches += Touch.RemoveFile(byRel(link.path).file))
    else
      goneLinks.foreach(link => keep(link.id, link.path))
    val gonePaths: Set[String] = goneLinks.map(_.path).toSet
    val unlinked: Seq[Action] = found.filter: row =>
      !pending(row.album) && !linkedPaths.contains(relOf(row)) && !gonePaths.contains(relOf(row)) &&
        !renamePaths.contains(relOf(row))
    .sortBy(relOf).map: row =>
      skipped(relOf(row), relOf(row), "unlinked", None)

    val ordered: Seq[Action] = actions.toSeq ++ pendingActions ++ unlinked ++ goneActions.sortBy(_.path)
    (session, Plan(ordered, links.toSeq, touches.toSeq))

  private enum Edit:
    case Pure
    case Skip(reason: String)
    case Write(touches: Seq[Touch])

  private def edit(album: Catalog.Album, row: Album.Loaded, matched: MatchSet): Edit =
    val file: Album = row.album
    val google: Option[String] = storedGoogle(album, file)
    val published: Option[String] = storedPublished(album, file, google)
    if (google.toSeq ++ published.toSeq).exists(badControl) then Edit.Skip("title")
    else
      val googleArg: Option[Option[String]] = google match
        case Some(text) if text != file.googleTitle => Some(Some(text))
        case None if file.googleTitle.nonEmpty => Some(None)
        case _ => None
      val includeArg: Option[Seq[Include.Entry]] = includeChange(row.file, file, matched)
      val title: Option[Touch] =
        if googleArg.isDefined || published.isDefined then
          Some(Touch.EditTitles(row.file, googleArg, published))
        else None
      val include: Option[Touch] = includeArg.map(entries => Touch.EditInclude(row.file, entries))
      val fileTouches: Seq[Touch] = title.toSeq ++ include.toSeq
      if fileTouches.isEmpty then Edit.Pure else Edit.Write(fileTouches)

  private def fresh(
    album: Catalog.Album,
    parsed: Title.Parsed,
    matched: MatchSet
  ): Either[String, String] =
    val (googleTitle, owner) = album.owner match
      case Some(label) => (album.title, Some(label))
      case None => ("", None)
    if owner.isDefined && (googleTitle.isEmpty || owner.exists(name => name.trim.isEmpty || badControl(name)) || badControl(googleTitle)) then
      Left("owner")
    else if badControl(googleTitle) then Left("title")
    else
      val written: Album = Album(
        title = parsed.title,
        span = parsed.span,
        include = matched.include,
        external = true,
        googleAlbumId = "",
        publishedTitle = "",
        googleTitle = googleTitle,
        owner = owner
      )
      Right(Album.renderFile(written))

  private def prepare(
    session: Session,
    album: Catalog.Album,
    span: Option[Title.Span],
    monthFlag: Boolean
  ): (Session, Prepared) =
    val (matchedSession, hits, _) = AgentCommands.gather(session, album)
    val claimed: Set[(LocalDate, String)] = hits.map(hit => hit._1 -> hit._3).toSet
    if span.exists(dates => claimed.exists(pair => !dates.contains(pair._1))) then
      (matchedSession, Prepared.Refuse("span"))
    else
      val days: Seq[LocalDate] = span.fold(album.items.flatMap(_.date).distinct.sorted)(AgentCommands.eachDay)
      val (scanned, universe) = AgentCommands.scanDays(matchedSession, days)
      val monthPassed: Boolean =
        monthFlag && span.exists(dates => AgentCommands.monthAllowsAll(dates, claimed, universe))
      val include: Seq[Include.Entry] =
        if claimed.isEmpty then Seq.empty
        else if monthPassed then Seq(Include.Entry.All(Seq.empty))
        else if monthFlag then Include.compress(None, claimed, universe)
        else Include.compress(span, claimed, universe)
      try
        AgentCommands.requireInclude(monthPassed, span, include, claimed, universe)
        (scanned, Prepared.Ready(MatchSet(include, claimed, universe)))
      catch
        case _: IllegalArgumentException =>
          (scanned, Prepared.Refuse("include"))

  // A stored google-title whose span differs stays, including the include. place() is only for an unset key,
  // so a file created with item days shrunk does not look like a rename.
  private def dateSpanMoved(archive: File, album: Catalog.Album, file: Album, rel: String): Boolean =
    if file.googleTitle.nonEmpty then !titleSpan(album.title).equals(titleSpan(file.googleTitle))
    else !titleSpan(album.title).equals(file.span) && !samePlace(archive, album, rel)

  private def samePlace(archive: File, album: Catalog.Album, rel: String): Boolean =
    try
      val (parsed, _) = AgentCommands.place(album)
      Album.relative(archive, Title.file(archive, parsed)) == rel
    catch
      case _: IllegalArgumentException => false

  private def ownerBlocked(album: Catalog.Album, file: Album): Boolean =
    if album.owner.isDefined && (file.googleAlbumId.nonEmpty || !file.external) then true
    else file.owner.isDefined && storedGoogle(album, file).isEmpty && file.googleTitle.nonEmpty

  // None omits the key. A shared album keeps the exact catalog title even when it equals the path title.
  // A client stores it only when the catalog title differs, so publish does not rename the Library album.
  // An empty catalog title is not stored: google-title = "" makes the next scan reject an owner file.
  private def storedGoogle(album: Catalog.Album, file: Album): Option[String] =
    if album.title.isEmpty || !storesTitle(album, file) then None
    else if client(file) && album.title == file.title then None
    else Some(album.title)

  // The catalog title would be written onto this file when it is non-empty.
  private def storesTitle(album: Catalog.Album, file: Album): Boolean =
    (album.owner.isDefined && file.external && file.googleAlbumId.isEmpty) ||
      (client(file) && album.owner.isEmpty)

  private def storedPublished(album: Catalog.Album, file: Album, google: Option[String]): Option[String] =
    if !client(file) || album.title.isEmpty then None
    else
      val desired: String = google.getOrElse(file.title)
      Option.when(desired.nonEmpty && file.publishedTitle != desired)(desired)

  // putInclude writes `include = []` for an empty list, so a missing key that is already empty is left alone.
  // An expansion that already equals the matched set is left alone, even when the spelling differs.
  private def includeChange(file: File, album: Album, matched: MatchSet): Option[Seq[Include.Entry]] =
    val expanded: Set[(LocalDate, String)] = album.include.foldLeft(Set.empty[(LocalDate, String)]): (acc, entry) =>
      acc ++ Include.expand(album.span, entry, matched.universe)
    val sameExpanded: Boolean = expanded.equals(matched.claimed)
    val sameRendered: Boolean = album.include.map(Include.render) == matched.include.map(Include.render)
    if sameExpanded || sameRendered then None
    else if matched.include.isEmpty && !Album.includePresent(Files.read(file)) then None
    else Some(matched.include)

  // True when a toml or the link file changed.
  private def perform(archive: File, plan: Plan): Boolean =
    val wrote: mutable.LinkedHashSet[String] = mutable.LinkedHashSet.empty
    plan.touches.foreach:
      case Touch.CreateFile(dest, text) =>
        AlbumLinks.writeNew(archive, dest, text)
        wrote += Album.relative(archive, dest)
      case Touch.EditTitles(file, googleTitle, publishedTitle) =>
        Album.spliceTitles(file, googleTitle, publishedTitle)
        wrote += Album.relative(archive, file)
      case Touch.EditInclude(file, include) =>
        Album.putInclude(file, include)
        wrote += Album.relative(archive, file)
      case Touch.RemoveFile(file) =>
        wrote += Album.relative(archive, file)
        NFiles.deleteIfExists(file.toPath)
    wrote.foreach(path => Transcript.println(s"wrote $path"))
    val linksChanged: Boolean = AlbumLinks.publish(archive, plan.links)
    if linksChanged then Transcript.println("wrote .gphoto-sync/album-links.tsv")
    wrote.nonEmpty || linksChanged

  private def pair(
    albums: Seq[Catalog.Album],
    files: Seq[Album.Loaded],
    relOf: Album.Loaded => String,
    hit: (Catalog.Album, Album.Loaded) => Boolean
  ): (Map[String, Album.Loaded], Set[String]) =
    val hits: Seq[(Catalog.Album, Album.Loaded)] =
      albums.flatMap(album => files.filter(file => hit(album, file)).map(album -> _))
    val byAlbum: Map[String, Seq[(Catalog.Album, Album.Loaded)]] = hits.groupBy(_._1.id)
    val byFile: Map[String, Seq[(Catalog.Album, Album.Loaded)]] = hits.groupBy((_, file) => relOf(file))
    val crowded: Set[String] = byFile.collect { case (path, rows) if rows.sizeIs > 1 => path }.toSet
    val unique: Map[String, Album.Loaded] = byAlbum.flatMap: (id, rows) =>
      rows match
        case Seq((_, file)) if !crowded.contains(relOf(file)) => Some(id -> file)
        case _ => None
    val ambiguous: Set[String] = byAlbum.keySet.filterNot(unique.contains).toSet
    (unique.toMap, ambiguous)

  private def agrees(album: Catalog.Album, file: Album): Boolean =
    if file.googleTitle.nonEmpty then file.googleTitle == album.title
    else if file.external then true
    else clientHit(album, file)

  private def clientHit(album: Catalog.Album, file: Album): Boolean =
    val key: String = if file.publishedTitle.nonEmpty then file.publishedTitle else file.title
    key == album.title || key == working(album.title)

  private def pending(album: Album): Boolean = !album.external && album.googleAlbumId.isEmpty

  // Empty google-title is not a stored rename. Do not write the catalog title.
  private def pendingRename(album: Catalog.Album, file: Album): Boolean =
    album.owner.isEmpty && file.owner.isEmpty && file.external && file.googleTitle.isEmpty &&
      file.title != album.title

  private def client(album: Album): Boolean = !album.external && album.googleAlbumId.nonEmpty

  private def monthFile(album: Album): Boolean =
    album.span.exists: span =>
      span.grain.equals(Title.Grain.Month) && YearMonth.from(span.start).equals(YearMonth.from(span.end))

  private def hard(album: Catalog.Album): Option[String] =
    if album.autoAdd then Some("auto-add")
    else if album.unsets.nonEmpty || album.error.exists(_.nameFailures > 0) then Some("unread names")
    else None

  private def working(title: String): String = title.split(' ').filter(_.nonEmpty).mkString(" ")

  private def titleSpan(title: String): Option[Title.Span] = Title.parse(working(title)).flatMap(_.span)

  private def placeReason(message: String): String = message match
    case "leftover starts with a digit" => "digit"
    case "leftover is not a name" => "leftover"
    case "curated album has no name" => "no name"
    case "no such directory" => "no such directory"
    case other => other

  private def badControl(text: String): Boolean =
    text.exists(ch => ch == '\t' || ch == '\r' || ch == '\n')

  private def skipped(label: String, path: String, reason: String, id: Option[String]): Action =
    Action(Kind.Skipped, label, path, reason, id)

  private[photo] def actionsValue(actions: Seq[Action]): Json =
    Json.Obj(Seq("actions" -> Json.Arr(actions.map(actionJson))))

  private def jsonOf(plan: Plan): Json =
    Json.Obj(Seq(
      "actions" -> Json.Arr(plan.actions.map(actionJson)),
      "links" -> Json.Arr(plan.links.map(link => Json.Obj(Seq(
        "id" -> Json.Str(link.id),
        "path" -> Json.Str(link.path)
      ))))
    ))

  private def actionJson(action: Action): Json =
    Json.Obj(Seq(
      "class" -> Json.Str(action.kind.wire),
      "title" -> Json.Str(action.label),
      "path" -> Json.Str(action.path),
      "reason" -> (if action.reason.isEmpty then Json.Null else Json.Str(action.reason)),
      "id" -> action.id.fold(Json.Null)(Json.Str.apply)
    ))
