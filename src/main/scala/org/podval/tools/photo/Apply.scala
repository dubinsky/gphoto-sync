package org.podval.tools.photo

import java.io.File
import java.time.LocalDate

private[photo] final class AgentApply(perform: Boolean) extends Operation:
  override def execute(session: Session): (Session, Result) =
    try AgentCommands.catching(session)(Apply.execute(session, perform))
    catch case failed: Outcomes.AppendFailed =>
      (session, Result.Usage(failed.getMessage))

private[photo] object Apply:
  enum Kind(val wire: String) derives CanEqual:
    case Satisfied extends Kind("satisfied")
    case Done extends Kind("done")
    case Click extends Kind("click")
    case Skip extends Kind("skip")
    case NeedsAgent extends Kind("needs-agent")

  private enum Pace:
    case Go
    case Stop
    case Busy(result: Result)

  private final case class Shown(row: Requests.Row, kind: Kind, label: String)

  def execute(session: Session, perform: Boolean): (Session, Result) =
    val requests: File = AgentCommands.requestsFile(session)
    val rows: Seq[Requests.Row] = Requests.read(requests)
    val outcomesFile: File = Outcomes.file(requests)
    val outcomes: Map[String, Outcomes.Status] = Outcomes.read(outcomesFile)
    if rows.isEmpty then (session, Result.Ok("", Json.Arr(Seq.empty)))
    else if !perform then
      val (loaded, albums) = session.loadCatalog
      val shown: Seq[Shown] = rows.map: row =>
        val kind: Kind = classify(row, albums, outcomes, loaded)
        Shown(row, kind, label(row, albums, kind))
      (loaded, Result.Ok(AgentCommands.textOf(shown.map(lineOf)), valueOf(shown)))
    else
      val (loaded, _) = session.loadCatalog
      applyAll(loaded, outcomesFile, rows)

  def run(session: Session, row: Requests.Row, albums: Seq[Catalog.Album]): (Session, Result) =
    val previous: Result => Unit = AgentCommands.afterHeld
    AgentCommands.afterHeld = result => record(session, row, result)
    try
      row.action match
        case Requests.Action.RenameAlbum =>
          val album: Catalog.Album = albums.find(_.id == row.albumId).getOrElse(
            throw IllegalArgumentException("no such album")
          )
          AgentCommands.renameAlbum(session, album, row.`new`)
        case Requests.Action.SetDescription =>
          val album: Catalog.Album = AgentCommands.locateItem(albums, row.itemKey)._1
          AgentCommands.setDescription(session, album, row.itemKey, row.current, row.`new`)
        case Requests.Action.SetDate =>
          val album: Catalog.Album = AgentCommands.locateItem(albums, row.itemKey)._1
          AgentCommands.setDate(session, album, row.itemKey, row.current, row.`new`)
        case Requests.Action.DownloadItem =>
          val album: Catalog.Album = albums.find(_.id == row.albumId).getOrElse(
            throw IllegalArgumentException("no such album")
          )
          AgentCommands.downloadItem(session, album, row.itemKey, row.current)
        case Requests.Action.DeleteItem =>
          val album: Catalog.Album = albums.find(_.id == row.albumId).getOrElse(
            throw IllegalArgumentException("no such album")
          )
          AgentCommands.deleteItem(session, albums, album.title, Some(album.id), row.itemKey, row.current)
        case Requests.Action.MoveItem =>
          val source: Catalog.Album = albums.find(_.id == row.albumId).getOrElse(
            throw IllegalArgumentException("no such album")
          )
          val destination: Catalog.Album = albums.find(_.id == row.`new`).getOrElse(
            throw IllegalArgumentException("no such destination")
          )
          AgentCommands.moveItem(
            session,
            albums,
            source.title,
            Some(source.id),
            row.itemKey,
            row.current,
            destination.title,
            Some(destination.id)
          )
    finally AgentCommands.afterHeld = previous

  private def record(session: Session, row: Requests.Row, result: Result): Unit =
    val status: Option[Outcomes.Status] = result match
      case Result.Failed(_, _, message) if message == Refresh.AlreadyRunning =>
        None
      case Result.Failed(_, _, _) =>
        Some(Outcomes.Status.NeedsAgent)
      case Result.Ok(_, _) if row.action == Requests.Action.SetDescription =>
        Some(Outcomes.Status.Done)
      case _ =>
        None
    status.foreach: outcome =>
      Outcomes.append(Outcomes.file(AgentCommands.requestsFile(session)), row.id, outcome)

  private def applyAll(
    session: Session,
    outcomesFile: File,
    rows: Seq[Requests.Row]
  ): (Session, Result) =
    val start: (Session, Seq[Shown], Pace) = (session, Seq.empty, Pace.Go)
    val (done, shown, pace) = rows.foldLeft(start): (acc, row) =>
      acc match
        case (current, lines, Pace.Go) =>
          step(current, outcomesFile, row, lines)
        case stopped =>
          stopped
    pace match
      case Pace.Busy(result) =>
        (done, result)
      case _ =>
        (done, Result.Ok(AgentCommands.textOf(shown.map(lineOf)), valueOf(shown)))

  private def step(
    session: Session,
    outcomesFile: File,
    row: Requests.Row,
    lines: Seq[Shown]
  ): (Session, Seq[Shown], Pace) =
    val albums: Seq[Catalog.Album] = Catalog.read(catalogFile(session))
    val outcomes: Map[String, Outcomes.Status] = Outcomes.read(outcomesFile)
    val kind: Kind = classify(row, albums, outcomes, session)
    if kind != Kind.Click then (session, lines :+ Shown(row, kind, label(row, albums, kind)), Pace.Go)
    else
      try
        val (next, result) = run(session, row, albums)
        result match
          case Result.Ok(_, _) =>
            val shownKind: Kind =
              if row.action == Requests.Action.SetDescription then Kind.Done else Kind.Satisfied
            val fresh: Seq[Catalog.Album] =
              try Catalog.read(catalogFile(next))
              catch case _: IllegalArgumentException => albums
            (next, lines :+ Shown(row, shownKind, label(row, fresh, shownKind)), Pace.Go)
          case Result.Usage(_) =>
            (session, lines :+ Shown(row, Kind.Skip, label(row, albums, Kind.Skip)), Pace.Go)
          case Result.Failed(_, _, message) if message == Refresh.AlreadyRunning =>
            (session, lines, Pace.Busy(result))
          case Result.Failed(_, _, _) =>
            (session, lines :+ Shown(row, Kind.NeedsAgent, label(row, albums, Kind.NeedsAgent)), Pace.Stop)
      catch
        case _: (IllegalArgumentException | IllegalStateException) =>
          (session, lines :+ Shown(row, Kind.Skip, label(row, albums, Kind.Skip)), Pace.Go)

  private def classify(
    row: Requests.Row,
    albums: Seq[Catalog.Album],
    outcomes: Map[String, Outcomes.Status],
    session: Session
  ): Kind =
    outcomes.get(row.id) match
      case Some(Outcomes.Status.NeedsAgent) => Kind.NeedsAgent
      case Some(Outcomes.Status.Done) => Kind.Done
      case None => fromCatalog(row, albums, session)

  private def fromCatalog(row: Requests.Row, albums: Seq[Catalog.Album], session: Session): Kind =
    row.action match
      case Requests.Action.RenameAlbum =>
        albums.find(_.id == row.albumId) match
          case None =>
            Kind.Skip
          case Some(album) if album.title == row.`new` =>
            Kind.Satisfied
          case Some(album) if album.title == row.current && row.`new` != row.current =>
            Kind.Click
          case _ =>
            Kind.Skip
      case Requests.Action.SetDate =>
        dated(row, albums)
      case Requests.Action.SetDescription =>
        described(row, albums)
      case Requests.Action.DownloadItem =>
        download(row, albums, session)
      case Requests.Action.DeleteItem =>
        deleted(row, albums)
      case Requests.Action.MoveItem =>
        moved(row, albums)

  private def dated(row: Requests.Row, albums: Seq[Catalog.Album]): Kind =
    items(albums, row.itemKey) match
      case Seq(item) if dayText(item.date) == row.`new`.take(10) =>
        Kind.Satisfied
      case Seq(item) if row.albumId.isEmpty && item.filename == row.current =>
        Kind.Click
      case _ =>
        Kind.Skip

  private def described(row: Requests.Row, albums: Seq[Catalog.Album]): Kind =
    items(albums, row.itemKey) match
      case Seq(item) if row.albumId.isEmpty && item.filename == row.current =>
        Kind.Click
      case _ =>
        Kind.Skip

  private def download(row: Requests.Row, albums: Seq[Catalog.Album], session: Session): Kind =
    if badFilename(row.current) then Kind.Skip
    else
      albums.find(_.id == row.albumId) match
        case None =>
          Kind.Skip
        case Some(album) =>
          album.items.find(_.key == row.itemKey) match
            case Some(item) if item.filename != row.current =>
              Kind.Skip
            case Some(item) if AgentCommands.stagingFile(session, item).exists =>
              Kind.Satisfied
            case Some(_) =>
              Kind.Click
            case None =>
              Kind.Skip

  private def deleted(row: Requests.Row, albums: Seq[Catalog.Album]): Kind =
    albums.find(_.id == row.albumId) match
      case None =>
        Kind.Skip
      case Some(album) =>
        val unset: Boolean = album.unsets.exists(_.key == row.itemKey)
        album.items.find(_.key == row.itemKey) match
          case None if !unset =>
            Kind.Satisfied
          case Some(item)
            if item.filename == row.current && album.owner.isEmpty && item.uploader.isEmpty =>
            Kind.Click
          case _ =>
            Kind.Skip

  private def moved(row: Requests.Row, albums: Seq[Catalog.Album]): Kind =
    val source: Option[Catalog.Album] = albums.find(_.id == row.albumId)
    val onItem: Boolean = source.exists(_.items.exists(_.key == row.itemKey))
    val onUnset: Boolean = source.exists(_.unsets.exists(_.key == row.itemKey))
    val names: Int = albums.find(_.id == row.`new`).toSeq.flatMap(_.items).count(_.filename == row.current)
    if !onItem && !onUnset && names == 1 then Kind.Satisfied
    else if onItem && names == 0 then Kind.Click
    else Kind.Skip

  private def items(albums: Seq[Catalog.Album], key: String): Seq[Catalog.Item] =
    albums.flatMap(_.items.filter(_.key == key))

  private def dayText(date: Option[LocalDate]): String = date.fold("-")(_.toString)

  private def badFilename(name: String): Boolean =
    name == "." || name == ".." || name.contains('/')

  private def label(row: Requests.Row, albums: Seq[Catalog.Album], kind: Kind): String =
    row.action match
      case Requests.Action.RenameAlbum =>
        albums.find(_.id == row.albumId) match
          case Some(album) => album.title
          case None =>
            if kind == Kind.Satisfied then row.`new` else row.current
      case _ =>
        row.current

  private def lineOf(shown: Shown): String =
    s"${shown.row.id} ${shown.row.action.wire} ${shown.kind.wire} ${Album.quote(shown.label)}"

  private def valueOf(shown: Seq[Shown]): Json =
    Json.Arr(shown.map: row =>
      Json.Obj(Seq(
        "id" -> Json.Str(row.row.id),
        "action" -> Json.Str(row.row.action.wire),
        "status" -> Json.Str(row.kind.wire),
        "label" -> Json.Str(row.label)
      ))
    )

  private def catalogFile(session: Session): File =
    File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
