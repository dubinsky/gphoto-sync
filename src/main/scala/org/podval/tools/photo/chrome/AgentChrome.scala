package org.podval.tools.photo.chrome

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles}
import java.time.{DateTimeException, LocalDate}
import org.podval.tools.photo.{
  AgentSupport,
  Album,
  Catalog,
  Files,
  Json,
  Operation,
  Pictures,
  Publish,
  Refresh,
  RefreshPlan,
  Result,
  Session,
  Title
}

private[photo] final class AgentRename(
    title: Option[String],
    current: Option[String],
    updated: Option[String],
    id: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val wantedTitle: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val wantedNew: String = updated.getOrElse(throw IllegalArgumentException("missing new"))
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        val album: Catalog.Album = AgentSupport.select(albums, wantedTitle, id)
        AgentChrome.acceptRequest(
          albums,
          AgentChrome.Action.RenameAlbum,
          album.id,
          "",
          wantedCurrent,
          wantedNew
        )
        AgentChrome.renameAlbum(loaded, album, wantedNew)

private[photo] final class AgentSetDescription(
    item: Option[String],
    current: Option[String],
    updated: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val text: String = updated.getOrElse(throw IllegalArgumentException("missing new"))
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        AgentChrome.acceptRequest(albums, AgentChrome.Action.SetDescription, "", key, wantedCurrent, text)
        if text.exists(ch => ch == '\t' || ch == '\n' || ch == '\r') then
          throw IllegalArgumentException("new contains a tab or a newline")
        val album: Catalog.Album = AgentChrome.locateItem(albums, key)._1
        AgentChrome.setDescription(loaded, album, key, wantedCurrent, text)

private[photo] final class AgentSetDate(
    item: Option[String],
    current: Option[String],
    updated: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val local: String = updated.getOrElse(throw IllegalArgumentException("missing new"))
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        AgentChrome.acceptRequest(albums, AgentChrome.Action.SetDate, "", key, wantedCurrent, local)
        val album: Catalog.Album = AgentChrome.locateItem(albums, key)._1
        AgentChrome.setDate(loaded, album, key, wantedCurrent, local)

private[photo] final class AgentDownload(
    title: Option[String],
    item: Option[String],
    current: Option[String],
    id: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val wantedTitle: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        val album: Catalog.Album = AgentSupport.select(albums, wantedTitle, id)
        AgentChrome.acceptRequest(albums, AgentChrome.Action.DownloadItem, album.id, key, wantedCurrent, "")
        AgentChrome.downloadItem(loaded, album, key, wantedCurrent)

private[photo] final class AgentAdd(
    title: Option[String],
    item: Option[String],
    current: Option[String],
    dest: Option[String],
    id: Option[String],
    destId: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val wantedTitle: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val destTitle: String = dest.getOrElse(throw IllegalArgumentException("missing dest"))
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        AgentChrome.putInAlbum(loaded, albums, wantedTitle, id, key, wantedCurrent, destTitle, destId)

private[photo] final class AgentRemove(
    title: Option[String],
    item: Option[String],
    current: Option[String],
    id: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val wantedTitle: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        AgentChrome.takeOut(loaded, albums, wantedTitle, id, key, wantedCurrent)

private[photo] final class AgentMove(
    title: Option[String],
    item: Option[String],
    current: Option[String],
    dest: Option[String],
    id: Option[String],
    destId: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val wantedTitle: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val destTitle: String = dest.getOrElse(throw IllegalArgumentException("missing dest"))
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        AgentChrome.moveItem(loaded, albums, wantedTitle, id, key, wantedCurrent, destTitle, destId)

private[photo] final class AgentDelete(
    title: Option[String],
    item: Option[String],
    current: Option[String],
    id: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val wantedTitle: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val key: String = item.getOrElse(throw IllegalArgumentException("missing item"))
      val wantedCurrent: String = current.getOrElse(throw IllegalArgumentException("missing current"))
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        AgentChrome.deleteItem(loaded, albums, wantedTitle, id, key, wantedCurrent)

private[photo] final class AgentUpload(
    path: Option[String],
    dest: Option[String],
    destId: Option[String]
) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val wantedPath: String = path.getOrElse(throw IllegalArgumentException("missing path"))
      val destTitle: String = dest.getOrElse(throw IllegalArgumentException("missing dest"))
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        AgentChrome.uploadItem(loaded, albums, wantedPath, destTitle, destId)

private[photo] final class AgentCreate(title: Option[String]) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val wanted: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        AgentChrome.createAlbum(loaded, albums, wanted)

private[photo] final class AgentDeleteAlbum(title: Option[String], id: Option[String]) extends Operation:
  override def execute(session: Session): (Session, Result) =
    AgentSupport.catching(session):
      val wanted: String = title.getOrElse(throw IllegalArgumentException("missing title"))
      val (loaded, albums) = session.loadCatalog
      AgentSupport.catching(loaded):
        AgentChrome.deleteAlbum(loaded, albums, wanted, id)

private[photo] object AgentChrome:
  enum Action derives CanEqual:
    case RenameAlbum, SetDescription, SetDate, DownloadItem, DeleteItem

  private val RenameStep: String = "More options, then Edit album, Rename album, or Rename"
  private val DescriptionStep: String = "Open info, or the i key, then Add a description"
  private val DateStep: String = "Date taken, then Save, Done, or OK"
  private val DownloadStep: String = "More options, then Download"
  private[photo] val AddStep: String = "More options, then Add to album, then the existing album"
  private[photo] val RemoveStep: String = "More options, then Remove from album, then Remove"
  private val TrashStep: String = "Move to trash"
  private[photo] val UploadStep: String = "Add photos or Select photos, then Select from computer"
  private val CreateStep: String = "Create album, then Edit album name, Done"
  private val DeleteAlbumStep: String = "More options, then Delete album, then Delete"

  def renameAlbum(session: Session, album: Catalog.Album, updated: String): (Session, Result) =
    AgentSupport.withLock(session, album.title)(heldRename(session, album, updated))

  def setDescription(
      session: Session,
      album: Catalog.Album,
      key: String,
      current: String,
      text: String
  ): (Session, Result) =
    AgentSupport.withLock(session, album.title)(heldDescription(session, key, current, text))

  def setDate(
      session: Session,
      album: Catalog.Album,
      key: String,
      current: String,
      local: String
  ): (Session, Result) =
    AgentSupport.withLock(session, album.title)(heldDate(session, key, current, local))

  def downloadItem(
      session: Session,
      album: Catalog.Album,
      key: String,
      current: String
  ): (Session, Result) =
    val item: Catalog.Item = album.items
      .find(_.key == key)
      .getOrElse(
        throw IllegalArgumentException("no such item")
      )
    if stagingFile(session, item).exists then itemOk(session, album, item, "download-item", "unchanged")
    else AgentSupport.withLock(session, album.title)(heldDownload(session, album.id, key, current))

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
    val album: Catalog.Album = AgentSupport.select(albums, title, id)
    val destination: Catalog.Album = AgentSupport.select(albums, destTitle, destId)
    val item: Catalog.Item = album.items
      .find(_.key == key)
      .getOrElse(
        throw IllegalArgumentException("no such item")
      )
    requireCurrent(item.filename, current)
    if album.id == destination.id then throw IllegalArgumentException("destination is the source album")
    acceptPut(session, destination, item)
    AgentSupport.withLock(session, album.title)(heldPut(session, album.id, key, current, destination.id))

  def takeOut(
      session: Session,
      albums: Seq[Catalog.Album],
      title: String,
      id: Option[String],
      key: String,
      current: String
  ): (Session, Result) =
    val album: Catalog.Album = AgentSupport.select(albums, title, id)
    val item: Catalog.Item = album.items
      .find(_.key == key)
      .getOrElse(
        throw IllegalArgumentException("no such item")
      )
    requireCurrent(item.filename, current)
    AgentSupport.withLock(session, album.title)(heldTake(session, album.id, key, current))

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
    val album: Catalog.Album = AgentSupport.select(albums, title, id)
    val destination: Catalog.Album = AgentSupport.select(albums, destTitle, destId)
    val item: Catalog.Item = album.items
      .find(_.key == key)
      .getOrElse(
        throw IllegalArgumentException("no such item")
      )
    requireCurrent(item.filename, current)
    if album.id == destination.id then throw IllegalArgumentException("destination is the source album")
    acceptPut(session, destination, item)
    AgentSupport.withLock(session, album.title)(heldMove(session, album.id, key, current, destination.id))

  def deleteItem(
      session: Session,
      albums: Seq[Catalog.Album],
      title: String,
      id: Option[String],
      key: String,
      current: String
  ): (Session, Result) =
    val album: Catalog.Album = AgentSupport.select(albums, title, id)
    val item: Catalog.Item = album.items
      .find(_.key == key)
      .getOrElse(
        throw IllegalArgumentException("no such item")
      )
    requireCurrent(item.filename, current)
    acceptDelete(album, item)
    AgentSupport.withLock(session, album.title)(heldDelete(session, album.id, key, current))

  def uploadItem(
      session: Session,
      albums: Seq[Catalog.Album],
      pathText: String,
      destTitle: String,
      destId: Option[String]
  ): (Session, Result) =
    val (_, relative) = uploadFile(session, pathText)
    val (day, stem, _, extension) = uploadName(relative)
    val destination: Catalog.Album = AgentSupport.select(albums, destTitle, destId)
    val loaded: Session = refuseUpload(session, albums, destination, day, stem, extension)
    AgentSupport.withLock(loaded, destination.title)(heldUpload(loaded, pathText, destination.id))

  def createAlbum(session: Session, albums: Seq[Catalog.Album], title: String): (Session, Result) =
    acceptCreate(title, albums)
    AgentSupport.withLock(session, title)(heldCreate(session, title))

  def deleteAlbum(
      session: Session,
      albums: Seq[Catalog.Album],
      title: String,
      id: Option[String]
  ): (Session, Result) =
    val album: Catalog.Album = AgentSupport.select(albums, title, id)
    acceptAlbumDelete(album)
    AgentSupport.withLock(session, album.title)(heldDeleteAlbum(session, album.id))

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
      case Left(done)   => done
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
            case Left(done)    => done
            case Right(opened) =>
              ui = opened
              try
                ui.renameAlbum(fresh, updated)
                publishTitle(session, fresh, updated) match
                  case Left(detail) => renameFailed(session, fresh, "refresh-only", detail)
                  case Right(next)  => renameOk(next, fresh, updated)
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
    val fresh: Catalog.Album = albums
      .find(_.id == album.id)
      .getOrElse(
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
              case None        => Left("missing album block")
          else Right(acc :+ RefreshPlan.copiedLines(next, blocks))
    groups
      .flatMap: lines =>
        try
          Files.write(RefreshPlan.renderLines(lines), partial)
          Right(())
        catch
          case error: Exception =>
            Left(Operation.messageOf(error))
      .flatMap(_ => RefreshPlan.checkRetitle(partial))
      .flatMap(_ => RefreshPlan.swap(live, partial, previous))
      .map(_ => session.copy(catalogCache = None))

  private def renameOk(session: Session, album: Catalog.Album, updated: String): (Session, Result) =
    val text: String = PlaywrightPhotosUi.redactSecrets(s"rename-album ${Album.quote(updated)}\n", updated)
    val value: Json = Json.Obj(
      Seq(
        "action" -> Json.Str("rename-album"),
        "title" -> Json.Str(updated),
        "catalog" -> Json.Str("updated"),
        "albumId" -> Json.Str(album.id),
        "itemKey" -> Json.Str("")
      )
    )
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
    val value: Json = Json.Obj(
      Seq(
        "action" -> Json.Str("rename-album"),
        "title" -> Json.Str(album.title),
        "step" -> Json.Str(RenameStep),
        "recovery" -> Json.Str(recovery),
        "catalog" -> Json.Str("unchanged"),
        "albumId" -> Json.Str(album.id),
        "itemKey" -> Json.Str("")
      )
    )
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
    val album: Catalog.Album = albums
      .find(_.id == albumId)
      .getOrElse(
        throw IllegalArgumentException("no such album")
      )
    acceptRequest(albums, Action.DownloadItem, album.id, key, current, "")
    val item: Catalog.Item = album.items
      .find(_.key == key)
      .getOrElse(
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
                  if found.key == item.key then found.copy(date = Some(day)) else found)
                Right(acc :+ RefreshPlan.recordLines(revised))
            else Right(acc :+ RefreshPlan.copiedLines(next, blocks))
      groups
        .flatMap: lines =>
          try
            Files.write(RefreshPlan.renderLines(lines), partial)
            Right(())
          catch
            case error: Exception =>
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
        publishAlbum(
          session,
          Seq(
            destination.id -> (dest => revisedAdd(dest, line)),
            source.id -> (next => revisedTake(next, item))
          )
        ) match
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
    val album: Catalog.Album = Catalog
      .read(catalog)
      .find(_.id == albumId)
      .getOrElse(
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
              case (Left(message), _)  => Left(message)
              case (Right(acc), album) =>
                blocks.get(album.id) match
                  case Some(lines) => Right(acc :+ lines)
                  case None        => Left("missing album block")
          groups
            .flatMap: lines =>
              try
                Files.write(RefreshPlan.renderLines(lines), partial)
                Right(())
              catch
                case error: Exception =>
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
    RefreshPlan
      .checkRetitle(partial)
      .flatMap: _ =>
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
    else
      albums.filter(_.title == title) match
        case Seq()  => ()
        case Seq(_) => throw IllegalArgumentException(s"album titled ${Album.quote(title)} already exists")
        case _      => throw IllegalArgumentException(s"more than one album titled ${Album.quote(title)}")

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

  private[photo] final case class Committed(session: Session, key: String)

  private[photo] def commitPut(
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

  private[photo] def commitTake(
      ui: PhotosAct,
      session: Session,
      album: Catalog.Album,
      item: Catalog.Item
  ): Either[String, Session] =
    ui.takeOut(album, item)
    publishAlbum(session, Seq(album.id -> (next => revisedTake(next, item))))

  private[photo] def commitUpload(
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

  private[photo] def rereadAdd(
      session: Session,
      sourceId: String,
      key: String,
      current: String,
      destinationId: String
  ): (Catalog.Album, Catalog.Item, Catalog.Album) =
    val file: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    val albums: Seq[Catalog.Album] = Catalog.read(file)
    val source: Catalog.Album = albums
      .find(_.id == sourceId)
      .getOrElse(
        throw IllegalArgumentException("no such album")
      )
    val destination: Catalog.Album = albums
      .find(_.id == destinationId)
      .getOrElse(
        throw IllegalArgumentException("no such album")
      )
    val item: Catalog.Item = source.items
      .find(_.key == key)
      .getOrElse(
        throw IllegalArgumentException("no such item")
      )
    requireCurrent(item.filename, current)
    if source.id == destination.id then throw IllegalArgumentException("destination is the source album")
    acceptPut(session, destination, item)
    (source, item, destination)

  private[photo] def rereadTake(
      session: Session,
      albumId: String,
      key: String,
      current: String
  ): (Catalog.Album, Catalog.Item) =
    val file: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    val albums: Seq[Catalog.Album] = Catalog.read(file)
    val album: Catalog.Album = albums
      .find(_.id == albumId)
      .getOrElse(
        throw IllegalArgumentException("no such album")
      )
    val item: Catalog.Item = album.items
      .find(_.key == key)
      .getOrElse(
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
    val album: Catalog.Album = albums
      .find(_.id == albumId)
      .getOrElse(
        throw IllegalArgumentException("no such album")
      )
    val item: Catalog.Item = album.items
      .find(_.key == key)
      .getOrElse(
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
        groups
          .flatMap: lines =>
            try
              Files.write(RefreshPlan.renderLines(lines), partial)
              Right(())
            catch
              case error: Exception =>
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

  private[photo] def rereadUpload(
      session: Session,
      pathText: String,
      destinationId: String
  ): (Catalog.Album, File, String) =
    val catalog: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    val albums: Seq[Catalog.Album] = Catalog.read(catalog)
    val destination: Catalog.Album = albums
      .find(_.id == destinationId)
      .getOrElse(
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
    val (loaded, grouped, sidecars) =
      AgentSupport.loadGrouped(session, AgentSupport.civilDay(session.root, day), readSidecars = true)
    val groupedMap: Map[String, Set[String]] = grouped.toMap
    // dropOnePrefix of VID_1000.mp4 is 1000, the still beside it. The grouped stem is 1000v.
    val groupedStem: String = AgentSupport.resolve(groupedMap, stem, extension).getOrElse(stem)
    val matched: Boolean = albums.exists: album =>
      album.items.exists: item =>
        item.date.contains(day) && {
          val (itemStem, itemExt) = Files.splitOnLast(item.filename, '.')
          AgentSupport.resolve(groupedMap, itemStem, itemExt).contains(groupedStem)
        }
    if matched then throw IllegalArgumentException("catalog item exists")
    sidecars
      .get(groupedStem)
      .foreach: metadata =>
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
      try AgentSupport.picturePath(relative)
      catch
        case _: IllegalArgumentException =>
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
      val (_, grouped, _) = AgentSupport.loadGrouped(session, AgentSupport.civilDay(session.root, date))
      AgentSupport.resolve(grouped.toMap, stem, extension).map(local => (date, local))

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
    val value: Json = Json.Obj(
      Seq(
        "action" -> Json.Str(action),
        "title" -> Json.Str(destination.title),
        "catalog" -> Json.Str("updated"),
        "albumId" -> Json.Str(destination.id),
        "itemKey" -> Json.Str(key)
      )
    )
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
    val value: Json = Json.Obj(
      Seq(
        "action" -> Json.Str("create-album"),
        "title" -> Json.Str(title),
        "catalog" -> Json.Str("updated"),
        "albumId" -> Json.Str(id),
        "itemKey" -> Json.Str("")
      )
    )
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
    val value: Json = Json.Obj(
      Seq(
        "action" -> Json.Str("create-album"),
        "title" -> Json.Str(title),
        "step" -> Json.Str(CreateStep),
        "recovery" -> Json.Str(recovery),
        "catalog" -> Json.Str("unchanged"),
        "albumId" -> Json.Str(""),
        "itemKey" -> Json.Str("")
      )
    )
    (session, Result.Failed(PlaywrightPhotosUi.redactSecrets(text, title), value, Refresh.MutationFailed))

  private def deleteAlbumOk(session: Session, album: Catalog.Album): (Session, Result) =
    val text: String = PlaywrightPhotosUi.redactSecrets(s"delete-album ${Album.quote(album.title)}\n", album.title)
    val value: Json = Json.Obj(
      Seq(
        "action" -> Json.Str("delete-album"),
        "title" -> Json.Str(album.title),
        "catalog" -> Json.Str("updated"),
        "albumId" -> Json.Str(album.id),
        "itemKey" -> Json.Str("")
      )
    )
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
    val value: Json = Json.Obj(
      Seq(
        "action" -> Json.Str("delete-album"),
        "title" -> Json.Str(album.title),
        "step" -> Json.Str(DeleteAlbumStep),
        "recovery" -> Json.Str(recovery),
        "catalog" -> Json.Str("unchanged"),
        "albumId" -> Json.Str(""),
        "itemKey" -> Json.Str("")
      )
    )
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
    val value: Json = Json.Obj(
      Seq(
        "action" -> Json.Str("move-item"),
        "title" -> Json.Str(source.title),
        "destination" -> Json.Str(destination.title),
        "catalog" -> Json.Str("updated"),
        "albumId" -> Json.Str(source.id),
        "destinationId" -> Json.Str(destination.id),
        "itemKey" -> Json.Str(key)
      )
    )
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
    val value: Json = Json.Obj(
      Seq(
        "action" -> Json.Str("move-item"),
        "title" -> Json.Str(source.title),
        "destination" -> Json.Str(destination.title),
        "step" -> Json.Str(step),
        "recovery" -> Json.Str(recovery),
        "catalog" -> Json.Str("unchanged"),
        "albumId" -> Json.Str(source.id),
        "destinationId" -> Json.Str(destination.id),
        "itemKey" -> Json.Str(item.key)
      )
    )
    (session, Result.Failed(PlaywrightPhotosUi.redactSecrets(text, source.title), value, Refresh.MutationFailed))

  private def itemOk(
      session: Session,
      album: Catalog.Album,
      item: Catalog.Item,
      action: String,
      catalog: String
  ): (Session, Result) =
    val text: String = PlaywrightPhotosUi.redactSecrets(s"$action ${Album.quote(item.filename)}\n", album.title)
    val value: Json = Json.Obj(
      Seq(
        "action" -> Json.Str(action),
        "title" -> Json.Str(album.title),
        "catalog" -> Json.Str(catalog),
        "albumId" -> Json.Str(album.id),
        "itemKey" -> Json.Str(item.key)
      )
    )
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
    val value: Json = Json.Obj(
      Seq(
        "action" -> Json.Str(action),
        "title" -> Json.Str(album.title),
        "step" -> Json.Str(step),
        "recovery" -> Json.Str(recovery),
        "catalog" -> Json.Str("unchanged"),
        "albumId" -> Json.Str(album.id),
        "itemKey" -> Json.Str(item.key)
      )
    )
    (session, Result.Failed(PlaywrightPhotosUi.redactSecrets(text, album.title), value, Refresh.MutationFailed))

  private[photo] def collapsedText(detail: String, fallback: String): String =
    val message: String = detail.replace('\t', ' ').replace('\r', ' ').replace('\n', ' ')
    if message.isEmpty then fallback else message

  def acceptRequest(
      albums: Seq[Catalog.Album],
      action: Action,
      albumId: String,
      itemKey: String,
      current: String,
      updated: String
  ): Unit = action match
    case Action.RenameAlbum =>
      val album: Catalog.Album = albums
        .find(_.id == albumId)
        .getOrElse(
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
      val album: Catalog.Album = albums
        .find(_.id == albumId)
        .getOrElse(
          throw IllegalArgumentException("no such album")
        )
      val item: Catalog.Item = album.items
        .find(_.key == itemKey)
        .getOrElse(
          throw IllegalArgumentException("no such item")
        )
      requireCurrent(item.filename, current)
      if updated.nonEmpty then throw IllegalArgumentException("new is not empty")
      requireFilename(item.filename)
    case Action.DeleteItem =>
      val album: Catalog.Album = albums
        .find(_.id == albumId)
        .getOrElse(
          throw IllegalArgumentException("no such album")
        )
      val item: Catalog.Item = album.items
        .find(_.key == itemKey)
        .getOrElse(
          throw IllegalArgumentException("no such item")
        )
      requireCurrent(item.filename, current)
      if updated.nonEmpty then throw IllegalArgumentException("new is not empty")
      if album.owner.isDefined then throw IllegalArgumentException("album has an owner")
      if item.uploader.isDefined then throw IllegalArgumentException("item has an uploader")

  private[photo] def locateItem(albums: Seq[Catalog.Album], key: String): (Catalog.Album, Catalog.Item) =
    albums.flatMap(album => album.items.filter(_.key == key).map(album -> _)) match
      case Seq(found) => found
      case Seq()      => throw IllegalArgumentException("no such item")
      case _          => throw IllegalArgumentException("item key is not unique")

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
    if name == "." || name == ".." || name.contains('/') then throw IllegalArgumentException("filename is not valid")
