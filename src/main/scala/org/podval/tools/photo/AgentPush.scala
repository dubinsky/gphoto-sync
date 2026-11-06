package org.podval.tools.photo

import java.io.File
import java.nio.charset.StandardCharsets
import java.time.LocalDate
import org.podval.tools.photo.chrome.{AgentChrome, PhotosAct}

private[photo] object AgentPush:

  // claim is not reentrant, and each row has to publish before the next click.
  // One push holds the lock and the browser. The public commands still take both themselves.
  private[photo] def push(session: Session, dryRun: Boolean): (Session, Result) =
    val planned: Either[(Session, Result), (Session, Seq[PushAction])] =
      try Right(planPush(session))
      catch
        case error: IllegalArgumentException =>
          Left((session, Result.Usage(Operation.messageOf(error))))
    planned match
      case Left(done)              => done
      case Right((ready, actions)) =>
        if dryRun || actions.isEmpty then (ready, Result.Ok(pushText(actions), Json.Null))
        else AgentSupport.withLock(ready, actions.head.shownTitle)(runPush(ready, actions))

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
      case _: Remove => AgentChrome.RemoveStep
      case _: Add    => AgentChrome.AddStep
      case _: Upload => AgentChrome.UploadStep

    def shownTitle: String = this match
      case Remove(_, _, _, albumTitle)    => albumTitle
      case Add(_, _, _, _, albumTitle, _) => albumTitle
      case Upload(_, _, albumTitle)       => albumTitle

  private final case class PushStop(
    line: String,
    step: String,
    recovery: String,
    detail: String,
    title: String,
    session: Option[Session] = None,
    picture: Option[String] = None
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
    val chosen: Seq[PushRow] = Archive
      .scan(archive)
      .albums
      .flatMap: loaded =>
        val path: String = Album.relative(archive, loaded.file)
        // No link means there is no album page to open. Owner, auto-add, and unread names are the
        // item-command refusals, so one such album does not fail the others.
        if !loaded.album.external || loaded.album.owner.isDefined then None
        else
          links
            .get(path)
            .flatMap: id =>
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
        val civil: Day = AgentSupport.civilDay(current.root, day)
        val (next, grouped, sidecars) = AgentSupport.loadGrouped(current, civil, readSidecars = true)
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
            case None       => inner
            case Some(pair) =>
              inner.updated(pair, inner.getOrElse(pair, Seq.empty) :+ (album -> item))
    val removes: Seq[PushAction] = pushRemoves(row, facts)
    val adds: Seq[PushAction] = pushAdds(row, desired, hits)
    val uploads: Seq[PushAction] = pushUploads(ready, row, desired, facts, hits)
    (ready, removes ++ adds ++ uploads)

  private def pushRemoves(row: PushRow, facts: Map[LocalDate, PushDay]): Seq[PushAction] =
    row.catalog.items
      .flatMap: item =>
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
    desired.toSeq
      .flatMap: pair =>
        val holders: Seq[(Catalog.Album, Catalog.Item)] = hits.getOrElse(pair, Seq.empty)
        if holders.exists(_._1.id == row.catalog.id) then Seq.empty
        else
          holders
            .map(_._2.key)
            .distinct
            .flatMap: key =>
              holders
                .find((album, item) => item.key == key && album.id != row.catalog.id)
                .map: (album, item) =>
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
    desired.toSeq
      .flatMap: (day, stem) =>
        if hits.contains(day -> stem) then None
        else
          facts
            .get(day)
            .flatMap: fact =>
              val metadata: Metadata = fact.sidecars.getOrElse(stem, Metadata.default)
              // The Library API already recorded this file. A second upload would be another library copy.
              val marked: Boolean =
                metadata.`google-media-item-id`.exists(_.nonEmpty) || metadata.`already-in-google`.contains(true)
              if marked then None
              else
                pushFile(archive, fact.directory, stem).map: relative =>
                  PushAction.Upload(row.catalog.id, relative, row.catalog.title)
      .sortBy:
        case action: PushAction.Upload => utf8Key(action.pathText)
        case _                         => Seq.empty

  private def pushFile(archive: File, directory: File, stem: String): Option[String] =
    val planned: Seq[Pictures.Planned] = Pictures
      .plan(Files.listFiles(directory))
      .filter: row =>
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
      facts
        .get(day)
        .flatMap: fact =>
          val (stem, extension) = Files.splitOnLast(item.filename, '.')
          AgentSupport.resolve(fact.grouped, stem, extension).map(local => day -> local)

  private def pushDays(album: Album): Seq[LocalDate] =
    album.span match
      case Some(span) => AgentSupport.eachDay(span)
      case None       => album.include.flatMap(pushEntryDays).distinct.sorted

  private def pushEntryDays(entry: Include.Entry): Seq[LocalDate] = entry match
    case Include.Entry.All(_)            => Seq.empty
    case Include.Entry.Dates(span, _, _) => AgentSupport.eachDay(span)

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
          case (left @ Left(_), _)              => left
          case (Right((current, done)), action) =>
            applyPush(opened, current, action) match
              case Right((next, line)) => Right(next -> (done :+ line))
              case Left(stop)          => Left(pushFailed(stop.session.getOrElse(current), done, stop))
      outcome match
        case Left(done)             => done
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
          val (album, item) = AgentChrome.rereadTake(session, remove.albumId, remove.key, remove.current)
          AgentChrome.commitTake(ui, session, album, item) match
            case Right(next)   => Right(next -> remove.line)
            case Left(message) => Left(stopped(remove, "refresh-only", message))
        case add: PushAction.Add =>
          val (_, item, destination) =
            AgentChrome.rereadAdd(session, add.sourceId, add.key, add.current, add.destinationId)
          if destination.title == "New album" then
            throw Refresh.ActMiss("New album is not a destination", confirmed = false)
          AgentChrome.commitPut(ui, session, item, destination, add.current) match
            case Right(done)   => Right(done.session -> add.line)
            case Left(message) => Left(stopped(add, "refresh-only", message))
        case upload: PushAction.Upload =>
          val (destination, file, name, day, groupedStem) =
            AgentChrome.rereadUpload(session, upload.pathText, upload.destinationId)
          AgentChrome.commitUpload(ui, session, destination, file, name, day, groupedStem) match
            case Right(done)                                      => Right(done.session -> upload.line)
            case Left(AgentChrome.CommitStop.BeforeSwap(message)) =>
              Left(stopped(upload, "refresh-only", message))
            case Left(AgentChrome.CommitStop.SidecarUnmarked(next, message, picture)) =>
              Left(stopped(upload, "refresh-only", message).copy(session = Some(next), picture = Some(picture)))
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
    val shown: String = AgentChrome.collapsedText(stop.detail, "push failed")
    val catalog: String = if done.nonEmpty || stop.picture.isDefined then "updated" else "unchanged"
    val unmarked: Seq[String] = stop.picture.fold(Seq.empty): picture =>
      Seq("sidecar not marked", s"photo item set already-in-google=true $picture")
    val rows: Seq[String] = done ++ (Seq(
      "failed push",
      stop.line,
      s"step ${stop.step}",
      s"recovery ${stop.recovery}",
      s"catalog $catalog"
    ) ++ unmarked ++ Seq(shown)).filter(_.nonEmpty)
    val text: String = Redaction.redactSecrets(rows.mkString("", "\n", "\n"), stop.title)
    (session, Result.Failed(text, Json.Null, Refresh.MutationFailed))

  private def pushText(actions: Seq[PushAction]): String =
    pushText(actions.map(_.line), actions.headOption.fold("")(_.shownTitle))

  private def pushText(lines: Seq[String], title: String): String =
    Redaction.redactSecrets(AgentSupport.textOf(lines), title)
