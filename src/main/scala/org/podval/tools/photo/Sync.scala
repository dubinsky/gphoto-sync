package org.podval.tools.photo

import com.google.photos.library.v1.PhotosLibraryClient
import java.io.File
import java.time.LocalDate
import scala.jdk.CollectionConverters.{IterableHasAsScala, ListHasAsScala, SeqHasAsJava}

final case class Sync(path: Option[String], recursive: Boolean, dryRun: Boolean) extends Operation:
  override def execute(session: Session): (Session, Result) =
    Operation.run(session):
      val parsed: Option[Path] = path.map(session.parse)
      val root: Root = parsed.fold(session.root)(_.root)
      val prefix: Option[String] = parsed.map(_.toString.stripPrefix("/")).filter(_.nonEmpty)
      Sync.run(
        originals = root.configuration.archive,
        prefix = prefix,
        recursive = recursive,
        dryRun = dryRun,
        google = () => Sync.openLibrary()
      )

object Sync:
  private val batchSize: Int = 50

  private[photo] trait Library extends AutoCloseable:
    def listAlbum(albumId: String): Set[String]
    def libraryIds(ids: Seq[String]): Set[String]
    def removeFromAlbum(albumId: String, ids: Seq[String]): Unit
    override def close(): Unit = ()

  private[photo] def openLibrary(): Library = ClientLibrary(Google.newClient())

  private[photo] def run(
    originals: File,
    prefix: Option[String],
    recursive: Boolean,
    dryRun: Boolean,
    google: () => Library
  ): Unit =
    val selectedPrefix: Option[String] = prefix.map(_.stripPrefix("/")).filter(_.nonEmpty)
    if Album.needsRecursive(selectedPrefix) && !recursive then
      throw IllegalArgumentException("year or root requires --recursive")

    val scan: Archive.Scan = Archive.scan(originals)
    // Album files only. Sync does not write sidecar bytes. A dry run leaves the index.
    if !dryRun then DirectoryIndex.dropAll()
    val pictures: Seq[Claims.Resolved] = Claims.resolve(scan)
    val selected: Seq[Claims.Resolved] = select(pictures, selectedPrefix)
    val albums: Seq[LoadedAlbum] = scan.albums
    val byTitle: Map[String, LoadedAlbum] = albums.map(loaded => loaded.album.title -> loaded).toMap
    val facts: Map[String, Archive.Picture] = scan.pictures.map(picture => picture.path -> picture).toMap
    val universe: Map[LocalDate, Set[String]] = Archive.universe(scan.pictures)
    val inScope: Seq[LoadedAlbum] = scope(selected, byTitle, albums)
    if inScope.isEmpty then Transcript.println("no albums")
    else syncAlbums(originals, universe, facts, selected, byTitle, albums, inScope, dryRun, google)

  // Membership is remote, so a dry run still lists and batchGets in order to print the same lines.
  // It does not write album files and does not call batchRemove.
  private def syncAlbums(
    originals: File,
    universe: Map[LocalDate, Set[String]],
    facts: Map[String, Archive.Picture],
    selected: Seq[Claims.Resolved],
    byTitle: Map[String, LoadedAlbum],
    albums: Seq[LoadedAlbum],
    inScope: Seq[LoadedAlbum],
    dryRun: Boolean,
    google: () => Library
  ): Unit =
    val library: Library = google()
    try
      val mediaByTitle: Map[String, Set[String]] = listAll(library, inScope, selected)
      val filed: Seq[(Claims.Resolved, String)] = filedPictures(facts, selected, byTitle, albums)
      val present: Set[String] = lookup(library, filed, byTitle, albums)
      val plan: Plan = planFor(filed, byTitle, albums, mediaByTitle, present)
      printPlan(plan)
      if !dryRun then applyPlan(originals, universe, library, plan)
    finally library.close()

  private def listAll(
    library: Library,
    albums: Seq[LoadedAlbum],
    selected: Seq[Claims.Resolved]
  ): Map[String, Set[String]] =
    albums.foldLeft(Map.empty[String, Set[String]]): (acc, loaded) =>
      val ids: Set[String] =
        try library.listAlbum(loaded.album.googleAlbumId)
        catch
          case error: Exception =>
            fail(albumFailure(loaded, selected, error))
      acc.updated(loaded.album.title, ids)

  private def albumFailure(loaded: LoadedAlbum, selected: Seq[Claims.Resolved], error: Exception): Seq[String] =
    val message: String = Album.messageOf(error)
    val lines: Seq[String] = picturesFor(loaded, selected).map: picture =>
      s"${picture.path} ${loaded.album.title}: $message"
    if lines.nonEmpty then lines else Seq(s"- ${loaded.album.title}: $message")

  private def lookup(
    library: Library,
    filed: Seq[(Claims.Resolved, String)],
    byTitle: Map[String, LoadedAlbum],
    albums: Seq[LoadedAlbum]
  ): Set[String] =
    val ids: Seq[String] = filed.map(_._2).distinct
    ids
      .grouped(batchSize)
      .foldLeft(Set.empty[String]): (acc, batch) =>
        val found: Set[String] =
          try library.libraryIds(batch)
          catch
            case error: Exception =>
              val message: String = Album.messageOf(error)
              val lines: Seq[String] = filed
                .filter(pair => batch.contains(pair._2))
                .flatMap: (picture, _) =>
                  failureLines(picture, byTitle, albums, message)
              fail(if lines.nonEmpty then lines else Seq(s"- -: $message"))
        acc ++ found

  private def failureLines(
    picture: Claims.Resolved,
    byTitle: Map[String, LoadedAlbum],
    albums: Seq[LoadedAlbum],
    message: String
  ): Seq[String] =
    val titles: Seq[String] = (clientClaims(picture, byTitle) ++ excludedAlbums(picture, albums))
      .map(_.album.title)
      .distinct
    if titles.isEmpty then Seq(s"${picture.path} -: $message")
    else titles.map(title => s"${picture.path} $title: $message")

  private def fail(lines: Seq[String]): Nothing =
    lines.foreach(Transcript.println)
    throw IllegalArgumentException(lines.mkString("\n"))

  private def filedPictures(
    facts: Map[String, Archive.Picture],
    selected: Seq[Claims.Resolved],
    byTitle: Map[String, LoadedAlbum],
    albums: Seq[LoadedAlbum]
  ): Seq[(Claims.Resolved, String)] =
    selected.flatMap: picture =>
      facts
        .get(picture.path)
        .flatMap(_.metadata.`google-media-item-id`.filter(_.nonEmpty))
        .flatMap: id =>
          val relevant: Boolean =
            clientClaims(picture, byTitle).nonEmpty || excludedAlbums(picture, albums).nonEmpty
          Option.when(relevant)(picture -> id)

  private def planFor(
    filed: Seq[(Claims.Resolved, String)],
    byTitle: Map[String, LoadedAlbum],
    albums: Seq[LoadedAlbum],
    mediaByTitle: Map[String, Set[String]],
    present: Set[String]
  ): Plan =
    filed.foldLeft(Plan(Seq.empty, Seq.empty)): (plan, pair) =>
      val (picture, id) = pair
      val name: String = pictureName(picture.path)
      if !present.contains(id) then
        val excepts: Seq[ExceptChange] = exceptsFor(clientClaims(picture, byTitle), picture, name)
        plan.copy(excepts = plan.excepts ++ excepts)
      else
        val missing: Seq[LoadedAlbum] = clientClaims(picture, byTitle).filterNot: loaded =>
          mediaByTitle.getOrElse(loaded.album.title, Set.empty).contains(id)
        val stillThere: Seq[LoadedAlbum] = excludedAlbums(picture, albums).filter: loaded =>
          mediaByTitle.getOrElse(loaded.album.title, Set.empty).contains(id)
        plan.copy(
          excepts = plan.excepts ++ exceptsFor(missing, picture, name),
          removes = plan.removes ++ removesFor(stillThere, picture, name, id)
        )

  private def exceptsFor(albums: Seq[LoadedAlbum], picture: Claims.Resolved, name: String): Seq[ExceptChange] =
    val day: LocalDate = Album.splitPath(picture.path)._1
    albums.map(loaded => ExceptChange(loaded.file, loaded.album.title, day, name))

  private def removesFor(
    albums: Seq[LoadedAlbum],
    picture: Claims.Resolved,
    name: String,
    id: String
  ): Seq[RemoveChange] =
    albums.map: loaded =>
      RemoveChange(loaded.album.googleAlbumId, loaded.album.title, name, picture.path, id)

  private def printPlan(plan: Plan): Unit =
    plan.excepts.foreach(change => Transcript.println(s"except ${change.title} ${Album.quote(change.stem)}"))
    plan.removes.foreach(change => Transcript.println(s"remove ${change.title} ${Album.quote(change.stem)}"))

  private def applyPlan(
    originals: File,
    universe: Map[LocalDate, Set[String]],
    library: Library,
    plan: Plan
  ): Unit =
    plan.excepts
      .groupBy(_.file)
      .foreach: (file, changes) =>
        Album.storeRemovals(file, originals, changes.map(change => change.day -> change.stem), universe)
    plan.removes
      .groupBy(_.albumId)
      .foreach: (albumId, changes) =>
        changes
          .map(_.mediaId)
          .distinct
          .grouped(batchSize)
          .foreach: batch =>
            try library.removeFromAlbum(albumId, batch)
            catch
              case error: Exception =>
                val message: String = Album.messageOf(error)
                val lines: Seq[String] = changes
                  .filter(change => batch.contains(change.mediaId))
                  .map: change =>
                    s"${change.path} ${change.title}: $message"
                fail(if lines.nonEmpty then lines else Seq(s"- $albumId: $message"))

  private def picturesFor(loaded: LoadedAlbum, selected: Seq[Claims.Resolved]): Seq[Claims.Resolved] =
    selected.filter: picture =>
      picture.albums.contains(loaded.album.title) || excepts(loaded.album, picture)

  private def scope(
    selected: Seq[Claims.Resolved],
    byTitle: Map[String, LoadedAlbum],
    albums: Seq[LoadedAlbum]
  ): Seq[LoadedAlbum] =
    selected
      .flatMap(picture => clientClaims(picture, byTitle) ++ excludedAlbums(picture, albums))
      .distinctBy(_.album.title)
      .sortBy(_.album.title)

  private def clientClaims(picture: Claims.Resolved, byTitle: Map[String, LoadedAlbum]): Seq[LoadedAlbum] =
    picture.albums.flatMap(byTitle.get).filter(created)

  private def excludedAlbums(picture: Claims.Resolved, albums: Seq[LoadedAlbum]): Seq[LoadedAlbum] =
    albums.filter(loaded => created(loaded) && excepts(loaded.album, picture))

  private def created(loaded: LoadedAlbum): Boolean =
    !loaded.album.external && loaded.album.googleAlbumId.nonEmpty

  private def excepts(album: Album, picture: Claims.Resolved): Boolean =
    val (day, stem) = Album.splitPath(picture.path)
    album.positive(day, stem) && !album.claims(day, stem)

  private def select(pictures: Seq[Claims.Resolved], prefix: Option[String]): Seq[Claims.Resolved] =
    pictures.filter(picture => Album.pictureMatches(picture.path, prefix))

  private type LoadedAlbum = Album.Loaded
  private final case class ExceptChange(file: File, title: String, day: LocalDate, stem: String)
  private final case class RemoveChange(albumId: String, title: String, stem: String, path: String, mediaId: String)
  private final case class Plan(excepts: Seq[ExceptChange], removes: Seq[RemoveChange])

  private def pictureName(path: String): String = Files.splitOnLast(path, '/')._2.getOrElse(path)

  private final class ClientLibrary(client: PhotosLibraryClient) extends Library:
    override def close(): Unit = client.close()

    override def listAlbum(albumId: String): Set[String] =
      client.searchMediaItems(albumId).iterateAll().asScala.map(_.getId).filter(_.nonEmpty).toSet

    // batchGet rejects an empty list and more than 50 ids.
    override def libraryIds(ids: Seq[String]): Set[String] =
      ids.distinct
        .grouped(batchSize)
        .foldLeft(Set.empty[String]): (found, batch) =>
          val results = client.batchGetMediaItems(batch.asJava).getMediaItemResultsList.asScala.toSeq
          found ++ results.flatMap: result =>
            if result.hasMediaItem then
              val id: String = result.getMediaItem.getId
              Option.when(id.nonEmpty)(id)
            else None

    override def removeFromAlbum(albumId: String, ids: Seq[String]): Unit =
      ids.distinct
        .grouped(batchSize)
        .foreach: batch =>
          client.batchRemoveMediaItemsFromAlbum(albumId, batch.asJava)
