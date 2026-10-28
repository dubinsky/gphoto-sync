package org.podval.tools.photo

import com.google.photos.library.v1.PhotosLibraryClient
import java.io.File
import java.time.{LocalDate, ZoneId}
import scala.jdk.CollectionConverters.{IterableHasAsScala, ListHasAsScala, SeqHasAsJava}

final case class Sync(path: Option[Path], recursive: Boolean, dryRun: Boolean) extends Operation:
  override def execute(): Unit =
    val root: Root = path.fold(Root.get)(_.root)
    val prefix: Option[String] = path.map(_.toString.stripPrefix("/")).filter(_.nonEmpty)
    Sync.run(
      originals = root.configuration.byYearDirectory,
      zone = root.configuration.zoneId,
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
    zone: ZoneId,
    prefix: Option[String],
    recursive: Boolean,
    dryRun: Boolean,
    google: () => Library
  ): Unit =
    val selectedPrefix: Option[String] = prefix.map(_.stripPrefix("/")).filter(_.nonEmpty)
    if Album.needsRecursive(selectedPrefix) && !recursive then
      throw IllegalArgumentException("year or root requires --recursive")

    val scan: Archive.Scan = Archive.scan(originals)
    val pictures: Seq[Claims.Resolved] = Claims.resolve(scan, zone) match
      case Left(error) =>
        println(error)
        throw IllegalArgumentException(error)
      case Right(value) => value

    val selected: Seq[Claims.Resolved] = select(pictures, selectedPrefix)
    val albums: Seq[LoadedAlbum] = scan.albums
    val byTitle: Map[String, LoadedAlbum] = albums.map(loaded => loaded.album.title -> loaded).toMap
    val facts: Map[String, Archive.Picture] = scan.pictures.map(picture => picture.path -> picture).toMap
    val inScope: Seq[LoadedAlbum] = scope(selected, byTitle, albums)
    if inScope.isEmpty then println("no albums")
    else syncAlbums(facts, selected, byTitle, albums, inScope, dryRun, google)

  // Membership is remote, so a dry run still lists and batchGets in order to print the same lines.
  // It does not write album files and does not call batchRemove.
  private def syncAlbums(
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
      if !dryRun then applyPlan(library, plan)
    finally
      library.close()

  private def listAll(
    library: Library,
    albums: Seq[LoadedAlbum],
    selected: Seq[Claims.Resolved]
  ): Map[String, Set[String]] =
    albums.foldLeft(Map.empty[String, Set[String]]): (acc, loaded) =>
      val ids: Set[String] =
        try library.listAlbum(loaded.album.googleAlbumId)
        catch case error: Exception =>
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
    ids.grouped(batchSize).foldLeft(Set.empty[String]): (acc, batch) =>
      val found: Set[String] =
        try library.libraryIds(batch)
        catch case error: Exception =>
          val message: String = Album.messageOf(error)
          val lines: Seq[String] = filed.filter(pair => batch.contains(pair._2)).flatMap: (picture, _) =>
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
    lines.foreach(println)
    throw IllegalArgumentException(lines.mkString("\n"))

  private def filedPictures(
    facts: Map[String, Archive.Picture],
    selected: Seq[Claims.Resolved],
    byTitle: Map[String, LoadedAlbum],
    albums: Seq[LoadedAlbum]
  ): Seq[(Claims.Resolved, String)] =
    selected.flatMap: picture =>
      facts.get(picture.path).flatMap(_.metadata.`google-media-item-id`.filter(_.nonEmpty)).flatMap: id =>
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
        val excludes: Seq[ExcludeChange] = excludesFor(clientClaims(picture, byTitle), picture, name)
        plan.copy(excludes = plan.excludes ++ excludes)
      else
        val missing: Seq[LoadedAlbum] = clientClaims(picture, byTitle).filterNot: loaded =>
          mediaByTitle.getOrElse(loaded.album.title, Set.empty).contains(id)
        val stillThere: Seq[LoadedAlbum] = excludedAlbums(picture, albums).filter: loaded =>
          mediaByTitle.getOrElse(loaded.album.title, Set.empty).contains(id)
        plan.copy(
          excludes = plan.excludes ++ excludesFor(missing, picture, name),
          removes = plan.removes ++ removesFor(stillThere, picture, name, id)
        )

  private def excludesFor(albums: Seq[LoadedAlbum], picture: Claims.Resolved, name: String): Seq[ExcludeChange] =
    albums.filterNot(_.album.exclude.contains(name)).map: loaded =>
      val drop: Option[String] = loaded.album match
        case curated: Album.Curated if curated.members.contains(picture.path) => Some(picture.path)
        case _ => None
      ExcludeChange(loaded.file, loaded.album.title, name, drop)

  private def removesFor(
    albums: Seq[LoadedAlbum],
    picture: Claims.Resolved,
    name: String,
    id: String
  ): Seq[RemoveChange] =
    albums.map: loaded =>
      RemoveChange(loaded.album.googleAlbumId, loaded.album.title, name, picture.path, id)

  private def printPlan(plan: Plan): Unit =
    plan.excludes.foreach(change => println(s"exclude ${change.title} ${change.name}"))
    plan.removes.foreach(change => println(s"remove ${change.title} ${change.name}"))

  private def applyPlan(library: Library, plan: Plan): Unit =
    plan.excludes.groupBy(_.file).foreach: (file, changes) =>
      Album.storeRemovals(file, changes.map(_.name), changes.flatMap(_.dropMember))
    plan.removes.groupBy(_.albumId).foreach: (albumId, changes) =>
      changes.map(_.mediaId).distinct.grouped(batchSize).foreach: batch =>
        try library.removeFromAlbum(albumId, batch)
        catch case error: Exception =>
          val message: String = Album.messageOf(error)
          val lines: Seq[String] = changes.filter(change => batch.contains(change.mediaId)).map: change =>
            s"${change.path} ${change.title}: $message"
          fail(if lines.nonEmpty then lines else Seq(s"- $albumId: $message"))

  private def picturesFor(loaded: LoadedAlbum, selected: Seq[Claims.Resolved]): Seq[Claims.Resolved] =
    selected.filter: picture =>
      picture.dated.contains(loaded.album.title) ||
        picture.curated.contains(loaded.album.title) ||
        nameExcluded(loaded.album, picture.path)

  private def scope(
    selected: Seq[Claims.Resolved],
    byTitle: Map[String, LoadedAlbum],
    albums: Seq[LoadedAlbum]
  ): Seq[LoadedAlbum] =
    selected.flatMap(picture => clientClaims(picture, byTitle) ++ excludedAlbums(picture, albums))
      .distinctBy(_.album.title)
      .sortBy(_.album.title)

  private def clientClaims(picture: Claims.Resolved, byTitle: Map[String, LoadedAlbum]): Seq[LoadedAlbum] =
    (picture.dated.toSeq ++ picture.curated).flatMap(byTitle.get).filter(created)

  private def excludedAlbums(picture: Claims.Resolved, albums: Seq[LoadedAlbum]): Seq[LoadedAlbum] =
    albums.filter(loaded => created(loaded) && nameExcluded(loaded.album, picture.path))

  private def created(loaded: LoadedAlbum): Boolean =
    !loaded.album.external && loaded.album.googleAlbumId.nonEmpty

  private def nameExcluded(album: Album, picturePath: String): Boolean =
    album.exclude.contains(pictureName(picturePath)) && covers(album, picturePath)

  private def covers(album: Album, picturePath: String): Boolean = album match
    case event: Album.Event =>
      folderDay(picturePath).exists(day => Claims.folderDayInInterval(event, day))
    case day: Album.Day => picturePath.startsWith(s"${day.title}/")
    case month: Album.Month =>
      Album.monthYearMonth(month.title).exists: key =>
        picturePath.startsWith(s"${Album.monthPrefix(key)}/")
    case curated: Album.Curated => !curated.members.contains(picturePath)

  private def folderDay(picturePath: String): Option[LocalDate] =
    picturePath.split('/').toSeq.filter(_.nonEmpty) match
      case Seq(year, month, day, _*) if numbered(year, 4) && numbered(month, 2) && numbered(day, 2) =>
        Some(LocalDate.of(year.toInt, month.toInt, day.toInt))
      case _ => None

  private def numbered(name: String, length: Int): Boolean =
    name.length == length && name.forall(_.isDigit)

  private def select(pictures: Seq[Claims.Resolved], prefix: Option[String]): Seq[Claims.Resolved] =
    pictures.filter(picture => Album.matchesPrefix(picture.path, prefix))

  private type LoadedAlbum = Album.Loaded
  private final case class ExcludeChange(
    file: File,
    title: String,
    name: String,
    dropMember: Option[String]
  )
  private final case class RemoveChange(albumId: String, title: String, name: String, path: String, mediaId: String)
  private final case class Plan(excludes: Seq[ExcludeChange], removes: Seq[RemoveChange])

  private def pictureName(path: String): String = Files.splitOnLast(path, '/')._2.getOrElse(path)

  private final class ClientLibrary(client: PhotosLibraryClient) extends Library:
    override def close(): Unit = client.close()

    override def listAlbum(albumId: String): Set[String] =
      client.searchMediaItems(albumId).iterateAll().asScala.map(_.getId).filter(_.nonEmpty).toSet

    // batchGet rejects an empty list and more than 50 ids.
    override def libraryIds(ids: Seq[String]): Set[String] =
      ids.distinct.grouped(batchSize).foldLeft(Set.empty[String]): (found, batch) =>
        val results = client.batchGetMediaItems(batch.asJava).getMediaItemResultsList.asScala.toSeq
        found ++ results.flatMap: result =>
          if result.hasMediaItem then
            val id: String = result.getMediaItem.getId
            Option.when(id.nonEmpty)(id)
          else None

    override def removeFromAlbum(albumId: String, ids: Seq[String]): Unit =
      ids.distinct.grouped(batchSize).foreach: batch =>
        client.batchRemoveMediaItemsFromAlbum(albumId, batch.asJava)
