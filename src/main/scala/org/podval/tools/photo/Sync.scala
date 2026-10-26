package org.podval.tools.photo

import com.google.api.gax.core.FixedCredentialsProvider
import com.google.photos.library.v1.{PhotosLibraryClient, PhotosLibrarySettings}
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

  private[photo] def openLibrary(): Library =
    val credentials = GoogleAuth.loadUserCredentials()
    val settings = PhotosLibrarySettings.newBuilder()
      .setCredentialsProvider(FixedCredentialsProvider.create(credentials))
      .build()
    ClientLibrary(PhotosLibraryClient.initialize(settings))

  private[photo] def run(
    originals: File,
    zone: ZoneId,
    prefix: Option[String],
    recursive: Boolean,
    dryRun: Boolean,
    google: () => Library
  ): Unit =
    val selectedPrefix: Option[String] = prefix.map(_.stripPrefix("/")).filter(_.nonEmpty)
    if needsRecursive(selectedPrefix) && !recursive then
      throw IllegalArgumentException("year or root requires --recursive")

    val pictures: Seq[Claims.Resolved] = Claims(originals, zone) match
      case Left(error) =>
        println(error)
        throw IllegalArgumentException(error)
      case Right(value) => value

    val selected: Seq[Claims.Resolved] = select(pictures, selectedPrefix)
    val albums: Seq[LoadedAlbum] = loadAlbums(originals)
    val byTitle: Map[String, LoadedAlbum] = albums.map(loaded => loaded.album.title -> loaded).toMap
    val inScope: Seq[LoadedAlbum] = scope(selected, byTitle, albums)
    if inScope.isEmpty then println("no albums")
    else syncAlbums(originals, selected, byTitle, albums, inScope, dryRun, google)

  // Membership is remote, so a dry run still lists and batchGets in order to print the same lines.
  // It does not write album files and does not call batchRemove.
  private def syncAlbums(
    originals: File,
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
      val filed: Seq[(Claims.Resolved, String)] = filedPictures(originals, selected, byTitle, albums)
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
    val message: String = messageOf(error)
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
          val message: String = messageOf(error)
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
    originals: File,
    selected: Seq[Claims.Resolved],
    byTitle: Map[String, LoadedAlbum],
    albums: Seq[LoadedAlbum]
  ): Seq[(Claims.Resolved, String)] =
    selected.flatMap: picture =>
      mediaId(originals, picture.path).flatMap: id =>
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
      ExcludeChange(loaded.file, loaded.album.title, name, picture.path, drop)

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
          val message: String = messageOf(error)
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
    case event: Album.Event => picturePath.startsWith(s"${dayOf(event)}/")
    case day: Album.Day => picturePath.startsWith(s"${day.title}/")
    case month: Album.Month => picturePath.startsWith(s"${month.title}/")
    case _: Album.Curated => true

  private def dayOf(event: Album.Event): String =
    val date: LocalDate = event.pathDay
    f"${date.getYear}%04d/${date.getMonthValue}%02d/${date.getDayOfMonth}%02d"

  private def needsRecursive(prefix: Option[String]): Boolean =
    val segments: Seq[String] = prefix.filter(_.nonEmpty).toSeq.flatMap(_.split('/').filter(_.nonEmpty))
    segments.isEmpty || (segments.sizeIs == 1 && isNumber(segments.head, 4))

  private def select(pictures: Seq[Claims.Resolved], prefix: Option[String]): Seq[Claims.Resolved] =
    prefix.filter(_.nonEmpty) match
      case None => pictures
      case Some(path) =>
        pictures.filter(picture => picture.path == path || picture.path.startsWith(s"$path/"))

  private final case class LoadedAlbum(file: File, album: Album)
  private final case class ExcludeChange(
    file: File,
    title: String,
    name: String,
    path: String,
    dropMember: Option[String]
  )
  private final case class RemoveChange(albumId: String, title: String, name: String, path: String, mediaId: String)
  private final case class Plan(excludes: Seq[ExcludeChange], removes: Seq[RemoveChange])

  private def loadAlbums(originals: File): Seq[LoadedAlbum] =
    val curated: Seq[LoadedAlbum] = tomlFiles(originals).map: file =>
      val (name, _) = Files.nameAndExtension(file)
      loadAlbum(originals, name)
    childDirectories(originals).filter(dir => isNumber(dir.getName, 4)).foldLeft(curated): (loaded, yearDir) =>
      loaded ++ yearAlbums(originals, yearDir)

  private def yearAlbums(originals: File, yearDir: File): Seq[LoadedAlbum] =
    val year: String = yearDir.getName
    val months: Seq[LoadedAlbum] = tomlFiles(yearDir).flatMap: file =>
      val (name, _) = Files.nameAndExtension(file)
      Option.when(isNumber(name, 2))(loadAlbum(originals, s"$year/$name"))
    childDirectories(yearDir).filter(dir => isNumber(dir.getName, 2)).foldLeft(months): (loaded, monthDir) =>
      loaded ++ monthAlbums(originals, year, monthDir)

  private def monthAlbums(originals: File, year: String, monthDir: File): Seq[LoadedAlbum] =
    val month: String = monthDir.getName
    val days: Seq[LoadedAlbum] = tomlFiles(monthDir).flatMap: file =>
      val (name, _) = Files.nameAndExtension(file)
      Option.when(isNumber(name, 2))(loadAlbum(originals, s"$year/$month/$name"))
    childDirectories(monthDir).filter(dir => isNumber(dir.getName, 2)).foldLeft(days): (loaded, dayDir) =>
      loaded ++ dayAlbums(originals, year, month, dayDir)

  private def dayAlbums(originals: File, year: String, month: String, dayDir: File): Seq[LoadedAlbum] =
    val day: String = dayDir.getName
    tomlFiles(dayDir).flatMap: file =>
      val (name, _) = Files.nameAndExtension(file)
      Option.when(!name.headOption.exists(_.isDigit))(loadAlbum(originals, s"$year/$month/$day/$name"))

  private def loadAlbum(originals: File, title: String): LoadedAlbum =
    val file: File = File(originals, s"$title.toml")
    val segments: Seq[String] = title.split('/').toSeq.filter(_.nonEmpty)
    val album: Album = segments match
      case Seq(year, month, day, _) if isNumber(year, 4) && isNumber(month, 2) && isNumber(day, 2) =>
        Album.readEvent(file, title, LocalDate.of(year.toInt, month.toInt, day.toInt))
      case Seq(year, month, day) if isNumber(year, 4) && isNumber(month, 2) && isNumber(day, 2) =>
        Album.readDay(file, title)
      case Seq(year, month) if isNumber(year, 4) && isNumber(month, 2) =>
        Album.readMonth(file, title)
      case Seq(_) =>
        Album.readCurated(file, title)
      case _ =>
        throw IllegalArgumentException(s"not an album title: $title")
    LoadedAlbum(file, album)

  private def childDirectories(directory: File): Seq[File] =
    Files.list(directory)._1.sortBy(_.getName)

  private def tomlFiles(directory: File): Seq[File] =
    Files.list(directory)._2.filter(_.getName.endsWith(".toml")).sortBy(_.getName)

  private def isNumber(name: String, length: Int): Boolean =
    name.length == length && name.forall(_.isDigit)

  private def mediaId(originals: File, picturePath: String): Option[String] =
    val (parent, name) = Files.splitOnLast(picturePath, '/')
    val stem: String = name.getOrElse(throw IllegalArgumentException(s"not a picture path: $picturePath"))
    val file: File = File(File(originals, parent), s"$stem.toml")
    if !file.isFile then None
    else Metadata.read(file).`google-media-item-id`.filter(_.nonEmpty)

  private def pictureName(path: String): String = Files.splitOnLast(path, '/')._2.getOrElse(path)

  private def messageOf(error: Exception): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)

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
