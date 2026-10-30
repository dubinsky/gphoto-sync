package org.podval.tools.photo

import com.google.photos.library.v1.PhotosLibraryClient
import com.google.photos.library.v1.upload.UploadMediaItemRequest
import com.google.photos.library.v1.util.NewMediaItemFactory
import com.google.photos.types.proto.Album as GoogleAlbum
import java.io.{File, RandomAccessFile}
import scala.jdk.CollectionConverters.{IterableHasAsScala, ListHasAsScala, SeqHasAsJava}
import scala.jdk.OptionConverters.*

final case class Publish(path: Option[Path], recursive: Boolean, dryRun: Boolean) extends Operation:
  override def execute(): Unit =
    val root: Root = path.fold(Root.get)(_.root)
    val prefix: Option[String] = path.map(_.toString.stripPrefix("/")).filter(_.nonEmpty)
    Publish.run(
      originals = root.configuration.archive,
      prefix = prefix,
      recursive = recursive,
      dryRun = dryRun,
      google = () => Publish.openLibrary(),
      exiftool = Publish.runExiftool,
      tempDirectory = File(System.getProperty("java.io.tmpdir"))
    )

object Publish:
  private val batchSize: Int = 50

  // jpg first so a still and a video with the same stem upload the still.
  private val uploadable: Seq[String] = Seq(
    Extension.JPG.name,
    Extension.MOV.name,
    Extension.MP4.name,
    Extension.AVI.name,
    Extension.TGP.name
  )

  private[photo] final case class CreateItem(uploadToken: String, fileName: String)
  private[photo] final case class Created(
    uploadToken: String,
    mediaItemId: Option[String],
    status: Option[String] = None
  )

  private[photo] trait GoogleTransport extends AutoCloseable:
    def listAppAlbums(): Seq[(String, String)]
    def createAlbum(title: String): String
    def updateAlbumTitle(albumId: String, title: String): Unit
    def upload(file: File, mimeType: String): String
    def batchCreate(items: Seq[CreateItem]): Seq[Created]
    // Ids from batchCreate that batchGet actually returns. A duplicate of a library item this
    // client did not create comes back with an id batchGet cannot see.
    def owns(ids: Seq[String]): Set[String]
    def batchAdd(albumId: String, mediaItemIds: Seq[String]): Unit
    override def close(): Unit = ()

  private[photo] def runExiftool(argv: Seq[String]): Either[String, Unit] =
    val process: Process = new ProcessBuilder(argv.asJava).redirectErrorStream(true).start()
    val output: String =
      val source = scala.io.Source.fromInputStream(process.getInputStream, "UTF-8")
      try source.mkString finally source.close()
    val code: Int = process.waitFor()
    if code == 0 then Right(())
    else
      val detail: String = output.trim
      Left(if detail.isEmpty then s"exiftool exited with $code" else s"exiftool exited with $code: $detail")

  private[photo] def openLibrary(): GoogleTransport = LibraryTransport(Google.newClient())

  private[photo] def run(
    originals: File,
    prefix: Option[String],
    recursive: Boolean,
    dryRun: Boolean,
    google: () => GoogleTransport,
    exiftool: Seq[String] => Either[String, Unit],
    tempDirectory: File
  ): Unit =
    val selectedPrefix: Option[String] = prefix.map(_.stripPrefix("/")).filter(_.nonEmpty)
    if Album.needsRecursive(selectedPrefix) && !recursive then
      throw IllegalArgumentException("year or root requires --recursive")

    val scan: Archive.Scan = Archive.scan(originals)
    val pictures: Seq[Claims.Resolved] = Claims.resolve(scan)
    val selected: Seq[Claims.Resolved] = select(pictures, selectedPrefix).filter: picture =>
      picture.albums.nonEmpty || picture.excludedOnly
    if selected.isEmpty then
      println("no albums")
    else
      val titles: Seq[String] = distinctTitles(selected)
      val byTitle: Map[String, Album.Loaded] = scan.albums.map(loaded => loaded.album.title -> loaded).toMap
      val albums: Map[String, Album.Loaded] = titles.map(title => title -> byTitle(title)).toMap
      val facts: Map[String, Archive.Picture] = scan.pictures.map(picture => picture.path -> picture).toMap
      val work: Seq[Work] = selected.map: picture =>
        if picture.excludedOnly then SkipExcluded(picture.path)
        else plan(picture, albums, facts(picture.path), tempDirectory)
      // Only albums that will receive an upload or an existing media id.
      val ensuredTitles: Seq[String] = work.flatMap {
        case item: UploadNew => item.targets
        case item: FileExisting => item.targets
        case _ => Seq.empty
      }.distinct
      if dryRun then
        selected.foreach(picture => println(picture.line))
        val split: DesiredSplit = splitDesired(ensuredTitles, albums)
        split.blocked.foreach: title =>
          reportClaimers(
            title,
            collisionText(title, ensuredTitles, albums),
            selected,
            (picture, album, message) => println(s"$picture $album: $message")
          )
        split.open.foreach: title =>
          println(title)
          val album: Album = albums(title).album
          if album.desiredGoogleTitle != album.title then
            println(s"google ${Album.quote(album.title)} ${Album.quote(album.desiredGoogleTitle)}")
      else
        publish(
          scan.albums,
          selected,
          albums,
          work,
          ensuredTitles,
          google,
          exiftool
        )

  private def publish(
    allAlbums: Seq[Album.Loaded],
    selected: Seq[Claims.Resolved],
    albums: Map[String, Album.Loaded],
    work: Seq[Work],
    ensuredTitles: Seq[String],
    google: () => GoogleTransport,
    exiftool: Seq[String] => Either[String, Unit]
  ): Unit =
    var failureLines: List[String] = Nil
    def fail(picture: String, album: String, message: String): Unit =
      failureLines = s"$picture $album: $message" :: failureLines

    var skippedExcluded: Int = 0
    var skippedExternal: Int = 0
    var skippedExists: Int = 0
    var uploaded: Int = 0
    var filed: Int = 0

    var opened: Option[GoogleTransport] = None
    def transport: GoogleTransport = opened match
      case Some(value) => value
      case None =>
        val created: GoogleTransport = google()
        opened = Some(created)
        created

    try
      work.foreach:
        case broken: Broken => fail(broken.path, broken.album, broken.message)
        case _ => ()
      // Open before album writes so a missing token fails with its path and does not look like an ensure error.
      if ensuredTitles.nonEmpty then
        val _: GoogleTransport = transport
      val ensured: Map[String, String] = ensure(allAlbums, ensuredTitles, albums, selected, transport, fail)
      val (uploadedItems, alreadyInGoogle) = uploadNew(work, ensured, exiftool, transport, fail)
      uploaded = uploadedItems.length
      skippedExists += alreadyInGoogle
      filed = fileIntoAlbums(work, uploadedItems, ensured, transport, fail)
    finally
      opened.foreach(_.close())

    work.foreach:
      case _: SkipExcluded => skippedExcluded += 1
      case _: SkipExternal => skippedExternal += 1
      case _: AlreadyInGoogle => skippedExists += 1
      case _: FileExisting => skippedExists += 1
      case _: Broken => ()
      case _: StayLocal => ()
      case _: UploadNew => ()

    failureLines.reverse.foreach(println)
    val failed: Int = failureLines.length
    println(
      s"uploaded=$uploaded filed=$filed skipped-excluded=$skippedExcluded " +
        s"skipped-external=$skippedExternal skipped-exists=$skippedExists failed=$failed"
    )
    if failed > 0 then throw IllegalArgumentException("publish failed")

  private def uploadNew(
    work: Seq[Work],
    ensured: Map[String, String],
    exiftool: Seq[String] => Either[String, Unit],
    transport: GoogleTransport,
    fail: (String, String, String) => Unit
  ): (Seq[(UploadNew, String)], Int) =
    val ready: Seq[UploadNew] = work.collect { case item: UploadNew => item }.flatMap: item =>
      val live: Seq[String] = item.targets.filter(ensured.contains)
      if live.isEmpty then None else Some(item.copy(targets = live))

    ready.grouped(batchSize).foldLeft((Vector.empty[(UploadNew, String)], 0)): (acc, batch) =>
      val prepared: Seq[(UploadNew, String)] = batch.flatMap: picture =>
        val preparedFile: Either[String, File] = picture.argv match
          case None => Right(picture.bytes)
          case Some(argv) => exiftool(argv).map(_ => picture.bytes)
        preparedFile match
          case Left(message) =>
            fail(picture.path, picture.targets.mkString(","), message)
            cleanup(picture)
            None
          case Right(file) =>
            val uploaded: Option[(UploadNew, String)] =
              try Some(picture -> transport.upload(file, mime(picture.extension)))
              catch case error: Exception =>
                fail(picture.path, picture.targets.mkString(","), Album.messageOf(error))
                None
            cleanup(picture)
            uploaded
      val (created, already) = createBatch(prepared, transport, fail)
      (acc._1 ++ created, acc._2 + already)

  private def createBatch(
    batch: Seq[(UploadNew, String)],
    transport: GoogleTransport,
    fail: (String, String, String) => Unit
  ): (Seq[(UploadNew, String)], Int) =
    if batch.isEmpty then (Seq.empty, 0)
    else
      val items: Seq[CreateItem] = batch.map: (picture, token) =>
        val name: String = Files.splitOnLast(picture.path, '/')._2.getOrElse(picture.path)
        CreateItem(token, s"$name.${picture.extension}")
      val results: Either[String, Seq[Created]] =
        try Right(transport.batchCreate(items))
        catch case error: Exception => Left(Album.messageOf(error))
      results match
        case Left(message) =>
          batch.foreach((picture, _) => fail(picture.path, picture.targets.mkString(","), message))
          (Seq.empty, 0)
        case Right(created) =>
          val returned: Seq[String] = created.flatMap(_.mediaItemId.filter(_.nonEmpty))
          val owned: Either[String, Set[String]] =
            if returned.isEmpty then Right(Set.empty)
            else
              try Right(transport.owns(returned))
              catch case error: Exception => Left(Album.messageOf(error))
          owned match
            case Left(message) =>
              batch.foreach((picture, _) => fail(picture.path, picture.targets.mkString(","), message))
              (Seq.empty, 0)
            case Right(visible) =>
              val stored: Seq[(UploadNew, String)] = batch.zip(created).flatMap:
                case ((picture, _), result) => acceptMediaId(picture, result, visible, fail)
              batch.drop(created.length).foreach((picture, _) => rejectMissingId(picture, None, fail))
              val already: Int = batch.zip(created).count:
                case (_, result) => result.mediaItemId.exists(id => id.nonEmpty && !visible.contains(id))
              (stored, already)

  private def acceptMediaId(
    picture: UploadNew,
    result: Created,
    owned: Set[String],
    fail: (String, String, String) => Unit
  ): Option[(UploadNew, String)] =
    result.mediaItemId.filter(_.nonEmpty) match
      case Some(id) if owned.contains(id) =>
        Files.write(Metadata.encode(picture.metadata.copy(`google-media-item-id` = Some(id))), picture.metadataFile)
        Some(picture -> id)
      case Some(_) =>
        // 2026-10-26: a byte-identical library JPEG returns an id batchGet cannot see.
        Files.write(Metadata.encode(picture.metadata.copy(`already-in-google` = Some(true))), picture.metadataFile)
        None
      case None =>
        rejectMissingId(picture, result.status, fail)
        None

  private def rejectMissingId(
    picture: UploadNew,
    status: Option[String],
    fail: (String, String, String) => Unit
  ): Unit =
    val message: String = status.filter(_.nonEmpty).fold("batchCreate returned no media item id"): text =>
      s"batchCreate returned no media item id: $text"
    fail(picture.path, picture.targets.mkString(","), message)

  private def fileIntoAlbums(
    work: Seq[Work],
    uploaded: Seq[(UploadNew, String)],
    ensured: Map[String, String],
    transport: GoogleTransport,
    fail: (String, String, String) => Unit
  ): Int =
    val titleById: Map[String, String] = ensured.map(_.swap)
    val fromNew: Seq[(String, String, String)] = uploaded.flatMap: (picture, mediaId) =>
      picture.targets.filter(ensured.contains).map(title => (ensured(title), mediaId, picture.path))
    val fromExisting: Seq[(String, String, String)] = work.collect { case item: FileExisting => item }.flatMap: item =>
      item.targets.filter(ensured.contains).map(title => (ensured(title), item.mediaId, item.path))
    (fromNew ++ fromExisting).groupBy(_._1).foldLeft(0): (count, entry) =>
      val (albumId, rows) = entry
      val albumTitle: String = titleById.getOrElse(albumId, albumId)
      rows.map(row => (row._2, row._3)).grouped(batchSize).foldLeft(count): (acc, batch) =>
        try
          transport.batchAdd(albumId, batch.map(_._1))
          acc + batch.length
        catch case error: Exception =>
          batch.foreach((_, picture) => fail(picture, albumTitle, Album.messageOf(error)))
          acc

  private def ensure(
    allAlbums: Seq[Album.Loaded],
    titles: Seq[String],
    albums: Map[String, Album.Loaded],
    pictures: Seq[Claims.Resolved],
    transport: GoogleTransport,
    fail: (String, String, String) => Unit
  ): Map[String, String] =
    if titles.isEmpty then Map.empty
    else
      val knownIds: Map[String, String] = localAlbumIds(allAlbums)
      val split: DesiredSplit = splitDesired(titles, albums)
      split.blocked.foreach: title =>
        failClaimers(title, collisionText(title, titles, albums), pictures, fail)
      val open: Seq[String] = split.open
      val needRemote: Boolean = open.exists(title => albums(title).album.googleAlbumId.isEmpty)
      val remoteResult: Either[String, Seq[(String, String)]] =
        if !needRemote then Right(Seq.empty)
        else
          try Right(transport.listAppAlbums())
          catch case error: Exception => Left(Album.messageOf(error))
      remoteResult match
        case Left(message) =>
          open.foreach(title => failClaimers(title, message, pictures, fail))
          Map.empty
        case Right(remote) =>
          val (ensured, _, _) = open.foldLeft((Map.empty[String, String], remote, knownIds)): (state, title) =>
            val (done, remoteNow, idsNow) = state
            ensureOne(albums(title), remoteNow, idsNow, transport) match
              case Left(message) =>
                failClaimers(title, message, pictures, fail)
                (done, remoteNow, idsNow)
              case Right(result) =>
                (done.updated(title, result.id), result.remote, result.ids)
          ensured

  private def failClaimers(
    title: String,
    message: String,
    pictures: Seq[Claims.Resolved],
    fail: (String, String, String) => Unit
  ): Unit =
    reportClaimers(title, message, pictures, fail)

  private def reportClaimers(
    title: String,
    message: String,
    pictures: Seq[Claims.Resolved],
    report: (String, String, String) => Unit
  ): Unit =
    val claimers: Seq[Claims.Resolved] = pictures.filter(_.albums.contains(title))
    if claimers.isEmpty then report("-", title, message)
    else claimers.foreach(picture => report(picture.path, title, message))

  private final case class DesiredSplit(blocked: Set[String], open: Seq[String])

  private def splitDesired(titles: Seq[String], albums: Map[String, Album.Loaded]): DesiredSplit =
    val blocked: Set[String] = titles.filter { title =>
      val desired: String = albums(title).album.desiredGoogleTitle
      titles.exists(other => other != title && albums(other).album.desiredGoogleTitle == desired)
    }.toSet
    DesiredSplit(blocked, titles.filterNot(blocked.contains))

  private def collisionText(title: String, titles: Seq[String], albums: Map[String, Album.Loaded]): String =
    val desired: String = albums(title).album.desiredGoogleTitle
    val others: String = titles
      .filter(other => other != title && albums(other).album.desiredGoogleTitle == desired)
      .mkString(", ")
    s"google title ${Album.quote(desired)} is also desired by $others"

  private[photo] final case class Ensured(id: String, remote: Seq[(String, String)], ids: Map[String, String])

  private[photo] def ensureOne(
    loaded: Album.Loaded,
    remote: Seq[(String, String)],
    ids: Map[String, String],
    transport: GoogleTransport
  ): Either[String, Ensured] =
    val album: Album = loaded.album
    if album.owner.nonEmpty then
      Left(s"${album.title} is owned by someone else")
    else
      val desired: String = album.desiredGoogleTitle
      val resolved: Either[String, (String, Seq[(String, String)], Map[String, String], Boolean)] =
        if album.googleAlbumId.nonEmpty then Right((album.googleAlbumId, remote, ids, false))
        else
          remote.filter(_._2 == desired) match
            case Seq() =>
              try
                val id: String = transport.createAlbum(desired)
                Album.storeGoogle(loaded.file, id, desired)
                Right((id, remote :+ (id, desired), ids.updated(id, album.title), true))
              catch case error: Exception => Left(Album.messageOf(error))
            case Seq((id, _)) =>
              ids.get(id) match
                case Some(other) if other != album.title =>
                  Left(s"google album id is already stored on $other")
                case _ =>
                  try
                    Album.storeGoogle(loaded.file, id, desired)
                    Right((id, remote, ids.updated(id, album.title), true))
                  catch case error: Exception => Left(Album.messageOf(error))
            case _ =>
              Left(s"more than one Google album titled $desired")

      resolved.flatMap: (id, remoteNow, idsNow, wrote) =>
        val titleNeedsUpdate: Boolean =
          album.publishedTitle.isEmpty || album.publishedTitle != desired
        if wrote || !titleNeedsUpdate then Right(Ensured(id, remoteNow, idsNow))
        else
          try
            transport.updateAlbumTitle(id, desired)
            Album.storeGoogle(loaded.file, id, desired)
            val seen: Seq[(String, String)] = remoteNow.map:
              case (existing, _) if existing == id => (id, desired)
              case pair => pair
            val remoteAfter: Seq[(String, String)] =
              if seen.exists(_._1 == id) then seen else seen :+ (id, desired)
            Right(Ensured(id, remoteAfter, idsNow))
          catch case error: Exception => Left(Album.messageOf(error))

  private def plan(
    picture: Claims.Resolved,
    albums: Map[String, Album.Loaded],
    fact: Archive.Picture,
    tempDirectory: File
  ): Work =
    val titles: Seq[String] = picture.albums
    if titles.exists(title => albums(title).album.external) then SkipExternal(picture.path)
    else
      val targets: Seq[String] = titles.filterNot(title => albums(title).album.external)
      if targets.isEmpty then SkipExcluded(picture.path)
      else
        val directory: File = fact.directory
        val name: String = fact.name
        val metadataFile: File = File(directory, s"$name.toml")
        val metadata: Metadata = fact.metadata
        if metadata.`already-in-google`.contains(true) then AlreadyInGoogle(picture.path)
        else metadata.`google-media-item-id`.filter(_.nonEmpty) match
          case Some(id) => FileExisting(picture.path, id, targets)
          case None =>
            literalUpload(directory, name) match
              case Left(message) => Broken(picture.path, targets.mkString(","), message)
              case Right(None) => StayLocal(picture.path)
              case Right(Some((source, ext))) =>
                ExifOverride(source, ext, metadata, tempDirectory) match
                  case Left(message) => Broken(picture.path, targets.mkString(","), message)
                  case Right((bytes, argv)) =>
                    UploadNew(picture.path, targets, metadata, metadataFile, source, bytes, ext, argv)

  private def cleanup(picture: UploadNew): Unit =
    if picture.bytes.getPath != picture.source.getPath && picture.bytes.isFile then picture.bytes.delete()

  private def mime(ext: String): String = ext match
    case Extension.JPG.name => "image/jpeg"
    case Extension.MOV.name => "video/quicktime"
    case Extension.MP4.name => "video/mp4"
    case Extension.AVI.name => "video/x-msvideo"
    case Extension.TGP.name => "video/3gpp"
    case other => throw IllegalArgumentException(s"no mime type for $other")

  private def select(pictures: Seq[Claims.Resolved], prefix: Option[String]): Seq[Claims.Resolved] =
    pictures.filter(picture => Album.pictureMatches(picture.path, prefix))

  private def distinctTitles(pictures: Seq[Claims.Resolved]): Seq[String] =
    pictures.flatMap(_.albums).distinct

  private sealed trait Work
  private final case class SkipExternal(path: String) extends Work
  private final case class SkipExcluded(path: String) extends Work
  private final case class StayLocal(path: String) extends Work
  private final case class AlreadyInGoogle(path: String) extends Work
  private final case class Broken(path: String, album: String, message: String) extends Work
  private final case class FileExisting(path: String, mediaId: String, targets: Seq[String]) extends Work
  private final case class UploadNew(
    path: String,
    targets: Seq[String],
    metadata: Metadata,
    metadataFile: File,
    source: File,
    bytes: File,
    extension: String,
    argv: Option[Seq[String]]
  ) extends Work

  private def localAlbumIds(albums: Seq[Album.Loaded]): Map[String, String] =
    albums.foldLeft(Map.empty[String, String]): (ids, loaded) =>
      val id: String = loaded.album.googleAlbumId
      if id.isEmpty then ids else ids.updated(id, loaded.album.title)

  // A grouped name that repair would change is not renamed here. The literal name.ext must already be on disk.
  private def literalUpload(directory: File, name: String): Either[String, Option[(File, String)]] =
    val literal: Option[(File, String)] = uploadable
      .find(ext => File(directory, s"$name.$ext").isFile)
      .map(ext => File(directory, s"$name.$ext") -> ext)
    literal match
      case Some(_) if !name.headOption.exists(_.isDigit) =>
        Left(s"$name does not start with a digit")
      case Some(found) => Right(Some(found))
      case None =>
        misnamedUpload(directory, name) match
          case Some(actual) => Left(s"$actual is not the literal file")
          case None => Right(None)

  private def misnamedUpload(directory: File, name: String): Option[String] =
    Files.list(directory)._2.map(_.getName).find(fileName => misnamedUploadable(fileName, name))

  private def misnamedUploadable(fileName: String, pictureName: String): Boolean =
    val (stem, extOpt) = Files.splitOnLast(fileName, '.')
    extOpt.exists: ext =>
      val (stemFixed, extFixed) = Pictures.repaired(stem, ext)
      stemFixed == pictureName && uploadable.contains(extFixed) && fileName != s"$pictureName.$extFixed"

  private final class LibraryTransport(client: PhotosLibraryClient) extends GoogleTransport:
    override def close(): Unit = client.close()

    override def listAppAlbums(): Seq[(String, String)] =
      client.listAlbums(true).iterateAll().asScala.map(album => album.getId -> album.getTitle).toSeq

    override def createAlbum(title: String): String = client.createAlbum(title).getId

    override def updateAlbumTitle(albumId: String, title: String): Unit =
      val album: GoogleAlbum = GoogleAlbum.newBuilder().setId(albumId).build()
      client.updateAlbumTitle(album, title)

    override def upload(file: File, mimeType: String): String =
      val data: RandomAccessFile = new RandomAccessFile(file, "r")
      try
        val response = client.uploadMediaItem(
          UploadMediaItemRequest.newBuilder().setMimeType(mimeType).setDataFile(data).build()
        )
        response.getUploadToken.toScala.filter(_.nonEmpty).getOrElse(
          throw IllegalArgumentException(s"upload returned no token for ${file.getPath}")
        )
      finally data.close()

    override def batchCreate(items: Seq[CreateItem]): Seq[Created] =
      val newItems = items.map: item =>
        // The only overload that sets fileName requires a non-empty description.
        NewMediaItemFactory.createNewMediaItem(item.uploadToken, item.fileName, " ")
      .asJava
      val results = client.batchCreateMediaItems(newItems).getNewMediaItemResultsList.asScala.toSeq
      results.map: result =>
        val id: Option[String] =
          if result.hasMediaItem && result.getMediaItem.getId.nonEmpty then Some(result.getMediaItem.getId)
          else None
        val status: Option[String] =
          if id.isDefined then None
          else Option(result.getStatus).map(_.getMessage).filter(text => text != null && text.nonEmpty)
        Created(Option(result.getUploadToken).getOrElse(""), id, status)

    override def owns(ids: Seq[String]): Set[String] =
      if ids.isEmpty then Set.empty
      else
        client.batchGetMediaItems(ids.asJava).getMediaItemResultsList.asScala.flatMap: result =>
          if result.hasMediaItem && result.getMediaItem.getId.nonEmpty then Some(result.getMediaItem.getId)
          else None
        .toSet

    override def batchAdd(albumId: String, mediaItemIds: Seq[String]): Unit =
      client.batchAddMediaItemsToAlbum(albumId, mediaItemIds.asJava)
