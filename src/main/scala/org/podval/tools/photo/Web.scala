package org.podval.tools.photo

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.io.File
import java.net.{BindException, InetSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.file.{FileSystemException, Files as NFiles, NoSuchFileException, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import java.time.{DateTimeException, Instant, LocalDate}
import java.util.concurrent.{CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.annotation.tailrec

final case class Web(port: Int) extends Operation:
  override def execute(session: Session): (Session, Result) =
    Serve.runtimeDirectory() match
      case Left(message) =>
        (session, Result.Usage(message))
      case Right(runtime) =>
        Web.useSession(session)
        try
          Web.start(runtime, port) match
            case Left(message) =>
              (session, Result.Usage(message))
            case Right(_) =>
              (session, Result.Ok("", Json.Null))
        finally
          Web.sessionRef.set(null)

object Web:
  private[photo] val defaultPort: Int = 8765

  private[photo] final case class Rejected(status: Int, message: String)

  private val loopback: String = "127.0.0.1"
  private val alreadyRunning: String = "photo web is already running"
  private val escapeMessage: String = "path escapes the archive"
  private val missingPicture: String = "no such picture"
  private val notPicture: String = "not a picture path"
  private val noDisplay: String = "no displayable image"
  private val lockName: String = "web.lock"
  private val fileMode: String = "rw-------"

  private final case class Held(runtime: File, channel: FileChannel, lock: FileLock)
  private final case class Route(method: String, path: String, run: HttpExchange => Unit)

  private val owner = new AtomicReference[FileChannel](null)
  private val serverRef = new AtomicReference[HttpServer](null)
  private val runtimeRef = new AtomicReference[Option[File]](None)
  private val gate = new AtomicReference[CountDownLatch](null)
  private val signalsInstalled = new AtomicBoolean(false)
  private val catalogLogged = new AtomicBoolean(false)
  // One process session. gather reuses its DirectoryIndex. It is not serve's index.
  private val sessionRef = new AtomicReference[Session](null)
  // A collected channel is closed, and that close drops this JVM's fcntl lock on web.lock.
  private var pinned: List[FileChannel] = Nil

  // Append a route here.
  private val routes: List[Route] = List(
    Route("GET", "/", exchange => sendResource(exchange, "index.html", "text/html; charset=utf-8")),
    Route("GET", "/app.js", exchange => sendResource(exchange, "app.js", "text/javascript; charset=utf-8")),
    Route("GET", "/app.css", exchange => sendResource(exchange, "app.css", "text/css; charset=utf-8")),
    Route("GET", "/api/tree", exchange => api(exchange)(tree(exchange))),
    Route("GET", "/api/day", exchange => api(exchange)(day(exchange))),
    Route("GET", "/api/album", exchange => api(exchange)(album(exchange))),
    Route("GET", "/api/thumb", exchange => guard(exchange)(serveThumb(exchange))),
    Route("GET", "/api/file", exchange => guard(exchange)(serveFile(exchange))),
    Route("POST", "/api/toggle", exchange => post(exchange)(toggle(exchange))),
    Route("POST", "/api/albums", exchange => post(exchange)(createAlbum(exchange)))
  )

  private[photo] def handlerRuntime: Option[File] = runtimeRef.get

  private[photo] def useSession(session: Session): Unit =
    sessionRef.set(session.copy(style = PathStyle.OneShot, cursor = session.root))

  private[photo] def sessionNow: Option[Session] = Option(sessionRef.get)

  // unknown never disagrees. in and out disagree only when desired does not match the mark.
  private[photo] def disagree(desired: Boolean, observed: String): Boolean =
    (observed == "in" || observed == "out") && desired != (observed == "in")

  private[photo] def start(runtime: File, port: Int): Either[String, Unit] =
    if port < 1 || port > 65535 then Left("port must be 1 to 65535")
    else acquire(runtime) match
      case Left(message) =>
        Left(message)
      case Right(held) =>
        try serve(held, port)
        finally release(held)

  private[photo] def stop(): Unit =
    val running: HttpServer = serverRef.get
    if running != null then running.stop(1)
    val latch: CountDownLatch = gate.get
    if latch != null then latch.countDown()

  // Decode once. A decoded ".." is still rejected. Callers pass the raw path, not a second decoding.
  private[photo] def reject(archive: File, raw: String): Either[Rejected, java.nio.file.Path] =
    percentDecode(raw).flatMap(decoded => opened(archive, decoded))

  // Query values are already decoded once. Decoding again would turn %252e%252e into "..".
  private[photo] def opened(archive: File, decoded: String): Either[Rejected, java.nio.file.Path] =
    lexical(decoded).flatMap(names => confine(archive, names))

  // Path.startsWith matches elements. "Pictures-other" is not under "Pictures".
  private[photo] def contained(root: java.nio.file.Path, candidate: java.nio.file.Path): Boolean =
    candidate.startsWith(root)

  private def acquire(runtime: File): Either[String, Held] = synchronized {
    val current: FileChannel = owner.get
    if current != null && current.isOpen then Left(alreadyRunning)
    else
      Serve.prepare(runtime)
      val lockFile: File = File(runtime, lockName)
      val channel: FileChannel = FileChannel.open(
        lockFile.toPath,
        StandardOpenOption.CREATE,
        StandardOpenOption.READ,
        StandardOpenOption.WRITE
      )
      NFiles.setPosixFilePermissions(lockFile.toPath, PosixFilePermissions.fromString(fileMode))
      try
        val lock: FileLock = channel.tryLock()
        if lock == null then
          channel.close()
          Left(alreadyRunning)
        else
          owner.set(channel)
          Right(Held(runtime, channel, lock))
      catch case _: OverlappingFileLockException =>
        // Closing any descriptor for this file drops every fcntl lock this JVM holds on it.
        pinned = channel :: pinned
        Left(alreadyRunning)
  }

  private def release(held: Held): Unit = synchronized {
    try
      if held.lock.isValid then held.lock.release()
    finally
      if owner.get eq held.channel then owner.set(null)
      held.channel.close()
  }

  private def serve(held: Held, port: Int): Either[String, Unit] =
    val latch = new CountDownLatch(1)
    gate.set(latch)
    var server: HttpServer = null
    var pool: ExecutorService = null
    try
      server = HttpServer.create(new InetSocketAddress(loopback, port), 0)
      pool = daemons()
      server.setExecutor(pool)
      server.createContext("/", exchange => dispatch(exchange))
      runtimeRef.set(Some(held.runtime))
      serverRef.set(server)
      server.start()
      println(s"http://$loopback:$port/")
      Console.err.println(s"bound $loopback:$port")
      installSignals()
      latch.await()
      Right(())
    catch case caught: BindException =>
      val detail: String = Option(caught.getMessage).getOrElse("")
      if detail.contains("Address already in use") then Left(s"address already in use: $loopback:$port")
      else Left(detail)
    finally
      if server != null && serverRef.compareAndSet(server, null) then runtimeRef.set(None)
      if server != null then server.stop(0)
      if pool != null then pool.shutdownNow()
      if gate.get eq latch then gate.set(null)
      sessionRef.set(null)

  private def daemons(): ExecutorService =
    val ids = new AtomicInteger
    Executors.newFixedThreadPool(8, (task: Runnable) =>
      val thread = new Thread(task, s"photo-web-${ids.incrementAndGet()}")
      thread.setDaemon(true)
      thread
    )

  private def installSignals(): Unit =
    if signalsInstalled.compareAndSet(false, true) then
      installSignal("INT")
      installSignal("TERM")

  // While this server is up, the signal stops it and execute returns, so the process exits 0.
  // Otherwise the previous handler still runs.
  private def installSignal(name: String): Unit =
    val signal = new sun.misc.Signal(name)
    var previous: sun.misc.SignalHandler = sun.misc.SignalHandler.SIG_DFL
    val handler: sun.misc.SignalHandler = (caught: sun.misc.Signal) =>
      val live: Boolean = serverRef.get != null
      stop()
      if !live && (previous ne null) && (previous ne sun.misc.SignalHandler.SIG_DFL) &&
        (previous ne sun.misc.SignalHandler.SIG_IGN)
      then previous.handle(caught)
    previous = sun.misc.Signal.handle(signal, handler)

  private def dispatch(exchange: HttpExchange): Unit =
    val method: String = exchange.getRequestMethod
    val path: String = exchange.getRequestURI.getPath
    if method == "OPTIONS" then sendError(exchange, 405, "method not allowed")
    else routes.find(route => route.method == method && route.path == path) match
      case Some(route) =>
        route.run(exchange)
      case None if routes.exists(_.path == path) =>
        sendError(exchange, 405, "method not allowed")
      case None =>
        sendError(exchange, 404, "not found")

  private def sendResource(exchange: HttpExchange, name: String, contentType: String): Unit =
    val path: String = s"org/podval/tools/photo/web/$name"
    Option(getClass.getClassLoader.getResourceAsStream(path)) match
      case None =>
        sendError(exchange, 500, "missing page")
      case Some(in) =>
        val bytes: Array[Byte] =
          try in.readAllBytes()
          finally in.close()
        send(exchange, 200, contentType, bytes)

  private def sendError(exchange: HttpExchange, status: Int, message: String): Unit =
    val bytes: Array[Byte] =
      Serve.write(Json.Obj(Seq("error" -> Json.Str(message)))).getBytes(StandardCharsets.UTF_8)
    send(exchange, status, "application/json; charset=utf-8", bytes)

  private def send(exchange: HttpExchange, status: Int, contentType: String, bytes: Array[Byte]): Unit =
    exchange.getResponseHeaders.set("Content-Type", contentType)
    exchange.sendResponseHeaders(status, bytes.length)
    val body = exchange.getResponseBody
    try body.write(bytes)
    finally body.close()

  private def api(exchange: HttpExchange)(body: => Either[Rejected, Json]): Unit =
    try
      body match
        case Left(rejected) =>
          sendError(exchange, rejected.status, rejected.message)
        case Right(json) =>
          val bytes: Array[Byte] = Serve.write(json).getBytes(StandardCharsets.UTF_8)
          send(exchange, 200, "application/json; charset=utf-8", bytes)
    catch case error: IllegalArgumentException =>
      val message: String = Operation.messageOf(error)
      val shown: String = if message.contains("AF1Qip") then "failed" else message
      sendError(exchange, 400, shown)

  private def currentSession(): Session =
    val seen: Session = sessionRef.get
    if seen != null then seen else throw IllegalStateException("photo web has no archive session")

  // getAndUpdate can retry. The listing that produced next stays outside this function.
  private def publish(start: Session, next: Session): Unit =
    sessionRef.getAndUpdate(seen => if seen eq start then next else seen)

  private def tree(exchange: HttpExchange): Either[Rejected, Json] =
    val start: Session = currentSession()
    val view: CatalogView = viewCatalog(start)
    queryValue(exchange, "at").flatMap: at =>
      val where: String = at.getOrElse("")
      branchOf(where).flatMap: branch =>
        val archive: File = start.root.configuration.archive
        openDirectory(archive, where, Rejected(400, "not a branch")).flatMap: resolved =>
          val children: Either[Rejected, Seq[File]] =
            try Right(Files.listDirectories(resolved.toFile))
            catch case _: IllegalArgumentException => Left(Rejected(500, "stray file"))
          children.flatMap: dirs =>
            val entries: Either[Rejected, Seq[Json]] = branch match
              case Branch.Years =>
                Right(dirs.filter(dir => isNumber(dir.getName, 4)).map: dir =>
                  entryJson(dir.getName, dir.getName, None)
                )
              case Branch.Months(year) =>
                Right(dirs.filter(dir => isMonth(dir.getName)).map: dir =>
                  entryJson(dir.getName, s"$year/${dir.getName}", None)
                )
              case Branch.Days(year, month) =>
                dayCounts(archive, year, month, dirs)
            entries.map: listed =>
              val folder: File = resolved.toFile
              val albums: Seq[Json] = branch match
                case Branch.Days(_, _) =>
                  tomlAlbums(archive, folder).map(pair => albumJson(archive, pair._1, pair._2))
                case _ =>
                  Seq.empty
              // A year has no album files. Root curated titles stay on curated, not albums.
              val curated: Seq[Json] = branch match
                case Branch.Years =>
                  tomlAlbums(archive, folder).map(pair => albumJson(archive, pair._1, pair._2))
                case _ =>
                  Seq.empty
              Json.Obj(Seq(
                "at" -> Json.Str(where),
                "kind" -> Json.Str(branch.kind),
                "entries" -> Json.Arr(listed),
                "albums" -> Json.Arr(albums),
                "curated" -> Json.Arr(curated),
                "catalog" -> catalogJson(start, view)
              ))

  private def day(exchange: HttpExchange): Either[Rejected, Json] =
    val start: Session = currentSession()
    val view: CatalogView = viewCatalog(start)
    publish(start, view.next)
    queryValue(exchange, "day").flatMap:
      case None =>
        Left(Rejected(400, "not a day"))
      case Some(text) =>
        val parsed: Day = AgentCommands.parseDay(start, text)
        val archive: File = start.root.configuration.archive
        val shown: String = Title.dayText(parsed.date)
        openDirectory(archive, shown, Rejected(404, "no such day")).flatMap: resolved =>
          val curated: Seq[Album] = tomlAlbums(archive, archive).map(_._2)
          val stems: Seq[Json] = pictureStems(resolved).map: (stem, extensions) =>
            val claimed: Seq[String] = curated.filter(_.claims(parsed.date, stem)).map(_.title)
            stemJson(shown, stem, extensions, claimed)
          val albums: Seq[Json] =
            dayAlbumFiles(archive, resolved).map(pair => albumJson(archive, pair._1, pair._2))
          Right(Json.Obj(Seq(
            "day" -> Json.Str(shown),
            "stems" -> Json.Arr(stems),
            "albums" -> Json.Arr(albums),
            "curated" -> Json.Arr(curated.map(album => Json.Str(album.title))),
            "catalog" -> catalogJson(start, view)
          )))

  private def album(exchange: HttpExchange): Either[Rejected, Json] =
    val start: Session = currentSession()
    val view: CatalogView = viewCatalog(start)
    queryValue(exchange, "title").flatMap:
      case None =>
        Left(Rejected(400, "not an album title"))
      case Some(text) =>
        Title.parse(text) match
          case None =>
            Left(Rejected(400, "not an album title"))
          case Some(parsed) =>
            val archive: File = start.root.configuration.archive
            val file: File = Title.file(archive, parsed)
            if !file.isFile then Left(Rejected(404, s"no such album: ${parsed.title}"))
            else albumResponse(start, view, parsed, file)

  private def albumResponse(start: Session, view: CatalogView, parsed: Title.Parsed, file: File): Either[Rejected, Json] =
    val archive: File = start.root.configuration.archive
    val loaded: Album = Album.read(archive, file)
    val dates: Seq[LocalDate] =
      loaded.span.map(AgentCommands.eachDay).getOrElse(curatedDays(loaded.include))
    // Not loadGrouped: a missing day is a gap, and a symlink is resolved before listFiles.
    groupedDays(archive, dates).map: rows =>
      val observed: Observed = observe(archive, view, file, loaded)
      publish(start, observed.next)
      val shown: Title.Parsed = Title.parse(loaded.title).getOrElse(parsed)
      Json.Obj(Seq(
        "title" -> Json.Str(shown.title),
        "span" -> shown.span.fold(Json.Null)(span => Json.Str(Title.renderSpan(span))),
        "name" -> shown.name.fold(Json.Null)(Json.Str.apply),
        "curated" -> Json.Bool(shown.span.isEmpty),
        "external" -> Json.Bool(loaded.external),
        "owner" -> loaded.owner.fold(Json.Null)(Json.Str.apply),
        "path" -> Json.Str(Album.relative(archive, file)),
        "membership" -> membershipJson(observed.reason),
        "tiles" -> Json.Arr(tileRows(loaded, rows, observed)),
        "unmatched" -> Json.Arr(observed.unmatched),
        "include" -> Json.Arr(loaded.include.map(entry => Json.Str(Include.logical(entry)))),
        "catalog" -> catalogJson(start, view)
      ))

  private enum Branch:
    case Years
    case Months(year: String)
    case Days(year: String, month: String)

    def kind: String = this match
      case Years => "years"
      case Months(_) => "months"
      case Days(_, _) => "days"

  private enum Joined:
    case One(found: Catalog.Album)
    case Unknown(reason: String)

  private final case class CatalogView(present: Boolean, readable: Boolean, albums: Seq[Catalog.Album], next: Session)

  private final case class Observed(
    reason: Option[String],
    hits: Set[(LocalDate, String)],
    unmatched: Seq[Json],
    next: Session
  )

  private def branchOf(at: String): Either[Rejected, Branch] =
    if at.isEmpty then Right(Branch.Years)
    else lexical(at).flatMap:
      case year :: Nil if isNumber(year, 4) =>
        Right(Branch.Months(year))
      case year :: month :: Nil if isNumber(year, 4) && isMonth(month) =>
        Right(Branch.Days(year, month))
      case _ =>
        Left(Rejected(400, "not a branch"))

  private def openDirectory(archive: File, relative: String, missing: Rejected): Either[Rejected, java.nio.file.Path] =
    val directory: File = if relative.isEmpty then archive else File(archive, relative)
    // toRealPath throws when the path is absent. A gap is not that call.
    if !directory.isDirectory then Left(missing)
    else
      try
        val root: java.nio.file.Path = archive.toPath.toRealPath()
        val resolved: java.nio.file.Path = directory.toPath.toRealPath()
        if contained(root, resolved) then Right(resolved) else Left(Rejected(400, escapeMessage))
      catch
        case _: NoSuchFileException => Left(missing)
        case _: FileSystemException => Left(Rejected(400, escapeMessage))

  private def dayCounts(
    archive: File,
    year: String,
    month: String,
    children: Seq[File]
  ): Either[Rejected, Seq[Json]] =
    val root: java.nio.file.Path = archive.toPath.toRealPath()
    children.filter(dir => isDay(dir.getName)).foldLeft[Either[Rejected, List[Json]]](Right(Nil)): (acc, directory) =>
      acc.flatMap: soFar =>
        val name: String = directory.getName
        if !directory.isDirectory then Right(soFar)
        else
          try
            val resolved: java.nio.file.Path = directory.toPath.toRealPath()
            if !contained(root, resolved) then Left(Rejected(400, escapeMessage))
            else
              val count: Int = pictureStems(resolved).length
              Right(soFar :+ entryJson(name, s"$year/$month/$name", Some(count)))
          catch
            case _: NoSuchFileException => Right(soFar)
            case _: FileSystemException => Left(Rejected(400, escapeMessage))

  private def entryJson(name: String, path: String, count: Option[Int]): Json =
    val fields: Seq[(String, Json)] = Seq("name" -> Json.Str(name), "path" -> Json.Str(path))
    Json.Obj(count.fold(fields)(value => fields :+ ("count" -> Json.Long(value))))

  private def albumJson(archive: File, file: File, album: Album): Json =
    Json.Obj(Seq(
      "title" -> Json.Str(album.title),
      "path" -> Json.Str(Album.relative(archive, file))
    ))

  private def stemJson(day: String, stem: String, extensions: Seq[String], curated: Seq[String]): Json =
    Json.Obj(Seq(
      "stem" -> Json.Str(stem),
      "path" -> Json.Str(s"$day/$stem"),
      "extensions" -> Json.Arr(extensions.map(Json.Str.apply)),
      "kind" -> Json.Str(kindOf(extensions)),
      "uploadable" -> Json.Bool(canUpload(extensions)),
      "curated" -> Json.Arr(curated.map(Json.Str.apply))
    ))

  private def tomlAlbums(archive: File, directory: File): Seq[(File, Album)] =
    Option(directory.listFiles).map(_.toSeq).getOrElse(Seq.empty)
      .filter(file => file.isFile && file.getName.endsWith(".toml"))
      .map(file => file -> Album.read(archive, file))
      .sortBy(_._2.title)

  private def dayAlbumFiles(archive: File, directory: java.nio.file.Path): Seq[(File, Album)] =
    tomlAlbums(archive, directory.toFile).filter: (file, _) =>
      !Files.nameAndExtension(file)._1.headOption.exists(_.isDigit)

  private val pictureExtensions: Set[String] =
    (Extension.normal ++ Extension.raw ++ Extension.image).map(_.name)

  private def pictureStems(resolved: java.nio.file.Path): Seq[(String, Seq[String])] =
    Pictures.grouped(Files.listFiles(resolved.toFile)).flatMap: (stem, extensions) =>
      val names: Seq[String] = extensions.toSeq.sorted
      if names.exists(pictureExtensions.contains) then Some(stem -> names) else None

  private def kindOf(extensions: Seq[String]): String =
    val names: Set[String] = extensions.toSet
    if names.contains(Extension.JPG.name) || names.contains(Extension.JPEG.name) then "jpeg"
    else if names.contains(Extension.GIF.name) then "gif"
    else if names.contains(Extension.PNG.name) then "png"
    else if names.exists(previewExtension) then "preview"
    else if names.exists(name => Extension.video.exists(_.name == name)) then "video"
    else "none"

  private def previewExtension(name: String): Boolean =
    name == Extension.WEBP.name || name == Extension.HEIC.name || name == Extension.HEIF.name ||
      name == Extension.AVIF.name || Extension.raw.exists(_.name == name)

  private def canUpload(extensions: Seq[String]): Boolean =
    extensions.exists(Publish.uploadable.contains)

  private def curatedDays(include: Seq[Include.Entry]): Seq[LocalDate] =
    include.flatMap:
      case Include.Entry.Dates(span, _, _) => AgentCommands.eachDay(span)
      case _ => Nil
    .distinct

  private def groupedDays(
    archive: File,
    dates: Seq[LocalDate]
  ): Either[Rejected, List[(LocalDate, Seq[(String, Seq[String])])]] =
    val root: java.nio.file.Path = archive.toPath.toRealPath()
    dates.foldLeft[Either[Rejected, List[(LocalDate, Seq[(String, Seq[String])])]]](Right(Nil)): (acc, date) =>
      acc.flatMap: soFar =>
        val directory: File = File(archive, Title.dayText(date))
        if !directory.isDirectory then Right(soFar)
        else
          try
            val resolved: java.nio.file.Path = directory.toPath.toRealPath()
            if !contained(root, resolved) then Left(Rejected(400, escapeMessage))
            else Right(soFar :+ (date -> pictureStems(resolved)))
          catch
            case _: NoSuchFileException => Right(soFar)
            case _: FileSystemException => Left(Rejected(400, escapeMessage))

  private def tileRows(
    album: Album,
    rows: List[(LocalDate, Seq[(String, Seq[String])])],
    observed: Observed
  ): Seq[Json] =
    val universe: Map[LocalDate, Set[String]] = rows.foldLeft(Map.empty[LocalDate, Set[String]]): (acc, row) =>
      val names: Set[String] = row._2.map(_._1).toSet
      if names.isEmpty then acc else acc.updated(row._1, names)
    val claimed: Option[Set[(LocalDate, String)]] = album.span match
      case Some(_) =>
        None
      case None =>
        Some(album.include.foldLeft(Set.empty[(LocalDate, String)]): (acc, entry) =>
          acc ++ Include.expand(None, entry, universe)
        )
    rows.flatMap: (date, stems) =>
      stems.flatMap: (stem, extensions) =>
        if claimed.forall(_.contains(date -> stem)) then
          Some(tileJson(date, stem, extensions, album.claims(date, stem), mark(observed, date, stem)))
        else None

  private def tileJson(
    day: LocalDate,
    stem: String,
    extensions: Seq[String],
    desired: Boolean,
    observed: String
  ): Json =
    val shown: String = Title.dayText(day)
    Json.Obj(Seq(
      "day" -> Json.Str(shown),
      "stem" -> Json.Str(stem),
      "path" -> Json.Str(s"$shown/$stem"),
      "desired" -> Json.Bool(desired),
      "observed" -> Json.Str(observed),
      "extensions" -> Json.Arr(extensions.map(Json.Str.apply)),
      "kind" -> Json.Str(kindOf(extensions)),
      "uploadable" -> Json.Bool(canUpload(extensions))
    ))

  private def mark(observed: Observed, day: LocalDate, stem: String): String =
    if observed.reason.isDefined then "unknown"
    else if observed.hits.contains(day -> stem) then "in"
    else "out"

  private def membershipJson(reason: Option[String]): Json =
    val (joined, ambiguous) = reason match
      case None => (true, false)
      case Some("ambiguous") => (false, true)
      case Some("error") => (true, false)
      case Some("auto-add") => (true, false)
      case Some(_) => (false, false)
    Json.Obj(Seq(
      "joined" -> Json.Bool(joined),
      "ambiguous" -> Json.Bool(ambiguous),
      "observed" -> Json.Str(if reason.isEmpty then "known" else "unknown"),
      "reason" -> reason.fold(Json.Null)(Json.Str.apply)
    ))

  private def observe(archive: File, view: CatalogView, file: File, album: Album): Observed =
    if !view.readable then
      val reason: String = if view.present then "unreadable" else "missing"
      Observed(Some(reason), Set.empty, Seq.empty, view.next)
    else join(archive, file, album, view.albums, AlbumLinks.read(archive)) match
      case Joined.Unknown(reason) =>
        Observed(Some(reason), Set.empty, Seq.empty, view.next)
      case Joined.One(found) if found.autoAdd =>
        Observed(Some("auto-add"), Set.empty, Seq.empty, view.next)
      case Joined.One(found) if found.error.isDefined =>
        Observed(Some("error"), Set.empty, Seq.empty, view.next)
      case Joined.One(found) =>
        val (gathered, hits, nonHits) = AgentCommands.gather(view.next, found)
        val pairs: Set[(LocalDate, String)] = hits.map(hit => hit._1 -> hit._3).toSet
        val misses: Seq[Json] = nonHits.collect:
          case AgentCommands.NonHit.Miss(date, filename, _) =>
            Json.Obj(Seq(
              "filename" -> Json.Str(filename),
              "day" -> date.fold(Json.Null)(day => Json.Str(Title.dayText(day)))
            ))
        Observed(None, pairs, misses, gathered)

  private def join(
    archive: File,
    file: File,
    album: Album,
    catalog: Seq[Catalog.Album],
    links: Seq[AlbumLinks.Row]
  ): Joined =
    val relative: String = Album.relative(archive, file)
    links.find(_.path == relative) match
      case Some(row) =>
        catalog.find(_.id == row.id) match
          case Some(found) => Joined.One(found)
          case None => Joined.Unknown("link-absent")
      case None =>
        val key: String = album.desiredGoogleTitle
        catalog.filter(_.title == key) match
          case Seq(one) => Joined.One(one)
          case Seq() => Joined.Unknown("unjoined")
          case _ => Joined.Unknown("ambiguous")

  private def viewCatalog(session: Session): CatalogView =
    try
      val (next, albums) = session.loadCatalog
      CatalogView(present = true, readable = true, albums, next)
    catch
      case error: IllegalArgumentException if Operation.messageOf(error) == "catalog is absent" =>
        CatalogView(present = false, readable = false, Seq.empty, session)
      case error: IllegalArgumentException =>
        logUnreadable(Operation.messageOf(error))
        CatalogView(present = true, readable = false, Seq.empty, session)

  private def logUnreadable(message: String): Unit =
    if catalogLogged.compareAndSet(false, true) then
      val line: String =
        if message.contains("AF1Qip") then "catalog is unreadable"
        else s"catalog is unreadable: $message"
      Console.err.println(line)

  private def catalogJson(session: Session, view: CatalogView): Json =
    val file: File = File(File(session.root.configuration.archive, ".gphoto-sync"), "catalog.txt")
    val mtime: Json =
      if !file.isFile then Json.Null else Json.Str(Instant.ofEpochMilli(file.lastModified).toString)
    Json.Obj(Seq(
      "present" -> Json.Bool(view.present),
      "readable" -> Json.Bool(view.readable),
      "mtime" -> mtime,
      "zone" -> Json.Str(session.root.configuration.zoneId.toString)
    ))

  private def queryValue(exchange: HttpExchange, key: String): Either[Rejected, Option[String]] =
    val raw: String = Option(exchange.getRequestURI.getRawQuery).getOrElse("")
    if raw.isEmpty then Right(None)
    else decodeParams(raw.split("&").toList, Nil).map: pairs =>
      pairs.collectFirst { case (name, value) if name == key => value }

  @tailrec
  private def decodeParams(rest: List[String], acc: List[(String, String)]): Either[Rejected, List[(String, String)]] =
    rest match
      case Nil =>
        Right(acc.reverse)
      case head :: tail =>
        val (keyRaw, valueRaw) = head.span(_ != '=')
        val valuePart: String = if valueRaw.isEmpty then "" else valueRaw.drop(1)
        percentDecode(keyRaw) match
          case Left(rejected) =>
            Left(rejected)
          case Right(key) =>
            percentDecode(valuePart) match
              case Left(rejected) =>
                Left(rejected)
              case Right(value) =>
                decodeParams(tail, (key -> value) :: acc)

  private def isNumber(name: String, length: Int): Boolean =
    name.length == length && name.forall(_.isDigit)

  private def isMonth(name: String): Boolean =
    isNumber(name, 2) && name.toInt >= 1 && name.toInt <= 12

  private def isDay(name: String): Boolean =
    isNumber(name, 2) && name.toInt >= 1 && name.toInt <= 31

  private def percentDecode(raw: String): Either[Rejected, String] =
    decodeBytes(raw, Nil) match
      case Left(rejected) =>
        Left(rejected)
      case Right(bytes) =>
        val decoder = StandardCharsets.UTF_8.newDecoder()
        decoder.onMalformedInput(CodingErrorAction.REPORT)
        decoder.onUnmappableCharacter(CodingErrorAction.REPORT)
        try Right(decoder.decode(ByteBuffer.wrap(bytes)).toString)
        catch case _: CharacterCodingException => Left(Rejected(400, escapeMessage))

  @tailrec
  private def decodeBytes(rest: String, acc: List[Byte]): Either[Rejected, Array[Byte]] =
    if rest.isEmpty then Right(acc.reverse.toArray)
    else if rest.head != '%' then
      val (plain, more) = rest.span(_ != '%')
      val bytes: List[Byte] = plain.getBytes(StandardCharsets.UTF_8).toList.reverse
      decodeBytes(more, bytes ++ acc)
    else
      val hex: String = rest.drop(1).take(2)
      if hex.length != 2 || !hex.forall(isHex) then Left(Rejected(400, escapeMessage))
      else decodeBytes(rest.drop(3), Integer.parseInt(hex, 16).toByte :: acc)

  private def isHex(ch: Char): Boolean =
    ch.isDigit || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')

  private def lexical(decoded: String): Either[Rejected, List[String]] =
    if decoded.exists(ch => ch == '\\' || ch == '\u0000') || decoded.startsWith("/") then
      Left(Rejected(400, escapeMessage))
    else
      val names: List[String] = segments(decoded, Nil)
      if names.exists(name => name.isEmpty || name == "." || name == "..") then Left(Rejected(400, escapeMessage))
      else Right(names)

  @tailrec
  private def segments(rest: String, acc: List[String]): List[String] =
    val (name, more) = rest.span(_ != '/')
    if more.isEmpty then (name :: acc).reverse
    else segments(more.drop(1), name :: acc)

  // Walk each existing name. toRealPath of a missing child under an outside symlink is NoSuchFileException,
  // which would hide the escape. NOFOLLOW_LINKS leaves the link name, so the prefix check would accept it.
  private def confine(archive: File, names: List[String]): Either[Rejected, java.nio.file.Path] =
    try
      val root: java.nio.file.Path = archive.toPath.toRealPath()
      names.foldLeft[Either[Rejected, java.nio.file.Path]](Right(root)): (soFar, name) =>
        soFar.flatMap: dir =>
          val resolved: java.nio.file.Path = dir.resolve(name).toRealPath()
          if contained(root, resolved) then Right(resolved) else Left(Rejected(400, escapeMessage))
    catch
      case _: NoSuchFileException => Left(Rejected(404, missingPicture))
      case _: FileSystemException => Left(Rejected(400, escapeMessage))

  private def guard(exchange: HttpExchange)(body: => Unit): Unit =
    try body
    catch case error: IllegalArgumentException =>
      val message: String = Operation.messageOf(error)
      val shown: String = if message.contains("AF1Qip") then "failed" else message
      sendError(exchange, 400, shown)

  private def serveThumb(exchange: HttpExchange): Unit =
    prepared(exchange) match
      case Left(rejected) =>
        sendError(exchange, rejected.status, rejected.message)
      case Right(relative, files) =>
        val (bytes, marker) = Thumb.thumbnail(relative, files) match
          case Some(image) => (image, "hit")
          case None => (Thumb.placeholder, "placeholder")
        exchange.getResponseHeaders.set("X-Photo-Thumb", marker)
        exchange.getResponseHeaders.set("Cache-Control", "private, no-store")
        send(exchange, 200, "image/jpeg", bytes)

  private def serveFile(exchange: HttpExchange): Unit =
    prepared(exchange) match
      case Left(rejected) =>
        sendError(exchange, rejected.status, rejected.message)
      case Right(_, files) =>
        Thumb.display(files) match
          case None =>
            sendError(exchange, 404, noDisplay)
          case Some(Thumb.Display.Stream(file, contentType)) =>
            sendStream(exchange, file, contentType)
          case Some(Thumb.Display.Inline(bytes, contentType)) =>
            send(exchange, 200, contentType, bytes)

  private def prepared(exchange: HttpExchange): Either[Rejected, (String, Seq[(String, File)])] =
    val archive: File = currentSession().root.configuration.archive
    queryValue(exchange, "path").flatMap:
      case None =>
        Left(Rejected(400, notPicture))
      case Some(decoded) =>
        locate(archive, decoded)

  private def locate(archive: File, decoded: String): Either[Rejected, (String, Seq[(String, File)])] =
    if decoded.isEmpty then Left(Rejected(400, notPicture))
    else opened(archive, decoded) match
      case Left(rejected) if rejected.status == 400 =>
        Left(rejected)
      case _ =>
        dateStem(decoded) match
          case None =>
            Left(Rejected(400, notPicture))
          case Some(day, stem) =>
            opened(archive, day).flatMap: dir =>
              val rows: Seq[Pictures.Planned] =
                Pictures.plan(Files.listFiles(dir.toFile)).filter(row => row.stem == stem && !row.omitted)
              if !rows.exists(row => pictureExtensions.contains(row.extension)) then
                Left(Rejected(404, missingPicture))
              else openSources(archive, day, rows).flatMap: files =>
                if files.isEmpty then Left(Rejected(404, missingPicture))
                else Right(s"$day/$stem" -> files)

  private def dateStem(decoded: String): Option[(String, String)] =
    segments(decoded, Nil) match
      case year :: month :: day :: stem :: Nil
        if isNumber(year, 4) && isMonth(month) && isDay(day) && !stem.contains('.') =>
          Some(s"$year/$month/$day" -> stem)
      case _ =>
        None

  private def openSources(
    archive: File,
    day: String,
    rows: Seq[Pictures.Planned]
  ): Either[Rejected, Seq[(String, File)]] =
    val byExt: Map[String, Pictures.Planned] =
      rows.foldLeft(Map.empty[String, Pictures.Planned]): (acc, row) =>
        if acc.contains(row.extension) then acc else acc.updated(row.extension, row)
    Thumb.sourceOrder.foldLeft[Either[Rejected, List[(String, File)]]](Right(Nil)): (acc, ext) =>
      acc.flatMap: soFar =>
        byExt.get(ext) match
          case None =>
            Right(soFar)
          case Some(row) =>
            opened(archive, s"$day/${row.file.getName}") match
              case Left(rejected) if rejected.status == 400 =>
                Left(rejected)
              case Left(_) =>
                Right(soFar)
              case Right(resolved) =>
                Right(soFar :+ (ext -> resolved.toFile))

  private def sendStream(exchange: HttpExchange, file: File, contentType: String): Unit =
    exchange.getResponseHeaders.set("Content-Type", contentType)
    exchange.sendResponseHeaders(200, file.length)
    val out = exchange.getResponseBody
    val in = NFiles.newInputStream(file.toPath)
    try in.transferTo(out)
    finally
      in.close()
      out.close()

  private def post(exchange: HttpExchange)(body: => Either[Rejected, (Int, Json)]): Unit =
    try
      body match
        case Left(rejected) =>
          if rejected.status >= 500 then Console.err.println(rejected.message)
          sendError(exchange, rejected.status, rejected.message)
        case Right((status, json)) =>
          val bytes: Array[Byte] = Serve.write(json).getBytes(StandardCharsets.UTF_8)
          send(exchange, status, "application/json; charset=utf-8", bytes)
    catch case error: IllegalArgumentException =>
      val message: String = Operation.messageOf(error)
      val shown: String = if message.contains("AF1Qip") then "failed" else message
      val status: Int = if serverFailure(shown) then 500 else 400
      if status >= 500 then Console.err.println(shown)
      sendError(exchange, status, shown)

  private def serverFailure(message: String): Boolean =
    message == "album file is busy" ||
      message.contains("include array does not end") ||
      message.contains("include is not a single array")

  private def readJsonBody(exchange: HttpExchange): Either[Rejected, Json] =
    val header: String = Option(exchange.getRequestHeaders.getFirst("Content-Type")).getOrElse("")
    val media: String = header.takeWhile(_ != ';').trim.toLowerCase
    if media != "application/json" then Left(Rejected(415, "unsupported media type"))
    else
      val text: String =
        val in = exchange.getRequestBody
        try new String(in.readAllBytes(), StandardCharsets.UTF_8)
        finally in.close()
      Serve.readJson(text) match
        case Right((value, rest)) if rest.forall(_.isWhitespace) => Right(value)
        case _ => Left(Rejected(400, "not json"))

  private def bodyFields(json: Json): Either[Rejected, Seq[(String, Json)]] = json match
    case Json.Obj(fields) => Right(fields)
    case _ => Left(Rejected(400, "not json"))

  private def stringField(fields: Seq[(String, Json)], key: String): Option[String] =
    fields.collectFirst:
      case (name, Json.Str(value)) if name == key => value

  // Read, toggle, and splice share one lock. An include taken before tryLock is not written.
  private def toggle(exchange: HttpExchange): Either[Rejected, (Int, Json)] =
    val archive: File = currentSession().root.configuration.archive
    readJsonBody(exchange).flatMap(bodyFields).flatMap: fields =>
      val title: String = stringField(fields, "title").getOrElse("")
      val path: String = stringField(fields, "path").getOrElse("")
      Title.parse(title) match
        case None =>
          Left(Rejected(400, s"not an album title: $title"))
        case Some(parsed) =>
          (try Album.picturePath(path)
          catch case _: DateTimeException => None) match
            case None =>
              Left(Rejected(400, s"not a picture path: $path"))
            case Some(pair) =>
              val file: File = Title.file(archive, parsed)
              Album.withWriteLock:
                if !file.isFile then Left(Rejected(404, s"no such album: ${parsed.title}"))
                else
                  val loaded: Album = Album.read(archive, file)
                  if loaded.span.exists(span => !span.contains(pair._1)) then
                    Left(Rejected(400, s"$path is outside ${loaded.title}"))
                  else
                    val dates: Seq[LocalDate] = loaded.span match
                      case Some(span) => AgentCommands.eachDay(span)
                      case None => (curatedDays(loaded.include) :+ pair._1).distinct
                    stemUniverse(archive, dates).flatMap: universe =>
                      val stems: Set[String] = universe.getOrElse(pair._1, Set.empty)
                      if !stems.contains(pair._2) then Left(Rejected(400, s"no such picture: $path"))
                      else
                        val next: Seq[Include.Entry] =
                          Include.toggle(loaded.include, loaded.span, pair, universe)
                        Album.spliceInclude(file, next)
                        val desired: Boolean =
                          next.exists(entry => Include.claims(loaded.span, entry, pair._1, pair._2))
                        Right((200, Json.Obj(Seq(
                          "title" -> Json.Str(loaded.title),
                          "path" -> Json.Str(path),
                          "desired" -> Json.Bool(desired),
                          "include" -> Json.Arr(next.map(entry => Json.Str(Include.logical(entry))))
                        ))))

  private def createAlbum(exchange: HttpExchange): Either[Rejected, (Int, Json)] =
    try
      val session: Session = currentSession()
      val archive: File = session.root.configuration.archive
      readJsonBody(exchange).flatMap(bodyFields).flatMap: fields =>
        dayField(fields, "start").flatMap: startDay =>
          dayField(fields, "end").flatMap: endDay =>
            nameField(fields).flatMap: name =>
              if endDay.isBefore(startDay) then Left(Rejected(400, "not a span"))
              else createdTitle(startDay, endDay, name).flatMap: parsed =>
                val dest: File = Title.file(archive, parsed)
                val parent: File = dest.getParentFile
                if parent == null || !parent.isDirectory then Left(Rejected(400, "no such directory"))
                else if dest.exists then Left(Rejected(409, s"album already exists: ${parsed.title}"))
                else seedAlbum(session, archive, parsed).flatMap: (album, seed) =>
                  val written: File = Album.create(archive, album)
                  Right((201, Json.Obj(Seq(
                    "title" -> Json.Str(parsed.title),
                    "path" -> Json.Str(Album.relative(archive, written)),
                    "external" -> Json.Bool(album.external),
                    "seed" -> Json.Str(seed)
                  ))))
    catch case error: IllegalArgumentException =>
      val message: String = Operation.messageOf(error)
      val shown: String = if message.contains("AF1Qip") then "failed" else message
      if shown.startsWith("album already exists:") then Left(Rejected(409, shown))
      else if serverFailure(shown) then Left(Rejected(500, shown))
      else Left(Rejected(400, shown))

  // No catalog title is all and not external. A readable match is external, with no google-album-id.
  private def seedAlbum(
    session: Session,
    archive: File,
    parsed: Title.Parsed
  ): Either[Rejected, (Album, String)] =
    val span: Title.Span = parsed.span.getOrElse(throw IllegalArgumentException("not a span"))
    val view: CatalogView = viewCatalog(session)
    if view.present && !view.readable then Left(Rejected(400, "catalog is unreadable"))
    else view.albums.filter(_.title == parsed.title) match
      case matched if matched.sizeIs > 1 =>
        Left(Rejected(400, "ambiguous catalog title"))
      case Seq(found) if found.autoAdd || found.error.isDefined =>
        Left(Rejected(400, "catalog album is not readable"))
      case Seq(found) =>
        val (gathered, hits, _) = AgentCommands.gather(session, found)
        publish(session, gathered)
        val claimed: Set[(LocalDate, String)] = hits.map(hit => hit._1 -> hit._3).toSet
        if claimed.exists(pair => !span.contains(pair._1)) then
          throw IllegalArgumentException("matched day outside the span")
        stemUniverse(archive, AgentCommands.eachDay(span)).map: universe =>
          val include: Seq[Include.Entry] =
            if claimed.isEmpty then Seq.empty
            else Include.compress(Some(span), claimed, universe)
          val album: Album = found.owner match
            case Some(label) =>
              Album.external(parsed.title, Some(span), include, Some(found.title), Some(label))
            case None =>
              Album.external(parsed.title, Some(span), include, None, None)
          (album, "matched")
      case _ =>
        Right((
          Album.pending(parsed.title, Some(span), Seq(Include.Entry.All(Seq.empty))),
          "all"
        ))

  private def createdTitle(
    start: LocalDate,
    end: LocalDate,
    name: Option[String]
  ): Either[Rejected, Title.Parsed] =
    val parsed: Title.Parsed = Title.Parsed(Some(Title.Span(start, end, Title.Grain.Day)), name)
    Title.parse(parsed.title) match
      case Some(again) if again.title == parsed.title => Right(parsed)
      case _ => Left(Rejected(400, s"not an album title: ${parsed.title}"))

  private def dayField(fields: Seq[(String, Json)], key: String): Either[Rejected, LocalDate] =
    stringField(fields, key).flatMap(civilDay) match
      case Some(day) => Right(day)
      case None => Left(Rejected(400, "not a day"))

  private def nameField(fields: Seq[(String, Json)]): Either[Rejected, Option[String]] =
    fields.collectFirst { case ("name", value) => value } match
      case None =>
        Right(None)
      case Some(Json.Str("")) =>
        Right(None)
      case Some(Json.Str(text)) if Title.legalName(text) =>
        Right(Some(text))
      case Some(Json.Str(text)) =>
        Left(Rejected(400, s"not an album name: $text"))
      case Some(_) =>
        Left(Rejected(400, "not an album name"))

  private def civilDay(text: String): Option[LocalDate] =
    text.split('/') match
      case Array(year, month, day)
          if year.length == 4 && month.length == 2 && day.length == 2 &&
            year.forall(_.isDigit) && month.forall(_.isDigit) && day.forall(_.isDigit) =>
        try Some(LocalDate.of(year.toInt, month.toInt, day.toInt))
        catch case _: DateTimeException => None
      case _ =>
        None

  private def stemUniverse(
    archive: File,
    dates: Seq[LocalDate]
  ): Either[Rejected, Map[LocalDate, Set[String]]] =
    groupedDays(archive, dates).map: rows =>
      rows.foldLeft(Map.empty[LocalDate, Set[String]]): (acc, row) =>
        val names: Set[String] = row._2.map(_._1).toSet
        if names.isEmpty then acc else acc.updated(row._1, names)
