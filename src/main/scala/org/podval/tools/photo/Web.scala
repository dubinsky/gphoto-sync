package org.podval.tools.photo

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.io.File
import java.net.{BindException, InetSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.file.{FileSystemException, Files as NFiles, NoSuchFileException, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.{CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.annotation.tailrec

final case class Web(port: Int) extends Operation:
  override def execute(session: Session): (Session, Result) =
    Serve.runtimeDirectory() match
      case Left(message) =>
        (session, Result.Usage(message))
      case Right(runtime) =>
        Web.start(runtime, port) match
          case Left(message) =>
            (session, Result.Usage(message))
          case Right(_) =>
            (session, Result.Ok("", Json.Null))

object Web:
  private[photo] val defaultPort: Int = 8765

  private[photo] final case class Rejected(status: Int, message: String)

  private val loopback: String = "127.0.0.1"
  private val alreadyRunning: String = "photo web is already running"
  private val escapeMessage: String = "path escapes the archive"
  private val missingPicture: String = "no such picture"
  private val lockName: String = "web.lock"
  private val fileMode: String = "rw-------"

  private final case class Held(runtime: File, channel: FileChannel, lock: FileLock)
  private final case class Route(method: String, path: String, run: HttpExchange => Unit)

  private val owner = new AtomicReference[FileChannel](null)
  private val serverRef = new AtomicReference[HttpServer](null)
  private val runtimeRef = new AtomicReference[Option[File]](None)
  private val gate = new AtomicReference[CountDownLatch](null)
  private val signalsInstalled = new AtomicBoolean(false)
  // A collected channel is closed, and that close drops this JVM's fcntl lock on web.lock.
  private var pinned: List[FileChannel] = Nil

  // Append a route here.
  private val routes: List[Route] = List(
    Route("GET", "/", exchange => sendResource(exchange, "index.html", "text/html; charset=utf-8")),
    Route("GET", "/app.js", exchange => sendResource(exchange, "app.js", "text/javascript; charset=utf-8")),
    Route("GET", "/app.css", exchange => sendResource(exchange, "app.css", "text/css; charset=utf-8"))
  )

  private[photo] def handlerRuntime: Option[File] = runtimeRef.get

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
    percentDecode(raw).flatMap(lexical).flatMap(names => confine(archive, names))

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
