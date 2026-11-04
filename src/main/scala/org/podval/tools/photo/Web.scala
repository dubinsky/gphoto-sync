package org.podval.tools.photo

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.io.File
import java.net.{BindException, InetSocketAddress}
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.file.{Files as NFiles, StandardOpenOption}
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.{CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

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

  private val lockName: String = "web.lock"
  private val fileMode: String = "rw-------"

  private final case class Held(runtime: File, channel: FileChannel, lock: FileLock)

  private val owner = new AtomicReference[FileChannel](null)
  private val serverRef = new AtomicReference[HttpServer](null)
  private val runtimeRef = new AtomicReference[Option[File]](None)
  private val gate = new AtomicReference[CountDownLatch](null)
  private val signalsInstalled = new AtomicBoolean(false)
  // One process session. gather reuses its DirectoryIndex. It is not serve's index.
  private val sessionRef = new AtomicReference[Session](null)
  // A collected channel is closed, and that close drops this JVM's fcntl lock on web.lock.
  private var pinned: List[FileChannel] = Nil

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
    WebRoutes.dispatch(exchange)

  private[photo] def currentSession(): Session =
    val seen: Session = sessionRef.get
    if seen != null then seen else throw IllegalStateException("photo web has no archive session")

  // getAndUpdate can retry. The listing that produced next stays outside this function.
  private[photo] def publish(start: Session, next: Session): Unit =
    sessionRef.getAndUpdate(seen => if seen eq start then next else seen)
