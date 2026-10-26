package org.podval.tools.photo

import com.google.api.client.json.GenericJson
import com.google.api.client.json.JsonObjectParser
import com.google.api.client.json.gson.GsonFactory
import com.google.auth.oauth2.{ClientId, DefaultPKCEProvider, TokenStore, UserAuthorizer}
import java.awt.Desktop
import java.io.{BufferedReader, File, FileInputStream, IOException, InputStreamReader}
import java.net.{InetSocketAddress, ServerSocket, Socket, SocketTimeoutException, URI, URLDecoder}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.security.SecureRandom
import java.util.Base64
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.SeqHasAsJava

object GoogleAuth:
  private val tokenUser: String = "user"
  private val loginWaitMs: Int = 3 * 60 * 1000

  private val scopes: java.util.Collection[String] = Seq(
    "https://www.googleapis.com/auth/photoslibrary.appendonly",
    "https://www.googleapis.com/auth/photoslibrary.readonly.appcreateddata",
    "https://www.googleapis.com/auth/photoslibrary.edit.appcreateddata"
  ).asJava

  private def homeDirectory: File = Option(System.getenv("HOME"))
    .map(File(_))
    .getOrElse(throw IllegalArgumentException("no HOME environment variable"))

  private def configDirectory: File = File(homeDirectory, ".config")

  private def clientJson: File = File(configDirectory, "org.podval.tools.photo.client.json")

  private def tokenJson: File = File(configDirectory, "org.podval.tools.photo.token.json")

  def redirectHost(redirect: URI): String =
    val host = redirect.getHost
    if host == null then throw IllegalArgumentException(s"redirect URI has no host: $redirect")
    else host

  def requireState(expected: String, actual: Option[String]): Unit =
    if actual != Some(expected) then throw IllegalArgumentException("OAuth state mismatch")

  def missingClientJson(file: File): String = s"missing OAuth client JSON: ${file.getAbsolutePath}"

  def requireClientJson(file: File): Unit =
    if !file.isFile then throw IllegalArgumentException(missingClientJson(file))

  def writeToken(file: File, contents: String): Unit =
    val path = file.toPath
    Option(path.getParent).foreach(java.nio.file.Files.createDirectories(_))
    val perms = PosixFilePermissions.fromString("rw-------")
    if !java.nio.file.Files.exists(path) then
      java.nio.file.Files.createFile(path, PosixFilePermissions.asFileAttribute(perms))
    java.nio.file.Files.writeString(path, contents)
    java.nio.file.Files.setPosixFilePermissions(path, perms)

  def login(clientFile: File = clientJson, tokenFile: File = tokenJson): Unit =
    val (clientId, redirect) = readClient(clientFile)
    val host = redirectHost(redirect)
    val sockets = openLoopback(host)
    try
      val port = sockets.head.getLocalPort
      val rawPath = redirect.getRawPath
      val path = if rawPath == null || rawPath.isEmpty then "" else rawPath
      val callback = new URI("http", null, host, port, path, null, null)
      val pkce = new DefaultPKCEProvider()
      val authorizer = UserAuthorizer.newBuilder()
        .setClientId(clientId)
        .setScopes(scopes)
        .setCallbackUri(callback)
        .setPKCEProvider(pkce)
        .setTokenStore(TokenFileStore(tokenFile))
        .build()
      val state = newState()
      browse(authorizer.getAuthorizationUrl(
        null,
        state,
        callback,
        java.util.Map.of("include_granted_scopes", "false")
      ))
      val params = readCallback(sockets)
      requireState(state, params.get("state"))
      if params.contains("error") then throw IllegalArgumentException("Google login failed")
      val code = params.get("code").filter(_.nonEmpty).getOrElse(
        throw IllegalArgumentException("Google login returned no authorization code")
      )
      val credentials = authorizer.getCredentialsFromCode(code, callback)
      authorizer.storeCredentials(tokenUser, credentials)
      println(s"logged in (${tokenFile.getAbsolutePath})")
    finally
      sockets.foreach(_.close())

  def status(tokenFile: File = tokenJson): Unit =
    if tokenFile.isFile then println(s"logged in (${tokenFile.getAbsolutePath})")
    else println(s"not logged in (${tokenFile.getAbsolutePath})")

  def logout(tokenFile: File = tokenJson): Unit =
    if tokenFile.isFile then
      java.nio.file.Files.delete(tokenFile.toPath)
      println(s"logged out (${tokenFile.getAbsolutePath})")
    else
      println(s"not logged in (${tokenFile.getAbsolutePath})")

  private[photo] def readClient(file: File): (ClientId, URI) =
    requireClientJson(file)
    val json =
      try
        val stream = new FileInputStream(file)
        new JsonObjectParser(GsonFactory.getDefaultInstance).parseAndClose(
          stream,
          StandardCharsets.UTF_8,
          classOf[GenericJson]
        )
      catch
        case _: IOException =>
          throw IllegalArgumentException(s"cannot parse OAuth client JSON: ${file.getAbsolutePath}")
        case _: RuntimeException =>
          throw IllegalArgumentException(s"cannot parse OAuth client JSON: ${file.getAbsolutePath}")
    val detail = Option(json.get("installed")).orElse(Option(json.get("web"))) match
      case Some(map: java.util.Map[?, ?]) => map
      case _ =>
        throw IllegalArgumentException(s"OAuth client JSON has no installed or web client: ${file.getAbsolutePath}")
    val clientId =
      try ClientId.fromJson(json)
      catch
        case _: IOException =>
          throw IllegalArgumentException(s"cannot parse OAuth client JSON: ${file.getAbsolutePath}")
    val redirect = detail.get("redirect_uris") match
      case uris: java.util.List[?] =>
        val values = uris.iterator()
        if !values.hasNext then
          throw IllegalArgumentException(s"OAuth client JSON has no redirect URI: ${file.getAbsolutePath}")
        else values.next() match
          case uri: String => URI.create(uri)
          case _ =>
            throw IllegalArgumentException(s"OAuth client JSON has no redirect URI: ${file.getAbsolutePath}")
      case _ =>
        throw IllegalArgumentException(s"OAuth client JSON has no redirect URI: ${file.getAbsolutePath}")
    (clientId, redirect)

  private[photo] def loopbackBindHosts(host: String): Seq[String] =
    host match
      case "localhost" => Seq("127.0.0.1", "::1")
      case "127.0.0.1" | "::1" => Seq(host)
      case _ => throw IllegalArgumentException(s"redirect host is not loopback: $host")

  private def openLoopback(host: String): Seq[ServerSocket] =
    val bindHosts = loopbackBindHosts(host)
    val first = new ServerSocket()
    try
      first.setReuseAddress(true)
      first.bind(new InetSocketAddress(bindHosts.head, 0))
      val port = first.getLocalPort
      val rest = bindHosts.drop(1).foldLeft(Seq.empty[ServerSocket]): (opened, bindHost) =>
        val socket = new ServerSocket()
        try
          socket.setReuseAddress(true)
          socket.bind(new InetSocketAddress(bindHost, port))
          opened :+ socket
        catch
          case e: Exception =>
            socket.close()
            opened.foreach(_.close())
            throw e
      val sockets = first +: rest
      sockets.foreach(_.setSoTimeout(200))
      sockets
    catch
      case e: Exception =>
        first.close()
        throw e

  private def newState(): String =
    val bytes = new Array[Byte](32)
    SecureRandom().nextBytes(bytes)
    Base64.getUrlEncoder.withoutPadding.encodeToString(bytes)

  private def browse(url: java.net.URL): Unit =
    try
      if !Desktop.isDesktopSupported || !Desktop.getDesktop.isSupported(Desktop.Action.BROWSE) then
        throw IllegalStateException("cannot open a browser for Google login")
      Desktop.getDesktop.browse(url.toURI)
    catch
      case _: Exception =>
        throw IllegalStateException("cannot open a browser for Google login")

  private def readCallback(sockets: Seq[ServerSocket]): Map[String, String] =
    val socket = acceptLoopback(sockets)
    try
      socket.setSoTimeout(loginWaitMs)
      val reader = BufferedReader(InputStreamReader(socket.getInputStream, StandardCharsets.UTF_8))
      val line =
        try Option(reader.readLine()).getOrElse("")
        catch
          case _: SocketTimeoutException =>
            throw IllegalStateException("Google login timed out")
      val params = queryParameters(line)
      val body = "You can close this window.".getBytes(StandardCharsets.UTF_8)
      val header =
        s"HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: ${body.length}\r\nConnection: close\r\n\r\n"
      val out = socket.getOutputStream
      out.write(header.getBytes(StandardCharsets.US_ASCII))
      out.write(body)
      out.flush()
      params
    finally
      socket.close()

  @tailrec
  private def acceptLoopback(sockets: Seq[ServerSocket], deadlineMs: Long = System.currentTimeMillis() + loginWaitMs): Socket =
    if System.currentTimeMillis() >= deadlineMs then
      throw IllegalStateException("Google login timed out")
    else
      val accepted = sockets.foldLeft(Option.empty[Socket]): (found, server) =>
        found.orElse:
          try Some(server.accept())
          catch case _: SocketTimeoutException => None
      accepted match
        case Some(socket) => socket
        case None => acceptLoopback(sockets, deadlineMs)

  private def queryParameters(requestLine: String): Map[String, String] =
    val target = requestLine.split(" ", 3) match
      case Array(_, value, _*) => value
      case _ => ""
    val query = target.span(_ != '?') match
      case (_, rest) if rest.nonEmpty => rest.drop(1)
      case _ => ""
    if query.isEmpty then Map.empty
    else query.split('&').toList.foldLeft(Map.empty[String, String]): (acc, pair) =>
      val (key, rest) = pair.span(_ != '=')
      val value = if rest.isEmpty then "" else rest.drop(1)
      acc.updated(
        URLDecoder.decode(key, StandardCharsets.UTF_8),
        URLDecoder.decode(value, StandardCharsets.UTF_8)
      )

  private final class TokenFileStore(file: File) extends TokenStore:
    override def load(id: String): String =
      if file.isFile then java.nio.file.Files.readString(file.toPath) else null

    override def store(id: String, tokens: String): Unit = writeToken(file, tokens)

    override def delete(id: String): Unit =
      if file.isFile then java.nio.file.Files.delete(file.toPath)

case object Login extends Operation:
  override def execute(): Unit = GoogleAuth.login()

case object Status extends Operation:
  override def execute(): Unit = GoogleAuth.status()

case object Logout extends Operation:
  override def execute(): Unit = GoogleAuth.logout()
