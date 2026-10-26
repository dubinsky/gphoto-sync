package org.podval.tools.photo

import com.google.api.client.json.GenericJson
import com.google.api.client.json.JsonObjectParser
import com.google.api.client.json.gson.GsonFactory
import com.google.auth.oauth2.{ClientId, DefaultPKCEProvider, TokenStore, UserAuthorizer}
import java.awt.Desktop
import java.io.{BufferedReader, File, FileInputStream, IOException, InputStreamReader}
import java.net.{InetSocketAddress, ServerSocket, URI, URLDecoder}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.security.SecureRandom
import java.util.Base64
import scala.jdk.CollectionConverters.SeqHasAsJava

object GoogleAuth:
  private val tokenUser: String = "user"

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
    java.nio.file.Files.writeString(path, contents)
    java.nio.file.Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rw-------"))

  def login(clientFile: File = clientJson, tokenFile: File = tokenJson): Unit =
    val (clientId, redirect, authUri, tokenUri) = readClient(clientFile)
    val host = redirectHost(redirect)
    val server = new ServerSocket()
    try
      server.bind(new InetSocketAddress(host, 0))
      val port = server.getLocalPort
      val rawPath = redirect.getRawPath
      val path = if rawPath == null || rawPath.isEmpty then "" else rawPath
      val callback = new URI("http", null, host, port, path, null, null)
      val pkce = new DefaultPKCEProvider()
      val builder = UserAuthorizer.newBuilder()
        .setClientId(clientId)
        .setScopes(scopes)
        .setCallbackUri(callback)
        .setPKCEProvider(pkce)
        .setTokenStore(TokenFileStore(tokenFile))
      authUri.foreach(uri => builder.setUserAuthUri(uri))
      tokenUri.foreach(uri => builder.setTokenServerUri(uri))
      val authorizer = builder.build()
      val state = newState()
      browse(authorizer.getAuthorizationUrl(null, state, callback))
      val params = readCallback(server)
      requireState(state, params.get("state"))
      if params.contains("error") then throw IllegalArgumentException("Google login failed")
      val code = params.get("code").filter(_.nonEmpty).getOrElse(
        throw IllegalArgumentException("Google login returned no authorization code")
      )
      val credentials = authorizer.getCredentialsFromCode(code, callback)
      authorizer.storeCredentials(tokenUser, credentials)
      println(s"logged in (${tokenFile.getAbsolutePath})")
    finally
      server.close()

  def status(tokenFile: File = tokenJson): Unit =
    if tokenFile.isFile then println(s"logged in (${tokenFile.getAbsolutePath})")
    else println(s"not logged in (${tokenFile.getAbsolutePath})")

  def logout(tokenFile: File = tokenJson): Unit =
    if tokenFile.isFile then
      java.nio.file.Files.delete(tokenFile.toPath)
      println(s"logged out (${tokenFile.getAbsolutePath})")
    else
      println(s"not logged in (${tokenFile.getAbsolutePath})")

  private[photo] def readClient(file: File): (ClientId, URI, Option[URI], Option[URI]) =
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
    (clientId, redirect, stringField(detail, "auth_uri").map(URI.create), stringField(detail, "token_uri").map(URI.create))

  private def stringField(detail: java.util.Map[?, ?], key: String): Option[String] =
    detail.get(key) match
      case value: String if value.nonEmpty => Some(value)
      case _ => None

  private def newState(): String =
    val bytes = new Array[Byte](32)
    SecureRandom().nextBytes(bytes)
    Base64.getUrlEncoder.withoutPadding.encodeToString(bytes)

  private def browse(url: java.net.URL): Unit =
    if !Desktop.isDesktopSupported || !Desktop.getDesktop.isSupported(Desktop.Action.BROWSE) then
      throw IllegalStateException("cannot open a browser for Google login")
    else
      Desktop.getDesktop.browse(url.toURI)

  private def readCallback(server: ServerSocket): Map[String, String] =
    val socket = server.accept()
    try
      val reader = BufferedReader(InputStreamReader(socket.getInputStream, StandardCharsets.UTF_8))
      val line = Option(reader.readLine()).getOrElse("")
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
