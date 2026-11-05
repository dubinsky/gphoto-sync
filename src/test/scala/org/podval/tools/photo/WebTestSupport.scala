package org.podval.tools.photo

import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, File}
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.Files as JFiles
import java.nio.file.attribute.PosixFilePermissions
import java.time.Duration
import java.util.concurrent.atomic.AtomicReference

private[photo] trait WebTestSupport extends Matchers:
  private[photo] val loopback: String = "127.0.0.1"

  private[photo] val client: HttpClient =
    HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(2)).build()

  private[photo] def running(runtime: File, port: Int)(body: => Unit): (String, String, Either[String, Unit]) =
    val out = new ByteArrayOutputStream
    val err = new ByteArrayOutputStream
    val result = new AtomicReference[Either[String, Unit]](null)
    val thread = new Thread(
      () =>
        Console.withOut(out):
          Console.withErr(err):
            result.set(Web.start(runtime, port))
      ,
      "web-test"
    )
    thread.setDaemon(true)
    thread.start()
    try
      waitFor(out, s"http://$loopback:$port/")
      body
    finally
      Web.stop()
      thread.join(5000)
    thread.isAlive shouldBe false
    (text(out), text(err), Option(result.get).getOrElse(Left("no result")))

  private def waitFor(out: ByteArrayOutputStream, needle: String): Unit =
    val deadline: Long = System.nanoTime() + 5000000000L
    def loop(): Unit =
      if text(out).contains(needle) then ()
      else if System.nanoTime() >= deadline then fail(s"not bound: ${text(out)}")
      else
        Thread.sleep(20)
        loop()
    loop()

  private[photo] def get(port: Int, path: String): HttpResponse[String] = send(port, path, "GET")

  private[photo] def send(port: Int, path: String, method: String): HttpResponse[String] =
    val request: HttpRequest = HttpRequest
      .newBuilder(URI.create(s"http://$loopback:$port$path"))
      .timeout(Duration.ofSeconds(2))
      .method(method, HttpRequest.BodyPublishers.noBody())
      .build()
    client.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))

  private[photo] def freePort(): Int =
    val socket = new java.net.ServerSocket(0)
    try socket.getLocalPort
    finally socket.close()

  private[photo] def perms(file: File): String =
    PosixFilePermissions.toString(JFiles.getPosixFilePermissions(file.toPath))

  private[photo] def text(bytes: ByteArrayOutputStream): String =
    new String(bytes.toByteArray, StandardCharsets.UTF_8)

  private[photo] def deleteTree(file: File): Unit =
    if JFiles.isSymbolicLink(file.toPath) then file.delete()
    else
      if file.isDirectory then Option(file.listFiles).foreach(_.foreach(deleteTree))
      file.delete()
