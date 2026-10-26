package org.podval.tools.photo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.File
import java.net.URI
import java.nio.file.attribute.PosixFilePermissions

class GoogleAuthTest extends AnyFlatSpec, Matchers:
  "loopbackBindHosts" should "listen on both loopback addresses only for localhost" in :
    GoogleAuth.loopbackBindHosts("localhost") shouldBe Seq("127.0.0.1", "::1")
    GoogleAuth.loopbackBindHosts("LOCALHOST") shouldBe Seq("127.0.0.1", "::1")
    GoogleAuth.loopbackBindHosts("127.0.0.1") shouldBe Seq("127.0.0.1")
    GoogleAuth.loopbackBindHosts("::1") shouldBe Seq("::1")
    GoogleAuth.loopbackBindHosts("[::1]") shouldBe Seq("::1")
    GoogleAuth.loopbackBindHosts("0:0:0:0:0:0:0:1") shouldBe Seq("::1")
    GoogleAuth.loopbackBindHosts(GoogleAuth.redirectHost(URI.create("http://[::1]/cb"))) shouldBe Seq("::1")
    val rejected = intercept[IllegalArgumentException]:
      GoogleAuth.loopbackBindHosts("example.com")
    rejected.getMessage shouldBe "redirect host is not loopback: example.com"

  "redirectHost" should "take the host from the redirect URI" in :
    GoogleAuth.redirectHost(URI.create("http://localhost:1234/cb")) shouldBe "localhost"
    GoogleAuth.redirectHost(URI.create("http://127.0.0.1/cb")) shouldBe "127.0.0.1"

  "requireState" should "reject a mismatch" in :
    val mismatch = intercept[IllegalArgumentException]:
      GoogleAuth.requireState("expected-state", Some("other-state"))
    mismatch.getMessage shouldBe "OAuth state mismatch"
    intercept[IllegalArgumentException]:
      GoogleAuth.requireState("expected-state", None)
    GoogleAuth.requireState("expected-state", Some("expected-state"))

  "readClient" should "take the redirect host from the client JSON" in :
    val secret = "client-secret-should-not-appear"
    val dir = java.nio.file.Files.createTempDirectory("gphoto-auth-test")
    val file = dir.resolve("client.json").toFile
    try
      java.nio.file.Files.writeString(
        file.toPath,
        s"""{"installed":{"client_id":"test-client-id","client_secret":"$secret","redirect_uris":["http://127.0.0.1/cb"],"auth_uri":"https://accounts.google.com/o/oauth2/auth","token_uri":"https://oauth2.googleapis.com/token"}}"""
      )
      val (clientId, redirect) = GoogleAuth.readClient(file)
      GoogleAuth.redirectHost(redirect) shouldBe "127.0.0.1"
      redirect.getRawPath shouldBe "/cb"
      clientId.getClientId shouldBe "test-client-id"
      Option(clientId.getClientSecret).exists(_.nonEmpty) shouldBe true
    finally
      java.nio.file.Files.deleteIfExists(file.toPath)
      java.nio.file.Files.deleteIfExists(dir)

  "login" should "report a missing client JSON path without reading a secret" in :
    val secret = "client-secret-should-not-appear"
    val dir = java.nio.file.Files.createTempDirectory("gphoto-auth-test")
    val decoy = dir.resolve("decoy.json").toFile
    val missing = dir.resolve("org.podval.tools.photo.client.json").toFile
    val token = dir.resolve("org.podval.tools.photo.token.json").toFile
    try
      java.nio.file.Files.writeString(decoy.toPath, s"""{"installed":{"client_secret":"$secret"}}""")
      val ex = intercept[IllegalArgumentException]:
        GoogleAuth.login(missing, token)
      ex.getMessage shouldBe GoogleAuth.missingClientJson(missing)
      ex.getMessage.contains(secret) shouldBe false
      token.exists shouldBe false
      java.nio.file.Files.readString(decoy.toPath) should include (secret)
    finally
      java.nio.file.Files.deleteIfExists(decoy.toPath)
      java.nio.file.Files.deleteIfExists(token.toPath)
      java.nio.file.Files.deleteIfExists(missing.toPath)
      java.nio.file.Files.deleteIfExists(dir)

  "writeToken" should "set mode 0600 on the token file it creates" in :
    val dir = java.nio.file.Files.createTempDirectory("gphoto-auth-test")
    val file = File(dir.toFile, "token.json")
    try
      file.getAbsolutePath.startsWith(dir.toAbsolutePath.toString) shouldBe true
      GoogleAuth.writeToken(file, """{"refresh_token":"test-only"}""")
      val perms = java.nio.file.Files.getPosixFilePermissions(file.toPath)
      PosixFilePermissions.toString(perms) shouldBe "rw-------"
      java.nio.file.Files.readString(file.toPath) shouldBe """{"refresh_token":"test-only"}"""
    finally
      java.nio.file.Files.deleteIfExists(file.toPath)
      java.nio.file.Files.deleteIfExists(dir)
