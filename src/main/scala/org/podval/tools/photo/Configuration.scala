package org.podval.tools.photo

import org.podval.tools.toml.TomlCodec
import zio.schema.codec.DecodeError
import zio.schema.{DeriveSchema, Schema}
import java.io.File
import java.time.ZoneId

final case class Configuration(
  rootPath: String,
  byYearDirectoryName: String,
  uploadDirectoryName: String,
  zoneId: ZoneId
):
  private def rootDirectory: File = File(Configuration.homeDirectory, rootPath)
  def byYearDirectory: File = File(rootDirectory, byYearDirectoryName)
  def uploadDirectory: File = File(rootDirectory, uploadDirectoryName)

object Configuration:
  private def homeDirectory: File = Option(System.getenv("HOME"))
    .map(File(_))
    .getOrElse(throw IllegalArgumentException(s"no HOME environment variable"))

  private def default: Configuration = Configuration(
    rootPath = "Pictures",
    byYearDirectoryName = "originals",
    uploadDirectoryName = "Upload",
    zoneId = ZoneId.of("America/New_York")
  )

  given Schema[Configuration] = DeriveSchema.gen

  private val configurationFileName: String = "org.podval.tools.photo.toml"

  private def configurationFile: File = File(File(homeDirectory, ".config"), configurationFileName)

  def read: Configuration =
    if !configurationFile.exists() then write(default)

    TomlCodec.decode[Configuration](Files.read(configurationFile)) match
      case Left(error: DecodeError) => throw IllegalArgumentException(s"Error parsing configuration file '$configurationFile': ${error.getMessage}")
      case Right(value) => value

  def write(configuration: Configuration): Unit =
    Files.write(TomlCodec.encode(configuration), configurationFile)
