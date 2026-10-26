package org.podval.tools.photo

import io.github.wasabithumb.jtoml.JToml
import io.github.wasabithumb.jtoml.except.TomlException
import io.github.wasabithumb.jtoml.value.table.TomlTable
import java.io.File
import java.time.{DateTimeException, ZoneId}

final case class Configuration(
  rootPath: String,
  byYearDirectoryName: String,
  uploadDirectoryName: String,
  zoneId: ZoneId
) derives CanEqual:
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

  private val configurationFileName: String = "org.podval.tools.photo.toml"

  private def configurationFile: File = File(File(homeDirectory, ".config"), configurationFileName)

  def read: Configuration =
    if !configurationFile.exists() then write(default)
    try decode(Files.read(configurationFile))
    catch case exception: (IllegalArgumentException | TomlException) =>
      throw IllegalArgumentException(
        s"Error parsing configuration file '$configurationFile': ${exception.getMessage}",
        exception
      )

  def write(configuration: Configuration): Unit =
    Files.write(encode(configuration), configurationFile)

  private[photo] def decode(text: String): Configuration =
    val table: TomlTable =
      if text.isBlank then TomlTable.create()
      else JToml.jToml.readFromString(text)
    val rootPath: String = requiredString(table, "rootPath")
    val byYearDirectoryName: String = requiredString(table, "byYearDirectoryName")
    val uploadDirectoryName: String = requiredString(table, "uploadDirectoryName")
    val zone: String = requiredString(table, "zoneId")
    val zoneId: ZoneId =
      try ZoneId.of(zone)
      catch case exception: DateTimeException =>
        throw IllegalArgumentException(s"zoneId '$zone' is not a zone", exception)
    Configuration(
      rootPath = rootPath,
      byYearDirectoryName = byYearDirectoryName,
      uploadDirectoryName = uploadDirectoryName,
      zoneId = zoneId
    )

  private[photo] def encode(configuration: Configuration): String =
    val table: TomlTable = TomlTable.create()
    table.put("rootPath", configuration.rootPath)
    table.put("byYearDirectoryName", configuration.byYearDirectoryName)
    table.put("uploadDirectoryName", configuration.uploadDirectoryName)
    table.put("zoneId", configuration.zoneId.toString)
    JToml.jToml.writeToString(table)

  private def requiredString(table: TomlTable, key: String): String =
    Option(table.get(key)) match
      case None =>
        throw IllegalArgumentException(s"$key is required")
      case Some(value) if value.isPrimitive && value.asPrimitive.isString =>
        value.asPrimitive.asString
      case _ =>
        throw IllegalArgumentException(s"$key must be a string")
