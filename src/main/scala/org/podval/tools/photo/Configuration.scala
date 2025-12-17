package org.podval.tools.photo

import java.time.ZoneId

final class Configuration(
  val rootPath: String,
  val uploadDirectoryName: String,
  val zoneId: ZoneId
)
  
object Configuration:
  private def home: String = Option(System.getenv("HOME"))
    .getOrElse(throw IllegalArgumentException(s"no HOME environment variable"))

  // TODO read from configuration file!
  def get: Configuration = Configuration(
    rootPath = s"$home/Pictures/originals",
    uploadDirectoryName = "Upload",
    zoneId = ZoneId.of("America/New_York")
  )
  