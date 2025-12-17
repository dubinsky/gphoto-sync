package org.podval.tools.photo

import java.io.File

final class Root(val configuration: Configuration) extends Directory:
  override def root: Root = this

  override def name: String = ""
  
  override def toString: String = name

  override protected def getDirectory: File = configuration.byYearDirectory

  override def lsd: Seq[Directory] = uploadDirectory +: Year.lsd(this)

  override def item(name: String): Directory =
    if name == configuration.uploadDirectoryName
    then uploadDirectory
    else Year(this, name)

  def year(num: Int): Year = Year(this, num)

  private val uploadDirectory: UploadDirectory = UploadDirectory(this, configuration.uploadDirectoryName)

  def parse(path: String): Path = item(path.split("/").filter(_.nonEmpty).toList)

  def parsePicture(path: String): Picture = parse(path) match
    case picture: Picture => picture
    case path => throw IllegalArgumentException(s"'$path' is not a picture")

object Root:
  lazy val get: Root = Root(Configuration.read)
