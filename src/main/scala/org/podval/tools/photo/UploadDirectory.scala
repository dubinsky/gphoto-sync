package org.podval.tools.photo

import java.io.File

object UploadDirectory:
  val directoryName: String = ".upload"

final class UploadDirectory(
    override val root: Root,
    override val name: String
) extends Pictures:
  override def toString: String = s"$root/$name"

  override def day: Option[Day] = None

  override protected def getDirectory: File = root.configuration.uploadDirectory
