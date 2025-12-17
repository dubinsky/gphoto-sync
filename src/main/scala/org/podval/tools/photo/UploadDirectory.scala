package org.podval.tools.photo

import java.io.File

final class UploadDirectory(
  override val root: Root,
  override val name: String
) extends Pictures:

  override def day: Option[Day] = None

  override protected def getDirectory: File = root.configuration.uploadDirectory
