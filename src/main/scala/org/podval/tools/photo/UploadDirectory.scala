package org.podval.tools.photo

import java.io.File

final class UploadDirectory(
  override val root: Root,
  override val name: String
) extends Pictures[UploadDirectory]:

  override protected def getDirectory: File = File(root.directory, name)

  override def day: Option[Day] = None
