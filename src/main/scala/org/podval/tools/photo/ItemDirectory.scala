package org.podval.tools.photo

import java.io.File

trait ItemDirectory extends Item with Directory:
  final override protected def getDirectory: File = File(parent.directory, name)
