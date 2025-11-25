package org.podval.tools.photo

trait Path:
  def root: Root
  
  def name: String

  def lsd(isLong: Boolean): Unit

  def forEachPicture(filter: Filter, action: Picture => Unit): Unit
