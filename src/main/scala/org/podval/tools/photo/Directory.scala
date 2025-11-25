package org.podval.tools.photo

import java.io.File

trait Directory[
  Self <: Directory[Self, Selector, Item],
  Selector,
  Item <: Sub[Self, Selector, Item]
] extends Path:
  self: Self =>

  final lazy val directory: File =
    val result: File = getDirectory
    require(!result.exists || result.isDirectory, s"'$result' is not a directory")
    result
  
  protected def getDirectory: File

  def lsd: Seq[Item]

  def item(selector: Selector): Item

  final override def lsd(isLong: Boolean): Unit = 
    println(this.toString)
    if isLong then lsd.foreach(_.lsd(isLong))
      
  final override def forEachPicture(filter: Filter, action: Picture => Unit): Unit =
    lsd.foreach(_.forEachPicture(filter, action))
