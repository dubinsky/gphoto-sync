package org.podval.tools.photo

trait NumberedMaker[
  Parent <: Directory,
  What <: ItemDirectory
]:
  final def lsd(parent: Parent): Seq[Directory] = Files.listDirectories(parent.directory).map: file =>
    val name: String = file.getName
    require(name == toString(name.toInt))
    apply(parent, name.toInt)

  final def apply(parent: Parent, name: String): What =
    apply(parent, name.toInt)

  def apply(parent: Parent, number: Int): What

  def toString(number: Int): String
