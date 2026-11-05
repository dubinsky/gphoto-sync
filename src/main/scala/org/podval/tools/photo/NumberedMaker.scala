package org.podval.tools.photo

trait NumberedMaker[
  Parent <: Directory,
  What <: ItemDirectory
]:
  def kind: String

  final def lsd(parent: Parent): Seq[Directory] =
    Files
      .listDirectories(parent.directory)
      .flatMap: file =>
        val name: String = file.getName
        if name.startsWith(".") then None
        else
          val number: Int = name.toInt
          require(name == toString(number))
          Some(apply(parent, number))

  final def apply(parent: Parent, name: String): What =
    if name.startsWith(".") then throw IllegalArgumentException(s"'$name' is not a $kind")
    else apply(parent, name.toInt)

  def apply(parent: Parent, number: Int): What

  def toString(number: Int): String
