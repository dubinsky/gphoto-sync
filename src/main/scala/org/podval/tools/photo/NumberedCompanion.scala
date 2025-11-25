package org.podval.tools.photo

import java.io.File

trait NumberedCompanion[
  Parent <: Directory[Parent, Int, Num],
  Num <: Sub[Parent, Int, Num] & Directory[Num, ItemSelector, Item],
  ItemSelector,
  Item <: Sub[Num, ItemSelector, Item]
]:
  final def apply(parent: Parent, directory: File): Num =
    val name: String = directory.getName
    val number: Int = name.toInt
    require(toString(number) == name)
    apply(parent: Parent, number)

  def apply(parent: Parent, number: Int): Num

  def toString(number: Int): String
