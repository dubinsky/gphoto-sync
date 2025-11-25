package org.podval.tools.photo

trait DirectoryOfNumbered[
  Self <: Directory[Self, Int, Num],
  Num <: Sub[Self, Int, Num] & Directory[Num, ItemSelector, Item],
  ItemSelector,
  Item <: Sub[Num, ItemSelector, Item]
]:
  self: Self =>

  def numberedItemCompanion: NumberedCompanion[Self, Num, ItemSelector, Item]

  final def lsd: Seq[Num] = Files.listDirectories(directory).map(numberedItemCompanion(this, _))

  final def item(number: Int): Num = numberedItemCompanion(this, number)
