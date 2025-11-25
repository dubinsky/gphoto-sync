package org.podval.tools.photo

trait DirectoryOfNumbered[
  Self <: Directory[Self, Int, Num],
  Num <: Sub[Self, Int, Num] & Directory[Num, ItemSelector, Item],
  ItemSelector,
  Item <: Sub[Num, ItemSelector, Item]
]:
  self: Self =>

  def numberedItemCompanion: NumberedCompanion[Self, Num, ItemSelector, Item]

  final override def lsd: Seq[Num] = Files.listDirectories(directory).map(numberedItemCompanion(this, _))

  final override def item(number: Int): Num = numberedItemCompanion(this, number)
