package org.podval.tools.photo

abstract class Numbered[
  Parent <: Directory[Parent, Int, Self],
  Self <: SubDirectory[Parent, Int, Self, ItemSelector, Item],
  // TODO Self <: Numbered[Parent, Self, ItemSelector, Item],
  ItemSelector,
  Item <: Sub[Self, ItemSelector, Item]
](
   final override val parent: Parent,
   val number: Int
 ) extends SubDirectory[Parent, Int, Self, ItemSelector, Item]:
  self: Self =>

  def numberedCompanion: NumberedCompanion[Parent, Self, ItemSelector, Item]
  
  final override def name: String = numberedCompanion.toString(number)
