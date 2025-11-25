package org.podval.tools.photo

import java.io.File

trait SubDirectory[
  Parent <: Directory[Parent, Selector, Self],
  Selector,
  Self <: Sub[Parent, Selector, Self] & Directory[Self, ItemSelector, Item],
  // TODO Self <: SubDirectory[Parent, Selector, Self, ItemSelector, Item],
  ItemSelector,
  Item <: Sub[Self, ItemSelector, Item]
] extends Sub[Parent, Selector, Self] with Directory[Self, ItemSelector, Item]:
  self: Self =>

  final override protected def getDirectory: File = File(parent.directory, name)
