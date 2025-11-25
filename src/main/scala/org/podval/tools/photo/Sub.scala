package org.podval.tools.photo

// An item of a Directory
trait Sub[
  Parent <: Directory[Parent, Selector, Self],
  Selector,
  Self <: Sub[Parent, Selector, Self]
] extends Path:
  self: Self =>

  def parent: Parent

  final override def toString: String = s"$parent/$name"


