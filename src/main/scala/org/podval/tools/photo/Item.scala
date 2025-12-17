package org.podval.tools.photo

trait Item extends Path:
  def parent: Directory

  final override def root: Root = parent.root

  final override def toString: String = s"$parent/$name"
