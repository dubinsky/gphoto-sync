package org.podval.tools.photo

final class Year private(override val parent: Root, val number: Int) extends ItemDirectory:
  require(number > 1000)

  override def name: String = Year.toString(number)

  override def lsd: Seq[Directory] = Month.lsd(this)

  override def item(name: String): Month = Month(this, name)

  def month(num: Int): Month = Month(this, num)

object Year extends NumberedMaker[Root, Year]:
  override def apply(root: Root, number: Int): Year = new Year(root, number)
  
  override def toString(number: Int) = f"$number%04d"
