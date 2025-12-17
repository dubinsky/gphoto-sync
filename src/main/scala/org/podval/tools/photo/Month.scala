package org.podval.tools.photo

final class Month private(override val parent: Year, val number: Int) extends ItemDirectory:
  require(number >= 1 && number <= 12)

  override def name: String = Month.toString(number)

  override def lsd: Seq[Directory] = Day.lsd(this)

  override def item(name: String): Day = Day(this, name)

  def day(num: Int): Day = Day(this, num)

object Month extends NumberedMaker[Year, Month]:
  override def apply(year: Year, number: Int): Month = new Month(year, number)
  
  override def toString(number: Int) = f"$number%02d"
