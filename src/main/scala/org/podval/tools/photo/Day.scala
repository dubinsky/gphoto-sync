package org.podval.tools.photo

final class Day private(month: Month, number: Int) extends
  Numbered[Month, Day, String, Picture[Day]](month, number) with Pictures[Day]:

  require(number >= 1 && number <= 31)

  override def root: Root = parent.parent.parent

  override def day: Option[Day] = Some(this)

  override def numberedCompanion: NumberedCompanion[Month, Day, String, Picture[Day]] = Day

object Day extends NumberedCompanion[Month, Day, String, Picture[Day]]:
  override def apply(month: Month, number: Int): Day = new Day(month, number)
  
  override def toString(number: Int) = f"$number%02d"
