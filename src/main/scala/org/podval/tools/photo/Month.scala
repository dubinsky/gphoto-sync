package org.podval.tools.photo

final class Month private(year: Year, number: Int) extends
  Numbered[Year, Month, Int, Day](year, number) with DirectoryOfNumbered[Month, Day, String, Picture[Day]]:
  
  require(number >= 1 && number <= 12)

  override def numberedCompanion: NumberedCompanion[Year, Month, Int, Day] = Month

  override def numberedItemCompanion: NumberedCompanion[Month, Day, String, Picture[Day]] = Day

object Month extends NumberedCompanion[Year, Month, Int, Day]:
  override def apply(year: Year, number: Int): Month = new Month(year, number)
  
  override def toString(number: Int) = f"$number%02d"
