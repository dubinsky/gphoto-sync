package org.podval.tools.photo

final class Year private(root: Root, number: Int) extends
  Numbered[Root, Year, Int, Month](root, number) with DirectoryOfNumbered[Year, Month, Int, Day]:
  
  require(number > 1000)

  override def numberedCompanion: NumberedCompanion[Root, Year, Int, Month] = Year

  override def numberedItemCompanion: NumberedCompanion[Year, Month, Int, Day] = Month

object Year extends NumberedCompanion[Root, Year, Int, Month]:
  override def apply(root: Root, number: Int): Year = new Year(root, number)
  
  override def toString(number: Int) = f"$number%04d"
