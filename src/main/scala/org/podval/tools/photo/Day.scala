package org.podval.tools.photo

import java.time.LocalDate

final class Day private(override val parent: Month, val number: Int) extends ItemDirectory with Pictures:
  require(number >= 1 && number <= 31)

  override def name: String = Day.toString(number)

  override def day: Option[Day] = Some(this)

  def date: LocalDate = LocalDate.of(parent.parent.number, parent.number, number)

object Day extends NumberedMaker[Month, Day]:
  override def apply(month: Month, number: Int): Day = new Day(month, number)
  
  override def toString(number: Int) = f"$number%02d"
