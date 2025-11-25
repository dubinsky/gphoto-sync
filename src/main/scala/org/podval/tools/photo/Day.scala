package org.podval.tools.photo

final class Day private(
  val month: Month,
  number: Int                     
) extends Numbered[Month, Day, String, Picture](
  month,
  number
):
  require(number >= 1 && number <= 31)

  override def numberedCompanion: NumberedCompanion[Month, Day, String, Picture] = Day

  override def lsd: Seq[Picture] = for (name, extensions) <- Files.group(directory) yield Picture(this, name, extensions)

  override def item(pictureName: String): Picture = Picture(this, pictureName, Files.group(directory, pictureName))

object Day extends NumberedCompanion[Month, Day, String, Picture]:
  override def apply(month: Month, number: Int): Day = new Day(month, number)
  
  override def toString(number: Int) = f"$number%02d"
