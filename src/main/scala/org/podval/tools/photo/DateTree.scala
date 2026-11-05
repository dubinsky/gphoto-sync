package org.podval.tools.photo

import java.io.File

private[photo] object DateTree:
  def isNumber(name: String, length: Int): Boolean =
    name.length == length && name.forall(_.isDigit)

  def isMonth(name: String): Boolean =
    isNumber(name, 2) && name.toInt >= 1 && name.toInt <= 12

  def isDay(name: String): Boolean =
    isNumber(name, 2) && name.toInt >= 1 && name.toInt <= 31

  final case class YearDir(directory: File, year: Int)
  final case class MonthDir(directory: File, year: Int, month: Int)
  final case class DayDir(directory: File, day: Int)

  def years(root: File): Seq[YearDir] =
    directories(root)
      .filter(dir => isNumber(dir.getName, 4))
      .map: dir =>
        YearDir(dir, dir.getName.toInt)

  def months(year: YearDir): Seq[MonthDir] =
    directories(year.directory)
      .filter(dir => isMonth(dir.getName))
      .map: dir =>
        MonthDir(dir, year.year, dir.getName.toInt)

  def days(month: MonthDir): Seq[DayDir] =
    directories(month.directory)
      .filter(dir => isDay(dir.getName))
      .map: dir =>
        DayDir(dir, dir.getName.toInt)

  private def directories(directory: File): Seq[File] =
    Option(directory.listFiles).map(_.toSeq).getOrElse(Seq.empty).filter(_.isDirectory).sortBy(_.getName)
