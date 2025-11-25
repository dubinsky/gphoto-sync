package org.podval.tools.photo

import java.io.File

final class Day private(month: Month, number: Int) extends
  Numbered[Month, Day, String, Picture](month, number):

  require(number >= 1 && number <= 31)

  override def numberedCompanion: NumberedCompanion[Month, Day, String, Picture] = Day

  def lsd(fix: Boolean): Seq[Picture] = for (name, extensions) <- group(fix) yield Picture(this, name, extensions)
  
  def item(name: String): Picture = Picture(this, name, group(fix = false).find(_._1 == name).get._2)

  private def group(fix: Boolean): Seq[(String, Set[String])] = Files.listFiles(directory)
    .map(filter(_, fix))
    .groupBy(_._1)
    .view
    .mapValues(_.map(_._2).toSet)
    .toSeq
    .sortBy(_._1)

  private def filter(file: File, fix: Boolean): (String, String) =
    val (name, extensionOpt) = Files.nameAndExtension(file)
    val extension: String = extensionOpt.getOrElse(throw IllegalArgumentException(s"$file: no extension"))
    Picture.verifyExtension(file.toString, extension)
    
    def fixOrError(nameShouldBe: String): Unit =
      if fix
      then Files.move(file, File(file.getParentFile, nameShouldBe))
      else throw IllegalArgumentException(s"$file: WRONG NAME; SHOULD BE $nameShouldBe; no '--fix'")

    if name.startsWith("IMG_") then fixOrError(s"${name.drop("IMG_".length)}.$extension")
    if extension.contains("JPG") then fixOrError(s"$name.jpg")

    (name, extension)
    
object Day extends NumberedCompanion[Month, Day, String, Picture]:
  override def apply(month: Month, number: Int): Day = new Day(month, number)
  
  override def toString(number: Int) = f"$number%02d"
