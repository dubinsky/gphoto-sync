package org.podval.tools.photo

import java.io.File

trait Pictures[Self <: Pictures[Self]] extends Directory[Self, String, Picture[Self]]:
  self: Self =>

  def day: Option[Day]
  
  def root: Root

  final def lsd(fix: Boolean): Seq[Picture[Self]] = for (name, extensions) <- group(fix) yield Picture(this, name, extensions)

  final def item(name: String): Picture[Self] = Picture(this, name, group(fix = false).find(_._1 == name).get._2)

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

    val nameShouldBe: Option[String] =
      if name.startsWith("IMG_") then Some(name.drop("IMG_".length)) else None

    val extensionShouldBe: Option[String] =
      if extension == "JPG" then Some("jpg") else None

    if nameShouldBe.isEmpty && extensionShouldBe.isEmpty
    then
      (name, extension)
    else
      val nameCorrect: String = nameShouldBe.getOrElse(name)
      val extensionCorrect: String = extensionShouldBe.getOrElse(extension)
      val shouldBe: String = s"$nameCorrect.$extensionCorrect"
      if fix
      then Files.move(file, File(file.getParentFile, shouldBe))
      else println(s"$file: WRONG NAME; SHOULD BE $shouldBe; no '--fix'")
      (nameCorrect, extensionCorrect)
