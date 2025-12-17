package org.podval.tools.photo

import java.io.File

trait Pictures extends Directory:
  def day: Option[Day]

  override def lsd: Seq[Directory] = Seq.empty

  final def item(name: String): Picture = mkPicture(name, group(fix = false).find(_._1 == name).get._2)

  final def lsd(fix: Boolean): Seq[Picture] = for (name, extensions) <- group(fix) yield mkPicture(name, extensions)

  private def group(fix: Boolean): Seq[(String, Set[String])] = Files
    .listFiles(directory)
    .map(nameAndExtension(_, fix))
    .groupBy(_._1)
    .view
    .mapValues(_.map(_._2).toSet)
    .toSeq
    .sortBy(_._1)

  private def nameAndExtension(file: File, fix: Boolean): (String, String) =
    val (name, extensionOpt) = Files.nameAndExtension(file)
    val extension: String = extensionOpt.getOrElse(throw IllegalArgumentException(s"$file: no extension"))
    Pictures.verifyExtension(file.toString, extension)

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

  private def mkPicture(
    name: String,
    extensions: Set[String]
  ): Picture =
    val nameFull: String = s"$this/$name"

    require(name.head.isDigit, s"Non-digit name start: $nameFull")
    require(extensions.nonEmpty, s"No files for $nameFull")

    extensions.foreach(Pictures.verifyExtension(nameFull, _))

    def noMoreThanOne(from: Set[Extension.Descriptor], what: String): Option[Extension.Descriptor] =
      val all: Set[Extension.Descriptor] = from.filter(extension => extensions.contains(extension.name))
      require(all.size <= 1, s"too many $what extensions for $nameFull: $all")
      all.headOption

    Picture(
      this,
      name,
      hasMetadata = extensions.contains(Metadata.extension),
      normal      = noMoreThanOne(Extension.normal   , "normal"   ),
      raw         = noMoreThanOne(Extension.raw      , "raw"      ),
      thumbnail   = noMoreThanOne(Extension.thumbnail, "thumbnail"),
      aux         = noMoreThanOne(Extension.aux      , "aux"      )
    )

object Pictures:
  private val extensions: Set[String] = Extension.all.map(_.name) ++ Set(
    Metadata.extension
  )

  private def verifyExtension(name: String, extension: String): Unit =
    if !extensions.contains(extension) then throw IllegalArgumentException(s"$name: illegal extension $extension")
