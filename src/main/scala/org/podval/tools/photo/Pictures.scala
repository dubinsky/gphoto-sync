package org.podval.tools.photo

import java.io.File

trait Pictures extends Directory:
  def day: Option[Day]

  override def lsd: Seq[Directory] = Seq.empty

  final def item(name: String): Picture = mkPicture(name, group(NameMode.Leave).find(_._1 == name).get._2)

  final def lsd(names: NameMode): Seq[Picture] =
    for (name, extensions) <- group(names) yield mkPicture(name, extensions)

  private def group(names: NameMode): Seq[(String, Set[String])] =
    Pictures.group(Files.listFiles(directory), names)

  private def mkPicture(
    name: String,
    extensions: Set[String]
  ): Picture =
    val nameFull: String = s"$this/$name"

    require(name.nonEmpty && name.head.isDigit, s"Non-digit name start: $nameFull")
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

enum NameMode:
  case Leave, Preview, Apply

object Pictures:
  // Package-visible so tests can group a temp day folder without Root.
  private[photo] def group(files: Seq[File], names: NameMode): Seq[(String, Set[String])] = files
    .filterNot: file =>
      val (name, extension) = Files.nameAndExtension(file)
      // Album files are non-digit .toml; a digit name is a picture sidecar.
      extension.contains(Metadata.extension) && !name.headOption.exists(_.isDigit)
    .map(nameAndExtension(_, names))
    .groupBy(_._1)
    .view
    .mapValues(_.map(_._2).toSet)
    .toSeq
    .sortBy(_._1)

  private val droppedPrefixes: Seq[String] = Seq("MVIMG_", "PXL_", "IMG_", "VID_", "MVI_")

  private def dropOnePrefix(stem: String): String =
    droppedPrefixes
      .find(prefix => stem.take(prefix.length).equalsIgnoreCase(prefix))
      .fold(stem)(prefix => stem.drop(prefix.length))

  private[photo] def repaired(stem: String, extension: String): (String, String) =
    val stemFixed: String = dropOnePrefix(stem)
    val lower: String = extension.toLowerCase(java.util.Locale.ROOT)
    val extensionFixed: String =
      if extension != lower && Extension.all.exists(_.name == lower) then lower else extension
    (stemFixed, extensionFixed)

  private def nameAndExtension(file: File, names: NameMode): (String, String) =
    val (name, extensionOpt) = Files.nameAndExtension(file)
    val extension: String = extensionOpt.getOrElse(throw IllegalArgumentException(s"$file: no extension"))
    val (nameCorrect, extensionCorrect) = repaired(name, extension)
    // Check the corrected extension so a ".JPG" rename is not rejected first.
    verifyExtension(file.toString, extensionCorrect)
    if name != nameCorrect || extension != extensionCorrect then
      val shouldBe: String = s"$nameCorrect.$extensionCorrect"
      if nameCorrect.nonEmpty && nameCorrect.head.isDigit then
        val destination: File = File(file.getParentFile, shouldBe)
        names match
          case NameMode.Apply => Files.move(file, destination)
          case NameMode.Preview => println(s"moving $file to $destination")
          case NameMode.Leave => println(s"$file: WRONG NAME; SHOULD BE $shouldBe; repair --names")
      else names match
        case NameMode.Leave =>
          println(s"$file: WRONG NAME; SHOULD BE $shouldBe; repair --names")
          println(s"$file: NOT RENAMED; $shouldBe does not start with a digit")
        case NameMode.Preview | NameMode.Apply =>
          println(s"$file: NOT RENAMED; $shouldBe does not start with a digit")
    (nameCorrect, extensionCorrect)

  private val extensions: Set[String] = Extension.all.map(_.name) ++ Set(
    Metadata.extension
  )

  private def verifyExtension(name: String, extension: String): Unit =
    if !extensions.contains(extension) then throw IllegalArgumentException(s"$name: illegal extension $extension")
