package org.podval.tools.photo

import java.io.File
import java.time.{LocalDate, YearMonth}

// One walk of originals for album files and picture facts.
// Stray non-toml files are ignored. lsd still rejects them.
object Archive:
  final case class Picture(
    path: String,
    name: String,
    directory: File,
    folderDay: LocalDate,
    yearMonth: YearMonth,
    extensions: Set[String],
    metadata: Metadata
  )

  final case class Scan(albums: Seq[Album.Loaded], pictures: Seq[Picture]):
    def ++(that: Scan): Scan = Scan(albums ++ that.albums, pictures ++ that.pictures)

  private val pictureExtensions: Set[String] = (Extension.normal ++ Extension.raw).map(_.name)

  def scan(originals: File): Scan =
    val curated: Seq[Album.Loaded] = albumFiles(originals).map(file => Album.Loaded(file, Album.read(originals, file)))
    childDirectories(originals).filter(dir => isNumber(dir.getName, 4)).foldLeft(Scan(curated, Seq.empty)):
      (scan, yearDir) => scan ++ scanYear(originals, yearDir)

  // Pictures only. The migrator uses this while album files still have the old keys.
  def pictures(originals: File): Seq[Picture] =
    childDirectories(originals).filter(dir => isNumber(dir.getName, 4)).flatMap: yearDir =>
      val year: Int = yearDir.getName.toInt
      childDirectories(yearDir).filter(dir => isMonth(dir.getName)).flatMap: monthDir =>
        val month: Int = monthDir.getName.toInt
        childDirectories(monthDir).filter(dir => isDay(dir.getName)).flatMap: dayDir =>
          dayPictures(originals, year, month, dayDir)

  def universe(pictures: Seq[Picture]): Map[LocalDate, Set[String]] =
    pictures.foldLeft(Map.empty[LocalDate, Set[String]]): (acc, picture) =>
      acc.updated(picture.folderDay, acc.getOrElse(picture.folderDay, Set.empty) + picture.name)

  private def scanYear(originals: File, yearDir: File): Scan =
    rejectToml(yearDir)
    val year: Int = yearDir.getName.toInt
    childDirectories(yearDir).filter(dir => isMonth(dir.getName)).foldLeft(Scan(Seq.empty, Seq.empty)):
      (scan, monthDir) => scan ++ scanMonth(originals, year, monthDir)

  private def scanMonth(originals: File, year: Int, monthDir: File): Scan =
    val month: Int = monthDir.getName.toInt
    val albums: Seq[Album.Loaded] = albumFiles(monthDir).map: file =>
      Album.Loaded(file, Album.read(originals, file))
    childDirectories(monthDir).filter(dir => isDay(dir.getName)).foldLeft(Scan(albums, Seq.empty)):
      (scan, dayDir) => scan ++ scanDay(originals, year, month, dayDir)

  private def scanDay(originals: File, year: Int, month: Int, dayDir: File): Scan =
    val albums: Seq[Album.Loaded] = dayFiles(dayDir).filter(isAlbumFile).map: file =>
      Album.Loaded(file, Album.read(originals, file))
    Scan(albums, dayPictures(originals, year, month, dayDir))

  private def dayPictures(originals: File, year: Int, month: Int, dayDir: File): Seq[Picture] =
    val folderDay: LocalDate = LocalDate.of(year, month, dayDir.getName.toInt)
    val files: Seq[File] = Files.listFiles(dayDir)
    Pictures.group(files, NameMode.Leave).flatMap: (name, extensions) =>
      Option.when(extensions.exists(pictureExtensions.contains)):
        Picture(
          path = s"${Album.relative(originals, dayDir)}/$name",
          name = name,
          directory = dayDir,
          folderDay = folderDay,
          yearMonth = YearMonth.of(year, month),
          extensions = extensions,
          metadata = readMetadata(dayDir, name, extensions)
        )

  private def readMetadata(directory: File, name: String, extensions: Set[String]): Metadata =
    if extensions.contains(Metadata.extension)
    then Metadata.read(File(directory, s"$name.${Metadata.extension}"))
    else Metadata.default

  // A digit-leading toml in a day folder is a picture sidecar.
  private def isAlbumFile(file: File): Boolean =
    val (name, extension) = Files.nameAndExtension(file)
    extension.contains("toml") && !name.headOption.exists(_.isDigit)

  private def rejectToml(directory: File): Unit =
    albumFiles(directory).foreach: file =>
      throw IllegalArgumentException(s"$file: album files do not live in a year directory")

  private def albumFiles(directory: File): Seq[File] =
    children(directory).filter(file => file.isFile && file.getName.endsWith(".toml")).sortBy(_.getName)

  private def dayFiles(directory: File): Seq[File] =
    children(directory).filter(_.isFile).sortBy(_.getName)

  private def childDirectories(directory: File): Seq[File] =
    children(directory).filter(_.isDirectory).sortBy(_.getName)

  private def children(directory: File): Seq[File] =
    Option(directory.listFiles).map(_.toSeq).getOrElse(Seq.empty)

  private def isMonth(name: String): Boolean = isNumber(name, 2) && name.toInt >= 1 && name.toInt <= 12

  private def isDay(name: String): Boolean = isNumber(name, 2) && name.toInt >= 1 && name.toInt <= 31

  private def isNumber(name: String, length: Int): Boolean =
    name.length == length && name.forall(_.isDigit)
