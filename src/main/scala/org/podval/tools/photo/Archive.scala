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
    val curated: Seq[Album.Loaded] = tomlFiles(originals).map: file =>
      val (name, _) = Files.nameAndExtension(file)
      Album.Loaded(file, Album.readCurated(file, name))
    childDirectories(originals).filter(dir => isNumber(dir.getName, 4)).foldLeft(Scan(curated, Seq.empty)):
      (scan, yearDir) => scan ++ scanYear(originals, yearDir)

  private def scanYear(originals: File, yearDir: File): Scan =
    val yearName: String = yearDir.getName
    val year: Int = yearName.toInt
    val months: Seq[Album.Loaded] = tomlFiles(yearDir).flatMap: file =>
      val (name, _) = Files.nameAndExtension(file)
      Option.when(isMonth(name)):
        Album.Loaded(file, Album.readMonth(file, s"$yearName/$name"))
    childDirectories(yearDir).filter(dir => isMonth(dir.getName)).foldLeft(Scan(months, Seq.empty)):
      (scan, monthDir) => scan ++ scanMonth(originals, year, yearName, monthDir)

  private def scanMonth(originals: File, year: Int, yearName: String, monthDir: File): Scan =
    val monthName: String = monthDir.getName
    val month: Int = monthName.toInt
    val albums: Seq[Album.Loaded] = tomlFiles(monthDir).flatMap: file =>
      val (name, _) = Files.nameAndExtension(file)
      val title: String = s"$yearName/$monthName/$name"
      if isDay(name) then Some(Album.Loaded(file, Album.readDay(file, title)))
      else if Album.isTitledMonthName(name) then Some(Album.Loaded(file, Album.readMonth(file, title)))
      else None
    childDirectories(monthDir).filter(dir => isDay(dir.getName)).foldLeft(Scan(albums, Seq.empty)):
      (scan, dayDir) => scan ++ scanDay(originals, year, month, dayDir)

  private def scanDay(originals: File, year: Int, month: Int, dayDir: File): Scan =
    val folderDay: LocalDate = LocalDate.of(year, month, dayDir.getName.toInt)
    val files: Seq[File] = Files.listFiles(dayDir)
    val events: Seq[Album.Loaded] = files.filter(isEventFile).sortBy(_.getName).map: file =>
      Album.Loaded(file, Album.readEvent(file, Album.title(originals, file), folderDay))
    val pictures: Seq[Picture] = Pictures.group(files, NameMode.Leave).flatMap: (name, extensions) =>
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
    Scan(events, pictures)

  private def readMetadata(directory: File, name: String, extensions: Set[String]): Metadata =
    if extensions.contains(Metadata.extension)
    then Metadata.read(File(directory, s"$name.${Metadata.extension}"))
    else Metadata.default

  private def isEventFile(file: File): Boolean =
    val (name, extension) = Files.nameAndExtension(file)
    extension.contains(Metadata.extension) && !name.headOption.exists(_.isDigit)

  private def childDirectories(directory: File): Seq[File] =
    children(directory).filter(_.isDirectory).sortBy(_.getName)

  private def tomlFiles(directory: File): Seq[File] =
    children(directory).filter(file => file.isFile && file.getName.endsWith(".toml")).sortBy(_.getName)

  private def children(directory: File): Seq[File] =
    Option(directory.listFiles).map(_.toSeq).getOrElse(Seq.empty)

  private def isMonth(name: String): Boolean = isNumber(name, 2) && name.toInt >= 1 && name.toInt <= 12

  private def isDay(name: String): Boolean = isNumber(name, 2) && name.toInt >= 1 && name.toInt <= 31

  private def isNumber(name: String, length: Int): Boolean =
    name.length == length && name.forall(_.isDigit)
