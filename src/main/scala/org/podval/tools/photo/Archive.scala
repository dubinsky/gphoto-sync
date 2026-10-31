package org.podval.tools.photo

import java.io.File
import java.time.{LocalDate, YearMonth}

// One walk of originals for album files and picture facts.
// This walk ignores stray files. lsd allows album .toml files, AGENTS.md, and dotfiles.
// Any other non-toml file is stray.
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
    DirectoryIndex.cursor.filter(session => sameArchive(session, originals)) match
      case Some(_) => scanIndexed(originals)
      case None => scanCold(originals)

  private def sameArchive(session: Session, originals: File): Boolean =
    DirectoryIndex.key(session.root.configuration.archive) == DirectoryIndex.key(originals)

  private def scanCold(originals: File): Scan =
    val curated: Seq[Album.Loaded] = albumFiles(originals).map(file => Album.Loaded(file, Album.read(originals, file)))
    childDirectories(originals).filter(dir => isNumber(dir.getName, 4)).foldLeft(Scan(curated, Seq.empty)):
      (scan, yearDir) => scan ++ scanYear(originals, yearDir)

  // Readdir the root, every month, and every day. A year hit does not skip a month, and a month hit does not skip a day.
  private def scanIndexed(originals: File): Scan =
    val curated: Seq[Album.Loaded] = loadRoot(originals)
    childDirectories(originals).filter(dir => isNumber(dir.getName, 4)).foldLeft(Scan(curated, Seq.empty)):
      (scan, yearDir) =>
        rejectToml(yearDir)
        val year: Int = yearDir.getName.toInt
        val months: Seq[File] = childDirectories(yearDir).filter(dir => isMonth(dir.getName))
        rememberYear(yearDir, months)
        months.foldLeft(scan): (scan, monthDir) =>
          val month: Int = monthDir.getName.toInt
          val monthAlbums: Seq[Album.Loaded] = loadMonth(originals, monthDir)
          childDirectories(monthDir).filter(dir => isDay(dir.getName)).foldLeft(Scan(scan.albums ++ monthAlbums, scan.pictures)):
            (scan, dayDir) => scan ++ loadDay(originals, year, month, dayDir)

  private def loadRoot(originals: File): Seq[Album.Loaded] =
    val fingerprint: Vector[FileStamp] = DirectoryIndex.fingerprint(originals, filesOnly = false)
    DirectoryIndex.now.cached(originals) match
      case Some(entry) if entry.fingerprint == fingerprint =>
        entry.albums
      case _ =>
        val albums: Seq[Album.Loaded] = albumFiles(originals).map(file => Album.Loaded(file, Album.read(originals, file)))
        DirectoryIndex.replace(DirectoryIndex.now.remember(
          originals,
          DirEntry(fingerprint, Seq.empty, albums, None)
        ))
        albums

  // Month names only. albums stays empty: a year directory rejects album files.
  private def rememberYear(yearDir: File, months: Seq[File]): Unit =
    val fingerprint: Vector[FileStamp] = DirectoryIndex.fingerprint(yearDir, filesOnly = false)
    DirectoryIndex.now.cached(yearDir) match
      case Some(entry) if entry.fingerprint == fingerprint =>
        ()
      case _ =>
        val names: Seq[(String, Set[String])] = months.map(month => month.getName -> Set.empty[String])
        DirectoryIndex.replace(DirectoryIndex.now.remember(
          yearDir,
          DirEntry(fingerprint, names, Seq.empty, None)
        ))

  private def loadMonth(originals: File, monthDir: File): Seq[Album.Loaded] =
    val fingerprint: Vector[FileStamp] = DirectoryIndex.fingerprint(monthDir, filesOnly = false)
    DirectoryIndex.now.cached(monthDir) match
      case Some(entry) if entry.fingerprint == fingerprint =>
        entry.albums
      case _ =>
        val albums: Seq[Album.Loaded] = albumFiles(monthDir).map(file => Album.Loaded(file, Album.read(originals, file)))
        DirectoryIndex.replace(DirectoryIndex.now.remember(
          monthDir,
          DirEntry(fingerprint, Seq.empty, albums, None)
        ))
        albums

  private def loadDay(originals: File, year: Int, month: Int, dayDir: File): Scan =
    val fingerprint: Vector[FileStamp] = DirectoryIndex.fingerprint(dayDir, filesOnly = true)
    DirectoryIndex.now.cached(dayDir) match
      case Some(entry) if entry.fingerprint == fingerprint =>
        Scan(entry.albums, picturesFrom(originals, year, month, dayDir, entry.grouped))
      case _ =>
        // Grouped, not group: a warm albums must not print WRONG NAME. Listing, if any, is stale.
        val grouped: Seq[(String, Set[String])] = Pictures.grouped(Files.listFiles(dayDir))
        val albums: Seq[Album.Loaded] = dayAlbums(originals, dayDir)
        DirectoryIndex.replace(DirectoryIndex.now.remember(
          dayDir,
          DirEntry(fingerprint, grouped, albums, None)
        ))
        Scan(albums, picturesFrom(originals, year, month, dayDir, grouped))

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
    Scan(dayAlbums(originals, dayDir), dayPictures(originals, year, month, dayDir))

  private def dayPictures(originals: File, year: Int, month: Int, dayDir: File): Seq[Picture] =
    val files: Seq[File] = Files.listFiles(dayDir)
    picturesFrom(originals, year, month, dayDir, Pictures.group(files, NameMode.Leave))

  private def picturesFrom(
    originals: File,
    year: Int,
    month: Int,
    dayDir: File,
    grouped: Seq[(String, Set[String])]
  ): Seq[Picture] =
    val folderDay: LocalDate = LocalDate.of(year, month, dayDir.getName.toInt)
    grouped.flatMap: (name, extensions) =>
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

  private[photo] def dayAlbums(originals: File, dayDir: File): Seq[Album.Loaded] =
    dayFiles(dayDir).filter(isAlbumFile).map(file => Album.Loaded(file, Album.read(originals, file)))

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
