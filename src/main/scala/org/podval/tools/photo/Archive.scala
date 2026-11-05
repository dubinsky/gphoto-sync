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

  private val pictureExtensions: Set[String] = (Extension.normal ++ Extension.raw ++ Extension.image).map(_.name)

  def scan(originals: File): Scan =
    DirectoryIndex.cursor.filter(session => sameArchive(session, originals)) match
      case Some(_) => scanIndexed(originals)
      case None    => scanCold(originals)

  private def sameArchive(session: Session, originals: File): Boolean =
    DirectoryIndex.key(session.root.configuration.archive) == DirectoryIndex.key(originals)

  private def scanCold(originals: File): Scan =
    val curated: Seq[Album.Loaded] = albumFiles(originals).map(file => Album.Loaded(file, Album.read(originals, file)))
    DateTree
      .years(originals)
      .foldLeft(Scan(curated, Seq.empty)): (scan, year) =>
        scan ++ scanYear(originals, year)

  // Readdir the root, every month, and every day. A year hit does not skip a month, and a month hit does not skip a day.
  private def scanIndexed(originals: File): Scan =
    val curated: Seq[Album.Loaded] = loadRoot(originals)
    DateTree
      .years(originals)
      .foldLeft(Scan(curated, Seq.empty)): (scan, year) =>
        rejectToml(year.directory)
        val months: Seq[DateTree.MonthDir] = DateTree.months(year)
        rememberYear(year.directory, months.map(_.directory))
        months.foldLeft(scan): (scan, month) =>
          val monthAlbums: Seq[Album.Loaded] = loadMonth(originals, month.directory)
          DateTree
            .days(month)
            .foldLeft(Scan(scan.albums ++ monthAlbums, scan.pictures)): (scan, day) =>
              scan ++ loadDay(originals, month, day)

  private def loadRoot(originals: File): Seq[Album.Loaded] =
    val fingerprint: Vector[FileStamp] = DirectoryIndex.fingerprint(originals, filesOnly = false)
    DirectoryIndex.now.cached(originals) match
      case Some(entry) if entry.fingerprint == fingerprint =>
        entry.albums
      case _ =>
        val albums: Seq[Album.Loaded] =
          albumFiles(originals).map(file => Album.Loaded(file, Album.read(originals, file)))
        DirectoryIndex.replace(
          DirectoryIndex.now.remember(
            originals,
            DirEntry(fingerprint, Seq.empty, albums, None)
          )
        )
        albums

  // Month names only. albums stays empty: a year directory rejects album files.
  private def rememberYear(yearDir: File, months: Seq[File]): Unit =
    val fingerprint: Vector[FileStamp] = DirectoryIndex.fingerprint(yearDir, filesOnly = false)
    DirectoryIndex.now.cached(yearDir) match
      case Some(entry) if entry.fingerprint == fingerprint =>
        ()
      case _ =>
        val names: Seq[(String, Set[String])] = months.map(month => month.getName -> Set.empty[String])
        DirectoryIndex.replace(
          DirectoryIndex.now.remember(
            yearDir,
            DirEntry(fingerprint, names, Seq.empty, None)
          )
        )

  private def loadMonth(originals: File, monthDir: File): Seq[Album.Loaded] =
    val fingerprint: Vector[FileStamp] = DirectoryIndex.fingerprint(monthDir, filesOnly = false)
    DirectoryIndex.now.cached(monthDir) match
      case Some(entry) if entry.fingerprint == fingerprint =>
        entry.albums
      case _ =>
        val albums: Seq[Album.Loaded] =
          albumFiles(monthDir).map(file => Album.Loaded(file, Album.read(originals, file)))
        DirectoryIndex.replace(
          DirectoryIndex.now.remember(
            monthDir,
            DirEntry(fingerprint, Seq.empty, albums, None)
          )
        )
        albums

  private def loadDay(originals: File, month: DateTree.MonthDir, day: DateTree.DayDir): Scan =
    val fingerprint: Vector[FileStamp] = DirectoryIndex.fingerprint(day.directory, filesOnly = true)
    DirectoryIndex.now.cached(day.directory) match
      case Some(entry) if entry.fingerprint == fingerprint =>
        Scan(entry.albums, picturesFrom(originals, month, day, entry.grouped))
      case _ =>
        // Grouped, not group: a warm albums must not print WRONG NAME. Listing, if any, is stale.
        val grouped: Seq[(String, Set[String])] = Pictures.grouped(Files.listFiles(day.directory))
        val albums: Seq[Album.Loaded] = dayAlbums(originals, day.directory)
        DirectoryIndex.replace(
          DirectoryIndex.now.remember(
            day.directory,
            DirEntry(fingerprint, grouped, albums, None)
          )
        )
        Scan(albums, picturesFrom(originals, month, day, grouped))

  // Pictures only. The migrator uses this while album files still have the old keys.
  def pictures(originals: File): Seq[Picture] =
    DateTree
      .years(originals)
      .flatMap: year =>
        DateTree
          .months(year)
          .flatMap: month =>
            DateTree
              .days(month)
              .flatMap: day =>
                dayPictures(originals, month, day)

  def universe(pictures: Seq[Picture]): Map[LocalDate, Set[String]] =
    pictures.foldLeft(Map.empty[LocalDate, Set[String]]): (acc, picture) =>
      acc.updated(picture.folderDay, acc.getOrElse(picture.folderDay, Set.empty) + picture.name)

  private def scanYear(originals: File, year: DateTree.YearDir): Scan =
    rejectToml(year.directory)
    DateTree
      .months(year)
      .foldLeft(Scan(Seq.empty, Seq.empty)): (scan, month) =>
        scan ++ scanMonth(originals, month)

  private def scanMonth(originals: File, month: DateTree.MonthDir): Scan =
    val albums: Seq[Album.Loaded] = albumFiles(month.directory).map: file =>
      Album.Loaded(file, Album.read(originals, file))
    DateTree
      .days(month)
      .foldLeft(Scan(albums, Seq.empty)): (scan, day) =>
        scan ++ scanDay(originals, month, day)

  private def scanDay(originals: File, month: DateTree.MonthDir, day: DateTree.DayDir): Scan =
    Scan(dayAlbums(originals, day.directory), dayPictures(originals, month, day))

  private def dayPictures(originals: File, month: DateTree.MonthDir, day: DateTree.DayDir): Seq[Picture] =
    val files: Seq[File] = Files.listFiles(day.directory)
    picturesFrom(originals, month, day, Pictures.group(files, NameMode.Leave))

  private def picturesFrom(
      originals: File,
      month: DateTree.MonthDir,
      day: DateTree.DayDir,
      grouped: Seq[(String, Set[String])]
  ): Seq[Picture] =
    val folderDay: LocalDate = LocalDate.of(month.year, month.month, day.day)
    grouped.flatMap: (name, extensions) =>
      Option.when(extensions.exists(pictureExtensions.contains)):
        Picture(
          path = s"${Album.relative(originals, day.directory)}/$name",
          name = name,
          directory = day.directory,
          folderDay = folderDay,
          yearMonth = YearMonth.of(month.year, month.month),
          extensions = extensions,
          metadata = readMetadata(day.directory, name, extensions)
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

  private def children(directory: File): Seq[File] =
    Option(directory.listFiles).map(_.toSeq).getOrElse(Seq.empty)
