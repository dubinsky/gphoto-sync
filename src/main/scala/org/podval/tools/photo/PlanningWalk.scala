package org.podval.tools.photo

import java.io.File
import java.time.LocalDate

private[photo] object PlanningWalk:
  // Pictures.group(Leave) prints WRONG NAME into this command and throws on a stray file.
  // grouped does not enact. A non-picture is dropped so one unrelated day cannot fail the move.
  private[photo] def planningUniverse(originals: File): Map[LocalDate, Set[String]] =
    def accepted(file: File): Boolean =
      if file.getName.startsWith(".") then false
      else
        val (stem, extension) = Files.nameAndExtension(file)
        extension match
          case None =>
            false
          case Some("toml") if !stem.headOption.exists(_.isDigit) =>
            false
          case Some(raw) =>
            val lower: String = raw.toLowerCase(java.util.Locale.ROOT)
            val corrected: String =
              if raw != lower && Extension.all.exists(_.name == lower) then lower else raw
            Extension.all.exists(_.name == corrected) || corrected == Metadata.extension
    DateTree
      .years(originals)
      .foldLeft(Map.empty[LocalDate, Set[String]]): (universe, year) =>
        DateTree
          .months(year)
          .foldLeft(universe): (universe, month) =>
            DateTree
              .days(month)
              .foldLeft(universe): (universe, day) =>
                val stems: Set[String] =
                  Pictures.grouped(Files.listFiles(day.directory).filter(accepted)).map(_._1).toSet
                if stems.isEmpty then universe
                else universe.updated(LocalDate.of(month.year, month.month, day.day), stems)

  // Album files only. Archive.scan also lists pictures with NameMode.Leave.
  private[photo] def planningAlbums(originals: File): Seq[Album.Loaded] =
    def albumsIn(directory: File, skipDigitToml: Boolean): Seq[Album.Loaded] =
      Album
        .children(directory)
        .filter(_.isFile)
        .filter: file =>
          val (stem, _) = Files.nameAndExtension(file)
          file.getName.endsWith(".toml") && !(skipDigitToml && stem.headOption.exists(_.isDigit))
        .sortBy(_.getName)
        .map(file => Album.Loaded(file, Album.read(originals, file)))
    val curated: Seq[Album.Loaded] = albumsIn(originals, skipDigitToml = false)
    DateTree
      .years(originals)
      .foldLeft(curated): (albums, year) =>
        Album
          .children(year.directory)
          .filter(file => file.isFile && file.getName.endsWith(".toml"))
          .foreach: file =>
            throw IllegalArgumentException(s"$file: album files do not live in a year directory")
        DateTree
          .months(year)
          .foldLeft(albums): (albums, month) =>
            val withMonth: Seq[Album.Loaded] = albums ++ albumsIn(month.directory, skipDigitToml = false)
            DateTree
              .days(month)
              .foldLeft(withMonth): (albums, day) =>
                albums ++ albumsIn(day.directory, skipDigitToml = true)
