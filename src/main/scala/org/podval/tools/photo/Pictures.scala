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
  // One planned rename. Publish reads the same rows group enacts.
  private[photo] final case class Planned(
    file: File,
    stem: String,
    extension: String,
    omitted: Boolean,
    renameRefused: Boolean,
    notRenamed: Option[String]
  ):
    def finalName: String = s"$stem.$extension"

    def literal: Boolean = file.getName == finalName

  // Package-visible so tests can group a temp day folder without Root.
  private[photo] def group(files: Seq[File], names: NameMode): Seq[(String, Set[String])] =
    val planned: Seq[Planned] = plan(files)
    enact(planned, names)
    planned
      .filterNot(_.omitted)
      .groupBy(_.stem)
      .view
      .mapValues(_.map(_.extension).toSet)
      .toSeq
      .sortBy(_._1)

  // Side-effect free. misnamedUpload ignores omitted rows and still sees a refused rename.
  private[photo] def plan(files: Seq[File]): Seq[Planned] =
    val read: Seq[Planned] = files.filterNot(albumToml).map(plannedFile)
    decide(withVideoSuffix(read)).sortBy(_.file.getName)

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

  private val stillExtensions: Set[String] = Set(Extension.JPG.name, Extension.GIF.name)

  private val videoExtensions: Set[String] = Set(
    Extension.MP4.name,
    Extension.MOV.name,
    Extension.AVI.name,
    Extension.TGP.name
  )

  private val normalExtensions: Set[String] = Extension.normal.map(_.name)

  private val rawExtensions: Set[String] = Extension.raw.map(_.name)

  private val orphanExtensions: Set[String] = Set(Metadata.extension, Extension.THM.name, Extension.TIF.name)

  // Album files are non-digit .toml; a digit name is a picture sidecar.
  private def albumToml(file: File): Boolean =
    val (name, extension) = Files.nameAndExtension(file)
    extension.contains(Metadata.extension) && !name.headOption.exists(_.isDigit)

  private def plannedFile(file: File): Planned =
    val (name, extensionOpt) = Files.nameAndExtension(file)
    val extension: String = extensionOpt.getOrElse(throw IllegalArgumentException(s"$file: no extension"))
    val (stem, extensionCorrect) = repaired(name, extension)
    // Check the corrected extension so a ".JPG" rename is not rejected first.
    verifyExtension(file.toString, extensionCorrect)
    Planned(file, stem, extensionCorrect, omitted = false, renameRefused = false, notRenamed = None)

  // A video gains one v only when this repaired stem also has a still. An already-split pair does not share that stem.
  private def withVideoSuffix(rows: Seq[Planned]): Seq[Planned] =
    val byStem: Map[String, Seq[Planned]] = rows.groupBy(_.stem)
    rows.map: row =>
      val bucket: Seq[Planned] = byStem(row.stem)
      val stillAndVideo: Boolean =
        bucket.exists(item => stillExtensions.contains(item.extension)) &&
          bucket.exists(item => videoExtensions.contains(item.extension))
      if stillAndVideo && videoExtensions.contains(row.extension) then row.copy(stem = row.stem + "v") else row

  private def decide(rows: Seq[Planned]): Seq[Planned] =
    rows.groupBy(_.stem).toSeq.flatMap: (stem, stemRows) =>
      // A non-digit stem is returned and not renamed. lsd still rejects it.
      if stem.isEmpty || !stem.head.isDigit then stemRows.map(_.copy(renameRefused = true))
      else digitStem(stem, stemRows)

  private def digitStem(stem: String, rows: Seq[Planned]): Seq[Planned] =
    markNoPicture(stem, markFreeze(stem, markSecondNormal(stem, markCollisions(rows))))

  private def markCollisions(rows: Seq[Planned]): Seq[Planned] =
    val byExtension: Map[String, Seq[Planned]] = rows.groupBy(_.extension)
    rows.map: row =>
      val same: Seq[Planned] = byExtension(row.extension)
      val decided: Planned =
        if same.size < 2 then row
        else same.find(_.literal) match
          case Some(kept) if kept.file eq row.file => row
          case Some(_) =>
            row.copy(omitted = true, notRenamed = Some(s"${row.finalName} already exists"))
          case None =>
            val others: String = same.filterNot(_.file eq row.file).map(_.file.getName).sorted.mkString(", ")
            row.copy(omitted = true, notRenamed = Some(s"${row.finalName} collides with $others"))
      // The name can belong to a row that is leaving. Do not move onto it.
      val destination: File = File(decided.file.getParentFile, decided.finalName)
      if decided.omitted || decided.literal || destination.getPath == decided.file.getPath || !destination.exists
      then decided
      else decided.copy(omitted = true, notRenamed = Some(s"${decided.finalName} already exists"))

  // Keep every already-literal normal. Dropping one would let publish upload a single file.
  private def markSecondNormal(stem: String, rows: Seq[Planned]): Seq[Planned] =
    val open: Seq[Planned] = rows.filter(row => !row.omitted && normalExtensions.contains(row.extension))
    if open.map(_.extension).toSet.size < 2 then rows
    else
      val literal: Seq[Planned] = open.filter(_.literal)
      val blocked: Set[File] =
        if literal.isEmpty then open.map(_.file).toSet
        else open.filterNot(_.literal).map(_.file).toSet
      rows.map: row =>
        if blocked.contains(row.file) then
          row.copy(omitted = true, notRenamed = Some(s"$stem would have two normal files"))
        else row

  // A collision or a second normal blocks every other rename onto this stem.
  private def markFreeze(stem: String, rows: Seq[Planned]): Seq[Planned] =
    val normals: Seq[Planned] = rows.filter(row => normalExtensions.contains(row.extension))
    val blocked: Boolean = normals.exists(_.omitted) || normals.count(row => !row.omitted) >= 2
    if !blocked then rows
    else rows.map: row =>
      if row.omitted || row.literal then row
      else row.copy(omitted = true, notRenamed = Some(s"$stem is not one normal picture"))

  private def markNoPicture(stem: String, rows: Seq[Planned]): Seq[Planned] =
    val returned: Seq[Planned] = rows.filterNot(_.omitted)
    val pictured: Boolean = returned.exists: row =>
      normalExtensions.contains(row.extension) || rawExtensions.contains(row.extension)
    if pictured then rows
    else rows.map: row =>
      if row.omitted || !orphanExtensions.contains(row.extension) then row
      else row.copy(omitted = true, notRenamed = Some(s"$stem has no picture file"))

  // Source-filename order keeps a dry-run stable. One refusal does not stop the rest of the day.
  private def enact(rows: Seq[Planned], names: NameMode): Unit =
    rows.foreach: row =>
      val changes: Boolean = row.file.getName != row.finalName
      val destination: File = File(row.file.getParentFile, row.finalName)
      names match
        case NameMode.Leave =>
          if changes then Transcript.println(s"${row.file}: WRONG NAME; SHOULD BE ${row.finalName}; repair --names")
        case NameMode.Preview =>
          if !row.omitted && !row.renameRefused && changes then
            Transcript.println(s"moving ${row.file} to $destination")
        case NameMode.Apply =>
          if !row.omitted && !row.renameRefused && changes then
            Transcript.println(s"moving ${row.file} to $destination")
            Files.move(row.file, destination)
      val reason: Option[String] =
        if row.omitted then row.notRenamed
        else if row.renameRefused && changes then Some(s"${row.finalName} does not start with a digit")
        else None
      reason.foreach(text => Transcript.println(s"${row.file}: NOT RENAMED; $text"))

  private val extensions: Set[String] = Extension.all.map(_.name) ++ Set(
    Metadata.extension
  )

  private def verifyExtension(name: String, extension: String): Unit =
    if !extensions.contains(extension) then throw IllegalArgumentException(s"$name: illegal extension $extension")
