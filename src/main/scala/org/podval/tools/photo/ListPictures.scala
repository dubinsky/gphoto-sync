package org.podval.tools.photo

import java.time.Instant

final case class ListPictures(
  path: String,
  long: Boolean,
  nameContains: Option[String],
  hasExtension: Option[String],
  property: Option[String],
  timestampFromNameFormat: Option[String],
  timestampFromName: Boolean,
  timestampFromNameOnly: Boolean,
  timestampsVary: Boolean,
  hasMetadata: Boolean,
  noNormal: Boolean,
  noTimestamp: Boolean,
  wrongTimestamp: Boolean,
  wrongDay: Boolean,
  wrong: Boolean
) extends Operation:

  override def execute(session: Session): (Session, Result) =
    Operation.run(session):
      val parsed: Path = session.root.parse(path)
      val propertyAndValue: Option[Property.AndValue[?]] = property.map(Cli.parsePropertyAndValue)
      forEachPicture(parsed, propertyAndValue)

  private def forEachPicture(path: Path, propertyAndValue: Option[Property.AndValue[?]]): Unit = path match
    case pictures: Pictures => pictures.lsd(NameMode.Leave).foreach(ls(_, propertyAndValue))
    case directory: Directory => directory.lsd.foreach(forEachPicture(_, propertyAndValue))

  private def ls(picture: Picture, propertyAndValue: Option[Property.AndValue[?]]): Unit =
    lazy val audit: PictureAudit.Result = PictureAudit(picture)

    val included: Boolean =
      (nameContains.isEmpty || picture.name.contains(nameContains.get)) &&
      (hasExtension.isEmpty || picture.extensions.map(_.name).contains(hasExtension.get)) &&
      (propertyAndValue.isEmpty || propertyAndValue.get.is(picture.metadata)) &&
      (timestampFromNameFormat.isEmpty || picture.timestampFromName.map(_._2).contains(timestampFromNameFormat.get)) &&
      (!timestampFromName || picture.timestampFromName.isDefined) &&
      (!timestampFromNameOnly || picture.timestampFromName.isDefined && picture.extensions.forall(_.fromMetadata.isEmpty)) &&
      (!timestampsVary || audit.timestampsVary) &&
      (!hasMetadata || picture.hasMetadata) &&
      (!noNormal || picture.normal.isEmpty) &&
      (!noTimestamp || audit.timestamp.isEmpty) &&
      (!wrongTimestamp || audit.timestampWrong) &&
      (!wrongDay || audit.dayShouldBe.isDefined) &&
      (!wrong || (picture.normal.isEmpty || audit.timestamp.isEmpty || audit.timestampWrong || audit.dayShouldBe.isDefined))

    if included then
      val showAudit: Boolean = long || timestampsVary || noTimestamp || wrongTimestamp || wrongDay || wrong
      val normalGenerationSuppressed: Boolean = picture.metadata.`bad-raw`.contains(true)

      Transcript.println(
        s"$picture " ++
        picture.extensions.map(_.name).mkString("+") ++
        (if picture.normal.isDefined then "" else " NO NORMAL IMAGE" ++ (if !normalGenerationSuppressed then "" else "; GENERATION SUPPRESSED")) ++
        (if !picture.hasMetadata then "" else s" [${picture.metadata}]") ++
        picture.timestampFromName.fold("")(timestampFromName => s" timestampFromName=${timestampFromName._1} (${timestampFromName._2})") ++
        (if !showAudit then "" else
          audit.timestamp.fold(" NO TIMESTAMP")(timestamp => s" timestamp=$timestamp") ++
            audit.dayShouldBe.fold("")(dayShouldBe => s" WRONG DAY; SHOULD BE: $dayShouldBe") ++
            "\n" ++
            picture.extensions.map(extensionToString(_, audit.timestamp)).mkString("\n")
          )
      )

  private def extensionToString(extension: Extension, timestamp: Option[Instant]): String =
    s"  .${extension.name}" ++
      extension.fromMetadata.fold("")(fromMetadata => s" fromMetadata=$fromMetadata") ++
      s" current=${extension.current}" ++
      timestamp.flatMap(timestamp => Option.when(!Dates.equal(timestamp, extension.current))(timestamp)).fold("")(toSet => s" toSet=$toSet")
