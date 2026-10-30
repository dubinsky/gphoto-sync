package org.podval.tools.photo

final case class SetMetadata(picture: String, property: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    Operation.run(session):
      // A bad path fails before the property is checked.
      val parsed: Picture = session.parsePicture(picture)
      val propertyAndValue: Property.AndValue[?] = Cli.parsePropertyAndValue(property)
      val metadataUpdated: Metadata = propertyAndValue.set(parsed.metadata)

      if metadataUpdated == Metadata.default then
        Transcript.println(s"$parsed: deleting metadata file")
        Files.delete(parsed.metadataFile)
      else
        val string: String = Metadata.encode(metadataUpdated)
        Transcript.println(s"$parsed: writing metadata file\n$string")
        Files.write(string, parsed.metadataFile)
