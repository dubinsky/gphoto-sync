package org.podval.tools.photo

final case class SetMetadata(picture: Picture[?], property: Metadata.Property) extends Operation:
  override def execute(): Unit =
    val metadataUpdated: Metadata = picture.metadata.set(property)

    if metadataUpdated == Metadata.default then
      println(s"$picture: deleting metadata file")
      Files.delete(picture.metadataFile)
    else
      val string: String = Metadata.unparse(metadataUpdated)
      println(s"$picture: writing metadata file\n$string")
      Files.write(string, picture.metadataFile)
