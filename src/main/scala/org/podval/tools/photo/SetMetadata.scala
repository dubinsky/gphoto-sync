package org.podval.tools.photo

final case class SetMetadata(picture: Picture, propertyAndValue: Property.AndValue[?]) extends Operation:
  override def execute(): Unit =
    val metadataUpdated: Metadata = propertyAndValue.set(picture.metadata)
    
    if metadataUpdated == Metadata.default then
      println(s"$picture: deleting metadata file")
      Files.delete(picture.metadataFile)
    else
      val string: String = Metadata.encode(metadataUpdated)
      println(s"$picture: writing metadata file\n$string")
      Files.write(string, picture.metadataFile)
