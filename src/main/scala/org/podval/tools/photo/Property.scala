package org.podval.tools.photo

import java.time.LocalDate

sealed abstract class Property[T](
  val name: String,
  val get: Metadata => Option[T]
):
  def set(metadata: Metadata, value: Option[T]): Metadata
  def parser(string: String): T

object Property:
  final class AndValue[T](property: Property[T], value: Option[String]):
    def set(metadata: Metadata): Metadata = property.set(metadata, value.map(property.parser))

    def is(metadata: Metadata): Boolean =
      val m: Option[T] = property.get(metadata)
      value match
        case None        => m.isEmpty
        case Some("*")   => m.isDefined
        case Some(value) => m.contains(property.parser(value))

  val all: Seq[Property[?]] = Seq(Source, Timestamp, Date, BadRaw)

  def forName(name: String): Property[?] = all.find(_.name == name).get

  sealed abstract class StringProperty(
    name: String,
    get: Metadata => Option[String]
  ) extends Property[String](
    name,
    get
  ):
    final override def parser(string: String): String = string

  sealed abstract class BooleanProperty(
    name: String,
    get: Metadata => Option[Boolean]
  ) extends Property[Boolean](
    name,
    get
  ):
    final override def parser(string: String): Boolean = string.toBoolean

  private case object Source extends StringProperty("source", _.source):
    def set(metadata: Metadata, value: Option[String]): Metadata = metadata.copy(source = value)

  private case object Timestamp extends StringProperty("timestamp", _.timestamp):
    def set(metadata: Metadata, value: Option[String]): Metadata = metadata.copy(timestamp = value)

  private case object Date extends Property[LocalDate]("date", _.date):
    def set(metadata: Metadata, value: Option[LocalDate]): Metadata = metadata.copy(date = value)
    def parser(string: String): LocalDate = LocalDate.parse(string)

  private case object BadRaw extends BooleanProperty("bad-raw", _.`bad-raw`):
    def set(metadata: Metadata, value: Option[Boolean]): Metadata = metadata.copy(`bad-raw` = value)
