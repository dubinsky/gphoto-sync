package org.podval.tools.photo

import org.podval.tools.toml.TomlCodec
import zio.schema.{DeriveSchema, Schema}
import zio.schema.codec.DecodeError
import java.io.File

final case class Metadata(
  source: Option[String] = None,
  timestamp: Option[String] = None,
  `bad-raw`: Option[Boolean] = None
) derives CanEqual:

  def is(property: Metadata.Property): Boolean =
    def p[T](f: Metadata => Option[T], parser: String => T = identity): Boolean =
      property.value.fold(f(this).isEmpty)(value => if value == "*" then f(this).isDefined else f(this).contains(parser(value)))

    property.name match
      case "source" => p(_.source)
      case "timestamp" => p(_.timestamp)
      case "bad-raw" => p(_.`bad-raw`, _.toBoolean)

  def set(property: Metadata.Property): Metadata =
    val value = property.value
    property.name match
      case "source"    => copy(source    = value)
      case "timestamp" => copy(timestamp = value) // TODO parse timestamp
      case "bad-raw"   => copy(`bad-raw` = value.map(_.toBoolean))

  override def toString: String = Seq(
    source.map(source => s"source=$source"),
    timestamp.map(timestamp => s"timestamp=$timestamp"),
    Option.when(`bad-raw`.contains(true))("bad-raw=true")
  )
    .flatten
    .mkString(", ")

object Metadata:
  final class Property(val name: String, val value: Option[String])

  object Property:
    def apply(string: String): Property =
      val (name: String, value: Option[String]) = Files.splitOnLast(string, '=')
      new Property(name, value)

  val extension: String = "toml"

  val default: Metadata = Metadata()

  given Schema[Metadata] = DeriveSchema.gen

  def parse(file: File): Metadata = TomlCodec.decode[Metadata](Files.read(file)) match
    case Left(error: DecodeError) => throw IllegalArgumentException(s"Error parsing TOML file '$file': ${error.getMessage}")
    case Right(value) => value

  def unparse(metadata: Metadata): String = TomlCodec.encode(metadata)
