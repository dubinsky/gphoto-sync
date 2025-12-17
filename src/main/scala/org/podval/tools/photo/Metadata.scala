package org.podval.tools.photo

import org.podval.tools.toml.TomlCodec
import zio.schema.{DeriveSchema, Schema}
import zio.schema.codec.DecodeError
import java.io.File
import java.time.LocalDate

final case class Metadata(
  source: Option[String] = None,
  timestamp: Option[String] = None, // TODO turn into LocalDateTime - a coercion seems to be missing though?
  date: Option[LocalDate] = None,
  `bad-raw`: Option[Boolean] = None
) derives CanEqual:
  override def toString: String = Property.all
    .flatMap(property => property.get(this).map(value => s"${property.name}=$value"))
    .mkString(", ")

object Metadata:
  val extension: String = "toml"

  val default: Metadata = Metadata()

  given Schema[Metadata] = DeriveSchema.gen

  def read(file: File): Metadata = TomlCodec.decode[Metadata](Files.read(file)) match
    case Left(error: DecodeError) => throw IllegalArgumentException(s"Error parsing TOML file '$file': $error")
    case Right(value) => value

  def encode(metadata: Metadata): String = TomlCodec.encode(metadata)
