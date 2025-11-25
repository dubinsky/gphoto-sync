package org.podval.tools.photo

import toml.Node.Pair
import toml.Value.{Bool, Str}
import toml.{Root, Toml}
import toml.derivation.auto.*
import java.io.File

final case class Metadata(
  source: Option[String] = None,
  timestamp: Option[String] = None,
  `bad-raw`: Option[Boolean] = None
) derives CanEqual:
  override def toString: String = Seq(
    source.map(source => s"source=$source"),
    timestamp.map(timestamp => s"timestamp=$timestamp"),
    Option.when(`bad-raw`.contains(true))("bad-raw=true")
  )
    .flatten
    .mkString(", ")

object Metadata:
  val default: Metadata = Metadata()

  type Update = (
    source: Option[String],
    timestamp: Option[String],
    badRaw: Boolean
  )
  
  def parse(file: File): Metadata = Toml.parseAs[Metadata](Files.read(file)) match
    case Left(error) => throw IllegalArgumentException(s"Error parsing TOML file '$file': $error")
    case Right(value) => value

  def unparse(metadata: Metadata): String =
    // TODO toml-scala library can parse a string into a case class - but can not print a string from the case class!!!

    val pairs: List[Pair] = List(
      metadata.source.map(source => Pair("source", Str(source))),
      metadata.timestamp.map(timestamp => Pair("timestamp", Str(timestamp))),
      Option.when(metadata.`bad-raw`.contains(true))(Pair("bad-raw", Bool(true))),
    )
      .flatten

    toml.Toml.generate(Root(pairs))
