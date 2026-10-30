package org.podval.tools.photo

import java.io.File
import java.nio.file.NoSuchFileException

object Results:
  enum Status(val wire: String) derives CanEqual:
    case Done extends Status("done")
    case Failed extends Status("failed")
    case Skipped extends Status("skipped")

  object Status:
    def fromWire(text: String): Status = text match
      case "done" => Status.Done
      case "failed" => Status.Failed
      case "skipped" => Status.Skipped
      case _ => throw IllegalArgumentException("unknown result status")

  final case class Row(id: String, action: Requests.Action, status: Status, note: String) derives CanEqual

  private val Header: String = "# gphoto-results 2"
  private val VersionError: String = "results are not version 2"

  def read(file: File): Seq[Row] =
    if !file.isFile then Seq.empty
    else
      try parse(TextLines.read(file, Header, VersionError))
      catch case _: NoSuchFileException => Seq.empty

  private def parse(text: String): Seq[Row] =
    TextLines.dataLines(text).filter(_.nonEmpty).foldLeft(Seq.empty[Row]): (rows, line) =>
      val row: Row = parseRow(line)
      if rows.exists(_.id == row.id) then throw IllegalArgumentException("duplicate request id")
      rows :+ row

  private def parseRow(line: String): Row =
    TextLines.columns(line, 4) match
      case Seq(id, action, status, note) =>
        Requests.requireId(id)
        Row(id, Requests.Action.fromWire(action), Status.fromWire(status), note)
      case _ =>
        throw IllegalArgumentException("wrong column count")
