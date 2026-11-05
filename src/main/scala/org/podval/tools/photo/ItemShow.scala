package org.podval.tools.photo

final case class ItemShow(picture: String) extends Operation:
  override def execute(session: Session): (Session, Result) =
    var produced: Json = Json.Null
    Operation.run(session, produced):
      val parsed: Picture = session.parsePicture(picture)
      val names: Seq[String] = parsed.extensions.toSeq.map(_.name).sorted
      val sidecarText: String = if parsed.hasMetadata then parsed.metadata.toString else "-"
      val folderText: String = parsed.parent.day.fold("-")(_.date.toString)
      val audit: Dates.Assessment = PictureAudit(parsed)
      val titles: Seq[String] = claiming(parsed)
      Transcript.println(parsed.toString)
      Transcript.println(s"extensions: ${names.mkString("+")}")
      Transcript.println(s"sidecar: $sidecarText")
      Transcript.println(s"folder-day: $folderText")
      Transcript.println(s"assessed: ${assessedText(audit)}")
      Transcript.println(s"albums: ${if titles.isEmpty then "-" else titles.mkString(",")}")
      produced = Json.Obj(
        Seq(
          "path" -> Json.Str(parsed.toString.stripPrefix("/")),
          "extensions" -> Json.Arr(names.map(Json.Str(_))),
          "sidecar" -> Json.Obj(sidecarFields(parsed)),
          "folderDay" -> parsed.parent.day.fold(Json.Null)(day => Json.Str(day.date.toString)),
          "assessed" -> assessedJson(audit),
          "albums" -> Json.Arr(titles.map(Json.Str(_)))
        )
      )

  private def claiming(parsed: Picture): Seq[String] =
    parsed.parent.day.fold(Seq.empty): day =>
      Archive
        .scan(parsed.root.configuration.archive)
        .albums
        .map(_.album)
        .filter(_.claims(day.date, parsed.name))
        .map(_.title)
        .sorted

  private def assessedText(audit: Dates.Assessment): String =
    if PictureAudit.noAssessedTimestamp(audit) || audit.disagreement.isDefined then "-"
    else
      audit.clock.fold("-"): clock =>
        val shown: String = clock.offset.fold(clock.civil.toString)(offset => s"${clock.civil}${offset.getId}")
        s"$shown source=${clock.source}"

  private def assessedJson(audit: Dates.Assessment): Json =
    if PictureAudit.noAssessedTimestamp(audit) || audit.disagreement.isDefined then Json.Null
    else
      audit.clock.fold(Json.Null): clock =>
        Json.Str(clock.offset.fold(clock.civil.toString)(offset => s"${clock.civil}${offset.getId}"))

  private def sidecarFields(parsed: Picture): Seq[(String, Json)] =
    if !parsed.hasMetadata then Seq.empty
    else
      Property.all.flatMap: property =>
        jsonOf(property.name, parsed.metadata).map(property.name -> _)

  private def jsonOf(name: String, metadata: Metadata): Option[Json] = name match
    case "source"               => metadata.source.map(Json.Str(_))
    case "timestamp"            => metadata.timestamp.map(Json.Str(_))
    case "date"                 => metadata.date.map(day => Json.Str(day.toString))
    case "bad-raw"              => metadata.`bad-raw`.map(Json.Bool(_))
    case "orientation"          => metadata.orientation.map(Json.Str(_))
    case "google-media-item-id" => metadata.`google-media-item-id`.map(Json.Str(_))
    case "already-in-google"    => metadata.`already-in-google`.map(Json.Bool(_))
    case "uploader"             => metadata.uploader.map(Json.Str(_))
    case _                      => None
