package org.podval.tools.photo

import java.io.File

object Claims:
  final case class Resolved(
    path: String,
    albums: Seq[String],
    excludedOnly: Boolean
  ):
    def line: String =
      val text: String = if albums.isEmpty then "-" else albums.mkString(",")
      s"$path albums=$text"

  def apply(originals: File): Seq[Resolved] =
    resolve(Archive.scan(originals))

  private[photo] def resolve(scan: Archive.Scan): Seq[Resolved] =
    val albums: Seq[Album] = scan.albums.map(_.album)
    scan.pictures.map: picture =>
      val titles: Seq[String] = albums.filter(_.claims(picture.folderDay, picture.name)).map(_.title).sorted
      val matched: Boolean = albums.exists(_.positive(picture.folderDay, picture.name))
      Resolved(picture.path, titles, excludedOnly = matched && titles.isEmpty)

final case class ListAlbums(path: Option[String]) extends Operation:
  override def execute(session: Session): (Session, Result) =
    var produced: Json = Json.Null
    Operation.run(session, produced):
      val parsed: Option[Path] = path.map(session.parse)
      val root: Root = parsed.fold(session.root)(_.root)
      val prefix: Option[String] = parsed.map(_.toString.stripPrefix("/")).filter(_.nonEmpty)
      val scan: Archive.Scan = Archive.scan(root.configuration.archive)
      val pictures: Seq[Claims.Resolved] = Claims.resolve(scan)
      val notes: Seq[Album] = annotations(scan.albums, prefix)
      val shown: Seq[Claims.Resolved] =
        pictures.filter(picture => Album.pictureMatches(picture.path, prefix))
      notes.foreach(album => Transcript.println(annotation(album)))
      shown.foreach(picture => Transcript.println(picture.line))
      produced = Json.Obj(
        Seq(
          "annotations" -> Json.Arr(
            notes.map(note =>
              Json.Obj(
                Seq(
                  "title" -> Json.Str(note.title),
                  "googleTitle" -> Json.Str(note.googleTitle),
                  "owner" -> note.owner.fold(Json.Null)(Json.Str(_))
                )
              )
            )
          ),
          "pictures" -> Json.Arr(
            shown.map(picture =>
              Json.Obj(
                Seq(
                  "path" -> Json.Str(picture.path),
                  "albums" -> Json.Arr(picture.albums.map(Json.Str(_))),
                  "excludedOnly" -> Json.Bool(picture.excludedOnly)
                )
              )
            )
          )
        )
      )

  private def annotations(albums: Seq[Album.Loaded], prefix: Option[String]): Seq[Album] =
    albums
      .map(_.album)
      .filter(album => Album.albumMatches(album.startDir, album.title, prefix))
      .filter(showAnnotation)
      .sortBy(_.title)

  private def showAnnotation(album: Album): Boolean =
    album.owner.nonEmpty || (album.googleTitle.nonEmpty && album.googleTitle != album.title)

  private def annotation(album: Album): String =
    val google: String =
      if album.googleTitle.isEmpty then ""
      else s" google=${Album.quote(album.googleTitle)}"
    val ownerText: String = album.owner.fold(""): name =>
      s" owner=${Album.quote(name)}"
    s"album ${Album.quote(album.title)}$google$ownerText"
