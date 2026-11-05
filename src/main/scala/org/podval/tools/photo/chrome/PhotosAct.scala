package org.podval.tools.photo.chrome

import java.io.File
import org.podval.tools.photo.Catalog

private[photo] final case class AlbumCard(
  id: String,
  title: String,
  countText: String,
  auth: String,
  leaves: String = ""
)

// Gradle tests pass a fake. The Playwright class is loaded only when this default is called.
private[photo] trait PhotosUi:
  def albumCards(): Seq[AlbumCard]
  def readOwner(card: AlbumCard): Option[String]
  def readAlbum(card: AlbumCard): Catalog.Album
  def close(): Unit

// A refresh fake is not a mutation fake.
private[photo] trait PhotosAct extends PhotosUi:
  def renameAlbum(album: Catalog.Album, updated: String): Unit
  def setDescription(item: Catalog.Item, text: String): Unit
  def setDate(item: Catalog.Item, local: String): Unit
  def downloadItem(item: Catalog.Item, destination: File): Unit
  def putInAlbum(item: Catalog.Item, destination: Catalog.Album, current: String): Catalog.Item
  def takeOut(album: Catalog.Album, item: Catalog.Item): Unit
  def trash(album: Catalog.Album, item: Catalog.Item): Unit
  def uploadItem(destination: Catalog.Album, file: File, current: String): Catalog.Item
  def createAlbum(title: String): (Seq[AlbumCard], Option[Catalog.Album])
  def deleteAlbum(album: Catalog.Album): Seq[AlbumCard]
