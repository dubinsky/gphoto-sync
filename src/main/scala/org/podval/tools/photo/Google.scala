package org.podval.tools.photo

import com.google.auth.oauth2.{AccessToken, GoogleCredentials}
import com.google.api.gax.core.FixedCredentialsProvider
import com.google.photos.library.v1.{PhotosLibraryClient, PhotosLibrarySettings}
import scala.jdk.CollectionConverters.IterableHasAsScala

//import java.io.ByteArrayInputStream
//import java.nio.charset.StandardCharsets

object Google:
  def main(args: Array[String]): Unit =
    // TODO use GoogleCredential.fromStream() in alter-rebbe code!
    // com.google.auth.oauth2.ServiceAccountCredentials.fromStream(
    //   ByteArrayInputStream(serviceAccountKey.getBytes(StandardCharsets.UTF_8))
    // )

    // TODO retrieve token from rclone: 'rclone config string podval-photos:'
    val accessToken: String = ""
    val settings: PhotosLibrarySettings = PhotosLibrarySettings
      .newBuilder
      .setCredentialsProvider(
        FixedCredentialsProvider.create(new GoogleCredentials(AccessToken(accessToken, null)))
      )
      .build

    val client: PhotosLibraryClient = PhotosLibraryClient.initialize(settings)

    // TODO there is no way to list albums that were not created by my app!!!
    val page = client.listAlbums.getPage
    page.getValues.asScala.foreach(album => println(album.getTitle))
