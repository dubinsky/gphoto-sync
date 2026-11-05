package org.podval.tools.photo

import com.google.api.gax.core.FixedCredentialsProvider
import com.google.photos.library.v1.{PhotosLibraryClient, PhotosLibrarySettings}

object Google:
  // TODO there is no way to list albums that were not created by my app!!!

  // 2026-10-26 spike on this account. batchCreate returns one result per item and does not throw.
  // A new JPEG: status OK, and an id this client can batchGet, batchAdd, and searchMediaItems.
  // A byte-identical JPEG already in the library: status OK and an id. batchGet of that id returns
  // INVALID_ARGUMENT, "The provided ID does not match any media items." batchAdd rejects the same id.
  // Publish does not store that id. It marks the picture already-in-google.
  // The same day, a video CreateDate taken from the sidecar string came back as that clock in UTC,
  // matching JPEG DateTimeOriginal. A Keys:CreationDate offset did not, and Rotation=90 produced
  // no width or height. A video timestamp is CreateDate. A video orientation override stays a failure.

  def newClient(): PhotosLibraryClient =
    val credentials = GoogleAuth.loadUserCredentials()
    val settings = PhotosLibrarySettings
      .newBuilder()
      .setCredentialsProvider(FixedCredentialsProvider.create(credentials))
      .build()
    PhotosLibraryClient.initialize(settings)
