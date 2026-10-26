package org.podval.tools.photo

object Google:
  // TODO there is no way to list albums that were not created by my app!!!

  // 2026-10-26 spike on this account. batchCreate returns one result per item and does not throw.
  // A new JPEG: status OK, and an id this client can batchGet, batchAdd, and searchMediaItems.
  // A byte-identical JPEG already in the library: status OK and an id. batchGet of that id returns
  // INVALID_ARGUMENT, "The provided ID does not match any media items." batchAdd rejects the same id.
  // Publish does not store that id. It marks the picture already-in-google.

  def main(args: Array[String]): Unit = println("use photo login")
