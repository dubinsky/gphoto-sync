package org.podval.tools.photo

type Filter = (
  name: Option[String],
  extension: Option[String],
  normalMissing: Boolean,
  timestampMissing: Boolean,
  settingTimestamp: Boolean,
  wrongDay: Boolean,
  needsFixing: Boolean,
  hasMetadata: Boolean,
  source: Option[String],
  timestamp: Option[String],
  badRaw: Boolean
)
