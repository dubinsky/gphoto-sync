package org.podval.tools.photo

enum Json:
  case Null
  case Bool(value: Boolean)
  case Long(value: Long)
  case Str(value: String)
  case Arr(values: Seq[Json])
  case Obj(fields: Seq[(String, Json)])

enum Result:
  case Ok(text: String, value: Json)
  case Failed(text: String, value: Json, message: String)
  case Usage(message: String)
