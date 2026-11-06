package org.podval.tools.photo

import scala.util.matching.Regex

private[photo] object Redaction:
  private val Secret: Regex = "AF1Qip[A-Za-z0-9_-]*".r

  private[photo] def redactSecrets(text: String, title: String): String =
    val replacement: String = if title.nonEmpty then title else "album"
    Secret.replaceAllIn(text, Regex.quoteReplacement(replacement))
