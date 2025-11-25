package org.podval.tools.photo

import scala.sys.process.{Process, ProcessBuilder}
import java.time.ZoneOffset

object Util:
  def UTC: ZoneOffset = ZoneOffset.UTC

  def dropSuffix(string: String, suffix: String): String =
    if string.endsWith(suffix) then string.substring(0, string.length - suffix.length) else string

  def execute(dryRun: Boolean, message: String, action: => Unit): Unit =
    if dryRun then println(s"$message - skipping: --dry-run ") else 
      println(message)
      action

  def grep(process: ProcessBuilder, what: String): Option[String] =
    // Note: on cr3 files, return code of 'dcraw' is non-zero for reasons unknown,
    // but it does print the data, so...
    try
      process
        .!!
        .split('\n')
        .find(_.startsWith(what))
        .map(_.substring(what.length))
        .map(_.trim)
    catch
      case _: RuntimeException => None
    