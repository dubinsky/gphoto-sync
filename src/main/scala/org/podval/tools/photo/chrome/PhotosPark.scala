package org.podval.tools.photo.chrome

import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NFiles, Paths}

private[photo] object PhotosPark:
  private var createdOutput: Option[String] = None
  private[photo] var focused: Option[Monitor] = None
  private var savedCursor: Option[(String, String)] = None
  private var ruleEnabled: Boolean = false

  // Launch returns before Hyprland lists the window, so the confirm poll stays when createdOutput is set.
  // The output is created before launch, and only after detection succeeds.
  private[photo] def preparePark(): Unit =
    try
      savedCursor = Some(cursorPos())
      focused = focusedMonitor()
      val before: Set[String] = monitorNames().toSet
      hypr("hyprctl", "output", "create", "headless")
      monitorNames().find(name => !before.contains(name)) match
        case None =>
          System.err.println("refresh park: headless missing")
        case Some(name) =>
          createdOutput = Some(name)
          hypr("hyprctl", "eval", PhotosScripts.placeLua(name))
          savedCursor.foreach(cursor =>
            hypr("hyprctl", "dispatch", s"hl.dsp.cursor.move({ x = ${cursor._1}, y = ${cursor._2} })")
          )
          hypr(
            "hyprctl",
            "eval",
            "hl.window_rule({ name = \"gphoto-sync-park\", enabled = true, match = { class = \"^gphoto-sync$\" }, monitor = " +
              PhotosScripts.quoted(name + " silent") + ", no_initial_focus = true, no_anim = true })"
          )
          ruleEnabled = true
    catch
      case error: Exception =>
        parkLog(error)

  private[photo] def confirmPark(): Unit =
    createdOutput match
      case None       => ()
      case Some(name) =>
        try
          findWindow(System.currentTimeMillis + 20000L) match
            case None =>
              System.err.println("refresh park: window missing")
            case Some((address, monitorId)) =>
              val onCreated: Boolean = monitorId.exists(id =>
                topObjects(hypr("hyprctl", "monitors", "-j")).exists(obj =>
                  jsonNumber(obj, "id").contains(id) && jsonString(obj, "name").contains(name)
                )
              )
              if !onCreated then
                System.err.println(s"refresh park: window not on $name")
                hypr("hyprctl", "dispatch", PhotosScripts.moveLua(name, address))
                savedCursor.foreach(cursor =>
                  hypr("hyprctl", "dispatch", s"hl.dsp.cursor.move({ x = ${cursor._1}, y = ${cursor._2} })")
                )
        catch
          case error: Exception =>
            parkLog(error)

  // Remove the output this process created even when the monitor restore throws.
  private[photo] def unpark(): Unit =
    val enabled: Boolean = ruleEnabled
    val cursor: Option[(String, String)] = savedCursor
    val monitor: Option[Monitor] = focused
    val name: Option[String] = createdOutput
    ruleEnabled = false
    savedCursor = None
    focused = None
    createdOutput = None
    if enabled then
      try hypr("hyprctl", "eval", "hl.window_rule({ name = \"gphoto-sync-park\", enabled = false })")
      catch
        case error: Exception =>
          parkLog(error)
    try monitor.foreach(recorded => hypr("hyprctl", "eval", PhotosScripts.restoreLua(recorded)))
    catch
      case error: Exception =>
        parkLog(error)
    try
      cursor.foreach(recorded =>
        hypr("hyprctl", "dispatch", s"hl.dsp.cursor.move({ x = ${recorded._1}, y = ${recorded._2} })")
      )
    catch
      case error: Exception =>
        parkLog(error)
    name.foreach: output =>
      try hypr("hyprctl", "output", "remove", output)
      catch
        case error: Exception =>
          parkLog(error)

  private def parkLog(error: Exception): Unit =
    System.err.println(s"refresh park: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}")

  private def cursorPos(): (String, String) =
    val text: String = hypr("hyprctl", "cursorpos")
    val matched = """(-?\d+)\s*,\s*(-?\d+)""".r.findFirstMatchIn(text)
    matched.fold(("0", "0"))(found => (found.group(1), found.group(2)))

  private def monitorNames(): Seq[String] =
    topObjects(hypr("hyprctl", "monitors", "-j")).flatMap(obj => jsonString(obj, "name"))

  private def focusedMonitor(): Option[Monitor] =
    topObjects(hypr("hyprctl", "monitors", "-j"))
      .collectFirst:
        case obj if """"focused"\s*:\s*true""".r.findFirstIn(obj).isDefined =>
          for
            name <- jsonString(obj, "name")
            x <- jsonNumber(obj, "x")
            y <- jsonNumber(obj, "y")
          yield Monitor(name, x, y)
      .flatten

  private def findWindow(deadlineMs: Long): Option[(String, Option[String])] =
    findWindowNow() match
      case found @ Some(_)                                => found
      case None if System.currentTimeMillis >= deadlineMs => None
      case None                                           =>
        Thread.sleep(300)
        findWindow(deadlineMs)

  private def findWindowNow(): Option[(String, Option[String])] =
    topObjects(hypr("hyprctl", "clients", "-j"))
      .collectFirst:
        case obj if jsonNumber(obj, "pid").exists(mainChrome) =>
          jsonString(obj, "address").map(address => (address, jsonNumber(obj, "monitor")))
      .flatten

  private def mainChrome(pid: String): Boolean =
    val path = Paths.get("/proc", pid, "cmdline")
    if !NFiles.isRegularFile(path) then false
    else
      val text: String = new String(NFiles.readAllBytes(path), StandardCharsets.UTF_8).replace('\u0000', ' ')
      text.contains("gphoto-sync-chrome") && text.contains("remote-debugging-pipe") && !text.contains("--type=")

  private[photo] def hypr(args: String*): String =
    val process = new ProcessBuilder(args*).redirectErrorStream(true).start()
    val text: String = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    if process.waitFor() != 0 then throw IllegalStateException("hyprctl failed")
    text

  private def jsonString(obj: String, key: String): Option[String] =
    s""""$key"\\s*:\\s*"([^"]*)"""".r.findFirstMatchIn(obj).map(_.group(1))

  private def jsonNumber(obj: String, key: String): Option[String] =
    s""""$key"\\s*:\\s*(-?\\d+)""".r.findFirstMatchIn(obj).map(_.group(1))

  private def topObjects(json: String): Seq[String] =
    def rec(rest: String, acc: Seq[String]): Seq[String] =
      val from: Int = rest.indexOf('{')
      if from < 0 then acc.reverse
      else
        val (obj, more) = takeObject(rest.substring(from))
        rec(more, obj +: acc)
    rec(json, Seq.empty)

  private def takeObject(rest: String): (String, String) =
    def rec(s: String, depth: Int, acc: String): (String, String) =
      if s.isEmpty then (acc, s)
      else
        val ch: Char = s.head
        val next: Int = if ch == '{' then depth + 1 else if ch == '}' then depth - 1 else depth
        if next == 0 then (acc + ch, s.drop(1))
        else rec(s.drop(1), next, acc + ch)
    if rest.isEmpty || rest.head != '{' then ("", rest)
    else rec(rest.drop(1), 1, "{")

  private[photo] final case class Monitor(name: String, x: String, y: String)
