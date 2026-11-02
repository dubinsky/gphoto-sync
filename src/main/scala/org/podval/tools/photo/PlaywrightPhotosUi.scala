package org.podval.tools.photo

import com.microsoft.playwright.{BrowserContext, BrowserType, Locator, Page, Playwright}
import com.microsoft.playwright.options.{AriaRole, WaitUntilState}
import scala.annotation.tailrec
import scala.util.matching.Regex
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as NFiles, Paths}

// Gradle tests construct a fake PhotosUi. Nothing in the test suite calls open.
private[photo] object PlaywrightPhotosUi:
  private val Secret: Regex = "AF1Qip[A-Za-z0-9_-]*".r

  private[photo] def redactSecrets(text: String, title: String): String =
    val replacement: String = if title.nonEmpty then title else "album"
    Secret.replaceAllIn(text, Regex.quoteReplacement(replacement))

  def open(notice: Refresh.Notice => Unit): PhotosAct =
    val ui = new PlaywrightPhotosUi(notice)
    try
      ui.start()
      ui
    catch case error: Throwable =>
      ui.close()
      throw error

private final class PlaywrightPhotosUi(notice: Refresh.Notice => Unit) extends PhotosAct:
  private var playwright: Playwright = null
  private var context: BrowserContext = null
  private var page: Page = null
  private var createdOutput: Option[String] = None
  private var focused: Option[Monitor] = None

  private val albumsUrl: String = "https://photos.google.com/albums"

  def start(): Unit =
    val created: Playwright = Playwright.create()
    playwright = created
    val options = new BrowserType.LaunchPersistentContextOptions()
    options.setHeadless(false)
    options.setExecutablePath(Paths.get("/usr/bin/google-chrome-stable"))
    options.setViewportSize(1400, 900)
    options.setAcceptDownloads(true)
    options.setIgnoreDefaultArgs(java.util.List.of("--enable-automation"))
    options.setArgs(java.util.List.of(
      "--disable-blink-features=AutomationControlled",
      "--no-first-run",
      "--no-default-browser-check",
      "--window-position=40,40",
      "--window-size=1400,900"
    ))
    val home: String = Option(System.getenv("HOME")).filter(_.nonEmpty).getOrElse(System.getProperty("user.home"))
    context = created.chromium().launchPersistentContext(Paths.get(home, ".config", "gphoto-sync-chrome"), options)
    context.setDefaultTimeout(0)
    context.setDefaultNavigationTimeout(120000)
    val pages = context.pages()
    page = if pages.isEmpty then context.newPage() else pages.get(0)
    park()
    navigate(albumsUrl)

  def albumCards(): Seq[AlbumCard] =
    ensureSession()
    navigate(albumsUrl)
    if !wizardReady then ensureSession()
    page.evaluate(installScript)
    firstSweep()
    secondSweep()
    val tsv: String = page.evaluate("() => window.__gphoto.tsv()") match
      case text: String => text
      case _ => ""
    tsv.split("\n", -1).toSeq.filter(_.nonEmpty).map(Refresh.cardOf)

  def readOwner(card: AlbumCard): Option[String] =
    if !onPhotos || !wizardReady then ensureSession()
    val value: Object = page.evaluate(ownerScript, cardArgs(card))
    value match
      case null => None
      case text: String if text.isEmpty => None
      case text: String => Some(text)
      case _ => throw IllegalStateException("owner read failed")

  def readAlbum(card: AlbumCard): Catalog.Album =
    if !onPhotos || !wizardReady then ensureSession()
    try
      val value: Object = page.evaluate(readScript, cardArgs(card))
      val text: String = value match
        case body: String => body
        case _ => throw IllegalStateException("read failed")
      Refresh.parseBody(text)
    catch case error: Exception =>
      throw IllegalStateException(redact(card, error))

  def close(): Unit =
    val currentContext = context
    val currentPlaywright = playwright
    context = null
    playwright = null
    page = null
    try if currentContext != null then currentContext.close()
    finally
      try if currentPlaywright != null then currentPlaywright.close()
      finally unpark()

  def renameAlbum(album: Catalog.Album, updated: String): Unit =
    var confirmed = false
    try
      // start() leaves the context timeout unlimited so a sweep can scroll. A miss here must not hang.
      page.setDefaultTimeout(20000)
      ensureSession()
      navigate(s"https://photos.google.com/album/${album.id}")
      clickMore()
      click(menuItem(awaitMenu(System.currentTimeMillis + 20000L)).first())
      focusTitle(album.title, System.currentTimeMillis + 20000L)
      page.keyboard().press("Control+A")
      page.keyboard().insertText(updated)
      refuseDialog()
      confirmDone()
      confirmed = true
      awaitLive(album.id, updated, System.currentTimeMillis + 20000L)
    catch
      case miss: Refresh.ActMiss =>
        throw miss
      case error: Exception =>
        throw Refresh.ActMiss(Operation.messageOf(error), confirmed)

  private def clickMore(): Unit =
    val options: Locator = page.locator("[aria-label=\"More options\"]")
    awaitCount(options, System.currentTimeMillis + 20000L, "More options is absent")
    click(options.last())

  @tailrec
  private def awaitMenu(deadline: Long): String =
    val names: Seq[String] = Seq("Edit album", "Rename album", "Rename")
    names.find(name => menuItem(name).count() > 0) match
      case Some(name) => name
      case None if System.currentTimeMillis >= deadline =>
        throw Refresh.ActMiss("menuitem is absent", confirmed = false)
      case None =>
        Thread.sleep(300)
        awaitMenu(deadline)

  private def menuItem(name: String): Locator =
    page.getByRole(AriaRole.MENUITEM, new Page.GetByRoleOptions().setName(name).setExact(true))

  @tailrec
  private def focusTitle(current: String, deadline: Long): Unit =
    if asBool(page.evaluate(focusTitleScript, current)) then ()
    else if System.currentTimeMillis >= deadline then
      throw Refresh.ActMiss("title field is absent", confirmed = false)
    else
      Thread.sleep(300)
      focusTitle(current, deadline)

  private def refuseDialog(): Unit =
    val named: Boolean = Seq("Delete forever", "Empty trash").exists(visibleButton)
    val prefix: Locator = page.getByRole(
      AriaRole.BUTTON,
      new Page.GetByRoleOptions().setName(java.util.regex.Pattern.compile("^Move to trash"))
    )
    val prefixed: Boolean = prefix.count() > 0 && prefix.first().isVisible
    if named || prefixed then throw Refresh.ActMiss("refused dialog", confirmed = false)

  private def visibleButton(name: String): Boolean =
    val found: Locator = page.getByRole(
      AriaRole.BUTTON,
      new Page.GetByRoleOptions().setName(name).setExact(true)
    )
    found.count() > 0 && found.first().isVisible

  private def confirmDone(): Unit =
    val done: Locator = page.getByRole(
      AriaRole.BUTTON,
      new Page.GetByRoleOptions().setName("Done").setExact(true)
    )
    if done.count() > 0 then click(done.first())
    else page.keyboard().press("Enter")

  @tailrec
  private def awaitLive(albumId: String, updated: String, deadline: Long): Unit =
    val done: Locator = page.getByRole(
      AriaRole.BUTTON,
      new Page.GetByRoleOptions().setName("Done").setExact(true)
    )
    val editorOpen: Boolean = done.count() > 0 || asBool(page.evaluate(editorOpenScript, updated))
    if editorOpen && System.currentTimeMillis < deadline then
      Thread.sleep(300)
      awaitLive(albumId, updated, deadline)
    else if editorOpen then
      throw Refresh.ActMiss("live title differs", confirmed = true)
    else
      // The live title is this id's card on the albums list.
      val live: Option[String] = albumCards().find(_.id == albumId).map(_.title)
      if live.contains(updated) then ()
      else throw Refresh.ActMiss("live title differs", confirmed = true)

  @tailrec
  private def awaitCount(locator: Locator, deadline: Long, absent: String): Unit =
    if locator.count() > 0 then ()
    else if System.currentTimeMillis >= deadline then throw Refresh.ActMiss(absent, confirmed = false)
    else
      Thread.sleep(300)
      awaitCount(locator, deadline, absent)

  private def click(locator: Locator): Unit =
    locator.click(new Locator.ClickOptions().setTimeout(20000))

  private val focusTitleScript: String =
    """(current) => {
      const fields = Array.from(document.querySelectorAll('input, textarea, [contenteditable="true"]'));
      const valueOf = (node) => typeof node.value === 'string' ? node.value : (node.innerText || '');
      const match = fields.find((node) => valueOf(node) === current);
      if (!match) return false;
      match.focus();
      return document.activeElement === match;
    }"""

  private val editorOpenScript: String =
    """(wanted) => {
      const fields = Array.from(document.querySelectorAll('input, textarea, [contenteditable="true"]'));
      const valueOf = (node) => typeof node.value === 'string' ? node.value : (node.innerText || '');
      return fields.some((node) => valueOf(node) === wanted);
    }"""

  private def navigate(url: String): Unit =
    page.navigate(url, new Page.NavigateOptions().setTimeout(120000).setWaitUntil(WaitUntilState.DOMCONTENTLOADED))

  private def firstSweep(): Unit =
    // Step cap is the albums-list-v2b.js stop, not a walk of the cards.
    var step = 0
    var stable = 0
    var atBottom = 0
    var last = -1
    var stop = false
    while !stop do
      page.evaluate("() => window.__gphoto.grab()")
      val moved: Boolean = scrollBy(0.7)
      page.evaluate("() => window.scrollBy(0, 600)")
      Thread.sleep(2500)
      page.evaluate("() => window.__gphoto.grab()")
      step += 1
      val seen: Int = seenCount()
      if seen == last then stable += 1
      else
        stable = 0
        last = seen
      atBottom = if moved then 0 else atBottom + 1
      notice(Refresh.Notice("scroll", Seq("seen" -> seen.toString)))
      stop = (atBottom >= 8 && stable >= 15) || step > 2000

  private def secondSweep(): Unit =
    page.evaluate("() => window.__gphoto.resetTop()")
    Thread.sleep(3000)
    // Step cap is the albums-list-v2b.js second sweep, not a walk of the cards.
    var step = 0
    var stop = false
    while !stop && step < 4000 do
      page.evaluate("() => window.__gphoto.grab()")
      val moved: Boolean = scrollBy(0.5)
      Thread.sleep(1200)
      step += 1
      notice(Refresh.Notice("scroll", Seq("seen" -> seenCount().toString)))
      if !moved then
        page.evaluate("() => window.__gphoto.grab()")
        stop = true

  private def scrollBy(factor: Double): Boolean =
    asBool(page.evaluate(s"() => window.__gphoto.scrollBy($factor)"))

  private def seenCount(): Int =
    page.evaluate("() => String(window.__gphoto.seen.size)") match
      case text: String => Integer.parseInt(text)
      case number: Number => number.intValue
      case _ => 0

  private def ensureSession(): Unit =
    val deadline: Long = System.currentTimeMillis + 60L * 60L * 1000L
    var announced = false
    var onPhotosSince = 0L
    while System.currentTimeMillis < deadline do
      if onPhotos then
        if onPhotosSince == 0L then onPhotosSince = System.currentTimeMillis
        if wizardReady then return
        if System.currentTimeMillis - onPhotosSince > 60000L then
          onPhotosSince = System.currentTimeMillis
          page.reload(new Page.ReloadOptions().setTimeout(120000).setWaitUntil(WaitUntilState.DOMCONTENTLOADED))
      else
        onPhotosSince = 0L
        if !announced then
          announced = true
          notice(Refresh.Notice("signin", Seq.empty))
      if announced then notice(Refresh.Notice("signin", Seq.empty))
      Thread.sleep(2000)
    throw IllegalStateException("sign-in timed out")

  private def wizardReady: Boolean =
    try
      asBool(page.evaluate(wizardScript))
    catch case _: Exception =>
      false

  private def onPhotos: Boolean =
    try
      val host: String = new java.net.URI(page.url()).getHost
      host == "photos.google.com"
    catch case _: Exception =>
      false

  private def asBool(value: Object): Boolean = value match
    case flag: java.lang.Boolean => flag.booleanValue
    case _ => false

  private def cardArgs(card: AlbumCard): java.util.Map[String, String] =
    val map = new java.util.HashMap[String, String]()
    map.put("id", card.id)
    map.put("title", card.title)
    map.put("count", card.countText)
    map.put("auth", card.auth)
    map

  private def redact(card: AlbumCard, error: Exception): String =
    val message: String = PlaywrightPhotosUi.redactSecrets(
      Option(error.getMessage).getOrElse("read failed"),
      card.title
    )
    message.split('\n').headOption.filter(_.nonEmpty).getOrElse("read failed")

  // Launch returns before Hyprland lists the window. One look misses it and the window stays on the focused monitor.
  // Create the output only after the window is found. A missing window logs and stays where it is.
  private def park(): Unit =
    val signature: String = System.getenv("HYPRLAND_INSTANCE_SIGNATURE")
    if signature == null || signature.isEmpty then ()
    else
      try
        findWindow(System.currentTimeMillis + 20000L) match
          case None =>
            System.err.println("refresh park: window missing")
          case Some(address) =>
            val cursor: (String, String) = cursorPos()
            val before: Set[String] = monitorNames().toSet
            focused = focusedMonitor()
            hypr("hyprctl", "output", "create", "headless")
            monitorNames().find(name => !before.contains(name)) match
              case None =>
                System.err.println("refresh park: headless missing")
              case Some(name) =>
                createdOutput = Some(name)
                hypr("hyprctl", "eval", placeLua(name))
                hypr("hyprctl", "dispatch", moveLua(name, address))
                hypr("hyprctl", "dispatch", s"hl.dsp.cursor.move({ x = ${cursor._1}, y = ${cursor._2} })")
      catch case error: Exception =>
        parkLog(error)

  // Remove the output this process created even when the monitor restore throws.
  private def unpark(): Unit =
    val name: Option[String] = createdOutput
    createdOutput = None
    name.foreach: output =>
      try focused.foreach(monitor => hypr("hyprctl", "eval", restoreLua(monitor)))
      catch case error: Exception =>
        parkLog(error)
      try hypr("hyprctl", "output", "remove", output)
      catch case error: Exception =>
        parkLog(error)

  private def placeLua(name: String): String =
    val headless: String =
      "hl.monitor({ output = " + quoted(name) + ", mode = \"1280x800@60\", position = \"20000x20000\", scale = 1 })"
    focused match
      case None => headless
      case Some(monitor) =>
        headless + "\n" + restoreLua(monitor)

  private def restoreLua(monitor: Monitor): String =
    "hl.monitor({ output = " + quoted(monitor.name) + ", mode = \"preferred\", position = " +
      quoted(s"${monitor.x}x${monitor.y}") + ", scale = \"auto\" })"

  private def moveLua(name: String, address: String): String =
    "hl.dsp.window.move({ monitor = " + quoted(name) + ", follow = false, window = \"address:" + address + "\" })"

  private def quoted(text: String): String =
    val body: String = text.flatMap:
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case ch => String.valueOf(ch)
    "\"" + body + "\""

  private def parkLog(error: Exception): Unit =
    System.err.println(s"refresh park: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}")

  private def cursorPos(): (String, String) =
    val text: String = hypr("hyprctl", "cursorpos")
    val matched = """(-?\d+)\s*,\s*(-?\d+)""".r.findFirstMatchIn(text)
    matched.fold(("0", "0"))(found => (found.group(1), found.group(2)))

  private def monitorNames(): Seq[String] =
    topObjects(hypr("hyprctl", "monitors", "-j")).flatMap(obj => jsonString(obj, "name"))

  private def focusedMonitor(): Option[Monitor] =
    topObjects(hypr("hyprctl", "monitors", "-j")).collectFirst:
      case obj if """"focused"\s*:\s*true""".r.findFirstIn(obj).isDefined =>
        for
          name <- jsonString(obj, "name")
          x <- jsonNumber(obj, "x")
          y <- jsonNumber(obj, "y")
        yield Monitor(name, x, y)
    .flatten

  private def findWindow(deadlineMs: Long): Option[String] =
    findWindowNow() match
      case found @ Some(_) => found
      case None if System.currentTimeMillis >= deadlineMs => None
      case None =>
        Thread.sleep(300)
        findWindow(deadlineMs)

  private def findWindowNow(): Option[String] =
    topObjects(hypr("hyprctl", "clients", "-j")).collectFirst:
      case obj if jsonNumber(obj, "pid").exists(mainChrome) =>
        jsonString(obj, "address")
    .flatten

  private def mainChrome(pid: String): Boolean =
    val path = Paths.get("/proc", pid, "cmdline")
    if !NFiles.isRegularFile(path) then false
    else
      val text: String = new String(NFiles.readAllBytes(path), StandardCharsets.UTF_8).replace('\u0000', ' ')
      text.contains("gphoto-sync-chrome") && text.contains("remote-debugging-pipe") && !text.contains("--type=")

  private def hypr(args: String*): String =
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

  private final case class Monitor(name: String, x: String, y: String)

  private val wizardScript: String =
    """() => { const data = window.WIZ_global_data; return !!(data && data.cfb2h && data.SNlM0e && /^\d+$/.test(String(data.S06Grb || ''))); }"""

  private val installScript: String =
    """() => {
      const g = window.__gphoto = window.__gphoto || {};
      g.seen = new Map();
      g.CNT = /^\s*([\d,]+)\s+items?\b/;
      g.grab = function () {
        for (const a of document.querySelectorAll('a[href*="/album/"],a[href*="/share/"]')) {
          const u = new URL(a.href, location.href);
          const m = u.pathname.match(/\/(album|share)\/(AF1Qip[^/?#]+)/);
          if (!m) continue;
          const id = m[2];
          const auth = m[1] === 'share' ? (u.searchParams.get('key') || '') : '';
          const leaves = [];
          const w = document.createTreeWalker(a, NodeFilter.SHOW_TEXT);
          let n;
          while (n = w.nextNode()) { if (n.nodeValue.trim() !== '') leaves.push(n.nodeValue); }
          const hasCnt = l => l.some(x => g.CNT.test(x));
          const prev = g.seen.get(id);
          const better = !prev || (hasCnt(leaves) && !hasCnt(prev.leaves)) || (hasCnt(leaves) === hasCnt(prev.leaves) && leaves.length > prev.leaves.length);
          g.seen.set(id, { id, auth: auth || (prev && prev.auth) || '', leaves: better ? leaves : prev.leaves });
        }
      };
      g.scrollers = function () {
        const s = [document.scrollingElement || document.documentElement];
        for (const el of document.querySelectorAll('*')) {
          if (el.scrollHeight > el.clientHeight + 50) {
            const o = getComputedStyle(el).overflowY;
            if (o === 'auto' || o === 'scroll') s.push(el);
          }
        }
        return s;
      };
      g.scrollBy = function (factor) {
        let moved = false;
        for (const el of g.scrollers()) {
          const before = el.scrollTop;
          el.scrollTop = before + Math.max(300, Math.floor(el.clientHeight * factor));
          if (el.scrollTop !== before) moved = true;
        }
        return moved;
      };
      g.resetTop = function () {
        for (const el of g.scrollers()) el.scrollTop = 0;
        window.scrollTo(0, 0);
      };
      g.tsv = function () {
        const rows = [];
        for (const r of g.seen.values()) {
          let count = '', title = null;
          for (const t of r.leaves) {
            const m = t.match(g.CNT);
            if (m) { if (count === '') count = m[1].replace(/,/g, ''); }
            else if (title === null && t !== 'More options') title = t;
          }
          rows.push([title === null ? '' : title, count, r.id, r.auth, JSON.stringify(r.leaves)].join('\t'));
        }
        return rows.join('\n') + (rows.length ? '\n' : '');
      };
      return true;
    }"""

  private val ownerScript: String =
    """async (card) => {
      const bl = WIZ_global_data.cfb2h, at = WIZ_global_data.SNlM0e, ME = String(WIZ_global_data.S06Grb || '');
      if (!/^\d+$/.test(ME)) throw new Error('no user gaia id');
      const sleep = ms => new Promise(r => setTimeout(r, ms));
      async function rpc(id, args) {
        const body = 'f.req=' + encodeURIComponent(JSON.stringify([[[id, JSON.stringify(args), null, 'generic']]])) + '&at=' + encodeURIComponent(at);
        const ctl = new AbortController();
        const t = setTimeout(() => ctl.abort(), 30000);
        try {
          const r = await fetch('/_/PhotosUi/data/batchexecute?rpcids=' + id + '&bl=' + encodeURIComponent(bl) + '&hl=en&rt=c', { method: 'POST', headers: { 'content-type': 'application/x-www-form-urlencoded;charset=UTF-8' }, body, signal: ctl.signal, credentials: 'include' });
          if (!r.ok) throw new Error('http ' + r.status);
          const txt = (await r.text()).replace(/^\)\]\}'/, '');
          for (const line of txt.split('\n')) {
            if (line.includes('wrb.fr')) {
              try { const a = JSON.parse(line); const row = a.find(x => x && x[0] === 'wrb.fr'); if (row) return row[2]; } catch (e) {}
            }
          }
          throw new Error('no wrb');
        } finally { clearTimeout(t); }
      }
      async function retry(f, n = 4) { let e; for (let i = 0; i < n; i++) { try { return await f(); } catch (x) { e = x; await sleep(1000 * Math.pow(2, i)); } } throw e; }
      const auth = card.auth ? card.auth : null;
      const raw0 = await retry(() => rpc('snAcKc', [card.id, null, null, auth]));
      const first = JSON.parse(raw0);
      try {
        const o = first[3][5];
        const gid = o && String(o[1]);
        const nm = o && o[11] && o[11][0];
        if (gid && ME && gid !== ME) return String(nm || gid).replace(/[\t\n\r]/g, ' ');
      } catch (e) {}
      return '';
    }"""

  private val readScript: String =
    """async (card) => {
      const bl = WIZ_global_data.cfb2h, at = WIZ_global_data.SNlM0e, ME = String(WIZ_global_data.S06Grb || '');
      if (!/^\d+$/.test(ME)) throw new Error('no user gaia id');
      const sleep = ms => new Promise(r => setTimeout(r, ms));
      async function rpc(id, args) {
        const body = 'f.req=' + encodeURIComponent(JSON.stringify([[[id, JSON.stringify(args), null, 'generic']]])) + '&at=' + encodeURIComponent(at);
        const ctl = new AbortController();
        const t = setTimeout(() => ctl.abort(), 30000);
        try {
          const r = await fetch('/_/PhotosUi/data/batchexecute?rpcids=' + id + '&bl=' + encodeURIComponent(bl) + '&hl=en&rt=c', { method: 'POST', headers: { 'content-type': 'application/x-www-form-urlencoded;charset=UTF-8' }, body, signal: ctl.signal, credentials: 'include' });
          if (!r.ok) throw new Error('http ' + r.status);
          const txt = (await r.text()).replace(/^\)\]\}'/, '');
          for (const line of txt.split('\n')) {
            if (line.includes('wrb.fr')) {
              try { const a = JSON.parse(line); const row = a.find(x => x && x[0] === 'wrb.fr'); if (row) return row[2]; } catch (e) {}
            }
          }
          throw new Error('no wrb');
        } finally { clearTimeout(t); }
      }
      async function retry(f, n = 4) { let e; for (let i = 0; i < n; i++) { try { return await f(); } catch (x) { e = x; await sleep(1000 * Math.pow(2, i)); } } throw e; }
      const okThumb = u => typeof u === 'string' && (u.startsWith('https://photos.fife.usercontent.google.com/') || u.startsWith('https://lh3.googleusercontent.com/pw/'));
      const sp = s => String(s).replace(/[\t\n\r]/g, ' ');
      const auth = card.auth ? card.auth : null;
      const count = Number(card.count);
      const L = [['album', card.id, count, card.title].join('\t')];
      const raw0 = await retry(() => rpc('snAcKc', [card.id, null, null, auth]));
      let first = JSON.parse(raw0);
      try {
        const o = first[3][5];
        const gid = o && String(o[1]);
        const nm = o && o[11] && o[11][0];
        if (gid && ME && gid !== ME) L.push(['owner', card.id, sp(nm || gid)].join('\t'));
      } catch (e) {}
      const items = [], seen = new Set();
      let d = first, tok = null, empty = 0, pages = 0;
      while (true) {
        const before = items.length;
        for (const it of (d && d[1]) || []) {
          if (items.length >= count) break;
          if (Array.isArray(it) && typeof it[0] === 'string' && it[0].startsWith('AF1Qip') && Array.isArray(it[1]) && okThumb(it[1][0]) && !seen.has(it[0])) {
            seen.add(it[0]);
            items.push({ k: it[0], ts: it[2], tz: it[4] });
          }
        }
        empty = items.length === before ? empty + 1 : 0;
        pages++;
        const nt = d && d[2];
        if (items.length >= count || !nt || nt === tok || empty >= 3 || pages > 500) break;
        tok = nt;
        d = JSON.parse(await retry(() => rpc('snAcKc', [card.id, tok, null, auth])));
      }
      const out = new Array(items.length);
      let i = 0, fails = 0, nulls = 0;
      const worker = async () => {
        while (i < items.length) {
          const j = i++, it = items[j];
          let r0 = null;
          try {
            let raw = await retry(() => rpc('fDcn4b', [it.k, 1, null, null, 1]));
            let v = raw && JSON.parse(raw);
            r0 = v && v[0];
            if (!r0 || !r0[2]) {
              raw = await retry(() => rpc('fDcn4b', [it.k, null, auth]));
              v = raw && JSON.parse(raw);
              r0 = v && v[0];
            }
          } catch (e) {}
          let ts = r0 && r0[3] != null ? r0[3] : it.ts, tz = r0 && r0[4] != null ? r0[4] : (it.tz || 0);
          const date = ts != null ? new Date(Number(ts) + Number(tz || 0)).toISOString().slice(0, 10) : '-';
          let up = '-';
          try {
            const o = r0 && r0[28];
            if (o && o[1] != null && ME && String(o[1]) !== ME) up = sp(o[11] && o[11][0] ? o[11][0] : o[1]);
          } catch (e) {}
          const fn = r0 && typeof r0[2] === 'string' && r0[2] !== '' ? r0[2] : null;
          const mark = String.fromCharCode(1);
          const bad = fn !== null && /[\t\n\r]/.test(fn);
          if (fn !== null && (bad || fn.indexOf(mark) !== -1)) {
            if (bad) fails++;
            const escaped = fn.split(mark).join(mark + mark).replace(/\t/g, mark + 't').replace(/\n/g, mark + 'n').replace(/\r/g, mark + 'r');
            out[j] = ['item', card.id, escaped, date, it.k, up].join('\t');
          } else if (fn === null) {
            fails++;
            nulls++;
            out[j] = ['unset', card.id, date, it.k, up].join('\t');
          } else out[j] = ['item', card.id, fn, date, it.k, up].join('\t');
        }
      };
      await Promise.all(Array.from({ length: 6 }, worker));
      L.push(...out);
      if (items.length < count || fails) L.push(['error', card.id, items.length, count, nulls].join('\t'));
      return L.join('\n') + '\n';
    }"""
