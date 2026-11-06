package org.podval.tools.photo.chrome

import com.microsoft.playwright.{BrowserContext, BrowserType, FileChooser, Locator, Page, Playwright}
import com.microsoft.playwright.options.{AriaRole, WaitUntilState}
import scala.annotation.tailrec
import scala.util.matching.Regex
import java.io.File
import java.nio.file.Paths
import java.time.{LocalDate, Month}
import java.time.format.TextStyle
import java.util.Locale
import org.podval.tools.photo.{Catalog, Operation, Refresh}

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
    catch
      case error: Throwable =>
        ui.close()
        throw error

private final class PlaywrightPhotosUi(notice: Refresh.Notice => Unit) extends PhotosAct:
  private var playwright: Playwright = null
  private var context: BrowserContext = null
  private var page: Page = null

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
    options.setArgs(
      java.util.List.of(
        "--disable-blink-features=AutomationControlled",
        "--no-first-run",
        "--no-default-browser-check",
        "--window-position=40,40",
        "--window-size=1400,900",
        "--class=gphoto-sync"
      )
    )
    val home: String = Option(System.getenv("HOME")).filter(_.nonEmpty).getOrElse(System.getProperty("user.home"))
    // A set signature can outlive the compositor. All three checks have to hold.
    val hyprland: Boolean =
      try
        val signature: String = System.getenv("HYPRLAND_INSTANCE_SIGNATURE")
        val runtime: String = System.getenv("XDG_RUNTIME_DIR")
        signature != null && !signature.isBlank &&
        runtime != null && !runtime.isBlank &&
        new File(new File(runtime, "hypr"), signature).isDirectory &&
        { PhotosPark.hypr("hyprctl", "monitors", "-j"); true }
      catch
        case _: Exception =>
          false
    if !hyprland then System.err.println("refresh park: hyprland not running")
    else PhotosPark.preparePark()
    context = created.chromium().launchPersistentContext(Paths.get(home, ".config", "gphoto-sync-chrome"), options)
    context.setDefaultTimeout(0)
    context.setDefaultNavigationTimeout(120000)
    val pages = context.pages()
    page = if pages.isEmpty then context.newPage() else pages.get(0)
    if hyprland then PhotosPark.confirmPark()
    navigate(albumsUrl)

  def albumCards(): Seq[AlbumCard] =
    ensureSession()
    navigate(albumsUrl)
    if !wizardReady then ensureSession()
    page.evaluate(PhotosScripts.installScript)
    firstSweep()
    secondSweep()
    val tsv: String = page.evaluate("() => window.__gphoto.tsv()") match
      case text: String => text
      case _            => ""
    tsv.split("\n", -1).toSeq.filter(_.nonEmpty).map(Refresh.cardOf)

  def readOwner(card: AlbumCard): Option[String] =
    if !onPhotos || !wizardReady then ensureSession()
    val value: Object = page.evaluate(PhotosScripts.ownerScript, cardArgs(card))
    value match
      case null                         => None
      case text: String if text.isEmpty => None
      case text: String                 => Some(text)
      case _                            => throw IllegalStateException("owner read failed")

  def readAlbum(card: AlbumCard): Catalog.Album =
    if !onPhotos || !wizardReady then ensureSession()
    try
      val value: Object = page.evaluate(PhotosScripts.readScript, cardArgs(card))
      val text: String = value match
        case body: String => body
        case _            => throw IllegalStateException("read failed")
      Refresh.parseBody(text)
    catch
      case error: Exception =>
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
      finally PhotosPark.unpark()

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

  def setDescription(item: Catalog.Item, text: String): Unit =
    var confirmed = false
    try
      // start() leaves the context timeout unlimited so a sweep can scroll. A miss here must not hang.
      page.setDefaultTimeout(20000)
      ensureSession()
      navigate(s"https://photos.google.com/photo/${item.key}")
      focusDescription(System.currentTimeMillis + 20000L, opened = false)
      page.keyboard().press("Control+A")
      if text.isEmpty then page.keyboard().press("Backspace")
      else page.keyboard().insertText(text)
      page.keyboard().press("Tab")
      confirmed = true
      awaitDescription(text, System.currentTimeMillis + 20000L)
    catch
      case miss: Refresh.ActMiss =>
        throw miss
      case error: Exception =>
        throw Refresh.ActMiss(Operation.messageOf(error), confirmed)

  def setDate(item: Catalog.Item, local: String): Unit =
    var confirmed = false
    try
      // start() leaves the context timeout unlimited so a sweep can scroll. A miss here must not hang.
      page.setDefaultTimeout(20000)
      ensureSession()
      navigate(s"https://photos.google.com/photo/${item.key}")
      val civil: String = fillDate(local)
      confirmDate()
      confirmed = true
      // fDcn4b runs on this page. The albums list is a different read.
      val read: String = readItemDay(item.key)
      if read == civil then ()
      else throw Refresh.ActMiss("read date differs", confirmed = true)
    catch
      case miss: Refresh.ActMiss =>
        throw miss
      case error: Exception =>
        throw Refresh.ActMiss(Operation.messageOf(error), confirmed)

  def downloadItem(item: Catalog.Item, destination: File): Unit =
    try
      // start() leaves the context timeout unlimited so a sweep can scroll. A miss here must not hang.
      page.setDefaultTimeout(20000)
      ensureSession()
      navigate(s"https://photos.google.com/photo/${item.key}")
      clickMore()
      val menu: Locator = page.getByRole(
        AriaRole.MENUITEM,
        new Page.GetByRoleOptions().setName(java.util.regex.Pattern.compile("^Download\\b"))
      )
      awaitCount(menu, System.currentTimeMillis + 20000L, "Download is absent")
      // waitForDownload follows the context timeout, which start() left unlimited.
      val saved = page.waitForDownload(
        new Page.WaitForDownloadOptions().setTimeout(20000),
        () => click(menu.first())
      )
      saved.saveAs(destination.toPath)
    catch
      case miss: Refresh.ActMiss =>
        throw miss
      case error: Exception =>
        throw Refresh.ActMiss(Operation.messageOf(error), confirmed = false)

  def putInAlbum(item: Catalog.Item, destination: Catalog.Album, current: String): Catalog.Item =
    var confirmed = false
    try
      // start() leaves the context timeout unlimited so a sweep can scroll. A miss here must not hang.
      page.setDefaultTimeout(20000)
      ensureSession()
      navigate(s"https://photos.google.com/photo/${item.key}")
      // That option creates an album. This command only picks an existing one.
      if destination.title == "New album" then
        throw Refresh.ActMiss("New album is not a destination", confirmed = false)
      val before: Set[String] = readKeyList(destination.id).toSet
      clickMore()
      val add: Locator = menuItem("Add to album")
      awaitCount(add, System.currentTimeMillis + 20000L, "Add to album is absent")
      click(add.first())
      val option: Locator = page.getByRole(
        AriaRole.OPTION,
        new Page.GetByRoleOptions().setName(destination.title).setExact(true)
      )
      awaitCount(option, System.currentTimeMillis + 20000L, "destination album is absent")
      click(option.first())
      confirmed = true
      val after: Seq[String] = readKeyList(destination.id)
      val added: Seq[String] = after.filterNot(before.contains)
      val named: Map[String, Catalog.Item] =
        if added.lengthIs == 1 then
          readFact(added.head, destination.id) match
            case Some(found) => Map(added.head -> found)
            case None        => Map.empty
        else Map.empty
      AgentChrome.confirmedAdd(before, after, named, current) match
        case Some(found) => found
        case None        => throw Refresh.ActMiss("live key differs", confirmed = true)
    catch
      case miss: Refresh.ActMiss =>
        throw miss
      case error: Exception =>
        throw Refresh.ActMiss(Operation.messageOf(error), confirmed)

  def takeOut(album: Catalog.Album, item: Catalog.Item): Unit =
    var confirmed = false
    try
      // start() leaves the context timeout unlimited so a sweep can scroll. A miss here must not hang.
      page.setDefaultTimeout(20000)
      ensureSession()
      navigate(s"https://photos.google.com/photo/${item.key}")
      clickMore()
      val menu: Locator = page.getByRole(
        AriaRole.MENUITEM,
        new Page.GetByRoleOptions().setName(java.util.regex.Pattern.compile("Remove from album"))
      )
      awaitCount(menu, System.currentTimeMillis + 20000L, "Remove from album is absent")
      click(menu.last())
      val name: String = awaitRemove(System.currentTimeMillis + 20000L)
      val dialog: Locator = visibleDialog.getOrElse(
        throw Refresh.ActMiss("Remove is absent", confirmed = false)
      )
      val buttons: Seq[String] = dialogButtons(dialog)
      // A trash dialog is not a remove, so it is not clicked.
      if isTrash(dialogText(dialog), buttons) then throw Refresh.ActMiss("trash dialog", confirmed = false)
      if !isRemoveButton(name) || !buttons.contains(name) then
        throw Refresh.ActMiss("Remove is absent", confirmed = false)
      val button: Locator = dialog.getByRole(
        AriaRole.BUTTON,
        new Locator.GetByRoleOptions().setName(name).setExact(true)
      )
      if button.count() == 0 then throw Refresh.ActMiss("Remove is absent", confirmed = false)
      click(button.first())
      confirmed = true
      if readKeyList(album.id).contains(item.key) then throw Refresh.ActMiss("source key is present", confirmed = true)
    catch
      case miss: Refresh.ActMiss =>
        throw miss
      case error: Exception =>
        throw Refresh.ActMiss(Operation.messageOf(error), confirmed)

  def trash(album: Catalog.Album, item: Catalog.Item): Unit =
    var confirmed = false
    try
      // start() leaves the context timeout unlimited so a sweep can scroll. A miss here must not hang.
      page.setDefaultTimeout(20000)
      ensureSession()
      navigate(s"https://photos.google.com/photo/${item.key}")
      // An absent source key is not a click. The caller still removes the catalog line.
      if readKeyList(album.id).contains(item.key) then
        openTrash()
        val name: String = awaitTrashConfirm(System.currentTimeMillis + 20000L)
        val dialog: Locator = visibleDialog.getOrElse(
          throw Refresh.ActMiss("Move to trash is absent", confirmed = false)
        )
        refuseWrong(name)
        val button: Locator = dialog.getByRole(
          AriaRole.BUTTON,
          new Locator.GetByRoleOptions().setName(name).setExact(true)
        )
        if button.count() == 0 then throw Refresh.ActMiss("Move to trash is absent", confirmed = false)
        click(button.first())
        confirmed = true
        if readKeyList(album.id).contains(item.key) then
          throw Refresh.ActMiss("source key is present", confirmed = true)
    catch
      case miss: Refresh.ActMiss =>
        throw miss
      case error: Exception =>
        throw Refresh.ActMiss(Operation.messageOf(error), confirmed)

  def uploadItem(destination: Catalog.Album, file: File, current: String): Catalog.Item =
    var confirmed = false
    try
      // start() leaves the context timeout unlimited so a sweep can scroll. A miss here must not hang.
      page.setDefaultTimeout(20000)
      ensureSession()
      navigate(s"https://photos.google.com/album/${destination.id}")
      val before: Set[String] = readKeyList(destination.id).toSet
      // Add photos is the toolbar once the album has items. An empty album shows Select photos.
      val opener: String =
        if visibleButton("Add photos") then "Add photos"
        else if visibleButton("Select photos") then "Select photos"
        else throw Refresh.ActMiss("Add photos is absent", confirmed = false)
      refuseDialog()
      val open: Locator = page.getByRole(
        AriaRole.BUTTON,
        new Page.GetByRoleOptions().setName(opener).setExact(true)
      )
      click(open.first())
      refuseDialog()
      if !visibleButton("Select from computer") then
        throw Refresh.ActMiss("Select from computer is absent", confirmed = false)
      val computer: Locator = page.getByRole(
        AriaRole.BUTTON,
        new Page.GetByRoleOptions().setName("Select from computer").setExact(true)
      )
      // Dialog Done adds a library photo. The chooser is the upload, so the flag flips at setFiles.
      val chooser: FileChooser = page.waitForFileChooser(new Runnable:
        def run(): Unit = click(computer.first()))
      chooser.setFiles(file.toPath)
      confirmed = true

      @tailrec
      def awaitAdded(deadline: Long): Catalog.Item =
        val after: Seq[String] = readKeyList(destination.id)
        val added: Seq[String] = after.filterNot(before.contains)
        val named: Map[String, Catalog.Item] =
          if added.lengthIs == 1 then
            readFact(added.head, destination.id) match
              case Some(found) => Map(added.head -> found)
              case None        => Map.empty
          else Map.empty
        AgentChrome.confirmedAdd(before, after, named, current) match
          case Some(found)                                  => found
          case None if System.currentTimeMillis >= deadline =>
            throw Refresh.ActMiss("live key differs", confirmed = true)
          case None =>
            Thread.sleep(300)
            awaitAdded(deadline)

      awaitAdded(System.currentTimeMillis + 20000L)
    catch
      case miss: Refresh.ActMiss =>
        throw miss
      case error: Exception =>
        throw Refresh.ActMiss(Operation.messageOf(error), confirmed)

  def createAlbum(title: String): (Seq[AlbumCard], Option[Catalog.Album]) =
    var confirmed = false
    try
      // start() leaves the context timeout unlimited so a sweep can scroll. A miss here must not hang.
      page.setDefaultTimeout(20000)
      ensureSession()
      navigate(albumsUrl)
      // The plus menu is a different control. An absent button is not a reason to open it.
      if !visibleButton("Create album") then throw Refresh.ActMiss("Create album is absent", confirmed = false)
      val button: Locator = page.getByRole(
        AriaRole.BUTTON,
        new Page.GetByRoleOptions().setName("Create album").setExact(true)
      )
      // The album exists as soon as this click returns, even if Done never runs.
      click(button.first())
      confirmed = true
      val field: Locator = page.getByRole(
        AriaRole.TEXTBOX,
        new Page.GetByRoleOptions().setName("Edit album name").setExact(true)
      )
      // awaitCount marks the miss unconfirmed. This click already created the album.
      awaitEditor(field, "Edit album name is absent")
      field.first().fill(title)
      val done: Locator = page.getByRole(
        AriaRole.BUTTON,
        new Page.GetByRoleOptions().setName("Done").setExact(true)
      )
      awaitEditor(done, "Done is absent")
      click(done.first())
      // The live title is the albums-list card. The textarea just filled is not that check.
      val cards: Seq[AlbumCard] = albumCards()
      val matches: Seq[AlbumCard] = cards.filter(_.title == title)
      val read: Option[Catalog.Album] =
        if matches.lengthIs == 1 then Some(readAlbum(matches.head)) else None
      (cards, read)
    catch
      case miss: Refresh.ActMiss =>
        throw miss
      case error: Exception =>
        throw Refresh.ActMiss(Operation.messageOf(error), confirmed)

  def deleteAlbum(album: Catalog.Album): Seq[AlbumCard] =
    var confirmed = false
    try
      // start() leaves the context timeout unlimited so a sweep can scroll. A miss here must not hang.
      page.setDefaultTimeout(20000)
      ensureSession()
      navigate(s"https://photos.google.com/album/${album.id}")
      clickMore()
      val menu: Locator = menuItem("Delete album")
      awaitCount(menu, System.currentTimeMillis + 20000L, "Delete album is absent")
      click(menu.first())
      val dialog: Locator = awaitAlbumDelete(System.currentTimeMillis + 20000L)
      val button: Locator = dialog.getByRole(
        AriaRole.BUTTON,
        new Locator.GetByRoleOptions().setName("Delete").setExact(true)
      )
      if button.count() == 0 then throw Refresh.ActMiss("Delete is absent", confirmed = false)
      // Keep album is the other button. This click is the delete.
      click(button.first())
      confirmed = true
      albumCards()
    catch
      case miss: Refresh.ActMiss =>
        throw miss
      case error: Exception =>
        throw Refresh.ActMiss(Operation.messageOf(error), confirmed)

  // Delete forever and Empty trash are not this command. Remove and New album are not either.
  private def openTrash(): Unit =
    val button: Locator = page.locator("[aria-label=\"Move to trash\"]")
    if button.count() > 0 then
      val label: String = Option(button.last().getAttribute("aria-label")).getOrElse("")
      refuseWrong(label)
      click(button.last())
    else
      clickMore()
      val menu: Locator = page.getByRole(
        AriaRole.MENUITEM,
        new Page.GetByRoleOptions().setName(java.util.regex.Pattern.compile("Move to trash"))
      )
      awaitCount(menu, System.currentTimeMillis + 20000L, "Move to trash is absent")
      val name: String = Option(menu.last().innerText()).getOrElse("").replaceAll("\\s+", " ").trim
      refuseWrong(name)
      click(menu.last())

  @tailrec
  private def awaitTrashConfirm(deadline: Long): String =
    visibleDialog match
      case None if System.currentTimeMillis >= deadline =>
        throw Refresh.ActMiss("Move to trash is absent", confirmed = false)
      case None =>
        Thread.sleep(300)
        awaitTrashConfirm(deadline)
      case Some(dialog) =>
        val buttons: Seq[String] = dialogButtons(dialog)
        // Delete forever and Empty trash stay unclicked even when Move to trash is also listed.
        buttons.find(isForeverName) match
          case Some(name) =>
            throw Refresh.ActMiss(name.trim, confirmed = false)
          case None =>
            buttons.find(isTrashConfirm) match
              case Some(name) =>
                refuseWrong(name)
                name
              case None if System.currentTimeMillis >= deadline =>
                throw Refresh.ActMiss("Move to trash is absent", confirmed = false)
              case None =>
                Thread.sleep(300)
                awaitTrashConfirm(deadline)

  private def refuseWrong(name: String): Unit =
    val folded: String = name.trim.toLowerCase(java.util.Locale.ROOT)
    if isForeverName(name) then throw Refresh.ActMiss(name.trim, confirmed = false)
    else if folded == "new album" || folded.startsWith("remove") then
      throw Refresh.ActMiss("Move to trash is absent", confirmed = false)

  private def isForeverName(name: String): Boolean =
    val folded: String = name.trim.toLowerCase(java.util.Locale.ROOT)
    folded.startsWith("delete forever") || folded.startsWith("empty trash")

  private def isTrashConfirm(name: String): Boolean =
    java.util.regex.Pattern
      .compile(
        "^Move to trash\\b",
        java.util.regex.Pattern.CASE_INSENSITIVE
      )
      .matcher(name)
      .find()

  @tailrec
  private def awaitRemove(deadline: Long): String =
    visibleDialog match
      case None if System.currentTimeMillis >= deadline =>
        throw Refresh.ActMiss("Remove is absent", confirmed = false)
      case None =>
        Thread.sleep(300)
        awaitRemove(deadline)
      case Some(dialog) =>
        val buttons: Seq[String] = dialogButtons(dialog)
        if isTrash(dialogText(dialog), buttons) then throw Refresh.ActMiss("trash dialog", confirmed = false)
        buttons.find(isRemoveButton) match
          case Some(name)                                   => name
          case None if System.currentTimeMillis >= deadline =>
            throw Refresh.ActMiss("Remove is absent", confirmed = false)
          case None =>
            Thread.sleep(300)
            awaitRemove(deadline)

  @tailrec
  private def awaitAlbumDelete(deadline: Long): Locator =
    visibleDialog match
      case None if System.currentTimeMillis >= deadline =>
        throw Refresh.ActMiss("Delete is absent", confirmed = false)
      case None =>
        Thread.sleep(300)
        awaitAlbumDelete(deadline)
      case Some(dialog) =>
        val buttons: Seq[String] = dialogButtons(dialog)
        // Delete forever, Empty trash, and Move to trash stay unclicked even when Delete is also listed.
        if buttons.exists(refusedButton) then throw Refresh.ActMiss("refused dialog", confirmed = false)
        val named: Set[String] = buttons.map(_.trim).toSet
        val text: String = dialogText(dialog)
        if named.contains("Delete") && named.contains("Keep album") && text.contains("remain in Google Photos") then
          dialog
        else if System.currentTimeMillis >= deadline then throw Refresh.ActMiss("Delete is absent", confirmed = false)
        else
          Thread.sleep(300)
          awaitAlbumDelete(deadline)

  private def visibleDialog: Option[Locator] =
    val found: Locator = page.locator("[role=\"dialog\"], [role=\"alertdialog\"]")
    if found.count() > 0 && found.first().isVisible then Some(found.first()) else None

  private def dialogText(dialog: Locator): String =
    Option(dialog.innerText()).getOrElse("")

  private def dialogButtons(dialog: Locator): Seq[String] =
    dialog.evaluate(PhotosScripts.dialogButtonsScript) match
      case text: String => text.split("\n", -1).toSeq.filter(_.nonEmpty)
      case _            => throw IllegalStateException("dialog read failed")

  private def isTrash(text: String, buttons: Seq[String]): Boolean =
    val lower: String = text.toLowerCase(java.util.Locale.ROOT)
    lower.contains("trash") || lower.contains("delete forever") || buttons.exists(refusedButton)

  private def refusedButton(name: String): Boolean =
    val folded: String = name.toLowerCase(java.util.Locale.ROOT)
    folded == "delete forever" || folded == "empty trash" || folded.startsWith("move to trash")

  private def isRemoveButton(name: String): Boolean =
    java.util.regex.Pattern.compile("^Remove\\b").matcher(name).find()

  private def readKeyList(albumId: String): Seq[String] =
    page.evaluate(PhotosScripts.albumKeysScript, albumId) match
      case text: String => text.split("\n", -1).toSeq.filter(_.nonEmpty)
      case _            => throw IllegalStateException("read failed")

  private def readFact(key: String, albumId: String): Option[Catalog.Item] =
    page.evaluate(PhotosScripts.itemFactScript, key + "\t" + albumId) match
      case text: String if text.isEmpty => None
      case text: String                 =>
        text.split("\t", -1).toSeq match
          case Seq(filename, date, uploader) if filename.nonEmpty =>
            Some(Catalog.Item(filename, factDate(date), key, factUploader(uploader)))
          case _ => None
      case _ =>
        throw IllegalStateException("read failed")

  private def factDate(text: String): Option[LocalDate] =
    if text.isEmpty || text == "-" then None else Some(LocalDate.parse(text))

  private def factUploader(text: String): Option[String] =
    if text.isEmpty || text == "-" then None else Some(text)

  @tailrec
  private def focusDescription(deadline: Long, opened: Boolean): Unit =
    if asBool(page.evaluate(PhotosScripts.focusDescriptionScript)) then ()
    else if System.currentTimeMillis >= deadline then
      throw Refresh.ActMiss("Add a description is absent", confirmed = false)
    else if !opened then
      val button: Locator = page.locator("[aria-label=\"Open info\"]")
      if button.count() > 0 then click(button.last())
      else page.keyboard().press("i")
      Thread.sleep(300)
      focusDescription(deadline, opened = true)
    else
      Thread.sleep(300)
      focusDescription(deadline, opened = true)

  @tailrec
  private def awaitDescription(text: String, deadline: Long): Unit =
    if asBool(page.evaluate(PhotosScripts.descriptionShownScript, text)) then ()
    else if System.currentTimeMillis >= deadline then
      throw Refresh.ActMiss("panel does not show the description", confirmed = true)
    else
      Thread.sleep(300)
      awaitDescription(text, deadline)

  private def fillDate(local: String): String = local match
    case clock(year, month, day, hour, minute) =>
      val hour24: Int = hour.toInt
      val meridiem: String = if hour24 < 12 then "AM" else "PM"
      val hour12: String = hour24 % 12 match
        case 0 => "12"
        case n => n.toString
      openDate(System.currentTimeMillis + 20000L)
      val monthField: String = fieldInput("Month").inputValue().trim
      val typed: Map[String, String] = Map(
        "Year" -> year,
        "Month" -> monthText(month.toInt, monthField),
        "Day" -> day.toInt.toString,
        "Hour" -> hour12,
        "Minutes" -> minute,
        "AM/PM" -> meridiem
      )
      Seq("Year", "Month", "Day", "Hour", "Minutes", "AM/PM").foreach: label =>
        val input: Locator = fieldInput(label)
        click(input)
        page.keyboard().press("Control+A")
        page.keyboard().insertText(typed(label))
        page.keyboard().press("Tab")
      correctMeridiem(meridiem)
      s"$year-$month-$day"
    case _ =>
      throw Refresh.ActMiss("date is not valid", confirmed = false)

  private def monthText(month: Int, current: String): String =
    if current.nonEmpty && current.forall(_.isDigit) then month.toString
    else Month.of(month).getDisplayName(TextStyle.FULL, Locale.ENGLISH)

  private def openDate(deadline: Long): Unit =
    if dateTaken().count() == 0 then
      val button: Locator = page.locator("[aria-label=\"Open info\"]")
      if button.count() > 0 then click(button.last())
      else page.keyboard().press("i")
    awaitCount(dateTaken(), deadline, "Date taken is absent")
    click(dateTaken().last())
    awaitCount(page.locator("input[aria-label=\"Year\"]"), deadline, "Year is absent")

  private def dateTaken(): Locator =
    page.locator("[aria-label^=\"Date taken\"]")

  private def fieldInput(label: String): Locator =
    val found: Locator = page.locator(s"input[aria-label=\"$label\"]")
    awaitCount(found, System.currentTimeMillis + 20000L, s"$label is absent")
    found.last()

  private def correctMeridiem(wanted: String): Unit =
    val input: Locator = fieldInput("AM/PM")
    if input.inputValue().trim.equalsIgnoreCase(wanted) then ()
    else
      click(input)
      val option: Locator = page.getByRole(
        AriaRole.OPTION,
        new Page.GetByRoleOptions().setName(wanted).setExact(true)
      )
      if option.count() > 0 then click(option.first())
      else
        page.keyboard().press("Control+A")
        page.keyboard().insertText(wanted)
        page.keyboard().press("Enter")
      if !input.inputValue().trim.equalsIgnoreCase(wanted) then
        throw Refresh.ActMiss("AM/PM differs", confirmed = false)

  private def confirmDate(): Unit =
    refuseDialog()
    Seq("Save", "Done", "OK").find(visibleButton) match
      case Some(name) =>
        val found: Locator = page.getByRole(
          AriaRole.BUTTON,
          new Page.GetByRoleOptions().setName(name).setExact(true)
        )
        click(found.first())
      case None =>
        throw Refresh.ActMiss("Save, Done, or OK is absent", confirmed = false)

  private def readItemDay(key: String): String =
    if !onPhotos || !wizardReady then ensureSession()
    page.evaluate(PhotosScripts.itemDateScript, key) match
      case text: String => text
      case _            => throw IllegalStateException("read failed")

  private val clock: Regex = """(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})""".r

  private def clickMore(): Unit =
    val options: Locator = page.locator("[aria-label=\"More options\"]")
    awaitCount(options, System.currentTimeMillis + 20000L, "More options is absent")
    click(options.last())

  @tailrec
  private def awaitMenu(deadline: Long): String =
    val names: Seq[String] = Seq("Edit album", "Rename album", "Rename")
    names.find(name => menuItem(name).count() > 0) match
      case Some(name)                                   => name
      case None if System.currentTimeMillis >= deadline =>
        throw Refresh.ActMiss("menuitem is absent", confirmed = false)
      case None =>
        Thread.sleep(300)
        awaitMenu(deadline)

  private def menuItem(name: String): Locator =
    page.getByRole(AriaRole.MENUITEM, new Page.GetByRoleOptions().setName(name).setExact(true))

  @tailrec
  private def focusTitle(current: String, deadline: Long): Unit =
    if asBool(page.evaluate(PhotosScripts.focusTitleScript, current)) then ()
    else if System.currentTimeMillis >= deadline then throw Refresh.ActMiss("title field is absent", confirmed = false)
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
    val editorOpen: Boolean = done.count() > 0
    if editorOpen && System.currentTimeMillis < deadline then
      Thread.sleep(300)
      awaitLive(albumId, updated, deadline)
    else if editorOpen then throw Refresh.ActMiss("live title differs", confirmed = true)
    else
      // The live title is this id's card on the albums list.
      val live: Option[String] = albumCards().find(_.id == albumId).map(_.title)
      if live.contains(updated) then ()
      else throw Refresh.ActMiss("live title differs", confirmed = true)

  private def awaitEditor(locator: Locator, absent: String): Unit =
    try awaitCount(locator, System.currentTimeMillis + 20000L, absent)
    catch
      case miss: Refresh.ActMiss =>
        val detail: String = Option(miss.getMessage).filter(_.nonEmpty).getOrElse(absent)
        throw Refresh.ActMiss(detail, confirmed = true)

  @tailrec
  private def awaitCount(locator: Locator, deadline: Long, absent: String): Unit =
    if locator.count() > 0 then ()
    else if System.currentTimeMillis >= deadline then throw Refresh.ActMiss(absent, confirmed = false)
    else
      Thread.sleep(300)
      awaitCount(locator, deadline, absent)

  private def click(locator: Locator): Unit =
    locator.click(new Locator.ClickOptions().setTimeout(20000))

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
      case text: String   => Integer.parseInt(text)
      case number: Number => number.intValue
      case _              => 0

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
    try asBool(page.evaluate(PhotosScripts.wizardScript))
    catch
      case _: Exception =>
        false

  private def onPhotos: Boolean =
    try
      val host: String = new java.net.URI(page.url()).getHost
      host == "photos.google.com"
    catch
      case _: Exception =>
        false

  private def asBool(value: Object): Boolean = value match
    case flag: java.lang.Boolean => flag.booleanValue
    case _                       => false

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
