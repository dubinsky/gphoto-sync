// Acts on the sandbox photo URL saved by sandbox.mjs.
// Refuses to continue unless the viewer screenshot is the blue sandbox image.
// Parks the window on a Hyprland headless output so a workspace switch does not hide the page.
import { createRequire } from 'node:module'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { execFileSync } from 'node:child_process'

const require = createRequire(path.join(
  os.homedir(),
  '.local/share/gphoto-sync/reader/node_modules/playwright-core/package.json'
))
const { chromium } = require('playwright-core')

const home = os.homedir()
const profile = path.join(home, '.config/gphoto-sync-chrome')
const work = path.join(os.tmpdir(), 'gphoto-sync-refresh')
const statePath = path.join(work, 'sandbox-state.json')
const shot = '/tmp/gphoto-sandbox-step.png'
const phase = process.argv[2] || 'labels'

let createdHeadless = null
let savedCursor = null
let savedMonitor = null

function log(message) {
  const line = new Date().toISOString() + ' ' + message
  console.log(line)
  fs.appendFileSync(path.join(work, 'sandbox-act.log'), line + '\n')
}

function hypr(args) {
  return execFileSync('hyprctl', args, { encoding: 'utf8' })
}

function hyprJson(command) {
  return JSON.parse(hypr([command, '-j']))
}

function catalogChromePid() {
  for (const ent of fs.readdirSync('/proc')) {
    if (!/^\d+$/.test(ent)) continue
    let text = ''
    try {
      text = fs.readFileSync('/proc/' + ent + '/cmdline').toString('utf8')
    } catch {
      continue
    }
    if (text.includes('gphoto-sync-chrome') && text.includes('remote-debugging-pipe') && !text.includes('--type=')) {
      return Number(ent)
    }
  }
  return null
}

function rememberCursor() {
  if (savedCursor) return
  const match = hypr(['cursorpos']).match(/(-?\d+)\s*,\s*(-?\d+)/)
  if (match) savedCursor = { x: Number(match[1]), y: Number(match[2]) }
}

function restoreCursor() {
  if (!savedCursor) return
  hypr(['dispatch', 'hl.dsp.cursor.move({ x = ' + savedCursor.x + ', y = ' + savedCursor.y + ' })'])
}

function ourChromeClient() {
  const pid = catalogChromePid()
  if (!pid) return null
  const clients = hyprJson('clients')
  return clients.find(entry => entry.pid === pid) || null
}

function parkOffWorkspace() {
  if (!process.env.HYPRLAND_INSTANCE_SIGNATURE) return
  rememberCursor()
  const started = Date.now()
  let client = null
  while (Date.now() - started < 20000) {
    client = ourChromeClient()
    if (client) break
    execFileSync('sleep', ['0.3'])
  }
  if (!client) {
    log('park skipped: window missing')
    return
  }
  const before = hyprJson('monitors')
  const focused = before.find(monitor => monitor.focused) || before[0]
  savedMonitor = focused ? { name: focused.name, x: focused.x, y: focused.y } : null
  const names = new Set(before.map(monitor => monitor.name))
  if (![...names].some(name => name.startsWith('HEADLESS-'))) {
    hypr(['output', 'create', 'headless'])
    const created = hyprJson('monitors').map(monitor => monitor.name).filter(name => !names.has(name))
    createdHeadless = created[0] || null
  }
  const headless = createdHeadless || hyprJson('monitors').map(monitor => monitor.name).find(name => name.startsWith('HEADLESS-'))
  if (!headless) {
    log('park skipped: headless missing')
    return
  }
  const lua = [
    'hl.monitor({ output = ' + JSON.stringify(headless) + ', mode = "1280x800@60", position = "20000x20000", scale = 1 })',
    savedMonitor
      ? 'hl.monitor({ output = ' + JSON.stringify(savedMonitor.name) + ', mode = "preferred", position = ' + JSON.stringify(savedMonitor.x + 'x' + savedMonitor.y) + ', scale = "auto" })'
      : '',
  ].filter(Boolean).join('\n')
  hypr(['eval', lua])
  hypr(['dispatch', 'hl.dsp.window.move({ monitor = ' + JSON.stringify(headless) + ', follow = false, window = "address:' + client.address + '" })'])
  const parked = ourChromeClient()
  log('parked on ' + headless + (parked && parked.size ? ' size ' + parked.size.join('x') : ''))
  restoreCursor()
}

function unpark() {
  try {
    if (savedMonitor) {
      hypr(['eval', 'hl.monitor({ output = ' + JSON.stringify(savedMonitor.name) + ', mode = "preferred", position = ' + JSON.stringify(savedMonitor.x + 'x' + savedMonitor.y) + ', scale = "auto" })'])
    }
    if (createdHeadless) {
      hypr(['output', 'remove', createdHeadless])
      log('removed ' + createdHeadless)
      createdHeadless = null
    }
    restoreCursor()
  } catch (error) {
    log('unpark: ' + (error && error.message ? error.message.split('\n')[0] : String(error)))
  }
}

function blueSpot(file) {
  const dim = execFileSync('ffprobe', [
    '-v', 'error', '-select_streams', 'v:0', '-show_entries', 'stream=width,height', '-of', 'csv=p=0', file,
  ], { encoding: 'utf8' }).trim()
  const [width, height] = dim.split(',').map(Number)
  const raw = execFileSync('ffmpeg', [
    '-v', 'error', '-i', file, '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-',
  ], { encoding: 'buffer', maxBuffer: width * height * 3 + 1024 })
  let count = 0
  let sumX = 0
  let sumY = 0
  let minX = width
  let minY = height
  let maxX = 0
  let maxY = 0
  for (let i = 0; i + 2 < raw.length; i += 3) {
    const r = raw[i]
    const g = raw[i + 1]
    const b = raw[i + 2]
    if (!(b > 140 && r < 80 && g < 100)) continue
    const pixel = i / 3
    const x = pixel % width
    const y = Math.floor(pixel / width)
    count += 1
    sumX += x
    sumY += y
    if (x < minX) minX = x
    if (y < minY) minY = y
    if (x > maxX) maxX = x
    if (y > maxY) maxY = y
  }
  return {
    count,
    x: count ? Math.round(sumX / count) : 0,
    y: count ? Math.round(sumY / count) : 0,
    w: count ? maxX - minX : 0,
    h: count ? maxY - minY : 0,
  }
}

function blueCount(file) {
  const dim = execFileSync('ffprobe', [
    '-v', 'error', '-select_streams', 'v:0', '-show_entries', 'stream=width,height', '-of', 'csv=p=0', file,
  ], { encoding: 'utf8' }).trim()
  const [width, height] = dim.split(',').map(Number)
  const raw = execFileSync('ffmpeg', [
    '-v', 'error', '-i', file, '-f', 'rawvideo', '-pix_fmt', 'rgb24', '-',
  ], { encoding: 'buffer', maxBuffer: width * height * 3 + 1024 })
  let count = 0
  for (let i = 0; i + 2 < raw.length; i += 3) {
    const r = raw[i]
    const g = raw[i + 1]
    const b = raw[i + 2]
    if (b > 140 && r < 80 && g < 100) count += 1
  }
  return { width, height, count }
}

const context = await chromium.launchPersistentContext(profile, {
  executablePath: '/usr/bin/google-chrome-stable',
  headless: false,
  viewport: { width: 1100, height: 700 },
  acceptDownloads: true,
  ignoreDefaultArgs: ['--enable-automation'],
  args: ['--disable-blink-features=AutomationControlled', '--no-first-run', '--no-default-browser-check'],
})
const page = context.pages()[0] || await context.newPage()
page.setDefaultTimeout(20000)
const state = JSON.parse(fs.readFileSync(statePath, 'utf8'))

async function confirmSandbox() {
  await page.goto(state.href, { waitUntil: 'domcontentloaded', timeout: 60000 })
  await page.waitForTimeout(2500)
  await page.screenshot({ path: shot })
  const sample = blueCount(shot)
  log('blue ' + sample.count + ' of ' + sample.width + 'x' + sample.height)
  if (sample.count < 1500) throw new Error('viewer is not the sandbox image')
}

function loadProgress() {
  return JSON.parse(fs.readFileSync(path.join(work, 'sandbox-progress.json'), 'utf8'))
}

async function pageFlags() {
  return await page.evaluate(() => {
    const lines = (document.body.innerText || '').split('\n').map(line => line.trim())
    return {
      edit: lines.includes('Edit album'),
      placeholder: lines.includes('Add a title'),
      exact: lines.find(line => line.toLowerCase() === 'gphoto-sync validation') || '',
      renamed: lines.find(line => line.toLowerCase() === 'gphoto-sync validation renamed') || '',
      albumPage: /\/album\//.test(location.pathname),
    }
  })
}

try {
  parkOffWorkspace()
  if (phase === 'save-album') {
    const progress = loadProgress()
    await page.goto(progress.href, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2500)
    await page.screenshot({ path: shot })
    const sample = blueCount(shot)
    const flags = await pageFlags()
    log('album ' + JSON.stringify({ ...flags, blue: sample.count }))
    if (sample.count < 200) throw new Error('album page is not the sandbox photo')
    if (flags.placeholder && !flags.ours) {
      const titleField = page.getByText('Add a title', { exact: true })
      const titleCount = await titleField.count()
      let titleBounds = null
      let titleArea = 0
      for (let index = 0; index < titleCount; index += 1) {
        const bounds = await titleField.nth(index).boundingBox()
        if (!bounds || bounds.height < 16 || bounds.height > 140 || bounds.width < 80) continue
        const area = bounds.width * bounds.height
        if (area > titleArea) {
          titleArea = area
          titleBounds = bounds
        }
      }
      if (!titleBounds) throw new Error('title field has no box')
      await page.mouse.click(titleBounds.x + titleBounds.width / 2, titleBounds.y + Math.min(titleBounds.height / 2, 30))
      await page.keyboard.type('gphoto-sync validation')
    }
    const done = page.getByRole('button', { name: 'Done', exact: true })
    if (await done.count()) {
      await done.first().click()
      log('clicked Done')
      await page.waitForTimeout(2500)
    }
    const saved = await pageFlags()
    log('saved ' + JSON.stringify(saved))
    if (!saved.exact) throw new Error('album title was not saved')
    fs.writeFileSync(path.join(work, 'sandbox-progress.json'), JSON.stringify({
      album: saved.exact,
      href: page.url(),
    }) + '\n')
    if (!saved.renamed && !saved.edit) {
      await page.locator('[aria-label="More options"]').last().click()
      await page.waitForTimeout(400)
      const menu = await page.evaluate(() => {
        return Array.from(document.querySelectorAll('[role="menuitem"], [role="option"]'))
          .map(node => (node.innerText || '').trim().replace(/\s+/g, ' '))
          .slice(0, 20)
      })
      log('menu ' + menu.join(' | '))
      const renameItem = menu.find(text => /^(edit album|rename album|rename)$/i.test(text))
      if (!renameItem) {
        log('stopped before rename')
      } else {
        await page.getByRole('menuitem', { name: renameItem, exact: true }).click()
        await page.waitForTimeout(1500)
        const focused = await page.evaluate(title => {
          const wanted = title.toLowerCase()
          const fields = Array.from(document.querySelectorAll('input, textarea, [contenteditable="true"]'))
          const match = fields.find(node => (node.value || node.innerText || '').trim().toLowerCase() === wanted)
          const target = match || fields.find(node => /add a title/i.test((node.getAttribute('placeholder') || '') + (node.innerText || '')))
          if (target) target.focus()
          const node = document.activeElement
          return {
            focused: Boolean(node && (node === target || node.tagName === 'INPUT' || node.tagName === 'TEXTAREA' || node.isContentEditable)),
            matched: Boolean(match),
          }
        }, saved.exact)
        log('title field ' + JSON.stringify(focused))
        if (focused.focused) {
          await page.keyboard.press('Control+A')
          await page.keyboard.type('gphoto-sync validation renamed')
          if (await done.count()) await done.first().click()
          else await page.keyboard.press('Enter')
          await page.waitForTimeout(1500)
          const renamed = await pageFlags()
          log('renamed ' + JSON.stringify(renamed))
          if (renamed.renamed) {
            fs.writeFileSync(path.join(work, 'sandbox-progress.json'), JSON.stringify({
              album: renamed.renamed,
              href: page.url(),
            }) + '\n')
          }
        }
      }
    }
  } else if (phase === 'describe') {
    const progress = loadProgress()
    await page.goto(state.href, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2500)
    await page.screenshot({ path: shot })
    const viewer = blueCount(shot)
    log('viewer blue ' + viewer.count)
    if (viewer.count < 1500) throw new Error('opened item is not the sandbox image')
    async function descriptionReady() {
      return await page.evaluate(() => (document.body.innerText || '').includes('Add a description'))
    }
    let ready = false
    const started = Date.now()
    while (Date.now() - started < 5000) {
      if (await descriptionReady()) {
        ready = true
        break
      }
      await page.waitForTimeout(300)
    }
    if (!ready) {
      const info = page.locator('[aria-label="Open info"]')
      if (await info.count()) await info.last().click()
      else await page.keyboard.press('i')
      const opened = Date.now()
      while (Date.now() - opened < 5000) {
        if (await descriptionReady()) break
        await page.waitForTimeout(300)
      }
    }
    const panel = await page.evaluate(() => {
      const lines = (document.body.innerText || '').split('\n').map(line => line.trim()).filter(Boolean)
      const filename = lines.find(line => /\.(jpe?g|png|heic|gif|webp)$/i.test(line)) || ''
      const labels = Array.from(document.querySelectorAll('button, [role="button"], input, textarea'))
        .map(node => node.getAttribute('aria-label') || node.getAttribute('placeholder') || '')
        .filter(text => /description|date|time|info|edit|done|save/i.test(text))
        .slice(0, 20)
      return {
        filename,
        labels,
        addDescription: lines.includes('Add a description'),
        descriptionSet: lines.includes('gphoto-sync sandbox description'),
      }
    })
    log('panel ' + JSON.stringify(panel))
    const next = { ...progress, item: page.url() }
    if (panel.filename) next.filename = panel.filename
    fs.writeFileSync(path.join(work, 'sandbox-progress.json'), JSON.stringify(next) + '\n')
    if (!panel.descriptionSet && panel.addDescription) {
      const field = page.getByText('Add a description', { exact: true })
      const fieldCount = await field.count()
      let fieldIndex = -1
      for (let index = 0; index < fieldCount; index += 1) {
        const bounds = await field.nth(index).boundingBox()
        if (bounds && bounds.x > 700 && bounds.height > 8 && bounds.height < 80) fieldIndex = index
      }
      log('description field ' + fieldIndex + ' of ' + fieldCount)
      if (fieldIndex < 0) throw new Error('description field is not in the info panel')
      const bounds = await field.nth(fieldIndex).boundingBox()
      const focused = await page.evaluate(({ x, y }) => {
        const editors = Array.from(document.querySelectorAll('[contenteditable="true"], textarea, input')).map(node => {
          const raw = (node.innerText || node.value || '').trim()
          const safe = /^(add a description|gphoto-sync sandbox description)?$/i.test(raw) ? raw : (raw ? 'other' : '')
          return {
            tag: node.tagName,
            label: node.getAttribute('aria-label') || '',
            placeholder: node.getAttribute('placeholder') || '',
            safe,
          }
        })
        const node = Array.from(document.querySelectorAll('[contenteditable="true"], textarea, input')).find(item => {
          const blob = (item.getAttribute('aria-label') || '') + ' ' + (item.getAttribute('placeholder') || '') + ' ' + (item.innerText || '')
          return /description/i.test(blob)
        })
        if (node) node.focus()
        const atPoint = document.elementFromPoint(x, y)
        return {
          editors,
          focused: Boolean(node),
          point: atPoint ? atPoint.tagName + ' ' + (atPoint.getAttribute('aria-label') || '') : '',
        }
      }, { x: Math.round(bounds.x + 12), y: Math.round(bounds.y + Math.min(bounds.height / 2, 20)) })
      log('description focus ' + JSON.stringify(focused))
      const active = await page.evaluate(() => {
        const node = document.activeElement
        return {
          tag: node ? node.tagName : '',
          label: node ? (node.getAttribute('aria-label') || '') : '',
          editable: Boolean(node && (node.tagName === 'INPUT' || node.tagName === 'TEXTAREA' || node.isContentEditable)),
        }
      })
      log('description active ' + JSON.stringify(active))
      await page.screenshot({ path: shot })
      if (active.editable && !/search/i.test(active.label)) {
        await page.keyboard.type('gphoto-sync sandbox description')
        await page.keyboard.press('Tab')
        await page.waitForTimeout(1000)
      }
      const set = await page.evaluate(() => {
        const wanted = 'gphoto-sync sandbox description'
        const inField = Array.from(document.querySelectorAll('input, textarea, [contenteditable="true"]'))
          .some(node => (node.value || node.innerText || '').includes(wanted))
        return { inText: (document.body.innerText || '').includes(wanted), inField }
      })
      log('description visible ' + JSON.stringify(set))
      if (set.inText || set.inField) next.description = 'gphoto-sync sandbox description'
      fs.writeFileSync(path.join(work, 'sandbox-progress.json'), JSON.stringify(next) + '\n')
    }
  } else if (phase === 'date') {
    const progress = loadProgress()
    await page.goto(state.href, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2500)
    await page.screenshot({ path: shot })
    const viewer = blueCount(shot)
    log('viewer blue ' + viewer.count)
    if (viewer.count < 1500) throw new Error('opened item is not the sandbox image')
    if (!(await page.evaluate(() => (document.body.innerText || '').includes('Add a description') || (document.body.innerText || '').includes('gphoto-sync sandbox description')))) {
      await page.locator('[aria-label="Open info"]').last().click()
      await page.waitForTimeout(1000)
    }
    const kept = await page.evaluate(() => (document.body.innerText || '').includes('gphoto-sync sandbox description'))
    log('description kept ' + kept)
    await page.evaluate(() => {
      const heading = Array.from(document.querySelectorAll('div, span, h2, h3')).find(node => (node.innerText || '').trim() === 'Details')
      if (heading) heading.scrollIntoView({ block: 'center' })
    })
    await page.waitForTimeout(500)
    const detail = await page.evaluate(() => {
      const lines = (document.body.innerText || '').split('\n').map(line => line.trim()).filter(Boolean)
      const index = lines.findIndex(line => line === 'Details')
      const nearby = (index < 0 ? [] : lines.slice(index, index + 12)).filter(line => /detail|date|time|oct|jpg|gphoto|20\d\d|edit/i.test(line))
      const labels = Array.from(document.querySelectorAll('button, [role="button"], [aria-label]'))
        .map(node => node.getAttribute('aria-label') || '')
        .filter(label => /date|time/i.test(label))
        .slice(0, 12)
      return { nearby, labels }
    })
    log('details ' + JSON.stringify(detail))
    const dateButton = page.locator('[aria-label^="Date taken"]')
    const dateCount = await dateButton.count()
    let dateIndex = -1
    for (let index = 0; index < dateCount; index += 1) {
      const bounds = await dateButton.nth(index).boundingBox()
      if (bounds && bounds.width > 20 && bounds.height > 8 && bounds.x > 700) dateIndex = index
    }
    log('date button ' + dateIndex + ' of ' + dateCount)
    if (dateIndex < 0) throw new Error('date control is not in the info panel')
    await dateButton.nth(dateIndex).click()
    await page.waitForTimeout(800)
    const fields = await page.evaluate(() => {
      return Array.from(document.querySelectorAll('input, textarea, [role="dialog"] button')).map(node => ({
        tag: node.tagName,
        label: node.getAttribute('aria-label') || '',
        placeholder: node.getAttribute('placeholder') || '',
        type: node.getAttribute('type') || '',
      })).filter(field => /date|time|hour|minute|year|month|day|save|done|cancel/i.test(field.label + ' ' + field.placeholder + ' ' + field.type) || field.tag === 'INPUT').slice(0, 12)
    })
    log('date fields ' + JSON.stringify(fields))
    const current = await page.evaluate(() => {
      const wanted = ['Year', 'Month', 'Day', 'Hour', 'Minutes', 'AM/PM']
      const out = {}
      for (const label of wanted) {
        const node = Array.from(document.querySelectorAll('input')).find(item => {
          return item.getAttribute('aria-label') === label && item.getBoundingClientRect().width > 8
        })
        const value = node ? String(node.value || '').trim() : ''
        out[label] = /^(?:\d{1,4}|am|pm|oct|october|jan|january)?$/i.test(value) ? value : (value ? 'other' : '')
      }
      return out
    })
    log('date values ' + JSON.stringify(current))
    const monthValue = /^\d+$/.test(current.Month) ? '1' : 'January'
    const choices = { Year: '2001', Month: monthValue, Day: '1', Hour: '1', Minutes: '02', 'AM/PM': 'AM' }
    for (const [label, value] of Object.entries(choices)) {
      const input = page.locator('input[aria-label="' + label + '"]')
      const count = await input.count()
      let index = -1
      for (let item = 0; item < count; item += 1) {
        const bounds = await input.nth(item).boundingBox()
        if (bounds && bounds.width > 8 && bounds.height > 8) index = item
      }
      if (index < 0) throw new Error('missing ' + label)
      await input.nth(index).click()
      await page.keyboard.press('Control+A')
      await page.keyboard.type(value)
      await page.keyboard.press('Tab')
    }
    const ampm = page.locator('input[aria-label="AM/PM"]')
    let ampmIndex = -1
    const ampmCount = await ampm.count()
    for (let item = 0; item < ampmCount; item += 1) {
      const bounds = await ampm.nth(item).boundingBox()
      if (bounds && bounds.width > 8 && bounds.height > 8) ampmIndex = item
    }
    if (ampmIndex >= 0) {
      let ampmValue = await ampm.nth(ampmIndex).inputValue()
      log('ampm ' + ampmValue)
      if (ampmValue.trim().toUpperCase() !== 'AM') {
        await ampm.nth(ampmIndex).click()
        const option = page.getByRole('option', { name: 'AM', exact: true })
        if (await option.count()) await option.first().click()
        else {
          await page.keyboard.press('Control+A')
          await page.keyboard.type('AM')
          await page.keyboard.press('Enter')
        }
        ampmValue = await ampm.nth(ampmIndex).inputValue()
        log('ampm after ' + ampmValue)
      }
    }
    const save = page.getByRole('button', { name: /^(Save|Done|OK)$/ })
    if (await save.count()) {
      await save.first().click()
      log('clicked date save')
    } else {
      const names = await page.evaluate(() => {
        return Array.from(document.querySelectorAll('button, [role="button"]'))
          .map(node => (node.getAttribute('aria-label') || node.innerText || '').trim().replace(/\s+/g, ' '))
          .filter(name => /^(save|done|ok|cancel)$/i.test(name))
          .slice(0, 8)
      })
      log('date save buttons ' + names.join(' | '))
      throw new Error('date save button was not found')
    }
    await page.waitForTimeout(1500)
    const after = await page.evaluate(() => {
      const lines = (document.body.innerText || '').split('\n').map(line => line.trim())
      return {
        year: lines.some(line => line.includes('2001')),
        description: lines.includes('gphoto-sync sandbox description'),
      }
    })
    log('date saved ' + JSON.stringify(after))
    await page.reload({ waitUntil: 'domcontentloaded' })
    await page.waitForTimeout(2500)
    await page.screenshot({ path: shot })
    const again = blueCount(shot)
    if (again.count < 1500) throw new Error('reloaded item is not the sandbox image')
    if (!(await page.locator('[aria-label^="Date taken"]').count())) {
      await page.locator('[aria-label="Open info"]').last().click()
      await page.waitForTimeout(1000)
    }
    const shown = await page.evaluate(() => {
      return Array.from(document.querySelectorAll('[aria-label^="Date taken"], [aria-label^="Time taken"]')).filter(node => {
        const rect = node.getBoundingClientRect()
        return rect.width > 20 && rect.x > 700
      }).map(node => node.getAttribute('aria-label'))
    })
    log('visible date ' + JSON.stringify(shown))
  } else if (phase === 'check') {
    await page.goto(state.href, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2500)
    await page.screenshot({ path: shot })
    const viewer = blueCount(shot)
    log('viewer blue ' + viewer.count)
    if (viewer.count < 1500) throw new Error('opened item is not the sandbox image')
    const hasDate = await page.locator('[aria-label^="Date taken"]').count()
    if (!hasDate) {
      await page.locator('[aria-label="Open info"]').last().click()
      await page.waitForTimeout(1000)
    }
    const facts = await page.evaluate(() => {
      const labels = Array.from(document.querySelectorAll('[aria-label]'))
        .map(node => node.getAttribute('aria-label') || '')
        .filter(label => /^Date taken:|^Time taken:/.test(label))
      return {
        labels: labels.slice(0, 6),
        description: (document.body.innerText || '').includes('gphoto-sync sandbox description'),
        filename: (document.body.innerText || '').includes('gphoto-sync-sandbox.jpg'),
      }
    })
    log('check ' + JSON.stringify(facts))
  } else if (phase === 'download') {
    await page.goto(state.href, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2500)
    await page.screenshot({ path: shot })
    const viewer = blueCount(shot)
    log('viewer blue ' + viewer.count)
    if (viewer.count < 1500) throw new Error('opened item is not the sandbox image')
    const staged = path.join(home, 'Pictures/originals/.gphoto-sync/.download/2001/01/01/gphoto-sync-sandbox.jpg')
    if (fs.existsSync(staged)) throw new Error('staged file already exists')
    const downloadPromise = page.waitForEvent('download', { timeout: 30000 })
    await page.locator('[aria-label="More options"]').last().click()
    await page.getByRole('menuitem', { name: /^Download\b/ }).click()
    const download = await downloadPromise
    const suggested = download.suggestedFilename()
    log('suggested ' + suggested)
    if (suggested !== 'gphoto-sync-sandbox.jpg') throw new Error('download name is not the sandbox file')
    fs.mkdirSync(path.dirname(staged), { recursive: true })
    await download.saveAs(staged)
    const info = execFileSync('ffprobe', ['-v', 'error', '-select_streams', 'v:0', '-show_entries', 'stream=width,height', '-of', 'csv=p=0', staged], { encoding: 'utf8' }).trim()
    log('staged ' + info + ' bytes ' + fs.statSync(staged).size)
  } else if (phase === 'move') {
    const progress = loadProgress()
    const source = progress.href
    await page.goto(state.href, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2000)
    await page.screenshot({ path: shot })
    if (blueCount(shot).count < 1500) throw new Error('opened item is not the sandbox image')
    await page.locator('[aria-label="More options"]').last().click()
    await page.getByRole('menuitem', { name: 'Add to album' }).click()
    await page.getByRole('option', { name: 'New album', exact: true }).click()
    const titleField = page.getByText('Add a title', { exact: true })
    await titleField.first().waitFor({ timeout: 20000 })
    const titleCount = await titleField.count()
    let titleBounds = null
    let titleArea = 0
    for (let index = 0; index < titleCount; index += 1) {
      const bounds = await titleField.nth(index).boundingBox()
      if (!bounds || bounds.height < 16 || bounds.height > 140 || bounds.width < 80) continue
      const area = bounds.width * bounds.height
      if (area > titleArea) {
        titleArea = area
        titleBounds = bounds
      }
    }
    if (!titleBounds) throw new Error('destination title field has no box')
    await page.mouse.click(titleBounds.x + titleBounds.width / 2, titleBounds.y + Math.min(titleBounds.height / 2, 30))
    await page.keyboard.type('gphoto-sync validation dest')
    await page.getByRole('button', { name: 'Done', exact: true }).click()
    await page.waitForTimeout(2500)
    const destTitle = await page.evaluate(() => {
      const lines = (document.body.innerText || '').split('\n').map(line => line.trim())
      return lines.find(line => line.toLowerCase() === 'gphoto-sync validation dest') || ''
    })
    log('dest title ' + JSON.stringify(destTitle))
    if (!destTitle) throw new Error('destination album title was not saved')
    const dest = page.url()
    fs.writeFileSync(path.join(work, 'sandbox-progress.json'), JSON.stringify({ ...progress, dest, destTitle }) + '\n')
    await page.goto(source, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2000)
    await page.screenshot({ path: shot })
    const before = blueCount(shot)
    log('source blue ' + before.count)
    if (before.count < 200) throw new Error('source album is not showing the sandbox photo')
    await page.locator('[aria-label="More options"]').last().click()
    await page.getByRole('menuitem', { name: 'Select photos', exact: true }).click()
    await page.waitForTimeout(500)
    await page.locator('a[href*="/photo/"], [role="checkbox"]').first().click()
    await page.waitForTimeout(500)
    const remove = page.getByRole('button', { name: /^Remove/ })
    if (!(await remove.count())) {
      const names = await page.evaluate(() => {
        return Array.from(document.querySelectorAll('button, [role="button"], [role="menuitem"]'))
          .map(node => (node.getAttribute('aria-label') || node.innerText || '').trim().replace(/\s+/g, ' '))
          .filter(name => /remove|selected/i.test(name))
          .slice(0, 8)
      })
      log('remove controls ' + names.join(' | '))
      throw new Error('remove control was not found')
    }
    await remove.first().click()
    await page.waitForTimeout(600)
    const dialog = page.locator('[role="dialog"]')
    if (await dialog.count()) {
      const kind = await dialog.first().evaluate(node => {
        const text = node.innerText || ''
        if (/trash|delete forever/i.test(text)) return 'trash'
        if (/remove/i.test(text)) return 'remove'
        return 'other'
      })
      log('confirm ' + kind)
      if (kind !== 'remove') throw new Error('refusing a confirm dialog that is not remove')
      await dialog.first().getByRole('button', { name: /^Remove\b/ }).click()
    }
    await page.waitForTimeout(2000)
    await page.screenshot({ path: shot })
    log('source after ' + blueCount(shot).count)
    await page.goto(dest, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2000)
    await page.screenshot({ path: shot })
    log('dest blue ' + blueCount(shot).count)
  } else if (phase === 'remove') {
    const progress = loadProgress()
    await page.goto(progress.href, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2000)
    await page.screenshot({ path: shot })
    const before = blueCount(shot)
    log('source blue ' + before.count)
    if (before.count < 200) throw new Error('source album is not showing the sandbox photo')
    await page.locator('a[href*="/photo/"]').first().click()
    await page.waitForTimeout(2000)
    await page.screenshot({ path: shot })
    const opened = blueCount(shot)
    log('opened blue ' + opened.count)
    if (opened.count < 1500) throw new Error('opened item is not the sandbox image')
    await page.locator('[aria-label="More options"]').last().click()
    await page.waitForTimeout(400)
    const menu = await page.evaluate(() => {
      return Array.from(document.querySelectorAll('[role="menuitem"]'))
        .map(node => (node.innerText || '').trim().replace(/\s+/g, ' '))
        .slice(0, 20)
    })
    log('menu ' + menu.join(' | '))
    const remove = page.getByRole('menuitem', { name: /Remove from album/ })
    if (!(await remove.count())) throw new Error('remove from album is not in the menu')
    await remove.last().click()
    await page.waitForTimeout(600)
    const dialog = page.locator('[role="dialog"]')
    if (await dialog.count()) {
      const kind = await dialog.first().evaluate(node => {
        const text = node.innerText || ''
        if (/trash|delete forever/i.test(text)) return 'trash'
        if (/remove/i.test(text)) return 'remove'
        return 'other'
      })
      log('confirm ' + kind)
      if (kind !== 'remove') throw new Error('refusing a confirm dialog that is not remove')
      await dialog.first().getByRole('button', { name: /^Remove\b/ }).click()
    }
    await page.waitForTimeout(1500)
    await page.goto(progress.href, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2000)
    await page.screenshot({ path: shot })
    log('source after ' + blueCount(shot).count)
    await page.goto(progress.dest, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2000)
    await page.screenshot({ path: shot })
    log('dest blue ' + blueCount(shot).count)
  } else if (phase === 'trash') {
    await page.goto(state.href, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2500)
    await page.screenshot({ path: shot })
    const viewer = blueCount(shot)
    log('viewer blue ' + viewer.count)
    if (viewer.count < 1500) throw new Error('opened item is not the sandbox image')
    const trashButton = page.locator('[aria-label="Move to trash"]')
    if (await trashButton.count()) await trashButton.last().click()
    else {
      await page.locator('[aria-label="More options"]').last().click()
      await page.getByRole('menuitem', { name: /Move to trash/ }).last().click()
    }
    await page.waitForTimeout(800)
    const dialog = page.locator('[role="alertdialog"], [role="dialog"]')
    if (await dialog.count()) {
      const buttons = await dialog.first().evaluate(node => {
        return Array.from(node.querySelectorAll('button, [role="button"]'))
          .map(item => (item.innerText || item.getAttribute('aria-label') || '').trim().replace(/\s+/g, ' '))
          .filter(name => /trash|cancel|delete|move|undo/i.test(name))
          .slice(0, 8)
      })
      log('dialog buttons ' + buttons.join(' | '))
      const forever = buttons.some(name => /delete forever|empty trash/i.test(name))
      if (forever) throw new Error('refusing delete forever')
      const confirmName = buttons.find(name => /^Move to trash\b/i.test(name))
      if (!confirmName) throw new Error('move to trash confirm button was not found')
      await dialog.first().getByRole('button', { name: confirmName }).click()
    }
    await page.waitForTimeout(1500)
    const notice = await page.evaluate(() => {
      const text = document.body.innerText || ''
      return { moved: /moved to trash/i.test(text), undo: /\bundo\b/i.test(text) }
    })
    log('notice ' + JSON.stringify(notice))
    await page.goto(state.href, { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2000)
    const after = await page.evaluate(() => {
      const text = document.body.innerText || ''
      return {
        restore: /restore/i.test(text),
        trash: /trash/i.test(text),
        filename: text.includes('gphoto-sync-sandbox.jpg'),
      }
    })
    await page.screenshot({ path: shot })
    log('after ' + JSON.stringify({ ...after, blue: blueCount(shot).count }))
    await page.goto('https://photos.google.com/trash', { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2500)
    const inTrash = await page.evaluate(() => (document.body.innerText || '').includes('gphoto-sync-sandbox.jpg'))
    await page.screenshot({ path: '/tmp/gphoto-sandbox-trash.png' })
    const trashBlue = blueCount('/tmp/gphoto-sandbox-trash.png')
    log('trash filename ' + inTrash + ' blue ' + trashBlue.count)
  } else if (phase === 'trash-check') {
    await page.goto('https://photos.google.com/trash', { waitUntil: 'domcontentloaded', timeout: 60000 })
    await page.waitForTimeout(2500)
    await page.screenshot({ path: shot })
    const spot = blueSpot(shot)
    log('trash spot ' + JSON.stringify(spot))
    if (spot.count < 20000) throw new Error('trash has no sandbox thumbnail')
    try {
      await page.mouse.click(spot.x, spot.y)
    } catch (error) {
      log('mouse ' + (error && error.message ? error.message.split('\n')[0] : String(error)))
      await page.evaluate(({ x, y }) => {
        const node = document.elementFromPoint(x, y)
        const target = node && (node.closest('a, [role="link"]') || node)
        if (target) target.click()
      }, spot)
    }
    await page.waitForTimeout(2000)
    await page.screenshot({ path: shot })
    const viewer = blueCount(shot)
    const facts = await page.evaluate(() => {
      const text = document.body.innerText || ''
      const restore = Array.from(document.querySelectorAll('button, [role="button"]'))
        .some(node => /restore/i.test((node.getAttribute('aria-label') || '') + ' ' + (node.innerText || '')))
      return { restore, filename: text.includes('gphoto-sync-sandbox.jpg'), sandbox: text.includes('GPHOTO SYNC SANDBOX') }
    })
    log('trashed item ' + JSON.stringify({ ...facts, blue: viewer.count }))
    if (viewer.count < 1500) throw new Error('opened trash item is not the sandbox image')
  } else {
  await confirmSandbox()
  if (phase === 'new-album') {
    await page.locator('[aria-label="More options"]').last().click()
    await page.getByRole('menuitem', { name: 'Add to album' }).click()
    const option = page.getByRole('option', { name: 'New album', exact: true })
    await option.waitFor({ state: 'visible' })
    await option.click()
    const titleField = page.getByText('Add a title', { exact: true })
    await titleField.first().waitFor({ timeout: 20000 })
    await page.screenshot({ path: shot })
    const sample = blueCount(shot)
    log('edit album blue ' + sample.count)
    if (sample.count < 200) throw new Error('edit page is not the sandbox photo')
    const titleCount = await titleField.count()
    let titleBounds = null
    let titleArea = 0
    for (let index = 0; index < titleCount; index += 1) {
      const bounds = await titleField.nth(index).boundingBox()
      if (!bounds || bounds.height < 16 || bounds.height > 140 || bounds.width < 80) continue
      const area = bounds.width * bounds.height
      if (area > titleArea) {
        titleArea = area
        titleBounds = bounds
      }
    }
    if (!titleBounds) throw new Error('title field has no box')
    await page.mouse.click(titleBounds.x + titleBounds.width / 2, titleBounds.y + Math.min(titleBounds.height / 2, 30))
    await page.keyboard.type('gphoto-sync validation')
    await page.waitForTimeout(400)
    const header = await page.evaluate(() => {
      return Array.from(document.querySelectorAll('button, [role="button"]')).map(node => {
        const rect = node.getBoundingClientRect()
        return {
          label: node.getAttribute('aria-label') || '',
          x: Math.round(rect.x),
          y: Math.round(rect.y),
          w: Math.round(rect.width),
          h: Math.round(rect.height),
        }
      }).filter(button => button.y < 90 && button.w > 8 && button.h > 8).slice(0, 12)
    })
    log('header ' + JSON.stringify(header))
    const named = header.find(button => button.label === 'Done')
    const check = named || header.slice().sort((a, b) => a.x - b.x)[0]
    if (!check) throw new Error('no edit-album confirm button')
    await page.mouse.click(check.x + check.w / 2, check.y + check.h / 2)
    log('clicked header ' + (check.label || 'leftmost'))
    await page.waitForTimeout(2500)
    const saw = await page.evaluate(() => (document.body.innerText || '').includes('gphoto-sync validation'))
    log('title visible ' + saw)
    if (!saw) throw new Error('album title was not saved')
    fs.writeFileSync(path.join(work, 'sandbox-progress.json'), JSON.stringify({
      album: 'gphoto-sync validation',
      href: page.url(),
    }) + '\n')
  }
  }
} catch (error) {
  log('FAILED ' + (error && error.message ? error.message.split('\n')[0] : String(error)))
  try { await page.screenshot({ path: shot }) } catch { /* page may already be gone */ }
  process.exitCode = 1
} finally {
  await context.close().catch(() => {})
  unpark()
}
