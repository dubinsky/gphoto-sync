const treeRoute = "/api/tree";
const dayRoute = "/api/day";
const albumRoute = "/api/album";
const thumbRoute = "/api/thumb";
const fileRoute = "/api/file";
const toggleRoute = "/api/toggle";
const albumsRoute = "/api/albums";

const stride = 160;
const gap = 8;

// Phrases for membership.reason. joined and ambiguous are not a sentence.
const reasonPhrase = {
  missing: "catalog missing",
  unreadable: "catalog unreadable",
  unjoined: "no catalog album",
  ambiguous: "ambiguous catalog title",
  "link-absent": "linked catalog album is absent",
  error: "catalog album was not fully read",
  "auto-add": "catalog album was not read"
};

const catalog = document.getElementById("catalog");
const errorLine = document.getElementById("error");
const back = document.getElementById("back");
const heading = document.getElementById("heading");
const meta = document.getElementById("meta");
const ownerLine = document.getElementById("owner");
const reasonLine = document.getElementById("reason");
const curatedLabel = document.getElementById("curated-label");
const curatedSelect = document.getElementById("curated");
const createForm = document.getElementById("create");
const listing = document.getElementById("listing");
const grid = document.getElementById("grid");
const spacer = document.getElementById("spacer");
const unmatched = document.getElementById("unmatched");
const panel = document.getElementById("panel");
const panelNote = document.getElementById("panel-note");
const full = document.getElementById("full");

const state = {
  view: { kind: "tree", at: "" },
  album: null,
  items: [],
  cursor: 0,
  columns: 1,
  target: "",
  pending: false,
  pendingIndex: -1
};

const pool = [];
let loadToken = 0;
let toggleGeneration = 0;
let painting = false;
let paintAgain = false;
let clickToggle = 0;

// Same rule as Web.disagree: unknown never disagrees.
function disagree(desired, observed) {
  return (observed === "in" || observed === "out") && desired !== (observed === "in");
}

function showError(message) {
  errorLine.textContent = message || "failed";
  errorLine.hidden = false;
}

function clearError() {
  errorLine.textContent = "";
  errorLine.hidden = true;
}

function showCatalog(value) {
  if (!value || !value.present) catalog.textContent = "catalog missing";
  else if (!value.readable) catalog.textContent = "catalog unreadable";
  else if (!value.mtime) catalog.textContent = value.zone || "";
  else {
    try {
      catalog.textContent = new Intl.DateTimeFormat(undefined, {
        timeZone: value.zone,
        dateStyle: "medium",
        timeStyle: "medium"
      }).format(new Date(value.mtime));
    } catch {
      catalog.textContent = value.mtime;
    }
  }
}

async function readBody(response) {
  try {
    return await response.json();
  } catch {
    return null;
  }
}

async function getJson(url) {
  const response = await fetch(url);
  const body = await readBody(response);
  if (!response.ok) throw new Error(body && body.error ? body.error : "failed");
  return body;
}

function viewFromLocation() {
  const params = new URLSearchParams(location.search);
  if (params.has("album")) return { kind: "album", album: params.get("album") };
  if (params.has("day")) return { kind: "day", day: params.get("day") };
  return { kind: "tree", at: params.get("at") || "" };
}

function href(view) {
  if (view.kind === "album") return "?album=" + encodeURIComponent(view.album);
  if (view.kind === "day") return "?day=" + encodeURIComponent(view.day);
  if (view.at) return "?at=" + encodeURIComponent(view.at);
  return location.pathname;
}

function go(view) {
  history.pushState(null, "", href(view));
  return load();
}

function parentOf(view, album) {
  if (view.kind === "tree") {
    if (!view.at) return null;
    const parts = view.at.split("/");
    parts.pop();
    return { kind: "tree", at: parts.join("/") };
  }
  if (view.kind === "day") {
    const parts = view.day.split("/");
    return { kind: "tree", at: parts.slice(0, 2).join("/") };
  }
  if (!album || !album.path) return { kind: "tree", at: "" };
  const slash = album.path.lastIndexOf("/");
  if (slash < 0) return { kind: "tree", at: "" };
  const dir = album.path.slice(0, slash);
  const parts = dir.split("/");
  if (parts.length >= 3) return { kind: "day", day: parts.slice(0, 3).join("/") };
  return { kind: "tree", at: dir };
}

function setHeading(text) {
  heading.textContent = text || "";
  heading.hidden = !text;
}

function resetChrome() {
  meta.hidden = true;
  ownerLine.hidden = true;
  reasonLine.hidden = true;
  curatedLabel.hidden = true;
  createForm.hidden = true;
  listing.replaceChildren();
  unmatched.replaceChildren();
}

function addLink(text, view) {
  const item = document.createElement("li");
  const button = document.createElement("button");
  button.type = "button";
  button.textContent = text;
  button.addEventListener("click", () => { go(view); });
  item.append(button);
  listing.append(item);
}

function showBack(view, album) {
  back.hidden = parentOf(view, album) == null;
}

function marked(item) {
  if (state.view.kind === "day") return (item.curated || []).indexOf(state.target) !== -1;
  return item.desired === true;
}

function columnCount() {
  return Math.max(1, Math.floor((grid.clientWidth + gap) / stride));
}

function paint() {
  if (grid.hidden) return;
  if (painting) {
    paintAgain = true;
    return;
  }
  painting = true;
  try {
  let columns = columnCount();
  let rows = state.items.length === 0 ? 0 : Math.ceil(state.items.length / columns);
  const cap = Math.max(stride, Math.floor(window.innerHeight * 0.7));
  grid.style.height = Math.min(cap, Math.max(rows * stride, stride)) + "px";
  const again = columnCount();
  if (again !== columns) {
    columns = again;
    rows = state.items.length === 0 ? 0 : Math.ceil(state.items.length / columns);
    grid.style.height = Math.min(cap, Math.max(rows * stride, stride)) + "px";
  }
  state.columns = columns;
  spacer.style.height = (rows * stride) + "px";
  const screen = grid.clientHeight;
  const top = grid.scrollTop;
  const from = Math.max(0, top - screen);
  const to = top + (2 * screen);
  let first = rows === 0 ? 0 : Math.floor(from / stride);
  let last = rows === 0 ? -1 : Math.floor(to / stride);
  if (last >= rows) last = rows - 1;
  const wanted = [];
  for (let row = first; row <= last; row++) {
    for (let column = 0; column < columns; column++) {
      const index = (row * columns) + column;
      if (index < state.items.length) wanted.push(index);
    }
  }
  while (pool.length < wanted.length) pool.push(makeCell());
  for (let i = 0; i < pool.length; i++) {
    const cell = pool[i];
    if (i >= wanted.length) {
      cell.cell.hidden = true;
      cell.img.removeAttribute("src");
      delete cell.img.dataset.path;
      continue;
    }
    bindCell(cell, wanted[i]);
  }
  curatedSelect.disabled = state.pending;
  } finally {
    painting = false;
    if (paintAgain) {
      paintAgain = false;
      paint();
    }
  }
}

function makeCell() {
  const cell = document.createElement("div");
  cell.className = "cell";
  const img = document.createElement("img");
  img.alt = "";
  img.draggable = false;
  const desired = document.createElement("span");
  desired.className = "mark desired";
  desired.setAttribute("aria-label", "desired");
  const observed = document.createElement("span");
  observed.className = "mark observed";
  observed.setAttribute("aria-label", "observed");
  const marks = document.createElement("div");
  marks.className = "marks";
  marks.append(desired, observed);
  const corner = document.createElement("span");
  corner.className = "corner";
  corner.hidden = true;
  cell.append(img, marks, corner);
  const entry = { cell, img, desired, observed, corner };
  // A double-click opens the full image, so the toggle waits and does not write.
  cell.addEventListener("click", () => {
    const index = Number(cell.dataset.index);
    state.cursor = index;
    paint();
    scheduleToggle(index);
  });
  cell.addEventListener("dblclick", (event) => {
    event.preventDefault();
    cancelScheduledToggle();
    const index = Number(cell.dataset.index);
    state.cursor = index;
    paint();
    openPanel(state.items[index]);
  });
  grid.append(cell);
  return entry;
}

function bindCell(entry, index) {
  const item = state.items[index];
  const row = Math.floor(index / state.columns);
  const column = index % state.columns;
  entry.cell.hidden = false;
  entry.cell.dataset.index = String(index);
  entry.cell.style.transform = "translate(" + (column * stride) + "px, " + (row * stride) + "px)";
  entry.cell.classList.toggle("focus", index === state.cursor);
  entry.cell.classList.toggle("busy", index === state.pendingIndex);
  const src = thumbRoute + "?path=" + encodeURIComponent(item.path);
  if (entry.img.dataset.path !== item.path) {
    entry.img.dataset.path = item.path;
    entry.img.src = src;
  }
  entry.img.alt = item.path;
  const on = marked(item);
  entry.desired.classList.toggle("on", on);
  entry.desired.classList.toggle("dim", item.uploadable === false);
  entry.desired.setAttribute("aria-pressed", on ? "true" : "false");
  const album = state.view.kind === "album";
  entry.observed.hidden = !album;
  entry.observed.classList.remove("on", "unknown");
  entry.observed.textContent = "";
  if (album && item.observed === "in") entry.observed.classList.add("on");
  if (album && item.observed === "unknown") {
    entry.observed.classList.add("unknown");
    entry.observed.textContent = "?";
  }
  entry.corner.hidden = !album || !disagree(on, item.observed);
}

function revealCursor() {
  if (state.items.length === 0) return;
  const row = Math.floor(state.cursor / state.columns);
  const top = row * stride;
  const bottom = top + stride;
  const viewTop = grid.scrollTop;
  const viewBottom = viewTop + grid.clientHeight;
  if (top < viewTop) grid.scrollTop = top;
  else if (bottom > viewBottom) grid.scrollTop = bottom - grid.clientHeight;
}

function cancelScheduledToggle() {
  clearTimeout(clickToggle);
  clickToggle = 0;
}

function scheduleToggle(index) {
  cancelScheduledToggle();
  if (state.pending) return;
  const token = loadToken;
  clickToggle = setTimeout(() => {
    clickToggle = 0;
    if (token !== loadToken) return;
    toggleIndex(index);
  }, 300);
}

function moveCursor(key) {
  cancelScheduledToggle();
  const last = state.items.length - 1;
  if (last < 0) return;
  const columns = state.columns;
  let next = state.cursor;
  if (key === "ArrowLeft") next = Math.max(0, state.cursor - 1);
  else if (key === "ArrowRight") next = Math.min(last, state.cursor + 1);
  else if (key === "ArrowUp") next = state.cursor - columns < 0 ? state.cursor : state.cursor - columns;
  else next = state.cursor + columns > last ? state.cursor : state.cursor + columns;
  state.cursor = next;
  revealCursor();
  paint();
}

function applyDesired(item, kind, title, value) {
  if (kind === "day") {
    const names = item.curated || [];
    item.curated = names;
    const has = names.indexOf(title) !== -1;
    if (value && !has) names.push(title);
    if (!value && has) item.curated = names.filter((name) => name !== title);
  } else item.desired = value;
}

async function toggleIndex(index) {
  cancelScheduledToggle();
  if (state.pending) return;
  const view = state.view;
  if (!view || (view.kind !== "album" && view.kind !== "day")) return;
  const item = state.items[index];
  if (!item) return;
  const title = view.kind === "album" ? (state.album && state.album.title) : state.target;
  if (!title) return;
  const generation = ++toggleGeneration;
  const before = view.kind === "album" ? item.desired === true : (item.curated || []).indexOf(title) !== -1;
  state.pending = true;
  state.pendingIndex = index;
  applyDesired(item, view.kind, title, !before);
  paint();
  try {
    const response = await fetch(toggleRoute, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ title: title, path: item.path })
    });
    const body = await readBody(response);
    if (generation !== toggleGeneration) return;
    if (!response.ok || !body || typeof body.desired !== "boolean") {
      applyDesired(item, view.kind, title, before);
      showError(body && body.error ? body.error : "failed");
    } else {
      applyDesired(item, view.kind, title, body.desired);
      clearError();
    }
  } catch (failure) {
    if (generation !== toggleGeneration) return;
    applyDesired(item, view.kind, title, before);
    showError(failure.message || "failed");
  } finally {
    if (generation !== toggleGeneration) return;
    state.pending = false;
    state.pendingIndex = -1;
    paint();
  }
}

function openPanel(item) {
  if (!item) return;
  panel.hidden = false;
  panelNote.hidden = true;
  full.hidden = true;
  full.alt = item.path;
  full.src = fileRoute + "?path=" + encodeURIComponent(item.path);
}

function closePanel() {
  panel.hidden = true;
  panelNote.hidden = true;
  full.removeAttribute("src");
}

function fillSelect(titles, selected) {
  curatedSelect.replaceChildren();
  titles.forEach((title) => {
    const option = document.createElement("option");
    option.value = title;
    option.textContent = title;
    curatedSelect.append(option);
  });
  curatedSelect.value = selected;
}

function showGrid(items) {
  state.items = items;
  state.cursor = 0;
  grid.hidden = false;
  grid.scrollTop = 0;
  paint();
}

function hideGrid() {
  grid.hidden = true;
  state.items = [];
  pool.forEach((entry) => {
    entry.cell.hidden = true;
    entry.img.removeAttribute("src");
    delete entry.img.dataset.path;
  });
}

async function showTree(next, token) {
  const at = next.at || "";
  const url = at ? treeRoute + "?at=" + encodeURIComponent(at) : treeRoute;
  const body = await getJson(url);
  if (token !== loadToken) return;
  // A newer screen is mounted only after this body arrives. x still means the previous view until then.
  toggleGeneration++;
  state.pending = false;
  state.pendingIndex = -1;
  cancelScheduledToggle();
  closePanel();
  state.view = next;
  state.album = null;
  resetChrome();
  hideGrid();
  showCatalog(body.catalog);
  setHeading(at);
  const parts = at ? at.split("/") : [];
  createForm.hidden = !(parts.length === 0 || parts.length === 2);
  (body.entries || []).forEach((entry) => {
    if (body.kind === "days") {
      const label = entry.count == null ? entry.name : entry.name + " (" + entry.count + ")";
      addLink(label, { kind: "day", day: entry.path });
    } else addLink(entry.name, { kind: "tree", at: entry.path });
  });
  (body.curated || []).forEach((album) => addLink(album.title, { kind: "album", album: album.title }));
  (body.albums || []).forEach((album) => addLink(album.title, { kind: "album", album: album.title }));
  showBack(next, null);
}

async function showDay(next, token) {
  const body = await getJson(dayRoute + "?day=" + encodeURIComponent(next.day));
  if (token !== loadToken) return;
  toggleGeneration++;
  state.pending = false;
  state.pendingIndex = -1;
  cancelScheduledToggle();
  closePanel();
  state.view = next;
  state.album = null;
  resetChrome();
  showCatalog(body.catalog);
  setHeading(body.day);
  const titles = body.curated || [];
  state.target = titles.indexOf(state.target) === -1 ? (titles[0] || "") : state.target;
  fillSelect(titles, state.target);
  curatedLabel.hidden = false;
  (body.albums || []).forEach((album) => addLink(album.title, { kind: "album", album: album.title }));
  showBack(next, null);
  showGrid(body.stems || []);
}

async function showAlbum(next, token) {
  const body = await getJson(albumRoute + "?title=" + encodeURIComponent(next.album));
  if (token !== loadToken) return;
  toggleGeneration++;
  state.pending = false;
  state.pendingIndex = -1;
  cancelScheduledToggle();
  closePanel();
  state.view = next;
  state.album = body;
  resetChrome();
  showCatalog(body.catalog);
  setHeading(body.title);
  meta.hidden = false;
  meta.textContent = body.span == null ? "curated" : body.span;
  if (body.owner != null) {
    const name = body.name == null ? body.title : body.name;
    ownerLine.hidden = false;
    ownerLine.textContent = name + "; not pushed";
  }
  const phrase = body.membership && reasonPhrase[body.membership.reason];
  if (phrase) {
    reasonLine.hidden = false;
    reasonLine.textContent = phrase;
  }
  (body.unmatched || []).forEach((row) => {
    const line = document.createElement("p");
    line.textContent = row.day ? row.filename + " " + row.day : row.filename;
    unmatched.append(line);
  });
  showBack(next, body);
  showGrid(body.tiles || []);
}

async function load() {
  const token = ++loadToken;
  const next = viewFromLocation();
  clearError();
  try {
    if (next.kind === "album") await showAlbum(next, token);
    else if (next.kind === "day") await showDay(next, token);
    else await showTree(next, token);
  } catch (failure) {
    if (token !== loadToken) return;
    showError(failure.message || "failed");
  }
}

function isField(target) {
  if (!target || !target.tagName) return false;
  const tag = target.tagName;
  return tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT" || target.isContentEditable;
}

back.addEventListener("click", () => {
  const parent = parentOf(state.view, state.album);
  if (parent) go(parent);
});

curatedSelect.addEventListener("change", () => {
  state.target = curatedSelect.value;
  paint();
});

createForm.addEventListener("submit", async (event) => {
  event.preventDefault();
  const start = document.getElementById("start").value.trim();
  const end = document.getElementById("end").value.trim();
  const name = document.getElementById("name").value.trim();
  const payload = { start: start, end: end };
  if (name) payload.name = name;
  const submit = createForm.querySelector("button");
  submit.disabled = true;
  try {
    const response = await fetch(albumsRoute, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });
    const body = await readBody(response);
    if (!response.ok || !body || !body.title) {
      showError(body && body.error ? body.error : "failed");
      return;
    }
    clearError();
    createForm.reset();
    await go({ kind: "album", album: body.title });
  } catch (failure) {
    showError(failure.message || "failed");
  } finally {
    submit.disabled = false;
  }
});

full.addEventListener("load", () => {
  if (panel.hidden) return;
  panelNote.hidden = true;
  full.hidden = false;
});

full.addEventListener("error", () => {
  if (panel.hidden || !full.getAttribute("src")) return;
  full.hidden = true;
  panelNote.hidden = false;
});

grid.addEventListener("scroll", () => { paint(); });
window.addEventListener("resize", () => { if (!grid.hidden) paint(); });

document.addEventListener("keydown", (event) => {
  if (isField(event.target)) return;
  if (event.key === "Escape") {
    if (!panel.hidden) {
      event.preventDefault();
      closePanel();
    }
    return;
  }
  if (grid.hidden) return;
  if (event.key === "ArrowLeft" || event.key === "ArrowRight" || event.key === "ArrowUp" || event.key === "ArrowDown") {
    event.preventDefault();
    if (panel.hidden) moveCursor(event.key);
    else cancelScheduledToggle();
    return;
  }
  if (!panel.hidden) return;
  if (event.key === "Enter" && event.target && event.target.tagName === "BUTTON") return;
  if (event.key === "x" || event.key === "Enter") {
    if (event.repeat) return;
    event.preventDefault();
    toggleIndex(state.cursor);
  }
});

window.addEventListener("popstate", () => { load(); });
load();
