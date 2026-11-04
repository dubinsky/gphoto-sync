package org.podval.tools.photo

private[photo] object PhotosScripts:
  private[photo] val focusTitleScript: String =
    """(current) => {
      const fields = Array.from(document.querySelectorAll('input, textarea, [contenteditable="true"]'));
      const valueOf = (node) => typeof node.value === 'string' ? node.value : (node.innerText || '');
      const match = fields.find((node) => valueOf(node) === current);
      if (!match) return false;
      match.focus();
      return document.activeElement === match;
    }"""

  // Only Add a description. Another field is not this edit, so a miss types nothing.
  private[photo] val focusDescriptionScript: String =
    """() => {
      const fields = Array.from(document.querySelectorAll('input, textarea, [contenteditable="true"]'));
      const valueOf = (node) => typeof node.value === 'string' ? node.value : (node.innerText || '');
      const match = fields.find((node) => {
        const label = node.getAttribute('aria-label') || '';
        const placeholder = node.getAttribute('placeholder') || '';
        return label === 'Add a description' || placeholder === 'Add a description' || valueOf(node).trim() === 'Add a description';
      });
      if (!match) return false;
      match.focus();
      if (document.activeElement !== match) return false;
      match.__gphotoDescription = true;
      return true;
    }"""

  // Panel text outside the description control. That control's value is not this check.
  private[photo] val descriptionShownScript: String =
    """(text) => {
      const wanted = text == null ? '' : String(text);
      const valueOf = (node) => typeof node.value === 'string' ? node.value : (node.innerText || '');
      const controls = Array.from(document.querySelectorAll('input, textarea, [contenteditable="true"]')).filter((node) => node.__gphotoDescription);
      const panel = (document.body && document.body.innerText) || '';
      if (wanted === '') {
        const cleared = controls.some((node) => {
          const value = valueOf(node).trim();
          return value === '' || value === 'Add a description';
        });
        return cleared && panel.includes('Add a description');
      }
      if (!document.body) return false;
      const lines = [];
      const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT, {
        acceptNode(node) {
          if (controls.some((control) => control.contains(node))) return NodeFilter.FILTER_REJECT;
          return NodeFilter.FILTER_ACCEPT;
        }
      });
      let current;
      while ((current = walker.nextNode())) {
        String(current.textContent || '').split('\n').forEach((part) => {
          const trimmed = part.trim();
          if (trimmed) lines.push(trimmed);
        });
      }
      return lines.some((line) => line === wanted);
    }"""

  private[photo] val wizardScript: String =
    """() => { const data = window.WIZ_global_data; return !!(data && data.cfb2h && data.SNlM0e && /^\d+$/.test(String(data.S06Grb || ''))); }"""

  private[photo] val installScript: String =
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

  private[photo] val ownerScript: String =
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

  private[photo] val readScript: String =
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

  // Same fDcn4b item read as readScript. The date fields are not this check.
  // A timestamp on the first response stays. The second form needs the share auth key.
  private[photo] val itemDateScript: String =
    """async (key) => {
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
      function shareAuth() {
        const here = new URL(location.href);
        const own = here.searchParams.get('key');
        if (own) return own;
        for (const a of document.querySelectorAll('a[href*="/share/"]')) {
          const u = new URL(a.href, location.href);
          if (!/\/share\/(AF1Qip[^/?#]+)/.test(u.pathname)) continue;
          const found = u.searchParams.get('key');
          if (found) return found;
        }
        return null;
      }
      let r0 = null;
      try {
        const raw = await retry(() => rpc('fDcn4b', [key, 1, null, null, 1]));
        const v = raw && JSON.parse(raw);
        r0 = v && v[0];
      } catch (e) {}
      let ts = r0 && r0[3] != null ? r0[3] : null;
      let tz = r0 && r0[4] != null ? r0[4] : 0;
      if (ts == null) {
        const auth = shareAuth();
        if (!auth) throw new Error('read failed');
        const raw = await retry(() => rpc('fDcn4b', [key, null, auth]));
        const v = raw && JSON.parse(raw);
        r0 = v && v[0];
        ts = r0 && r0[3] != null ? r0[3] : null;
        tz = r0 && r0[4] != null ? r0[4] : 0;
      }
      if (ts == null) throw new Error('read failed');
      return new Date(Number(ts) + Number(tz || 0)).toISOString().slice(0, 10);
    }"""

  private[photo] val dialogButtonsScript: String =
    """(root) => {
      const names = [];
      for (const node of root.querySelectorAll('button, [role="button"]')) {
        const raw = (node.innerText || node.getAttribute('aria-label') || '').replace(/\s+/g, ' ').trim();
        if (raw) names.push(raw);
      }
      return names.join('\n');
    }"""

  // A key past the catalog count is still a new key.
  private[photo] val albumKeysScript: String =
    """async (albumId) => {
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
      function albumAuth(id) {
        const here = new URL(location.href);
        const own = here.pathname.match(/\/share\/(AF1Qip[^/?#]+)/);
        if (own && own[1] === id) {
          const key = here.searchParams.get('key');
          if (key) return key;
        }
        for (const a of document.querySelectorAll('a[href*="/share/"]')) {
          const u = new URL(a.href, location.href);
          const m = u.pathname.match(/\/share\/(AF1Qip[^/?#]+)/);
          if (!m || m[1] !== id) continue;
          const found = u.searchParams.get('key');
          if (found) return found;
        }
        return null;
      }
      const okThumb = u => typeof u === 'string' && (u.startsWith('https://photos.fife.usercontent.google.com/') || u.startsWith('https://lh3.googleusercontent.com/pw/'));
      const auth = albumAuth(albumId);
      const items = [], seen = new Set();
      let d = JSON.parse(await retry(() => rpc('snAcKc', [albumId, null, null, auth])));
      let tok = null, empty = 0, pages = 0;
      while (true) {
        const before = items.length;
        for (const it of (d && d[1]) || []) {
          if (Array.isArray(it) && typeof it[0] === 'string' && it[0].startsWith('AF1Qip') && Array.isArray(it[1]) && okThumb(it[1][0]) && !seen.has(it[0])) {
            seen.add(it[0]);
            items.push(it[0]);
          }
        }
        empty = items.length === before ? empty + 1 : 0;
        pages++;
        const nt = d && d[2];
        if (!nt || nt === tok || empty >= 3 || pages > 500) break;
        tok = nt;
        d = JSON.parse(await retry(() => rpc('snAcKc', [albumId, tok, null, auth])));
      }
      return items.join('\n');
    }"""

  // Same fDcn4b item read as readScript.
  // A timestamp on the first response stays. The second form needs the album auth key.
  private[photo] val itemFactScript: String =
    """async (spec) => {
      const parts = String(spec).split('\t');
      const key = parts[0];
      const albumId = parts.length > 1 ? parts[1] : '';
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
      function albumAuth(id) {
        const here = new URL(location.href);
        const own = here.pathname.match(/\/share\/(AF1Qip[^/?#]+)/);
        if (own && own[1] === id) {
          const key = here.searchParams.get('key');
          if (key) return key;
        }
        for (const a of document.querySelectorAll('a[href*="/share/"]')) {
          const u = new URL(a.href, location.href);
          const m = u.pathname.match(/\/share\/(AF1Qip[^/?#]+)/);
          if (!m || m[1] !== id) continue;
          const found = u.searchParams.get('key');
          if (found) return found;
        }
        return null;
      }
      const sp = s => String(s).replace(/[\t\n\r]/g, ' ');
      let r0 = null, firstTs = null, firstTz = null;
      try {
        const raw = await retry(() => rpc('fDcn4b', [key, 1, null, null, 1]));
        const v = raw && JSON.parse(raw);
        r0 = v && v[0];
        if (r0 && r0[3] != null) firstTs = r0[3];
        if (r0 && r0[4] != null) firstTz = r0[4];
      } catch (e) {}
      let fn = r0 && typeof r0[2] === 'string' && r0[2] !== '' ? r0[2] : null;
      if (fn === null) {
        const auth = albumAuth(albumId);
        if (auth) {
          try {
            const raw = await retry(() => rpc('fDcn4b', [key, null, auth]));
            const v = raw && JSON.parse(raw);
            const next = v && v[0];
            if (next) {
              if (next[3] == null && firstTs != null) next[3] = firstTs;
              if (next[4] == null && firstTz != null) next[4] = firstTz;
              r0 = next;
              fn = typeof next[2] === 'string' && next[2] !== '' ? next[2] : null;
            }
          } catch (e) {}
        }
      }
      if (fn === null) return '';
      const ts = r0 && r0[3] != null ? r0[3] : null;
      const tz = r0 && r0[4] != null ? r0[4] : 0;
      const date = ts != null ? new Date(Number(ts) + Number(tz || 0)).toISOString().slice(0, 10) : '-';
      let up = '-';
      try {
        const o = r0 && r0[28];
        if (o && o[1] != null && ME && String(o[1]) !== ME) up = sp(o[11] && o[11][0] ? o[11][0] : o[1]);
      } catch (e) {}
      return sp(fn) + '\t' + date + '\t' + up;
    }"""

  private[photo] def placeLua(name: String): String =
    val headless: String =
      "hl.monitor({ output = " + quoted(name) + ", mode = \"1280x800@60\", position = \"20000x20000\", scale = 1 })"
    PhotosPark.focused match
      case None => headless
      case Some(monitor) =>
        headless + "\n" + restoreLua(monitor)

  private[photo] def restoreLua(monitor: PhotosPark.Monitor): String =
    "hl.monitor({ output = " + quoted(monitor.name) + ", mode = \"preferred\", position = " +
      quoted(s"${monitor.x}x${monitor.y}") + ", scale = \"auto\" })"

  private[photo] def moveLua(name: String, address: String): String =
    "hl.dsp.window.move({ monitor = " + quoted(name) + ", follow = false, window = \"address:" + address + "\" })"

  private[photo] def quoted(text: String): String =
    val body: String = text.flatMap:
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case ch => String.valueOf(ch)
    "\"" + body + "\""
