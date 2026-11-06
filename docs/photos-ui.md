# Photos UI

## Refresh

`photo refresh` only reads Google Photos.

`photo refresh` builds the catalog: every album with its item count and id, then each item's filename and date, album by
album.
It does not shell out.
It rewrites `catalog.txt` for every album, or for the titles the operator names.
A change the operator makes in Google Photos by hand is not written into `catalog.txt` until a refresh reads that album.
`photo publish` and `photo sync` are separate commands.

### Route

`photo refresh` calls photos.google.com's own RPCs from the signed-in Chrome profile.

- Not the Photos Library API (since 2025 it only sees media the calling app created).
  No connector exists.
- Not DOM scraping (lazy grid, no filenames in DOM).
  Call the page's `batchexecute` endpoint from the page's JS context, so cookies and XSRF apply.
- The browser is the signed-in profile `~/.config/gphoto-sync-chrome`.
  It is not the normal Chrome profile and does not appear in that profile's avatar menu.
- The window stays mapped while the album list scrolls.
  When detection succeeds, the window is parked as Browser says.
  On any other desktop the window stays visible.
- The work directory is `${TMPDIR:-/tmp}/gphoto-sync-refresh`, mode `0700`.
  Part files, `albums.tsv`, and `status.txt` are written there.
  A successful publish removes that directory.
  The lock file is the sibling `${TMPDIR:-/tmp}/gphoto-sync-refresh.lock`, mode `0600`.
  It is not inside the work directory, and the publish does not delete it.
  A second refresh prints `already running` as its first line, then `status.txt`, and exits 2.
- One album keeps one winner part.
  A thrown read is its own `# FAILED` buffer, not a line appended to a winner.

### Steps

1. **Sign in.** Open https://photos.google.com/albums in the profile above.
   Leave the window up for up to one hour.
   Do not publish until the Gaia id `S06Grb` is all digits.
   If that hour ends first, the command fails with `sign-in timed out`.
2. **Album list.** Scroll /albums to the bottom.
   The first sweep grabs the cards, scrolls every overflow container by `max(300, 0.7 * clientHeight)`, scrolls the
   window by 600, and sleeps 2500 ms. `stable` counts steps whose seen-card count did not change.
   `atBottomRounds` increments when a step does not move a scroller.
   The first sweep stops when `atBottomRounds >= 8` and `stable >= 15`, or when `step > 2000`.
   The second sweep scrolls back to the top, sleeps 3000 ms, then takes up to 4000 steps of
   `max(300, 0.5 * clientHeight)` and 1200 ms, and stops when a step does not move.
   Per album record the title, the count, the media key (`AF1Qip…` from `/album/<key>` or `/share/<key>`), and for a
   shared album the auth key (`/share/<key>?key=<authKey>`).
   The command does not write the page script into the work directory.
   Each row is the title, the count, the album key, the auth key or an empty field, and one JSON field of that card's
   text leaves.
   The count is the first text node of the card that matches `/^\s*([\d,]+)\s+items?\b/`, with the commas removed.
   It is not the share page, and it is not an integer inside `snAcKc`.
   The title is the first other text node that is not `More options`.
   A publishing refresh writes `albums.tsv` as one whole replacement after the short-scroll checks have passed.
   `--list` does not write that file.
   A row there keeps those five fields.
   Auto-add is not a column.
   - Keep titles EXACT.
     Real titles can have leading/trailing spaces; never trim.
     Titles are not unique; the album key is the identity.
3. **Skip stored auto-add albums.** An auto-add id is one that already has an `auto-add` line.
   A new id is not guessed from the album page, and its items are read.
   A stored auto-add id is not in the item-read set, including on `--full` and `--album`.
   A publishing refresh reads one owner-only `snAcKc` page for that id.
   `--list` does not call `snAcKc`.
   The carried album keeps the `auto-add` line and has no `item`, `unset`, or `error` line.
   An `owner` line may still be present.
4. **Read items.** From the page's JS context, read each album in the album-list TSV.
   The read:
   - Reads `WIZ_global_data.cfb2h` (build label `bl`, changes every few days, read live), `WIZ_global_data.SNlM0e` (XSRF
     `at`), and `WIZ_global_data.S06Grb` (the signed-in user's Gaia id, a string of digits).
   - POSTs `/_/PhotosUi/data/batchexecute?rpcids=<id>&bl=<bl>&hl=en&rt=c` with
     `f.req=[[[rpc, JSON.stringify(args), null, "generic"]]]&at=<at>`; parse the `wrb.fr` row.
   - Album pages: RPC `snAcKc`, args `[albumKey, pageToken, null, authKey]`.
     Response `[null, [items...], pageToken, ...]`; each item is
     `["AF1Qip…", [thumbUrl, w, h, ...], timestampMs, dedupKey, tzOffsetMs, ...]`.
     **The thumb URL is either `https://photos.fife.usercontent.google.com/...` or
     `https://lh3.googleusercontent.com/pw/...`.
     Accept both.** Filtering on photos.fife only silently drops items and makes albums read as empty or short (the main
     cause of `got 0 of N`).
     Follow the page token; stop at the count, when the token is missing/unchanged, or after ~3 pages with no new keys.
   - **Album owner:** in the first `snAcKc` page, `d[3][5]` is the album owner: `[1]` = Gaia id, `[11][0]` = display
     name.
     One call per album is enough for an owner-only pass (260 albums in under a minute).
     Write an `owner` line for that album id only when the owner isn't the signed-in account.
     The label is written as Files describes.
     The album owner is not the uploader of an item.
   - Item details: RPC `fDcn4b`, args `[itemKey, 1, null, null, 1]`.
     In `result[0]`: `[2]` = filename, `[3]` = timestamp ms (UTC), `[4]` = timezone offset ms. Item date =
     `new Date(ts + tz).toISOString().slice(0,10)`; no timestamp means `-`.
     Verified: dates match the album subjects.
   - **Uploader:** `result[0][28]` is the item's owner: `[1]` = Gaia account id, `[11][0]` = display name.
     Compare `[1]` with `WIZ_global_data.S06Grb`.
     When they differ, the uploader is the display name, or the account id when no name is shown.
     When they match, the uploader is `-`.
     Verified on shared albums, and it is present in both `fDcn4b` forms.
     This is the item's uploader, not the album owner.
   - **Shared albums:** `fDcn4b` with `[itemKey, 1, null, null, 1]` returns null for items in shared albums (every name
     fails).
     Use `[itemKey, null, authKey]` instead (authKey may be null for owned albums); result `[0]` has the same layout:
     `[2]` filename, `[3]` timestamp, `[4]` tz offset.
     Try the first form, fall back to the second.
   - 6 parallel workers, 4 retries with backoff, 30 s timeout per request, 500 ms between albums, smallest albums first.
   - Writes one current part file per album at `parts/<album key>` in the refresh directory.
     The album key is a URL path segment and contains no slash, so it is the filename.
     The download name from the page is not the stored name.
     The first line is exactly `# part`, a tab, and that album key.
     The rest is the Catalog records for that album only.
     A part has no `# gphoto-catalog 2` line.
     A thrown read is stored only when that album has no winner.
     The buffer is `# part`, a tab, and the album key, then `# FAILED`, a tab, and the message.
     Each tab or newline in the message is a space.
     An ok read is not read again.
     A clean shortfall (`name-failures` 0 and `got` below `of`) is three in-memory reads.
     One winner file is written after they settle.
     Any other non-failed read, including a name-failure, is repeated while that read count is under 2.
     Passes run while `pass <= 4`.
     A pass that writes no part file stops.
     A failed write counts as a write.
     The attempt count does not survive the process.
     A filename that contains a tab or a newline fails the check.
     The command does not substitute a space in a filename.
     Progress is `status.txt` in that same directory.
     Each update replaces the whole file.
     The lines are `key=value` and the file ends with LF.
     `heartbeat` is epoch milliseconds.
     The other keys are `phase` and the counts that phase has: `seen`, `pass`, `need`, `done`, `total`, and `error`.
     The file contains no album id.
   - Never invent a filename for a name that was not read.
     Write an `unset` line for that returned item, as Files describes.
     Write an `error` line when the number of returned item keys is less than the album-list count, or when any name was
     not read.
     Fill `of`, `got`, and `name-failures` as Files describes.
5. **Resume.** Reuse `albums.tsv` only when `phase` is `items` and that file was written as one whole replacement after
   the short-scroll checks passed.
   Check 4 compares that saved card list, not a TSV of only the missing albums.
   A crash during `scroll` is wiped, and the next run scrolls.
   A failed check sets `phase` to `error` and does not reuse parts.
   The next run scrolls.
   A winner part is not read again.
   A `# FAILED` part is retried.
   A shortfall is the album-list count above the number of item keys returned, with zero name failures.
   Read that album three times in this pass.
   When all three reads return the same item keys and the same album-list count, the shortfall is stable.
   Keep the items from that read.
   When the three reads do not agree, keep the read with the most item keys.
   Either way, stop, leave the album-list count on the `album` line, and write an `error` line with `got` equal to the
   number of item keys returned, `of` equal to that count, and `name-failures` 0.
   Do not read that album again in this pass.
   The missing items are ones Google Photos no longer serves.
   They are usually contributions whose owners later stopped sharing them.
   Do not invent filenames for them.
   Name failures are not this shortfall.
   The per-request retries above still apply to those.
   No model chooses the next batch.
   A resume pass that writes no new part stops.
   Count one read per attempt.
   The validation reader counted a duplicate part file as another attempt.
   The command does not.
6. **Merge and verify.** Merge parts, keeping per album key the best read (a read with no `error` line beats a read with
   one, then more `item` lines).
   Never let a worse new read replace a better earlier one in the published catalog.
   The merged lines are Catalog records.
   `count` on each `album` line is the card count from step 2.
   The completeness checks are in Publish.

### Read one album

A refresh the operator limits to named album ids reads those albums and leaves every other album's lines as they are.
The published file still contains every album.
The new records replace only the named ids.
Checks 1, 2, 3, and 6 run on that spliced file.
Checks 4 and 5 apply to the named ids against this pass, not to the albums copied through.
Then replace `catalog.txt` with the same rename as a full pass.

The count on the album line is still the album-list count.
Take it from that album's card on `https://photos.google.com/albums`, with the same text-node match as step 2.
Opening `/album/<id>` redirects to `/share/<id>`.
That page has no "N items" text node.
`snAcKc` returns the item keys.
An integer in that payload once matched the album-list count, and the path of that integer was not recorded.
Do not use an unidentified payload integer as `count`.
Do not take `count` from the share page.

Item keys, filenames, and dates are the step 4 reads.
One run returns the album-list count, the item keys, and each item's filename and date.
A second run, with no edit between them, returns the same count and the same keys.
Gradle tests do not call Google Photos.

Checking that one item is no longer in an album is a key comparison.
Read that album's `snAcKc` keys and see that the item key is absent.
The comparison does not use the share page's item count.

### Browser

The catalog read launches Google Chrome stable with user-data dir `~/.config/gphoto-sync-chrome`.
That directory is not `~/.config/google-chrome`, and it does not appear in normal Chrome's profile menu.
The window is headed.
The catalog read uses a 1400 by 900 viewport and the same window size, at position 40,40.
It drops the default `--enable-automation` argument.
It passes `--disable-blink-features=AutomationControlled`, `--no-first-run`, `--no-default-browser-check`,
`--window-position=40,40`, `--window-size=1400,900`, and `--class=gphoto-sync`.
Downloads are accepted.
The page timeout is unlimited.
Navigation waits at most 120 seconds.

Park only when detection succeeds, and do it before Chrome is launched.
Detection is a non-blank `HYPRLAND_INSTANCE_SIGNATURE`, the directory
`$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE`, and `hyprctl monitors -j` exiting 0.
The cursor and the focused monitor are recorded before `hyprctl output create headless`.
The new name is the one absent from the set taken before the create.
Do not assume `HEADLESS-1`.
Place and the focused-monitor restore stay one eval: headless mode `1280x800@60`, position `20000x20000`, scale `1`; the
focused monitor mode `preferred`, saved `XxY`, scale `auto`.
Restore the cursor before the rule, and again after a fallback `moveLua`.
On exit, restore the cursor only when prepare recorded one.
The `("0", "0")` fold inside `cursorPos` is not a recorded cursor unless prepare stored that result.
The rule is `hyprctl eval` of `hl.window_rule`, name `gphoto-sync-park`, `enabled` true, `match.class` `^gphoto-sync$`,
`monitor` the created name plus ` silent`, `no_initial_focus` true, and `no_anim` true.
Do not use `hyprctl keyword`.
Do not write the rule or the output into the Hyprland config.
The same name updates that rule.
The class match is replaced.
Effects append.
The last `monitor` value is the one applied.
Disabling passes `enabled` false and does not omit it.
After launch, poll every 300 ms for up to 20 seconds only when this process created an output.
A failed detection does not poll and does not call `hyprctl` again.
The command line check is unchanged.
The window's command line contains `gphoto-sync-chrome` and `remote-debugging-pipe`, and it does not contain `--type=`.
Resolve the client `monitor` id through `hyprctl monitors -j`.
If that monitor is the created output, do not move the window.
Otherwise log `refresh park: window not on` and the output name, then dispatch `moveLua` only.
`moveLua` is `hl.dsp.window.move`, `follow` false, and `window` `address:` plus the address.
Do not call `output create`, `hl.monitor`, or `hl.window_rule` from that fallback.
A missing window logs `refresh park: window missing` and leaves the window where it is.
A missing output is logged in prepare: `refresh park: headless missing`.
Do not log that line for a failed detection, and do not log it again after launch.
Leave the window where it is.
Do not pin the window.
On exit, disable the rule if it was enabled, restore the physical monitor when one was recorded, restore the cursor when
prepare recorded one, and remove only the output this process created.
A failed detection records none of these, so exit calls no `hyprctl`.
When detection fails, leave the window visible.
Log `refresh park: hyprland not running` once.
Do not call `confirmPark`.
Do not log `refresh park: headless missing`.

### Parts and status

The work directory is `${TMPDIR:-/tmp}/gphoto-sync-refresh`.
The lock file is the sibling `${TMPDIR:-/tmp}/gphoto-sync-refresh.lock`.
While a refresh is running, part files, `albums.tsv`, and `status.txt` are in the work directory.
The command does not write a generated page script there.
One current part file per album is `parts/<album key>`, with the header in step 4.
`albums.tsv` there is the album list from step 2, written only after the short-scroll checks passed.
`status.txt` there is the running status, with the fields in step 4.
A second refresh prints `already running`, then that file, and exits 2.
When `catalog.txt` has been replaced, the work directory is removed.
The sibling lock file is not removed with it.
A failed check leaves the work directory and sets `phase` to `error`.
The next refresh scrolls.
It does not open the previous refresh's parts.
`.gphoto-sync/.download/` is only for media from `photo agent download-item`.
`catalog.txt.partial` and `catalog.txt.previous` stay next to `catalog.txt`.

### Privacy

Album titles, keys and filenames are personal.
Keep outputs on the machine that holds the archive.
Treat assembled scripts as private because they contain album keys.
`catalog.txt` is private catalog bytes.
`catalog.txt.partial` and `catalog.txt.previous` are too, while they exist.

### Publish

The merged catalog is `archive/.gphoto-sync/catalog.txt`.
`photo refresh` writes the new catalog to `catalog.txt.partial` in the same directory.
It replaces `catalog.txt` only when every check below holds.

1. The partial file is UTF-8, LF, no CR, no BOM, and ends with LF.
2. The first line is the Catalog version line.
3. Every later line is one Catalog record.
   No blank line.
   No `## `, `OWNER:`, `AUTO-ADD`, `ERROR partial:`, or `[UNSET]`.
4. The set of `album` ids equals the set of album ids from this pass's album list, each exactly once.
5. The pass did not die mid-album.
   Every album id from that list has an `album` line.
   Every non-auto-add album either has an `error` line or has an `item` count equal to its `count`.
   A count of 0 with no items is complete.
   An auto-add album has no `item`, `unset`, or `error` line.
6. Catalog's other integrity rules hold, including at most one `owner` and one `auto-add` per id.

If a check fails, `catalog.txt` stays, and `catalog.txt.partial` stays for inspection.
If they pass, `photo refresh` renames `catalog.txt` to `catalog.txt.previous`, replacing an older previous, and then
renames `catalog.txt.partial` to `catalog.txt`.
If the second rename fails, it renames `catalog.txt.previous` back to `catalog.txt`.
When both renames have succeeded, the publish is accepted and the reader deletes `catalog.txt.previous`.
If that delete fails, `catalog.txt` is already the new file.
Remove `catalog.txt.previous` by hand.
`catalog.txt.previous` exists only between those two renames.
It is not a second catalog to read or to patch.
`catalog.txt` is the only catalog the reader keeps between runs.
`catalog-local.txt` was the validation proof copy and has been removed.
`catalog-refresh.txt` was never a file.
rclone syncs `catalog.txt`.
It does not keep a second catalog.
The partial and the previous are not a second grammar and not a new store.

The reader relies on that rename.
It never sees a half-written `catalog.txt`.
A file that fails the version line is an error, not a cue to parse the old shape.

`photo refresh` is the command.
This section is its procedure.

## Push

It is not part of `photo publish`.
It does not create a Library API album, does not write `google-album-id`, and does not call `batchCreate`.

An app-created album stays on `photo publish` and `photo sync`.
A file with no `album-links.tsv` row is skipped.
A link whose album is absent from the catalog is skipped.
A catalog album with an `owner` line, `auto-add`, or unread names is skipped.
Album order is the archive-relative path, compared as UTF-8 bytes, then the title.

For one album, the day folders are the album span.
A curated album has no span, so the days are the days named by `include`.
The include lines expand to the desired day and stem pairs.
An excepted stem is not in that set.
Catalog items match local stems by the gather rules: an exact stem, then one leading prefix.
A desired pair that gather did not hit is a file with no catalog item.

An excepted stem that is an item in this album is `remove-from-album`.
It is not `delete-item` and it is not an upload.
A catalog item that gather matches to a desired pair, and that is not already an item in this album, is `add-to-album`
from the album that holds the key.
When more than one album holds that key, the source is the first of those albums in catalog order.
A desired pair that is already an item in this album is a no-op.
A desired pair with no catalog item, and whose sidecar has a non-empty `google-media-item-id` or `already-in-google`
true, is not an upload.
A desired pair with no catalog item and no such sidecar mark is `upload-item` of the archive file.
The file is `jpg`, `jpeg`, `mov`, `mp4`, `avi`, or `3gp`.
RAW, `heic`, `png`, and `webp` stay local and are not an upload line.

Inside one album the order is remove, then add, then upload.
The run finishes one album before the next.
`--dry-run` prints that plan and does not take the refresh lock and does not open Chrome.
A line is `remove`, `add`, or `upload`.
The fields are quoted.
The line has no catalog id and no item key.

A real run takes one refresh lock and one browser.
It calls `takeOut`, `putInAlbum`, and the upload function.
It does not shell out to `photo agent`.
Each clean success publishes before the next action.
Earlier actions in the run may already have published.
The failure says `catalog unchanged` when this run published nothing, and `catalog updated` when an earlier action
already published.
Recovery is one-click when the confirm did not run, and refresh-only when it did.
The next run plans from the new catalog.
It does not retry a refresh-only step by clicking it.
The command does not write an album toml, `status.txt`, `albums.tsv`, or `parts/`.
Do not invent or guess a `batchexecute` RPC for this.

## Requests

`photo agent rename-album` changes one album title.
The album has no `owner` line.
The control is More options, then the menuitem Edit album, Rename album, or Rename.
Replace the title field and confirm with Done, or with Enter when Done is absent.
After the live title equals `--new`, that album line's title becomes `--new`.
The album id, the count, the owner line, and the item lines stay.

`photo agent set-description` sets one item's description.
The item key is an `item` line on exactly one album.
Open info when the panel is closed.
The control is the button whose aria-label is Open info, or the i key.
The empty field's text is Add a description.
Type `--new`.
An empty `--new` clears the field.
The command succeeds when the panel shows the resulting text.
There is no Done button.
The catalog is not written.

`photo agent set-date` sets one item's date taken.
The item key is an `item` line on exactly one album.
In the info panel, open the control whose aria-label starts with `Date taken`.
Open info first when that control is absent.
The fields are Year, Month, Day, Hour, Minutes, and AM/PM.
Type each value.
Read AM/PM back after typing it.
When it is not the value in `--new`, choose that option, or type it and press Enter.
Confirm with Save, Done, or OK.
Then the same `fDcn4b` read Refresh uses.
The catalog `date` becomes the `YYYY-MM-DD` prefix of `--new` only when that read shows that calendar day.
The catalog omits the clock.
The item key, the filename, the uploader, and the album count stay.
A read that shows a different day leaves the old date.

`photo agent download-item` downloads one item into the staging directory in Download.
The item key is one `item` line on that album.
This is not the unique-across-catalog check.
A filename of `.`, `..`, or a string containing `/` is rejected.
The control is More options, then the menuitem whose accessible name starts with Download.
Shift+D is not required.
The command does not choose a quality option.
The command creates the staging day directory.
When that file is already there, it keeps the file and does not overwrite it.
The catalog is not written.
Google Photos is not changed.
The command does not replace an archive original.
`photo file` is still the filer.

`photo agent add-to-album` adds one item to an existing album.
The destination is a different album.
Before Chrome opens, the command refuses when the destination has an `owner` line, when the destination is `auto-add`,
when `name-failures` is greater than 0, or when any `unset` line is present.
It also refuses when a local day and stem are known for this filename and the destination album file excepts that stem.
That test is `positive` and not `claims` for the day and stem, the same test Sync uses.
When no local day and stem are known, that clause does not refuse.
When the destination has no album file, that clause does not refuse.
A refusal is usage.
Chrome stays closed and `catalog.txt` stays byte-identical.
The command opens the item the way `photo agent download-item` opens it.
The control is More options, then the menuitem Add to album, then the option whose accessible name equals the
destination title, exact.
It does not click New album, Add a title, or Done to create an album.
When the destination title is New album, the command fails one-click and clicks nothing.
When the exact destination option is absent, the command fails one-click and clicks nothing else.
It reads the destination keys with `snAcKc` before that option and again after it.
It publishes only when the destination key set grew by exactly one key that was absent before and that key's `fDcn4b`
filename equals `--current`.
The second `fDcn4b` form is `[itemKey, null, auth]` and it passes the album auth key.
A timestamp on the first response stays.
The date and the uploader come from that read when it has them, and otherwise from the source line.
The filename on the new `item` line is `--current`.
The destination `count` increases by one.
When an `error` line is present, `of` becomes the new count and `got` increases by one.
`name-failures` stays.
When there is no `error` line, none is written.
The source album is not edited.
No `unset` line is written.
Anything else leaves `catalog.txt` byte-identical.
A miss before Add to album or before the destination option is one-click.
After the destination option was clicked, a live read that is not the clean case is refresh-only.

`photo agent remove-from-album` removes one item from an album.
The command does not require the uploader to be `-` and does not reject an `owner` line.
It opens the item the way `photo agent download-item` opens it.
It does not use the album-grid Select photos path.
The control is More options, then the menuitem whose name matches Remove from album.
If a dialog is open, the command reads it before any confirm click.
Trash, or Delete forever, is not a remove.
The command does not click that dialog.
It does not click a button whose name starts with Move to trash, Delete forever, or Empty trash.
A remove dialog is confirmed with the button whose name matches `^Remove\b`.
The command reads the source keys after that click.
It publishes only when that source key is gone.
That `item` line is removed and `count` decreases by one.
When an `error` line is present, `of` becomes the new count and `got` decreases by one.
`name-failures` stays.
The command does not set `count` to the number of remaining item lines while an `error` line or an `unset` line remains.
Anything else leaves `catalog.txt` byte-identical.
A miss before Remove from album or before the Remove button is one-click.
After the Remove button was clicked, a source key that is still present is refresh-only.

`photo agent move-item` moves one item from the source album to the destination album in one session.
The command does not require the uploader to be `-` and does not reject an `owner` line on the source.
The destination is a different album.
`acceptPut` runs on the destination before Chrome opens, and again on the reread under the lock.
An `owner` line, `auto-add`, unread names, or an excepted stem does not click.
That refusal is the same one `photo agent add-to-album` uses.
It is usage.
Chrome stays closed and `catalog.txt` stays byte-identical.
The session calls `putInAlbum` and then `takeOut`.
It does not run `photo agent add-to-album` or `photo agent remove-from-album`.
One swap publishes both albums only when both halves are clean.
The destination gains the new item line.
The filename is `--current`.
The date and the uploader come from the read when it has them, and otherwise from the source line.
The destination `count` increases by one.
When an `error` line is present, `of` becomes the new count and `got` increases by one.
`name-failures` stays.
The source loses that item line and `count` decreases by one.
When an `error` line is present, `of` becomes the new count and `got` decreases by one.
`name-failures` stays.
Every other album is copied unchanged.
The catalog check is the same one `photo agent add-to-album` and `photo agent remove-from-album` use.
Any other outcome leaves `catalog.txt` byte-identical, including when the add already happened.
If `putInAlbum` misses before the confirm, `takeOut` is not called and recovery is one-click.
If the add confirm ran and the new key is not clean, `takeOut` is not called.
Recovery is refresh-only and the text names both titles.
If `putInAlbum` returned the clean key and `takeOut` then fails, recovery is refresh-only, the text names both titles,
and neither album is published.
A catalog read that throws after both halves return is refresh-only and leaves the file byte-identical.

`photo agent upload-item` uploads one archive file into an existing album, as Upload describes.
`photo agent create-album` creates an empty album, as Create describes.
`photo agent delete-album` deletes an album and leaves library items, as Delete album describes.
`photo agent delete-item` moves one library item to Google Photos trash, as Delete describes.
Before Chrome opens, the album has no `owner` line and the item's uploader is `-`.
A refusal is usage.
Chrome stays closed and `catalog.txt` stays byte-identical.

Google Photos cannot rename an item's filename (it is read-only in the UI).
There is no `rename-item` action.
A filename change is done locally after a download.
`photos-renames.tsv` is no longer supported.
Album renames are `photo agent rename-album`.

### Download

`photo agent download-item` writes one file at `archive/.gphoto-sync/.download/YYYY/MM/DD/<catalog filename>`.
The date is that item's catalog date.
When the date is `-`, the directory is `undated` instead of `YYYY/MM/DD`.
The filename is copied unchanged, including a camera prefix and the extension's case.
The command creates the staging day directory.
When that file is already there, it keeps the file and does not overwrite it.
It does not open the browser for a file that is already there.
A catalog filename that contains `/`, or is `.` or `..`, is rejected.

This directory is a staging inbox, not the archive.
Files are grouped by the catalog day because the same camera name occurs on many days.
They are not flat, and they are not grouped by album.
Album titles are not unique, and some are not safe directory names.
The same date and filename in two albums is one file here.
Nothing in this directory is renamed.
`photo file` is still the filer.
It files a dated download into the archive day folder, as Files describes.
The strip happens on the archive name.
Undated files stay in `undated/`.
The program does not read `.download` as archive pictures.
`photo agent download-item` does not rewrite `catalog.txt`, and it does not change Google Photos.
The command does not replace an archive original.
The archive inbox `.upload` is a different directory.

#### How `photo agent download-item` downloads

- Use the Google Photos **UI Download** control only.
  Do not invent or guess `batchexecute` RPC ids for download; unknown RPCs may change library data.
  Catalog reads (`snAcKc`, `fDcn4b`) stay read-only inventory work and are not a substitute for downloading originals.
- Open the item, then More options, then the menuitem whose accessible name starts with Download.
  Shift+D is optional when the viewer is focused.
  It is not a required step.
  The command does not choose a quality option.
  If Download is absent, that is a miss.
- Chrome's suggested download name may differ from the catalog filename.
  The bytes are still saved at the staging path above, using the catalog filename unchanged.
- When the same catalog day and filename already exist under `.download`, keep the existing file and do not overwrite
  it.
  The command does not open the browser for that file.
- UI Download (More options → Download / Shift+D) returns the file as Google Photos stores it.
  There is normally no quality chooser for a single-photo download.
  The info panel's pixel size is that stored file.
  A match means the download is not a web preview of a larger Google item.
- Still never invent or guess batchexecute RPC ids for download.

#### Operator notes

- A stored Google file can still be smaller than the camera JPEG in the archive.
  The 2012-08-20 Galaxy Nexus shots are the checked case: the archive file is 2592 on the long side, and both Google
  names are 2048 on the long side.
  The info panel for `20120820_184958.jpg` showed 1536×2048, matching the staged file.
  Keep the archive file.
  Do not replace it with the staged download.
- When both `IMG_<timestamp>.jpg` and `<timestamp>.jpg` are the same shot in Google, the `IMG_` copy is the Google
  re-encode (`Software` is `Google`, and the file is smaller at the same pixel size).
  The bare name is the better of the two Google files.
  Neither one replaces the archive original.
- For doubt on a future download: open the photo info panel (i) and compare pixels (and camera/date) to the staged file
  before re-downloading.
  Takeout remains the bulk alternative for archives.

### Upload

`photo agent upload-item` uploads one archive file into an existing album.
This is not `batchCreate`.
The command does not call `Publish`.
Before Chrome opens, the command refuses when `path` is outside the archive, when the extension is not `jpg`, `jpeg`,
`mov`, `mp4`, `avi`, or `3gp`, when gather's stem and civil-date rules find a catalog item, or when that file's sidecar
has a non-empty `google-media-item-id` or `already-in-google` true.
RAW, `heic`, `png`, and `webp` stay local.
`acceptPut` runs on the destination before Chrome opens, and again on the reread under the lock.
An `owner` line, `auto-add`, unread names, or an excepted stem does not open Chrome.
A refusal is usage.
Chrome stays closed and `catalog.txt` stays byte-identical.
The command opens the destination album.
When that album has items, the control is the button whose accessible name is `Add photos`.
When that album is empty, the control is the button whose accessible name is `Select photos`.
If neither button is present, the command fails one-click and clicks nothing else.
That button opens the add dialog.
The commit control is the button whose accessible name is `Select from computer`.
That button opens the file chooser.
Choosing the file uploads it into the open album.
There is no second confirm button.
The dialog button `Done` adds selected library photos.
The command does not click it.
The command does not click a photo in the picker.
The command does not click New album, Add a title, or the menuitem Import photos from your computer.
If `Select from computer` is absent, the command fails one-click and clicks nothing else.
The command reads the destination keys with `snAcKc` before the file is chosen and again after it.
Do not invent or guess a `batchexecute` RPC for the upload.
It publishes only when the destination key set grew by exactly one key that was absent before and that key's `fDcn4b`
filename equals the archive file's name.
The new `item` line uses that filename.
The date and the uploader come from that read when it has them.
The destination `count` increases by one.
When an `error` line is present, `of` becomes the new count and `got` increases by one.
`name-failures` stays.
When there is no `error` line, none is written.
No `unset` line is written.
Anything else leaves `catalog.txt` byte-identical.
A miss before the file is chosen is one-click.
After the file was chosen, a live read that is not the clean case is refresh-only.

### Create

`photo agent create-album` creates an empty album.
The command does not call `Publish.createAlbum`.
The command does not write an album toml.
Linking stays `photo reconcile` or `photo agent album-file`.
Before Chrome opens, the command refuses an empty title, a title that contains a tab, a CR, or a newline, and a title
that already names a stored album.
A refusal is usage.
Chrome stays closed and `catalog.txt` stays byte-identical.
The command opens `https://photos.google.com/albums`.
The control is the button whose accessible name is `Create album`.
If that button is absent, the command fails one-click and clicks nothing else.
The command does not open the plus menu.
The command does not click `Create and add photos`.
Clicking `Create album` creates the album and opens the editor.
The album exists before Done.
The textarea's accessible name is `Edit album name`.
The confirm button's accessible name is `Done`.
The command does not take an id from the page address.
After Done, the live title is the albums-list card, not the textarea just filled.
The command reads `albumCards` and `readAlbum` in memory.
It publishes with `RefreshPlan.plan` and `RefreshPlan.publish` in `Mode.Album`.
The command does not call `Refresh.work`, `Refresh.run`, or `wipe`.
The command does not write `status.txt`, `albums.tsv`, or `parts/`.
`Mode.Album` does not use the 250-card floor and does not emit `Drop`.
When the live card title is missing or ambiguous, `plan` fails, `catalog.txt` stays byte-identical, and recovery is
refresh-only, because the create click already happened.
A miss before that click is one-click.
Success is the catalog `RefreshPlan.publish` writes for that new card.
The command does not store a Library API album id in `google-album-id`.
Do not invent or guess a `batchexecute` RPC for this.

### Delete

Before Chrome opens, the album has no `owner` line and the item's uploader is `-`.
A refusal is usage.
Chrome stays closed and `catalog.txt` stays byte-identical.
The command opens the item the way `photo agent download-item` opens it.
The control is the button whose aria-label is `Move to trash`, or More options and then the menuitem `Move to trash`.
The confirm button's name starts with `Move to trash`.
`photo agent delete-item` does not click Delete forever or Empty trash.
`photo agent delete-item` publishes nothing for Delete forever or Empty trash.
`photo agent delete-item` does not click Remove, Remove from album, or New album.
`photo agent delete-item` does not delete the album.
After the confirm, the command reads the source keys with `snAcKc`.
Do not invent or guess a `batchexecute` RPC for this.
When the source key is already absent before the trash click, `photo agent delete-item` does not click.
`photo agent delete-item` still removes that `item` line.
When the trash confirm ran and the source key is gone, `photo agent delete-item` removes that `item` line.
`count` decreases by one.
When an `error` line is present, `of` becomes the new count and `got` decreases by one.
`name-failures` stays.
The command does not set `count` to the number of remaining `item` lines while an `error` line or an `unset` line
remains.
A miss before the confirm leaves `catalog.txt` byte-identical.
When the confirm ran and the source key is still present, `catalog.txt` stays byte-identical.
`photo agent delete-item` does not change the archive or `.download/`.
`photo agent delete-item` does not trash any other item, including an `IMG_` re-encode copy.

### Delete album

Before Chrome opens, the album has no `owner` line and it is not `auto-add`.
A refusal is usage.
Chrome stays closed and `catalog.txt` stays byte-identical.
The command does not enqueue `delete-item`.
The command opens the album.
The control is More options, then the menuitem whose accessible name is `Delete album`.
The dialog's buttons are `Keep album` and `Delete`.
The confirm button's accessible name is `Delete`.
The command clicks it only when the other button is `Keep album` and the dialog text contains "remain in Google Photos".
`photo agent delete-album` does not click Delete forever or Empty trash.
`photo agent delete-album` does not click a button whose name starts with Move to trash.
Those buttons stay unclicked even when `Delete` is also listed.
`photo agent delete-album` does not trash the album's members and does not empty trash.
A miss before the `Delete` click is one-click.
After that click, the command reads `albumCards` in memory.
It does not call `readAlbum`.
It does not call `RefreshPlan.plan` or `RefreshPlan.publish`.
It does not call `Refresh.work`, `Refresh.run`, or `wipe`.
It does not write `status.txt`, `albums.tsv`, or `parts/`.
It publishes only when that card list has at least 250 cards and the card id set is exactly the stored ids minus the
deleted id.
The new catalog is the header plus every other album's source block, in stored order.
A survivor card whose count or title differs is still copied.
The command does not apply `Add`, `CountChange`, or `Retitle`.
Any other card list leaves `catalog.txt` byte-identical, and recovery is refresh-only.
The operator then runs `photo refresh`.
Do not invent or guess a `batchexecute` RPC for this.

### Process

`photo agent rename-album` is the applier for an album rename.
`photo agent set-description` is the applier for an item description.
`photo agent set-date` is the applier for an item date.
`photo agent download-item` is the applier for a download.
`photo agent add-to-album` is the applier for adding an item to an existing album.
`photo agent remove-from-album` is the applier for removing an item from an album.
`photo agent move-item` is the applier for moving an item from one album to another.
`photo agent delete-item` is the applier for moving one library item to trash.
`photo agent upload-item` is the applier for uploading one archive file into an existing album.
`photo agent create-album` is the applier for creating an empty album.
`photo agent delete-album` is the applier for deleting an album.
