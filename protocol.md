# Google Photos reconciliation protocol

The archive agent reconciles the archive with Google Photos.
This note is the contract.
A catalog id is a Photos UI key.
It is not a Library API id.
Keep catalog ids and item keys out of chat and out of `google-album-id` and `google-media-item-id`.

- Files — who writes each file.
- Catalog — the `catalog.txt` grammar.
- Album links — the `album-links.tsv` grammar.
- Refresh — how `photo refresh` reads Google Photos.
- Album files — match, the album file, reconcile, and filing.
- Requests — `photo agent rename-album`, `photo agent set-description`, `photo agent set-date`,
  `photo agent download-item`, `photo agent add-to-album`, `photo agent remove-from-album`, `photo agent move-item`,
  `photo agent delete-item`, and the legacy request grammar.

## Files

The archive is the configured originals directory.
`rootPath` in the configuration file names it, under the home directory.
The catalog, the request file, and the result file live in `archive/.gphoto-sync/`.
There is no config key for that directory.

`photo refresh` rewrites `catalog.txt`.
`photo agent rename-album` rewrites one album line after the live title equals the new title.
`photo agent set-description` does not rewrite `catalog.txt`.
`photo agent set-date` rewrites that item's date after the `fDcn4b` read shows the calendar day.
`photo agent download-item` stages one original and does not rewrite `catalog.txt`.
`photo agent add-to-album` rewrites the destination album only for the clean confirmed key and otherwise leaves
`catalog.txt` byte-identical.
`photo agent remove-from-album` rewrites that album only when the source key is gone and otherwise leaves `catalog.txt`
byte-identical.
`photo agent move-item` rewrites the source album and the destination album in one swap only when `putInAlbum` and
`takeOut` are both clean.
Otherwise `catalog.txt` stays byte-identical.
If the add confirm ran, recovery is refresh-only and the text names both titles.
`photo agent delete-item` removes that item line when the source key is already absent, and when the trash confirm
leaves the key gone.
Delete forever and Empty trash are not clicked.
`photo agent delete-item` publishes nothing for Delete forever or Empty trash.
When the confirm ran and the key is still present, `catalog.txt` stays byte-identical.
`photo agent delete-item` does not change the archive or `.download/`.
`requests.tsv` is a legacy file that a later command will read.
It is not a second current path.
The script does not apply the batch.
`photo reconcile` reads and writes `album-links.tsv`.
`photo refresh` does not write that file.
The program does not append `requests.tsv`.
The local reader reads it and does not write it.
`photo agent request` has no action that appends a row.
`photo refresh` does not write `requests.tsv`.
The local reader writes `results.tsv`.
The program reads it.
`photo agent download-item` stages that original in `.download/`, as Download describes.
`photo file` files a dated download from there, as Filing describes.

`photo refresh` only reads Google Photos.
`photo agent rename-album` changes one album title.
`photo agent set-description` changes one item description.
`photo agent set-date` changes one item's date taken.
`photo agent download-item` downloads one original and does not change Google Photos.
`photo agent add-to-album` adds one item to an existing album.
`photo agent remove-from-album` removes one item from an album.
`photo agent move-item` adds one item to an existing album and then removes it from the source album, in one session.
`photo agent delete-item` moves one library item to Google Photos trash.
`photo publish` and `photo sync` still change Google Photos.
A request batch is not a refresh.

`catalog.txt.partial` and `catalog.txt.previous` exist only while a catalog rewrite is published.
They are not a second catalog.

## Catalog

`archive/.gphoto-sync/catalog.txt` is the catalog.

The reader accepts one grammar.
A file whose first line is not exactly `# gphoto-catalog 2` fails with `catalog is not version 2`.
The parser does not look at the rest, and it does not try a `##` header.
UTF-8, no BOM, LF only.
A CR is an error.
A blank line is an error.
No comments beyond the version line.
No quotes and no backslash escapes.
Nothing is trimmed.
No field contains a tab or a newline.
Title spaces, including leading and trailing spaces, are significant.
Exact titles are never trimmed.
Titles are not unique.
The album id is the identity.
An album id and an item key are Photos UI keys and start with `AF1Qip`.

```
# gphoto-catalog 2
album	<id>	<count>	<title>
owner	<id>	<label>
auto-add	<id>
error	<id>	<got>	<of>	<name-failures>
unset	<id>	<date>	<item-key>	<uploader>
item	<id>	<filename>	<date>	<item-key>	<uploader>
```

Column counts are fixed: `album` 4, `owner` 3, `auto-add` 2, `error` 5, `unset` 5, `item` 6.
`<id>` is the album id on every line, so a line does not need a section.
Order is not significant.
The local reader still groups lines by album so a person can read the file.

Integers are `[0-9]+` and fit in a 32-bit signed int.
No sign and no leading zero, except the number 0 itself.
`date` is `-` or `YYYY-MM-DD`.
It is the calendar date Google Photos shows for that item.
A clock time is omitted.
The date is the item's date, not the album's date.
`uploader` is `-` when the signed-in account uploaded the item.
Otherwise it is the display name, or the account id when no name is shown.
Empty is not a substitute for `-`.
An item filename is exact.
An item key is the stable id of that item in that album.
It is not a library id.
The same filename in two albums is two keys.
A move writes a new destination key, as Catalog updates describes.

The following are parse errors:

- Exactly one `album` line per id.
  A second `album`, `owner`, `auto-add`, or `error` for the same id is an error.
- Every `owner`, `auto-add`, `error`, `unset`, and `item` id has that album line.
- `owner` is present only when the signed-in account does not own the album.
  No `owner` line means that account owns it.
  The owner is not the uploader.
  Do not infer ownership from who uploaded the items.
  Do not put the owner on the `album` line.
  The label is non-empty and is not only whitespace.
  It may contain spaces and colons.
  The local reader turns each tab or newline in a label or an uploader name into one space before it writes the field,
  with no collapsing and no other trim.
  Titles and filenames are not rewritten.
  If a title or a filename contains a tab or a newline, the local reader fails the completeness check instead of
  substituting.
- `count` is the album-list count.
  It may exceed the number of `item` lines when an `error` line is present.
- `error.of` equals the album `count`.
  `error.got` equals the number of `item` lines plus the number of `unset` lines for that id, and `error.got` is less
  than or equal to `error.of`.
  `error.name-failures` equals the number of `unset` lines.
- The identity of an `item` or `unset` line is the pair (album id, item key).
  A second `item` or `unset` with that same pair is an error, including one `item` and one `unset`.
  Other fields do not make the pair legal twice.
- An `item` filename is non-empty.
  An empty filename is a parse error.
  `unset` has no filename column.
- No `error` and no `auto-add`: the number of `item` lines equals `count`, and there is no `unset` line.
  A count of 0 with no items is an empty album.
- `auto-add` has no `item`, no `unset`, and no `error`.
  An `owner` line may sit on an auto-add album.
  Its items are not read.
- An `unset` line is an item whose filename was not read.
  It still carries a date, an item key, and an uploader when the local reader knows them.
  The date may be `-`.
  Do not invent a filename for it.

`count` on the album line stays the album-list count under an error, including when `name-failures` is 0.
The `item` lines are then the items to match.
The reader does not invent filenames for the missing ones.
An album with `name-failures` greater than 0, or with an `unset` line, still has unread names.
Refresh decides which read is kept.

Album ids are private album links.
Do not share the file.

## Album links

`archive/.gphoto-sync/album-links.tsv` is the link file.
The file is absent until the first reconcile that has a row.
Do not create it empty.
A file that exists and does not start with `# gphoto-album-links 1` is `album links are not version 1`.
The parser does not rewrite it into the new shape and does not delete it.

```
# gphoto-album-links 1
<catalog-album-id>	<archive-relative toml path>
```

Two columns.
The path is archive-relative, such as `2010/07/album.toml`, with no leading slash.
UTF-8, LF only.
A CR is an error.
No blank line.
Nothing is trimmed.
A second row for the same id or the same path is an error.
A pending album has no row.
Human command output does not print the catalog id.

## Refresh

`photo refresh` builds the catalog: every album with its item count and id, then each item's filename and date, album by
album.
It runs in the calling process.
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
  When `HYPRLAND_INSTANCE_SIGNATURE` is set, park the window as Browser says.
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
     The label is written as Catalog specifies.
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
     Write an `unset` line for that returned item, as Catalog describes.
     Write an `error` line when the number of returned item keys is less than the album-list count, or when any name was
     not read.
     Fill `of`, `got`, and `name-failures` as Catalog requires.
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
`--window-position=40,40`, and `--window-size=1400,900`.
Downloads are accepted.
The page timeout is unlimited.
Navigation waits at most 120 seconds.

Park only when `HYPRLAND_INSTANCE_SIGNATURE` is set.
Look for the Chrome window until it exists, every 300 ms, for up to 20 seconds.
Launch returns before Hyprland lists the window.
One look misses it and leaves the window on the focused monitor.
The window's command line contains `gphoto-sync-chrome` and `remote-debugging-pipe`, and it does not contain `--type=`.
Remember the cursor with `hyprctl cursorpos` before creating an output.
Record the focused monitor's name and position.
Create a headless output with `hyprctl output create headless`.
The new monitor is the name that was not in the set taken before that create.
Do not assume the name `HEADLESS-1`.
Place it with one `hl.monitor` call: that output name, mode `1280x800@60`, position `20000x20000`, scale `1`.
In the same eval, restore the focused monitor: mode `preferred`, its saved `x` and `y` as `XxY`, scale `auto`.
Move the Chrome window with `hl.dsp.window.move`.
`monitor` is the created name, `follow` is false, and `window` is `address:` plus the client address.
Lua arguments use `=`.
Restore the cursor with `hyprctl dispatch` of `hl.dsp.cursor.move({ x = N, y = M })`.
Do not pin the window.
Do not write the output into the Hyprland config.
When Chrome exits, restore the physical monitor, then remove only the output this process created.
If that window is still missing after 20 seconds, log `refresh park: window missing` and leave the window where it is.
When `HYPRLAND_INSTANCE_SIGNATURE` is unset, leave the window visible.

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

Do not write `requests.tsv` from a refresh.

`photo refresh` is the command.
This section is its procedure.
Do not treat a request row as a run of Refresh.
Refresh does not add a request-file action.

## Album files

Match one catalog album to the archive and write one album file.
Title grammar and on-disk paths are the archive-layout section of `README.adoc` in this repository.
A legacy request row follows Requests.
A historical album file is `external = true`.
Leave `google-album-id` unset.

`photo reconcile` links every catalog album to the files already in the archive.
It is not `photo agent album-file`.
Reconcile is that command.

`google-title` and `owner` mean what the archive-layout section of the README says.
An `owner` line is Catalog.

An album with an `owner` line is shared.
`photo agent album-file` does not rename it.
`google-title` is the exact catalog title, including its spaces.
`owner` is the label on that line.
The path title is still the local file, chosen from the working title below.

An album with no `owner` line, whose Google title is the path title, leaves `google-title` unset.
When the catalog title differs from that path title, `photo agent album-file` prints `rename yes`.
It does not append a request row.
A rename the operator wants is `photo agent rename-album`.
`--current` is the exact catalog title.
`--new` is the path title.

An album with no `owner` line, whose Google title is an override, stores that exact string in `google-title`.
`photo agent rename-album` is still the rename.
`--current` is the exact catalog title.
`--new` is a string the operator chose.

Sync compares Library API media ids and does not match filenames, so it cannot reconcile a hand-made album.
`photo agent album-file` stops at the local file.
It does not publish, sync, or run `repair --names`.
`photo agent move-item` uses the catalog item key, as Requests describes.
`photo agent delete-item` moves one library item to trash, as Delete describes.

### Names

Take the working title from the catalog title.
Remove leading and trailing spaces.
Collapse each run of spaces to one space.
Use that working title for the local path title.
When there is no `owner` line and `google-title` is unset, that path title is the `--new` of `photo agent rename-album`.
`--current` stays the exact catalog title.

### Skip

Skip an `auto-add` album.
`error`, `name-failures`, and `unset` mean what Catalog says.
When `name-failures` is 0, match the `item` lines.
Do not wait for a refetch.
When `name-failures` is greater than 0, or an `unset` line is present, skip that album until the names are read.
A working title with no year and no month name is a curated album.
Leave it as a `.toml` directly in `originals`.

### Match

The program performs the match.
Join a catalog item to the archive by its filename stem and its civil date.
The stem matches when that day's folder contains it.
Ignore extension case.
Stem case stays exact.
RAW, JPEG, and THM in one folder are one stem.
An uploader other than the signed-in account still counts when the stem matches.
An item with no local file stays in Google Photos.

When the raw stem is absent from the archive, drop one leading prefix, whichever is longest.
The prefixes are `MVIMG_`, `PXL_`, `IMG_`, `VID_`, and `MVI_`, in either case.
A `.MP` or `.PORTRAIT` tail stays in the stem.
Accept the remainder only when that same day's folder contains it.
A repaired stem that exists only on another day stays absent.
The same camera counter on another day is a different picture.

A still (`jpg` or `gif`) and a video (`mp4`, `mov`, `avi`, or `3gp`) in that day's folder that drop to one stem use a
trailing `v` on the video only.

The denominator of the match count is the `item` lines.
The album `count` is not the denominator.
Items the `error` line says were not returned are ignored.
The match rate does not decide whether to write the file.
Write the file from the pairs that match.
A catalog item with no local file stays out of `include`.
It is downloaded later into `archive/.gphoto-sync/.download/`, as Download describes.
`photo file` files a dated download from there.
Zero local matches still get a file.
`include` is then empty.

### Leftover name

Tokens are whitespace-separated words of the working title.
Remove the 4-digit year of the items.
Remove a day number that is one of the item days.
Remove one English month name, or its abbreviation (`Jan`, `Feb`, `Mar`, `Apr`, `May`, `Jun`, `Jul`, `Aug`, `Sep`,
`Sept`, `Oct`, `Nov`, `Dec`), when other name words remain.
Remove that single month word even when the span crosses into a later month.
`2010 February Purim Seuda` leaves `Purim Seuda`.
`2019 Feb 3 Bar Mitzvah Gold` leaves `Bar Mitzvah Gold`.

Two month words stay in the leftover.
They are not a span.
Turn the hyphen between them into a space, and keep any words after them.
`2011 March-April` stays `March April`.
A leftover that is nothing but those month words is dropped, and the title has no name.
`2009 December` and `2009 July` have no name.

A day number that is not an item day stays in the name.
`19 Kislev` keeps `19` because those items are not on the 19th.
When the leftover starts with a digit, write no file.
The scanner treats that `.toml` as a picture sidecar.

### File

One album.
A picture may be in several albums.
Date grain and an optional name are not types.
The file sits in the start folder, next to the pictures or the day folders.
The title is not the path.
An existing destination is an error unless `--replace`.
That is a curated file at the archive root, a month file, and a day file.
Day folders do hold non-digit album files.

A catalog album that is not a `YYYY/MM` title uses the catalog items' days.
The start is the earliest of those days.
The end is the latest.
The name is the leftover.
`include` lists only the matched pairs, which may be empty.
A leftover that is only month words is dropped.
One civil day with an empty leftover is `YYYY/MM/DD/album.toml`.
Use `all` when every local stem that day is matched.
Otherwise that day uses the compressor lines.
A stem with `bad-raw = true` and no JPEG still counts.

`include` is the compressor output over the matched `(day, stem)` set.
`all` is every picture whose folder day is in the title span.
`all except` is that set minus stems missing on every day they occur, and only when that line is strictly shorter than
the per-day lines.
Otherwise write per-day lines.
Do not merge days into one range.
A stem range is a maximal contiguous slice of that day's scanned stems, of equal width, in string order.
Quote a stem only when a bare token would be wrong: a space, `"`, `\`, `..`, or the word `except`.
Write the bare form otherwise.
`include = ["all"]` is the local spelling of `all`.
Publish still omits a stem that an external album also claims.
That skip is not a reason to avoid `all`.

Both albums may list a shared stem.
There is no narrower-wins rule.

A `YYYY/MM` catalog title stays a month title, including a partial month.
It does not become a file in the first day folder, and the English month is not a name.
The file is `originals/YYYY/MM/album.toml` when the leftover is empty, or `originals/YYYY/MM/Name.toml` when a real
label remains.
`all` on that file is used only when every local day in the month that has pictures has at least one matched pair, and
the number of missing local `(day, stem)` pairs is at most `max(5, (localPairs * 2) / 100)`.
`localPairs` is that count of local `(day, stem)` pairs, on days that have pictures.
The arithmetic is integer arithmetic, not floating point.
When the heuristic passes, `include` is `["all"]`.
When it fails, the album span stays the month, the title is still `YYYY/MM` or `YYYY/MM/Name`, and `include` is
`Include.compress(None, claimed, universe)`.
`claimed` is the matched `(day, stem)` set, and `universe` is the local pictures in the month.
That is per-day lines, and not album-level `all` or `all except`.
Write that file.
The match rate does not decide.
A day span that already reaches a later month stays a day span.

| Situation | File | Title | Include |
|---|---|---|---|
| Heuristic passes, no real leftover | `originals/YYYY/MM/album.toml` | `YYYY/MM` | `["all"]` |
| Heuristic passes, a real label remains | `originals/YYYY/MM/Name.toml` | `YYYY/MM/Name` | `["all"]` |
| Heuristic fails | `originals/YYYY/MM/album.toml` or `Name.toml` | `YYYY/MM` or `YYYY/MM/Name` | per-day lines, not album-level `all` or `all except` |

### Toml

Write keys in this order: `external = true`, then quoted `google-title` and `owner` when set, then `google-album-id` and
`published-title` when they are already set, then `include`, one string per line.
Quote a stem inside an include entry only when a bare token would be wrong.
Store `google-title` and `owner` exactly, with their spaces.
Leave the file as written.
Do not run the file through jtoml.
jtoml sorts keys and drops comments.

### Check

The command enforces `Include.expand` in process before the rename.
It does not shell out.
In the ordinary case, the expansion equals the matched `(day, stem)` set.
When the month heuristic passes, the expansion equals the local universe of the month, the matched set is a subset,
every day that has pictures has a match, and the number of missing `(day, stem)` pairs is at most the cap in the File
section.
A failed check writes no file.
From this repository, run `./gradlew run --args='albums YYYY/MM' --quiet`.
That line stays as a human check.
The picture line's `albums=` value contains the title just written, with no spaces after commas.
There is no `dated=` column.
An empty claim prints `albums=-`.

### Reconcile

`photo reconcile` reads `catalog.txt` and `album-links.tsv`.
The link grammar is Album links.
It does not call `photo agent album-file` and it does not append `rename-album`.
A rename is `photo agent rename-album`.

Pending means `external` left false and `google-album-id` empty.
There is no pending key.
Reconcile does not create, rewrite, or delete a pending file.
A pending album has no link row.

The first link of an external file uses the working title.
That title splits the catalog title on spaces, drops empty tokens, and rejoins them.
When `google-title` is present, the match is that exact catalog title.
Otherwise the match is the working title against the path title.
If the path that placement would use already exists and the working titles agree, reconcile links that file.
It does not create a second file.
If that path exists and the working titles disagree, reconcile reports it and skips it.
It does not overwrite.
Two catalog titles that place to one free path record one create.
The extra album is `ambiguous title` and is not linked.
Two catalog titles that place to one existing path are both `ambiguous title`.
Neither album takes that file.

After a row exists, the catalog id is the identity.
A later rename keeps the file path and does not move the file.
It sets `google-title` to the exact new catalog title when the omit rule below stores that key.
If that title newly parses as a different date span, reconcile reports it and leaves the file.

Omit `google-title` only when the album has no owner.
A shared album keeps `google-title` equal to the exact catalog title even when that equals the path title.
A file that `checkGoogle` would reject is reported and skipped.
Reconcile does not write `owner` onto a file that has `google-album-id`.
It does not write `google-album-id`.
An empty catalog title is not written.
Reconcile skips that album instead of storing `google-title = ""` or an empty `published-title`.

A published client album is `external` false with `google-album-id` set.
The first link is `published-title`, the title the client last wrote.
The UI id is stored only in `album-links.tsv`.
It is not copied into `google-album-id`.
When the matched include differs, reconcile updates `include`.
On that client album the same splice sets `published-title` to the desired title the file will have after the splice.
That is the new `google-title` when that line is stored, otherwise the path title.
The next `photo publish` then does not call `updateAlbumTitle`.
When the catalog title equals the path title, omit `google-title` and set `published-title` to the path title if it
differs.
External albums do not gain a `published-title`.

`--delete` removes only a toml whose stored link id is gone.
Day-folder pictures stay.
Without `--delete`, a gone file is reported and kept, and its link row stays so a later `--delete` still sees it.
An external file that never linked is reported and kept.
`--dry-run` prints the classes and writes neither toml nor `album-links.tsv`.
`--dry-run --delete` prints `gone` and its link rows are the rows a real `--delete` would keep.
It still does not remove the toml.

Classes are `create`, `update`, `gone`, `pending`, and `skipped`.
`photo agent drift` reports those classes and writes nothing.

### Filing

`photo file` reads `<archive>/.gphoto-sync/.download/` only.
There is no config key.
The staging layout is Download.
A dated file is `YYYY/MM/DD/<catalog filename>`.
When the catalog date is `-`, the directory is `undated`.
Undated files stay there and are not filed.
Nothing inside `.download/` is renamed.
The strip happens on the archive name.

The destination name drops one leading prefix, whichever is longest, as Match does, and lowercases a known extension.
That is the same repair `repair --names` uses.
The command plans each day once, on that day's files plus every staged file for that day.
A video gains a trailing `v` when a still of the repaired stem is in that set, including one staged with the video.
A stem that is empty or does not start with a digit stays staged.

The destination is `<archive>/YYYY/MM/DD/<name>`.
The command creates the day directory.
It never overwrites an archive file.
If the destination exists, or the repaired name would collide with a different picture, the staged file stays and the
command reports it.
The same catalog date and filename in two albums is one staged file and one archive file.
Items in no album are out of scope.
This command does not download and does not call Google.
Other people's items that the Download control can fetch stay on `photo agent download-item`.
`--dry-run` prints the actions and writes nothing.

## Requests

`photo agent rename-album` changes one album title.
The album has no `owner` line.
`--current` equals the catalog title exactly.
`--new` is non-empty and differs from `--current`.
The control is More options, then the menuitem Edit album, Rename album, or Rename.
Replace the title field and confirm with Done, or with Enter when Done is absent.
After the live title equals `--new`, that album line's title becomes `--new`.
The album id, the count, the owner line, and the item lines stay.

`photo agent set-description` sets one item's description.
The item key is an `item` line on exactly one album.
`--current` equals that filename.
`--new` may be empty.
A tab or a newline in `--new` is rejected.
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
`--current` equals that filename.
`--new` is `YYYY-MM-DD HH:MM` local.
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
`--title` names the album.
`--id` is required when more than one album has that title, and it must match that title.
The item key is one `item` line on that album.
`--current` equals that filename.
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
`--title` names the source album and `--dest` names the destination album.
`--id` is required when more than one album has the source title, and it must match.
`--dest-id` is required when more than one album has the destination title, and it must match.
The source item is that album's one `item` line whose key is `--item` and whose filename equals `--current`.
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
`--title` names the album.
`--id` is required when more than one album has that title, and it must match.
The item is that album's one `item` line whose key is `--item` and whose filename equals `--current`.
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
`--title` names the source album and `--dest` names the destination album.
`--id` is required when more than one album has the source title, and it must match.
`--dest-id` is required when more than one album has the destination title, and it must match.
The source item is that album's one `item` line whose key is `--item` and whose filename equals `--current`.
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
Success names the action and both titles.
Stdout has no album id and no item key.

`photo agent delete-item` moves one library item to Google Photos trash, as Delete describes.
`--title` names the album.
`--id` is required when more than one album has that title, and it must match.
The item is that album's one `item` line whose key is `--item` and whose filename equals `--current`.
Before Chrome opens, the album has no `owner` line and the item's uploader is `-`.
A refusal is usage.
Chrome stays closed and `catalog.txt` stays byte-identical.

The grammar below is legacy.
The column tables, the version lines, and Catalog updates describe the old row statuses.
They stay until a later command reads `requests.tsv`.
That file is not a second current path.
The script does not apply the batch.

`archive/.gphoto-sync/requests.tsv` is written by the program.
The local reader reads it and does not write it.
The file is absent between batches.
Do not create it empty.
A file that exists and does not start with `# gphoto-requests 2` is `requests are not version 2`.
The program does not rewrite it into the new shape and does not delete it.

```
# gphoto-requests 2
<request-id>	<action>	<album-id>	<item-key>	<current>	<new>
```

Six columns on every row.
Empty means unused.
An unused field is empty, not omitted.
No field contains a tab or a newline.
UTF-8, LF only.
A CR is an error.
A blank line is not a row.
A trailing partial line with no LF is a parse error, not a row.
The first appended row creates the file and writes the header and that row, each ending in LF.
The program does not create the file before that.

Album ids and item keys are the stable keys.
`current` must match the catalog exactly.
A mismatch does not append.
Rows are processed in file order.

A request id is `r` plus a non-zero decimal with no leading zero: `r[1-9][0-9]*`.
`r1` is legal.
`r0`, `r00`, `r000`, and `r001` are not.
The program assigns the next integer after the maximum already in the file and formats it with no padding.
An empty file's first id is `r1`.

| action | album-id | item-key | current | new |
|---|---|---|---|---|
| `rename-album` (legacy) | album id | empty | current title, exact | new title |
| `move-item` (legacy) | source album id | item key | current filename | destination album id |
| `set-description` (legacy) | empty | item key | current filename | new description, empty clears |
| `set-date` (legacy) | empty | item key | current filename | `YYYY-MM-DD HH:MM` local |
| `download-item` (legacy) | album id | item key | current filename | empty |
| `delete-item` (legacy) | album id | item key | current filename | empty |

| action | Accept only when |
|---|---|
| `rename-album` (legacy) | The album id exists, there is no `owner` line, `current` equals the catalog title exactly, `new` is non-empty, and `new` differs from `current`. |
| `move-item` (legacy) | The source album and the destination album exist, the item key is an `item` line on the source (not an `unset` line), `current` equals that filename, and the destination id differs from the source. |
| `set-description` (legacy) | `album-id` is empty, the item key is an `item` line on exactly one album, `current` equals that filename, and `new` may be empty. |
| `set-date` (legacy) | The same item identity as `set-description`, and `new` matches `YYYY-MM-DD HH:MM`. |
| `download-item` (legacy) | The album id and item key name one `item` line, `current` equals the filename, `new` is empty, and the filename is not `.`, `..`, or a string containing `/`. |
| `delete-item` (legacy) | The same identity check as legacy `download-item`, `new` is empty, the album has no `owner` line, and the item's uploader is `-`. |

`rename-album` changes the album title.
A legacy `move-item` row adds the item to the destination album, then removes it from the source album.
The item stays in the library.
A legacy `set-description` row sets the item's description.
An empty `new` clears it.
A legacy `set-date` row changes the item's date taken.
`new` is the item's local time.
A legacy `download-item` row downloads the original into the staging directory in Download.
For that row, the program does not create the staging day directory.
The local reader does.
A legacy `delete-item` row is not the applier.
`photo agent delete-item` moves that one library item to Google Photos trash, as Delete describes.

### Catalog updates

This subsection describes the old row statuses.

Applying a batch updates `catalog.txt` in that pass.
A legacy `download-item` row does not change it.
A legacy `delete-item` row is not the applier.
`photo agent delete-item` changes the catalog as Delete describes.
`set-description` does not change it: the catalog has no description column.
A `new` value that contains a tab or a newline fails that row and is not written.
The result note does not contain an album id or an item key.
A key learned during the batch is written only on a catalog line.

`rename-album` replaces that album line's title with `new` after the live title equals `new`.
The album id, the count, the owner line, and the item lines stay.
The control is More options, then the menuitem Edit album, Rename album, or Rename.
Replace the title field and confirm with Done, or with Enter when Done is absent.

`set-date` sets that item's `date` to the `YYYY-MM-DD` prefix of `new` after a fresh item read shows that calendar day.
The catalog omits the clock.
The item key, the filename, the uploader, and the album count stay.
When the read date is not that day, the row is `failed` and the old date stays.
In the info panel, open the control whose aria-label starts with `Date taken`.
Open info first when that control is absent.
The fields are Year, Month, Day, Hour, Minutes, and AM/PM.
Type each value.
Read AM/PM back after typing it.
When it is not the value in `new`, choose that option, or type it and press Enter.
Confirm with Save, Done, or OK.
The item read that decides the catalog date is the same `fDcn4b` read Refresh uses.

A legacy `move-item` row adds the item to the destination album, then removes it from the source album.
`photo agent move-item` is not this row.
The destination key is not the source key.
Before the change, read the destination album's item keys with `snAcKc`.
On the open item, More options, then Add to album, then the existing destination album.
The sandbox run opened that same menu and created a new album (New album, Add a title, Done).
It did not click an existing album.
Removal on the source is More options, then Select photos, then the item, then Remove.
The confirm dialog must be a remove.
A dialog about trash is not this action.
Opening the item and choosing Remove from album, then confirming Remove, is the same removal.
After those steps, read both albums' item keys again.
The destination key is the one key present after and absent before whose `fDcn4b` filename equals `current`.
The date and the uploader on the new line come from that read when they are present, and otherwise from the source line.
When that key is exactly one and the filename equals `current`, write the destination `item` line with the new key and
the same filename.
Increment the destination `count` by one.
When the destination has an `error` line, set `of` to the new count, increment `got` by one, and leave `name-failures`
unchanged.
When it has no `error` line, write none.
The new number of item lines equals the new `count`.

When the destination key set grew by exactly one and that key's filename was not read, write an `unset` line for the new
key.
Increment `count` by one and `name-failures` by one.
`got` counts that `unset` line.
`of` is the new count.
An `unset` line requires an `error` line.
The row is not `done`.
The filename was not confirmed.

When the new key cannot be identified, do not invent a key or a filename.
No new key on the destination leaves the destination lines unchanged.
More than one new key leaves the destination lines unchanged.
One new key whose filename was read and is not `current` is not a line.
When the source key is also gone, that destination is a gap until a later single-album read.
Set its `count` to the previous count plus one.
Write an `error` line, or update the one it has.
`of` is that count, `got` is the item lines plus the unset lines still present, and `name-failures` is unchanged.

On the source, when a source read shows the old key gone, remove that `item` line, decrement `count` by one, and adjust
an `error` line the way `photo agent delete-item` does.
When the old key is still present, leave the source lines.
The row is `done` only when the source line was removed and the destination `item` line was written with the new key and
the filename `current`.
Otherwise the row is `failed`.
The catalog still follows the two reads.

`set-description` opens info when the panel is closed.
The control is the button whose aria-label is Open info, or the i key.
The empty field's text is Add a description.
Type `new` into that field.
An empty `new` clears it.
The row is `done` only when the panel shows the resulting text.

Google Photos cannot rename an item's filename (it is read-only in the UI).
There is no `rename-item` action.
A filename change is done locally after a download.
New actions are added to this table only after the operator agrees.
`photos-renames.tsv` is no longer supported.
Album renames are `photo agent rename-album`.
The `rename-album` row is legacy.

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
It files a dated download into the archive day folder, as Filing describes.
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
- The command does not write `results.tsv` and does not rewrite the catalog.
- UI Download (More options → Download / Shift+D) returns the file as Google Photos stores it.
  There is normally no quality chooser for a single-photo download.
  The info panel's pixel size is that stored file.
  A match means the download is not a web preview of a larger Google item.
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
- Still never invent or guess batchexecute RPC ids for download.

### Delete

`photo agent delete-item` moves one library item to Google Photos trash.
`--title` names the album.
`--id` is required when more than one album has that title, and it must match.
The item is that album's one `item` line whose key is `--item` and whose filename equals `--current`.
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
There is no results row.
`photo agent delete-item` does not trash any other item, including an `IMG_` re-encode copy.

### Results

`archive/.gphoto-sync/results.tsv` is written by the local reader.

```
# gphoto-results 2
<request-id>	<action>	<done|failed|skipped>	<note>
```

One line per request row, four columns.
The status is exactly `done`, `failed`, or `skipped`.
The note is one field and may be empty.
The note does not contain an album id or an item key.
The program prints the note unchanged.
It does not try to redact.
UTF-8, LF only.
A CR is an error.
A blank line is not a row.
A trailing partial line with no LF is a parse error, not a row.
The results file is separate from the request file.
The local reader never writes into the request file.
It is rewritten for each processed batch.

### Process

`photo agent rename-album` is the applier for an album rename.
`photo agent set-description` is the applier for an item description.
`photo agent set-date` is the applier for an item date.
`photo agent download-item` is the applier for a download.
`photo agent add-to-album` is the applier for adding an item to an existing album.
`photo agent remove-from-album` is the applier for removing an item from an album.
`photo agent move-item` is the applier for moving an item from one album to another.
`photo agent delete-item` is the applier for moving one library item to trash.
The numbered steps are the legacy batch.
The script does not apply the batch.

1. The operator tells the archive agent to process the request file (e.g. "handle requests" / "process requests").
2. The local reader re-reads this note, validates every row against the catalog and live Google Photos (missing, stale,
   duplicated or invalid rows are noted in results and skipped), and performs the valid actions in Google Photos without
   a dry run or approval step.
   It re-checks each current value just before changing it, then verifies the result.
3. The local reader updates the catalog in the Catalog shape and writes `archive/.gphoto-sync/results.tsv`.
4. The program reads the results file.
   The archive agent reads it and continues from there.
   Delete or replace `requests.tsv` before the next batch.

The local reader never deletes, shares or edits anything in Google Photos without the operator's explicit OK.
Request handling when the operator says to process the file is that OK for the actions in `requests.tsv`.
A legacy `delete-item` row, once the operator says to process the request file, is the OK to trash those items.
`photo agent delete-item` is not this row.
Deleting an album or emptying trash still needs a separate explicit OK, as do sharing and deleting library items not
named by `photo agent delete-item`.
