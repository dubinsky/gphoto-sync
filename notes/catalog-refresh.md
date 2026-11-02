# List Google Photos albums and their items (read-only)

`photo refresh` builds a catalog: every album with its item count and id, then each item's filename and date, album by
album.
The command only reads Google Photos.
It does not create, share, delete, or edit anything there.
`photo publish` and `photo sync` are separate commands and are not this note.
The lines it writes are version 2, as the catalog section of `notes/protocol.md` specifies.
`photo refresh` rewrites `catalog.txt`.

## Route: photos.google.com's own RPCs, from the signed-in Chrome on this machine

`photo refresh` is the reader.
It runs in the calling process.
It does not shell out.

- Not the Photos Library API (since 2025 it only sees media the calling app created).
  No connector exists.
- Not DOM scraping (lazy grid, no filenames in DOM).
  Call the page's `batchexecute` endpoint from the page's JS context, so cookies and XSRF apply.
- The browser is the signed-in profile `~/.config/gphoto-sync-chrome`.
  It is not the normal Chrome profile and does not appear in that profile's avatar menu.
- The window stays mapped while the album list scrolls.
  When `HYPRLAND_INSTANCE_SIGNATURE` is set, park the window as the Browser section says.
  On any other desktop the window stays visible.
- The work directory is `${TMPDIR:-/tmp}/gphoto-sync-refresh`, mode `0700`.
  Part files, `albums.tsv`, and `status.txt` are written there.
  A successful publish removes that directory.
  The lock file is the sibling `${TMPDIR:-/tmp}/gphoto-sync-refresh.lock`, mode `0600`.
  It is not inside the work directory, and the publish does not delete it.
  A second refresh prints `already running` as its first line, then `status.txt`, and exits 2.
- One album keeps one winner part.
  A thrown read is its own `# FAILED` buffer, not a line appended to a winner.

## Steps
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
   A new id is not guessed, and its items are read.
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
     Write an `owner` line for that album id only when the owner isn't the user.
     The label is written as the catalog section of `notes/protocol.md` specifies.
     Albums owned by others can't be renamed.
     The album owner is not the uploader of an item.
     No `owner` line means the user owns the album.
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
     The rest is the version-2 records for that album only.
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
     Write an `unset` line for that returned item.
     It carries a date, an item key, and an uploader when those are known.
     The date may be `-`.
     Write an `error` line when the number of returned item keys is less than the album-list count, or when any name was
     not read.
     `of` equals the album-list count.
     `got` equals the number of `item` lines plus the number of `unset` lines.
     `name-failures` equals the number of `unset` lines.
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
   The completeness checks are in Where to write it.

## Output format (catalog)

Emit the version-2 records from the catalog section of `notes/protocol.md`.
`count` on each `album` line is the album-list count from step 2.

## Read one album

A refresh Leonid limits to named album ids reads those albums and leaves every other album's lines as they are.
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
The live gate below is that proof.
Gradle tests do not call Google Photos.

Checking that one item is no longer in an album is a key comparison.
Read that album's `snAcKc` keys and see that the item key is absent.
The comparison does not use the share page's item count.

## Open questions
- Auto-add is a stored `auto-add` line.
  The command does not guess a new id from the album page.

## Browser

The catalog read launches `/usr/bin/google-chrome-stable` with user-data dir `~/.config/gphoto-sync-chrome`.
That directory is not `~/.config/google-chrome`, and it does not appear in normal Chrome's profile menu.
The window is headed.
The catalog read uses a 1400 by 900 viewport and the same window size, at position 40,40.
It drops the default `--enable-automation` argument.
It passes `--disable-blink-features=AutomationControlled`, `--no-first-run`, `--no-default-browser-check`,
`--window-position=40,40`, and `--window-size=1400,900`.
Downloads are accepted.
The page timeout is unlimited.
Navigation waits at most 120 seconds.

Park only when `HYPRLAND_INSTANCE_SIGNATURE` is set, and only after the Chrome window exists.
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
If the window command line has no `remote-debugging-pipe`, log `refresh park:` and leave the window where it is.
When `HYPRLAND_INSTANCE_SIGNATURE` is unset, leave the window visible.

## Parts and status

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
`.gphoto-sync/.download/` is only for media from `download-item`.
`catalog.txt.partial` and `catalog.txt.previous` stay next to `catalog.txt`.

## Privacy
Album titles, keys and filenames are personal.
Keep outputs on this computer.
Treat assembled scripts as private because they contain album keys.
`catalog.txt` is private catalog bytes.
`catalog.txt.partial` and `catalog.txt.previous` are too, while they exist.

## Where to write it

The merged catalog is the version-2 file the catalog section of `notes/protocol.md` names:
`~/Pictures/originals/.gphoto-sync/catalog.txt`.
`photo refresh` writes the new catalog to `catalog.txt.partial` in the same directory.
It replaces `catalog.txt` only when every check below holds.

1. The partial file is UTF-8, LF, no CR, no BOM, and ends with LF.
2. The first line is exactly `# gphoto-catalog 2`.
3. Every later line parses as one version-2 record.
   No blank line.
   No `## `, `OWNER:`, `AUTO-ADD`, `ERROR partial:`, or `[UNSET]`.
4. The set of `album` ids equals the set of album ids from this pass's album list, each exactly once.
5. The pass did not die mid-album.
   Every album id from that list has an `album` line.
   Every non-auto-add album either has an `error` line or has an `item` count equal to its `count`.
   A count of 0 with no items is complete.
   An auto-add album has no `item`, `unset`, or `error` line.
6. At most one `owner` and one `auto-add` per id, and the other integrity rules in the catalog section of
   `notes/protocol.md` hold.

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
Handling `requests.tsv` updates the catalog per `notes/protocol.md` and is not a run of this note.
`photo refresh` is the command.
This note is its procedure.
Do not treat a request row as a run of this note.
This note does not add a request-file action.

## Live gate

This is not a Gradle test.
Leonid runs it on this machine, against the signed-in profile `~/.config/gphoto-sync-chrome`.
The mutation is a sandbox album, never a historical archive album.
Creating it, changing it, and deleting it need his explicit OK.
The sandbox title matches no catalog line and is not a date span.
Start with one item so the card count is 1.

| Behavior | Mutation | Command | Observation | Cleanup |
|---|---|---|---|---|
| Album list | Create the sandbox album with one item | `photo refresh --list` | Diff prints the new title and card count 1. No historical title is in the reread list. `catalog.txt` is unchanged | Keep the album for the next rows |
| One-album read | No further edit | `photo refresh --album TITLE` | The catalog gains that title with count 1 and the sandbox filename. Other album blocks are unchanged | Keep |
| Incremental re-read | Add one item to the sandbox album | `photo refresh` | Before the command, record a checksum of `catalog.txt`. Stdout lists the sandbox title as reread, old count 1, new count 2. After the publish, the auto-add album's lines are unchanged. Every album id other than the sandbox and any real drop is unchanged. Every untouched album block, including the auto-add block, is byte-identical to the checksummed file. This is the first full publish | Keep |
| Title-only | Rename the sandbox album. Do not add or remove items | `photo refresh` | The album line has the new title. The item lines are unchanged. The title is reported as retitle, not reread | Keep |
| Drop | Delete the sandbox album, with explicit OK | `photo refresh` | `photo agent catalog` no longer lists the title. Day-folder pictures are untouched | This is the album cleanup. Trash leftover library items only with a separate OK |

Do not run this table from Gradle.
