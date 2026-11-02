# Google Photos reconciliation protocol

The archive agent reconciles `~/Pictures/originals` with Google Photos.
The catalog, the request file, and the result file live in the archive's `.gphoto-sync` child,
`~/Pictures/originals/.gphoto-sync/`.
There is no config key for that directory.
The program reads the catalog and writes request rows.
It reads the result file.
`photo refresh` rewrites `catalog.txt`.
Nothing else in the program does.
`photo reconcile` reads and writes `album-links.tsv`.
`photo refresh` does not write that file.
The program does not change Google Photos except `photo publish` and `photo sync`.
Applying `requests.tsv` stays the archive agent plus `reader/sandbox-act.mjs` in `~/Podval/gphoto-sync`.
The archive copy of that script stays until the deletion commit.
In this note, the local reader is that applier when a request batch is processed.
`photo refresh` is the catalog re-read.
The Chrome read is `notes/catalog-refresh.md`.
`photo refresh` does not write `requests.tsv`.

## Catalog

`~/Pictures/originals/.gphoto-sync/catalog.txt` is rewritten by `photo refresh`.
Nothing else in the program rewrites it.

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
`uploader` is `-` when Leonid uploaded the item.
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
- `owner` is present only when Leonid does not own the album.
  No `owner` line means he owns it.
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
  `error.name-failures` equals the number of `unset` lines for that id.
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
How `photo refresh` chooses which read to keep is `notes/catalog-refresh.md`.

Album ids are private album links.
Do not share the file.

`photo refresh` rewrites this file as `notes/catalog-refresh.md` describes, for every album, or for the titles he names.
Refresh rewrites the catalog only.
It does not change Google Photos.
A change Leonid makes in Google Photos by hand is not written into `catalog.txt` until a refresh reads that album.
Applying a request batch is the other writer.
That pass updates the catalog as Catalog updates, Download, and Delete describe.

## Album links

`~/Pictures/originals/.gphoto-sync/album-links.tsv` is written by `photo reconcile`.
`photo refresh` does not write it.
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

## Requests

`~/Pictures/originals/.gphoto-sync/requests.tsv` is written by the program.
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
| `rename-album` | album id | empty | current title, exact | new title |
| `move-item` | source album id | item key | current filename | destination album id |
| `set-description` | empty | item key | current filename | new description, empty clears |
| `set-date` | empty | item key | current filename | `YYYY-MM-DD HH:MM` local |
| `download-item` | album id | item key | current filename | empty |
| `delete-item` | album id | item key | current filename | empty |

| action | Accept only when |
|---|---|
| `rename-album` | The album id exists, there is no `owner` line, `current` equals the catalog title exactly, `new` is non-empty, and `new` differs from `current`. |
| `move-item` | The source album and the destination album exist, the item key is an `item` line on the source (not an `unset` line), `current` equals that filename, and the destination id differs from the source. |
| `set-description` | `album-id` is empty, the item key is an `item` line on exactly one album, `current` equals that filename, and `new` may be empty. |
| `set-date` | The same item identity as `set-description`, and `new` matches `YYYY-MM-DD HH:MM`. |
| `download-item` | The album id and item key name one `item` line, `current` equals the filename, `new` is empty, and the filename is not `.`, `..`, or a string containing `/`. |
| `delete-item` | The same identity check as `download-item`, `new` is empty, the album has no `owner` line, and the item's uploader is `-`. |

`rename-album` changes the album title.
`move-item` adds the item to the destination album, then removes it from the source album.
The item stays in the library.
`set-description` sets the item's description.
An empty `new` clears it.
`set-date` changes the item's date taken.
`new` is the item's local time.
`download-item` downloads the original into the staging directory in the Download section.
`delete-item` moves that one library item to Google Photos trash (see Delete).
The program only writes the row.
For `delete-item`, the program does not touch Google Photos and does not rewrite the catalog.
The local reader does both, after Leonid tells it to process the file.

## Catalog updates

Applying a batch updates `catalog.txt` in that pass.
`download-item` does not change it.
`delete-item` changes it as Delete describes.
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
The item read that decides the catalog date is the same `fDcn4b` read a catalog refresh uses.

`move-item` adds the item to the destination album, then removes it from the source album.
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
an `error` line the way `delete-item` does.
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
New actions are added to this table only after Leonid agrees.
`photos-renames.tsv` is no longer supported.
Album renames are `rename-album` rows in `requests.tsv`.

## Download

`download-item` writes one file at `~/Pictures/originals/.gphoto-sync/.download/YYYY/MM/DD/<catalog filename>`.
The date is that item's catalog date.
When the date is `-`, the directory is `undated` instead of `YYYY/MM/DD`.
The filename is copied unchanged, including a camera prefix and the extension's case.
The local reader creates the staging day directory.
The program does not.
When that file is already there, it keeps the file and does not overwrite it.
A catalog filename that contains `/`, or is `.` or `..`, fails that row.

This directory is a staging inbox, not the archive.
Files are grouped by the catalog day because the same camera name occurs on many days.
They are not flat, and they are not grouped by album.
Album titles are not unique, and some are not safe directory names.
The same date and filename in two albums is one file here.
Nothing in this directory is renamed.
`photo file` reads `.download/` and files a dated download into the archive day folder, as `notes/albums.md` describes.
The strip happens on the archive name.
Undated files stay in `undated/`.
The program does not read `.download` as archive pictures.
`download-item` does not rewrite `catalog.txt`, and it does not change Google Photos.
The archive inbox `.upload` is a different directory.

### How the local reader downloads originals

- Use the Google Photos **UI Download** control only.
  Do not invent or guess `batchexecute` RPC ids for download; unknown RPCs may change library data.
  Catalog reads (`snAcKc`, `fDcn4b`) stay read-only inventory work and are not a substitute for downloading originals.
- Open the item (album grid, or the item photo URL), open **More options**, choose **Download** (keyboard shortcut
  Shift+D when the viewer has focus).
  If a quality chooser appears, pick the original / full file, not a small web preview.
- Chrome's suggested download name often matches the catalog filename; still place the bytes at the staging path above
  using the catalog filename unchanged.
- When the same catalog day and filename already exist under `.download`, keep the existing file (do not overwrite), per
  the rule above.
- After the batch, write `results.tsv` as usual.
  Downloading does not rewrite the catalog.
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

## Delete

`delete-item` moves one library item to Google Photos trash.
The program only writes the row.
It does not touch Google Photos and does not rewrite the catalog.
The local reader does both, after Leonid tells it to process the file.

- Skip the row unless Leonid owns the album (no `owner` line) and the item's uploader is `-`.
- Skip the row when the current filename does not match the catalog exactly.
- If the item is already gone from Google Photos, remove its `item` line and mark the row `skipped`.
- On success, remove that item's `item` line and mark the row `done`.
- On both of those paths, decrement that album's `count` by one.
  That is the new album-list count.
  When an `error` line is present, set `of` to the new `count`, decrement `got` by one, and leave `name-failures`
  unchanged.
  Do not set `count` to the number of remaining `item` lines while an `error` line or an `unset` line remains.
- A UI failure leaves the catalog unchanged and marks the row `failed`.
- Use the Google Photos UI only: open the item and move it to trash.
  The control is the button whose aria-label is `Move to trash`, or More options and then that menuitem.
  The confirm button's name starts with `Move to trash`.
  Do not empty trash, do not choose Delete forever, and do not delete the album.
  Do not invent or guess a `batchexecute` RPC for this.
- Do not change anything under `~/Pictures/originals` or `.gphoto-sync/.download`.
- Only items in `delete-item` rows that Leonid put in `requests.tsv` are trashed; no other items (for example `IMG_`
  re-encode copies) are trashed as a side effect.

## Results

`~/Pictures/originals/.gphoto-sync/results.tsv` is written by the local reader.

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

## Process

1. Leonid tells the archive agent to process the request file (e.g. "handle requests" / "process requests").
2. The local reader re-reads this protocol, validates every row against the catalog and live Google Photos (missing,
   stale, duplicated or invalid rows are noted in results and skipped), and performs the valid actions in Google Photos
   without a dry run or approval step.
   It re-checks each current value just before changing it, then verifies the result.
3. The local reader updates the catalog in this version-2 shape and writes
   `~/Pictures/originals/.gphoto-sync/results.tsv`.
4. The program reads the results file.
   The archive agent reads it and continues from there.
   Delete or replace `requests.tsv` before the next batch.

The local reader never deletes, shares or edits anything in Google Photos without Leonid's explicit OK.
Request handling when Leonid says to process the file is that OK for the actions in `requests.tsv`.
A `delete-item` row, once Leonid says to process the request file, is the OK to trash those items.
Deleting an album or emptying trash still needs a separate explicit OK, as do sharing and deleting library items not in
`delete-item` rows.
