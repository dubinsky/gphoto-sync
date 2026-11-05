# Files

The archive is the configured originals directory.
`rootPath` in the configuration file names it, under the home directory.
The catalog and `album-links.tsv` live in `archive/.gphoto-sync/`.
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
`photo agent upload-item` adds one destination `item` line only for the clean confirmed key and otherwise leaves
`catalog.txt` byte-identical.
`photo agent create-album` adds the new album `RefreshPlan.publish` writes for that card and otherwise leaves
`catalog.txt` byte-identical.
It does not write an album toml.
`photo agent delete-album` omits that album's block when the live card list has at least 250 cards and those ids are
exactly the stored ids minus the deleted id.
Otherwise it leaves `catalog.txt` byte-identical.
It copies every other block and does not retitle or recount.
It does not trash library items.
`photo push` rewrites `catalog.txt` for each action that publishes, as Photos UI describes.
`photo reconcile` reads and writes `album-links.tsv`.
`photo refresh` does not write that file.
`photo agent download-item` stages that original in `.download/`, as Photos UI describes.
`photo file` files a dated download from there, as Filing describes.

`photo push` changes Google Photos for external albums, as Photos UI describes.
`photo publish` and `photo sync` still change Google Photos.

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
A catalog id is not a Library API id.

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

## Album files

Match one catalog album to the archive and write one album file.
Title grammar and on-disk paths are as Archive describes.
`google-album-id` stays unset.

`photo reconcile` links every catalog album to the files already in the archive.
It is not `photo agent album-file`.
Reconcile is that command.

`google-title` and `owner` mean what Archive describes.
An `owner` line is Catalog.

An album with an `owner` line is shared.
`photo agent album-file` does not rename it.
`google-title` is the exact catalog title, including its spaces.
`owner` is the label on that line.
The path title is still the local file, chosen from the working title below.

An album with no `owner` line, whose Google title is the path title, leaves `google-title` unset.
When the catalog title differs from that path title, `photo agent album-file` prints `rename yes`.
A rename the operator wants is `photo agent rename-album`.
`--current` is the exact catalog title.
`--new` is the path title.

An album with no `owner` line, whose Google title is an override, stores that exact string in `google-title`.
`photo agent rename-album` is still the rename.
`--current` is the exact catalog title.
`--new` is a string the operator chose.

Sync compares Library API media ids and does not match filenames, so it cannot reconcile a hand-made album.
`photo agent album-file` stops at the local file.
`photo agent move-item` uses the catalog item key, as Photos UI describes.
`photo agent delete-item` moves one library item to trash, as Photos UI describes.

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
A stem found only on the previous or next civil day is a date mismatch, not a match, not a missing item, and not a
second file.
A stem found only further away stays unmatched.
The same camera counter on another day is a different picture.

A still (`jpg` or `gif`) and a video (`mp4`, `mov`, `avi`, or `3gp`) in that day's folder that drop to one stem use a
trailing `v` on the video only.

The denominator of the match count is the `item` lines.
The album `count` is not the denominator.
Items the `error` line says were not returned are ignored.
The match rate does not decide whether to write the file.
Write the file from the pairs that match.
A catalog item with no local file stays out of `include`.
It is downloaded later into `archive/.gphoto-sync/.download/`, as Photos UI describes.
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
A client album still receives the stem.
`all` is still legal.
The external claim is not a reason to leave the stem out of the client album.

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

Then write quoted `google-title` and `owner` when set, then `google-album-id` and `published-title` when they are
already set, then `include`, one string per line.
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
From this repository, run `./gradlew run --args='album claims YYYY/MM' --quiet`.
That line stays as a human check.
The picture line's `albums=` value contains the title just written, with no spaces after commas.
There is no `dated=` column.
An empty claim prints `albums=-`.

### Reconcile

`photo reconcile` reads `catalog.txt` and `album-links.tsv`.
The link grammar is Album links.
`photo reconcile` does not call `photo agent album-file`.
A rename is `photo agent rename-album`.

Pending means the file contains `external = false` and the id is empty.
A missing key is not pending.
There is no pending key.
Reconcile does not create, rewrite, or delete a pending file.
A pending album has no link row.

`pending-rename` is not pending.
It is an external file with no `owner` line and an empty `google-title`, matched to a catalog album whose title differs
from the path title.
The text line is `pending-rename` and the path.
The text line does not include a catalog id.
Reconcile does not write that file.
Reconcile does not add a link row for that match.
A link row that already names the file stays.

The first link of an external file uses the working title.
That title splits the catalog title on spaces, drops empty tokens, and rejoins them.
When `google-title` is present, the match is that exact catalog title.
Otherwise the match is the working title against the path title.
If the path that placement would use already exists, the working titles agree, and the file is not `pending-rename`,
reconcile links that file.
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

A published client album contains the line `external = false` and the id.
A file with the id and without that line does not read.
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

Classes are `create`, `update`, `gone`, `pending`, `pending-rename`, and `skipped`.
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
If the planned stem is absent on the staged day and `resolve` hits the previous or next civil day, print
`date-mismatch <catalog-day> <catalog-filename> <archive-day> <stem>`, do not create the day directory, and do not
write.
The staged file stays in `.download`.
A stem found only further away stays the current file-or-keep behavior.
It never overwrites an archive file.
If the destination exists, or the repaired name would collide with a different picture, the staged file stays and the
command reports it.
The same catalog date and filename in two albums is one staged file and one archive file.
Items in no album are out of scope.
This command does not download and does not call Google.
Other people's items that the Download control can fetch stay on `photo agent download-item`.
`--dry-run` prints the actions and writes nothing.
