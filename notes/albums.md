# Google Photos albums

Reconcile one Google Photos album with `~/Pictures/originals`.
The catalog format and the request rows are in `notes/protocol.md`.
Read that file before writing a request.
Title grammar and on-disk paths are the archive-layout section of `~/Podval/gphoto-sync/README.adoc`.
The Chrome read is `notes/catalog-refresh.md`.

`photo refresh` rewrites `catalog.txt`.
This note does not.
It matches one catalog album to the archive and writes one album file.
A historical album file is `external = true`.
Leave `google-album-id` unset.
A catalog id is a Photos UI key.
It is not a Library API id, so it never goes in `google-album-id` or `google-media-item-id`.
Keep catalog ids and item keys out of the chat.

`photo reconcile` links every catalog album to the files already in the archive.
It is not `photo agent album-file`.
The Reconcile section is that command.

`google-title` and `owner` mean what the archive-layout section of the README says.
Read the `owner` line in `notes/protocol.md`.

An album with an `owner` line is shared.
Write no `rename-album` row for it.
`google-title` is the exact catalog title, including its spaces.
`owner` is the label on that line.
The path title is still the local file, chosen from the working title below.

An album Leonid owns, whose Google title is the path title, leaves `google-title` unset.
When the catalog title differs from that path title, write one `rename-album` row in
`~/Pictures/originals/.gphoto-sync/requests.tsv` in the same step as the local file.
`current` is the exact catalog title.
`new` is the path title.
Leave the row for Leonid to approve.
The local reader applies it.

An album Leonid owns, whose Google title is an override, stores that exact string in `google-title`.
A rename of that album uses the stored `google-title` as `current`.
The new title is a string he chose.
`photos-renames.tsv` is retired.

Sync compares Library API media ids and does not match filenames, so it cannot reconcile a hand-made album.
A later `move-item` uses the catalog item key, as `notes/protocol.md` describes.
Stop at the local file and the request row.
It does not publish, sync, or run `repair --names`.

## Names

Take the working title from the catalog title.
Remove leading and trailing spaces.
Collapse each run of spaces to one space.
Use that working title for the local path title.
When Leonid owns the album and `google-title` is unset, that path title is also the new Google title.
The request's `current` stays the exact catalog title.

## Skip

Skip an `auto-add` album.
The meaning of `error`, `name-failures`, and `unset` is the catalog section of `notes/protocol.md`.
When `name-failures` is 0, match the `item` lines.
Do not wait for a refetch.
When `name-failures` is greater than 0, or an `unset` line is present, skip that album until the names are read.
A working title with no year and no month name is a curated album.
Leave it as a `.toml` directly in `originals`.

## Match

The program performs the match.
Join a catalog item to the archive by its filename stem and its civil date.
The stem matches when that day's folder contains it.
Ignore extension case.
Stem case stays exact.
RAW, JPEG, and THM in one folder are one stem.
An uploader other than Leonid still counts when the stem matches.
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
It is downloaded later into `~/Pictures/originals/.gphoto-sync/.download/`, as `notes/protocol.md` describes.
Zero local matches still get a file.
`include` is then empty.

## Leftover name

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

## File

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

## Toml

Write keys in this order: `external = true`, then quoted `google-title` and `owner` when set, then `google-album-id` and
`published-title` when they are already set, then `include`, one string per line.
Quote a stem inside an include entry only when a bare token would be wrong.
Store `google-title` and `owner` exactly, with their spaces.
Leave the file as written.
Do not run the file through jtoml.
jtoml sorts keys and drops comments.

## Check

The command enforces `Include.expand` in process before the rename.
It does not shell out.
In the ordinary case, the expansion equals the matched `(day, stem)` set.
When the month heuristic passes, the expansion equals the local universe of the month, the matched set is a subset,
every day that has pictures has a match, and the number of missing `(day, stem)` pairs is at most the cap in the File
section.
A failed check writes no file.
From `~/Podval/gphoto-sync`, run `./gradlew run --args='albums YYYY/MM' --quiet`.
That line stays as a human check.
The picture line's `albums=` value contains the title just written, with no spaces after commas.
There is no `dated=` column.
An empty claim prints `albums=-`.

## Reconcile

`photo reconcile` reads `catalog.txt` and `album-links.tsv`.
The link grammar is `notes/protocol.md`.
It does not call `photo agent album-file` and it does not append `rename-album`.

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

After a row exists, the catalog id is the identity.
A later rename keeps the file path and does not move the file.
It sets `google-title` to the exact new catalog title when the omit rule below stores that key.
If that title newly parses as a different date span, reconcile reports it and leaves the file.

Omit `google-title` only when the album has no owner.
A shared album keeps `google-title` equal to the exact catalog title even when that equals the path title.
A file that `checkGoogle` would reject is reported and skipped.
Reconcile does not write `owner` onto a file that has `google-album-id`.
It does not write `google-album-id`.

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

Classes are `create`, `update`, `gone`, `pending`, and `skipped`.
