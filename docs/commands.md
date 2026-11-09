# Commands

### photo ls

`photo ls [path]` lists the date tree.
With no path, the shell lists the cursor and a one-shot lists the archive root.
A year, a month, or the root lists the directories inside it.
`photo ls 2010` lists months.
It does not list the items in that year.
`photo ls -R 2010` lists those items and does not print directory names.
Item listings are a day, a picture, `--recursive` / `-R`, or one of the picture filters below.
Those listings are ordered by the assessed civil time, including subseconds.
A picture with no assessed time goes after the timed rows.
That includes a folder-day clock and a sidecar disagreement.
Equal times, and those untimed rows, are ordered by the archive path.
Directory lines stay in directory-name order.
`--long` / `-l` counts on a directory, or the long item line on a day or a picture.
`--recursive` / `-R` lists items under the path.
`--name-contains` / `-n` keeps pictures whose name contains this text.
`--has-extension` / `-e` keeps pictures that have this extension.
`--property` keeps pictures whose sidecar property matches.
`--timestamp-from-name-format` keeps pictures whose filename clock uses this format.
`--timestamp-from-name` keeps pictures whose filename encodes a time.
`--timestamp-from-name-only` keeps pictures with a filename clock and no usable voting capture time.
`--timestamps-vary` keeps pictures whose file times differ by more than an hour.
`--has-metadata` keeps pictures that have a sidecar.
`--no-normal` keeps pictures with no normal image.
`--no-timestamp` keeps pictures with no assessed timestamp.
`--wrong-timestamp` keeps pictures whose file mtime differs from the assessed timestamp.
`--wrong-day` keeps pictures filed on a different day than the assessed timestamp.
`--wrong` keeps pictures with no normal image, no timestamp, a wrong timestamp, or a wrong day.
It does not rename, generate, set mtimes, rewrite albums, or move files.

### photo repair

`photo repair <path>` writes repairs.
At least one of `--names`, `--raw`, `--timestamps`, and `--days` is required.
`--names` renames one leading `MVIMG_`, `PXL_`, `IMG_`, `VID_`, or `MVI_` prefix and lowercases a known extension.
When a video shares a repaired stem with a still, the video's stem gains a trailing `v`.
It does not overwrite a file that is already there.
A stem that does not start with a digit is not renamed.
It rewrites `JPG` to `jpg`, `CR2` to `cr2`, and `Mp4` to `mp4`.
It lowercases `JPEG` to `jpeg` and does not produce `jpg`.
`--raw` makes a JPEG from RAW with dcraw when no normal image exists and `bad-raw` is not set.
`--timestamps` sets file mtimes from the assessed instant.
`--days` moves a picture into the assessed day folder.
A day-folder source still rewrites curated include paths before any of its files move, and a source that is not
`YYYY/MM/DD/stem` does not.
The empty-directory climb stops at the archive root.
A source under `.upload` does not delete `.upload`.
The steps run in that order, and each later step sees the files written by the earlier ones.
`--dry-run` prints those actions and writes nothing.

### photo item show

`ItemShow` prints the picture, the extensions, the sidecar (`Metadata.toString`, or `-`), the folder day, the assessed
clock, and the claiming album titles.
JSON has `path`, `extensions`, `sidecar`, `folderDay`, `assessed`, and `albums`.
It does not write.
Which clock wins is `docs/clock.md`.

### photo item set

This is the sidecar write.
The properties are `source`, `timestamp`, `date`, `bad-raw`, `orientation`, `google-media-item-id`, `already-in-google`,
and `uploader`.
`ItemSet` encodes through `Metadata.encode` and writes `{stem}.toml`, or deletes the file when the result is
`Metadata.default`.
`date=` stores that civil day and moves the picture.
Clearing `date` does not move the files.
Any other property leaves the files in their folder.
A claiming dated album that does not contain the new day is refused and nothing is written.
`--dry-run` prints the actions and writes nothing.

### photo item month

`item month <picture>...` includes the picture in `YYYY/MM`, creating that album when absent.
The folder day decides the month.
Every other album is left unchanged.
A picture may belong to any number of albums.
The created album lists each picture on its day instead of `all`.
An existing month album may compress to `all` when the month becomes complete.
Pictures are grouped with the earliest month first, and `create` is printed before that month's pictures.
`--dry-run` prints the actions and writes nothing.

### photo item into

`item into <title> <picture>...` includes the picture in an album that already exists.
Every other album is left unchanged.
A picture may belong to any number of albums.
A picture outside the span is refused, and nothing is written.
`--dry-run` prints the actions and writes nothing.

### photo album ls

`photo album ls [path]` lists album titles, one per line, in Scala string order.
No path lists every album.
A path keeps the titles `Album.albumMatches` selects.

### photo album show

`photo album show <title>` prints one album file.
`path` is archive-relative.
A missing span or name prints `-`.
An empty Google string prints `-`.
A missing owner prints `-`.
Include lines are indented two spaces.
No include key prints `include:` and no further lines.

### photo album claims

`photo album claims [path]` lists album claims.
Each picture line ends with `albums=` and every claiming title, in Scala string order, with no spaces after commas.
`2009/03/10/2412 albums=2009/03/10/Party,Best` is a picture in both albums.
An empty claim and an excluded-only claim both print `albums=-`.

### photo album new

`photo album new` creates a local album.
`--span` is a date span with no name.
`--name` is an album name.
A span starts as `include` `all` and does not create the later days of the span.
That file starts with `external = false`.
A name with no span writes the line `external = false` and omits `include`.
It is not an empty file.
An existing title is left unchanged.
`--dry-run` prints the actions and writes nothing.

### photo album rm

`photo album rm <title>` deletes the album file and leaves the pictures.
An external album is refused.
Empty directories above the file are removed, and the climb stops at the archive root.
`--dry-run` prints those actions and deletes nothing.

### photo album mv

`photo album mv <old> <new>` moves the album file.
The bytes move, so a comment and `google-title` stay.
An include line outside the new span is refused, and nothing is written.
An external album is refused.
`--dry-run` prints those actions and writes nothing.

### photo album include

`photo album include <title> <picture>...` adds pictures to one album and recompresses `include`.
Other album files stay unchanged.
`--dry-run` prints the actions and writes nothing.

### photo album exclude

`photo album exclude <title> <picture>...` removes pictures from that album and recompresses `include`.
A picture outside the album span is refused, and nothing is written.
A picture whose claim does not change prints nothing and writes nothing.
Other album files stay unchanged.
`--dry-run` prints the actions and writes nothing.

### photo album merge

`photo album merge <first> <second>` merges two adjoining month albums.
The album whose span starts earlier is the survivor.
The result runs from that start through the later end.
A day album, an overlap, a gap, an external album, a name mismatch, an existing result file, or Google metadata on the
later file is refused, and nothing is written.
`--dry-run` prints the actions and writes nothing.

### photo publish

`photo publish [path]` uploads `jpg`, `jpeg`, and video other than `m4v` into albums and files existing media ids.
A year or the archive root requires `--recursive`.
`--dry-run` prints the local plan and does not call Google.
A picture is added to every client album that lists it.
An external album is not a publish target.
Upload follows the sidecar and runs only when a client album will receive the picture.
A picture listed only by external albums is not uploaded.
An unclaimed picture is left out of the summary.
A picture excluded from every album that matches it counts as skipped-excluded and is not uploaded.
`skipped-external` counts a picture whose claims are all external.
A picture claimed by both kinds is not that count.
A picture sidecar `uploader`, when set, is not uploaded, and the summary counts `skipped-uploader`.

### photo sync

`photo sync [path]` reads Google and turns removals into local except lists.
A year or the archive root requires `--recursive`.
`--dry-run` reads Google, prints except and remove lines, and does not write.
An except line is `except`, the album title, and the stem in quotes.
A remove line is `remove`, the album title, and the stem in quotes.

### photo push

`photo push` edits external albums through the Photos UI.
Push scope is an external file with no `owner` line.
An `owner` line is skipped.
App-created albums stay on `photo publish` and `photo sync`.
The album order is the archive-relative path, in byte order, then the title.
For one album the actions are remove, then add, then upload, and one album finishes before the next.
An excepted stem is removed from the album.
It is not uploaded and it is not deleted from the library.
A catalog item that gather matches, and that is not already in the album, is added from the album that holds it.
An archive file with no catalog item and no `google-media-item-id` or `already-in-google` is uploaded.
RAW, `heic`, `png`, and `webp` stay local.
The run takes one refresh lock and one browser.
Each clean action publishes before the next.
On failure, stop.
That action leaves `catalog.txt` byte-identical.
Earlier actions may already have published.
The operator refreshes the named titles and runs `photo push` again.
`--dry-run` prints the plan and does not take the lock and does not open Chrome.
A line is `remove`, `add`, or `upload`, the fields are quoted, and the line has no catalog id.
The command is not part of `photo publish`.
It is not an MCP tool.
The command runs in the calling process.

### photo refresh

`photo refresh` reads the signed-in Chrome profile and rewrites `catalog.txt`.
The default is incremental.
It item-reads an album that is new or whose card count changed, and it does not item-read a stored auto-add album.
`--full` item-reads every album that is not auto-add.
`--album TITLE` may be repeated.
The title is exact.
`--full` together with `--album` is a usage error.
`--list` prints the diff and writes nothing that survives the process.
It composes with the default, with `--full`, and with `--album`.
A full or named list adds `would-read` or `owner-only` to the count diff.
`--allow-shrink` skips only the rows < 250 floor.
The command runs in the calling process.
It is not an MCP tool.
A second refresh prints `already running` and exits 2.

### photo reconcile

`photo reconcile` links catalog albums to local album files.
The first link of an external file is the working title, or `google-title` when that key is present.
A later rename keeps the path.
`--dry-run` prints the classes and writes nothing.
`--delete` removes a toml whose link id is gone.
Day-folder pictures stay.
The command runs in the calling process.
It is not an MCP tool.

### photo file

`photo file` files a dated download from `.gphoto-sync/.download/` into that civil day folder.
The layout is `YYYY/MM/DD/<catalog filename>`, or `undated` when the catalog date is `-`.
Undated files stay staged.
Nothing inside `.download/` is renamed.
One leading `MVIMG_`, `PXL_`, `IMG_`, `VID_`, or `MVI_` prefix is stripped on the archive name, and a known extension is
lowercased.
When a video shares that repaired stem with a still in that day's files or staged with it, the video gains a trailing
`v`.
The command creates the day directory.
It never overwrites an archive file.
A collision, or a stem that is empty or does not start with a digit, stays staged and is reported.
The same catalog date and filename in two albums is one staged file and one archive file.
Items in no album are out of scope.
Other people's downloadable items stay on `download-item`, which this command does not reimplement.
It does not call Google.
It prints `file <day> <staged-name> <archive-name>` or `keep <staged-path> <reason>`.
A real run adds `wrote <day>/<archive-name>` for each file it files.
An adjacent-day resolve prints `date-mismatch`, does not create a directory, and adds no `wrote` line.
`--dry-run` prints those actions and writes nothing.
The command runs in the calling process.
It is not an MCP tool.

### photo card

`photo card` prints one removable `vfat` or `exfat` volume.
It mounts that volume read-only when it is unmounted and `--dry-run` was not passed.
It does not copy.
`photo card --dry-run` prints that volume and does not mount.

### photo card copy

`photo card copy` copies `DCIM` picture files flat into `.upload`.
It does not delete the card.
With no path it uses the same discovery.
`photo card copy --dry-run` mounts when the volume is unmounted, prints `copy` and `keep` lines, and writes nothing.
A path is the directory that contains `DCIM` and skips discovery.
Filing is `photo repair .upload --names --days`.
The day is the assessed civil day already stored in `Dates.Assessment.dayShouldBe`, produced by `PictureAudit.apply` and
`Dates.dayShouldBe`, and this command does not define a second rule.
A filed stem is claimed when `Include.claims` matches that folder day.
`include = all` does that inside the album span.
`include = all except` does not claim an excepted stem.
No include line is written.
Unclaimed pictures are omitted from `photo publish`.
The command runs in the calling process.
It is not an MCP tool.

`photo login`, `photo status`, and `photo logout` manage the Google Photos token.

### photo login

`GoogleAuth.login` stores the token and prints `logged in (<absolute token path>)`.
It does not print the config path.

### photo status

The transcript is `logged in (<absolute token path>)` or `not logged in (<absolute token path>)`.
`value` is JSON null.
It is not an MCP tool.
A configuration error still fails in `Session.open`, before this transcript, and the message names
`~/.config/org.podval.tools.photo.toml` by the path `Configuration` reads.

### photo logout

If the token file exists, `GoogleAuth.logout` deletes it and prints `logged out (<absolute token path>)`.
Otherwise it prints `not logged in (<absolute token path>)`.

### photo shell

`photo shell` keeps `cd`, `pwd`, and `exit` in the shell.
Tab completion runs when the process has a console, and a non-console shell stays line at a time.
`bin/photo` starts `photo shell` with the terminal inherited, so an interactive session has a console.
A pipe has no console.
It starts a detached `photo serve` with `setsid` when the socket is down, and it does not stop that process on exit.
`photo refresh`, `photo reconcile`, `photo file`, `photo card`, `photo push`, `photo web`, `photo agent rename-album`,
`photo agent set-description`, `photo agent set-date`, `photo agent download-item`, `photo agent add-to-album`,
`photo agent remove-from-album`, `photo agent move-item`, `photo agent delete-item`, `photo agent upload-item`,
`photo agent create-album`, and `photo agent delete-album` stay in the shell process.
Commands that do not run in the calling process, and are not `cd`, `pwd`, or `exit`, still go to the socket.
Path arguments are archive-root paths.
`agent include` paths are sent unchanged.
A command is sent only when `archive` matches this process.
A mismatch names both paths, says to run `photo serve stop`, and does not run the command in this process.

A one-shot uses the socket when it is up and `archive` matches.
It runs in this process when the lock is free.
`photo refresh`, `photo reconcile`, `photo file`, `photo card`, `photo push`, `photo web`, `photo agent rename-album`,
`photo agent set-description`, `photo agent set-date`, `photo agent download-item`, `photo agent add-to-album`,
`photo agent remove-from-album`, `photo agent move-item`, `photo agent delete-item`, `photo agent upload-item`,
`photo agent create-album`, and `photo agent delete-album` run in this process even when the socket is up.
A one-shot does not start `photo serve`.
A mismatch names both paths and does not run in this process.
