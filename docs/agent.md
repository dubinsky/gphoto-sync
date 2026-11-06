# Agent

When `photo` is on `PATH`, call the command and use its JSON.
Do not parse `catalog.txt`, sidecar TOML, or the `YYYY/MM/DD` tree to answer these questions.
The grammars stay in their own sections for an agent that does not have the program.
This section does not restate the catalog grammar.
It does not document the MCP JSON-RPC envelope.
A truncated tool result is incomplete.
The day is whatever the command prints.
Keep catalog ids and item keys out of chat and out of `google-album-id` and `google-media-item-id`.
`photo agent` reads the catalog and the archive.
It writes an album file only for `photo agent album-file`.
It does not print album ids or item keys.

### photo agent stems

`photo agent stems <day>` lists that day's grouped stems.
The command does not write.
A line is the stem, then the extensions sorted and joined with `+`.
The metadata comes from sidecar TOML, not from EXIF.
JSON has `day` and `stems`.
Each stem has `stem`, `extensions`, and `metadata`.
A `metadata` key that is unset is absent.
The keys that may be present are `source`, `timestamp`, `date`, `bad-raw`, `orientation`, `google-media-item-id`,
`already-in-google`, and `uploader`.
Keep `google-media-item-id` out of chat.

### photo agent years

`photo agent years` lists the archive years.
The command does not write.
A line is the year, with no leading slash, such as `2009`.
JSON has `years`, an array of those strings.
The JSON has no album id, item key, or `google-media-item-id`.

### photo agent months

`photo agent months <year>` lists the months of one year.
The command does not write.
A line is the archive-relative month, with no leading slash, such as `2010/07`.
JSON has `year`, `present`, and `months`.
`present` is false when that year directory is absent, and `months` is then empty.
The JSON has no album id, item key, or `google-media-item-id`.

### photo agent days

`photo agent days <month>` lists the days of one month.
The month is `YYYY/MM`, such as `2010/07`.
The command does not write.
A line is the archive-relative day, with no leading slash, such as `2010/07/19`.
JSON has `month`, `present`, and `days`.
`present` is false when that month directory is absent, and `days` is then empty.
The JSON has no album id, item key, or `google-media-item-id`.

### photo agent claims

`photo agent claims <path>` lists album claims for one day.
The path is that day.
The command does not write.
A line is `<day>/<stem> albums=<title>,<title>`, with titles sorted and no spaces after commas.
An empty claim and an excluded-only claim both print `albums=-`.
JSON has `path`, `present`, and `claims`.
Each claim has `path`, `stem`, `albums`, and `excludedOnly`.
`albums` is titles only.
`excludedOnly` is how JSON tells an empty claim from an excluded-only claim.
`Pictures.plan` omits a stem that has only `toml`, `thm`, or `tif`, so both `agent stems` and `claims` omit it, and a
day of only `2412.tif` is `present: true` and `"claims": []`.
When the day directory is absent, `present` is false and `claims` is empty.
The JSON has no album id, item key, or `google-media-item-id`.

### photo agent catalog

`photo agent catalog` lists every catalog album, sorted by title.
The command does not write.
A line is `AgentCommands.catalogLine`: the quoted title, `count=<count> items=<items>`, then `owned` or
`owner="<label>"`, and, when set, `auto-add`, `error got=<got> of=<of> name-failures=<n>`, `unset=<n>`, and `ambiguous`.
The line does not include the album id.
JSON has `albums`.
Each album has `id`, `title`, `count`, `owned`, `owner`, `autoAdd`, `error`, `unset`, `items`, and `ambiguous`.
`owner` and `error` may be null.
When `error` is an object it has `got`, `of`, and `nameFailures`.
Keep the album id out of chat.

### photo agent items

`photo agent items [--id <id>] <album>` lists that album's item and unset rows.
The album is an exact catalog title.
The command does not write.
An auto-add album prints `auto-add`.
Otherwise a line is `<filename> <date> <uploader>`, and an unset line is `unset <date> <uploader>`.
On the line, `date` and `uploader` are `-` when the catalog has no value.
JSON has `id`, `error`, `items`, and `unsets`.
Each item has `filename`, `date`, `key`, and `uploader`.
Each unset has `date`, `key`, and `uploader`.
In JSON, `date` and `uploader` are null when the catalog has no value.
`error` is the same nullable object as catalog.
Keep the album id and the item key out of chat.

### photo agent match

`photo agent match [--id <id>] <album>` matches that album's items to local stems.
The command does not write.
A line is `matched <n> of <m>`, then one `<day> <filename> <stem>` line per hit, then one `unmatched <day> <filename>`
line per miss, and that day is `-` when the catalog date is `-`.
A date mismatch in that non-hit section is `date-mismatch <catalog-day> <filename> <archive-day> <stem>`.
An auto-add album prints `skipped auto-add`.
An album with unread names prints `skipped unread names`.
JSON has `id`, `matched`, `unmatched`, and `dateMismatches`.
Each matched element has `day` and `stem`.
`unmatched` is an array of strings.
Each `dateMismatches` element has `catalogDay`, `filename`, `archiveDay`, and `stem`.
Keep the album id and every unmatched string out of chat.

A stem found only on the previous or next civil day is a date mismatch, not a match and not a missing item.

### photo agent locate

`photo agent locate <day> <stem>` lists catalog rows that match one local stem.
The command does not write.
The stem is the grouped stem `photo agent stems` lists.
The catalog filename uses the prefix rule in Match.
A date of `-` is not a row, and `photo agent missing` lists those items.
A line is the album title, the date `YYYY-MM-DD`, the filename, and the uploader.
The title is not quoted.
The line has no album id and no item key.
More than one row is `ambiguous: true` with every row present.
JSON has `day`, `stem`, `directory`, `present`, `ambiguous`, and `matches`.
Each match has `title`, `date`, `filename`, `uploader`, `id`, and `key`.
When the day directory is absent, `directory` and `present` are false and `matches` is empty.
When the stem is not in that day's grouped stems, `directory` is true, `present` is false, and `matches` is empty.
Keep the album id and the item key out of chat.

### photo agent include

`photo agent include [--span <span>] <year>/<month>/<day>/<stem> …` prints compressed include lines.
The command does not write.
The human line is `Include.render`.
JSON has `rendered` and `logical`, each an array of strings.
The JSON has no album id, item key, or `google-media-item-id`.

### photo agent album-file

`photo agent album-file [--id <id>] [--dry-run] [--replace] [--google-title <exact>] <album>` writes one album file.
A create is external and omits the key.
`--dry-run` prints that file and writes nothing.
`--replace` overwrites an existing file.
When the album has no owner, it keeps the Google id and published title.
`--replace` that keeps an id writes `external = false` with that id.
`--replace` that does not keep an id writes an external file, including a replace of a pending client file.
`--replace` of an owner still clears the id and writes an external file.
`--google-title` stores that exact string.
When the album has no owner and `--google-title` is absent, the command prints `rename yes` when the catalog title
differs from the path title, and `rename no` when those titles are the same.
An album with an owner prints `rename no`.
The command does not publish, sync, or run `repair --names`.

### photo agent rename-album

`photo agent rename-album` changes one album title.
`--current` equals the catalog title exactly.
`--new` is non-empty and differs from `--current`.

### photo agent set-description

`photo agent set-description` changes one item description.
`--current` equals that filename.
`--new` may be empty.
An empty `--new` is two argv elements on the serve socket, the flag and then an empty string.
MCP sends that empty string.
`photo shell` sends it when the line contains `""`.
`Cli.splitLine` groups with double quotes and does not treat a single quote as syntax, so `''` in `photo shell` is not
an empty token.
A one-shot `photo` argument cannot be empty.
`bin/photo` exits 1 before Gradle when any argument is empty, because Plexus would drop `''`.
There is no second launcher for that case.
A tab or a newline in `--new` is rejected.

### photo agent set-date

`photo agent set-date` changes one item's date taken.
`--current` equals that filename.
`--new` is `YYYY-MM-DD HH:MM` local.

### photo agent download-item

`photo agent download-item` downloads one original and does not change Google Photos.
`--title` names the album.
`--id` is required when more than one album has that title, and it must match that title.
`--current` equals that filename.

### photo agent add-to-album

`photo agent add-to-album` adds one item to an existing album.
`--title` names the source album and `--dest` names the destination album.
`--id` is required when more than one album has the source title, and it must match.
`--dest-id` is required when more than one album has the destination title, and it must match.
The source item is that album's one `item` line whose key is `--item` and whose filename equals `--current`.

### photo agent remove-from-album

`photo agent remove-from-album` removes one item from an album.
`--title` names the album.
`--id` is required when more than one album has that title, and it must match.
The item is that album's one `item` line whose key is `--item` and whose filename equals `--current`.

### photo agent move-item

`photo agent move-item` adds one item to an existing album and then removes it from the source album, in one session.
`--title` names the source album and `--dest` names the destination album.
`--id` is required when more than one album has the source title, and it must match.
`--dest-id` is required when more than one album has the destination title, and it must match.
The source item is that album's one `item` line whose key is `--item` and whose filename equals `--current`.
Success names the action and both titles.
Stdout has no album id and no item key.

### photo agent delete-item

`photo agent delete-item` moves one library item to Google Photos trash.
`--title` names the album.
`--id` is required when more than one album has that title, and it must match.
The item is that album's one `item` line whose key is `--item` and whose filename equals `--current`.

### photo agent upload-item

`photo agent upload-item` uploads one archive file into an existing album.
The positional is `path`.
`--dest` names the destination album.
`--dest-id` is required when more than one album has that title, and it must match.

### photo agent create-album

`photo agent create-album` creates an empty album.
`--title` is the only flag.
A missing `--title` is usage.

### photo agent delete-album

`photo agent delete-album` deletes an album and leaves its photos and videos in the library.
`--title` names the album.
`--id` is required when more than one album has that title, and it must match.
The command is the confirmation.
There is no confirmation file and no `--yes`.

### photo agent drift

`photo agent drift` reports reconcile drift.
The command does not write.
The human line is `Reconcile.line`.
JSON has `actions`.
Each action has `class`, `title`, `path`, `reason`, and `id`.
`reason` and `id` may be null, and both keys are present.
Keep the album id out of chat.

`photo agent drift` reports create, update, gone, pending, pending-rename, and skipped.
It uses the classes reconcile already computes and writes nothing.
A line is the class, then the album title or the relative path.
A skipped line adds a short reason.
The line does not include a catalog id.

### photo agent missing

`photo agent missing` lists every catalog item whose filename stem is absent from that civil day's folder.
A stem on the previous or next civil day is not a missing line, and a stem found only further away still is.
The command does not write.
A line is `missing <title> <date> <filename> <uploader>`.
The title is not quoted.
`date` and `uploader` are the string `-` when the catalog has no value.
The line does not include a catalog id or an item key.
JSON has `missing`.
Each element has `title`, `date`, `filename`, `uploader`, and `key`.
Keep the item key out of chat.
The lookup is the one `photo agent match` uses: an exact stem, then one leading `MVIMG_`, `PXL_`, `IMG_`, `VID_`, or
`MVI_` prefix when that same day's folder contains the remainder.
A `.MP` or `.PORTRAIT` tail stays in the stem.
Extension case is ignored.
Stem case stays exact.
The trailing `v` on a video that shares a stem with a still is the rule match already applies.
Unset lines and auto-add albums are skipped.
An item date of `-` is listed with that date and is not looked up in a folder.
Other people's items are listed with the uploader.

### photo serve

`photo serve` listens on one Unix socket for this user.
There is no TCP port.
The socket is `$XDG_RUNTIME_DIR/org.podval.tools.photo/serve.sock`.
The directory is mode `0700` and the socket is mode `0600`.
`serve.lock` in that directory is an exclusive lock held until the process exits.
A second `photo serve` prints `already running` and exits 0.
It does not unlink the socket.
A stale socket whose lock is free is unlinked by the next `photo serve` after it takes the lock.
The process ignores standard input.
Each connection gets one accept line, then one command, then the connection closes.
The request has `argv`, an array of strings, and an optional string `id` that the response echoes.
Every response includes `archive`, the absolute path of the archive this process loaded.
The accept line is that response before the request is read.
Flags precede positionals in `argv`.
`text` is the human output.
`value` is the command's JSON value, or `null`.
A failure puts the usage line in `error` and does not write a second line to stderr.
`cd`, `pwd`, `exit`, `photo refresh`, `photo reconcile`, `photo file`, `photo card`, `photo push`, `photo web`,
`photo agent rename-album`, `photo agent set-description`, `photo agent set-date`, `photo agent download-item`,
`photo agent add-to-album`, `photo agent remove-from-album`, `photo agent move-item`, `photo agent delete-item`,
`photo agent upload-item`, `photo agent create-album`, and `photo agent delete-album` are usage on the socket.
`photo serve stop` asks that process to exit between commands.
It does not preempt a command already running.

### photo mcp

`photo mcp` speaks MCP on standard input and output.
It is a client of `photo serve`, the same way `photo shell` is.
It starts a detached `photo serve` when the socket is down, and it does not stop that process on exit.
It does not run a command in this process when the socket is down.
A mismatch names both paths, says to run `photo serve stop`, and does not run the command in this process.
Tools are `agent stems`, `agent years`, `agent months`, `agent days`, `agent claims`, `agent catalog`, `agent items`,
`agent match`, `agent locate`, `agent include`, `agent rename-album`, `agent set-description`, `agent set-date`,
`agent download-item`, `agent add-to-album`, `agent remove-from-album`, `agent move-item`, `agent delete-item`,
`agent upload-item`, `agent create-album`, `agent delete-album`, `agent drift`, and `agent missing`.
Local archive writes are `item set` (`item-set`), `album new` (`album-new`), `album rm` (`album-rm`), `album mv` (`album-mv`),
`album include` (`album-include`), `album exclude` (`album-exclude`), `item month` (`item-month`), `item into` (`item-into`),
and `album merge` (`album-merge`).
Each sets `destructiveHint` and keeps `--dry-run`.
The host confirms.
This program adds no confirmation file and no second path flag.
Local archive reads are `album ls` (`album-ls`) and `album show` (`album-show`).
Each sets `readOnlyHint` to true and `openWorldHint` to false.
`cd`, `pwd`, `exit`, `serve`, and `shell` are not tools.
`agent album-file`, `ls`, `item show`, `album claims`, `repair`, `publish`, `sync`, `refresh`, `reconcile`, `file`,
`card`, and `web` are not tools.
Tool JSON may contain album ids and item keys.
Chat should not.
The user adds the server.
This program does not write the Grok config.

```toml
[mcp_servers.photo]
command = "photo"
args = [ "mcp" ]
enabled = true
```

Another computer runs `photo mcp` over SSH, and that stdio stream is MCP: the archive host is the machine with the
files, and there is no second transport.

`command` is whatever executable already runs `photo serve`.
The supported install is the `photo` symlink to `bin/photo`.
The foreground process for `photo mcp` is `gradlew --project-dir` this checkout, then `run`.
The caller's cwd is not the project selector.
`photo shell` is the exception.
`bin/photo` runs the `photoRuntime` task, then `exec`s that java with the terminal inherited and
`--enable-native-access=ALL-UNNAMED`, so tab completion runs.
Do not point `photo mcp` at that java line.
The detached process `Serve.spawnServe` starts is `setsid java -cp` the Gradle JVM's class path, then
`org.podval.tools.photo.Cli serve`.
That child is not `photo` and not `gradlew`.
The Gradle main class is `org.podval.tools.photo.Cli`.
If `command` is `java`, the args start with `mcp` after the class name.
Grok's default startup timeout of 30 seconds covers `Serve.ensure` plus the 5 second accept-line probe.
It does not cover a cold `gradlew run`.
The first `photo mcp` after the Gradle daemon has stopped can exceed that budget.
Later runs use the daemon.
`photo mcp` has no second launcher.
A catalog or item listing can exceed Grok's 20_000 byte inline cap.
Raising `[mcp] max_output_bytes` is a user config change, not a default this program sets.

Formats are `docs/files.md`.
Clicks are `docs/photos-ui.md`.
Opening either means the task is a change to the program.
