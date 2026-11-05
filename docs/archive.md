# Archive layout

Originals are laid out as `YYYY/MM/DD`.
One album file describes one album.
A picture may be in several albums.
Date grain and an optional name are not types.
The file sits in the start folder, next to the pictures or the day folders.
The title is not the path.
There is no year album.
A `.toml` in a year directory is not an album.
A digit-leading `.toml` in a day folder is a picture sidecar.
A curated album is `originals/<Name>.toml`.
The name is not a 4-digit year, not `album`, and not `to-…`.
`rootPath` in the config file is the archive.
On this machine that is `Pictures/originals`.
The inbox is the child `.upload`.
Publish uploads `jpg`, `jpeg`, and video other than `m4v`.
RAW stays local.
`heic`, `heif`, `avif`, `png`, and `webp` stay local and are not uploaded.
The old sibling `Upload` is gone.
Year, month, and day skip a child whose name starts with `.`.
`AGENTS.md` may sit in the archive root.
The install writes that file when it is absent.
The text is the README template.
The file points at this checkout's `docs/agent.md` and not at the archive.
Any other non-toml file next to child directories is stray.
`.gphoto-sync/` is the agent's workspace.
There is no config key for it.
The agent queries read `catalog.txt` in that directory.
`photo refresh` creates and replaces `catalog.txt`.
`photo file` reads `.download/` in that directory.

The title uses `/` on both sides of a range.
`-` means only "through".

| Title | Meaning |
| --- | --- |
| `YYYY/MM` | that month |
| `YYYY/MM-MM` | start month through a later month of the same year |
| `YYYY/MM-YYYY/MM` | start month through a later year-month |
| `YYYY/MM/DD` | that day |
| `YYYY/MM/DD-DD` | start day through a later day of that month |
| `YYYY/MM/DD-MM/DD` | start day through a later month and day of that year |
| `YYYY/MM/DD-YYYY/MM/DD` | start day through a later civil date |
| any of those plus `/Name` | the same span, with a name |

An equal day canonicalizes.
`2010/07/19-19` is the title `2010/07/19`.
An equal month is rejected.
`2010/07-2010/07` is not an album.
A name is not exactly two digits, does not start with a digit, does not start with `to-`, and is not `album`.

The end date is only in the filename, with the prefix `to-` and hyphens where the title uses slashes.
There is no `to` key.
An unnamed album of exactly that folder is `album.toml`.
A named album of exactly that folder is `Name.toml`.
A span is `to-<end>.toml` or `to-<end> Name.toml`.
In a day folder the end is `DD`, `MM-DD`, or `YYYY-MM-DD`.
In a month folder the end is `MM` or `YYYY-MM`.
`to-02` is day 02 in a day folder and month 02 in a month folder.
`2010/07/19/to-19.toml` is an error.

| File under `originals/` | Title |
| --- | --- |
| `2010/07/album.toml` | `2010/07` |
| `2016/07/Camp Menucha Rochel.toml` | `2016/07/Camp Menucha Rochel` |
| `2005/04/Toronto.toml` | `2005/04/Toronto` |
| `2011/06/12/album.toml` | `2011/06/12` |
| `2011/06/12/Chupa Sonya.toml` | `2011/06/12/Chupa Sonya` |
| `2010/07/04/to-18 Cape Cod.toml` | `2010/07/04-18/Cape Cod` |
| `2010/07/19/to-08-23.toml` | `2010/07/19-08/23` |
| `2010/08/05/to-17 Seattle.toml` | `2010/08/05-17/Seattle` |
| `2010/02/28/to-03-07 Purim Seuda.toml` | `2010/02/28-03/07/Purim Seuda` |
| `2009/07/03/to-09-12.toml` | `2009/07/03-09/12` |
| `2010/07/to-2011-02.toml` | `2010/07-2011/02` |
| `Best.toml` | `Best` |

Membership is the `include` list.
A missing `include` is an empty album.
`include` is required for a non-empty album.
An entry is `all`, a date expression, a date plus one stem, or a date plus `stem..stem`, and it may end with `except`
and stems.
There is no album-level `exclude`.
`all` means every picture whose folder day is in the title span, and it is illegal when the album has no span.
A stem range is inclusive string order.
Both ends have the same width.
An except stem must be one the positive pattern can match.
Quote a stem only when a bare token would be wrong: it contains a space, `"`, `\`, or `..`, or it is the word `except`.
The tools write the bare form otherwise.
The parser accepts unnecessary quotes.
Album membership uses the folder day.
It does not use a filename clock.

Client files start with `external = false`.
External files omit the key.
The remaining hand-written keys, in order, are `google-title`, `owner`, `google-album-id`, `published-title`, and
`include`.
A legacy `external = true` line is not rewritten.
External is a missing key or `external = true`.
A historical album file is external.
The key may be absent.
`external = true` is still external.
Do not pass that file through jtoml.
jtoml rewrites sort keys and drops comments.
`google-title`, when set, is the Google Photos title, stored exactly, including spaces.
Empty means that title is the path title.
`owner`, when set, is the display name of someone else who owns the album.
That file is `external`, has no `google-album-id`, and is not renamed.
`published-title` is the title this client last wrote, not that override.
