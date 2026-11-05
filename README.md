# Google Photo Synchronization

Manage the local photo archive and publish its albums to Google Photos.

| Reader | File | Stops before |
| --- | --- | --- |
| A person installing and operating the archive | `README.md`, then `docs/archive.md`, `docs/clock.md`, `docs/commands.md`, `docs/web.md` | Catalog grammar, RPC names, control names |
| An agent calling `photo` | `docs/agent.md` | Every other file |
| An agent changing this program | The one file the task names: `docs/files.md` or `docs/photos-ui.md` | The other one, and `docs/agent.md` |

## Install

The supported system is Linux.
The program starts Chrome at `/usr/bin/google-chrome-stable` (`PlaywrightPhotosUi`), and `photo serve` listens on a Unix
socket.
The socket is `$XDG_RUNTIME_DIR/org.podval.tools.photo/serve.sock`.

The install is a checkout of this repository.
The program is the Gradle `application` whose main class is `org.podval.tools.photo.Cli`, run with `./gradlew run`.
A package would hide the tree an agent edits.

David Heinemeier Hansson, *The malleable computer*, 15 April 2026,
<https://world.hey.com/dhh/the-malleable-computer-7c187a9b>: "Open source promised that users would be free to change
whatever code they were running.
The reality, however, is that hardly any of them ever did — it was simply too hard.
Now, with AI, it suddenly isn't."
When it breaks, the agent fixes it in this checkout; when something is missing, the agent adds it here.

The archive is `originals/YYYY/MM/DD` under `rootPath`.
Relaying some other tree into that shape is work for the owner and their agent.
The contract is `docs/archive.md`.

```
originals/
  2010/
    07/
      04/
        to-18 Cape Cod.toml
      19/
        1234.jpg
  Best.toml
```

The configuration file is `~/.config/org.podval.tools.photo.toml`.
The keys are `rootPath` and `zoneId`.
The defaults are `Pictures/originals` and `America/New_York`.
`rootPath` is relative to `$HOME`.
`rootPath` must be the archive.
`byYearDirectoryName` and `uploadDirectoryName` must not be set.
The first run writes the defaults.
Later edits, including comments, stay, because the program does not rewrite the file.
There is no configuration command.

The Desktop client JSON is `~/.config/org.podval.tools.photo.client.json`.
The token is `~/.config/org.podval.tools.photo.token.json`.

| Tool | Why |
| --- | --- |
| A JDK, via `./gradlew run` | The program |
| `/usr/bin/google-chrome-stable` | Refresh, push, and the in-process agent mutations |
| `ffprobe` | Video clocks |
| `dcraw` and ImageMagick `convert` | CRW, and `repair --raw` |
| `/usr/bin/vendor_perl/exiftool` | The timestamp written at publish |
| `lsblk` and `udisksctl` | `photo card` |

The archive root gets the `AGENTS.md` reproduced below.

```
# Originals

This directory is the local photo archive.

`.gphoto-sync/` is data for the program and the agents.
It is not a second checkout.

To use the program, read `/home/dub/Podval/gphoto-sync/docs/agent.md` and stop.
To edit an album file by hand, read the archive page of that checkout, `docs/archive.md`.
A change to the program reads `docs/files.md` or `docs/photos-ui.md` for the one operation being changed.
```

`photo status` is a login report.
It does not print the configuration path.
The transcript strings are on `docs/commands.md`.

## Google Photos API

The Library API, since 31 March 2025, can create albums and upload, and can list or edit only content this OAuth client
created.
It cannot list or rename albums I made by hand, and it cannot delete media.

- https://issuetracker.google.com/issues/368779600?pli=1

- [RClone Google Photo documentation](https://rclone.org/googlephotos/)
- [RClone Google Photo OAuth documentation](https://rclone.org/drive/#making-your-own-client-id)
- [RClone Download Proxy](https://github.com/rclone/gphotosdl)
- [Google Photos API](https://developers.google.com/photos/library/guides/get-started-library)
- [Google Photo Client Library](https://developers.google.com/photos/overview/get-started-java)
- [NEW Picker API](https://developers.google.com/photos/picker/guides/get-started-picker)

History is [docs/history.md](docs/history.md).
