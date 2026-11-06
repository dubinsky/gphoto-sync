# Google Photo Synchronization

Manage the local photo archive and publish its albums to Google Photos.

History is [docs/history.md](docs/history.md).


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

`photo` on `PATH` is the symlink `~/.local/bin/photo` to `bin/photo` in this checkout.
For every command except `shell`, the script `exec`s this checkout's `gradlew` with
`--quiet --console=plain --project-dir` set to this checkout, then `run`, and one attached `--args=` value.
The script does not `cd`.
Executing this checkout's `gradlew` does not select this checkout.
`--project-dir` does.
The caller's cwd stays unchanged.
The script does not call `./gradlew run "$@"`.
Gradle would consume those flags.
A copy of the script goes stale.
The symlink is the install.
The program does not create the symlink.
`photo shell` needs the terminal.
`gradlew run` forks the program on pipes, so that process has no console and its stdin is already closed.
The script runs the `photoRuntime` task, reads `build/photo-runtime.txt`, and `exec`s that `java` on
`org.podval.tools.photo.Cli` with the terminal inherited and `--enable-native-access=ALL-UNNAMED`.
Tab completion runs in an interactive terminal.
A pipe stays line at a time.
`photo serve` in the foreground is the Gradle `run`.
The detached server `Serve.spawnServe` starts is `setsid java -cp` the Gradle JVM classpath, then
`org.podval.tools.photo.Cli serve`.
The wrapper is not that child.
Do not change `spawnServe` to call `photo`.
A cold `gradlew run` can exceed Grok's 30 second MCP startup budget.
Later runs use the Gradle daemon.
`./gradlew run` remains the program for every command except `shell`.
The symlink is how `photo` spells that.

The agent is in the checkout.
It does all of the following and then stops.
It does not refresh, sync, publish, or edit pictures.

1. Read repository `AGENTS.md`.
   The task is to install, so follow that branch and read the Install section.
   Do not treat the rest of the README as the operating manual.
   An operating task reads `docs/agent.md` and stops.
2. From the checkout root, create the link.
   `$(pwd -P)` must be this checkout:

```sh
mkdir -p "$HOME/.local/bin"
ln -sfn "$(pwd -P)/bin/photo" "$HOME/.local/bin/photo"
```

3. Confirm `readlink -f "$HOME/.local/bin/photo"` is `"$(pwd -P)/bin/photo"`.
   Confirm `command -v photo` prints `"$HOME/.local/bin/photo"`.
   If it prints anything else, stop and report that path.
   Do not delete the other file.
   Do not edit shell startup files.
   On this machine `~/.local/bin` is already on `PATH`.
   If it is not, stop and say so.
4. Do not run `photo`.
   Do not run `./gradlew`.
5. Resolve the archive root without starting the program.
   If `~/.config/org.podval.tools.photo.toml` exists, read the TOML string `rootPath` and do not rewrite the file.
   The archive root is `"$HOME/$rootPath"`, which is what `new File(HOME, rootPath)` does in `Configuration.archive`,
   including the JDK join of a leading slash.
   If the file does not exist, the archive root is `$HOME/Pictures/originals` and the agent does not create the toml.
   If the file exists but `rootPath` is missing or not a string, stop.
   Do not read `catalog.txt`.
6. If `<archive-root>/AGENTS.md` exists, leave it and report its first `docs/agent.md` path.
   If it does not, `mkdir -p` the archive root only, substitute `/PATH/TO/THIS/CHECKOUT` with `"$(pwd -P)"`, and write
   the template from the README.
   Do not write any other file into the archive.

Paste this prompt:

```text
Install gphoto-sync from this checkout so `photo` is on PATH.
Read AGENTS.md and follow it.
Do not refresh, sync, publish, or edit pictures.
```

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

Write the archive-root `AGENTS.md` only when that file is absent.
Substitute `/PATH/TO/THIS/CHECKOUT` in the template below.
That path is the checkout of this program, not the archive.
`rootPath` is relative to `$HOME`.
A missing config file means `$HOME/Pictures/originals`.
The install does not create the config file.

```
# Originals

This directory is the local photo archive.

`.gphoto-sync/` is data for the program and the agents.
It is not a second checkout.

To use the program, read `/PATH/TO/THIS/CHECKOUT/docs/agent.md` and stop.
To edit an album file by hand, read the archive page of that checkout, `docs/archive.md`.
A change to the program reads `docs/files.md` or `docs/photos-ui.md` for the one operation being changed.
```

`photo status` is a login report.
It does not print the configuration path.
The transcript strings are on `docs/commands.md`.
