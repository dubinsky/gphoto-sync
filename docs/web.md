### photo web

`photo web` binds `127.0.0.1:8765`, edits local include lists, reads `catalog.txt`, and does not publish, sync, refresh,
push, or copy a card.
The default of `--port` is 8765.
`Cli.webPort` uses `Web.defaultPort` when `--port` is absent.
A non-integer, `0`, or a value outside 1..65535 is `port must be 1 to 65535`.
The toggle splices one include list under the album write lock.
Create writes one span album.
The command runs in the calling process.
It is not an MCP tool.
