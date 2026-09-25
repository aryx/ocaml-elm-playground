# networking/: networking, from scratch, for teaching

One idea per module, each `.mli` with its diagram, worked example and
references. The protocols and the netcode are pure OCaml, no system
call: a message is bytes in, bytes out, so the tests need no network
and the browser runs them too. The tutorial is
`docs/claude_notes/tutorials/notes_networking.md`.

## The folders

    protocols --> netcode --> the Playground's Multiplayer
         \
          '--> unix (the sockets) --> relay/, ircd/ (the servers)

| folder (library) | what | modules |
|---|---|---|
| `protocols/` (`networking_protocols`) | the bytes other programs agree on | `Url` (RFC 3986), `Urlencoded` (a form's fields, both ways), `Http` (HTTP/1.1 messages, both sides), `Websocket` (RFC 6455), `Irc` (RFC 1459), `Wire` (values as bytes: varints, zigzag, garbage refused), `Transport` (where a peer's packets go, whatever carries them) |
| `netcode/` (`networking_netcode`) | the machinery of a multiplayer game | `Checksum` (desyncs), `Sim_net` (a network in one process, from a seed), `Inputs` (the input exchange), `Lockstep`, `Rollback`, `Snapshot`, `Prediction`, `Interpolation` (client-server) |
| `unix/` (`networking_unix`, native only) | the sockets | `Tcp`, `Udp`, `Server`, `Relay`, `Relay_client`, `Universe_server`, `Irc_server`, `Http_client`, `Http_request`, `Http_server` (a web server's event loop), `Connect` |
| `relay/`, `ircd/`, `httpd/` | the servers, programs | `relay_server.exe`, `tiny_ircd.exe`, `tiny_httpd.exe` (a directory's files, for TinyMosaic) |

Why this split:

- `protocols/` is what any program talking to another needs, a game or
  not: TinyIRC, the images' `Download`, `Cmd.Http_get`. Its modules
  are other people's standards (the RFCs), except `Wire`, our own
  messages' rules, and `Transport`, the record of functions a platform
  opens (UDP, a relay, `Sim_net`) so that the pure code can use a
  socket without opening one.
- `netcode/` is the games' own: keeping two or more copies of a game
  in step (lockstep, rollback) or one copy on a server and the rest
  guessing (snapshots, prediction, interpolation). It writes its
  messages with `Wire`; nothing in `protocols/` knows of it.
- `unix/` opens the sockets, natively; in a browser the web platform
  does it with its own WebSocket.

A new standard's bytes go in `protocols/`, a new way of keeping games
in step in `netcode/`, anything that makes a system call in `unix/`.

## Using it

`(libraries networking)` gives both pure halves: `networking` is a
library with no module of its own that depends on the two. The
modules are unwrapped: `Rollback`, not `Networking.Rollback`.

## Tests

`networking/tests/` (`make test`): the `.mli`s' worked examples (the
RFCs' own examples where they give some), and the netcode through
`Sim_net`'s bad networks; `unix/tests/` over localhost.
