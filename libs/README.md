# libs/: the from-scratch libraries, independent of the Playground

Everything here is written from scratch, for teaching: one idea per
module, each `.mli` with its diagram, worked example and references, a
tutorial per library in `docs/claude_notes/tutorials/`. None of it
knows the Playground: the rule of this directory is that a module here
speaks pixels, samples, bodies, bytes or rectangles, never shapes and
`computer`s. The Playground (`playground/`, at the top) is built on
them: its `apis/` folder has an Evan-style API over each, the only
place a game meets them.

    libs/  -->  playground/  -->  gamekits/ appkits/  -->  games/ apps/ examples/

| folder | what | tutorial |
|---|---|---|
| `core/` | Elm's core, the small part the Playground's programs need (`Basics`, `Color`, `Set`, `Cmd`, `Sub`, ...), and `Base64` | |
| `random/` | `Lehmer`, the seeded generator under `Playground.random` | |
| `crypto/` | `Sha1` (the WebSocket handshake), and what TLS 1.3 needs: `Sha256`, `Sha512`, `Hmac`, `Hkdf`, `Chacha20`, `Poly1305`, `Chacha20_poly1305`, `Aes`, `Gcm`, `Bignum`, `X25519`, `Ecdsa`, `Rsa` (plan_tls.md) | |
| `compression/` | `Huffman`, `Inflate`/`Deflate`/`Zlib` and their checksums, `Lzw`, and MPEG's `Bits` and `Vlc`: what the image, video and audio formats share | `notes_images.md` |
| `graphics/` | the 2D and 3D software rasterizers, fonts, the image and video formats | `notes_2d.md`, `notes_images.md`, ... |
| `physics/` | 2D and 3D physics engines, collision, gravity | `notes_2d_physics.md`, `notes_3d_physics.md` |
| `audio/` | a software synthesizer, the audio formats, instruments and effects ([`audio/README.md`](audio/README.md)) | `notes_audio.md`, `notes_synth.md` |
| `ai/` | game AI: movement, decisions, bots, search, learning ([`ai/README.md`](ai/README.md)) | `notes_ai.md`, `notes_ai_learning.md` |
| `networking/` | the protocols, the netcode, the sockets ([`networking/README.md`](networking/README.md)) | `notes_networking.md` |
| `gui/` | a GUI toolkit: widgets, the four architectures, layout, text | `notes_gui.md` |
| `program_analysis/` | what analyses programs whatever their language: `highlight/` (`Highlight_code`, the categories and colours a language's highlighter turns tokens into, codemap's); to come, the modules a file uses, a generic AST, dataflow | `plan_tinybox_codemap.md` |
| `juice/` | game feel: easing, tweens, squash, trauma, particles | `notes_juice.md` |
| `terminal/` | the VT100's screen (`Vt`: bytes and escape sequences into a grid of cells), the tty's line discipline (`Line_discipline`: echo, Backspace, a line on Enter), `Talk` (the programs that ask and wait, as values, and the machine playing one on a `Vt`), `Curses` (a screen drawn whole, sent as what changed) and `Tui` (a full-screen program, Model-View-Update); `unix/`'s `Tty_unix` runs one in a real terminal, native only | `plan_terminal.md` |

They are private libraries, each installed as part of one of the opam
packages (its dune file's `(package ...)`), pure OCaml unless the
folder says otherwise (`networking/unix/`: the sockets, native only),
so every backend, the web one included, can use them.

Among themselves they depend little: `core/`, `random/`, `crypto/` and
`compression/` at the bottom; `networking/` on the first three;
`graphics/` on `compression/` (its formats' codes), on `core/`,
on `audio/`'s samples (a video's sound) and on `networking/` (an image
downloaded); `physics/` on `graphics/`' geometry; `gui/` on `core/`;
`audio/` on `compression/` (MP3's codes); `terminal/` on `random/` (`Talk`'s
seeded numbers); `ai/` and `juice/` on nothing of the others.

The languages were here, and moved to the top, to `languages/`, beside
`appkits/` and `gamekits/`: a language is made for a program or two
(Lisp for TinyEmacs, BASIC for TinyBasic), as a kit is, so it is part
of their code, and counts toward their budget (README's "A budget"),
where these libraries, truly general, do not. So did the browsers'
engine, `web/`: HTML and CSS to `languages/html` and `languages/css`,
the layout to `appkits/browser/layout` (its README). dune finds a
library by its name, not its folder, so a program says `(libraries
audio ai)`, whatever `libs/` looks like.
