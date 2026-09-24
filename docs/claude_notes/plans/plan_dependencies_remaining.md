# Plan: what we still borrow, and what we already own

## Context

This project's teaching libraries replace, one by one, the libraries a
game would normally borrow: stb_image and imagelib went when
`graphics/images/` got its own PNG, GIF and JPEG readers
([`done/plan_images_teaching.md`](done/plan_images_teaching.md)), and
OpenGL and Cairo have from-scratch twins in `graphics/3d` and
`graphics/2d` ([`done/plan_software_2d.md`](done/plan_software_2d.md),
[`done/plan_opengl.md`](done/plan_opengl.md)). This document is the
inventory: every external library still linked, what it still does for
us, and whether writing our own would teach something. It is a map of
what the repository owns, as much as a plan, and it points to the
plans that already cover a piece rather than repeating them.

The rule of thumb for what stays borrowed: **the operating system's
boundary** (a window, a sound card, a socket, the clock) is not an
algorithm, and replacing it teaches the OS's API, not an idea. Everything
above that boundary -- decoding, drawing, synthesizing, speaking a
protocol -- is fair game.

The inventory was made on 2026-09-23 from the dune files'
`(libraries ...)` and the backends' code; redo it (`grep -r
libraries --include=dune`) when this document looks stale.

## 1. The inventory

### The native software backends (`playground/platforms/software/`, `native_common/`)

The path that should end up owning everything above the OS.

| borrowed | what it still does for us | where | ours one day? |
|---|---|---|---|
| **curl** (ocurl) | downloads an image, texture or sound given as a URL | `Download.ml` (for `Image_decode`, `Texture_decode`, and the two native `Playground_platform`s' audio fetcher) | **yes, next**: section 2 |
| **SDL2** (tsdl) | the window and blitting our framebuffer to it (`get_window_surface`, `update_window_surface`); keyboard, mouse and text-input events; the sound card (`open_audio_device`, `queue_audio`: our mixer's samples, SDL only plays them) | `Native_loop_2d.ml`, `Native_loop_3d.ml` | no: the OS boundary. A "TinyX11" client (the X protocol over a Unix socket) would be the one teaching version, Linux only, much later |
| **logs** | logging | `native_common`, `graphics/images` | not worth it: no idea in it |
| **unix** | `gettimeofday`, and tomorrow the sockets | `native_common`, a few old games (`Pong`, `Asteroid`, `Tetris`) | no: the OCaml runtime's own boundary |

### The native Cairo and OpenGL backends (`playground/platforms/native/`)

Borrowed on purpose: they are the "real library" our rasterizers are
compared against, and the fast path. What they still do that the
software path does not is the interesting part:

- **Cairo** (cairo2): 2D shapes, where `graphics/2d` does the same --
  except **text**. Cairo draws `words` with the system's outline fonts
  (`select_font_face`, fontconfig and FreeType under it); our software
  path has only Hershey's stroke fonts. The gap is a **TrueType reader
  and glyph rasterizer**, already planned in
  [`plan_2d_remaining.md`](plan_2d_remaining.md) ("A TrueType outline
  font": quadratic Béziers flattened, filled by `Fill`). Cairo is
  also the reference in `graphics/tests/Unit_circle.ml`
  (`test_close_to_cairo`), which is the right use of a borrowed
  library: an oracle.
- **Str**: only `Shape_render_native.ml`'s `#rrggbb` color parsing,
  three regexps for what a few `String.sub`s and `int_of_string
  ("0x" ^ ...)` do. An easy removal, whenever that file is touched.
- **OpenGL** (tgls, ctypes-foreign, and `threads.posix` only so that
  those link): replaced by `graphics/3d` on the software path; the GPU
  runs the shaders, and that stays so.

### The web backends (`playground/platforms/web/`, `playground/platforms/svg/`)

`js_of_ocaml` and `vdom`, and through them the browser: the DOM and
SVG renderer, WebGL, WebAudio (`AudioContext`), XMLHttpRequest for the
audio fetcher, and `<img>`, which means **the browser decodes the
pictures, not our PNG/GIF/JPEG readers** (they do run in JavaScript,
`graphics/tests/js/`, but only the native backends use them). The
browser is the web's OS boundary; this stays borrowed.

### Below every backend: libm

*(Added 2026-09-23, missed by the first inventory because no dune file
names it.)* Every `sin`, `cos`, `sqrt`, `exp` of every backend,
natively, is the C library's `libm` (and in a browser, the JavaScript
engine's `Math`). Not a dependency to drop -- it is fast and right --
but the most used code nobody here has read, and a teaching subject:
`math/libm/`, [`plan_teaching_other.md`](plan_teaching_other.md)
section 4b.

### Tests and tools

`alcotest` and `testo`: borrowed, as a compiler is.

## 2. Our own HTTP client, and curl gone (`networking/`)

**Status (2026-09-23): option 1 below is done.** `networking/` has
`Url` (RFC 3986: parsing checked against appendix B's regexp, and
resolution against all 42 examples of section 5.4) and `Http` (the
request, the response, the four ways a body ends, chunked);
`networking/unix/` has `Tcp` and `Http_client` (redirections followed,
tested against a server forked on localhost); `Download` uses it for
`http://`, and curl only for `https://`. What follows is the plan as
written before.

The piece that starts the networking teaching
([`plan_networking_teaching.md`](plan_networking_teaching.md)) on the
simplest protocol of all, before the games' lockstep and rollback: a
**request, and its answer**.

What curl does for us today is one blocking GET of a file
(`Download.local_file`). An HTTP/1.1 client doing the same is small and
teaches the layering every protocol since has kept:

- **A URL** parsed: scheme, host, port, path (RFC 3986's grammar, the
  part we need).
- **A name resolved** to an address (`Unix.getaddrinfo`: DNS stays the
  OS's; a DNS query written by hand, UDP and RFC 1035's message format,
  is an optional later module, since it is a nice small binary
  protocol).
- **A TCP connection** (`Unix.socket`, `connect`): the OS boundary,
  in `networking/unix/` (native only), behind a capability
  (`Cap.network`, [`plan_caps.md`](plan_caps.md) phase 3b, `Download`
  included: a URL needs the program's grant).
- **The request and the response**, pure OCaml in `networking/` like the
  rest of that plan's modules (no sockets there): the request line and
  headers written, the status line and headers parsed, the body read
  by `Content-Length`, or by **chunked** transfer coding, or to the
  end of the connection -- the three ways HTTP/1.1 says where a body
  ends, each tested on bytes recorded in the `.mli`'s worked example.
  Redirections (`301`, `302`, `Location`) followed, as curl's
  `set_followlocation` does now; `gzip` refused politely by not
  asking for it (or accepted, since `deflate/`'s `Inflate` is already
  there: `Content-Encoding: gzip` is deflate with a header, a nice
  reuse).
- **Tests without the Internet**: the parser on recorded answers, and
  a whole GET against a tiny server in the test itself, over
  localhost (the relay server the networking plan needs later is the
  same kind of program).

**The catch: HTTPS.** Every URL the repository actually downloads is
`https://` (`examples/Turtle.ml` and `examples/Mario.ml`, from
elm-lang.org), and so is nearly all of today's web. Three ways to go,
to be decided when we get there:

1. **Plain HTTP ours, curl kept for `https://`**: the teaching comes
   first, and curl is no longer needed for anything local or
   `http://`. The least code, but curl doesn't leave.
2. **TLS 1.3 from scratch** (RFC 8446): X25519 for the key exchange,
   ChaCha20-Poly1305 for the records, SHA-256 and HKDF for the keys --
   each a famous, small, well-specified algorithm with test vectors in
   its RFC, which is exactly what this repository likes. Certificate
   checking (X.509, ASN.1, RSA or ECDSA signatures, a trust store) is
   the big part; a first version could skip it and say so loudly
   (encrypted, not authenticated). A plan of its own
   (`plan_crypto_teaching.md`), and a large one; its primitives in
   `crypto/` ([`plan_teaching_other.md`](plan_teaching_other.md)
   section 4b), the handshake in `networking/`.
3. **The images local**: Turtle and Mario's GIFs downloaded once into
   the repository (as the golden-frame plan wants anyway, see
   [`plan_2d_remaining.md`](plan_2d_remaining.md) section 4), and
   HTTPS URLs refused with a message. Removes curl today, but it is
   Evan's art, and a user's `image "https://..."` stops working.

The recommended order: 1 now (the HTTP client, curl only for
`https://`), 2 as its own plan if the appetite is there, which would
then drop curl for good.

## 3. The formats we don't read yet

Not dependencies (nothing borrowed reads them either, since stb_image
left), but the other half of "what we own", each already planned:

- **Images**: progressive JPEG, BMP, PPM/PGM, TGA --
  [`plan_images_remaining.md`](plan_images_remaining.md) section 1.
- **Video**: AVI with Motion JPEG, MPEG-1 --
  [`plan_video_teaching.md`](plan_video_teaching.md); Y4M, FLI/FLC and
  YUV are done.
- **Sound**: AU, IMA ADPCM, FLAC --
  [`plan_audio_formats.md`](plan_audio_formats.md); WAV, MIDI, ABC and
  MOD are done. No MP3 or Vorbis, deliberately so far (both big).
- **Fonts**: TrueType, as above
  ([`plan_2d_remaining.md`](plan_2d_remaining.md)).

## Phasing

1. *(done)* **HTTP/1.1 client** (section 2, option 1): `Url`, `Http`
   in `networking/`; `Tcp`, `Http_client` in `networking/unix/`;
   `Download.ml` over it for `http://`, curl only for `https://`.
   Tests: RFC 3986's examples, recorded answers, a localhost GET.
   Its sequel, a request from a program's `update` as an Elm `Cmd`
   (non-blocking), is phase 0b of
   [`plan_networking_teaching.md`](plan_networking_teaching.md).
2. **Str out** of `Shape_render_native.ml` (any time, a few lines).
3. **TrueType** ([`plan_2d_remaining.md`](plan_2d_remaining.md)): the
   software path's `words` in a real font, Cairo's last advantage gone.
4. *(optional)* **gzip** answers through `Inflate`; a hand-written
   **DNS** query.
5. *(a plan of its own, if wanted)* **TLS 1.3**, and curl removed.
6. Redo the inventory (section 1) and update this document.

## Out of scope

- Replacing SDL, the browser, the OpenGL driver, or `unix`: the OS
  boundary (section 1's rule of thumb).
- HTTP/2 and HTTP/3: binary framing and QUIC teach more about
  performance engineering than about protocols; named in the `Http`
  `.mli` as what the web moved to, and why.
