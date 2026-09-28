# Similar projects: is there anything like this one?

Is there anything like ocaml-elm-playground as a whole: a beautiful
beginner API, 2D and 3D, physics, audio, many tiny games sharing
genre kits, apps, borrowed ways of programming? No single
project I know of does all of it. Several do parts, and comparing them
shows what's unusual here.

This is a different kind of related work from the other notes: not
where each layer's ideas and algorithms come from, but which projects
are this project's peers, and where it stands among them. For the
lineage of each layer, against its own field:
[`notes_playground_related_work.md`](notes_playground_related_work.md)
(2D), [`notes_playground3d_related_work.md`](notes_playground3d_related_work.md)
(3D), [`notes_physics_related_work.md`](notes_physics_related_work.md),
[`notes_audio_related_work.md`](notes_audio_related_work.md),
[`notes_gui_related_work.md`](notes_gui_related_work.md) (the apps).

(From memory, as of 2026: to check against each project's sources
before citing it.)

## The closest projects, and what each lacks

| Project | What it shares | What it lacks |
|---|---|---|
| Evan Czaplicki's elm-playground | The API this project started from: pictures, animations, games, functional Model-View-Update | 2D only; no 3D, physics or sound; few games |
| Haskell's Gloss | The same layering: `display` / `animate` / `simulate` / `play`, pure functions over a picture type | Native only (OpenGL); no games, no engines underneath |
| Chris Smith's CodeWorld (Haskell) | `drawingOf` / `animationOf` / `activityOf`, in the browser, for teaching; probably the closest in spirit to elm-playground | 2D only; no 3D, physics or sound; no game collection |
| GraphicSVG (Elm, McMaster Outreach) | An elm-playground-like shapes library, taught to schoolchildren | Browser and SVG only; 2D; no engines |
| Racket's 2htdp/universe (*How to Design Programs*) | Functional games for teaching (`big-bang`: a world, a draw function) | Simple 2D; no from-scratch engines underneath |
| raylib (C) | The breadth: 2D, 3D, audio, about 150 small examples, a set of little games | Imperative C over OpenGL and a sound library; it teaches how to use them, not how they work |
| PICO-8, TIC-80 (fantasy consoles) | Tiny games, built-in sound and a music tracker, a community of remade classics | Lua, closed or limited machines, 2D only, nothing explained underneath |
| Pygame Zero, and the *Code the Classics* books | A beginner API, classic games remade with their history | Python on pygame; no 3D, no from-scratch engines |
| Processing / p5.js, with Daniel Shiffman's *The Nature of Code* | Beginner creative coding in 2D and 3D; the closest teaching of physics (forces, springs, particles) | The physics is taught in a book, not built as an engine that games use; sound is a library |
| javidx9's "Code-It-Yourself" videos (OneLoneCoder, C++ on olcPixelGameEngine) | Classic games, each rebuilt to show its trick: pseudo-3D racing, Mode 7, the raycaster, Worms, Asteroids, Tetris -- the closest to `games2.5d/` | One video and one file per game; no shared kits, no common API beyond pixels |
| Lou's Pseudo 3d Page, Fabien Sanglard's *Game Engine Black Books* | The trick of one game written out (OutRun's road; Wolfenstein 3D and Doom) | Explanations, not a library the reader's games are built on |
| Casey Muratori's Handmade Hero | Everything from scratch, for teaching: a software renderer, audio mixing, the game loop | One game, in C, as a long video series; no beginner API |
| *Computer Graphics from Scratch* (Gambetta), tinyrenderer (ssloy), Box2D Lite (Catto) | The from-scratch explanation of one layer each (rasterizers; a physics engine) | One layer each; no games around them |

## In OCaml

| Project | What it shares | What it lacks |
|---|---|---|
| OCaml-Canvas (OCamlPro) | One API for native and js_of_ocaml | An imperative canvas; no MVU, no games |
| Vg / Gg (Daniel Bünzli) | Declarative pictures, several renderers (Cairo, SVG, canvas, PDF) | No input, animation or games |
| OCaml Joy (Tarides, Outreachy) | A port of Python's Joy: shapes for beginners | Pictures only |
| raylib-ocaml | raylib's breadth | Bindings to C; native only; imperative |

## Whole systems, small enough to read

The apps (`apps/`, e.g. `TinyOffice`) and the planned tiny OS series
(ix) push past games, towards a whole personal computing system kept
small. The peers there are not game libraries:

- **VPRI's STEPS** (Alan Kay et al., 2006-2012): a whole system, office
  apps included, in about 20,000 lines -- the closest in ambition.
- **Project Oberon** (Wirth, Gutknecht): an OS, a compiler and its
  apps, explained in one book.
- **Smalltalk-80, Squeak, the Lively Kernel**: everything in one
  small, readable, live environment.

## What seems unique here: four things at once

1. **A tiny, functional, Evan-style API** (`Playground`, and on top
   `Camera2d`, `Tilemap`, `Sprite`, `Scene2d`, `Physics`, `Audio`),
   the same program running natively and in a browser.
2. **Every layer under it written from scratch, to be read**: the 2D
   and 3D software rasterizers (`graphics/`), the physics engine
   (`physics/`), the synthesizer (`audio/`), each module's `.mli` with
   its diagram, a worked example and its references, and tests checking
   them.
3. **Determinism, as a teaching and testing tool**: golden frames and
   golden WAVs, scripted runs (`-script`, `-dump-frame`,
   `-dump-audio`), and phases that can be switched off to see what they
   bring (`solver=off`, `rotation=off`, `physics=engine`, the debug
   keys).
4. **Many tiny classic games** built on all of it (the Tiny* toys, in
   2D, 2.5D and 3D), each with its history and a header saying which
   components it uses; the games of a genre share a kit
   (`gamekits/`), and the 2.5D ones mark their original's trick. Around
   them, apps (`apps/`) and borrowed ways of programming built on the
   same API (`Logo`, `Bigbang`, `Puzzlescript`).

Most projects choose between "easy to use" (raylib, PICO-8, Pygame
Zero, elm-playground) and "shows how it works" (Handmade Hero, the
from-scratch books, javidx9). This one does both in the same codebase
-- and in OCaml, where I know of nothing comparable. The nearest whole,
put together, would be CodeWorld's API, javidx9's games and STEPS's
ambition.
