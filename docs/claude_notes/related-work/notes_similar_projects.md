# Similar projects: is there anything like this one?

Is there anything like ocaml-elm-playground as a whole: a beautiful
beginner API, 2D and 3D, physics, audio, many tiny games? No single
project I know of does all of it. Several do parts, and comparing them
shows what's unusual here.

This is a different kind of related work from the other notes: not
where each layer's ideas and algorithms come from, but which projects
are this project's peers, and where it stands among them. For the
lineage of each layer, against its own field:
[`notes_playground_related_work.md`](notes_playground_related_work.md)
(2D), [`notes_playground3d_related_work.md`](notes_playground3d_related_work.md)
(3D), [`notes_physics_related_work.md`](notes_physics_related_work.md),
[`notes_audio_related_work.md`](notes_audio_related_work.md).

(From memory, as of 2026: to check against each project's sources
before citing it.)

## The closest projects, and what each lacks

| Project | What it shares | What it lacks |
|---|---|---|
| Evan Czaplicki's elm-playground | The API this project started from: pictures, animations, games, functional Model-View-Update | 2D only; no 3D, physics or sound; few games |
| Racket's 2htdp/universe (*How to Design Programs*) | Functional games for teaching (`big-bang`: a world, a draw function) | Simple 2D; no from-scratch engines underneath |
| raylib (C) | The breadth: 2D, 3D, audio, about 150 small examples, a set of little games | Imperative C over OpenGL and a sound library; it teaches how to use them, not how they work |
| PICO-8, TIC-80 (fantasy consoles) | Tiny games, built-in sound and a music tracker, a community of remade classics | Lua, closed or limited machines, 2D only, nothing explained underneath |
| Pygame Zero, and the *Code the Classics* books | A beginner API, classic games remade with their history | Python on pygame; no 3D, no from-scratch engines |
| Processing / p5.js, with Daniel Shiffman's *The Nature of Code* | Beginner creative coding in 2D and 3D; the closest teaching of physics (forces, springs, particles) | The physics is taught in a book, not built as an engine that games use; sound is a library |
| Casey Muratori's Handmade Hero | Everything from scratch, for teaching: a software renderer, audio mixing, the game loop | One game, in C, as a long video series; no beginner API |
| *Computer Graphics from Scratch* (Gambetta), tinyrenderer (ssloy), Box2D Lite (Catto) | The from-scratch explanation of one layer each (rasterizers; a physics engine) | One layer each; no games around them |

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
   2D and 3D), each with its history and a header saying which
   components it uses.

Most projects choose between "easy to use" (raylib, PICO-8, Pygame
Zero, elm-playground) and "shows how it works" (Handmade Hero, the
from-scratch books). This one does both in the same codebase -- and in
OCaml, where I know of nothing comparable.
