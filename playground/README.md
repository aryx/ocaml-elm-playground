# playground/: the Playground, and everything a program uses through it

The OCaml port of Evan Czaplicki's elm-playground: pictures, animations
and games as `view` and `update` over a model, the same source running
natively and in a browser. Two libraries live here, the 2D playground
(`elm_playground`) and the 3D one (`elm_playground_3d`), each with a
*virtual* module, its platform, which each backend implements.

## The folders

    playground/
      Playground  Playground_platform      the API, and its virtual backend
      Playground3d  Playground3d_platform
      Gpu_scene                             the GPU backends' shared scene
      apis/     the Playground's face of each from-scratch library
      layers/   the structures a game is built from
      ways/     borrowed ways of programming, each building the app for you
      native/ native_common/ software/ web/ svg/     the backends
      tests/

| folder | what | modules |
|---|---|---|
| `playground/` itself | the API: shapes, colors, the `computer` (mouse, keyboard, screen, time), `picture`, `animation`, `game`; its 3D twin; the backends' virtual module | `Playground`, `Playground_platform`, `Playground3d`, `Playground3d_platform`, `Gpu_scene` |
| `apis/` | each a small Evan-style API over one of the repository's from-scratch libraries, the only place a game meets them | `Audio`, `Audio3d`, `Audio_debug` (over `audio/`); `Physics`, `Physics3d` (over `physics/`); `Ai`, `Ai_debug` (over `ai/`); `Gui` (over `gui/`); `Juice`, `Juice3d` (over `juice/`); `Multiplayer`, `Multiplayer3d` (over `networking/`) |
| `layers/` | what games keep writing, written once: a camera, sprites, a tile map, the scenes of a game (title, play, game over); in 3D a camera that follows, a character controller, a ragdoll, portals | `Camera2d`, `Sprite`, `Tilemap`, `Scene2d`; `Camera3d`, `Character3d`, `Ragdoll3d`, `Portal3d` |
| `ways/` | other people's ways of programming, each of which builds the Playground `app` for you, so a program written on one has no `update` and no `view` of its own | `Logo` and `Logo3d` (the turtle), `Bigbang` (HtDP's world programs), `Universe` (its worlds with a mailbox), `Puzzlescript` (a game as a map and rewrite rules), `Karel` (Pattis's robot) |
| `native/`, `native_common/`, `software/`, `web/`, `svg/` | the backends: SDL and Cairo or OpenGL, our own software rasterizer, the browser (vdom, WebGL), the 3D scene drawn as SVG | a `Playground_platform` and/or `Playground3d_platform` each |

What decides the folder is *what a module is to a game*: `apis/` are
services (sound, physics, AI, widgets, feel, the network), `layers/` are
parts of the game itself, `ways/` replace the game's `update` and
`view`. The 2D and 3D versions sit side by side (`Juice`, `Juice3d`), as
in the rest of the repository; the 3D ones belong to the 3D library,
so a 2D program needs no OpenGL.

`Character3d`, `Ragdoll3d` and `Portal3d` are built on `Physics3d` but
live in `layers/`: a game is made of them, where it only calls
`Physics3d`. `Audio` and `Audio_debug` are in `apis/` though the
backends use them too (they play and show the sound): a folder says
what a module is, not who uses it.

## How: folders, not libraries

The folders don't change the libraries. `playground/dune` says
`(include_subdirs unqualified)`: the modules of `apis/`, `layers/` and
`ways/` belong to its two stanzas as if they were in `playground/`
itself, the 3D stanza listing its modules by name wherever they sit (a
3D module missing from the list lands in the 2D library and breaks its
build, so the list can't drift). So a game's `(libraries
elm_playground)` still gives it `Sprite` and `Physics`. The backends'
and tests' folders define their own stanzas, so they say
`(include_subdirs no)`.

A new module goes in `apis/` if it's a new library's face, `layers/`
if games would otherwise each rewrite it, `ways/` if it builds the
`app`; the root is for the API itself. Add it to the 3D stanza's list
if it's 3D.

## Tests

`playground/tests/` (`make test`): the layers', the ways' and the apis'
worked examples, without a screen; the backends are tested by the
golden frames (`tests/2d/`, `tests/3d/`), every example rendered offscreen
and compared pixel by pixel.
