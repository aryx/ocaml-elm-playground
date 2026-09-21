# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`ocaml-elm-playground` is an OCaml port of Evan Czaplicki's [elm-playground](https://github.com/evancz/elm-playground): a library for making pictures, animations, and simple video games (using the Elm "Model-View-Update" architecture) that run either natively (SDL/Cairo desktop app) or in the browser (js_of_ocaml + vdom), from the *same* application code.

## Build, test, and doc commands

```bash
make               # = dune build (also regenerates the *.opam files)
make test           # = dune runtest -f
make clean          # = dune clean
dune build @doc     # build odoc API docs (make doc does this too)
dune build games/web --profile=release-js     # build web games with small .js output
dune build examples/web --profile=release-js  # same, for examples
```

Run a single native example/game directly with dune, e.g.:
```bash
dune exec examples/Keyboard.exe
dune exec games/Snake.exe
```

Web (`js/`) targets are executables built in `(modes js)`; after building, the resulting `.bc.js` is copied next to the corresponding `.html` file in `examples/web/`/`games/web/` (see the `website`/`js` Makefile targets and the README's "Simple web application" section for the manual `dune build --root . && cp _build/default/Toy.bc.js static/` pattern used by the two toy example projects under `docs/`).

`make test` also runs the golden frame tests (`tests/2d/`, `tests/3d/`,
see `tests/common/Testutil_golden.mli`): every software-rasterizer
example rendered offscreen (SDL's dummy video driver) and compared pixel
by pixel with `tests/*/golden/*.png`. After an intended pixel change,
look at the new frames in `_build/default/tests/*/actual/`, then
`make approve-golden2d` / `make approve-golden3d`.

`make test` skips the scenes deep into a game (more than 100 frames to
render: each is seconds of CPU, and they run in parallel), keeping every
example's frames and the first frame of each game. `make
test-golden-all` runs those too -- before a release, or after touching a
renderer.

The native software backends' debug keys (rendering toggles, "h" for
help) only work when run with `-debug-keys` (e.g.
`dune exec examples3d/Cubes3d.exe -- -debug-keys`); other flags:
`-uncapped`, `-fixed-time t`, `-keys k`, `-dump-frame n file`,
`-script "right:1-60,space:30"` (game keys held over frames).

`make check` runs the project's semgrep config (`semgrep.jsonnet`) via a local `osemgrep` binary — not generally runnable outside the author's machine.

## Architecture

### Virtual module split: one API, two backends

The whole library hinges on dune's `virtual_modules` mechanism:

- `playground/Playground.mli` + `playground/Playground.ml` (library `elm_playground`) define the **entire public API**: shapes, colors, `computer`/`mouse`/`keyboard`/`screen` records, animation helpers (`spin`/`wave`/`zigzag`), and the three entry points `picture`, `animation`, `game` which all produce a `('model, 'msg) app` value.
- `playground/Playground_platform.mli` declares a single virtual function, `run_app : ('a, 'b) Playground.app -> unit`. The `elm_playground` library declares this as a `virtual_modules Playground_platform` in `playground/dune` — it has no implementation.
- Two separate libraries each provide a concrete `Playground_platform.ml` and `(implements elm_playground)` in their dune file:
  - `playground/native/Playground_platform.ml` → library `elm_playground_native` (SDL2 via `tsdl`, 2D drawing via `cairo2`, images via `imagelib`, HTTP via `curl`, logging via `logs`).
  - `playground/web/Playground_platform.ml` → library `elm_playground_web` (DOM/vdom via the `vdom` library, compiled with js_of_ocaml).

Every example/game module (`open Playground; ... let main = Playground_platform.run_app app`) is therefore **backend-agnostic source code**. The `examples/`, `games/` dune files link against `elm_playground_native`; the `examples/web/`, `games/web/` subdirectories reuse the exact same `.ml` files via a `(copy_files ../Foo.ml)` rule per file (a build-time copy, not a committed symlink or duplicate -- there is only one real source file per example/game, and only one top-level dir per example/game family) and link the result against `elm_playground_web` with `(modes js)`. `playground3d`'s `examples3d/` and `games3d/` both default to the OpenGL backend at the top level (`examples3d/dune`, `games3d/dune` link `elm_playground_3d_opengl` directly), with `examples3d/software/`, `games3d/software/` as the `copy_files` subdirectories reusing those same `.ml` files against `elm_playground_3d_software` instead -- `examples3d/web/` (SVG, `elm_playground_3d_web`) and `examples3d/webgl/`/`games3d/webgl/` (`elm_playground_3d_webgl`) are the browser equivalents, same `copy_files` pattern. `examples3d/dune` (unlike `games3d/dune`) is missing five examples that `examples3d/software/` has (`Cube3d`, `InteractiveCube3d`, `PaintersAlgorithmFail3d`, `FloatingCity3d`, `Corridor3d`) -- see `examples3d/dune`'s trailing comment for why.

### Supporting libraries

- `core/` → library `elm_core` (unwrapped): `Basics.ml` (float-friendly arithmetic operators, meant to be `open`ed by playground code), `Color.ml`, `Set_.ml`/`Set_.mli` (custom polymorphic set, exposed as `Set.ml`), `Keyboard.ml`, `Time.ml`. Reimplements small pieces of Elm's core/stdlib for ease of porting Elm code.
- `system/` → library `elm_system` (unwrapped, depends on `elm_core`): `Cmd.ml`, `Sub.ml` — Elm's Cmd/Sub effect-system stand-ins.
- Dependency order: `elm_core` ← `elm_system` ← `elm_playground` ← {`elm_playground_native` | `elm_playground_web`}.
- `gamekits/` → genre kits, private libraries on top of `elm_playground` shared by the games of a genre (`gamekits/racing/`: `Road`, `Car`, used by `games2.5d/TinyOutRun.ml` and `games3d/TinyVirtuaRacing.ml`, and `Topdown`, used by `games/TinyMicroMachines.ml` and `games2.5d/TinyMarioKart.ml`; `gamekits/racing/3d/`: `Track3d`, a course as a ribbon of quads with a width, a height and a bank -- its own library `kit_racing3d`, since it draws and `elm_playground_3d` is virtual -- built from control points for `games3d/TinyMarioKart64.ml`'s circuit and `games3d/TinyStarFox.ml`'s canyon, and from a `Road.t` for `games3d/TinyVirtuaRacing.ml`'s stage; `gamekits/sectors/`: `Sectors`, a Doom-style level, used by `games2.5d/TinyDoom.ml` and `games3d/TinyDoom3d.ml`; `gamekits/heightmap/`: `Heightmap`, a terrain as a grid of heights, used by `games2.5d/TinyComanche.ml` and `games3d/TinyComanche3d.ml`; `gamekits/segments/`: `Segments` (a Descent mine of boxes, and the openings where they touch) and `Sixdof` (a ship that can point anywhere), used by `games2.5d/TinyDescent.ml` and `games3d/TinyDescent3d.ml`; `gamekits/isometric/`: `Isometric`, a world seen from one fixed angle -- the two lines of the projection, the shadow that gives back the height they throw away, the back-to-front sort and the line of sight, used by `games2.5d/TinyZaxxon.ml` and `games2.5d/TinyDiablo.ml`; `gamekits/rhythm/`: `Rhythm`, a rhythm game's machinery -- the music's clock less the calibration, the grades and their windows, a chart played through, charts made from a tune's voices, the difficulty as the same chart reduced, the strum and the long notes -- used by `games/TinyDDR.ml`, `games2.5d/TinyGuitarHero.ml` and `games3d/TinyRockBand.ml`; `gamekits/shmup/`: `Shots`, `Path`, used by `games/TinyInvaders.ml` and `games/TinyGalaga.ml`, and in 3D by `games3d/TinyStarFox.ml` (a path across a canyon's cross-section); `gamekits/platformer/`: `Tile_move`, `Ladder`, used by `games/TinyMario.ml`, `games/TinyLodeRunner.ml`, `games/TinyRick.ml`, and `Slope`, shaped ground felt by sensors, used by `games/TinySonic.ml` and `games/TinyMarioWorld.ml`; `gamekits/brawler/`: `Hitbox`, `Frame_data`, `Stickman`, used by `games/TinyFinalFight.ml`, `games/TinyStreetFighter.ml`; `gamekits/brawler/3d/`: `Skeleton`, a fighter as a tree of boxes with joint angles and poses interpolated between keyframes (hierarchical transforms) -- its own library `kit_brawler3d`, used by `games3d/TinyVirtuaFighter.ml` (which keeps the 2D kit's `Frame_data` and `Hitbox` unchanged) and `games3d/TinyAloneInTheDark.ml`; `gamekits/puzzle/`: `Push`, `Undo`, used by `games/TinySokoban.ml`, `games/TinyBabaIsYou.ml`; `gamekits/sports/`: `Free_ball` (the ball pushed ahead of a player instead of carried) and `Formation` (a place per player, pulled towards the ball), used by `games/TinyKickOff2.ml`, `games/TinySpeedball2.ml` and `games/TinySensibleSoccer.ml`); generic layers (`Camera2d`, `Tilemap`, `Sprite`, `Scene2d`) are in `playground/` itself, as are the borrowed *ways of programming* -- `Logo` (the turtle), `Bigbang` (HtDP's world programs) and `Puzzlescript` (a game as a map and rewrite rules), each of which builds a Playground `app` for you, so a program written on one has no `update` and no `view` of its own; their demos live in `examples/` (`LogoFractals`, `BigBangRocket`, `PuzzleScriptSokoban`). See `docs/claude_notes/plan_games.md`.
- `games2.5d/` → the pseudo-3D games (`TinyZaxxon`, `TinyDiablo`, `TinyDungeonMaster`, `TinyWolfenstein`, `TinyOutRun`, `TinyGuitarHero`, `TinyMarioKart`, `TinyDoom`, `TinyComanche`, and `TinyDescent`, `TinyBattlezone` and `TinyElite`, full 3D but drawn by the game itself): a 3D look on the 2D playground, each with its original's trick written out in the game and marked in it (`grep "the trick of this game" games2.5d/*.ml`), laid out like `games/` (`web/`, `software/`); `games2.5d/README.md` compares the tricks, their sizes and their `games3d/` twins.
- `graphics/` → private libraries (no `public_name`, each installed as part of a package via its `(package ...)` field), the from-scratch rendering algorithms, independent of the Playground, one idea per module with its `.mli` explaining it: `graphics/core` (`Framebuffer`, `Blit`, `Opti`), `graphics/2d` (the 2D software rasterizer: `Fill`, `Line`, `Circle`, ...), `graphics/3d` (the 3D one: `Triangle`, `Zbuffer`, `Clip`, ..., `Render`), `graphics/3d/geometry` (`Vec3`, `Camera`, `Mat4`, `Lighting`, shared by all 3D backends), `graphics/gpu` (`Mesh_cache`, the GPU backends' retained meshes), `graphics/font` (Hershey), `graphics/images` (decoding). Unit tests in `graphics/tests/`. The `playground/software/` and `playground3d/software/` backends are thin adapters over them.

### The Model-View-Update pattern

`game view update initial_state` (see `playground/Playground.ml` and the README example) builds an `app` from:
- a `view : computer -> 'model -> shape list` function,
- an `update : computer -> 'model -> 'model` function,
- an initial model value.

`picture` and `animation` are simplified special cases of the same `app` type (`picture : shape list -> (screen, msg1) app`, `animation : (time -> shape list) -> (animation, msg) app`). The playground coordinate system is centered at `(0, 0)` (not top-left), which is a deliberate deviation from typical screen coordinates — keep this in mind when writing or debugging view code.

### opam packages are generated, not hand-edited

`dune-project` has `(generate_opam_files true)` and declares five `(package ...)` stanzas (`elm_core`, `elm_system`, `elm_playground`, `elm_playground_native`, `elm_playground_web`) with their dependencies. The `*.opam` files at the repo root are generated from this — edit `dune-project`, then run `make` (or `dune build <name>.opam`) to regenerate them, rather than editing the `.opam` files directly. `elm_playground_native.opam.template` is the one exception (hand-maintained template consumed during opam generation for that package).

### Docs (`docs/`)

`docs/` is the published GitHub Pages site (served from the `master` branch's `/docs` folder per repo settings) — it is *generated* output (`make website`: `dune build @doc` then copy `_build/default/_doc/_html` to `docs/`, plus `make js` to build the release-mode JS bundles for `examples/web`/`games/web`). `docs/toy-native-example/` and `docs/toy-web-example/` are the two minimal standalone example projects referenced in the README's "Simple native/web application" walkthroughs; keep them in sync with the corresponding README code snippet (see the `coupling:` comment in `README.md` referencing `docs/toy-native-example/toy.ml` and `examples/Keyboard.ml`).
