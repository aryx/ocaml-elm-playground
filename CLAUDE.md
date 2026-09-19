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
dune build games/js --profile=release-js     # build web games with small .js output
dune build examples/js --profile=release-js  # same, for examples
```

Run a single native example/game directly with dune, e.g.:
```bash
dune exec examples/Keyboard.exe
dune exec games/Snake.exe
```

Web (`js/`) targets are executables built in `(modes js)`; after building, the resulting `.bc.js` is copied next to the corresponding `.html` file in `examples/js/`/`games/js/` (see the `website`/`js` Makefile targets and the README's "Simple web application" section for the manual `dune build --root . && cp _build/default/Toy.bc.js static/` pattern used by the two toy example projects under `docs/`).

`make test` also runs the golden frame tests (`tests/2d/`, `tests/3d/`,
see `tests/common/Testutil_golden.mli`): every software-rasterizer
example rendered offscreen (SDL's dummy video driver) and compared pixel
by pixel with `tests/*/golden/*.png`. After an intended pixel change,
look at the new frames in `_build/default/tests/*/actual/`, then
`make approve-golden2d` / `make approve-golden3d`.

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

Every example/game module (`open Playground; ... let main = Playground_platform.run_app app`) is therefore **backend-agnostic source code**. The `examples/`, `games/` dune files link against `elm_playground_native`; the `examples/js/`, `games/js/` subdirectories reuse the exact same `.ml` files via a `(copy_files ../Foo.ml)` rule per file (a build-time copy, not a committed symlink or duplicate -- there is only one real source file per example/game, and only one top-level dir per example/game family) and link the result against `elm_playground_web` with `(modes js)`. `playground3d`'s analogous `examples3d/opengl/` directory (reusing `examples3d/Foo.ml` against `elm_playground_3d_opengl`) follows the same pattern.

### Supporting libraries

- `core/` → library `elm_core` (unwrapped): `Basics.ml` (float-friendly arithmetic operators, meant to be `open`ed by playground code), `Color.ml`, `Set_.ml`/`Set_.mli` (custom polymorphic set, exposed as `Set.ml`), `Keyboard.ml`, `Time.ml`. Reimplements small pieces of Elm's core/stdlib for ease of porting Elm code.
- `system/` → library `elm_system` (unwrapped, depends on `elm_core`): `Cmd.ml`, `Sub.ml` — Elm's Cmd/Sub effect-system stand-ins.
- Dependency order: `elm_core` ← `elm_system` ← `elm_playground` ← {`elm_playground_native` | `elm_playground_web`}.
- `kits/` → genre kits, private libraries on top of `elm_playground` shared by the games of a genre (`kits/racing/`: `Road`, `Car`, used by `games/TinyOutRun.ml` and `games3d/TinyVirtuaRacing.ml`; `kits/shmup/`: `Shots`, `Path`, used by `games/TinyInvaders.ml` and `games/TinyGalaga.ml`; `kits/platformer/`: `Tile_move`, `Ladder`, used by `games/TinyMario.ml`, `games/TinyLodeRunner.ml`, `games/TinyRick.ml`; `kits/brawler/`: `Hitbox`, `Frame_data`, `Stickman`, used by `games/TinyFinalFight.ml`, `games/TinyStreetFighter.ml`); generic layers (`Camera2d`, `Tilemap`, `Sprite`, `Scene2d`) are in `playground/` itself. See `docs/claude_notes/plan_games.md`.
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

`docs/` is the published GitHub Pages site (served from the `master` branch's `/docs` folder per repo settings) — it is *generated* output (`make website`: `dune build @doc` then copy `_build/default/_doc/_html` to `docs/`, plus `make js` to build the release-mode JS bundles for `examples/js`/`games/js`). `docs/toy-native-example/` and `docs/toy-web-example/` are the two minimal standalone example projects referenced in the README's "Simple native/web application" walkthroughs; keep them in sync with the corresponding README code snippet (see the `coupling:` comment in `README.md` referencing `docs/toy-native-example/toy.ml` and `examples/Keyboard.ml`).
