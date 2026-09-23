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
dune build games/fps/web --profile=release-js # build a genre's web games with small .js output (make js: all)
dune build examples/web --profile=release-js  # same, for examples
```

Run a single native example/game directly with dune, e.g.:
```bash
dune exec examples/Keyboard.exe
dune exec games/arcade/Snake.exe
```

Web (`js/`) targets are executables built in `(modes js)`; after building, the resulting `.bc.js` is copied next to the corresponding `.html` file in `examples/web/`/`games/<genre>/web/` (see the `website`/`js` Makefile targets and the README's "Simple web application" section for the manual `dune build --root . && cp _build/default/Toy.bc.js static/` pattern used by the two toy example projects under `docs/`).

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
`dune exec examples/Cubes3d.exe -- -debug-keys`); other flags:
`-uncapped`, `-fixed-time t`, `-keys k`, `-dump-frame n file`,
`-script "right:1-60,space:30"` (game keys held over frames).

`make check` runs the project's semgrep config (`semgrep.jsonnet`) via a local `osemgrep` binary — not generally runnable outside the author's machine.

## Architecture

### Virtual module split: one API, two backends

The whole library hinges on dune's `virtual_modules` mechanism:

- `playground/Playground.mli` + `playground/Playground.ml` (library `elm_playground`) define the **entire public API**: shapes, colors, `computer`/`mouse`/`keyboard`/`screen` records, animation helpers (`spin`/`wave`/`zigzag`), and the three entry points `picture`, `animation`, `game` which all produce a `('model, 'msg) app` value.
- `playground/Playground_platform.mli` declares a single virtual function, `run_app : ('a, 'b) Playground.app -> unit`. The `elm_playground` library declares this as a `virtual_modules Playground_platform` in `playground/dune` — it has no implementation.
- Two separate libraries each provide a concrete `Playground_platform.ml` and `(implements elm_playground)` in their dune file:
  - `playground/native/Playground_platform.ml` → library `elm_playground_native` (SDL2 via `tsdl`, 2D drawing via `cairo2`, images decoded by our own `graphics/images/`, HTTP via `curl`, logging via `logs`).
  - `playground/web/Playground_platform.ml` → library `elm_playground_web` (DOM/vdom via the `vdom` library, compiled with js_of_ocaml).
- The 3D playground is the same split, in the same directories: `playground/Playground3d.mli` (library `elm_playground_3d`, virtual module `Playground3d_platform`), implemented by `playground/native/Playground3d_platform.ml` (`elm_playground_3d_opengl`), `playground/software/` (`elm_playground_3d_software`), `playground/web/` (`elm_playground_3d_webgl`) and `playground/svg/` (`elm_playground_3d_web`, the scene compiled to 2D shapes drawn as SVG). A directory holds a 2D and a 3D library side by side, each stanza listing its `(modules ...)`: the libraries (and opam packages) stay separate, so a 2D program needs no OpenGL. The SVG backend has its own directory because its implementation module has the same name as WebGL's.

Every example/game module (`open Playground; ... let main = Playground_platform.run_app app`) is therefore **backend-agnostic source code**. The `examples/`, `games/<genre>/` dune files link against `elm_playground_native`; the `examples/web/`, `games/<genre>/web/` subdirectories reuse the exact same `.ml` files via `copy_files` (a build-time copy, not a committed symlink or duplicate -- there is only one real source file per example/game) and link the result against `elm_playground_web` with `(modes js)`. The 3D examples (`*3d.ml`) sit in `examples/` too, with a second `executables` stanza per dune file (each stanza listing its `(modules ...)`): OpenGL (`elm_playground_3d_opengl`) at the top level, `elm_playground_3d_software` in `examples/software/`, WebGL (`elm_playground_3d_webgl`) in `examples/web/`, and SVG (`elm_playground_3d_web`) in `examples/svg/`, which has only 3D examples. A genre's 3D games (`games/<genre>/`) are laid out the same way, WebGL in its `web/`. `examples/dune`'s OpenGL stanza (unlike the games') is missing five 3D examples that `examples/software/` has (`Cube3d`, `InteractiveCube3d`, `PaintersAlgorithmFail3d`, `FloatingCity3d`, `Corridor3d`) -- see `examples/dune`'s trailing comment for why.

### Supporting libraries

- `core/` → library `elm_core` (unwrapped): `Basics.ml` (float-friendly arithmetic operators, meant to be `open`ed by playground code), `Color.ml`, `Set_.ml`/`Set_.mli` (custom polymorphic set, exposed as `Set.ml`), `Keyboard.ml`, `Time.ml`, and `Cmd.ml`, `Sub.ml` — Elm's Cmd/Sub effect-system stand-ins (in Elm's core too, as `Platform.Cmd`/`Platform.Sub`). Reimplements small pieces of Elm's core/stdlib for ease of porting Elm code. Installed as `elm_playground.core`, a sub-library of the `elm_playground` package.
- Dependency order: `elm_core` ← `elm_playground` ← {`elm_playground_native` | `elm_playground_web`}.
- `gamekits/` → genre kits, private libraries on top of `elm_playground` shared by the games of a genre (`gamekits/racing/`: `Road`, `Car`, used by `TinyOutRun.ml` and `TinyVirtuaRacing.ml`, and `Topdown`, used by `TinyMicroMachines.ml` and `TinyMarioKart.ml`, and with its walls, bounces and bumps by the fixed-screen racers from above, `TinyGranTrak10.ml`, `TinySuperSprint.ml`, `TinySuperOffRoad.ml`, and the scrolling `TinySupercars.ml`, and the city of `TinyGTA.ml`; `gamekits/racing/3d/`: `Track3d`, a course as a ribbon of quads with a width, a height and a bank -- its own library `kit_racing3d`, since it draws and `elm_playground_3d` is virtual -- built from control points for `TinyMarioKart64.ml`'s circuit and `TinyStarFox.ml`'s canyon, and from a `Road.t` for `TinyVirtuaRacing.ml`'s stage; `gamekits/sectors/`: `Sectors`, a Doom-style level, used by `TinyDoom.ml` and `TinyDoom3d.ml`; `gamekits/heightmap/`: `Heightmap`, a terrain as a grid of heights, used by `TinyComanche.ml` and `TinyComanche3d.ml`, and for its Hyrule Field by `TinyZeldaOcarina.ml`; `gamekits/segments/`: `Segments` (a Descent mine of boxes, and the openings where they touch) and `Sixdof` (a ship that can point anywhere), used by `TinyDescent.ml` and `TinyDescent3d.ml`; `gamekits/isometric/`: `Isometric`, a world seen from one fixed angle -- the two lines of the projection, the shadow that gives back the height they throw away, the back-to-front sort and the line of sight, used by `TinyZaxxon.ml`, `TinyDiablo.ml` and `TinySuperOffRoad.ml`; `gamekits/rhythm/`: `Rhythm`, a rhythm game's machinery -- the music's clock less the calibration, the grades and their windows, a chart played through, charts made from a tune's voices, the difficulty as the same chart reduced, the strum and the long notes -- used by `TinyDDR.ml`, `TinyGuitarHero.ml` and `TinyRockBand.ml`; `gamekits/shmup/`: `Shots`, `Path`, used by `TinyInvaders.ml` and `TinyGalaga.ml`, and in 3D by `TinyStarFox.ml` (a path across a canyon's cross-section); `gamekits/platformer/`: `Tile_move`, `Ladder`, used by `TinyMario.ml`, `TinyLodeRunner.ml`, `TinyRick.ml`, and `Slope`, shaped ground felt by sensors, used by `TinySonic.ml` and `TinyMarioWorld.ml`; `gamekits/brawler/`: `Hitbox`, `Frame_data`, `Stickman`, used by `TinyFinalFight.ml`, `TinyStreetFighter.ml`; `gamekits/brawler/3d/`: `Skeleton`, a fighter as a tree of boxes with joint angles and poses interpolated between keyframes (hierarchical transforms) -- its own library `kit_brawler3d`, used by `TinyVirtuaFighter.ml` (which keeps the 2D kit's `Frame_data` and `Hitbox` unchanged), `TinyAloneInTheDark.ml` and `TinyZeldaOcarina.ml` (its swords and shield held in the skeleton's hands); `gamekits/puzzle/`: `Push`, `Undo`, used by `TinySokoban.ml`, `TinyBabaIsYou.ml`, and `Sokoban` (the rules, the `.xsb` level format and a solver), shared by `TinySokoban.ml` and its level editor `TinySokobanEd.ml`; `gamekits/sports/`: `Free_ball` (the ball pushed ahead of a player instead of carried) and `Formation` (a place per player, pulled towards the ball), used by `TinyKickOff2.ml`, `TinySpeedball2.ml` and `TinySensibleSoccer.ml`; `gamekits/adventure/`: `Adventure`, a world as data (rooms, objects each somewhere, flags) and the player's sentences answered by rules, used by `TinyZork.ml`, which parses what is typed into a sentence, and `TinyManiacMansion.ml`, which builds one from clicks; `gamekits/crush/`: `Crush`, a level of 2D slices played on the slice you stand in or crushed into their union, the rules of `TinyCrush.ml` (a cabinet projection on the 2D playground) and of its 3D twin `TinyCrush3d.ml`; `gamekits/cards/`: `Cards` (the 52 cards, Microsoft's numbered deals -- its random generator written out, deal 1 the worked example -- and a card drawn, the suits as shapes), used by `TinySolitaire.ml` and `TinyFreeCell.ml`); generic layers (`Camera2d`, `Tilemap`, `Sprite`, `Scene2d`) are in `playground/` itself, as are the borrowed *ways of programming* -- `Logo` (the turtle), `Bigbang` (HtDP's world programs) and `Puzzlescript` (a game as a map and rewrite rules), each of which builds a Playground `app` for you, so a program written on one has no `update` and no `view` of its own; their demos live in `examples/` (`LogoFractals`, `BigBangRocket`, `PuzzleScriptSokoban`). See `docs/claude_notes/plan_games.md`.
- `games/<genre>/` → the games, one directory per section of `CATALOG.md` (`shmup`, `fighting`, `platform`, `arcade`, `puzzle`, `cards`, `adventure`, `rpg`, `fps`, `flight`, `racing`, `sports`, `strategy`, `rhythm`, `programming`), each section starting with its genre's definition, 2D, 2.5D and 3D side by side, so that twins sit together (`TinyDoom`/`TinyDoom3d` in `fps/`). A genre is self-contained, laid out like `examples/`: its dune file has a 2D stanza (Cairo) and a 3D one (OpenGL), each listing its `(modules ...)`, and its own `software/` and `web/` (WebGL for the 3D games, and the `.html` pages) `copy_files` its sources. The Makefile's `GENRES` lists the genres' directories. The 2.5D games (`TinyZaxxon`, `TinyDiablo`, `TinyMazeWar`, `TinyDungeonMaster`, `TinyWolfenstein`, `TinyOutRun`, `TinyGuitarHero`, `TinyMarioKart`, `TinySuperOffRoad`, `TinyShufflePuck`, `TinyGTA`, `TinyCrush`, `TinyFez`, `TinyDoom`, `TinyComanche`, and `TinyDescent`, `TinyBattlezone` and `TinyElite`, full 3D but drawn by the game itself) are a 3D look on the 2D playground, each with its original's trick written out in the game and marked in it (`grep -r "the trick of this game" games/`); `games/README-2.5d.md` compares the tricks, their sizes and their 3D twins. `games/template.ml` (and `template.html`) is the skeleton of a new game.
- `apps/<category>/` → the Tiny applications, laid out like a genre of games (with `software/` and `web/`). `apps/office/` has TinyVisiCalc to TinyOffice, and the modules they share (`Stroke_text`, `Part_*`, `Figure_shapes`; their `File_menu`, shared by every category's apps, is the appkit `appkits/file_menu`); `apps/gamedev/` the game-making tools (`TinyAseprite`, the sprite editor, and `TinyTiled`, the map editor, both over XPM files -- a character per pixel is a character per cell); `apps/music/` the music programs (`TinyMediaPlayer`, every format the repository reads, recognized by its bytes, over the library `music_media` -- `Media`, and `Our_media`, the media of our own the music programs start from; `TinySoundtracker`, the tracker, over `audio/formats/mod`; `TinyMinimoog`, over `Minimoog_voice`, the Model D's voice built from `audio/`'s blocks, in the library `music_voices` so `apps/music/tests/` can play it without a screen: unit tests and a golden WAV per preset, `make approve-golden-music`); `internet/`, `devtools/`, `graphics/`, `system/` are placeholders, a comment-only dune file each saying what it might hold. An app of a new category needing a shared office module (a File menu) is the moment to turn that module into an appkit.
- Game-making tools: `games/README-tools.md` says where one goes (a genre's level editor in `games/<genre>/` as `Tiny<Game>Ed`, sharing the kit with its game; a tool for any game in `apps/gamedev/`; one for any program in `apps/graphics/` or `apps/music/`) and how a game gets what it made (the reader's format, a file beside the game embedded by dune at build time, exported back with `Playground_platform.export`). `TinySokobanEd` (`games/puzzle/`, over `TinySokoban.xsb`) and `TinyAseprite`/`TinyTiled` (`apps/gamedev/`, over TinyMario's `mario_*.xpm` sprites and `mario_level.xpm`, read by `Sprite.of_xpm` and `Tilemap.of_xpm`) are the worked examples.
- Artwork vs shapes: a game with pixel art draws, by default, in the medium its original really used (sprites for Pac-Man and Mario, shapes for the vector-display games and Pong), and the flag `artwork=shapes`/`artwork=sprites` flips it -- `Sprite.artwork ~default computer.flags`, documented in `Sprite.mli`, with each game's header saying its default and why. Both looks are frozen as golden frames (`Testutil_golden.flagged`, e.g. `TinyCeleste_shapes.png`).
- `graphics/` → private libraries (no `public_name`, each installed as part of a package via its `(package ...)` field), the from-scratch rendering algorithms, independent of the Playground, one idea per module with its `.mli` explaining it: `graphics/core` (`Framebuffer`, `Blit`, `Opti`), `graphics/2d` (the 2D software rasterizer: `Fill`, `Line`, `Circle`, ...), `graphics/3d` (the 3D one: `Triangle`, `Zbuffer`, `Clip`, ..., `Render`), `graphics/3d/geometry` (`Vec3`, `Camera`, `Mat4`, `Lighting`, shared by all 3D backends), `graphics/gpu` (`Mesh_cache`, the GPU backends' retained meshes), `graphics/font` (Hershey), `graphics/images` (loading and caching image files and textures, `Image_decode`, `Texture_decode`), and under it the image formats, read by our own code, pure OCaml, each its own library in the `elm_playground` package: `rgba/` (`Rgba_image`, the decoded picture), `deflate/` (`Crc32`, `Adler32`, `Huffman`, `Inflate`, `Zlib`), `png/`, `gif/` (`Lzw`, `Gif`), `jpeg/` (`Dct`, `Jpeg`, baseline only), and `xpm/` (read and written, for `Sprite`); see `docs/claude_notes/tutorials/notes_images.md`, and `graphics/tests/` for their fixtures (PngSuite, GIFs and JPEGs of our own). Unit tests in `graphics/tests/`. The 2D and 3D `playground/software/` backends are thin adapters over them.

### The Model-View-Update pattern

`game view update initial_state` (see `playground/Playground.ml` and the README example) builds an `app` from:
- a `view : computer -> 'model -> shape list` function,
- an `update : computer -> 'model -> 'model` function,
- an initial model value.

`picture` and `animation` are simplified special cases of the same `app` type (`picture : shape list -> (screen, msg1) app`, `animation : (time -> shape list) -> (animation, msg) app`). The playground coordinate system is centered at `(0, 0)` (not top-left), which is a deliberate deviation from typical screen coordinates — keep this in mind when writing or debugging view code.

### The catalogue

`CATALOG.md` lists every game (`games/<genre>/`, a section per genre) and app (`apps/`), one table row each: name linked to its source, how it is drawn (2D, 2.5D, 3D), the original it is after, a one-line description (a future tooltip). A new game or app gets its row there; the screenshot and web page are found by convention (see the file's introduction). `tests/catalog/` checks it in `make test`: every executable named in the genres' and `apps/`'s dune files needs its row, its golden frame (`tests/2d/golden/<Name>.png`, `tests/3d/` for a 3D game) and its web page (`<dir>/web/<Name>.html`), and every row must name an existing program.

### opam packages are generated, not hand-edited

`dune-project` has `(generate_opam_files true)` and declares nine `(package ...)` stanzas, one per public library (`elm_playground`, which also installs `elm_core` as `elm_playground.core`, `elm_playground_native`, `elm_playground_software`, `elm_playground_web`, `elm_playground_3d`, `elm_playground_3d_opengl`, ...) with their dependencies. The libraries shared by the native backends (`playground/native_common/`, `graphics/images/`) are private, installed as part of `elm_playground_software` (see `playground/native_common/dune` for why, and the caveat). The `*.opam` files at the repo root are generated from this — edit `dune-project`, then run `make` (or `dune build <name>.opam`) to regenerate them, rather than editing the `.opam` files directly. `elm_playground_native.opam.template` is the one exception (hand-maintained template consumed during opam generation for that package).

### Docs (`docs/`)

`docs/` is the published GitHub Pages site (served from the `master` branch's `/docs` folder per repo settings) — it is *generated* output (`make website`: `dune build @doc` then copy `_build/default/_doc/_html` to `docs/`, plus `make js` to build the release-mode JS bundles for `examples/web`/`games/<genre>/web`). `docs/toy-native-example/` and `docs/toy-web-example/` are the two minimal standalone example projects referenced in the README's "Simple native/web application" walkthroughs; keep them in sync with the corresponding README code snippet (see the `coupling:` comment in `README.md` referencing `docs/toy-native-example/toy.ml` and `examples/Keyboard.ml`).
