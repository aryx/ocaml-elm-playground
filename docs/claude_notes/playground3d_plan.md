# A 3D playground on top of ocaml-elm-playground

## Context

`ocaml-elm-playground` currently only supports 2D: `Playground.mli` defines a
closed `shape`/`form` variant, and two concrete backends (`elm_playground_native`,
Cairo-on-SDL; `elm_playground_web`, an SVG-based mini-vdom) implement the single
virtual function `Playground_platform.run_app`. The goal is a 3D counterpart,
in the same "beginner-friendly, zero-asset-pipeline, tiny combinator API" spirit
as Evan Czaplicki's original elm-playground, eventually capable of running a
port of `~/software-src/game/tiny-minecraft/main.py` (the classic
`fogleman/Minecraft` Pyglet demo: a voxel world with textured blocks,
first-person WASD+mouse-look movement, gravity/collision, and raycast block
picking).

Prior art found and mined for ideas: `lucamug/elm-playground-3d` (projects 3D
shapes down to plain 2D elm-playground shapes, rendered via SVG — good shape
for our *web* backend, but has no depth-sorting or backface culling) and
`ianmackenzie/elm-3d-scene` (full WebGL engine — too heavyweight/different
philosophy for this project). No 3D-related code existed anywhere in this
repo before this plan (confirmed by grep).

Key design decision, reconciling "reuse everything" with "a *real* software
rasterizer on native, on top of raw SDL, not just Cairo polygon fill": a 3D
game is its own full app (owns its window/loop), not a shape embedded in a 2D
scene, so it gets its **own** virtual-module pair (`elm_playground_3d` /
`Playground3d_platform.run_app3d`) alongside the existing 2D one — but the two
concrete backends diverge in ambition:

- **Native** gets a genuine from-scratch scanline rasterizer: raw SDL only for
  window/event-loop/present, an OCaml `Bigarray` framebuffer + z-buffer that
  hand-written code fills directly (triangle rasterization, depth test, and
  later per-pixel texture sampling for Minecraft), blitted via an SDL
  streaming texture. No Cairo vector calls in this path.
- **Web** reuses the lucamug trick and, concretely, reuses **all** of
  `elm_playground_web` unchanged: project the 3D scene down every frame to an
  ordinary `Playground.shape list` (with real backface culling + depth sort,
  which lucamug's version lacks) and hand it to `Playground.game`, then call
  straight into `elm_playground_web`'s existing `run_app`. Effectively zero
  new rendering code.

## Package/directory layout (mirrors existing conventions exactly)

New `(package ...)` stanzas in `dune-project` (same `(= :version)` pinning
pattern as the existing five):

- `elm_playground_3d` — interface only, like `elm_playground`. Deps:
  `(elm_core (= :version))`, `(elm_playground (= :version))` (reuses
  `Playground.color`/`Playground.shape`/`computer` and, for the web backend's
  compile-down step, `Playground.polygon`/`group`/`fade`/`game`).
- `elm_playground_3d_native` — deps: `tsdl`, `(elm_playground_3d (= :version))`.
  (No `cairo2`/`stb_image`/`curl` needed for M1 — pure Bigarray + SDL.)
- `elm_playground_3d_web` — deps: `(elm_playground_3d (= :version))`,
  `(elm_playground_web (= :version))` (yes, one concrete backend library
  depending on another — dune allows this fine; `elm_playground_3d_web`
  `(implements elm_playground_3d)` while also linking the already-concrete
  `elm_playground_web`).

Directories, mirroring `playground/`:
```
playground3d/
  dune                        (virtual_modules Playground3d_platform, wrapped false)
  Playground3d.mli / .ml      (the public API — see below)
  Playground3d_platform.mli   (just: val run_app3d : ('a,'b) Playground3d.app3d -> unit)
  native/dune, native/Playground3d_platform.ml   (library elm_playground_3d_native)
  web/dune,    web/Playground3d_platform.ml      (library elm_playground_3d_web)
examples3d/   dune + a couple of .ml demos   (like examples/)
games3d/      dune (empty/placeholder (names) list until Minecraft lands)
```
`examples/dune`/`games/dune`'s single shared `(executables (names ...)
(libraries elm_playground elm_playground_native))` stanza is the pattern to
copy verbatim for `examples3d/dune` and `games3d/dune` (swap in
`elm_playground_3d`/`elm_playground_3d_native`). Web `_js` siblings
(`examples3d_js/`, `games3d_js/`, symlinked `.ml` + hand-written `.html`,
`(modes js)`, the `(alias default)` html-copy rule) and the `Makefile`'s
`OPAMS`/`ODOC_DIRS`/`website` loop updates are mechanical, same shape as the
existing `_js` dirs — deferred to Phase 2 below, not a design question.

## `Playground3d.mli` — public API sketch

```ocaml
type number = Playground.number

(* -- shape3d/form3d, mirrors Playground.ml's shape/form split -- *)
type shape3d = { alpha : number; form : form3d }
and form3d =
  | Polygon3d of Playground.color * (number * number * number) list
  | Group3d of shape3d list
  (* Words3d deferred: not needed for the cube/minecraft targets *)

val polygon3d : Playground.color -> (number*number*number) list -> shape3d
val group3d   : shape3d list -> shape3d
val cube      : Playground.color -> number -> shape3d   (* 6 explicit faces, a la lucamug *)
val plane     : Playground.color -> number -> number -> shape3d

val move3d   : number -> number -> number -> shape3d -> shape3d
val move_x3d : number -> shape3d -> shape3d  (* + move_y3d, move_z3d *)
val rotate3d : number -> number -> number -> shape3d -> shape3d  (* degrees, per axis *)
val scale3d  : number -> shape3d -> shape3d
val fade3d   : number -> shape3d -> shape3d

(* -- camera: plain eye/target closure, no matrix type exposed -- *)
type camera
val camera : eye:(number*number*number) -> target:(number*number*number)
  -> ?fov:number -> ?near:number -> ?far:number -> unit -> camera

(* -- the shared pure pipeline both backends can use -- *)
val project : camera -> Playground.screen -> number*number*number -> (number*number) option
val render3d_to_2d : camera -> Playground.screen -> shape3d -> Playground.shape
  (* backface-culls + painter's-algorithm depth-sorts Group3d, THEN projects —
     the two things lucamug's version is missing *)

(* -- entry points, mirroring picture/animation/game -- *)
type ('model,'msg) app3d
val game3d :
  (Playground.computer -> 'model -> camera * shape3d list) ->
  (Playground.computer -> 'model -> 'model) ->
  'model -> ('model, Playground.msg) app3d
(* picture3d / animation3d: simplified special cases, added if/when needed *)
```

`Playground3d_platform.mli`: `val run_app3d : ('a,'b) Playground3d.app3d -> unit`.

## Phasing

**Phase 0 — write this plan into the repo.** (this document)

**Phase 1 — core `elm_playground_3d` library (pure, no backend).**
`playground3d/Playground3d.mli/.ml`: shape3d/form3d, combinators, `cube`,
`camera`, `project`, `render3d_to_2d` (with backface cull via face-normal
sign + `Array.sort` on face centroid distance for the painter's algorithm),
`app3d`/`game3d`. Add the `elm_playground_3d` package stanza to
`dune-project`. No window yet, but the pure math is unit-testable with Testo
(`tests/Testutil_playground3d.ml` + a test asserting e.g. a point at the
camera's focal point projects near screen-center, a point behind the camera
projects to `None`, and a cube's face vertices after `move3d`/`scale3d` land
where expected) — this is the cheapest place to catch projection-math bugs
before any rendering exists.

**Phase 2 — web backend (`elm_playground_3d_web`), reusing everything.**
`playground3d/web/Playground3d_platform.ml`'s `run_app3d` builds an ordinary
`Playground.app` — `Playground.game (fun computer model -> let (cam, shapes) =
view3d computer model in [render3d_to_2d cam computer.screen (group3d
shapes)]) update init` — and calls `elm_playground_web`'s existing
`Playground_platform.run_app` on it directly. Add the
`elm_playground_3d_web` package stanza. `examples3d_js/`/`games3d_js/` +
Makefile loop updates land here too, once there's something to demo.

**Phase 3 — native backend (`elm_playground_3d_native`), the real rasterizer.**
`playground3d/native/Playground3d_platform.ml`: own `Tsdl` window (same
`Sdl.init`/`Sdl.create_window` calls as `elm_playground_native`, but no
Cairo), a `Bigarray.Array1.t` RGBA framebuffer + `float array` z-buffer sized
to the window, per frame: flatten `Group3d` transforms to world-space
triangles (split each `Polygon3d` face into a fan of triangles), transform +
`project`, backface-cull, scanline-rasterize each triangle into the
framebuffer with a per-pixel z-test using `Bigarray.Array1.unsafe_get/set`
(no per-pixel allocation), then upload via an `SDL_TEXTUREACCESS_STREAMING`
texture and `Sdl.render_copy` + present. Re-implement (small, ~100 lines, not
worth factoring out yet) the event-draining/60fps-cap/keyboard+mouse
`computer` bookkeeping pattern from `elm_playground_native`'s
`Playground_platform.ml`. Add the `elm_playground_3d_native` package stanza.

**Phase 4 — `examples3d/` validation demos.** A spinning-cube example and a
small multi-cube scene (to exercise depth-sorting/backface-culling
correctness and, per the performance concern raised during planning, to
benchmark real FPS with an on-screen counter *before* committing to the full
Minecraft port). Dune wiring copies `examples/dune`'s single `(executables
(names ...))` pattern.

**Phase 5 — tiny-minecraft port (own follow-up plan, not detailed here).**
Once Phases 1-4 are validated: port `Model`'s `sectorize`/`exposed`/
`hit_test`/`add_block`/`remove_block`/`check_neighbors` (the block-visibility
culling that makes the original fast) to `games3d/Minecraft.ml`, add
first-person WASD+mouse-look camera controls and gravity/collision physics
(from the Python `Window` class), and extend `shape3d`/the native rasterizer
with per-face UV texture sampling (needed for `texture.png`'s grass/dirt/
stone/sand/brick atlas — textured, non-axis-aligned quads are the one thing
this design doesn't yet handle, and are naturally easy in a hand-written
scanline rasterizer via barycentric UV interpolation, harder on the web/SVG
side where it'd likely be approximated with per-face flat average color
first).

## Verification

- Phase 1: `dune build @default`, `dune runtest -f` (new Testo cases pass).
- Phase 2/3: `dune exec examples3d/Cube3d.exe` renders and animates a cube
  correctly (visual check); `dune build examples3d_js --profile=release-js`
  produces a working `.bc.js` for the same demo in a browser.
- Phase 4: FPS counter on the multi-cube demo gives a concrete answer to
  "will this be fast enough" before Phase 5 is attempted.
