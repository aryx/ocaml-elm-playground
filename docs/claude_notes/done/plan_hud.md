# Plan: a 2D HUD overlay for playground3d/

## Context

Flagged repeatedly as a gap: `game3d`'s `view3d` only ever returns a 3D
scene (`camera * shape3d list`), so there's no way to draw a score, a
crosshair, or an instructions overlay on top of it.
`games3d/StarCollector3d.ml`'s header comment calls this out explicitly
(score is tracked but never shown); `examples3d/InteractiveCube3d.ml`'s
header notes the on-screen instructions text lucamug's original has
had to be dropped for the same reason; `plan_tiny_minecraft.md` flags
it as one of only two real gaps blocking that port (its crosshair/score
HUD).

## Design

### The API surface barely changes

`game3d`/`view3d`'s signature stays exactly `camera * shape3d list` --
no third tuple element, no new top-level entry point. Instead, one new
leaf case is added to the existing `form3d` variant (the same move
`SmoothPolygon3d` made for shading):

```ocaml
| Hud of Playground.shape
```

with a smart constructor `val hud : Playground.shape -> shape3d`. A HUD
element is just another entry in the `shape3d list` you already
return -- e.g.:

```ocaml
(cam, [ ground; player; stars_group;
        hud (words black (Printf.sprintf "Score: %d" m.score) |> move (-260.) 280.) ])
```

This is deliberately *compositional*: `hud` is the only new
combinator. Everything inside it -- `words`, `rectangle`, `circle`,
`image`, `group`, `move`, `rotate`, `scale`, `fade` -- is the existing
2D `Playground` API, completely unchanged, and a HUD shape is
positioned using the exact same coordinate system (origin at screen
center, `computer.screen`'s `top`/`left`/`bottom`/`right` bounds --
already available in `view3d`'s `computer` argument) as a 2D
`picture`/`animation`/`game`. Nothing new to learn if you already know
the 2D playground.

### Semantics: screen-space, not scene-space

A `Hud` shape is **exempt from 3D spatial transforms**: `move3d`,
`rotate3d`, and `scale3d` are no-ops on it (their tree-walk simply
passes a `Hud` node through unchanged), even if it's nested inside a
`Group3d` that gets moved/rotated (e.g. `InteractiveCube3d.ml`'s
mouse-driven turntable rotation of the whole scene). This has to be
called out clearly in the doc comment, since it's the one place this
form3d case behaves differently in kind from every other one -- but
it's the only sensible semantics for "HUD": the whole point is that it
stays fixed on screen regardless of what the 3D scene around it is
doing.

`fade3d` **does** apply, and for free, via the exact mechanism already
in place for every other leaf case: `fade3d` (`playground3d/Playground3d.ml`)
already works by setting `shape.alpha` on every leaf it recurses into
(including through `Group3d`, so an outer `fade3d` on a group *does*
reach a `Hud` nested inside it -- no asymmetry here, unlike the
alpha-on-3D-faces situation below), and the existing web-side
`flatten_faces`/`render3d_to_2d` already turns that per-leaf `alpha`
into a real `Playground.fade alpha ...` call when building each
resulting 2D shape (see `Playground.polygon color projected |>
Playground.fade alpha` in today's code). `Hud`'s case does the same:
`Playground.fade shape.alpha s`. This is why `collect_hud_shapes`
below is written once in `Playground3d.ml` and shared by both
backends, rather than duplicated per-backend.

**Simplification, stated up front:** this is alpha *applied to the
Hud's own 2D shape*, unrelated to `playground3d/native/Playground3d_platform.ml`'s
top-of-file comment that native doesn't honor `shape3d`-level alpha
for *3D faces* -- that limitation is untouched by this plan and
doesn't apply to `Hud` at all, since `Hud` never goes through the 3D
rasterizer's per-face fill path.

No hit-testing/interactivity on HUD elements -- 2D `Playground.shape`
values aren't clickable today either, so this isn't a new limitation.

## Shared: `collect_hud_shapes` in `Playground3d.ml`

One tree-walk, written once and used by both backends (mirroring how
`flatten_faces`/`face_normal`/`face_centroid` are already shared
building blocks in this file):

```ocaml
let rec collect_hud_shapes (shape : shape3d) : Playground.shape list =
  match shape.form with
  | Hud s -> [ Playground.fade shape.alpha s ]
  | Group3d shapes -> List.concat_map collect_hud_shapes shapes
  | Polygon3d _ | TexturedPolygon3d _ | SmoothPolygon3d _ -> []
```

## Native implementation

### The key finding: `render_shape`/`render` can't be called directly today

The obvious plan -- "just call `playground/native/Playground_platform.ml`'s
existing `render : Playground.shape list -> unit` from
`playground3d/native/`" -- doesn't work as-is, and it's worth recording
*why*, since it's not obvious: dune seals a virtual module's
implementation to exactly its virtual `.mli`'s signature. Confirmed by
inspecting the compiled `.cmi`:

```bash
$ strings _build/default/playground/native/.elm_playground_native.objs/byte/playground_platform.cmi \
    | grep -i render_shape
# (nothing -- only run_app is visible, matching playground/Playground_platform.mli exactly)
```

So even though `elm_playground_3d_native` already transitively links
`elm_playground_native` (every `examples3d`/`games3d` executable does,
to satisfy `elm_playground`'s virtual module requirement at link time),
`render_shape`/`render`/`hook` are invisible outside that module today.

**Fix: extract them into a new, plain (non-virtual) sibling module**,
following the exact precedent already sitting right next to them --
`playground/native/Image_native.ml`/`.mli` is already exactly this
kind of thing (a normal, directly-`open`-able module in the same
library, distinct from the virtual `Playground_platform`). Add
`playground/native/Shape_render_native.ml`/`.mli`:

```ocaml
val render : Cairo.context -> width:float -> height:float -> Playground.shape list -> unit
```

moving `render`/`render_shape`/`hook`/`empty_hook`/`convert`/
`render_transform`/the per-form `render_*` helpers there essentially
as-is, with `g_cr`/`g_sx`/`g_sy` (currently module-global refs, fine
when there's only one caller) turned into ordinary parameters, since
there are now genuinely two independent callers (the 2D backend's own
loop, and playground3d's new HUD pass) that each have their own
`Cairo.context` and window size. `Playground_platform.ml` becomes a
thin caller: `Shape_render_native.render cr ~width:(float sx)
~height:(float sy) shapes` in place of today's `render shapes`.

This is a real refactor, not a copy-paste, but a small, mechanical
one -- the actual per-shape drawing code (`render_circle`,
`render_words`, etc.) doesn't change at all, only how it receives the
context/size. **Verification**: screenshot a couple of existing 2D
examples (e.g. `Mario.exe`, anything using `words`) before and after,
confirm pixel-identical output -- this step must not change any 2D
rendering behavior.

### Reusing the *exact* pixel buffer the 3D rasterizer already writes into

The nice part: `playground3d/native/Playground3d_platform.ml`'s
`framebuffer` is obtained via `Sdl.get_surface_pixels window_surface
Bigarray.int32` -- **the identical trick**
`playground/native/Playground_platform.ml` already uses (`let pixels =
... in Cairo.Image.create_for_data32 ~w:sx ~h:sy pixels`) to let Cairo
draw straight into an SDL window surface's own pixel memory. So the
HUD pass doesn't need a separate surface, an alpha-blend compositing
step, or any pixel-format conversion at all: after the 3D scene is
fully rasterized into `framebuffer` for the frame, wrap that *same*
Bigarray in a fresh `Cairo.context` and draw the HUD shapes directly on
top of the already-rendered pixels, once, right before
`Sdl.update_window_surface`:

```ocaml
match Playground3d.collect_hud_shapes (Playground3d.group3d shapes) with
| [] -> ()
| hud_shapes ->
    let surface = Cairo.Image.create_for_data32 ~w:sx ~h:sy framebuffer in
    let cr = Cairo.create surface in
    Cairo.identity_matrix cr;
    Cairo.translate cr (float sx /. 2.) (float sy /. 2.);
    Shape_render_native.render cr ~width:(float sx) ~height:(float sy) hud_shapes
```

No `Cairo.paint`/clear step (unlike the 2D backend's per-frame reset)
-- this pass must only add pixels on top, never erase the 3D frame
underneath. `collect_hud_shapes` (see below -- lives in the shared
`Playground3d.ml`, not duplicated here) is a small tree-walk, parallel
to (not a case added inside) `flatten_faces`, since a `Hud` node
produces no triangles at all. Native's own separate `flatten_faces`
(in `Playground3d_platform.ml`, distinct from the pure one in
`Playground3d.ml` used by the web backend) gets one added arm, `| Hud _
-> []` (contributes no geometry -- it's collected separately, never
rasterized as a face).

### Dune/dependency changes

- `playground3d/native/dune`: add `cairo2` (for `Cairo.context`/
  `Cairo.Image.create_for_data32`) and `elm_playground_native` (for
  `Shape_render_native`) to `libraries`.
- `dune-project`'s `elm_playground_3d_native` package stanza: add the
  same two as opam dependencies, mirroring how `elm_playground_native`'s
  own stanza already declares `cairo2`/`tsdl`.

## Web implementation (expected to be nearly free, per the established pattern)

`render3d_to_2d` currently builds its result as `Playground.group
shapes2d` from the depth-sorted, projected 3D faces alone. Add:

```ocaml
let hud_shapes = collect_hud_shapes shape in
Playground.group (shapes2d @ hud_shapes)
```

-- appended *after* the depth-sorted/backface-culled 3D-derived shapes
(later elements paint on top, same convention relied on elsewhere), so
a HUD shape reaches `elm_playground_web`'s existing, unmodified SVG
renderer exactly the way any 2D `Playground.shape` already does -- zero
new rendering code on this backend, the same "reuse everything" shape
as the original web `render3d_to_2d` design. `flatten_faces` (the
web-facing one, shared with `render3d_to_2d`) also needs its own `| Hud
_ -> []` arm, same reasoning as native's.

## Demo

Extend `games3d/StarCollector3d.ml` (its own header comment already
calls out the missing score display as the reason there's "no visible
progress ... besides watching stars appear and disappear") to render
the actual score via `hud`, e.g.:

```ocaml
hud (words black (Printf.sprintf "Score: %d" m.score) |> move (computer.screen.left +. 40.) (computer.screen.top -. 40.))
```

This both exercises the feature end-to-end and closes a gap the
project has been carrying since that file was written.

## Phasing

1. Extract `Shape_render_native` out of `playground/native/
   Playground_platform.ml`; verify no 2D rendering regression.
2. `Playground3d.mli`/`.ml`: `Hud` case, `hud` constructor,
   move3d/rotate3d/scale3d no-ops, the shared `collect_hud_shapes`,
   web-side `flatten_faces`/`render3d_to_2d` cases.
3. Native: dune/dependency changes, `flatten_faces`'s `Hud -> []` arm,
   the post-3D Cairo HUD pass in `run_app3d`.
4. `games3d/StarCollector3d.ml`: add the score HUD.
5. Update `notes_3d.md` (a short new subsection) and
   `plan_tiny_minecraft.md` (mark the HUD gap resolved).

## Verification

- `dune build` (full project) after each phase; `dune runtest -f`.
- Screenshot `Mario.exe` (or another `words`-using 2D example) before
  and after Phase 1, confirm pixel-identical.
- Screenshot `StarCollector3d.exe` after Phase 4: confirm the score
  text stays fixed on screen while the player/stars/camera move
  underneath it, and that it's legible on top of the 3D scene.
- No `examples3d_js`/`games3d_js` executables exist yet (`elm_playground_3d_web`
  is only ever built as a library today, unlike the 2D playground's
  `_js` targets), so there's nothing to run/screenshot for the web side
  yet -- `dune build` compiling `elm_playground_3d_web` cleanly after
  Phase 2's added case is the only web-side check available right now.
