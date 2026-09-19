# Plan: `elm_playground_3d_opengl`, a real GPU-accelerated 3rd backend

## Context

`notes_playground3d_related_work.md`'s whole point was that `playground3d/`
deliberately sits at the "hand-written, read every line" end of the
spectrum, opposite OpenGL/WebGL/Vulkan. This plan is the natural
follow-up experiment that comparison invites: build a **third**
concrete implementation of the `Playground3d_platform` virtual module,
alongside `native` (software rasterizer) and `web` (SVG), that hands
the exact same `shape3d`/`camera` scenes to a real GPU via OpenGL --
and see, concretely, how much of `playground3d/native/`'s hand-rolled
code (the z-buffer, backface culling, the perspective-correct-
interpolation trick, `rasterize_triangle`'s whole edge-function pixel
loop) simply disappears into one-line hardware feature toggles, versus
what has to be rebuilt in a different shape (GLSL shaders, vertex
buffers, real 4x4 matrices). This is the concrete version of the
"assembly language ceiling" comparison the related-work doc could only
describe abstractly.

Same API, unmodified: any `examples3d`/`games3d` source file that only
calls `Playground3d`'s public combinators should be linkable against
this backend the same way it's linkable against `native` today, no
source changes required (modulo the "not everything is supported in
v1" scope below -- exactly the same kind of accepted asymmetry the
`web` backend already has with textures).

## Naming and layout

`elm_playground_3d_opengl` (library/package), directory `playground3d/opengl/`
-- mirrors `playground3d/native/`'s and `playground3d/web/`'s naming
exactly. ("native_accelerated" was considered and rejected: it reads
oddly once there are two things called "native" living side by side;
naming this one after the technology, like `web` is named after SVG's
delivery medium... actually named after *its* technology, is more
consistent.)

## Dependency: `tgls`

Verified available in the opam repo (`opam show tgls` --
`0.8.6`/`0.9.0`/`0.9.1`, not currently installed in this switch):
"Thin bindings to OpenGL {3,4} and OpenGL ES {2,3} for OCaml", by
Daniel Bünzli -- **the same author as `tsdl`**, and the standard,
long-established pairing for "SDL2 window + OpenGL rendering" in
OCaml. `tsdl` itself (already a dependency here) already exposes
everything needed for the window/context side, confirmed in its
`.mli`: `Sdl.Window.opengl` (a window creation flag), `Sdl.gl_create_context`,
`Sdl.gl_set_attribute`, `Sdl.gl_make_current`, `Sdl.gl_swap_window`.
`tgls`'s `Tgl3.Gl` module is the target (OpenGL 3.2+ core profile --
modern enough for the programmable pipeline this plan needs: VAOs,
VBOs, GLSL shaders -- old enough to be broadly available and to match
what most existing tgls examples/tutorials target).

## Reuse opportunity: extract the SDL event-loop/computer bookkeeping

`playground3d/native/Playground3d_platform.ml`'s `run_app3d` currently
re-implements (deliberately, per `plan_playground3d.md`'s Phase 3:
"small, ~100 lines, not worth factoring out yet") the same
event-draining/60fps-cap/keyboard+mouse `Playground.computer`
bookkeeping pattern `playground/native/Playground_platform.ml` already
has for the 2D backend. That "not worth it yet" judgment flips here:
this new backend needs the *exact same* SDL window/event/computer
plumbing (only the "how do I turn a `shape3d list` into pixels" part
differs) -- a second real caller is exactly when it becomes worth
extracting.

Add a small shared module (e.g. `playground3d/native_common/Native_loop.ml`/`.mli`,
a new plain library `elm_playground_3d_native_common` depended on by
both `elm_playground_3d_native` and `elm_playground_3d_opengl`)
providing the window-creation, SDL-event-draining-into-`computer`, and
frame-pacing logic, parameterized over a `draw_frame : Playground.computer -> unit`
callback each backend supplies. This also makes the "how much shorter
is the code" comparison this plan is explicitly for actually fair --
the two backends' *unique* line counts become just their two very
different `draw_frame` implementations, not diluted by re-deriving the
same event loop twice.

## Rendering pipeline: one-for-one against the software rasterizer

| Software rasterizer (`playground3d/native/`) | OpenGL equivalent |
|---|---|
| `project_vertex`'s manual look-at + perspective-divide math | A small hand-rolled `Mat4` module (`look_at`, `perspective`, 4x4 multiply -- no existing matrix library in this repo, and pulling one in would undercut the "how much of this is genuinely simpler" question) producing a `uMVP` uniform; `gl_Position = uMVP * vec4(pos, 1.0)` in the vertex shader does the rest on the GPU |
| The z-buffer (`float array` + per-pixel compare in `rasterize_triangle`) | `Gl.enable Gl.depth_test` -- one line |
| Backface culling (`dot normal (sub camera.eye centroid) > 0.` per face) | `Gl.enable Gl.cull_face; Gl.cull_face Gl.back` -- two lines |
| Perspective-correct interpolation (the `u/z`/`v/z`/`1/z` trick, and the whole "p" toggle) | Automatic and unconditional -- GPU rasterizer hardware always perspective-correct-interpolates fragment shader inputs; no toggle, no code, not even a concept to name |
| `rasterize_triangle`'s edge-function/barycentric pixel-filling loop | The GPU's fixed-function rasterizer; zero OCaml/GLSL code at all |
| `make_shader`'s Phong branch (interpolate normals, `brightness_of_normal` per pixel) | A GLSL fragment shader doing the same dot-product lighting formula, per pixel, in parallel across the GPU -- genuinely the same math, just running somewhere else |
| `sample_texture` (nearest-neighbor manual UV lookup) | `texture(uSampler, vUv)` in the fragment shader, after one `glTexImage2D` upload (deferred -- see Scope) |

## Scope for v1 (simplifications, stated up front)

- **One shading mode: real per-pixel Phong**, always on -- no
  flat/flat_shading/Gouraud/mode-cycling toggle initially. This is a
  backend-local runtime-key concern (native's "m"/"b"/"f"/"z"/"p" keys
  are handled entirely inside `Playground3d_platform.ml`, never part
  of the public `Playground3d` API), so skipping it doesn't stop any
  example/game source file from running -- it's just less to build
  first. Flat/Gouraud can be added later as alternate fragment
  shaders/uniforms if wanted, once v1 works.
- ~~No textures initially~~ **DONE** (Phase 5): `Texture_native` was
  moved into `elm_playground_3d_native_common` (it never depended on
  anything native-rendering-specific, just stb_image/curl) so both
  backends share the exact same download/decode/cache code; only the
  "upload to the GPU + sample in the shader" half was new. Faces are
  grouped by material (`Flat` vs `Textured of string`, one draw call
  per group, since a single GL draw call can only bind one texture)
  rather than each picking its own fill closure independently like
  native does. Verified pixel-identical against native on a
  controlled, fixed-camera `TexturedCube3d.exe` screenshot (no v-flip
  needed -- `glTexImage2D`'s row 0 already becomes texture coordinate
  v=0, the same "v=0 is the top row" convention this project's UV
  already uses).
- ~~No wireframe or painter's-algorithm modes initially~~ **Wireframe
  DONE** (Phase 5, one line: `Gl.polygon_mode Gl.front_and_back
  Gl.line`, "f" to toggle, same key as native). Painter's-algorithm
  stays permanently out of scope for this backend, as expected -- a
  hardware z-buffer makes it moot.
- **Naive per-frame vertex upload**: flatten `shape3d` into a fresh
  vertex buffer and re-upload it (`glBufferData`) every frame, rather
  than caching a VAO/VBO per shape across frames. Matches this
  project's established "measure before optimizing" pattern
  (`notes_3d_opti.md`) -- an FPS-counter checkpoint (reusing
  `Fps`-equivalent bookkeeping) on a `Cubes3d`-equivalent scene decides
  whether caching is actually needed before it's built.
- **No HUD support initially.** The just-planned `plan_hud.md` design
  for native works by pointing a `Cairo.context` directly at the SDL
  window surface's own pixel `Bigarray` -- that trick doesn't transfer
  here, since OpenGL owns the framebuffer on the GPU side, not as a
  plain CPU-side pixel buffer. A GL-backend HUD would need a different
  approach later (render the HUD via Cairo to an offscreen surface,
  upload it as a GL texture, draw it as a screen-aligned textured quad
  in an orthographic pass on top) -- noted as real follow-up work, not
  blocking this plan.

## Phasing

1. Extract the shared SDL event-loop/`computer`-bookkeeping module
   (`Native_loop`, see above) out of `playground3d/native/`; verify no
   behavior change in any existing `examples3d`/`games3d` native demo.
2. Window/GL-context setup + a hard-coded "hello triangle" (no
   `shape3d` integration at all yet) -- prove the `tgls`/`tsdl`/GLSL
   pipeline works end to end (shader compile/link, VAO/VBO, one draw
   call, `gl_swap_window`) before wiring in the real API.
3. `Mat4` module; wire `camera`/`Playground.screen` into `uMVP`; render
   one real `shape3d` scene (a `Cubes3d`-equivalent) with depth test +
   backface culling + the Phong fragment shader.
4. `dune-project`'s `elm_playground_3d_opengl` package stanza;
   `examples3d`/`games3d` dune wiring so at least one existing demo
   (e.g. a `Cubes3d`/`Spheres3d` copy, or a dune `(select ...)`-style
   alternate target) can link this backend instead of `native`.
5. **DONE.** Textures, wireframe, and a written LOC + FPS comparison
   against `native` -- see `notes_playground3d_related_work.md`'s
   "Postscript" section for the actual measured numbers (not just the
   expectation): ~22% less non-comment code, and anywhere from ~6x to
   ~33x faster depending on scene size, with the naive per-frame
   OCaml-side vertex rebuild (not the GPU itself) becoming the
   bottleneck at larger triangle counts -- exactly the kind of
   "measure before optimizing" finding worth having before chasing a
   per-shape GPU buffer cache.

## Verification

- `dune build` after each phase.
- Phase 2: a colored triangle actually appears on screen (screenshot).
- Phase 3: screenshot the same scene/camera angle already used for
  `Cubes3d.exe`/`Spheres3d.exe` on `native`, compare side by side --
  should look recognizably like the same scene (framing, colors,
  relative shading), not necessarily pixel-identical (real Phong vs.
  our per-pixel approximation will look at least subtly different,
  which is itself worth noting).
- Phase 5 (if reached): `wc -l` (or similar) on `playground3d/native/`'s
  rendering-specific code (excluding the now-shared event loop) versus
  `playground3d/opengl/`'s, and an FPS comparison on the same scene --
  write both up, since "how much shorter" was the actual motivating
  question for this whole plan.
