# Plan: `elm_playground_3d_webgl`, a GPU-accelerated web backend

## Context

`playground3d/` has three backends today, and one hole:

|              | CPU                          | GPU                          |
|--------------|------------------------------|------------------------------|
| **native**   | `native/` (software rasterizer) | `opengl/` (OpenGL 3.3 via tgls) |
| **web**      | `web/` (3D -> 2D SVG polygons)  | **missing: `webgl/`**        |

`web/` compiles the scene down to `Playground.shape`s every frame
(`Playground3d.render3d_to_2d`: backface culling + a painter's sort + a
plain 2D projection) and hands them to the unmodified 2D SVG backend.
That was a cheap, clever first web backend, but it has the known
weaknesses of a painter's algorithm (`PaintersAlgorithmFail3d`), no
textures (a flat placeholder color), no per-pixel lighting, and it
creates one SVG `<polygon>` per visible triangle per frame, which is
hopeless for `Minecraft3d`-sized scenes.

`webgl/` would be to `web/` what `opengl/` is to `native/`: same
`shape3d`/`camera` API, unmodified example/game sources, a real GPU
doing the z-buffer/culling/rasterization/Phong. It would also make the
published `docs/examples3d/` pages (and a future `docs/games3d/`,
e.g. Minecraft3d in the browser) look like the native OpenGL version
instead of like a flat-shaded SVG approximation.

## How hard? Short answer: medium-small, about half of `opengl/`

Most of the hard thinking was already done for `opengl/`
(`done/plan_opengl.md`), and WebGL 1 is essentially OpenGL ES 2.0,
i.e. the same programmable pipeline the OpenGL backend already uses.
Of the ~585 lines of `playground3d/opengl/Playground3d_platform.ml`:

- **~200 lines are pure OCaml with no GL call at all** and carry over
  verbatim: `Mat4` (`look_at`, `perspective`, `mat4_mul`), the
  `shape3d` -> per-material vertex list flattening (`collect_batches`,
  `fan_triangles`, `group_by_material`, `vertex_floats_of_group`),
  `light_dir`, `rgb_of_color`. See Phase 1: share them instead of
  copying them.
- **~150 lines are GL calls** that translate nearly 1-for-1 to
  js_of_ocaml's `WebGL` module (`gl##createShader`, `gl##bufferData`,
  `gl##drawArrays`, ...), modulo the WebGL 1 gotchas listed below.
- **The SDL event loop (`Native_loop`) does not carry over**, but the
  web side already has its own loop in `playground/web/`, and there is
  a way to reuse it with zero changes (see "Event loop" below).

Estimated new code: ~250-350 lines (backend) + dune/html/Makefile
wiring. Nothing here looks risky except texture loading (async, CORS)
and wireframe (no `glPolygonMode` in WebGL), both of which can be
deferred without blocking anything.

## Dependency: none new

`js_of_ocaml` 5.8.2 (already installed, already a transitive dep of
the web backends) ships a `WebGL` module (`js_of_ocaml/webGL.mli`):
`WebGL.getContext : Dom_html.canvasElement t -> renderingContext t opt`,
plus `Typed_array.float32Array` for vertex data. It is **WebGL 1
only** (no `webgl2` context binding), which is fine: everything
`opengl/` does fits in WebGL 1 (see gotchas). `brr` (Bünzli's newer
browser bindings, with WebGL2 support) is not installed and not
needed.

One interop wrinkle: the existing web code uses vdom's `Js_browser`
(gen_js_api, `Ojs.t`), while `WebGL` uses js_of_ocaml's `Js.t`
objects. Both are raw JS values at runtime, so crossing between them
is a `Js.Unsafe.coerce`/`Obj.magic` at the one or two places it's
needed -- or, simpler, the new backend uses only `Js_of_ocaml`
(`Dom_html` for the canvas, `WebGL`, `Typed_array`) and never touches
`Js_browser` itself.

## Naming and layout

- `playground3d/webgl/Playground3d_platform.ml`, library/package
  `elm_playground_3d_webgl`, `(implements elm_playground_3d)` --
  mirrors `native/`/`opengl/`/`web/`, named after the technology
  like `opengl/`.
- `examples3d/webgl/` and `games3d/webgl/` with `(copy_files ../Foo.ml)`
  + `.html` pages, exactly like `examples3d/js/` (which should probably
  keep that name: `js/` = the SVG web backend, `webgl/` = this one).
- A new `(package (name elm_playground_3d_webgl) ...)` stanza in
  `dune-project` (regenerates the `.opam` file).

## Phase 1: extract the backend-independent GPU scene prep

Same move as `Native_loop` was for `opengl/`: a second real caller is
exactly when factoring out becomes worth it.

Move the pure-OCaml half of `opengl/Playground3d_platform.ml` (Mat4,
`material`, `vertex_data`, `collect_batches`, `group_by_material`,
`vertex_floats_of_group`, `floats_per_vertex`, `light_dir`,
`rgb_of_color`) into a new ordinary module, e.g.
`playground3d/Gpu_scene.ml`/`.mli`, **inside the `elm_playground_3d`
library itself** (next to `Playground3d.ml`, not in a separate
library). Rationale: it pattern-matches on `Playground3d.shape3d`, and
`Native_loop.mli`'s comment explains why a separate helper library
can't depend on the virtual `elm_playground_3d` (dune rejects a second
path to it). A virtual library can have normal, non-virtual modules
that its implementations use, so that's the natural home.

The GLSL sources could also be shared, but GLSL 3.30 core and GLSL ES
1.00 differ in enough surface syntax (`in`/`out` vs
`attribute`/`varying`, `FragColor` vs `gl_FragColor`, `texture` vs
`texture2D`, a mandatory `precision mediump float;`) that two short
literal strings side by side are clearer than a templating scheme. The
*lighting formula* (ambient 0.25, same light_dir uniform) stays
identical, which is what matters for a fair comparison.

Verify: `opengl/` still builds and `Cubes3d`/`TexturedCube3d` on
OpenGL render the same as before (screenshot diff).

## Event loop: piggyback on the 2D web backend (zero changes to it)

`web/Playground3d_platform.ml` already turns an `app3d` into a
`Playground.game` and hands it to `Playground_platform.run_app`, which
provides the `requestAnimationFrame` loop, the fixed-timestep 60Hz
Ticks, the key/mouse listeners on `window`, and the
`Playground.computer` bookkeeping (in `Playground.game_update`). The
WebGL backend can reuse all of it the same way, with one twist: its
`view2d` draws the 3D scene into a WebGL `<canvas>` **as a side effect**
and returns only the HUD shapes:

```ocaml
let view2d (computer : Playground.computer) (model : 'model) : Playground.shape list =
  let (cam, shapes) = Playground3d.view3d app3d computer model in
  draw_gl (Lazy.force gl_state) computer cam shapes;   (* WebGL draw *)
  Playground3d.collect_hud_shapes (Playground3d.group3d shapes)  (* SVG overlay *)
```

This works because `run_app`'s `animation_frame` calls `app.view`
exactly once per frame, after the Ticks. The page then has two
layers: a full-window `<canvas>` (WebGL) underneath, and the existing
`position: fixed` full-window `<svg>` on top, which has no background
so it is transparent except where HUD shapes are drawn.

Payoffs of this layering, beyond "no new loop to write":

- **HUD for free.** `StarCollector3d`'s score works on day one, via
  the existing SVG renderer -- something `opengl/` still can't do (it
  would need the Cairo -> texture -> screen-quad pass sketched in
  `done/plan_opengl.md`).
- **Mouse coordinates for free.** The 2D backend's `adjust_x_y` asks
  the root `<svg>` (via `getScreenCTM`) to convert client coordinates
  to playground coordinates. Since the `<svg>` still covers the whole
  window, that keeps working unchanged.

Details to get right:

- **The canvas must survive the first frame.** `run_app` does
  `Element.remove_all_children body` before inserting the first
  `<svg>`, and the first `view2d` call happens just before that. So
  either (a) create the canvas lazily and re-append it if it has no
  parent (`canvas##.parentNode` is null) -- a one-line check per
  frame -- or (b) append it to `document.documentElement` rather than
  `<body>`. (a) is less surprising.
- **Same framing as the SVG layer.** The `<svg>` uses
  `viewBox` = `Playground.default_width x default_height` stretched to
  100%/100% with the default `preserveAspectRatio="xMidYMid meet"`,
  i.e. letterboxed and centered. The canvas should be full-window too
  (`width`/`height` = `window.innerWidth/innerHeight * devicePixelRatio`,
  re-checked each frame to follow resizes) with `gl##viewport` set to
  the same centered, aspect-preserving rectangle, so the HUD and the
  3D scene line up, and the aspect passed to `perspective` is the
  screen's (`computer.screen`), like `opengl/`.
- **Stacking order:** canvas `z-index: 0`, svg on top; the svg keeps
  receiving mouse events (it's the element under the pointer).
- **Backend-local keys** ("f" for wireframe, if/when added): the 2D
  loop doesn't expose raw key events, but the backend can add its own
  `keydown` listener on `window` (`Dom_html.addEventListener`),
  independent of the game's.

Alternative, only if the side-effect-in-view trick turns out to be too
clever: extract the rAF/Tick/listeners part of
`playground/web/Playground_platform.ml`'s `run_app` into a `Web_loop`
module parameterized over a `draw` callback, the web twin of
`Native_loop`. More code motion in a file shared with every 2D web
game, so not the first choice.

## Rendering: `opengl/` -> WebGL 1 differences (the actual gotchas)

| `opengl/` (GL 3.3 core) | `webgl/` (WebGL 1 = GLES 2.0) |
|---|---|
| `uniform_matrix4fv ... true` (GL transposes our row-major Mat4) | **`transpose` must be `false` in WebGL 1** (`INVALID_VALUE` otherwise): transpose on the CPU before upload, or have `Gpu_scene` produce column-major directly |
| VAO (`gen_vertex_arrays`) | No VAO in core WebGL 1 (only via `OES_vertex_array_object`). Not needed: there is one VBO with a fixed layout, so bind it and set the 4 `vertexAttribPointer`s once (or once per frame, cheap) |
| GLSL `#version 330 core`, `in`/`out`, `FragColor`, `texture()` | GLSL ES 1.00: `attribute`/`varying`, `gl_FragColor`, `texture2D()`, and `precision mediump float;` required in the fragment shader |
| `layout (location = N)` | Not in GLSL ES 1.00: `bindAttribLocation` before linking, or `getAttribLocation` after |
| `Gl.polygon_mode ... Gl.line` (wireframe, one line) | **No `polygonMode` in WebGL.** Wireframe needs either a second, per-frame-built `gl.LINES` vertex list (3 edges per triangle) or the barycentric-coordinate fragment shader trick. Deferred (see Scope) |
| `Bigarray.float32` -> `Gl.buffer_data` | Build a `Typed_array.float32Array` (e.g. `new%js Typed_array.float32Array n` + a fill loop, or `Typed_array.float32Array_fromArray`) -> `gl##bufferData` |
| `Gc.full_major ()` workaround for the tgls `glShaderSource` race | Not applicable (no ctypes); delete |
| Depth test, backface culling, perspective-correct interpolation | Identical: `gl##enable gl##._DEPTH_TEST_`, `gl##enable gl##._CULL_FACE_`. Ask for a depth buffer explicitly in `getContextWithAttributes` (`depth: true`, which is the default but worth being explicit about) |
| Uses 32-bit indices? | No: `opengl/` uses `drawArrays`, not indexed draws, so WebGL 1's 16-bit `drawElements` index limit is irrelevant |

## Textures (Phase 4, the one genuinely different part)

`opengl/` reuses `Texture_native` (stb_image + curl, synchronous). In
the browser, image loading is **asynchronous**:

- `get_or_create_gl_texture src`: first call creates the GL texture
  filled with the same 1x1 magenta "missing" pixel, starts
  `let img = new Image(); img.src = src`, and on `img.onload` uploads
  it with `gl##texImage2D_fromImage`, replacing the placeholder. The
  next frame (games redraw at 60Hz anyway) shows the real texture.
- `preload_texture src` = the same, called earlier -- the web twin of
  `Playground_platform.preload_image`.
- Non-power-of-two textures work in WebGL 1 only with
  `CLAMP_TO_EDGE` and no mipmaps; `opengl/` already uses exactly
  `NEAREST` + `CLAMP_TO_EDGE`, so no change.
- Row order: `texImage2D` from an `<img>` puts the image's top row at
  v=0 by default (`UNPACK_FLIP_Y_WEBGL` false), same as `opengl/`'s
  stb_image upload, so the same "no v-flip" conclusion should hold.
  Verify with `TexturedCube3d`'s checker, like `opengl/` did.
- **Two practical traps:**
  - *CORS / `file://`*: WebGL refuses to upload a cross-origin image
    (tainted), and Chrome treats `file://` pages as cross-origin even
    for sibling files. So textured examples must be served over HTTP
    (`python3 -m http.server` in `_build/default/...`, or GitHub
    Pages). Untextured examples still work from `file://`. An
    `http(s)` texture URL from another site needs CORS headers on that
    site and `img.crossOrigin = "anonymous"`.
  - *Paths*: examples use repo-root-relative paths
    (`"examples3d/checker.png"`, `"games3d/texture.png"`), which on
    the web resolve relative to the `.html` page. The dune rule that
    copies the `.html` pages should also copy the images to e.g.
    `examples3d/webgl/examples3d/checker.png`, and `make website`
    likewise under `docs/`.

## Scope for v1 (simplifications, stated up front)

- One shading mode: per-pixel Phong, same as `opengl/`.
- Naive per-frame re-flatten + re-upload of the whole scene, same as
  `opengl/`. **Expect this to be the bottleneck sooner than on native**:
  `collect_batches`/`vertex_floats_of_group` are allocation-heavy list
  code, and js_of_ocaml-compiled OCaml is several times slower than
  native OCaml at that. `opengl/`'s own postscript in
  `notes_playground3d_related_work.md` already found the OCaml-side
  rebuild, not the GPU, to be the limit at large triangle counts.
  Measure (e.g. `Minecraft3d` on WebGL vs OpenGL) before building a
  per-shape GPU buffer cache.
- HUD: yes (free, via the SVG overlay).
- Textures: Phase 4 (placeholder color before that, like `web/`).
- Wireframe: out of v1 (no `polygonMode`); maybe later via a LINES pass.
- No `"b"`/`"z"`/`"p"`/`"m"` debug toggles (same reasons as `opengl/`).
- WebGL unavailable (very old browser, some headless setups):
  `WebGL.getContext` returns null -> log an error in the page (a
  one-line `<p>` in the body) rather than a blank page. Falling back
  to the SVG backend automatically would need both backends linked in
  one executable, which the virtual-module setup doesn't allow; not
  worth it.

## Phasing

1. **DONE.** **Extract `Gpu_scene`** out of `opengl/` into
   `elm_playground_3d` (see above). No behavior change; verify OpenGL
   screenshots. Done as a verbatim move (checked by diffing the moved
   lines against the old file: only 3 comments that said "below"/"this
   backend" changed). `Gpu_scene.mli` exports only what a backend
   uses (`look_at`, `perspective`, `mat4_mul`, `material`,
   `vertex_data`, `group_by_material`, `floats_per_vertex`,
   `vertex_floats_of_group`, `light_dir`); `collect_batches`,
   `fan_triangles`, the vec3 helpers etc. stay private. OpenGL
   `TexturedCube3d`, `Spheres3d`, `StarCollector3d` screenshots render
   as before.
2. **Skeleton + hello triangle**: `playground3d/webgl/` library, the
   `view2d` side-effect wiring, canvas creation/sizing, context
   creation, shader compile/link with error log (same `get*Parameter`
   + `get*InfoLog` checks as `opengl/`, just as important here -- a
   failed shader is a silent black canvas otherwise), one hardcoded
   triangle. `examples3d/webgl/dune` + one `.html`. Proves the
   jsoo/WebGL pipeline end to end.
3. **Real scenes**: `Gpu_scene` flattening -> `Float32Array` ->
   `drawArrays`, MVP uniform (transposed on the CPU), depth test +
   culling, Phong. `Cube3d`, `Cubes3d`, `Spheres3d`,
   `PaintersAlgorithmFail3d` (should now render *correctly*, which is
   a nice demo of why a z-buffer matters), `FloatingCity3d`,
   `InteractiveCube3d` (mouse), and `StarCollector3d` with its HUD.
4. **Textures**: async `Image` loading, `TexturedCube3d`, then
   `Minecraft3d` in a new `games3d/webgl/`. FPS number for Minecraft3d
   on WebGL vs OpenGL vs native, written up next to the existing
   numbers in `notes_playground3d_related_work.md`'s postscript.
5. **Publish**: `dune-project` package stanza, Makefile `js`/`website`
   targets copy `examples3d/webgl/` (and `games3d/webgl/`) under
   `docs/`, link them from the docs index; README-3d.md mentions the
   4th backend.
6. (Optional) wireframe via a LINES pass; per-shape buffer caching if
   Phase 4's numbers say it's needed.

## Verification

- `dune build` (and `dune build examples3d/webgl --profile=release-js`)
  after each phase.
- Headless Chrome (`note_headless.md`): `--dump-dom` no longer shows
  the picture (it's pixels in a `<canvas>` now, only the HUD stays in
  the DOM), so use `--screenshot`, with WebGL enabled in headless mode
  (`--use-angle=swiftshader` / `--enable-unsafe-swiftshader`, software
  GL, so fine for correctness, meaningless for FPS). `web_headless.js`
  (node + fake DOM) cannot help here: no WebGL in its fake DOM.
- Side-by-side screenshots against `opengl/` on the same examples:
  should be nearly pixel-identical (same Mat4, same shader math, same
  lighting constants); differences would point at a matrix transpose
  or precision (`mediump`) issue. If `mediump` shows banding, switch
  the fragment shader to `highp`.
- A real browser (not just headless) for FPS and for input: keys and
  mouse in `InteractiveCube3d`/`StarCollector3d`/`Minecraft3d`.
