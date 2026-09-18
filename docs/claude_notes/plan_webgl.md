# Plan: `elm_playground_3d_webgl`, a GPU-accelerated web backend

## Context

`playground3d/` has three backends today, and one hole:

|              | CPU                               | GPU                          |
|--------------|-----------------------------------|------------------------------|
| **native**   | `software/` (from-scratch rasterizer over `graphics/3d/`) | `opengl/` (OpenGL 3.3 via tgls) |
| **web**      | `web/` (3D -> 2D SVG polygons)    | **missing: `webgl/`**        |

`web/` compiles the scene down to `Playground.shape`s every frame
(`Playground3d.render3d_to_2d`: backface culling + a painter's sort + a
plain 2D projection, flat shading from `Lighting`) and hands them to
the unmodified 2D SVG backend. That was a cheap, clever first web
backend, but it has the known weaknesses of a painter's algorithm
(`PaintersAlgorithmFail3d`), no textures (a flat placeholder color),
no smooth shading (`Smooth` is `Flat` there), it drops triangles
crossing the near plane (`plan_3d_remaining.md`: `Corridor3d` in a
browser), and it creates one SVG `<polygon>` per visible triangle per
frame, hopeless for `Minecraft3d`-sized scenes.

`webgl/` would be to `web/` what `opengl/` is to `software/`: same
`shape3d`/`camera` API and `?rendering` hints, unmodified example/game
sources, a real GPU doing the z-buffer, culling, near-plane clipping,
rasterization and per-pixel lighting. It would also make the published
`docs/examples3d/` pages (and a future `docs/games3d/`) look like the
OpenGL version instead of a flat-shaded SVG approximation.

## How hard? Short answer: medium-small, less than half of `opengl/`

Most of the hard thinking was done for `opengl/` (`done/plan_opengl.md`,
`notes_opengl.md`), and WebGL 1 is essentially OpenGL ES 2.0, the same
programmable pipeline. And the backend-independent code is now already
shared, outside any backend:

- `graphics/3d/geometry/` (library `graphics_3d_geometry`, no
  dependencies, pure OCaml, so it compiles with js_of_ocaml as is):
  `Vec3`, `Mat4` (`look_at`, `perspective`, `mul`), `Lighting`
  (`light_dir`, `ambient`), `Camera`.
- `playground3d/Gpu_scene.ml` (in `elm_playground_3d` itself): the
  `shape3d` -> per-material vertex lists flattening
  (`group_by_material`, `vertex_floats_of_group`, `floats_per_vertex`).
  It lives in the virtual library because it matches on
  `Playground3d.shape3d`, and a separate helper library depending on
  the virtual `elm_playground_3d` would give each backend a second path
  to it, which dune rejects (see `Native_loop.mli`).

What's left in `playground3d/opengl/Playground3d_platform.ml` (418
lines) is only what talks to OpenGL: shaders, texture upload, the
buffer/attribute setup, the rendering-hint toggles, and the SDL
window/context via `Native_loop`. The first four translate nearly
1-for-1 to js_of_ocaml's `WebGL` module (`gl##createShader`,
`gl##bufferData`, `gl##drawArrays`, ...), modulo the WebGL 1 gotchas
below; `Native_loop` is replaced by the 2D web backend's own loop (see
"Event loop").

Estimated new code: ~250-300 lines (backend) + dune/html/Makefile
wiring. Nothing here looks risky except texture loading (async, CORS)
and wireframe (no `glPolygonMode` in WebGL), both deferrable.

## Dependency: none new

`js_of_ocaml` 5.8.2 (already installed, already a transitive dep of
the web backends) ships a `WebGL` module (`js_of_ocaml/webGL.mli`):
`WebGL.getContext : Dom_html.canvasElement t -> renderingContext t opt`,
`getExtension`, plus `Typed_array.float32Array` for vertex data. It is
**WebGL 1 only** (no `webgl2` context binding). That's enough for
everything `opengl/` does, with one extension (see the gotchas:
`OES_standard_derivatives`, for flat shading). `brr` (Bünzli's newer
browser bindings, with WebGL2) is not installed and not needed.

One interop wrinkle: the existing web code uses vdom's `Js_browser`
(gen_js_api, `Ojs.t`), while `WebGL` uses js_of_ocaml's `Js.t`
objects. Both are raw JS values at runtime; simplest is for the new
backend to use only `Js_of_ocaml` (`Dom_html` for the canvas, `WebGL`,
`Typed_array`) and never touch `Js_browser` itself.

## Naming and layout

- `playground3d/webgl/Playground3d_platform.ml`, library/package
  `elm_playground_3d_webgl`, `(implements elm_playground_3d)`, depending
  on `elm_playground_web` (for its `run_app`, like `web/`),
  `graphics_3d_geometry` (`Mat4`, `Lighting`) and `js_of_ocaml`. Not on
  `graphics_images` (stb_image + curl, native only): the browser decodes
  images itself.
- `examples3d/webgl/` and `games3d/webgl/` with `(copy_files ../Foo.ml)`
  + `.html` pages, exactly like `examples3d/js/` (which keeps that name:
  `js/` = the SVG web backend, `webgl/` = this one).
- A new `(package (name elm_playground_3d_webgl) ...)` stanza in
  `dune-project` (regenerates the `.opam` file), plus the `Makefile`'s
  `OPAMS`/`ODOC_DIRS` lists.

## Event loop: piggyback on the 2D web backend (zero changes to it)

`web/Playground3d_platform.ml` already turns an `app3d` into a
`Playground.game` and hands it to `Playground_platform.run_app`, which
provides the `requestAnimationFrame` loop, the fixed-timestep 60Hz
Ticks, the key/mouse listeners on `window`, and the
`Playground.computer` bookkeeping (`Playground.game_update`). The
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
  the existing SVG renderer, something `opengl/` still can't do
  (`plan_3d_remaining.md`: "The OpenGL backend has no HUD").
- **Mouse coordinates for free.** The 2D backend's `adjust_x_y` asks
  the root `<svg>` (via `getScreenCTM`) to convert client coordinates
  to playground coordinates. Since the `<svg>` still covers the whole
  window, that keeps working unchanged.

Details to get right:

- **The canvas must survive the first frame.** `run_app` does
  `Element.remove_all_children body` before inserting the first
  `<svg>`, and the first `view2d` call happens just before that. So
  either (a) create the canvas lazily and re-append it if it has no
  parent (`canvas##.parentNode` is null), a one-line check per frame,
  or (b) append it to `document.documentElement` rather than `<body>`.
  (a) is less surprising.
- **Same framing as the SVG layer.** The `<svg>` uses
  `viewBox` = `Playground.default_width x default_height` stretched to
  100%/100% with the default `preserveAspectRatio="xMidYMid meet"`,
  i.e. letterboxed and centered. The canvas should be full-window too
  (`width`/`height` = `window.innerWidth/innerHeight * devicePixelRatio`,
  re-checked each frame to follow resizes) with `gl##viewport` set to
  the same centered, aspect-preserving rectangle, so the HUD and the
  3D scene line up, and the aspect passed to `Mat4.perspective` is the
  screen's (`computer.screen`), like `opengl/`.
- **Stacking order:** canvas `z-index: 0`, svg on top; the svg keeps
  receiving mouse events (it's the element under the pointer).
- **`?rendering`**: `run_app3d ?rendering` sets the starting shading,
  culling and texture filtering (see "Rendering hints" below); the 2D
  `run_app ?rendering` (antialiasing, smooth_images) only affects the
  HUD layer, so the default is fine there.

Alternative, only if the side-effect-in-view trick turns out to be too
clever: extract the rAF/Tick/listeners part of
`playground/web/Playground_platform.ml`'s `run_app` into a `Web_loop`
module parameterized over a `draw` callback, the web twin of
`Native_loop`. More code motion in a file shared with every 2D web
game (and actively worked on), so not the first choice.

## Rendering hints (`Playground3d.rendering`)

`opengl/` honors all three hints, and they all map to WebGL 1:

- `shading`: the same `uShading` int uniform (0 no lighting, 1 flat,
  2 smooth). `Flat` uses `dFdx`/`dFdy` of the position to get the
  face normal in the fragment shader, which in WebGL 1 needs the
  `OES_standard_derivatives` extension (`gl##getExtension`, plus
  `#extension GL_OES_standard_derivatives : enable` at the top of the
  shader). Supported essentially everywhere; if it's missing, fall
  back to `Smooth` for `Flat`, which only differs on curved shapes.
- `backface_culling`: `gl##enable`/`gl##disable CULL_FACE_` each frame,
  same as `opengl/`.
- `smooth_textures`: `LINEAR` vs `NEAREST` texture filter, same as
  `opengl/`; both fine on non-power-of-two textures in WebGL 1 as long
  as there are no mipmaps and the wrap mode is `CLAMP_TO_EDGE`, which
  is what `opengl/` does.

**Debug keys** ("m" shading, "b" culling, "i" filtering, "f" wireframe
on `opengl/`): the native backends only enable them with
`-debug-keys` (`Native_loop.debug_keys_enabled`). The web has no
command line; the equivalent is a URL query, `Foo.html?debug-keys`,
read from `window.location.search`. The keys then come from the
backend's own `keydown` listener on `window`
(`Dom_html.addEventListener`), independent of the game's. Optional,
v2.

## Rendering: `opengl/` -> WebGL 1 differences (the actual gotchas)

| `opengl/` (GL 3.3 core) | `webgl/` (WebGL 1 = GLES 2.0) |
|---|---|
| `uniform_matrix4fv ... true` (GL transposes our row-major `Mat4.t`) | **`transpose` must be `false` in WebGL 1** (`INVALID_VALUE` otherwise): transpose on the CPU before upload. A `Mat4.transpose` would be the natural home (with a `Unit_` test in `graphics/tests/`) |
| VAO (`gen_vertex_arrays`) | No VAO in core WebGL 1 (only via `OES_vertex_array_object`). Not needed: one VBO with a fixed layout, bind it and set the 4 `vertexAttribPointer`s once |
| GLSL `#version 330 core`, `in`/`out`, `FragColor`, `texture()` | GLSL ES 1.00: `attribute`/`varying`, `gl_FragColor`, `texture2D()`, and `precision mediump float;` required in the fragment shader (maybe `highp`, see Verification) |
| `layout (location = N)` | Not in GLSL ES 1.00: `bindAttribLocation` before linking, or `getAttribLocation` after |
| `dFdx`/`dFdy` (core in GLSL 3.30) for `Flat` | Needs `OES_standard_derivatives`, see above |
| `uniform bool`, `uniform int`, `?:` in GLSL | Same in GLSL ES 1.00 |
| `const float ambient = 0.25` in the shader | Same literal; or upload `Lighting.ambient` as a uniform, like `light_dir` already is, so no backend retypes a lighting constant |
| `Gl.polygon_mode ... Gl.line` (wireframe, one line) | **No `polygonMode` in WebGL.** Wireframe needs a per-frame `gl.LINES` vertex list (3 edges per triangle) or the barycentric fragment shader trick. Deferred |
| `Bigarray.float32` -> `Gl.buffer_data` | A `Typed_array.float32Array` (`new%js Typed_array.float32Array n` + a fill loop from `Gpu_scene.vertex_floats_of_group`'s array) -> `gl##bufferData` |
| `Gc.full_major ()` workaround for the tgls `glShaderSource` race | Not applicable (no ctypes); omit |
| Depth test, culling, perspective-correct interpolation, near-plane clipping | Identical, all in hardware. Ask for a depth buffer explicitly in `getContextWithAttributes` (`depth: true`, the default, but worth being explicit about) |
| `drawArrays`, no indices | Same, so WebGL 1's 16-bit `drawElements` index limit is irrelevant |

## Textures (the one genuinely different part)

`opengl/` loads textures with `graphics/images/Texture_decode`
(stb_image + curl, synchronous). In the browser, image loading is
**asynchronous**:

- `get_or_create_gl_texture src`: the first call creates the GL texture
  filled with the same 1x1 magenta "missing" pixel, starts
  `let img = new Image(); img.src = src`, and on `img.onload` uploads
  it with `gl##texImage2D_fromImage`, replacing the placeholder. The
  next frame (games redraw at 60Hz anyway) shows the real texture.
- `preload_texture src` = the same, called earlier, the web twin of
  `Playground_platform.preload_image`.
- Row order: `texImage2D` from an `<img>` puts the image's top row at
  v=0 by default (`UNPACK_FLIP_Y_WEBGL` false), same as `opengl/`'s
  upload, so the same "no v-flip" conclusion should hold. Verify with
  `TexturedCube3d`'s checker, like `opengl/` did.
- **Two practical traps:**
  - *CORS / `file://`*: WebGL refuses to upload a cross-origin image
    (tainted), and Chrome treats `file://` pages as cross-origin even
    for sibling files. So textured examples must be served over HTTP
    (`python3 -m http.server` in `_build/default/...`, or GitHub
    Pages). Untextured examples still work from `file://`. An `http(s)`
    texture URL from another site needs CORS headers on that site and
    `img.crossOrigin = "anonymous"`.
  - *Paths*: examples use repo-root-relative paths
    (`"examples3d/checker.png"`, `"games3d/texture.png"`), which on
    the web resolve relative to the `.html` page. The dune rule that
    copies the `.html` pages should also copy the images to e.g.
    `examples3d/webgl/examples3d/checker.png`, and `make website`
    likewise under `docs/`.

## Performance: follows `plan_opengl_perf.md`

`webgl/` rebuilds and re-uploads the whole scene every frame, like
`opengl/` today. `plan_opengl_perf.md` diagnosed that for Minecraft3d
the bottleneck is this CPU-side rebuild (`view` + `Gpu_scene`), not
the GPU, and proposes `cached3d` with a GPU mesh cache in `Gpu_scene`,
parameterized over the backend's upload/free functions so `webgl/`
gets it for free. Two consequences here:

- js_of_ocaml-compiled OCaml is several times slower than native at
  this allocation-heavy list code, so the naive rebuild will hit its
  limit on smaller scenes than on `opengl/`. Fine for the `examples3d`
  scenes and StarCollector3d; Minecraft3d on WebGL only makes sense
  after `plan_opengl_perf.md`'s Phase 1-2.
- Whichever of the two plans is done second should use the other's
  `Gpu_scene` interface: keep the WebGL-specific code to "upload a
  float array, draw it", so a mesh cache slots in without a rewrite.

## Scope for v1 (simplifications, stated up front)

- The three `shading` modes, `backface_culling`, `smooth_textures`: yes
  (the starting values from `?rendering`); the debug keys: v2.
- Naive per-frame rebuild + upload (see Performance).
- HUD: yes (free, via the SVG overlay).
- Textures: Phase 4 (the magenta placeholder before that).
- Wireframe: out of v1 (no `polygonMode`).
- WebGL unavailable (very old browser, some headless setups):
  `WebGL.getContext` returns null -> show an error in the page (a `<p>`
  in the body) rather than a blank page. An automatic fallback to the
  SVG backend would need both backends linked in one executable, which
  the virtual-module setup doesn't allow; not worth it.

## Phasing

1. **DONE.** `Gpu_scene` extracted out of `opengl/` into
   `elm_playground_3d`, and (by the 3D reorg, see
   `done/plan_code_reorg_teaching_3d.md`) the matrices and lighting
   into `graphics/3d/geometry/` (`Mat4`, `Lighting`, `Vec3`), so the
   whole backend-independent part is already shared.
2. **DONE.** **Skeleton + hello triangle**: `playground3d/webgl/` library, the
   `view2d` side-effect wiring, canvas creation/sizing, context
   creation, shader compile/link with the error log (same
   `get*Parameter` + `get*InfoLog` checks as `opengl/`, just as
   important here: a failed shader is otherwise a silent blank
   canvas), one hardcoded triangle. `examples3d/webgl/dune` + one
   `.html`. Proves the jsoo/WebGL pipeline end to end.
   Verified with headless Chrome screenshots (`--use-angle=swiftshader
   --enable-unsafe-swiftshader`) at 800x600 and 600x900: the triangle
   with its interpolated colors, letterboxed and centered like the
   `<svg>`, undistorted; `--dump-dom` shows the canvas survives
   `run_app`'s first-frame `<body>` reset, next to the `<svg>`. The
   canvas's drawing-buffer size follows its `clientWidth/Height` (its
   laid-out size) rather than `window.innerWidth/Height`. The
   `elm_playground_3d_webgl` package stanza was needed now already
   (dune wants a package for a `public_name`); the `Makefile` lists
   (`OPAMS`, `ODOC_DIRS`, `js`, `website`) stay for Phase 5. No-WebGL
   is a `failwith` for now (in the console), not yet a message in the
   page.
3. **DONE.** **Real scenes**: `Gpu_scene` -> `Float32Array` -> `drawArrays`,
   the transposed `Mat4` MVP, depth test, the `rendering` hints and
   `OES_standard_derivatives`. First `examples3d/webgl/Triangle3d`
   (Phase 2's page, a WebGL-only example: one orange triangle facing
   the camera, which the Phase 2 backend ignores to draw its hardcoded
   one), then, via `copy_files`, `Cube3d`, `Cubes3d`, `Spheres3d`,
   `PaintersAlgorithmFail3d` (now drawn *correctly*, a nice demo of
   why a z-buffer matters), `Corridor3d` (now clipped correctly, unlike
   `web/`), `FloatingCity3d`, `InteractiveCube3d` (mouse), and
   `StarCollector3d` with its HUD.
   Done as planned: `Mat4.transpose` (+ a test in `Unit_vec3`), the
   OpenGL shaders in GLSL ES 1.00 (with `highp` when available,
   `uAmbient` a uniform set from `Lighting.ambient`, and the `#extension`
   line and the `dFdx` flat normal only when `getExtension` finds
   `OES_standard_derivatives`), the attribute pointers set once at
   init (one buffer, no VAO needed), textured faces drawn with a 1x1
   magenta placeholder texture, `games3d/webgl/` for StarCollector3d.
   Verified with headless Chrome screenshots of every page (no console
   error): `Corridor3d`, the one static scene, matches the software
   backend's golden frame (framing, wall lighting, stripes), which
   checks the transpose, the letterboxing and the lighting together;
   the depth test fixes `PaintersAlgorithmFail3d`; StarCollector3d's
   score shows top-left. Not checked yet: the `Flat` and
   `No_lighting` modes and culling off (no way to pick them without
   the debug keys or a `?rendering` in an example), and keyboard/mouse
   input in a real browser.
4. **DONE.** **Textures**: async `Image` loading, `TexturedCube3d`.
   Rather than uploading from `img.onload`, two caches: `images` (src
   -> `<img>`, download started by `preload_texture` or the first
   draw, usable before the GL context exists) and the GL textures
   (magenta until the frame after the image is `complete`, then
   uploaded once). A failed load (404) or a failed upload (the
   `SecurityError` of a `file://` page, as predicted) keeps the
   magenta and warns once in the console. `crossOrigin = "anonymous"`
   only for `http(s)` srcs. The path problem is solved by a
   one-rule `examples3d/webgl/examples3d/dune` copying `checker.png`
   where the page looks for it (a dune rule can only write in its own
   directory), built by the html's default alias. Verified with
   headless Chrome through `python3 -m http.server`: the checker's
   quadrants are where the OpenGL backend has them (no v-flip,
   confirmed); from `file://`, magenta plus the one warning.
   Phase 5 must copy that `examples3d/checker.png` under `docs/` too.
   `make serve-build` builds and serves `_build/default/` on
   http://localhost:8001/, for trying the web pages locally (textures
   included).
5. **Publish**: `dune-project` package stanza, `Makefile`
   (`OPAMS`, `ODOC_DIRS`, the `js`/`website` targets copying
   `examples3d/webgl/` under `docs/`), links from the docs index;
   README-3d.md mentions the 4th backend.
6. (Later) debug keys via `?debug-keys`; wireframe via a LINES pass;
   `games3d/webgl/Minecraft3d` once `plan_opengl_perf.md`'s cache
   exists, with its fps next to OpenGL's and software's.

## Verification

- `dune build` (and `dune build examples3d/webgl --profile=release-js`)
  after each phase; `make test` stays green (the golden frames only
  cover the software backends, so they can't test this one, but they
  do catch an accidental change to shared code like `Gpu_scene` or
  `Mat4`).
- Headless Chrome (`note_headless.md`): `--dump-dom` no longer shows
  the picture (it's pixels in a `<canvas>` now, only the HUD stays in
  the DOM), so use `--screenshot`, with WebGL enabled in headless mode
  (`--use-angle=swiftshader` / `--enable-unsafe-swiftshader`, software
  GL: fine for correctness, meaningless for fps). `web_headless.js`
  (node + fake DOM) can't help: no WebGL in its fake DOM.
- Side-by-side screenshots against `opengl/` on the same examples,
  e.g. with a fixed time (the native `-fixed-time t` flag; the web page
  would need the same, e.g. `?fixed-time=t`, for a meaningful
  comparison): should be nearly pixel-identical (same `Mat4`, same
  shader math, same `Lighting` constants); differences would point at
  the matrix transpose or at `mediump` precision. If `mediump` shows
  banding, switch the fragment shader to `highp`.
- A real browser (not just headless) for fps and input: keys and
  mouse in `InteractiveCube3d`/`StarCollector3d`.
