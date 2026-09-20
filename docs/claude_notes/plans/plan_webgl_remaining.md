# Plan: what's left for the WebGL backend

The backend itself is done: see
[`done/plan_webgl.md`](done/plan_webgl.md) (`playground3d/webgl/`,
phases 1-5: the shared `Gpu_scene`, the canvas under `run_app`'s
`<svg>`, real scenes with the rendering hints, textures from `<img>`,
`make serve-build`, publishing) and the shader tutorial,
[`notes_opengl_shaders.md`](notes_opengl_shaders.md). What's left,
roughly from most to least worth doing.

## 1. `Mesh_cache`, then TinyMinecraft in the browser

**DONE**, except its fps: see [`plan_opengl_perf.md`](plan_opengl_perf.md),
Phases 3-4 and Results (headless Chrome renders WebGL in software, so
the fps needs a real browser).

The big one, owned by [`plan_opengl_perf.md`](plan_opengl_perf.md):
its Phase 3 is WebGL's upload/draw/free for
`Mesh_cache` (`Gpu_scene.group_by_material`'s `on_cached`, a WebGL
buffer and vertex count per material of a cached mesh, `deleteBuffer`
in the sweep; and, WebGL 1 having no VAOs, the attribute pointers set
again for each buffer drawn).

Then the WebGL-specific part of TinyMinecraft:

- `games3d/webgl/dune`: `TinyMinecraft` next to `StarCollector3d` (the
  `minecraft_model` library is plain OCaml, it should compile with
  js_of_ocaml as is);
- its texture atlas, `"games3d/minecraft.png"`, copied where the page
  looks for it, `games3d/webgl/games3d/minecraft.png` (a one-rule dune
  file, like `examples3d/webgl/examples3d/dune`), and by `make website`;
- its fps next to OpenGL's and software's. js_of_ocaml-compiled OCaml
  is several times slower than native at `view` and `Gpu_scene`'s list
  code, so the cache matters even more here.

## 2. Debug keys, and with them the untested rendering modes

**DONE**, with 3 and the first two of 4, see the end of 3.

The native backends' `m`/`b`/`i` keys need `-debug-keys`; a page has no
command line, so: `Foo.html?debug-keys`, read from
`window.location.search`, and the backend's own `keydown` listener on
`window` (`Dom_html.addEventListener`), independent of the game's.
Needs the rendering hints to become refs, as in the OpenGL backend.

It's also the way to finally *see* `Flat` (the
`OES_standard_derivatives` path of the fragment shader), `No_lighting`
and culling off on WebGL: never checked on screen so far (see
`done/plan_webgl.md`, Phase 3).

## 3. `?fixed-time=t`, for comparing with the golden frames

Native has `-fixed-time t`: every frame at the same time, so a frame is
deterministic. On the web, only `Corridor3d` (static) could be compared
pixel for pixel with its software golden frame; with a
`?fixed-time=t` page parameter (the Tick's time replaced in
`run_app3d`'s `update2d`/`view2d`), every example could, in headless
Chrome.

**Done** (2, 3, and the no-WebGL message and wireframe of 4), in
`playground3d/webgl/Playground3d_platform.ml`, sections "Page
parameters" and "Debug keys": `?debug-keys` (`m`, `b`, `i`, `f`, `o`,
their state in the page's title), `?keys=k` (native's `-keys k`, the
keys pressed before the first frame, since headless Chrome can't press
any), `?fixed-time=t`. Wireframe draws each triangle's 3 edges as
`LINES`, bypassing the mesh cache (its meshes have only triangles);
lines are never culled, so `b` changes nothing in wireframe. No WebGL:
a message in the page, kept there by `ensure_in_page`, and one console
error.

Verified by screenshotting, through `make serve-build`'s kind of
server, every scene of `tests/3d/Golden_frames.ml` at 1000x1000 with
`?fixed-time=1000&keys=...`, compared with its software golden frame
(pixels differing by more than 24 in some channel):

- filled scenes, including `Spheres3d_m` (no lighting), culling off
  (`Cubes3d_b`) and nearest textures (`TexturedCube3d_i`): 0.08% to
  0.5%, i.e. edge pixels;
- `CachedGrid3d`: 5.3%, all at the edges of its 1600 small cubes (the
  faces' colors match within 1; the GPU's coverage rule fills ~10k more
  edge pixels); the same frame with the cache off (`keys=o`): identical;
- `Spheres3d_mm` (`Flat`, the `OES_standard_derivatives` path): 7%
  from the software golden, but 0.15% from the OpenGL backend's frame
  (`-fixed-time 1000 -keys mm -dump-frame`): the GPU backends' flat
  shading (the true face normal, per pixel) differs from the software
  one, not WebGL from OpenGL;
- wireframe (`_f`): ~6%, lines lit like the faces (as OpenGL's
  polygon mode does) and not culled.

The no-WebGL message was checked with Chrome's `--disable-3d-apis`.

## 4. Smaller

- **No WebGL**: DONE (see 3).
- **Wireframe**: DONE, as `gl.LINES` (see 3).
- **A real browser**: `StarCollector3d` (arrow keys) and
  `TexturedCube3d` (its texture loaded over HTTP) work, through
  `make serve-build`. Not tried there yet: the mouse
  (`InteractiveCube3d`) and `TinyMinecraft` (pointer lock, and its fps).
- **Publishing**: the pages go live only once `next` is merged into
  `master` and `make website` is run there (with a `git add -f` of the
  gitignored `.bc.js`); `OPAMS`/`ODOC_DIRS` in the `Makefile` still
  list none of the 3D packages.
