# Plan: what's left for the WebGL backend

The backend itself is done: see
[`done/plan_webgl.md`](done/plan_webgl.md) (`playground3d/webgl/`,
phases 1-5: the shared `Gpu_scene`, the canvas under `run_app`'s
`<svg>`, real scenes with the rendering hints, textures from `<img>`,
`make serve-build`, publishing) and the shader tutorial,
[`notes_opengl_shaders.md`](notes_opengl_shaders.md). What's left,
roughly from most to least worth doing.

## 1. `Mesh_cache`, then Minecraft3d in the browser

The big one, owned by [`plan_opengl_perf.md`](plan_opengl_perf.md)
and in progress there: its Phase 3 is WebGL's upload/draw/free for
`Mesh_cache` (`Gpu_scene.group_by_material`'s `on_cached`, a WebGL
buffer and vertex count per material of a cached mesh, `deleteBuffer`
in the sweep; and, WebGL 1 having no VAOs, the attribute pointers set
again for each buffer drawn).

Then the WebGL-specific part of Minecraft3d:

- `games3d/webgl/dune`: `Minecraft3d` next to `StarCollector3d` (the
  `minecraft_model` library is plain OCaml, it should compile with
  js_of_ocaml as is);
- its texture atlas, `"games3d/texture.png"`, copied where the page
  looks for it, `games3d/webgl/games3d/texture.png` (a one-rule dune
  file, like `examples3d/webgl/examples3d/dune`), and by `make website`;
- its fps next to OpenGL's and software's. js_of_ocaml-compiled OCaml
  is several times slower than native at `view` and `Gpu_scene`'s list
  code, so the cache matters even more here.

## 2. Debug keys, and with them the untested rendering modes

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

## 4. Smaller

- **No WebGL**: today a `failwith`, i.e. a console error and a blank
  page; show a message in the page instead. It has to survive
  `run_app`'s first-frame `<body>` reset, like the canvas does
  (`ensure_in_page`).
- **Wireframe**: WebGL has no polygon mode; draw each triangle's 3
  edges as `gl.LINES` (a second, per-frame vertex list), or the
  barycentric-coordinates fragment shader trick.
- **A real browser**: keys and mouse (`InteractiveCube3d`,
  `StarCollector3d`) were only tried in headless Chrome, which can't
  press keys.
- **Publishing**: the pages go live only once `next` is merged into
  `master` and `make website` is run there (with a `git add -f` of the
  gitignored `.bc.js`); `OPAMS`/`ODOC_DIRS` in the `Makefile` still
  list none of the 3D packages.
