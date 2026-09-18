# Plan: the 3D software rasterizer, reorganized for teaching (`graphics/3d/`)

## Context

The 2D software rasterizer ended up as teaching-quality code: small
modules each explaining one classic algorithm (`graphics/2d/Fill.ml`,
`Line.ml`, `Circle.ml`, ...), ASCII diagrams and worked examples in the
`.mli` files, references to the papers that introduced each idea,
tests checking those examples, one function per feature with debug keys
to turn features on and off, optimizations kept side by side with the
simple code they replace (`graphics/core/Opti.ml`, the "o" key), and a
tutorial, `notes_2d.md`. See `plan_software_2d.md`.

The 3D software rasterizer is the opposite shape: almost everything is
in one 989-line file, `playground3d/software/Playground3d_platform.ml`,
mixing the pure algorithms (projection, the triangle loop, the
z-buffer, shading, texturing) with SDL (the window, pixel formats),
Playground3d's types (`shape3d`, `camera`, `Playground.color`) and Cairo
(the HUD pass). It is well commented -- often at length -- but you
can't read "how a z-buffer works" without reading around everything
else. This plan does for 3D what was done for 2D: move the algorithms
into `graphics/3d/`, one module per idea, independent of the
Playground, leaving `playground3d/software/` a thin adapter.

## What's in the 989 lines today

| Section (line) | What | Goes to |
|---|---|---|
| Vec3 aliases (41) | short names for `Vec3` | stays as aliases, or goes away |
| Colors (60) | `rgb_of_color` | the adapter (it's `Playground.color`) |
| Shading (72-163) | 4 modes, `light_dir`, `ambient`, `brightness_of_normal` | `graphics/3d/Lighting.ml` + `Shading.ml` |
| Textures (166-237) | `sample_texture_{nearest,bilinear}` on `Stb_image.t` | `graphics/3d/Texture.ml`, on a plain image type |
| Projection (239-377) | `view_space`, the `vertex` record, `project_vertex` | `graphics/3d/geometry/Camera.ml` + `graphics/3d/Project.ml` |
| Interpolation (379-410) | linear vs perspective-correct ("p") | `graphics/3d/Interpolate.ml` |
| Shading per pixel (412-451) | `make_shader`: flat/Gouraud/Phong | `graphics/3d/Shading.ml` |
| Flatten + cull (453-488) | `flatten_faces`, `fan_triangles`, culling test | adapter (flatten) + `graphics/3d/Cull.ml` |
| Triangle (490-600) | edge functions + z-buffer, the crack fix | `graphics/3d/Triangle.ml` + `Zbuffer.ml` |
| Painter's (601-656) | the triangle loop without depth test ("z") | `graphics/3d/Painter.ml` |
| Wireframe (657-691) | a DDA line drawer ("f") | reuse `graphics/2d/Line.ml` (Bresenham) |
| Pixel packing opti (694-765) | SDL pixel format, fast RGB shifts | gone (see "Framebuffer" below) |
| Render one frame (768-888) | per-material fill, the pipeline | `graphics/3d/Render.ml` (pipeline) + adapter |
| Run app (890-989) | window, keys, HUD via Cairo | `playground3d/software/Playground3d_platform.ml`, ~100 lines |

Also duplicated elsewhere, to fold in along the way: the lighting
formula and constants exist 3 times (this file, `Playground3d.ml` for
the web backend, `Gpu_scene.ml` for OpenGL), and the camera basis twice
(`view_space` here, `Mat4.look_at`).

## Principles (the same as for 2D)

- **Independent of the Playground.** `graphics/3d/` knows points,
  colors as ints, triangles, cameras as plain records: no `shape3d`, no
  `Playground.color`, no SDL, no Cairo. Converting a Playground3d scene
  into that is the adapter's job.
- **One idea per module, one feature per function.** The z-buffer and
  the painter's algorithm are two functions a key switches between, not
  one function with a branch in its inner loop (the current code
  already does this; keep it).
- **Every `.mli` explains its algorithm** with an ASCII diagram, a
  worked example, and the paper that introduced it; `graphics/tests/`
  checks the examples.
- **Debug keys** for every feature, shown in the window title (like the
  2D backend's), and the **magnifier** from 2D.
- **Optimizations keep their simple version**, switched by `Opti`
  ("o"), measured with `scripts/bench_playground.sh`, recorded in
  `notes_3d_opti.md`.
- **Comments describe the code as it is**, no "moved from" notes; long
  explanatory comments move with their code (e.g. the vertex record's
  perspective-correct explanation, the crack fix, culling vs z-buffer).
- **The pixels don't change** during the reorganization proper (phases
  1-5): each phase is checked against reference frames captured before
  starting (see Verification).

## Target layout

```
graphics/3d/geometry/        (graphics_3d_geometry, exists)
  Vec3, Mat4                 (exist)
  Camera                     eye/target/fov/near/far; the view basis
                             (right, up, forward) shared by view_space
                             and Mat4.look_at; perspective projection
graphics/3d/                 (graphics_3d, new, private like the others)
  Lighting                   the "sun", ambient, Lambert's cosine law:
                             brightness_of_normal (one copy for all
                             three 3D backends)
  Project                    a 3D point -> a screen vertex (x, y, and
                             z, 1/z, u/z, v/z for interpolation)
  Cull                       backface culling: is this face turned away?
  Zbuffer                    the depth buffer: create, clear, test-and-set
  Interpolate                barycentric weights; linear vs
                             perspective-correct ("p")
  Shading                    flat / Gouraud / Phong: brightness across a
                             triangle ("m")
  Texture                    textures as plain RGBA images; nearest and
                             bilinear sampling ("i")
  Triangle                   the edge-function triangle loop (Pineda),
                             with the z-buffer test
  Painter                    the painter's algorithm: sort faces far to
                             near, draw without a depth test ("z")
  Clip                       (new, phase 6) clipping triangles against
                             the near plane (Sutherland-Hodgman)
  Render                     the pipeline: faces -> cull -> project ->
                             clip -> triangles -> pixels, with an
                             options record like Shape_render_software's
playground3d/software/
  Shape3d_render_software    Playground3d's shape3d/camera -> Render's
                             faces and camera (the adapter; like
                             Shape_render_software in 2D)
  Playground3d_platform      window, keys, title, HUD, the loop (~100 lines)
```

Module names are checked not to clash with the 2D ones (`graphics/`
libraries are unwrapped): `Triangle`, `Texture`, `Render`, `Clip` are
new; `Line`, `Fill` stay 2D.

## Two groundwork decisions

### The framebuffer: use `graphics/core/Framebuffer`

Today the 3D rasterizer writes a flat `Array1` of int32 in the SDL
window surface's native pixel format, converting each color through
`Sdl.map_rgb` or precomputed shifts (the "fast RGB packing"
optimization, `notes_3d_opti.md` optimization 2). The 2D side instead
uses `graphics/core/Framebuffer` (an `Array2` view of the same kind of
memory, 0xAARRGGBB) and has run fine on this machine's SDL surfaces.

Proposal: render 3D into a `Framebuffer.t` too, with a startup check
that the window surface really is 32-bit xRGB (failing with a clear
message otherwise, rather than drawing wrong colors). What this buys:
the 3D rasterizer can use everything in `graphics/`: `Line` (Bresenham)
for wireframe instead of its own DDA copy, `Framebuffer.blend` (for a
future alpha), the `Magnifier`, and -- the big one -- the HUD drawn by
`Shape_render_software` instead of Cairo, making the 3D software
backend Cairo-free (the 2D plan's phase 9). The pixel-packing
optimization then disappears; its history stays in `notes_3d_opti.md`.

The depth buffer becomes its own small module, `Zbuffer` (a float
array plus width, with `clear` and `test_and_set`), the natural place
to explain Catmull's idea.

### Textures: plain RGBA, and fixing the channel mess properly

The 3D samplers read `Stb_image.t` with its own channel count (3 or 4),
because forcing 4 channels with the pinned stb_image corrupts the
metadata (see `Texture_decode`). `graphics/3d/Texture` should instead
take a plain RGBA image, like `Blit.image` in 2D, which also lets 3D
share `Blit`'s bilinear sampler. So: `Texture_decode` expands 3-channel
images to 4 itself (a few lines, no stb_image forcing), and the same
fix removes the suspected garbling of RGB images in the 2D software
backend (`Image_decode` forces 4 channels today). Checked with a test
image of each kind (RGB PNG, RGBA PNG, JPEG).

## The modules, with their references

(To double-check against the papers when writing each `.mli`: these
are from memory.)

- **Camera, Project**: the view basis (right/up/forward from a
  look-at), perspective division, the viewport. Worked example: a point
  at a known position lands at a known pixel. Roberts, "Machine
  Perception of Three-Dimensional Solids", MIT 1963 (perspective and
  homogeneous coordinates in computer graphics); the camera analogy of
  `notes_3d.md` section 4.
- **Lighting**: Lambert's cosine law (brightness proportional to the
  cosine between the normal and the light direction, i.e. their dot
  product), plus an ambient floor. Lambert, *Photometria*, 1760. One
  copy, used by the software backend, the web backend's flat shading,
  and `Gpu_scene` (the OpenGL shader keeps its GLSL copy, with a
  comment pointing here).
- **Cull**: Sutherland, Sproull, Schumacker, "A Characterization of
  Ten Hidden-Surface Algorithms", 1974 (back-face elimination as the
  first, cheapest step). Keeps the current long comment on why culling
  changes nothing visible in filled mode but halves the work.
- **Zbuffer**: Catmull, PhD thesis, Utah, 1974 (and Straßer, 1974).
  Diagram: two overlapping triangles, the depth kept per pixel.
- **Interpolate**: barycentric coordinates from edge functions;
  linear interpolation of 1/z, u/z, v/z and the perspective divide.
  Heckbert and Moreton, "Interpolation for Polygon Texture Mapping and
  Shading", 1991; Blinn, "Hyperbolic Interpolation", IEEE CG&A, 1992.
  Worked example: the middle of a slanted quad, where linear and
  perspective-correct u differ, with numbers.
- **Shading**: flat (one brightness per face), Gouraud (per vertex,
  interpolated), Phong (normals interpolated, lit per pixel). Gouraud,
  "Continuous Shading of Curved Surfaces", IEEE Trans. on Computers,
  1971; Phong, "Illumination for Computer Generated Pictures", CACM,
  1975.
- **Texture**: Catmull 1974 (texture mapping); nearest vs bilinear,
  pointing to `graphics/core/Blit` and `notes_2d.md` section 8.
- **Triangle**: edge functions, bounding box, the pixel-center rule,
  the crack fix (the epsilon, with its long comment). Pineda, "A
  Parallel Algorithm for Polygon Rasterization", SIGGRAPH 1988. Diagram:
  the three half-planes whose intersection is the triangle.
- **Painter**: Newell, Newell, Sancha, "A Solution to the Hidden
  Surface Problem", ACM National Conference, 1972. Keeps the pointer to
  `PaintersAlgorithmFail3d`.
- **Clip** (new): Sutherland and Hodgman, "Reentrant Polygon Clipping",
  CACM 1974; Blinn and Newell, "Clipping Using Homogeneous
  Coordinates", SIGGRAPH 1978.
- **Render**: the pipeline as one readable function, with an ASCII
  diagram of the stages (the 3D twin of `Shape_render_software`'s):
  faces -> cull -> project -> clip -> fan into triangles -> rasterize
  (z-buffer or painter's; filled or wireframe).

## Keys (all in the window title, like 2D)

Existing, kept: "m" shading, "b" culling, "f" wireframe, "z" painter's
vs z-buffer, "p" interpolation, "i" texture filtering. New: "o"
optimizations (`Opti`), "x" the magnifier ("z" is taken in 3D), and
with phase 6 "c" near-plane clipping and "t" the top-left fill rule.

## New teaching features (phase 6, after the reorganization)

Each a separate function, with its own key, test, and entry in
`notes_3d.md` or `notes_3d_opti.md`:

1. **Near-plane clipping** ("c"). Today a triangle with a vertex behind
   the camera is dropped whole, which should show as holes near the
   camera in first-person scenes (to confirm on Minecraft3d before
   starting, with a screenshot). Clip it against the
   near plane instead (Sutherland-Hodgman on one plane: a triangle
   becomes 0, 1 or 2 triangles). Diagram: a triangle crossing the near
   plane, and the quad that remains.
2. **The top-left fill rule** ("t"), the rigorous version of the crack
   fix: each pixel on an edge shared by two triangles belongs to exactly
   one of them. Test: two triangles sharing an edge, each pixel covered
   exactly once (the 3D twin of 2D's "shared edge" test, counting writes
   instead of blending).
3. **Incremental edge functions** (an `Opti`): the "not done" item of
   `notes_3d_opti.md` -- step the three edge values by constants per
   pixel and per row instead of recomputing them, the simple version
   kept. Measured on Cubes3d and Minecraft3d.
4. Maybe: **perspective-correct Gouraud/Phong** (they're interpolated
   linearly today, a known simplification), as one more choice of the
   "p" key.

## Phasing

0. **Groundwork**, before moving any code:
   - a deterministic frame for checking: a `-fixed-time t` flag in
     `Native_loop` (the clock the app sees stays at t) and a
     `-dump-frame n file` flag (write frame n as a PPM, then exit), so a
     scene renders the same pixels every run; capture reference frames
     of every `examples3d/` scene (and a few with keys pressed:
     wireframe, painter's, flat shading, textures);
   - the texture channel fix (`Texture_decode` expanding to RGBA).
1. **Camera and Lighting**: `Camera` in `graphics/3d/geometry/`
   (`Mat4.look_at` using its basis), `graphics/3d/Lighting` replacing
   the 3 copies. Frames identical (web and OpenGL too, by eye).
2. **The small modules**: `Project`, `Cull`, `Zbuffer`, `Interpolate`,
   `Shading`, `Texture`, each moved with its comments, tested with its
   worked example.
3. **Triangle and Painter** on a `Framebuffer.t` (the framebuffer
   decision above), wireframe through `graphics/2d/Line`.
4. **Render and the adapter**: `graphics/3d/Render` (the pipeline and
   its options record), `playground3d/software/Shape3d_render_software`,
   `Playground3d_platform` down to the window, keys and loop; the window
   title with every key's state; the magnifier.
5. **Cairo-free HUD**: drawn with `Shape_render_software`; the 3D
   software backend then links the software 2D backend instead of the
   Cairo one (`examples3d/dune`, `games3d/dune`).
6. **New features**: clipping, top-left rule, incremental edge
   functions, as above.
7. **Docs**: `notes_3d.md` with pointers into the new modules (and a
   short "reading order" for students: Camera, Project, Triangle,
   Zbuffer, Interpolate, Shading, Texture, Render), `notes_3d_opti.md`
   with the Opti numbers, `README-3d.md`'s key table.

## Verification

- `dune build`, `dune runtest` at every phase; the new tests are the
  `.mli`s' worked examples (a projected point, the linear vs
  perspective-correct numbers, an edge-function inside/outside example,
  the z-buffer keeping the nearer of two triangles, a painter's sort
  order, a clipped triangle's pieces, the top-left rule's
  each-pixel-once).
- Phases 1-5 must not change a single pixel: compare each scene's
  dumped frame with its reference (`cmp`); any difference is a bug in
  the move, not a feature.
- Phase 6 changes pixels on purpose: screenshots before/after (e.g.
  Minecraft3d near the ground for clipping), and the magnifier on a
  shared edge for the fill rule.
- Performance: `scripts/bench_playground.sh` on Cubes3d, Spheres3d,
  TexturedCube3d, Minecraft3d before phase 1 and after phases 3, 4 and
  6 -- the reorganization must not cost frames (per-pixel closures and
  allocations are the usual suspects, see `notes_opti.md`'s lesson).

## Out of scope

- The OpenGL and web backends, beyond using the shared `Camera` and
  `Lighting` (and the web's flat shading formula).
- `Playground3d`'s public API: unchanged (apps don't change).
- Alpha / `fade3d` in the software rasterizer (still not honored; the
  Framebuffer switch makes it possible later: sort transparent faces
  back to front and `blend` them after the opaque ones).
