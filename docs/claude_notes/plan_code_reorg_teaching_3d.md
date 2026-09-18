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

0. **DONE.** **Groundwork**, before moving any code. In the end:
   `Native_loop`'s `-fixed-time t`, `-keys k`, `-dump-frame n file`
   (the software backend writes a PPM; with `-dump-frame` the loop
   ignores mouse and keyboard, which otherwise leaked the pointer's
   position into InteractiveCube3d's frame), and
   `scripts/ref_frames_3d.sh capture|check <dir>`: 18 frames (every
   examples3d scene, Minecraft3d's first frame, and the key variants:
   shading modes, wireframe, painter's, culling, interpolation,
   filtering), deterministic run after run. Two findings: neither 3D
   backend called `Native_loop.parse_cli_and_setup_logging`, so
   `-v`/`-debug` never worked in 3D (now called); and the stb_image
   channel bug is worse than the old comment said -- with
   `~channels:4`, the binding allocates for the file's 3 channels, so
   an RGB image loaded that way (as `Image_decode` did for every 2D
   image) was truncated, and read past its end by the Cairo backend.
   Fixed with `graphics/images/Rgba` (expand to RGBA in OCaml; tested,
   including a test pinning the binding's behavior); textures are RGBA
   too now, pixel-identical in all 18 frames.
   The original plan for this phase:
   - a deterministic frame for checking: a `-fixed-time t` flag in
     `Native_loop` (the clock the app sees stays at t) and a
     `-dump-frame n file` flag (write frame n as a PPM, then exit), so a
     scene renders the same pixels every run; capture reference frames
     of every `examples3d/` scene (and a few with keys pressed:
     wireframe, painter's, flat shading, textures);
   - the texture channel fix (`Texture_decode` expanding to RGBA).
1. **DONE.** **Camera and Lighting**: `Camera` in `graphics/3d/geometry/`
   (`Mat4.look_at` using its basis), `graphics/3d/Lighting` replacing
   the 3 copies. Frames identical (web and OpenGL too, by eye).
   In the end: `Camera` (`basis`, `view`, `focal`, `ndc`, with a
   worked example: fov 90, 5 up and 10 ahead is ndc y 0.5) is used by
   the software backend's `project_vertex`, the web's
   `Playground3d.project`, and `Mat4.look_at`; `Lighting` (the sun, the
   ambient floor, Lambert's cosine law, with the long comments) by the
   software backend, the web's flat shading, and `Gpu_scene.light_dir`
   (the GLSL shader still spells out the formula; its comments point
   to `Lighting`). New library `graphics_3d` (package
   elm_playground_3d); tests `Unit_camera` and `Unit_lighting`. The 18
   software frames are byte-identical. The OpenGL backend has no frame
   dump, so instead of checking it by eye: its basis and light
   direction are the exact same float operations as before, as are
   the web's projection and brightness (`Float.max` became
   `Stdlib.max`, which differs only on NaN, and normals are never NaN).
2. **DONE.** **The small modules**: `Project`, `Cull`, `Zbuffer`, `Interpolate`,
   `Shading`, `Texture`, each moved with its comments, tested with its
   worked example.
   In the end: the six modules in `graphics/3d/`, each with its
   diagram, worked example (checked by `Unit_project`, `Unit_cull`,
   `Unit_zbuffer`, `Unit_interpolate`, `Unit_shading`,
   `Unit_texture`) and references. `Interpolate` and `Shading` take
   their mode as an argument (`make mode v0 v1 v2`); the mode refs and
   the keys stay in `Playground3d_platform` until phase 4's options
   record. `Texture.image` has `Blit.image`'s layout (plain RGBA), but
   is its own type, so that `graphics_3d`, which the web backend links
   for `Lighting`, doesn't depend on `graphics_core`; the backend turns
   `Texture_decode`'s images into it (no copy, same bytes).
   `Texture.sample_bilinear` is still its own, not `Blit`'s: it rounds
   once, at the end, and `Blit.sample_bilinear` rounds after each of
   its 3 mixes, so sharing it would change pixels. A finding on the
   way: `Blit.draw`'s optimized path rounds once too, so in 2D the "o"
   key can change a bilinear image's pixels by 1 (to fix in 2D, making
   `Blit.lerp` not round).
   The 18 frames are byte-identical, and the speed is unchanged:
   `Native_loop` got `-uncapped` (no 60 fps pacing), so
   `-fixed-time 1000 -uncapped -dump-frame 200 /dev/null` times 200
   frames of the same scene, best of 3, dev build: Cubes3d 12.59s
   before, 12.63s after; Spheres3d (Phong) 5.43s / 5.50s;
   TexturedCube3d 9.37s / 9.40s (all within noise; Minecraft3d left
   out, too slow to mean anything). The platform file is at 700
   lines, from 940.
3. **DONE.** **Triangle and Painter** on a `Framebuffer.t` (the framebuffer
   decision above), wireframe through `graphics/2d/Line`.
   In the end: `Triangle.fill` (Pineda's edge functions, the crack
   fix's epsilon, with or without a `Zbuffer`: one loop for both
   visibility modes) and `Triangle.outline`, `Painter.sort_far_to_near`,
   each with its worked example (`Unit_triangle`, `Unit_painter`). The
   backend draws into a `Framebuffer.t` over the window surface, after
   checking it's 32-bit xRGB; the SDL pixel packing is gone (see
   `notes_3d_opti.md`). Library moves: `graphics_3d` now uses
   `graphics_core` and `graphics_2d`, so it became part of
   `elm_playground_3d_software` (which now depends on
   `elm_playground_software`), and `Lighting`, which the web and OpenGL
   backends use too, moved to `graphics/3d/geometry/` (the library
   shared by all 3D backends). Done in two steps: the Framebuffer
   switch alone kept all 17 golden frames (and Minecraft3d's) identical,
   and was faster, not slower (200 frames: Cubes3d 12.63s -> 11.83s,
   Spheres3d Phong 5.50s -> 4.46s, TexturedCube3d 9.40s -> 8.45s); then
   wireframe through `Line.draw` (clipping, Bresenham) instead of the
   3D code's own DDA changed the 2 wireframe goldens on purpose: every
   line within a pixel of where it was (the DDA rounded the endpoints,
   `Line.draw` takes the pixel containing them), and no more gaps (the
   DDA took one step too few when a line's length wasn't a whole
   number); approved after comparing them.
4. **DONE.** **Render and the adapter**: `graphics/3d/Render` (the pipeline and
   its options record), `playground3d/software/Shape3d_render_software`,
   `Playground3d_platform` down to the window, keys and loop; the window
   title with every key's state; the magnifier.
   In the end: `Render.render ?options fb zbuffer camera faces`, its
   faces Playground-free (`paint`: a 0xRRGGBB color or a
   `Texture.image`; points with uv and normal), the pipeline diagram
   and a reading order in `Render.mli`, tested by `Unit_render`. The
   adapter resolves colors and textures while flattening the shape3d
   tree. `Playground3d_platform` (241 lines, from 940 before phase 2)
   has one `options` ref instead of six mode refs, the keys "o" (Opti)
   and "x" (the magnifier; "z" is taken) besides the six existing
   ones, and a window title with every key's state, through a new
   `?title_keys` of `Native_loop.run` (e.g. "Playground3D -- 1000x1000
   -- 31 fps -- m:phong b:cull=on f:wire=off z:zbuffer p:perspective
   i:bilinear o:opti=on x:zoom=off"). All golden frames (and
   Minecraft3d's) identical, a new golden Cubes3d with "o" identical to
   Cubes3d's on purpose, same speed (Cubes3d 11.78s, Spheres3d Phong
   4.50s, TexturedCube3d 8.39s); checked on a real window: the keys,
   the title, and the magnifier.
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
  Since phase 2, this is part of `make test`: `tests/3d/` has the 17
  frames (all but Minecraft3d's) as golden PNGs, the pre-reorganization
  references (checked pixel for pixel), and a Testo test per scene
  (`tests/3d/Golden_frames.ml`) renders it offscreen (SDL's "dummy"
  video driver: no display needed, same pixels) and compares; a
  difference fails with the number of pixels and the new frame as a
  PNG, and `make approve-golden3d` accepts intended changes.
  `scripts/ref_frames_3d.sh` stays for Minecraft3d, by hand.
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
