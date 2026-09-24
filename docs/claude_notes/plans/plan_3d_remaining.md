# Plan: what's left after the 3D teaching reorganization

The reorganization itself is done: see
[`done/plan_code_reorg_teaching_3d.md`](done/plan_code_reorg_teaching_3d.md)
(the 3D software rasterizer as `graphics/3d/`, one module per idea;
clipping, the top-left fill rule, incremental edge functions; golden
frame tests for 2D and 3D; the "h" help and `-debug-keys`). What it
left open, roughly from most to least worth doing. Each changes pixels
on purpose, so each ends with new golden frames, approved after looking
at them (`make approve-golden2d` / `approve-golden3d`).

## 1. Rounding instead of truncating in `Render.scale_channel`

`scale_channel c brightness = int_of_float (float c *. brightness)`
truncates, so a brightness a hair below 1 turns 255 into 254: e.g. with
Gouraud, a pixel between three fully lit vertices gets 0.99999..., not
exactly 1. That's what made the incremental edge functions change
Spheres3d's Gouraud golden by 1/255 on 1472 pixels (see
`notes_3d_opti.md`, optimization 3): the truncation is the fragile part.
Fix: `int_of_float ((float c *. brightness) +. 0.5)` (brightness is at
most 1, so 255 stays 255). Changes most lit pixels by at most 1: nearly
every 3D golden, so best done together with 2.

## 2. The top-left fill rule as the default

Today the epsilon is the default and "t" switches to the top-left rule
(`Triangle.mli`). The rule is the rigorous one (each pixel of a shared
edge drawn exactly once, tested by `Unit_triangle`), and with it the
incremental edge functions give exactly the simple version's values.
One line in `Render.default_options`; the epsilon stays a key away.
Changes a few edge pixels in most 3D goldens (4 on Cubes3d, 103 on
Spheres3d when tried).

## 3. Perspective-correct Gouraud and Phong

Phase 6's item 4, not done. `Shading` interpolates brightness (Gouraud)
and normals (Phong) linearly in screen space (see
`done/plan_gouraud_phong.md`'s "Simplifications"), while `Interpolate`
does u, v and z perspective-correctly. Same fix as for textures:
interpolate brightness/z (or normal/z) and 1/z, divide at the end; the
vertices already carry `inv_z`. Could be one more choice of the "p"
key (linear / perspective-correct for everything). Visible only on
large, oblique, curved triangles: a new example may be needed to show
it (a big low-poly sphere close to the camera?).

## 4. 2D: `Blit`'s bilinear rounding

In [`plan_2d_remaining.md`](plan_2d_remaining.md), item 1: once fixed,
`Texture.sample_bilinear` could share `Blit`'s sampler.

## 5. The GPU backends, for bigger scenes

What [`done/plan_opengl_perf.md`](done/plan_opengl_perf.md) left, only
if a scene's numbers ask for it (`-debug`'s stats line), each
independent, for both GPU backends:

- **fog + a draw distance**, the Python Minecraft's two other tricks:
  `rendering` hints, since fog changes pixels (a few fragment-shader
  lines on the GPU; a new one-idea `graphics/3d/Fog` for the software
  rasterizer);
- **frustum culling** of cached nodes: skip the chunks outside the
  camera's view, with a bounding box computed once by `cached3d`;
- **indexed drawing** (`draw_elements`: 4 vertices per quad instead of
  6, see `notes_opengl.md` section 7);
- **a per-node model matrix**, so that moving objects can be cached
  too (today `move3d` on a `cached3d` gives an uncached group);
- **texture-atlas bleeding**: faint lines along some block edges in
  TinyMinecraft's WebGL screenshot (headless Chrome's SwiftShader; not
  seen on OpenGL), probably a sample from the neighboring atlas cell at
  a cell's border. The classic fix: shrink each cell's UV rectangle by
  half a texel. To check in a real browser first.

## 6. Ray tracing, the other renderer

Its own plan now:
[`plan_raytracing_teaching.md`](plan_raytracing_teaching.md) --
`graphics/3d/Raytrace` over the same `Render.face list` the rasterizer
takes, for `-dump-frame` stills and a low-resolution preview key, with
the shadows, mirrors and glass the rasterizer cannot do; the tutorial
is [`notes_raytracing.md`](../tutorials/notes_raytracing.md).

## Smaller things noticed along the way

- **The SVG backend's package name**: `elm_playground_3d_web` draws
  through SVG and lives in `playground/platforms/svg/`, next to the WebGL one in
  `playground/platforms/web/` (`done/plan_merge_2d_3d.md`). Renaming it
  `elm_playground_3d_svg` would say what it is, at the cost of
  renaming an opam package.
- **Five 3D examples build only on the software rasterizer and the web
  backends**, not on OpenGL: `Cube3d`, `InteractiveCube3d`,
  `PaintersAlgorithmFail3d`, `FloatingCity3d`, `Corridor3d`. Two of
  them are deliberate (the "z" and "c" toggles are the software
  rasterizer's, and `InteractiveCube3d` wants a HUD), but `Cube3d` and
  `FloatingCity3d` render fine everywhere else -- see `examples/dune`'s
  trailing comment.

- **Hershey's colon** at small sizes, e.g. the HUD's "Mouse:": see
  [`plan_2d_remaining.md`](plan_2d_remaining.md), item 3.
- **Transparency on the software 3D backend**: `fade3d` is ignored
  (README.md's 3D limitations). The classic way: draw the opaque faces
  with the z-buffer, then the transparent ones sorted far to near
  (`Painter`), blended (`Framebuffer.blend`), testing but not writing
  the z-buffer.
- **The z-buffer and the painter's algorithm share one loop**,
  `Triangle.fill` with an optional z-buffer, where the plan's
  principles wanted two functions, each readable on its own. Split them
  if that reads better for teaching (no speed difference either way).
- **More incremental stepping** (`notes_3d_opti.md`, "Not done"): along
  y too, and the barycentric weights (so z, u/z, v/z) instead of
  multiplying per pixel.
- **The web backend drops triangles crossing the near plane**
  (`Playground3d.render3d_to_2d`): `Clip` could be used there too, in
  view coordinates, before projecting each polygon (e.g. `Corridor3d`
  in a browser).
- **The OpenGL backend has no "h" help.** (Its HUD is done: drawn by
  the 2D software rasterizer, its transparency recovered by
  `graphics/core/Matting`, blended by the GPU. It costs ~20ms each time
  the HUD's shapes change, nearly all of it Matting's pass over every
  pixel of the window: fine for a score, a small hitch for TinyMinecraft's
  position readout, which changes at each block crossed. Next step if
  it matters: matte only the rows and columns the shapes touch.)
- **Golden frames not covered**: TinyMinecraft (slow, and a 1.5 MB frame;
  `scripts/frames/ref_frames_3d.sh` checks it by hand). The games drawing
  random numbers (StarCollector3d; in 2D, Snake and Tetris) are, since
  they take a `seed=n` flag (`Random.init n` instead of
  `Random.self_init`) that the runner passes. The Cairo 2D backend isn't
  covered either (its pixels depend on the installed Cairo).
- **`imagelib`** is used by the golden tests (`tests/common/`) but not
  declared in `dune-project`; fine as long as the tests belong to no
  package.
