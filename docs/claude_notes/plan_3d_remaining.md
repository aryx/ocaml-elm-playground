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

## 4. 2D: `Blit`'s simple and optimized bilinear paths differ by 1

`Blit.sample_bilinear` (the simple path, "o" off) rounds after each of
its 3 mixes (`Blit.lerp`), `Blit.draw`'s optimized path rounds once, at
the end: so in 2D the "o" key can change a bilinear image's pixels by 1,
against the rule that an optimization keeps the pixels. Fix: `lerp` on
floats, rounding once. Then `graphics/3d/Texture.sample_bilinear`, which
rounds once too, could share Blit's sampler (the plan's original idea;
see `Texture.mli`). No 2D golden has an image (Turtle and Mario
download theirs), so `graphics/tests/Unit_blit.ml` is the check; and a
local image in an example would let a golden cover it.

## Smaller things noticed along the way

- **Hershey's colon** at small sizes (the 3D HUD, ~13 pixels): two tiny
  stroked diamonds, a smudge close to the previous letter ("Mouse:").
  The help panel draws its text 1.3 times bigger, where it reads fine.
  Maybe a minimum dot size, or a bit more spacing, in `graphics/font` or
  the stroke renderer.
- **Transparency on the software 3D backend**: `fade3d` is ignored
  (README-3d's limitations). The classic way: draw the opaque faces
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
  (`Playground3d.render3d_to_2d`): `Clip` could be used there too, on
  the projected polygons (e.g. `examples3d/Corridor3d` in a browser).
- **The OpenGL backend has no HUD** (`notes_3d.md` section 12), and no
  "h" help.
- **Golden frames not covered**: Minecraft3d (slow, and a 1.5 MB frame;
  `scripts/ref_frames_3d.sh` checks it by hand), and the games using
  `Random.self_init` (StarCollector3d; in 2D, Snake and Tetris): a
  `-seed n` flag would make them testable. The Cairo 2D backend isn't
  covered either (its pixels depend on the installed Cairo).
- **`imagelib`** is used by the golden tests (`tests/common/`) but not
  declared in `dune-project`; fine as long as the tests belong to no
  package.
