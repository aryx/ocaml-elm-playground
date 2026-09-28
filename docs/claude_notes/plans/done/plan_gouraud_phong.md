# Plan: Gouraud/Phong shading + a `sphere` primitive

## Context

`notes_3d_shading.md`'s roadmap (items 3-4) already identified what's
needed: Gouraud and Phong shading are meaningless on `cube`/`box`/
`plane` (every one of their faces has its own independent, unshared
corners, specifically so hard edges stay sharp -- see `Playground3d.
box_faces`), so they'd render pixel-for-pixel identical to
`flat_shading`. To make them visibly different at all, we need a curved
shape approximated by many small faces with genuinely varying
per-vertex normals -- a `sphere`. This plan covers adding both together.

## Design

### 1. A new mesh variant: `SmoothPolygon3d`

Rather than generalizing the mesh representation to track shared
vertices/adjacency across faces (a much bigger change), lean on the
fact that a sphere's vertex normal has a closed form: since it's
centered at the origin, the outward normal at any point on it is just
that point's own position, normalized. No averaging of neighboring
faces' normals is needed at all. So the minimal addition is a new
`form3d` case carrying an explicit normal per point, alongside the
existing `Polygon3d` (flat) and `TexturedPolygon3d`:

```ocaml
| SmoothPolygon3d of Playground.color * ((number*number*number) * (number*number*number)) list
  (* (point, normal) pairs *)
```

No texture support on this first version (kept minimal, like the other
additions in this project) -- a textured+smooth face can be a later
variant if ever needed.

### 2. `sphere color radius`

A standard UV-sphere: fixed latitude/longitude segment counts (no
resolution parameter exposed, matching `cube`/`box`'s no-parameters
style), each lat/lon cell built as one `SmoothPolygon3d` quad with each
corner's normal computed analytically (`normalize` of that corner's own
position). Corner order per quad needs checking by hand against
`face_normal`'s winding convention (cross of the first 3 listed points)
so backface culling -- which always uses the *winding-based* flat
normal, never the stored per-vertex ones, even for `SmoothPolygon3d` --
agrees with the analytical normals' direction. Degenerate (zero-area)
quads at the two poles (where all "lat=0" corners coincide at one
point) are expected and harmless: `fan_triangles` still splits them,
and the rasterizer's existing `if area <> 0.` check silently skips the
resulting zero-area triangle.

### 3. `move3d`/`rotate3d`/`scale3d` and normals

- `move3d`/`scale3d`: unchanged (`map_points`, transforming positions
  only) -- a normal is a direction, translating or uniformly scaling a
  shape shouldn't touch it.
- `rotate3d`: needs a new `map_points_and_normals` that applies the
  *same* rotation to both a point and (for `SmoothPolygon3d`) its
  normal -- rotation is the one transform where a direction genuinely
  needs to move too. For `Polygon3d`/`TexturedPolygon3d` (no explicit
  per-point normal), this behaves exactly like today's `map_points`.

### 4. Native: per-vertex normals through the pipeline

- `flatten_faces` (native): every leaf face's point list gets a normal
  attached to each point -- the single winding-based `face_normal` for
  `Polygon3d`/`TexturedPolygon3d` (same value repeated for every point,
  which is exactly what makes `flat_shading` uniform across a face
  today), or the stored per-vertex normal for `SmoothPolygon3d`.
- `vertex` gains a `normal : vec3` field, filled in by `project_vertex`.
- A new `make_shader v0 v1 v2 : l0:float -> l1:float -> l2:float ->
  float`, alongside the existing `make_interpolator` (for depth/UV) and
  `fill_of_material` (for color) -- a third, orthogonal "decide once
  per triangle, apply once per pixel" strategy function, matching the
  same shape as those two:
  - `Flat_color` -> always `1.`
  - `Flat_shading` -> `brightness_of_normal v0.normal` (any vertex; all
    3 share the same normal on a flat face), ignoring the weights
  - `Gouraud` -> `brightness_of_normal` computed once per *vertex*, then
    the 3 resulting numbers blended via `l0`/`l1`/`l2` (linear, not
    perspective-correct -- see "Simplifications" below)
  - `Phong` -> interpolate the *normal's components* via `l0`/`l1`/`l2`
    (also linear), renormalize (an interpolated blend of unit vectors
    isn't unit-length in general), then `brightness_of_normal` on that
  - `shading_mode`'s type grows from 2 to 4 cases; `cycle_shading_mode`
    cycles through all 4 in this order.
- `fill_of_material` drops its `normal` parameter (brightness is no
  longer decided there) and instead takes `~brightness` as a 4th
  per-pixel argument, multiplying it into whatever color it resolves --
  unifies flat/textured color resolution with however brightness ended
  up being computed, without `fill_of_material` needing to know which
  shading mode is active at all.
- `rasterize_triangle`/`rasterize_triangle_painters`: build both
  `interpolate` and `shade_pixel` once per triangle (same spot
  `interpolate` is already built), call `shade_pixel ~l0 ~l1 ~l2` next
  to the existing `interpolate ~l0 ~l1 ~l2` call, pass the result into
  `fill ~u ~v ~brightness`.

### 5. Web backend

`SmoothPolygon3d` compiles down like `Polygon3d` for
`render3d_to_2d`/web (flat color, winding-based normal for the
depth-sort/cull step, no smooth shading -- consistent with the web
backend never doing any per-pixel work at all, textures included).

## Simplifications (stated up front, not discovered by accident this time)

- Gouraud/Phong interpolate brightness/normals *linearly*, not
  perspective-correctly (unlike `u`/`v`/`z`, which the `p` toggle
  already covers) -- brightness differences are usually too subtle for
  the difference to be visible, and reusing `make_interpolator`'s
  machinery for a third, differently-shaped attribute (a normal, not a
  single float) would complicate that function for likely-invisible
  benefit. Worth revisiting only if it turns out to actually matter.
- One fixed light, no color, no shadows -- unchanged from today,
  explicitly out of scope here (see `notes_3d_shading.md`'s further
  roadmap items).
- No new "which primitives support smooth shading" combinators beyond
  `sphere` itself (no `cylinder`/`cone` in this pass).

## Phasing

1. `Playground3d.mli`/`.ml`: `SmoothPolygon3d`, `sphere`,
   `map_points_and_normals`, `rotate3d` switched to use it, web-side
   `flatten_faces`/`render3d_to_2d` handling.
2. Native: `vertex.normal`, `flatten_faces` normal plumbing,
   `make_shader`, `fill_of_material` signature change, rewire both
   rasterizers, extend `shading`/`cycle_shading_mode` to 4 cases.
3. A new `examples3d/` demo built around one or more spheres, so
   pressing `m` visibly cycles through all 4 modes on a shape where
   they actually differ.
4. Update `notes_3d_shading.md` (mark items 3-4 done, describe what was
   actually built vs. the roadmap's guess) and `notes_3d.md`'s `m`
   entry (§11).

## Verification

- `dune build` after each phase.
- Screenshot the sphere demo in each of the 4 modes (forcing
  `shading_mode`'s default temporarily, same technique used throughout
  `notes_3d_opti.md`'s fixes) and confirm they're visibly different:
  `flat_color` unlit, `flat_shading` visibly faceted (flat per-face
  brightness), `Gouraud` smoother but still faceted right at silhouette
  edges, `Phong` the smoothest overall.
- Confirm `cube`/`box`/`plane`-only scenes (e.g. `Cubes3d.exe`) still
  look identical across all 4 modes, i.e. that this change didn't
  accidentally affect flat-shaded shapes.
