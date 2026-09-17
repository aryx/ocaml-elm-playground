# playground3d/ native rasterizer: optimizations and fixes log

Purpose: track each performance optimization and rendering-correctness
fix applied to the native rasterizer
(`playground3d/native/Playground3d_platform.ml`), with what it changed,
why the "simpler" code before it wasn't good enough, and (for the perf
ones) measured before/after FPS -- so the reasoning stays visible even
as the code accumulates optimizations. Companion to
[`notes_3d.md`](notes_3d.md) (the concepts) and
[`playground3d_plan.md`](playground3d_plan.md) (the overall design).

Benchmarks below are the on-screen FPS counter (the window title) each
`examples3d/` demo already shows, at the default 1000x1000 window, on
this machine. Not a rigorous benchmark suite (no fixed camera/scene
across runs, single sample each) -- order-of-magnitude comparisons, not
precise numbers.

## Baseline (before any of the changes below)

- `Cube3d.ml` (1 flat-colored cube, ~12 triangles): ~87 fps
- `Cubes3d.ml` (25 flat-colored cubes, ~150 triangles, some overlapping): ~28-29 fps
- `TexturedCube3d.ml` (1 textured cube, ~12 triangles): ~17 fps

## Fix 1: rasterizer "crack" (missing pixels along a triangle seam)

- **Symptom**: thin box-shaped markers (`examples3d/InteractiveCube3d.ml`'s
  axis indicators) intermittently showed a gap in the middle as the
  scene rotated.
- **Cause**: every rectangular face is rasterized as 2 triangles
  sharing a diagonal edge (see `fan_triangles`). The inside-triangle
  test used a strict `>= 0.`/`<= 0.` comparison on the 3 edge-function
  values. In exact math, a pixel sitting exactly on that shared diagonal
  gives both triangles an edge value of exactly 0, so both draw it
  (harmless double-drawing). In floating point, the two triangles reach
  that same physical edge via different vertex triples (different
  argument order to the same `edge` function), so the two computations
  aren't bit-for-bit identical -- rounding can nudge *both* triangles'
  computed value to something like `-1e-10` at once, so *neither* draws
  that pixel: a 1-pixel gap along the diagonal. Angle-dependent, since
  it only shows up when rounding happens to tip a shared value across
  zero for that specific projected orientation.
- **Fix**: a small inclusive epsilon tolerance (`1e-4`) on the
  comparison instead of testing against exactly `0.`, so a shared edge
  is now reliably "inside" for both triangles even after rounding
  error. (The fully rigorous alternative, a "top-left fill rule" like
  real GPUs use, assigns each shared-edge pixel to exactly one
  triangle so there's neither a gap nor double-drawing at all -- noted
  in the code as the option to reach for if this epsilon ever proves
  insufficient.)
- **Where**: `rasterize_triangle`'s `inside` test.
- **Not a perf change** -- a correctness fix, no FPS impact.

## Optimization 1: hoist `1. /. area` out of the per-pixel loop

- **What**: barycentric coordinates were computed as `w0 /. area`,
  `w1 /. area`, `w2 /. area` -- 3 divisions per pixel. Replaced with
  `inv_area = 1. /. area` computed once per triangle, then
  `w0 *. inv_area` etc (multiplication is cheaper than division, and
  this is now done only once per pixel instead of three times).
- **Why the old code wasn't wrong, just needlessly slow**: division
  and multiplication-by-the-reciprocal give the same result here (up
  to entirely negligible floating-point rounding); the only difference
  is doing the division 3 times per pixel instead of once per triangle.
- **Where**: `rasterize_triangle`.
- **Impact**: `Cubes3d.ml` ~28-29fps -> ~37fps (~30% faster on a
  flat-colored, division-heavy scene).

## Optimization 2: skip `Sdl.map_rgb`'s per-pixel FFI call for textures

- **What**: `pixel_of_rgb` (called once per pixel of a *textured*
  face, since a textured face's color differs at every pixel) called
  `Sdl.map_rgb`, a call into the C SDL library, for every single pixel.
  Replaced with a direct bit-shift computed from the window's pixel
  format masks (queried once at startup via
  `Sdl.pixel_format_enum_to_masks`), falling back to the original,
  always-correct `Sdl.map_rgb`-based `pixel_of_rgb_via_sdl` whenever
  the window's pixel format isn't the common 32-bit/8-bit-per-channel
  case the fast path assumes (kept as a real, reachable code path, not
  just a comment -- see the "Optimization: fast RGB -> pixel packing"
  section of the file).
- **Why the old code wasn't wrong, just slow for this case**:
  `Sdl.map_rgb` is the only fully general way to pack an (r, g, b)
  triple correctly for *any* pixel format (565, paletted, etc). For a
  flat-colored face it's called once per face, so its cost is
  invisible; a textured face calls it once per pixel, and the
  OCaml-to-C call overhead, paid at every single pixel, dominates.
- **Where**: new "Optimization: fast RGB -> pixel packing" section,
  kept deliberately separate from the "Render one frame" section so
  that section stays a plain read of what a frame does.
- **Impact**: `TexturedCube3d.ml` ~17fps -> ~83fps (~5x faster; now
  close to the ~87fps flat-color baseline, confirming this was indeed
  the dominant cost).

## Fix 2: perspective-correct interpolation (texture "swimming")

- **Symptom**: on `TexturedCube3d.ml` (checker.png's 4-quadrant test
  texture), the crosshair where the 4 quadrants meet appeared to
  shift/wobble slightly within each face as the cube rotated, even
  though the cube's geometry itself wasn't deforming.
- **Cause**: `rasterize_triangle` interpolated `u`, `v`, and view-space
  `z` *linearly in screen space* via barycentric weights. That's exact
  for the screen-space position itself, but perspective projection
  divides by depth (`screen_x` proportional to `view_x / view_z`),
  which makes the map from 3D position to screen position nonlinear --
  so a quantity that's linear in 3D (like a texture's `u`/`v`, or `z`
  itself) is *not* linear in screen space, and linearly interpolating
  it is only an approximation: exact at the 3 corners, increasingly
  wrong towards the interior, and *more* wrong the more a triangle's
  depth varies across itself (i.e. the more obliquely it's viewed).
  Since the cube keeps rotating, each face's obliqueness keeps
  changing, so the approximation error -- and with it, where the
  texture's own fixed detail *appears* to sit -- keeps changing too.
  This is the classic "affine texture mapping" artifact, notorious from
  the original PlayStation's 3D rendering.
- **Fix**: a well-known trick (see e.g. Heckbert & Moreton, 1991, on
  perspective texture mapping): interpolate `1/z`, `u/z`, and `v/z`
  instead of `z`, `u`, `v` directly -- these quotients genuinely *are*
  linear in screen space, so interpolating them linearly is exact, not
  an approximation -- then divide back out once at the end (the
  "perspective divide") to recover the true `u`/`v`/`z` at that pixel.
  As a side effect, the z-buffer's depth test also became more accurate
  (it was linearly interpolating `z` before too), for free, from the
  same change.
- **Where**: `vertex`'s fields (`vz`/`vu`/`vv` -> `inv_z`/`u_over_z`/
  `v_over_z`), `project_vertex` (computes the `/z` versions), and
  `rasterize_triangle`'s per-pixel interpolation + perspective divide.
- **Not primarily a perf change** (one extra division per pixel where
  the z-test passes) -- `TexturedCube3d.ml` stayed at ~83fps.

## Current numbers (after all of the above)

- `Cube3d.ml`: ~87-89 fps (unaffected by any of these; already
  dominated by other costs, and never had visible texture-warping or a
  texture-pixel-packing cost to begin with)
- `Cubes3d.ml`: ~37 fps (was ~28-29 fps)
- `TexturedCube3d.ml`: ~83 fps (was ~17 fps), and no longer visibly warps

## Not done (deliberately, to keep the code simple)

- **Incremental edge-function stepping**: the standard next
  optimization for this style of rasterizer (compute each pixel's edge
  values via a running per-pixel/per-row delta instead of recomputing
  the full edge formula from scratch each time) -- probably worth real
  FPS on larger/many-triangle scenes, but it turns the "obviously
  correct, recompute from scratch" loop into a small stateful
  accumulator, a real complexity cost. Not applied since Optimizations
  1 and 2 already gave large wins without changing the loop's shape at
  all; worth revisiting if a future scene (e.g. Cubes3d at a much
  larger triangle count, or the eventual tiny-minecraft port) still
  isn't fast enough after these.
- **Top-left fill rule** (mentioned under Fix 1): the fully rigorous
  fix for the crack bug; not needed since the epsilon tolerance already
  fixes it invisibly at this project's scale.
