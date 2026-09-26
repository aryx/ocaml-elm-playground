# Plan: what's left for the ray tracer

The ray tracing plan is done: see
[`done/plan_raytracing_teaching.md`](done/plan_raytracing_teaching.md) --
its phases 0 to 9 and 11, each with its status, what it checked and
what it met on the way -- and its tutorial,
[`notes_raytracing.md`](../tutorials/notes_raytracing.md). Six
algorithms kept side by side (ray casting, Lambert's light, shadow
rays, Whitted's mirrors and glass, soft shadows, path tracing), a BVH,
CSG, patterns, the progressive `Povray` way, and the "y" and "v" keys
on any `shape3d` scene; about 1,700 lines of implementation. What's
left, from most to least worth doing, by what it would teach for its
lines (the author's rule for this plan: "we should add if it adds
moderate LOC for what it teaches").

## 1. The rasterizer's orthographic bug

Found by the ray tracer's A/B in phase 1, a `TODO` in
`graphics/3d/Interpolate.ml`: with an orthographic camera the
rasterizer's perspective-correct interpolation interpolates 1/z, where
depth is linear on the screen, and where two faces cross the one behind
wins (162 pixels of 19,200 in `Unit_raytrace.ml`'s scene, 1 with
`Linear`). The fix is a line (interpolate linearly when `camera.ortho >
0`); it waits because it changes the golden frames of
`TinyMonumentValley`, `TinyPerspective` and `TinyFez` -- the author's
call, to do with a golden run. Then the A/B test's orthographic case
without `~interpolation:Linear`.

## 2. TinyMyst (the plan's phase 10)

The payoff the plan designed in full (`done/plan_raytracing_teaching.md`,
"The payoff: TinyMyst"): an island as a `Povray` scene, nodes as
cameras, its stills ray traced once by a script and committed, the
hotspots found by casting the click's ray (the solid it meets named),
one puzzle, `games/adventure/`. A game more than a lesson in rendering
-- the lesson is that the way is enough to build on.

DONE (2026-09-26): `TinyMyst.ml` and `games/adventure/myst/`. The
logic HyperTalk cards (the author's choice), through
`libs/languages/hypertalk`. No `named` verb: `Povray.pick` returns the
very solid given to the scene, and the game keeps each button's name
beside its solid, found by `==`; a card lists its own buttons, so a
switch seen from afar is not one. The stills are JPEGs (17, 384 KB),
made by `myst/make_stills.exe`, checked against the scene by
`myst/tests/`. Next, the author's idea: a TinyPovray or TinyBlender
(`apps/graphics/`), a scene edited and ray traced.

DONE (2026-09-26): `TinyBlender.ml`, Blender 2.8's quad view over
`appkits/modeler` (`Modeler`, `Modeler_view`), the camera's view ray
traced live by the Povray way (half resolution, six bounces), F12
larger with two rays a pixel. TinyPovray, the scene as text, is still
to do; it would give TinyBlender a file format of its own.

## 3. Small things the phases left

- **The way's `image`**: a texture file on a way's solid. Loading a
  file is the platform's (`Image_decode`, natively and on the web), not
  the pure library's; `Uv_function` is ready for it, and a solid needs
  (u, v) (a sphere's latitude and longitude, a box's faces).
- **`PovrayQuake`, and `shapes`**: a `shape3d` scene inside a `Povray`
  one. Dropped in the phase 0 review (the way is in the 2D library,
  `Shape3d_render_software` in the 3D one); a program in the software 3D
  stanza could still do it with `Shape3d_render_software.solids`, if
  TinyQuake's level were a value outside the game.
- **`-dump-size`'s double render**: `-dump-frame` draws the window's
  frame at the window's size before making the still again at its own;
  a supersampled still pays twice.
- **Accumulating paths**: the path tracer's samples fixed by the keys 1
  to 4; adding samples while the camera stays, and the picture
  converging as you watch, is the planned "progressive" in its fullest
  sense (a float buffer and a pass counter).
- **A budget from the frame time**: the way's rays per frame are fixed,
  for deterministic golden frames; adjusting them to hold 30 fps, off
  under `-fixed-time`, is an exercise the plan offered.
- **The browser, measured in a page**: the numbers under node are the
  browser's engine without the page; a timing in TinyMosaic's or a real
  browser's window, of `PovrayWhitted`'s progressive picture, would
  complete `Bvh.mli`'s table.

## 4. Exercises, in `notes_raytracing.md` §11

Depth of field, motion blur, absorption in glass, dispersion, all the
cores (OCaml 5's domains), CSG of closed meshes, emissive surfaces,
denoising -- each a few dozen lines, each worth a paragraph of the
tutorial when done.
