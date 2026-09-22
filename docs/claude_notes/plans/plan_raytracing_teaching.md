# Plan: ray tracing, the other renderer, for teaching (`graphics/3d/`)

## Context

`graphics/3d/` teaches how a rasterizer works: project the triangles,
fill their pixels, keep the nearest (`notes_3d.md`,
`done/plan_code_reorg_teaching_3d.md`). Section 6 of that tutorial
already explains, in prose, the *other* way of making a picture --
**ray tracing**: instead of asking "which pixels does this triangle
cover?", ask, for each pixel, "what does the eye see through it?" --
and ends with "`Playground3d` is a rasterizer, not a ray tracer, on
both backends". This plan makes that paragraph executable.

The hook is already in place, which is most of why this is worth
doing now. `Shape3d_render_software.mli`
exposes

```ocaml
val faces : Playground3d.shape3d -> Render.face list
val camera : Playground3d.camera -> Camera.t
```

and a `Render.face` is a polygon **in world coordinates**, with a
colour or a texture, texture coordinates and a normal per point.
That is exactly a ray tracer's input, so the new renderer is a second
consumer of the existing scene, not a refactor of anything:

```ocaml
Raytrace.render ?options fb (camera cam) (faces shape)
```

And `-dump-frame n file` (`Native_loop_3d.ml`)
is already a separate code path from the 60 fps loop, so a renderer
that takes ten seconds a frame can live in this project without
slowing any game down by a microsecond.

**What it buys, and why it is the best remaining 3D lesson**: the
rasterizer here has no shadows at all (`TinyQuake.ml` bakes
lightmaps offline precisely to fake them -- which is itself a ray
caster, run once at startup), no reflections and no refraction.
A ray tracer gets each of those for a few lines, *on the same scene*,
because each one is just another ray. One key, two pictures, and the
whole trade-off of real-time graphics becomes visible.

**And an inversion worth teaching**: everywhere else in this project
the software backend is the humble one and the GPU backends are
"better". Here it is the opposite -- the software renderer produces
the highest-fidelity image in the repository, and the OpenGL and
WebGL backends *cannot* follow it (see Groundwork: the hardware
exists, the APIs we target do not expose it).

Companions: [`notes_raytracing.md`](../tutorials/notes_raytracing.md),
the tutorial (written ahead of the code, as its specification),
[`notes_raytracing_related_work.md`](../related-work/notes_raytracing_related_work.md)
(POV-Ray, PBRT, Cycles, smallpt, the RTX era, the ICFP entries, and
the ceiling here), and [`notes_3d.md`](../tutorials/notes_3d.md) (the
rasterizer this is the counterpart of; its section 6 gains a pointer
here).

## Prior art in the house: the author's ICFP 2000 entry

The author has written one of these before, and kept the log. The
**ICFP Programming Contest 2000** (Cornell and Bell Labs) set exactly
this task: a ray tracer driven by **GML**, a PostScript-like stack
language describing the scene. The entry is at
`~/Dropbox/Downloads/icfp-raytrace-2020/` (the folder's name says
2020; the contest was 2000), and it is worth reading before writing a
line of this plan's code, for three reasons.

**1. Its notes are a list of the bugs this plan will otherwise hit
again.** From `txt/history.txt`, with the hours they cost:

- the ray direction **must be normalized**, or the dot product is not
  a projection and the sphere's `tca` test is wrong;
- the sign in the ray/plane intersection (three hours, re-derived by
  hand from the task's formula);
- **shadow acne**: a shadow ray starting exactly on the surface hits
  the surface itself; the entry's fix is in `raytracer.ml` --
  `point_from_ray shadow_ray 0.001`, with the comment `(* acne pb *)`;
- **clamping**: light contributions summing past 1.0 wrapped round mod
  255, which is why "la sphere de l'autre cote etait pas belle";
- a directional light needs its own "distance to the light is
  infinite" case, or the shadow test compares against a finite
  distance and nothing is ever in shadow.

**2. Its structure is the phasing.** `txt/plan.txt` subdivides the
task the way this plan does below: ray casting against a plane, no
lighting at all; then spheres; then a point light with no shadow;
then the shadow ray; then specular; then transforms; then procedural
texturing. "difficulty = geometry 3d !!", says the note, which is
still true and is why the `.mli`s get diagrams.

**3. Its optimisation notes are the winners' notes.** `txt/optimisation.txt`
records, in real time, what the two OCaml teams at the top were
doing -- PLClub (University of Pennsylvania) won, **Camls 'R Us**
(INRIA: Ailleret, Cuoq, Doligez, Harley, Le Fessant, Leroy, Schmitt)
came second, both in OCaml. Their published techniques, which map
directly onto phases below:

- a **bounding sphere per object**, tested before the exact
  intersection: about **75% of the exact tests eliminated** (phase 3
  here does the same job with a BVH);
- the scene's top-level unions **rearranged into a tree** rather than
  a list (a BVH by another name);
- **attenuation cutoff**: track how much light a reflected ray can
  still contribute and stop the recursion when it is negligible
  (phase 4);
- specialising surface functions that are constant or constant per
  face, so the scripting language costs nothing per pixel (we have no
  scripting language -- see Groundwork -- so this one we get for
  free, and it is worth saying *why*).

`txt/bench.txt` has the era's number for scale: the `bigfib` scene, no
recursion and no specular, **123 s in bytecode**. A useful reminder of
what "too slow" meant in 2000, and of how much of the difference since
is algorithms rather than clock speed.

## Principles

The eight of [`../README.md`](../README.md), with three of this area's
own:

- **One scene, two renderers.** The ray tracer takes the *same*
  `Render.face list` as the rasterizer, from the same `shape3d` tree.
  No separate scene format, no exporter, no second set of examples to
  keep in sync -- and therefore a real A/B: the same frame, both ways,
  diffed.
- **The slow renderer never enters the frame loop.** It runs for
  `-dump-frame`, for a deliberately low-resolution preview key, and
  in the tests. Nothing in the 60 fps path changes, and no game gets
  slower.
- **Every existing example and game must ray trace with no change at
  all.** The lighting model (`Lighting`: one directional sun plus
  ambient) is kept for the first phases, so `-raytrace` on
  `Cubes3d.exe` is *the same picture, with shadows*. New material
  verbs (`shiny`, `glassy`) are additions a scene may opt into, which
  the rasterizer ignores.

## The Playground API, Evan-style

Mostly there is none, and that is the design: a ray tracer is a
*renderer*, not a vocabulary. A scene is still `shape3d`s, a camera is
still `Playground3d.camera`, and the new renderer is chosen by a flag
or a key. What the scenes need to opt into the extra physics of light
is three words, tentative and to be settled by writing the ICFP scenes
with them:

```ocaml
(* in Playground3d, like fade3d: a property of a shape the ray tracer
   honours and the rasterizer ignores *)
val shiny : number -> shape3d -> shape3d    (* 0. matte .. 1. mirror *)
val glassy : number -> shape3d -> shape3d   (* the index of refraction,
                                               1.5 for glass, 1.33 water *)
val lamp : color -> number -> shape3d       (* a light, with a radius:
                                               0. hard shadows, more =
                                               soft (phase 9) *)
```

`shiny` and `glassy` set a `material` on every leaf polygon, exactly
as `fade3d` sets `alpha` today -- the same mechanism, the same
one-line implementation, and a property no other backend has to
understand. `lamp` is a shape so that a light can be `move3d`d,
`group3d`ed and animated like everything else; the rasterizer draws it
as a small glowing ball (or nothing), the ray tracer shoots shadow
rays at it.

The rest is flags and keys (see Groundwork), not API.

## Target layout

```
graphics/3d/geometry/
  Ray                   NEW: a ray (origin, unit direction), and the
                        intersections: ray/triangle (Moller-Trumbore),
                        ray/sphere, ray/plane, ray/AABB (slabs).
                        In geometry/ because physics/3d's Collide3d
                        needs the same ray/triangle (see Groundwork)
graphics/3d/
  Bvh                   NEW: a bounding volume hierarchy over faces:
                        build (median split, then SAH), traverse;
                        beside the brute-force "test them all"
  Material              NEW: matte / shiny / glassy, and the reflection
                        and refraction directions (Snell, Schlick)
  Raytrace              NEW: the renderer -- camera rays, the recursive
                        shade (shadow, reflection, refraction), the
                        depth limit and the attenuation cutoff,
                        supersampling; same signature shape as Render
  Render                (exists) the rasterizer; its `face` gains a
                        `material` field, defaulting to matte
playground/software/
  Shape3d_render_software  (exists) `faces` fills the new field;
                        a `raytrace` entry point beside `render`
  Playground3d_platform (exists) the -raytrace flag, the preview key,
                        the ray-traced dump
examples/
  Raytracing*.ml        the new scenes (below), including the ICFP ones
graphics/tests/         ray/triangle against the analytic answers, the
                        BVH against brute force, the acne regression
tests/3d/               small golden ray-traced frames
```

`Raytrace` is in `graphics/3d/` and not in a directory of its own
because it is the same subject as `Render`, seen from the other end,
and the two should be read together.

## Groundwork decisions

### No GML, and why that is the interesting half

The ICFP task was two problems welded together: a stack language to
interpret, and a ray tracer to write. **We take only the second**
(the author's call, 2026-09-20): scenes are written programmatically
with the constructors this project already has (`sphere`, `plane`,
`cube`, `move3d`, `group3d`), like every other example here.

It is worth saying what that removes, because it is exactly what the
two winning teams spent their optimisation effort on: with GML, an
object's surface is a *closure* called per intersection point
(`(face, u, v) -> colour, kd, ks, n`), so a scene's texture is a
program and the interpreter sits in the innermost loop -- hence
PLClub's and Camls 'R Us's trick of detecting surface functions that
are constant, or constant per face, and never calling them again.
Writing scenes in OCaml gives us that specialisation for free, at the
price of not being able to load somebody else's `.gml`. We keep the
*idea* (a procedural surface is a function of `(face, u, v)`: phase 7
has it as an ordinary OCaml closure) and drop the parser.

### Where the ray primitive lives

`Ray` goes in `graphics/3d/geometry/`, beside `Vec3`, `Mat4`,
`Camera` and `Lighting`, rather than in `graphics/3d/`. The reason is
concrete: `physics/3d`'s `Collide3d`
([`plan_physics3d_teaching.md`](done/plan_physics3d_teaching.md), phase 4)
needs Möller-Trumbore too, for picking, bullets, ground checks and the
gravity gun, and `physics_3d` already depends on `graphics_3d_geometry`
and on nothing else. One implementation, tested once, used by the
renderer and by the engine -- and the `.mli` says both.

### Materials without breaking the other backends

`Render.face` gains `material : Material.t`, defaulting to matte, set
by `shiny`/`glassy` through `Shape3d_render_software.faces`. The
rasterizer ignores it (a later, optional use: a Blinn-Phong highlight
for `shiny`, which it *can* do). The OpenGL, WebGL and web/vdom
backends never see it: they read the `shape3d` tree and simply do not
implement those two verbs, which is already true of other things
(`Pixelate`, the magnifier). A scene using `glassy` still runs
everywhere; the glass just looks like plastic.

### Lights: the existing sun first, `lamp` later

`Lighting` is one fixed directional light (`light_dir`) plus ambient
0.25, shared by every backend. Phases 1-5 keep exactly that, which is
what makes "any existing scene, ray traced, with no edit" true -- and
it happens to be *the ICFP scenes' own lighting model* (`spheres.gml`
is a directional light `(1, -1, 0)` at intensity 1 with ambient 0.4).
Explicit `lamp`s, point and area, arrive with soft shadows in phase 9,
as an addition rather than a replacement.

### How it is run, and at what resolution

- `-raytrace`: `-dump-frame n file` renders that frame with
  `Raytrace` instead of `Render`, into an offscreen framebuffer.
- `-dump-size w h`: that framebuffer's size, independent of the
  window (a 1600x1200 still from an 800x600 game), with the HUD pass
  optional (`-no-hud`: the art shot).
- `-rt-samples n` (supersampling, 1 by default), `-rt-bounces n`
  (recursion depth, 3 by default -- the ICFP scenes' own value),
  `-rt-brute` (no BVH: the slow, readable path, for the comparison).
- A debug key, **"y"** (free in
  the software backend's `Playground3d_platform.ml`, where b, f, z, p,
  i, c, o, x, r, m, t, h are taken), ray traces the live frame at the
  current `Pixelate` resolution -- at "r"'s quarter resolution that is
  a preview in a fraction of a second, and it is what makes the
  comparison a *key press* rather than a command line.

### Determinism

No `Random` anywhere: supersampling is a fixed stratified grid, and
phase 9's soft shadows and path tracing take an explicit seed. That
is what lets `tests/3d/` hold golden ray-traced frames, and it follows
the project's rule (principle 5) rather than the field's habit.

### The GPU question, stated once

Modern GPUs do have ray tracing hardware -- NVIDIA's RTX from 2018
(Turing), AMD from RDNA 2 (2020), Intel Arc, Apple from M3 (2023):
dedicated units for BVH traversal and ray/triangle tests. It is
exposed through **DXR (DirectX 12), Vulkan ray tracing and Metal**,
and **not** through OpenGL (one dead vendor extension aside) or
WebGL; WebGPU has not shipped it. So the honest sentence for the docs
is: *our GPU backends cannot ray trace because of the APIs they
target, not because GPUs cannot* -- and even on an RTX card, games are
hybrid (rasterize what is directly visible, ray trace shadows and
reflections, denoise the result). Quake II RTX (NVIDIA, 2019) is the
famous full-path-traced exception, and a 1997 game was chosen for it
for a reason. (Dates from memory, to check.)

## The modules, with their references

- **Ray**: Tomas Möller and Ben Trumbore, "Fast, Minimum Storage
  Ray/Triangle Intersection" (Journal of Graphics Tools, 1997);
  ray/sphere and ray/AABB (the slab method, Kay and Kajiya 1986) from
  Christer Ericson, *Real-Time Collision Detection* (2005), chapter 5
  -- the same book `physics/3d` leans on.
- **Raytrace**: **Glassner (ed.), *An Introduction to Ray Tracing*
  (1989)** -- the book this is written out of: Haines on the essential
  algorithms (chapter 2), Hanrahan on intersections (3), Glassner on
  surface physics (4), Cook on stochastic sampling (5), Arvo and Kirk
  on acceleration (6), and Heckbert's "Writing a ray tracer" (7).
  Then Arthur Appel, "Some Techniques for Shading Machine
  Renderings of Solids" (1968), ray *casting* -- visibility and hard
  shadows, no recursion; **Turner Whitted, "An Improved Illumination
  Model for Shaded Display" (CACM, 1980)**, the recursive one:
  reflection, refraction, shadows, and the famous chrome-and-glass
  spheres over a checkerboard, which is still what everyone's first
  ray tracer draws. Andrew Glassner (ed.), *An Introduction to Ray
  Tracing* (1989).
- **Material**: Bui Tuong Phong (1975) and Jim Blinn (1977) for the
  highlight (the rasterizer's `Shading` already cites them); Snell's
  law for refraction; Christophe Schlick, "An Inexpensive BRDF Model
  for Physically-based Rendering" (1994) for the Fresnel
  approximation.
- **Bvh**: Kay and Kajiya (1986) for bounding volume hierarchies;
  the surface area heuristic (Goldsmith and Salmon, 1987; MacDonald
  and Booth, 1990). And, in this house, Camls 'R Us's 75% (above).
- **Phase 9**: Robert Cook, Thomas Porter, Loren Carpenter,
  "Distributed Ray Tracing" (SIGGRAPH 1984) -- soft shadows, glossy
  reflections, motion blur, all from jittered rays; James Kajiya,
  "The Rendering Equation" (SIGGRAPH 1986); Kevin Beason's **smallpt**
  (2007), a path tracer in 99 lines of C++, the model for how small
  this can be; Peter Shirley's *Ray Tracing in One Weekend* (2016-),
  the best modern on-ramp; Pharr, Jakob and Humphreys, *Physically
  Based Rendering* (2004-2023), the reference.
- **The contest**: the ICFP 2000 task (Cornell and Bell Labs), the
  author's entry and notes (above), and the published write-ups of
  PLClub (first) and Camls 'R Us (second), both OCaml.

## New examples

In `examples/`, named `Raytracing*` (the `Physics*` precedent), each
with golden frames, each runnable on every backend -- rasterized by
default, ray traced with "y" or `-raytrace`, which is the point:

- **`RaytracingSpheres3d.ml`** -- the ICFP `spheres.gml`, rebuilt with
  the playground's constructors: two matte spheres (0.8, 0.8, 0.2) at
  (-1.2, 0, 3) and (1.2, 1, 3), a matte white plane at y = -3, a
  directional light at (1, -1, 0), ambient 0.4, depth 3, field of view
  90. The first picture a ray tracer draws, and the first picture this
  one has to match.
- **`RaytracingFib3d.ml`** -- the ICFP `fib.gml`: a 5x5 grid of
  spheres whose *heights are Fibonacci numbers*, `y = fib(1 + (x+z)/2)`,
  cycling through six colours, the whole thing translated to
  (-7, -3, 3). In GML that scene was a program (the recursion was in
  the scene language); here it is four lines of OCaml, which is
  precisely the trade the Groundwork section describes.
- **`RaytracingWhitted3d.ml`** -- the 1980 picture: a mirror sphere
  and a glass sphere over a checkerboard, with their reflections of
  each other. The test of phases 4 and 5, and the image that says
  "this is not a rasterizer".
- **`RaytracingShadows3d.ml`** -- one scene, four keys: no shadows,
  hard shadows, the acne bug deliberately switched back on (the
  epsilon set to 0), and soft shadows (phase 9). Seeing acne once is
  worth a paragraph of explanation.
- **`RaytracingSplit3d.ml`** -- the same frame rasterized on the left
  half and ray traced on the right, with the two timings on screen.
  The whole plan in one window.
- **Existing scenes, no edit**: `Cubes3d.ml`, `Spheres3d.ml`,
  `TinyQuake.ml` and `TinyMinecraft.ml` ray traced with
  `-raytrace` for the README's stills -- and TinyQuake in
  particular is the interesting one, since its *baked* lightmaps and
  the ray tracer's real shadows can be compared on the same level.

## Phasing

0. **Groundwork**: `Ray` in `graphics/3d/geometry/` (ray/triangle,
   ray/sphere, ray/plane, ray/AABB) with its tests; `Raytrace` and
   `Material` skeletons; `Render.face`'s `material` field; the
   `-raytrace` flag and the "y" key wired to a stub that renders a
   blank frame.
1. **Ray casting** (Appel 1968): camera rays from `Camera.t`, nearest
   hit by brute force, the face's flat colour. No lighting, no
   shadows. `RaytracingSpheres3d.ml` in silhouette, and the first
   timing measured (to fill in).
2. **Shading and shadows**: `Lighting`'s own formula at the hit point
   (so the picture matches the rasterizer's), then one shadow ray per
   light -- the thing the rasterizer cannot do. The epsilon, the
   clamp, and the directional light's infinite distance: the ICFP
   entry's three bugs, each as a test. `RaytracingShadows3d.ml`.
3. **Making it fast**: `Bvh`, beside the brute-force path
   (`-rt-brute`), both giving identical images -- a property test on
   random scenes, as `physics/2d`'s broad phase has. The numbers (rays
   per second, nodes visited, the ratio) go in the `.mli` and the
   tutorial.
4. **Reflection**: the recursive call, the depth limit, and Camls 'R
   Us's attenuation cutoff, with a counter showing how many rays each
   saves. `shiny`.
5. **Refraction**: Snell, total internal reflection, Schlick's
   Fresnel. `glassy`; `RaytracingWhitted3d.ml` -- the 1980 image.
6. **The still-image pipeline**: `-dump-size`, stratified
   supersampling (`-rt-samples`), the optional HUD pass, PPM out (the
   existing dump path). `RaytracingSplit3d.ml`.
7. **Surfaces**: textures at the hit point (reusing `Texture` and the
   faces' uv), and procedural surfaces as OCaml closures
   `(face, u, v) -> colour` -- the ICFP surface function, without the
   interpreter. The checkerboard of `RaytracingWhitted3d.ml` becomes
   one of these.
8. **The ICFP scenes**: `RaytracingSpheres3d.ml` and
   `RaytracingFib3d.ml` finished and compared with the entry's own
   `.ppm` output where it still renders -- a twenty-six-year-old
   regression test.
9. *(optional)* **Distributed and path tracing**: `lamp` with a
   radius, jittered shadow rays (soft shadows), then a cosine-weighted
   path tracer with N samples per pixel (Kajiya 1986; smallpt as the
   size to beat). Minutes per frame, for the poster shot, clearly
   labelled as such.
10. **Docs**: `notes_raytracing.md` checked against the code and its
    numbers filled in; `notes_3d.md` section 6's pointer; the
    README.md's stills regenerated with `-raytrace`.

## Status

**Not started** (2026-09-20). Written as the specification, with
[`notes_raytracing.md`](../tutorials/notes_raytracing.md) beside it.
Decisions taken, with their reasons, so they are not re-argued:

- **the author asked for it** (2026-09-20), as a high-fidelity mode
  for `-dump-frame` stills, and asked what it would cost; the answer
  that made it worth planning is that `Shape3d_render_software.faces`
  already hands over exactly what a ray tracer needs;
- **no GML parser** (the author's call): scenes are written with the
  existing 3D constructors. The `.gml` files stay as a reference for
  *what* to draw, not as input;
- **the ICFP scenes are reproduced as `Raytracing*.ml`**,
  at the author's asking -- `spheres.gml` and `fib.gml` first;
- the contest was **ICFP 2000**, not 2020 (the local folder is
  misnamed; noted here so the next reader does not repeat it);
- `Ray` goes in `graphics/3d/geometry/`, shared with `physics/3d`;
- the existing `Lighting` sun stays for phases 1-5, so every existing
  scene ray traces unchanged.

- **the 1980s book, settled** (the author's pick, 2026-09-20):
  **Glassner (ed.), *An Introduction to Ray Tracing* (1989)**, whose
  nine chapters map almost one to one onto this plan's modules (the
  table is in
  [`notes_raytracing_related_work.md`](../related-work/notes_raytracing_related_work.md)).
  It is the reference `Raytrace.mli` opens with; Heckbert's chapter 7,
  "Writing a ray tracer", is the closest thing to this plan written
  thirty-seven years earlier.

## Verification

- `make test` (`graphics/tests/`): ray/triangle and ray/sphere against
  hand-computed intersections and against analytic answers; the BVH
  returning exactly brute force's hit on random scenes (the property
  test); the shadow-acne regression (a plane lit from above: with the
  epsilon at 0 it is stippled, with it set it is not); the clamp (a
  point lit by three lights stays at most white).
- **Golden frames** in `tests/3d/`, small and deterministic: the
  golden runner cannot pass flags, but it *can* press keys, so a
  ray-traced golden scene is `("examples/software/RaytracingSpheres3d", "ry", 1)`
  -- quarter resolution with "r", then "y". Cheap enough to run in
  `make test`; the big ones stay behind `make test-golden-all`.
- **The numbers, measured rather than asserted**: rays per second
  brute force vs BVH, the frame time at 1x and 4x supersampling, the
  rays saved by the attenuation cutoff, and the rasterizer's own time
  on the same frame for scale.
- By eye, once: `RaytracingWhitted3d.ml` against the 1980 picture.

## Out of scope

- Real-time ray tracing, denoisers, and anything in the 60 fps path.
- A GML interpreter (above), and any scene file format.
- Spectral rendering, volumetrics, participating media, caustics via
  photon mapping, subsurface scattering.
- GPU ray tracing: our OpenGL and WebGL backends cannot, and adding a
  Vulkan or Metal backend for it is a different project.
- Displacement, normal and bump maps; anisotropic materials.
- Animation rendered offline (a sequence of stills is a shell loop,
  not a feature).
