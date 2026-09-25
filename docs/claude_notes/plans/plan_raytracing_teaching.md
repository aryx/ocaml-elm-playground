# Plan: ray tracing, the other renderer, for teaching (`graphics/3d/`), and the `Povray` way

## Context

`graphics/3d/` teaches how a rasterizer works: project the triangles,
fill their pixels, keep the nearest (`notes_3d.md`,
`done/plan_code_reorg_teaching_3d.md`). Section 6 of that tutorial
already explains, in prose, the *other* way of making a picture --
**ray tracing**: instead of asking "which pixels does this triangle
cover?", ask, for each pixel, "what does the eye see through it?" --
and ends with "`Playground3d` is a rasterizer, not a ray tracer, on
both backends". This plan makes that paragraph executable, in two
halves:

1. **The renderer**, `Raytrace` in `graphics/3d/`, and the second
   consumer of the existing scene: any `shape3d` scene ray traced with
   `-raytrace` or the "y" key, the same picture with shadows. This is
   the A/B lesson, and it is what the first version of this plan was.
2. **A way, `Povray`** (`playground/ways/`), revised in 2026-09-25: a
   scene language with what a triangle list cannot say -- exact
   spheres, infinite planes, **constructive solid geometry**,
   procedural surfaces, lights, glass -- and an `app` that ray traces
   it progressively, the picture filling in row after row as POV-Ray's
   window did. This is the expressive half, and the one programs are
   written on.

And a payoff: **TinyMyst** (`games/adventure/`), whose island is a
`Povray` scene, its stills rendered by this ray tracer as Myst's were
by StrataVision 3D (1993) -- the ray tracer as a game's art pipeline.

The hook for half 1 is already in place, which is most of why this is
worth doing now. `Shape3d_render_software.mli` exposes

```ocaml
val faces : Playground3d.shape3d -> Render.face list
val camera : Playground3d.camera -> Camera.t
```

and a `Render.face` is a polygon **in world coordinates**, with a
colour or a texture, texture coordinates and a normal per point.
That is exactly a ray tracer's input for triangles. And `-dump-frame n
file` (`Native_loop_3d.ml`) is already a separate code path from the
60 fps loop, so a renderer that takes ten seconds a frame can live in
this project without slowing any game down by a microsecond.

**What it buys, and why it is the best remaining 3D lesson**: the
rasterizer here has no shadows at all (`TinyQuake.ml` bakes
lightmaps offline precisely to fake them -- which is itself a ray
caster, run once at startup), no reflections and no refraction.
A ray tracer gets each of those for a few lines, *on the same scene*,
because each one is just another ray. One key, two pictures, and the
whole trade-off of real-time graphics becomes visible.

**Why a way too.** Fed only the rasterizer's triangles, the ray tracer
inherits the rasterizer's limits: a sphere arrives faceted, a plane
arrives as a finite quad, and a lens -- two spheres intersected --
cannot be said at all. What makes ray tracing *expressive*, not only
prettier, is that an object is anything a ray can be intersected
with: a quadric solved exactly, and CSG, which is nearly free for a
ray (intersect the intervals where the ray is inside) and nearly
impossible for a rasterizer. That is what POV-Ray's scene language
(1991, from DKBTrace, 1986) and the ICFP 2000 task's GML were built
around, and what a way can give a program in a dozen verbs.

**And an inversion worth teaching**: everywhere else in this project
the software backend is the humble one and the GPU backends are
"better". Here it is the opposite -- the software renderer produces
the highest-fidelity image in the repository, and the OpenGL and
WebGL backends *cannot* follow it (see Groundwork: the hardware
exists, the APIs we target do not expose it). And since `Raytrace` is
pure OCaml drawing into an `Rgba_image`, a `Povray` program runs in a
browser too, compiled by js_of_ocaml: the slow renderer is the most
portable one.

Companions: [`notes_raytracing.md`](../tutorials/notes_raytracing.md),
the tutorial (written ahead of the code, as its specification; it
gains a section on CSG and one on the way),
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
line of this plan's code, for four reasons.

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

**4. Its scene language is the `Povray` way's vocabulary.** GML's
operators, less the stack machine: `sphere`, `cube`, `cylinder`,
`cone`, `plane`; `union`, `intersect`, `difference`; `translate`,
`scale`, `rotatex`...; `light`, `pointlight`, `spotlight`; a surface
as a function of `(face, u, v)`. The task came in three tiers (tier 1
spheres and planes, tier 2 the other solids and point lights, tier 3
CSG's intersection and difference and spotlights); the way's phases
follow the tiers.

`txt/bench.txt` has the era's number for scale: the `bigfib` scene, no
recursion and no specular, **123 s in bytecode**. A useful reminder of
what "too slow" meant in 2000, and of how much of the difference since
is algorithms rather than clock speed.

## Principles

The eight of [`../README.md`](../README.md), with four of this area's
own:

- **One renderer, two ways in.** `Raytrace` takes one object type:
  analytic primitives, CSG nodes, and triangles as one primitive among
  the others. The `shape3d` path turns a scene into triangles (via
  `faces`, unchanged); the `Povray` way builds the richer objects
  directly. There is still no scene file format, no exporter, and the
  A/B stays real: the same `shape3d` frame, both ways, diffed.
- **The slow renderer never enters the frame loop.** On the `shape3d`
  path it runs for `-dump-frame`, for a deliberately low-resolution
  preview key, and in the tests. On the `Povray` path the renderer
  *is* the app, but it renders a bounded slice per frame
  (progressively, see Groundwork), so the window never stops
  answering. Nothing in any game's 60 fps path changes.
- **Every existing example and game must ray trace with no change at
  all.** The lighting model (`Lighting`: one directional sun plus
  ambient) is kept for the `shape3d` path, so `-raytrace` on
  `Cubes3d.exe` is *the same picture, with shadows*. The `shape3d`
  scenes get two opt-in verbs (`shiny`, `glassy`, below) that the
  rasterizer ignores; everything else expressive lives in the way.
- **Deterministic, whatever the slicing.** A picture rendered in one go
  and the same picture rendered a row per frame, or coarse-to-fine,
  are the same bytes (a test). No `Random`; seeds are explicit.

## The Playground API, Evan-style

### On `shape3d`: two verbs, and no more

A ray tracer is first a *renderer*: on the `shape3d` path a scene is
still `shape3d`s, a camera still `Playground3d.camera`, and the new
renderer is chosen by a flag or a key. What those scenes need to opt
into the physics of light is two words:

```ocaml
(* in Playground3d, like fade3d: a property of a shape the ray tracer
   honours and the rasterizer ignores *)
val shiny : number -> shape3d -> shape3d    (* 0. matte .. 1. mirror *)
val glassy : number -> shape3d -> shape3d   (* the index of refraction,
                                               1.5 for glass, 1.33 water *)
```

`shiny` and `glassy` set a `material` on every leaf polygon, exactly
as `fade3d` sets `alpha` today -- the same mechanism, the same
one-line implementation, and a property no other backend has to
understand. (The first version of this plan had a third, `lamp`;
lights now belong to the way, where a scene says what it is lit by,
and the `shape3d` path keeps `Lighting`'s sun.)

### The `Povray` way

A way, like `Logo` or `Puzzlescript`: a vocabulary and an `app` built
for you, so a program written on it has no `update` and no `view`. The
names are POV-Ray's where POV-Ray has one, GML's otherwise.
Tentative, to be settled by writing the scenes below with them:

```ocaml
type obj            (* a solid: it has an inside, so it can be CSG'd *)
type surface        (* a colour, a pattern, or a function of the point *)
type light
type scene

(* the solids, each intersected exactly (a quadric solved, not
   tessellated): unit-sized, centred at the origin, then moved *)
val sphere : surface -> obj
val box : surface -> obj
val cylinder : surface -> obj
val cone : surface -> obj
val torus : number -> surface -> obj      (* the tube's radius; a quartic *)
val plane : surface -> obj                (* y = 0, infinite, the inside below *)

(* constructive solid geometry: where the ray is inside *)
val union : obj list -> obj
val inter : obj list -> obj
val diff : obj -> obj -> obj              (* the first minus the second *)

(* moving a solid: the ray is moved the other way (the inverse matrix),
   which is why a scaled sphere is an ellipsoid for free *)
val move : number -> number -> number -> obj -> obj
val scale : number -> number -> number -> obj -> obj
val rotate : number -> number -> number -> obj -> obj

(* the rasterizer's world, brought in: a shape3d as triangles, so that
   TinyQuake's level or a Logo3d tree can stand in a Povray scene *)
val shapes : Playground3d.shape3d list -> obj

(* surfaces *)
val color : Playground.color -> surface
val checker : Playground.color -> Playground.color -> surface
val marble : Playground.color -> Playground.color -> surface  (* Perlin noise *)
val wood : Playground.color -> Playground.color -> surface
val image : string -> surface                                 (* a texture file *)
val pattern : (x:number -> y:number -> z:number -> Playground.color) -> surface
                                          (* GML's surface function *)
val shiny : number -> surface -> surface  (* the finish: reflection *)
val glassy : number -> surface -> surface (* refraction, the index *)
val highlight : number -> surface -> surface  (* Phong's exponent *)

(* lights *)
val sun : Playground.color -> number -> number -> number -> light
                                          (* directional, towards (x, y, z) *)
val lamp : Playground.color -> number -> number -> number -> light
                                          (* a point: hard shadows *)
val spot : ... -> light
val area_lamp : number -> light -> light  (* a radius: soft shadows (phase 9) *)

(* the scene, and the app *)
val scene : ?ambient:number -> ?sky:Playground.color -> camera:Playground3d.camera
  -> light list -> obj list -> scene
val still : scene -> (Playground.screen, msg) Playground.app
val orbit : scene -> (model, msg) Playground.app    (* the mouse turns the camera *)
val animation : (Playground.time -> scene) -> (model, msg) Playground.app
                                          (* frame by frame, each progressive *)
```

A first program, the one every ray tracer draws first:

```ocaml
open Povray
let app =
  still (scene ~camera:(Playground3d.camera ...)
    [ lamp white 5. 10. (-5.) ]
    [ plane (checker white black);
      move (-1.) 1. 0. (sphere (shiny 0.9 (color grey)));
      move 1.2 1. 1. (sphere (glassy 1.5 (color white)));
      diff (box (color red)) (scale 1.3 1.3 1.3 (sphere (color red))) ])
let main = Playground_platform.run_app app
```

**What the app is**: a 2D Playground `app` whose `view` is one
`Playground.bitmap` (the `shape` TinyPhotoshop and TinyMediaPlayer
already draw pixels with), and whose `update` renders the next slice
of the picture into it (see Groundwork, progressive rendering). A
`Povray` program therefore runs on every 2D backend, Cairo, software
and the browser, with no 3D backend at all. Keys: space restarts, "s"
exports the finished picture as a PNG (`Playground_platform.export`),
"1".."4" the samples per pixel, "b" the bounces; a line of HUD says
the rows done, the rays shot, and the time so far.

## Target layout

```
graphics/3d/geometry/
  Ray                   NEW: a ray (origin, unit direction), and the
                        intersections: ray/triangle (Moller-Trumbore),
                        ray/sphere, ray/plane, ray/AABB (slabs).
                        In geometry/ because physics/3d's Collide3d
                        needs the same ray/triangle (see Groundwork)
graphics/3d/raytrace/   NEW, its own library (graphics_raytrace),
                        pure, over geometry/ and images' Rgba_image
  Solid                 the object type: primitives (sphere, box,
                        cylinder, cone, torus, plane, triangle), CSG
                        nodes, transforms; each solid's hit *intervals*
                        along a ray (entering, leaving), which is what
                        CSG combines
  Csg                   union, intersection, difference of interval
                        lists, and the normal flipped on a subtracted
                        surface -- small, and the lesson of its own
  Bvh                   a bounding volume hierarchy over solids: build
                        (median split, then SAH), traverse; beside the
                        brute-force "test them all"
  Material              matte / shiny / glassy, and the reflection and
                        refraction directions (Snell, Schlick)
  Pattern               checker, marble, wood, over Noise (Perlin 1985)
  Raytrace              the renderer: camera rays, the recursive shade
                        (shadow, reflection, refraction), the depth
                        limit and the attenuation cutoff, supersampling;
                        [render_rows] as well as [render], for the
                        progressive app
graphics/3d/
  Render                (exists) the rasterizer; its `face` gains a
                        `material` field, defaulting to matte
playground/ways/
  Povray                NEW: the way above -- the vocabulary over
                        Solid, and the progressive app over bitmap
playground/platforms/software/
  Shape3d_render_software  (exists) `faces` fills the new field;
                        `solids` turns the faces into Solid triangles
  Playground3d_platform (exists) the -raytrace flag, the preview key,
                        the ray-traced dump
examples/
  Raytracing*.ml        the `shape3d` scenes ray traced (below)
  Povray*.ml            the way's scenes, including the ICFP ones
games/adventure/
  TinyMyst.ml           the payoff (below), and its stills
graphics/tests/         ray/primitives against the analytic answers,
                        CSG's intervals, the BVH against brute force,
                        the acne regression, progressive = at once
tests/2d/, tests/3d/    small golden frames (the way's in 2d/: it is a
                        2D app)
```

`Raytrace` is in `graphics/3d/` because it is the same subject as
`Render`, seen from the other end, and the two should be read
together; it has its own library because the way needs it without the
3D backends' rasterizer (see Groundwork).

## Groundwork decisions

### No GML parser, and what we keep of GML

The ICFP task was two problems welded together: a stack language to
interpret, and a ray tracer to write. **We take only the second**
(the author's call, 2026-09-20): scenes are written programmatically,
in OCaml, like every other example here.

It is worth saying what that removes, because it is exactly what the
two winning teams spent their optimisation effort on: with GML, an
object's surface is a *closure* called per intersection point
(`(face, u, v) -> colour, kd, ks, n`), so a scene's texture is a
program and the interpreter sits in the innermost loop -- hence
PLClub's and Camls 'R Us's trick of detecting surface functions that
are constant, or constant per face, and never calling them again.
Writing scenes in OCaml gives us that specialisation for free, at the
price of not being able to load somebody else's `.gml`.

What the revision of 2026-09-25 adds: we **keep GML's vocabulary**
-- its solids, its CSG, its lights, its surface functions -- as the
`Povray` way. The parser was the uninteresting half; the solids and
CSG were the interesting geometry, and the first version of this plan
had dropped them along with the parser, by feeding the ray tracer only
triangles.

### Where the way sits, and what it depends on

`Povray` is a way (it builds the `app`: `playground/README.md`'s rule),
so it goes in `playground/ways/`. It uses `Playground3d.camera` (the
same camera record, so a scene can be framed as the `shape3d` scenes
are) and `shapes` takes `shape3d`s, so it belongs to the 3D stanza of
`playground/dune`, listed by name as `Logo3d` is. Its app is
nevertheless a 2D `app` (a bitmap), so a `Povray` program runs on the
2D backends; whether that needs the 3D library to stop being virtual
for such a program, or `shapes` to move out, is **to be settled in
phase 0** by trying it -- the fallback is to drop `shapes` from the
way and keep it in `examples/`. `graphics_raytrace` depends only on
`graphics_3d_geometry` and `Rgba_image`, both pure, so it compiles to
JavaScript.

### Progressive rendering

The way's `update` gets a budget per frame (a number of rays, adjusted
from the frame time so the app stays near 30 fps) and renders that
many more pixels. The order is **coarse to fine**, as POV-Ray's
"mosaic preview" and every modern viewport: first one ray per 8x8
block drawn as a block, then 4x4, 2x2, then every pixel, then the
supersamples -- each pass reusing the rays of the one before (a
pixel's first sample is its block's corner). So a picture is
recognisable in a fraction of a second and sharpens while you watch,
and dragging the camera (`orbit`) restarts at 8x8: interactive
exploration of a ray-traced scene, with no GPU.

`Raytrace.render_rows` and `Raytrace.render_pixels` are what the way
calls; the test is that rendering in slices, in any order, gives the
same bytes as `Raytrace.render` in one go.

### Where the ray primitive lives

`Ray` goes in `graphics/3d/geometry/`, beside `Vec3`, `Mat4`,
`Camera` and `Lighting`, rather than in `graphics/3d/`. The reason is
concrete: `physics/3d`'s `Collide3d`
([`plan_physics3d_teaching.md`](done/plan_physics3d_teaching.md), phase 4)
needs Möller-Trumbore too, for picking, bullets, ground checks and the
gravity gun, and `physics_3d` already depends on `graphics_3d_geometry`
and on nothing else. One implementation, tested once, used by the
renderer and by the engine -- and the `.mli` says both.

### CSG with intervals

A solid's intersection with a ray is not one point but a list of
intervals `[t_in, t_out]` where the ray is inside (a sphere gives one,
a torus up to two, a plane a half-line). Union, intersection and
difference are then list operations on sorted intervals -- the
merge of two sorted lists, a lesson any student can check by hand --
and the visible hit is the first `t_in` in front of the eye. The
normal at a `t_in` that came from a subtracted solid's `t_out` is that
solid's normal flipped (the inside of the hole faces out). Scott Roth,
"Ray Casting for Modeling Solids" (1982), is the paper; the worked
example in `Csg.mli` is the ICFP's `holes` scene: a cube minus three
cylinders.

Triangles have no inside, so `shapes` gives solids that can be
unioned but not intersected or subtracted: `inter` and `diff` of a
mesh are refused with a message (a closed mesh could have an inside by
ray parity -- an exercise).

### Materials without breaking the other backends

`Render.face` gains `material : Material.t`, defaulting to matte, set
by `shiny`/`glassy` through `Shape3d_render_software.faces`. The
rasterizer ignores it (a later, optional use: a Blinn-Phong highlight
for `shiny`, which it *can* do). The OpenGL, WebGL and web/vdom
backends never see it: they read the `shape3d` tree and simply do not
implement those two verbs, which is already true of other things
(`Pixelate`, the magnifier). A scene using `glassy` still runs
everywhere; the glass just looks like plastic.

### Lights: the existing sun on `shape3d`, the scene's own in the way

`Lighting` is one fixed directional light (`light_dir`) plus ambient
0.25, shared by every backend. The `shape3d` path keeps exactly that,
which is what makes "any existing scene, ray traced, with no edit"
true -- and it happens to be *the ICFP scenes' own lighting model*
(`spheres.gml` is a directional light `(1, -1, 0)` at intensity 1 with
ambient 0.4). The way's scenes list their own lights: sun, lamp, spot
(tier 3 of the task), and area lamps with soft shadows in phase 9.

### How the `shape3d` path is run, and at what resolution

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

No `Random` anywhere: supersampling is a fixed stratified grid, noise
is Perlin's permutation table written out, and phase 9's soft shadows
and path tracing take an explicit seed (`Lehmer`). That is what lets
the tests hold golden ray-traced frames, and it follows the project's
rule (principle 5) rather than the field's habit.

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
- **Solid**: Pat Hanrahan's chapter 3 of Glassner (below), the
  quadrics and the torus's quartic; the transformed ray (the inverse
  matrix, and why the normal is transformed by its transpose).
- **Csg**: Scott D. Roth, "Ray Casting for Modeling Solids" (Computer
  Graphics and Image Processing, 1982); Requicha's representation of
  solids (1980) for the background; POV-Ray's `union`/`intersection`/
  `difference`/`merge` and GML's tier 3.
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
  ray tracer draws.
- **Material**: Bui Tuong Phong (1975) and Jim Blinn (1977) for the
  highlight (the rasterizer's `Shading` already cites them); Snell's
  law for refraction; Christophe Schlick, "An Inexpensive BRDF Model
  for Physically-based Rendering" (1994) for the Fresnel
  approximation.
- **Pattern**: Ken Perlin, "An Image Synthesizer" (SIGGRAPH 1985) --
  noise, and marble as `sin (x + turbulence)`; Darwyn Peachey's
  "Solid Texturing of Complex Surfaces" (same SIGGRAPH), wood as rings.
- **Bvh**: Kay and Kajiya (1986) for bounding volume hierarchies;
  the surface area heuristic (Goldsmith and Salmon, 1987; MacDonald
  and Booth, 1990). And, in this house, Camls 'R Us's 75% (above).
- **Povray**: POV-Ray's scene description language (the POV-Team,
  1991-; David Buck's DKBTrace, 1986, before it) for the names; its
  documentation's tutorial, whose first scene the way's first example
  follows.
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
- **TinyMyst**: Rand and Robyn Miller, *Myst* (Cyan, Brøderbund,
  1993), made in HyperCard with its stills rendered in StrataVision
  3D on Macintoshes; the Millers' own accounts of the making (to find
  and cite before writing the header).

## New examples

**On `shape3d`**, in `examples/`, named `Raytracing*` (the `Physics*`
precedent), rasterized by default, ray traced with "y" or
`-raytrace`, which is the point:

- **`RaytracingShadows3d.ml`** -- one scene, four keys: no shadows,
  hard shadows, the acne bug deliberately switched back on (the
  epsilon set to 0), and soft shadows (phase 9). Seeing acne once is
  worth a paragraph of explanation.
- **`RaytracingSplit3d.ml`** -- the same frame rasterized on the left
  half and ray traced on the right, with the two timings on screen.
  The first half of the plan in one window.
- **Existing scenes, no edit**: `Cubes3d.ml`, `Spheres3d.ml`,
  `TinyQuake.ml` and `TinyMinecraft.ml` ray traced with
  `-raytrace` for the README's stills -- and TinyQuake in
  particular is the interesting one, since its *baked* lightmaps and
  the ray tracer's real shadows can be compared on the same level.

**On the way**, in `examples/`, named `Povray*`, each a 2D app with
its golden frame (the finished picture, small):

- **`PovraySpheres.ml`** -- the ICFP `spheres.gml`: two matte spheres
  (0.8, 0.8, 0.2) at (-1.2, 0, 3) and (1.2, 1, 3), a matte white
  plane at y = -3, a directional light at (1, -1, 0), ambient 0.4,
  depth 3, field of view 90. The first picture a ray tracer draws, and
  the first picture this one has to match -- now with exact spheres.
- **`PovrayFib.ml`** -- the ICFP `fib.gml`: a 5x5 grid of spheres
  whose *heights are Fibonacci numbers*, `y = fib(1 + (x+z)/2)`,
  cycling through six colours, the whole thing translated to
  (-7, -3, 3). In GML that scene was a program (the recursion was in
  the scene language); here it is four lines of OCaml, which is
  precisely the trade the Groundwork section describes.
- **`PovrayWhitted.ml`** -- the 1980 picture: a mirror sphere and a
  glass sphere over a checkerboard, with their reflections of each
  other. The test of phases 4 and 5, and the image that says "this is
  not a rasterizer".
- **`PovrayCsg.ml`** -- the ICFP's `holes` and `dice`: a cube minus
  three cylinders, a die as a box intersected with a sphere minus its
  pips; and a glass lens, two spheres intersected, focusing the
  checkerboard. `orbit`, so the holes can be looked through.
- **`PovrayMarble.ml`** -- patterns: marble, wood, a checker, and a
  `pattern` closure, on the same four spheres.
- **`PovrayQuake.ml`** -- `shapes` at work: TinyQuake's first room
  as triangles, with a glass sphere and a lamp added by the way.

## The payoff: TinyMyst

*Myst* (1993) was a slideshow of pre-rendered stills -- about 2,500 --
with hotspots: click the right part of the picture and you move to
the next still, or pull a lever and see the picture of the lever
pulled. It was built in HyperCard, its stills rendered on Macintoshes
with StrataVision 3D, which is to say *by a ray tracer*, overnight.
That is exactly the shape of what this plan builds, turned into a
game:

- **The island is a `Povray` scene**: a dock, a library, a tower, a
  few trees as `shapes` from `Logo3d` (the L-system trees of
  `LogoFractals3d`), the sea a plane with a pattern. A **node** is a
  camera (a position and a heading); a **state** is what can change
  (the lever up or down, a door open, the tower turned). A still is
  `Raytrace.render` of the scene in a state, from a node.
- **Hotspots come from the ray tracer too**. A click is a ray: cast it
  from the node's camera through the clicked pixel, and the solid it
  hits carries a name (`named "lever"`, an extra verb of the way) --
  so the game's clickable regions are the objects themselves, not
  rectangles drawn by hand as Cyan's were. One primitive, two uses:
  picture and picking, exactly as `Ray` serves both the renderer and
  `physics/3d`.
- **The stills are made once, not at every start**. Following the
  repository's precedent for slow-to-make data (`make_photos.sh`, the
  video clips' `make_clips.sh`): a script ray traces every (node,
  state) pair at 512x342 (the Mac's screen, as TinyHyperCard's cards)
  into PNGs, committed, embedded by dune; the game shows them. A flag
  `render=live` ray traces them in the game instead, progressively --
  the way's app inside a game, the lesson made visible. A test renders
  one node at low resolution and compares it with the committed still,
  so the script's output cannot drift from the scene.
- **The game logic: to be decided** (see Status). Either plain OCaml
  data (a node's exits, a hotspot's action), or -- truer to the
  original -- a HyperTalk script per card, run by
  `appkits/hypertalk`, TinyHyperCard's language (`on mouseUp` / `go
  to card "tower"` / `set the lever to "up"`). Myst *was* a HyperCard
  stack; TinyMyst could be one too.
- **Tiny**: one island, eight to twelve nodes, one puzzle (the
  original's marker switches: find and flip them, count them, and the
  number opens the library's secret), two linking books, and an end.
  Transitions: a cut, and optionally Myst's dissolve (the first
  still's pixels fading to the next's).

It lives in `games/adventure/` (with Zork and Maniac Mansion: the
graphic adventure, its fourth style), is 2D in `CATALOG.md`'s sense (it
draws bitmaps), and gets its catalogue row, golden frames and web page
like any game.

## Phasing

0. **Groundwork**: `Ray` in `graphics/3d/geometry/` (ray/triangle,
   ray/sphere, ray/plane, ray/AABB) with its tests; the
   `graphics/3d/raytrace/` library with `Solid`, `Raytrace` and
   `Material` skeletons; `Render.face`'s `material` field; the
   `-raytrace` flag and the "y" key wired to a stub; `Povray`'s empty
   app showing a bitmap, and the dune question (where the way sits)
   settled.
1. **Ray casting** (Appel 1968): camera rays from `Camera.t`, nearest
   hit by brute force, the flat colour. No lighting, no shadows. On
   both paths: a `shape3d` scene in silhouette, and `PovraySpheres.ml`
   with exact spheres and the infinite plane (GML's tier 1). The first
   timing measured (to fill in).
2. **Shading and shadows**: `Lighting`'s own formula at the hit point
   on the `shape3d` path (so the picture matches the rasterizer's),
   the scene's sun and lamps on the way's; one shadow ray per light --
   the thing the rasterizer cannot do. The epsilon, the clamp, and the
   directional light's infinite distance: the ICFP entry's three bugs,
   each as a test. `RaytracingShadows3d.ml`.
3. **The progressive app**: `render_rows`, the ray budget per frame,
   coarse to fine, `orbit`; the "same bytes whatever the slicing" test.
   From here on the way is pleasant to use, which is why it comes
   before the speed.
4. **Making it fast**: `Bvh`, beside the brute-force path
   (`-rt-brute`), both giving identical images -- a property test on
   random scenes, as `physics/2d`'s broad phase has. The numbers (rays
   per second, nodes visited, the ratio; natively and in the browser)
   go in the `.mli` and the tutorial.
5. **Reflection and refraction**: the recursive call, the depth limit,
   Camls 'R Us's attenuation cutoff with a counter showing how many
   rays it saves; Snell, total internal reflection, Schlick's Fresnel.
   `shiny`, `glassy` on both paths; `PovrayWhitted.ml` -- the 1980
   image.
6. **The other solids and CSG** (GML's tiers 2 and 3): box, cylinder,
   cone, torus, transforms by the inverse ray; `Csg`'s intervals;
   spots. `PovrayCsg.ml`.
7. **Surfaces**: textures at the hit point (reusing `Texture` and the
   faces' uv), `Pattern` over noise, and `pattern` closures -- the
   ICFP surface function, without the interpreter. `PovrayMarble.ml`;
   `PovrayFib.ml` and the ICFP scenes compared with the entry's own
   `.ppm` output where it still renders -- a twenty-six-year-old
   regression test.
8. **The still-image pipeline** on the `shape3d` path: `-dump-size`,
   stratified supersampling (`-rt-samples`), the optional HUD pass.
   `RaytracingSplit3d.ml`; `shapes` and `PovrayQuake.ml`.
9. *(optional)* **Distributed and path tracing**: `area_lamp`, jittered
   shadow rays (soft shadows), then a cosine-weighted path tracer with
   N samples per pixel (Kajiya 1986; smallpt as the size to beat). The
   progressive app is what makes this bearable: the noisy picture
   converging while you watch.
10. **TinyMyst**: `named` and picking; the island and its nodes; the
    stills' script; the game, and its golden frames. Done last on
    purpose: it is the proof that the way is enough to build something
    on.
11. **Docs**: `notes_raytracing.md` checked against the code, its
    numbers filled in and its new sections (CSG, the progressive app,
    the way, TinyMyst's pipeline); `notes_3d.md` section 6's pointer;
    `playground/README.md`'s `ways/` row.

## Status

**Not started.** Written as the specification, with
[`notes_raytracing.md`](../tutorials/notes_raytracing.md) beside it.
Decisions taken, with their reasons, so they are not re-argued:

- **the author asked for it** (2026-09-20), as a high-fidelity mode
  for `-dump-frame` stills, and asked what it would cost; the answer
  that made it worth planning is that `Shape3d_render_software.faces`
  already hands over exactly what a ray tracer needs;
- **no GML parser** (the author's call): scenes are written in OCaml.
  The `.gml` files stay as a reference for *what* to draw, not as
  input;
- **the ICFP scenes are reproduced**, at the author's asking --
  `spheres.gml` and `fib.gml` first (now as `Povray*.ml`, written on
  the way, rather than as `shape3d` scenes);
- the contest was **ICFP 2000**, not 2020 (the local folder is
  misnamed; noted here so the next reader does not repeat it);
- `Ray` goes in `graphics/3d/geometry/`, shared with `physics/3d`;
- the existing `Lighting` sun stays for the `shape3d` path, so every
  existing scene ray traces unchanged;
- **the 1980s book, settled** (the author's pick, 2026-09-20):
  **Glassner (ed.), *An Introduction to Ray Tracing* (1989)**, whose
  nine chapters map almost one to one onto this plan's modules (the
  table is in
  [`notes_raytracing_related_work.md`](../related-work/notes_raytracing_related_work.md)).
  It is the reference `Raytrace.mli` opens with; Heckbert's chapter 7,
  "Writing a ray tracer", is the closest thing to this plan written
  thirty-seven years earlier;
- **the revision of 2026-09-25** (the author's idea: "a way around the
  3D scene description, but with more expressivity"): the `Povray`
  way, GML's vocabulary without its parser; the renderer's input
  widened from faces to solids; `lamp` moved from `Playground3d` to
  the way; the progressive app; TinyMyst as the payoff (the author
  liked Myst's idea in the same conversation).

Open, for the author:

- the way's **name**: `Povray` (the convention: a way is named after
  what it borrows, `Logo`, `Puzzlescript`, `Karel`), or `Gml` (the
  contest's, and the author's own history with it);
- **TinyMyst's logic**: OCaml data, or HyperTalk cards through
  `appkits/hypertalk` (truer to 1993, and a second user for the
  appkit, but HyperTalk's subset may need a verb or two);
- **where `Povray` sits** if the 3D stanza turns out to be awkward for
  a 2D app (phase 0 decides by trying; moving a module is asked first).

## Verification

- `make test` (`graphics/tests/`): ray/triangle, ray/sphere,
  ray/cylinder and the torus's quartic against hand-computed
  intersections and analytic answers; CSG's interval operations on
  worked examples (the `.mli`'s); the BVH returning exactly brute
  force's hit on random scenes (the property test); the shadow-acne
  regression (a plane lit from above: with the epsilon at 0 it is
  stippled, with it set it is not); the clamp (a point lit by three
  lights stays at most white); progressive = at once, byte for byte.
- **Golden frames**: the way's scenes in `tests/2d/` (small, the
  finished picture: the runner steps frames until the app says done);
  the `shape3d` path in `tests/3d/`, where the golden runner cannot
  pass flags but *can* press keys, so a ray-traced golden scene is
  `("examples/software/Cubes3d", "ry", 1)` -- quarter resolution with
  "r", then "y". Cheap enough to run in `make test`; the big ones stay
  behind `make test-golden-all`. TinyMyst: its first still, a
  hotspot's result, and the committed-still check above.
- **The numbers, measured rather than asserted**: rays per second
  brute force vs BVH, natively and in the browser; the frame time at
  1x and 4x supersampling; the rays saved by the attenuation cutoff;
  the rasterizer's own time on the same frame for scale.
- By eye, once: `PovrayWhitted.ml` against the 1980 picture.

## Out of scope

- Real-time ray tracing, denoisers, and anything in a game's 60 fps
  path.
- A GML interpreter, a POV-Ray `.pov` reader, and any scene file
  format (the way *is* the format, in OCaml).
- Blobs, height fields, isosurfaces, sweeps and lathes (POV-Ray's
  richer solids: exercises, each a new `Solid` case), and CSG of meshes
  (an exercise, by ray parity).
- Spectral rendering, volumetrics, participating media, caustics via
  photon mapping, subsurface scattering.
- GPU ray tracing: our OpenGL and WebGL backends cannot, and adding a
  Vulkan or Metal backend for it is a different project.
- Displacement, normal and bump maps; anisotropic materials.
- Animation rendered offline (a sequence of stills is a shell loop,
  not a feature); `Povray.animation` renders live, progressively.
- A full Myst: Myst Island's four Ages, the red and blue books'
  story, QuickTime movies, sound (an ambient loop from `audio/`, and
  the dissolve, are the most TinyMyst gets).
