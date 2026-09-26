# Ray tracing, from scratch: a tutorial for `graphics/3d/`

The other way to make a picture. [`notes_3d.md`](notes_3d.md) is the
rasterizer: take a triangle, find the pixels it covers, keep the
nearest. This note is its mirror image -- take a pixel, find what the
eye sees through it -- and what that one reversal buys: shadows,
mirrors, glass, solids made of solids, soft light and clean edges, none
of which the rasterizer can do, at a cost of seconds per frame instead
of milliseconds.

It was written as the specification of the ray tracer of
[`plan_raytracing_teaching.md`](../plans/done/plan_raytracing_teaching.md),
before the code, and checked against it once built (2026-09-26): its
numbers are measured ones now. The ray tracer grew one algorithm at a
time, each the one before plus an idea, and **keeps them all**: the
same scene can be drawn by each and the pictures put side by side
(the `Povray` way's flag `evolution`, the software backend's "y"
key), which is this note's order too:

```
  1. ray casting      (Appel 1968)      §4    what the eye sees, flat
  2. Lambert's light                    §5    lit by each lamp
  3. shadow rays      (Appel 1968)      §5    unless something is in the way
  4. mirrors, glass   (Whitted 1980)    §7-8  more rays, recursively
  5. soft shadows     (Cook 1984)       §9    a lamp with a size
  6. path tracing     (Kajiya 1986)     §9    the light bounced from everything
```

Companions: [`notes_3d.md`](notes_3d.md) (whose section 6
introduces the two strategies and points here),
[`notes_3d_shading.md`](../dev/notes_3d_shading.md) (the lighting formula
both renderers share) and
[`notes_3d_opti.md`](../dev/notes_3d_opti.md) (making the rasterizer fast,
the same kind of story as section 6 below), and
[`notes_raytracing_related_work.md`](../related-work/notes_raytracing_related_work.md)
(where this sits among POV-Ray, PBRT, Cycles and the RTX hardware).

One thing to have in mind throughout: **two ways in, one renderer**.
A `shape3d` scene, any game's or example's, reaches the ray tracer as
its triangles (`Shape3d_render_software.solids`, from the same faces
the rasterizer draws), so every such picture has a rasterized twin one
key away. And the `Povray` way (`playground/ways/Povray.mli`, §12)
describes what a triangle list cannot say -- exact spheres, infinite
planes, solids made of solids, marble, glass -- in POV-Ray's and the
ICFP 2000 contest's vocabulary, written in OCaml.

## 0. Where the code is, and a reading order

All in `libs/graphics/3d/`:

| module | what | section |
|---|---|---|
| `geometry/Ray` | a ray, and where its line meets a triangle, sphere, plane, box (shared with `physics/3d`'s `Collide3d`) | §3 |
| `geometry/Camera`, `Lighting` | where the rays come from; the brightness formula, the rasterizer's | §2, §5 |
| `geometry/Material` | how shiny, how glassy (data, carried by the rasterizer's faces too) | §7, §8 |
| `raytrace/Raytrace` | the renderer: cast, shade, recurse; the algorithms; the picture a slice at a time | §4-§9 |
| `raytrace/Solid` | what a ray can meet: primitives, transformed, CSG; patterns | §3, §8b, §8c |
| `raytrace/Transform`, `Csg` | move the ray, not the solid; solids of solids by intervals | §8b |
| `raytrace/Bvh` | not testing every solid | §6 |
| `raytrace/Perlin` | noise, for marble and wood | §8c |
| `Render` | the rasterizer, for comparison | `notes_3d.md` |
| `playground/ways/Povray` | the scene language, the progressive app | §12 |

Read §1-§5 for a working ray tracer (visibility, light, shadows),
§6 for the part that makes it usable, §7-§9 for what it can do that
a rasterizer cannot, and §10-§12 for where it sits in this project.
Each `.mli` has its diagram and a worked example; `graphics/tests/
Unit_raytrace.ml` checks them.

## 1. The reversal

```
   RASTERIZING                        RAY TRACING
   for each triangle:                 for each pixel:
     project it to the screen           make a ray through it
     for each pixel it covers:          for each object:
       if nearer, paint it                if the ray hits it, keep
                                            the nearest hit
                                        shade that hit

   cost ~ triangles x their pixels    cost ~ pixels x objects
                                             (until section 6)
```

The rasterizer's loop is *geometry to pixels*; the ray tracer's is
*pixels to geometry*. Three consequences, and they are the whole
subject:

- **The rasterizer only ever knows about one triangle at a time.**
  That is why it cannot do shadows (is anything between this point and
  the sun? it has no idea), reflections (what is over there? no idea)
  or refraction. Everything a rasterizer does about those is a trick
  played with extra passes and stored textures -- shadow maps,
  environment maps, and `TinyQuake.ml`'s baked lightmaps.
- **The ray tracer has the whole scene in hand at every pixel**, so
  each of those is just another ray: one towards the light (shadow),
  one bounced off the surface (mirror), one bent through it (glass).
  Five lines each.
- **And it pays for that at every pixel**, where the rasterizer pays
  once per triangle. A scene of 5,000 triangles at 640x480 is
  307,200 pixels x 5,000 = **1.5 billion** intersection tests, done
  naively. Section 6 is about getting that down by three orders of
  magnitude.

An old idea, for the record: Arthur Appel shot rays at surfaces to
find shadows in **1968**; Turner Whitted made the rays recursive --
reflection and refraction, the chrome and glass spheres everyone's
first ray tracer still draws -- in **1980**. The book to have open
while reading this note is **Glassner (ed.), *An Introduction to Ray
Tracing* (1989)**, whose nine chapters cover, in order, very nearly
the sections below; Paul Heckbert's chapter 7, "Writing a ray
tracer", is where this one's outline comes from.

## 2. Where the rays come from

A ray is an origin and a unit direction:

```ocaml
type t = private { origin : Vec3.t; direction : Vec3.t }
val make : Vec3.t -> Vec3.t -> t   (* the direction normalized, once *)
```

The first rays start at the eye and go through the pixels. `Camera`
already has everything needed: `basis` gives the camera's `right`, `up`
and `forward`, and the vertical field of view sets how wide the window
is at one unit in front:

```
                        the image plane, 1 unit in front of the eye
                       +-------------------------+   ^
                      /                          |   |  height = 2 tan(fov/2)
     eye  +----------+          . pixel (x, y)   |   |
           \          \                          |   v
            \          +-------------------------+
             forward        width = height * aspect

   dir = normalize (forward
                    + right * ((x + 0.5) / w * 2 - 1) * width  / 2
                    + up    * (1 - (y + 0.5) / h * 2) * height / 2)
```

The `+ 0.5` puts the ray through the *centre* of the pixel -- and §9
is what happens when you shoot several, elsewhere in the pixel,
instead. It is the rasterizer's projection (`Camera.ndc`) run
backwards, pixel centres and near plane included, so the two renderers
draw the same frame: `Cubes3d` ray cast against rasterized with no
lighting, **1 pixel in 1,000,000 apart**. That A/B found a bug on its
first run -- in the *rasterizer*: with an orthographic camera its
perspective-correct interpolation interpolates 1/z, which is wrong when
depth is linear on the screen, and where two faces cross the one behind
won (a TODO in `Interpolate.ml`).

**Normalize the direction.** This is the first bug everyone writes,
and it is in the author's own ICFP 2000 notes with the hour it cost
(`txt/history.txt`, "a ray must be normalised !!"): several
intersection formulas project a vector onto the direction with a dot
product, and a dot product is only a projection if the vector is a
unit one. The symptom is a picture that looks nearly right, with
distances subtly wrong. Here `Ray.make` normalizes, so the bug cannot
be written.

**Two conventions to convert.** The ICFP task's GML world is
left-handed (its eye looks along +z with x to the right), ours
right-handed (looking along -z): copied as is, a picture comes out
mirrored, so every z is negated. And GML's field of view is
horizontal, the Playground's vertical: 90 degrees across 320 x 240 is
73.7 up. `PovraySpheres.ml` and `PovrayFib.ml` do both, and say so.

## 3. What a ray hits

**A triangle** (Möller-Trumbore, 1997) is the one that matters, since
the playground's scene is polygons. The idea: solve
`origin + t * dir = (1-u-v) A + u B + v C` for `(t, u, v)` with
Cramer's rule, which falls out as a handful of cross and dot products
and no matrix at all. The three answers come out at once: `t` (how
far), and `(u, v)`, the barycentric coordinates -- which are exactly
what is needed to interpolate the normal and the texture coordinates
at the hit, the same interpolation the rasterizer does across a
triangle's pixels (`Interpolate`).

Rejections in order: the direction parallel to the plane (the
determinant near zero), `u < 0 or u > 1`, `u + v > 1`. Not `t < 0`:
`Ray` answers for the whole line, behind the origin too, and "in
front" is decided by the caller (`Solid.hit`) -- the solids of §8b
need the points behind as well as in front.

**A sphere**, analytically, because it is cheap and because it gives
this project a small delight: `Playground3d.sphere` is *tessellated*
into a lat/long mesh, so the rasterizer draws it faceted; a ray tracer
that recognises a sphere draws it **exactly round, at any zoom**. Same
scene, same call, rounder picture.

```
        sphere centre C, radius r; ray from O along d (unit)

            L = C - O            tca = L . d      (how far along the
        O----+------>d                             ray the centre is)
              \     |                             d2  = |L|^2 - tca^2
               \    | d2                         (the closest approach,
                \   |                             squared)
                 \  |              hit if d2 <= r^2, at
                  \ |              t = tca - sqrt(r^2 - d2)
                   C
```

with `tca < 0` meaning the sphere is behind the eye -- a test whose
absence, in the author's ICFP entry, left spheres unlit for an hour
and a half. `Ray.sphere` gives both roots, entering and leaving; the
first one in front is the hit, the second one when the ray starts
inside (glass seen from within).

**A plane** is one division (`t = -(n.O - d) / (n.d)`), and **an
axis-aligned box** is the slab test: clip the ray's `t` interval
against each pair of parallel faces and see whether anything is left.
It is both a solid (the `Povray` way's `box`, §8b) and what §6 tests
first.

## 4. The first picture: ray casting

With §2 and §3, a renderer already exists:

```
  for each pixel:
     ray = through the pixel                       (§2)
     hits = [ intersect ray f | f <- faces ]       (§3)
     case nearest hit of
       none  -> background
       some  -> the face's colour                  (flat, no light yet)
```

It draws silhouettes, correctly sorted, with no z-buffer anywhere --
visibility has been solved by *asking*, rather than by remembering the
nearest depth per pixel. That is worth pausing on: the z-buffer, the
painter's algorithm and BSP trees (`notes_3d.md` §6) are three answers
to a question the ray tracer never has to ask.

It is also, at this stage, unusably slow. Measured: `Cubes3d` at
1000 x 1000, a million rays against its 300 triangles, **29 seconds**,
some ten million ray/triangle tests a second -- where the rasterizer
draws the same frame sixty times a second. §6.

## 5. Light, and the shadow ray

The brightness formula is *not* new: `Lighting` is Lambert's cosine
law with an ambient floor, shared by the rasterizer, the OpenGL
shader and the web backend, and the ray tracer uses the same one at
the hit point, with the normal interpolated from the triangle's
corners by the `(u, v)` §3 handed back. That is deliberate: it makes
the two renderers' pictures *comparable*, so the only difference on
screen is what the technique adds.

What it adds, first and best, is the **shadow ray**:

```
                   * the light
                  /
                 /   <-- shadow ray: does anything block it?
      ----------X------------------
               hit                        if yes: ambient only
                                          if no:  ambient + Lambert
```

Five lines, and the scene acquires the thing no rasterizer gets for
free. Three traps, all of them in the author's ICFP 2000 log, all of
them worth a test:

- **Shadow acne.** The shadow ray starts *on* the surface, and floating
  point being what it is, it immediately hits that same surface at
  `t = 0.0000001`. Every lit plane comes out stippled with dark
  speckles. The fix is one epsilon -- start the ray a hair along its
  own direction (the entry's `point_from_ray shadow_ray 0.001`, with
  the comment `(* acne pb *)`), or ignore hits closer than epsilon.
  Measured: 31% of a floor in its own shadow with no epsilon, none
  with 1e-4 -- but on a *tilted* floor. Against the plane y = 0 the
  arithmetic happened to come out exact or a hair above, 0 points in
  10,000: a test written on the tidy case would have passed with the
  bug in. The "y" key's last renderer shows it, on triangles.
- **Clamping.** Two lights, each contributing 0.7, make 1.4; write
  that into a byte and 1.4 * 255 = 357 wraps round to 101, and the
  brightest part of the picture comes out dark. Clamp to 1.0 before
  converting. (In the log: "c'est parce qu'il faut clampfer".)
- **Directional lights have no position**, so "is the blocker nearer
  than the light?" has to compare against infinity rather than a
  distance. Forget it and nothing is ever in shadow.

With shadows in place, `Cubes3d.ml` ray traced is the same picture as
rasterized, *plus* the cubes' shadows on each other -- the plan's
argument for keeping the existing lighting model.

## 6. Not testing every triangle: the BVH

The ray tracer's cost is "pixels x objects" and the fix is to make the
second factor logarithmic. Wrap every face in a box, wrap groups of
boxes in bigger boxes, and a ray that misses the big box skips
everything inside it:

```
        +--------------------------------+
        |            root box            |
        |  +-----------+   +----------+  |      a ray entering here
        |  |  left     |   |  right   |  |  ---> tests the root, then
        |  | +--+ +--+ |   | +--+ +--+|  |      only the boxes it
        |  | |f1| |f2| |   | |f3| |f4||  |      actually enters
        |  | +--+ +--+ |   | +--+ +--+|  |
        |  +-----------+   +----------+  |
        +--------------------------------+

   build: split the faces by the middle of their longest axis
          (then, better: the surface area heuristic)
   walk:  slab test the box; if hit, recurse into both children,
          nearest first, and stop when the far one cannot beat
          the hit already found
```

A ray then does roughly `log2(faces)` box tests plus a handful of
triangle tests. Measured (`graphics/tests/bench/Raytrace_bench.ml`,
400 x 300, shadow rays included, natively and under node):

| scene | | tests a ray | native | node |
|---|---|---|---|---|
| `Cubes3d`'s 300 triangles | brute force | 583 | 6.6 s | 20 s |
| | BVH, median split | 4.7 | 0.21 s | 1.0 s |
| | BVH, surface area heuristic | 2.5 | 0.15 s | 0.76 s |
| 102,400 triangles | BVH, SAH (built in 1 s) | 4.9 | 0.57 s | 2.6 s |

340 times the triangles for 4 times the time: log n against n.
`Cubes3d` at 1000 x 1000 with its shadows, **38 s by brute force, 1.5 s
with the tree, the same PNG byte for byte**.

Three things the building of it taught, each now a comment in `Bvh`:
**a box must never say no** when its solid says yes (a ray aimed at a
triangle's corner missed its box by the last bit of rounding; boxes
are padded); **ties are broken as brute force breaks them** (the edge
two triangles share, the first in the list winning), or the pictures
are the same only nearly; and **the exact SAH sorts at every level**,
16 s to build 100,000 triangles, where binning it (Wald 2007) takes 1 s
for trees as good.

Two notes. First, the brute-force version **stays** (`-rt-brute`),
because it is the definition of correct: the property test is that the
BVH returns exactly what brute force returns, on random scenes -- the
same discipline `physics/2d`'s broad phase follows (`notes_2d_physics.md`
§9). Second, this is old and was known immediately: the ICFP 2000
entries were doing it in the 72 hours of the contest, and the Camls 'R Us
team reported that a **bounding sphere per object eliminated about 75%
of exact intersection tests**, with the scene's unions rearranged into
a tree for balance -- a BVH by another name, found under deadline.

## 7. Mirrors: the recursion

Whitted's 1980 addition, and the point at which "ray casting" becomes
"ray tracing". At a shiny surface, shoot one more ray, in the mirror
direction, and mix what it sees into the colour:

```
        incoming d        reflected r = d - 2 (d . n) n
              \           /
               \    n    /
                \   ^   /
                 \  |  /
        ----------\-|-/-----------
                   \|/
                    X
```

Three things keep it finite and fast:

- **a depth limit** (the ICFP scenes use 3 -- two mirrors facing each
  other would otherwise recurse forever);
- **an attenuation cutoff**: carry how much this ray can still
  contribute (each bounce multiplies it by the material's
  reflectivity) and stop when it drops below, say, 1/255 of a shade --
  nobody can see it. This is Camls 'R Us's own optimisation from the
  contest, and it is nearly free;
- **the same epsilon as §5**, or a mirror reflects itself at `t = 0`.

In `PovrayWhitted.ml`, the 1980 picture: 120,000 camera rays, 88,000
reflected or refracted ones, and 10,900 the cutoff did not shoot.

## 8. Glass: refraction

A transparent surface splits the ray in two: one reflected (§7), one
*bent* into the material by Snell's law, `n1 sin(a1) = n2 sin(a2)`
(glass 1.5, water 1.33, air 1.0).

```
        air        |  glass         and when a ray inside glass hits
         \    a1   |                the surface too steeply, there is
          \        |                no way out: total internal
           \       |                reflection (why a diamond sparkles
   ---------X------+------          and why a fish sees the sky in a
             \  a2 |                circle overhead)
              \    |
```

How much goes each way is the **Fresnel** term -- more reflection at
grazing angles, which is why a lake is a mirror at the far shore and
clear at your feet. Schlick's 1994 approximation is one line and is
what everyone ships.

Glass is also the best debugging picture there is: nothing else makes
a wrong normal, a wrong epsilon or a wrong depth limit so obvious.

## 8b. Solids, and solids made of solids

A triangle has no inside; a ball, a box, a cylinder, a cone, a torus
have one. So a solid along a ray is not a point but **where the ray is
inside it**: intervals of t, entering to leaving. Union, intersection
and difference -- constructive solid geometry -- are then merges of
two sorted lists (`Csg.mli`), nearly free for a ray tracer and nearly
impossible for a rasterizer:

```
   A        ====[======]=====[=====]======
   B        ========[=======]==============
   A - B    ====[===]============[=]======    inside A, not B
```

and one rule for the surfaces: where the result is entered by leaving
B (the wall of a hole drilled in A), B's normal points *into* the
result, and is turned round. `PovrayCsg.ml`: a cube drilled three ways,
a die (a cube rounded by a sphere, minus its pips), a lens (two spheres
intersected).

Each primitive is written once, of size one at the origin, and a
stretched or turned one is met by **moving the ray the other way**,
into its space (`Transform.mli`): a sphere scaled by (2, 1, 1) is an
ellipsoid for free, its normals carried back by the inverse transpose.
The transform keeps its inverse beside it, built step by step: no
matrix is ever inverted. The torus is a quartic, found by bracketing
and bisection rather than Ferrari's formula. And the check, which
found a bug of its own (a ray through a cone's apex): 14,376 points on
random solids, inside by the intervals exactly when inside by each
solid's definition (Requicha's point membership classification).

## 8c. Surfaces

A colour for every point of space, the solid carved out of it as from
a block of marble (Perlin, Peachey, both SIGGRAPH 1985): `Perlin.mli`'s
noise -- a random gradient at each point of a grid, blended smoothly
-- its table Perlin's own, so that `noise 3.14 42 7` is his
0.13691995878400012 to the last digit; turbulence, the noise summed at
doubling frequencies; marble as `sin (x + turbulence)`, wood as rings.
And a surface as a closure, GML's surface function, the "interpreter
in the inner loop" the ICFP 2000 winners optimized away -- here just an
OCaml function. On the `shape3d` path a texture is sampled at the
hit's (u, v) by the rasterizer's own `Texture.sample_bilinear`: a
textured quad drawn both ways, **0 of 10,800 pixels more than 2
apart**.

## 9. Clean edges: supersampling

The rasterizer antialiases by coverage (`notes_2d.md`); a ray tracer
does it by *shooting more rays per pixel* and averaging -- 4, 16, at
stratified positions inside the pixel (`options.samples`, the way's
keys 1 to 4, `-rt-samples`). It costs exactly its multiplier, and it
is the reason a ray-traced still at 4 samples looks like a photograph
where the rasterized frame looks like a video game. Each sample is
clamped before the average, or three suns on one would outweigh the
rest; a black edge through a pixel's middle, 2 x 2 samples: exactly
127.

**Randomness.** Putting samples at random rather than on a grid is
**distributed ray tracing** (Cook, Porter, Carpenter, 1984), and the
same trick buys soft shadows: a lamp with a size (`area_lamp`), 16
shadow rays to random points of it, the share that reach it -- 0 in
the umbra, 1 in the open, between in the penumbra (measured: 0.12,
0.19, 0.31 ... along a ball's shadow's edge). Take it all the way --
one random bounce more at every point, recursively -- and it is **path
tracing** (Kajiya, "The Rendering Equation", 1986): the light arriving
from every direction, estimated one path at a time. The ambient term
disappears, because it was a guess at exactly the light path tracing
now computes: from the sky, and from the other surfaces -- a red wall
tints what faces it (colour bleeding), a corner is darker than open
floor. `PovrayCornell.ml`, the Cornell box (1984), shows it; the price
is noise, shrinking as 1 / sqrt paths. Both are some thirty lines each,
where *smallpt* is 99 of C++.

Random, and deterministic: each pixel's numbers come from its own seed,
from its place in the picture (`Lehmer`, the project's generator), so
the picture is the same on every run and whatever order it is made in
-- `tests/2d/` holds golden path-traced frames, and this project does
not do global `Random` (principle 5).

## 10. What the rasterizer still wins, and what GPUs changed

Speed, by three or four orders of magnitude, on the thing it does: a
triangle's pixels, found by walking edges, with the whole scene
streamed once through the pipeline. That is why every game rasterizes
what is directly visible.

What changed since 2018 is that GPUs grew **dedicated ray tracing
hardware** -- NVIDIA's RTX (Turing, 2018), AMD's RDNA 2 (2020),
Intel Arc, Apple's M3 (2023): units that traverse a BVH and test
ray/triangle in silicon. Games since then are *hybrid*: rasterize
visibility, ray trace the shadows and reflections that rasterization
fakes badly, then denoise. Quake II RTX (2019) is the exception that
path traces everything -- and it is a 1997 game, which tells you the
budget.

None of that is reachable from here: it is exposed through DXR,
Vulkan ray tracing and Metal, and **OpenGL and WebGL have no ray
tracing API at all** (WebGPU has not shipped one either). So in this
project the situation is inverted, pleasingly: the software backend is
the only one that can do this, and the highest-fidelity image the
playground can produce comes from the renderer written in OCaml, not
from the GPU.

**The ray caster already here.** The other one the repository has is `TinyQuake.ml`'s
`clear`: it has no intersection formula at all, it walks the segment in steps
of 8 units asking the level's BSP tree `solid_at`, fine for baking
lightmaps once at startup and hopeless per pixel. And its `lit` starts
from half a unit along the normal: §5's epsilon, found again.

## 11. What's missing, and exercises

Beyond what was built, each an exercise, in rough order of
difficulty:

- **depth of field**: jitter the eye over a lens disc and aim every
  ray at the point it had on the focal plane (§2's camera gains an
  aperture and a focal distance); §9's samples average it out;
- **motion blur** (Cook, Porter, Carpenter, 1984): give each sample a
  time within the frame and ask the game's `view` for the scene at
  that time -- which makes the BVH (§6) a per-sample cost;
- **absorption in glass** (Beer's law): tint a refracted ray by the
  distance it travelled inside, so thick glass is darker than thin
  (§8);
- **dispersion**: an index of refraction per color channel, and the
  prism's rainbow (§8);
- **the rasterizer's orthographic bug** (§2): interpolate linearly
  when the camera is orthographic, and see the ray tracer agree;
- **accumulating paths**: keep adding path-traced samples while the
  camera doesn't move, instead of the keys' fixed count -- the picture
  converging as you watch;
- **all the cores**: the rows of the image are independent, so OCaml
  5's `Domain`s would divide the time by the number of cores (the
  project builds with OCaml 4.08, so behind a flag);
- **CSG of meshes**: a closed triangle mesh has an inside too, found by
  counting crossings (§8b refuses them);
- **emissive surfaces** in the path tracer: a lamp as a glowing solid,
  hit by chance as well as aimed at (the path tracer here already aims
  one ray a bounce at each lamp: next event estimation);
- **denoising**, what the RTX games do with 1 or 2 samples per pixel
  (§10): blur where the normals and depth agree, stop at the edges.

## 12. In the playground

**On any `shape3d` scene**, the software backend, with `-debug-keys`:

- the **"y" key** cycles the renderer: the rasterizer, then each of the
  ray tracer's algorithms, then the acne bug; at "r"'s quarter
  resolution it is a preview rather than a wait;
- the **"v" key**, versus: the rasterizer's frame on the left, the ray
  tracer's on the right, and the time each took (`RaytracingSplit3d`:
  15 ms against 470 at a third of the resolution);
- **`-raytrace`** starts on the ray tracer; `-dump-frame n file`,
  `-dump-size w h` (the still bigger than the window), `-no-hud`,
  `-rt-samples`, `-rt-bounces`, `-rt-brute` (watch it crawl);
- two words for scenes that want more than matte:
  `Playground3d.shiny` and `glassy`, set on a shape as `fade3d` is,
  ignored by every other backend;
- `RaytracingShadows3d.ml` (the shadows, the mirror, the acne bug on a
  key) and `RaytracingSplit3d.ml`.

**The `Povray` way** (`playground/ways/Povray.mli`): a scene of solids,
surfaces and lights, and an app built for you that ray traces it **a
slice per frame, coarse to fine** -- a ray per 8 x 8 block first, the
whole picture blurred at once, sharpening as you watch; no ray shot
twice, and the finished picture the same bytes as the one-go render,
whatever the slices (a test). `orbit`: drag to turn the camera, each
move back to the coarse pass, so a ray-traced scene can be explored
with no GPU. Arrows: the algorithm; the flag `evolution`: all of them
side by side; 1 to 4: rays a pixel; "s": save. The examples, a 2D app
each, running in a browser too:

- `PovraySpheres.ml` and `PovrayFib.ml`: the ICFP 2000 contest's
  `spheres.gml` and `fib.gml`. The author's own entry rendered `fib`
  in 2000, and its picture is kept as a test
  (`graphics/tests/icfp2000/`): today's ray tracer gives **76,770 of
  its 76,800 pixels exactly, none more than 1 apart** -- a regression
  test twenty-six years long;
- `PovrayWhitted.ml` (the 1980 picture), `PovrayCsg.ml` (§8b),
  `PovrayMarble.ml` (§8c), `PovrayCornell.ml` (§9).

The plan for all of it, and the order:
[`plan_raytracing_teaching.md`](../plans/done/plan_raytracing_teaching.md).

## Glossary

- **Ray**: an origin and a unit direction; **primary ray** (from the
  eye), **shadow ray** (towards a light), **secondary ray** (reflected
  or refracted).
- **Ray casting** (Appel, 1968): primary rays and shadow rays only;
  **ray tracing** (Whitted, 1980): with recursion.
- **Barycentric coordinates** `(u, v)`: where in a triangle a hit
  landed, and how its normal and texture coordinates are interpolated.
- **Möller-Trumbore**: the standard ray/triangle test, returning
  `t, u, v` at once.
- **Shadow acne**: the self-intersection speckle of a shadow ray
  starting exactly on its surface; **epsilon**: the offset that cures
  it.
- **BVH** (bounding volume hierarchy): boxes inside boxes, so a ray
  skips most of the scene; **slab test**: the ray/box test it uses;
  **SAH**: the surface area heuristic for building a good one.
- **Attenuation cutoff**: stopping the recursion when a ray can no
  longer change the pixel.
- **Snell's law**, **total internal reflection**, **Fresnel** (and
  **Schlick's approximation**): what glass does.
- **Supersampling**: several rays per pixel; **stratified**: on a
  regular sub-grid; **distributed ray tracing** (Cook et al., 1984):
  jittering them on purpose, for soft shadows and depth of field.
- **Path tracing** (Kajiya, 1986): following bounces to get global
  illumination; **smallpt**: the 99-line one.
- **Hybrid rendering**: what RTX-era games do -- rasterize visibility,
  ray trace the rest, denoise.
- **CSG** (constructive solid geometry): union, intersection,
  difference of solids; **intervals**: where a ray is inside a solid.
- **Perlin noise**: smooth, repeatable randomness in space;
  **turbulence**: its octaves summed; **solid texture**: a colour for
  every point of space.
- **Progressive rendering**: the picture shown as it is made, coarse to
  fine.
- **Rendering equation** (Kajiya, 1986): the light leaving a point, as
  an integral of the light arriving at it; path tracing estimates it.

## References

- Arthur Appel, "Some Techniques for Shading Machine Renderings of
  Solids", AFIPS Spring Joint Computer Conference, 1968.
- Turner Whitted, "An Improved Illumination Model for Shaded
  Display", Communications of the ACM 23(6), 1980.
- Robert L. Cook, Thomas Porter, Loren Carpenter, "Distributed Ray
  Tracing", SIGGRAPH '84.
- James T. Kajiya, "The Rendering Equation", SIGGRAPH '86.
- Timothy L. Kay, James T. Kajiya, "Ray Tracing Complex Scenes",
  SIGGRAPH '86 (bounding volume hierarchies, the slab test).
- Robert L. Cook, "Stochastic Sampling in Computer Graphics", ACM
  Transactions on Graphics 5(1), 1986.
- Jeffrey Goldsmith, John Salmon, "Automatic Creation of Object
  Hierarchies for Ray Tracing", IEEE Computer Graphics and
  Applications 7(5), 1987.
- Andrew S. Glassner (ed.), "An Introduction to Ray Tracing", Academic
  Press, 1989 (Heckbert's chapter 7, "Writing a ray tracer").
- J. David MacDonald, Kellogg S. Booth, "Heuristics for ray tracing
  using space subdivision", The Visual Computer 6(3), 1990 (the
  surface area heuristic).
- Christophe Schlick, "An Inexpensive BRDF Model for Physically-based
  Rendering", Computer Graphics Forum 13(3), 1994.
- Tomas Möller, Ben Trumbore, "Fast, Minimum Storage Ray/Triangle
  Intersection", Journal of Graphics Tools 2(1), 1997.
- Christer Ericson, "Real-Time Collision Detection", Morgan Kaufmann,
  2005 (chapter 5: ray/sphere, ray/box).
- Kevin Beason, "smallpt: Global Illumination in 99 lines of C++",
  2007.
- Peter Shirley, "Ray Tracing in One Weekend", 2016.
- Scott D. Roth, "Ray Casting for Modeling Solids", Computer Graphics
  and Image Processing 18(2), 1982 (CSG by intervals).
- Ken Perlin, "An Image Synthesizer", SIGGRAPH '85, and "Improving
  Noise", SIGGRAPH '02; Darwyn Peachey, "Solid Texturing of Complex
  Surfaces", SIGGRAPH '85.
- Cindy Goral, Kenneth Torrance, Donald Greenberg, Bennett Battaile,
  "Modeling the Interaction of Light Between Diffuse Surfaces",
  SIGGRAPH '84 (the Cornell box).
- Matt Pharr, Wenzel Jakob, Greg Humphreys, "Physically Based
  Rendering: From Theory to Implementation", 4th ed., MIT Press, 2023
  (1st ed. 2004).
