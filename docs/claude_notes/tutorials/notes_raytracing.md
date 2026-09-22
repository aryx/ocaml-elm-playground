# Ray tracing, from scratch: a tutorial for `graphics/3d/`

The other way to make a picture. [`notes_3d.md`](notes_3d.md) is the
rasterizer: take a triangle, find the pixels it covers, keep the
nearest. This note is its mirror image -- take a pixel, find the
triangle the eye sees through it -- and what that one reversal buys:
shadows, mirrors, glass and clean edges, none of which the rasterizer
can do, at a cost of seconds per frame instead of milliseconds.

It is the specification of the ray tracer planned in
[`plan_raytracing_teaching.md`](../plans/plan_raytracing_teaching.md):
written before the code, to be checked against it and have its numbers
filled in. Companions: [`notes_3d.md`](notes_3d.md) (whose section 6
introduces the two strategies and points here),
[`notes_3d_shading.md`](../dev/notes_3d_shading.md) (the lighting formula
both renderers share) and
[`notes_3d_opti.md`](../dev/notes_3d_opti.md) (making the rasterizer fast,
the same kind of story as section 6 below), and
[`notes_raytracing_related_work.md`](../related-work/notes_raytracing_related_work.md)
(where this sits among POV-Ray, PBRT, Cycles and the RTX hardware).

One thing to have in mind throughout: **both renderers read the same
scene**. `Shape3d_render_software.faces` turns a `shape3d` tree into
world-space polygons with normals and texture coordinates; the
rasterizer projects them, the ray tracer shoots at them. Nothing is
exported, converted or duplicated, so every picture in this note has a
rasterized twin one key away.

## 0. Where the code is, and a reading order

| module | what | section |
|---|---|---|
| `graphics/3d/geometry/Ray` | a ray, and what it hits: triangle, sphere, plane, box | §3 |
| `graphics/3d/geometry/Camera` (exists) | where the rays come from | §2 |
| `graphics/3d/geometry/Lighting` (exists) | the brightness formula, shared with the rasterizer | §5 |
| `graphics/3d/Raytrace` | the renderer: cast, shade, recurse | §4, §5, §7, §8 |
| `graphics/3d/Bvh` | not testing every triangle | §6 |
| `graphics/3d/Material` | matte, mirror, glass | §7, §8 |
| `graphics/3d/Render` (exists) | the rasterizer, for comparison | `notes_3d.md` |
| the software backend's `Playground3d_platform` | the `-raytrace` flag, the "y" key, the dump | §12 |

Read §1-§5 for a working ray tracer (visibility, light, shadows),
§6 for the part that makes it usable, §7-§9 for what it can do that
a rasterizer cannot, and §10-§12 for where it sits in this project.

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
type t = { origin : Vec3.t; dir : Vec3.t }   (* dir normalized! *)
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
instead.

**Normalize the direction.** This is the first bug everyone writes,
and it is in the author's own ICFP 2000 notes with the hour it cost
(`txt/history.txt`, "a ray must be normalised !!"): several
intersection formulas project a vector onto `dir` with a dot product,
and a dot product is only a projection if the vector is a unit one.
The symptom is a picture that looks nearly right, with distances
subtly wrong.

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

Rejections in order: `dir` parallel to the plane (the determinant near
zero), `u < 0 or u > 1`, `u + v > 1`, and `t < epsilon` (behind us, or
on the surface we started from -- see §5).

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
and a half.

**A plane** is one division (`t = -(n.O - d) / (n.d)`), and **an
axis-aligned box** is the slab test: clip the ray's `t` interval
against each pair of parallel faces and see whether anything is left.
Boxes are not in the scene; they are what §6 tests first.

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

It is also, at this stage, unusably slow, and the number to keep is
the one from §1: 1.5 billion tests for a modest scene. §6.

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
triangle tests -- for 5,000 faces, about 20 instead of 5,000. Same
picture, a couple of orders of magnitude faster (the measured numbers
go here, and in `Bvh.mli`, once built).

Two notes. First, the brute-force version **stays** (`-rt-brute`),
because it is the definition of correct: the property test is that the
BVH returns exactly what brute force returns, on random scenes -- the
same discipline `physics/2d`'s broad phase follows (`notes_2d_physics.md`
§9). Second, this is old and was known immediately: the ICFP 2000
entries were doing it in 1972-hours-of-contest, and the Camls 'R Us
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

## 9. Clean edges: supersampling

The rasterizer antialiases by coverage (`notes_2d.md`); a ray tracer
does it by *shooting more rays per pixel* and averaging -- 4, 16, at
stratified positions inside the pixel. It costs exactly its
multiplier, and it is the reason a ray-traced still at 4 samples looks
like a photograph where the rasterized frame looks like a video game.

Jittering those samples deliberately, rather than putting them on a
grid, is **distributed ray tracing** (Cook, Porter, Carpenter, 1984),
and the same trick then buys soft shadows (jitter the point on the
light), glossy reflections (jitter the mirror direction) and depth of
field (jitter the eye). Take it all the way -- bounce diffusely, many
times, thousands of samples -- and it is **path tracing** (Kajiya,
"The Rendering Equation", 1986), with colour bleeding and ambient
occlusion for free and minutes per frame to pay. Kevin Beason's
*smallpt* fits one in 99 lines of C++, which is the size this project
should aim at if it goes there.

Here, the samples are a fixed stratified grid and any jitter is
seeded, because `tests/3d/` holds golden ray-traced frames and this
project does not do global `Random` (principle 5).

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

**The ray caster already here.** The one the repository has is `TinyQuake.ml`'s
`clear`: it has no intersection formula at all, it walks the segment in steps
of 8 units asking the level's BSP tree `solid_at`, fine for baking
lightmaps once at startup and hopeless per pixel. And its `lit` starts
from half a unit along the normal: §5's epsilon, found again.

## 11. What's missing, and exercises

Beyond the plan's phases (which stop at soft shadows and a small path
tracer), each an exercise, in rough order of difficulty:

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
- **progressive rendering**: keep adding samples to the "y" key's
  frame while the camera doesn't move, instead of one fixed count;
- **all the cores**: the rows of the image are independent, so OCaml
  5's `Domain`s would divide the time by the number of cores (the
  project builds with OCaml 4.08, so behind a flag);
- **CSG**: the ICFP 2000 scenes had union, intersection and
  difference of solids, which need the ray's whole list of entries and
  exits, not just the nearest hit -- and our scene is faces, not
  solids;
- **explicit light sampling** in the path tracer (next event
  estimation): aim one ray per bounce at a light instead of waiting to
  hit one by chance, the difference between minutes and seconds per
  frame;
- **denoising**, what the RTX games do with 1 or 2 samples per pixel
  (§10): blur where the normals and depth agree, stop at the edges.

## 12. In the playground

Nothing in the scene changes. The same `shape3d` tree, the same
camera, the same examples:

- **`-raytrace`** makes `-dump-frame n file` render that frame with
  `Raytrace` instead of `Render`, at `-dump-size w h` (bigger than the
  window if you like), with `-rt-samples` and `-rt-bounces` for
  quality and `-rt-brute` to watch it crawl;
- the **"y" key** ray traces the live frame, which at "r"'s quarter
  resolution is a preview rather than a wait: press "r" twice, "y",
  and compare;
- two new words for scenes that want more than matte:
  `Playground3d.shiny` and `glassy`, set on a shape like `fade3d` is,
  ignored by every other backend;
- the examples: `RaytracingSpheres3d.ml` and `RaytracingFib3d.ml`
  (the ICFP 2000 contest's own `spheres.gml` and `fib.gml` scenes,
  written with the playground's constructors instead of GML),
  `RaytracingWhitted3d.ml` (the 1980 picture),
  `RaytracingShadows3d.ml` (including the acne bug, on a key), and
  `RaytracingSplit3d.ml` (the same frame both ways, side by side,
  with both timings).

The plan for all of it, and the order:
[`plan_raytracing_teaching.md`](../plans/plan_raytracing_teaching.md).

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
- Matt Pharr, Wenzel Jakob, Greg Humphreys, "Physically Based
  Rendering: From Theory to Implementation", 4th ed., MIT Press, 2023
  (1st ed. 2004).
