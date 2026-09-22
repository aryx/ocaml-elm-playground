# 3D rendering, from scratch: a tutorial for `Playground3d`

You said you're new to 3D, so this is a from-the-ground-up explanation:
what "rendering a 3D scene" actually means, the handful of ideas every
3D engine (from a 1970s wireframe plotter to a modern game on a GPU) is
built out of, where those ideas came from historically, and exactly
which of them `Playground3d` uses -- with pointers into the actual
code. If you read this once, the code in `Playground3d.ml` and
`graphics/3d/` should stop looking like a wall of trigonometry and start
looking like a small, recognizable set of standard building blocks.

See also [`plan_playground3d.md`](plan_playground3d.md) (the original
design plan) for *why* this library exists and how it's organized as
OCaml packages; this note is about the 3D *concepts*, not the OCaml
architecture.

## 0. Where the code is, and a reading order

The native software rasterizer is one small module per idea, in
`graphics/3d/`, independent of the Playground (points, colors as ints,
cameras as plain records); each `.mli` explains its algorithm with a
diagram, a worked example checked by `graphics/tests/`, and the paper
that introduced it. Suggested reading order, which is also the order of
this note:

| module | what | section |
|---|---|---|
| `geometry/Vec3` | points and vectors, dot and cross products, face normals | §2, §3 |
| `geometry/Camera` | the look-at camera: view coordinates, perspective | §4 |
| `Project` | a 3D point to a pixel, with what's interpolated | §4 |
| `Cull` | backface culling | §5 |
| `Zbuffer`, `Painter` | hidden surface removal, two ways | §6 |
| `Triangle` | edge functions, the fill rule | §7 |
| `Clip` | near-plane clipping | §7 |
| `geometry/Lighting`, `Shading` | Lambert's law; flat, Gouraud, Phong | §8 |
| `Interpolate`, `Texture` | perspective-correct interpolation, texture sampling | §9 |
| `Render` | the whole pipeline, one function | all |

Around them: `geometry/Mat4` (the same camera as matrices, for the
OpenGL backend); `Shape3d_render_software.ml`,
which turns the Playground's `shape3d`s and `camera` into `Render`'s
faces; and its `Playground3d_platform.ml`, the window, the debug keys (§11),
the HUD (§12) and the loop. The web backend (`Playground3d.ml`'s
`render3d_to_2d`) shares `Camera` and `Lighting`. `tests/3d/` checks
whole frames of every example against golden images.

## 1. The big picture: what does "rendering" even mean?

A 3D scene is just numbers: a list of points in space (each a triple
`(x, y, z)`), grouped into shapes, plus a camera (a position and a
direction to look in). "Rendering" is the process of turning those
numbers into a flat, 2D image -- exactly the same job a real camera (or
your eye) does with real light. There's a useful analogy here that
holds up surprisingly well: think of the whole pipeline as taking a
photograph.

1. You set up objects in a room (**world space** -- where is everything,
   in one shared coordinate system?).
2. You place a camera and point it somewhere (**view/camera space** --
   re-describe the room *relative to the camera*, as if the camera were
   always at the origin looking straight down one axis).
3. The camera's lens focuses the 3D world onto a flat sensor
   (**projection** -- squash 3D down to 2D, with things farther away
   appearing smaller, the way real lenses and eyes work).
4. The sensor is a grid of pixels (**screen space** -- convert to actual
   pixel coordinates you can draw).

Every 3D engine, from a 1963 wireframe plotter to a 2026 game, does
some version of these four steps. `Playground3d`'s pipeline is exactly
this, and you can see all four stages as separate, named functions:
step 1 is `move3d`/`rotate3d`/`scale3d` (`Playground3d.ml`), steps 2-4
are `Camera.view`, `Camera.ndc` and `Project.vertex` in `graphics/3d/`
(or `project` in `Playground3d.ml` for the web backend's simpler
2D-point-only version, which uses the same `Camera` functions).

## 2. Points, vectors, and the two operations everything is built from

A 3D point is a triple of numbers, `(x, y, z)`. In this codebase it's
just an OCaml tuple (`Vec3.t = float * float * float`, in
`graphics/3d/geometry/Vec3.ml`, shared by all the backends). The
same triple is used both for *positions* ("this corner of the cube is
here") and *directions/vectors* ("this face points this way") --
context tells you which one you mean.

Two operations on vectors show up constantly, and it's worth building
intuition for both, since almost every "clever" line in this codebase
is just one of these two:

- **Dot product** (`dot` in the code): multiply matching components and
  add them up. `dot a b = ax*bx + ay*by + az*bz`. Intuition: it measures
  *how aligned two directions are*. Two vectors pointing the same way
  give a big positive number; perpendicular vectors give exactly zero;
  opposite vectors give a big negative number. We use this for:
  - **Backface culling** (§5): "is this face's outward direction
    roughly the same as the direction from the face to the camera?"
  - **Projection** (§4): "how far along the camera's right/up/forward
    axes is this point?" (projecting one vector onto another *is* a dot
    product).

- **Cross product** (`cross` in the code): given two vectors, produces a
  third vector that's perpendicular to *both* of them, using the
  right-hand rule (point your right hand's fingers along the first
  vector, curl them towards the second, your thumb points along the
  result). We use this for:
  - **Computing a face's normal** (§3): a face is planar, so any two of
    its edges' cross product gives a vector perpendicular to the whole
    face.
  - **Building a camera's "right" axis** (§4): "forward" and "world up"
    aren't perpendicular to each other in general, so `cross forward
    up_hint` gives you a true "right" direction that *is* perpendicular
    to forward.

The software rasterizer and the web backend use no matrices -- no 4x4
matrices, no matrix-vector multiplication. This is a deliberate
simplification (see `plan_playground3d.md`): everything is done with
these two operations on plain 3-tuples instead. (`geometry/Mat4` builds
the same camera as 4x4 matrices, only because the OpenGL backend's GPU
wants them, see §10.) That's *not* how a real game
engine or GPU pipeline works internally (they represent every
transform, including projection, as a 4x4 matrix and multiply them
together) -- but the matrix version is mathematically equivalent to,
and mostly a more general/composable way of writing, the same dot- and
cross-product operations. Trading generality for "you can read every
line and see what number produced what" was the right call for a
learning-oriented library like this one.

## 3. Meshes, faces, quads, triangles, vertices, normals

- A **vertex** is one corner point of a shape (a `vec3`).
- A **face** is one flat (planar) surface of a solid, described by an
  ordered list of vertices going around its boundary. A cube has 6
  faces.
- A **quad** just means "a 4-sided face" (a quadrilateral) -- e.g. every
  face of `cube`. A **triangle** is a 3-sided face.
- A **mesh** is the general term for "a shape made of many faces". Our
  `shape3d`/`form3d` (in `Playground3d.mli`) is a (very small) mesh
  representation: `Polygon3d`/`TexturedPolygon3d` are faces, `Group3d`
  is how you combine faces (and groups of faces) into a mesh.
- The **normal** of a face is a vector perpendicular to it, by
  convention pointing "outwards" (away from the solid the face belongs
  to). The obvious way is a cross product of two of its edges -- a
  face is planar, so *any* two non-parallel edges give the same normal
  direction -- but it fails on a degenerate corner (two equal points,
  like a sphere's pole); `Vec3.face_normal` uses Newell's method
  instead, a sum over all the edges, robust to that.

**Why triangles, specifically, are the universal building block of 3D
graphics:** 3 points are *always* exactly planar/flat (any 3 points in
space lie on some single flat plane -- you can't avoid it), but 4 or
more points are only flat *if you're careful to place them that way*.
A slightly "warped" quad (imagine bending one corner of a cube face
very slightly out of alignment) isn't flat, and isn't a well-defined
surface. This is why real GPUs and rendering engines only ever really
know how to draw triangles -- a quad you build (like `cube`'s faces
here) is really "secretly" always meant to be thought of as 2
triangles glued together along a diagonal. You can see this literally
in `Render`'s `fan_triangles`: it takes a face's point list
(e.g. a cube face's 4 corners) and splits it into a *fan* of triangles
-- `(p0,p1,p2), (p0,p2,p3), ...` -- before rasterizing each one
separately. The web backend never does this, because it hands whole
polygons (not triangles) to `Playground.polygon`, which is fine there
since SVG (unlike a low-level rasterizer) is happy to fill an arbitrary
polygon directly.

**Winding order.** The *order* you list a face's vertices in matters:
going around them counter-clockwise (as seen from the side the face
should be visible from) vs. clockwise flips which way the computed
normal points. `box_faces` in `Playground3d.ml` was written (by hand,
checked with the actual cross-product math) so every face's 4 corners
are listed in the order that makes its outward normal come out
correct. If you ever add a new hand-built shape and backface culling
makes it disappear entirely (or show only its *inside*), 9 times out of
10 the fix is "list the corners in the other order".

## 4. The camera and projection

### 4.1 The camera model: eye + target ("look-at")

Our `camera` (`Playground3d.mli`) is just two points: `eye` (where the
camera is) and `target` (a point it's looking towards). This is called
a **look-at camera**, and it's the simplest of a few standard camera
representations:

- **Look-at (eye + target + up-hint)** -- what we use. Very intuitive
  ("stand here, look at that"), but has one blind spot: if you try to
  look straight up or straight down, "forward" and "up" become the same
  direction, and the maths below (which needs them to be different, to
  compute a "right" direction) breaks down. Fine for the modest camera
  angles this library targets so far; documented as a known limitation.
- **Eye + yaw/pitch(/roll)** -- store the camera's facing as two (or
  three) angles instead of a target point ("I'm facing 30 degrees left
  of north, tilted 10 degrees down"). This is what most first-person
  games actually use internally (mouse-look naturally produces
  yaw/pitch deltas), and is the natural next step if/when
  `Playground3d` grows a first-person camera helper for something like
  a Minecraft-style game (see `plan_playground3d.md`'s Phase 5) --
  you'd store `(eye, yaw, pitch)` in the game's model and compute a
  `target` from them each frame to build our existing `camera` value.
- **Full 4x4 view matrix** -- the general, GPU-native representation;
  a look-at camera or a yaw/pitch camera are both just convenient ways
  to *construct* one. We don't expose this at all (see §2).

`camera`'s `fov`, `near`, and `far` fields define the camera's
**viewing frustum** -- literally the shape of the volume of space it
can see, which looks like a pyramid with its tip cut off (a
"frustum"): a narrow rectangle close to the eye, widening out into the
distance, bounded by the near and far planes. `fov` (field of view, in
degrees) controls how wide that pyramid is -- a small FOV is a
"zoomed-in telephoto lens" view, a large FOV is a "wide-angle/fisheye"
view. `near`/`far` exist mostly for numerical/practical reasons (a
point exactly *at* the camera can't be projected at all -- dividing by
a view-space depth of 0 -- and a "far" limit avoids drawing things
infinitely far away); see §7 for what "clipping" against the near
plane means, and `Clip`.

### 4.2 View space: re-describing the world relative to the camera

`Camera.view` takes a world-space point and re-expresses it in a
coordinate system centered on the camera, with 3 axes (`Camera.basis`,
also used by `Mat4.look_at`):

- **forward**: the direction the camera is looking (`target - eye`,
  normalized -- "normalize" just means "rescale to length 1", so it's a
  pure direction with no length information left in it).
- **right**: perpendicular to both "forward" and a world "up" hint
  (`(0,1,0)`, since this library's convention is Y-up), via `cross`.
- **up**: perpendicular to both "forward" and "right" (recomputed via
  another `cross`, rather than reusing the world up-hint directly,
  precisely *because* the world up-hint usually isn't exactly
  perpendicular to "forward" -- only the recomputed one is guaranteed
  to be).

Once you have those 3 perpendicular axes, "where is this point relative
to the camera" is just 3 dot products: how far along "right", how far
along "up", how far along "forward". That's exactly what `Camera.view`
computes. The resulting `(vx, vy, vz)` is in **view space**: `vz` is
literally "distance in front of the camera, measured along where it's
looking" -- which is exactly the depth value the z-buffer needs (§6).

### 4.3 Perspective projection: why farther things look smaller

**Perspective projection** is the mathematical model of "things farther
away look smaller", the way real cameras and eyes work. Its opposite is
**orthographic projection** ("parallel" projection, no size falloff
with distance -- used in some CAD software, isometric-looking games,
and 2D playground's own coordinate system, which has no notion of depth
at all). The core formula, seen in `Camera.ndc`, is: `screen_x` is proportional to `view_x / view_z` (and similarly for
y) -- literally "divide by depth". This single division is *the*
mathematical fact that produces the entire visual effect of things
shrinking with distance: double `view_z` (move a point twice as far
away, keeping its `view_x`/`view_y` the same) and its projected
position gets divided by 2, i.e. it moves twice as close to the center
of the screen -- which is exactly how apparent size shrinks with
distance. The `f = 1 / tan(fov/2)` factor is just a scale constant
derived from the field of view (a smaller FOV needs a bigger scale
factor, to "zoom in").

The result, before converting to actual pixels, is in **normalized
device coordinates** (NDC): roughly `-1..1` across the visible width
and height, regardless of the actual window size in pixels. The last
step (`Project.vertex`) is just remapping that `-1..1` range
onto actual pixel coordinates -- and, since our framebuffer's pixel
coordinates have `(0,0)` at the top-left with Y increasing *downward*
(the universal convention for image/pixel buffers), while
`Playground`'s 2D coordinate system has `(0,0)` at the center with Y
increasing *upward*, this step also does that flip (the `(fsy/.2.) -.
...` instead of `+.`).

## 5. Backface culling: skip half the work for free

For any *solid, opaque* object (like a cube), you can never actually
see the inside surface of the far side of it -- it's always hidden
behind the near side. **Backface culling** is the optimization of
detecting and skipping those never-visible faces *before* doing any
per-pixel work on them, using exactly the dot-product intuition from
§2: `dot normal (eye - centroid) > 0` (`Cull.faces_camera`) asks "does this face's outward
normal point roughly *towards* the camera, or roughly *away* from it?"
-- a face pointing away is a backface, and gets skipped entirely. On a
plain cube viewed from outside, this immediately discards 3 of its 6
faces (exactly the 3 you can't see) without rasterizing a single pixel
of them.

lucamug's elm-playground-3d has *no* backface culling at all -- it
projects and draws every face of every shape, in whatever order the
code happens to list them, relying on the specific camera angles in its
demos to happen to look right (a face you weren't supposed to see gets
drawn, but then a face you *were* supposed to see, drawn afterwards in
list order, happens to cover it up). This works for a fixed demo but
isn't a general solution -- it's one of the two things
`Playground3d.render3d_to_2d`'s doc comment calls out as a deliberate
improvement over the original (see `plan_playground3d.md`).

## 6. The hardest problem in classical 3D graphics: hidden surface removal

Given a pile of possibly-overlapping triangles, *which one wins at each
pixel*? This is called the **hidden surface removal** (or
**visibility**) problem, and its history is basically the history of
3D graphics research in the 1970s-80s. Backface culling (§5) throws
away faces that can *never* be visible from *any* angle at that camera
position; hidden surface removal is the remaining, harder problem of
correctly ordering the faces that *are* potentially visible, since two
of them might still overlap on screen with one truly in front of the
other.

- **Painter's algorithm** (the oldest, simplest idea: paint like an
  actual painter does, background first, then progressively closer
  things on top, so each new stroke naturally covers up what's behind
  it). Sort whole faces by distance from the camera, farthest first,
  and draw them in that order. This is exactly what
  `Playground3d.render3d_to_2d` does (`List.sort` by `dist_to_eye`,
  farthest-first) for the *web* backend, and `Painter.sort_far_to_near`
  for the native one when you press `z` (§11). It's simple and cheap, but has
  a well-known failure mode: it only works if you can put every face
  into one single consistent front-to-back order, which is impossible
  when faces interpenetrate, or when three faces mutually overlap each
  other in a cycle (A partly in front of B, B partly in front of C, C
  partly in front of A) -- no single sort order can get all three pairs
  right simultaneously. Not a concern for simple, mostly-convex,
  non-intersecting scenes like the current cube examples; would need
  revisiting for more complex geometry.

- **The Z-buffer** (aka depth buffer -- invented by Edwin Catmull, in
  his 1974 PhD dissertation; Catmull later co-founded Pixar). Instead
  of sorting whole *faces*, keep a "closest depth seen so far" number
  for *every individual pixel*, and update a pixel only when a new
  triangle's depth at that exact pixel is closer than what's already
  there. This is a purely local, per-pixel decision -- it doesn't
  require ever finding one global ordering of faces, so it has no
  trouble at all with the interpenetrating/cyclic cases that break the
  painter's algorithm. The cost is memory (one depth value per pixel)
  and, historically, the per-pixel comparison work -- cheap enough
  today that it's standard in essentially every real-time 3D renderer,
  built directly into GPU hardware. This is exactly what the *native*
  backend does by default: `Zbuffer` (a plain `float array`, one entry
  per pixel) and its `test_and_set`, called by `Triangle.fill` for
  every pixel, are a z-buffer, done by hand.

- **BSP trees** (Binary Space Partitioning -- Fuchs, Kedem, and Naylor,
  1980). A different idea: *before* you know where the camera is,
  precompute a tree that recursively splits the scene's geometry with
  planes, such that -- for *any* camera position -- walking the tree in
  a particular order (determined per-frame just by which side of each
  splitting plane the camera is on) visits the faces in guaranteed
  correct back-to-front (or front-to-back) order. Famous for making
  fully correct, glitch-free rendering of complex indoor scenes
  possible on hardware too slow for a per-pixel z-buffer -- id
  Software's *Doom* (1993, a 2D map extruded into a pseudo-3D view) and
  *Quake* (1996, genuinely full 3D) are the canonical examples. Mostly
  a historical/specialized technique today (real-time ray tracing and
  cheap z-buffers cover most needs), still occasionally used for static
  level geometry and physics.

- **Ray tracing** (Turner Whitted's 1980 paper is the usual reference
  point for the modern form, computing reflections/refractions/shadows,
  though the very basic idea is older). A fundamentally different
  strategy from everything above: instead of *projecting* triangles
  onto the screen and figuring out which one lands on which pixel
  ("rasterization" -- forward, geometry-to-pixels), shoot one ray *per
  pixel* from the camera through the scene, and find whichever triangle
  it hits first ("ray casting" -- backward, pixel-to-geometry). This
  naturally handles reflections, refraction, and soft shadows far more
  accurately than rasterization, at a much higher computational cost
  (historically far too slow for real-time use; dedicated GPU
  ray-tracing hardware, e.g. NVIDIA's RTX line from 2018 onwards, is
  what finally made a hybrid of the two approaches practical in games).

A ray tracer for the *software* backend is planned, as the other half
of this note -- shadows, mirrors and glass on the same scenes, for
`-dump-frame` stills rather than for the frame loop: see
[`notes_raytracing.md`](notes_raytracing.md) and
[`plan_raytracing_teaching.md`](../plans/plan_raytracing_teaching.md).

`Playground3d` is a rasterizer, not a ray tracer, on both backends;
its two backends land on two different points in the painter's-algorithm
vs. z-buffer trade-off above, purely because of what each platform
makes possible (the web backend has no way to touch individual pixels
at all, so a per-pixel z-buffer isn't an option there; see
`plan_playground3d.md`).

## 7. Rasterization: turning one triangle into pixels

Given a triangle already projected to 2D screen coordinates (§4), how
do you decide exactly which pixels it covers? `Triangle.fill`
(native backend only -- the web backend hands whole polygons to
`Playground.polygon`/SVG and never rasterizes by hand at all) uses the
**edge function** technique, essentially the same algorithm real GPU
hardware rasterizers use (formalized for graphics by Juan Pineda in a
1988 paper, "A Parallel Algorithm for Polygon Rasterization" -- notable
for being easy to parallelize across pixels, which is exactly what a
GPU wants).

The idea: for a triangle with corners `p0, p1, p2` and any point `p`,
define `edge(a, b, p)` as a signed value that's positive if `p` is on
one side of the line through `a` and `b`, negative on the other, and
zero exactly on the line (it's the z-component of a 2D cross product --
same underlying idea as §2's cross product, just done in 2D).
Computing all 3 edge functions for a point `p` against all 3 of the
triangle's edges tells you: if all 3 come out the same sign, `p` is
inside the triangle; if they don't, it's outside. `Triangle.fill`
does exactly this, for every pixel in the triangle's bounding box (the
smallest rectangle containing all 3 corners -- there's no point testing
pixels that can't possibly be inside).

The 3 edge-function values, once normalized by dividing by the
triangle's total area, are called **barycentric coordinates**
(`l0`/`l1`/`l2` in the code): 3 numbers, one per vertex, that sum to 1,
that tell you "how much weight" each of the triangle's 3 corners
contributes to this exact pixel (a pixel exactly at `p0` has
barycentric coordinates `(1, 0, 0)`; a pixel exactly in the middle has
roughly `(0.33, 0.33, 0.33)`). This turns out to be *the* general tool
for smoothly interpolating anything that's defined per-vertex across a
triangle's interior -- `Triangle.fill` uses it for two different
things simultaneously: the interpolated depth `z` (fed to the z-buffer
test, §6) and the interpolated texture coordinates `u`/`v` (fed to
`Texture`'s samplers, see §9). Gouraud shading (§8) is the same trick
applied to per-vertex *colors* instead.

An older, now mostly-historical alternative to edge functions is
**scanline rasterization**: for each horizontal row (scanline) the
triangle covers, work out the two x-coordinates where the triangle's
edges cross that row (by walking each edge's slope incrementally from
one scanline to the next), and fill in every pixel between them. This
is how software rasterizers were commonly written before the
edge-function approach became standard; it's a bit more fiddly to get
exactly right at triangle edges/shared vertices, and much harder to
parallelize (each row depends on incremental state from the row
above), which is part of why edge functions won out once parallel
hardware (GPUs) became the target. (The 2D rasterizer, `graphics/2d/Fill`,
is a scanline one: compare.)

Three refinements, each its own debug key (§11):

- **The fill rule** (`t`). A pixel whose center is exactly on an edge
  shared by two triangles -- a rectangle's diagonal, any mesh's inner
  edges -- must be drawn by exactly one of them: by both is wasted work
  (and wrong with transparency), by neither is a hole, a "crack". In
  floating point, the two triangles compute that edge's value with
  different roundings, so neither may see it as inside. The default fix
  is an epsilon (count slightly outside as inside: drawn by both); what
  GPUs do is the **top-left rule** (the pixel belongs to the triangle
  for which that edge is a top or left one), made exact by snapping the
  vertices to 1/256th of a pixel first ("sub-pixel precision"), after
  which no edge value needs rounding. See `Triangle.mli`, with a
  picture.
- **Incremental edge functions** (`o`, an optimization). An edge
  function is linear in x, so one pixel to the right it changes by a
  constant: add it instead of recomputing (see `notes_3d_opti.md`).
- **Near-plane clipping** (`c`). Projection divides by depth, so a
  vertex behind the camera has no sensible pixel. Dropping every
  triangle with such a vertex leaves holes near the camera, e.g. the
  floor under your feet in `Corridor3d.ml`; `Clip` cuts the triangle
  to its part in front of the near plane instead (Sutherland and
  Hodgman, 1974), which gives 0, 1 or 2 triangles.

## 8. Shading models: flat, Gouraud, Phong (and where we are)

"Shading" (as opposed to "shape") is about *how a surface's color is
computed*, typically in response to a light source -- this is a
separate concern from everything above (which is all purely about
*geometry*: where things are, which pixels they cover, which one is in
front). It's worth being precise about the terms here since they're
easy to conflate:

- **Flat color** (no lighting): every pixel of a face just gets the
  same, fixed color, with **no lighting calculation at all** -- the
  color or texture sample is used exactly as given, with no darkening
  on faces that would, in a real scene, be angled away from a light. This is simpler than any of the shading models below, not a
  variant of one of them.
- **Flat shading**: one step up from flat color -- compute lighting
  *once per face* (using the face's single normal, e.g. "how aligned is
  this face's normal with the direction to a light"), and paint the
  whole face that one resulting color. Faces closer to face-on to a
  light are brighter; every pixel of one face is still identical, so
  you can clearly see the "facets" of a low-poly model (this is the
  classic "faceted" low-poly look).
- **Gouraud shading** (Henri Gouraud, 1971 -- one of the foundational
  papers of computer graphics). Compute lighting *per vertex* instead
  (each vertex needs its own normal for this, usually averaged from the
  normals of all the faces meeting at that vertex, so it doesn't just
  equal one face's flat normal), then **linearly interpolate the
  resulting color** across each triangle using the exact same
  barycentric-coordinate trick from §7. The visual effect: a shape
  built from few triangles can look smoothly curved and shaded, instead
  of faceted, because the color itself blends smoothly between
  vertices, even though the underlying geometry is still flat
  triangles. The catch: small, sharp lighting features (e.g. a tight
  specular highlight) that would fall *between* vertices, away from any
  of them, get missed or smeared out, since only the vertices' own
  lighting values ever get computed.
- **Phong shading** (Bui Tuong Phong, 1973 -- note this is a different,
  related-but-distinct thing from the "Phong reflection model", the
  actual lighting *math* formula for computing a color from a light and
  a surface, which Phong also introduced and which is often used
  *inside* both Gouraud and Phong shading). Instead of interpolating
  the final *color* across a triangle, interpolate the *normal vector*
  itself (again via barycentric coordinates), and run the full lighting
  calculation **separately at every single pixel**, using that
  triangle's smoothly-interpolated normal. Much better at capturing
  small highlights (since every pixel gets its own real lighting
  calculation, not an interpolation of just 3 samples), at the cost of
  doing that calculation once per pixel instead of once per vertex --
  too expensive for real-time use for a long time, standard by the time
  GPUs could run a custom calculation ("shader") per pixel.

**Where `Playground3d` sits on this spectrum today: all four,
pluggable at runtime** (native backend only -- see §11's `m` toggle and
`notes_3d_shading.md` for the full writeup; the code is `Shading`,
and the lighting formula itself, Lambert's cosine law with an ambient
floor, is `geometry/Lighting`, shared with the web and OpenGL backends).
`flat_color`/`flat_shading` work exactly as described above, flat
shading using the same winding-based normal as backface culling (§5). Gouraud/Phong needed one more
piece first: a normal *per vertex*, which `cube`/`box`/`plane` have no
use for (each face's corners are independent points, not shared with
neighboring faces, so a per-vertex normal would just equal that one
face's flat normal) -- so they only look different from `flat_shading`
on a curved shape approximated by many small faces with genuinely
varying normals, like the `sphere` primitive added alongside this
(`Spheres3d.ml` is the demo built to show it).

## 9. Texture mapping: UV coordinates

Independent of shading (§8), you can also give a surface a color that
varies across it by *sampling an image* instead of using one flat
color -- **texture mapping**. Every vertex gets, in addition to its 3D
position, a 2D **UV coordinate** (`u, v`, each conventionally in
`0..1`) saying "which point in the source image does this vertex
correspond to" (`(0,0)` = the image's top-left corner, `(1,1)` = its
bottom-right, by the convention `textured_quad` uses). Just like depth
and color, UV coordinates get linearly interpolated across a triangle
using barycentric coordinates (§7) -- so a pixel in the *middle* of a
textured triangle samples from roughly the middle of the corresponding
part of the image, and so on smoothly across the whole face. This is
exactly what `Triangle.fill`'s `color ~u ~v` callback and `Texture`'s
samplers do.

Two choices, each with a simple and a better version, both
implemented (and switchable, §11):

- **Nearest-neighbor or bilinear sampling** (`i`):
  `Texture.sample_nearest` just takes the source pixel containing
  `(u, v)` and uses its exact color. The alternative,
  `Texture.sample_bilinear`, blends the 4 nearest source pixels
  proportionally, giving a smoother result when a texture is magnified
  (stretched larger than its native resolution) instead of visible
  blocky pixelation (a deliberately authentic look for a
  Minecraft-style game, incidentally, which can ask for it with
  `rendering`'s `smooth_textures = false`). The same two filters as
  the 2D rasterizer's images, `graphics/core/Blit`.
- **Linear or perspective-correct interpolation** (`p`): barycentric
  interpolation, done naively, is linear *in screen space*. Real
  perspective projection is *not* linear (§4.3's division by depth
  sees to that), so linearly interpolating UV coordinates (or colors,
  or anything else) directly in screen space is a subtly incorrect
  shortcut -- it can visibly warp textures on large triangles seen at a
  steep angle (a classic, very visible example is the "warping floor"
  look of the original PlayStation's 3D rendering, which used exactly
  this shortcut for performance reasons). The correct fix,
  **perspective-correct interpolation**, interpolates `u/z`, `v/z`, and
  `1/z` instead of `u`, `v` directly, then divides back out at the end
  -- `Interpolate`, the default (and what `Project.vertex` prepares
  the `1/z`, `u/z`, `v/z` for).

## 10. lucamug's elm-playground-3d vs. this library, side by side

| | lucamug's elm-playground-3d | `Playground3d` |
|---|---|---|
| Shape representation | `Shape3d`/`Form3d`, world-space points, no transform header | Same design, copied deliberately (see `plan_playground3d.md`) |
| Camera | Eye + target ("look-at"), fixed presets (`camera1`..`camera4`) | Same eye/target model, but a real record you construct with your own values, and (unlike lucamug's) usable as a genuinely *moving* value computed fresh each frame from your game's model |
| Backface culling | None | Yes (§5) |
| Hidden surface removal | None (relies on manual face ordering + specific camera angles) | Painter's algorithm on web (§6); a real z-buffer on native, or the painter's algorithm with `z` (§6) |
| Rendering target | SVG only (via elm-playground's existing renderer) | SVG (via `elm_playground_web`, reusing the same "compile 3D down to 2D shapes" trick) *and* a real hand-written software rasterizer for native |
| Textures | None | `textured_quad`/`textured_cube`, real per-pixel sampling on native (§9); flat placeholder color on web |
| Shading | None | flat_color/flat_shading/Gouraud/Phong, pluggable at runtime (§8, §11) |

The one approach neither library uses at all, worth knowing about as
"the other end of the spectrum": **WebGL/OpenGL**, as used by
`elm-explorations/webgl` and the much more full-featured
`ianmackenzie/elm-3d-scene` (mentioned in `plan_playground3d.md`).
There, instead of writing your own projection math and rasterizer by
hand in OCaml/Elm, you upload vertex data to the GPU and write small
programs ("shaders", in a C-like language called GLSL) that the GPU
itself runs, once per vertex and once per pixel, in parallel across
thousands of cores -- the GPU's own dedicated hardware does the
rasterization (edge functions, §7) and depth testing (z-buffer, §6)
for you. Vastly faster and more capable (real per-pixel Phong shading,
shadows, reflections, and more all become practical), but a genuinely
different, heavier-weight programming model (a shader language, GPU
buffer management, usually a real matrix/quaternion math library) --
deliberately not what this library is going for; the whole point of
`Playground3d` is that you can read every line of `Playground3d.ml`
and `graphics/3d/` and see exactly what number produced what pixel, the same "no magic" spirit as the original
2D `elm-playground`. (This project has an OpenGL backend too, to
compare: the same scenes, the same
`Lighting`, drawn by the GPU.) See `notes_playground3d_related_work.md` for the
fuller survey -- the rest of the Elm "3D playground" lineage
(`erkal`'s and `nateabele`'s projects too), plus VRML, OpenGL, WebGL,
Vulkan, and Unity, and how `Playground3d`'s teaching-first, no-GPU
design compares to each.

## 11. Try it yourself: runtime-toggleable rendering modes

The native software backend doesn't just describe
several of the trade-offs above -- they're wired up as live, in-game
toggles you can flip with a single key press while any native
example/game is running with the `-debug-keys` flag (off by default,
so a game can use any key), so you can directly compare "simple" vs "more
correct" side by side instead of just reading about the difference.
Each one is a field of `Render.options`, and each version lives in its
own module or function of `graphics/3d/` (e.g. `Interpolate.make`'s
two cases, `Painter` vs `Zbuffer`), so you can read either version of
a given trade-off on its own. `h` shows them all, with their current
state, over the frame; the window title too. The main ones:

- **`m` -- shading mode** (§8): cycles through all 4 modes described in
  §8 -- `flat_color` (no lighting at all -- every face/texel drawn
  exactly as given, the library's behavior until shading was added),
  `flat_shading` (one brightness value per face, from a fixed
  directional light and the face's own normal -- the same normal
  already computed for backface culling, reused here at no extra
  cost), `Gouraud` (one brightness value per *vertex*, blended across
  each triangle), and `Phong` (the vertex *normals* blended per pixel,
  brightness computed at every pixel). `cube`/`box`/`plane` render
  pixel-for-pixel identically in the 3 lit modes, since each face's
  corners are independent points, not shared with neighboring faces --
  run `Spheres3d.exe` and press `m` there instead, where the
  `sphere` primitive's genuinely varying per-vertex normals make all 4
  modes look visibly different from each other (`notes_3d_shading.md`
  has the full implementation writeup).
- **`b` -- backface culling on/off** (§5): off is the simplest possible
  code (draw every triangle, full stop); on is the library's default.
  **In *filled* mode this shows no visual difference at all** -- only
  the FPS counter changes (off draws roughly double the triangles for a
  closed shape like a cube). That's not a limitation, it's fundamental:
  the z-buffer already independently decides, per pixel, which triangle
  is nearest, and for a closed solid that decision always agrees with
  what culling would have picked anyway (a back face can never win the
  z-test against the front face covering the same pixels) -- so culling
  can only ever save work, never change the picture, whenever a z-test
  is present. To actually *see* it do something, press `f` first
  (wireframe, which has no per-pixel visibility resolution of any kind)
  and *then* toggle `b`: with culling off you'll see extra edges from
  each shape's hidden/inside faces that culling normally removes before
  they're ever drawn (e.g. on a single cube, the 3 short edges meeting
  at its otherwise entirely hidden far corner).
- **`f` -- wireframe vs filled** (§7): wireframe draws only each
  triangle's 3 edges as plain lines, with none of the bounding-box/
  edge-function/z-buffer machinery filled rendering needs -- a good way
  to *see* the actual triangle mesh underneath a shape (e.g. watch a
  cube's 6 quad faces resolve into 12 triangles, each pair split along
  its diagonal, exactly as described in §3 and §7 -- wireframe mode
  currently draws that internal diagonal too, not just each shape's
  true edges).
- **`z` -- painter's algorithm vs z-buffer** (§6): **run
  `PaintersAlgorithmFail3d.ml` for this one, not `Cubes3d.ml`.**
  `Cubes3d.ml`'s grid of separate, same-size, non-overlapping cubes
  turns out not to stress painter's algorithm enough to visibly break --
  a whole-face centroid-distance sort happens to get the order right
  almost everywhere for that scene (confirmed by hand: forcing
  `Painters_algorithm` as the default there and comparing screenshots
  showed no real difference). `PaintersAlgorithmFail3d.ml`'s two
  genuinely intersecting boxes are the real test: where they cross,
  *part* of one box's face is in front of the other and *part of that
  same face* is behind it, so no single "draw this whole face before/
  after that one" decision (all painter's algorithm gets to make, once
  per face) can be correct for the entire crossing at once. Toggling
  `z` there reliably shows a visible glitch; the z-buffer (the default)
  resolves it correctly per pixel, with no such error possible --
  exactly the historical trade-off §6 describes.
- **`p` -- perspective-correct vs linear interpolation** (§9's UV
  section, and the "why not linear?" reasoning that turned out to apply
  to depth too): try this on `TexturedCube3d.ml` specifically, and
  watch rather than look at a single frame -- the bug is about *motion*.
  `Linear` interpolates a triangle's depth and texture coordinates the
  naive way (directly, via screen-space barycentric weights), which is
  only an approximation, worse the more a triangle's depth varies
  across itself; since that varies continuously as the cube rotates,
  checker.png's own crosshair visibly swims/drifts within each face
  instead of staying put -- confirmed live: forcing `Linear` as the
  default reproduced exactly the swimming bug the original perspective-
  correct fix was written to solve. `Perspective_correct` (the default)
  interpolates `1/z`, `u/z`, `v/z` instead (genuinely linear in screen
  space, so exact rather than approximate) and holds the texture
  perfectly still on the rotating cube.

And the others, described above: `i` nearest or bilinear texture
filtering (§9), `c` near-plane clipping and `t` the top-left fill rule
(§7), `o` the simple versions of the optimizations (a quick way to see
what they buy, on the fps counter), and `x` a pixel magnifier following
the mouse (the one from the 2D rasterizer, `graphics/2d/Magnifier`),
to look at edges and the fill rule up close. `README-3d.md` has the
table.

## 12. HUD: a 2D overlay on top of the 3D scene

Every real-time 3D game needs *some* 2D on top of the 3D -- a score, a
health bar, a crosshair, an "instructions" text. This is universally
called a **HUD** ("heads-up display", borrowed from the transparent
displays projected onto a fighter pilot's windshield). The
implementation idea is almost always the same, regardless of engine:
render the 3D scene first, then render flat 2D elements *on top of*
the finished image, in screen space (pixel/screen coordinates, not
world-space X/Y/Z) -- a HUD element has no position "in" the 3D world
at all, and in particular no depth to z-test against anything.

`game3d`'s `view3d` still only ever returns one thing, a 3D scene
(`camera * shape3d list`) -- no second "2D overlay" return value was
added. Instead, `Playground3d.hud` wraps an ordinary 2D
`Playground.shape` (built with `words`/`rectangle`/`image`/`group`/
`move`/`fade`, the exact same combinators §1's `picture`/`animation`/
`game` already use) into a `shape3d` you drop directly into the list:

```ocaml
(cam, [ ground; player; stars_group;
        hud (words black (Printf.sprintf "Score: %d" m.score)
             |> move (computer.screen.left +. 60.) (computer.screen.top -. 40.)) ])
```

Nothing new to learn if you already know the 2D playground -- `hud` is
the *only* new combinator, and a `Hud` shape uses the exact same
coordinate system (origin at screen center, `computer.screen`'s
bounds) as any 2D `picture`. The one thing worth internalizing: a
`Hud` shape is **exempt from `move3d`/`rotate3d`/`scale3d`** -- they're
no-ops on it, even nested inside a `group3d` that itself gets rotated
(e.g. `InteractiveCube3d.ml`'s mouse-driven turntable). That's not an
inconsistency, it's the whole point of "HUD": text that's supposed to
stay glued to the corner of the screen would look broken if the 3D
scene's own transforms could drag it around.

**How each backend actually draws it**, since "on top of the finished
image" means something different depending on how that image gets
made in the first place:

- **Native** (the software rasterizer) already writes every pixel by
  hand into a `graphics/core` Framebuffer over the SDL window surface.
  Once the 3D scene is fully rasterized into it for the frame, the HUD
  pass draws the HUD shapes directly on top, into the *same*
  framebuffer, via `Shape_render_software.render` -- the exact same
  from-scratch 2D shape-drawing code (`graphics/2d/`) the 2D
  `elm_playground_software` backend uses for *everything*, reused
  unchanged here for just the HUD layer. No new rendering code, no
  alpha-blending step of its own to write: drawing a shape already only
  overwrites the pixels it actually covers. So the whole native 3D
  backend is from scratch, Cairo-free.
- **Web**: since this backend already compiles the whole 3D scene down
  to ordinary `Playground.shape` values every frame (`render3d_to_2d`,
  §10) and hands them to the existing SVG renderer, a `Hud` shape just
  needs to ride along in that same list, unprojected -- appended
  *after* the 3D-derived shapes (SVG draws later elements on top), so
  it reaches the unmodified web renderer exactly like any other 2D
  shape would. Genuinely free: zero new rendering code at all.
- **OpenGL**: not supported yet (see `docs/claude_notes/done/plan_opengl.md`'s
  Scope) -- that backend owns its own GPU-side framebuffer rather than
  a plain CPU pixel buffer, so native's "just draw into the same
  memory" trick doesn't transfer directly; it would need rendering the
  HUD to an offscreen surface, uploading it as a texture, and drawing
  a screen-aligned quad with it in a separate pass.

See `docs/claude_notes/done/plan_hud.md` for the full design writeup
(including the one non-obvious implementation wrinkle: dune seals a
virtual module's implementation to exactly its virtual `.mli`, so the
3D software backend's `Playground3d_platform.ml` can only call a 2D
backend's shape-drawing code through a plain sibling module of its
`Playground_platform`, like `Shape_render_software`).

## Glossary (quick reference)

- **Vertex**: one corner point of a shape.
- **Face / polygon**: a flat surface made of an ordered list of
  vertices. **Quad** = 4-vertex face, **triangle** = 3-vertex face.
- **Mesh**: a shape made of many faces.
- **Normal**: a vector perpendicular to a face (or, for shading, a
  vertex), conventionally pointing outward.
- **World / model space**: the shared coordinate system all objects in
  a scene are placed into.
- **View / camera space**: the world re-expressed relative to the
  camera, as if it were at the origin looking down one axis.
- **Projection**: converting 3D (view-space) coordinates to 2D.
  **Perspective** projection makes farther things look smaller (real
  cameras/eyes); **orthographic** projection doesn't.
- **NDC (normalized device coordinates)**: the `-1..1`-ish 2D
  coordinate range projection produces, before converting to actual
  pixel coordinates.
- **Frustum**: the pyramid-with-the-tip-cut-off-shaped volume of space
  a camera can see, bounded by its field of view and near/far planes.
- **FOV (field of view)**: how wide a camera's frustum is, in degrees.
- **Near/far plane**: the closest/farthest distance a camera can see;
  needed to avoid dividing by zero (at the camera itself) and to bound
  how far to render.
- **Backface culling**: skipping faces whose normal points away from
  the camera, since they can never be visible on an opaque solid.
- **Hidden surface removal / visibility**: the general problem of
  figuring out which face wins at each pixel when several overlap.
- **Painter's algorithm**: solve visibility by sorting whole faces
  back-to-front and drawing in that order.
- **Z-buffer / depth buffer**: solve visibility by keeping a per-pixel
  "closest depth so far" and only overwriting a pixel when something
  nearer comes along.
- **BSP tree**: a precomputed spatial data structure that gives a
  guaranteed-correct face ordering for any camera position, without a
  per-pixel depth buffer.
- **Ray tracing**: solving visibility (and more) by shooting a ray per
  pixel into the scene and finding what it hits, instead of projecting
  geometry onto the screen.
- **Rasterization**: the (opposite-of-ray-tracing) approach of
  projecting geometry onto the screen and figuring out which pixels
  each triangle covers.
- **Near-plane clipping**: cutting a triangle that goes behind the
  camera to its part in front of it, instead of dropping it.
- **Fill rule**: which triangle gets a pixel exactly on an edge shared
  by two; the **top-left rule** gives it to exactly one.
- **Edge function**: the per-edge test (from Pineda, 1988) used to
  decide whether a point is inside a triangle, and the basis for...
- **Barycentric coordinates**: 3 per-vertex weights (summing to 1) for
  a point inside a triangle, used to smoothly interpolate anything
  defined per-vertex (depth, UV coordinates, color) across it.
- **UV coordinates**: a 2D `(u, v)` position (usually `0..1`) into a
  texture image, stored per-vertex, used for texture mapping.
- **Texture mapping**: coloring a surface by sampling an image via
  interpolated UV coordinates, instead of (or blended with) a flat
  color.
- **Nearest-neighbor / bilinear filtering**: how to pick a color when a
  sampled UV coordinate falls between exact texture pixels -- snap to
  the closest one, or blend the 4 nearest.
- **Perspective-correct interpolation**: correctly accounting for
  projection's nonlinearity (§4.3) when interpolating UVs/colors/etc.
  across a triangle, instead of the simpler (and subtly wrong) linear
  screen-space interpolation.
- **Flat color**: no lighting at all.
- **Flat shading**: one lighting calculation per face.
- **Gouraud shading**: one lighting calculation per vertex, colors
  interpolated (via barycentric coordinates) across each triangle.
- **Phong shading**: normals interpolated across each triangle, full
  lighting calculation done at every pixel.
