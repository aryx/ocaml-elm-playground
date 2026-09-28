# Shading in Playground3d: current state and how to extend it

`notes_3d.md` section 8 explains the *concepts* (flat color vs. flat/
Gouraud/Phong shading) from first principles, and section 11 covers the
`m` runtime toggle. This note is the complement: a walk through the
*actual code* implementing shading today, why it was built this way,
and a concrete roadmap for each obvious next step, for whenever this
gets revisited (a `sphere`/`cone` primitive is a prerequisite for a few
of these -- see below).

## What exists today

The lighting formula is `graphics/3d/geometry/Lighting.ml` (`light_dir`,
`ambient`, `brightness_of_normal`, shared by the three 3D backends); the
4 modes are `graphics/3d/Shading.ml` (`Shading.make`, the native
software backend only -- the web backend has no per-pixel access at
all, so it can only light each face with one color, flat; see
`notes_3d.md`'s lucamug comparison table); the per-face colors are
`graphics/3d/Render.ml`'s `color_of_paint`.

- `type mode = Flat_color | Flat_shading | Gouraud | Phong` -- all 4
  modes from the roadmap below are now implemented; "m" cycles through
  them at runtime (`notes_3d.md` section 11).
- `light_dir : vec3` -- a single, fixed **directional** light (a "sun":
  see the field's own doc comment for why a directional light rather
  than a point light or spotlight was the right minimal choice here).
- `ambient : float = 0.25` -- a brightness floor, so a face pointing
  entirely away from the light doesn't go fully black (which would look
  like a hole rather than a shaded surface).
- `brightness_of_normal : vec3 -> float` -- the actual math: `max 0
  (dot normal light_dir)`, rescaled into `[ambient, 1.0]`. This is a
  single `dot` product (§2 of `notes_3d.md`) -- the entire "lighting
  model" is "how aligned is this surface with the light." Pure (no
  branching on the current shading mode); what varies between modes is
  *which* normal(s) get fed into it and *how often* (once per face,
  once per vertex, or once per pixel) -- see `Shading.make` below.
- `Shading.make mode v0 v1 v2 : l0:float -> l1:float -> l2:float -> float` --
  a per-triangle closure, built once and called once per covered pixel
  (the same "decide once per triangle, apply once per pixel" shape as
  `Interpolate.make` for depth/UV), that is the one place all 4 modes
  actually differ:
  - `Flat_color` -- ignores the normal entirely, always `1.`.
  - `Flat_shading` -- `brightness_of_normal v0.normal` computed once
    (any of the 3 vertices; a flat face's vertices all share the same
    winding-based normal), ignoring the barycentric weights.
  - `Gouraud` -- `brightness_of_normal` computed once per *vertex*
    (3 dot products per triangle), then the 3 numbers blended per pixel
    via `l0`/`l1`/`l2`.
  - `Phong` -- the vertex *normals themselves* blended per pixel via
    `l0`/`l1`/`l2`, renormalized, then `brightness_of_normal` computed
    on that (1 dot product per pixel, not per vertex).
- `Render.color_of_paint` no longer computes brightness at all -- it takes
  `~brightness` as a 4th per-pixel argument (alongside `~u`/`~v`) and
  just multiplies it into whichever color it resolves (flat, or a
  sampled texel), via `scale_channel`. This keeps it fully decoupled
  from *how* brightness was computed, which is what let Gouraud/Phong
  slot in without touching it at all.

Gouraud and Phong only look any different from `Flat_shading` on a
shape whose faces share vertices with *varying* normals across them --
i.e. a curved surface approximated by many small faces, like `sphere`
(see "3. Gouraud shading" below, now implemented). On `cube`/`box`/
`plane` (each face's corners are its own independent points, see
`Playground3d.box_faces`) all 3 lit modes render pixel-for-pixel
identically; only `Flat_color` looks different there.

There is currently no notion of light *color* (only intensity/
brightness -- a "white" light that just dims or brightens whatever
color is already there), no more than one light, and no shadows (a
surface's brightness only depends on its own orientation, never on
whether something else is blocking the light).

## Roadmap, roughly in order of effort

### 1. Multiple lights (small)

Change `light_dir` from one vector to a list of `(direction, weight)`
pairs, and have `brightness_of_normal` sum (or take the max of) each
light's contribution, clamped back into `[ambient, 1.0]` at the end.
No new concepts needed -- just repeating the existing dot-product
calculation once per light. A natural first step if a scene ever wants
e.g. a cooler fill light in addition to the main "sun".

### 2. Colored lights (small-medium)

Right now `scale_channel` multiplies every one of r/g/b by the *same*
brightness number, so a light can only ever dim or brighten a surface's
own color, never tint it. A colored light would need `brightness_of_
normal` to return an `(r, g, b)` triple of scale factors (e.g. a warm
light might scale red/green more than blue) instead of one float, and
`Render.color_of_paint`'s `shade` function to multiply each channel by its
own factor. Cheap to add; mostly a matter of deciding on a light-color
representation.

### 3. Gouraud shading -- DONE

Implemented alongside `Phong` and a new `sphere` primitive (see
`docs/claude_notes/plan_gouraud_phong.md` for the design writeup). The
key realization that avoided the originally-assumed prerequisite (a
general mesh representation tracking shared vertices/adjacency across
faces, so a vertex's normal could be the *average* of its neighbors):
`sphere` is centered at the origin, so a point on it has a normal in
closed form -- its own position, normalized -- no averaging needed at
all. So instead of generalizing `Playground3d.shape3d`'s mesh
representation, a new, minimal `form3d` case was added:

```ocaml
| SmoothPolygon3d of Playground.color * ((number*number*number) * (number*number*number)) list
  (* (point, normal) pairs -- each point carries its OWN normal,
     instead of sharing one normal computed from the face's winding
     order like Polygon3d/TexturedPolygon3d do *)
```

`sphere color radius` tessellates a standard UV-sphere out of
`SmoothPolygon3d` quads, each corner's normal computed analytically.
On the native side, `vertex` gained a `normal : vec3` field (filled in
by `Project.vertex`, alongside the existing depth/UV) and
`Shape3d_render_software.faces` attaches a normal to every point of every face --
either the single winding-based `face_normal` repeated for every point
of a `Polygon3d`/`TexturedPolygon3d` face (which is exactly what keeps
`Flat_shading` uniform across such a face), or the stored per-vertex
normal for a `SmoothPolygon3d` one. `Gouraud` then blends
`brightness_of_normal v0.normal`/`v1.normal`/`v2.normal` (3 dot
products, once per vertex) across each triangle the same way `u`/`v`
already are (barycentric interpolation, §7) -- see `Shading.make` above.

One simplification, stated up front rather than discovered by accident:
Gouraud/Phong interpolate brightness/normals *linearly*, not
perspective-correctly like `Interpolate.make`'s `u`/`v`/`z` (the `p`
toggle). Brightness differences are usually too subtle for the
difference to be visible, and reusing that machinery for a
differently-shaped attribute (a normal, not a single float) would add
real complexity for likely-invisible benefit -- worth revisiting only
if it turns out to actually matter.

### 4. Phong shading -- DONE

Built in the same pass as Gouraud (see above): instead of blending
*brightness values* per pixel, `Phong` blends the *normal components*
themselves via `l0`/`l1`/`l2`, renormalizes (an interpolated blend of
unit vectors isn't unit-length in general), and calls
`brightness_of_normal` on the result -- one dot product per pixel
instead of per vertex. `Spheres3d.ml` is the demo built to
actually show all 4 modes differing from each other (press "m"): two
spheres, since `cube`/`box`/`plane` render identically under all 3 lit
modes. No FPS regression worth calling out was observed in casual
testing; a `notes_3d_opti.md`-style benchmark hasn't been done, since
nothing so far has made this scene's frame rate feel like a bottleneck.

### 5. Shadows (large, a genuinely different feature)

Nothing above lets one shape's presence darken another shape's surface
-- every face's brightness is computed purely from its own orientation
relative to the light, oblivious to whatever else is in the scene. Real
shadows need either a second rendering pass from the light's point of
view (a "shadow map", checking whether a pixel is occluded from the
light before shading it) or a form of shadow ray-casting -- both a
substantially bigger addition than anything above, and probably not
worth attempting before the simpler steps (and before a scene, like a
tiny-minecraft-style world, exists that would actually benefit from
it).
