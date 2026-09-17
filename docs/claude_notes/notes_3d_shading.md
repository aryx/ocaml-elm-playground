# Shading in playground3d/: current state and how to extend it

`notes_3d.md` section 8 explains the *concepts* (flat color vs. flat/
Gouraud/Phong shading) from first principles, and section 11 covers the
`m` runtime toggle. This note is the complement: a walk through the
*actual code* implementing shading today, why it was built this way,
and a concrete roadmap for each obvious next step, for whenever this
gets revisited (a `sphere`/`cone` primitive is a prerequisite for a few
of these -- see below).

## What exists today

All of it lives in `playground3d/native/Playground3d_platform.ml`'s
"Shading" section (only the native backend does any of this -- the web
backend has no per-pixel access at all, so it can't shade anything; see
`notes_3d.md`'s lucamug comparison table).

- `type shading = Flat_color | Flat_shading` -- the two implemented
  modes, `Flat_color` being "what this file always did before shading
  was added" (every face drawn exactly as given, no lighting at all).
- `light_dir : vec3` -- a single, fixed **directional** light (a "sun":
  see the field's own doc comment for why a directional light rather
  than a point light or spotlight was the right minimal choice here).
- `ambient : float = 0.25` -- a brightness floor, so a face pointing
  entirely away from the light doesn't go fully black (which would look
  like a hole rather than a shaded surface).
- `brightness_of_normal : vec3 -> float` -- the actual math: `max 0
  (dot normal light_dir)`, rescaled into `[ambient, 1.0]`. This is a
  single `dot` product (§2 of `notes_3d.md`) -- the entire "lighting
  model" is "how aligned is this face with the light."
- `fill_of_material` calls `brightness_of_normal` **once per face**
  (not once per pixel -- see its own doc comment) and scales whichever
  color that face would have shown (a flat color, or a sampled texel)
  by that one brightness value via `scale_channel`.

That's the whole implementation: one dot product per face, applied
uniformly to every pixel of that face. There is currently no notion of
light *color* (only intensity/brightness -- a "white" light that just
dims or brightens whatever color is already there), no more than one
light, and no shadows (a face's brightness only depends on its own
orientation, never on whether something else is blocking the light).

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
`fill_of_material`'s `shade` function to multiply each channel by its
own factor. Cheap to add; mostly a matter of deciding on a light-color
representation.

### 3. Gouraud shading (needs a new primitive first)

As `notes_3d.md` §8/§11 explains: Gouraud needs a normal *per vertex*,
computed by averaging the normals of every face that shares that
vertex, then blended across a triangle the same way `u`/`v` already are
(barycentric interpolation, §7). The blocker isn't the interpolation
machinery -- we already have exactly that machinery, built for
perspective-correct texturing (`notes_3d_opti.md`'s "Fix 2") -- it's
that **our current shapes have no shared vertices to average in the
first place**: `cube`/`box`/`plane` each give every face its own 4
independent corner points (see `Playground3d.box_faces`), specifically
so hard edges stay sharp. Gouraud only looks *different* from flat
shading on a surface where neighboring faces have gradually-changing
normals -- i.e. a curved shape approximated by many small flat faces,
like a tessellated `sphere`. Concretely, this needs, in order:
1. A `sphere` (or `cone`/`cylinder`) primitive that tessellates a curved
   surface into many small triangular/quad faces (mentioned as a
   possible future primitive in `notes_3d.md`'s design-credit section).
2. A mesh representation that tracks *shared* vertices across faces
   (so a vertex's normal can be the average of its neighbors) --
   `Playground3d.shape3d` doesn't have this today at all.
3. Extending `vertex` (already carrying `u_over_z`/`v_over_z` for
   perspective-correct texturing) to also carry a per-vertex normal,
   interpolated the same way.

### 4. Phong shading (same prerequisite as Gouraud, one step further)

Once per-vertex normals and a curved primitive exist for Gouraud, Phong
is "only" a matter of interpolating the *normal itself* per pixel
(again, the exact same barycentric-interpolation trick, applied to a
third kind of vertex attribute alongside depth and UV) and calling
`brightness_of_normal` **inside the pixel loop** instead of once per
face. This does move real cost into the hot path (a dot product per
pixel instead of per face), which is worth benchmarking
(`notes_3d_opti.md`-style) once it exists, rather than assuming it's
free.

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
