# Playground3d vs. Doom and Quake

You asked for a comparison between this project's rendering strategy
and id Software's *Doom* (1993) and *Quake* (1996) -- two of the most
historically important 3D-graphics engines ever shipped, and a good
lens for understanding *why* our renderer is built the way it is versus
the ways it isn't. Companion to [`notes_3d.md`](notes_3d.md) (the
concepts) and [`notes_3d_opti.md`](notes_3d_opti.md) (our own
optimization history) -- read those first if a term here (BSP, z-buffer,
perspective-correct) is unfamiliar.

## The one-line version

Doom, Quake, and `Playground3d`'s native backend are all
**rasterizers**, not ray tracers (§6 of `notes_3d.md`) -- none of them
shoot rays into the scene; all three project geometry onto the screen
and figure out which pixels it covers. Where they differ enormously is
*how* they solve hidden-surface removal and shading, almost entirely
because of the hardware they were built for. Doom and Quake were
engineering feats of working around 1990s CPUs having no meaningful
floating-point throughput, no hardware z-buffer, and no GPU at all;
`Playground3d` was written 30 years later with none of those
constraints, so it gets to make much simpler (if slower, relative to
what's now possible) choices throughout.

## Doom: not actually 3D

The first thing worth knowing is that Doom isn't a true 3D engine at
all -- it's often called "2.5D". Its levels are described as a 2D floor
plan (a set of "sectors", each a flat polygon with a floor height and a
ceiling height), not arbitrary 3D geometry. This is why the original
Doom can't have rooms directly on top of other rooms, and couldn't look
up or down: the renderer fundamentally works in terms of a 2D map, not
3D space.

Doom rendered **column by column** (one vertical strip of the screen at
a time), which only makes sense once you know the map is really 2D:
for each screen column, it walks the map's **BSP tree** (§6 of
`notes_3d.md`) to find which wall segment is visible in that direction,
and tracks a simple list of "which vertical spans of this column are
already fully drawn" so it can stop as soon as a column is complete
(an early, cheap form of what modern GPUs call *early-z* rejection --
skip work for pixels you already know are covered). Because the BSP
guarantees walls are visited in the correct order, **Doom needs no
per-pixel depth buffer at all** -- a huge deal in 1993, when a
320x200x8-bit framebuffer alone (64,000 bytes) was already a real
chunk of a machine's total RAM, let alone a same-sized depth buffer
that would have doubled the memory *and* needed a comparison per pixel
CPUs of the day couldn't spare cycles for.

One detail worth calling out given our own perspective-correct saga
(`notes_3d_opti.md`'s "Fix 2"): Doom's *walls* don't actually need
perspective-correct texture mapping to look right, for a subtle reason
-- a vertical wall segment has (approximately) constant depth along any
one screen *column*, so linearly interpolating a texture coordinate
*down* that column happens to be fine. Doom's *floors and ceilings*
("visplanes") are a different story -- their depth varies continuously
across the screen -- and Doom's renderer handles those with separate,
more expensive code specifically because of this.

## Quake: the real thing, and where "perspective-correct" got its name

Quake is what people usually mean by "the first proper 3D engine": true
arbitrary geometry, looking up and down, rooms genuinely stacked on
other rooms. It still uses a BSP tree, but for a different primary
purpose than Doom's per-column walk: Quake precomputes, for each region
of the map, a **potentially visible set (PVS)** -- which *other*
regions could possibly be seen from here -- so at runtime it can throw
out huge chunks of the map without even attempting to project them.
This is a form of large-scale visibility culling that has no equivalent
in `Playground3d` at all: our backface culling (§5) only ever discards
individual *faces* of a *single already-selected* shape, one at a time
-- nothing in this codebase decides "don't even consider that whole
distant part of the scene," the way Quake's PVS does. That kind of
scene-level culling would become necessary here too, well before a
tiny-minecraft-style world got large.

Quake's world geometry, like Doom's, relied on the BSP for correct
ordering rather than a hardware z-buffer (still a rare, expensive thing
on a 1996 PC) -- but Quake is also the game most associated with
popularizing **perspective-correct texture mapping** as a named,
celebrated technique, for a reason directly relevant to our own
`notes_3d_opti.md`: a true, exact perspective divide (what
`make_interpolator`'s `Perspective_correct` mode does, once per pixel,
without a second thought) was **too slow to do at every pixel** on
period CPUs, which had no fast hardware division. Quake's software
renderer instead did the *exact* divide only once every 16 pixels along
a horizontal span, then **linearly interpolated between those known-
correct points** for the pixels in between -- a hybrid approximation,
correct at regular checkpoints and only slightly off between them,
trading a little accuracy for a lot of speed. Our renderer does the
"slow", always-exact version at every single pixel without needing any
such trick, purely because a 2020s CPU can do a floating-point division
about as fast as it can do a multiplication -- a cost that simply
doesn't need approximating away anymore. It's a nice, concrete
illustration of how "the obviously correct way" and "the way it
actually got built" have been pulled apart and back together by 30
years of hardware change.

## Lighting: sector levels and lightmaps vs. our one directional light

Doom's lighting is about as simple as ours: each **sector** (2D map
region) has a single, level-designer-chosen light level, applied
uniformly to every surface in that sector -- not physically simulated
at all, just an authored number, conceptually similar in spirit (though
not in mechanism) to our `flat_shading` mode giving each *face* one
brightness value (§8 of `notes_3d.md`).

Quake went considerably further with **lightmaps**: a low-resolution,
separate greyscale texture per surface, precomputed (mostly) at
map-build time from the positions of lights in the level, then blended
with the surface's normal texture at render time. This is what gives
Quake's world its characteristic soft, colored pools of light and
shadow gradients across a single wall -- something neither Doom's
flat per-sector levels nor our own single-direction, single-brightness-
per-face `flat_shading` can produce at all. Getting there from here
would mean, in roughly increasing order of effort: multiple light
directions summed together, per-vertex normals for Gouraud shading (see
`notes_3d.md` §8 and §11's "m" entry for why that needs a curved
primitive like a future `sphere` to even be visible), and eventually
something lightmap-like -- baked lighting data separate from a
texture's own color, which nothing in this codebase does yet.

## Textures and color

Both Doom and Quake used an 8-bit **indexed color palette** (256
possible colors per pixel, looked up from a shared palette table) --
not because it looked better, but because it was 4x less memory
bandwidth and storage than direct 24-bit RGB, which mattered enormously
on 1990s hardware. `Playground3d`'s native texture sampling
(`Texture_decode`/`sample_texture`) decodes straight to full RGB(A) via
`stb_image` (see `docs/claude_notes/notes_3d_opti.md`'s pixel-packing
section) with no palette anywhere -- direct color has simply stopped
being expensive enough to avoid.

## Resolution and the hardware gap, in one comparison

Doom shipped rendering **320x200** pixels; our 3D demos run
at **1000x1000** by default -- about 25x as many pixels -- on top of
also doing genuinely more expensive per-pixel work (an exact
perspective divide, a real per-pixel z-buffer test, texture sampling)
that both Doom and Quake went to serious lengths to avoid or
approximate. None of that is a knock on this codebase's own performance
work (`notes_3d_opti.md` is still worth doing!) -- it's the opposite: it
says something about just how much compute headroom a modern CPU hands
you for free, compared to what id Software's engineers were squeezing
blood from in the early-to-mid 1990s.

## Summary table

| | Doom (1993) | Quake (1996) | `Playground3d` native |
| --- | --- | --- | --- |
| Truly 3D? | No (2.5D, 2D map + heights) | Yes | Yes |
| Visibility structure | BSP tree, per-column | BSP tree + PVS | None (per-shape backface culling only) |
| Hidden-surface removal | BSP ordering, no z-buffer | Mostly BSP ordering | Real per-pixel z-buffer (`z`-togglable to painter's algorithm) |
| Perspective-correct texturing | N/A for walls (not needed); separate path for floors/ceilings | Approximated (exact every 16 pixels, linear between) | Exact, every pixel (`p`-togglable to naive linear) |
| Lighting | Per-sector flat light level | Precomputed lightmaps | One fixed directional light, flat per-face (`m`-togglable to none) |
| Color | 8-bit indexed palette | 8-bit indexed palette | Direct 24/32-bit RGB(A) |
| Typical resolution | 320x200 | ~320x240-640x480 | 1000x1000 |
