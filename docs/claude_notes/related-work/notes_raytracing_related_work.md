# graphics/3d's ray tracer vs. the rest of the ray tracing world

Where a small teaching ray tracer sits among POV-Ray, PBRT, Blender's
Cycles and the RTX hardware -- what they do that this will not, and
which of their ideas are small enough to write out and read. The
rasterizer's own lineage is in
[`notes_playground3d_related_work.md`](notes_playground3d_related_work.md)
and [`notes_vs_doom_quake.md`](notes_vs_doom_quake.md); this note is
the other renderer's. Companions:
[`notes_raytracing.md`](../tutorials/notes_raytracing.md) (how it
works) and
[`plan_raytracing_teaching.md`](../plans/plan_raytracing_teaching.md)
(what gets built, in what order).

## The one-line version

| | What it optimizes for | What you see / write |
|---|---|---|
| PBRT, Mitsuba | Physical correctness, and being *the* reference | A book-sized system, scene files, integrators, BSDFs; every choice justified by a paper |
| Arnold, RenderMan (RIS), V-Ray, Cycles, Octane | Film and product images: hours a frame, no visible noise, artists in control | Shader graphs, render layers, denoisers, farms; hundreds of thousands of lines |
| Embree, OptiX, Radeon ProRender | Making *other people's* ray tracers fast (CPU SIMD, GPU, RT cores) | A traversal and intersection API you feed your own shading into |
| DXR / Vulkan RT / Metal, in game engines | Real-time *hybrid*: rasterize visibility, ray trace shadows and reflections, denoise | Acceleration structures, ray shaders, and a denoiser you must not fight |
| POV-Ray | Hobby and demo rendering, driven by a scene language | A `.pov` file: `sphere { <0,1,2>, 1 pigment { Red } }`, and no code at all |
| Shadertoy / SDF ray marching | A picture in one fragment shader, real-time | A distance function, sphere-traced -- no triangles, no scene, no BVH |
| smallpt, *Ray Tracing in One Weekend* | Teaching, by being small enough to read in an evening | 99 lines of C++ / a free three-book series you type in |
| the ICFP 2000 entries | A whole ray tracer in 72 hours, in whatever language you brought | A GML interpreter plus a renderer; public write-ups of what was cut |
| `graphics/3d/Raytrace` + the "y" key | Every ray in the picture readable, on the *same* scene the rasterizer draws | `sphere red 1. \|> shiny 0.8 \|> move3d ...`, then one key, and stills from `-dump-frame` |

## Part 1: where it came from

Ray tracing is older than the rasterizer's tricks it is usually
contrasted with, and it arrived twice: once as engineering, once as
graphics.

- **Arthur Appel, 1968** (IBM): rays for *visibility and shadows*, on a
  plotter. No reflection, no recursion -- what we call ray casting,
  and what §4 of the tutorial builds first.
- **MAGI** (Mathematical Applications Group, Inc.) had commercial ray
  tracing software at the end of **1967**, for nuclear radiation
  exposure, and turned it into pictures: Goldstein and Nagel's "3-D
  Visual Simulation" (**1971**), the *MAGI/SynthaVision Sampler*
  (**1974**), and -- the part everyone remembers -- the solid-modelled
  sequences of Disney's *Tron* (1982), including its light cycles
  (the *Tron* attribution is the one fact here from memory, to check).
  A pleasing local coincidence: this repository already has a
  `games3d/TinyTron3d.ml`.
- **Turner Whitted, 1979-80** (Bell Labs): the recursive model --
  a ray that reflects, refracts and asks the light whether it can be
  seen -- shown in the short film *The Compleat Angler* (1979) and
  published as "An Improved Illumination Model for Shaded Display"
  (CACM, 1980). Its chrome-and-glass spheres over a checkerboard are
  still the first thing a new ray tracer is pointed at, and are
  `examples3d/RaytracingWhitted3d.ml` in the plan. Minutes to hours a
  frame on the hardware of the day (to check).
- **Cook, Porter and Carpenter, 1984** (Lucasfilm): "Distributed Ray
  Tracing" -- jitter the rays and the *same* machinery gives soft
  shadows, glossy reflections, depth of field and motion blur.
- **Kajiya, 1986**: "The Rendering Equation", and path tracing: stop
  enumerating effects, sample the integral. Everything since is a
  variance-reduction argument about that one equation -- Jensen's
  photon mapping (1996), Veach's multiple importance sampling and
  Metropolis light transport (1997) (dates from memory, to check).
- **Film took its time.** *Toy Story* (1995) was rendered with
  REYES -- a rasterizer -- and Pixar's own renderer stayed one for
  years. The first features rendered *entirely* with path tracing came
  later: *Monster House* (2006), *Cloudy with a Chance of Meatballs*
  (2009), and Pixar's own switch by *Monsters University* (2013).
  Worth knowing, because "ray tracing = movies, rasterization = games"
  is a tidy story that was untrue for two decades.
- **Hardware, 2018**: NVIDIA's RTX (GeForce RTX, September 2018) put
  BVH traversal and ray/triangle tests in silicon, with DXR and Vulkan
  ray tracing as the APIs; AMD followed (RDNA 2, 2020), then Intel Arc
  and Apple (M3, 2023). Games since are hybrid; *Quake II RTX* (2019)
  is the famous fully path-traced exception, and it is a 1997 game,
  which tells you the budget (dates after 2018 from memory, to check).

## Part 2: the systems today

- **PBRT** (Pharr, Jakob, Humphreys, 2004-2023) is not really a
  renderer but *the* book with a renderer inside it, literate-programming
  style -- which makes it this project's closest philosophical
  relative at the far end of the scale: everything explained, nothing
  hidden, and about a hundred times bigger than what is planned here.
  **Mitsuba** (Jakob) is its research sibling.
- **Production renderers**: Arnold (Sony Imageworks and beyond),
  RenderMan's RIS, V-Ray, Corona, Octane, and **Cycles**, Blender's
  path tracer (2011) -- the one anybody can read and run. What they
  have that a teaching renderer never will: denoisers, adaptive
  sampling, volumetrics, subsurface scattering, shader graphs,
  out-of-core geometry, and render farms.
- **Kernels rather than renderers**: Intel's **Embree** (CPU, SIMD
  packets), NVIDIA's **OptiX** (GPU), AMD's Radeon ProRender. They do
  exactly the two things this plan's `Ray` and `Bvh` do -- build an
  acceleration structure, intersect rays with it -- and nothing else,
  which is a good reminder of where the real engineering is.
- **Real-time**: DXR/Vulkan RT/Metal in engines; Unreal's Lumen mixes
  hardware rays with distance fields. Denoising is not a detail there:
  at one or two samples per pixel, the denoiser *is* the renderer.
- **POV-Ray** (1991, out of DKBTrace, David Kirk Buck, 1987) deserves
  its own line here, because it is the shape the ICFP 2000 task took:
  a **scene description language** rather than a program, with
  procedural textures written in it. That is exactly what this plan
  deliberately drops (see its Groundwork), keeping the idea -- a
  surface as a function of `(face, u, v)` -- as an ordinary OCaml
  closure.
- **The other intersection method**: Shadertoy-style **SDF ray
  marching** (sphere tracing, John Hart, 1996; Inigo Quilez's articles
  and the demoscene) -- no triangles and no BVH, just a distance
  function stepped along the ray, entirely inside a fragment shader.
  It is how the web gets real-time ray tracing today, and it would be
  a genuinely different, and small, follow-up for this project's WebGL
  backend.

## Part 3: the teaching lineage

- **Whitted's 1980 paper** is eight pages and still the clearest
  statement of the recursive idea.
- **Andrew Glassner (ed.), *An Introduction to Ray Tracing* (Academic
  Press, 1989)** -- the book, and the author's own pick when asked
  which 1980s one to cite (2026-09-20): the field's first, written by
  seven people who had each just invented a piece of it, and with
  enough concrete code to build from. Its table of contents is almost
  this project's module list, which is why it is named in
  `Raytrace.mli` rather than only here:

  | chapter | | ours |
  |---|---|---|
  | 1. An overview of ray tracing | Glassner | `notes_raytracing.md` §1 |
  | 2. Essential ray tracing algorithms | Eric Haines | §2-§5 |
  | 3. A survey of ray-surface intersection algorithms | Pat Hanrahan | `Ray` (§3) |
  | 4. Surface physics for ray tracing | Glassner | `Material` (§7-§8) |
  | 5. Stochastic sampling and distributed ray tracing | Robert Cook | §9 |
  | 6. A survey of ray tracing acceleration techniques | Arvo and Kirk | `Bvh` (§6) |
  | 7. Writing a ray tracer | Paul Heckbert | the whole plan |
  | 8. A ray tracing bibliography | Heckbert and Haines | these references |
  | 9. A ray tracing glossary | Glassner | our glossary |

  It is on the Internet Archive, so it can actually be checked rather
  than remembered.
- **The rest of that shelf**, for context: **David Rogers,
  *Procedural Elements for Computer Graphics* (1985)**, the
  algorithms-with-pseudocode one; **Roy Hall, *Illumination and Color
  in Computer Generated Imagery* (1989)**; **Watkins, Coy and Finlay,
  *Photorealism and Ray Tracing in C* (1992)**, a whole renderer's
  source on a disk; and **Foley and van Dam, *Computer Graphics:
  Principles and Practice*** (1982; the 1990 second edition is the one
  everyone owns). The last has a footnote in this house: the author's
  ICFP 2000 log records finding "cgpp" in a library mid-contest and
  judging it "not really useful" -- the textbooks explain the model,
  and what he needed at 17:00 on day two was the sign of a dot
  product. Which is an argument for the kind of `.mli` this project
  writes.
- **Peter Shirley**: *Realistic Ray Tracing* (2000), and then the free
  ***Ray Tracing in One Weekend*** series (2016-, with Trevor Black
  and Steve Hollasch), which is the modern on-ramp -- a working path
  tracer in a weekend, one idea per chapter. The closest thing in
  spirit to what this plan's tutorial is trying to be, with the
  difference that ours has to fit an existing scene and an existing
  lighting model rather than inventing its own.
- **smallpt** (Kevin Beason, 2007): a path tracer in **99 lines** of
  C++, with a famous annotated slide deck explaining every one. The
  size to measure ourselves against if the optional path-tracing phase
  happens.
- ***Ray Tracing Gems*** I (2019) and II (2021), free from NVIDIA: the
  practical corpus of the RTX era.
- **scratchapixel.com** for the derivations, and **PBRT** (above) as
  the reference when an approximation has to be justified.
- **The ICFP 2000 contest itself is a teaching artifact**, and an
  unusually good one: dozens of complete ray tracers written in 72
  hours, with public write-ups of what each team cut. Tom Rokicki's
  entry was famously asked "A Ray Tracer in 470 Lines of Perl?"; the
  two OCaml teams' notes are in Part 4. There is no better collection
  of evidence about which parts of a ray tracer are load-bearing.

## Part 4: in OCaml (and Elm)

- **ICFP 2000 is OCaml's ray tracing history**, and it is a good one:
  **PLClub** (University of Pennsylvania) took first place and
  **Camls 'R Us** (INRIA Rocquencourt: Sébastien Ailleret, Pascal
  Cuoq, Damien Doligez, Robert Harley, Fabrice Le Fessant, Xavier
  Leroy, Alan Schmitt) second, both in OCaml -- after which the judges
  called OCaml "the superior programming tool of choice for
  discriminating hackers". Camls 'R Us implemented all three tiers and
  published what made it fast: a bounding sphere per object
  (**about 75% of exact intersection tests eliminated**), the scene's
  top-level unions rearranged into a tree, an attenuation cutoff on
  reflected rays, and specialising the surface functions that turn out
  to be constant. Those are phases 3, 4 and 7 of our plan, and they
  are cited there by name.
- **This project's author entered that contest**, and kept the log
  (`~/Dropbox/Downloads/icfp-raytrace-2020/`, misnamed: the contest
  was 2000). It is prior art with the bugs still in it -- the
  unnormalized ray, the ray/plane sign, shadow acne and its epsilon,
  the missing clamp -- which is why the plan's phasing follows its
  `txt/plan.txt` and its tests are named after its `txt/history.txt`.
  Its `bigfib` scene took **123 s in bytecode** on 2000 hardware, no
  recursion and no specular: the scale marker for what "too slow"
  meant then.
- **Since then**, OCaml ray tracers are mostly benchmarks and toys
  (and the language's floating-point performance and GC are still the
  reason it suited the task). No established OCaml renderer to point
  at, as far as I know.
- **In Elm**: none, and there could not usefully be one -- Elm reaches
  the GPU through WebGL, which has no ray tracing API, and a per-pixel
  renderer in Elm itself would be far too slow. elm-3d-scene, which
  `playground3d` follows in spirit, is a rasterizer.
- **In this repository, three things are already ray-ish**, and the
  plan connects to all three: `games2.5d/TinyWolfenstein.ml`'s raycaster (the
  2D grid ancestor: one ray per screen column, DDA),
  `games3d/TinyQuake.ml`'s `light` tool (patches asking every lamp
  whether it can see them -- a *baked* ray caster, run once at
  startup, which is how a rasterizer buys shadows), and
  `physics/3d`'s planned `Collide3d` rays, which share the very same
  Möller-Trumbore routine
  ([`plan_physics3d_teaching.md`](../plans/plan_physics3d_teaching.md)).

## Where `Raytrace` and the playground's "y" key actually sit

Two levels, as everywhere in this project:

- **`graphics/3d/Raytrace`, the renderer**, at the legible end: one
  module per idea (`Ray`, `Bvh`, `Material`, `Raytrace`), the brute
  force kept runnable beside the BVH and required to agree with it,
  the epsilon and the clamp as *named tests* rather than folklore, and
  every `.mli` with its diagram, worked example and paper. Closer to
  smallpt's ambition than to PBRT's, with the difference that it is
  explained rather than compressed.
- **The playground end**: no new vocabulary at all. The same scene,
  the same camera, one key ("y") and one flag (`-raytrace`), plus two
  words (`shiny`, `glassy`) for scenes that want mirrors and glass.
  A reader who never opens `graphics/3d/` gets a better picture by
  pressing a key, which is the whole design.

**The ceiling, stated now**: Whitted-class, not physically based --
triangles plus analytic spheres, one directional sun (area lights and
soft shadows only if the optional phase happens), no global
illumination by default, no denoiser, no volumetrics or caustics, no
GPU, no scene file format, and seconds to minutes a frame at a
resolution measured in hundreds of pixels. It exists to make one
comparison possible on scenes this project already has, not to render
anybody's film.

## Postscript: the numbers (to come)

Once built: rays per second, brute force against the BVH, on the ICFP
`spheres` scene and on `games3d/TinyQuake.ml`'s level; the frame time
at 1 and 4 samples per pixel against the rasterizer's milliseconds on
the identical frame; the rays saved by the attenuation cutoff; the
line count of `graphics/3d/Raytrace` and friends against smallpt's 99
and the ICFP entries'; and, for fun, our 320x240 `spheres` render
against the author's own 2000-era entry on the same scene.

Sources: from memory unless linked above, and to be checked before
relying on them for teaching -- Wikipedia's ray tracing article for
the Appel/MAGI/Whitted/Monster House dates, the ICFP 2000 contest
pages and Camls 'R Us's write-up for the contest facts, and the
author's own entry for everything in Part 4's second bullet. The
post-2018 hardware dates and the *Tron* attribution are the least
verified here.
