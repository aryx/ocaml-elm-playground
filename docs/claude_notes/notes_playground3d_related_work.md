# playground3d/ vs. the rest of the 3D graphics world

You asked for a survey of related work: the small cluster of Elm "3D
playground" experiments this project's design was mined from (already
summarized in `notes_3d.md` §10 and `plan_playground3d.md`), plus the
bigger picture -- VRML, OpenGL, WebGL, Vulkan -- and where `playground3d/`
sits relative to all of it. The through-line, worth stating up front:
**every one of the industry APIs below is designed to be as fast and as
capable as possible; `playground3d/` is designed to be as *legible* as
possible.** Those are different, largely incompatible goals, and almost
every difference described here follows directly from which one a given
project picked. (The 2D twin of this note, for `playground/`:
[`notes_playground_related_work.md`](notes_playground_related_work.md).)

## The one-line version

| | What it optimizes for | What you can see happening |
|---|---|---|
| VRML/X3D | Describing a static-ish 3D scene declaratively, like HTML for 3D | A file format, not code -- nothing to "see happen" at all |
| OpenGL / WebGL | Real-time performance via dedicated GPU hardware | Nothing below the API call -- rasterization and blending happen inside the GPU, opaque by design |
| Vulkan | Maximum performance and explicit control, at the cost of verbosity | Even less than OpenGL -- you now also manage the GPU's memory/scheduling yourself, none of which is about pixels |
| Unity | Shipping a full game via an editor + engine, not writing a renderer | Least of all -- your C# script talks to an object model, which talks to a render pipeline, which talks to a shader, which talks to whichever of Direct3D/Metal/Vulkan/OpenGL the platform picked |
| elm-3d-scene / nateabele's elm-3d-playground | A good WebGL-backed *result* with a friendly API on top | Same as WebGL underneath -- the friendliness is a wrapper, not a change in what's hidden |
| lucamug's elm-playground-3d | Extending elm-playground's *existing* 2D renderer with zero new rendering code | Everything -- it's plain SVG shapes, the same as any 2D elm-playground picture |
| `playground3d/` | Every number that produces a pixel being readable in one file | Everything -- see below |

## Part 1: the Elm "3D playground" lineage

These are the projects actually consulted while designing this library
(`plan_playground3d.md`'s "Prior art" section); this section gives the
fuller picture that plan only sketched.

### lucamug/elm-playground-3d -- the direct ancestor

Already covered in detail in `notes_3d.md` §10 (a side-by-side table)
and credited in `README-3d.md`. The one-sentence summary for context
here: it adds 3D shapes to Evan Czaplicki's original 2D elm-playground
by projecting every 3D point down to a 2D one and handing the result to
elm-playground's *existing*, unmodified SVG renderer -- no WebGL, no
canvas, no new rendering code at all. This is also its biggest
limitation (no backface culling, no depth sorting -- see `notes_3d.md`
§5-6), and exactly the trick this project's *web* backend reuses and
extends (`render3d_to_2d` adds the culling and depth-sort lucamug's
version lacks; see `Playground3d.mli`'s design-credit comment).

### erkal/elm-3d-playground-exploration -- a sibling experiment

Erkal Selman's "experimental playground for game development with
Elm" -- explored around the same time as (and in the same general
orbit as) `ianmackenzie/elm-3d-scene`, which its author has also used
directly for creative-coding/visualization work. Less
formalized/documented than the other three projects here (no
stable public API contract the way lucamug's or nateabele's have), so
it's mentioned here mainly to be complete about what was surveyed,
rather than as a design influence on any specific `playground3d/`
decision.

### ianmackenzie/elm-3d-scene -- the "real engine" end of the Elm spectrum

A full, WebGL-backed 3D rendering library (via
`elm-explorations/webgl` underneath), explicitly aiming to make "3D
graphics as easy and enjoyable as possible, without having to worry
about low-level details like shader programs and transformation
matrices" -- lighting (multiple lights, HDR), shadows, physically-based
materials, exposure/tone mapping. This is a legitimate, capable
rendering engine, not a toy: the kind of shadows and per-pixel Phong
lighting `notes_3d_shading.md`'s roadmap describes as "large" or
"expensive" additions are things elm-3d-scene already does well, for
free, because the GPU does them. The cost, from this project's
perspective: all of that happens inside WebGL's shader pipeline, which
means it happens exactly like every other WebGL/OpenGL program's
lighting does (see Part 2) -- not in readable Elm code you could open
and step through.

### nateabele/elm-3d-playground -- an elm-playground *skin* on elm-3d-scene

Wraps elm-3d-scene in an API that deliberately mirrors elm-playground's
shape (`sphere`/`box`/`group`, `move`/`rotate`, a `Computer` parameter
for keyboard/mouse, explicitly aimed at making 3D approachable for
students). This is the closest thing to `playground3d/`'s own ambition
that already existed -- "Evan-light API, but for 3D" -- and it's the
project `games3d/StarCollector3d.ml`'s mechanics (not code) were
adapted from (see that file's header comment). The difference is
exactly the one-line summary at the top of this doc: nateabele's
project gets the *API ergonomics* of elm-playground while keeping
elm-3d-scene's real WebGL rendering underneath (so it's fast and
capable, but a student still can't read "how did this triangle become
these pixels" without learning WebGL/GLSL); `playground3d/` instead
keeps the *rendering* fully readable, at the cost of leaving all of
elm-3d-scene's realism on the table.

## Part 2: the industry APIs -- VRML, OpenGL, WebGL, Vulkan

None of these were mined for `playground3d/`'s design the way the Elm
projects above were -- they're included because understanding roughly
what they are, and roughly how much machinery sits between "your code"
and "a pixel" in each one, is what makes it possible to see just how
unusual `playground3d/`'s choice (no machinery at all) really is.

### VRML/X3D (1994, standardized 1997) -- a file format, not an API

VRML ("Virtual Reality Modeling Language") was an attempt to make 3D
scenes as embeddable in the web as HTML made 2D documents: a text file
describing a scene *graph* -- nodes for shapes, transforms, lights,
materials, and (via `Script` nodes and a `ROUTE` event-wiring system) a
little interactivity -- that a browser plug-in would parse and render.
X3D is its modern, XML-based successor. The relevant point of
comparison: VRML isn't really something you *program against* the way
every other entry in this table is -- there's no per-pixel loop to
reason about at all, because you never write one; you describe a scene
declaratively and the viewer's own (typically OpenGL-based, entirely
hidden) renderer does 100% of the work of turning it into pixels. In
that sense it's the opposite extreme from `playground3d/`: maximal
declarativeness, zero visibility into the rendering, whereas
`playground3d/` is maximal visibility with a comparably tiny
declarative surface (`shape3d`/`form3d`, `Group3d` for a scene graph --
this project's one real structural echo of VRML's scene-graph idea).

### OpenGL (1992-) -- the baseline GPU API

The industry-standard cross-platform API for talking to a GPU. Two very
different eras worth distinguishing:

- **Fixed-function OpenGL** (roughly 1.0-2.0, i.e. the classic "red
  book" era): you called functions like `glBegin(GL_TRIANGLES)`,
  `glVertex3f`, `glColor3f`, `glEnd`, and pushed/popped matrices onto a
  built-in matrix stack (`glTranslatef`, `glRotatef`) -- genuinely not
  that far in spirit from `playground3d/`'s own `move3d`/`rotate3d`
  combinators, actually, since the lighting/transform/rasterization
  math was all fixed, built-in behavior you invoked rather than wrote.
  The difference: it was still the *GPU's own dedicated hardware*
  doing the rasterization and lighting math per vertex/pixel, entirely
  opaque, whereas here every one of those steps (`Triangle.fill`'s edge
  functions, `Lighting.brightness_of_normal`'s dot product) is OCaml
  you're reading right now.
- **Programmable/"core" OpenGL** (2.0+, mandatory from 3.2 on): fixed
  functions were removed; instead you upload vertex data into GPU
  buffers (VBOs/VAOs) and write your own small GPU programs
  ("shaders", in the C-like **GLSL** language) that the GPU compiles
  and runs -- a *vertex shader* once per vertex, a *fragment shader*
  once per pixel, across thousands of GPU cores in parallel. This is
  what actually makes real-time Phong shading, shadow maps, and
  reflections practical at 60+ fps on large scenes -- `notes_3d_shading.md`'s
  "Phong: moves real cost into the hot path" caveat is exactly the
  problem GPU parallelism was built to make disappear. The cost: you
  now need a second programming language (GLSL) plus a fair amount of
  host-side bookkeeping (compiling/linking shader programs, describing
  buffer layouts, managing GPU state) just to draw a single triangle.

### WebGL (2011-) -- OpenGL ES, in the browser

Essentially "OpenGL ES 2.0/3.0, exposed to JavaScript" -- same
programmable-shader mental model as core OpenGL above, reachable from
a `<canvas>` element with no native toolchain. This is what
`elm-explorations/webgl`, and therefore `ianmackenzie/elm-3d-scene` and
everything built on it (nateabele's wrapper included), actually runs
on. From a teaching standpoint it inherits core OpenGL's whole
complexity budget (buffers, GLSL, a projection/view/model matrix stack
usually pulled in from a separate linear-algebra package) -- elm-3d-
scene's stated goal of hiding exactly that complexity from its users is
precisely the value it adds, and precisely the reason using it means
trusting a black box for the actual rendering, rather than reading it.

### Vulkan (2016-) -- OpenGL's low-level successor

Designed explicitly to fix what became OpenGL's biggest weakness at
scale: OpenGL hides a lot of GPU state and scheduling behind a
convenient but somewhat unpredictable driver, which becomes a
performance liability for large, professional game engines that want
precise, multi-threaded control over GPU work submission. Vulkan
exposes almost everything explicitly instead -- command buffers built
up and submitted by hand, explicit pipeline state objects, manual GPU
memory allocation, explicit synchronization primitives (fences,
semaphores) between GPU and CPU work. The trade-off is stark and
well-known in the industry: a Vulkan "hello triangle" is commonly
800-1500+ lines of C++ boilerplate before a single pixel is drawn,
against roughly a hundred for the equivalent in classic OpenGL. Vulkan
is, deliberately, not aimed at beginners or at readability at all --
it's the closest thing in this table to "assembly language for the
GPU," a ceiling for engine authors who have already outgrown OpenGL's
abstractions and need to hand-tune every millisecond, the polar
opposite motivation from this project's.

### Unity (2005-) -- a full engine and editor built on top of all that

Everything in this table so far is an *API* -- something you write
code against directly. Unity is a level up again: a complete game
*engine and visual editor*, where a scene is built largely by dragging
objects and components around a GUI, physics/animation/audio/asset
pipelines all come bundled, and gameplay logic is scripted in C#
against Unity's own high-level object model (`GameObject`,
`Transform`, `MonoBehaviour`) -- you essentially never touch a graphics
API directly at all. Under the hood, Unity's own renderer targets
whichever low-level graphics API fits the platform (Direct3D, Metal,
Vulkan, or OpenGL/OpenGL ES) and picks among several bundled rendering
pipelines (e.g. its "Universal" and "High-Definition" render
pipelines) for you. This makes it the practical, industry-default
choice for actually shipping a 3D game -- and also the furthest thing
in this document from "read every line that produces a pixel": between
your gameplay script and a pixel sit the chosen render pipeline, a
shader (often visually authored via Unity's node-based Shader Graph,
itself compiling down to HLSL), and finally whichever of Direct3D/
Metal/Vulkan/OpenGL that shader targets on a given machine -- several
more opaque layers than even elm-3d-scene's single WebGL layer.
`playground3d/`'s entire native rendering path, by contrast, is
shorter than the list of layers Unity has *before* it reaches a
graphics API at all.

## Where `playground3d/` actually sits

Put concretely: `playground3d/`'s native backend uses **no GPU API of
any kind** -- not fixed-function OpenGL, not programmable OpenGL/WebGL,
not Vulkan. `Playground3d_platform.ml` opens a raw SDL window and pixel
buffer, and `graphics/3d/` does every step by hand in ordinary OCaml,
one module per step: the camera/projection math (`Camera`, `Project`),
the rasterization (`Triangle`'s edge-function/barycentric test, §7 of
`notes_3d.md`), the depth test (`Zbuffer`, §6), clipping (`Clip`), and
the shading (`Shading`, `notes_3d_shading.md`) -- the exact same category of
work a GPU's fixed-function hardware or a GLSL shader would otherwise
do invisibly. The web backend doesn't use WebGL either -- it compiles
the 3D scene down to plain 2D SVG shapes (`render3d_to_2d`), reusing
elm-playground's original 2D renderer unchanged, the same trick as
lucamug's project.

This is a real ceiling, not an oversight: everything elm-3d-scene gets
essentially for free (real shadows, dozens of dynamic lights, textured
PBR materials, thousands of triangles at 60fps) would be a large,
qualitatively different undertaking here, likely requiring exactly the
kind of GPU API this section describes. That trade was made on
purpose, for the same reason Evan Czaplicki's original 2D
`elm-playground` doesn't wrap an existing charting/canvas library
either: **`playground3d/` is a teaching context first.** The entire
point is that a curious reader can open `Playground3d.ml` and
`graphics/3d/` and trace *every* number
that ends up as a pixel's color -- no GLSL, no driver, no scene-graph
file format, no hidden fixed-function hardware -- with an API small
enough (`box`/`cube`/`plane`/`sphere`, `move3d`/`rotate3d`/`scale3d`,
`camera`, `game3d`) to hold in your head next to the 2D `Playground`
API it mirrors. Every one of the industry systems in Part 2 solved a
real, harder problem (production-quality real-time graphics at scale)
that this project isn't trying to solve; every one of the Elm projects
in Part 1 sits somewhere on the spectrum between "readable but
limited" (lucamug) and "capable but opaque" (elm-3d-scene), and
`playground3d/` deliberately picked the readable end and pushed it
further, into a real (if modest) hand-written rasterizer instead of
lucamug's flatten-to-2D-SVG trick alone.

## Postscript: a real GPU backend now exists -- the actual numbers

Everything above was written when `playground3d/` had exactly the two
backends this doc contrasts against the industry (native, no GPU at
all; web, an SVG projection). `docs/claude_notes/plan_opengl.md`
followed up on this doc's own comparison table by actually building
the OpenGL end of the spectrum too (`elm_playground_3d_opengl`) --
closing this doc out with the measured answer, not just the
expectation, to "how much shorter is the code, and how much faster is
it" once a real GPU does the work this project otherwise hand-rolls.

**Code size** (at the time, before the rasterizer moved to
`graphics/3d/`): `playground3d/software/Playground3d_platform.ml` was 934
lines (377 non-comment/non-blank) versus `playground3d/opengl/Playground3d_platform.ml`'s
562 lines (295 non-comment/non-blank) -- roughly 40% shorter by raw
line count, about 22% shorter by actual code once both files' (this
project writes a lot of prose explaining *why*) comments are excluded.
Less dramatic than "the GPU does the rasterizer/z-buffer/culling for
you" might suggest, for a real reason: a meaningful share of what's
*left* in the OpenGL file is now bookkeeping the software rasterizer
never needed at all -- shader compilation/link-error checking, a
hand-rolled `Mat4`, and (once textures were added) a small
material-grouping/GPU-texture-cache layer, since a single GPU draw
call can only bind one texture at a time. The GPU doesn't just delete
code, it also demands a few new kinds of code the CPU path never had
to write.

**Frame rate**, measured the same way as `notes_3d_opti.md` (uncapped
-- `Native_loop`'s 60fps sleep temporarily removed -- with a
`Printf.eprintf` of the per-frame fps, on this machine's real GPU, an
NVIDIA RTX A400):

| Scene | native (software rasterizer) | opengl |
|---|---|---|
| `Cubes3d.exe` as shipped (25 cubes, 300 triangles) | ~23 fps | ~750-840 fps |
| Same scene, `grid_size` bumped to 25 (625 cubes, 7500 triangles) | ~6 fps | ~37-40 fps |

Two things worth being honest about rather than just quoting "33x
faster, then only 6x faster" as if that were the whole story:

1. **The GPU's advantage shrinks sharply as the scene grows** (33x at
   300 triangles, only ~6x at 7500) precisely because of the "Scope for
   v1" simplification stated up front in `plan_opengl.md`: the entire
   scene's vertex data is rebuilt from scratch in OCaml
   (`group_by_material`/`collect_batches`, list-heavy, one boxed tuple
   per vertex) and re-uploaded every single frame, with no per-shape
   GPU-side caching across frames at all. At larger triangle counts,
   this OCaml-side rebuild -- not the GPU's own rasterization/shading,
   which really is close to free at this scale -- becomes the
   bottleneck. This is exactly the kind of thing `notes_3d_opti.md`
   would say to measure before optimizing, rather than assume: caching
   per-shape buffers across frames (only re-uploading when a shape
   actually changes) is the obvious next step if this backend's scenes
   ever need to grow past a few thousand triangles, but wasn't worth
   building before this number existed.
2. **The absolute numbers are specific to this measurement's
   environment** (a remote, possibly software-composited X11 display)
   -- native's own ~23fps *at just 300 triangles, uncapped* is
   suspiciously low for a scene this small, and is at least partly
   explained by `Sdl.update_window_surface`'s full-frame pixel blit
   (unrelated to triangle count) rather than the rasterizer's own
   per-triangle work, whereas the GL path presents via
   `Sdl.gl_swap_window`, a different code path entirely. Re-running
   this same comparison on a different machine/display could well give
   different absolute fps numbers for both backends -- the *relative*
   story (GPU wins by a lot at small scenes, by much less once the
   naive per-frame OCaml rebuild dominates) is the more trustworthy
   takeaway here than either raw number in isolation.

Sources consulted for Part 1/2's factual claims: the READMEs of
[lucamug/elm-playground-3d](https://github.com/lucamug/elm-playground-3d),
[erkal/elm-3d-playground-exploration](https://github.com/erkal/elm-3d-playground-exploration),
[ianmackenzie/elm-3d-scene](https://github.com/ianmackenzie/elm-3d-scene),
and [nateabele/elm-3d-playground](https://github.com/nateabele/elm-3d-playground);
general, widely-documented industry knowledge of VRML/X3D, OpenGL's
fixed-function-vs-programmable history, WebGL, Vulkan's design goals
versus OpenGL's, and Unity's editor/engine/render-pipeline architecture.
