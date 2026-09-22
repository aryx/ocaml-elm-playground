# Plan: seeing the machine while it runs (`playground/Inspect`)

## Context

Bret Victor's "Inventing on Principle" (CUSEC, January 2012) states one
principle: **creators need an immediate connection to what they are
making**. Its most-quoted demo is a platformer whose author scrubs time
backwards and forwards and sees the character's whole trajectory drawn
across the level -- where the jump *came from* and where it is *going*
-- and changes a number to watch the arc move. The point is not the
debugger; it is that a thing you can only run is a thing you cannot
think about.

This project is unusually well placed to do that, for two reasons it
did not plan for:

- **The architecture.** A playground app is
  `init / update / view / subscriptions` over a model that is a value
  (`Playground.app`). `update` is a pure function of the `computer`
  (mouse, keyboard, time, screen, flags) and the model, so a run *is*
  its inputs: record them and the whole thing replays. Elm shipped
  exactly this debugger in 0.18; here it is the same idea in OCaml.
- **The teaching libraries.** `graphics/`, `physics/`, `audio/` and
  `ai/` each compute things worth *looking at*, and one of them
  already proves the shape works: `playground/Audio_debug` draws an
  oscilloscope and a spectrum over the frame ("v" with `-debug-keys`),
  as ordinary Playground shapes, so any backend can show them. The
  same trick is waiting for contacts and broad-phase grids, for
  A*'s frontier and a network's weights, for overdraw and triangle
  counts.

What exists today, and how scattered it is -- which is half the reason
for this plan:

| what | where | how it is reached |
|---|---|---|
| oscilloscope, spectrum | `playground/Audio_debug` | "v", `-debug-keys` |
| hitboxes, velocity arrows | `Physics.debug` | each game's own `hitboxes=1` flag |
| broad-phase counts | `Physics.broad_phase` | printed by `examples/PhysicsMarbles.ml` only |
| contacts, normals, the grid, the integrator | *planned, never done* | `plan_physics_remaining.md` §2 |
| frontier, flow field, steering forces, weights | *planned* | `plan_ai_teaching.md`'s `Ai_debug` |
| shading, culling, clipping, wireframe, Opti | the backends' keys | "b f i n o t z r", "m p c x" in 3D |
| the magnifier, the help overlay, pixelation | `playground/software/` | "z", "h", "r" |
| the frame rate | `Native_loop_2d`'s `Fps` | `-debug` |

Every one of those is a different mechanism reached a different way,
none of them survives into the web backend, and none of them can look
one frame *backwards*.

This plan absorbs the sketch at
[`plan_teaching_other.md`](plan_teaching_other.md) §3 ("A time-travel
debugger"), which is replaced by a pointer here, per
[`../guide-principles.md`](../guide-principles.md).

Companions: [`notes_inspect.md`](../tutorials/notes_inspect.md), the
tutorial (written ahead of the code, as its specification), and
[`notes_inspect_related_work.md`](../related-work/notes_inspect_related_work.md)
(Victor, Elm's debugger, Redux DevTools, `rr`, RenderDoc, and the
ceiling here).

## Principles

The eight of [`../README.md`](../README.md), with four of this area's
own:

- **Overlays are ordinary shapes.** `Audio_debug`'s rule, made
  general: a panel returns `Playground.shape list` (or `shape3d` via
  `hud`), so it draws on the software backend, OpenGL, WebGL and the
  web one alike, and golden frames can test it.
- **Nothing in the frame loop when it is off.** Recording is off by
  default; with it on, the cost is a `computer` copied per frame and
  a model kept every N frames, and the plan *measures* that rather
  than asserting it is small.
- **The game does not change.** The inspector wraps an `app`
  (`Inspect.wrap`), the way `Bigbang`, `Logo` and `Puzzlescript`
  already build one for you; a game gets pause, rewind and the panels
  without a line added, and opts in to nothing but a trail.
- **Determinism is the prerequisite, not a nice-to-have.** Replay is
  only correct if `update` is a pure function of the `computer`: no
  wall clock read behind the playground's back, no global `Random`
  (`plan_playground_other.md` §1's seeds), no effect that cannot be
  suppressed on a replayed frame. The plan's phase 0 is that, and the
  work doubles as what golden frames and lockstep networking need.

## The Playground API, Evan-style

Tentative (`playground/Inspect.mli`), to be settled by inspecting two
real games with it. The design goal is that **a game that wants
nothing does nothing**:

```ocaml
(* wrap an app: the inspector's keys, timeline and panels, over it.
   Done by the backends when -inspect is on, so games never call it *)
val wrap : ?show:('model -> string) -> ('model, 'msg) app -> ('model, 'msg) app

(* the panels, one per engine; cycled with one key *)
type panel = Off | Timeline | Physics | Graphics | Audio | Ai
val next_panel : panel -> panel
val shapes : panel -> Playground.screen -> Playground.shape list

(* what a game may opt into: Victor's trail. [where] says where the
   thing is in a given model; the past comes from the recording, the
   future from running [update] forward from now *)
val trail :
  past:int -> future:int -> ('model -> Playground.number * Playground.number) ->
  'model -> Playground.shape

(* what an engine's *_debug module provides, the Audio_debug shape:
   record what happened this frame, draw it when asked *)
```

`?show` is the one place OCaml is plainly worse than Elm, and the plan
says so rather than hiding it: Elm's debugger can print *any* model
because the runtime keeps the types; OCaml erases them, so a model is
shown only if the game hands over a printer. The timeline works
without one -- it shows the frame number, the inputs and the picture,
which is most of what anyone scrubs for.

Keys: the inspector is a **mode**, not another letter (both backends
have nearly run out). `-inspect` (or "g" with `-debug-keys`) opens it;
while it is open, space pauses, left/right step a frame, shift+arrow
steps ten, "[" and "]" jump a second, "p" cycles the panel, "t"
toggles the trail, and "g" closes it. The game's own keys are ignored
while paused, and fed to `update` again while stepping.

## Target layout

```
playground/
  Inspect.ml/.mli        the wrapper: the recording, the timeline, the
                         panel mechanism, the trail
  Audio_debug.ml         (exists) becomes the Audio panel, unchanged
  Physics_debug.ml       NEW: hitboxes and velocities (moved from
                         Physics.debug), contacts and normals, the
                         broad phase's grid and counts, the solver's
                         impulses, the integrator in use
  Ai_debug.ml            NEW (plan_ai_teaching.md's): frontier, flow
                         field, steering forces, states, search values
  Graphics_debug.ml      NEW: the frame's numbers -- shapes drawn,
                         triangles, pixels touched, overdraw, the time
                         each stage took; 2D and 3D
playground/native_common/
  Native_loop_2d.ml      (exists) -inspect, and the wrap
  Native_loop_3d.ml      (exists) the same, panels drawn through hud
playground/native/, software/, web/, svg/
                         the same flag, the same shapes
```

No new top-level directory: this is a *layer on the playground*, like
`Camera2d` and `Scene2d`, not a teaching library of algorithms. The
per-engine `*_debug` modules live beside the layer they draw, and each
depends on its engine (`physics_2d`, `ai`, `audio`), never the other
way round.

## Groundwork decisions

### What is recorded: the inputs, and a model now and then

A frame's input is the `computer` record -- mouse, keyboard, screen,
time, flags -- a handful of floats and a small key set. **Record
those, not the models**: the model is whatever the game invented, it
can be large, and it cannot be copied generically anyway. Replay is
then `update` over the recorded inputs, which is exactly what makes
replay a *test* of determinism rather than a snapshot viewer.

Scrubbing to frame 3,000 by replaying 3,000 frames is too slow, so
**keep a model every N frames** (a keyframe, N = 60 by default: at
most one second of replay to reach any frame). Models are immutable
values in OCaml, so a keyframe costs a pointer, and structure is
shared with its successors -- the cheapest part of the whole design,
and a nice thing to explain.

### Effects on a replayed frame

`update` returns a `Cmd`, and a replayed frame must not re-fire it:
the audio plan already names this hazard --
`plan_audio_teaching.md` and `Audio.mli` both say an `update` run
twice for a frame "plays its sound twice". So a replayed frame runs in
**silent mode**: commands are computed (they are part of the model's
transition) but dropped, and the panel shows what was dropped, which
is itself a good picture of what a frame *did*. HTTP and image loads
are already cached by the backends; the plan checks that rather than
assumes it.

### Determinism first (phase 0)

`Random.self_init` in Snake, Tetris, Asteroid and StarCollector3d
(`plan_playground_other.md` §1) makes those games unreplayable, and
`computer.time` comes from the wall clock. The fix is that plan's
seed, and recording the time *as an input* rather than reading it
during replay. Everything else here depends on it, and so do golden
frames and any future lockstep networking -- which is why it is phase
0 rather than a caveat.

### One mechanism for the panels

Each engine keeps a module of the `Audio_debug` shape: `record`
(called by the engine or the platform, cheap when off) and `shapes`
(the drawing, on demand). `Inspect` owns the key, the layout and the
cycling, so panels look the same in 2D and 3D and a new engine's panel
is one module and one line. `Physics.debug` and the games' `hitboxes`
flags stay working; they become thin wrappers over `Physics_debug`.

## The modules, with their references

- **Inspect**: Bret Victor, "Inventing on Principle" (CUSEC, 2012) --
  the principle, the scrubbed platformer, the trail; and "Learnable
  Programming" (2012), whose "make the data visible" argument is what
  the panels are for. **Elm's time-travel debugger** (elm-reactor,
  2014; shipped as `elm make --debug` in 0.18, 2016), whose import and
  export of a recorded history is phase 5 here. (Dates from memory, to
  check.)
- **The recording**: Doom's and Quake's *demos* (1993, 1996) -- a
  recorded game is its inputs, which is also why a demo desynchronises
  the moment the simulation stops being deterministic; GGPO-style
  rollback (2006) for the save/restore discipline, shared with
  [`plan_networking_teaching.md`](plan_networking_teaching.md).
- **The panels**: Box2D's `b2Draw` and Bullet's debug drawer (every
  physics engine ships one, and for the same reason); RenderDoc
  (Baldur Karlsson, 2013) and PIX for what a *frame* inspector shows.
- **Trails**: Victor's platformer; and, already in this repository,
  `games/TinySlingshot.ml`, which draws its aim arc by stepping the
  physics engine 60 times and plotting the result -- Victor's "see the
  future" done by hand, by a game, before this plan existed.

## New examples

- `examples/InspectTrail.ml`: a ball thrown across the screen, its
  past as fading dots and its future as hollow ones, the timeline
  under it. Scrub, and the two halves swap. The smallest possible
  statement of the whole idea.
- `examples/InspectPanels.ml`: one scene that turns on every panel in
  turn -- physics contacts, the broad-phase grid, the audio
  oscilloscope, A*'s frontier, the graphics counters -- so the panels
  can be compared, and golden-framed, in one place.
- **Existing games, no edit**: `games/TinySlingshot.ml` (whose own arc
  and the inspector's future-trail should coincide exactly -- a good
  test), `games/TinyPong.ml`, `games3d/TinyMario64.ml`.

## Phasing

0. **Determinism** (`plan_playground_other.md` §1): seeds in the
   `computer`, `-seed`, the four games that call `Random.self_init`
   converted, and `computer.time` recorded as an input. Test: a game
   run twice from the same seed gives byte-identical golden frames.
1. **The panel mechanism**: `Inspect.panel`, the key, the layout;
   `Audio_debug` moved under it unchanged (the proof that the shape
   generalises); `Physics_debug` with what
   `plan_physics_remaining.md` §2 has been waiting for (vectors,
   contacts and normals, the broad-phase grid, the solver's
   impulses).
2. **The rest of the panels**: `Graphics_debug` (shapes, triangles,
   pixels, overdraw, per-stage times, in 2D and 3D) and `Ai_debug`
   (`plan_ai_teaching.md`'s list). `examples/InspectPanels.ml`.
3. **Recording and pausing**: the ring of `computer`s, keyframes every
   60, `-inspect`, the timeline bar, space and the arrows. Measured:
   bytes per frame, milliseconds per frame, the longest run that fits
   in a chosen budget.
4. **Rewind and scrub**: replay from the nearest keyframe, commands
   suppressed and shown, the "replaying" badge. The determinism test
   becomes continuous: scrubbing back and forward must land on the
   same pixels.
5. **Trails**: `Inspect.trail`, past from the recording and future by
   running `update` forward; `examples/InspectTrail.ml`;
   TinySlingshot's own arc compared against it.
6. **A run as a test**: export the recording as `tests/2d/`'s
   machinery -- the inputs as a `-script`, the frame as a golden PNG.
   This is the feature that pays for the whole plan in a teaching
   repository: a student who finds a bug can hand over the run.
7. **The web backend**: the same flag (`?inspect`), the same shapes;
   the timeline as shapes rather than DOM, so there is one
   implementation.
8. **Docs**: `notes_inspect.md` checked against the code and its
   numbers filled in; the related-work note's postscript.

## Later, and what each would cost

Named here because they are the rest of Victor's talk, and because the
first thing a reader will ask is why they are not in the phases. The
order is by cost:

- **Tweakables** -- a number a game registers (`Inspect.tweak
  "gravity" 800.`) that the panel can slide while the game runs
  (the slider itself comes from
  [`plan_gui_teaching.md`](plan_gui_teaching.md), whose first users
  are this plan's timeline and panels), so
  the jump arc changes under your hand. This is Victor's actual demo
  and it is *cheap* (a registry, a slider, a lookup), but it adds API
  a game must adopt, and it interacts with replay (a tweak is an
  input too, and must be recorded). Worth doing right after phase 5,
  and deliberately not before: the recording has to exist first, or
  the tweak cannot be replayed. Processing's "tweak mode" (2015) is
  the closest existing thing.
- **Richer trails** -- not just "where was it" but any value over the
  whole run: the ball's height as a curve under the frame, the
  model's fields plotted, two runs overlaid. This is Victor's other
  essay, "Up and Down the Ladder of Abstraction" (2011), and it is a
  small step from phase 5's recording plus a `('model -> number)`
  argument. The reason it is later is that it wants a chart layer the
  playground does not have.
- **Liveness -- editing the code and seeing it now** -- the demo the
  talk is remembered for, and by far the most expensive. It needs the
  OCaml toplevel compiled to JavaScript in the browser
  ([`plan_teaching_other.md`](plan_teaching_other.md) §2's editor) or
  `Dynlink` and a hot-reloaded game module natively; plus a story for
  the model when its *type* changes under a running program (Elm's
  debugger keeps the model across a reload only while the type is
  unchanged). The honest framing: our replay already gives the *other*
  half of liveness -- change the code, rebuild, and re-run the same
  recorded inputs to see whether the bug moved -- which is the
  workflow this plan can deliver without a toplevel at all, and phase
  6's export is what makes it usable.

## Status

**Not started** (2026-09-20). Written as the specification, with
[`notes_inspect.md`](../tutorials/notes_inspect.md) beside it.
Decisions taken, with their reasons:

- **the area is called `inspect`** (the author's pick, 2026-09-20,
  from `inspect` / `seeing` / `tools` / `liveness`): it names both
  halves, inspecting a frame and inspecting the past, and works as a
  module name;
- **scope: what the Elm architecture gives nearly free first** (the
  author's call) -- the panels and the recorded, replayable run --
  with tweakables, richer trails and true liveness described in
  "Later, and what each would cost" rather than phased;
- **inputs are recorded, not models**, with keyframes for scrubbing;
- **the inspector wraps the app**, so games need no edit;
- this plan **absorbs** `plan_teaching_other.md` §3, which becomes a
  pointer.

## Verification

- **Replay is the test.** A recorded run, replayed, must produce the
  same model and the same pixels at every frame; scrubbing backwards
  and forwards to the same frame must too. That single property test
  covers the recording, the keyframes, the command suppression and
  the engines' determinism at once, and it is run on every game that
  has golden frames.
- Golden frames of each panel (`examples/InspectPanels.ml`), which is
  also how the panels stay drawn correctly on every backend.
- The measured cost: bytes and milliseconds per recorded frame, and
  the same with recording off (which must be zero, not "small").
- By hand, once, on a real bug: find one in a game, export the run,
  fix the code, replay the run. If that does not feel better than
  printing, the plan failed.

## Out of scope

- Live code editing and the browser toplevel (above).
- Profilers, flamegraphs and allocation tracking: that is
  [`dev/notes_debugging_techniques.md`](../dev/notes_debugging_techniques.md)'s
  territory, and the shell's.
- GPU frame capture (RenderDoc and friends): our GPU backends are thin
  adapters, and the interesting numbers are in the software one.
- Editing the model by hand in the inspector (Elm's debugger cannot
  either, and it would break replay's meaning).
- Recording hours of play, or saving recordings to disk beyond phase
  6's exported test.
