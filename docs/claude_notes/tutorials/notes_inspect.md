# Seeing a running program, from scratch: a tutorial for `playground/Inspect`

How a game engine shows you what it is doing -- the overlays that draw
an engine's own thinking, and the recorded run you can pause, step,
rewind and scrub -- and why the Elm architecture makes the second one
nearly free while OCaml makes one part of it harder than in Elm.

It is the specification of the layer planned in
[`plan_inspect_teaching.md`](../plans/plan_inspect_teaching.md):
written before the code, to be checked against it and have its numbers
filled in. Companions:
[`notes_inspect_related_work.md`](../related-work/notes_inspect_related_work.md)
(Victor, Elm's debugger, Redux DevTools, `rr`, RenderDoc), and
[`../dev/notes_debugging_techniques.md`](../dev/notes_debugging_techniques.md),
which is the *other* kind of debugging -- shell-side, offscreen
frames, scripted keys, headless runs -- and stays that way.

## 0. Where the code is, and a reading order

| module | what | section |
|---|---|---|
| `playground/Inspect` | the wrapper: recording, timeline, panels, trails | §3-§7 |
| `playground/apis/Audio_debug` (exists) | the oscilloscope and the spectrum | §7 |
| `playground/Physics_debug` | hitboxes, contacts, normals, the broad phase, impulses | §7 |
| `playground/Graphics_debug` | the frame's numbers: shapes, triangles, overdraw, times | §7 |
| `playground/apis/Ai_debug` | the frontier, flow fields, steering forces, search values | §7 |
| `playground/Playground` (exists) | `app`: `init`, `update`, `view` -- why any of this works | §2 |

Read §1-§2 for why this is possible at all, §3-§6 for the recorded
run, §7 for the per-engine panels, §8 for a run as a test, and §9-§10
for how it compares with the real tools and what is deliberately
missing.

## 1. The principle

Bret Victor's "Inventing on Principle" (2012) is one sentence and four
demos: **creators need an immediate connection to what they are
making.** The demo that matters here is the platformer -- he drags a
time slider and the character's path is drawn across the level, the
whole jump visible at once, past *and* future; he changes a constant
and the arc moves under his hand.

What is being attacked is a specific poverty: a running program shows
you one frame, the current one, and everything else -- how it got
here, where it is going, what it considered -- is invisible, so you
reason about it from memory and print statements. Every technique in
this note is a way of putting one of those things on the screen.

A game engine is the easiest possible place to try, for a reason worth
saying: **it already draws.** There is a frame, a coordinate system,
and a rendering path that takes shapes. An overlay is not a new
subsystem; it is more shapes.

## 2. Why the architecture gives it to us

A playground app is

```ocaml
type ('model, 'msg) app = {
  init : flags -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> shape list;
  subscriptions : 'model -> 'msg Sub.t;
}
```

and a game's own `update : computer -> 'memory -> 'memory` is a **pure
function** of the inputs and the previous state. Three consequences,
and they are the whole chapter:

```
   frame 0 ---- computer_1 ---> model_1 ---- computer_2 ---> model_2 ...
                                  |                            |
                                view                         view
                                  v                            v
                               picture                      picture
```

- **A run is its inputs.** Keep the `computer` of every frame and the
  entire history can be recomputed. Nothing else has to be saved.
- **The past is not lost.** Models are immutable values; keeping the
  one from 60 frames ago costs a pointer, and its unchanged parts are
  shared with the current one.
- **The future is computable.** Given a model, running `update`
  forward with no input says where the ball *will* be -- which is
  §6's trail, and which one game in this repository already does by
  hand (`TinySlingshot.ml` steps the physics engine 60 times to
  draw its aim arc).

Elm shipped this debugger in 0.18, and this is the same idea in OCaml
-- with one difference that must be said plainly. **Elm's debugger can
print any model; ours cannot.** Elm keeps type information at runtime,
so its debugger renders any value; OCaml erases types, so a model is
only printable if the game hands over a printer (`?show`). The
timeline works without one -- frame number, inputs, and the picture,
which is what scrubbing is actually for -- but the "inspect this
record's fields" panel Elm gets for free is, here, opt-in. It is the
one place this project's language is worse for the task, and pretending
otherwise would be dishonest.

## 3. What to record

Two choices, and the cheap one is also the more useful.

```
   record the MODELS                    record the INPUTS
   every frame's state                  every frame's computer
   big, game-specific,                  small, uniform, and a
   can't be copied generically          replay *proves* determinism
   scrubbing is a lookup                scrubbing is a recomputation
```

Record the inputs. A `computer` is the mouse (3 floats and a bool),
the keyboard (a small set), the screen (4 floats), the time and the
flags -- on the order of a hundred bytes a frame, so a minute of play
is a few megabytes at 60 fps (to measure). And a replay that
reproduces the run is exactly the property the golden frames, the
physics engine's fixed step and any future lockstep networking all
depend on: recording turns determinism from a claim into a test.

**Keyframes** make scrubbing bearable: keep the model every N frames
(60 by default). Reaching frame 3,000 is then "restore the model at
2,940, replay 60 frames", which is one second of game time and a few
milliseconds of CPU, instead of replaying from the beginning.

```
  frames   0        60       120      180      240   ...
  models   K--------K--------K--------K--------K
           ^ keyframe: an immutable value, shared structure, a pointer

  scrub to 173  =  restore K(120), then update 53 times
```

## 4. Replay, and the one hard part: effects

`update` returns a model *and* a `Cmd` -- play this sound, fetch this
URL. Re-running a frame re-produces the command, and that is where a
naive time-travel debugger becomes a noise machine: this project
already wrote the hazard down before the debugger existed
(`Audio.mli`: an `update` run twice for the same frame --
"a time-travel debugger replaying it" -- plays its sound twice).

So a replayed frame runs **silent**: the command is computed, because
it is part of the transition, and then dropped -- and shown in the
panel instead, which turns out to be a good picture of what a frame
did. Fetches and image loads are already cached, so they replay
harmlessly, but that is something to *check*, not assume.

The general rule, and it is the same one the networking plan will
need: **a frame is replayable if its only outputs are its model and
its picture.** Anything else -- sound, network, files, a random
number drawn from a global generator, a clock read behind the
playground's back -- has to be made an input or made suppressible.
That is why the plan's phase 0 is seeds and a recorded clock rather
than something more exciting.

## 5. Pause, step, rewind

With a recording and keyframes, the interface writes itself:

```
   [====================|--------------]  frame 1734 / 2400
    ^ scrub                ^ now

   space  pause / play        left/right  one frame
   [ / ]  one second          shift+arrow ten frames
```

Two details that are not obvious until you build it. **Stepping
forward while paused must re-run `update` with the recorded input**,
not with the live one, or the run forks -- and if you *want* it to
fork (play on from frame 1,000 with your hands), that is a deliberate
command that truncates the recording, exactly as an editor's undo
history does when you type after undoing.

And **the picture during a scrub is `view` of an old model**, not a
saved image: it is recomputed, so every overlay, panel and debug shape
works in the past too. Saving pixels would have been easier and much
less useful.

## 6. Trails: the past and the future of a sprite

Victor's platformer, and the reason this section exists:

```
        past (recorded)            future (computed)
     o - o - o - o - o - [X] - . - . - . - .
     faded dots, where it was      hollow dots, where it will be
                                   if nothing changes
```

- **The past** is free: the recording has the models, `where` says
  where the thing was in each.
- **The future** is `update` run forward from *now*, N times, with
  the input held or empty, collecting `where` at each step -- and
  then thrown away, because it is a prediction, not a state change.
  A pure `update` is what makes that safe: running it does nothing to
  the world.

`TinySlingshot.ml` already does exactly this by hand, and its
comment says why it looks right: the dots are the engine's own steps,
so the ball follows them precisely. The generic version has to agree
with the hand-written one -- a good test, and a good demonstration
that the trick was never specific to that game.

The natural extension, deliberately left for later (see the plan's
"Later"), is to trail a *number* rather than a position: the ball's
height over the whole run as a curve under the frame, two runs
overlaid, a field of the model plotted against time. That is Victor's
other essay, "Up and Down the Ladder of Abstraction" (2011), and it
wants a chart layer the playground does not have yet.

## 7. The panels: an engine drawing its own thinking

Each teaching library computes things worth seeing, and one of them
already proves the shape: `Audio_debug` records the samples the
platform just played and draws them as an oscilloscope (triggered on a
rising zero crossing, so a steady tone stands still) or a spectrum
(2048-point FFT, Hann-windowed, log frequency axis, decibels). Two
functions: `record`, cheap and called always; `shapes`, called when
the panel is open.

Every other engine has the same two functions waiting to be written:

| panel | what it draws | from |
|---|---|---|
| physics | hitboxes, velocity and force arrows, contact points and normals, the broad phase's grid and its pair counts, the solver's impulses as lengths, which integrator is running | `physics/2d`, and `plan_physics_remaining.md` §2's list |
| graphics | shapes drawn, triangles, pixels touched, **overdraw** (how many times each pixel was written -- the number that explains a slow frame), the time each stage took, and the `Opti` switch's effect | `graphics/`, `Render`'s options |
| audio | the oscilloscope and the spectrum | `Audio_debug` (exists) |
| ai | the frontier in the order it was explored, the flow field as arrows, each steering force on its body, each enemy's state written over it, the search's top moves with their values, a network's weights | `plan_ai_teaching.md`'s `Ai_debug` |

Two rules keep this from rotting. They are **shapes**, so every
backend draws them and golden frames test them; and they are **off by
default with zero cost**, because a `record` that formats a string
every frame is a tax on every player for the benefit of one developer.

The overdraw panel deserves its own sentence, because it is the one
that teaches something a number cannot: drawn as a heat map, it shows
*why* a frame is slow -- a big translucent shape behind everything, a
background redrawn twice -- in a way that a millisecond count never
does. That is the whole argument of this note in one picture.

## 8. A run as a test

The recording is a list of inputs and a frame number. This
repository's golden-frame machinery
(`tests/common/Testutil_golden.mli`) takes exactly that: an
executable, a `-script` of keys over frames, a frame number, and a
committed PNG. So **exporting a recording produces a test**, and the
loop closes:

```
   play until it goes wrong  ->  export  ->  a scripted golden test
                                              |
                              fix the code  <-+  replay it: is it fixed?
```

Elm's debugger has the same feature (export and import a history), and
it is the one that matters most in a teaching repository: a student
who finds a bug can hand over the run rather than describe it.

## 9. Compared with Elm's debugger, Redux DevTools and `rr`

The whole landscape, from Smalltalk to RenderDoc, is
[`notes_inspect_related_work.md`](../related-work/notes_inspect_related_work.md).

**A viewer for any value.** What Elm's debugger and Redux DevTools
spend most of their code on is what we deliberately lack: showing
*any* model, folding and unfolding its fields (§2).

**Where determinism comes from.** `rr` replays any Linux process, and
pays for it: it records the result of every system call and signal,
runs all threads on one core, and uses the CPU's performance counters
(retired branches) to replay an asynchronous event at the exact
instruction it hit. We get the same property from the architecture:
the only thing a frame reads from outside is `computer`, a record of
five fields (`mouse`, `keyboard`, `screen`, `time`, `flags`). The
price is that we replay only programs that keep the rule of §4 --
and six programs of this repository do not yet, calling
`Random.self_init` (`Snake.ml`, `Tetris.ml`, `TinyBlockout.ml`,
`TinyWorms.ml`, `StarCollector3d.ml`, `FloatingCity3d.ml`): exactly
what the plan's phase 0 is for, and exactly what `rr` never has to
ask of anybody.

**Messages or frames.** Elm and Redux record *messages* -- a click, a
key, a fetched response -- and show them as a list you can click; a
Redux action can even be switched off and the history recomputed
without it. A playground `game` has one message, the tick, so we
record the `computer` of each frame and show a timeline instead. That
is simpler and uniform, but it loses the list: "which frame did
something" is invisible until you scrub to it (§10's exercises).

## 10. What this does not do, what it would cost, and exercises

- **Live code editing** -- Victor's most famous demo, and the
  expensive one: it needs the OCaml toplevel compiled to JavaScript in
  the browser, or `Dynlink` and a hot-reloaded module natively, plus
  an answer for what happens to the model when its type changes. What
  *is* available without any of that is the other half: change the
  code, rebuild, and replay the recorded inputs -- which §8 makes
  routine.
- **Tweakables** -- a registered number the panel can slide while the
  game runs (`gravity`, `jump_speed`). Cheap, genuinely Victor's
  demo, and deliberately after the recording exists, because a tweak
  is an input and has to be recorded like one or replay stops meaning
  anything.
- **A generic model inspector** -- §2: OCaml erases types. A `?show`
  the game supplies, or nothing.
- **Profiling** -- flamegraphs and allocation tracking are the shell's
  job (`dev/notes_debugging_techniques.md`), not the frame's.

And things the real tools have, as exercises once `Inspect` exists, in
rough order of difficulty:

- **a smaller recording**: most frames' `computer` equals the
  previous one except for `time`; store only the frames where it
  changed (run-length), and measure against §3's estimate;
- **a message list**, Elm's: mark on the timeline the frames whose
  input changed (a key down, a click), so "which frame did
  something" is a click, not a scrub (§5);
- **a watchpoint**: run until a predicate on the model becomes true
  (`fun m -> m.lives < 3`), replaying from the keyframes -- the
  conditional breakpoint of a debugger, over frames (§3);
- **a bounded recording**: keep every keyframe for the last minute
  and thin the older ones, so a long session fits a fixed memory
  budget, at the price of slower scrubs into the distant past (§3);
- **onion skin**: draw the pictures of the frames around the current
  one, faded, the animator's trick -- `view` of old models, which §5
  already recomputes;
- **recording `msg`s** for a full `app` (the GUI programs, with their
  `Cmd` and `Sub`), not only a game's ticks: Elm's model exactly, and
  a message printer needed for the same reason as `?show` (§2);
- **two runs compared**: replay one recording against two builds of
  the game and stop at the first frame whose model differs (a hash of
  the model, as networking's desync check does), which turns "the
  fix changed something" into a frame number (§8);
- **omniscient queries** (Lewis, 2003): "when did `m.hero.x` last
  change?", answered by replaying and asking the question at every
  frame -- cheap here; for a native program, Pernosco builds a
  database of the whole `rr` recording to answer it.

## 11. In the playground

`-inspect` (or "g" with `-debug-keys`) wraps the running app; the
game needs no change, because the inspector wraps the `app` the way
`Bigbang`, `Logo` and `Puzzlescript` build one. While it is open,
space pauses, the arrows step, "[" and "]" jump a second, "p" cycles
the panels of §7 and "t" toggles the trail of §6. A game that wants a
trail of its own hero says where the hero is:

```ocaml
Inspect.trail ~past:60 ~future:60 (fun m -> (m.hero.x, m.hero.y)) model
```

and gets Victor's picture. Everything else is the inspector's, and the
plan for all of it is
[`plan_inspect_teaching.md`](../plans/plan_inspect_teaching.md).

## Glossary

- **Immediate connection** (Victor, 2012): seeing the effect of what
  you are making as you make it.
- **Time-travel debugger**: one that can move a running program
  backwards; here, by replaying recorded inputs.
- **Recording**: the inputs of every frame; **keyframe**: a model kept
  every N frames so scrubbing does not replay from the start.
- **Replay**: recomputing models from a recording; **silent replay**:
  the same with the frame's commands suppressed.
- **Scrub**: moving freely along the timeline; **step**: one frame at
  a time.
- **Trail**: where a thing was (from the recording) and where it will
  be (by running `update` forward).
- **Panel**: an engine's own drawing of what it just computed
  (oscilloscope, contacts, frontier, overdraw).
- **Overdraw**: how many times a pixel was written in one frame.
- **Determinism**: the same inputs giving the same run -- the property
  every one of the above depends on.
- **Tweakable**: a number the panel can change while the game runs
  (later; §10).

## References

- Bil Lewis, "Debugging Backwards in Time", AADEBUG (Fifth
  International Workshop on Automated Debugging), 2003.
- Bret Victor, "Up and Down the Ladder of Abstraction",
  worrydream.com, 2011.
- Bret Victor, "Inventing on Principle", talk at CUSEC, 2012.
- Bret Victor, "Learnable Programming", worrydream.com, 2012.
- Dan Abramov, "Live React: Hot Reloading with Time Travel", React
  Europe, 2015.
- Evan Czaplicki, "Time Travel made Easy", elm-lang.org, 2016 (the
  Elm 0.18 debugger).
- Robert O'Callahan, Chris Jones, Nathan Froyd, Kyle Huey, Albert
  Noll, Nimrod Partush, "Engineering Record and Replay for
  Deployability", USENIX Annual Technical Conference, 2017 (`rr`).
