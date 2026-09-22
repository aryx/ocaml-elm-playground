# playground/Inspect vs. the rest of the "see it while it runs" world

Where a small overlay-and-replay layer sits among Victor's live
environments, Elm's and Redux's time-travel debuggers, record-replay
debuggers like `rr`, and the frame inspectors game engines ship --
what they do that this will not, and which of their ideas fit in a few
hundred lines. Companions:
[`notes_inspect.md`](../tutorials/notes_inspect.md) (how it works) and
[`plan_inspect_teaching.md`](../plans/plan_inspect_teaching.md) (what
gets built, in what order). The shell-side half of debugging -- headless
runs, offscreen frames, scripted input -- is
[`../dev/notes_debugging_techniques.md`](../dev/notes_debugging_techniques.md),
and stays separate.

## The one-line version

| | What it optimizes for | What you see / write |
|---|---|---|
| Smalltalk-80, Self, Morphic, Lisp machines | A world you can open and change while it runs | Inspectors on any object, and no distinction between running and editing |
| Bret Victor's demos, Light Table, Khan Academy's live editor | Immediate connection: the effect of a change, now | A split screen: code on one side, its consequences on the other |
| Elm debugger (`--debug`), Redux DevTools | Replaying a pure state machine's history | A list of messages, a slider, import/export of a run |
| `rr`, gdb reverse debugging, Pernosco, Replay.io | Debugging *any* native program backwards | Record once, then step a real process in both directions |
| Unity, Unreal, Godot editors | An engine you can pause and poke at, in the editor that made it | Inspectors, gizmos, a frame debugger, time scale |
| RenderDoc, PIX, Nsight | One frame of GPU work, dissected | A capture: every draw call, buffer and texture, replayable |
| Tracy, Optick, Chrome tracing | Where the milliseconds went | Flamegraphs, per-frame timelines |
| Box2D's `b2Draw`, Bullet's debug drawer | An engine drawing its own state | Callbacks the app implements to draw shapes |
| `playground/Inspect` | Seeing what *these* engines compute, and replaying a run that is only its inputs | One key, shapes over the frame, a timeline, a trail -- and no change to the game |

## Part 1: where it came from

- **Smalltalk-80** (Xerox PARC, 1980) and **Self** with **Morphic**
  (Sun, 1990s): a running system you inspect and edit without
  stopping it. Everything since is an attempt to get some of that back
  in languages that compile. (Dates from memory, to check.)
- **Doom and Quake demos** (id Software, 1993, 1996): a recorded game
  is *its inputs*, replayed through the same simulation -- the whole
  idea of this plan's recording, shipped as a feature thirty years
  ago, and the reason demo desynchronisation is the classic evidence
  that a simulation stopped being deterministic.
- **Omniscient debugging** (Bil Lewis, 2003): record everything a
  program does and query it afterwards; the academic statement of
  "why can debuggers only go forwards?".
- **gdb's reverse execution** (gdb 7.0, 2009) and **`rr`** (Mozilla,
  2014): record-replay for real native programs, the latter cheaply
  enough to use daily. **Pernosco** and **Replay.io** are the modern
  commercial descendants.
- **Bret Victor, "Inventing on Principle"** (CUSEC, January 2012):
  the principle, the live-coded tree, the scrubbed platformer with the
  trajectory drawn across the level, and the binary-search
  visualisation showing every value at every step. Then **"Learnable
  Programming"** (2012) -- make the data visible, make time visible --
  and **"Up and Down the Ladder of Abstraction"** (2011), which is
  where the "plot it over the whole run" idea in this plan's "Later"
  comes from.
- **Light Table** (Chris Granger, 2012) and **Khan Academy's live
  editor** (John Resig, 2012) were both explicitly Victor-inspired,
  and are the honest evidence of how hard the general case is: both
  were widely admired, neither became how people write programs.
- **Elm's time-travel debugger** (elm-reactor, 2014; shipped as
  `elm make --debug` in 0.18, 2016), and **Redux DevTools** (Dan
  Abramov, 2015), whose "Hot Reloading with Time Travel" talk carried
  the idea into JavaScript. Both work for exactly the reason this
  plan does: a pure update function over an immutable state.

## Part 2: the systems today

- **Engine editors** -- Unity, Unreal, Godot -- give pause, step,
  inspectors on live objects, gizmos, and a time scale. What they
  cannot do is *rewind*: their state is mutable objects, so going back
  means serialising a world, and they mostly don't. (Unreal's
  rewind-oriented tools are for replays of networked matches, which is
  the same trick as demos.)
- **Frame inspectors** -- RenderDoc (Baldur Karlsson, 2013), PIX,
  Nsight -- capture one frame of GPU work and let you walk its draw
  calls, which is the graphics panel of this plan taken to its logical
  end, for hardware we deliberately do not target.
- **Profilers** -- Tracy, Optick, Chrome's tracing -- answer "where
  did the milliseconds go", which is a different question from "what
  is the program doing", and is out of scope here.
- **Physics engines all ship a debug drawer** (Box2D's `b2Draw`,
  Bullet's `btIDebugDraw`, Chipmunk's), for the same reason this plan
  has a physics panel: nobody can debug a contact they cannot see. The
  difference is that theirs is a callback interface an application
  implements, and ours is shapes the playground already knows how to
  draw on four backends.
- **Live coding as a performance practice** -- Sonic Pi, TidalCycles,
  Shadertoy, the demoscene -- is the one place Victor's liveness
  actually won, and it is worth noticing *why*: the programs are
  small, stateless enough to restart, and the feedback is the output
  itself. A game with a model is the hard case.
- **Explorable explanations** -- Nicky Case's, Bartosz Ciechanowski's
  articles, Red Blob Games (already this project's pathfinding
  reference) -- are Victor's other descendants: not tools for making,
  but documents you can poke. This project's examples with their
  switchable algorithms are closer to these than to a debugger.

## Part 3: the teaching lineage

- **Victor's talk and essays** are the primary sources, and short.
- **Elm's debugger** is the best small implementation to read, and its
  import/export of a recorded history is exactly phase 6 here.
- **Redux DevTools** is the same in an impure language, and therefore
  a good catalogue of the compromises purity avoids.
- **`rr`'s papers and talks** for how expensive the general case is:
  everything this plan gets from "a frame is its inputs", `rr` has to
  get from intercepting system calls and making threads deterministic.
- **Box2D's testbed** for how much an engine's own debug drawing
  teaches: most people's mental model of an impulse solver comes from
  watching one, not from reading one.

## Part 4: in Elm, and in OCaml

- **Elm** is where this idea is most at home, and the comparison is
  instructive rather than flattering: `elm make --debug` gives a
  message log, a model inspector and import/export of a history for
  *any* program, with no cooperation from the author, because the Elm
  runtime keeps type information and can render any value. Our
  equivalent has to ask the game for a printer (`?show`). Everything
  else -- the recording, the replay, the scrubbing -- carries over
  unchanged, because the architecture is the same one.
- **OCaml** has a genuinely interesting relative that few people know:
  **`ocamldebug` can run backwards**. It takes periodic checkpoints by
  forking the process and re-executes forward from the nearest one --
  the same keyframe-and-replay trick as this plan's timeline, in the
  language's own debugger, since the 1990s. It works only on bytecode
  and knows nothing about frames or pictures, but it is the right
  ancestor to cite. (To check against the manual before quoting.)
- OCaml's other gap is generic printing: without a ppx (`deriving
  show`) there is no way to print a value whose type is erased, which
  is the whole reason for `?show`. Adding a ppx dependency to the
  playground for the sake of the debugger would cost every user of the
  library a build dependency, which is why the plan does not.
- **This repository already has the pieces**, which is why the plan is
  mostly assembly rather than invention: `Audio_debug`'s oscilloscope
  and spectrum, `Physics.debug`'s hitboxes, the backends' rendering
  keys, `Input_script`'s scripted keys, `-fixed-time`, `-dump-frame`,
  the golden-frame machinery, and `TinySlingshot.ml`, which
  draws the future of its ball by stepping the engine sixty times --
  Victor's trail, hand-written by a game, before anyone planned one.

## Where `Inspect` actually sits

Two levels, as everywhere in this project:

- **The panels**, at the legible end: each engine's own `*_debug`
  module, two functions (`record`, `shapes`), drawing ordinary
  Playground shapes so every backend shows them and golden frames
  test them. Box2D's debug drawer's job, done in a way a reader can
  follow, for four engines instead of one.
- **The timeline**, at the simple end: one key, one bar, space and
  the arrows, and no change to any game -- because the inspector wraps
  the `app`, the way `Bigbang` and `Puzzlescript` build one.

**The ceiling, stated now**: no live code editing (the toplevel in the
browser is a different project), no generic model inspector (OCaml
erases types), no editing the model by hand, no profiling or GPU
capture, recordings measured in minutes rather than hours, and the
native backends first. What it is *for* is the thing none of the big
systems above can do for these engines: see a contact normal, an A*
frontier, an oscilloscope and a jump's future in the same frame, and
then step back sixty frames and look again.

## Postscript: the numbers (to come)

Once built: bytes and milliseconds per recorded frame (and zero when
off); the longest run that fits a chosen memory budget; the time to
scrub to an arbitrary frame with keyframes every 60 against replaying
from the start; how many of the repository's games replay
bit-identically once phase 0's seeds land (the interesting number, and
probably not all of them at first); and the line count of `Inspect`
against Elm's debugger and Redux DevTools.

Sources: from memory unless linked, and to be checked before relying
on them for teaching -- especially the dates of Light Table, the Khan
Academy editor, elm-reactor and Redux DevTools, and `ocamldebug`'s
reverse-execution mechanism, which should be quoted from the manual
rather than from recollection.
