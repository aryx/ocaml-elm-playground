# Plan: game feel ("juice"), from scratch, for teaching (`juice/`)

## Context

`graphics/` teaches how a picture is computed, `physics/` how motion
is, `audio/` how sound is, `ai/` how an enemy decides. None of them
teaches why one Breakout feels dead and another, with the same rules,
the same levels and the same score, feels *alive*. That difference is
a field of its own, with a name, a book and a canonical demo:

- Steve Swink, *Game Feel* (2008): "real-time control of virtual
  objects in a simulated space, with interactions emphasized by
  polish" -- the book that named it.
- Martin Jonasson and Petri Purho, "Juice it or lose it" (GDC Europe
  2012): one Breakout, made twice, the second time with every effect
  switched on one after the other (tweens, squash and stretch,
  particles, screen shake, sound, trails, a face on the paddle). The
  word *juice* comes from there, and so does this plan's flagship
  example.
- Jan Willem Nijman, "The Art of Screen Shake" (2013, Vlambeer): the
  same idea on a shooter, 30 small tricks in 30 minutes.

This plan does for feel what the others did for their fields: a small
library under `juice/`, one idea per module, each `.mli` with its
diagram, worked example and reference, and an **Evan-style API** over
it (`playground/Juice.mli`), so a game says *"shake a bit, freeze 6
frames, burst of sparks here"*, not how the noise or the emitter works.

What is already here, hand-written each time, which is the argument
for the library:

- **Hitstop**: `TinyStreetFighter` (a `hitstop` counter in the model,
  6 frames, the header explaining why).
- **Sparks and debris**: `TinyStreetFighter`, `TinyFinalFight`
  (`sparks : (x, y, age) list`, rotated rectangles), `TinyRobotron`,
  `TinyLunarLander` (`view_debris`).
- **Flash**: `TinyDefender`'s smart bomb (a white rectangle, faded).
- **Easing**: `Road.ml`'s `ease_in` and `ease_in_out`, two of Penner's
  curves, private to the racing kit.
- **Screen shake**: listed as an exercise at the end of
  `Camera2d.mli`, with Nijman's talk cited, not done.
- **The feel of the controls**: `TinyCeleste`'s `feel` record (coyote
  time, jump buffering, variable jump height, corner correction, each
  a switch) -- already the "simple and better version side by side"
  principle applied to feel, and the precedent for this plan's
  switches.
- **Keyframed poses**: `Stickman` and `Skeleton` interpolate between
  key poses, linearly.

Companions (to be written, the first ahead of the code as its
specification): `notes_juice.md` (the tutorial: curves, springs,
noise, emitters, the time a frame freezes) and
`notes_juice_related_work.md` (the talks, the engines' tween and
particle systems, the arcade games that did it first).

## Principles (the same as the others)

The shared list is in [`../README.md`](../README.md). Two matter more
here than anywhere else:

- **The simple version stays, beside the better one, switchable.**
  For juice this *is* the lesson: the flagship example is a game with
  every effect on a key, dry and juiced, so the difference is felt,
  not read. Jonasson and Purho's talk is exactly this.
- **Deterministic, seeded.** Particles and shake are where games reach
  for `Random.float` without thinking. Here an emitter takes a seed and
  a shake reads a noise function of time, so golden frames still work
  and a juiced game still replays.

And one of its own:

- **Juice is decoration, never rules.** Nothing in `juice/` changes
  what the game *is*: a particle does not collide, a shake does not
  move the hitbox, a tween does not decide when the ball arrives.
  Turning every effect off must leave the same game, frame for frame,
  in its model (hitstop is the one effect that touches time, and it
  is honest about it, below). This is what makes the switches safe,
  and what keeps juice out of `physics/`.

## Where it goes: `juice/`, `playground/Juice.mli`, and two small verbs elsewhere

The question this plan started from. Three options:

1. **One module, `playground/Juice.ml`.** Least ceremony, but breaks
   principle 1 (the algorithms would know `shape` and `computer`), has
   nowhere to put the diagrams and worked examples one idea at a time,
   and the 3D side (`Juice3d`) would have to reach into a 2D API
   module for its curves and noise.
2. **Spread over existing libraries.** Easing into `core/`, particles
   into `physics/`, shake into `Camera2d`. But `core/` is Elm's core
   reimplemented, and Elm's core has no easing (it lives in
   elm-community's `easing-functions`, a package); `physics/2d/`
   already has a `Particles` module, Jakobsen's Verlet particles, which
   are *physics* -- the emitter is the opposite, fake motion that
   collides with nothing, and Reeves' paper is a graphics paper; and
   `graphics/` is about rendering, which none of this is.
3. **A new top-level `juice/`**, a private library like `ai/`: pure
   OCaml, `(wrapped false)`, `(package elm_playground)`, with
   `playground/Juice.ml` the adapter, and `Juice3d.ml` later beside
   `Physics3d.ml`.

**Recommended: 3, plus two small verbs where they obviously belong**:
`Camera2d.shake` (the exercise its `.mli` already lists, a one-line
verb reading `juice/Trauma`) and, later, `Camera3d.shake`. The library
will be the smallest of the teaching ones (six modules), and that is
fine: `ai/` started with two.

Name: `juice/` (Jonasson and Purho's word, the one people search for)
rather than `feel/` (Swink's), the popular term winning (the author,
2026-09-23). The feel of the *controls* (coyote time, jump
buffering) stays where it is, in the games and `Character3d`: it
changes rules, so by the principle above it is not juice.

## The Playground API, Evan-style (tentative, to be settled by writing the games with it)

Two kinds of effect, and the API keeps them apart because Elm's
architecture does:

- **Stateless: a function of time**, like `wave` and `zigzag`. A tween
  needs only when it started:

  ```ocaml
  (* in the model: when the brick was hit *)
  let size = Juice.tween Juice.out_back 0. 1. 0.3 brick.hit_at computer in
  rectangle red 60 20 |> scale size
  ```

  and squash and stretch is the same, a curve read at a time:

  ```ocaml
  ball_shape |> Juice.stretch (Juice.squash 0.3 ball.landed_at computer)
  ```

  (They take the `computer` rather than `computer.time`, unlike
  `wave`: it carries the time *and* the flags, so `juice=off` reaches
  them too.)

- **Stateful: a bag of effects in the model**, stepped in `update`
  and drawn in `view`, like a `Physics.body`:

  ```ocaml
  type model = { ...; fx : Juice.t }

  (* update: the game says what happened *)
  let fx = if brick_broken then
             m.fx |> Juice.burst ~at:(bx, by) Juice.sparks
                  |> Juice.shake 0.4
                  |> Juice.freeze 4
           else m.fx in
  let fx = Juice.step computer fx in
  if Juice.frozen fx then { m with fx } else ... the game's own update ...

  (* view: the world shaken, the particles and flashes on top *)
  Juice.view m.fx world
  ```

Sketch of `playground/Juice.mli`:

```ocaml
(*****************************************************************************)
(* {1 Effects as functions of time} *)
(*****************************************************************************)
(* Nothing in the model but when it started; called in view, like wave. *)

(* curves: [0,1] -> [0,1], Penner's, named as everyone names them *)
type ease
val linear : ease
val in_quad : ease   val out_quad : ease   val in_out_quad : ease
val out_back : ease  val out_elastic : ease  val out_bounce : ease
val tween : ease -> number -> number -> number -> time -> computer -> number
  (* [tween e from to seconds started computer] *)

(* squash and stretch: no new backend primitive, see below *)
val squash : number -> time -> computer -> number * number
val stretch : number * number -> shape -> shape
val whiten : shape -> shape               (* the hit flash *)

(*****************************************************************************)
(* {1 Effects kept in the model} *)
(*****************************************************************************)
(* Values in the model, stepped in update, drawn in view, like a body. *)

(* a value that follows a target like a spring *)
type follow
val follow : ?frequency:number -> ?damping:number -> number -> follow
val toward : number -> follow -> follow   (* one tick *)
val value : follow -> number

(* the bag of effects *)
type t
val none : seed:int -> t
type burst                                 (* an emitter's recipe *)
val sparks : burst  val smoke : burst  val debris : color -> burst
val burst : at:number * number -> burst -> t -> t
val shake : number -> t -> t               (* adds trauma, 0..1 *)
val freeze : int -> t -> t                 (* hitstop, frames *)
val flash : color -> int -> t -> t
val step : computer -> t -> t              (* juice=off empties the bag *)
val frozen : t -> bool
val view : t -> shape list -> shape list
```

`trail` (the last n positions of a thing, drawn fading) is either a
`follow`-like value or a helper over a list the game keeps; left for
the second game to decide.

## Target layout

```
juice/
  dune          private library juice, (wrapped false), package elm_playground
  Ease          Penner's curves, and why out_back overshoots
  Tween         a value between two, as a function of the time it started
  Follow        second-order dynamics: frequency, damping, response (a critically
                damped spring, the smooth-damp of every engine)
  Trauma        screen shake: trauma decaying, squared, driving smooth 1D noise
  Emitter       Reeves' particle system: birth, life, the fixed step, death, seeded
  Squash        volume-preserving scale (sx * sy = 1) from a curve
  tests/        the worked examples, and the laws below
playground/
  Juice.ml(i)   the Evan-style API above; stretch and whiten on shapes
  Juice3d.ml(i) later: bursts of small cubes or billboards, the camera shaken
  Camera2d.ml   + shake
examples/
  JuiceBreakout.ml   the flagship, every effect on a key
  JuiceCurves.ml     every ease, plotted and played side by side
  JuiceParticles.ml  an emitter's numbers on sliders (Gui), fireworks
```

Names checked against the unwrapped libraries: `Ease`, `Tween`,
`Follow`, `Trauma`, `Emitter`, `Squash`, `Juice` are free; `Particles`
and `Springs` (physics/2d) and `Noise` (audio) are taken, which is one
more reason for `Emitter`/`Follow` and for keeping the shake's noise
inside `Trauma`. An example cannot be called `Juice.ml` (it would clash
with the module), hence `JuiceBreakout`.

## Groundwork decisions

### The juice flag: `juice=off`, `juice=hand`, `juice=engine`

A flag, like `artwork=shapes|sprites` (`Sprite.artwork`), read from
`computer.flags`:

    dune exec games/fighting/TinyStreetFighter.exe -- juice=off
    (in a browser, TinyStreetFighter.html?juice=hand)

- **`juice=off`: the dry game, for free.** `Juice.step` reads the flag
  and, when it is `off`, empties the bag: bursts, shakes and flashes
  are dropped as they come, `frozen` is never true, and `view` returns
  the world unchanged; `tween` and `squash` return their end value (no
  motion, the thing just *is* there). So every game using `Juice` has a
  dry mode without a line of its own -- the talk's "before" -- and the
  "decoration, never rules" principle becomes checkable on any of them,
  not only on `JuiceBreakout`: the same script run with `juice=off`
  and with the default must reach the same score and the same end
  (hitstop excepted, which shifts the frames; a scripted test compares
  at the end, not frame by frame).
- **`juice=hand` and `juice=engine`: the simple version beside the
  better one.** Only for the retrofitted games (`TinyStreetFighter`,
  `TinyFinalFight`, `TinyDefender`), whose hand-written sparks, hitstop
  counter and flash are exactly the "simple version" principle 3 wants
  kept: 10 lines in the game, readable without opening a library.
  `hand` keeps them; `engine` draws the same moments with `Juice`
  (bursts, trauma, a flash that decays) -- same game, two
  implementations of its feel, switchable. A retrofit then *adds* a
  branch instead of replacing code, which also keeps the diff small
  and the existing golden frames unchanged.

The reader, as `Sprite.artwork`:

```ocaml
type juice = Off | Hand | Engine
(* [mode ~default flags]: the flag juice=off|hand|engine, [default]
 * when absent or none of these (Engine for a game written with Juice,
 * Hand for a retrofitted one, whose original look stays the default) *)
val mode : default:juice -> flags -> juice
```

A game without hand-written juice treats `Hand` as `Off`. Each juiced
game's header says its default, as for `artwork`. Golden frames: the
default is the plain scene, and the other modes are flagged scenes
(`Testutil_golden.flagged`), e.g. `TinyStreetFighter_engine.png` next
to the existing `TinyStreetFighter.png`.

### Squash, stretch and flash without touching the backends

A shape has one `scale`, so non-uniform scaling is not a primitive, and
adding one means every backend (Cairo, software, web, SVG). It is not
needed: shapes are data (`form` is a tree), so `stretch sx sy` can
rewrite it in `Juice.ml` -- a circle becomes an oval, a rectangle's
sides are scaled, a polygon's points, a group's children and their
positions; a *rotated* rectangle or ngon becomes the polygon it is
(scaling after rotating is a shear). `whiten` is the same kind of walk,
every color replaced. The honest limits, in the `.mli`: `Words` and
`Image` are only scaled uniformly (by the geometric mean) and never
whitened.

### Randomness is a seed

`Juice.none ~seed` starts the bag; every burst draws from it and moves
it on, as `ai/` does. The golden frames of a juiced game are therefore
stable, and `JuiceBreakout` gets a golden frame mid-explosion.

### Shake from noise, not from `Random`

Eiserloh's lesson (GDC 2016): a shake from random offsets each frame
jitters; a shake read from smooth noise at the current time *shakes*.
And trauma, not shake, is what decays: the offset is `trauma²` times
the noise, so small hits barely move the screen and big ones really do.
The noise is a tiny 1D value noise inside `Trauma` (a function of time
and a seed), which also makes the shake deterministic for free.

### Hitstop is the one effect that touches time

`freeze n` makes `frozen` true for n frames; the game chooses to skip
its own update then (the API cannot do it for it without owning
`update`). The effects keep stepping while frozen -- the shake
continues, the sparks keep flying -- which is what the fighting games
do and what `TinyStreetFighter` does by hand today. So "every effect
off gives the same game" holds for everything but hitstop, and the
`.mli` says so: freezing changes *when* frames happen, not what.

### A fixed step, and a cap

Emitters step at 1/60 s like `Physics.step`, and a `t` holds at most a
few hundred particles (oldest dropped first): each is a shape, and the
software rasterizer and the SVG backend pay per shape. The cap and its
reason go in the `.mli` (honest about scale: this is Breakout-sized
juice, not a GPU particle system of a million points).

## The modules, with their references

- `Ease`: Robert Penner, *Programming Macromedia Flash MX* (2002), ch.
  7 -- the easing equations every tween library still copies; Disney's
  "slow in and slow out" (Thomas and Johnston, *The Illusion of Life*,
  1981) as where the idea came from.
- `Tween`: the tween as a pure function of its start time -- the Elm
  way (elm-community/easing-functions), not the Flash way (an object
  mutated every frame).
- `Follow`: second-order dynamics as a design tool, t3ssel8r, "Giving
  Personality to Procedural Animations using Math" (2022); critically
  damped springs, Ryan Juckett (2012) and Unity's `SmoothDamp` (Game
  Programming Gems 4, Thomas Lowe, 2004). The worked example: frequency
  2 Hz, damping 1, no overshoot, 95% there in about 0.4 s.
- `Trauma`: Squirrel Eiserloh, "Math for Game Programmers: Juicing Your
  Cameras With Math" (GDC 2016); Nijman (2013).
- `Emitter`: William Reeves, "Particle Systems -- A Technique for
  Modeling a Class of Fuzzy Objects" (SIGGRAPH 1983; the Genesis effect
  of *Star Trek II*).
- `Squash`: squash and stretch, the first of Disney's twelve principles
  (Thomas and Johnston, 1981); area preserved, `sx * sy = 1`.

## New examples

- **`JuiceBreakout`**, the flagship: Jonasson and Purho's talk as a
  program. Keys 1-9 each turn one effect on (tweened bricks entering,
  squash on bounce, particles, shake, hitstop, flash, trail, sound via
  `Audio`'s `Sfx` presets, the paddle's eyes following the ball with
  `Follow`), 0 all of them; the same model underneath, which a test
  checks by running a script with everything on and everything off and
  comparing the models (the "decoration, never rules" principle, as a
  test).
- **`JuiceCurves`**: every ease plotted as a curve, and a box moving
  with it beside the curve, all restarting together -- the picture
  that makes `out_back` and `out_elastic` obvious.
- **`JuiceParticles`**: one emitter, its numbers (rate, life, speed,
  spread, gravity, colors) on `Gui` sliders, bursts on click.

## Games with juice

Retrofit only where the hand-written version already exists, with a
small diff each, and ask before touching any:

- `TinyStreetFighter`: its `hitstop` and `sparks` onto `Juice.freeze`
  and `Juice.burst`, plus a shake on the heavy hits.
- `TinyFinalFight`: its `sparks` onto a burst.
- `TinyDefender`: the smart bomb's flash onto `Juice.flash`, plus a
  shake.
- `Road.ml`'s `ease_in`/`ease_in_out` could read `Ease`; not worth the
  kit depending on one more library unless it changes anyway.

Each such game's header gains "What it uses: ... Juice (...)", per the
header convention. New games in the catalogue can use it from the
start; `Juice.mli`'s header lists which do.

## Phasing

1. **`Ease` and `Tween`**, their tests (the curves' ends, `out_back`'s
   overshoot, the worked example's numbers), `JuiceCurves`. The
   stateless half of the API, `Juice.tween`.
2. **`Squash`, `stretch`, `whiten`** in `Juice.ml`, tested on each
   form (a rotated rectangle stretched is the right polygon).
3. **`Trauma`** and `Camera2d.shake`; `Juice.t` with `shake`, `freeze`,
   `flash`, `step`, `view`.
4. **`Emitter`**: bursts, the cap, the seed; `JuiceParticles`.
5. **`Follow`**; then **`JuiceBreakout`** with all nine switches, its
   golden frames (dry, and juiced mid-explosion) and the same-model
   test.
6. The retrofits above, one commit each, if the author wants them.
7. `Juice3d` (bursts and shake in 3D) -- only if a 3D game asks for it.
8. `notes_juice.md` checked against what was built, numbers filled in.

## Status

- 2026-09-23: plan written, from the author's question "any other
  field we miss?".
- 2026-09-23: the `juice=off|hand|engine` flag added, the author's
  idea, after `artwork=`.
- 2026-09-23: the name settled, `juice/` rather than `feel/`: the
  popular term, the one people search for (the author).
- 2026-09-23: `playground/Juice.mli` is one module, in two sections --
  the effects that are functions of time (tweens, squash, stretch,
  whiten) first, then the bag kept in the model (`Juice.t`: bursts,
  shake, freeze, flash) -- rather than two modules (a second name, and
  one that would clash with `juice/Ease` in the unwrapped libraries).
  Split later only if `JuiceBreakout` shows the halves used apart (the
  author).
- 2026-09-23, phase 1 DONE: `juice/Ease` (six families as their `in`
  curves, `out` and `in_out` as two functions over any curve --
  Penner's eighteen formulas from six curves, and `smoothstep`) and
  `juice/Tween` (`progress`, `lerp`, `value`, `finished`), 9 tests
  (the worked examples: quad 0.25/0.75 at a half; `out back` peaking at
  1.1000 at t = 0.580, which is what Penner's 1.70158 is for; the
  tween figure 0/75/100; the laws: every one of the 20 curves 0 at 0
  and 1 at 1, `out (out f) = f`, the bounces touching 1, the gentle
  curves monotone in [0, 1]). `playground/Juice` with the first
  section only: 19 named eases (no `smoothstep`: not one of Penner's
  names), `tween`, and one addition to the sketch, `curve` (an ease
  read directly, to draw it): `JuiceCurves` first plotted its graphs
  by calling `tween` with a rigged `computer`, a hack in a teaching
  example. `tween` takes the `computer` and honours `juice=off` (at its
  end at once); `curve` does not, a curve being data, not an effect.
  `examples/JuiceCurves` (native, software, web), golden frames
  `JuiceCurves.png` (the balls halfway: the frozen clock of the golden
  runner, 1000 s, is 1 s into the 3 s cycle) and `JuiceCurves_off.png`
  (flagged `juice=off`, the first use of a flagged scene outside
  `artwork=`). `notes_juice.md` written, §1 to §3 in full, §4 to §7
  sketched.
- 2026-09-23, phase 2 DONE: `juice/Squash`, two functions and no new
  maths -- `keep_area` ((1/k, k)) and `landing` (§1's `out_elastic` on
  the height: its overshoot *is* the stretch, so the plan's separate
  stretch rule was not needed); 3 tests (40% flatter: 66.7 × 24 for a
  40-pixel ball, 0.859 at 5%, 1.1 at 10%, the most 1.149 at 13%, round
  at the end; the area 1 at every moment). `Juice.squash amount
  seconds landed computer` (the sketch's signature gained the amount;
  (1, 1) with `juice=off`), `Juice.stretch` (a 2×2 matrix pushed down
  the shape tree: exact forms while it stays diagonal, polygons when
  something is rotated; words scaled by the mean) and `Juice.whiten`
  (images left alone), 6 tests in `playground/tests/Unit_juice.ml` (a
  ball's bottom staying on the ground, a rotated rectangle's polygon, a
  rotated group's children, words, whiten through groups, squash and
  `juice=off`). `examples/JuiceSquash`: dry, squashed, and squashed +
  flashed, side by side on a dark backdrop (the playground's white
  background hid the white flash); its bounce a parabola of the time,
  so the golden runner's 1000 s is exactly a landing. Golden frames
  `JuiceSquash.png` and `JuiceSquash_off.png`. The flash's 80 ms is a
  `Juice.tween` counting down, which `juice=off` zeroes -- a stand-in
  until phase 3's `Juice.flash`. `notes_juice.md` §4 in full.
- 2026-09-23, phase 3 DONE, and the first retrofit, `TinyBreakout`
  (the author: "so we can test for real" -- the talk's own game, in
  place of the planned `JuiceBreakout` example, which is dropped).
  - `juice/Trauma`: `add`, `decay`, `shake` (trauma²), `hash`, `noise`
    (1D value noise, smoothstep between lattice points, 25 a second),
    `jitter` (a new value each frame: the simple shake, kept to
    compare) and `offset` (40 px, 5° at most); 5 tests (hash −0.1084
    and 0.5113 at seed 1, noise 0.2014 at 0.5, decay, bounds, and the
    hash's quality). A wrong turn kept: the hash must give the same
    numbers natively (63-bit ints) and under js_of_ocaml (32-bit), so
    Park–Miller by Schrage's method (no product reaches 2³¹, checked
    against node's 32-bit arithmetic: the same four digits); started
    from neighbouring points and stepped three times it gave a
    correlation of −0.15 between neighbours (the generator is linear);
    an xor of the high bits between the steps brought it to 0.002.
  - **The wrong turn of phases 1 and 2**: `tween` and `squash` read
    the wall clock, `computer.time`. The golden runner freezes it (and
    `Scene2d.elapsed`, computed from it), so a game's bricks popping in
    would have stayed at scale 0 in every golden frame. Found while
    designing the `TinyBreakout` retrofit, before writing it. Fixed by
    making `Juice.t` the clock: it counts frames in `step` (an int, so
    75 frames is exactly 1.25 s), `Juice.now fx` gives the time to
    remember, and `tween`/`squash` take `fx` instead of the `computer`
    (`juice=off`, seen by `step`, travels in `fx`). `Juice.during
    seconds started fx` replaced the examples' countdown-tween trick.
    The two examples' models became a `Juice.t`; their golden frames
    moved to frames 60 and 75 (1 s and 1.25 s on the effects' clock)
    and matched the old ones to the pixel, which checked the refactor.
    `Juice.mli` now has three sections, not two: the clock, the
    functions of time, the effects that last.
  - `Juice.t`'s effects that last: `shake`, `freeze`, `flash`,
    `frozen`, `view` (the world grouped, turned and moved; the flash a
    10000-pixel rectangle over it, faded, since the view doesn't know
    the screen). `Juice.jitter` and `Juice.trauma` were written into the
    `.mli` for a planned `JuiceShake` example, then dropped with it (no
    user). 8 `playground/Juice` tests in all (2 new: the effects
    playing out, and `juice=off` emptying them).
  - No `Camera2d.shake`: `Juice.view` shakes the whole picture, and
    `Camera2d` is untouched until a game with a camera needs it (the
    author was asked; phase 3 went ahead without it).
  - `TinyBreakout`: the rules untouched; the old `update` renamed
    `update_rules`, and a new `update` steps the effects, calls it,
    and `juiced` compares the scene before and after -- the score up:
    shake 0.15; a ball lost: shake 0.7, a red flash; the ball going
    down near the paddle before and up after: a bounce, the ball
    (0.5) and the paddle (0.3) squashed, each about the side where they
    meet; a new wall: its bricks popping in (`out_back`, 0.4 s),
    yellow first, then green, orange, red 0.1 s apart. The background
    stays out of `Juice.view`, so a shake shows no edge. Hitstop left
    out on purpose: its 900-frame scripted golden plays by keys at
    given frames, and a freeze would make it miss the ball. That golden
    passed unchanged with the juice on, and the same script with
    `juice=off` gives the same frame 900 (0 pixels differ): the
    "decoration, never rules" test, done for real. One new golden,
    `TinyBreakout_pop.png` (frame 12: the yellow rows overshooting, the
    green ones growing). `main` now passes the flags, so `juice=off`
    reaches it. Then, at the author's request, all of it moved into one
    bannered section, "The juice (juice=off: none of it)", as
    `TinySoldat` keeps its `ai=engine` bots in theirs: `juiced`,
    `update`, and two view helpers, `pop` (a brick's size) and
    `squashed` (the ball's and the paddle's stretch); outside it only
    the model's three fields and `Juice.view` around the picture. The
    paddle's squash went from 0.3 s to the ball's 0.25 on the way (one
    duration for both); the three goldens passed unchanged.
  - `notes_juice.md`: §3 gained "whose time?", §5 in full (trauma,
    noise, the hash, hitstop, flash, juice watching the game), §8
    rewritten for the clock.
- 2026-09-23, phase 4 DONE: particles.
  - `juice/Hash`: the hash moved out of `Trauma` (it was about random
    numbers, not shaking), for `Trauma` and `Emitter` to share, with
    its explanation and its tests (`Unit_hash`: seed 1 at 0, 1, 2;
    mean and correlations), and `unit`, the same number in [0, 1].
  - `juice/Emitter`: Reeves's model -- a recipe (count, speed,
    direction, spread, life, size, spin, gravity, drag: each a pair of
    bounds or a number), a burst of particles born from it, stepped by
    semi-implicit Euler, gone when their life is spent; six draws a
    particle from `Hash` and a count of draws, so the same seed gives
    the same bursts; a cap (400), the oldest dropped. Generic: a
    particle carries a payload made from a random tone, so `juice/`
    knows nothing of colors. 5 tests (thrown up at 400 under 800: the
    top 96.67 at frames 29 and 30, −6.67 after a second, where the
    continuous answer is 100 and 0; drag 2: 36.17 and 30.85 after half
    a second; the same seed the same burst; lives spent; the cap -- whose
    test first expected 3 bursts of 20 under a cap of 50 to lose the
    whole first burst: it loses 10 of its 20, and the test, not the
    code, was wrong).
  - `Juice.burst ~at b fx` with `sparks`, `smoke` and `debris color`
    (the plan's three); the particles in `Juice.t`, stepped by `step`,
    drawn by `view` inside the shake, fading with age; nothing with
    `juice=off`. 2 new tests (a burst drawn then gone; `juice=off`).
    `smoke`'s comment first promised puffs "growing thin"; particles
    don't grow, so the comment was fixed rather than the feature added.
  - `TinyBreakout`: each brick broken bursts `debris` of its color at
    its center, found by comparing the wall before and after the rules'
    update (`broken`, in the juice section). A new golden,
    `TinyBreakout_debris.png` (frame 116, 8 frames after the first
    brick broke); `pop` and the 900-frame `play` passed unchanged, no
    particle being in flight at frames 12 and 900.
- 2026-09-23, phase 5 DONE: followers, and the eyes.
  - `juice/Follow`: the simple way beside the better one -- `smooth`,
    exponential decay by 1 − e^(−rate·dt) (the frame-rate-proof form
    of `x += (target - x) * 0.1`), and `chase`, a mass-spring-damper
    in frequency and damping, semi-implicit Euler. t3ssel8r's third
    number, the response (anticipation), left out and said so. 4 tests
    (2 Hz, z = 1: 0.830 at 0.25 s, 95% at frame 24, never past 1;
    z = 0.5: up to 1.142, where the continuous overshoot is 16.3%;
    `smooth` at 10: 1 − 1/e after 0.1 s, the same at 30 and at 144
    frames a second).
  - `Juice.follow`, `toward` (at the target at once with `juice=off`),
    `value`, and `Juice.on`, for juice a game draws itself. 1 new test.
  - `TinyBreakout`: eyes on the paddle, black with white pupils, the
    two numbers of where they look each a follower (3 Hz, z = 0.5)
    pulled towards the ball's direction, in the juice section
    (`look_at_ball`, `eyes`); none with `juice=off`. The `play`, `pop`
    and `debris` goldens changed only in a 60 × 10 box on the paddle
    (about 164 pixels each: the eyes) and were re-approved; the title's
    passed unchanged.
  - With the eyes, `TinyBreakout` has the talk's effects but the trail,
    the smile and the music (its header's exercises), and the plan's
    `JuiceBreakout` example is not needed. Left: the retrofits of
    `TinyStreetFighter` (hitstop, sparks) and `TinyDefender` (the
    flash), and `Juice3d`, if a 3D game asks.
- 2026-09-23, phase 6 DONE: the retrofits, `juice=hand|engine|off`.
  - `Juice.mode ~default flags` (`Off | Hand | Engine`), as the plan's
    flag section had it.
  - A bug found on the way: `freeze n` stood a game still for n − 1
    frames, not n -- the game steps the effects and then asks `frozen`,
    and `step` had already counted down. `step` now records whether
    this frame is frozen before counting (`frozen` a field); the unit
    test, which had encoded the wrong count ("not frozen after 2
    frames" for `freeze 2`), was fixed with it.
  - `TinyStreetFighter`: `juice=hand` (default) keeps its counter and
    stars; `juice=engine` leaves them empty and does `Juice.freeze 6`,
    a `sparks` burst at each hit and a 0.35 shake, and at the round's
    end a 0.9 shake and a white flash; `juice=off`, neither (and so no
    pauses). The engine's code in a section of its own
    (`engine_hits`, `engine_round_over`); its field is `juice`, not
    `fx`, `fireball` having an `fx` already. The 3 goldens passed
    unchanged (hand). The `sparks` preset, first used here, was too
    small and faint on the evening sky: 16 particles of 2–5 px became
    24 of 5–10 px, faster.
  - `TinyDefender`: `juice=hand` keeps the bomb's white rectangle;
    `juice=engine`: `Juice.flash white 10` and a 0.8 shake, around the
    camera's view, under the scanner and the score. No debris for the
    bombed landers: the shake is in screen space and debris would be in
    the scrolling world, and `Juice.view` does both in one place; left
    out. The 3 goldens passed unchanged.
  - `Testutil_golden`: a fourth kind of scene, `scripted_flagged` (a
    script and flags, which the runner already passed together; only
    the lists could not say it), for the two new goldens,
    `TinyStreetFighter_engine.png` (frame 136, the kick's sparks, the
    same hit as the hand's "fight") and `TinyDefender_engine.png`
    (frame 42, a bomb 2 frames earlier).
  - `notes_juice.md` §8: the three modes.

## Verification

- `juice/tests/`: each `.mli`'s worked example; laws: every ease is 0
  at 0 and 1 at 1; `Follow` with damping ≥ 1 never overshoots a step;
  `Squash` preserves area; an emitter with the same seed gives the same
  particles, and never holds more than its cap; `Trauma` at 0 is no
  offset.
- `playground/tests/`: `stretch` on each form, `whiten` on groups.
- Golden frames for the three examples (with `seed=1`), and
  `JuiceBreakout`'s same-model test.
- `make test-lite` as the gate, the affected golden scenes via dune.

## Out of scope

- The feel of controls (coyote time, jump buffering, input
  forgiveness): it changes the rules, so it stays in the games and
  `Character3d`, where `TinyCeleste` already switches it.
- GPU particles, shaders (bloom, chromatic aberration, CRT): backend
  work, and a different lesson.
- Animation state machines and skeletal blending: `Stickman` and
  `Skeleton` are the start of that, a separate topic.
- Cutscenes and timelines ("move here, wait, say this"): related (they
  would reuse `Tween`), but a way of scripting, not a feel; a sketch
  for `plan_teaching_other.md` if wanted.
