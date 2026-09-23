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
