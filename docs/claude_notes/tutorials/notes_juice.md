# Game feel, from scratch: a tutorial for `juice/`

Why one Breakout feels dead and another, with the same rules, feels
alive -- the small effects that make the difference, where each came
from, and how little code each is. It is also the specification of
`juice/` (see [`plan_juice_teaching.md`](../plans/plan_juice_teaching.md)):
the modules §0 marks done exist, the rest is written here first, so
its pointers name planned modules too.

The word comes from Martin Jonasson and Petri Purho's talk "Juice it
or lose it" (GDC Europe 2012): one Breakout, made twice, the second
time with the effects switched on one after the other until the same
game felt like a different one. Steve Swink's *Game Feel* (2008) is
the book of the subject; Jan Willem Nijman's "The Art of Screen
Shake" (2013) the shooter's version of the talk.

One rule runs through all of it: **juice is decoration, never rules**.
A particle does not collide, a shake does not move a hitbox, a tween
does not decide when the ball arrives. Switch every effect off (the
flag `juice=off`) and the game is the same game, only drier. That is
what makes each effect safe to add, and what keeps this out of
`physics/`.

## 0. Where the code is, and a reading order

| module | what | section |
|---|---|---|
| `juice/Ease` (done) | the curves: how a thing starts and stops | §1, §2 |
| `juice/Tween` (done) | a value between two, from when it started | §3 |
| `playground/Juice` (done: the clock, tweens, squash, stretch, whiten, shake, freeze, flash) | the Evan-style API | §3, §4, §5, §8 |
| `examples/JuiceCurves` (done) | every curve, plotted and played | §1 |
| `juice/Squash` (done) | squash and stretch | §4 |
| `examples/JuiceSquash` (done) | a ball dry, squashed, and flashed | §4 |
| `juice/Trauma` (done) | screen shake | §5 |
| `juice/Hash` (done) | random numbers that are the same every time | §5 |
| `juice/Emitter` (done) | particles | §6 |
| `games/arcade/TinyBreakout` (done) | the talk's game, juiced (`juice=off`: dry) | §5 |
| `juice/Follow` (done) | a value that follows a target | §7 |

§1 to §4 are things that are a function of time and nothing else.
§5 to §7 need a little state in the model. All of them run on the
effects' own clock (§3).

## 1. Easing: how a thing starts and stops

A curve takes the time gone, as a fraction from 0 to 1, and gives the
way gone, as a fraction: 0 at 0, 1 at 1. The straight line is what a
program does when nobody thought about it:

```
  way                                  way
 1 |              ___.               1 |        .----___
   |          _--'                     |     .-'
   |       _-'                         |   .'
   |    _-'   linear: a machine        |  /    out_quad: fast, then
   | _-'                               | /     slowing, as a thrown
 0 +'------------------> time        0 +/-----------------> time
   0                     1             0                     1
```

A box moving linearly starts at full speed and stops dead. Nothing
that weighs anything does that, and the eye notices before the mind
can say why. Disney's animators had a name for the fix, *slow in and
slow out*, one of their twelve principles (Thomas and Johnston, *The
Illusion of Life*, 1981): more drawings near the key poses, fewer in
between.

Robert Penner wrote the curves down as equations for Flash
(*Programming Macromedia Flash MX*, 2002), with the names every tween
library still uses -- jQuery's, CSS's `cubic-bezier` presets, Unity's
DOTween, Godot's Tween. Six families:

| family | its `in` curve | what it looks like |
|---|---|---|
| quad | t² | the gentlest |
| cubic | t³ | more marked |
| sine | 1 − cos(t·π/2) | a quarter of a cosine: the softest |
| back | (s+1)t³ − s·t² | goes back a little before going |
| elastic | a growing sine | a spring (backwards: wobbles, then settles) |
| bounce | a ball's bounces | backwards: they grow |

At t = 0.5, `quad` is 0.25: a quarter of the way at half the time.
`examples/JuiceCurves.ml` shows all nineteen at once, each as its graph
and as a ball on a track, all leaving together and arriving together:
only *how* they go differs.

## 2. Out is in, run backwards

Each family comes in three: `in` (starts slow), `out` (ends slow),
`in_out` (both). Only the `in` one needs writing:

```
  out f t = 1 − f (1 − t)
```

Run the time backwards (1 − t), then the way (1 − ...): the graph turns
half a turn around the middle of the square, and a curve that started
slow now ends slow. Doing it twice gives the curve back -- `out (out
f) = f`, which the tests check for every curve. `in_out` is `f`
squeezed into the first half of the time, then `out f` into the
second:

```
  in_out f t = f(2t) / 2              for t < 1/2
             = 1 − f(2 − 2t) / 2      after
```

So `juice/Ease` writes six curves and two functions, where Penner's
table has eighteen formulas -- and a curve you invent gets its `out`
and `in_out` for free. (Penner writes the bounce as its `out`, the way
a ball falls: four parabolas, each touching 1; its `in` is that run
backwards, by the same function.)

`back`'s constant, s = 1.70158, looks arbitrary and is not: it is the
one that makes `out back` overshoot by exactly 10%, up to 1.1 at t =
0.58, then settle at 1. A button that pops in with it grows 10% too
big and settles, which reads as *alive*; with s = 0 it is just `cubic`.

## 3. A tween is a formula of when it started

A tween (from the animators' *in-between*) takes a value from one
number to another over a time, along a curve. It is three small steps:

```
  progress   now → how far through the time, clamped to [0, 1]
  curve      that fraction through a curve (§1)
  lerp       from one number to the other: a + (b − a)·p
```

Worked example (`juice/Tween.mli`, checked by the tests): `out_quad`
from 0 to 100 over 2 s, started at 1 s.

```
  value
  100 |                  .------------   the end, held
      |              .-'
      |           .-'
      |         /
      |        /
    0 |-------'           the start, held before it begins
      +-------+-------+-------+-------> now (s)
      0       1       2       3
```

At 0.5 s it has not started: 0. At 2 s, halfway through the time,
`out_quad 0.5` = 0.75: 75, already three quarters of the way. At 3.5 s
it is over: 100, held.

There are two ways to write one, and the choice matters more than the
curves. **Flash's**: a tween is an object, registered once, that
changes a variable a bit every frame until it is done -- state outside
the program's own, which must be stopped or it keeps running, and which
a replay cannot replay. **Elm's** (elm-community/easing-functions): a
tween is a formula of the time now, and all the program keeps is *when
it started*. There is nothing to step and nothing to stop, and the same
time always gives the same frame -- which is what a golden frame test
needs, and a rewind, and lockstep networking. It is also the only way a
Model-View-Update program can have without cheating: its model can
hold a start time, not a running object.

The limit, stated once: a tween knows its end from the start. When the
end moves while it plays -- a camera following the player -- a curve is
the wrong tool, and §7's `Follow` is the right one.

**Whose time?** A formula of "the time now" still has to say which
clock. The first version read the wall clock, `computer.time`, like
Evan's `wave` -- and in a golden frame test, whose clock is frozen,
every tween then stayed at its start forever: a game's bricks popping
in would never have appeared. The effects run on their own clock
instead, one frame a step, counted in `Juice.t`: the same frames give
the same effects whatever the wall clock does, in a test, in a replay,
on a slow machine. It is the physics engine's fixed time step again
(and the reason `physics/` never reads the wall clock either).

## 4. Squash and stretch, and the hit flash

The first of Disney's twelve principles, and every animation student's
first exercise: the bouncing ball. A ball that stays round when it
hits the floor looks like a billiard ball on stone; the same ball
flattening for a tenth of a second reads as rubber, and as *hitting*
something. `examples/JuiceSquash.ml` shows the two side by side.

Two ideas make it (`juice/Squash`), and neither is new maths.

**Keep the area.** Squashed to 60% of its height and no wider, a ball
looks like it shrank; squashed and widened by as much, it looks like
the same ball under a force. Taller by k, narrower by k: (1/k, k).

```
     k = 1          k = 0.6              k = 1.15
                                          ___
     .--.                                /   \
    /    \       .----------.           |     |
    \    /       '----------'           |     |
     '--'                                \___/

   40 x 40        66.7 x 24            34.8 x 46
            (the same area: 1600 = 66.7 x 24 = 34.8 x 46)
```

**The landing is a curve** -- §1's `out_elastic`, read on the height:
flat at the moment it lands, back up, *past* round, and settled. The
elastic curve's overshoot is the stretch, so no second rule is needed
for it. Worked example (checked by the tests), 40% flatter at landing:
0.6 of its height (66.7 × 24 for a 40-pixel ball), 0.859 at 5% of the
time, 1.1 at 10%, the most, 1.149, at 13%, then 1.006 halfway, and
exactly round at the end.

**Squashing a shape.** A shape has one `scale`, so a stretch that is
not the same across and up is not something the playground draws. It
does not need to: shapes are data, a tree of forms each scaled, rotated
and moved inside its parent -- all of which a 2×2 matrix and a
translation say. A stretch is one more matrix, pushed down the tree:

```
  stretch (sx, sy) of a shape at (x, y), turned a, scaled s:

    its position       (x, y)  ->  (sx·x, sy·y)
    what is left       S · R(a) · s  for its form (or its children)

    left diagonal?     yes: a circle becomes an oval, a rectangle a
                       longer one -- exactly
                       no (something is rotated): the form becomes the
                       polygon it is (an oval by 32 points)
```

That is why a stretched thing should be built standing on (0, 0): the
stretch is about that point, so a ball whose bottom is there squashes
against the ground; one centred there would squash in the air. And
the honest limit: text cannot be stretched unevenly, so it is only
scaled by the mean.

**The hit flash** is the same kind of rewrite: `whiten` gives the same
tree, every color white, drawn for a frame or two when something is
hit. It is a silhouette -- the face's eyes vanish in it -- which is the
point: for 80 ms the thing is only its outline, and the eye reads
"hit" before it reads the picture.

## 5. Screen shake, hitstop and the flash

Squirrel Eiserloh's two lessons ("Juicing Your Cameras With Math", GDC
2016), each a line of `juice/Trauma`:

**What decays is trauma, not shake.** A hit adds trauma, 0 to 1, which
falls by 1 a second; the shake is trauma². Small knocks barely move the
screen, a big one really does, and a string of knocks adds up -- in
`TinyBreakout`, each brick is 0.15 of trauma, a shake of 2%, nothing;
but behind the wall, where the ball breaks a brick every few frames,
they pile up to a real shake, which is the game's best moment made
felt.

```
   trauma   shake = trauma^2   at most, with 40 pixels
    1.0         1.0              40 px
    0.5         0.25             10 px
    0.25        0.06              2.5 px
```

**The shake is read from noise, not drawn at random.** A new random
offset every frame (`Trauma.jitter`, kept to compare) makes the picture
jump from anywhere to anywhere, 60 times a second: a broken video
signal. Random values 25 times a second, and the offset gliding from
one to the next along smoothstep (`Trauma.noise`, 1D value noise), is a
camera knocked. Three noises of different seeds give the offset across,
up, and a slight turn.

The random values come from a hash of the seed and the point
(`juice/Hash`, shared with the particles of §6), and one detail of it
teaches something: OCaml's ints have 63 bits natively and
32 in a browser, so a hash whose products overflow shakes differently
on the two. Park and Miller's minimal standard generator (1988), by
Schrage's method, never makes a product above 2³¹. It is linear,
though, and started from neighbouring points it gave correlated values
(−0.15, the first version); an xor of the high bits between its steps
brings it to 0.002.

**Hitstop** (`Juice.freeze`): at a hit, the game stands still for a
few frames while the shake and the flash go on. A punch is felt more
than seen; the pause says it connected (`TinyStreetFighter` does it by
hand). It is the one effect that changes *when* things happen, not only
how they look, so `TinyBreakout` leaves it out: its golden test plays a
game by keys pressed at given frames, and a pause would make it miss
the ball.

**The flash** (`Juice.flash`): the whole screen tinted, fading out over
a few frames -- red when `TinyBreakout`'s ball is lost.

**Juice watching the game.** `TinyBreakout`'s rules were not touched.
Its old `update` is called as it was, and a new one compares the game
before and after it: the score went up, a brick broke, shake; a ball
went, shake and flash; the ball was going down near the paddle and now
goes up, a bounce, squash both. Run the same keys with `juice=off` and
the game is the same, to the pixel once the effects have faded: its
900-frame golden test passes unchanged, juiced.

## 6. Particles

William Reeves named them, for the Genesis effect of *Star Trek II*
(1982): a wall of fire over a planet, "a class of fuzzy objects" that no
surface can model, made instead of thousands of points, each a few
numbers, none of them designed ("Particle Systems", SIGGRAPH 1983).
Every engine has one since, and a game's sparks, dust and debris are
still his model (`juice/Emitter`):

```
  born      at a place, with a speed, a direction within a cone, a
            life, a size, a spin -- each drawn between two bounds
  moving    by its velocity, pulled by gravity, slowed by drag
  dying     when its life is spent
```

A *recipe* is those bounds; a *burst* is `count` particles born at
once from it. Nothing collides: a particle is decoration, which is why
`TinyBreakout`'s pieces may fly over its side walls. The motion is
still the physics engine's step, semi-implicit Euler, and the worked
example is its error: thrown up at 400 pixels a second under a gravity
of 800, a particle should rise 100 pixels in half a second; stepped at
60 frames a second it rises 96.67 and is at −6.67, not 0, a second
later. Invisible in a spark -- and the same lesson as
`physics/2d/Integrate.mli`'s orbit.

Two things make it fit the rest of this library. **Randomness is a
seed**: each draw is `juice/Hash`'s number for the seed and the count
of draws so far (six a particle), so the same seed and bursts give the
same particles, frame for frame -- a golden frame catches
`TinyBreakout`'s first brick breaking, pieces mid-flight, every time.
**A cap**: each particle is a shape to draw, so at most 400, the oldest
dropped first.

`juice/Emitter` knows nothing of shapes or colors: a particle carries
a payload, made from a random *tone* in [0, 1] at its birth, and
`Juice` makes it a color out of a palette -- `sparks` (white, yellow,
orange, fast, all around), `smoke` (grays, slow, rising), `debris c`
(pieces of `c`, thrown up, tumbling, falling). `TinyBreakout` bursts
`debris` of each brick's color where it broke, finding the bricks
broken by comparing the wall before and after the rules' update.

## 7. Follow: a value with a spring

A tween knows its end from the start (§3). A camera following the
player, a health bar draining, a pair of eyes following a ball: their
target moves while they go, so they are a state, stepped each frame
towards wherever the target is now (`juice/Follow`). The simple way and
the better one:

**Close a fraction of the gap.** Everyone writes `x += (target - x) *
0.1` first. Its flaw: 0.1 a frame is a different speed at 30 and at 144
frames a second. Written with the time, the fraction is 1 − e^(−rate·dt),
exponential decay, 63.2% of the way after 1/rate seconds whatever the
frame rate (the tests step it at 30 and at 144 and get the same
number). It never overshoots, and it starts at full speed: it follows
like a string, not like a thing that weighs something.

**A spring.** The value has a velocity, pulled towards the target and
slowed by damping, a mass on a spring in a bath:

```
  acceleration = w² (target − value) − 2 z w velocity,     w = 2π f
```

Two numbers to think in (t3ssel8r, "Giving Personality to Procedural
Animations using Math", 2022): the frequency f, how fast it answers,
and the damping z. At 1 it is critically damped, as fast as it can go
without going past (Unity's `SmoothDamp`); below 1 it overshoots and
settles; above 1, sluggish. Worked example, from 0 to 1 at 2 Hz: at
z = 1, 0.830 after a quarter second and 95% at 0.40 s, never past 1; at
z = 0.5, up to 1.142 -- the continuous answer is e^(−πz/√(1−z²)), 16.3%
too far, and stepping by frames loses some of it.

`TinyBreakout`'s paddle has eyes, as in the talk, and each of the two
numbers of where they look is
a spring at 3 Hz and z = 0.5, pulled towards the direction of the ball:
they dart after it, go a little past, and settle. With `juice=off`
there are no eyes (`Juice.on`, for the juice a game draws itself), and
a follower is at its target at once.

## 8. In the playground

Someone writing a game never opens `juice/`. One value goes in the
model, `fx : Juice.t` -- the effects' clock and the effects under way --
stepped in `update` (`Juice.step computer fx`, which also sees the flag
`juice=off`). Then:

```ocaml
(* in the model: when the brick appeared, Juice.now fx at the time *)
let size = Juice.tween Juice.out_back 0. 1. 0.3 brick.born m.fx in
rectangle red 60. 20. |> scale size
```

and the brick grows from nothing in 0.3 s, overshoots by 10%, and
settles. A landing is the same, a start time in the model:

```ocaml
ball |> Juice.stretch (Juice.squash 0.4 0.5 ball.landed m.fx)
```

and `Juice.whiten`, while `Juice.during 0.08 hit m.fx`, draws the hit
flash. The effects that last are said once, in `update`, and play out:

```ocaml
let fx = if hit then m.fx |> Juice.shake 0.5 |> Juice.burst ~at:(x, y) Juice.sparks else m.fx in
...
Juice.view m.fx world   (* in view: the particles drawn, shaken, flashed *)
```

`Juice.mli` has three sections, as the code has three kinds: the
clock, the effects that are functions of it, and the effects that
last.

**Juice written by hand before.** `TinyStreetFighter` had its hitstop
(a counter) and its sparks (a list of stars) before this library, and
`TinyDefender` its smart bomb's flash (a counter, a white rectangle).
They keep them: that is the simple version, and the default. The flag
`juice=engine` does the same moments with `Juice` instead -- `freeze`
for the hitstop, a burst of `sparks`, `flash` -- plus a shake, and
`juice=off` does neither (`Juice.mode ~default:Hand flags` reads it).
The engine's code is a section of its own in each game, as
`physics=engine` and `ai=engine` are in other games. Since the
engine's freeze lasts the same 6 frames as the hand's counter, the
same keys play the same fight in both, and a golden frame of each
catches the same hit.

## Glossary

- **juice**: the effects that change how a game feels and not what it
  does (Jonasson and Purho, 2012).
- **easing curve**: a function from the time gone to the way gone, 0
  at 0 and 1 at 1.
- **in / out / in_out**: slow at the start, at the end, at both; `out`
  is `in` run backwards.
- **overshoot**: going past the end before settling (`back`,
  `elastic`).
- **tween**: a value going from one number to another over a time,
  along a curve (the animators' *in-between*).
- **lerp**: linear interpolation, a + (b − a)·p.
- **progress**: how far through a tween's time, clamped to [0, 1].
- **squash and stretch**: flattening on impact and lengthening after,
  the area kept (Disney's first principle).
- **hit flash**: the thing drawn all white for a frame or two when hit.
- **trauma**: how shaken things are, 0 to 1, added by hits and decaying;
  the shake is its square (Eiserloh).
- **value noise**: random values at whole numbers, smoothed between them.
- **hitstop**: the game frozen for a few frames at a hit, the effects
  going on.
- **the effects' clock**: the frames `Juice.step` has counted, which
  every effect reads instead of the wall clock.
- **particle system**: many small things each born, moving and dying
  by itself (Reeves, 1983); a **recipe** gives the bounds each one's
  numbers are drawn between, a **burst** is many born at once.
- **follower**: a value going after a target that moves; **critically
  damped**: a spring as fast as it can be without overshooting.
