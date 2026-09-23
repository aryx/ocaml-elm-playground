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
| `playground/Juice` (done: tweens, squash, stretch, whiten) | the Evan-style API | §4, §8 |
| `examples/JuiceCurves` (done) | every curve, plotted and played | §1 |
| `juice/Squash` (done) | squash and stretch | §4 |
| `examples/JuiceSquash` (done) | a ball dry, squashed, and flashed | §4 |
| `juice/Trauma` (planned) | screen shake | §5 |
| `juice/Emitter` (planned) | particles | §6 |
| `juice/Follow` (planned) | a value that follows a target | §7 |

§1 to §4 are things that are a function of time and nothing else.
§5 to §7 need a little state in the model.

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

## 5. Screen shake (planned, `juice/Trauma`)

Squirrel Eiserloh's lesson ("Juicing Your Cameras With Math", GDC
2016): what decays is *trauma*, and the shake is trauma², so small hits
barely move the screen; and the offset is read from smooth noise, not
drawn at random each frame, which jitters instead of shaking.

## 6. Particles (planned, `juice/Emitter`)

William Reeves, "Particle Systems" (SIGGRAPH 1983, the Genesis effect
of *Star Trek II*): things born at a rate, each with a life, moving by
a fixed step, dying. Seeded, so a burst is the same burst every run,
and capped, since each is a shape to draw.

## 7. Follow: a value with a spring (planned, `juice/Follow`)

Second-order dynamics (t3ssel8r, 2022; the smooth-damp of every
engine): a frequency, a damping, and a state stepped each frame --
what a camera, a health bar or a pair of eyes needs when its target
moves.

## 8. In the playground

Someone writing a game never opens `juice/`. They write:

```ocaml
(* in the model: when the brick appeared *)
let size = Juice.tween Juice.out_back 0. 1. 0.3 brick.born computer in
rectangle red 60. 20. |> scale size
```

and the brick grows from nothing in 0.3 s, overshoots by 10%, and
settles. `Juice.tween` takes the `computer` rather than its time, like
nothing else in the playground: it needs the flags too, so that
`juice=off` puts every tween at its end at once. `Juice.curve` reads a
curve directly, to draw it. A landing is the same, a start time in
the model:

```ocaml
ball |> Juice.stretch (Juice.squash 0.4 0.5 ball.landed computer)
```

and `Juice.whiten` draws the flash. The effects that need state (§5 to
§7) will be a second section of `Juice.mli`, a value kept in the
model.

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
