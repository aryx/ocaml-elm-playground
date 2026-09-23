# Plan: what is left of juice/

`juice/` and `playground/Juice` are built (see
[`done/plan_juice_teaching.md`](done/plan_juice_teaching.md), its
Status the log of how), and eight programs use them: `JuiceCurves`,
`JuiceSquash`, `TinyBreakout`, `TinyTetris`, `TinyInvaders`, `Snake`,
`TinyFlappyBird` (juice on, `juice=off` dry), and `TinyStreetFighter`,
`TinyDefender` (`juice=hand|engine|off`). What is left, or known to be
missing, each item enough to start from.

## 1. `Juice3d`

Nothing for the 3D playground. The curves, `Follow`, `Hash` and
`Emitter` are dimension-free or nearly (`Emitter` is 2D: an x, a y, a
spin); what 3D needs is the view: particles as small cubes or
billboards in the scene, the shake as a camera offset (`Camera3d`), the
flash as a 2D overlay (the HUD). Start when a 3D game asks for it --
`TinyVirtuaFighter`'s hits, `TinyStarFox`'s explosions.

## 2. Particles in a scrolling world

`Juice.view` draws the particles and applies the shake in one place, in
whatever frame it is given. A game with a camera wants them apart: the
shake around the camera's view, on the screen, and the particles inside
it, in the world, so that debris stays where it was thrown while the
camera scrolls. That is why `TinyDefender`'s bombed landers throw no
debris. Likely a `Juice.particles fx : shape list` (the world's) and the
shake alone around the screen, or `Camera2d.shake` (the exercise its
`.mli` already lists), which phase 3 left out.

## 3. Games in Elm's message architecture

`Juice.step` takes the `computer` (for the flag and the step). The
author's `Tetris` and `Asteroid` are written with messages and a
`Tick` of the wall clock, no `computer` in `update`, so neither could
be juiced without restructuring. A `Juice.step_flags flags fx`, stepped
on each tick, would do; not needed until one of them wants juice.

## 4. Hitstop where goldens are scripted

`Juice.freeze` changes *when* things happen, so the games whose golden
tests replay keys at given frames (`TinyBreakout`'s 900-frame game, and
every scripted scene) were juiced without it. `TinyStreetFighter`'s
engine freeze keeps its hand counter's 6 frames, so its scripts still
land. A game that wants hitstop and a scripted golden has to write its
script against the frozen frames, and a `juice=off` run of it is then a
different game.

## 5. `Snake`'s eating, untested

`Snake`'s rules move the snake by the wall clock, which the golden
runner freezes: the snake moves once and never again, so there is no
golden frame of it eating (the burst and the gulp were checked on the
real clock). Counting frames instead would fix it; the rules are the
author's.

## 6. `Follow`'s response

t3ssel8r's second-order dynamics have a third number, the response,
which makes a follower anticipate -- start the wrong way, like `back`
among the curves. Left out; one more term in `Follow.chase`.

## 7. Presets for light backgrounds

`sparks` (white, yellow, orange) was tuned on `TinyStreetFighter`'s
evening sky and is faint on `TinyFlappyBird`'s pale blue; `debris c`
is one color, and a yellow bird's feathers vanish against it. A
palette argument, or presets per background, when a game needs it.

## 8. `notes_juice_related_work.md`

The plan named it as a companion and it was not written: the talks
(Jonasson and Purho, Nijman, Eiserloh, Swink's book), the engines'
tween and particle systems (Flash's Tween class, DOTween, Godot's
Tween and GPUParticles, Unity's Shuriken), the arcade games that did
it first (the hitstop of the fighting games, Defender's smart bomb),
and where this library's ceiling is.

## 9. The rest of the talk

`TinyBreakout`'s header lists what of "Juice it or lose it" it doesn't
have: a trail behind the ball (the last positions, fading), the
paddle's smile at each brick, music.
