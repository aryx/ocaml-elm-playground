# Plan: the playground API's other gaps

What the `Playground` API (and `Playground3d`) still lacks for the
games people want to make, beyond the four teaching areas (physics,
audio, networking: their own plans) and the teaching pieces of
[`plan_teaching_other.md`](plan_teaching_other.md). Each small, each an
API addition in Evan's spirit -- a few values and functions, no new
concepts where an old one works -- implemented on every backend.
Ordered by how soon games hit them.

## 1. Randomness, seeded and in the model

Evan's elm-playground has no randomness at all (Elm's `Random` needs a
command, which the playground hides); our games use OCaml's
`Random.self_init` (Snake, Tetris, StarCollector3d, Asteroid's
directions), which makes every run different: no golden frames for
them, no deterministic physics, no lockstep networking, no
time-travel debugger. It is phase 0 of
[`plan_inspect_teaching.md`](plan_inspect_teaching.md), which cannot
replay a run that is not deterministic.

The fix: a **seed** in the `computer` (or the model), and pure
functions from a seed to a value and the next seed -- Elm's `Random`,
without the command:

```ocaml
val random : number -> number -> seed -> number * seed   (* between a and b *)
val pick : 'a list -> seed -> 'a * seed
```

with `computer.seed` changing every tick from a seed chosen at start,
or given with `-seed n` (a new `Native_loop_3d` flag, like `-fixed-time`;
the golden tests pass it). The generator itself: `random/` in
`plan_teaching_other.md` item 4.

**Status**: half DONE. Runs are reproducible: the games drawing random
numbers take a `seed=n` flag (see `Playground.flags`), which seeds
OCaml's global `Random`, and the golden tests pass `seed=1` (Snake,
Tetris, StarCollector3d have golden frames). Left: the pure API above
(the seed in the model), needed only when replaying from a model
matters -- the time-travel debugger, rollback networking.

**Status (2026-09-23)**: the pure API is DONE, in the model rather
than the `computer` (no new field for every program to carry):
`Playground.seed`, `initial_seed`, `random`, `random_int`, `pick`, over
`random/Lehmer.mli` (Park and Miller's minimal standard, computed by
Schrage's trick so that native code and the browser draw the same
numbers, and a person's seed scrambled first: Lehmer's seeds 1 and 2
give related sequences). `Tetris.ml` is the worked conversion
(`tests/games/Unit_determinism.ml` checks it); the other games on the
global `Random` (Snake, TinyTetris, TinyBlockout, TinyWorms, Asteroid,
StarCollector3d, FloatingCity3d, TinyMinecraft) move to it one by one,
when each is next touched.

## 2. A camera for 2D: worlds bigger than the screen

The 2D playground has one screen, centered on (0, 0): a Mario level, a
Zelda map, a scrolling shooter don't fit. A **camera**, like the 3D
one: the part of the world the screen shows.

```ocaml
val camera : number -> number -> shape list -> shape list   (* look at (x, y) *)
val zoom : number -> shape list -> shape list
```

(or a field of the view's result), plus `computer.screen` still in
screen coordinates, and a way to put HUD shapes on top that don't move
(3D's `hud`, in 2D). Parallax (layers scrolling at different speeds)
is `camera` applied per layer.

**Status**: DONE, not in `Playground.mli` but as a layer on top of it,
`Camera2d.mli`: a camera record in the model, `view` (a
group scaled and moved), `to_world` for the mouse, `visible` for
culling, and one function per way of following the player (Keren's
"Scroll Back" GDC talk: `look_at`, `follow`, `window`, `clamp`), plus
`parallax`. The HUD is the shapes outside `Camera2d.view`. Unit tests
of the `.mli`'s worked examples in `playground/tests/`.

## 3. Tile maps

Most 2D games' worlds are **grids of tiles**: a level as rows of
characters, each a tile (`#` a wall, `.` floor, `?` a block), drawn
from a tile sheet, and collided with as a grid (much cheaper than
polygons: the physics plan's broad phase for free).

```ocaml
val tilemap : number -> (char -> shape) -> string list -> shape
```

A level becomes a string literal in the game's code -- readable, easy
to edit, no level editor needed (a level editor is a nice later
project, and a nice example game).

**Status**: DONE, `Tilemap.mli`, also a layer on top:
`of_strings`, `get`/`set`/`find` (a map is a value, changed when a coin
is taken), `center`/`cell`/`tile_at`, `view` and `view_visible` (only
the cells a `Camera2d.rect` touches), `hits` (box vs. grid). Both used
by `TinyMario.ml` (with a golden frame; its `camera=` and
`zoom=` flags compare the camera techniques).

## 4. Sprite sheets and animation frames

`image` draws a whole image. Games draw **parts** of one image (a sprite
sheet: all of Mario's poses in one file) and **animate** through them:

```ocaml
val sprite : string -> number -> number -> number -> number -> shape  (* src, x, y, w, h *)
val frames : shape list -> number -> time -> shape   (* cycle, frames per second *)
```

Mario today loads six GIFs from the network (`examples/Mario.ml`); with
a sheet, one local file (which also lets its golden frames exist: the
2D goldens exclude it because of the network).

**Status**: half DONE, as a layer on top, `Sprite.mli`:
pixel art typed as strings (drawn as rectangles, one per run of a
row's pixels), `flip`, and the animation frames (`cycle` by steps,
`frame` by time, for shapes or image urls). Left: sprite *sheets*,
which need `image` to draw part of an image -- a change to
`Playground.mli` and every backend.

## 5. Scenes: title, game, game over

Every game has a **title screen**, the game, a **game over**, often
levels and a pause. Today each game encodes that in its model by hand.
The Elm way needs no new API: a variant in the model
(`type scene = Title | Playing of game | Game_over of int`), and
`view`/`update` matching on it. So this is a **pattern to document**
(in the course, `plan_teaching_other.md` item 1) and a
`games/template.ml` showing it, not an API -- unless transitions
(fades between scenes) are wanted, then a small helper.

**Status**: DONE, `Scene2d.mli`: the variant stays the
game's, wrapped in a record keeping the time spent in the scene
(`elapsed`, `frames`, `blink`) and the previous frame's keyboard, for
keys `pressed` rather than held (without it, a space held on the title
skips the game over). Used by `TinyInvaders.ml`, with `Sprite`
and `Tilemap` (its eroding bunkers). Transitions wait for a group's
alpha in the renderers.

## 6. Input: touch, gamepads, text

- **Touch** on the web backend, for phones and tablets (where many
  young learners are): taps as mouse clicks is the minimum; a
  multi-touch `computer.touches` list for two-thumb games; an optional
  on-screen joystick for keyboard games.
- **Gamepads**: SDL's game controller API on native, the browser's
  Gamepad API on the web; a `computer.gamepad` with the sticks and
  buttons (mapped onto `to_x`/`to_y`/`kspace` when absent, so games work
  with either).
- **Text input**: typing a name for a high score; `computer.keyboard`
  has keys, not text (with shift, accents, IMEs).

## 7. Saving: high scores and saved games

A game can't remember anything between runs. A tiny key-value store:

```ocaml
val saved : string -> string option        (* read at start, in init *)
val save : string -> string -> ...         (* an effect: a Cmd, or a field *)
```

on a file in the user's directory on native, `localStorage` on the web.
The Elm-shaped question is how `update` asks for a write (a `Cmd`, which
the playground otherwise hides); the simplest answer is a `saves`
function beside `view`, like the audio plan's `sounds`: what should be
saved, from the model, written when it changes.

## 8. Text and fonts

- **Nicer text**: `words` in the software backend uses Hershey stroke
  fonts; a TrueType renderer (in `plan_2d_remaining.md`) for real
  typefaces, and `words` with a size, a font, bold.
- **Text layout**: multi-line text, alignment, wrapping -- for
  instructions, dialogs, a course's in-game explanations.

## 9. Smaller things

- **Screenshots and recordings**: a key saving the frame as a PNG
  (the golden machinery already writes them), or a GIF of the last
  seconds -- to share a game.
- **Full screen and resizing**: the window's size is fixed today
  (`computer.screen` exists for the web's resizing).
- **Performance hints**: when a game gets slow (too many shapes), a
  message saying so, and 3D's `cached3d` idea in 2D.
- **Accessibility**: color choices readable by color-blind players
  (a palette), keys remappable.

## Ordering

1 (seeded randomness) first: it's small, and physics, networking, the
debugger and the golden tests of half the games wait on it. Then 2-4
(camera, tile maps, sprites), which unlock the platformer and the
top-down adventure every learner wants to make; 5 is documentation;
6-7 when the web backend meets phones; 8-9 as they come.
