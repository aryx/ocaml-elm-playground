# Plan: the other teaching pieces (beyond graphics, physics, audio, network, AI)

The five teaching areas -- `graphics/` (done), `physics/`, `audio/`,
`ai/`, `network/` (planned: [`plan_physics_teaching.md`](done/plan_physics_teaching.md),
[`plan_audio_teaching.md`](plan_audio_teaching.md),
[`plan_ai_teaching.md`](plan_ai_teaching.md),
[`plan_networking_teaching.md`](plan_networking_teaching.md)) -- teach
how a game *runs*, to someone reading the implementation. What's
missing is on two sides: the **learner's** side (someone learning to
program with the playground, who will never open `graphics/`), and a
few more **game-making areas** worth teaching the same way. Ordered by
value; each item is a sketch, to become a plan of its own when started.
The playground API's own gaps (camera, tile maps, input, saving...)
are in [`plan_playground_other.md`](plan_playground_other.md).

## 1. The beginner's path: a course

**The biggest gap.** Every note so far (`notes_2d.md`, `notes_3d.md`,
`notes_2d_physics.md`, ...) is for someone reading the code of the
engine. Nothing yet is for someone *learning to program* with the
playground, which is what Evan's elm-playground was for.

A course, in lessons, each short, each ending with something to play
with and a few exercises:

1. **Pictures**: shapes, colors, `move`, `rotate`, `group` -- a face,
   a house, a flag.
2. **Animations**: time, `spin`, `wave`, `zigzag` -- a clock, a solar
   system.
3. **Games**: the model, `update`, the keyboard and mouse -- a ship
   that moves, then a small game (Pong).
4. **Physics**: bodies, gravity, collisions -- Asteroid, Slingshot.
5. **Sound**: beeps and music -- the games, heard.
6. **3D**: the same ideas, one dimension up.
7. **Two players**: the multiplayer API.

The model: *How to Design Programs* (pictures, then worlds, with a
design recipe), *The Nature of Code* (one idea per chapter, each
visual), Elm's guide (the architecture, gently). Written for the
browser (item 2), with every example runnable in the page. Where:
`docs/course/`, published on the GitHub Pages site next to the
examples.

## 2. Programming in the browser, no install

Installing OCaml, opam, dune, SDL and Cairo is the real barrier for a
beginner -- more than any concept. A page where you edit a game on the
left and see it run on the right, like elm-lang.org/try, Ellie for
Elm, the p5.js editor, or the OCaml playground:

- js_of_ocaml can compile the OCaml **toplevel** (the compiler itself)
  to JavaScript, so the page compiles and runs the code, with the web
  backend (SVG, WebGL, Web Audio) and the playground's libraries
  preloaded;
- errors shown next to the code, in words a beginner can act on (a
  second, real project: OCaml's type errors are notoriously hard to
  read -- see Elm's error messages, the reference);
- sharing a game as a link (the code in the URL, or a small paste
  service).

Size and startup time are the risks (the toplevel is several
megabytes); to measure first.

## 3. A time-travel debugger, and seeing the engines

Started, so it is a plan of its own:
[`plan_inspect_teaching.md`](plan_inspect_teaching.md)
(`playground/Inspect`: the per-engine panels -- physics contacts, the
broad phase's grid, A*'s frontier, the audio oscilloscope, overdraw --
and the recorded run that pauses, steps, rewinds and scrubs, with
Victor's trail of a sprite's past and future). The tutorial is
[`notes_inspect.md`](../tutorials/notes_inspect.md), the related-work
note
[`notes_inspect_related_work.md`](../related-work/notes_inspect_related_work.md).

What this section used to list -- pause and step, a slider over the
frames, seeing the model, replaying after a change, exporting a run as
a test -- are its phases 3 to 6; live code editing, which needs the
browser toplevel of section 2 above, stays out of it, and the plan
says what it would cost.

## 4. Randomness and procedural generation

A teaching area of its own, `random/` in the same style as
`graphics/` (one idea per module, diagrams, worked examples, papers,
tests):

- **Pseudo-random generators**: linear congruential (Lehmer, 1949),
  xorshift (Marsaglia, 2003), the NES's LFSR; seeds, periods, and why
  `Random.self_init` breaks reproducibility; testing randomness (and
  why the eye is bad at it).
- **Noise**: value noise, Perlin noise (Ken Perlin, 1985: an Academy
  Award for the film industry), simplex noise (2001); octaves (fractal
  noise) -- terrain, clouds, textures. TinyMinecraft's world, generated
  instead of hand-placed.
- **Generation**: mazes (recursive backtracking, Kruskal's, Prim's --
  and they're minimum spanning trees), cellular automata (Conway's Game
  of Life, 1970; caves from automata), L-systems (Aristid
  Lindenmayer, 1968: plants), dungeon rooms, Wave Function Collapse
  (Maxim Gumin, 2016).

Examples: Life, a maze and its solver (with item 5), a noise terrain
in 2D and in 3D, a growing plant. References: *The Algorithmic Beauty
of Plants* (Prusinkiewicz and Lindenmayer, 1990), Knuth's *TAOCP*
vol. 2 on random numbers, the *Procedural Content Generation in Games*
book (Shaker, Togelius, Nelson, 2016).

## 5. Game AI -- started, and now a plan of its own

`ai/`, the same style: small, classic, very visual algorithms, each
with its paper, each drawn while it runs. The sketch that was here
(pathfinding, steering and flocking, state machines, game-tree search,
and Monte Carlo as the modern twist) became
[`plan_ai_teaching.md`](plan_ai_teaching.md) once the first two
modules were written -- `ai/Minimax` (with `examples/AiTictactoe.ml`
and `games/AiOthello.ml`) and `ai/Pathfind` (with
`examples/AiPathfinding.ml`, and `gamekits/rts/Orders` over it). That plan
also adds what the sketch didn't have: **learning** -- a neural
network from scratch, trained while you watch, and behind a Monte
Carlo search on a 9x9 Go board. The notes are
[`notes_ai.md`](../tutorials/notes_ai.md),
[`notes_ai_learning.md`](../tutorials/notes_ai_learning.md) and
[`notes_ai_related_work.md`](../related-work/notes_ai_related_work.md).

## 6. Smaller teaching ideas

- **A "how it works" mode for the examples**: every example with a
  button (or key) showing the code of the idea it demonstrates, next to
  it -- the notes' diagrams, live.
- **Exercises with tests**: each lesson's exercises checked by golden
  frames or small property tests the student runs (`make test` for
  learners).
- **The notes as a book**: `notes_2d.md`, `notes_3d.md` and the future
  physics/audio notes, edited into one "how a game engine works, from
  scratch" book (the syncweb/literate style of the author's other
  projects is an option: the code and its explanation in one
  document).

## Ordering

1 and 2 go together (the course lives in the browser editor) and are
the largest; 3 is the most distinctive, and needs the deterministic
groundwork of the physics and networking plans (seeded randomness, the
fixed step); 4 is independent and can start any time (and 5, now
[`plan_ai_teaching.md`](plan_ai_teaching.md), already did -- its
seeded randomness is the same groundwork 4 would give it).
