# Plan: game AI, from scratch, for teaching (`ai/`)

## Context

`graphics/` teaches how pictures are computed, `physics/` how motion
is, `audio/` how sound is; this plan does the same for **deciding**: a
small collection of classic game-AI algorithms under `ai/`, each one
idea, each visual, each with its paper -- and, like `Playground` for
pictures and `Physics` for motion, a small **Evan-style API** over them
(`playground/Ai.mli`), so that a game says *what an enemy wants*, not
how the search works.

It has already started: `ai/Minimax` (with `examples/AiTictactoe.ml`
and `AiOthello.ml`) and `ai/Pathfind` (with
`examples/AiPathfinding.ml`, and `gamekits/rts/Orders` over it, which is
how TinyDune2's and TinyWarcraft2's units walk). This plan is written
after those two, to say where the rest goes: the real-time side
(steering, flocking, state machines), **bots** -- an AI that plays a
game the way a player does, through the same inputs, which is what
almost every game in this repository actually needs and what three of
them have already hand-written -- the deeper search (iterative
deepening, transposition tables, Monte Carlo), and the part nobody
else in this repository teaches yet -- **learning**: a neural network
written from scratch, trained while you watch, and finally put behind
a Monte Carlo search on a 9x9 Go board, which is the shape of AlphaGo
at a laptop's size.

Companions: [`notes_ai.md`](../tutorials/notes_ai.md), the tutorial
(written ahead of the code, as its specification),
[`notes_ai_learning.md`](../tutorials/notes_ai_learning.md) (the
networks, from one neuron to self-play), and
[`notes_ai_related_work.md`](../related-work/notes_ai_related_work.md)
(the games, the champions, the engines and libraries, and the teaching
lineage).

The sketch this plan grew out of is section 5 of
[`plan_teaching_other.md`](plan_teaching_other.md).

## Principles (the same as `graphics/`, `physics/` and `audio/`)

The shared list is in [`../README.md`](../README.md), with the long
form in [`../guide-principles.md`](../guide-principles.md); restated
here in `ai/`'s own terms, with the one it added (the last).

- **Independent of the Playground.** `ai/` knows states, moves, nodes,
  costs, points and vectors: no `computer`, no `shape`, no SDL.
  `playground/Ai.ml` is the adapter, as `Physics` is for `physics/`.
  Pure OCaml with no dependency, so the web backend has all of it too
  -- which matters for the networks: no BLAS, no C stub, no GPU.
- **Computed, not authored.** As a circle needs no image file, a
  ghost needs no scripted path: its behaviour comes out of a rule you
  can read. The point of the examples is that the *rule* is short and
  the *behaviour* is not.
- **One idea per module, the simple and the better version side by
  side**, switchable, so the difference can be **watched**: minimax
  against alpha-beta (the same move, a fraction of the nodes),
  breadth-first against A* (the same path, a fraction of the cells),
  random playouts against UCT, backpropagation written by hand against
  the same derivatives from autodiff.
- **Every `.mli` explains its idea** with a diagram, a worked example
  with numbers, and its references; `ai/tests/` checks the examples --
  the node counts, the path costs, the gradients.
- **Deterministic, so testable.** Monte Carlo playouts, a wandering
  enemy and a network's initial weights are all random; every one of
  them takes its randomness from an explicit seed, so a run repeats,
  golden frames work, and a test can assert *exactly* 18,297 nodes.
- **Honest about scale.** A laptop, pure OCaml, no GPU. A chess engine
  of a few hundred elo; a 9x9 Go player at a weak amateur's level; a
  network of a few thousand weights. Each `.mli` says what its idea
  reaches and what it doesn't -- the teaching is in the shape of the
  algorithm, not in its strength, and pretending otherwise is the one
  thing that would make this dishonest.
- **Comments describe the code as it is**; the long explanations live
  in the `.mli`s and the notes.

## Three kinds of mind, kept apart

Game AI is three different subjects that share a directory:

```
   the opponent              the agent                 the bot
   a turn-taking game        a world at 60 fps         a world at 60 fps
   "what move do I play?"    "where do I go?"          "what would a
                                                        player press?"
   Minimax, Mcts,            Pathfind, Steering,       Bot, Sense, over
   Deepening                 Flock, Fsm                all of the agent's
   seconds to think, once    a fraction of a frame     a frame, and a
                                                        reaction delay
   AiTictactoe, AiOthello,   TinyPacman's ghosts,      TinySoldat's two
   AiConnect4, AiChess,      TinyDune2's units, a      soldiers, TinyPong's
   AiGo                      flock, TinyZelda's        paddle, TinyMicro-
                             monsters                  Machines' rivals
```

The difference between the middle and the right column is not the
algorithms -- a bot *uses* steering, pathfinding and state machines --
it is **what it is allowed to touch**. A ghost may be a rule over the
game's state: it is a ghost, it has no hands. A bot stands where a
player stands, so it should press what a player could press, know what
a player could know, and take the time a player takes. That
restriction is the whole subject, and it is what the next section is
about.

They share three things, and it's worth knowing which: the **debug
overlay** (all three want to draw what the computer is thinking),
**learning** (a network can be an opponent's evaluation function, an
agent's policy or a bot's aim), and the fact that all three must be
**seeded** rather than globally random. Everything else is separate,
and the notes are split the same way.

## Bots: playing the game the way a player does

Three games in this repository have already written the same thing by
hand, independently, which is the usual sign that a layer is missing:

- `TinySoldat.ml` has a record `intent` (`run`, `jump`, `jet`,
  `shoot`, `grenade`, `aim`) and **two functions that fill it**:
  `human computer scenes p` from the keys and the mouse, and
  `bot p i` from the world. The update loop does not know which is
  which. Its bot already does the honest things: line of sight by a
  swept test against the map (`Physics.went_through`), an aim that
  wobbles by a sine (no `Random`, so it stays deterministic), keep
  your distance rules, and grenades at what it cannot see.
- `gamekits/racing/Topdown.mli` has `Topdown.computer track car`, which
  returns **a `(gas, steer)` pair** -- exactly the two numbers the
  player's keys produce -- by steering at the next waypoint. That is a
  bot living in a genre kit, and `TinyMicroMachines.ml` drives
  its rivals with it.
- `TinyPong.ml`'s `computer_player` moves the right paddle
  towards the ball at a limited speed. The limit *is* the difficulty,
  and it is the smallest possible example of the whole idea.

So the shape is already known, and the plan's job is to name it:

```
   the world  -->  Sense   -->  the mind    -->  intent     -->  the game's
   (the model)     what a       steering,        the same         update:
                   player       fsm,             record the       the same
                   could know   pathfind, aim    keys fill        for both
```

**`ai/Bot` is the two middle arrows**: a decision made from senses and
turned into the game's own input record, with the knobs that make a
bot *fair* rather than strong (below).
**`ai/Sense` is the left arrow**: what the bot is allowed to know --
line of sight, hearing range, and a memory of where the target was
last seen, which is what makes a bot look like it is searching for you
rather than tracking you through a wall.

### Difficulty without cheating

The lazy way to make a bot hard is to give it more: more speed, more
damage, the player's exact position through a wall. It is also the way
that makes a bot *unfun*, and every one of the references below says
so. The honest knobs, each a number in `Bot`:

- **reaction delay**: the bot acts on a state a few frames old (a
  human's is about 250 ms, i.e. 15 frames at 60 fps -- to check);
- **aim error**: an offset that decays as the target stays visible,
  so the bot "settles" on you like a hand does;
- **input rate**: how often it may change its mind, and what it may
  press at once (a bot that fires on the exact frame a target appears
  is recognisably a machine);
- **what it knows**: the `Sense` filter above, which is a *type*, not
  a promise -- if `bot : senses -> intent`, a bot that peeks at the
  whole world does not compile.

The last one is the reason to have `Sense` at all, and the only piece
of this that is really a design decision rather than a routine.

### Where bots live: `ai/`, the genre kits, and the `gamekits/bots/` question

Whether there should be a `gamekits/bots/` is worth answering explicitly,
because the evidence above cuts both ways. The proposal:

- **The mechanism goes in `ai/`** (`Bot`, `Sense`): the intent loop,
  the delay, the aim error, the memory. It is Playground-independent
  and genre-independent, it is what all three hand-written bots
  duplicate, and it belongs beside `Steering` and `Fsm`, which it is
  built on.
- **The knowledge stays in the genre kit**, where `Topdown.computer`
  already is: a racing line and waypoints mean nothing outside
  `gamekits/racing/`, a shooter's cover and weapon choice nothing outside
  a shooter, a fighting game's spacing nothing outside
  `gamekits/brawler/`. A kit is organised by genre here, and a bot is
  mostly genre knowledge.
- **So: no `gamekits/bots/` at first.** It would collect things that have
  nothing in common but the word "bot", and the one thing they *do*
  have in common is going into `ai/` anyway. The case to revisit it is
  concrete and worth watching for: **a deathmatch bot wanted by both a
  2D and a 3D shooter** (`TinySoldat.ml` and a 3D one on
  `Character3d`, see
  [`plan_physics3d_teaching.md`](done/plan_physics3d_teaching.md)) would
  share map awareness, waypoints, cover and weapon choice across two
  genres' kits -- and *that* is the day `gamekits/bots/` (or a
  `gamekits/shooter/`) earns its place, with the shared part moved into it
  and this paragraph replaced by the reason it happened.

### 2D and 3D from the same layer

A bot's mind is dimension-independent; only the senses and the intent
change. In 2D the senses are `Physics.went_through` against the map
and the intent is the game's key record; in 3D the senses are
`Collide3d`'s rays and the intent drives a `Character3d` capsule
(tutorial §14 of
[`notes_3d_physics.md`](../tutorials/notes_3d_physics.md)) -- the same
`Bot` with a different `sense` function and a different `intent`
record. Both are one type parameter apart, which is the test of
whether this layer is the right one: if `Bot` needs to know about
pixels, or about `y` being up, it is in the wrong directory.

## The Playground API, Evan-style

`playground/Ai.mli`, a few small families, each a one-liner in a game.
Tentative, to be refined by writing the games with it -- the same way
`Physics.mli` was.

**1. Steering, as forces on physics bodies.** Reynolds's behaviours are
accelerations, and `Physics` already takes accelerations (`fall`,
`thrust`, `push`), so they compose with everything already there:

```ocaml
val seek : number -> number -> body -> body     (* steer toward a point *)
val flee : number -> number -> body -> body
val arrive : number -> number -> body -> body   (* and slow down at the end *)
val chase : body -> body -> body                (* aim where it will be *)
val escaping : body -> body -> body
val wandering : body -> body                    (* a believable idle *)
val avoiding : body list -> body -> body
val flocking : body list -> body -> body        (* the three boid rules *)
val following : (number * number) list -> body -> body   (* a path *)
val steer_speed : number -> body -> body        (* how hard it may turn *)
```

so that a fish is one line:

```ocaml
let update _ fish = fish |> List.map (fun f -> f |> flocking fish |> step)
```

**2. Ways through a map**, over `Tilemap`, hiding the `problem` record:

```ocaml
val way : walkable:(int * int -> bool) -> (int * int) -> (int * int) -> (int * int) list
val way_cost : cost:(int * int -> number) -> (int * int) -> (int * int) -> (int * int) list
val flow : walkable:(int * int -> bool) -> (int * int) -> flow   (* one search, a crowd *)
val next_step : flow -> (int * int) -> (int * int) option
```

**3. An opponent for a turn game**, hiding the search:

```ocaml
type ('state, 'move) rules = { moves : ...; play : ...; score : ...; my_turn : ... }

val thinking_ahead : int -> ('state, 'move) rules -> ('state, 'move) opponent
val playing_out : int -> ('state, 'move) rules -> ('state, 'move) opponent  (* Monte Carlo *)
val within : number -> ('state, 'move) opponent -> ('state, 'move) opponent (* seconds *)
val best_move : ('state, 'move) opponent -> 'state -> 'move option
val thoughts : ('state, 'move) opponent -> 'state -> ('move * number) list  (* to draw *)
```

**4. A mind for an enemy**, the state machine in the shape a game wants:

```ocaml
val mode : ('mode * (computer -> 'model -> bool)) list -> 'mode -> computer -> 'model -> 'mode
```

**5. A bot**, which fills the game's own input record instead of the
player (the section above; `'intent` is the game's type, not ours, and
`'senses` is what the bot is allowed to know):

```ocaml
type ('senses, 'intent) bot

val bot : ('senses -> 'intent) -> ('senses, 'intent) bot
val reacting_in : int -> ('senses, 'intent) bot -> ('senses, 'intent) bot   (* frames *)
val aim_error : number -> ('senses, 'intent) bot -> ('senses, 'intent) bot
val skill : number -> ('senses, 'intent) bot -> ('senses, 'intent) bot      (* 0..1, sets both *)
val thinks : ('senses, 'intent) bot -> 'senses -> 'intent                   (* once a frame *)

(* the senses, over Physics/Collide3d, so a bot knows what a player could *)
val can_see : body -> body -> body list -> bool
val last_seen : ...   (* the memory that makes it search rather than track *)
```

so that a game's update keeps one line for both kinds of player:

```ocaml
let intent = if s.human then human computer p else Ai.thinks (bot_of i) (senses p i) in
```

### The open question: a board game as a way of programming

`AiOthello.ml` (257 lines) and `AiTictactoe.ml` (183) hand-write the
same loop: a cursor moved by the arrows or the mouse, a click that is
legal or isn't, the computer's answer, a pass, an end, a restart, and
the "what does it think of my moves" overlay. Only the rules and the
drawing differ. That is the same observation that made `Puzzlescript`,
`Bigbang` and `Logo`: a **way of programming** that builds the `app`
for you.

```ocaml
val board_game :
  rules:('state, 'move) rules ->
  square:(computer -> 'state -> int * int -> shape) ->
  opponent:('state, 'move) opponent ->
  'state -> ('state, msg) app
```

Then AiChess and AiGo are their rules and their squares, and nothing
else -- which is the only way a chess game stays readable. **To
decide** by writing AiConnect4 with it first (a third game is the
earliest point at which the shape is really known, not guessed).

**Decided, 2026-09-22: no.** Written and measured, as a kit
(`gamekits/board/`, beside `Cards` -- it is one genre's machinery, not
a borrowed way of programming, so it did not belong in `playground/`),
and AiConnect4 was ported onto it and played correctly. The numbers:
305 lines to 292. Thirteen lines, for a kit of 156 plus 87 of
interface, because the loop is not where these games spend their
lines -- the rules, the evaluation and the teaching overlays are --
and because a game that hands its model over has to hand back what it
took away: a second `rules` record (its `Minimax.game` is over
positions, the kit's `Ai.rules` over the whole game record), a `place`
helper duplicating the kit's own geometry, and its own cursor drawing,
since no two of these games draw a cursor the same way (Connect 4
holds the piece above the column, Go puts a ghost stone on the point).
And the two biggest games do not fit at all: AiChess's move is two
clicks, a piece and then its square, which is a second cursor the kit
has no word for; AiGo's opponent is MCTS with its *own* playout (the
eye rule, without which the playouts mean nothing), which
`Ai.playing_out` cannot take, so it would have wanted a third
`machine` variant -- and the kit was already growing one variant per
game. The sentence above ("their rules and their squares, and nothing
else") is the thing the experiment disproved. What the loop really is:
thirty lines a game, readable, and the part of it each game does
differently is the part that gives it its character. The layer's four
families (§ The Playground API) remain, and are what the games
actually wanted from this.

## Target layout

```
ai/                    (private, package elm_playground, pure OCaml:
                        every backend, the web one included)
  Pathfind      DONE  breadth-first, Dijkstra, A*, flow fields
  Minimax       DONE  minimax and alpha-beta
  Deepening           iterative deepening, move ordering, a node budget,
                      a resumable search (a frame's worth at a time)
  Zobrist             hashing a position; the transposition table
  Mcts                random playouts, then UCT: search without an
                      evaluation function
  Steering      DONE  seek, flee, arrive, pursue, evade, wander, avoid,
                      path following
  Flock         DONE  separation, alignment, cohesion
  Fsm           DONE  states and transitions; Pac-Man's four ghosts
  Sense               what a bot is allowed to know: line of sight,
                      hearing, a memory of where the target was
  Bot                 the player's inputs, filled by a machine: the
                      intent loop, reaction delay, aim error, skill
  Behavior      DONE  behavior trees: sequence, selector, decorator
  Utility       DONE  scoring the options instead of branching
  Influence           an influence map: whose ground is this
  Matrix              small dense matrices, the naive loops (and a
                      faster version beside them, like graphics/Opti)
  Neuron              the perceptron and its learning rule; and XOR,
                      the thing it cannot learn
  Net                 layers, activations, the forward pass
  Backprop            the loss, gradient descent, the chain rule by hand
  Grad                reverse-mode autodiff: the same derivatives, once
  Train               batches, learning rate, train/test, the loop
  Qlearn              rewards, temporal difference, Q-learning; self-play
ai/tests/             the worked examples; node counts; path costs;
                      gradients against finite differences
playground/Ai.ml      the Evan-style API above
playground/Ai_debug.ml  the overlay (Playground shapes, any backend)
scripts/train/        the offline training runs (native, minutes), their
                      output a small weights file
```

Keeping `ai/` flat, like `audio/`, until it passes ~15 modules; if it
is ever split (`ai/search/`, `ai/agents/`, `ai/learn/`), that is a move
of existing modules, so it needs the author's go-ahead first, not this
plan's.

## Groundwork decisions

### Thinking inside a 16 ms frame

The searches here can take a second; a frame has 16 ms. Three answers,
and each module says which it offers:

- **A budget, not a depth**: `~nodes:n` or `~within:seconds`, with
  iterative deepening (search 1 move ahead, then 2, then 3, until the
  budget is gone, keeping the last finished answer). This is also what
  makes move ordering pay: the previous depth's best move, tried first
  at the next depth, is what gets alpha-beta near its b^(d/2).
- **Resumable**: `start` / `step`, a frame doing a thousand nodes and
  the next one continuing. This costs a little in clarity and buys two
  things: a turn-taking game that doesn't freeze, and a search that can
  be *drawn while it runs* -- which `AiPathfinding.ml` already does by
  replaying a finished search's `visited` list, and would do live.
- **Neither**: the small games (tic-tac-toe searched to the end) just
  think in one frame and drop it. Saying so in the `.mli` is enough.

### Randomness, and why it is a seed

Playouts, wander, a network's initial weights, a self-play game: every
one is random, and every one has to repeat, or the golden frames and
the tests go. So no global `Random`: a seed is threaded explicitly
(`~seed:int`), and `ai/` carries a tiny xorshift of its own until the
planned `random/` ([`plan_teaching_other.md`](plan_teaching_other.md)
item 4) exists, then uses that.

### Drawing what the computer thinks

The best part of game AI is that all of it is visible, and none of it
is visible by default. `playground/Ai_debug.ml`, like `Audio_debug`
(and, once it exists, as one panel of
[`plan_inspect_teaching.md`](plan_inspect_teaching.md)'s `Inspect`,
which owns the key, the layout and the cycling so every engine's
panel behaves the same):
Playground shapes, so any backend can draw them, on the "v" key with
`-debug-keys`:

- the frontier in the order it was taken, and the path (as
  `AiPathfinding` already draws);
- the flow field as arrows on the tiles;
- each steering force as a vector on its body, and the wander circle;
- each enemy's current state, written over it;
- the search's top moves with their values (as `AiOthello`'s "v"
  already does), and the tree's shape at depth 1 and 2;
- a network's weights as a grid, and its loss as a curve.

### Where the learning happens

**Live, in the examples, by default.** XOR, a spiral, and our own
digits are small enough to train in seconds at 60 fps, and watching the
decision boundary move *is the lesson* -- the same trick as
TensorFlow Playground, which this project shares a name with by
accident.

**Offline for anything bigger** (`scripts/train/`, native, minutes),
its output a small text file of weights committed beside the game. No
data downloads, ever: the digit dataset is **generated from our own
Hershey font** (`graphics/font`) at random sizes, rotations and noise,
which is self-contained, honest about what it tests, and a nice loop --
the renderer teaching the network.

### No BLAS, and one honest optimisation

`Matrix` is float arrays and three nested loops. Then, beside it, the
faster version (blocking, unrolling, avoiding the transposes), with the
numbers in the `.mli`, exactly as `graphics/Opti` does for pixels. A
learner should see that a matrix multiply is nine lines before seeing
that it can be made four times faster.

## The modules, with their references

(To double-check against the sources when writing each `.mli`; dates
from memory until then.)

- **Pathfind** (done): Dijkstra (1959); Hart, Nilsson and Raphael
  (1968) for A*; Amit Patel's Red Blob Games pages, the clearest
  pictures of all of it.
- **Minimax** (done): von Neumann (1928); Shannon, "Programming a
  Computer for Playing Chess" (1950), which is also where "type A" (all
  moves, fixed depth) and "type B" (the plausible ones, deeper) come
  from; Knuth and Moore (1975) for the analysis of alpha-beta.
- **Deepening**: iterative deepening (Slate and Atkin's Chess 4.5,
  1977); the killer-move and history heuristics; quiescence search
  (search on until nothing is being captured -- the cure for the
  horizon effect, Berliner, 1973).
- **Zobrist**: Albert Zobrist, "A New Hashing Method with Application
  for Game Playing" (1970): one random number per (piece, square), the
  position's hash their xor, so a move updates it in two xors.
- **Mcts**: Rémi Coulom (2006, Crazy Stone) for Monte Carlo in Go;
  Kocsis and Szepesvári (2006) for UCT, which is UCB1 (Auer, Cesa-Bianchi
  and Fischer, 2002) applied to a tree; Browne et al., "A Survey of
  Monte Carlo Tree Search Methods" (2012).
- **Steering**: Craig Reynolds, "Steering Behaviors For Autonomous
  Characters" (GDC 1999). Its warning came from
  `TinyBoomerangFu.ml`, whose characters have no velocity (a fixed
  speed, a committed dash), so a steering API that is only forces on
  `Physics` bodies would have nothing to offer them; hence both forms,
  a force and a direction. **Settled, 2026-09-22**: that game's
  `ai=engine` bot does *not* use `Steering` after all. Its three ways
  of walking are a direction each, which `Steering.direction` would
  wrap without simplifying, and the one piece worth sharing --
  `clear_way`, last frame's way kept until it is itself blocked -- is
  an obstacle check, not a steering behaviour. It uses `Sense`, `Bot`
  and `Fsm`, which is where its difficulty actually lives.
- **Flock**: Craig Reynolds, "Flocks, Herds and Schools: A Distributed
  Behavioral Model" (SIGGRAPH 1987) -- three rules, and the birds of
  *Batman Returns*.
- **Fsm**: Pac-Man (1980) and its four ghosts, whose chase/scatter
  timing and per-ghost target tiles are documented down to the frame in
  the Pac-Man Dossier (Jamey Pittman, 2009). Second user, DONE:
  `TinyBoomerangFu.ml` with `ai=engine`, whose computer is three states
  in all but name -- dodge what is in the air, hunt while it holds its
  boomerang, keep away while it does not -- and whose hardest lesson is
  that the states need *hysteresis*, or the agent flips between two
  every frame and goes nowhere. On `Fsm` that lesson is a guard
  (`since >= 8` before leaving Dodge) instead of a condition buried in
  an if, which is the clearest case in this repository for the module
  existing at all.
- **Behavior**: behavior trees, Halo 2 (Damian Isla, GDC 2005); the
  successor everyone copied, and its costs (the blackboard).
- **Bot, Sense**: the deathmatch bots, which are where this was worked
  out -- the Reaper and Eraser bots for Quake and Quake II (1997-98,
  community-written, when the games shipped without any); **the Quake
  III Arena bot** (Jan Paul van Waveren, "Mr. Elusive", master's
  thesis, 1999-2001): its Area Awareness System is the canonical
  answer to "what does a bot know about the map", and its fuzzy-logic
  weapon choice and per-bot "characters" to "why does this one feel
  different"; **the Counter-Strike bot** (Michael Booth, GDC 2004),
  whose navigation mesh was *learned by watching players walk*, and
  whose stated goal was bots that are fun rather than strong.
  Alongside them, the honest-difficulty practice every game has:
  rubber-banding in racing games (Mario Kart the famous case, and
  `TinyMarioKart64.ml`'s `rubber` and `roll`: the computer's
  karts drive faster when the player is ahead, and an item box hands
  out what your place needs), and
  fighting-game opponents that read the player's inputs -- the thing
  players can feel and resent ("SNK boss syndrome"). The 2010 Google
  AI Challenge, whose game was Tron, is a nice small corpus of bots
  that play on exactly the same inputs a player has
  (`TinyTron.ml`'s header already points at it). (Names and
  dates from memory, to check.)
- **Utility**: utility AI, The Sims (1997-2000) -- needs scored, the
  highest wins; Dave Mark's "Behavioral Mathematics for Game AI" (2009).
- **Influence**: influence maps (Andrew Zobrist's Go program, 1969 --
  the same Zobrist). **Not written, and no phase ever claimed it**:
  the target layout lists it and the phases do not, which is how it
  came to be the one module of that list with nothing behind it. It is
  a small thing -- `Pathfind.field` run from every enemy at once, read
  as danger per tile rather than distance -- and the game that would
  want it is a strategy game deciding where not to walk. Left as the
  first thing to pick up after phase 13.
- **Neuron**: McCulloch and Pitts (1943); Rosenblatt's perceptron
  (1958) and its learning rule; Minsky and Papert, *Perceptrons*
  (1969), and XOR.
- **Net**, **Backprop**: Rumelhart, Hinton and Williams (1986); the
  worked example in the `.mli` done by hand with two inputs and two
  hidden units, so the numbers can be checked with a pen.
- **Grad**: reverse-mode automatic differentiation (Linnainmaa, 1970);
  Karpathy's micrograd (2020) as the clearest small implementation of
  the idea.
- **Qlearn**: Samuel's checkers player (1959: the first program to
  learn a game by playing itself); Sutton's temporal-difference
  learning (1988); Watkins's Q-learning (1989); Tesauro's TD-Gammon
  (1992), which learned backgammon to world class from self-play with
  a network of the size we can afford; Sutton and Barto,
  *Reinforcement Learning: An Introduction*.
- **The two joined**: Silver et al., AlphaGo (2016) and AlphaGo Zero
  (2017) -- MCTS guided by a network, the network trained on the
  search's own results. The shape is small; the compute was not.

## New examples

- `examples/AiSteering.ml`: one creature, one key per behaviour (seek,
  flee, arrive, wander, pursue, avoid), the force drawn as a vector.
  The point: the same body, six characters.
- `examples/AiFlock.ml`: the three rules with three sliders, and each
  one switchable off -- separation alone is a gas, cohesion alone is a
  blob, alignment alone is a current, and all three are a flock.
- `examples/AiGhosts.ml`: the four Pac-Man ghosts' rules side by side
  on one maze, each ghost's target tile drawn as it is computed --
  Blinky at you, Pinky four ahead, Inky's reflected vector, Clyde
  running home when close. (The dossier's actual numbers.)
- `examples/AiBots.ml`: two bots duelling in a small arena, with what
  each of them *senses* drawn on the arena (the sight line, the last
  seen position, the aim error as a cone) and its intent printed as
  the keys it is "pressing"; keys for the skill knobs, so a bot can be
  turned from harmless to unfair while you watch, and one key to let
  it cheat (full knowledge, no delay) to show what that looks like --
  it stops feeling like an opponent and starts feeling like a bug.
- `examples/AiPerceptron.ml`: click to drop red and blue points, watch
  the line move, and watch it never settle on XOR.
- `examples/AiNeuralNet.ml`: the spiral, the hidden layers, the
  decision boundary redrawn every frame while it trains, the loss
  curve under it.
- `examples/AiDigits.ml`: draw a digit with the mouse, the network says
  which, with its ten outputs as bars; trained on digits our own font
  drew.
- `examples/AiQlearn.ml`: a grid world with a cliff, the Q values drawn
  as four numbers in each cell and the best one as an arrow, the policy
  appearing over a few thousand episodes.

## Games with AI

- `AiOthello.ml` **done**: alpha-beta 4 deep, a square-weights
  evaluation, the cuts counted against plain minimax.
- `examples/AiTictactoe.ml` **done**: searched to the very end, so no
  evaluation at all: 549,946 nodes against 18,297.
- `examples/AiPathfinding.ml` **done**: the three searches watched,
  with walls and mud drawn by the mouse.
- `games/AiConnect4.ml`: the middle game between Othello and chess, and
  the one that needs every trick at once -- bitboards would be
  overkill, but iterative deepening, move ordering (the centre first)
  and a transposition table each show up plainly in the node counts.
  Solved by Victor Allis and, independently, James Allen (1988-89): the
  first player wins, playing the centre.
- `games/AiGo.ml`: **9x9 Go, with MCTS and no evaluation function at
  all.** The rules are small (liberties, capture, ko, area scoring) --
  far smaller than chess -- and the history is exactly right: from 1970
  to 2005 Go programs were hand-written and weak, and in 2006 Monte
  Carlo playouts made a better 9x9 player than thirty years of
  handcrafting. That is a story a 200-line program can actually retell.
- `AiChess.ml`: the big one, and the one to be honest about --
  the rules (castling, en passant, promotion, repetition, the 50-move
  rule) are more code than the search. Worth it for what it shows:
  material plus piece-square tables, ordering, quiescence, a
  transposition table, and a board that can be set up from a FEN string
  so the classic test positions can be pasted in.
- **The existing games' bots, rewritten on `ai/Bot`** (each proposed
  here, decided in the game): `TinySoldat.ml`'s two soldiers
  (its `intent`/`human`/`bot` triple is the model the module is being
  designed from, so it is the first user and the one that decides the
  API), `TinyPong.ml`'s paddle (the smallest one: skill is a
  speed limit), `TinyMicroMachines.ml`'s rivals through
  `gamekits/racing/Topdown.computer` (which stays in the kit, gaining the
  delay and error knobs), and `TinyXpilot.ml`'s "robots", which
  its header already lists as an exercise.
- **A 3D bot**, once `Character3d` exists
  ([`plan_physics3d_teaching.md`](done/plan_physics3d_teaching.md) phase
  9): the same `Bot` with rays for senses and a capsule to drive --
  the proof that the layer is dimension-independent, and the case that
  would justify a shared shooter kit (see the bots section).
- **The existing games' enemies, rewritten on `ai/`**: TinyPacman's
  ghosts on `Fsm` and the dossier's rules; TinyZelda's and TinyRogue's
  monsters, which currently walk at you and stick to walls, on
  `Pathfind`; TinyDune2 and TinyWarcraft2 already on it through
  `gamekits/rts/Orders`. Each of those touches a game's existing code, so
  each is proposed here and decided there.

## Phasing

0. **Groundwork, DONE**: `ai/` (library `ai`, package elm_playground,
   unwrapped, pure OCaml) and `ai/tests/`.
1. **Game-tree search, DONE**: `Minimax` (minimax, alpha-beta, the node
   counts), `examples/AiTictactoe.ml`, `AiOthello.ml`.
2. **Pathfinding, DONE**: `Pathfind` (breadth-first, Dijkstra, A*,
   flow fields), `examples/AiPathfinding.ml`, and `gamekits/rts/Orders`
   over it for TinyDune2 and TinyWarcraft2.
3. **Steering and flocking, DONE**: `Steering`, `Flock`, on `Physics.body`;
   `AiSteering`, `AiFlock`; the `Ai.mli` forces above, which is the
   first piece of the Evan-style layer and the one most likely to be
   right on the first try.
4. **Deciding, DONE**: `Fsm`, then `Behavior` and `Utility` as the
   comparison; `AiGhosts`; TinyPacman's ghosts on it, behind the flag
   `ai=engine` (the author's choice, 2026-09-22, the same pattern as
   `physics=engine`): the original hand-written ghosts stay the
   default and the code beside it, the ones on `ai/` are the flag, so
   the two can be read and played side by side.
5. **Bots, DONE**: `Sense` (what may be known: visible, audible, the
   last seen position and its age) and `Bot` (the intent loop, the
   reaction delay, the input rate, the aim error). TinySoldat's bots on
   it behind `ai=engine`, its hand-written ones still the default --
   and on the criterion below, see the status entry: the file grew, and
   the reason is worth more than the rule was. `examples/AiBots.ml`:
   the four handicaps on four keys, and what the bot knows drawn.
6. **Deeper search, DONE**: `Deepening` (iterative deepening,
   ordering, a node budget, thinking a frame at a time), `Zobrist` (the
   keys and the transposition table); `AiConnect4`, whose node counts
   are the test of every one of them -- and which showed that iterative
   deepening can cost rather than save (see the status entry).
7. **Monte Carlo, DONE**: `Mcts` (playouts, then UCT, and a tree
   grown a frame at a time); `AiGo` on 9x9, rules and all, with the
   one rule its playouts need (see the status entry).
8. **The Playground layer, DONE**: `playground/Ai.mli` finished (the
   five families above) and `Ai_debug` written, with
   `examples/AiDebug.ml` drawing all four of its pictures; the
   board-game builder written, measured on AiConnect4 and dropped (see
   § The open question, and the status entry).
9. **Chess, mostly DONE (ahead of its turn)**: `AiChess`, rules first (perft counts as the test: the
   standard node counts per depth from the start position are a
   ruthless check on a move generator), search second.
10. **Learning, DONE**: `Matrix`, `Neuron`, `Net`, `Backprop`, `Grad`
    and `Train`, with `AiPerceptron`, `AiNeuralNet` and `AiDigits`.
11. **Learning to play, DONE but for the compute**: `Qlearn` and
    `AiQlearn`; tic-tac-toe learned by playing the minimax player it
    cannot beat (four thousand games, then it draws every time); and
    AlphaGo's two hooks in `Mcts` -- `?prior` (PUCT) and `?evaluate`
    -- measured on tic-tac-toe with a perfect value function standing
    in for a trained one. Wiring an actual network into `AiGo` is left
    undone on purpose: see the status entry.
12. **Docs, DONE**: `notes_ai.md` and `notes_ai_learning.md` checked
    against the code claim by claim (see the status entry for what the
    check found);
    `notes_ai_related_work.md`'s postscript **written**, from
    measurements (how much code, 115k nodes a second for alpha-beta,
    depth 4 inside a 16 ms frame, 180k playouts a second at
    tic-tac-toe against 830 at 9x9 Go, and a digit network at 89%
    after five seconds of training).
13. *(later)* Navigation meshes (funnel/string-pulling) instead of
    grids; crowd avoidance (RVO/ORCA); planning (STRIPS, and GOAP as
    F.E.A.R. used it); genetic algorithms and neuroevolution (NEAT);
    a convolution layer, if the digits want one.

## Status

**DONE** (2026-09-23): phases 1-12 -- the searches, the agents, the
bots, the learning half, the Evan-style layer and its drawings, and
the docs pass that checked both tutorials claim by claim against the
code. Two things the plan decided *against* doing are recorded below
with their measurements (a board-game app builder; `Ai.within`, a
budget in seconds), and two it decided to leave for a machine rather
than for code (`AiGo` with a network, and `Influence`, which no phase
ever claimed). What is left is in
[`../plan_ai_remaining.md`](../plan_ai_remaining.md).

- **Phase 0-1, DONE** (`ai/Minimax`): the `game` record (moves, play,
  score, max_to_play -- players needn't alternate, so passing works,
  which Othello needs), `minimax` and `alphabeta` returning the same
  best move with the nodes counted and each move's value, exact from
  minimax and a bound from alpha-beta. Worked example: Russell and
  Norvig's 3-12-8 / 2-4-6 / 14-5-2 tree, 13 nodes against 11.
  `examples/AiTictactoe.ml` (searched to the end, every square labelled
  WIN/DRAW/LOSS, 549,946 nodes against 18,297 -- 3%) and
  `AiOthello.ml` (depth 4, square weights, "v" for what it thinks
  of your moves).
- **Phase 2, DONE** (`ai/Pathfind`): one search with three queues, plus
  `field`/`downhill` for a crowd. Worked examples: an empty 13x9 grid,
  10 steps, 80 cells for breadth-first and Dijkstra against 11 for A*;
  with a mud patch costing 5, breadth-first wades through for a cost of
  18 while Dijkstra and A* go around for 16, looking at 115 and 75
  cells. `examples/AiPathfinding.ml` watches all three;
  `gamekits/rts/Orders` turns them into a strategy game's three kinds of
  order (one unit to a place, one unit to whatever is nearest, a crowd
  to a place through one flow field).
- **Phase 3, DONE** (`ai/Steering`, `ai/Flock`, `playground/Ai`): a
  behaviour is a *desired velocity*, and `steer` turns it into the
  force (desired minus velocity, clamped), so behaviours add with
  `blend`; `direction` is the second form, for characters with a fixed
  speed and no velocity (TinyBoomerangFu's). Worked examples: seek from
  (0, 0) going up at 100 towards (300, 400), desired (120, 160), steer
  (44.7, 22.4) at a force of 50; pursue's guess, 2 seconds ahead of a
  target 400 away; two boids 10 apart, separation and cohesion pulling
  equally against each other (hence separation's half radius); and
  emergence measured -- 30 boids headed the golden angle apart go from
  an order of 0.04 (the length of their mean heading) to 1.00 in 20
  seconds. `playground/Ai.mli`'s first family, verbs on
  `Physics.body` next to `fall` and `push` (seek, flee, arrive, chase,
  escaping, wandering, avoiding, flocking, following, facing). Two
  changes from the sketch above, forced by what a body is: it has no
  field for a top speed or a turning force, so `steer_speed` became
  `?speed` and `?force` on every verb (200 and 400 by default); and
  `wandering` takes a time, the angle on its circle being smooth noise
  of it, since a body has nowhere to keep that angle either.
  `examples/AiSteering.ml` (seven keys, the force as a red arrow, each
  behaviour's thinking drawn: the slowing circle, the predicted point,
  the corridor, the road) and `examples/AiFlock.ml` (60 fish, a slider
  per weight and for the radius, s/a/c to switch a rule off, one fish's
  neighbourhood drawn). TinyBoomerangFu's `brain` on `Steering`:
  decided against, in the game (see **Steering** under the modules).
- **Phase 4, DONE** (`ai/Fsm`, `ai/Behavior`, `ai/Utility`). The open
  decision settled: `Fsm` is a module, a small one, earning its place
  by what a `match` doesn't give -- the rules as data (a list of
  from/label/guard/target, so a machine can be drawn and checked), the
  steps spent in the state counted for you (`after n`), and the
  transition that fired (for what happens on entering a state). One
  transition per step, the first rule that holds: the list's order is
  the priority. `Behavior` is pure: a tree *decides* an action and
  `path` says what it thought, no running status and no blackboard
  (the model is the memory); `Utility` scores the options, with
  `inertia` against flip-flopping. The three written as the same guard
  dog in their `.mli`s, and a test that they agree where the situation
  is clear-cut, and that only the machine remembers (hysteresis: it
  flees on until healed past 0.8, the others stop at 0.5).
  `TinyPacman.ml` with `ai=engine`: each ghost an `Fsm` machine,
  its whole life one table (released, out, a power pellet, the wave
  turns, eaten, another pellet, time's up, home), the scatter/chase
  waves a second machine -- which the tests check agrees with the
  original clock on all 6000 frames of its schedule; the default code
  untouched but for `move_ghost` split into its move and its arrivals.
  The cost, said in the game: a state changes a frame after its event,
  so `collide` remembers who it caught in between. `examples/AiGhosts.ml`:
  each ghost's target tile drawn as its rule computes it (Pinky's line
  ahead, Inky's doubled vector from Blinky, Clyde's 8-tile circle), the
  waves as a timeline, on the maze kit.
- **Phase 9, mostly DONE, ahead of phases 4-8** (`AiChess.ml`):
  the rules checked by perft (20/400/8902 from the start; Kiwipete,
  positions 3 and 4), material and Michniewski's piece-square tables,
  MVV-LVA ordering (2,305 positions instead of 25,206 on Kiwipete,
  3 ahead), quiescence against the horizon effect, FEN. For the
  quiescence, `Minimax.alphabeta` gained `?leaf`, the positions at the
  depth scored knowing the window there: without it, Kiwipete took 1.9
  s natively and 5 in JavaScript, with it 0.16 and 1.5. Left: the
  transposition table and iterative deepening (phase 6's `Zobrist` and
  `Deepening`, which AiChess should then use), and the draws by
  repetition and the 50-move rule.
- **Phase 5, DONE** (`ai/Sense`, `ai/Bot`): `Sense.target` per thing a
  bot might care about (visible, audible, the position it is at or was
  last seen at, its age, how long it has been in sight), the game
  passing the distance and whether the line is clear, since only it
  knows its walls -- so the module is dimension-free, and a 3D bot
  would differ by its `'v`. `Bot.t` is `sense` then `decide` with the
  handicaps: the delay (it decides on senses that many frames old), the
  rate (it may change its mind every so many frames, repeating itself
  in between), and `aim_error`, an offset halving every `settle` frames
  a target stays visible, deterministic (a wobble of the time and the
  bot's number, no Random). Worked example, checked: delay 15 and rate
  6, a target out at frame 100 is shot at 120; delay 15 rate 1, at 115;
  neither, at 100. `sense` is given what it sensed last frame -- a
  bot's memory is part of its senses -- so a game keeps no memory of
  its own; `last_senses` reads the newest for a game's tests.
- **The phase's criterion, not met, and what it showed instead**: the
  plan said the port must make `TinySoldat.ml` *shorter* or the layer
  is wrong. It made it longer (465 to 588 lines, with both bots in it),
  and the reason is the interesting part: the hand-written bot took the
  nearest enemy *through the walls* and asked what it could see only to
  decide whether to shoot, so it never had to look for anyone. Honest
  senses take that away, and the bot then needs a behaviour it never
  had -- patrolling when it knows nothing, hunting where it last saw
  you. The mechanism that moved out (delay, rate, memory, the settling
  aim) was small because the old bot had none of it. So the criterion
  measured the wrong thing for a game that was cheating rather than
  duplicating; the real test is a *second* user (Topdown's racing line,
  TinyPong's paddle), which is now an exercise in notes_ai.md section
  13. Two things the port did prove: the layer needs nothing of the
  Playground (the game passes distances and a boolean), and the flag
  pattern fits it -- `ai=engine`, the author's rule from phase 4, the
  two bots side by side in one file, the default unchanged (its golden
  frames did not move).
- **`examples/AiBots.ml`, DONE**: an arena with four walls, you and a
  bot that walks its rounds until it sees you; keys 1 to 4 turn off the
  reaction delay, the input rate, the aim error and the senses (the
  last of which lets it see through walls: the cheat, to feel). What it
  knows is drawn -- a green line while it sees you, a fading marker
  where it last saw you, with the frames since, and nothing while it
  has never seen you. Two things the writing taught, both now in the
  file: a patrol has to *sweep* (its first version shuffled left and
  right where it stood, and the bot never found anyone), and anything
  that walks into a wall must slide along it (its first version stopped
  dead, and the bot stuck in a corner for good).
- **Phase 6, DONE** (`ai/Zobrist`, `ai/Deepening`, `AiConnect4`):
  `Zobrist` draws one 64-bit number per (piece, square) plus one for
  the side to move, so a position's key is their xor and a move two
  more; and holds the transposition table (value, depth, exact or a
  bound, and the best move found, a deeper search replacing a
  shallower). `Deepening.search` is alpha-beta with the previous
  depth's best move first, the game's own ordering hint, the table, and
  a node budget that abandons the depth in progress; `start`/`think`/
  `plan` let a game search a frame's worth at a time, the allowance
  doubling when a depth does not fit (or a deep search would never
  finish in a frame). Checked (`Unit_deepening`, on Nim): every
  combination gives alpha-beta's value and finishes the depth asked
  for; the budget keeps the last finished depth; thinking in pieces
  ends where thinking at once does.
- **What AiConnect4 measured, and the surprise**: the opening searched
  7 moves ahead -- alpha-beta with the columns left to right, 65,724
  nodes; the middle columns first, 9,449; *plus* iterative deepening,
  12,818; plus the table, 7,742. Iterative deepening cost a third more
  rather than saving: the game's own hint already orders the moves
  well, so the shallower passes bought little ordering and were paid
  for in full. What it buys here is the time control (stop whenever,
  with a complete answer), and the table pays its repeats back. The
  plan had assumed the four lines would fall monotonically; they do not,
  and `Deepening.mli` and notes_ai.md section 9 now say why.
- **Monte Carlo, 2026-09-22**: `Mcts` is the four steps and UCB1
  (`sqrt 2` by default), with the tree grown in place so a game can
  keep it in its model -- `search` for all of it at once,
  `start`/`think`/`plan` for a frame's worth at a time, and the answer
  is the most visited child rather than the best scoring one. Checked
  on tic-tac-toe (`Unit_mcts`), where nothing in the code knows what a
  line of three is worth: 2000 playouts open in the centre, block, and
  take a win; blocking is 20/20 over 20 seeds against 5/20 at ten
  playouts; thinking in pieces ends where thinking at once does.
  `AiGo` is 9x9 Go -- groups and liberties, capture, suicide, simple
  ko, Chinese scoring with 6.5 komi -- and the two things it taught:
  the playouts need the eye rule or they say nothing, and a playout's
  cost is the whole game (15 s a move at first; candidates tried in a
  random order, a precomputed neighbour table and a stamped scratch
  array for the flood fill brought 1200 playouts to 1.5 s, and the
  game now thinks 12 a frame). Its first golden frame was a correct
  and dull one -- both sides passed and komi decided it -- which is why
  the board has a keyboard cursor: a scripted game needs to be able to
  put a stone down.
- **The layer, 2026-09-22**: the four remaining families of
  `playground/Ai.mli` (ways, an opponent, a mode, a bot) and
  `Ai_debug`'s four pictures, checked in `playground/tests/Unit_ai.ml`
  (Nim for the opponent: from five sticks it leaves four). Three
  things the writing decided, against the plan as written above.
  `Ai.within 0.2` (a budget in seconds) is gone: an `update` has no
  clock and a second cannot be replayed, so thinking across frames is
  a budget of work -- `pondering`/`ponder`/`settled`/`answer`, with
  `a_frame_of n`, since only a game knows what its own moves cost (12
  playouts is a frame of 9x9 Go and fifty frames of tic-tac-toe).
  `Ai.thoughts` runs a search per move, because alpha-beta's cut
  siblings report the bound that cut them and not their value -- the
  first version drew three equal bars where one move won and two lost.
  And the layer deliberately stops short of `TinyXCOM`'s pathfinding
  (corner-cutting, a friend in the way, steps of 4 and 6 time units):
  a `way` that took all of that would be `Pathfind` with new names.
  The proof that it is the right shape: `TinyDiablo` and
  `TinyDungeonMaster` each lost a hand-written `Pathfind.problem` for
  one line of `Ai.way`, with every golden frame and scenario test
  unchanged, pixel for pixel.
- **The board-game builder: written, measured, dropped** (same day,
  at the author's asking that it be a kit rather than a playground
  layer -- right, and that is where it was written). AiConnect4 on it
  went from 305 lines to 292, against a kit of 156 plus 87 of
  interface, and AiChess (two clicks a move) and AiGo (its own MCTS
  playout) could not use it at all. Deleted, with the reasoning kept
  in § The open question so that nobody spends the afternoon again.
  The negative result is worth as much as the layer is: the loop is
  thirty readable lines a game, and the part each game does
  differently is the part that gives it its character.
- **Learning, the first slice, 2026-09-22**: `Matrix` (the flat
  array in the open, the three loops, and a version that reads along
  rows instead of down columns -- 1.6x at n=64 to 2.2x at n=256, which
  is twice and not ten times, and the .mli says why: the arithmetic is
  the same and only the reading changes) and `Neuron` (Rosenblatt's
  rule, which moves only on a mistake). The measurement worth keeping
  from it: on XOR the rule gets *two* of four, while the best line
  that exists gets three -- a rule that cannot converge does not stop
  at the best approximation, it wanders. Both the plan and the
  tutorial had said 75%, from reasoning about what a line can do
  rather than from running it. `examples/AiPerceptron.ml` shows the
  line walking into place and stopping dead on AND, and swinging for
  ever on XOR, with the weights drawn as the line's normal.
- **TinyBoomerangFu on the layer, 2026-09-22** (the author asked
  whether it used `ai/` for its bots; it did not). Now it does, behind
  `ai=engine`, the hand-written `brain` still the default, as
  TinySoldat and TinyPacman do it. What the layer takes away from the
  three cooks: sight through a pillar (`Sense`, with the enemy
  remembered for 90 frames after it goes behind one), an answer on the
  same frame (`Bot`, delay 6 and rate 3), and hysteresis hidden inside
  an if (`Fsm`, `since >= 8` before leaving Dodge). What it does not use
  is `Steering` -- see the module's entry. Three tests in
  `tests/games`: a pillar blocks sight and a pit does not, the three
  modes and the eight frames of hysteresis, and the delay seen for what
  it is -- when the enemy jumps to the other side of the arena the bot
  keeps walking the old way for six frames. `ai/Bot`'s own answer to a
  bot that has only just started is worth knowing, and the test says
  it: it acts on the oldest senses it has, so the delay is a memory,
  not a blindfold.
- **Backprop, 2026-09-22**: the backward pass by hand, checked
  against finite differences on every activation to six digits -- the
  test that catches sign errors, and the reason to trust anything
  built on it. Two numbers the writing corrected. The worked example
  in the notes stepped the weight and quietly left the bias alone
  (a = 0.64307); training steps both, which gives 0.66317, and both
  numbers are now in the test so neither can drift. And the vanishing
  gradient, measured on a 4-8-8-8-8-8-1 net as the last layer's
  gradient over the first's: sigmoid 2159, tanh 0.7, relu 1.4 -- so
  with Glorot weights a tanh stack does not vanish at this depth at
  all, and the plan's "sigmoids multiply by under 1/4000" was true of
  the slopes and false of the network. Depth alone does not kill a
  gradient; depth with the wrong squash and the wrong starting weights
  does. `examples/AiNeuralNet.ml` is the two spirals, with the hidden
  layers on a key: 105 weights reach a loss of 0.023, and the same run
  with no hidden layer (3 weights, a straight boundary) sticks at
  0.075. Its cost is the drawing, not the arithmetic -- a frame is 9 ms
  of network and 40 of rasterizer -- so the decision field is quantised
  into bands and equal runs are drawn as one rectangle.
- **Grad, Train and AiDigits, 2026-09-22**, finishing the supervised
  half. `Grad` is reverse mode on scalars (micrograd's shape), and it
  agrees with the hand-written pass to the *last* digit over all 105
  weights of a 2-8-8-1 network -- the same arithmetic in a different
  order. Measured, it costs about 20x (9.6 us by hand, 197 through the
  graph), which is the honest reason the examples still use
  `Backprop`, and the reason real libraries do reverse mode over
  arrays rather than scalars. `Train` is the loop: a deterministic
  shuffle, batches, a decaying rate, and a held-out split -- with
  overfitting made to happen in a test (a 24-24 network on twenty
  examples: the training loss goes under 0.01 while the held-out one
  turns round and climbs), because the two curves crossing is the
  picture the subject is really about. `AiDigits` trains on digits our
  own Hershey font draws, shaken and inked by distance to the strokes;
  about 80% on held-out digits after six thousand examples, and worse
  on a mouse-drawn one, which is the point.
- **Qlearn, 2026-09-22**: the rule, the cliff world, and the thing
  the writing turned up. The plan (and the tutorial) said "turning
  exploration to zero visibly stops the learning". On the cliff it
  does not: every step costs 1, an untried action is worth 0, so
  greedy behaviour is exploratory all by itself and a learner with
  exploration off still finds the 13-step optimum. Optimism in the
  initial values, switched on by the sign of the rewards. The test now
  shows both halves -- pay only at the goal and the same learner ends
  with four state-action pairs after five hundred episodes and never
  reaches it -- which is a better lesson than the one that was
  planned. And the old result, working: as X against a perfect player
  it loses at first and draws after four thousand games (400 positions
  learned), which is Samuel 1959 in twenty lines.
- **The two halves joined, 2026-09-22**: `Mcts` now takes `?prior`
  and `?evaluate`, which is AlphaGo's shape as a mechanism; the
  selection rule becomes PUCT with a policy, and a value replaces the
  playout. Measured with a perfect value function in place of a
  trained one (deliberately: it measures the hook, not the network) --
  12 of 12 won positions found at twelve playouts against 10 of 12 for
  random playouts, 93% of the visits on the pointed move against 73%
  with a flat policy, and 11-0 over twenty games against the
  random-playout version of the same search. Two bugs the measuring
  found: PUCT needs a *normalised* policy (unnormalised priors make
  the exploring term swamp the win rate, and that match was 8-4 until
  it was fixed), and `plan`'s "most visited move" decides nothing at
  small budgets -- every child has one visit -- so ties now go to the
  better win rate, without which a search with a perfect evaluation
  picked losing moves. That change also made the weak ten-playout
  searcher better, 8 blocks of 20 rather than 5, and the numbers in
  Mcts.mli and notes_ai.md now say 8.
- **AiGo with a network: not done, and the reason**. Everything it
  needs is written (`Net`, `Backprop`, `Train`, `Mcts`'s two hooks),
  and what is missing is a machine. A 9x9 self-play run that produced
  a network worth having would be hours of this laptop, and a network
  trained on a few thousand playouts would be worse than the playouts
  it replaced -- so the honest result would be "no better", which
  teaches nothing the tic-tac-toe measurement does not teach in a
  second. The piece to write when someone wants it is a `scripts/train/`
  run that plays self-play games offline and writes a small weights
  file, with `AiGo` loading it behind a flag. Recorded here so the
  gap is a decision rather than an oversight.
- **The documents checked against the code, 2026-09-22**, which is
  the last phase and found seven things. Two were wrong: the layers
  picture said "10 weights and 4 biases" for a 2-3-1 network (it is 9),
  and drew three input nodes for two inputs; and `Train` was credited
  with a `step` that lives in `Backprop`. Two named things that are
  not used where the prose said: `TinyBoomerangFu` writes its
  hysteresis guard inline rather than calling `Fsm.after` (its own
  header said otherwise, and now does not), and no example uses
  `Ai.escaping`. One snippet did not typecheck (`Ai.thinks` returns a
  pair). And several numbers were real but unpinned -- breadth-first's
  80 cells on the mud grid, "10 of 12" and "11 wins" in the MCTS
  match -- which are now assertions rather than printfs, the way
  everything else here is. The vaguest claim, "TinySoldat grew by a
  quarter", turned out to be exactly right and is now the two numbers:
  465 lines to 588.
- **Open decisions**, to settle while writing, not now: the board-game
  app builder (§ The Playground API); whether `Fsm` is a module or just
  a pattern shown in a game (a state machine in OCaml is a variant and
  a `match`, and a library around that can easily be worse than
  nothing); whether the resumable search is worth its complexity
  outside AiChess; and how much of chess to write.
- **Bots, added 2026-09-20**, at the author's asking ("ultimately
  `ai/` and the Evan-like `Ai.mli` should also help writing 2d and 3d
  bots"): the plan had the opponent and the agent and no name for the
  thing most of this repository's games actually need -- a mind that
  plays through the player's own inputs. Written up from what three
  games already do by hand (`TinySoldat`'s `intent`/`human`/`bot`,
  `gamekits/racing/Topdown.computer`'s `(gas, steer)`, `TinyPong`'s
  speed-limited paddle), which is also why `Bot`'s API is being
  generalised from existing code rather than designed: the test of the
  port is that TinySoldat gets *shorter*.
- **`gamekits/bots/`: not yet, and the condition for changing that**
  (the author's question, same day). The mechanism (`Bot`, `Sense`) is
  genre-independent and goes in `ai/`; the knowledge is genre-specific
  and stays in the genre kit, where `Topdown.computer` already lives.
  A kit named after a technique rather than a genre would be the first
  of its kind here. Revisit when a deathmatch bot is wanted by both a
  2D and a 3D shooter -- that is a real shared body of knowledge (map
  awareness, cover, weapons), and then it becomes `gamekits/shooter/` or
  `gamekits/bots/`, with the reason recorded here.
- **The naming**: the AI demo games are `Ai*` (AiOthello, AiTictactoe,
  AiPathfinding), not `Tiny*`, because what they demonstrate is the
  algorithm rather than an arcade original -- kept for AiConnect4,
  AiGo, AiChess.

## Verification

- `make test`: every `.mli`'s worked example, as numbers -- the node
  counts, the path costs and the cells visited, the UCB formula's
  choice on a hand-made tree, a perceptron's convergence in a known
  number of steps, and **backprop's gradients against finite
  differences** (the one test that catches every sign error in a
  network).
- Playing strength, cheaply: a fixed set of seeds, and the deeper
  search beating the shallower one over N games (depth 4 against depth
  2; 10,000 playouts against 1,000). Not a rating, just a monotonicity
  check -- but it catches the bugs that leave a search *working* and
  playing badly.
- Golden frames for each example and game, through the scripted keys
  (`-script`), which is exactly why the seeds are explicit.
- **For bots, two checks the other kinds don't need**: that skill is
  monotone (a bot at skill 1 beats the same bot at skill 0 over N
  seeded duels -- the same trick as depth 4 against depth 2), and that
  a bot *cannot* cheat, which is a type rather than a test: if the
  mind is `senses -> intent`, a bot reaching into the world does not
  compile, and `Sense` is the only door.
- By eye, for the third that has no right answer: does the flock look
  like a flock, does the ghost look like it is hunting you, does the
  bot look like a player having a bad day rather than a machine.

## Out of scope

- GPUs, convolutional networks at any real size, transformers, and
  anything with a pretrained model in it. The point here is the
  mechanism at a size a reader can hold.
- Datasets that must be downloaded (hence the font-drawn digits).
- Navigation-mesh *generation* from geometry (recast-style): hard, and
  mostly orthogonal to the ideas.
- A strong chess or Go engine. A few hundred elo and a weak amateur,
  explained, beat a strong one that no one can read.
