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
and `games/AiOthello.ml`) and `ai/Pathfind` (with
`examples/AiPathfinding.ml`, and `kits/rts/Orders` over it, which is
how TinyDune2's and TinyWarcraft2's units walk). This plan is written
after those two, to say where the rest goes: the real-time side
(steering, flocking, state machines), the deeper search (iterative
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

## Two halves, kept apart

Game AI is two different subjects that share a directory:

```
   the opponent                      the agent
   a turn-taking game                a world running at 60 fps
   "what move do I play?"            "where do I go, what do I do?"
   Minimax, Mcts, Deepening          Pathfind, Steering, Flock, Fsm
   seconds to think, once            a fraction of a frame, every frame
   AiTictactoe, AiOthello,           TinyPacman's ghosts, TinyDune2's
   AiConnect4, AiChess, AiGo         units, a flock, TinyZelda's monsters
```

They share only two things, and it's worth knowing which: the **debug
overlay** (both want to draw what the computer is thinking), and
**learning** (a network can be an opponent's evaluation function or an
agent's policy). Everything else is separate, and the notes are split
the same way.

## The Playground API, Evan-style

`playground/Ai.mli`, three small families, each a one-liner in a game.
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
  Steering            seek, flee, arrive, pursue, evade, wander, avoid,
                      path following
  Flock               separation, alignment, cohesion
  Fsm                 states and transitions; Pac-Man's four ghosts
  Behavior            behavior trees: sequence, selector, decorator
  Utility             scoring the options instead of branching
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
is visible by default. `playground/Ai_debug.ml`, like `Audio_debug`:
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
  Characters" (GDC 1999).
- **Flock**: Craig Reynolds, "Flocks, Herds and Schools: A Distributed
  Behavioral Model" (SIGGRAPH 1987) -- three rules, and the birds of
  *Batman Returns*.
- **Fsm**: Pac-Man (1980) and its four ghosts, whose chase/scatter
  timing and per-ghost target tiles are documented down to the frame in
  the Pac-Man Dossier (Jamey Pittman, 2009).
- **Behavior**: behavior trees, Halo 2 (Damian Isla, GDC 2005); the
  successor everyone copied, and its costs (the blackboard).
- **Utility**: utility AI, The Sims (1997-2000) -- needs scored, the
  highest wins; Dave Mark's "Behavioral Mathematics for Game AI" (2009).
- **Influence**: influence maps (Andrew Zobrist's Go program, 1969 --
  the same Zobrist).
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

- `games/AiOthello.ml` **done**: alpha-beta 4 deep, a square-weights
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
- `games/AiChess.ml`: the big one, and the one to be honest about --
  the rules (castling, en passant, promotion, repetition, the 50-move
  rule) are more code than the search. Worth it for what it shows:
  material plus piece-square tables, ordering, quiescence, a
  transposition table, and a board that can be set up from a FEN string
  so the classic test positions can be pasted in.
- **The existing games' enemies, rewritten on `ai/`**: TinyPacman's
  ghosts on `Fsm` and the dossier's rules; TinyZelda's and TinyRogue's
  monsters, which currently walk at you and stick to walls, on
  `Pathfind`; TinyDune2 and TinyWarcraft2 already on it through
  `kits/rts/Orders`. Each of those touches a game's existing code, so
  each is proposed here and decided there.

## Phasing

0. **Groundwork, DONE**: `ai/` (library `ai`, package elm_playground,
   unwrapped, pure OCaml) and `ai/tests/`.
1. **Game-tree search, DONE**: `Minimax` (minimax, alpha-beta, the node
   counts), `examples/AiTictactoe.ml`, `games/AiOthello.ml`.
2. **Pathfinding, DONE**: `Pathfind` (breadth-first, Dijkstra, A*,
   flow fields), `examples/AiPathfinding.ml`, and `kits/rts/Orders`
   over it for TinyDune2 and TinyWarcraft2.
3. **Steering and flocking**: `Steering`, `Flock`, on `Physics.body`;
   `AiSteering`, `AiFlock`; the `Ai.mli` forces above, which is the
   first piece of the Evan-style layer and the one most likely to be
   right on the first try.
4. **Deciding**: `Fsm`, then `Behavior` and `Utility` as the
   comparison; `AiGhosts`; TinyPacman's ghosts on it.
5. **Deeper search**: `Deepening` (iterative deepening, ordering, a
   budget, resumable), `Zobrist`; `AiConnect4`, whose node counts are
   the test of every one of them.
6. **Monte Carlo**: `Mcts` (playouts, then UCT); `AiGo` on 9x9.
7. **The Playground layer**: `playground/Ai.mli` finished (the three
   families above), `Ai_debug`, and the board-game question settled by
   rewriting AiConnect4 on it.
8. **Chess**: `AiChess`, rules first (perft counts as the test: the
   standard node counts per depth from the start position are a
   ruthless check on a move generator), search second.
9. **Learning**: `Matrix`, `Neuron`, `Net`, `Backprop`, `Grad`,
   `Train`; `AiPerceptron`, `AiNeuralNet`, `AiDigits`.
10. **Learning to play**: `Qlearn`; `AiQlearn`; tic-tac-toe learned by
    self-play, then measured against the minimax player it cannot beat
    but can learn to draw with; and the network as `AiGo`'s playout
    policy and evaluation -- AlphaGo's shape, at a size that runs here.
11. **Docs**: `notes_ai.md` and `notes_ai_learning.md` checked against
    the code, the numbers filled in;
    `notes_ai_related_work.md`'s postscript.
12. *(later)* Navigation meshes (funnel/string-pulling) instead of
    grids; crowd avoidance (RVO/ORCA); planning (STRIPS, and GOAP as
    F.E.A.R. used it); genetic algorithms and neuroevolution (NEAT);
    a convolution layer, if the digits want one.

## Status

- **Phase 0-1, DONE** (`ai/Minimax`): the `game` record (moves, play,
  score, max_to_play -- players needn't alternate, so passing works,
  which Othello needs), `minimax` and `alphabeta` returning the same
  best move with the nodes counted and each move's value, exact from
  minimax and a bound from alpha-beta. Worked example: Russell and
  Norvig's 3-12-8 / 2-4-6 / 14-5-2 tree, 13 nodes against 11.
  `examples/AiTictactoe.ml` (searched to the end, every square labelled
  WIN/DRAW/LOSS, 549,946 nodes against 18,297 -- 3%) and
  `games/AiOthello.ml` (depth 4, square weights, "v" for what it thinks
  of your moves).
- **Phase 2, DONE** (`ai/Pathfind`): one search with three queues, plus
  `field`/`downhill` for a crowd. Worked examples: an empty 13x9 grid,
  10 steps, 80 cells for breadth-first and Dijkstra against 11 for A*;
  with a mud patch costing 5, breadth-first wades through for a cost of
  18 while Dijkstra and A* go around for 16, looking at 115 and 75
  cells. `examples/AiPathfinding.ml` watches all three;
  `kits/rts/Orders` turns them into a strategy game's three kinds of
  order (one unit to a place, one unit to whatever is nearest, a crowd
  to a place through one flow field).
- **Open decisions**, to settle while writing, not now: the board-game
  app builder (§ The Playground API); whether `Fsm` is a module or just
  a pattern shown in a game (a state machine in OCaml is a variant and
  a `match`, and a library around that can easily be worse than
  nothing); whether the resumable search is worth its complexity
  outside AiChess; and how much of chess to write.
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
- By eye, for the half that has no right answer: does the flock look
  like a flock, does the ghost look like it is hunting you.

## Out of scope

- GPUs, convolutional networks at any real size, transformers, and
  anything with a pretrained model in it. The point here is the
  mechanism at a size a reader can hold.
- Datasets that must be downloaded (hence the font-drawn digits).
- Navigation-mesh *generation* from geometry (recast-style): hard, and
  mostly orthogonal to the ideas.
- A strong chess or Go engine. A few hundred elo and a weak amateur,
  explained, beat a strong one that no one can read.
