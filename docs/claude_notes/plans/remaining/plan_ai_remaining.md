# Plan: what's left for the game AI

The algorithms and their games are done: see
[`done/plan_ai_teaching.md`](done/plan_ai_teaching.md) (`ai/`, phases
1-12 -- pathfinding, steering, flocking, state machines, behaviour
trees and utility, senses and bots, minimax and alpha-beta, iterative
deepening with a transposition table, Monte Carlo tree search and the
two hooks a network goes in, and the learning half from one neuron to
Q-learning), the Evan-style layer `Ai` with `Ai_debug`, the
eleven `examples/Ai*.ml`, the demo games `AiTictactoe`, `AiOthello`,
`AiConnect4`, `AiChess`, `AiGo`, and the tutorials
[`notes_ai.md`](../tutorials/notes_ai.md),
[`notes_ai_learning.md`](../tutorials/notes_ai_learning.md) and
[`notes_ai_related_work.md`](../related-work/notes_ai_related_work.md),
whose postscript has the numbers.

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example, its test, and -- where it replaces
something -- the older version kept beside it behind a switch, so the
difference is a number you can watch.

## 1. The two the plan decided against, and what would change that

Neither is an oversight; both are written up in
`done/plan_ai_teaching.md` with their reasoning, and both have a
condition attached.

- **`AiGo` with a network.** Everything it needs exists: `Net`,
  `Backprop`, `Train`, and `Mcts`'s `?prior` and `?evaluate` (measured
  on tic-tac-toe: 12 of 12 won positions found at twelve playouts
  against 10 of 12, and 11-0 over twenty games against the
  random-playout version). What is missing is a machine. A self-play
  run on 9x9 that produced a network worth having is hours of a
  laptop, and a network trained on a few thousand playouts is worse
  than the playouts it replaces -- so the honest report would be "no
  better". **The piece to write**: `scripts/train/` (the target layout
  has the directory and nothing in it), an offline self-play loop
  writing a small weights file, and `AiGo` loading it behind a flag
  (`ai=network`) with the random-playout version still the default, as
  `ai=engine` works elsewhere. Then the result to report is its score
  against its own older self, which is the honest measure and needs no
  human.
- **`Influence`**, the one module of the target layout that no phase
  ever claimed. It is small -- `Pathfind.field` run from every enemy
  at once, read as danger per tile rather than distance -- and the
  game that wants it is a strategy game deciding where *not* to walk.
  `TinyXCOM` or `TinyDune2` would be its first user, and the drawing
  is `Ai_debug.field` with a different colour ramp.

## 2. The searches and the paths

- **A binary heap for `Pathfind`'s frontier**, which today is a list
  kept in order (its `.mli` says so); time A* on a large map before and
  after. The first thing to do if any of this is ever used at scale.
- **Eight directions in `Pathfind`**, a diagonal costing sqrt 2 and the
  octile distance as the heuristic -- `Ai.way ~diagonal` already does
  this above the door, so the module is the part that lags.
- **Jump Point Search** (Harabor and Grastien, 2011): `Pathfind.astar`
  skipping the symmetric paths of an open grid, next to the plain
  version, measured on the same map.
- **Iterative deepening with a *time* limit** around
  `Minimax.alphabeta`, listed as an exercise in both `AiChess.ml` and
  `AiOthello.ml`. `Deepening` has the node budget; seconds need a
  clock the playground does not hand an `update` (see the layer's
  settled questions below).
- **A transposition table for `AiChess`**, which has none: `Zobrist`
  and `Deepening` are written and the game is the obvious second user.
- **Killer moves** in `Deepening`'s ordering -- a move that cut
  elsewhere, tried early -- which is the next ordering trick after the
  game's own hint.
- **`AiGo`'s playouts answering a capture or an atari** instead of
  playing anywhere (what every Monte Carlo Go program did after the
  eye rule), and **RAVE / all-moves-as-first** (Gelly and Silver,
  2007). Both are worth far more per playout than more playouts are,
  and `AiGo` is 1.2 ms a playout today.
- **The full ko rule** in `AiGo` (it has the simple one: a move may not
  take back the single stone that just took), which means keeping the
  positions already seen.

## 3. The minds

- **Nested states in `Fsm`** (Harel's statecharts, which its `.mli`
  notes it does not do).
- **A "running" status and a blackboard for `Behavior`**, for actions
  that take several frames -- without which a behaviour tree cannot
  express "walk there, then shoot".
- **A bot for a second genre** on `Sense` and `Bot`. Two users exist
  now (`TinySoldat`'s soldiers, `TinyBoomerangFu`'s cooks) and both are
  shooters of a kind; `gamekits/racing/Topdown.computer` (a racing
  line) and `TinyPong`'s paddle are the two nearest things still
  written by hand, and a racer would be the first bot whose senses are
  about a *track* rather than an enemy.
- **Hearing used for something**: `Sense.audible` is written, tested,
  and read by no game. A bot that turns towards a shot it could not
  see is the demonstration, and `TinySoldat` already has the shots.
- **Goal-oriented action planning** (Orkin's F.E.A.R., 2005):
  `Pathfind`'s A* searching over *world states*, an action's
  preconditions and effects as the edges -- which `Pathfind.problem`'s
  polymorphic `'node` already allows, so this is a game and an
  `.mli`, not a new search.

## 4. The learning half

From `notes_ai_learning.md` §10, in its order of difficulty:

- **momentum, then Adam** (Kingma and Ba, 2015) in `Backprop.step`,
  with the spiral's loss curve under each;
- **early stopping**: `Train` watching the held-out curve and keeping
  the weights from where it turned -- the curve is already drawn
  (`AiNeuralNet`'s "h"), and nothing acts on it;
- **weight decay and dropout** (Srivastava et al., 2014), and the gap
  between the two curves shrinking;
- **forward-mode autodiff** (dual numbers) beside `Grad`'s reverse
  mode, and why reverse wins when there are many weights and one loss;
- **TD(lambda)'s eligibility traces** in `Qlearn`, which TD-Gammon used
  instead of the one-step update;
- **a network instead of `Qlearn`'s table**, with DQN's experience
  replay and target network (Mnih et al., 2015) -- which are exactly
  what keeps it from diverging, and so are the point rather than the
  detail;
- **one convolutional layer** for `AiDigits` (LeCun et al., 1998), its
  weights shared across the image, against the 256-64-10 network;
- **the self-play loop on tic-tac-toe or Connect 4** before 9x9 Go,
  where a result comes in minutes and perfect play is known to check
  it. This is the cheap half of §1's first item, and the sensible
  thing to write first.

## 5. Seeing it

`Ai_debug` draws four things (a way, a field, an opponent's opinion, a
machine). Three more are worth having, and they are named in
`plan_inspect_teaching.md`'s table as this plan's to deliver:

- **the frontier in the order it was taken** -- which is the whole
  content of `examples/AiPathfinding.ml`, drawn by hand there and not
  reusable;
- **each steering force as a vector out of its body**, with the wander
  circle in front of it (again, `examples/AiSteering.ml` draws its own);
- **a bot's senses**: what it has seen, how stale it is, and what it is
  doing about it -- the only honest way to check a bot is not
  cheating, and the one drawing that would have made TinySoldat's port
  obvious.

The open question that comes with them: `Ai_debug` has no key of its
own, because the platform cannot know what a game is thinking
(`Audio_debug` can, which is why it has "v"). Either every game draws
these behind its own key, as TinyTowerDefense's "p" does, or the
inspect plan's panel mechanism grows a way for a game to hand it
shapes. The second is better and is that plan's business, not this
one's.

## 6. Settled, and not to be re-opened

Recorded here so that nobody spends the afternoon again; the reasoning
is in `done/plan_ai_teaching.md`.

- **A board-game app builder** (rules and squares in, an `app` out, in
  the shape of `Logo`/`Bigbang`/`Puzzlescript`): written as a kit,
  measured, deleted. AiConnect4 on it went 305 lines to 292, against a
  kit of 156 plus 87 of interface, and chess (two clicks a move) and Go
  (its own MCTS playout) could not use it at all.
- **`Ai.within 0.2`**, a search budget in seconds: an `update` has no
  clock and a second cannot be replayed, so thinking across frames is a
  budget of *work* (`Ai.a_frame_of`).
- **`Steering` for `TinyBoomerangFu`**: its characters have a fixed
  speed and a committed dash, so there is no velocity to steer.
- **`TinyXCOM`'s pathfinding staying on `Pathfind` directly**: its
  moves have rules the layer has no word for (no cutting a wall's
  corner, a friend blocking a tile, steps of 4 and 6 time units), and a
  `way` that took all of them would be `Pathfind` with new names.

## 7. Later (the plan's phase 13)

- **Navigation meshes** (funnel / string-pulling) instead of grids,
  which is what a 3D game with a floor rather than tiles wants.
- **Crowd avoidance** (RVO/ORCA): `Steering.avoid` handles obstacles
  and not each other, so a crowd through a doorway is a jam.
- **Planning** (STRIPS, and GOAP as F.E.A.R. used it) -- §3's last
  item, if it grows past one game.
- **Genetic algorithms and neuroevolution** (NEAT), which is the other
  way to get a policy and needs no gradient at all.
