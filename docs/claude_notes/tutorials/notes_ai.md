# Game AI, from scratch: a tutorial for `ai/`

How a computer decides: the handful of ideas behind every ghost that
chases you, every unit that walks around a rock, and every opponent
that beats you at a board game -- where they came from, and what each
one costs. It is also the specification of `ai/` (see
[`plan_ai_teaching.md`](../plans/plan_ai_teaching.md)): the modules
§0 marks done exist, the rest is written here first, so its pointers
name planned modules too. Companions:
[`notes_ai_learning.md`](notes_ai_learning.md) (the same subject when
nobody writes the rules: neural networks and self-play), and
[`notes_ai_related_work.md`](../related-work/notes_ai_related_work.md).

The word is a bad fit and worth getting out of the way. Almost none of
this is intelligence: it is search, arithmetic on vectors, and a
`match` on a state. What the player experiences as intent is nearly
always one short rule seen from the outside -- which is the good news,
because it means a page of code can produce something that looks alive.

## 0. Where the code is, and a reading order

| module (`ai/`) | what | section |
|---|---|---|
| `Pathfind` (done) | breadth-first, Dijkstra, A*, flow fields | §2, §3 |
| `Steering` (done) | seek, flee, arrive, wander, pursue, avoid | §4 |
| `Flock` (done) | separation, alignment, cohesion | §5 |
| `Fsm`, `Behavior`, `Utility` (done) | choosing what to do | §6 |
| `Sense`, `Bot` (done) | a mind that plays through the player's own inputs | §6 |
| `Minimax` (done) | the game tree, and alpha-beta | §7, §8 |
| `Deepening`, `Zobrist` (done) | making the search go deeper | §9 |
| `Mcts` (done) | playing without an evaluation function | §10 |
| `playground/Ai` (done) | the Evan-style API over all of it | §14 |
| `playground/Ai_debug` (done) | drawing what it thinks | §11 |

Read §2 to §5 for the real-time half (a world at 60 fps), §7 to §10
for the turn-taking half (an opponent). They barely touch. §6 is where
both are put to work: how a character decides what to do -- and, at
its end, what changes when the character is a *bot*, playing the same
game as you through the same inputs.

## 1. What a game's AI actually has to answer

Three questions, and they need completely different machinery:

```
   where do I go?        a path through a world       §2, §3
   how do I move?        an acceleration, this frame  §4, §5
   what do I do?         a choice between behaviours  §6
   what move do I play?  a search over the future     §7 - §10
```

Notice what is *not* on the list: understanding, learning, planning in
any general sense. A ghost that heads for the tile you are standing on
is four lines, and players have been ascribing personalities to those
four lines since 1980.

## 2. The world as a graph: one search, three queues

A grid of tiles is a graph: each walkable tile a node, each step an
edge. Finding a way is growing a **frontier** out from the start,
remembering for every tile the one it was reached from, until the goal
comes up; then read those links backwards, and that's the path.

All three classic searches are that same loop. The only difference is
**which tile you take out of the frontier next**:

```
  breadth-first   the oldest one (a queue)     fewest steps
  Dijkstra        the cheapest so far, g       cheapest, when steps differ
  A*              cheapest of g + h            the same, aimed at the goal
```

`h` is the **heuristic**: a guess at what is left from here. On a grid
where you move in four directions, the Manhattan distance
|dx| + |dy| is the classic one -- exactly right when nothing is in the
way, an underestimate otherwise. That "never overestimates" property
(**admissible**) is the whole guarantee: with it, A* still finds a
shortest path; without it, A* gets faster and can be wrong.

What the difference looks like, from `Pathfind`'s example -- the cells
each search actually pulls out of its frontier, going from S to G:

```
   breadth-first / Dijkstra        A*
   . . o o o o o o o . .       . . . . . . . . . . .
   . o o o o o o o o o .       . . . . . . . . . . .
   o o o o o o o o o o o       . . . . . . . . . . .
   o o S o o o o o G o o       . . S o o o o o G . .
   o o o o o o o o o o o       . . . . . . . . . . .
   . o o o o o o o o o .       . . . . . . . . . . .
   . . o o o o o o o . .       . . . . . . . . . . .

   80 cells for a 10-step path      11 cells for the same path
```

With `h = 0`, A* *is* Dijkstra -- they are one algorithm with a knob.

The numbers are worth keeping, because they are the honest version of
"A* is better". Put a patch of mud between S and G, two cells wide,
each step in it costing 5 instead of 1:

| | steps | cost | cells looked at |
|---|---|---|---|
| breadth-first | 10 | 18 (wades in) | 80 |
| Dijkstra | 16 | 16 (goes around) | 115 |
| A* | 16 | 16 | 75 |

Two lessons in one table. Breadth-first is not "worse", it answers a
different question -- fewest *steps*, not cheapest; if every step costs
the same, use it, it is the simplest thing that works. And A*'s
advantage shrinks exactly when the cheap way is not the straight way,
because that is when the guess is misleading.

`examples/AiPathfinding.ml` is this table, alive: draw walls and mud
with the mouse, press b, d or a, and watch the frontier light up in
the order the search takes it.

## 3. One search for a crowd: flow fields

Fifty units ordered to the same place is fifty A* searches, every time
anyone is nudged. Instead run Dijkstra from the **destination**, and
don't stop: you end up knowing the cost from there to every tile.
Read backwards, each tile points at the neighbour nearest the goal.

```
     the field, from G           the arrows units follow
     3 2 3 4 5                   > v < < <
     2 1 2 3 4                   > v < < <
     1 G 1 2 3                   > G < < <
```

One search for any number of units; each one only ever looks at the
tile under its feet, so being pushed off course costs nothing. This is
how a strategy game moves a crowd, and it is `Pathfind.field` and
`downhill`, with `gamekits/rts/Orders` turning it into TinyDune2's and
TinyWarcraft2's orders.

The same field, read differently, is an **influence map**: run it from
every enemy at once and you have "how dangerous is this tile", which is
how a unit decides to retreat.

## 4. Steering: forces, not paths

A path is a plan; movement is a force. Reynolds's insight (1999) was
that a believable character is a body with a small **steering force**
added each frame, and that a handful of such forces cover nearly
everything:

```
  seek     force toward the target, at full speed
  flee     the same, negated
  arrive   like seek, but the desired speed shrinks near the target,
           so it stops instead of orbiting
  pursue   seek where the target *will be*: its position plus its
           velocity times a guess at how long you will take
  wander   a point drifting slowly around a small circle in front of
           you -- random, but with momentum, so it looks like intent
           instead of a twitch
  avoid    a sideways push away from whatever is in front
```

Each one is:

```
  desired = (where I want to go) - (where I am), scaled to top speed
  steer   = desired - my velocity,  clamped to a maximum force
```

and because the result is an acceleration, they compose with the
physics engine that is already here: `Physics.body` takes
accelerations (`fall`, `thrust`, `push`), so a steering behaviour is
just another one, and adding two behaviours is adding two vectors.

That is also why `arrive` and `wander` matter more than they look.
`seek` alone produces the unmistakable video-game homing missile;
`arrive` is what makes something look like it *meant* to stop there,
and `wander` is what makes an idle creature look alive rather than
frozen. The believability is in the derivative.

## 5. Flocking: three rules

Reynolds again, 1987, and still the best short demonstration in the
whole subject. Each boid looks only at its neighbours -- the ones
within a radius -- and adds three forces:

```
  separation   steer away from the ones too close      (don't collide)
  alignment    match the neighbours' average heading   (go with them)
  cohesion     steer toward the neighbours' average position (stay together)
```

```
   separation        alignment          cohesion
    o   <-o           o->  o->           o  ->o
   ->o                  o->               o<- o
    o   o->           o->  o->           o<-  o
```

No leader, no global plan, nothing that knows what a flock is; and the
flock appears, splits around an obstacle and rejoins. It is the
cheapest possible lesson in emergence, which is why the same three
rules turn up in this repository twice: here, and as
`TurtlesFlocking` in the NetLogo-flavoured plan
([`plan_teaching_languages.md`](../plans/plan_teaching_languages.md)).

Two practical notes the demos always skip. The neighbour search is the
expensive part, and it is exactly the physics engine's broad phase
(`physics/2d/Broadphase`) -- a grid, not every pair. And the three
weights are the whole character of the flock: separation alone is a
gas, cohesion alone is a blob, alignment alone is a current.

## 6. Deciding what to do

The real-time half's last question. Three answers, in the order the
industry found them, and each is worth knowing because each fails
differently.

**A state machine.** States, and the conditions that move between
them. Pac-Man's ghosts, 1980, still the textbook example: *scatter*
(go to your own corner), *chase* (each ghost with its own rule),
*frightened* (run, randomly), *eaten* (return to the box). The
scatter/chase alternation is on a timer, and it is why the ghosts
periodically back off and let you breathe -- a design decision that
reads as mercy.

The four chase rules are the thing to steal, because they show how
little it takes to make characters feel different:

```
  Blinky    target = Pac-Man's tile                     (relentless)
  Pinky     target = four tiles ahead of Pac-Man        (cuts you off)
  Inky      target = Blinky's tile, reflected through
            two tiles ahead of Pac-Man                  (unpredictable)
  Clyde     Blinky's rule, unless he is within 8 tiles,
            then his own corner                         (shy)
```

Four one-line rules, four personalities, forty-five years of people
saying the ghosts "hunt in a pack". They don't; they never talk.

**Behavior trees** (Halo 2, 2005) are what you reach for when the state
machine's transitions outnumber its states -- n states have n² possible
transitions, and somewhere around a dozen states that stops fitting in
a head. A tree of *selectors* (try these until one works) and
*sequences* (do these in order until one fails), re-walked each tick;
the transitions become the tree's shape. The cost is that everything
the nodes need to share moves into a blackboard, which is a global
variable wearing a hat.

**Utility**: no graph at all. Score every option with a number and take
the best -- The Sims's needs, and the reason a Sim will abandon a
conversation to go to the bathroom without anyone writing that
transition. It scales beautifully and debugs horribly: when something
silly happens, the answer is always "some curve crossed another curve".

### A bot is the same mind, with its hands tied

All of the above decides by *reaching into the game's state*: a ghost
reads Pac-Man's tile. That is fine for a ghost, which is a rule wearing
a sprite. It is not fine for a **bot** -- an opponent that stands where
a player stands, in a game a human is playing at the same time.

The shape a bot wants is one line of types, and three games in this
repository found it independently:

```
   senses  --->   the mind   --->   intent   --->  the game's update
   what a         steering,         the same
   player         fsm, aim          record the
   could know                       keys fill
```

`TinySoldat.ml` has exactly that: a record `intent` (`run`,
`jump`, `jet`, `shoot`, `grenade`, `aim`), filled either by `human`
from the keyboard and mouse or by `bot` from the world, and an update
that cannot tell which. `gamekits/racing/Topdown.computer` returns
`(gas, steer)` -- the two numbers the player's keys produce.
`TinyPong.ml`'s paddle follows the ball *at a limited speed*,
and that limit is the entire difficulty setting.

Why the restriction matters, and why it is the interesting part: a bot
that knows everything and reacts instantly is trivial to write and
horrible to play against. The knobs that make one feel like an
opponent are all *handicaps* -- a reaction delay of some frames
(a human's is around a quarter of a second), an aim that starts off
and settles, a limit on how often it changes its mind, and senses that
stop at a wall (a line-of-sight test, and a memory of where you were
last seen, which is what makes a bot *search* for you instead of
tracking you through the floor). The bots people remember -- Quake
III's, whose Area Awareness System is a small book on its own, and
Counter-Strike's, whose navigation mesh was learned by watching people
walk -- are mostly this: careful restriction, not clever search.

It is also the one place in game AI where the type system does the
teaching. If a bot's mind is `senses -> intent`, then a bot that peeks
at the whole world *does not compile*, and "no cheating" stops being a
promise and becomes a signature. `ai/Sense` and `ai/Bot` are that
door, and nothing more: the mind behind them is the steering, the
state machine and the pathfinding of the sections above, unchanged.
`Sense` keeps, per target, whether it is visible (the game says
whether the line is clear: only it knows its walls), whether it is
audible, where it was last seen and how many frames ago, and how long
it has been in sight; `Bot` holds the loop and the handicaps -- the
reaction delay (it decides on senses `delay` frames old), the input
rate (it may change its mind every `rate` frames, and repeats itself
in between), and an aim error that halves the longer a target stays
visible. With a delay of 15 frames and a rate of 6, a target appearing
at frame 100 is shot at frame 120; with both off, at frame 100, which
is the machine you can feel.

`TinySoldat.ml` has both bots now, chosen by the flag `ai=engine`, as
its physics and TinyPacman's ghosts offer both ways of being written.
Reading them side by side shows what the door costs: the hand-written
bot takes the nearest enemy *through the walls*, so it never has to
look for anyone; take that away and it needs a behaviour it never had,
patrolling when it knows nothing, and hunting the place where it last
saw you. Honest senses are not a smaller program -- that game grew by
a quarter -- they are a different one.

`TinyBoomerangFu.ml` is the second one with the flag, and it shows a
different face of the same layer: its three cooks keep their tactics
and their four numbers, and lose three things. They see an enemy only
when no pillar stands between them (and remember it for ninety frames
after it disappears behind one), they act on what they saw six frames
ago, and they change their mind twenty times a second rather than
sixty. Its `dodge / hunt / keep away` becomes an `Fsm`, which is worth
it for one reason: the hysteresis that game learned the hard way -- an
agent that leaves *dodge* the instant a boomerang's line is clear
steps straight back into it -- is a guard on a transition
(`Fsm.after 8`) instead of a condition buried in an if. What it does
*not* take is steering: its characters have a fixed speed and a
committed dash, so there is no velocity to steer, and `Ai.seek` and
its family have nothing to offer them (§14).

## 7. The game tree: minimax

The turn-taking half. A two-player game where both sides see everything
is a tree: positions are nodes, moves are edges. One player, MAX, wants
the final number high; MIN wants it low. Search `depth` moves ahead,
score the leaves with an **evaluation function** (a guess: who stands
better here?), and carry the numbers back up -- the largest at a MAX
node, the smallest at a MIN node, each side assuming the other plays
well. Russell and Norvig's tree, which is `Minimax`'s worked example:

```
                      MAX                 3
            ___________|___________
           |           |           |
          MIN         MIN         MIN     3     2     2
         / | \       / | \       / | \
        3 12  8     2  4  6    14  5  2
```

MAX plays left, for 3. Thirteen nodes.

Everything hard about this is in the evaluation function, not the
search. The search is fifteen lines and is the same for every game; the
evaluation is where the game's knowledge lives, and where it goes
wrong. `AiOthello`'s is a table of what each square is worth:

```
      100 -20  10   5   5  10 -20 100
      -20 -50  -2  -2  -2  -2 -50 -20
       10  -2  -1  -1  -1  -1  -2  10
       ...
```

Corners can never be flipped back, so they are worth everything; the
squares next to a corner hand it over, so they are worth less than
nothing. And note what is *absent*: the number of disks you have, which
is the beginner's idea of the score and is actively misleading before
the endgame.

When the game is small enough, the guess disappears entirely.
Tic-tac-toe can be searched to the last move, so every position is
*known* to win, draw or lose -- which is what `examples/AiTictactoe.ml`
shows on every empty square, and why it cannot be beaten.

## 8. Alpha-beta: the same answer, far fewer nodes

In the tree above, after the left branch MAX knows it can get 3. In the
middle branch the first leaf is 2, so MIN can hold MAX to at most 2
there -- already worse than the 3 in hand. The 4 and the 6 cannot
change the decision, so they are never looked at. That is the entire
idea: **alpha** is MAX's best guaranteed so far, **beta** is MIN's, and
a branch is cut the moment it cannot beat what is already assured.

The answer is provably identical. Thirteen nodes become eleven here;
on tic-tac-toe from the empty board:

```
  minimax      549,946 positions
  alpha-beta    18,297 positions      3%
```

With moves tried in a good order, alpha-beta visits about b^(d/2)
nodes instead of b^d -- **twice as deep in the same time**, which in
chess is the difference between a beginner and a club player. With
moves in the worst order, it saves nothing. This is why §9 is mostly
about move *ordering* rather than about search at all.

One subtlety worth keeping, because it bites: after a cut, the values
of the *other* moves are no longer exact -- a cut branch's number is
only a bound ("at most 2"). If you want to show the player what the
computer thinks of every move, as `AiOthello`'s "v" key does, only
plain minimax gives you honest numbers.

## 9. Going deeper: the four tricks

Every one of these is in the engines, and every one is small:

- **Iterative deepening**: search 1 move ahead, then 2, then 3, until
  the clock runs out, keeping the last completed answer. It sounds
  wasteful and isn't: the tree grows so fast that all the earlier
  depths together cost a fraction of the last one, and they hand you a
  move ordering (last depth's best move, first) that pays for itself
  many times over. It also turns "think 4 deep" into "think for 0.2
  seconds", which is what a game actually wants.
- **Transposition tables**: the same position reached by different move
  orders is the same position. Store what you learned about it, keyed
  by a **Zobrist hash** -- one random 64-bit number per (piece,
  square), the position's key their xor, so a move updates the key with
  two xors instead of rehashing a board.
- **Quiescence search**: don't evaluate in the middle of a capture
  sequence. At the depth limit, keep searching captures only, until the
  position is quiet. Without it, a search happily walks into losing a
  queen one ply past its horizon -- the **horizon effect**, and the
  reason a naive engine plays bizarre delaying moves.
- **Move ordering**: captures first, then moves that were good
  elsewhere (killer moves). Cheap, and worth more than a whole extra
  ply of search.

Three of the four are `ai/Deepening` and `ai/Zobrist`, and Connect 4's
opening searched 7 moves ahead says what each is worth
(`AiConnect4.ml`, the numbers checked in `tests/games`):

```
   alpha-beta, the columns left to right    65,724 nodes
   the middle columns first                  9,449
   + iterative deepening, 1 to 7            12,818
   + the transposition table                 7,742
```

The third line is the interesting one: iterative deepening *cost* a
third more here. It is not free -- the shallower searches are real work
-- and it repays that only through the ordering it hands the deeper
ones. Connect 4 already has a good hint (the middle columns are in more
fours: 13 against 3 at the edge), so there was little ordering left to
buy. What iterative deepening always buys, hint or not, is the right to
stop whenever asked and still have a complete answer, which is what a
game at 60 frames a second needs; and the table makes its repeats
cheap, which is why those two lines belong together.

## 10. When you have no evaluation function: Monte Carlo

Go broke everything above. The board has ~250 moves per position
instead of chess's ~35, and worse, nobody could write the evaluation
function: whether a Go position is good depends on whether groups
eventually live, which is itself as hard as playing. From 1970 to 2005,
programs that were hand-written and knowledge-heavy stayed weak.

The 2006 answer was to give up on knowledge entirely. To score a
position, **play it out at random to the end**, many times, and count
the wins. Random play is terrible; the *average* of thousands of random
playouts is a surprisingly good estimate of who is better.

Do that inside a tree, and you get MCTS. Four steps per iteration:

```
  select    walk down the tree, at each node picking the child with
            the best UCB score
  expand    add one new child at the bottom
  simulate  play randomly from there to the end
  backup    add the result to every node on the way back up
```

The selection rule, **UCT** (Kocsis and Szepesvári, 2006), is the
whole trick:

```
                 wins(c)          ln N(parent)
  score(c)  =   ---------  +  C * sqrt( ------------ )
                  N(c)                     N(c)

                exploitation          exploration
                what has been         what has barely been
                working               tried, and might
```

The left term takes the move that has been winning; the right term
grows for children that have been visited little, so nothing is
abandoned on a small sample. The tree grows lopsided toward the good
lines *by itself* -- no depth limit, no ordering, no evaluation
function. Stop whenever you like and play the most-visited child.

Three properties make it worth a module of its own: it needs no
knowledge of the game beyond its rules, it is **anytime** (interrupt it
at any moment and it has an answer, which is perfect for a frame
budget), and it does not care how large the branching factor is.

The first of those is nearly true rather than exactly true, and the
exception is worth knowing. Uniformly random playouts work on
tic-tac-toe (`Unit_mcts`: with 2000 playouts a move it opens in the
centre, blocks a threat and takes a win, blocking 20 times out of 20
over 20 seeds -- against 8 out of 20 with ten playouts, barely better
than chance), but in Go they need exactly one rule: **do not
fill your own eyes**. A random player that fills its own eyes kills
its own groups, and then the playouts say nothing about the position.
That one rule -- a point surrounded by your own stones is not played
-- is the smallest thing that makes random Go mean something, and it
is where every Monte Carlo Go program starts.

`games/puzzle/AiGo.ml` is that program, on 9x9. It has no evaluation
function anywhere in it -- grep for one -- and it plays like a weak
amateur, which is the honest result: pure MCTS on 9x9 in 2006 was
about that, and it beat thirty years of handcrafted Go programs. The
numbers on screen are the whole method: a thousand random games, the
tree of a thousand positions they grew, and the share of them the move
it chose won. A playout costs about 1.2 ms there, so it thinks a dozen
a frame for a second and a half a move, never stopping the game --
which is the anytime property made visible, and is exactly what a
chess engine cannot do (interrupt alpha-beta and you have nothing; §9
buys that back with iterative deepening). What lifted this to
superhuman ten years later was replacing the random playouts and the
win counts with a neural network -- and the two places it goes are
`Mcts`'s own `?prior` (the selection rule becomes PUCT: an unvisited
move is as attractive as the policy says, not infinitely attractive)
and `?evaluate` (an opinion instead of a random game). Measured on
tic-tac-toe with a perfect value function standing in for a trained
one: 12 of 12 won positions found at twelve playouts against 10 of 12,
and 11-0 over twenty games against the version with neither. The
network that would fill those hooks for Go is
[`notes_ai_learning.md`](notes_ai_learning.md) §9, and the compute it
needs is why AiGo still plays out at random.

## 11. Seeing what it thinks

Every algorithm here is invisible by default and obvious once drawn,
which is why `playground/Ai_debug` is a first-class piece rather than
a convenience. Four drawings, one per thing an AI does, each a single
call and each returning plain shapes:

- `way`, the tiles it means to walk, joined, with a ring on the last;
- `field`, an arrow per tile fading with the distance still to go --
  the plan of a hundred monsters in one picture;
- `thoughts`, a bar per move against a zero line, longest first, the
  chosen one bright: `Ai.thoughts` or `Ai.so_far` made readable;
- `machine`, the modes in a ring with the current one lit and counting
  its frames, an arrow per change of mind carrying the few words that
  trigger it. A state machine is a drawing that was written down as a
  list, and this puts it back.

`examples/AiDebug.ml` is all four at once: a walker taking a way
across a map over the field everyone would follow, its mind beside it,
and a game of Nim whose opponent says what it makes of each move.

Unlike `Audio_debug`, this is not the backend's to draw and has no
debug key of its own: the platform knows the samples it plays, but
only a game knows what its enemies are thinking, so a game draws these
where and when it likes (TinyTowerDefense's "p" is the pattern).

Still missing, and worth doing: the frontier in the order it was taken
(the entire content of `AiPathfinding`, which draws its own), each
steering force as a vector out of its body with the wander circle in
front of it, and a bot's senses -- what it has seen and how stale it
is, the only honest way to check that it is not cheating.

Debugging an AI by reading its code is nearly hopeless; debugging it by
watching the arrows takes seconds.

## 12. Compared with Stockfish, Recast/Detour and the engines

**Search.** Stockfish is still §8's alpha-beta, with all four of §9's
tricks and dozens more (null-move pruning, late-move reductions,
aspiration windows, a search spread over many threads), on a board
held as 64-bit **bitboards** so that a move is a few shifts and ands,
and a small neural network as its evaluation (NNUE, since 2020) instead
of a hand-written table. Ours has no move generator in `ai/` at all:
`Minimax` takes the game's `moves`, `play` and `score` as functions,
so the same search plays tic-tac-toe, Othello and chess, and
`AiChess.ml` thinks 3 moves ahead where Stockfish, in the same second,
goes twenty and more.

**Navigation.** Recast turns a level's triangles into a **navigation
mesh** (the floor as convex polygons, not tiles), and Detour runs A* on
it, then straightens the zigzag with the funnel algorithm, and keeps a
crowd of agents apart with local avoidance. `Pathfind`'s nodes are
whatever the game says they are (`problem` is a `neighbors` function),
so a navmesh would fit in the same A*; what is missing is building one,
and the string-pulling after. Behavior-tree libraries
(BehaviorTree.CPP in robotics, Unreal's, Unity's assets) differ from
`Behavior` in the one way its `.mli` states: their nodes *run* over
many frames and answer "running", with a blackboard beside them; ours
only decides, and the game's model is the memory. The landscape at
length -- the games, the champions, the teaching lineage -- is in
[`notes_ai_related_work.md`](../related-work/notes_ai_related_work.md).

## 13. What's missing, and exercises

In rough order of difficulty:

- a binary heap for `Pathfind`'s frontier, which is a list kept in
  order (its `.mli` says so); time A* on a large map before and after;
- eight directions in `Pathfind`, with a diagonal step costing √2 and
  the octile distance as the heuristic instead of `manhattan`;
- the neighbour search of `Flock.neighbours` through a grid
  (`physics/2d/Broadphase`, §5) instead of every pair, and the number
  of boids a frame can hold, before and after;
- iterative deepening with a time limit (§9), around
  `Minimax.alphabeta` -- listed as an exercise in both `AiChess.ml` and
  `AiOthello.ml`;
- a transposition table with a Zobrist hash (§9), for `AiChess.ml`;
- nested states in `Fsm` (Harel's statecharts, which its `.mli` notes
  it does not do);
- a "running" status and a blackboard for `Behavior` (§6), for actions
  that take several frames;
- Jump Point Search (Harabor and Grastien, 2011), `Pathfind.astar`
  skipping the symmetric paths of an open grid;
- goal-oriented action planning (Orkin's F.E.A.R., 2005): `Pathfind`'s
  A* searching over *world states*, an action's preconditions and
  effects as the edges, which `Pathfind.problem`'s polymorphic `'node`
  already allows;
- playouts in `AiGo` that answer a capture or an atari instead of
  playing anywhere (the next thing every Monte Carlo Go program did
  after the eye rule), and RAVE / all-moves-as-first (Gelly and Silver,
  2007), which lets a move's results elsewhere in a playout count
  towards it here -- both worth far more per playout than more
  playouts are;
- the full ko rule (`AiGo` has the simple one: a move may not take back
  the single stone that just took), which means keeping the positions
  already seen;
- a bot for a *second* genre on `Sense` and `Bot` (§6):
  `gamekits/racing/Topdown.computer` (a racing line) and
  `TinyPong.ml`'s paddle are the two nearest, and a second user is the
  real test of that layer -- TinySoldat alone only showed what honest
  senses cost;
- hearing used for something (`Sense`'s `audible` is kept and no game
  reads it yet): a bot that turns towards a shot it could not see.

## 14. In the playground

The API (`playground/Ai.mli`) follows Evan's rule -- values
and small named things, not machinery. Steering behaviours are forces
on `Physics.body`, so they stack with gravity and thrust and each
other:

```ocaml
let update _ fish = fish |> List.map (fun f -> f |> flocking fish |> step)
```

The other four families are built now, each a few verbs over a module
of `ai/`:

```ocaml
let step = Ai.way ~walkable:(free level) monster.cell door      (* §2 *)
let field = Ai.flow ~walkable:(free level) door                 (* §3, one search, a crowd *)
let reply = Ai.best_move (Ai.thinking_ahead 4 othello) board    (* §7-§9 *)
let ghost = { g with mind = Ai.deciding changes g g.mind }      (* §6 *)
let intent = if p.human then keys computer else Ai.thinks brain world p.playing  (* §6 *)
```

and the search, the table, the frontier and the seeds stay on the
other side of the door. Two decisions the writing settled. There is no
`Ai.within 0.2` (a budget in seconds): the playground has no clock in
an `update`, and a budget in seconds cannot be replayed frame by frame
either, so thinking across frames is a budget of *work* a frame --
`Ai.pondering`, `ponder`, `settled`, `answer`, with `a_frame_of n` to
say how much a frame is worth, since only the game knows what its own
moves cost. And `Ai.thoughts` costs a search per move rather than one
search: a branch alpha-beta cuts never learns its own value, only that
it was not worth the trouble, so a search that says what it thinks of
everything is a slower search (`Minimax.mli` warns of exactly this,
and the first version of `Ai.thoughts` drew the bounds as if they were
values -- three equal bars where one move won and two lost).

Who uses what: `Ai.seek`, `flee`, `arrive`, `chase`, `escaping`,
`wandering`, `avoiding`, `following` (§4), `flocking` (§5) and
`facing`, each a verb on a `Physics.body`, by `examples/AiSteering.ml`
and `examples/AiFlock.ml`; `Ai.way` by `TinyDiablo.ml` (a click is a
path) and `TinyDungeonMaster.ml` (the monsters walk to you), where it
replaced a hand-written `Pathfind.problem` each, with the golden
frames coming out pixel for pixel the same; `Ai.way`, `Ai.flow`,
`Ai.deciding` and `Ai.thinking_ahead` together by
`examples/AiDebug.ml`, which draws all four (§11).

One thing that was tried and is not here: a board-game *builder*, in
the shape of `Logo`, `Bigbang` and `Puzzlescript` -- rules and squares
in, a whole `app` out -- since AiTictactoe, AiOthello, AiConnect4 and
AiGo each write the same cursor, click, reply and restart. It was
written as a kit and AiConnect4 was ported onto it: 305 lines became
292. The loop is not where these games spend their lines, a game that
hands over its model has to hand back a second rules record and its
own cursor drawing, and the two biggest games could not use it at all
(chess's move is two clicks; Go's opponent needs its own playout). It
was deleted; the plan keeps the numbers. Thirty readable lines a game,
and the part each does differently is the part that gives it its
character.

The rest is still used straight from `ai/`, and on purpose. The demo
games (`AiTictactoe`, `AiOthello`, `AiChess`, `AiConnect4`, `AiGo`,
`AiPathfinding`) are *about* the algorithm -- they count nodes and
compare searches, which is precisely what the layer hides -- and
`TinyXCOM.ml` keeps its own `Pathfind.problem` because its moves have
rules the layer has no word for (no cutting a wall's corner
diagonally, a friend blocking the tile, a step costing 4 or 6 time
units). A layer that swallowed those would be the whole of `Pathfind`
with different names. So: `Pathfind.astar` by `TinyTowerDefense.ml`
and `TinyXCOM.ml`, the
flow field (§3) by `gamekits/rts/Orders`, for `TinyDune2.ml` and
`TinyWarcraft2.ml`, the
three searches side by side in `examples/AiPathfinding.ml` (§2);
`Fsm` by the ghosts of `TinyPacman.ml` and `examples/AiGhosts.ml` (§6);
`Sense` and `Bot` by `TinySoldat.ml`'s soldiers and
`TinyBoomerangFu.ml`'s cooks, both with `ai=engine` (the second also
on `Fsm`, for its dodge/hunt/keep away and their hysteresis), and by
`examples/AiBots.ml`, where the four handicaps are four keys to turn
off -- the last of them its senses, after which it sees you through the
walls, and the demonstration is how quickly that stops being a game
(§6);
`Minimax` by `examples/AiTictactoe.ml` (§7, §8), `AiOthello.ml` and
`AiChess.ml` (with its quiescence, §9); `Deepening` and `Zobrist` by
`AiConnect4.ml`, which prints what they save after every move (§9);
`Mcts` by `AiGo.ml`, the one game here whose opponent has nothing to
say about a position except how often random play wins it (§10).
`Behavior` and `Utility` have only their tests so far.

## Glossary

- **Node**, **edge**, **frontier**: a graph search's pieces; **visited**:
  what it actually looked at, which is the cost.
- **Heuristic** (h), **cost so far** (g): A*'s two halves;
  **admissible**: a heuristic that never overestimates, which is what
  keeps A* right.
- **Flow field**: one search from a destination, followed by everyone.
- **Influence map**: the same, from the enemies: danger per tile.
- **Steering force**: desired velocity minus current velocity, clamped.
- **Arrive**, **wander**, **pursue**: the behaviours that read as
  intent rather than as homing.
- **Boid**: one member of a flock; **separation**, **alignment**,
  **cohesion**: its three rules.
- **Emergence**: behaviour of the group that is in no member's rule.
- **State machine**, **behavior tree**, **utility**: three ways to pick
  what to do; **blackboard**: what a behavior tree shares.
- **Bot**: a mind that plays through the player's own inputs;
  **intent**: the record those inputs fill, produced by a human or by
  a bot; **senses**: what a bot is allowed to know, and the reason it
  can be kept honest by a type.
- **Handicap knobs**: reaction delay, aim error, input rate -- what
  difficulty is made of when a bot is not allowed to cheat.
- **Game tree**, **ply**: one player's move; **MAX**, **MIN**.
- **Evaluation function**: the guess at a leaf, where a game's
  knowledge lives.
- **Alpha-beta**, **cut**: skipping what cannot change the answer;
  **move ordering**: what decides how much it saves.
- **Iterative deepening**, **transposition table**, **Zobrist hash**,
  **quiescence**, **horizon effect**: the four tricks of §9.
- **Playout** (rollout): a random game to the end; **MCTS**, **UCT**,
  **exploration vs exploitation**; **anytime**: interruptible with an
  answer ready.
- **Branching factor** (b), **depth** (d): why b^d is the enemy and
  b^(d/2) is the prize.

## References

- John von Neumann, "Zur Theorie der Gesellschaftsspiele",
  Mathematische Annalen 100, 1928 (the minimax theorem).
- Claude E. Shannon, "Programming a Computer for Playing Chess",
  Philosophical Magazine 41(314), 1950.
- Edsger W. Dijkstra, "A Note on Two Problems in Connexion with
  Graphs", Numerische Mathematik 1:269-271, 1959.
- Peter E. Hart, Nils J. Nilsson, Bertram Raphael, "A Formal Basis for
  the Heuristic Determination of Minimum Cost Paths", IEEE Transactions
  on Systems Science and Cybernetics 4(2):100-107, 1968.
- Albert L. Zobrist, "A New Hashing Method with Application for Game
  Playing", Technical Report 88, University of Wisconsin, 1970.
- Donald E. Knuth, Ronald W. Moore, "An Analysis of Alpha-Beta
  Pruning", Artificial Intelligence 6(4):293-326, 1975.
- Craig W. Reynolds, "Flocks, Herds, and Schools: A Distributed
  Behavioral Model", SIGGRAPH '87.
- David Harel, "Statecharts: A Visual Formalism for Complex Systems",
  Science of Computer Programming 8(3):231-274, 1987.
- Stuart Russell, Peter Norvig, "Artificial Intelligence: A Modern
  Approach", Prentice Hall, 1995 (in the 3rd edition, 2009: chapter 3,
  search; chapter 5, games, whose tree is §7's).
- Craig W. Reynolds, "Steering Behaviors For Autonomous Characters",
  Game Developers Conference, 1999.
- Peter Auer, Nicolò Cesa-Bianchi, Paul Fischer, "Finite-time Analysis
  of the Multiarmed Bandit Problem", Machine Learning 47, 2002 (UCB).
- Damian Isla, "Handling Complexity in the Halo 2 AI", Game Developers
  Conference, 2005.
- Mat Buckland, "Programming Game AI by Example", Wordware, 2005.
- Jeff Orkin, "Three States and a Plan: The A.I. of F.E.A.R.", Game
  Developers Conference, 2006.
- Levente Kocsis, Csaba Szepesvári, "Bandit based Monte-Carlo
  Planning", ECML 2006 (UCT).
- Rémi Coulom, "Efficient Selectivity and Backup Operators in
  Monte-Carlo Tree Search", Computers and Games 2006.
- Dave Mark, "Behavioral Mathematics for Game AI", Course Technology,
  2009.
- Jamey Pittman, "The Pac-Man Dossier", 2009.
- Daniel Harabor, Alban Grastien, "Online Graph Pruning for Pathfinding
  on Grid Maps", AAAI 2011 (Jump Point Search).
- Cameron B. Browne et al., "A Survey of Monte Carlo Tree Search
  Methods", IEEE Transactions on Computational Intelligence and AI in
  Games 4(1), 2012.
- Michele Colledanchise, Petter Ögren, "Behavior Trees in Robotics and
  AI: An Introduction", CRC Press, 2018.
