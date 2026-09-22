# ai/ vs. the rest of the game AI world

The deciding twin of [`notes_playground_related_work.md`](notes_playground_related_work.md)
(2D graphics), [`notes_physics_related_work.md`](notes_physics_related_work.md)
(physics) and [`notes_audio_related_work.md`](notes_audio_related_work.md)
(sound): where `ai/` and its Evan-style `Ai` API (see
[`plan_ai_teaching.md`](../plans/plan_ai_teaching.md)) come from, and
where they sit among the ghosts, the chess machines, the middleware and
the learning systems. The same through-line as the other three: **the
real systems are built to win, or to ship; `ai/` is built to be
legible**, and `Ai` to be sayable in one line.

One warning specific to this field. Game AI and academic AI have been
two different subjects for fifty years -- one wants a believable
opponent that loses entertainingly at 60 fps, the other wants to win --
and the words are shared while the goals are not. Half of what follows
is about the first, half about the second, and it is worth knowing at
every moment which one is being discussed.

## The one-line version

| | What it optimizes for | What you write |
|---|---|---|
| Arcade and console AI (Pac-Man, Galaga, Doom) | A convincing threat in a few hundred bytes | A target tile, a timer, a state |
| Middleware (Unity NavMesh, Unreal behavior trees, recast/detour, RVO2) | Shipping large worlds with hundreds of agents | A baked navmesh, a tree in an editor, tuned parameters |
| Game AI craft (F.E.A.R.'s GOAP, Halo's trees, The Sims' utility, Left 4 Dead's director) | The *player's experience* of intelligence | Goals, scored options, pacing rules |
| The champions (Deep Blue, Chinook, Logistello, AlphaGo) | Winning, at any engineering cost | Millions of positions a second, opening books, endgame tables, TPUs |
| ML frameworks (PyTorch, TensorFlow, JAX) | Training anything, fast, on GPUs | Tensors, autodiff, optimizers, someone else's kernels |
| Teaching code (Red Blob Games, Nature of Code, micrograd) | Understanding one idea at a time | A page you can read in full |
| `ai/` + `Ai` | Every decision readable, and drawable while it runs | `f \|> flocking fish \|> step`, `thinking_ahead 4 rules` |

## Part 1: the games -- where game AI came from

- **Space Invaders (1978)**: no AI at all, and the point of the entry.
  The aliens follow a fixed pattern that speeds up as they die -- an
  accident of the hardware drawing fewer sprites faster -- and players
  read it as mounting panic. The first lesson of the field is that
  perceived intelligence and implemented intelligence are unrelated.
- **Pac-Man (1980)**: four ghosts, four one-line target rules, a
  chase/scatter timer, and the enduring illusion of a pack hunt. Still
  the best teaching example in the subject, and documented to the frame
  in Jamey Pittman's *Pac-Man Dossier* (2009).
- **Doom (1993)**: monsters as a state machine over a handful of
  states, with line-of-sight checks and a "noise wakes the neighbours"
  propagation through the sectors -- and enough to carry a genre.
- **Half-Life (1998)**: the marines' flanking and the shouted callouts,
  mostly a state machine over navigation nodes, and the game that made
  the public notice enemy AI as a feature.
- **The deathmatch bots (1997-2004)**: a lineage of its own, because
  it is the only one where the AI plays *the player's* game. The
  Reaper and Eraser bots were written by players for Quake and Quake
  II, which shipped with none; **Quake III Arena** (1999) made them
  the single-player mode, and Jan Paul van Waveren's thesis on its
  bot -- the Area Awareness System, fuzzy weapon choice, per-bot
  "characters" -- is still the reference for what a bot should know
  about a map. **Counter-Strike's official bot** (Michael Booth, GDC
  2004) learned its navigation mesh by watching people walk, and was
  designed to be *fun* rather than strong. The whole lineage is an
  argument that a good bot is made of restrictions -- reaction delay,
  aim error, line of sight -- and not of better search. (Names and
  dates from memory, to check.)
- **The Sims (2000)**: utility AI, and the inversion that makes it
  work -- the *objects* advertise what they offer ("I reduce hunger by
  40"), so adding a new object adds behaviour without touching any
  character's code. Smart objects, and one of the genuinely good ideas.
- **Halo 2 (2004)**: behavior trees, presented by Damian Isla at GDC
  2005, and copied by the entire industry within five years.
- **F.E.A.R. (2005)**: GOAP (goal-oriented action planning, Jeff
  Orkin) -- the enemies are given goals and actions with preconditions
  and effects, and *plan* at runtime. Widely considered the best enemy
  AI ever shipped, and the planner is smaller than the animation work
  that sells it.
- **Left 4 Dead (2008)**: the AI Director, which is not an opponent at
  all but a pacing system -- it watches the players' stress and decides
  when to send a horde and when to let them breathe.
- **Creatures (Steve Grand, 1996)** and **Black & White (2001)**: the
  two games that shipped real learning -- neural networks, and in
  Creatures a genome and a biochemistry -- rather than search. Both are
  reminders that "learning in a shipped game" is old, and that its
  problem is not capability but *designability*: a creature that can
  learn anything can learn to be boring.
- Cars learned to drive too: **Colin McRae Rally 2.0 (2000)** used a
  neural network for the racing line, and **Forza**'s Drivatar learned
  a player's style.

## Part 2: the champions, game by game

The other tradition: programs built to win, and the dates are the
argument for why `ai/` teaches what it teaches.

- **Chess**: Turing's Turochamp (1948, executed by hand on paper);
  Shannon's paper (1950) with type A and type B search; Greenblatt's
  Mac Hack VI (1967), the first to play in tournaments; Belle, Cray
  Blitz, Hitech, Deep Thought; **Deep Blue beats Kasparov (1997)** with
  alpha-beta, custom silicon and ~200 million positions a second --
  a victory of §7-§9 of [`notes_ai.md`](../tutorials/notes_ai.md),
  scaled. Then Stockfish (open source, still alpha-beta, plus a small
  network for evaluation since 2020) and AlphaZero (2017).
- **Checkers**: Samuel's self-learning player (1959); Jonathan
  Schaeffer's Chinook, world champion in 1994, and in 2007 the game
  **solved** -- perfect play is a draw.
- **Othello**: Rosenbloom's IAGO (1982); Michael Buro's **Logistello
  beat the world champion 6-0 in 1997**; in 2023 Hiroki Takizawa's
  computation indicated the game is a draw with perfect play. This is
  why `AiOthello.ml` is a fair fight only at depth 4.
- **Backgammon**: Tesauro's **TD-Gammon (1992)**, 80 hidden units,
  trained by self-play, near world class -- and it changed human
  opening theory. The cheapest counterexample to "you need scale".
- **Go**: hand-written programs stayed weak for thirty-five years;
  **Monte Carlo playouts and UCT (2006)** -- Coulom's Crazy Stone,
  Kocsis and Szepesvári's UCT, Gelly and Silver's MoGo -- beat all of
  it on 9x9 within a year; **AlphaGo beat Lee Sedol in 2016**, AlphaGo
  Zero (2017) learned from self-play alone; then Leela Zero and KataGo
  put it on a desktop. The one game where the history *is* the
  curriculum, which is the whole case for `games/AiGo.ml`.
- **Poker** (imperfect information, a different subject entirely):
  Libratus (2017), Pluribus (2019).

## Part 3: the engines and the libraries

- **Navigation**: recast/detour (Mikko Mononen, open source) generates
  navigation meshes from level geometry and is what Unity's NavMesh and
  much of the industry use; the search on top is still A*, plus funnel
  string-pulling to straighten the path. Grid pathfinding survives in
  2D and strategy games, with Jump Point Search (2011) as the
  fast-on-uniform-grids trick, and flow fields for crowds (Supreme
  Commander 2, 2010).
- **Steering and crowds**: Reynolds's own OpenSteer; RVO2 / ORCA (van
  den Berg et al.) for reciprocal collision avoidance, which is what
  keeps a hundred units from deadlocking in a corridor; Unity and
  Unreal ship variants.
- **Behaviour**: Unreal's behavior trees and Environment Query System,
  in the editor; Unity's various behavior-tree assets; behaviac
  (Tencent); Dave Mark's Infinite Axis Utility System.
- **Machine learning**: PyTorch, TensorFlow, JAX for training; ONNX and
  small runtimes for shipping; Unity ML-Agents for RL in games. Nothing
  in that list is small, and nothing in it is readable end to end,
  which is the gap `ai/`'s learning half aims at rather than competes
  with.

## Part 4: the teaching lineage

- **Russell and Norvig, *Artificial Intelligence: A Modern
  Approach***: chapter 3 (search: BFS, Dijkstra, A*) and chapter 5
  (adversarial search: minimax, alpha-beta, MCTS) are the direct
  sources of `Pathfind.mli` and `Minimax.mli`, worked examples
  included.
- **Amit Patel's Red Blob Games**: the best explanations of
  pathfinding and grids that exist, interactive, and the reason a
  generation of game programmers understands A*.
- **Ian Millington, *AI for Games***: the field's reference textbook,
  broad and practical; **Mat Buckland, *Programming Game AI by
  Example*** (2004): state machines, steering and a soccer team, in
  readable C++; the **Game AI Pro** series (ed. Steve Rabin), the
  practitioners' chapters.
- **Daniel Shiffman, *The Nature of Code***: chapter 5 on autonomous
  agents and flocking, chapter 9 on genetic algorithms, chapter 10 on
  neural networks -- the same conviction as this repository, that these
  ideas belong in a visual sandbox.
- **Sutton and Barto, *Reinforcement Learning: An Introduction***:
  the source for §8 of [`notes_ai_learning.md`](../tutorials/notes_ai_learning.md).
- **Michael Nielsen, *Neural Networks and Deep Learning***, and
  **Andrej Karpathy's micrograd / "Neural Networks: Zero to Hero"**:
  the two clearest from-scratch treatments of backpropagation and
  autodiff.
- **TensorFlow Playground** (Smilkov, Carter, 2016): a network trained
  in a browser page, the decision boundary redrawn as it learns. The
  name collision with this project is a coincidence and the spirit is
  not.
- **Norvig, *Paradigms of AI Programming*** (1992): classic AI written
  as readable Lisp, and the closest ancestor in method to what `ai/`
  tries to be in OCaml.
- **The Pac-Man Dossier** (Jamey Pittman): a game's AI reverse
  engineered to the frame, and better documentation than most shipped
  engines have.

## Part 5: in Elm, and in OCaml

Evan's elm-playground has no AI of any kind, and Elm has no game AI
libraries -- which is unsurprising: Elm's world is UIs, and a search is
a loop with mutable state, the thing the language is least eager to
write.

OCaml has bindings rather than teaching code: **owl** (scientific
computing, the closest thing to NumPy), **ocaml-torch** and
**tensorflow-ocaml** (bindings to the C++ libraries). For game AI
specifically there is effectively nothing -- no pathfinding library, no
steering library, no behavior trees. So `ai/` is not competing with an
OCaml incumbent; it is writing the small version that did not exist,
which is also true of `graphics/`, `physics/` and `audio/` here.

There is a pleasing historical circle in the language itself: ML was
born (Milner, 1973) as the metalanguage of a theorem prover, in the era
when "AI languages" meant Lisp and Prolog, and the ancestor of every
pattern match in `ai/` comes from that world.

## Where `ai/` and `Ai` actually sit

Two levels, like graphics, physics and sound:

- **`ai/`, the algorithms**, at the legible end: one idea per module,
  each `.mli` with its diagram, its worked example *with the node
  counts*, and its paper; the simple version kept beside the better one
  so the difference is a number you can watch (13 nodes against 11;
  549,946 against 18,297; 80 cells against 11); explicit seeds, so
  every run repeats and every example can be a test.
- **`Ai`, the API**, at the simple end: steering behaviours as forces
  on `Physics.body`, so they compose with gravity and thrust; a path as
  a list of tiles; an opponent as a value you ask for a move. The
  search, the tables and the frontier stay behind the door.

The ceiling, deliberate and stated in each `.mli`: a chess engine of a
few hundred elo, a 9x9 Go player at weak amateur level, a network of a
few thousand weights, grids instead of navigation meshes. Against Deep
Blue's 200 million positions a second and AlphaGo Zero's thousands of
TPUs, that is nothing -- and the algorithms are the same ones, which is
exactly the point worth teaching.

## Postscript: the numbers

Measured on one laptop, native, with the code as it stands. They are
the honest answer to "what does a legible implementation cost", and
every one of them comes from a test or a run that can be repeated.

**How much code.** Nineteen modules in `ai/`: 1,607 lines of
implementation and 1,911 of interface -- more explanation than program,
which is the intended ratio. The largest implementation is `Mcts.ml`
at 180 lines, and Monte Carlo tree search, the transposition table and
reverse-mode autodiff are 180, 177 and 91 lines respectively. The
tests are 1,710 lines, about as much as the code they check. On top:
`playground/Ai` (272 lines over 288 of interface), `Ai_debug` (154),
and eleven examples totalling 2,445.

**How fast it searches.** Alpha-beta on Connect 4's opening, seven
moves ahead: 62,467 nodes in 543 ms, about **115,000 nodes a second**.
Deep Blue did 200 million positions a second in 1997 hardware; the
factor is about two thousand, and the algorithm is the same one.

**What fits in a frame.** Give that search a budget of one 16 ms frame
-- 1,839 nodes -- and iterative deepening finishes **depth 4** and
abandons depth 5. That is the number that decides what a game can
actually ask for at 60 frames a second, and it is why `Deepening` has
a node budget rather than a depth.

**How fast it plays out.** MCTS on tic-tac-toe: **180,000 playouts a
second**. The same code on 9x9 Go: about **830 a second**, one playout
being 1.2 ms of a hundred-odd moves each scanning the board. A factor
of two hundred between two games with the same search, which is the
whole reason Go needed a different idea rather than a faster machine.

**Training a network.** The digit reader is 17,098 weights (256-64-10)
on 320 training digits and 80 held out. One epoch is **147 ms**, after
which it is 26% right; after thirty more (4.7 seconds) it is **89%**.
A network worth calling small, trained in the time it takes to read
this sentence -- and still visibly worse on a digit drawn with a mouse
than on one its own font drew, which is the lesson it was built for.

**A network inside the search.** `AiGo` is not wired to one (see
`plan_ai_teaching.md` for why: the compute, not the code). What is
measured instead is the mechanism, on tic-tac-toe, with a perfect
value function standing in for a trained one: at twelve playouts the
search with a value head finds the winning move in 12 of 12 won
positions against 10 of 12 for random playouts, and over twenty games
at forty playouts each it wins 11 and loses 0 to the version without.
That is AlphaGo's shape working at a size where the numbers can be
checked in a second.

Sources: from memory, to be checked before relying on them for
teaching -- the books and papers named above, the GDC talks, the
Pac-Man Dossier, and the documentation of the engines and libraries
mentioned. Dates in Part 1 and Part 2 especially deserve a check.
