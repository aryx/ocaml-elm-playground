# ai/: game AI, from scratch, for teaching

One idea per module, each `.mli` with its diagram, worked example and
references; independent of the Playground (`Ai.ml`, in
`playground/apis/`, is the Evan-style API over it, `Ai_debug.ml` draws
what it thinks).
Pure OCaml, so every backend, the web included, has it. The tutorials
are `docs/claude_notes/tutorials/notes_ai.md` (the classic game AI)
and `notes_ai_learning.md` (neural networks and learning to play).

## The folders

A game's AI answers a few different questions, and each folder is one
of them, a library of its own:

| folder (library) | the question | modules | notes |
|---|---|---|---|
| `movement/` (`ai_movement`) | how do I get there? | `Pathfind` (breadth-first, Dijkstra, A*, flow fields), `Steering` (seek, flee, arrive, wander, pursue, avoid), `Flock` (separation, alignment, cohesion) | `notes_ai.md` §2-§5 |
| `decision/` (`ai_decision`) | what do I do now? | `Fsm` (states and transitions), `Behavior` (a tree of fallbacks), `Utility` (scores, the best one) | §6 |
| `bots/` (`ai_bots`) | how do I play as a player does? | `Sense` (what a bot may know: sight, hearing, memory), `Bot` (the loop and the handicaps: reaction time, aim error; through the player's own keys) | end of §6 |
| `search/` (`ai_search`) | what will my opponent answer? | `Minimax` (the game tree, alpha-beta), `Deepening` (iterative deepening, move ordering, the transposition table), `Zobrist` (hashing a position), `Mcts` (Monte Carlo tree search) | §7-§10 |
| `learning/` (`ai_learning`) | can it learn instead of being told? | `Matrix`, `Neuron`, `Net`, `Backprop`, `Grad` (autodiff), `Train` (supervised learning), `Adam` (descent with a memory), `Weights` (what was learned, as a file); `Qlearn` (learning to play from rewards) | `notes_ai_learning.md` |
| `language/` (`ai_language`) | what comes next? | `Tokenizer` (a text as numbers), `Sampling` (drawing the next token), `Bigram` (the table of letter pairs, counted, then learned), `Ngram_mlp` (embeddings, a network three letters back); after Karpathy's makemore, on its names (`data/names/`) | `notes_ai_learning.md` §11-§12 |

The first three are the *real-time* half (a world at 60 frames a
second: monsters, crowds, soldiers), `search/` the *turn-taking* half
(an opponent at a board game); `learning/` can replace the hand-written
part of either (an evaluation, a policy).

The first five folders do not use one another: each module depends
only on modules of its own folder (`Deepening` and `Mcts` on `Minimax`,
`Train` on `Backprop` and `Net`, ...), so a folder can be read, and
taught, on its own. `language/` is above `learning/`, which it is
written with (`Grad`, `Adam`, `Matrix`). A new
module goes where its question is; one that needs two folders (a
network inside the search, `notes_ai_learning.md` §9) belongs to the
game that joins them, or to a new folder above both.

## Using it

`(libraries ai)` gives all of it: `ai` is a library with no module of
its own that depends on the five (so the games and examples written
before the folders needn't change). A program that wants only one
names it, `(libraries ai_search)`. The modules are unwrapped: `Pathfind`,
not `Ai.Pathfind` -- `Ai` is the Playground's module.

## Tests

`ai/tests/` (`make test`): each `.mli`'s worked example, and the
properties the notes promise (alpha-beta always agreeing with minimax,
A* finding Dijkstra's cost, ...).
