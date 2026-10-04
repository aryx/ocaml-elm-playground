(* Learning a game by playing it against oneself: AlphaZero's loop
 * (notes_ai_learning.md section 16).
 *
 * Qlearn.mli learns from rewards, a table entry at a time. This is
 * the same wish -- nobody gives the answers -- at the size of a board
 * game, with two things in the table's place: a search, which looks
 * ahead, and a network ([Policy_value]), which has opinions. Each
 * makes the other better:
 *
 *        +--> the search, guided by the network, plays a game
 *        |    against itself                             [play]
 *        |      |
 *        |      v
 *        |    each position of the game becomes a lesson: "the
 *        |    search, having looked ahead, spent its visits like
 *        |    this; and the game ended like that"
 *        |      |
 *        |      v
 *        +--- the network is taught the lessons          [Policy_value.step]
 *
 * The search is a better player than the network it is guided by,
 * because it looks ahead: so its visits are a better policy than the
 * network's, and are what the network is taught. The network taught,
 * the search guided by it is better again. Nothing else enters: no
 * games of masters, no evaluation function, only the rules.
 *
 * Three details make it work, each a field of [settings]:
 *
 *  - the first moves of a self-play game are *drawn* in proportion to
 *    their visits rather than the most visited taken, or every game
 *    would be the same game ([exploring]);
 *  - at the root, part of the policy is replaced by chance, so that a
 *    move the network has written off is still tried sometimes, and
 *    found good if it is ([noise]);
 *  - the value of a position is for *whoever is to play there*, and
 *    the position is shown to the network from that side
 *    ([board]'s [encode]): one network plays both colours.
 *
 * Measured on tic-tac-toe, where the truth is known (Unit_selfplay,
 * and examples/AiSelfPlay.ml shows it happen). A network of 6,026
 * numbers that starts knowing nothing; an iteration is 20 games
 * against itself at 50 playouts a move, then 200 steps on batches of
 * 32 drawn from the last 3,000 lessons ([iterate], below). Against a perfect player
 * ([Minimax] to the end), and against one playing at random:
 *
 *                              the search with it      it alone, no search
 *     iterations    time       perfect    random       perfect    random
 *          0                    0-5-5     36-4-0        0-0-2     14-6-20
 *          6          5 s       0-10-0    38-2-0        0-1-1     28-7-5
 *         20         17 s       0-10-0    37-3-0        0-2-0     34-6-0
 *
 * (won-drawn-lost; 10 and 40 games with the search, 2 and 40 without.)
 * Knowing nothing, the search alone already draws the perfect player
 * when it moves first and loses when it moves second. Five seconds of
 * self-play later it loses to nobody. Alone -- its policy's first
 * choice, no looking ahead -- the network learns more slowly, and that
 * column is what it itself knows: after twenty iterations it no longer
 * loses to a random player, and draws the perfect one.
 *
 * References: David Silver et al., 2017 and 2018 (Policy_value.mli);
 * Jonathan Laurent, AlphaZero.jl, whose four parts (self-play, memory,
 * learning, arena) and Connect Four tutorial this follows; Gerald
 * Tesauro, TD-Gammon, 1992, the first to learn a game this way;
 * Arthur Samuel, "Some Studies in Machine Learning Using the Game of
 * Checkers", 1959, the first to try. *)

(* a game as the loop needs it: its rules, where it starts, and how a
 * network reads it *)
type ('state, 'move) board = {
  game : ('state, 'move) Minimax.game;
  start : 'state;
  inputs : int; (* the numbers [encode] gives *)
  moves : int; (* how many different moves there are in all *)
  (* the position from the side of whoever is to play *)
  encode : 'state -> float array;
  (* a move's place among the policy's scores, 0 to [moves] - 1 *)
  index : 'move -> int;
}

(* the network as the two guesses [Mcts] takes, [prior] and
 * [evaluate]: the policy over the legal moves only, their shares made
 * to sum to 1; the value turned from "for whoever is to play, -1 to
 * 1" into MAX's share, 0 to 1 *)
val guides : ('state, 'move) board -> Policy_value.t -> ('state -> ('move * float) list) * ('state -> float)

(*****************************************************************************)
(* {1 Playing} *)
(*****************************************************************************)

(* the move the search guided by the network visits most, from
 * [playouts] (50); None when the game is over *)
val choose : ?playouts:int -> seed:int -> ('state, 'move) board -> Policy_value.t -> 'state -> 'move option

(* the network alone: its policy's first choice among the legal moves,
 * no search. What it has learned, and nothing else. *)
val instinct : ('state, 'move) board -> Policy_value.t -> 'state -> 'move option

(* each legal move's visits, of a search from this position. [noise]
 * (0): that much of the root's policy replaced by random shares. *)
val visits :
  ?noise:float -> seed:int -> playouts:int -> ('state, 'move) board -> Policy_value.t -> 'state -> ('move * int) list

(*****************************************************************************)
(* {1 A game against itself} *)
(*****************************************************************************)

type settings = {
  playouts : int; (* the search's, at each move: 50 *)
  exploring : int; (* the first moves drawn in proportion to their visits: 4 *)
  noise : float; (* of the root's policy, the share left to chance: 0.25 *)
}

val default : settings

(* [play ~seed board net]: one game, and what it teaches -- a lesson
 * per position met, in order -- with how it ended, MAX's share (1
 * won, 0 lost, a half drawn) *)
val play :
  ?settings:settings -> seed:int -> ('state, 'move) board -> Policy_value.t -> Policy_value.lesson list * float

(*****************************************************************************)
(* {1 The loop} *)
(*****************************************************************************)
(* An iteration is so many games against itself, then so many steps on
 * lessons drawn from the newest it remembers -- not only the last
 * games', which are all alike: taught on those alone it forgets the
 * rest (the same reason as DQN's replay memory). *)

type schedule = {
  games : int; (* against itself, an iteration: 20 *)
  steps : int; (* downhill, after them: 200 *)
  batch : int; (* lessons a step: 32 *)
  remembered : int; (* the newest lessons kept: 3000 *)
}

val usual : schedule

type learner = {
  net : Policy_value.t;
  lessons : Policy_value.lesson array; (* newest first *)
  iteration : int;
  draws : Lehmer.state; (* the batches' dice *)
}

(* before the first game: [seed] is the batches' *)
val learner : seed:int -> Policy_value.t -> learner

(* the second half of an iteration alone, for a trainer that gets its
 * games elsewhere (several processes playing at once): these lessons
 * remembered, [steps] taken, the iteration counted; the loss of the
 * last step *)
val learn : ?schedule:schedule -> learner -> Policy_value.lesson list -> learner * float

(* one iteration, and the loss of its last step *)
val iterate : ?settings:settings -> ?schedule:schedule -> ('state, 'move) board -> learner -> learner * float
