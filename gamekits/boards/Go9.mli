(* Go on a board of 9 by 9: the rules, the counting, a random game
 * played out, and how a network reads a position. Shared by the game
 * (AiGo, whose header has the story of computers and Go) and by the
 * program that trains a network to play it (scripts/train/train_go),
 * so that both play the same game.
 *
 * A stone is put on an empty point; the groups of the other colour
 * left without a liberty are taken off; a stone may not take its own
 * group's last liberty, nor retake a ko at once (the simple ko: a
 * single stone taken by a single stone). Two passes in a row end the
 * game, counted the Chinese way: stones, plus the empty points that
 * touch one colour only, and [komi] for white. Black moves first;
 * white, the computer in AiGo, is MAX. *)

val size : int
val points : int (* 81 *)
val komi : float (* 6.5, to white *)

type stone = Empty | Black | White

type position = {
  board : stone array; (* row after row, [size] to a row *)
  turn : stone;
  ko : int option; (* the point a stone may not be put back on *)
  passes : int; (* in a row *)
}

type move = Put of int | Pass

val start : position
val other : stone -> stone

(* a point's column and row *)
val xy : int -> int * int

(* the points next to a point, worked out once *)
val around : int array array
val any_around : int -> (int -> bool) -> bool
val all_around : int -> (int -> bool) -> bool
val each_around : int -> (int -> unit) -> unit

(* the group of stones a point belongs to, and how many liberties it
 * has: the flood fill every Go program starts with *)
val group : stone array -> int -> int list * int

(* a stone put down: the captures taken off; None if it was not legal *)
val put : position -> int -> position option

(* the legal points *)
val legal : position -> int list
val play : position -> move -> position
val over : position -> bool

(* a colour's stones and the empty points only it surrounds *)
val area : position -> stone -> float

(* black's area less white's and the komi: above 0, black has won *)
val final_score : position -> float

(*****************************************************************************)
(* {1 For a search} *)
(*****************************************************************************)

(* the game for [Mcts]: a pass and every legal point; the score only
 * at the end, its sign who won; white MAX *)
val go : (position, move) Minimax.game

(* an empty point surrounded by the stones of whoever is to play: an
 * eye, and filling it is how a random player kills its own group *)
val own_eye : position -> int -> bool

(* a game played out at random to its end, own eyes not filled: what
 * [Mcts] judges a position by when it has nothing else *)
val playout : Lehmer.state -> (position, move) Minimax.game -> position -> position

(*****************************************************************************)
(* {1 As a network reads it} *)
(*****************************************************************************)

(* [go] without the moves that fill an eye of one's own: the one piece
 * of knowledge a search guided by a network is given too, as the
 * playouts are *)
val sensible : (position, move) Minimax.game

(* 243 numbers, three planes of 81: the stones of whoever is to play,
 * the other's, and the point of the ko if there is one *)
val encode : position -> float array

(* a move's place among the policy's 82 scores: a point its own
 * number, the pass last *)
val index : move -> int

(* the game as [Selfplay] needs it, over [sensible] *)
val board : (position, move) Selfplay.board

(* the same for games that must end, as those of a network against
 * itself: a position with the moves played so far, and none left
 * after [longest]. Two players who know nothing do not pass, and
 * would capture each other for ever. *)
val capped : longest:int -> (position * int, move) Selfplay.board

(* [turned s i]: where point [i] goes under the symmetry [s] of the
 * board, 0 to 7 (0 leaves it), the square's four turns and their
 * mirror images *)
val turned : int -> int -> int

(* a lesson seen under a symmetry: as good a lesson, the planes and
 * the policy turned the same way, the pass and the value unchanged.
 * A board has eight, so every game is eight games. *)
val lesson_turned : int -> Policy_value.lesson -> Policy_value.lesson
