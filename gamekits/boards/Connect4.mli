(* Connect 4: the rules, what a position is worth, and how a network
 * reads one. Shared by the game (AiConnect4, whose header has the
 * game's history) and by the program that trains a network to play it
 * (scripts/train/train_connect4), so that both play the same game.
 *
 * Seven columns of six; a piece dropped in a column lands on the
 * lowest empty square; four in a line, any direction, wins. The two
 * players are named from the game's side: [You] moves first, [Machine]
 * is MAX. *)

val columns : int
val rows : int

type piece = Empty | You | Machine

(* the board, column by column, bottom first; [turn] is whose it is *)
type position = { board : piece array; turn : piece }

val start : position

(* the piece at a column and a row; [Empty] outside the board *)
val at : piece array -> int -> int -> piece

(* the row a piece dropped in a column would land on, if any *)
val landing : piece array -> int -> int option

(* the columns not full, whether or not the game is over *)
val moves : position -> int list
val play : position -> int -> position

(* every line of four squares on the board, as (column, row): 69 *)
val lines : (int * int) list list
val four : piece array -> piece -> bool
val over : position -> bool

(*****************************************************************************)
(* {1 What a position is worth} *)
(*****************************************************************************)

val win : float

(* the evaluation a search without a network stops on: a four is
 * [win] or its opposite; otherwise every line of four squares with
 * pieces of one colour only is worth 1, 10 or 100 for one, two or
 * three of them, plus 3 a piece in the middle column, the other
 * player's subtracted *)
val score : position -> float

(* the game for [Minimax], [Deepening] and [Mcts]: no move once it is
 * over, [score] above, [Machine] MAX *)
val connect4 : (position, int) Minimax.game

(*****************************************************************************)
(* {1 What a search is given} *)
(*****************************************************************************)

(* the game's own hint for [Deepening]'s [order]: the middle columns
 * first. A piece in the middle is in more fours than one at the edge,
 * 13 against 3, so a middle move is likelier to be good, and a good
 * move tried first is what makes alpha-beta cut *)
val middle_first : position -> int list -> int list

(* a position's key for the transposition table: one number per piece
 * and square, and one for the turn ([Zobrist]) *)
val zobrist : Zobrist.t
val key : position -> int64

(* a player by iterative deepening to [depth], with the hints above
 * and a table of its own: what a trained network is measured against *)
val alphabeta : depth:int -> (position, int) Arena.player

(*****************************************************************************)
(* {1 As a network reads it} *)
(*****************************************************************************)

(* 84 numbers: 42 for the pieces of whoever is to play, 42 for the
 * other's, so that either side's view of a situation is one input *)
val encode : position -> float array

(* the game as [Alphazero] needs it: 84 inputs, 7 moves, a column its
 * own index *)
val board : (position, int) Alphazero.board
