(* Tic-tac-toe, the game self-play is first worked on
 * (notes_ai_learning.md section 16): small enough that the whole loop
 * runs in seconds, and solved, so that what the network learned can be
 * checked against the truth -- with best play it is a draw, and a
 * player that never loses to [Minimax] at full depth plays perfectly.
 *
 * The squares are numbered
 *
 *     0 1 2
 *     3 4 5
 *     6 7 8
 *
 * X plays first and is MAX. *)

type mark = Empty | X | O
type position = { cells : mark array; turn : mark }

val start : position
val winner : position -> mark

(* the rules: no move once a line is made or the board is full; the
 * score 1 for X's win, -1 for O's, 0 otherwise *)
val game : (position, int) Minimax.game

(* the position as a network reads it: 18 numbers, nine for the marks
 * of whoever is to play, nine for the other's, so that X's view and
 * O's of the same situation are the same input *)
val encode : position -> float array

(* "xx.oo....", rows one after the other; whose turn it is from the
 * counts *)
val of_string : string -> position
val to_string : position -> string
