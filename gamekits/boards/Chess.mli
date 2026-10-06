(* Chess: the rules, a computer that searches (alpha-beta, as Shannon
 * set it out in 1950), and how a network reads a position. Shared by
 * the game (AiChess, whose header has the story of computers and
 * chess, and what move ordering and quiescence are for) and by the
 * program that trains a network to play it (scripts/train/train_chess),
 * so that both play the same game.
 *
 * The rules are all here but two: a pawn reaching the last rank may
 * become any piece, castling, taking en passant, checkmate and
 * stalemate; not the draws by repetition and by fifty moves without a
 * capture. White is MAX. *)

(*****************************************************************************)
(* {1 The rules} *)
(*****************************************************************************)

type color = White | Black
type kind = Pawn | Knight | Bishop | Rook | Queen | King
type piece = { color : color; kind : kind }

(* the 64 squares, row by row from the top: 0 is a8, 63 is h1 *)
type position = {
  board : piece option array;
  turn : color;
  (* who may still castle, king side and queen side *)
  white_short : bool;
  white_long : bool;
  black_short : bool;
  black_long : bool;
  (* the square a pawn just jumped over, where it can be taken *)
  en_passant : int option;
  ply : int; (* the half-moves played *)
}

type move = { from : int; dest : int; promotion : kind option }

val other : color -> color
val row : int -> int
val col : int -> int
val on_board : int -> int -> bool

(* which way a pawn walks, in rows: white up the board, -1 *)
val forward : color -> int

(* a square's name, "e4" *)
val name : int -> string

(* a position from FEN, chess's own text format: the rows from the
 * top, a digit for empty squares, white in capitals; who plays; who
 * may castle; the en passant square *)
val of_fen : string -> position

val start : position

(* is this square attacked by that colour's pieces? *)
val attacked : piece option array -> int -> color -> bool

val king_square : piece option array -> color -> int

(* the player to play's king is attacked *)
val in_check : position -> bool

(* every move of the player to play, its own king forgotten *)
val pseudo_moves : position -> move list

val play : position -> move -> position

(* a move that does not leave one's own king attacked *)
val is_legal : position -> move -> bool
val legal : position -> move list
val can_move : position -> bool

(* the positions so many moves ahead, counted: the move generator's
 * test, against the numbers every chess programmer checks theirs with
 * (20, 400, 8902 from the start) *)
val perft : position -> int -> int

(*****************************************************************************)
(* {1 The computer} *)
(*****************************************************************************)

(* a pawn 100, a knight 320, a bishop 330, a rook 500, a queen 900 *)
val value : kind -> int

(* the board alone, for white: its material and what each piece's
 * square is worth, less black's (Tomasz Michniewski's "Simplified
 * Evaluation Function") *)
val static : position -> int

(* captures first, the biggest victim taken by the smallest attacker
 * (MVV-LVA): alpha-beta cuts most when the best move comes first *)
val order : position -> move list -> move list

val is_capture : position -> move -> bool

(* the captures played on until the board is quiet, within alpha-beta's
 * window, for the player to play; at most so many captures deep *)
val quiesce : position -> int -> int -> int -> int

(* more than any material *)
val mate : int

(* for white, a game over: a checkmate, the sooner the better; a
 * stalemate is 0 *)
val ended : position -> float

(* [static], or [ended] when there is no move *)
val score : position -> float

(* [quiesce] as [Minimax.alphabeta]'s [leaf] *)
val quiet : position -> alpha:float -> beta:float -> float

val chess : ordered:bool -> (position, move) Minimax.game

(* how far AiChess's computer looks: 3 *)
val depth : int

val search : ordered:bool -> quiescence:bool -> depth:int -> position -> move Minimax.result

(*****************************************************************************)
(* {1 As a network reads it} *)
(*****************************************************************************)
(* A position is shown from the side of whoever is to play: black sees
 * the board turned top to bottom, so that its own pawns walk up the
 * planes as white's do, and one network plays both colours.
 *
 *     17 planes of 8 by 8:
 *       0 to 5     my pawns, knights, bishops, rooks, queens, king
 *       6 to 11    the other's
 *      12 to 15    all ones if I may castle short, long; if the
 *                  other may, short, long
 *      16          the square where a pawn can be taken en passant
 *
 * A move is the square it leaves and the square it reaches, seen from
 * the same side: 64 times 64 places in the policy, most of them moves
 * no piece can make (the network is never asked to learn the rules:
 * [Alphazero.guides] keeps the legal ones).
 *
 * This is simpler than the paper's, and smaller. AlphaZero's input
 * had the last eight positions (for the repetitions) and counters; its
 * policy 73 planes, a plane a direction and a distance, with nine for
 * the promotions to a knight, a bishop or a rook. Here a pawn promoted
 * is a queen ([queening]). *)

(* a square seen from the other side of the board *)
val mirrored : int -> int

val planes : int

(* 1,088 numbers, the planes above *)
val encode : position -> float array

(* a move's place among the policy's 4,096 scores, made in this
 * position *)
val index : position -> move -> int

(* the rules with a promotion always a queen's *)
val queening : (position, move) Minimax.game

(* the pieces alone, white's less black's *)
val material : position -> int

(* the game as [Alphazero] needs it, over [queening]: a position with
 * the half-moves played, and none left after [longest]. Two players
 * who know nothing never mate each other, and a game without an end
 * teaches nothing: stopped there, the game is given to whoever is at
 * least a knight ahead, as a tournament's arbiter would, and is drawn
 * otherwise. That is the one piece of chess knowledge the learner is
 * given beyond the rules: that pieces are worth having. *)
val board : longest:int -> (position * int, move) Alphazero.board
