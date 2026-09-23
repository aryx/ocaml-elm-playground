(* Hashing a position, and remembering what was learned about it (see
 * notes_ai.md section 9).
 *
 * The same position turns up again and again in a search, reached by
 * different orders of the same moves -- a *transposition*. In Connect
 * 4, playing column 3 then 4 gives the board playing 4 then 3 gives;
 * searching it twice is searching the whole subtree twice.
 *
 * To recognise it you need a key for a position, and you need it to be
 * cheap, because the search makes one at every node. Albert Zobrist
 * (1970): draw one random 64-bit number per (piece, square) once, and
 * let a position's key be the xor of the numbers of the pieces on it.
 * Xor is its own inverse, so a move *updates* the key instead of
 * rehashing the board:
 *
 *     key' = key xor number(piece, from) xor number(piece, to)
 *
 * two xors, whatever the size of the board. One more number for "it is
 * the other side to play", xored in and out the same way, because the
 * same arrangement of pieces with the other player to move is a
 * different position.
 *
 * Example ([of_board]): a 3-square board with a piece 0 on square 1 and
 * a piece 1 on square 2 hashes to [number 0 1] xor [number 1 2]; put
 * them down in the other order and it is the same key, which is the
 * whole point.
 *
 * Two keys can collide -- 64 bits, so about one chance in 18 million
 * million million per pair, and a search that stores a million
 * positions is still far from worrying. Engines live with it; this one
 * says so rather than checking.
 *
 * {2 The table}
 *
 * What is worth storing about a position is what a search learned: its
 * value, how deep the search that found it was (a value from a deeper
 * search may replace a shallower one's, not the other way round), the
 * best move found there (which is the move to try first next time --
 * ordering, notes_ai.md section 9), and whether the value is exact or
 * only a bound, because alpha-beta often stops early and learns only
 * "at least this" or "at most that".
 *
 * References: Albert L. Zobrist, "A New Hashing Method with
 * Application for Game Playing", 1970 (technical report, University of
 * Wisconsin); Richard Greenblatt's Mac Hack VI had a transposition
 * table in 1967; every engine since. *)

(* the random numbers of one game: [pieces] kinds on [squares] squares *)
type t

(* [make ~pieces ~squares ~seed]: the numbers, drawn once; [seed] makes
 * it the same every run (a game replays the same) *)
val make : pieces:int -> squares:int -> seed:int -> t

(* [number z ~piece ~square]: the one to xor in for that piece there *)
val number : t -> piece:int -> square:int -> int64

(* [side z]: the number for "the other side to play" *)
val side : t -> int64

(* [of_board z pieces]: the key of a whole position, [pieces] being the
 * (piece, square) pairs on it (in any order) *)
val of_board : t -> (int * int) list -> int64

(*****************************************************************************)
(* {1 The transposition table} *)
(*****************************************************************************)

(* what a search learned about a position: [Exact] the value, or a
 * bound it stopped at ([Lower]: at least [value]; [Upper]: at most) *)
type bound = Exact | Lower | Upper

type 'move entry = { value : float; depth : int; bound : bound; best : 'move option }

type 'move table

(* [table ()]: empty *)
val table : unit -> 'move table

(* [find t key]: what is known, if anything *)
val find : 'move table -> int64 -> 'move entry option

(* [remember t key e]: kept unless what is there came from a search at
 * least as deep *)
val remember : 'move table -> int64 -> 'move entry -> unit

(* how many positions are in it, and how many [find]s have hit *)
val size : 'move table -> int
val hits : 'move table -> int
val forget : 'move table -> unit
