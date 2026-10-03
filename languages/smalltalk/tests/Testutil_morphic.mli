(* A Morphic world for the tests (Unit_morphic, Unit_tools): a system
 * of its own booted from Squeak's kernel, whose mouse the test moves
 * and whose keys it types; the globals F, a Form of 32 bits, and W,
 * the world drawn on it *)

type world

(* a world of this size (400 by 300), after its first cycle *)
val boot : ?size:int * int -> unit -> world

(* what an expression prints; a String, without its quotes; an error
 * as "error: ..." *)
val print : world -> string -> string

(* the mouse there with these buttons down (4 red, 2 yellow, 1 blue),
 * then a cycle *)
val move : world -> int -> int -> int -> unit

(* the mouse there, no cycle yet: for a test that wants the cycle's
 * own answer *)
val set_mouse : world -> int -> int -> int -> unit

(* a click: the button (the red one) down then up, a cycle each *)
val click : ?button:int -> world -> int -> int -> unit

(* characters typed, then a cycle *)
val typed : world -> string -> unit

(* the Form's colour there, as a Color prints *)
val colour : world -> int -> int -> string
