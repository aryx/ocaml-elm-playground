(* The Palm's Memo Pad: plain text, a memo's first line its title in
   the list -- the smallest of the four, and the one people used for
   everything the other three did not know about. *)

type t

val start : t

val update : Palm.input -> Palm.data -> t -> Palm.data * t
val view : time:float -> Palm.data -> t -> Playground.shape list

(* [wrap w s]: [s]'s lines cut at the spaces to fit [w] dots (a word
   longer than a line cut where it must) *)
val wrap : float -> string -> string list
