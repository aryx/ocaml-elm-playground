(* The native backends' store of documents: the files of one directory
 * (see Playground_platform.mli). The platform is the trusted computing
 * base, so these take no capability; Playground_platform's wrappers do. *)

(* $ELM_PLAYGROUND_STORE, or ~/.elm-playground/documents, made if
 * missing *)
val dir : unit -> string

val store : string -> string -> unit
val fetch : string -> string option
val stored : unit -> string list

(* a file in the current directory *)
val export : string -> string -> unit
