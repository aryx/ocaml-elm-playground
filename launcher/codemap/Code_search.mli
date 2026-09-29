(* Code_search: the code map's search (/, Map_v2), its matching and
   completion, pure: what a query finds among the directories, the files
   and the definitions.

   A query is a name, or a part of one: "invad" finds TinyInvaders.ml,
   "step" every definition named step or step_ball. Before a slash, a
   part of the path it must be under: "shmup/step" the steps of the
   shoot 'em ups. Ending with a slash, only directories: "arm/"; with two
   or more, every directory named exactly so, taken together (the
   author: "foo/arm/ bar/arm/ ... by typing arm/// you would do a
   multi-dir selection view with all the arm folders"). Case is ignored.

   Ranked: the name itself, then a name it starts, a word of a name it
   starts (after _ or a capital), a name it is inside; at each, the
   directories first, then the files, the definitions; the shallower
   first. *)

type kind = Dir | File | Def

(* a thing found: a directory's path (no final slash), a file's, a
   definition's file and line (from 0); its name, what is matched *)
type hit = { kind : kind; path : string; line : int; name : string }

(* the things to search: the directories and files by path, the
   definitions as file, line, name *)
val candidates : dirs:string list -> files:string list -> defs:(string * int * string) list -> hit array

(* the query without its final slashes, and how many there were *)
val parse : string -> string * int

(* the hits of a query, the best first *)
val matches : hit array -> string -> hit list

(* the directories a query ending in two slashes or more takes
   together: those named exactly as it says, under its path's part *)
val all_named : hit array -> string -> string list

(* the query completed as far as its best hits agree (Tab): their names'
   common start, the query's own case kept where it typed *)
val complete : hit list -> string -> string

(* [starts s p]: [s] begins with [p] *)
val starts : string -> string -> bool
