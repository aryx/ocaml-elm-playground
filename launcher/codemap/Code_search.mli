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
   first.

   A query starting with a double quote searches the text: ("Cap.fork")
   finds every line containing Cap.fork, the case ignored unless it has
   a capital (smart case, Emacs's), at least two characters; before the
   layers of plan_codemap_v2.md, a pattern to try. Starting with @, the
   references: @Cap.fork the lines whose code names Cap.fork (a
   reference the lexer found, not a comment's or a string's words), a
   last part alone (@fork) any path ending so. *)

type kind =
  | Dir
  | File
  | Def
  | Text
  | View (* claude: a config's view, by its name; its line its place among them *)
  | Tour (* claude: a config's tour, the same *)

(* a thing found: a directory's path (no final slash), a file's, a
   definition's file and line (from 0); its name, what is matched (a
   line's, its text, trimmed) *)
type hit = { kind : kind; path : string; line : int; name : string }

(* the things to search: the directories and files by path, the
   definitions as file, line, name *)
val candidates : ?views:string list -> ?tours:string list -> dirs:string list -> files:string list -> defs:(string * int * string) list -> unit -> hit array

(* the query without its final slashes, and how many there were *)
val parse : string -> string * int

(* the hits of a query, the best first; as good, those [near] says are
   first (the unit looked at); a definition in an .mli that its .ml
   defines too left out, the .ml's standing for it *)
val matches : ?near:(string -> bool) -> hit array -> string -> hit list

(* the directories a query ending in two slashes or more takes
   together: those named exactly as it says, under its path's part *)
val all_named : hit array -> string -> string list

(* the query completed as far as its best hits agree (Tab): their names'
   common start, the query's own case kept where it typed *)
val complete : hit list -> string -> string

(* [starts s p]: [s] begins with [p] *)
val starts : string -> string -> bool

(* a path's last part *)
val basename : string -> string

(* claude: the text searched for, if the query is a text search *)
val text_query : string -> string option

(* the lines of files (path, lines) containing a text, at most [limit] *)
val text_matches : ?limit:int -> (string * string array) list -> string -> hit list

(* claude: the name searched for, if the query is a reference search *)
val ref_query : string -> string option

(* the lines of files (path, references as line and dotted name, lines'
   text) referring to a name, at most [limit] *)
val ref_matches : ?limit:int -> (string * (int * string) list * string array) list -> string -> hit list

(* [contains s sub] *)
val contains : string -> string -> bool
