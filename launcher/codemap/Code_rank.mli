(* Code_rank: a definition's population, how much it is used, as a
   city's is its people (plan_codemap_google_maps.md, step 2): what the
   street map writes bigger and shows first.

   Counted over a map's files, once: its own file's uses (the names bound
   to it there, Code_file.uses) and the other files' (every name defined
   elsewhere resolved as a click resolves it, Code_names.find_in, and
   counted only when the answer is sure: a guess is not a use), and how
   many other files use it.

   The score is codemap's (Style.size_font_multiplier_of_categ): a weight
   by kind (a module or a type 5, a function 3.5, a value 3, a
   constructor 1.2), times a bucket of uses on a log scale (none 0.9, one
   1.3, a few 1.7, 5 or more 2.1, 20 or more 2.7, 100 or more 3.3), the
   other files' uses counting fully and the own file's a third: a helper
   used thirty times inside its file is a local habit, a function used
   by thirty files a capital.

   Worked example (the tests'): Road.ml defines curve and straight, uses
   straight once; Main.ml uses Road.curve twice and straight once through
   its open; Other.ml uses Road.curve once: curve 3 uses in 2 other files,
   straight 1 in 1 and 1 of its own. *)

type use = { own : int; others : int; files : int }

type t

(* [compute ?roots files]: the uses of every definition of [files] (the
   map's, lexed here) *)
val compute : ?roots:string list -> (string * Code_file.t Lazy.t) list -> t

(* [uses t path line name]: the definition of [name] at [line] (from 0)
   of [path]; none counted if it is not one *)
val uses : t -> string -> int -> string -> use

(* claude: the other files using it, and how many times each, the most
   first (a place card's "most from") *)
val users : t -> string -> int -> string -> (string * int) list

(* claude: the files' links: [(a, b, n)], file [a] using [b]'s
   definitions [n] times (b not a), the roads of Map_atlas *)
val links : t -> (string * string * int) list

(* codemap's buckets and weights, and their product *)
val bucket : int -> float
val weight : Highlight_code.category -> float
val score : t -> string -> int -> string -> Highlight_code.category -> float

(* claude: the whole counted once and saved as text, a fact a line (a
   web page's code map is given it in its bundle, make_codemap_data:
   lexing every file of a big repository in the browser froze the page
   on the first a) *)
val to_string : t -> string
val of_string : string -> t
