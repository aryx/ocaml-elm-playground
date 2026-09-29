(* Code_config: which files a directory's code map leaves out, in the
   file at its root .codemapignore, as the author's codemap reads it,
   gitignore's way: a line a pattern, # a comment; /x from the root
   only, x/ a directory only, x anywhere (any name on the way), * any
   characters of a name, ? one; ! (keep after all) not understood, the
   line skipped.

   claude: what the .codemapconfig files say (their colours among the
   rest) is Code_guide's, since they are jsonnet (plan_codemap_v2.md).

   Worked example (the tests'): with the ignore file "/gitlog.txt",
   "test/" and "*_tests.c", gitlog.txt and kernel/test/ are ignored,
   test.c and kernel/gitlog.txt are not, lib/io_tests.c is. *)

type rgb = int * int * int

type t

val empty : t

(* the ignore file's text, if there *)
val make : ignore:string option -> t

(* [ignored t path ~dir]: [path] (relative to the root, '/' between
   names) left out, a directory if [dir] *)
val ignored : t -> string -> dir:bool -> bool

(* "#e08030" as red, green, blue *)
val hex : string -> rgb option
