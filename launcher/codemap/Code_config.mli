(* Code_config: what a directory says of its own code map, in two files
   at its root, as the author's codemap reads them:

   - .codemapignore, gitignore's way: a line a pattern, # a comment;
     /x from the root only, x/ a directory only, x anywhere (any name
     on the way), * any characters of a name, ? one; ! (keep after all)
     not understood, the line skipped;

   - .codemapconfig, JSON (Json; jsonnet's someday): the colours of its
     parts, a directory's path (or a file's) to "#rrggbb", the longest
     path that is a file's (or its directory's) giving its colour --

       { "colors": { "kernel": "#e08030", "MISC/BIG": "#606060" } }

   Worked example (the tests'): with the ignore file "/gitlog.txt",
   "test/" and "*_tests.c", gitlog.txt and kernel/test/ are ignored,
   test.c and kernel/gitlog.txt are not, lib/io_tests.c is. *)

type rgb = int * int * int

type t

val empty : t

(* the two files' texts, if there; Error: the config's mistake *)
val make : ignore:string option -> config:string option -> (t, string) result

(* [ignored t path ~dir]: [path] (relative to the root, '/' between
   names) left out, a directory if [dir] *)
val ignored : t -> string -> dir:bool -> bool

(* the colours the config gives, for Code_map.make *)
val colours : t -> (string * rgb) list
