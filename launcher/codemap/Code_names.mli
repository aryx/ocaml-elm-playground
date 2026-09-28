(* Code_names: where a name used in one file and defined in another is
   (plan_codemap_naming.md, level 3), among the files of a map.

   Not the build's answer (which modules a library sees, which objects a
   program links): a search, exact where the language decides, ranked by
   nearness where it does not, and saying so -- one answer when the first
   is alone at its rank, else the list to choose from.

   OCaml. M.x: x among the top-level definitions of M's files (m.ml,
   m.mli), a module being a file by its name, the libraries being
   unwrapped. M.N.x: N.x in M's files (a nested module's x), else x in
   N's own (a wrapped library's). A bare x its file does not define:
   among the modules opened around it (let open M in, M.(e)), then its
   file's opens, the last open first, the first that has it (OCaml's own
   rule). Several files of one module's name (two Parser.ml): the one
   whose directory shares the most with the use's, the .ml before its
   .mli.

   C. One namespace per program, and a directory of hundreds of them (each
   Plan 9 command its own error()): the definitions (a function's body, a
   global) over the declarations (prototypes, externs) if there are any,
   then, nearest first: the use's own program (its directory, and the
   headers it includes of its own, #include "x.h"), the libraries (a
   directory named lib... or include), anywhere else by the length of
   the path shared with the use's.

   Worked example (the tests'): error() used in rc/exec.c, defined in
   rc/subr.c, sam/error.c and lib/error.c, declared in rc/fns.h: rc/subr.c,
   alone at its rank; print() used there, defined only in lib/fmt.c:
   lib/fmt.c. *)

type candidate = {
  path : string;
  line : int; (* from 0 *)
  col : int;
  len : int;
  near : int * int; (* its rank: lower is nearer (a group, then minus the path shared) *)
  other_project : bool; (* under another root than the use (below) *)
}

(* [find ?roots files ~from f r]: [r], used in [f] (at [from]), among
   [files] (the map's, lexed if need be): the candidates, the nearest
   first, and whether the first is alone at its rank. [roots]: the
   projects' top directories in the map (a .git, a dune-project there);
   a file's project is the deepest root above it, and another project's
   candidates come after all of the use's own, marked *)
val find :
  ?roots:string list -> (string * Code_file.t Lazy.t) list -> from:string -> Code_file.t -> Highlight_code.reference -> candidate list * bool

(* claude: the same, many times over the same files (a map's hovers,
   Code_rank's counting every name): the files indexed once -- OCaml's
   by module name, C's definitions by name (made on the first C search,
   lexing every C file) -- and searched there *)
type index

val index : (string * Code_file.t Lazy.t) list -> index
val find_in : ?roots:string list -> index -> from:string -> Code_file.t -> Highlight_code.reference -> candidate list * bool
