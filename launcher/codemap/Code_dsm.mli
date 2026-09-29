(* Code_dsm: a dependency structure matrix (DSM), codegraph's
   (~/github/codegraph, Dependencies_matrix_code), over the code map's
   files: the rows are units -- folders, files, definitions -- each
   expanded at will into its parts, and a cell (i, j) is how many uses
   row i makes of column j.

       what uses        1  2  3  4
       1 lib_core       .
       2 lib_security      .
       3 Store.ml       4  2  .          row 3 uses 1 four times
       4 CLI.ml         1     9  .

   The rows are layered, codegraph's partition (after the DSM
   literature): what uses nothing among its siblings first, what nothing
   uses last, a cycle's in between, so that a clean architecture is a
   lower-triangular matrix -- every use below the diagonal, from a later
   row to an earlier one -- and a cell above it is a use against the
   layers, a cycle.

   Expanding a folder puts its files and subfolders in its place;
   expanding a file, its definitions that a use in the matrix touches.
   Folders and files are weighed by the files' links (Code_rank); as
   soon as a definition is a row, by the definitions' own edges, found
   file by file (Code_street.uses) and cached by the caller.

   Worked example (the tests'): lib/ and app/, app/main.ml using
   lib/a.ml 3 times: lib first, app second, cell (app, lib) = 3, the
   matrix lower-triangular. *)

type node = Dir of string | File of string | Def of string * int * string (* its file, line (from 0), name *)

(* a use found in a source file: from a line (its enclosing definition's,
   [sdef]), of a name defined in [dst] at the line [ddef] *)
type edge = { sdef : int option; dst : string; ddef : int; ename : string }

(* what the matrix reads: the files, their links (a, b, n: a uses b n
   times), a file's top-level definitions (line, name), a file's uses of
   the others' definitions *)
type data = {
  files : string list;
  links : (string * string * int) list;
  defs : string -> (int * string) list;
  edges : string -> edge list;
}

type t

(* [make data units]: the matrix of [units] (a unit a folder, or a file
   when it is one of [data.files]); one unit alone, expanded at once: its
   inside *)
val make : data -> string list -> t

(* the rows, in order: each node, its depth, whether it can expand, and
   whether it is expanded (a group, its parts the rows after it) *)
type row = { node : node; depth : int; expandable : bool }

val rows : t -> row list

(* the groups: an expanded node and the span of rows (first, last) of its
   parts, and its depth *)
val groups : t -> (node * int * int * int) list

(* the cells: [matrix t].(i).(j), row i's uses of column j *)
val matrix : t -> int array array

(* the node expanded into its parts, or collapsed back *)
val toggle : t -> node -> t

(* a cell explained: the uses between two nodes, at the definitions'
   grain (from, to, how many), the most first *)
val explain : t -> node -> node -> (string * string * int) list

val path_of : node -> string
val name_of : node -> string

(* a cell zoomed into: a matrix of its row and its column alone, each
   expanded into its parts *)
val focus : t -> node -> node -> t
