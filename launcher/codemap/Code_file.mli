(* A source file, lexed and coloured once, as the map and the file's
   view draw it: its lines of spans (Highlight_code's), its grid (a
   byte per character, its category's, SeeSoft's picture of the file)
   and its definitions, what the map writes large over a file seen from
   afar (codemap's semantic zoom). *)

type t = {
  path : string; (* "games/platform/TinyMario.ml" *)
  lines : Highlight_code.span list array;
  grid : Bytes.t; (* [cols] a line: 0 a space, else 1 + Highlight_code.index *)
  defs : (int * string * Highlight_code.category) list; (* line (from 0), name, category: the top-level ones *)
}

(* the grid's width: longer lines are cut *)
val cols : int

(* [make path src]: [src] highlighted by its language's highlighter,
   chosen by [path]'s extension (OCaml's for .ml and .mli; another
   language's text is shown uncoloured) *)
val make : string -> string -> t

val nlines : t -> int

(* the category at a line and a column, None for a space *)
val at : t -> int -> int -> Highlight_code.category option

(* the modules [src] names: M in M.x, open M, include M *)
val modules_used : string -> string list
