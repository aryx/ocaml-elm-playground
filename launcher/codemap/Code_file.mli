(* A source file, lexed and coloured once, as the map and the file's
   view draw it: its lines of spans (Highlight_code's), its grid (a
   byte per character, its category's, SeeSoft's picture of the file)
   and its definitions, what the map writes large over a file seen from
   afar (codemap's semantic zoom). *)

type t = {
  path : string; (* "games/platform/TinyMario.ml" *)
  lines : Highlight_code.span list array;
  grid : Bytes.t; (* [cols] a line: 0 a space, else 1 + Highlight_code.index *)
  chars : Bytes.t; (* the same cells' characters, code page 437 (Vga_font) *)
  defs : (int * string * Highlight_code.category) list; (* line (from 0), name, category: the top-level ones *)
  marks : int list; (* claude: the lines (from 0) saying [trick] *)
  names : Highlight_code.occurrence list array; (* claude: a line's names bound in the file (parameters, locals, top-level definitions) *)
  uses : (int * int, Highlight_code.occurrence list) Hashtbl.t; (* ... by their binding's place *)
  (* claude: for the other files (plan_codemap_naming.md, level 3,
     Highlight_code.analysis): its top-level definitions, a line's names
     defined elsewhere, its opens (OCaml) and own headers (C) *)
  definitions : Highlight_code.definition list;
  refs : Highlight_code.reference list array;
  opens : string list;
  includes : string list;
}

(* claude: "the trick of this game", what a game faking 3D (and a few
   others) writes where its trick is (games/README-2.5d.md) *)
val trick : string

(* the grids' width: longer lines are cut. A cell is a byte of the
   line: a character of several bytes in UTF-8 is in its first cell, the
   others blank *)
val cols : int

(* claude: the characters a classic map's column draws of a line *)
val shown : int

(* [make path src]: [src] highlighted by its language's highlighter,
   chosen by [path]'s extension (OCaml's for .ml and .mli; another
   language's text is shown uncoloured) *)
val make : string -> string -> t

val nlines : t -> int

(* the category at a line and a column, None for a space *)
val at : t -> int -> int -> Highlight_code.category option

(* claude: the name bound in the file at a line and a column, if any
   (plan_codemap_naming.md, levels 1 and 2), and all the places of its binding:
   the binding itself and its uses *)
val name_at : t -> int -> int -> Highlight_code.occurrence option
val uses : t -> Highlight_code.occurrence -> Highlight_code.occurrence list

(* claude: the name defined elsewhere at a line and a column, if any *)
val ref_at : t -> int -> int -> Highlight_code.reference option

(* the modules [src] names: M in M.x, open M, include M *)
val modules_used : string -> string list
