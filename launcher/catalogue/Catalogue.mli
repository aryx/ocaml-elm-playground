(* The catalogue of the games and apps, CATALOG.md, read.
 *
 * CATALOG.md is written for people, a Markdown page, but on
 * conventions strict enough to be read by a program (see its
 * introduction): two parts, "# Games" and "# Apps"; in each, sections,
 * "## Platform", each starting with its paragraphs (a genre's
 * definition) and then a table, a row per program:
 *
 *   | [TinyMario](games/platform/TinyMario.ml) | 2D | 1985 | console | 1 | Super Mario Bros. (...) | Run, jump... | The side-scroller... |
 *
 * the name linking to its source, how it is drawn, the original's year,
 * platform, how many play here, the original it is after, a line to say
 * what it is, and what the original brought. Here
 * they become values, the Markdown taken out of the text (`code`,
 * **bold**, [links](...)): what tinybox's menu shows.
 *)

type program = {
  name : string; (* "TinyMario" *)
  source : string; (* "games/platform/TinyMario.ml" *)
  look : string; (* "2D", "2.5D", "3D" or "app" *)
  year : int; (* the original's *)
  platform : string; (* one of [platforms] *)
  players : string; (* "1", "2", "1-2", and " (net)" if also over the network *)
  after : string;
  one_line : string;
  brought : string;
}

type section = {
  title : string; (* "Platform" *)
  games : bool; (* under "# Games", else "# Apps" *)
  intro : string; (* its paragraphs, before the table, joined *)
  programs : program list;
}

(* the sections in the file's order, the ones without programs left out;
 * a row with a cell missing or a year that is not a number is left out
 * too (Unit_catalogue checks CATALOG.md has none) *)
val parse : string -> section list

(* the Platform column's words, in the order a menu shows them *)
val platforms : string list

(* [plays p n]: [n] people (1 or 2) can play [p] on one computer *)
val plays : program -> int -> bool

(* [online p]: [p] also plays over the network *)
val online : program -> bool

(* the decade of the year: 1985 -> 1980 *)
val decade : program -> int

(* Markdown's inline marks taken out: `code` and **bold** keep their
 * text, [text](target) its text:
 *
 *   plain "`games/fps/`: [TinyDoom](x.ml)'s **BSP**"  =  "games/fps/: TinyDoom's BSP"
 *)
val plain : string -> string

(* where a program's golden frame is: tests/3d/golden for a 3D
 * program, tests/2d/golden for the others (2.5D is the 2D playground) *)
val golden_frame : program -> string
