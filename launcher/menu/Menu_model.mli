(* Menu_model: tinybox's menu's types, and nothing else: what the menu
 * needs from where it runs (its host), how the catalogue is grouped and
 * filtered, and the model, what changes from a frame to the next. The
 * other parts of the menu open this module: Menu_groups (what is
 * shown), Menu_layout (where), Menu_update and Menu_view.
 *
 * The menu is a Playground game, so the model is all its state: there
 * is none elsewhere but the host's (a program running, sources being
 * fetched), reached through the host's functions. *)

(*****************************************************************************)
(* The host *)
(*****************************************************************************)

(* what the menu needs from where it runs: natively (Tinybox_native),
 * every program linked in, one started in a process of its own; on the
 * web (Tinybox_web), one started by loading its own page *)
type host = {
  runnable : string list; (* the programs it can start (the others greyed) *)
  thumbnail : Catalogue.program -> Playground.number -> Playground.shape option;
  play : Catalogue.program -> string; (* start it; "", or why it could not *)
  running : unit -> string option; (* the program the menu waits for, if any *)
  ended : unit -> string option;
      (* once, when the one waited for ended: "", or how it failed *)
  sources : unit -> sources;
      (* the repository's sources, for the code map: natively at once,
       * on the web once fetched (asking for them starts the fetch) *)
  preview : preview option; (* the chosen one, live in the panel *)
}

and sources =
  | Sources of (string * string) list (* (path, text) *)
  | Loading (* on their way *)
  | No_sources of string (* why not *)

(* the chosen program playing in the detail panel, after a second on it
 * (natively: Tinybox_native's "Previews") *)
and preview = {
  step : now:Playground.number -> dwell:int -> Catalogue.program option -> unit;
      (* a frame of the menu: [dwell] frames on the chosen one so far *)
  shapes : Catalogue.program -> Playground.shape list option;
      (* its view, in its 1000 by 1000, once it plays *)
  note : Catalogue.program -> string option; (* what to say of it, if anything *)
}

(*****************************************************************************)
(* Groups and filters *)
(*****************************************************************************)

(* how the programs are put in sections (b): the catalogue's own, by
 * genre; or by era, a decade a section; by platform; by players; by
 * the size of their code *)
type grouping = By_genre | By_era | By_platform | By_players | By_size

(* what is kept of them (p, e, m, l); None: any *)
type filters = {
  players : string option; (* "1", "2", "net" *)
  era : int option; (* a decade: 1980 *)
  platform : string option;
  look : string option; (* "2D", "2.5D", "3D", an app's *)
}

(* a section of the grid: its title and what it says of itself; whether
 * it is on the games' shelf, the apps', or (None) neither's; its
 * programs, in order *)
type group = { title : string; intro : string; games : bool option; programs : Catalogue.program list }

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  grouping : grouping;
  filters : filters;
  section : int; (* in Menu_groups.groups m.grouping m.filters *)
  pos : int; (* in Menu_groups.shown *)
  search : string option; (* Some q: searching, the grid is every match *)
  before : string Set_.t; (* the keys down the frame before *)
  repeat : (string * float) option; (* an arrow held, when it moves again *)
  status : string; (* what happened to the last one *)
  code : Codemap.t option; (* the chosen one's code, shown instead of the menu *)
  code_asked : bool; (* its code to be opened once the sources are here (code=) *)
}
