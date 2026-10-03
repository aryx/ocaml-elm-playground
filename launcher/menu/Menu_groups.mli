(* Menu_groups: what the menu shows. From the catalogue (Tinybox_data,
 * made at build time from CATALOG.md) and the model's grouping and
 * filters to the sections, the programs in the grid, and the chosen
 * one. Nothing is drawn here, and nothing reads the keys.
 *
 * Groups and filters, after Batocera's and RetroBox's front ends: the
 * programs are put in sections by genre (the catalogue's own, the
 * default), by size, by era, by platform or by players, and what is
 * kept of them is filtered by players, era, machine and look; a search
 * replaces the sections by every match. *)

open Menu_model

(*****************************************************************************)
(* The model at the start *)
(*****************************************************************************)

(* the menu's first model, from its flags (the command line's, or the
 * URL's): chosen=<Name> puts it on that program, code=<Name> also asks
 * for that program's code map. A name as tinybox's command line takes
 * one: any case, "Tiny" optional *)
val initial : Playground.flags -> model

(*****************************************************************************)
(* Groups and filters *)
(*****************************************************************************)

(* the groupings, in the order b goes through them, and their names *)
val groupings : grouping list
val grouping_name : grouping -> string

val no_filters : filters

(* the values a filter goes through: only those some program has *)
val decades : int list
val platforms : string list
val looks : string list

(* [cycle values cur]: the value after [cur], and after the last one,
 * any (None) *)
val cycle : 'a list -> 'a option -> 'a option

(* the sections of a grouping, those with a program passing the filters;
 * each one's programs by year, oldest first (by lines, for the sizes) *)
val groups : grouping -> filters -> group array

(*****************************************************************************)
(* What is shown *)
(*****************************************************************************)

(* the section shown, if any passes the filters *)
val current_group : model -> group option

(* the programs in the grid: the section's, or with a search every
 * match (name, original and line) *)
val shown : model -> Catalogue.program list

(* the one at the model's position *)
val chosen : model -> Catalogue.program option

(* a program's own code in lines, counted at build time, as its code map
 * shows it; and programs by it, smallest first *)
val lines_of : Catalogue.program -> int
val by_lines : Catalogue.program list -> Catalogue.program list

(*****************************************************************************)
(* The previews' clock *)
(*****************************************************************************)

(* the menu's frames, counted by update: frames rather than seconds, so
 * that -fixed-time shows the previews too *)
val frames : int ref

(* the program chosen, and the frame it was *)
val chosen_since : (string * int) ref

(* [track chosen], once a frame: the one chosen now noted, and for how
 * many frames it has been *)
val track : Catalogue.program option -> int
