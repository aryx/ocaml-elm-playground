(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Menu_groups.mli *)

open Playground
open Menu_model

(*****************************************************************************)
(* The catalogue *)
(*****************************************************************************)

let sections : Catalogue.section array = Array.of_list (Catalogue.parse Tinybox_data.catalogue)

let everything : Catalogue.program list =
  Array.to_list sections |> List.concat_map (fun (s : Catalogue.section) -> s.programs)

let lowercase = String.lowercase_ascii

(*****************************************************************************)
(* Groups and filters *)
(*****************************************************************************)


(* claude: size second: tinybox is first for learning, the smallest
 * programs the first to read *)
let groupings = [ By_genre; By_size; By_era; By_platform; By_players ]

let grouping_name = function
  | By_genre -> "genre"
  | By_era -> "era"
  | By_platform -> "machine"
  | By_players -> "players"
  | By_size -> "size"


let no_filters = { players = None; era = None; platform = None; look = None }

let passes (f : filters) (p : Catalogue.program) : bool =
  (match f.players with
  | None -> true
  | Some "net" -> Catalogue.online p
  | Some "2" -> Catalogue.plays p 2
  | Some _ -> Catalogue.plays p 1)
  && (match f.era with None -> true | Some d -> Catalogue.decade p = d)
  && (match f.platform with None -> true | Some pf -> p.platform = pf)
  && match f.look with None -> true | Some l -> p.look = l

(* the values a filter goes through: only those some program has *)
let decades : int list = List.sort_uniq compare (List.map Catalogue.decade everything)

let platforms : string list =
  List.filter (fun pf -> List.exists (fun (p : Catalogue.program) -> p.platform = pf) everything) Catalogue.platforms

let looks = [ "2D"; "2.5D"; "3D"; "app" ]

let platform_title = function
  | "arcade" -> "Arcade"
  | "console" -> "Consoles"
  | "handheld" -> "Handhelds"
  | "computer" -> "Home computers"
  | "PC" -> "The PC"
  | "Mac" -> "The Macintosh"
  | "workstation" -> "Workstations"
  | "mainframe" -> "Mainframes and minicomputers"
  | "web" -> "The web"
  | "phone" -> "Phones"
  | "tabletop" -> "Tabletop"
  | "instrument" -> "Instruments"
  | pf -> String.capitalize_ascii pf

(* a section of the grid: the catalogue's, or made by a grouping *)

(* claude: every grouping's programs by year, oldest first -- CATALOG.md's
 * sections are in that order too, this in case a row is added out of it *)
let by_year (ps : Catalogue.program list) = List.stable_sort (fun (a : Catalogue.program) b -> compare a.year b.year) ps

(* claude: the size of a program's own code (files, lines), as its code
 * map in the panel shows it: counted at build time (Tinybox_data.sizes,
 * launcher/codegen) *)
let sizes : (string, int * int) Hashtbl.t Lazy.t =
  lazy
    (let h = Hashtbl.create 256 in
     List.iter (fun (name, s) -> Hashtbl.replace h name s) Tinybox_data.sizes;
     h)

let size_of (p : Catalogue.program) : int * int = Option.value ~default:(0, 0) (Hashtbl.find_opt (Lazy.force sizes) p.name)

let lines_of p = snd (size_of p)
let by_lines (ps : Catalogue.program list) = List.stable_sort (fun a b -> compare (lines_of a) (lines_of b)) ps

(* the sizes' sections: at most [hi] lines *)
let size_classes =
  [ ("Under 500 lines", 500); ("500 to 1,000 lines", 1000); ("1,000 to 2,000 lines", 2000); ("2,000 to 5,000 lines", 5000); ("Over 5,000 lines", max_int) ]

let groups (grouping : grouping) (f : filters) : group array =
  let keep ps = List.filter (passes f) ps in
  let make title intro programs = { title; intro; games = None; programs } in
  (match grouping with
  | By_genre ->
      Array.to_list sections
      |> List.map (fun (s : Catalogue.section) -> { title = s.title; intro = s.intro; games = Some s.games; programs = by_year (keep s.programs) })
  | By_era ->
      decades
      |> List.map (fun d ->
             make (Printf.sprintf "The %ds" d)
               (Printf.sprintf "The programs whose original came out in the %ds, the oldest first." d)
               (by_year (keep (List.filter (fun p -> Catalogue.decade p = d) everything))))
  | By_platform ->
      platforms
      |> List.map (fun pf ->
             make (platform_title pf)
               (Printf.sprintf "The programs whose original ran on: %s. The oldest first." pf)
               (by_year (keep (List.filter (fun (p : Catalogue.program) -> p.platform = pf) everything))))
  | By_players ->
      [
        make "One player" "Alone, or against the computer." (by_year (keep (List.filter (fun p -> Catalogue.plays p 1) everything)));
        make "Two players" "Two on one keyboard (or split screen)." (by_year (keep (List.filter (fun p -> Catalogue.plays p 2) everything)));
        make "Over the network" "Two computers, one game: net=host on one, net=join on the other (Multiplayer)."
          (by_year (keep (List.filter Catalogue.online everything)));
      ]
  | By_size ->
      snd
        (List.fold_left
           (fun (lo, acc) (title, hi) ->
             let ps = List.filter (fun p -> let n = lines_of p in n > lo && n <= hi) everything in
             (hi, acc @ [ make title "The programs by the size of their own code (its files and kits', as the code map shows it), the smallest first: the first to read." (by_lines (keep ps)) ]))
           (0, []) size_classes))
  |> List.filter (fun g -> g.programs <> [])
  |> Array.of_list

(* the value after [cur] in [values], and after the last one, any *)
let cycle (values : 'a list) (cur : 'a option) : 'a option =
  match cur with
  | None -> List.nth_opt values 0
  | Some v ->
      let rec after = function [] | [ _ ] -> None | x :: (y :: _ as rest) -> if x = v then Some y else after rest in
      after values

let contains (s : string) (q : string) : bool =
  let s = lowercase s and q = lowercase q in
  let n = String.length q in
  let rec at i = i + n <= String.length s && (String.sub s i n = q || at (i + 1)) in
  at 0

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

let initial_model : model =
  { grouping = By_genre; filters = no_filters; section = 0; pos = 0; search = None; before = Set_.empty; repeat = None; status = ""; code = None; code_asked = false }

(* claude: a program named as tinybox's command line names one: its
 * name, in any case, "Tiny" optional (turbopascal, TinyTurboPascal) *)
let find_program (name : string) : Catalogue.program option =
  let q = lowercase name in
  List.find_opt (fun (p : Catalogue.program) -> lowercase p.name = q || lowercase p.name = "tiny" ^ q) everything

(* claude: the menu started on a program, its flags (natively the
 * command line's, on the web the URL's): chosen=<Name> puts it in the
 * grid on that program, code=<Name> opens that program's code map too --
 * so a link, tinybox.html?code=TinyTurboPascal, is a program's code to
 * read (the website's cards link there) *)
let initial (flags : flags) : model =
  (* claude: and style=classic, the code maps' style (Code_map) *)
  Option.iter Code_map.choose_style (List.assoc_opt "style" flags);
  let named key = Option.bind (List.assoc_opt key flags) find_program in
  match (named "code", named "chosen") with
  | None, None -> initial_model
  | Some p, _ | None, Some p ->
      let gs = groups By_genre no_filters in
      let rec find i =
        if i >= Array.length gs then initial_model
        else
          let rec index k = function [] -> None | (q : Catalogue.program) :: rest -> if q.name = p.name then Some k else index (k + 1) rest in
          match index 0 gs.(i).programs with
          | Some pos -> { initial_model with section = i; pos }
          | None -> find (i + 1)
      in
      { (find 0) with code_asked = named "code" <> None }

(* the section shown, if any passes the filters *)
let current_group (m : model) : group option =
  let gs = groups m.grouping m.filters in
  if gs = [||] then None else Some gs.(min m.section (Array.length gs - 1))

(* the programs in the grid *)
let shown (m : model) : Catalogue.program list =
  match m.search with
  | Some q when q <> "" ->
      List.filter
        (fun (p : Catalogue.program) -> passes m.filters p && (contains p.name q || contains p.after q || contains p.one_line q))
        everything
  | _ -> ( match current_group m with Some g -> g.programs | None -> [])

let chosen (m : model) : Catalogue.program option = List.nth_opt (shown m) m.pos

(*****************************************************************************)
(* The chosen one *)
(*****************************************************************************)

(* the menu's frames, counted by update: the previews' clock, frames
 * rather than seconds, so that -fixed-time shows them too (the host's
 * previews, and the code's in the panel) *)
let frames = ref 0

(* the program chosen, and the frame it was *)
let chosen_since : (string * int) ref = ref ("", 0)

(* a frame: the one chosen now, and how long it has been *)
let track (chosen : Catalogue.program option) : int =
  incr frames;
  (match chosen with Some p when fst !chosen_since <> p.name -> chosen_since := (p.name, !frames) | _ -> ());
  !frames - snd !chosen_since
