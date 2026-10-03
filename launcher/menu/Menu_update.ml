(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Menu_update.mli *)

open Playground
open Menu_model
open Menu_layout

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let now (computer : computer) : number =
  let (Time t) = computer.time in
  t

(* the section after (or before) the one shown, across both shelves *)
let step_section (m : model) (d : int) : int =
  let n = max 1 (Array.length (Menu_groups.groups m.grouping m.filters)) in
  (min m.section (n - 1) + d + n) mod n

(* the first section of a shelf, the programs grouped by genre again *)
let to_shelf (m : model) (games : bool) : model =
  let gs = Menu_groups.groups By_genre m.filters in
  let rec go i = if i >= Array.length gs || gs.(i).games = Some games then i else go (i + 1) in
  { m with grouping = By_genre; section = min (go 0) (max 0 (Array.length gs - 1)); pos = 0; search = None }

(* the program chosen started, the host's way (a process of its own, or
 * its page) *)
let start (host : host) (m : model) : model =
  match Menu_groups.chosen m with Some p -> { m with status = host.play p } | None -> m

(* the program running, if the menu waits for one: looked at once a frame *)
let wait (host : host) (m : model) : model =
  match host.ended () with Some status -> { m with status } | None -> m

(* an arrow's press, and then, held, again every 0.08 s after 0.4 s *)
let arrow (computer : computer) (m : model) : string option * (string * float) option =
  let keys = computer.keyboard.keys in
  let t = now computer in
  let arrows = [ "ArrowLeft"; "ArrowRight"; "ArrowUp"; "ArrowDown" ] in
  match List.find_opt (fun k -> Set_.mem k keys && not (Set_.mem k m.before)) arrows with
  | Some k -> (Some k, Some (k, t +. 0.4))
  | None -> (
      match m.repeat with
      | Some (k, next) when Set_.mem k keys -> if t >= next then (Some k, Some (k, t +. 0.08)) else (None, m.repeat)
      | _ -> (None, None))

let move_pos (m : model) (key : string) : model =
  let n = List.length (Menu_groups.shown m) in
  let pos =
    match key with
    | "ArrowLeft" -> m.pos - 1
    | "ArrowRight" -> m.pos + 1
    | "ArrowUp" -> m.pos - cols
    | "ArrowDown" -> m.pos + cols
    | _ -> m.pos
  in
  { m with pos = max 0 (min (n - 1) pos) }

let to_section (m : model) (i : int) : model = { m with section = i; pos = 0; search = None }

(* a key of the filter bar *)
let bar_key (computer : computer) (m : model) (key : string) : model =
  let f = m.filters in
  let refilter filters = { m with filters; section = 0; pos = 0 } in
  match key with
  | "b" -> { m with grouping = Option.value ~default:By_genre (Menu_groups.cycle Menu_groups.groupings (Some m.grouping)); section = 0; pos = 0 }
  | "p" -> refilter { f with players = Menu_groups.cycle [ "1"; "2"; "net" ] f.players }
  | "e" -> refilter { f with era = Menu_groups.cycle Menu_groups.decades f.era }
  | "m" -> refilter { f with platform = Menu_groups.cycle Menu_groups.platforms f.platform }
  | "l" -> refilter { f with look = Menu_groups.cycle Menu_groups.looks f.look }
  | "c" -> refilter Menu_groups.no_filters
  | "r" ->
      (* claude: a random program of the grid, from the clock (the same
       * one under -fixed-time, so a golden frame could show it) *)
      let n = List.length (Menu_groups.shown m) in
      if n = 0 then m else { m with pos = Hashtbl.hash (int_of_float (now computer *. 1000.)) mod n }
  | _ -> m

(* the whole screen, but for the title and what the program brought
 * above and the status and keys below *)
let code_map_area (screen : screen) = (screen.left +. 20., screen.top -. 92., int_of_float screen.width - 40, int_of_float screen.height - 162)

(* claude: the sources not here yet (the web's, on their way) *)
let not_yet = function Loading -> "its code: on its way..." | No_sources why -> "its code: " ^ why | Sources _ -> ""

(* the chosen program's code map *)
let open_code (host : host) (screen : screen) (m : model) : model =
  match (Menu_groups.chosen m, host.sources ()) with
  | Some p, Sources sources -> { m with code = Some (Codemap.make ~area:(code_map_area screen) ~sources ~program:p.name ~path:p.source) }
  | Some _, s -> { m with status = not_yet s }
  | None, _ -> m

(* claude: tinybox's own code map, the menu and the code map showing
 * themselves: all of launcher/ its own code, from its main, the
 * languages it uses (OCaml's lexer and highlighter) and the program
 * analysis it does (libs/code: Highlight_code); not the
 * kits, which it names only to run the programs they are (the editors,
 * -tty) *)
let tinybox_code (host : host) (screen : screen) (m : model) : model =
  let starts pre p = String.length p >= String.length pre && String.sub p 0 (String.length pre) = pre in
  let own p = starts "launcher/" p || starts "languages/" p || starts "libs/code/" p in
  match host.sources () with
  | Sources sources ->
      { m with code = Some (Codemap.make_own ~own ~area:(code_map_area screen) ~sources ~program:"tinybox" ~path:"launcher/native/Tinybox.ml") }
  | s -> { m with status = not_yet s }

let update (host : host) (computer : computer) (m : model) : model =
  let m = wait host m in
  (* claude: code= asked for its code map: opened when the sources are
   * here (on the web, a few seconds after the page) *)
  let m =
    if not m.code_asked then m
    else
      match host.sources () with
      | Sources _ -> { (open_code host computer.screen m) with code_asked = false; status = "" }
      | Loading as s -> { m with status = not_yet s }
      | No_sources _ as s -> { m with code_asked = false; status = not_yet s }
  in
  match m.code with
  | Some code ->
      let keys = computer.keyboard.keys in
      let pressed k = Set_.mem k keys && not (Set_.mem k m.before) in
      let key, repeat = arrow computer m in
      { m with code = Codemap.update computer ~pressed ~arrow:key code; repeat; before = keys }
  | None ->
  let keys = computer.keyboard.keys in
  let pressed k = Set_.mem k keys && not (Set_.mem k m.before) in
  let shift = Set_.mem "Shift" keys in
  let key, repeat = arrow computer m in
  let m = { m with repeat } in
  let m = match key with Some k -> move_pos m k | None -> m in
  let m =
    match m.search with
    | Some q ->
        (* claude: typing: every character typed is the query's, but
         * for the / that opened it, already typed this frame *)
        let typed = String.concat "" (List.map (String.make 1) (List.filter (fun c -> c >= ' ' && c <> '/') (List.of_seq (String.to_seq computer.keyboard.typed)))) in
        if pressed "Escape" then { m with search = None; pos = 0 }
        else if pressed "Backspace" && q <> "" then { m with search = Some (String.sub q 0 (String.length q - 1)); pos = 0 }
        else if typed <> "" then { m with search = Some (q ^ typed); pos = 0 }
        else m
    | None ->
        if pressed "/" then { m with search = Some ""; pos = 0 }
        else if pressed "Tab" then to_section m (step_section m (if shift then -1 else 1))
        else if pressed "PageDown" then to_section m (step_section m 1)
        else if pressed "PageUp" then to_section m (step_section m (-1))
        else if pressed "g" then to_shelf m true
        else if pressed "a" then to_shelf m false
        else if pressed "s" then open_code host computer.screen m
        else if pressed "o" then (
          (* claude: the panel's magnifying glass: round, wide, none *)
          Code_map.cycle_glass ~panel:true ();
          m)
        else
          match List.find_opt pressed [ "b"; "p"; "e"; "m"; "l"; "c"; "r" ] with
          | Some k -> bar_key computer m k
          | None -> m
  in
  let m = if pressed "Enter" then start host m else m in
  (* the mouse *)
  let mouse = computer.mouse in
  let at = (mouse.mx, mouse.my) in
  let m =
    if mouse.mwheel > 0. then move_pos m "ArrowUp" else if mouse.mwheel < 0. then move_pos m "ArrowDown" else m
  in
  let m =
    if not (mouse.mclick || mouse.mdouble) then m
    else if near (left_edge +. 90., 452.) at ~w:190. ~h:44. then tinybox_code host computer.screen m
    else if near games_tab at ~w:90. ~h:36. then to_shelf m true
    else if near apps_tab at ~w:80. ~h:36. then to_shelf m false
    (* claude: a click on the preview plays the program (the author: "when
     * we click on the preview we should also run the game") *)
    else if near (shot_x, shot_y) at ~w:shot ~h:shot then start host m
    else if in_code_area at then open_code host computer.screen m
    else if near prev_arrow at ~w:40. ~h:40. then to_section m (step_section m (-1))
    else if near next_arrow at ~w:40. ~h:40. then to_section m (step_section m 1)
    else
      match List.find_opt (fun (_, x) -> near (x +. (bar_width /. 2.), bar_y) at ~w:bar_width ~h:24.) bar with
      | Some (k, _) -> bar_key computer m k
      | None ->
      let n = List.length (Menu_groups.shown m) in
      match List.find_opt (fun i -> match cell_centre m i with Some c -> near c at ~w:thumb ~h:(thumb +. 30.) | None -> false) (List.init n Fun.id) with
      | Some i -> if mouse.mdouble then start host { m with pos = i } else { m with pos = i }
      | None -> m
  in
  let dwell = Menu_groups.track (Menu_groups.chosen m) in
  Option.iter (fun pv -> pv.step ~now:(now computer) ~dwell (Menu_groups.chosen m)) host.preview;
  { m with before = keys }
