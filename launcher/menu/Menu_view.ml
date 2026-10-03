(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Menu_view.mli *)

open Playground
open Menu_model
open Menu_layout

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let background = rgb 12 10 28
let panel = rgb 26 22 56
let cyan = rgb 0 225 255
let magenta = rgb 255 60 170
let yellow = rgb 255 215 70
let ink = rgb 228 228 240
let dim = rgb 140 140 180
let grey = rgb 80 80 100

(* claude: how wide a character is, for the size given: the playground
 * centres words and cannot say how wide they are, so a left end is
 * placed by an estimate -- 0.47 of the size for Cairo's sans-serif,
 * measured on this menu's own frames (Widget.text_width's 0.6 is the
 * Hershey font's) *)
let em = 0.47

(* words at [size], their left end at [x] *)
let text ?(size = 16.) (color : color) (x : number) (y : number) (s : string) : shape =
  let w = em *. size *. float_of_int (String.length s) in
  words color s |> scale (size /. words_font_size) |> move (x +. (w /. 2.)) y

let centred ?(size = 16.) (color : color) (x : number) (y : number) (s : string) : shape =
  words color s |> scale (size /. words_font_size) |> move x y

(* [s] cut at the last character that fits in [width] at [size], with
 * "..." *)
let cut ~(size : number) ~(width : number) (s : string) : string =
  let n = int_of_float (width /. (em *. size)) in
  if String.length s <= n then s else String.sub s 0 (max 0 (n - 3)) ^ "..."

(* [s] in lines of at most [width] at [size], at most [lines] of them *)
let wrap ~(size : number) ~(width : number) ~(lines : int) (s : string) : string list =
  let n = max 1 (int_of_float (width /. (em *. size))) in
  let words = List.filter (( <> ) "") (String.split_on_char ' ' s) in
  let rec go acc line = function
    | [] -> List.rev (if line = "" then acc else line :: acc)
    | w :: rest ->
        let next = if line = "" then w else line ^ " " ^ w in
        if String.length next <= n || line = "" then go acc next rest else go (line :: acc) w rest
  in
  let all = go [] "" words in
  if List.length all <= lines then all
  else List.filteri (fun i _ -> i < lines - 1) all @ [ cut ~size ~width (String.concat " " (List.filteri (fun i _ -> i >= lines - 1) all)) ]

let paragraph ?(size = 14.) (color : color) (x : number) (y : number) ~(width : number) ~(lines : int) (s : string) :
    shape list =
  List.mapi (fun i l -> text ~size color x (y -. (float_of_int i *. size *. 1.45)) l) (wrap ~size ~width ~lines s)

(* a frame [w] by [h] around (0, 0), [t] thick *)
let frame (color : color) (w : number) (h : number) (t : number) : shape =
  group
    [
      rectangle color (w +. (2. *. t)) t |> move_y ((h +. t) /. 2.);
      rectangle color (w +. (2. *. t)) t |> move_y (-.(h +. t) /. 2.);
      rectangle color t h |> move_x (-.(w +. t) /. 2.);
      rectangle color t h |> move_x ((w +. t) /. 2.);
    ]

let picture (host : host) (p : Catalogue.program) (size : number) : shape =
  match host.thumbnail p size with
  | Some shape -> shape
  | None -> group [ rectangle panel size size; centred dim 0. 0. "no picture" ]

(* the chosen one playing, if the host previews it *)
let preview_shapes (host : host) (p : Catalogue.program) : shape list option =
  Option.bind host.preview (fun pv -> pv.shapes p)

let header (computer : computer) (m : model) : shape list =
  let shelf = match (m.search, Menu_groups.current_group m) with None, Some g -> g.games | _ -> None in
  let games = shelf = Some true and apps = shelf = Some false in
  let tab on (x, y) label = [ text ~size:22. (if on then yellow else dim) (x -. 40.) y label ] @ if on then [ rectangle yellow 70. 3. |> move (x -. 2.) (y -. 17.) ] else [] in
  [ text ~size:40. magenta left_edge 452. "TINY"; text ~size:40. cyan (left_edge +. 96.) 452. "BOX" ]
  @ tab games games_tab "GAMES"
  @ tab apps apps_tab "APPS"
  @ [
      (match m.search with
      | Some q ->
          let cursor = if Float.rem (Menu_update.now computer) 1. < 0.5 then "_" else " " in
          text ~size:18. yellow (-340.) 452. (cut ~size:18. ~width:250. ("/" ^ q ^ cursor))
      | None -> text ~size:14. dim (-330.) 452. "/ to search   click TINYBOX: its own code");
    ]

let section_bar (m : model) : shape list =
  let n = List.length (Menu_groups.shown m) in
  match m.search with
  | Some q -> [ text ~size:24. yellow left_edge 400. (Printf.sprintf "Search: %s" q); text ~size:14. dim (-150.) 400. (Printf.sprintf "%d found" n) ]
  | None -> (
      let gs = Menu_groups.groups m.grouping m.filters in
      match Menu_groups.current_group m with
      | None -> [ text ~size:24. magenta left_edge 400. "Nothing passes the filters"; text ~size:13. dim left_edge 360. "c clears them" ]
      | Some g ->
          [
            centred ~size:24. cyan (fst prev_arrow) (snd prev_arrow) "<";
            text ~size:24. yellow (-834.) 400. (cut ~size:24. ~width:320. g.title);
            centred ~size:24. cyan (fst next_arrow) (snd next_arrow) ">";
            text ~size:14. dim (-150.) 400. (Printf.sprintf "%d / %d" (min m.section (Array.length gs - 1) + 1) (Array.length gs));
          ]
          @ paragraph ~size:13. dim left_edge 360. ~width:800. ~lines:1 g.intro)

(* the filter bar: each key, what it chooses, its value (yellow when it
 * filters) *)
let filter_bar (m : model) : shape list =
  let f = m.filters in
  let any = function Some v -> (v, true) | None -> ("any", false) in
  let words = function
    | "b" -> ("group", (Menu_groups.grouping_name m.grouping, m.grouping <> By_genre))
    | "p" -> ("players", any f.players)
    | "e" -> ("era", any (Option.map (fun d -> Printf.sprintf "%ds" d) f.era))
    | "m" -> ("machine", any f.platform)
    | "l" -> ("look", any f.look)
    | _ -> ("clear  r random", ("", false))
  in
  List.concat_map
    (fun (k, x) ->
      let label, (value, on) = words k in
      (* claude: each word placed on its own, with room to spare: short
       * words' widths are the estimate's worst *)
      [ text ~size:13. cyan x bar_y k; text ~size:13. dim (x +. 16.) bar_y (if value = "" then label else label ^ ":");
        text ~size:13. (if on then yellow else ink) (x +. 24. +. (8. *. float_of_int (String.length label))) bar_y value ])
    bar

(* claude: by genre, the section's smallest program: where to start
 * reading it (tinybox is first for learning); its badge in the grid,
 * and in the details why *)
let start_here (m : model) : string option =
  match (m.grouping, m.search, Menu_groups.shown m) with
  | By_genre, None, (_ :: _ :: _ as ps) -> Some (List.hd (Menu_groups.by_lines ps)).name
  | _ -> None

let grid (computer : computer) (host : host) (m : model) : shape list =
  (* claude: by genre, the section's smallest program: where to start
   * reading it (tinybox is first for learning) *)
  let start = start_here m in
  Menu_groups.shown m
  |> List.mapi (fun i (p : Catalogue.program) -> (i, p))
  |> List.concat_map (fun (i, (p : Catalogue.program)) ->
         match cell_centre m i with
         | None -> []
         | Some (x, y) ->
             let on = i = m.pos in
             let ok = List.mem p.name host.runnable in
             let glow = wave 0.35 1. 1.2 computer.time in
             [
               group
                 ((if on then [ frame cyan thumb thumb 4. |> fade glow ] else [ frame grey thumb thumb 1. ])
                 @ [ picture host p thumb |> fade (if ok then 1. else 0.35) ]
                 (* claude: grouped by size, its lines on a badge at the
                  * thumbnail's bottom right *)
                 @
                 if m.grouping = By_size && m.search = None then
                   let s = string_of_int (Menu_groups.lines_of p) in
                   let w = (7. *. float_of_int (String.length s)) +. 8. in
                   [ rectangle black w 17. |> fade 0.8 |> move ((thumb /. 2.) -. (w /. 2.) -. 2.) (-.(thumb /. 2.) +. 10.5);
                     centred ~size:12. yellow ((thumb /. 2.) -. (w /. 2.) -. 2.) (-.(thumb /. 2.) +. 10.5) s ]
                 else if start = Some p.name then
                   [ rectangle (rgb 30 150 70) 74. 17. |> move ((-.thumb /. 2.) +. 39.) ((thumb /. 2.) -. 10.5);
                     centred ~size:12. white ((-.thumb /. 2.) +. 39.) ((thumb /. 2.) -. 10.5) "start here" ]
                 else [])
               |> move x y;
               centred ~size:13. (if on then yellow else if ok then ink else grey) x (y -. (thumb /. 2.) -. 16.) (cut ~size:13. ~width:cell_w p.name);
             ])

(* "1 player", "1-2 players, over the network" *)
let players_text (p : Catalogue.program) : string =
  let count = match String.index_opt p.players ' ' with Some i -> String.sub p.players 0 i | None -> p.players in
  (count ^ if count = "1" then " player" else " players") ^ if Catalogue.online p then ", over the network" else ""

(* claude: the chosen one's code at a glance, its own code's map
 * (Codemap.preview) in the panel under its picture: made once it has
 * been chosen for 20 frames (lexing its files is not free while the
 * arrows run through the grid), the last one kept (a map keeps its
 * picture) *)
let code_preview : (string * Code_map.t) option ref = ref None

let code_of (host : host) (p : Catalogue.program) : Code_map.t option =
  match !code_preview with
  | Some (name, c) when name = p.name -> Some c
  | _ ->
      if fst !Menu_groups.chosen_since = p.name && !Menu_groups.frames - snd !Menu_groups.chosen_since >= 20 then (
      match host.sources () with
      | Sources sources ->
        let c = Codemap.preview ~area:code_area ~sources ~program:p.name ~path:p.source in
        code_preview := Some (p.name, c);
        Some c
      | Loading | No_sources _ -> None)
      else None

let code_panel (host : host) (computer : computer) (p : Catalogue.program) : shape list =
  let x, y, w, h = code_area in
  let w = float_of_int w and h = float_of_int h in
  let cx = x +. (w /. 2.) and cy = y -. (h /. 2.) in
  let code_of = code_of host in
  (* claude: the sources not here (yet): say so where the map would be *)
  match host.sources () with
  | (Loading | No_sources _) as s ->
    [ rectangle panel w h |> move cx cy; centred ~size:14. dim cx cy (Menu_update.not_yet s); frame cyan w h 2. |> move cx cy ]
  | Sources _ ->
  (* claude: how much code, all the files shown, once the map is made *)
  let size =
    match code_of p with
    | Some c -> Printf.sprintf " (%d file%s, %s)" (Code_map.files c) (if Code_map.files c = 1 then "" else "s") (Code_map.lines_text (Code_map.lines c))
    | None -> ""
  in
  (match code_of p with
  | Some c -> Code_map.view ~chrome:false computer c
  | None -> [ rectangle panel w h |> move cx cy; centred ~size:14. dim cx cy "its code..." ])
  @ [ frame cyan w h 2. |> move cx cy; text ~size:12. cyan x (y -. h -. 16.) ("its code" ^ size ^ ": a click, or s, to read it   o: the glass (" ^ Code_map.glass_name ~panel:true () ^ ")") ]
  (* claude: the mouse over it: a magnifying glass, the code under it
   * readable (Code_map.glass), over everything else *)
  @ match code_of p with Some c -> Code_map.glass ~panel:true computer c | None -> []

(* the chosen one, large, and what the catalogue says of it *)
let details (computer : computer) (host : host) (m : model) : shape list =
  match Menu_groups.chosen m with
  | None -> [ centred ~size:18. dim shot_x shot_y "nothing here" ]
  | Some p ->
      let left = text_x in
      let ok = List.mem p.name host.runnable in
      [ frame magenta shot shot 3. |> move shot_x shot_y ]
      @ (match preview_shapes host p with Some _ -> [] | None -> [ picture host p shot |> move shot_x shot_y ])
      @ [
          text ~size:24. ink left 410. (cut ~size:24. ~width:(text_w -. 60.) p.name);
          text ~size:16. yellow (left +. text_w -. 40.) 410. p.look;
          (* claude: and the size of its own code, as its code map shows it *)
          text ~size:13. yellow left 380.
            (Printf.sprintf "%d   %s   %s   %s" p.year p.platform (players_text p) (Code_map.lines_text (Menu_groups.lines_of p)));
        ]
      (* claude: why "start here": the smallest of its section, and what
       * is counted (not the library: a program over libs/ can look
       * smaller than it is) *)
      @ (match (start_here m, Menu_groups.current_group m) with
        | Some name, Some g when name = p.name ->
            [ text ~size:12. (rgb 90 210 120) left 358.
                (cut ~size:12. ~width:text_w
                   (Printf.sprintf "start here: the smallest in %s (its own code: libs/ not counted)" g.title)) ]
        | _ -> [])
      (* claude: what the host says of its preview (natively, the
       * software rasterizer's time on a 3D one: tinybox as its stress
       * test, every 3D game drawn live) *)
      @ (match Option.bind host.preview (fun pv -> pv.note p) with
        | Some note -> [ text ~size:11. dim left 358. note ]
        | None -> [])
      @ paragraph ~size:14. cyan left 330. ~width:text_w ~lines:2 ("After " ^ p.after)
      @ paragraph ~size:14. ink left 280. ~width:text_w ~lines:3 p.one_line
      @ paragraph ~size:12. dim left 210. ~width:text_w ~lines:9 p.brought
      @ (if ok then [] else [ text ~size:14. magenta left 45. "not in this tinybox" ])
      @ code_panel host computer p

(* claude: tinybox's version, at the bottom right (the author: as the
 * code map's, to see at once whether a page runs the latest, a browser
 * keeping the program it has for a while): 0.01, 0.02, ..., raised by
 * hand at each publish of a change to tinybox (make publish) *)
let version = "0.01"

let footer (host : host) (m : model) : shape list =
  let playing = match host.running () with Some name -> [ text ~size:18. yellow left_edge (-440.) ("> " ^ name ^ " is running") ] | None -> [] in
  let v = "tinybox " ^ version in
  [ text ~size:13. dim left_edge (-475.) "arrows move   tab section   g/a games/apps   b group   p e m l filter   / search   s read its code   enter play it" ]
  @ [ text ~size:13. dim (-.left_edge -. (em *. 13. *. float_of_int (String.length v))) (-475.) v ]
  @ playing
  @ if m.status = "" then [] else [ text ~size:16. magenta (-400.) (-440.) (cut ~size:16. ~width:340. m.status) ]

(* claude: a CRT's lines, every 4 pixels a darker one over everything *)
let scanlines (screen : screen) : shape list =
  List.init (int_of_float (screen.height /. 4.)) (fun i ->
      rectangle black screen.width 1.5 |> move_y (screen.top -. (float_of_int i *. 4.)) |> fade 0.18)

(* claude: the preview, its 1000 by 1000 scaled to the panel's 400, and
 * then the background's colour all round the panel, over whatever the
 * program draws beyond its screen (the playground has no clipping) *)
let live (host : host) (screen : screen) (m : model) : shape list =
  match Option.bind (Menu_groups.chosen m) (preview_shapes host) with
  | None -> []
  | Some shapes ->
      let half = shot /. 2. in
      let band w h x y = rectangle background w h |> move x y in
      [
        rectangle black shot shot |> move shot_x shot_y;
        group shapes |> scale (shot /. 1000.) |> move shot_x shot_y;
        band screen.width (screen.top -. (shot_y +. half)) 0. ((screen.top +. shot_y +. half) /. 2.);
        band screen.width ((shot_y -. half) -. screen.bottom) 0. ((screen.bottom +. shot_y -. half) /. 2.);
        band ((shot_x -. half) -. screen.left) shot ((screen.left +. shot_x -. half) /. 2.) shot_y;
        band (screen.right -. (shot_x +. half)) shot ((screen.right +. shot_x +. half) /. 2.) shot_y;
      ]

let view (host : host) (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  match m.code with
  | Some code ->
      Codemap.view computer code
      (* claude: under the map's title, what the program brought (the
       * catalogue's): what to look for in its code *)
      @ (match Menu_groups.chosen m with
        | _ when Codemap.program code = "tinybox" && not (Codemap.file_open code) ->
            [ centred ~size:13. cyan 0. (screen.top -. 72.)
                "every program in one binary (after BusyBox), its menu (after Batocera's), and this code map (after codemap and SeeSoft)" ]
        | Some p when not (Codemap.file_open code) ->
            [ centred ~size:13. cyan 0. (screen.top -. 72.) (cut ~size:13. ~width:(screen.width -. 80.) p.brought) ]
        | _ -> [])
  | None ->
  [ rectangle background screen.width screen.height ]
  @ live host screen m
  @ header computer m @ section_bar m @ filter_bar m @ grid computer host m @ details computer host m @ footer host m @ scanlines screen
