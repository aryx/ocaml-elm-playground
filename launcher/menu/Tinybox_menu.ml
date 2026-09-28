(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tinybox_menu.mli.
 *
 * A Playground game like the others, its world the catalogue
 * (Tinybox_data, made at build time from CATALOG.md and the golden
 * frames). The screen, 16:9, 1778 by 1000 (run_app's ~screen, scaled
 * to the window):
 *
 *   TINYBOX  GAMES  APPS  / search
 *   < Platform >              3 / 15  +-------------+  TinyMario  2D
 *   Run and jump from platform to...  |             |  After Super Mario
 *   +----+ +----+ +----+ +----+ +----+|  the chosen |  Bros. (...)
 *   |    | |    | |    | |    | |    ||  one, live  |  Run, jump, stomp...
 *   +----+ +----+ +----+ +----+ +----+|             |  The side-scroller:
 *   TinyMario TinySonic ...           +-------------+  ...
 *   +----+ +----+ +----+ +----+ +----++--------------------------------+
 *   ...                               |  its code (Codemap.preview),   |
 *                                     |  a click (or s) opens it all   |
 *                                     +--------------------------------+
 *   arrows move  tab section  / search  s read its code  enter play it
 *
 * Keys: the arrows, Tab and Shift-Tab (the next section, across both
 * shelves), g and a (the games' and the apps' first section), Enter
 * (play), / (search every program by name, original and line; Escape
 * leaves it). The mouse: a click chooses, a double click plays, the
 * wheel scrolls, the tabs and arrows at the top are buttons.
 *
 * Groups and filters, after Batocera's and RetroBox's (the catalogue's
 * Year, Platform and Players columns): b groups the programs by genre
 * (the catalogue's sections, the default), by era (a decade a
 * section, oldest first), by platform or by players; p, e, m and l
 * filter by players (1, 2, over the network), era, machine and look
 * (2D, 2.5D, 3D, app), each key going through the values and back to
 * any; c clears them; r jumps to a random program of the grid. The
 * filter bar under the section's title says what is chosen, and a click
 * on one of its words does what its key does.
 *
 * The look: a dark cabinet's colours, the chosen thumbnail's frame
 * pulsing, scanlines over everything (thin translucent rectangles, a
 * CRT's gaps between its lines).
 *
 * The thumbnails (250 by 250) are the host's: natively PNGs in the
 * binary, on the web URLs (Tinybox_menu.mli).
 *
 * The code: s opens the chosen program's code map (codemap/, after
 * codemap), its files and what it uses as a treemap to zoom into, a file
 * read by clicking on it; Escape comes back.
 *
 * The previews: a second (60 frames) on a program, and its picture comes
 * alive -- the program itself, playing its golden scene's script in the
 * detail panel, run by the menu (natively: Tinybox_native's "Previews").
 *)

open Playground

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

type grouping = By_genre | By_era | By_platform | By_players | By_size

(* claude: size second: tinybox is first for learning, the smallest
 * programs the first to read *)
let groupings = [ By_genre; By_size; By_era; By_platform; By_players ]

let grouping_name = function
  | By_genre -> "genre"
  | By_era -> "era"
  | By_platform -> "machine"
  | By_players -> "players"
  | By_size -> "size"

(* None: any *)
type filters = { players : string option; (* "1", "2", "net" *) era : int option; platform : string option; look : string option }

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
type group = { title : string; intro : string; games : bool option; programs : Catalogue.program list }

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

(* see Tinybox_menu.mli *)
type host = {
  runnable : string list;
  thumbnail : Catalogue.program -> number -> shape option;
  play : Catalogue.program -> string;
  running : unit -> string option;
  ended : unit -> string option;
  sources : unit -> sources;
  preview : preview option;
}

and sources = Sources of (string * string) list | Loading | No_sources of string

and preview = {
  step : now:number -> dwell:int -> Catalogue.program option -> unit;
  shapes : Catalogue.program -> shape list option;
  note : Catalogue.program -> string option;
}

type model = {
  grouping : grouping;
  filters : filters;
  section : int; (* in [groups m.grouping m.filters] *)
  pos : int; (* in [shown] *)
  search : string option; (* Some q: searching, the grid is every match *)
  before : string Set_.t; (* the keys down the frame before *)
  repeat : (string * float) option; (* an arrow held, when it moves again *)
  status : string; (* what happened to the last one *)
  code : Codemap.t option; (* the chosen one's code, shown instead of the menu *)
  code_asked : bool; (* its code to be opened once the sources are here (code=) *)
}

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
  (* claude: and style=streets, the code maps' style (Code_map) *)
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

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

(* claude: the menu's screen, 16:9 (run_app's ~screen): the grid on the
 * left, the chosen one on the right *)
let screen_w = 1778
let screen_h = 1000
let left_edge = -869. (* the left margin's end *)

let cols = 5
let rows = 4
let thumb = 132.
let cell_w = 160.
let cell_h = 172.
let grid_left = left_edge (* the first column's left edge *)
let grid_top = 285. (* the first row's top edge *)
let shot = 400. (* the chosen one's screenshot *)
let shot_x = 160.
let shot_y = 230.
let text_x = 385. (* what the catalogue says, right of the screenshot *)
let text_w = 480.

(* its code, under both: where Code_map draws it (its top left, pixels) *)
let code_area = (-40., 10., 909, 440)

let in_code_area ((mx, my) : number * number) : bool =
  let x, y, w, h = code_area in
  mx >= x && mx <= x +. float_of_int w && my <= y && my >= y -. float_of_int h

(* the first row shown: the chosen one's row kept in view *)
let first_row (m : model) : int = max 0 ((m.pos / cols) - rows + 1)

(* the centre of the thumbnail of the program at [i] in [shown], if it
 * is in view *)
let cell_centre (m : model) (i : int) : (number * number) option =
  let r = (i / cols) - first_row m in
  if r < 0 || r >= rows then None
  else
    let c = i mod cols in
    Some (grid_left +. (float_of_int c *. cell_w) +. (thumb /. 2.), grid_top -. (float_of_int r *. cell_h) -. (thumb /. 2.))

(* the buttons at the top: the two shelves, the section's arrows *)
let games_tab = (-560., 455.)
let apps_tab = (-460., 455.)
let prev_arrow = (-859., 400.)
let next_arrow = (-499., 400.)

(* the filter bar: each word's left end, and its key *)
let bar_y = 322.
let bar = [ ("b", -869.); ("p", -709.); ("e", -559.); ("m", -434.); ("l", -284.); ("c", -164.) ]
let bar_width = 120.

let near ((x, y) : number * number) ((mx, my) : number * number) ~(w : number) ~(h : number) : bool =
  Float.abs (mx -. x) <= w /. 2. && Float.abs (my -. y) <= h /. 2.

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let now (computer : computer) : number =
  let (Time t) = computer.time in
  t

(* the section after (or before) the one shown, across both shelves *)
let step_section (m : model) (d : int) : int =
  let n = max 1 (Array.length (groups m.grouping m.filters)) in
  (min m.section (n - 1) + d + n) mod n

(* the first section of a shelf, the programs grouped by genre again *)
let to_shelf (m : model) (games : bool) : model =
  let gs = groups By_genre m.filters in
  let rec go i = if i >= Array.length gs || gs.(i).games = Some games then i else go (i + 1) in
  { m with grouping = By_genre; section = min (go 0) (max 0 (Array.length gs - 1)); pos = 0; search = None }

(* the program chosen started, the host's way (a process of its own, or
 * its page) *)
let start (host : host) (m : model) : model =
  match chosen m with Some p -> { m with status = host.play p } | None -> m

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
  let n = List.length (shown m) in
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
  | "b" -> { m with grouping = Option.value ~default:By_genre (cycle groupings (Some m.grouping)); section = 0; pos = 0 }
  | "p" -> refilter { f with players = cycle [ "1"; "2"; "net" ] f.players }
  | "e" -> refilter { f with era = cycle decades f.era }
  | "m" -> refilter { f with platform = cycle platforms f.platform }
  | "l" -> refilter { f with look = cycle looks f.look }
  | "c" -> refilter no_filters
  | "r" ->
      (* claude: a random program of the grid, from the clock (the same
       * one under -fixed-time, so a golden frame could show it) *)
      let n = List.length (shown m) in
      if n = 0 then m else { m with pos = Hashtbl.hash (int_of_float (now computer *. 1000.)) mod n }
  | _ -> m

(* the whole screen, but for the title and what the program brought
 * above and the status and keys below *)
let code_map_area (screen : screen) = (screen.left +. 20., screen.top -. 92., int_of_float screen.width - 40, int_of_float screen.height - 162)

(* claude: the sources not here yet (the web's, on their way) *)
let not_yet = function Loading -> "its code: on its way..." | No_sources why -> "its code: " ^ why | Sources _ -> ""

(* the chosen program's code map *)
let open_code (host : host) (screen : screen) (m : model) : model =
  match (chosen m, host.sources ()) with
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
          Code_map.cycle_glass ();
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
    else if in_code_area at then open_code host computer.screen m
    else if near prev_arrow at ~w:40. ~h:40. then to_section m (step_section m (-1))
    else if near next_arrow at ~w:40. ~h:40. then to_section m (step_section m 1)
    else
      match List.find_opt (fun (_, x) -> near (x +. (bar_width /. 2.), bar_y) at ~w:bar_width ~h:24.) bar with
      | Some (k, _) -> bar_key computer m k
      | None ->
      let n = List.length (shown m) in
      match List.find_opt (fun i -> match cell_centre m i with Some c -> near c at ~w:thumb ~h:(thumb +. 30.) | None -> false) (List.init n Fun.id) with
      | Some i -> if mouse.mdouble then start host { m with pos = i } else { m with pos = i }
      | None -> m
  in
  let dwell = track (chosen m) in
  Option.iter (fun pv -> pv.step ~now:(now computer) ~dwell (chosen m)) host.preview;
  { m with before = keys }

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
  let shelf = match (m.search, current_group m) with None, Some g -> g.games | _ -> None in
  let games = shelf = Some true and apps = shelf = Some false in
  let tab on (x, y) label = [ text ~size:22. (if on then yellow else dim) (x -. 40.) y label ] @ if on then [ rectangle yellow 70. 3. |> move (x -. 2.) (y -. 17.) ] else [] in
  [ text ~size:40. magenta left_edge 452. "TINY"; text ~size:40. cyan (left_edge +. 96.) 452. "BOX" ]
  @ tab games games_tab "GAMES"
  @ tab apps apps_tab "APPS"
  @ [
      (match m.search with
      | Some q ->
          let cursor = if Float.rem (now computer) 1. < 0.5 then "_" else " " in
          text ~size:18. yellow (-340.) 452. (cut ~size:18. ~width:250. ("/" ^ q ^ cursor))
      | None -> text ~size:14. dim (-330.) 452. "/ to search   click TINYBOX: its own code");
    ]

let section_bar (m : model) : shape list =
  let n = List.length (shown m) in
  match m.search with
  | Some q -> [ text ~size:24. yellow left_edge 400. (Printf.sprintf "Search: %s" q); text ~size:14. dim (-150.) 400. (Printf.sprintf "%d found" n) ]
  | None -> (
      let gs = groups m.grouping m.filters in
      match current_group m with
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
    | "b" -> ("group", (grouping_name m.grouping, m.grouping <> By_genre))
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
  match (m.grouping, m.search, shown m) with
  | By_genre, None, (_ :: _ :: _ as ps) -> Some (List.hd (by_lines ps)).name
  | _ -> None

let grid (computer : computer) (host : host) (m : model) : shape list =
  (* claude: by genre, the section's smallest program: where to start
   * reading it (tinybox is first for learning) *)
  let start = start_here m in
  shown m
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
                   let s = string_of_int (lines_of p) in
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
      if fst !chosen_since = p.name && !frames - snd !chosen_since >= 20 then (
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
    [ rectangle panel w h |> move cx cy; centred ~size:14. dim cx cy (not_yet s); frame cyan w h 2. |> move cx cy ]
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
  @ [ frame cyan w h 2. |> move cx cy; text ~size:12. cyan x (y -. h -. 16.) ("its code" ^ size ^ ": a click, or s, to read it   o: the glass (" ^ Code_map.glass_name () ^ ")") ]
  (* claude: the mouse over it: a magnifying glass, the code under it
   * readable (Code_map.glass), over everything else *)
  @ match code_of p with Some c -> Code_map.glass computer c | None -> []

(* the chosen one, large, and what the catalogue says of it *)
let details (computer : computer) (host : host) (m : model) : shape list =
  match chosen m with
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
            (Printf.sprintf "%d   %s   %s   %s" p.year p.platform (players_text p) (Code_map.lines_text (lines_of p)));
        ]
      (* claude: why "start here": the smallest of its section, and what
       * is counted (not the library: a program over libs/ can look
       * smaller than it is) *)
      @ (match (start_here m, current_group m) with
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

let footer (host : host) (m : model) : shape list =
  let playing = match host.running () with Some name -> [ text ~size:18. yellow left_edge (-440.) ("> " ^ name ^ " is running") ] | None -> [] in
  [ text ~size:13. dim left_edge (-475.) "arrows move   tab section   g/a games/apps   b group   p e m l filter   / search   s read its code   enter play it" ]
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
  match Option.bind (chosen m) (preview_shapes host) with
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
      @ (match chosen m with
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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run ?(network : < Cap.network ; .. > option) (host : host) : unit =
  let network = (network :> < Cap.network > option) in
  let flags = Playground_platform.flags () in
  Playground_platform.run_app ~screen:(screen_w, screen_h) ~flags ?network (game (view host) (update host) (initial flags))
