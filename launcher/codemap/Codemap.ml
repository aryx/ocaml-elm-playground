(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Codemap.mli *)

(* which files: the program's own code, then with all it uses, then all;
 * or a directory's, read from the disk (tinybox codemap <dir>), no
 * program in it to start from *)
type scope =
  | Own
  | Uses
  | Whole
  | Directory of string
  (* claude: directories and files seen together (a search's name//, or
   * all it found), what to call them, and the map they were chosen
   * from, Escape's way back *)
  | Selection of string * string list * t
  (* claude: a unit and the units tied to it, its users and what it uses,
   * shown as [tmode] says (d cycling: both, its users, what it uses, it
   * alone), and the map it came from *)
  | Tied of { unit : string; users : string list; uses : string list; tmode : int; tbefore : t }

and t = {
  program : string;
  path : string;
  sources : (string * string) list;
  scope : scope;
  area : float * float * int * int;
  map : Code_map.t;
  file : Code_view.t option; (* a file open over the map *)
  graph : Map_graph.t option; (* claude: codegraph's matrix over the map (ctrl+click, g) *)
  tour : (int * int) option; (* claude: the tour's stop: a file (its place in the map's entries), a stop in it *)
  own : string -> bool; (* claude: its own code's files *)
  guide : Code_guide.t option; (* claude: what the configs among its sources say *)
}

(* claude: the code map's configs among the sources (tinybox embeds them
 * there, Code_deps.repository_configs), set apart: the code, and what
 * the configs say (Code_guide), a config's mistake left out *)
let is_config (path : string) = let f = Filename.basename path in f = ".codemapconfig" || Filename.check_suffix f ".libsonnet"

let guide_of (sources : (string * string) list) : (string * string) list * Code_guide.t option =
  let configs, code = List.partition (fun (p, _) -> is_config p) sources in
  if configs = [] then (code, None)
  else
    let paths = List.filter_map (fun (p, _) -> if Filename.basename p = ".codemapconfig" then Some p else None) configs in
    (code, Some (fst (Code_guide.load ~read:(fun p -> List.assoc_opt p configs) paths)))

(*****************************************************************************)
(* Which files *)
(*****************************************************************************)

(* claude: the lexed files, kept: a map shown again, or another program's
 * sharing the Playground's, costs nothing *)
let lexed : (string, Code_file.t Lazy.t) Hashtbl.t = Hashtbl.create 256

let entry (path : string) (src : string) : Code_map.entry =
  let file =
    match Hashtbl.find_opt lexed path with
    | Some f -> f
    | None ->
        let f = lazy (Code_file.make path src) in
        Hashtbl.replace lexed path f;
        f
  in
  { path; nlines = Code_deps.count_lines src; file }

(* claude: the sources' fan-in (Code_deps.fan_in), counted once for a
 * set of sources, when first asked (the capitals ranked by it) *)
let fan_ins : ((string * string) list * (string, int) Hashtbl.t Lazy.t) list ref = ref []

let fan_in_of (sources : (string * string) list) : (string, int) Hashtbl.t Lazy.t =
  match List.find_opt (fun (s, _) -> s == sources) !fan_ins with
  | Some (_, f) -> f
  | None ->
      let f = lazy (Code_deps.fan_in sources) in
      fan_ins := (sources, f) :: !fan_ins;
      f

let map_of ~(style : Code_map_base.style option) ~(guide : Code_guide.t option) ~(roots : string list) ~(colours : (string * (int * int * int)) list) ~(own : string -> bool) ~(area : float * float * int * int) ~(sources : (string * string) list) ~(program : string) ~(path : string)
    ~(scope : scope) : Code_map.t =
  let paths =
    match scope with
    | Own -> Code_deps.closure ~keep:own sources path
    | Uses -> Code_deps.closure sources path
    | Whole | Directory _ -> List.map fst sources
    | Selection (_, set, _) -> List.filter (fun p -> List.exists (fun d -> p = d || Code_search.starts p (d ^ "/")) set) (List.map fst sources)
    | Tied r ->
        let set = r.unit :: (match r.tmode with 0 -> r.users @ r.uses | 1 -> r.users | 2 -> r.uses | _ -> []) in
        List.filter (fun p -> List.exists (fun d -> p = d || Code_search.starts p (d ^ "/")) set) (List.map fst sources)
  in
  let entries = List.filter_map (fun p -> Option.map (entry p) (List.assoc_opt p sources)) paths in
  let n = List.length entries in
  (* claude: and their lines, all of them, not only the program's file's *)
  let files = Printf.sprintf "%s, %s" (if n = 1 then "1 file" else Printf.sprintf "%d files" n) (Code_map.lines_text (Code_map.lines_of entries)) in
  let title =
    match scope with
    | Own -> Printf.sprintf "%s: its code, %s   (w: with what it uses)" program files
    | Uses -> Printf.sprintf "%s and what it uses: %s   (w: the whole repository)" program files
    | Whole -> Printf.sprintf "the whole repository: %s   (w: %s's code)" files program
    | Directory name -> (
        (* claude: the project in a sentence, its root config's *)
        (* or, for a directory inside a project, its summary *)
        let said = match guide with Some g -> ( match Code_guide.title g with Some s -> Some s | None -> Code_guide.dir_summary g "") | None -> None in
        match said with Some s -> Printf.sprintf "%s: %s   (%s)" name s files | None -> Printf.sprintf "%s: %s" name files)
    | Selection (what, _, _) -> Printf.sprintf "%s   (%s; esc back)" what files
    | Tied r ->
        let shown = match r.tmode with 0 -> "its users and what it uses" | 1 -> "its users: " ^ String.concat ", " r.users | 2 -> "what it uses: " ^ String.concat ", " r.uses | _ -> "alone" in
        Printf.sprintf "%s, %s   (%s; d: next, esc back)" r.unit shown files
  in
  (* claude: numbered in their reading order (Code_deps.closure's), but
   * the whole repository's and a directory's *)
  let numbered = match scope with Own | Uses -> true | Whole | Directory _ | Selection _ | Tied _ -> false in
  (* claude: a program's map resolves its names against every source, the
   * ones it does not draw too (a click on game peeks at Playground's) *)
  let beyond =
    match scope with
    | Own | Uses | Selection _ | Tied _ -> List.filter_map (fun (p, src) -> if List.mem p paths then None else Some (entry p src)) sources
    | Whole | Directory _ -> []
  in
  let top_kept = match scope with Selection _ | Tied _ -> true | _ -> false in
  Code_map.make ~fan_in:(fan_in_of sources) ~top_kept ~numbered ~colours ~roots ?guide ~beyond ?style ~area ~title ~marked:[ path ] entries

let make_own ~(own : string -> bool) ~(area : float * float * int * int) ~(sources : (string * string) list) ~(program : string) ~(path : string) : t =
  let sources, guide = guide_of sources in
  let colours = match guide with Some g -> Code_guide.colours g | None -> [] in
  let map = map_of ~style:None ~guide ~roots:[] ~colours ~own ~area ~sources ~program ~path ~scope:Own in
  (* claude: in v2, opened on the program's file, at the ground: its kits
   * a (the street) or the wheel away (plan_codemap_v2.md) *)
  let map = if Code_map.style_name () = "v2" then Code_map.focus_on map path else map in
  { program; path; sources; scope = Own; area; map; file = None; graph = None; tour = None; own; guide }

let make ~area ~sources ~program ~path : t = make_own ~own:(Code_deps.own path) ~area ~sources ~program ~path

let of_directory ?guide ?(colours = []) ?(roots = []) ~(area : float * float * int * int) ~(name : string) ~(sources : (string * string) list) () : t =
  let scope = Directory name in
  let own _ = true in
  { program = name; path = ""; sources; scope; area; map = map_of ~style:None ~guide ~roots ~colours ~own ~area ~sources ~program:name ~path:"" ~scope; file = None; graph = None; tour = None; own; guide }

let preview ~(area : float * float * int * int) ~(sources : (string * string) list) ~(program : string) ~(path : string) : Code_map.t =
  let sources, guide = guide_of sources in
  (* claude: the menu's glance, the classic picture: the game's code itself,
   * its definitions larger, its kits beside it (the author's choice) *)
  map_of ~style:(Some Map_classic.style) ~guide ~roots:[] ~colours:[] ~own:(Code_deps.own path) ~area ~sources ~program ~path ~scope:Own

(*****************************************************************************)
(* Update and view *)
(*****************************************************************************)

(* claude: the tour: the code read in order, file by file in the map's
 * order (the reading order), each file's header, its sections and its
 * tricks (Code_map.stops), each opened in the file view with its line
 * lit near the top. n the next stop, from the map or a file; p the one
 * before; Escape the map, where n goes on. A file is lexed when the tour
 * reaches it. *)
let entry_at (t : t) (k : int) : Code_map.entry = List.nth (Code_map.entries t.map) k

let next_stop (t : t) : (int * int) option =
  let n = List.length (Code_map.entries t.map) in
  match t.tour with
  | None -> if n > 0 then Some (0, 0) else None
  | Some (k, s) ->
      if s + 1 < List.length (Code_map.stops (entry_at t k)) then Some (k, s + 1) else if k + 1 < n then Some (k + 1, 0) else t.tour

let prev_stop (t : t) : (int * int) option =
  match t.tour with
  | None -> None
  | Some (k, s) ->
      if s > 0 then Some (k, s - 1) else if k > 0 then Some (k - 1, List.length (Code_map.stops (entry_at t (k - 1))) - 1) else t.tour

let go (t : t) (tour : (int * int) option) : t =
  match tour with
  | None -> t
  | Some (k, s) ->
      let e = entry_at t k in
      let line = match List.nth_opt (Code_map.stops e) s with Some (l, _) -> l | None -> 0 in
      { t with tour; file = Some (Code_view.make ~line ~lit:line (Lazy.force e.file)) }

let update (computer : Playground.computer) ~(pressed : string -> bool) ~(arrow : string option) (t : t) : t option =
  let (Time now) = computer.time in
  if pressed "n" && not (Code_map.searching t.map) then Some (go t (next_stop t))
  else if pressed "p" && t.tour <> None && not (Code_map.searching t.map) then Some (go t (prev_stop t))
  else
  match (t.graph, t.file) with
  | Some g, _ -> (
      match Map_graph.update computer ~pressed g with
      | g, Map_graph.Stay -> Some { t with graph = Some g }
      | _, Back -> Some { t with graph = None }
      | _, Go (p, line) -> Some { t with graph = None; map = Code_map.go_back_to t.map p line })
  | None, Some v ->
      if pressed "Escape" || pressed "Backspace" then Some { t with file = None }
      else Some { t with file = Some (Code_view.update computer ~pressed ~arrow v) }
  | None, None ->
      (* claude: d, the tied view's next mode *)
      match t.scope with
      | Tied r when pressed "d" && not (Code_map.searching t.map) ->
          let rec next m = let m = (m + 1) mod 4 in if (m = 1 && r.users = []) || (m = 2 && r.uses = []) then next m else m in
          let scope = Tied { r with tmode = next r.tmode } in
          let map = map_of ~style:None ~guide:t.guide ~roots:[] ~colours:(match t.guide with Some g -> Code_guide.colours g | None -> []) ~own:t.own ~area:t.area ~sources:t.sources ~program:t.program ~path:t.path ~scope in
          Code_map.morph_from ~old:t.map map ~now;
          Some { t with scope; map }
      | _ ->
      if pressed "w" && (not (Code_map.searching t.map)) && (match t.scope with Directory _ | Selection _ | Tied _ -> false | _ -> true) then
        let scope = match t.scope with Own -> Uses | Uses -> Whole | Whole | Directory _ | Selection _ | Tied _ -> Own in
        Some { t with scope; map = map_of ~style:None ~guide:t.guide ~roots:[] ~colours:[] ~own:t.own ~area:t.area ~sources:t.sources ~program:t.program ~path:t.path ~scope; tour = None }
      else (
        match Code_map.update computer ~pressed ~arrow t.map with
        (* claude: from a selection, back to the map it was chosen from *)
        | _, Close -> ( match t.scope with Selection (_, _, before) -> Some before | Tied r -> Some r.tbefore | _ -> None)
        (* claude: up from a folder laid out alone: the map it came from,
         * on the folder's parent (not where one was before flying in,
         * which may be deeper) *)
        | map, Up -> (
            match t.scope with
            | Selection (_, [ p ], before) ->
                let parent = match Filename.dirname p with "." -> "" | d -> d in
                let rec up q = if q = "" then Code_map.focus_on before.map "" else if Code_map.has before.map q then Code_map.focus_on before.map q else up (match Filename.dirname q with "." -> "" | d -> d) in
                let back = up parent in
                Code_map.morph_from ~old:map back ~now;
                Some { before with map = back }
            | Selection (_, _, before) -> Some before
            | Tied r -> Some r.tbefore
            | _ -> Some { t with map })
        | map, Graph (unit, units) ->
            let title =
              match units with
              | [ one ] -> Printf.sprintf "inside %s: its parts' dependencies, codegraph's matrix" one
              | _ when unit = "" -> "the whole: its parts' dependencies, codegraph's matrix"
              | _ -> Printf.sprintf "%s and what it is tied to, codegraph's matrix" unit
            in
            let title = if Code_map.street_on map && List.length units > 1 then Printf.sprintf "%s and its street, codegraph's matrix" unit else title in
            Some { t with map; graph = Some (Map_graph.make ~expand:unit map ~title units) }
        | map, Tied (unit, users, uses) ->
            let scope = Tied { unit; users; uses; tmode = 0; tbefore = { t with map } } in
            let next = map_of ~style:None ~guide:t.guide ~roots:[] ~colours:(match t.guide with Some g -> Code_guide.colours g | None -> []) ~own:t.own ~area:t.area ~sources:t.sources ~program:t.program ~path:t.path ~scope in
            Code_map.morph_from ~old:map next ~now;
            Some { t with scope; map = next; tour = None }
        | map, Stay -> Some { t with map }
        | map, Select (what, set) ->
            let scope = Selection (what, set, { t with map }) in
            let next = map_of ~style:None ~guide:t.guide ~roots:[] ~colours:(match t.guide with Some g -> Code_guide.colours g | None -> []) ~own:t.own ~area:t.area ~sources:t.sources ~program:t.program ~path:t.path ~scope in
            (* claude: its rectangles moving from where they were *)
            Code_map.morph_from ~old:map next ~now;
            Some { t with scope; map = next; tour = None }
        (* a file opened by hand: the tour, if any, left *)
        | map, Open (f, line) -> Some { t with map; file = Some (Code_view.make ~line f); tour = None })

let file_open (t : t) : bool = t.file <> None
let program (t : t) : string = t.program

let view (computer : Playground.computer) (t : t) : Playground.shape list =
  match t.graph with
  | Some g -> Map_graph.view computer g
  | None ->
  match t.file with
  | Some v ->
      Code_view.view computer v
      (* claude: on the tour, where it is *)
      @ (match t.tour with
        | Some (k, s) ->
            let e = entry_at t k in
            let stops = Code_map.stops e in
            let n = List.length (Code_map.entries t.map) in
            let name = match Code_map.number t.map e.path with Some i -> Printf.sprintf "%d %s" i (Filename.basename e.path) | None -> e.path in
            let what = match List.nth_opt stops s with Some (_, w) -> w | None -> "" in
            [
              Playground.words (Playground.rgb 90 210 120)
                (Printf.sprintf "tour: %s (file %d of %d), %s (%d/%d)   n next  p back  esc the map" name (k + 1) n what (s + 1)
                   (List.length stops))
              |> Playground.scale (14. /. Playground.words_font_size)
              |> Playground.move 0. (-448.);
            ]
        | None -> [])
  (* claude: over the map, the magnifying glass *)
  | None -> Code_map.view computer t.map @ Code_map.glass computer t.map

(*****************************************************************************)
(* Alone *)
(*****************************************************************************)

(* claude: a directory's map as a program of its own (tinybox codemap
 * <dir>), on the menu's screen (16:9), the keys given as the menu gives
 * them: pressed this frame, an arrow held repeating every 0.08 s after
 * 0.4 s. Made at the first frame, when the screen is known; Escape on
 * the map leaves it as it is, the window closing the program. *)
type alone = { code : t option; before : string Set_.t; repeat : (string * float) option }

let area_of (screen : Playground.screen) = (screen.left +. 20., screen.top -. 92., int_of_float screen.width - 40, int_of_float screen.height - 162)

(* claude: the map opened where the command line or the page's URL says
 * (a link to a part of the code, Codemap_web):
 *   focus=<path>   a folder, or a file (flown to)
 *   line=<n>       with a file, its definition there peeked at (from 1)
 *   def=<name>     a top-level definition so named, the first found
 *                  (under focus if given), peeked at
 *   code=<Program> a program's own code, as tinybox's menu shows it
 *                  (tinybox.html?code=), w widening it *)
let opened_at (c : t) (flags : (string * string) list) : t =
  (* claude: code=<Program>, its own code (as tinybox.html?code=: its
   * file and the kits' and languages' modules it names, Code_deps.own),
   * w widening it; the configs kept (the author: the README's links to
   * a program's "419 lines in 3 files") *)
  let c =
    match List.assoc_opt "code" flags with
    | None -> c
    | Some name -> (
        let file = name ^ ".ml" in
        (* the shallowest so named: apps/devtools/TinyTurboPascal.ml, not
         * its terminal twin in tty/ *)
        let depth p = List.length (String.split_on_char '/' p) in
        match List.sort (fun (a, _) (b, _) -> compare (depth a) (depth b)) (List.filter (fun (p, _) -> Filename.basename p = file) c.sources) with
        | [] -> c
        | (path, _) :: _ ->
            let own = Code_deps.own path in
            let colours = match c.guide with Some g -> Code_guide.colours g | None -> [] in
            let map = map_of ~style:None ~guide:c.guide ~roots:[] ~colours ~own ~area:c.area ~sources:c.sources ~program:name ~path ~scope:Own in
            let map = if Code_map.style_name () = "v2" then Code_map.focus_on map path else map in
            { c with program = name; path; scope = Own; own; map })
  in
  let focus = List.assoc_opt "focus" flags in
  let is_file p = List.exists (fun (e : Code_map.entry) -> e.path = p) (Code_map.entries c.map) in
  let c = match focus with Some p when not (is_file p) -> { c with map = Code_map.focus_on c.map p } | _ -> c in
  let def_named name =
    let under p = match focus with None -> true | Some d -> p = d || String.starts_with ~prefix:(d ^ "/") p in
    List.find_map
      (fun (e : Code_map.entry) ->
        if not (under e.path) then None
        (* claude: a section's title (* diff *) is among the defs, for
         * the map's labels: not what def=diff means *)
        else List.find_map (fun (l, n, cat) -> if n = name && cat <> Highlight_code.Comment_section then Some (e.path, l) else None) (Lazy.force e.file).defs)
      (Code_map.entries c.map)
  in
  let line = Option.bind (List.assoc_opt "line" flags) int_of_string_opt in
  match (List.assoc_opt "def" flags, focus, line) with
  | Some name, _, _ -> ( match def_named name with Some (p, l) -> { c with map = Code_map.go_back_to c.map p (Some l) } | None -> c)
  | None, Some p, Some n when is_file p -> { c with map = Code_map.go_back_to c.map p (Some (n - 1)) }
  | None, Some p, None when is_file p -> { c with map = Code_map.go_back_to c.map p None }
  | _ -> c

type directory = { guide : Code_guide.t option; colours : (string * (int * int * int)) list option; roots : string list option; name : string; sources : (string * string) list }

(* claude: the map, its directory given by [get] once it has it (a web
 * page's, fetched; Error, why not, said on the screen) *)
let run_loading ~(get : unit -> (directory, string) result option) : unit =
  let status = ref "its code: on its way..." in
  let update (computer : Playground.computer) (m : alone) : alone =
    let made =
      match m.code with
      | Some c -> Some c
      | None -> (
          match get () with
          | None -> None
          | Some (Error why) ->
              status := "its code: " ^ why;
              None
          | Some (Ok d) ->
          let c = of_directory ?guide:d.guide ?colours:d.colours ?roots:d.roots ~area:(area_of computer.screen) ~name:d.name ~sources:d.sources () in
          Some (opened_at c (Playground_platform.flags ())))
    in
    match made with
    | None -> m
    | Some code ->
    let keys = computer.keyboard.keys in
    let pressed k = Set_.mem k keys && not (Set_.mem k m.before) in
    let (Time now) = computer.time in
    let arrow, repeat =
      match List.find_opt pressed [ "ArrowLeft"; "ArrowRight"; "ArrowUp"; "ArrowDown" ] with
      | Some k -> (Some k, Some (k, now +. 0.4))
      | None -> (
          match m.repeat with
          | Some (k, next) when Set_.mem k keys -> if now >= next then (Some k, Some (k, now +. 0.08)) else (None, m.repeat)
          | _ -> (None, None))
    in
    { code = Some (Option.value (update computer ~pressed ~arrow code) ~default:code); before = keys; repeat }
  in
  let view (computer : Playground.computer) (m : alone) =
    match m.code with
    | Some c -> view computer c
    | None -> [ Playground.words (Playground.rgb 200 200 200) !status |> Playground.scale (20. /. Playground.words_font_size) ]
  in
  let flags = Playground_platform.flags () in
  (* claude: style=streets, the map's style (Code_map); a directory's
   * map is drawn by default in the new one, Map_v2 *)
  Code_map.choose_style (Option.value (List.assoc_opt "style" flags) ~default:"v2");
  Playground_platform.run_app ~screen:(1778, 1000) ~flags
    (Playground.game view update { code = None; before = Set_.empty; repeat = None })

let run_directory ?guide ?colours ?roots ~(name : string) ~(sources : (string * string) list) () : unit =
  let d = Some (Ok { guide; colours; roots; name; sources }) in
  run_loading ~get:(fun () -> d)
