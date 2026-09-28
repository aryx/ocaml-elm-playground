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
type scope = Own | Uses | Whole | Directory of string

type t = {
  program : string;
  path : string;
  sources : (string * string) list;
  scope : scope;
  area : float * float * int * int;
  map : Code_map.t;
  file : Code_view.t option; (* a file open over the map *)
  tour : (int * int) option; (* claude: the tour's stop: a file (its place in the map's entries), a stop in it *)
  own : string -> bool; (* claude: its own code's files *)
}

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

let map_of ~(roots : string list) ~(colours : (string * (int * int * int)) list) ~(own : string -> bool) ~(area : float * float * int * int) ~(sources : (string * string) list) ~(program : string) ~(path : string)
    ~(scope : scope) : Code_map.t =
  let paths =
    match scope with
    | Own -> Code_deps.closure ~keep:own sources path
    | Uses -> Code_deps.closure sources path
    | Whole | Directory _ -> List.map fst sources
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
    | Directory name -> Printf.sprintf "%s: %s" name files
  in
  (* claude: numbered in their reading order (Code_deps.closure's), but
   * the whole repository's and a directory's *)
  let numbered = match scope with Own | Uses -> true | Whole | Directory _ -> false in
  Code_map.make ~numbered ~colours ~roots ~area ~title ~marked:[ path ] entries

let make_own ~(own : string -> bool) ~(area : float * float * int * int) ~(sources : (string * string) list) ~(program : string) ~(path : string) : t =
  { program; path; sources; scope = Own; area; map = map_of ~roots:[] ~colours:[] ~own ~area ~sources ~program ~path ~scope:Own; file = None; tour = None; own }

let make ~area ~sources ~program ~path : t = make_own ~own:(Code_deps.own path) ~area ~sources ~program ~path

let of_directory ?(colours = []) ?(roots = []) ~(area : float * float * int * int) ~(name : string) ~(sources : (string * string) list) () : t =
  let scope = Directory name in
  let own _ = true in
  { program = name; path = ""; sources; scope; area; map = map_of ~roots ~colours ~own ~area ~sources ~program:name ~path:"" ~scope; file = None; tour = None; own }

let preview ~(area : float * float * int * int) ~(sources : (string * string) list) ~(program : string) ~(path : string) : Code_map.t =
  map_of ~roots:[] ~colours:[] ~own:(Code_deps.own path) ~area ~sources ~program ~path ~scope:Own

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
  if pressed "n" then Some (go t (next_stop t))
  else if pressed "p" && t.tour <> None then Some (go t (prev_stop t))
  else
  match t.file with
  | Some v ->
      if pressed "Escape" || pressed "Backspace" then Some { t with file = None }
      else Some { t with file = Some (Code_view.update computer ~pressed ~arrow v) }
  | None ->
      if pressed "w" && (match t.scope with Directory _ -> false | _ -> true) then
        let scope = match t.scope with Own -> Uses | Uses -> Whole | Whole | Directory _ -> Own in
        Some { t with scope; map = map_of ~roots:[] ~colours:[] ~own:t.own ~area:t.area ~sources:t.sources ~program:t.program ~path:t.path ~scope; tour = None }
      else (
        match Code_map.update computer ~pressed ~arrow t.map with
        | _, Close -> None
        | map, Stay -> Some { t with map }
        (* a file opened by hand: the tour, if any, left *)
        | map, Open (f, line) -> Some { t with map; file = Some (Code_view.make ~line f); tour = None })

let file_open (t : t) : bool = t.file <> None
let program (t : t) : string = t.program

let view (computer : Playground.computer) (t : t) : Playground.shape list =
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

let run_directory ?colours ?roots ~(name : string) ~(sources : (string * string) list) () : unit =
  let update (computer : Playground.computer) (m : alone) : alone =
    let code = match m.code with Some c -> c | None -> of_directory ?colours ?roots ~area:(area_of computer.screen) ~name ~sources () in
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
  let view (computer : Playground.computer) (m : alone) = match m.code with Some c -> view computer c | None -> [] in
  Playground_platform.run_app ~screen:(1778, 1000) ~flags:(Playground_platform.flags ())
    (Playground.game view update { code = None; before = Set_.empty; repeat = None })
