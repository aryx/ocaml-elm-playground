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

(* which files: the program's own code, then with all it uses, then all *)
type scope = Own | Uses | Whole

type t = {
  program : string;
  path : string;
  sources : (string * string) list;
  scope : scope;
  area : float * float * int * int;
  map : Code_map.t;
  file : Code_view.t option; (* a file open over the map *)
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

let map_of ~(area : float * float * int * int) ~(sources : (string * string) list) ~(program : string) ~(path : string) ~(scope : scope) :
    Code_map.t =
  let paths =
    match scope with
    | Own -> Code_deps.closure ~keep:(Code_deps.own path) sources path
    | Uses -> Code_deps.closure sources path
    | Whole -> List.map fst sources
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
  in
  Code_map.make ~area ~title ~marked:[ path ] entries

let make ~(area : float * float * int * int) ~(sources : (string * string) list) ~(program : string) ~(path : string) : t =
  { program; path; sources; scope = Own; area; map = map_of ~area ~sources ~program ~path ~scope:Own; file = None }

let preview ~(area : float * float * int * int) ~(sources : (string * string) list) ~(program : string) ~(path : string) : Code_map.t =
  map_of ~area ~sources ~program ~path ~scope:Own

(*****************************************************************************)
(* Update and view *)
(*****************************************************************************)

let update (computer : Playground.computer) ~(pressed : string -> bool) ~(arrow : string option) (t : t) : t option =
  match t.file with
  | Some v ->
      if pressed "Escape" || pressed "Backspace" then Some { t with file = None }
      else Some { t with file = Some (Code_view.update computer ~pressed ~arrow v) }
  | None ->
      if pressed "w" then
        let scope = match t.scope with Own -> Uses | Uses -> Whole | Whole -> Own in
        Some { t with scope; map = map_of ~area:t.area ~sources:t.sources ~program:t.program ~path:t.path ~scope }
      else (
        match Code_map.update computer ~pressed ~arrow t.map with
        | _, Close -> None
        | map, Stay -> Some { t with map }
        | map, Open (f, line) -> Some { t with map; file = Some (Code_view.make ~line f) })

let file_open (t : t) : bool = t.file <> None

let view (computer : Playground.computer) (t : t) : Playground.shape list =
  match t.file with
  | Some v -> Code_view.view computer v
  (* claude: over the map, the magnifying glass *)
  | None -> Code_map.view computer t.map @ Code_map.glass computer t.map
