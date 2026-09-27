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

type t = {
  program : string;
  path : string;
  sources : (string * string) list;
  whole : bool; (* the whole repository, not the program's files *)
  map : Code_map.t;
  file : Code_view.t option; (* a file open over the map *)
}

(*****************************************************************************)
(* Which files *)
(*****************************************************************************)

let module_name (path : string) : string = String.capitalize_ascii (Filename.remove_extension (Filename.basename path))

let count_lines (s : string) : int =
  let n = ref 1 in
  String.iter (fun c -> if c = '\n' then incr n) s;
  !n

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
  { path; nlines = count_lines src; file }

(* the program's file and the modules it uses, transitively *)
let closure (sources : (string * string) list) (path : string) : string list =
  let by_name : (string, string list) Hashtbl.t = Hashtbl.create 1024 in
  (* claude: not the platforms: every program runs on one, and through
   * it (the native one's downloads) reaches TLS and its cryptography *)
  let platform p = String.length p > 20 && String.sub p 0 20 = "playground/platforms" in
  List.iter
    (fun (p, _) ->
      if Filename.check_suffix p ".ml" && not (platform p) then
        let m = module_name p in
        Hashtbl.replace by_name m (p :: Option.value ~default:[] (Hashtbl.find_opt by_name m)))
    sources;
  let seen = Hashtbl.create 256 in
  let rec visit = function
    | [] -> ()
    | p :: rest when Hashtbl.mem seen p -> visit rest
    | p :: rest ->
        Hashtbl.replace seen p ();
        let used =
          match List.assoc_opt p sources with
          | None -> []
          | Some src ->
              Code_file.modules_used src
              |> List.filter_map (fun m -> match Hashtbl.find_opt by_name m with Some [ q ] -> Some q | _ -> None)
        in
        visit (used @ rest)
  in
  visit [ path ];
  (* each .ml with its .mli *)
  List.filter (fun (p, _) -> Hashtbl.mem seen p || (Filename.check_suffix p ".mli" && Hashtbl.mem seen (Filename.remove_extension p ^ ".ml"))) sources
  |> List.map fst

let map_of ~(sources : (string * string) list) ~(program : string) ~(path : string) ~(whole : bool) : Code_map.t =
  let paths = if whole then List.map fst sources else closure sources path in
  let entries = List.filter_map (fun p -> Option.map (entry p) (List.assoc_opt p sources)) paths in
  let title =
    if whole then Printf.sprintf "the whole repository: %d files" (List.length entries)
    else Printf.sprintf "%s: %d files, its own and what it uses" program (List.length entries)
  in
  Code_map.make ~title ~marked:[ path ] entries

let make ~(sources : (string * string) list) ~(program : string) ~(path : string) : t =
  { program; path; sources; whole = false; map = map_of ~sources ~program ~path ~whole:false; file = None }

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
        let whole = not t.whole in
        Some { t with whole; map = map_of ~sources:t.sources ~program:t.program ~path:t.path ~whole }
      else (
        match Code_map.update computer ~pressed ~arrow t.map with
        | _, Close -> None
        | map, Stay -> Some { t with map }
        | map, Open (f, line) -> Some { t with map; file = Some (Code_view.make ~line f) })

let view (computer : Playground.computer) (t : t) : Playground.shape list =
  match t.file with Some v -> Code_view.view computer v | None -> Code_map.view computer t.map
