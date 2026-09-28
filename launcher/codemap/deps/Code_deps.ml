(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_deps.mli *)

let modules_used (src : string) : string list =
  let rec go acc = function
    | (a : Token_ml.t) :: (b :: _ as rest) when a.kind = Uident && b.text = "." -> go (a.text :: acc) rest
    | (a : Token_ml.t) :: ((b : Token_ml.t) :: _ as rest) when (a.text = "open" || a.text = "include") && b.kind = Uident ->
        go (b.text :: acc) rest
    | _ :: rest -> go acc rest
    | [] -> acc
  in
  List.sort_uniq compare (go [] (Lexer_ml.tokens src))

let count_lines (s : string) : int =
  let n = ref 1 in
  String.iter (fun c -> if c = '\n' then incr n) s;
  !n

let module_name (path : string) : string = String.capitalize_ascii (Filename.remove_extension (Filename.basename path))

let starts (prefix : string) (s : string) : bool = String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

(* the program's own code: its folder's, and the kits' (games' and
 * apps') and the languages' (each made for a program or a few), not the
 * Playground's nor libs/' (the from-scratch libraries, truly general):
 * its budget, 5,000 lines, counts this, tests/catalog *)
let own (program_path : string) (p : string) : bool =
  Filename.dirname p = Filename.dirname program_path || starts "gamekits/" p || starts "appkits/" p || starts "languages/" p

(* claude: a module's implementation: its .ml, or the lexer or parser
 * its .ml is generated from (Lexer_ml.mll) *)
let source_suffixes = [ ".ml"; ".mll"; ".mly" ]
let is_impl (p : string) : bool = List.exists (Filename.check_suffix p) source_suffixes

let closure ?(keep = fun _ -> true) (sources : (string * string) list) (path : string) : string list =
  let by_name : (string, string list) Hashtbl.t = Hashtbl.create 1024 in
  let contents : (string, string) Hashtbl.t = Hashtbl.create 4096 in
  (* claude: not the platforms: every program runs on one, and through
   * it (the native one's downloads) reaches TLS and its cryptography *)
  let platform p = starts "playground/platforms" p in
  List.iter
    (fun (p, src) ->
      Hashtbl.replace contents p src;
      if is_impl p && not (platform p) then
        let m = module_name p in
        Hashtbl.replace by_name m (p :: Option.value ~default:[] (Hashtbl.find_opt by_name m)))
    sources;
  let seen = Hashtbl.create 256 in
  let order = ref [] in
  (* breadth first: the program, then what it names, then what those
   * name -- the order to read them in *)
  let rec visit = function
    | [] -> ()
    | p :: rest when Hashtbl.mem seen p -> visit rest
    | p :: rest ->
        Hashtbl.replace seen p ();
        order := p :: !order;
        let used =
          match Hashtbl.find_opt contents p with
          | None -> []
          | Some src ->
              modules_used src
              |> List.filter_map (fun m -> match Hashtbl.find_opt by_name m with Some [ q ] when keep q -> Some q | _ -> None)
        in
        visit (rest @ used)
  in
  visit [ path ];
  (* each .ml with its .mli, first *)
  List.rev !order
  |> List.concat_map (fun p ->
         let mli = Filename.remove_extension p ^ ".mli" in
         if is_impl p && Hashtbl.mem contents mli then [ mli; p ] else [ p ])

(* claude: the repository's own sources, as they are in _build: its
 * source files, and not the build's copies of them (a genre's web/ and
 * software/) nor the modules rules make (dune's alias modules, the
 * embedded pictures and pages, ocamllex's output), which say so on their
 * first line *)
let source_roots = [ "games"; "apps"; "gamekits"; "appkits"; "languages"; "playground"; "libs"; "launcher" ]
let skipped_dirs = [ "web"; "software"; "svg"; "tests" ]

let read (path : string) : string = In_channel.with_open_bin path In_channel.input_all

let generated (path : string) (text : string) : bool =
  let first = match String.index_opt text '\n' with Some i -> String.sub text 0 i | None -> text in
  starts "(* Auto-generated" first || starts "(* generated" first || starts "# " first
  || Filename.basename path = "Hud_render.ml"
  || Filename.basename path = "Hud_render.mli"

(* claude: tinybox's own code too, the code map showing itself: in
 * _build, launcher/native/ also holds the programs it copies in
 * (copy_files) and the modules it generates, among them the one being
 * written from this list; so of it only its own modules, Tinybox*, but
 * the generated ones *)
let launcher_own (f : string) : bool =
  starts "Tinybox" f && not (List.exists (fun p -> starts p f) [ "Tinybox_data"; "Tinybox_sources"; "Tinybox_thumbs" ])

let repository_sources ~(root : string) : (string * string) list =
  let rec walk (dir : string) : (string * string) list =
    Sys.readdir (Filename.concat root dir) |> Array.to_list |> List.sort compare
    |> List.concat_map (fun f ->
           let path = Filename.concat dir f in
           if Sys.is_directory (Filename.concat root path) then if f.[0] = '.' || List.mem f skipped_dirs then [] else walk path
           else if dir = "launcher/native" && not (launcher_own f) then []
           else if is_impl f || Filename.check_suffix f ".mli" then
             let text = read (Filename.concat root path) in
             if generated path text then [] else [ (path, text) ]
           else [])
  in
  List.concat_map walk (List.filter (fun d -> Sys.file_exists (Filename.concat root d)) source_roots)

let budget = 5000

let own_size (sources : (string * string) list) (path : string) : int * int =
  let contents = Hashtbl.create 4096 in
  List.iter (fun (p, src) -> Hashtbl.replace contents p src) sources;
  let paths = closure ~keep:(own path) sources path in
  (List.length paths, List.fold_left (fun n p -> n + match Hashtbl.find_opt contents p with Some src -> count_lines src | None -> 0) 0 paths)
