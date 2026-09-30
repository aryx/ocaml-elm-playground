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
    (* claude: module B = Scratch_blocks, the alias then used as B.x *)
    | (a : Token_ml.t) :: (b : Token_ml.t) :: (c : Token_ml.t) :: (d : Token_ml.t) :: rest
      when a.text = "module" && b.kind = Uident && c.text = "=" && d.kind = Uident ->
        go (d.text :: acc) (d :: rest)
    | _ :: rest -> go acc rest
    | [] -> acc
  in
  List.sort_uniq compare (go [] (Lexer_ml.tokens src))

let module_name (path : string) : string = String.capitalize_ascii (Filename.remove_extension (Filename.basename path))

(* claude: how central each module is: the other files naming it (an
 * open, an include, a qualified name), by the module's file (its path
 * without extension: an .ml and its .mli one). Several files of one name
 * (~/ix's 26 CLI.ml, one per program): a use counts for the nearest, the
 * one sharing the most directory with the user's, as the resolver
 * (Code_names) would pick *)
let fan_in (sources : (string * string) list) : (string, int) Hashtbl.t =
  let h = Hashtbl.create 1024 in
  let by_name : (string, string list) Hashtbl.t = Hashtbl.create 1024 in
  let paths = Hashtbl.create 4096 in
  List.iter (fun (p, _) -> Hashtbl.replace paths p ()) sources;
  let is_c p = Filename.check_suffix p ".c" || Filename.check_suffix p ".h" in
  (* claude: C's files too (~/ix's boot code, ~/principia): a header and
   * its .c one module, named by their base name ("lib.h", "lib.c" -> lib) *)
  List.iter
    (fun (p, _) ->
      if Filename.check_suffix p ".ml" || Filename.check_suffix p ".mli" || is_c p then
        let m = if is_c p then Filename.remove_extension (Filename.basename p) else module_name p and k = Filename.remove_extension p in
        let l = Option.value (Hashtbl.find_opt by_name m) ~default:[] in
        if not (List.mem k l) then Hashtbl.replace by_name m (k :: l))
    sources;
  (* claude: the modules that have a header: an #include names one of
   * them, never a stranger's .c of the same base name (~/principia's
   * <draw.h> counted for lib_gui's draw.c, 172 files) *)
  let headers = Hashtbl.create 256 in
  List.iter (fun (p, _) -> if Filename.check_suffix p ".h" then Hashtbl.replace headers (Filename.remove_extension p) ()) sources;
  let shared a b =
    let a = String.split_on_char '/' (Filename.dirname a) and b = String.split_on_char '/' (Filename.dirname b) in
    let rec go n = function x :: r, y :: r' when x = y -> go (n + 1) (r, r') | _ -> n in
    go 0 (a, b)
  in
  (* claude: as near, the one sharing more directory names anywhere: an
   * x86 file's "dat.h" is 386/'s, not arm/'s *)
  let common a b =
    let b = String.split_on_char '/' (Filename.dirname b) in
    List.length (List.filter (fun x -> List.mem x b) (String.split_on_char '/' (Filename.dirname a)))
  in
  (* a C file's headers, #include "x.h" and <x.h>, by base name *)
  let includes (src : string) : string list =
    String.split_on_char '\n' src
    |> List.filter_map (fun l ->
           let l = String.trim l in
           if String.length l > 9 && String.sub l 0 8 = "#include" then
             let rest = String.trim (String.sub l 8 (String.length l - 8)) in
             if String.length rest > 2 && (rest.[0] = '"' || rest.[0] = '<') then
               let close = if rest.[0] = '"' then '"' else '>' in
               match String.index_from_opt rest 1 close with
               | Some j -> Some (Filename.remove_extension (Filename.basename (String.sub rest 1 (j - 1))))
               | None -> None
             else None
           else None)
  in
  List.iter
    (fun (p, src) ->
      if Filename.check_suffix p ".ml" || Filename.check_suffix p ".mli" || is_c p then
        let self = Filename.remove_extension p in
        (* an .ml and its .mli are one module, counted once: the .ml's (C:
         * each file counts, a header's includes too) *)
        if is_c p || Filename.check_suffix p ".ml" || not (Hashtbl.mem paths (self ^ ".ml")) then
          List.iter
            (fun m ->
              match Hashtbl.find_opt by_name m with
              | Some ks -> (
                  let ks = List.filter (fun k -> k <> self) ks in
                  let ks = if is_c p && List.exists (Hashtbl.mem headers) ks then List.filter (Hashtbl.mem headers) ks else ks in
                  (* the nearest; as near, the shallowest: a virtual
                   * module's interface above its implementations
                   * (Playground_platform.mli over each platform's .ml) *)
                  let depth k = List.length (String.split_on_char '/' k) in
                  let better k b =
                    shared k p > shared b p
                    || (shared k p = shared b p && (common k p > common b p || (common k p = common b p && depth k < depth b)))
                  in
                  let best = List.fold_left (fun acc k -> match acc with Some b when not (better k b) -> acc | _ -> Some k) None ks in
                  match best with Some k -> Hashtbl.replace h k (1 + Option.value (Hashtbl.find_opt h k) ~default:0) | None -> ())
              | None -> ())
            (if is_c p then includes src else modules_used src))
    sources;
  h

let fan (t : (string, int) Hashtbl.t) (path : string) : int = Option.value (Hashtbl.find_opt t (Filename.remove_extension path)) ~default:0

let count_lines (s : string) : int =
  let n = ref 1 in
  String.iter (fun c -> if c = '\n' then incr n) s;
  !n

let starts (prefix : string) (s : string) : bool = String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

(* the program's own code: its folder's, and the kits' (games' and
 * apps') and the languages' (each made for a program or a few), not the
 * Playground's nor libs/' (the from-scratch libraries, truly general):
 * its budget, 5,000 lines, counts this, tests/catalog *)
let own (program_path : string) (p : string) : bool =
  Filename.dirname p = Filename.dirname program_path || starts "gamekits/" p || starts "appkits/" p || starts "languages/" p
  (* claude: the kits that draw with the Playground, not in appkits/
   * (tiny_appkits) but in a subfolder of the apps they help:
   * apps/internet/browser/, apps/office/file_menu/, ... *)
  || (match String.split_on_char '/' p with "apps" :: _ :: _ :: _ :: _ -> true | _ -> false)

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

let repository_configs ~(root : string) : (string * string) list =
  let is_config f = f = ".codemapconfig" || Filename.check_suffix f ".libsonnet" in
  let here dir = Sys.readdir (Filename.concat root dir) |> Array.to_list |> List.sort compare in
  let rec walk (dir : string) : (string * string) list =
    here dir
    |> List.concat_map (fun f ->
           let path = Filename.concat dir f in
           if Sys.is_directory (Filename.concat root path) then if f.[0] = '.' || List.mem f skipped_dirs then [] else walk path
           else if is_config f then [ (path, read (Filename.concat root path)) ]
           else [])
  in
  List.filter_map (fun f -> if is_config f && not (Sys.is_directory (Filename.concat root f)) then Some (f, read (Filename.concat root f)) else None) (here ".")
  @ List.concat_map walk (List.filter (fun d -> Sys.file_exists (Filename.concat root d)) source_roots)

let budget = 5000

let own_size (sources : (string * string) list) (path : string) : int * int =
  let contents = Hashtbl.create 4096 in
  List.iter (fun (p, src) -> Hashtbl.replace contents p src) sources;
  let paths = closure ~keep:(own path) sources path in
  (List.length paths, List.fold_left (fun n p -> n + match Hashtbl.find_opt contents p with Some src -> count_lines src | None -> 0) 0 paths)
