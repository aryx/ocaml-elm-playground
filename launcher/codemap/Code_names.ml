(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_names.mli *)

type candidate = { path : string; line : int; col : int; len : int; near : int * int; other_project : bool }

let has_ext exts p = List.exists (Filename.check_suffix p) exts
let is_ml = has_ext [ ".ml"; ".mli"; ".mll"; ".mly" ]
(* claude: assembly with C: one namespace, a.out's _ dropped (Highlight_asm) *)
let is_c = has_ext [ ".c"; ".h"; ".s"; ".S"; ".asm" ]
let module_of (p : string) = String.capitalize_ascii (Filename.remove_extension (Filename.basename p))

(* how many directories two paths share from the top *)
let shared (a : string) (b : string) : int =
  let rec go xs ys n = match (xs, ys) with x :: xs, y :: ys when x = y -> go xs ys (n + 1) | _ -> n in
  go (String.split_on_char '/' (Filename.dirname a)) (String.split_on_char '/' (Filename.dirname b)) 0

(* a file's definitions of a name (N.x in a nested module) in a namespace *)
let defined (f : Code_file.t) (name : string) (space : Highlight_code.space) : Highlight_code.definition list =
  List.filter (fun (d : Highlight_code.definition) -> d.dname = name && d.dspace = space) f.definitions

(* its length on the page: x's in N.x *)
let candidate path (d : Highlight_code.definition) near other_project =
  let shown = match String.rindex_opt d.dname '.' with Some i -> String.length d.dname - i - 1 | None -> String.length d.dname in
  { path; line = d.dline; col = d.dcol; len = shown; near; other_project }

(* claude: a path's project, the deepest root it is under ("" the map's
 * top, when no root holds it) *)
let project (roots : string list) (p : string) : string =
  List.fold_left
    (fun best r ->
      let under = r = "" || (String.length p > String.length r && String.sub p 0 (String.length r + 1) = r ^ "/") in
      if under && String.length r > String.length best then r else best)
    "" roots

(* claude: the map's files, found without scanning them all: OCaml's by
 * module name (Hashtbl.find_all: two Parser.ml), C's definitions by name
 * and namespace, made on the first C search (it lexes every C file) *)
type index = {
  ml : (string, string * Code_file.t Lazy.t) Hashtbl.t;
  c : (string * Highlight_code.space, string * Highlight_code.definition) Hashtbl.t Lazy.t;
  (* claude: the C files by path, and each one's headers, included
   * transitively, made when first asked *)
  cfiles : (string, Code_file.t Lazy.t) Hashtbl.t;
  closure : (string, string list) Hashtbl.t;
  (* claude: opti: the C files by base name, in cfiles' own order, and
   * the includes resolved, by the includer's directory
   * (resolve_include_opti) *)
  by_base : (string, string list) Hashtbl.t;
  resolved : (string * string, string option) Hashtbl.t;
  (* claude: the system's headers, by base name: included from many top
   * folders (~/principia's libc.h, from 30), so sharing one does not
   * make two files one program (troff "using" sam's linep) *)
  system : (string, unit) Hashtbl.t Lazy.t;
}

(* claude: a C file's #include lines, "x.h" and <x/y.h>, as written *)
let includes_of (f : Code_file.t) : string list =
  Array.to_list f.lines
  |> List.filter_map (fun spans ->
         let b = Buffer.create 80 in
         List.iter (fun (sp : Highlight_code.span) -> while Buffer.length b < sp.col do Buffer.add_char b ' ' done; Buffer.add_string b sp.text) spans;
         let l = String.trim (Buffer.contents b) in
         if String.length l > 9 && String.sub l 0 8 = "#include" then
           let r = String.trim (String.sub l 8 (String.length l - 8)) in
           if String.length r > 2 && (r.[0] = '"' || r.[0] = '<') then
             let close = if r.[0] = '"' then '"' else '>' in
             match String.index_from_opt r 1 close with Some j -> Some (String.sub r 1 (j - 1)) | None -> None
           else None
         else None)

(* claude: a top folder's name ("" at the top) *)
let top_of (p : string) : string = match String.index_opt p '/' with Some i -> String.sub p 0 i | None -> ""

(* claude: a header included from six top folders or more is the
 * system's (Linux 0.01's linux/sched.h, from four, is still a program's
 * own: kernel/, mm/ and fs/ are one kernel) *)
let system_headers (files : (string * Code_file.t Lazy.t) list) : (string, unit) Hashtbl.t =
  let tops : (string, string list) Hashtbl.t = Hashtbl.create 256 in
  List.iter
    (fun (p, lf) ->
      if is_c p then
        List.iter
          (fun inc ->
            let b = Filename.basename inc and t = top_of p in
            let l = Option.value (Hashtbl.find_opt tops b) ~default:[] in
            if not (List.mem t l) then Hashtbl.replace tops b (t :: l))
          (includes_of (Lazy.force lf)))
    files;
  let h = Hashtbl.create 64 in
  Hashtbl.iter (fun b l -> if List.length l >= 6 then Hashtbl.replace h b ()) tops;
  h

let index (files : (string * Code_file.t Lazy.t) list) : index =
  let ml = Hashtbl.create 256 in
  List.iter (fun (p, lf) -> if is_ml p then Hashtbl.add ml (module_of p) (p, lf)) files;
  let c =
    lazy
      (let h = Hashtbl.create 1024 in
       List.iter
         (fun (p, lf) ->
           if is_c p then
             let (f : Code_file.t) = Lazy.force lf in
             List.iter (fun (d : Highlight_code.definition) -> Hashtbl.add h (d.dname, d.dspace) (p, d)) f.definitions)
         files;
       h)
  in
  let cfiles = Hashtbl.create 256 in
  List.iter (fun (p, lf) -> if is_c p then Hashtbl.replace cfiles p lf) files;
  let by_base = Hashtbl.create 256 in
  Hashtbl.iter (fun p _ -> let b = Filename.basename p in Hashtbl.replace by_base b (p :: Option.value (Hashtbl.find_opt by_base b) ~default:[])) cfiles;
  Hashtbl.filter_map_inplace (fun _ l -> Some (List.rev l)) by_base;
  { ml; c; cfiles; closure = Hashtbl.create 256; by_base; resolved = Hashtbl.create 1024; system = lazy (system_headers files) }

(* an include resolved: the file of that path's tail, the nearest *)
let nearest_include (from : string) (inc : string) (candidates : string list) : string option =
  (* claude: only within the includer's own top folder, or an include/
   * directory (Linux's -Iinclude): never a stranger's header of the
   * same name (~/ix's draw9.c's <libc.h>, Plan 9's, found in tiny/) *)
  let top p = match String.index_opt p '/' with Some i -> String.sub p 0 i | None -> "" in
  let reachable p = top p = top from || List.mem "include" (String.split_on_char '/' p) in
  let ends p = reachable p && (p = inc || (String.length p > String.length inc && String.sub p (String.length p - String.length inc - 1) (String.length inc + 1) = "/" ^ inc)) in
  (* claude: as near, the one sharing more directory names anywhere (an
   * x86 file's "dat.h" is core/386/'s, not core/arm/'s) *)
  let common p = let b = String.split_on_char '/' (Filename.dirname from) in List.length (List.filter (fun x -> List.mem x b) (String.split_on_char '/' (Filename.dirname p))) in
  let key p = (shared p from, common p) in
  List.fold_left (fun acc p -> if ends p then (match acc with Some q when key q >= key p -> acc | _ -> Some p) else acc) None candidates

(* claude: among every C file, in cfiles' order *)
let resolve_include_simple (ix : index) (from : string) (inc : string) : string option =
  nearest_include from inc (List.rev (Hashtbl.fold (fun p _ acc -> p :: acc) ix.cfiles []))

(* claude: opti: among the C files of the include's base name only (in
 * cfiles' order: the ties fall as in the simple one), and each include
 * resolved once for its includer's directory, all its answer depends
 * on: the simple one went through every C file for every include of
 * every file, 9 s of principia's 2,200 files' uses counted (Code_rank)
 * and the map frozen *)
let resolve_include_opti (ix : index) (from : string) (inc : string) : string option =
  let k = (Filename.dirname from, inc) in
  match Hashtbl.find_opt ix.resolved k with
  | Some r -> r
  | None ->
      let r = nearest_include from inc (Option.value (Hashtbl.find_opt ix.by_base (Filename.basename inc)) ~default:[]) in
      Hashtbl.replace ix.resolved k r;
      r

let resolve_include (ix : index) (from : string) (inc : string) : string option =
  if !Opti.enabled then resolve_include_opti ix from inc else resolve_include_simple ix from inc

(* a C file's headers, transitively *)
let header_closure (ix : index) (p : string) : string list =
  match Hashtbl.find_opt ix.closure p with
  | Some l -> l
  | None ->
      let seen = Hashtbl.create 16 in
      let rec go q depth =
        if depth < 8 then
          match Hashtbl.find_opt ix.cfiles q with
          | Some lf ->
              List.iter
                (fun inc -> match resolve_include ix q inc with Some h when not (Hashtbl.mem seen h) -> Hashtbl.replace seen h (); go h (depth + 1) | _ -> ())
                (includes_of (Lazy.force lf))
          | None -> ()
      in
      go p 0;
      let l = Hashtbl.fold (fun h () acc -> h :: acc) seen [] in
      Hashtbl.replace ix.closure p l;
      l

(* sorted, and whether the first is alone at its rank *)
let ranked (cs : candidate list) : candidate list * bool =
  let cs = List.stable_sort (fun a b -> compare (a.near, a.path) (b.near, b.path)) cs in
  match cs with a :: b :: _ -> (cs, a.near < b.near) | [ _ ] -> (cs, true) | [] -> ([], false)

(*****************************************************************************)
(* OCaml *)
(*****************************************************************************)

(* claude: where to look, in order: a module's files and the name there.
 * M.x: x in M. M.N.x: N.x in M (a nested module), then x in N's own file
 * (a wrapped library's). A bare x: in the modules opened around it
 * (let open, M.(e)), innermost first, then the file's opens, the last
 * first *)
let tries (f : Code_file.t) (r : Highlight_code.reference) : (string * string) list =
  match r.rpath with
  | [] -> List.map (fun m -> (m, r.rname)) (r.ropens @ List.rev f.opens)
  | m :: rest -> (m, String.concat "." (rest @ [ r.rname ])) :: (match List.rev rest with n :: _ -> [ (n, r.rname) ] | [] -> [])

let find_ml ~other (ix : index) ~from (f : Code_file.t) (r : Highlight_code.reference) =
  (* claude: a module's files: when one is in the use's own directory
   * (its Lexer.mll beside it), only that one module -- a name not found
   * there is unresolved, not another Lexer's (the author, at ~/ix:
   * languages/ml's Lexer.token found in languages/c) *)
  let nearest_files m =
    let all = Hashtbl.find_all ix.ml m in
    let here = List.filter (fun (p, _) -> Filename.dirname p = Filename.dirname from) all in
    if here <> [] then here else all
  in
  (* the first place, in that order, that defines it *)
  let rec first = function
    | [] -> []
    | (m, name) :: rest -> (
        let cs =
          List.concat_map
            (fun (p, lf) ->
              if p <> from then
                (* its latest definition (ranked by path too: x.ml
                 * before x.mli) *)
                match List.rev (defined (Lazy.force lf) name r.rspace) with
                | d :: _ -> [ candidate p d ((if other p then 1 else 0), - shared p from) (other p) ]
                | [] -> []
              else [])
            (nearest_files m)
        in
        match cs with [] -> first rest | cs -> cs)
  in
  let cs, _ = ranked (first (tries f r)) in
  (* sure: no other directory as near (a module's .ml and .mli are one) *)
  let sure = match cs with a :: rest -> not (List.exists (fun c -> Filename.dirname c.path <> Filename.dirname a.path && c.near = a.near) rest) | [] -> false in
  (cs, sure)

(*****************************************************************************)
(* C *)
(*****************************************************************************)

(* a path with its ..s and .s gone: kernel/pc/../port/lib.h is
 * kernel/port/lib.h *)
let normalize (p : string) : string =
  let parts =
    List.fold_left
      (fun acc s -> match (s, acc) with ("." | ""), _ -> acc | "..", _ :: rest when List.hd acc <> ".." -> rest | _ -> s :: acc)
      [] (String.split_on_char '/' p)
  in
  String.concat "/" (List.rev parts)

let find_c ~other (ix : index) ~from (f : Code_file.t) (r : Highlight_code.reference) =
  let dir = Filename.dirname from in
  (* the program's own: its directory, and the headers it includes by a
   * path from it (#include "../port/lib.h") *)
  let included = List.map (fun i -> normalize (Filename.concat dir i)) f.includes in
  let own_header p = Filename.dirname p = dir || List.mem (normalize p) included in
  let library p = List.exists (fun d -> String.length d >= 3 && (String.sub d 0 3 = "lib" || d = "include")) (String.split_on_char '/' (Filename.dirname p)) in
  (* claude: C links only what can link: a definition in the use's own
   * top folder, or in a file sharing a header with the use's (one
   * program: Linux 0.01's fork.c declares copy_page_tables itself, and
   * shares linux/sched.h with mm/memory.c) (the author, at ~/ix: tiny/'s calls resolved into kernel/ and
   * languages/, programs never linked together) *)
  let top p = match String.index_opt p '/' with Some i -> String.sub p 0 i | None -> "" in
  let mine = lazy (header_closure ix from) in
  let declares h = match Hashtbl.find_opt ix.cfiles h with Some lf -> List.exists (fun (d : Highlight_code.definition) -> d.dname = r.rname) (Lazy.force lf).definitions | None -> false in
  (* a top-level library (lib/, libc/, lib_core/): what programs link *)
  let top_library p = let t = top p in String.length t >= 3 && String.sub t 0 3 = "lib" in
  let linkable p =
    top p = top from
    || top_library p
    || List.mem p (Lazy.force mine)
    || (let theirs = header_closure ix p in
        List.exists (fun h -> List.mem h theirs && not (Hashtbl.mem (Lazy.force ix.system) (Filename.basename h))) (Lazy.force mine))
  in
  ignore declares;
  let all = List.filter (fun (p, _) -> p <> from && linkable p) (Hashtbl.find_all (Lazy.force ix.c) (r.rname, r.rspace)) in
  (* the definitions, if any, over the declarations *)
  let best = List.fold_left (fun m (_, (d : Highlight_code.definition)) -> max m d.drank) 0 all in
  let cs =
    List.filter_map
      (fun (p, (d : Highlight_code.definition)) ->
        if d.drank < best then None
        else
          (* another project's after all of its own *)
          (* claude: the use's own top folder before a library: the
           * kernel's qlock for the kernel, libc's for the programs *)
          let group = (if own_header p then 0 else if top p = top from then 1 else if library p then 2 else 3) + if other p then 4 else 0 in
          Some (candidate p d (group, - shared p from) (other p)))
      all
  in
  ranked cs

let find_in ?(roots = []) (ix : index) ~from f r =
  let here = project roots from in
  let other p = project roots p <> here in
  if is_c from then find_c ~other ix ~from f r else find_ml ~other ix ~from f r

let find ?roots files ~from f r = find_in ?roots (index files) ~from f r
