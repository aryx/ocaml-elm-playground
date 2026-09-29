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
}

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
  { ml; c }

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
            (Hashtbl.find_all ix.ml m)
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
  let all = List.filter (fun (p, _) -> p <> from) (Hashtbl.find_all (Lazy.force ix.c) (r.rname, r.rspace)) in
  (* the definitions, if any, over the declarations *)
  let best = List.fold_left (fun m (_, (d : Highlight_code.definition)) -> max m d.drank) 0 all in
  let cs =
    List.filter_map
      (fun (p, (d : Highlight_code.definition)) ->
        if d.drank < best then None
        else
          (* another project's after all of its own *)
          let group = (if own_header p then 0 else if library p then 1 else 2) + if other p then 3 else 0 in
          Some (candidate p d (group, - shared p from) (other p)))
      all
  in
  ranked cs

let find_in ?(roots = []) (ix : index) ~from f r =
  let here = project roots from in
  let other p = project roots p <> here in
  if is_c from then find_c ~other ix ~from f r else find_ml ~other ix ~from f r

let find ?roots files ~from f r = find_in ?roots (index files) ~from f r
