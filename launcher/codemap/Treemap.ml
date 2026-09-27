(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Treemap.mli. After codemap's libs/treemap/treemap.ml (its literate
 * Treemap.tex.nw explains the squarified algorithm step by step). *)

type rect = { x : float; y : float; w : float; h : float }
type 'a tree = Dir of string * 'a tree list | File of string * float * 'a
type algo = Squarified | Slice_and_dice

let rec size = function File (_, s, _) -> s | Dir (_, kids) -> List.fold_left (fun acc k -> acc +. size k) 0. kids

(*****************************************************************************)
(* Trees *)
(*****************************************************************************)

let of_paths (files : (string * float * 'a) list) : 'a tree =
  (* claude: files grouped by their first component, recursively *)
  let rec build name (entries : (string list * float * 'a) list) : 'a tree =
    let here = List.filter_map (function [ f ], s, d -> Some (File (f, s, d)) | _ -> None) entries in
    let below = List.filter (function _ :: _ :: _, _, _ -> true | _ -> false) entries in
    let dirs = List.sort_uniq compare (List.map (function d :: _, _, _ -> d | [], _, _ -> "") below) in
    let subdirs =
      List.map
        (fun d -> build d (List.filter_map (function x :: rest, s, dt when x = d && rest <> [] -> Some (rest, s, dt) | _ -> None) below))
        dirs
    in
    Dir (name, subdirs @ here)
  in
  build "" (List.map (fun (p, s, d) -> (String.split_on_char '/' p, s, d)) files)

let rec fold_singletons = function
  | File _ as f -> f
  | Dir (name, [ Dir (sub, kids) ]) -> fold_singletons (Dir ((if name = "" then sub else name ^ "/" ^ sub), kids))
  | Dir (name, kids) -> Dir (name, List.map fold_singletons kids)

(*****************************************************************************)
(* Slice and dice *)
(*****************************************************************************)

let slice ~(horizontal : bool) (sizes : float list) (r : rect) : rect list =
  let total = List.fold_left ( +. ) 0. sizes in
  let pos = ref 0. in
  List.map
    (fun s ->
      let f = if total > 0. then s /. total else 0. in
      let at = !pos in
      pos := !pos +. f;
      if horizontal then { r with x = r.x +. (at *. r.w); w = f *. r.w } else { r with y = r.y +. (at *. r.h); h = f *. r.h })
    sizes

(*****************************************************************************)
(* Squarified *)
(*****************************************************************************)

(* the worst aspect ratio of a row of areas [row] along a side [side]
 * (the paper's formula): max (side^2 * biggest / sum^2, sum^2 / (side^2 *
 * smallest)) *)
let worst (row : float list) (side : float) : float =
  let s = List.fold_left ( +. ) 0. row in
  let hi = List.fold_left Float.max 0. row and lo = List.fold_left Float.min Float.infinity row in
  Float.max (side *. side *. hi /. (s *. s)) (s *. s /. (side *. side *. lo))

(* [areas] (summing to [r]'s area) placed in rows: the next one joins the
 * row if that does not make the row's worst aspect worse *)
let rec squarify_areas (areas : float list) (row : float list) (r : rect) : rect list =
  let side = Float.min r.w r.h in
  match areas with
  | a :: rest when row = [] || worst (row @ [ a ]) side <= worst row side -> squarify_areas rest (row @ [ a ]) r
  | _ ->
      (* the row laid along the shorter side, the rest after it *)
      let srow = List.fold_left ( +. ) 0. row in
      let thick = if side > 0. then srow /. side else 0. in
      let row_rect, rest =
        if r.w >= r.h then ({ r with w = thick }, { r with x = r.x +. thick; w = r.w -. thick })
        else ({ r with h = thick }, { r with y = r.y +. thick; h = r.h -. thick })
      in
      let placed = slice ~horizontal:(r.w < r.h) row row_rect in
      if areas = [] then placed else placed @ squarify_areas areas [] rest

let squarify (sizes : float list) (r : rect) : rect list =
  let total = List.fold_left ( +. ) 0. sizes in
  if sizes = [] then []
  else if total <= 0. then List.map (fun _ -> { r with w = 0.; h = 0. }) sizes
  else
    (* claude: biggest first, as the paper does, and back in the given order *)
    let indexed = List.mapi (fun i s -> (i, s)) sizes in
    let sorted = List.stable_sort (fun (_, a) (_, b) -> compare b a) indexed in
    let area = r.w *. r.h in
    let rects = squarify_areas (List.map (fun (_, s) -> s /. total *. area) sorted) [] r in
    let placed = Array.make (List.length sizes) r in
    List.iter2 (fun (i, _) rc -> placed.(i) <- rc) sorted rects;
    Array.to_list placed

(*****************************************************************************)
(* Nesting *)
(*****************************************************************************)

type 'a placed = { rect : rect; depth : int; path : string; node : 'a tree }

let name_of = function Dir (n, _) | File (n, _, _) -> n

(* the border round a directory's children: thinner the deeper *)
let inset (depth : int) (r : rect) : rect =
  let b = Float.min r.w r.h *. if depth <= 1 then 0.012 else 0.02 in
  { x = r.x +. b; y = r.y +. b; w = Float.max 0. (r.w -. (2. *. b)); h = Float.max 0. (r.h -. (2. *. b)) }

let layout (algo : algo) (r : rect) (tree : 'a tree) : 'a placed list =
  let rec go depth path rect node acc =
    let path = if path = "" then name_of node else if name_of node = "" then path else path ^ "/" ^ name_of node in
    let acc = { rect; depth; path; node } :: acc in
    match node with
    | File _ -> acc
    | Dir (_, kids) ->
        let kids = List.filter (fun k -> size k > 0.) kids in
        let inner = if depth = 0 then rect else inset depth rect in
        let sizes = List.map size kids in
        let rects =
          match algo with Squarified -> squarify sizes inner | Slice_and_dice -> slice ~horizontal:(depth mod 2 = 0) sizes inner
        in
        List.fold_left2 (fun acc k rc -> go (depth + 1) path rc k acc) acc kids rects
  in
  List.rev (go 0 "" r tree [])
