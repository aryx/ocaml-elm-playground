(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_layers.mli *)

open Playground
open Code_map_base

(*****************************************************************************)
(* The layers *)
(*****************************************************************************)

(* claude: the layers (l), codemap's: the whole map coloured by a
 * measure, at two levels as codemap's (the author): from afar a file in
 * one colour (the macro level), nearer each definition's lines in its
 * own (the micro level), so that zooming in shows which definition made
 * the file red. A measure is ranked, each file (each definition) by its
 * place among all (a percentile), so that the whole gradient is used:
 * raw, the values sat in a narrow band, and the earth was all one
 * colour (the author, at principia: lib_core not red).
 *
 * Used vs using (the author: "what is used vs what is using", "to know
 * what is the bottom of this project, what is the top, what is the
 * middle, at a high level", "and this depends on where we are"): the
 * parts of the unit looked at (at the earth, the top folders; in a
 * region, its subfolders and files), each by the uses coming into it
 * from the other parts over those and the uses going out to them, its
 * files in its colour: lib_core red, the libraries on it yellow, the
 * programs green. Nearer, a definition by
 * the files using it over those and the files its calls reach, ranked. The call depth: a definition by its depth over its depth and
 * height in the call graph, a file its definitions' mean. Green the top,
 * what uses; red the bottom, what is used: a road's two ends' colours *)
let layer_names = [ "used vs using"; "the call depth"; "roles"; "tested"; "described" ]
let layer_count = List.length layer_names


(* a layer's values: each definition in the call graph, its first line,
 * lines and value (0 the top, 1 the bottom), by file; each file's *)
type layer_data = { defs : (string, (int * int * float) list) Hashtbl.t; files : (string, float) Hashtbl.t }

(* each key its rank among all, from 0 to 1, the equal ones the same (their
 * ranks' mean) *)
let percentiles (vals : ('a * float) list) : ('a, float) Hashtbl.t =
  let sorted = Array.of_list (List.stable_sort (fun (_, x) (_, y) -> compare x y) vals) in
  let n = Array.length sorted in
  let tbl = Hashtbl.create (max 16 n) in
  let i = ref 0 in
  while !i < n do
    let j = ref !i in
    while !j + 1 < n && snd sorted.(!j + 1) = snd sorted.(!i) do incr j done;
    let pct = if n <= 1 then 0.5 else float_of_int (!i + !j) /. 2. /. float_of_int (n - 1) in
    for k = !i to !j do Hashtbl.replace tbl (fst sorted.(k)) pct done;
    i := !j + 1
  done;
  tbl

let layer_cache : (Code_rank.t * int * string * layer_data) option ref = ref None

let layer_of (t : t) (layer : int) (top : string) (r : Code_rank.t) : layer_data =
  match !layer_cache with
  | Some (r', k, top', d) when r' == r && k = layer && (layer <> 1 || top' = top) -> d
  | _ ->
      let places = Code_rank.places r in
      let ratio a b = if a + b = 0 then None else Some (float_of_int a /. float_of_int (a + b)) in
      let def_value (pl : Code_rank.place) = if layer = 1 then ratio pl.used pl.reach else ratio pl.depth pl.height in
      let ranked = percentiles (List.filter_map (fun (p, l, pl) -> Option.map (fun x -> ((p, l), x)) (def_value pl)) places) in
      let defs = Hashtbl.create 1024 in
      List.iter
        (fun (p, l, (pl : Code_rank.place)) ->
          match Hashtbl.find_opt ranked (p, l) with
          | Some x -> Hashtbl.replace defs p ((l, pl.lines, x) :: Option.value (Hashtbl.find_opt defs p) ~default:[])
          | None -> ())
        places;
      let files =
        if layer = 1 then begin
          (* the parts of the unit looked at, each by the uses coming into
           * it from the others over those and the uses going out to them,
           * counted in uses (a stray link a few, libc's thousands:
           * layering by the longest paths put the programs with lib_core
           * over one), its files its colour *)
          let parts = List.map (fun i -> t.placed.(i).path) (Code_units.children t.placed t.focus) in
          let part_of p = List.find_opt (fun q -> p = q || Code_search.starts p (q ^ "/")) parts in
          let ins = Hashtbl.create 64 and outs = Hashtbl.create 64 in
          let add tbl k n = Hashtbl.replace tbl k (n + Option.value (Hashtbl.find_opt tbl k) ~default:0) in
          List.iter
            (fun (a, b, n) -> match (part_of a, part_of b) with Some pa, Some pb when pa <> pb -> add outs pa n; add ins pb n | _ -> ())
            (Code_rank.links r);
          let value q = ratio (Option.value (Hashtbl.find_opt ins q) ~default:0) (Option.value (Hashtbl.find_opt outs q) ~default:0) in
          let tbl = Hashtbl.create 1024 in
          List.iter (fun (e : entry) -> match Option.bind (part_of e.path) value with Some x -> Hashtbl.replace tbl e.path x | None -> ()) t.entries;
          tbl
        end
        else begin
          (* its definitions' mean, by their lines *)
          let tbl = Hashtbl.create 1024 in
          Hashtbl.iter
            (fun p ds ->
              let w, n = List.fold_left (fun (w, n) (_, k, x) -> (w +. (x *. float_of_int k), n + k)) (0., 0) ds in
              if n > 0 then Hashtbl.replace tbl p (w /. float_of_int n))
            defs;
          tbl
        end
      in
      let d = { defs; files } in
      layer_cache := Some (r, layer, top, d);
      d

(* claude: the uses being counted, a key waiting for them (Code_map's
 * update, the next frame): said in the middle of the map *)
let counting (c : camera) : shape list =
  let a = c.a in
  let msg = "counting the uses of every definition... a few seconds" in
  let w = text_width 18. msg +. 40. and h = 44. in
  let cx = float_of_int a.pw /. 2. and cy = float_of_int a.ph /. 2. in
  [ rectangle (rgb 16 14 34) w h |> move (sx a cx) (sy a cy) |> fade 0.95; label a yellow 18. cx cy msg ]

(* claude: the roles layer (Code_roles, after codemap's Archi_code): each
 * file by its role, tests, interfaces, per-CPU code, entry points...,
 * in its role's colour at every level (a role is a file's, not its
 * definitions'); its key the roles found under the unit looked at, how
 * many files each *)
let roles_cache : (Code_rank.t * entry list * (string, Code_roles.category) Hashtbl.t) option ref = ref None

let roles_of (t : t) (r : Code_rank.t) : (string, Code_roles.category) Hashtbl.t =
  match !roles_cache with
  | Some (r', es, tbl) when r' == r && es == t.entries -> tbl
  | _ ->
      let tbl = Code_roles.categories ~links:(Code_rank.links r) (List.map (fun (e : entry) -> e.path) (t.entries @ t.beyond)) in
      roles_cache := Some (r, t.entries, tbl);
      tbl

(* claude: a layer of files each in a colour of a short list (roles,
 * tested, described): [kind path] its row, a name and a colour, or none
 * (drawn as the map is); the rows in the key in [rows]' order, how many
 * files each under the unit looked at *)
let file_layer (t : t) (c : camera) ~(title : string) ~(rows : (string * (int * int * int)) list) (kind : string -> (string * (int * int * int)) option) : shape list * shape list =
  let a = c.a in
  let tints =
    if Map_paint.at_ground t c <> None then []
    else
      Array.to_list t.placed
      |> List.filter_map (fun (p : entry Treemap.placed) ->
             match (p.node, clip c p.rect) with
             | File _, Some (x0, y0, x1, y1) when not (Map_paint.outside t p) -> (
                 match kind p.path with
                 | Some (_, (r, g, b)) ->
                     let w = float_of_int (x1 - x0) and h = float_of_int (y1 - y0) in
                     Some (rectangle (rgb r g b) w h |> move (sx a (float_of_int x0 +. (w /. 2.))) (sy a (float_of_int y0 +. (h /. 2.))) |> fade 0.75)
                 | None -> None)
             | _ -> None)
  in
  let top = t.placed.(t.focus).path in
  let under p = top = "" || p = top || Code_search.starts p (top ^ "/") in
  let kinds = List.filter_map (fun (e : entry) -> if under e.path then Option.map fst (kind e.path) else None) t.entries in
  let rows = List.filter_map (fun (name, col) -> match List.length (List.filter (( = ) name) kinds) with 0 -> None | n -> Some (name, col, n)) rows in
  let head = Printf.sprintf "layer: %s   (l: next, shift+l: back)" title in
  let row = 18. in
  let w = 20. +. List.fold_left (fun m (name, _, n) -> Float.max m (30. +. text_width 13. (Printf.sprintf "%s  %d" name n))) (text_width 14. head) rows in
  let h = 30. +. (row *. float_of_int (List.length rows)) in
  let x0 = float_of_int a.pw -. w -. 10. and y0 = float_of_int a.ph -. h -. 10. in
  ( tints,
    [ rectangle (rgb 16 14 34) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.9 ]
    @ [ label a yellow 14. (x0 +. 10. +. (text_width 14. head /. 2.)) (y0 +. 14.) head ]
    @ List.concat
        (List.mapi
           (fun i (name, (r, g, b), n) ->
             let y = y0 +. 32. +. (float_of_int i *. row) in
             let str = Printf.sprintf "%s  %d" name n in
             [ circle (rgb r g b) 5. |> move (sx a (x0 +. 16.)) (sy a y); label a ink 13. (x0 +. 28. +. (text_width 13. str /. 2.)) y str ])
           rows) )

let roles_shapes (t : t) (c : camera) (r : Code_rank.t) : shape list * shape list =
  let tbl = roles_of t r in
  let rows = List.filter_map (fun cat -> if cat = Code_roles.Plain then None else Some (Code_roles.name cat, Code_roles.colour cat)) Code_roles.all in
  file_layer t c ~title:"roles" ~rows (fun p -> match Hashtbl.find_opt tbl p with Some cat when cat <> Code_roles.Plain -> Some (Code_roles.name cat, Code_roles.colour cat) | _ -> None)

(* claude: tested (the author, a layer proposed): the tests' files (their
 * role, Code_roles), and each other file by whether the tests reach it,
 * through the files they use and those use, transitively (the links,
 * Code_rank): green reached, red not -- where tests are missing; an
 * interface, checked through its implementation, left out *)
let tested_cache : (Code_rank.t * entry list * (string, int) Hashtbl.t) option ref = ref None

let tested_shapes (t : t) (c : camera) (r : Code_rank.t) : shape list * shape list =
  let roles = roles_of t r in
  let reached =
    match !tested_cache with
    | Some (r', es, tbl) when r' == r && es == t.entries -> tbl
    | _ ->
        let succ = Hashtbl.create 1024 in
        List.iter (fun (a, b, _) -> Hashtbl.replace succ a (b :: Option.value (Hashtbl.find_opt succ a) ~default:[])) (Code_rank.links r);
        let tbl = Hashtbl.create 1024 in
        let rec go p = if not (Hashtbl.mem tbl p) then begin Hashtbl.replace tbl p 0; List.iter go (Option.value (Hashtbl.find_opt succ p) ~default:[]) end in
        Hashtbl.iter (fun p cat -> if cat = Code_roles.Test then List.iter go (Option.value (Hashtbl.find_opt succ p) ~default:[])) roles;
        tested_cache := Some (r, t.entries, tbl);
        tbl
  in
  (* the tests blue here: the roles' green is the reached's *)
  let test = ("tests", (90, 150, 245)) and yes = ("reached by the tests", (90, 220, 120)) and no = ("not reached", (240, 80, 70)) in
  file_layer t c ~title:"tested" ~rows:[ test; yes; no ] (fun p ->
      match Hashtbl.find_opt roles p with
      | Some Code_roles.Test -> Some test
      | Some Code_roles.Interface -> None
      | _ -> Some (if Hashtbl.mem reached p then yes else no))

(* claude: described (the author, a layer proposed): each file by what its
 * config says of it (Code_guide.file_note): a summary and capitals or
 * important lines, green; a summary alone, yellow; nothing, red -- where
 * the configs are still to write (codemapconfig_guidelines.md) *)
let described_shapes (t : t) (c : camera) : shape list * shape list =
  let full = ("summary and capitals", (90, 220, 120)) and some = ("a summary", (245, 225, 90)) and none = ("not described", (240, 80, 70)) in
  file_layer t c ~title:"described" ~rows:[ full; some; none ] (fun p ->
      match Code_guide.file_note t.guide p with
      | Some n when n.summary <> None && (n.capitals <> [] || n.important <> []) -> Some full
      | Some n when n.summary <> None -> Some some
      | _ -> Some none)

(* claude: a layer's tints, under the names, and its key, over them *)
let layer_shapes (t : t) (c : camera) : shape list * shape list =
  match (t.layer, t.rank) with
  | 0, _ -> ([], [])
  | _, None -> ([], counting c)
  | 3, Some r -> roles_shapes t c r
  | 4, Some r -> tested_shapes t c r
  | 5, _ -> described_shapes t c
  | _, Some r ->
      let a = c.a in
      let data = layer_of t t.layer t.placed.(t.focus).path r in
      let rect_px ?(alpha = 0.6) (x0, y0, x1, y1) (r, g, b) =
        let w = x1 -. x0 and h = y1 -. y0 in
        rectangle (rgb r g b) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade alpha
      in
      let defs_of p = Option.value (Hashtbl.find_opt data.defs p) ~default:[] in
      let tints =
        match Map_paint.at_ground t c with
        | Some e ->
            (* at the ground, each line of each definition shown *)
            List.concat_map
              (fun (path, (g : Code_ground.t)) ->
                List.concat_map
                  (fun (l0, n, x) ->
                    List.filter_map
                      (fun l -> if l < Array.length g.places then let x0, y0, w, h = Code_ground.box g l in Some (rect_px ~alpha:0.45 (x0, y0, x0 +. w, y0 +. Float.max 1.5 h) (Map_skeleton.heat x)) else None)
                      (List.init n (fun i -> l0 + i)))
                  (defs_of path))
              (Map_skeleton.grounds t e)
        | None ->
            Array.to_list t.placed
            |> List.mapi (fun i p -> (i, p))
            |> List.concat_map (fun (i, (p : entry Treemap.placed)) ->
                   match (p.node, clip c p.rect, t.geometry.(i)) with
                   | File _, Some (x0, y0, x1, y1), geo when not (Map_paint.outside t p) -> (
                       match geo with
                       | Some g when g.cell_h *. c.z >= 1.5 ->
                           (* the micro level: each definition's lines, a
                            * rectangle per column they are in *)
                           List.concat_map
                             (fun (l0, n, x) ->
                               let last = l0 + n - 1 in
                               List.filter_map
                                 (fun col ->
                                   let first = max l0 (col * g.lpc) and upto = min last ((col * g.lpc) + g.lpc - 1) in
                                   if upto < first then None
                                   else
                                     let ux, uy = line_pos p.rect g first in
                                     let px0 = Float.max (float_of_int x0) (to_px c ux) and px1 = Float.min (float_of_int x1) (to_px c (ux +. g.colw)) in
                                     let py0 = Float.max (float_of_int y0) (to_py c uy) and py1 = Float.min (float_of_int y1) (to_py c (uy +. (float_of_int (upto - first + 1) *. g.cell_h))) in
                                     if px1 <= px0 || py1 <= py0 then None else Some (rect_px ~alpha:0.5 (px0, py0, px1, py1) (Map_skeleton.heat x)))
                                 (List.init ((last / g.lpc) - (l0 / g.lpc) + 1) (fun j -> (l0 / g.lpc) + j)))
                             (defs_of p.path)
                       | _ -> (
                           (* the macro level: the file in its mean's colour *)
                           match Hashtbl.find_opt data.files p.path with
                           | Some x -> [ rect_px ~alpha:0.8 (float_of_int x0, float_of_int y0, float_of_int x1, float_of_int y1) (Map_skeleton.heat x) ]
                           | None -> []))
                   | _ -> [])
      in
      (* the key, bottom right: the gradient, its two ends named *)
      let name = Option.value (List.nth_opt layer_names (t.layer - 1)) ~default:"" in
      let head = Printf.sprintf "layer: %s   (l: next, shift+l: back)" name in
      let w = 330. and h = 64. in
      let x0 = float_of_int a.pw -. w -. 10. and y0 = float_of_int a.ph -. h -. 10. in
      let steps = 32 in
      let bw = (w -. 24.) /. float_of_int steps in
      ( tints,
      [ rectangle (rgb 16 14 34) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.9 ]
      @ [ label a yellow 14. (x0 +. 12. +. (text_width 14. head /. 2.)) (y0 +. 14.) head ]
      @ List.init steps (fun k ->
            let r, g, b = Map_skeleton.heat (float_of_int k /. float_of_int (steps - 1)) in
            rectangle (rgb r g b) (bw +. 0.5) 10. |> move (sx a (x0 +. 12. +. (bw *. (float_of_int k +. 0.5)))) (sy a (y0 +. 34.)))
      @
      let lo, hi = if t.layer = 1 then ("using: the most", "used: the most") else ("the top of the call stack", "the bottom") in
      [ label a ink 12. (x0 +. 12. +. (text_width 12. lo /. 2.)) (y0 +. 52.) lo; label a ink 12. (x0 +. w -. 12. -. (text_width 12. hi /. 2.)) (y0 +. 52.) hi ] )
