(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_atlas.mli *)

open Playground
open Code_map_base

(* claude: at the street, a panel's name under a pixel: a click there
 * goes to that file (the author) *)
let street_title_at (t : t) (c : camera) (px : float) (py : float) : string option =
  match Map_paint.at_ground t c with
  | Some e when t.street ->
      let s = Map_paint.street_of t e in
      List.find_map
        (fun (p : Code_street.panel) ->
          let text = Printf.sprintf "%s   (%d tie%s)" p.path p.count (if p.count = 1 then "" else "s") in
          let tw = 0.5 *. 14. *. float_of_int (String.length text) in
          if px >= p.ground.ox && px <= p.ground.ox +. 16. +. tw && py >= p.ground.oy -. 22. && py <= p.ground.oy then Some p.path else None)
        (Code_street.panels s)
  | _ -> None

(* claude: a match under the mouse (a search's, a mark's), its line and
 * the code around it beside the mouse (the author: "when you hover a
 * match, we peek preview the content of the match and the code
 * around"); the lit hits' places found by an index of the layout, not
 * spot's scan, there being thousands *)
let hovered_match (t : t) (c : camera) : (Code_search.hit * color * string option) option =
  match t.pointer with
  | None -> None
  | Some _ when t.peek <> None -> None
  | Some (u, v) -> (
      let mx = to_px c u and my = to_py c v in
      let lit =
        (match t.search with Some _ -> List.map (fun h -> (h, rgb 255 225 90, None)) (Map_search.search_hits t) | None -> [])
        @ (if t.mark_group < 0 then []
           else
             match List.nth_opt (Map_search.mark_groups t) t.mark_group with
             | Some (_, ls) -> List.concat_map (fun (l : mark) -> let r, g, b = l.mcolour in List.map (fun h -> (h, rgb r g b, l.msay)) (Map_search.mark_hits t l)) ls
             | None -> [])
      in
      let lines = List.filter (fun ((h : Code_search.hit), _, _) -> h.kind = Def || h.kind = Text) lit in
      if lines = [] then None
      else
        let index = Hashtbl.create 256 in
        Array.iteri (fun i (p : entry Treemap.placed) -> match p.node with File _ -> Hashtbl.replace index p.path i | Dir _ -> ()) t.placed;
        let ground = Map_paint.at_ground t c <> None in
        let where (h : Code_search.hit) =
          if ground then Option.map (fun (x, y, xe) -> (x, y, Float.max xe (x +. 30.))) (Map_skeleton.spot t c h.path h.line)
          else
            match Hashtbl.find_opt index h.path with
            | Some i -> (
                match t.geometry.(i) with
                | Some g ->
                    let x, y = line_pos t.placed.(i).rect g h.line in
                    let px = to_px c x and py = to_py c (y +. (g.cell_h /. 2.)) in
                    Some (px, py, px)
                | None -> None)
            | None -> None
        in
        let best = ref None in
        List.iter
          (fun ((h : Code_search.hit), col, say) ->
            match where h with
            | Some (x, y, xe) ->
                let dx = if mx < x then x -. mx else if mx > xe then mx -. xe else 0. in
                let d = Float.hypot dx (y -. my) in
                if d < 9. && (match !best with Some (d', _) -> d < d' | None -> true) then best := Some (d, (h, col, say))
            | None -> ())
          (List.filteri (fun i _ -> i < 5000) lines);
        Option.map snd !best)

let match_preview (t : t) (c : camera) : shape list =
  match (hovered_match t c, t.pointer) with
  | Some (h, col, say), Some (u, v) ->
            let mx = to_px c u and my = to_py c v in
            let title = Printf.sprintf "%s:%d%s   (click: its definition)" h.path (h.line + 1) (match say with Some s -> "   " ^ s | None -> "") in
            Map_cards.code_card ~lit:(h.line, col) t c h.path (h.line - 3) (h.line + 3) title mx my
  | _ -> []

(* claude: a config's tour under way: its name, the stop, its words,
 * above the map's foot *)
let tour_banner (t : t) (c : camera) : shape list =
  match t.tour_on with
  | None -> []
  | Some (tr, k) ->
      let a = c.a in
      let say = match List.nth_opt tr.stops k with Some (i : Code_guide.item) -> Option.value i.say ~default:(Code_guide.anchor_name i.at) | None -> "" in
      let head = Printf.sprintf "%s   stop %d of %d   (n next, p back, Esc the end)" tr.name (k + 1) (List.length tr.stops) in
      let lines = Map_names.wrap 90 say in
      let w = Float.min (float_of_int a.pw -. 40.) (40. +. List.fold_left (fun m l -> Float.max m (text_width 18. l)) (text_width 14. head) lines) in
      let h = 34. +. (24. *. float_of_int (List.length lines)) in
      let x0 = (float_of_int a.pw -. w) /. 2. and y0 = float_of_int a.ph -. h -. 12. in
      [ rectangle (rgb 16 14 34) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.95 ]
      @ frame a (rgb 90 210 120) x0 y0 (x0 +. w) (y0 +. h) 2.
      @ [ label a (rgb 90 210 120) 14. (x0 +. 14. +. (text_width 14. head /. 2.)) (y0 +. 14.) head ]
      @ List.mapi (fun i l -> label a ink 18. (x0 +. 14. +. (text_width 18. l /. 2.)) (y0 +. 36. +. (24. *. float_of_int i)) l) lines

let anchor_line (t : t) (path : string) (anchor : string) : int option =
  match Map_skeleton.entry_of t path with Some e -> Map_names.capital_line e anchor | None -> None

(* claude: a bone under the mouse, its dot or its role (the author: "at
 * the skeleton, can we also hover a bone ... when the bone is an entity
 * especially") *)
let hovered_bone (t : t) (c : camera) : Code_guide.bone option =
  match t.pointer with
  | Some (u, v) when t.xray && t.peek = None ->
      let mx = to_px c u and my = to_py c v in
      List.find_map
        (fun ((bn : Code_guide.bone), x, y, tw) ->
          let on_dot = Float.hypot (mx -. x) (my -. y) < 11. in
          let on_role = mx >= x +. 12. && mx <= x +. 22. +. tw && my >= y -. 35. && my <= y -. 13. in
          if on_dot || on_role then Some bn else None)
        !Map_skeleton.drawn_bones
  | _ -> None

(* its card: an entity's role and the start of its definition, readable;
 * a file's or a directory's role and what its config says of it *)
let bone_card (t : t) (c : camera) : shape list =
  match (hovered_bone t c, t.pointer) with
  | Some bn, Some (u, v) -> (
      let mx = to_px c u and my = to_py c v in
      match Map_skeleton.bone_line t bn with
      | Some l -> (
          match Map_skeleton.entry_of t bn.bpath with
          | Some e ->
              let _, last = Map_skeleton.extent (Lazy.force e.file) l in
              Map_cards.code_card t c bn.bpath l (min last (l + 11)) (Printf.sprintf "%s   %s:%d   (click: all of it)" bn.role bn.bpath (l + 1)) mx my
          | None -> [])
      | None ->
          let a = c.a in
          let said =
            match Code_guide.file_note t.guide bn.bpath with
            | Some { summary = Some s; _ } -> Some s
            | _ -> Code_guide.dir_summary t.guide bn.bpath
          in
          let lines = (bn.role :: [ bn.bpath ]) @ (match said with Some s -> Map_names.wrap 60 s | None -> []) in
          let w = 24. +. List.fold_left (fun m l -> Float.max m (text_width 15. l)) 0. lines and h = 12. +. (22. *. float_of_int (List.length lines)) in
          let x0 = Float.min (mx +. 18.) (float_of_int a.pw -. w -. 4.) and y0 = Float.min (my +. 18.) (float_of_int a.ph -. h -. 4.) in
          let r, g, b = Map_skeleton.ivory in
          [ rectangle (rgb 18 16 36) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.96 ]
          @ frame a (rgb r g b) x0 y0 (x0 +. w) (y0 +. h) 1.5
          @ List.mapi (fun i l -> label a (if i = 0 then rgb r g b else if i = 1 then dim else ink) 15. (x0 +. 12. +. (text_width 15. l /. 2.)) (y0 +. 17. +. (22. *. float_of_int i)) l) lines)
  | _ -> []

(* claude: where a hit is on the map, now: a unit's centre, a line's
 * place; a view's, each of its units' *)
let hit_spots (t : t) (c : camera) (h : Code_search.hit) : (float * float) list =
  let index = Hashtbl.create 256 in
  Array.iteri (fun i (p : entry Treemap.placed) -> Hashtbl.replace index p.path i) t.placed;
  let unit p = match Hashtbl.find_opt index p with Some i -> (match clip c t.placed.(i).rect with Some (a0, b0, a1, b1) -> [ (float_of_int (a0 + a1) /. 2., float_of_int (b0 + b1) /. 2.) ] | None -> []) | None -> [] in
  match h.kind with
  | Dir | File -> unit h.path
  | Def | Text -> (
      match Map_skeleton.spot t c h.path h.line with
      | Some (x, y, _) -> [ (x, y) ]
      | None -> (
          match Hashtbl.find_opt index h.path with
          | Some i -> (
              match t.geometry.(i) with
              | Some g ->
                  let x, y = line_pos t.placed.(i).rect g h.line in
                  [ (to_px c x, to_py c (y +. (g.cell_h /. 2.))) ]
              | None -> unit h.path)
          | None -> []))
  | View -> ( match List.nth_opt (Code_guide.views t.guide) h.line with Some v -> List.concat_map unit (match v.of_ with Some o -> o :: v.files | None -> v.files) | None -> [])
  | Tour -> (
      match List.nth_opt (Code_guide.tours t.guide) h.line with
      | Some ({ stops = i :: _; _ } : Code_guide.tour) -> ( match Code_guide.split i.at with Some p, _ -> unit p | None, p -> unit p)
      | _ -> [])

(* the chosen hit, as the list scrolls (the author: "they should glow ...
 * as the eyes otherwise can not always discern where is the match"): a
 * ring pulsing round it, and a thread from its row in the box to it *)
let chosen_glow (t : t) (c : camera) (s : search) (h : Code_search.hit) : shape list =
  let a = c.a in
  let w = Float.min 760. (float_of_int a.pw -. 40.) in
  let x0 = (float_of_int a.pw -. w) /. 2. and y0 = 12. and row = 24. in
  let first = max 0 (s.sel - 7) in
  let ry = y0 +. 44. +. (float_of_int (s.sel - first) *. row) +. (row /. 2.) in
  let pulse = 0.5 +. (0.5 *. Float.sin (t.clock *. 7.)) in
  let glow = rgb 255 235 120 in
  List.concat_map
    (fun (x, y) ->
      let rx = if x < x0 then x0 else if x > x0 +. w then x0 +. w else x in
      let from = (rx, if y < ry then ry -. (row /. 2.) else ry +. (row /. 2.)) in
      let from = if y > y0 +. 200. then (rx, y0 +. 200.) else from in
      let pts = Code_road.bspline [| from; ((fst from +. x) /. 2., (snd from +. y) /. 2.); (x, y) |] in
      Code_road.road ~colours:((255, 235, 120), (255, 235, 120)) a pts 2. 0.6
      @ [
          circle glow (14. +. (8. *. pulse)) |> move (sx a x) (sy a y) |> fade (0.25 +. (0.2 *. pulse));
          circle glow 7. |> move (sx a x) (sy a y) |> fade 0.9;
        ])
    (hit_spots t c h)

let search_shapes (t : t) (c : camera) : shape list =
  match t.search with
  | None -> []
  | Some s ->
      let hits = Map_search.search_hits t in
      let chosen = List.nth_opt hits s.sel in
      Map_search.search_lit ?chosen t c hits @ Map_search.search_box t c s hits @ (match chosen with Some h -> chosen_glow t c s h | None -> [])

let labels (t : t) (c : camera) (q : float) : shape list =
  let kept = Map_names.names t c in
  (* claude: the layer first, under the names; its key last, over them *)
  let layer_tints, layer_key = Map_layers.layer_shapes t c in
  layer_tints
  @ (match Map_paint.at_ground t c with
  | Some e when t.street -> Map_cards.street_labels t c e @ Map_cards.line_lit t c e @ Map_cards.names_glow t c e
  | Some e -> Map_cards.notes t c e @ Map_cards.line_lit t c e @ Map_cards.names_glow t c e
  | None -> [])
  @ List.rev_map (fun (n : Map_names.name) -> n.draw) kept
  (* the legend when a plate other than the skeleton is on *)
  @ (if t.xray then Map_skeleton.skeleton_shapes t c @ Map_anatomy.anatomy_shapes t c @ (if true then Map_anatomy.legend ?pointer:(Option.map (fun (u, v) -> (to_px c u, to_py c v)) t.pointer) c else []) else [])
  @ Map_cards.unit_ties t c kept
  @ Map_cards.hover_card t c kept
  @ Map_peek.peek_shapes t c q
  @ Map_peek.peek_glow t c
  @ Map_search.marks_shapes t c
  @ search_shapes t c
  @ match_preview t c
  @ bone_card t c
  @ layer_key
  @ (if t.deferred <> None then Map_layers.counting c else [])
  @ tour_banner t c

(* claude: at the ground, the line under a pixel (Code_ground's layout,
 * not the treemap's): what Enter opens, what the status line says *)
let pick (t : t) (c : camera) (_ : float) (px : float) (py : float) : (string * int * int) option =
  (* the column under the pixel, in a line of a layout *)
  let col g l = let x0, _, _, _ = Code_ground.box g l in max 0 (int_of_float ((px -. x0) /. Code_ground.cell_w g l)) in
  match (Map_peek.peek_geom t c : Map_peek.peek option) with
  | Some pk ->
      (* a peek open: its line under the pixel, or nothing (a click there
       * closes it, Code_map) *)
      if not (Map_peek.inside_peek pk px py) then None
      else
        Option.map
          (fun l -> let x0, _, _, _ = Code_ground.box pk.pg l in (pk.pe.path, l, max 0 (int_of_float ((px -. pk.ix -. x0) /. Code_ground.cell_w pk.pg l))))
          (Code_ground.line_at pk.pg (px -. pk.ix) (py -. pk.iy))
      |> (function None when Map_peek.inside_peek pk px py -> Some (pk.pe.path, -2, 0) | r -> r)
  | None ->
  match Map_paint.at_ground t c with
  | Some e when t.street -> (
      let s = Map_paint.street_of t e in
      match Code_street.line_at s px py with
      | Some (p, l) -> ( match Code_street.ground_of s p with Some g -> Some (p, l, col g l) | None -> None)
      | None -> None)
  | Some e -> let g = Map_paint.ground_of t e in Option.map (fun l -> (e.path, l, col g l)) (Code_ground.line_at g px py)
  | None ->
      (* a section's title in a file's table of contents: the whole
       * section, the column -1 saying so (Code_map) *)
      Option.map (fun (p, l) -> (p, l, -1)) (List.find_map (fun (n : Map_names.name) -> if Map_names.within n.nbox px py then n.sect else None) (Map_names.names t c))

let style : style = { sname = "atlas"; paint = Map_paint.paint; labels; pick; unit_at = Map_names.unit_at; units = true }
