(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_paint.mli *)

open Code_map_base

(*****************************************************************************)
(* The picture *)
(*****************************************************************************)

(* a file's columns: a strip per column of 80 characters (its geometry's
 * k columns of lpc lines), the last only as long as the lines left, a
 * gap between them when they are wide enough on the screen to show one *)
let paint_columns (img : Rgba_image.t) (c : camera) (r : Treemap.rect) (g : geometry) (nlines : int) (x0, y0, x1, y1) (col : int * int * int) =
  let colw_px = g.colw *. c.z in
  let gap = if colw_px >= 5. then Float.max 1. (0.12 *. colw_px) else 0. in
  let pad = if r.h *. c.z >= 12. then Float.min 3. (0.04 *. r.h *. c.z) else 0. in
  let top = to_py c r.y +. pad and bottom = to_py c (r.y +. r.h) -. pad in
  for i = 0 to g.k - 1 do
    let left = to_px c (r.x +. (float_of_int i *. g.colw)) +. (gap /. 2.) in
    let right = to_px c (r.x +. (float_of_int (i + 1) *. g.colw)) -. (gap /. 2.) in
    let lines = min g.lpc (nlines - (i * g.lpc)) in
    let frac = float_of_int lines /. float_of_int g.lpc in
    let low = top +. (frac *. (bottom -. top)) in
    let px v = int_of_float (Float.round v) in
    let sx0 = max x0 (px left) and sx1 = min x1 (px right) and sy0 = max y0 (px top) and sy1 = min y1 (px low) in
    if lines > 0 && sx1 > sx0 && sy1 > sy0 then fill img sx0 sy0 sx1 sy1 col
  done

(* claude: outside the unit looked at (Code_units, t.focus), the map
 * dimmed: the units are often tall and narrow, the screen wide, so the
 * one framed shares the screen with its neighbours -- kept, for knowing
 * where one is, but in the shade *)
let outside (t : t) (p : entry Treemap.placed) : bool =
  t.focus <> 0
  &&
  let f = t.placed.(t.focus).rect in
  let u = p.rect.x +. (p.rect.w /. 2.) and v = p.rect.y +. (p.rect.h /. 2.) in
  not (u >= f.x && u < f.x +. f.w && v >= f.y && v < f.y +. f.h)

(*****************************************************************************)
(* The ground *)
(*****************************************************************************)

(* claude: the ground level (Code_ground, the plan's step 5): the unit
 * looked at a file, and the camera there, the whole map is that file,
 * each line as high as it matters *)
let at_ground (t : t) (c : camera) : entry option =
  match t.placed.(t.focus).node with
  | File (_, _, e) when t.focus <> 0 ->
      let f = fit c.a t.placed.(t.focus).rect in
      if
        Float.abs (Float.log (f.z /. c.z)) < 0.1
        && Float.abs (f.cx -. c.cx) *. c.z < 0.05 *. float_of_int c.a.pw
        && Float.abs (f.cy -. c.cy) *. c.z < 0.05 *. float_of_int c.a.ph
      then Some e
      else None
  | _ -> None

(* what the config calls important in a file, found: its line, its
 * weight, its words; the capitals among them, weighing 3 *)
(* claude: found once a file (an anchor found is a search of its lines:
 * every frame, a big street's panels cost a browser 0.6 s a frame) *)
let important_cache : (string, Code_guide.t * (int * int * string option) list) Hashtbl.t = Hashtbl.create 64

let important (t : t) (e : entry) : (int * int * string option) list =
  match Hashtbl.find_opt important_cache e.path with
  | Some (g, r) when g == t.guide -> r
  | _ ->
      let r =
        match Code_guide.file_note t.guide e.path with
        | None -> []
        | Some n ->
            let f = Lazy.force e.file in
            List.filter_map
              (fun (it : Code_guide.item) -> match Code_guide.find f it.at with Ok l -> Some (l, it.weight, it.say) | Error _ -> None)
              (n.important @ List.map (fun (it : Code_guide.item) -> { it with weight = 3 }) n.capitals)
      in
      Hashtbl.replace important_cache e.path (t.guide, r);
      r

(* the file's lines laid out on the window's map (its pixels, not the
 * picture's, which may be more: Code_ground.scale), kept *)
let ground_cache : (string * int * int * Code_ground.t) option ref = ref None

let ground_of (t : t) (e : entry) : Code_ground.t =
  let a = t.cam.a in
  match !ground_cache with
  | Some (p, w, h, g) when p = e.path && w = a.pw && h = a.ph -> g
  | _ ->
      let f = Lazy.force e.file in
      let weights = Code_ground.weights f ~important:(List.map (fun (l, w, _) -> (l, w)) (important t e)) in
      let g = Code_ground.layout weights ~pw:a.pw ~ph:a.ph in
      ground_cache := Some (e.path, a.pw, a.ph, g);
      g

let paint_ground ~(aa : bool) (t : t) (c : camera) (e : entry) : Rgba_image.t =
  let img = Rgba_image.create ~width:c.a.pw ~height:c.a.ph in
  let bg = file_background t e.path in
  fill img 0 0 c.a.pw c.a.ph bg;
  let q = float_of_int c.a.pw /. float_of_int t.cam.a.pw in
  let g = Code_ground.scale (ground_of t e) q in
  (* the important lines marked in the margin, a bar as thick as their
   * weight *)
  List.iter
    (fun (l, w, _) ->
      let x0, y0, _, h = Code_ground.box g l in
      let bar = int_of_float (q *. float_of_int (1 + w)) in
      fill img (int_of_float x0 - bar - int_of_float (2. *. q)) (int_of_float y0) (int_of_float x0 - int_of_float (2. *. q)) (int_of_float (y0 +. h)) (255, 215, 70))
    (important t e);
  Code_ground.paint img (Lazy.force e.file) g ~bg ~aa;
  img

(* claude: the street level (Code_street, the plan's step 6): at the
 * ground, a, the file on the left and what it uses on the right, roads
 * from each use to its definition; laid out on the window's map, kept *)
let street_cache : (string * int * int * int * Code_street.t) option ref = ref None

let street_mode (t : t) : Code_street.mode = match t.street_mode with 2 -> Users | 3 -> Both | _ -> Uses

let street_of (t : t) (e : entry) : Code_street.t =
  let a = t.cam.a in
  match !street_cache with
  | Some (p, w, h, m, s) when p = e.path && w = a.pw && h = a.ph && m = t.street_mode -> s
  | _ ->
      let f = Lazy.force e.file in
      (* claude: its panels may be beyond the map (a folder laid out alone) *)
      let file p = List.find_map (fun (x : entry) -> if x.path = p then Some (Lazy.force x.file) else None) (t.entries @ t.beyond) in
      let mode = street_mode t in
      let uses = if mode = Users then [] else Code_street.uses ~index:(index_of t) ~roots:t.roots ~path:e.path f in
      (* what uses it: the files linked to it (Code_rank, every file of the
       * map counted once), the most tied first, their uses of it *)
      let users =
        if mode = Uses then []
        else
          Code_rank.links (rank_of t)
          |> List.filter_map (fun (src, dst, n) -> if dst = e.path && src <> e.path then Some (src, n) else None)
          |> List.sort (fun (_, n) (_, m) -> compare m n)
          |> List.filteri (fun i _ -> i < 12)
          |> List.concat_map (fun (src, _) ->
                 match file src with
                 | Some g -> List.filter (fun (ed : Code_street.edge) -> ed.target = e.path) (Code_street.uses ~index:(index_of t) ~roots:t.roots ~path:src g)
                 | None -> [])
      in
      let focus = Code_ground.weights f ~important:(List.map (fun (l, w, _) -> (l, w)) (important t e)) in
      let s = Code_street.layout ~mode ~first:(fun p -> Code_deps.own e.path p) ~focus_path:e.path ~focus ~file ~uses ~users ~pw:a.pw ~ph:a.ph () in
      street_cache := Some (e.path, a.pw, a.ph, t.street_mode, s);
      s

(* claude: the street that fits the file looked at (the author: "if the
 * file is not used by anything but uses some files, go in the fan-out
 * view; if it's using and is used, the view with the left and right
 * files around"): 3 both, 1 its uses, 2 its users *)
let best_street_mode (t : t) : int =
  match t.placed.(t.focus).node with
  | File (_, _, e) ->
      let links = Code_rank.links (rank_of t) in
      let uses = List.exists (fun (src, dst, _) -> src = e.path && dst <> e.path) links in
      let used = List.exists (fun (src, dst, _) -> dst = e.path && src <> e.path) links in
      (match (uses, used) with true, true -> 3 | false, true -> 2 | _ -> 1)
  | Dir _ -> 1

let paint_street ~(aa : bool) (t : t) (c : camera) (e : entry) : Rgba_image.t =
  let img = Rgba_image.create ~width:c.a.pw ~height:c.a.ph in
  let bg = file_background t e.path in
  fill img 0 0 c.a.pw c.a.ph dark;
  let q = float_of_int c.a.pw /. float_of_int t.cam.a.pw in
  let s = Code_street.scale (street_of t e) q in
  (* the focus's column *)
  fill img (int_of_float s.focus.ox) 0 (int_of_float (s.focus.ox +. (s.focus.colw *. float_of_int s.focus.cols))) c.a.ph bg;
  let mark (g : Code_ground.t) (l : int) col =
    if l < Array.length g.places then
      let x0, y0, _, h = Code_ground.box g l in
      fill img (int_of_float (x0 -. (5. *. q))) (int_of_float y0) (int_of_float (x0 -. (2. *. q))) (int_of_float (y0 +. h)) col
  in
  (* the uses marked green in the focus's margin, the definitions used
   * red in the panels' *)
  (* the ties: green at a user's line, red at the definition used *)
  List.iter (fun (ed : Code_street.edge) -> mark s.focus ed.from_line (90, 220, 120)) s.uses;
  List.iter (fun (ed : Code_street.edge) -> mark s.focus ed.target_line (250, 80, 70)) s.users;
  Code_ground.paint img (Lazy.force e.file) s.focus ~bg ~aa;
  List.iter
    (fun (p : Code_street.panel) ->
      match List.find_opt (fun (x : entry) -> x.path = p.path) (t.entries @ t.beyond) with
      | None -> ()
      | Some x ->
          let pbg = file_background t p.path in
          let x0 = int_of_float p.ground.ox and y0 = int_of_float (p.ground.oy -. (22. *. q)) in
          let y1 = match p.ground.places with [||] -> y0 | ps -> let l = ps.(Array.length ps - 1) in int_of_float (p.ground.oy +. l.y +. l.h) in
          (* its own width only: a left panel must not cover the focus *)
          let x1 = int_of_float (p.ground.ox +. (p.ground.colw *. float_of_int p.ground.cols)) in
          fill img x0 y0 x1 (y1 + 2) pbg;
          List.iter (fun (ed : Code_street.edge) -> if ed.target = p.path then mark p.ground ed.target_line (250, 80, 70)) s.uses;
          List.iter (fun (ed : Code_street.edge) -> if ed.src = p.path then mark p.ground ed.from_line (90, 220, 120)) s.users;
          Code_ground.paint img (Lazy.force x.file) p.ground ~bg:pbg ~aa)
    (Code_street.panels s);
  img

let paint ~(aa : bool) (t : t) (c : camera) : Rgba_image.t =
  match at_ground t c with
  | Some e when t.street -> paint_street ~aa t c e
  | Some e -> paint_ground ~aa t c e
  | None ->
  let img = Rgba_image.create ~width:c.a.pw ~height:c.a.ph in
  fill img 0 0 c.a.pw c.a.ph dark;
  Array.iteri
    (fun i (p : entry Treemap.placed) ->
      let shade = outside t p in
      let fill img x0 y0 x1 y1 col = fill img x0 y0 x1 y1 (if shade then mix col 0.3 dark else col) in
      let paint_columns img c r g n box col = paint_columns img c r g n box (if shade then mix col 0.3 dark else col) in
      match clip c p.rect with
      | None -> ()
      | Some ((x0, y0, x1, y1) as box) -> (
          match (p.node, t.geometry.(i)) with
          | Dir _, _ ->
              fill img x0 y0 x1 y1 (dir_colour t p.path p.depth);
              (* a region's border, a country's *)
              if p.depth = 1 && x1 - x0 > 8 && y1 - y0 > 8 then begin
                let edge = mix (region_colour t p.path) 0.7 dark in
                fill img x0 y0 x1 (y0 + 2) edge;
                fill img x0 (y1 - 2) x1 y1 edge;
                fill img x0 y0 (x0 + 2) y1 edge;
                fill img (x1 - 2) y0 x1 y1 edge
              end
          | File (_, _, e), Some g ->
              let bg = file_background t p.path in
              (* the code only when the file is the place looked at: a
               * good part of the map, and near enough to be read (a file of five
               * lines is readable from afar, specks among the columns);
               * the plan's ground level is to come, step 5 *)
              if readable c g && p.rect.w *. p.rect.h *. c.z *. c.z >= 0.08 *. float_of_int (c.a.pw * c.a.ph) then paint_code ~aa img c p.rect g (Lazy.force e.file) box bg
              else begin
                fill img x0 y0 x1 y1 bg;
                paint_columns img c p.rect g e.nlines box (mix (region_colour t p.path) 0.55 bg)
              end
          | File _, None -> ()))
    t.placed;
  img

(* claude: the street's files beside the focus (its panels), for g *)
let street_files (t : t) : string list =
  match at_ground t t.cam with Some e when t.street -> List.map (fun (p : Code_street.panel) -> p.path) (Code_street.panels (street_of t e)) | _ -> []
