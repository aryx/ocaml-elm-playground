(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_v2.mli *)

open Playground
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
let important (t : t) (e : entry) : (int * int * string option) list =
  match Code_guide.file_note t.guide e.path with
  | None -> []
  | Some n ->
      let f = Lazy.force e.file in
      List.filter_map
        (fun (it : Code_guide.item) -> match Code_guide.find f it.at with Ok l -> Some (l, it.weight, it.say) | Error _ -> None)
        (n.important @ List.map (fun (it : Code_guide.item) -> { it with weight = 3 }) n.capitals)

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
let street_cache : (string * int * int * Code_street.t) option ref = ref None

let street_of (t : t) (e : entry) : Code_street.t =
  let a = t.cam.a in
  match !street_cache with
  | Some (p, w, h, s) when p = e.path && w = a.pw && h = a.ph -> s
  | _ ->
      let f = Lazy.force e.file in
      let edges = Code_street.uses ~index:(index_of t) ~roots:t.roots ~path:e.path f in
      let file p = List.find_map (fun (x : entry) -> if x.path = p then Some (Lazy.force x.file) else None) t.entries in
      let focus = Code_ground.weights f ~important:(List.map (fun (l, w, _) -> (l, w)) (important t e)) in
      let s = Code_street.layout ~first:(fun p -> Code_deps.own e.path p) ~focus ~file edges ~pw:a.pw ~ph:a.ph in
      street_cache := Some (e.path, a.pw, a.ph, s);
      s

let paint_street ~(aa : bool) (t : t) (c : camera) (e : entry) : Rgba_image.t =
  let img = Rgba_image.create ~width:c.a.pw ~height:c.a.ph in
  let bg = file_background t e.path in
  fill img 0 0 c.a.pw c.a.ph dark;
  let q = float_of_int c.a.pw /. float_of_int t.cam.a.pw in
  let s = Code_street.scale (street_of t e) q in
  fill img 0 0 (int_of_float s.split) c.a.ph bg;
  let mark (g : Code_ground.t) (l : int) col =
    if l < Array.length g.places then
      let x0, y0, _, h = Code_ground.box g l in
      fill img (int_of_float (x0 -. (5. *. q))) (int_of_float y0) (int_of_float (x0 -. (2. *. q))) (int_of_float (y0 +. h)) col
  in
  (* the uses marked green in the focus's margin, the definitions used
   * red in the panels' *)
  List.iter (fun (ed : Code_street.edge) -> mark s.focus ed.from_line (90, 220, 120)) s.edges;
  Code_ground.paint img (Lazy.force e.file) s.focus ~bg ~aa;
  List.iter
    (fun (p : Code_street.panel) ->
      match List.find_opt (fun (x : entry) -> x.path = p.path) t.entries with
      | None -> ()
      | Some x ->
          let pbg = file_background t p.path in
          let x0 = int_of_float p.ground.ox and y0 = int_of_float (p.ground.oy -. (22. *. q)) in
          let y1 = match p.ground.places with [||] -> y0 | ps -> let l = ps.(Array.length ps - 1) in int_of_float (p.ground.oy +. l.y +. l.h) in
          fill img x0 y0 c.a.pw (y1 + 2) pbg;
          List.iter (fun (ed : Code_street.edge) -> if ed.target = p.path then mark p.ground ed.target_line (250, 80, 70)) s.edges;
          Code_ground.paint img (Lazy.force x.file) p.ground ~bg:pbg ~aa)
    s.panels;
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
                let edge = mix (archi t.colours p.path) 0.7 dark in
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
                paint_columns img c p.rect g e.nlines box (mix (archi t.colours p.path) 0.55 bg)
              end
          | File _, None -> ()))
    t.placed;
  img

(*****************************************************************************)
(* The names *)
(*****************************************************************************)

(* a name's place: its node, its box in the map's pixels, how much it
 * matters, and how to draw it *)
(* claude: words cut into lines of at most [width] characters *)
let wrap (width : int) (text : string) : string list =
  let words = List.filter (( <> ) "") (String.split_on_char ' ' text) in
  let lines, last =
    List.fold_left
      (fun (lines, cur) w -> if cur = "" then (lines, w) else if String.length cur + 1 + String.length w > width then (cur :: lines, w) else (lines, cur ^ " " ^ w))
      ([], "") words
  in
  List.rev (if last = "" then lines else last :: lines)

type name = { node : int; nbox : float * float * float * float; nrank : float; draw : shape; said : string list option }

(* claude: the capitals the configs name (Code_guide.capitals): where
 * each is in its file, found once (its file lexed then) *)
let capital_lines : (string, int option) Hashtbl.t = Hashtbl.create 16

let capital_line (e : entry) (at : string) : int option =
  let key = e.path ^ "\000" ^ at in
  match Hashtbl.find_opt capital_lines key with
  | Some l -> l
  | None ->
      let l = Result.to_option (Code_guide.find (Lazy.force e.file) at) in
      Hashtbl.replace capital_lines key l;
      l

(* a capital: a dot where it is and its name, what the config says of it
 * on its card; under the names of the regions and of their
 * subdirectories (a genre's name says more from afar), above the rest *)
let capitals (t : t) (c : camera) : name list =
  let a = c.a in
  let where = Hashtbl.create 64 in
  Array.iteri (fun i (p : entry Treemap.placed) -> match p.node with File (_, _, e) -> Hashtbl.replace where e.path (i, e) | Dir _ -> ()) t.placed;
  List.filter_map
    (fun (path, (it : Code_guide.item)) ->
      match Hashtbl.find_opt where path with
      | Some (i, e) when not (outside t t.placed.(i)) -> (
          match (clip c t.placed.(i).rect, t.geometry.(i)) with
          | Some _, Some g -> (
              match capital_line e it.at with
              | Some line ->
                  let x, y = line_pos t.placed.(i).rect g line in
                  let px = to_px c x and py = to_py c (y +. (g.cell_h /. 2.)) in
                  let label = snd (Code_guide.split it.at) |> fun s -> match String.index_opt s ':' with Some k -> String.sub s (k + 1) (String.length s - k - 1) | None -> s in
                  let size = 15. in
                  let tw = 0.5 *. size *. float_of_int (String.length label) in
                  let x0 = px -. 6. and x1 = px +. 10. +. tw +. 4. in
                  let dot = circle yellow 5. |> move (sx a px) (sy a py) in
                  let ring = circle black 7. |> move (sx a px) (sy a py) in
                  let text = words yellow label |> scale (size /. words_font_size) |> move (sx a (px +. 10. +. (tw /. 2.))) (sy a py) in
                  let shadow = words black label |> scale (size /. words_font_size) |> move (sx a (px +. 11.5 +. (tw /. 2.))) (sy a (py +. 1.5)) |> fade 0.8 in
                  let said = [ "* " ^ label ^ "   " ^ path ] @ (match it.say with Some s -> wrap 48 s | None -> []) @ [ "click: to its file" ] in
                  Some { node = i; nbox = (x0, py -. (size /. 2.) -. 2., x1, py +. (size /. 2.) +. 2.); nrank = 805.; draw = group [ ring; dot; shadow; text ]; said = Some said }
              | None -> None)
          | _ -> None)
      | _ -> None)
    (Code_guide.capitals t.guide)

(* the names over the map, the directories' first: a directory's centred
 * on it, as large as it fits (a region's up to 64, deeper ones smaller),
 * standing up when its block is tall and narrow; a file's smaller. Then
 * placed greedily, the most important first, none over another
 * (Code_map_base.place's way, the boxes kept for the mouse) *)
let names (t : t) (c : camera) : name list =
  let a = c.a in
  (* claude: the unit looked at and those holding it: named on the
   * breadcrumb, not over the map they fill (Code_units) *)
  let above = Code_units.ancestors t.placed t.focus in
  let crumbs =
    if t.focus = 0 then []
    else
      let x = ref 6. in
      List.mapi
        (fun k i ->
          let p = t.placed.(i) in
          let name = if i = 0 then (match String.index_opt t.title ':' with Some j -> String.sub t.title 0 j | None -> t.title) else (match p.node with Dir (n, _) | File (n, _, _) -> n) in
          let text = if k = 0 then name else "> " ^ name in
          let box, shape = tab a ~alpha:0.9 (lighter (archi t.colours p.path)) 16. !x 6. text in
          let _, _, x1, _ = box in
          x := x1 +. 4.;
          { node = i; nbox = box; nrank = 10000.; draw = shape; said = None })
        above
  in
  (* at the ground, the file is the map: only the breadcrumb *)
  let ground = at_ground t c <> None in
  let cands = ref (crumbs @ if ground then [] else capitals t c) in
  Array.iteri
    (fun i (p : entry Treemap.placed) ->
      match clip c p.rect with
      | Some (x0, y0, x1, y1) when (not ground) && p.depth > 0 && (not (List.mem i above)) && not (outside t p && (match p.node with File _ -> true | Dir _ -> false)) ->
          let w = float_of_int (x1 - x0) and h = float_of_int (y1 - y0) in
          let cx = (float_of_int x0 +. float_of_int x1) /. 2. and cy = (float_of_int y0 +. float_of_int y1) /. 2. in
          let is_dir, name = match p.node with Dir (n, _) -> (true, n) | File (n, _, _) -> (false, n) in
          let len = float_of_int (max 1 (String.length name)) in
          let cap = if not is_dir then 16. else match p.depth with 1 -> 64. | 2 -> 36. | _ -> 24. in
          let across = Float.min (w /. (0.55 *. len)) (Float.min (h /. 2.5) cap) in
          let upright = Float.min (h /. (0.55 *. len)) (Float.min (w /. 2.5) cap) in
          let stand = is_dir && upright > across *. 1.3 in
          let size = if stand then upright else across in
          let least = if is_dir then 11. else 10. in
          if size >= least then begin
            let tw = 0.5 *. size *. len in
            let bw, bh = if stand then (size, tw) else (tw, size) in
            let colour = if is_dir then (if p.depth = 1 then mix (archi t.colours p.path) 0.3 (245, 245, 250) else mix (archi t.colours p.path) 0.55 (235, 235, 245)) else archi t.colours p.path in
            let r, g, b = colour in
            let text dx dy col alpha = words col name |> scale (size /. words_font_size) |> (if stand then rotate 90. else Fun.id) |> move (sx a (cx +. dx)) (sy a (cy +. dy)) |> fade alpha in
            let draw =
              if is_dir then (if outside t p then text 0. 0. (rgb r g b) 0.4 else group [ text 2. 2. black 0.75; text 0. 0. (rgb r g b) 1. ])
              else text 0. 0. (lighter (r, g, b)) 0.85
            in
            let nrank = if is_dir then 1000. -. (100. *. float_of_int p.depth) +. size else size in
            cands := { node = i; nbox = (cx -. (bw /. 2.), cy -. (bh /. 2.), cx +. (bw /. 2.), cy +. (bh /. 2.)); nrank; draw; said = None } :: !cands
          end
      | _ -> ())
    t.placed;
  let overlaps (a0, b0, a1, b1) (c0, d0, c1, d1) = a0 < c1 && c0 < a1 && b0 < d1 && d0 < b1 in
  let on_map (x0, y0, x1, y1) = x0 >= 0. && y0 >= 0. && x1 <= float_of_int a.pw && y1 <= float_of_int a.ph in
  List.fold_left
    (fun kept n -> if on_map n.nbox && not (List.exists (fun k -> overlaps n.nbox k.nbox) kept) then n :: kept else kept)
    []
    (List.stable_sort (fun a b -> compare b.nrank a.nrank) !cands)

let within (x0, y0, x1, y1) x y = x >= x0 && x < x1 && y >= y0 && y < y1

let unit_at (t : t) (c : camera) (_ : float) (px : float) (py : float) : int option =
  Option.map (fun n -> n.node) (List.find_opt (fun n -> within n.nbox px py) (names t c))

(* a directory's or a file's card: its path, what its config says of it
 * (Code_guide), and what it holds *)
let card (t : t) (i : int) : string list =
  let p = t.placed.(i) in
  let said = function Some s -> wrap 48 s | None -> [] in
  match p.node with
  | File (_, _, e) -> (e.path :: said (Option.bind (Code_guide.file_note t.guide e.path) (fun n -> n.summary))) @ [ lines_text e.nlines ]
  | Dir (_, kids) ->
      let prefix = p.path ^ "/" in
      let n = String.length prefix in
      let files, lines =
        List.fold_left
          (fun (f, l) (e : entry) -> if String.length e.path > n && String.sub e.path 0 n = prefix then (f + 1, l + e.nlines) else (f, l))
          (0, 0) t.entries
      in
      let subdirs = List.length (List.filter (function Treemap.Dir _ -> true | File _ -> false) kids) in
      [ prefix ]
      @ said (Code_guide.dir_summary t.guide p.path)
      @ [
        Printf.sprintf "%d files, %s" files (lines_text lines);
        (if subdirs = 0 then "no subdirectory" else Printf.sprintf "%d subdirector%s" subdirs (if subdirs = 1 then "y" else "ies"));
        "click: fly into it";
      ]

(* the card of the name under the mouse, beside it, on the map *)
let hover_card (t : t) (c : camera) (kept : name list) : shape list =
  match t.pointer with
  | None -> []
  | Some (u, v) -> (
      let a = c.a in
      let mx = to_px c u and my = to_py c v in
      match List.find_opt (fun n -> within n.nbox mx my) kept with
      | None -> []
      | Some n ->
          let lines = match n.said with Some l -> l | None -> card t n.node in
          let size = 15. and gap = 6. in
          let w = 16. +. (0.5 *. size *. float_of_int (List.fold_left (fun m s -> max m (String.length s)) 0 lines)) in
          let h = 12. +. (float_of_int (List.length lines) *. (size +. gap)) in
          let x0 = Float.min (mx +. 18.) (float_of_int a.pw -. w -. 4.) and y0 = Float.min (my +. 18.) (float_of_int a.ph -. h -. 4.) in
          let col = lighter (archi t.colours t.placed.(n.node).path) in
          [
            rectangle (rgb 18 16 36) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.95;
          ]
          @ frame a col x0 y0 (x0 +. w) (y0 +. h) 1.5
          @ List.mapi
              (fun k s ->
                (* centred: the words' width is only estimated *)
                let y = y0 +. 6. +. (float_of_int k *. (size +. gap)) +. (size /. 2.) +. 3. in
                label a (if k = 0 then col else ink) size (x0 +. (w /. 2.)) y s)
              lines)

(* claude: at the ground, what the config says of an important line, a
 * note after its end when the column has room for it *)
let notes_on (t : t) (c : camera) (e : entry) (g : Code_ground.t) : shape list =
  let a = c.a in
  let f = Lazy.force e.file in
  List.filter_map
    (fun (l, _, say) ->
      match say with
      | None -> None
      | Some say ->
          let x0, y0, w, h = if l < Array.length g.places then Code_ground.box g l else (0., 0., 0., 0.) in
          (* the line's end: its last character *)
          let last = ref 0 in
          for k = 0 to Code_file.cols - 1 do
            let ch = Bytes.get f.chars ((l * Code_file.cols) + k) in
            if ch <> '\000' && ch <> ' ' then last := k + 1
          done;
          let cw = if h >= 7. then h /. 2. else w /. 80. in
          let size = 12. in
          let start = x0 +. (float_of_int !last *. cw) +. 12. in
          (* wrapped in the room after the line's end, three lines at most *)
          let room = int_of_float ((x0 +. w -. start) /. (0.5 *. size)) in
          (* not beside a line squeezed thin (a street panel's) *)
          if room < 16 || h < 7. then None
          else
            let lines = wrap room ("<- " ^ say) in
            let lines = if List.length lines > 3 then List.filteri (fun k _ -> k < 3) lines else lines in
            let n = float_of_int (List.length lines) in
            let tw = 0.5 *. size *. float_of_int (List.fold_left (fun m l -> max m (String.length l)) 0 lines) in
            let top = y0 +. (h /. 2.) -. (n *. (size +. 2.) /. 2.) in
            Some
              (group
                 ((rectangle (rgb 18 16 36) (tw +. 8.) ((n *. (size +. 2.)) +. 4.)
                  |> move (sx a (start +. (tw /. 2.))) (sy a (top +. (n *. (size +. 2.) /. 2.)))
                  |> fade 0.85)
                 :: List.mapi
                      (fun k l ->
                        let lw = 0.5 *. size *. float_of_int (String.length l) in
                        words (rgb 255 215 70) l |> scale (size /. words_font_size)
                        |> move (sx a (start +. (lw /. 2.))) (sy a (top +. ((float_of_int k +. 0.5) *. (size +. 2.)))))
                      lines)))
    (important t e)

let notes (t : t) (c : camera) (e : entry) : shape list = notes_on t c e (ground_of t e)

(* claude: at the street, each panel's title, the roads *)
let street_labels (t : t) (c : camera) (e : entry) : shape list =
  let a = c.a in
  let s = street_of t e in
  (* the line under the mouse: its roads lit, the others dimmed *)
  let hover = match t.pointer with Some (u, v) -> Code_street.line_at s ~focus_path:e.path (to_px c u) (to_py c v) | None -> None in
  Code_street.roads ?hover ~focus_path:e.path a s
  @ Code_street.ends ?hover ~focus_path:e.path a s
  (* claude: the configs' notes, the focus's and the panels' *)
  @ notes_on t c e s.focus
  @ List.concat_map
      (fun (p : Code_street.panel) -> match List.find_opt (fun (x : entry) -> x.path = p.path) t.entries with Some x -> notes_on t c x p.ground | None -> [])
      s.panels
  @ List.map
      (fun (p : Code_street.panel) ->
        let text = Printf.sprintf "%s   (%d use%s)" p.path p.count (if p.count = 1 then "" else "s") in
        let tw = 0.5 *. 14. *. float_of_int (String.length text) in
        label a (lighter (archi t.colours p.path)) 14. (p.ground.ox +. 8. +. (tw /. 2.)) (p.ground.oy -. 11.) text)
      s.panels
  @
  if s.panels = [] then [ label a dim 16. (s.split +. ((float_of_int a.pw -. s.split) /. 2.)) (float_of_int a.ph /. 2.) "(nothing of this map's other files used)" ]
  else []

(* claude: at the ground or the street, the line under the mouse framed *)
let line_lit (t : t) (c : camera) (e : entry) : shape list =
  match t.pointer with
  | None -> []
  | Some (u, v) -> (
      let mx = to_px c u and my = to_py c v in
      let grounds = if t.street then let s = street_of t e in s.focus :: List.map (fun (p : Code_street.panel) -> p.ground) s.panels else [ ground_of t e ] in
      match List.find_map (fun g -> Option.map (fun l -> (g, l)) (Code_ground.line_at g mx my)) grounds with
      | Some (g, l) ->
          let x, y, w, h = Code_ground.box g l in
          frame c.a (rgb 240 240 250) x y (x +. w) (y +. Float.max h 2.) 1.
      | None -> [])

(* claude: at the ground or the street, the name under the mouse bound
 * in its file: its binding pulsing cyan, its uses yellow, as on the
 * map read up close (Code_map.names_lit), placed where the lines are
 * laid out now (Code_ground), in the focus and in the panels; and a use
 * in a line too thin to read magnified while the mouse is there, a
 * callout (the author: "temporarily magnify the calls") -- not the
 * layout changed, which would move the line under the mouse *)
let names_glow (t : t) (c : camera) (e : entry) : shape list =
  match t.pointer with
  | None -> []
  | Some (u, v) -> (
      let a = c.a in
      let mx = to_px c u and my = to_py c v in
      let file p = List.find_map (fun (x : entry) -> if x.path = p then Some (Lazy.force x.file) else None) t.entries in
      let grounds =
        if t.street then
          let s = street_of t e in
          (e.path, s.focus) :: List.map (fun (p : Code_street.panel) -> (p.path, p.ground)) s.panels
        else [ (e.path, ground_of t e) ]
      in
      match List.find_map (fun (p, g) -> Option.map (fun l -> (p, g, l)) (Code_ground.line_at g mx my)) grounds with
      | None -> []
      | Some (p, g, l) -> (
          match file p with
          | None -> []
          | Some f -> (
              let x0, _, _, _ = Code_ground.box g l in
              let col = int_of_float ((mx -. x0) /. Code_ground.cell_w g l) in
              match Code_file.name_at f l col with
              | None -> []
              | Some o ->
                  let occurrences = List.filter (fun (w : Highlight_code.occurrence) -> w.line < Array.length g.places) (Code_file.uses f o) in
                  (* the binding pulsing cyan, the uses yellow *)
                  let lit =
                    List.concat_map
                      (fun (w : Highlight_code.occurrence) ->
                        let x, y, _, h = Code_ground.box g w.line in
                        let cw = Code_ground.cell_w g w.line in
                        let ww = float_of_int w.len *. cw and hh = Float.max 3. h in
                        let px = x +. (float_of_int w.col *. cw) +. (ww /. 2.) and py = y +. (h /. 2.) in
                        let binding = (w.line, w.col) = o.bound_at in
                        List.map (move (sx a px) (sy a py)) (Code_view.glow_at t.clock (if binding then rgb 0 225 255 else yellow) ww hh))
                      occurrences
                  in
                  (* a use in a line too thin to read: a callout, the line's
                   * words round it drawn readable over it, moved down when
                   * it would cover another *)
                  let size = 15. in
                  let placed = ref [] in
                  let callouts =
                    List.filter_map
                      (fun (w : Highlight_code.occurrence) ->
                        let x, y, cwidth, h = Code_ground.box g w.line in
                        if h >= 7. || (w.line, w.col) = o.bound_at then None
                        else
                          let text = String.map (fun ch -> if ch = '\000' then ' ' else ch) (Bytes.sub_string f.chars (w.line * Code_file.cols) Code_file.cols) in
                          (* a use past the grid's width (a long line, cut) shows the line's end *)
                          let from = max 0 (min (String.length text) (w.col - 24)) in
                          let snippet = String.trim (String.sub text from (max 0 (min 64 (String.length text - from)))) in
                          let snippet = (if from > 0 then "... " else "") ^ snippet in
                          let tw = (0.5 *. size *. float_of_int (String.length snippet)) +. 12. in
                          let bw = Float.min tw cwidth and bh = size +. 6. in
                          let bx = x +. 6. in
                          let overlaps by = List.exists (fun (ox, oy) -> Float.abs (oy -. by) < bh && Float.abs (ox -. bx) < bw) !placed in
                          let rec free by k = if k = 0 || not (overlaps by) then by else free (by +. bh +. 2.) (k - 1) in
                          let by = free (y +. (h /. 2.)) 8 in
                          placed := (bx, by) :: !placed;
                          let cx = bx +. (bw /. 2.) in
                          Some
                            (List.map (move (sx a cx) (sy a by)) (Code_view.glow_at t.clock yellow bw bh)
                            @ [
                                rectangle (rgb 18 16 36) bw bh |> move (sx a cx) (sy a by) |> fade 0.9;
                                words ink snippet |> scale (size /. words_font_size) |> move (sx a (bx +. 6. +. ((tw -. 12.) /. 2.))) (sy a by);
                              ]))
                      occurrences
                    |> List.concat
                  in
                  lit @ callouts)))

let labels (t : t) (c : camera) (_ : float) : shape list =
  let kept = names t c in
  (match at_ground t c with
  | Some e when t.street -> street_labels t c e @ line_lit t c e @ names_glow t c e
  | Some e -> notes t c e @ line_lit t c e @ names_glow t c e
  | None -> [])
  @ List.rev_map (fun n -> n.draw) kept @ hover_card t c kept

(* claude: at the ground, the line under a pixel (Code_ground's layout,
 * not the treemap's): what Enter opens, what the status line says *)
let pick (t : t) (c : camera) (_ : float) (px : float) (py : float) : (string * int * string) option =
  match at_ground t c with
  | Some e when t.street -> Option.map (fun (p, l) -> (p, l, "")) (Code_street.line_at (street_of t e) ~focus_path:e.path px py)
  | Some e -> Option.map (fun l -> (e.path, l, "")) (Code_ground.line_at (ground_of t e) px py)
  | None -> None

let style : style = { sname = "v2"; paint; labels; pick; unit_at; units = true }
