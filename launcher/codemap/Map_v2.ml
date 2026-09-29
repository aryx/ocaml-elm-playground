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
let street_cache : (string * int * int * int * Code_street.t) option ref = ref None

let street_mode (t : t) : Code_street.mode = match t.street_mode with 2 -> Users | 3 -> Both | _ -> Uses

let street_of (t : t) (e : entry) : Code_street.t =
  let a = t.cam.a in
  match !street_cache with
  | Some (p, w, h, m, s) when p = e.path && w = a.pw && h = a.ph && m = t.street_mode -> s
  | _ ->
      let f = Lazy.force e.file in
      let file p = List.find_map (fun (x : entry) -> if x.path = p then Some (Lazy.force x.file) else None) t.entries in
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
      match List.find_opt (fun (x : entry) -> x.path = p.path) t.entries with
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

type name = { node : int; nbox : float * float * float * float; nrank : float; draw : shape; said : string list option; sect : (string * int) option }

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
  (* claude: from a unit, the capitals of files at most two directories
   * below it: every directory described, all of them at once were a
   * rash of dots; flying in shows the deeper ones *)
  let top = t.placed.(t.focus).path in
  let depth p = if p = "" then 0 else List.length (String.split_on_char '/' p) in
  let near path =
    let d = Filename.dirname path in
    let d = if d = "." then "" else d in
    (top = "" && depth d <= 2) || (top <> "" && (d = top || Code_search.starts d (top ^ "/")) && depth d - depth top <= 2)
  in
  (* and a file's first only (the config's main one), fourteen a region
   * at most (the unit's immediate subdirectories) *)
  let region p =
    let rest = if top = "" then p else String.sub p (String.length top + 1) (max 0 (String.length p - String.length top - 1)) in
    match String.index_opt rest '/' with Some k -> String.sub rest 0 k | None -> rest
  in
  let seen_file = Hashtbl.create 64 and per_region = Hashtbl.create 16 in
  let fans = Lazy.force t.fan_in in
  let fan p = Option.value (Hashtbl.find_opt fans (String.capitalize_ascii (Filename.remove_extension (Filename.basename p)))) ~default:0 in
  let dir_of p = match Filename.dirname p with "." -> "" | d -> d in
  let lines p = match Hashtbl.find_opt where p with Some (_, (e : entry)) -> e.nlines | None -> 0 in
  let sorted = List.stable_sort (fun (p, _) (q, _) -> compare (fan q, lines q) (fan p, lines p)) (Code_guide.capitals t.guide) in
  (* claude: a module's .ml and .mli naming the same capital: one *)
  let seen_name = Hashtbl.create 64 in
  let modname p = Filename.remove_extension p in
  let chosen =
    List.filter
      (fun (path, (it : Code_guide.item)) ->
        near path
        (* a hub's capitals all (the core's: game, computer, shape) *)
        && (fan path >= 100 || not (Hashtbl.mem seen_file path))
        && (not (Hashtbl.mem seen_name (modname path, it.at)))
        && (Hashtbl.replace seen_name (modname path, it.at) (); true)
        &&
        let r = region path in
        let n = Option.value (Hashtbl.find_opt per_region r) ~default:0 in
        Hashtbl.replace seen_file path ();
        if n >= 14 then false else (Hashtbl.replace per_region r (n + 1); true))
      (* claude: the most central first, the files the most files name
       * (Code_deps.fan_in); and from more than a level above, only a file
       * some others depend on: a program nobody names is one of the
       * project's drivers, not its core (the author: "games and apps are
       * like device drivers in a linux kernel"), its capitals shown from
       * its genre *)
      (List.filter (fun (p, _) -> fan p >= 3 || depth (dir_of p) - depth top <= 1) sorted)
  in
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
                  (* claude: a name too short to say anything from afar (t), its module's with it *)
                  let label = if String.length label <= 2 then String.capitalize_ascii (Filename.remove_extension (Filename.basename path)) ^ "." ^ label else label in
                  (* claude: as large as central: the core's the map's largest *)
                  let size = match fan path with n when n >= 100 -> 24. | n when n >= 30 -> 19. | _ -> 15. in
                  let tw = 0.5 *. size *. float_of_int (String.length label) in
                  let x0 = px -. 6. and x1 = px +. 10. +. tw +. 4. in
                  let dot = circle yellow (size /. 3.) |> move (sx a px) (sy a py) in
                  let ring = circle black ((size /. 3.) +. 2.) |> move (sx a px) (sy a py) in
                  let text = words yellow label |> scale (size /. words_font_size) |> move (sx a (px +. 10. +. (tw /. 2.))) (sy a py) in
                  let shadow = words black label |> scale (size /. words_font_size) |> move (sx a (px +. 11.5 +. (tw /. 2.))) (sy a (py +. 1.5)) |> fade 0.8 in
                  let said = [ "* " ^ label ^ "   " ^ path ] @ (match it.say with Some s -> wrap 48 s | None -> []) @ [ "click: to its file" ] in
                  Some { node = i; nbox = (x0, py -. (size /. 2.) -. 2., x1, py +. (size /. 2.) +. 2.); nrank = 805. +. float_of_int (min 14 (fan path / 10)); draw = group [ ring; dot; shadow; text ]; said = Some said; sect = None }
              | None -> None)
          | _ -> None)
      | _ -> None)
    chosen

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
          { node = i; nbox = box; nrank = 10000.; draw = shape; said = None; sect = None })
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
          (* claude: a directory looked at, a file big enough on the map:
           * its name on a tab at its top, its card (what the configs say
           * of it), its table of contents (its sections where they are) --
           * what tells files apart, readable *)
          let card_file =
            match p.node with
            | File (_, _, e) when (match t.placed.(t.focus).node with Dir _ -> true | File _ -> false) && w >= 105. && h >= 70. && not (outside t p) -> Some e
            | _ -> None
          in
          (match card_file with
          | Some e ->
              let col = archi t.colours p.path in
              let fx0 = float_of_int x0 and fy0 = float_of_int y0 in
              let box, shape = tab a (lighter col) 15. (fx0 +. 3.) (fy0 +. 3.) name in
              cands := { node = i; nbox = box; nrank = 700.; draw = shape; said = None; sect = None } :: !cands;
              (* the card, under the tab, wrapped to the block *)
              (match Option.bind (Code_guide.file_note t.guide e.path) (fun n -> n.summary) with
              | Some said ->
                  let size = 13. in
                  let chars = max 12 (int_of_float ((w -. 16.) /. (0.5 *. size))) in
                  (* a narrow block, a line more *)
                  let lines = List.filteri (fun k _ -> k < if w < 180. then 4 else 3) (wrap chars said) in
                  let n = float_of_int (List.length lines) in
                  let top = fy0 +. 28. in
                  let lw = List.fold_left (fun m l -> Float.max m (0.5 *. size *. float_of_int (String.length l))) 0. lines in
                  let bh = n *. (size +. 3.) in
                  let draw =
                    group
                      ((rectangle (rgb 16 14 32) (lw +. 10.) (bh +. 6.) |> move (sx a (fx0 +. 6. +. (lw /. 2.))) (sy a (top +. (bh /. 2.))) |> fade 0.85)
                      :: List.mapi
                           (fun k l ->
                             let tw = 0.5 *. size *. float_of_int (String.length l) in
                             words ink l |> scale (size /. words_font_size) |> move (sx a (fx0 +. 8. +. (tw /. 2.))) (sy a (top +. ((float_of_int k +. 0.5) *. (size +. 3.)))))
                           lines)
                  in
                  cands := { node = i; nbox = (fx0 +. 4., top -. 3., fx0 +. 12. +. lw, top +. bh +. 3.); nrank = 820.; draw; said = None; sect = None } :: !cands
              | None -> ());
              (* the sections, each where it is in the columns *)
              (match t.geometry.(i) with
              | Some g ->
                  let f = Lazy.force e.file in
                  let r, gg, b = Highlight_code.rgb Comment_section in
                  List.iter
                    (fun (l, title, (cat : Highlight_code.category)) ->
                      let telling = String.length title < 36 && title <> "" && title.[0] <> '-' && title.[0] <> '*' && not (String.contains title '/') in
                      if cat = Comment_section && l > 0 && telling then begin
                        let lx, ly = line_pos p.rect g l in
                        let px = to_px c lx +. 4. and py = to_py c ly in
                        let size = 12. in
                        let tw = 0.5 *. size *. float_of_int (String.length title + 2) in
                        let text = "* " ^ title in
                        let draw =
                          group
                            [
                              rectangle (rgb 16 14 32) (tw +. 6.) (size +. 4.) |> move (sx a (px +. (tw /. 2.))) (sy a py) |> fade 0.8;
                              words (rgb r gg b) text |> scale (size /. words_font_size) |> move (sx a (px +. (tw /. 2.) +. 2.)) (sy a py);
                            ]
                        in
                        if px +. tw < float_of_int x1 then cands := { node = i; nbox = (px, py -. (size /. 2.) -. 2., px +. tw +. 6., py +. (size /. 2.) +. 2.); nrank = 400.; draw; said = None; sect = Some (e.path, l) } :: !cands
                      end)
                    f.defs
              | None -> ())
          | None ->
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
            cands := { node = i; nbox = (cx -. (bw /. 2.), cy -. (bh /. 2.), cx +. (bw /. 2.), cy +. (bh /. 2.)); nrank; draw; said = None; sect = None } :: !cands
          end)
      | _ -> ())
    t.placed;
  let overlaps (a0, b0, a1, b1) (c0, d0, c1, d1) = a0 < c1 && c0 < a1 && b0 < d1 && d0 < b1 in
  let on_map (x0, y0, x1, y1) = x0 >= 0. && y0 >= 0. && x1 <= float_of_int a.pw && y1 <= float_of_int a.ph in
  List.fold_left
    (fun kept n ->
      let free box = on_map box && not (List.exists (fun k -> overlaps box k.nbox) kept) in
      if free n.nbox then n :: kept
      else if n.said <> None && n.nrank >= 815. && n.nrank < 820. then begin
        (* claude: a hub's capital (fan-in 100 and more: ranked 815 and up)
         * that collides is nudged up or down a line or two, the core's
         * names shown near their place rather than not at all *)
        let x0, y0, x1, y1 = n.nbox in
        let h = y1 -. y0 in
        match List.find_opt (fun dy -> free (x0, y0 +. dy, x1, y1 +. dy)) [ h; -.h; 2. *. h; -2. *. h; 3. *. h; -3. *. h ] with
        | Some dy -> { n with nbox = (x0, y0 +. dy, x1, y1 +. dy); draw = n.draw |> move 0. (-.dy) } :: kept
        | None -> kept
      end
      else kept)
    []
    (List.stable_sort (fun a b -> compare b.nrank a.nrank) !cands)

let within (x0, y0, x1, y1) x y = x >= x0 && x < x1 && y >= y0 && y < y1

let unit_at (t : t) (c : camera) (_ : float) (px : float) (py : float) : int option =
  (* a section's title is not a unit: a click on it peeks (pick) *)
  Option.map (fun n -> n.node) (List.find_opt (fun n -> n.sect = None && within n.nbox px py) (names t c))

(* a directory's or a file's card: its path, what its config says of it
 * (Code_guide), and what it holds *)
let card (t : t) (i : int) : string * string list option =
  let p = t.placed.(i) in
  (* claude: what the configs say of it, its description; the counts were
   * not what one wants to know (the author) *)
  match p.node with
  | File (_, _, e) -> (e.path, Option.map (wrap 52) (Option.bind (Code_guide.file_note t.guide e.path) (fun n -> n.summary)))
  | Dir _ -> (p.path ^ "/", Option.map (wrap 52) (Code_guide.dir_summary t.guide p.path))

(* the card of the name under the mouse, beside it, on the map: its path,
 * and its description, readable; "not described yet" where no config
 * says anything of it (the configs to write) *)
let hover_card (t : t) (c : camera) (kept : name list) : shape list =
  match t.pointer with
  | None -> []
  | Some (u, v) -> (
      let a = c.a in
      let mx = to_px c u and my = to_py c v in
      match List.find_opt (fun n -> within n.nbox mx my) kept with
      | None -> []
      | Some n ->
          let title, body, described =
            match n.said with
            | Some (t0 :: rest) -> (t0, rest, true)
            | _ -> (
                match card t n.node with
                | title, Some lines -> (title, lines, true)
                | title, None -> (title, [ "not described yet" ], false))
          in
          let ts = 13. and size = 16. and gap = 6. in
          let width s z = text_width z s in
          let w = 24. +. Float.max (width title ts) (List.fold_left (fun m l -> Float.max m (width l size)) 0. body) in
          let h = 18. +. ts +. (float_of_int (List.length body) *. (size +. gap)) in
          let x0 = Float.min (mx +. 18.) (float_of_int a.pw -. w -. 4.) and y0 = Float.min (my +. 18.) (float_of_int a.ph -. h -. 4.) in
          let col = lighter (archi t.colours t.placed.(n.node).path) in
          [ rectangle (rgb 18 16 36) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.96 ]
          @ frame a col x0 y0 (x0 +. w) (y0 +. h) 1.5
          @ [ label a col ts (x0 +. 12. +. (width title ts /. 2.)) (y0 +. 6. +. (ts /. 2.)) title ]
          @ List.mapi
              (fun k l ->
                let y = y0 +. 12. +. ts +. (float_of_int k *. (size +. gap)) +. (size /. 2.) in
                (* claude: on their left, their widths estimated (text_width) *)
                label a (if described then ink else dim) size (x0 +. 12. +. (width l size /. 2.)) y l)
              body)

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
  let hover = match t.pointer with Some (u, v) -> Code_street.line_at s (to_px c u) (to_py c v) | None -> None in
  Code_street.roads ?hover a s
  @ Code_street.ends ?hover a s
  (* claude: the configs' notes, the focus's and the panels' *)
  @ notes_on t c e s.focus
  @ List.concat_map
      (fun (p : Code_street.panel) -> match List.find_opt (fun (x : entry) -> x.path = p.path) t.entries with Some x -> notes_on t c x p.ground | None -> [])
      (Code_street.panels s)
  @ List.map
      (fun (p : Code_street.panel) ->
        let text = Printf.sprintf "%s   (%d tie%s)" p.path p.count (if p.count = 1 then "" else "s") in
        let tw = 0.5 *. 14. *. float_of_int (String.length text) in
        label a (lighter (archi t.colours p.path)) 14. (p.ground.ox +. 8. +. (tw /. 2.)) (p.ground.oy -. 11.) text)
      (Code_street.panels s)
  (* the files tied but not shown, at the foot of their side *)
  @ (let foot more (x0 : float) =
       match more with
       | [] -> []
       | _ ->
           let n = List.length more in
           let text =
             Printf.sprintf "and %d more: %s%s" n
               (String.concat ", " (List.map (fun (p, k) -> Printf.sprintf "%s (%d)" (Filename.basename p) k) (List.filteri (fun i _ -> i < 4) more)))
               (if n > 4 then ", ..." else "")
           in
           [ label a dim 12. (x0 +. 8. +. (0.25 *. 12. *. float_of_int (String.length text))) (float_of_int a.ph -. 10.) text ]
     in
     foot s.left_more 0.
     @ (match s.right with p :: _ -> foot s.right_more p.ground.ox | [] -> foot s.right_more (float_of_int a.pw *. 0.6)))
  @
  if Code_street.panels s = [] then
    [ label a dim 16. (float_of_int a.pw /. 2.) 24. (match street_mode t with Uses -> "(it uses nothing of this map's other files)" | Users -> "(nothing of this map uses it)" | Both -> "(no tie with this map's other files)") ]
  else []

(* claude: at the ground or the street, the line under the mouse framed *)
let line_lit (t : t) (c : camera) (e : entry) : shape list =
  match t.pointer with
  | None -> []
  | Some (u, v) -> (
      let mx = to_px c u and my = to_py c v in
      let grounds = if t.street then let s = street_of t e in s.focus :: List.map (fun (p : Code_street.panel) -> p.ground) (Code_street.panels s) else [ ground_of t e ] in
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
(* claude: a name defined elsewhere, hovered at the ground or the street:
 * the first lines of its definition beside the mouse, readable (the
 * author: "when we hover a use where the entity is not in the view ... a
 * simple hover should probably again show the external def"); a click
 * peeks at all of it *)
let preview_cache : (string * int * float, Rgba_image.t) Hashtbl.t = Hashtbl.create 16

(* claude: a card of code beside the mouse: [path]'s lines [first] to
 * [lastl], readable, painted once (preview_cache), [lit] a line tinted
 * in a colour (a match's) *)
let code_card ?lit (t : t) (c : camera) (path : string) (first : int) (lastl : int) (title : string) (mx : float) (my : float) : shape list =
  match List.find_opt (fun (x : entry) -> x.path = path) (t.entries @ t.beyond) with
  | None -> []
  | Some x ->
      let a = c.a in
      let g = Lazy.force x.file in
      let n = Code_file.nlines g in
      let first = max 0 first and lastl = min (n - 1) lastl in
      let lines = lastl - first + 1 in
      let iw = 560. and ih = float_of_int lines *. 15. in
      let q = Playground_platform.pixel_ratio () in
      let img =
        match Hashtbl.find_opt preview_cache (path, first, q) with
        | Some img when img.height = int_of_float (ih *. q) -> img
        | _ ->
            let weights = Array.init n (fun k -> if k >= first && k <= lastl then 1. else 0.) in
            let lay = Code_ground.layout weights ~pw:(int_of_float iw) ~ph:(int_of_float ih) in
            let img = Rgba_image.create ~width:(int_of_float (iw *. q)) ~height:(int_of_float (ih *. q)) in
            let bg = (22, 20, 38) in
            fill img 0 0 img.width img.height bg;
            Code_ground.paint img g (Code_ground.scale lay q) ~bg ~aa:true;
            Hashtbl.replace preview_cache (path, first, q) img;
            img
      in
      let bw = iw +. 16. and bh = ih +. 34. in
      let x0 = Float.min (mx +. 20.) (float_of_int a.pw -. bw -. 6.) and y0 = Float.min (my +. 20.) (float_of_int a.ph -. bh -. 6.) in
      let rr, gg, bb = archi t.colours path in
      [ rectangle (rgb 22 20 38) bw bh |> move (sx a (x0 +. (bw /. 2.))) (sy a (y0 +. (bh /. 2.))) |> fade 0.97 ]
      @ frame a (lighter (rr, gg, bb)) x0 y0 (x0 +. bw) (y0 +. bh) 1.5
      @ [
          label a (lighter (rr, gg, bb)) 13. (x0 +. 8. +. (text_width 13. title /. 2.)) (y0 +. 13.) title;
          bitmap iw ih img |> move (sx a (x0 +. 8. +. (iw /. 2.))) (sy a (y0 +. 26. +. (ih /. 2.)));
        ]
      @
      match lit with
      | Some (l, colour) when l >= first && l <= lastl ->
          let y = y0 +. 26. +. (float_of_int (l - first) *. 15.) +. 7.5 in
          [ rectangle colour iw 15. |> move (sx a (x0 +. 8. +. (iw /. 2.))) (sy a y) |> fade 0.25 ]
      | _ -> []

let preview (t : t) (c : camera) (path : string) (f : Code_file.t) (l : int) (col : int) (mx : float) (my : float) : shape list =
  match Code_file.ref_at f l col with
  | None -> []
  | Some r -> (
      match Code_names.find_in ~roots:t.roots (index_of t) ~from:path f r with
      | (cand : Code_names.candidate) :: _, _ -> (
          match List.find_opt (fun (x : entry) -> x.path = cand.path) (t.entries @ t.beyond) with
          | None -> []
          | Some x ->
              let g = Lazy.force x.file in
              let n = Code_file.nlines g in
              (* its first lines, to a blank line, eight at most *)
              let rec last k = if k >= n - 1 || k - cand.line >= 7 then k else if Bytes.for_all (fun ch -> ch = '\000' || ch = ' ') (Bytes.sub g.chars ((k + 1) * Code_file.cols) Code_file.cols) then k else last (k + 1) in
              code_card t c cand.path cand.line (last cand.line) (Printf.sprintf "%s:%d   (click: all of it)" cand.path (cand.line + 1)) mx my)
      | [], _ -> [])

let names_glow (t : t) (c : camera) (e : entry) : shape list =
  match t.pointer with
  | None -> []
  | Some _ when t.peek <> None -> []
  | Some (u, v) -> (
      let a = c.a in
      let mx = to_px c u and my = to_py c v in
      let file p = List.find_map (fun (x : entry) -> if x.path = p then Some (Lazy.force x.file) else None) t.entries in
      let grounds =
        if t.street then
          let s = street_of t e in
          (e.path, s.focus) :: List.map (fun (p : Code_street.panel) -> (p.path, p.ground)) (Code_street.panels s)
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
              | None -> preview t c p f l col mx my
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

(*****************************************************************************)
(* The skeletons *)
(*****************************************************************************)

(* claude: the skeletons (Code_guide.skeleton, x), at every level: from
 * afar, a bone a dot at its line in its file's columns; at the ground and
 * the street, its definition lit in the shaded file; the joints between
 * the bones on the map, ivory roads, their direction the road's taper;
 * an end off the map, a stub to the map's edge naming it *)
let ivory = (245, 232, 200)

(* the grounds on the map now, a file's path and its layout: the focus's
 * and, at the street, the panels' *)
let grounds (t : t) (e : entry) : (string * Code_ground.t) list =
  if t.street then
    let s = street_of t e in
    (e.path, s.focus) :: List.map (fun (p : Code_street.panel) -> (p.path, p.ground)) (Code_street.panels s)
  else [ (e.path, ground_of t e) ]

let entry_of (t : t) (path : string) : entry option = List.find_opt (fun (x : entry) -> x.path = path) (t.entries @ t.beyond)

(* where a file's line is on the map now: its left, its middle's height,
 * the end of its text *)
let spot (t : t) (c : camera) (path : string) (line : int) : (float * float * float) option =
  match at_ground t c with
  | Some e -> (
      match List.assoc_opt path (grounds t e) with
      | Some g when line < Array.length g.places ->
          let x, y, w, h = Code_ground.box g line in
          let f = Option.map (fun (x : entry) -> Lazy.force x.file) (entry_of t path) in
          let last = ref 0 in
          Option.iter (fun (f : Code_file.t) -> for k = 0 to Code_file.cols - 1 do if Bytes.get f.chars ((line * Code_file.cols) + k) <> '\000' && Bytes.get f.chars ((line * Code_file.cols) + k) <> ' ' then last := k + 1 done) f;
          Some (x, y +. (h /. 2.), Float.min (x +. w) (x +. (float_of_int !last *. Code_ground.cell_w g line)))
      | _ -> None)
  | None -> (
      let found = ref None in
      Array.iteri (fun i (p : entry Treemap.placed) -> match p.node with File (_, _, x) when x.path = path -> found := Some i | _ -> ()) t.placed;
      match !found with
      | Some i -> (
          match (clip c t.placed.(i).rect, t.geometry.(i)) with
          | Some _, Some g ->
              let x, y = line_pos t.placed.(i).rect g line in
              let px = to_px c x and py = to_py c (y +. (g.cell_h /. 2.)) in
              Some (px, py, px)
          | _ -> None)
      | None -> None)

(* a bone's line in its file, found once; a whole file's or directory's
 * none *)
let bone_line (t : t) (b : Code_guide.bone) : int option =
  if b.banchor = "" then None else match entry_of t b.bpath with Some e -> capital_line e b.banchor | None -> None

(* where a whole file or directory is on the map: its top left corner, a
 * little in (its name is at its centre) *)
let unit_spot (t : t) (c : camera) (path : string) : (float * float * float) option =
  let found = ref None in
  Array.iteri (fun i (p : entry Treemap.placed) -> if p.path = path then found := Some i) t.placed;
  match !found with
  | Some i -> (
      match clip c t.placed.(i).rect with
      | Some (x0, y0, x1, y1) when x1 - x0 > 30 && y1 - y0 > 30 ->
          let x = float_of_int x0 +. 26. and y = float_of_int y0 +. 30. in
          Some (x, y, x)
      | _ -> None)
  | None -> None

(* a definition's lines: from its header to the next top-level one *)
let extent (f : Code_file.t) (line : int) : int * int =
  let next = List.fold_left (fun acc (l, _, (cat : Highlight_code.category)) -> if l > line && l < acc && cat <> Comment_section then l else acc) (Code_file.nlines f) f.defs in
  (line, next - 1)

(* claude: the blood, pulses running along a joint in its direction,
 * three to a joint, a lap every two seconds *)
let blood (a : area) (clock : float) (pts : (float * float) list) : shape list =
  let pts = Array.of_list pts in
  let n = Array.length pts in
  if n < 2 then []
  else
    let r, g, b = Code_anatomy.colour Blood in
    List.init 3 (fun k ->
        let phase = Float.rem ((clock *. 0.5) +. (float_of_int k /. 3.)) 1. in
        let x, y = pts.(min (n - 1) (int_of_float (phase *. float_of_int (n - 1)))) in
        [ circle (rgb 40 0 10) 6. |> move (sx a x) (sy a y); circle (rgb r g b) 4.5 |> move (sx a x) (sy a y) ])
    |> List.concat

(* claude: the bones drawn last, their dot's place and their role's
 * width, for a hover (bone_card) and a click (Code_map) *)
let drawn_bones : (Code_guide.bone * float * float * float) list ref = ref []

let skeleton_shapes (t : t) (c : camera) : shape list =
  drawn_bones := [];
  let a = c.a in
  let ground = at_ground t c in
  let all = List.concat_map (fun (d : Code_guide.dir_note) -> d.skeletons) (Code_guide.dirs t.guide) in
  (* at the ground, the skeletons with a bone on the map *)
  let bone_spot (b : Code_guide.bone) =
    if b.banchor = "" then (if ground = None then Option.map (fun s -> (0, s)) (unit_spot t c b.bpath) else None)
    else Option.bind (bone_line t b) (fun l -> Option.map (fun s -> (l, s)) (spot t c b.bpath l))
  in
  (* the skeletons at hand: at the ground and the street, the file's
   * (a bone in it); from afar, the unit's config's, else the nearest
   * directory's above that has some. One at a time, x going to the next
   * and past the last turning the X-ray off; the deeper directories'
   * packed, a dot each, named: fly in to spread them *)
  let here = t.placed.(t.focus).path in
  let parent d = match String.rindex_opt d '/' with Some i -> String.sub d 0 i | None -> "" in
  let under d p = d = "" || p = d || (String.length p > String.length d && String.sub p 0 (String.length d + 1) = d ^ "/") in
  (* a skeleton inside one file is that file's: spread at its ground,
   * a dot from afar; a region's are the ones spanning its files *)
  let one_file (s : Code_guide.skeleton) =
    match s.bones with b :: rest -> b.banchor <> "" && List.for_all (fun (x : Code_guide.bone) -> x.bpath = b.bpath && x.banchor <> "") rest | [] -> false
  in
  let candidates =
    match ground with
    | Some e -> List.filter (fun (s : Code_guide.skeleton) -> List.exists (fun (b : Code_guide.bone) -> b.bpath = e.path) s.bones) all
    | None ->
        (* above the unit, only a skeleton with two bones on the map: one
         * mostly off it would be stubs *)
        let seen (s : Code_guide.skeleton) = List.length (List.filter (fun b -> bone_spot b <> None) s.bones) >= 2 in
        let rec level d =
          let l = List.filter (fun (s : Code_guide.skeleton) -> s.sdir = d && (not (one_file s)) && (d = here || seen s)) all in
          match l with [] when d <> "" -> level (parent d) | l -> l
        in
        level here
  in
  let k = List.length candidates in
  if t.xray_n >= max 1 k then begin
    t.xray <- false;
    t.xray_n <- 0
  end;
  let shown = match List.nth_opt candidates t.xray_n with Some s -> [ s ] | None -> [] in
  let deeper =
    if ground <> None then []
    (* claude: only a level down (the unit's own, or its subdirectories'):
     * with every directory described, all the levels' dots at once hid
     * the map; flying in shows the next level's *)
    else
      List.filter
        (fun (s : Code_guide.skeleton) -> (one_file s || s.sdir <> here) && (s.sdir = here || parent s.sdir = here) && under here s.sdir && not (List.memq s candidates))
        all
  in
  let banner =
    match shown with
    | [ s ] ->
        let text = Printf.sprintf "X-ray: %s   (%d/%d, x: %s)" s.sname (t.xray_n + 1) k (if t.xray_n + 1 < k then "the next" else "off") in
        let tw = 0.5 *. 16. *. float_of_int (String.length text) in
        [
          (* at the map's foot: the bones sit at the regions' top corners *)
          rectangle (rgb 18 16 36) (tw +. 20.) 26. |> move (sx a (float_of_int a.pw /. 2.)) (sy a (float_of_int a.ph -. 16.)) |> fade 0.92;
          words (let r, g, b = ivory in rgb r g b) text |> scale (16. /. words_font_size) |> move (sx a (float_of_int a.pw /. 2.)) (sy a (float_of_int a.ph -. 16.));
        ]
    | _ -> []
  in
  (* from afar, a file whose bones are a few pixels apart (TinyInvaders'
   * five) is one dot, its skeleton's name, the joints to other files
   * leaving from it: coming nearer spreads it *)
  let packed_files =
    if ground <> None then []
    else
      let by_file = Hashtbl.create 8 in
      List.iter
        (fun (sk : Code_guide.skeleton) ->
          List.iter
            (fun (b : Code_guide.bone) ->
              match bone_spot b with
              | Some (_, (x, y, _)) -> Hashtbl.replace by_file b.bpath ((x, y, sk.sname) :: Option.value (Hashtbl.find_opt by_file b.bpath) ~default:[])
              | None -> ())
            sk.bones)
        shown;
      Hashtbl.fold
        (fun path spots acc ->
          let xs = List.map (fun (x, _, _) -> x) spots and ys = List.map (fun (_, y, _) -> y) spots in
          let lo l = List.fold_left Float.min Float.infinity l and hi l = List.fold_left Float.max Float.neg_infinity l in
          if List.length spots >= 2 && hi xs -. lo xs +. (hi ys -. lo ys) < 60. then
            (path, ((lo xs +. hi xs) /. 2., (lo ys +. hi ys) /. 2., List.sort_uniq compare (List.map (fun (_, _, n) -> n) spots))) :: acc
          else acc)
        by_file []
  in
  let packed_at path = List.assoc_opt path packed_files in
  (* a bone's place: its file's dot when packed *)
  let bone_spot (b : Code_guide.bone) =
    match (packed_at b.bpath, bone_spot b) with Some (x, y, _), Some (l, _) -> Some (l, (x +. 12., y, x +. 12.)) | _, s -> s
  in
  let skeleton_on = List.mem Code_anatomy.Skeleton !Code_anatomy.shown and blood_on = List.mem Code_anatomy.Blood !Code_anatomy.shown in
  let r, g, b = ivory in
  let ink_i = rgb r g b in
  let bones = List.concat_map (fun (s : Code_guide.skeleton) -> s.bones) shown in
  (* the shade: from afar the whole map; at the ground, every line but
   * the bones' definitions *)
  let shade =
    match ground with
    | None -> [ rectangle (rgb 8 6 20) (float_of_int a.pw) (float_of_int a.ph) |> move (sx a (float_of_int a.pw /. 2.)) (sy a (float_of_int a.ph /. 2.)) |> fade 0.6 ]
    | Some e ->
        List.concat_map
          (fun (path, (gr : Code_ground.t)) ->
            match entry_of t path with
            | None -> []
            | Some x ->
                let f = Lazy.force x.file in
                let lit = List.filter_map (fun (bn : Code_guide.bone) -> if bn.bpath = path then Option.map (extent f) (bone_line t bn) else None) bones in
                List.concat
                  (List.init (Array.length gr.places) (fun l ->
                       if List.exists (fun (s, e) -> l >= s && l <= e) lit then []
                       else
                         let x0, y0, w, h = Code_ground.box gr l in
                         [ rectangle (rgb 8 6 20) (w +. 16.) (h +. 0.5) |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.72 ])))
          (grounds t e)
  in
  (* the joints, bent one way or the other so that a -> b and b -> a (a
   * loop, the model and its update) are two roads *)
  let where at = match List.find_opt (fun (bn : Code_guide.bone) -> bn.bat = at) bones with Some bn -> Option.map (fun (l, s) -> (bn, l, s)) (bone_spot bn) |> fun x -> (bn, x) |> Option.some | None -> None in
  let stubs = ref [] in
  let joints =
    List.concat_map
      (fun (s : Code_guide.skeleton) ->
        List.concat_map
          (fun (j : Code_guide.joint) ->
            let same_pack = match (where j.jfrom, where j.jto) with Some (x, _), Some (y, _) -> x.bpath = y.bpath && packed_at x.bpath <> None | _ -> false in
            match (where j.jfrom, where j.jto) with
            | _ when same_pack -> []
            | Some (_, Some (_, _, (ax0, ay, _))), Some (_, Some (_, _, (bx0, by, _))) ->
                (* from dot to dot *)
                let ax = ax0 -. 12. and bx = bx0 -. 12. in
                let dx = bx -. ax and dy = by -. ay in
                let len = Float.max 1. (Float.sqrt ((dx *. dx) +. (dy *. dy))) in
                (* the perpendicular turns with the direction: a -> b and b -> a
                 * bend to opposite sides by themselves, a loop drawn as two *)
                let bend = Float.min 70. (Float.max 30. (0.12 *. len)) in
                let mx = ((ax +. bx) /. 2.) +. (-.dy /. len *. bend) and my = ((ay +. by) /. 2.) +. (dx /. len *. bend) in
                let pts = Map_atlas.bspline [| (ax, ay); (mx, my); (bx, by) |] in
                (if skeleton_on then Map_atlas.road ~colours:(ivory, (200, 170, 110)) a pts 6. 0.85 else [])
                @ (if blood_on then blood a t.clock pts else [])
                @ (match j.jsay with Some w when skeleton_on || blood_on -> [ words ink_i w |> scale (13. /. words_font_size) |> move (sx a mx) (sy a my) ] | _ -> [])
            | Some (_, Some (_, _, (ax0, ay, _))), Some (bn, None) | Some (bn, None), Some (_, Some (_, _, (ax0, ay, _))) ->
                (* an end off the map: a stub to its port on the edge (below) *)
                stubs := (ax0 -. 12., ay, bn) :: !stubs;
                []
            | _ -> [])
          s.joints)
      shown
  in
  (* the bones: a dot and their role *)
  let marks =
    List.concat_map
      (fun (bn : Code_guide.bone) ->
        match bone_spot bn with
        | _ when packed_at bn.bpath <> None -> []
        | None -> []
        | Some (_, (x, y, _)) ->
            let size = 14. in
            let tw = 0.5 *. size *. float_of_int (String.length bn.role) in
            (* above the header's start, over the shaded line before it *)
            let lx = x in
            drawn_bones := (bn, x -. 12., y, tw) :: !drawn_bones;
            [
              circle (rgb 20 16 30) 8. |> move (sx a (x -. 12.)) (sy a y);
              circle ink_i 6. |> move (sx a (x -. 12.)) (sy a y);
              rectangle (rgb 18 16 36) (tw +. 10.) (size +. 6.) |> move (sx a (lx +. (tw /. 2.))) (sy a (y -. 24.)) |> fade 0.9;
              words ink_i bn.role |> scale (size /. words_font_size) |> move (sx a (lx +. (tw /. 2.))) (sy a (y -. 24.));
            ])
      bones
  in
  let dots =
    List.concat_map
      (fun (_, (x, y, names)) ->
        let name = String.concat ", " names in
        let tw = 0.5 *. 14. *. float_of_int (String.length name) in
        [
          circle (rgb 20 16 30) 10. |> move (sx a x) (sy a y);
          circle ink_i 8. |> move (sx a x) (sy a y);
          rectangle (rgb 18 16 36) (tw +. 10.) 20. |> move (sx a (x +. 16. +. (tw /. 2.))) (sy a (y -. 18.)) |> fade 0.9;
          words ink_i name |> scale (14. /. words_font_size) |> move (sx a (x +. 16. +. (tw /. 2.))) (sy a (y -. 18.));
        ])
      packed_files
  in
  let deep_dots =
    List.concat_map
      (fun (s : Code_guide.skeleton) ->
        let spots = List.filter_map (fun b -> Option.map snd (bone_spot b)) s.bones in
        match spots with
        | [] -> []
        | _ ->
            let n = float_of_int (List.length spots) in
            let x = List.fold_left (fun acc (x, _, _) -> acc +. x) 0. spots /. n and y = List.fold_left (fun acc (_, y, _) -> acc +. y) 0. spots /. n in
            let tw = 0.5 *. 13. *. float_of_int (String.length s.sname) in
            (* a file's skeleton a small dot, its name the file's capital's
             * business: a hundred games must not crowd the map *)
            if one_file s then [ circle (rgb 20 16 30) 6. |> move (sx a x) (sy a y); circle ink_i 4. |> move (sx a x) (sy a y) ]
            else
            [
              circle (rgb 20 16 30) 9. |> move (sx a x) (sy a y);
              circle ink_i 7. |> move (sx a x) (sy a y);
              rectangle (rgb 18 16 36) (tw +. 10.) 18. |> move (sx a (x +. 14. +. (tw /. 2.))) (sy a (y -. 16.)) |> fade 0.85;
              words ink_i s.sname |> scale (13. /. words_font_size) |> move (sx a (x +. 14. +. (tw /. 2.))) (sy a (y -. 16.));
            ])
      deeper
  in
  (* the ends off the map: a port each on the map's right edge, at the
   * height of the stubs going to it, the ports spread so that their names
   * do not overlap; each named once *)
  let ports =
    let by = Hashtbl.create 8 in
    List.iter (fun (x, y, (bn : Code_guide.bone)) -> Hashtbl.replace by bn.bat ((x, y, bn) :: Option.value (Hashtbl.find_opt by bn.bat) ~default:[])) !stubs;
    let ps = Hashtbl.fold (fun _ l acc -> let (_, _, bn) = List.hd l in (bn, l, List.fold_left (fun m (_, y, _) -> m +. y) 0. l /. float_of_int (List.length l)) :: acc) by [] in
    let ps = List.sort (fun (_, _, y) (_, _, y') -> compare y y') ps in
    let last = ref neg_infinity in
    List.map (fun (bn, l, y) -> let y = Float.max y (!last +. 24.) in last := y; (bn, l, y)) ps
  in
  let ex = float_of_int a.pw -. 20. in
  let stub_shapes =
    List.concat_map
      (fun ((bn : Code_guide.bone), l, py) ->
        (* a definition: its name and file; a whole unit: its path and
         * what it is for *)
        let text = if bn.banchor = "" then Printf.sprintf "%s: %s" bn.bpath bn.role else Printf.sprintf "%s  %s" (snd (Code_guide.split bn.bat)) bn.bpath in
        let tw = 0.5 *. 13. *. float_of_int (String.length text) in
        List.concat_map
          (fun (x, y, _) ->
            let pts = Map_atlas.bspline [| (x, y); ((x +. ex) /. 2., ((y +. py) /. 2.) -. 30.); (ex, py) |] in
            (if skeleton_on then Map_atlas.road ~colours:(ivory, (200, 170, 110)) a pts 4. 0.6 else []) @ if blood_on then blood a t.clock pts else [])
          l
        @
        if skeleton_on then
          [
            rectangle (rgb 18 16 36) (tw +. 10.) 18. |> move (sx a (ex -. 4. -. (tw /. 2.))) (sy a (py +. 13.)) |> fade 0.9;
            words ink_i text |> scale (13. /. words_font_size) |> move (sx a (ex -. 4. -. (tw /. 2.))) (sy a (py +. 13.));
          ]
        else [])
      ports
  in
  let none =
    if shown = [] && deeper = [] && skeleton_on then [ label a dim 16. (float_of_int a.pw /. 2.) 30. "(no skeleton here: the configs name none)" ] else []
  in
  shade @ joints @ stub_shapes @ (if skeleton_on then marks @ dots @ deep_dots @ banner else []) @ none

(* claude: the anatomy's other plates (Code_anatomy): each file's facts,
 * found a few files a frame from afar (the whole repository's X-ray
 * opening at once), kept *)
let facts_cache : (string, Code_anatomy.facts) Hashtbl.t = Hashtbl.create 256

let facts_of (t : t) ?(budget = ref max_int) (e : entry) : Code_anatomy.facts option =
  match Hashtbl.find_opt facts_cache e.path with
  | Some f -> Some f
  | None when !budget <= 0 -> None
  | None ->
      decr budget;
      let public =
        if Filename.check_suffix e.path ".ml" then Option.map (fun (m : entry) -> Code_anatomy.public_names (Lazy.force m.file)) (entry_of t (e.path ^ "i")) else None
      in
      let f = Code_anatomy.facts (Lazy.force e.file) ~public in
      Hashtbl.replace facts_cache e.path f;
      Some f

let anatomy_shapes (t : t) (c : camera) : shape list =
  let a = c.a in
  let on s = List.mem s !Code_anatomy.shown in
  let col s = let r, g, b = Code_anatomy.colour s in rgb r g b in
  let tint s alpha (x0, y0, w, h) = rectangle (col s) w (Float.max 1.5 h) |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade alpha in
  match at_ground t c with
  | Some e ->
      (* at the ground and the street: the lines, tinted *)
      List.concat_map
        (fun (path, (g : Code_ground.t)) ->
          match Option.bind (entry_of t path) (fun x -> facts_of t x) with
          | None -> []
          | Some (fs : Code_anatomy.facts) ->
              let box l = if l < Array.length g.places then Some (Code_ground.box g l) else None in
              let lines s alpha ls = List.filter_map (fun l -> Option.map (tint s alpha) (box l)) ls in
              (if on Muscles then
                 List.concat_map (fun (a0, b0, st) -> if st < 0.35 then [] else lines Muscles (0.06 +. (0.3 *. Float.min 1. st)) (List.init (b0 - a0 + 1) (fun k -> a0 + k))) fs.muscles
               else [])
              @ (if on Nerves then lines Nerves 0.4 fs.nerves else [])
              @ (if on Lungs then lines Lungs 0.4 fs.lungs else [])
              @
              if on Skin then
                List.filter_map (fun l -> Option.map (fun (x0, y0, _, h) -> rectangle (col Skin) 5. (Float.max 3. h) |> move (sx a (x0 -. 12.)) (sy a (y0 +. (h /. 2.)))) (box l)) fs.skin
              else [])
        (grounds t e)
  | None ->
      (* from afar: a file tinted by its muscles, a dot for its nerves and
       * one for its lungs, as big as they are many, its skin a frame *)
      let budget = ref 30 in
      (* the muscles relative: the strongest sixth of the files known *)
      (* a file's strength: its definitions', weighed by their lines *)
      let strength (fs : Code_anatomy.facts) =
        let w, n = List.fold_left (fun (w, n) (a0, b0, st) -> let k = float_of_int (b0 - a0 + 1) in (w +. (st *. k), n +. k)) (0., 0.) fs.muscles in
        if n = 0. then 0. else w /. n
      in
      let all = Hashtbl.fold (fun _ fs acc -> strength fs :: acc) facts_cache [] |> List.sort compare |> Array.of_list in
      let cut = if Array.length all = 0 then 1. else all.(min (Array.length all - 1) (Array.length all * 85 / 100)) in
      Array.to_list t.placed
      |> List.concat_map (fun (p : entry Treemap.placed) ->
             match (p.node, clip c p.rect) with
             | File (_, _, e), Some (x0, y0, x1, y1) when not (outside t p) -> (
                 match facts_of t ~budget e with
                 | None -> []
                 | Some fs ->
                     let x0 = float_of_int x0 and y0 = float_of_int y0 and x1 = float_of_int x1 and y1 = float_of_int y1 in
                     let w = x1 -. x0 and h = y1 -. y0 in
                     let strongest = strength fs in
                     let dot s k i =
                       if k = 0 then []
                       else
                         let r = Float.min (Float.min w h /. 3.) (2. +. Float.sqrt (float_of_int k)) in
                         [ circle (col s) r |> move (sx a (x0 +. 3. +. r +. (float_of_int i *. ((2. *. r) +. 2.)))) (sy a (y0 +. 3. +. r)) |> fade 0.9 ]
                     in
                     (* the strongest files only: the heavy lifters *)
                     (if on Muscles && strongest > cut && strongest > 0. then [ tint Muscles (Float.min 0.7 (0.25 +. (0.45 *. ((strongest -. cut) /. Float.max 0.01 cut)))) (x0, y0, w, h) ] else [])
                     @ (if on Nerves then dot Nerves (List.length fs.nerves) 0 else [])
                     @ (if on Lungs then dot Lungs (List.length fs.lungs) 1 else [])
                     @ if on Skin && fs.skin <> [] then List.map (fade 0.45) (frame a (col Skin) x0 y0 x1 y1 1.) else [])
             | _ -> [])

(* the atlas's key: the plates, the ones shown bright *)
let legend (c : camera) : shape list =
  let a = c.a in
  let x0 = float_of_int a.pw -. 420. and y0 = 36. in
  let row i s =
    let r, g, b = Code_anatomy.colour s in
    let on = List.mem s !Code_anatomy.shown in
    let y = y0 +. 22. +. (float_of_int i *. 20.) in
    let text = Printf.sprintf "%s  %s: %s" (Code_anatomy.key s) (Code_anatomy.name s) (Code_anatomy.meaning s) in
    [
      circle (rgb r g b) 6. |> move (sx a (x0 +. 14.)) (sy a y) |> fade (if on then 1. else 0.25);
      words (if on then ink else dim) text |> scale (14. /. words_font_size) |> move (sx a (x0 +. 30. +. (0.25 *. 14. *. float_of_int (String.length text)))) (sy a y);
    ]
  in
  (rectangle (rgb 18 16 36) 410. 150. |> move (sx a (x0 +. 205.)) (sy a (y0 +. 70.)) |> fade 0.92)
  :: (words ink "the X-ray (x)" |> scale (14. /. words_font_size) |> move (sx a (x0 +. 60.)) (sy a (y0 +. 4.)))
  :: List.concat (List.mapi row Code_anatomy.all)

(* claude: a definition's body, read over the map (a click at the ground
 * or the street, Code_map: t.peek): its lines alone laid out by
 * Code_ground, the letters as big as the box allows (18 pixels at most),
 * painted once, at the window's resolution *)
let peek_images : (string * int * int * float, Rgba_image.t) Hashtbl.t = Hashtbl.create 8

(* the peek on the map: its entry and file, its lines' layout (from the
 * box's inner corner, the window the scroll shows: 17 pixels a line),
 * the box and its inner corner, the lines asked for and those shown *)
type peek = {
  pe : entry;
  pf : Code_file.t;
  pg : Code_ground.t;
  bx : float;
  by : float;
  bw : float;
  bh : float;
  ix : float;
  iy : float;
  iw : float;
  ih : float;
  first : int;
  last : int;
  shown_first : int;
  shown_last : int;
}

(* a peek at a depth in the stack: each one shifted right and down, and
 * smaller, so that the ones under it show *)
let geom_of (t : t) (c : camera) ((path, first, last) : string * int * int) (scroll : int) (depth : int) : peek option =
  let shift = 30. *. float_of_int depth in
  (
      match entry_of t path with
      | None -> None
      | Some e ->
          let a = c.a in
          let f = Lazy.force e.file in
          let n = Code_file.nlines f in
          let first = max 0 first and last = min (n - 1) last in
          let lines = last - first + 1 in
          let bw = Float.min (float_of_int a.pw -. 80. -. shift) 820. and bh = Float.min (float_of_int a.ph -. 60. -. shift) ((float_of_int lines *. 17.) +. 56.) in
          let iw = bw -. 24. and ih = bh -. 48. in
          (* the window: as many lines as fit at 17 pixels, from the scroll *)
          let cap = max 1 (int_of_float (ih /. 17.)) in
          let shown_first = first + max 0 (min scroll (lines - cap)) in
          let shown_last = min last (shown_first + cap - 1) in
          let weights = Array.init n (fun l -> if l >= shown_first && l <= shown_last then 1. else 0.) in
          let pg = Code_ground.layout weights ~pw:(int_of_float iw) ~ph:(int_of_float ih) in
          let cx = float_of_int a.pw /. 2. and cy = float_of_int a.ph /. 2. in
          let bx = cx -. (bw /. 2.) +. shift and by = cy -. (bh /. 2.) +. (shift /. 2.) in
          Some { pe = e; pf = f; pg; bx; by; bw; bh; ix = bx +. 12.; iy = by +. 36.; iw; ih; first; last; shown_first; shown_last })

let peek_geom (t : t) (c : camera) : peek option =
  match t.peek with Some top -> geom_of t c top t.peek_scroll (List.length t.peek_stack) | None -> None

let inside_peek (pk : peek) (x : float) (y : float) = x >= pk.bx && x < pk.bx +. pk.bw && y >= pk.by && y < pk.by +. pk.bh

(* a name's occurrences glowing in a layout moved by (dx, dy): the binding
 * pulsing cyan, the uses yellow, on the lines [shown] *)
let glows (t : t) (a : area) (g : Code_ground.t) ((dx, dy) : float * float) (f : Code_file.t) (o : Highlight_code.occurrence) ~(shown : int -> bool) :
    shape list =
  List.concat_map
    (fun (w : Highlight_code.occurrence) ->
      if w.line >= Array.length g.places || not (shown w.line) || g.places.(w.line).h < 0.5 then []
      else
        let x, y, _, h = Code_ground.box g w.line in
        let cw = Code_ground.cell_w g w.line in
        let ww = float_of_int w.len *. cw and hh = Float.max 3. h in
        let px = dx +. x +. (float_of_int w.col *. cw) +. (ww /. 2.) and py = dy +. y +. (h /. 2.) in
        let binding = (w.line, w.col) = o.bound_at in
        List.map (move (sx a px) (sy a py)) (Code_view.glow_at t.clock (if binding then rgb 0 225 255 else yellow) ww hh))
    (Code_file.uses f o)

let peek_level (t : t) (c : camera) (q : float) (pk : peek) ~(top : bool) : shape list =
  let a = c.a in
  let path = pk.pe.path in
  let key = (path, pk.shown_first, pk.shown_last, q) in
  let img =
    match Hashtbl.find_opt peek_images key with
    | Some img -> img
    | None ->
        let img = Rgba_image.create ~width:(int_of_float (pk.iw *. q)) ~height:(int_of_float (pk.ih *. q)) in
        let bg = (22, 20, 38) in
        fill img 0 0 img.width img.height bg;
        Code_ground.paint img pk.pf (Code_ground.scale pk.pg q) ~bg ~aa:true;
        Hashtbl.replace peek_images key img;
        img
  in
  let cx = pk.bx +. (pk.bw /. 2.) and cy = pk.by +. (pk.bh /. 2.) in
  let name = List.fold_left (fun acc (l, nm, _) -> if l = pk.first then Some nm else acc) None pk.pf.defs in
  let more = pk.shown_first > pk.first || pk.shown_last < pk.last in
  let hint =
    (if more then Printf.sprintf "lines %d-%d of %d, the wheel scrolls; " (pk.shown_first - pk.first + 1) (pk.shown_last - pk.first + 1) (pk.last - pk.first + 1) else "")
    ^ if top then "a name: its definition; outside or Escape: back" else ""
  in
  let title = Printf.sprintf "%s:%d%s%s" path (pk.first + 1) (match name with Some nm -> "  " ^ nm | None -> "") (if hint = "" then "" else "   (" ^ hint ^ ")") in
  let r, g, b = archi t.colours path in
  [ rectangle (rgb 22 20 38) pk.bw pk.bh |> move (sx a cx) (sy a cy) ]
  @ frame a (lighter (r, g, b)) pk.bx pk.by (pk.bx +. pk.bw) (pk.by +. pk.bh) (if top then 2. else 1.)
  @ [
      label a (lighter (r, g, b)) 14. (pk.bx +. 12. +. (0.25 *. 14. *. float_of_int (String.length title))) (pk.by +. 18.) title;
      bitmap pk.iw pk.ih img |> move (sx a (pk.ix +. (pk.iw /. 2.))) (sy a (pk.iy +. (pk.ih /. 2.)));
    ]
  (* a peek under another, dimmed *)
  @ if top then [] else [ rectangle (rgb 0 0 0) pk.bw pk.bh |> move (sx a cx) (sy a cy) |> fade 0.35 ]

let peek_shapes (t : t) (c : camera) (q : float) : shape list =
  match t.peek with
  | None -> []
  | Some top ->
      let a = c.a in
      let full_w = float_of_int a.pw and full_h = float_of_int a.ph in
      let n = List.length t.peek_stack in
      let under =
        List.concat
          (List.mapi
             (fun k (pk, scroll) -> match geom_of t c pk scroll (n - 1 - k) with Some g -> peek_level t c q g ~top:false | None -> [])
             (List.rev t.peek_stack))
      in
      (rectangle (rgb 0 0 0) full_w full_h |> move (sx a (full_w /. 2.)) (sy a (full_h /. 2.)) |> fade 0.45)
      :: under
      @ match geom_of t c top t.peek_scroll n with Some g -> peek_level t c q g ~top:true | None -> []

(* claude: a name hovered in the peek: its binding and uses glowing in
 * the peek, and outside it on the map, where the same file is laid out *)
let peek_glow (t : t) (c : camera) : shape list =
  match (peek_geom t c, t.pointer) with
  | Some pk, Some (u, v) -> (
      let a = c.a in
      let mx = to_px c u and my = to_py c v in
      if not (inside_peek pk mx my) then []
      else
        match Code_ground.line_at pk.pg (mx -. pk.ix) (my -. pk.iy) with
        | None -> []
        | Some l -> (
            let x0, _, _, _ = Code_ground.box pk.pg l in
            let col = int_of_float ((mx -. pk.ix -. x0) /. Code_ground.cell_w pk.pg l) in
            match Code_file.name_at pk.pf l col with
            | None -> preview t c pk.pe.path pk.pf l col mx my
            | Some o ->
                let outside =
                  match at_ground t c with
                  | Some e ->
                      let gs = if t.street then let s = street_of t e in (e.path, s.focus) :: List.map (fun (p : Code_street.panel) -> (p.path, p.ground)) (Code_street.panels s) else [ (e.path, ground_of t e) ] in
                      List.concat_map (fun (p, g) -> if p = pk.pe.path then glows t a g (0., 0.) pk.pf o ~shown:(fun _ -> true) else []) gs
                  | None -> []
                in
                outside @ glows t a pk.pg (pk.ix, pk.iy) pk.pf o ~shown:(fun l -> l >= pk.shown_first && l <= pk.shown_last)))
  | _ -> []

(*****************************************************************************)
(* The search *)
(*****************************************************************************)

(* claude: the search (/): what it looks among, the map's directories, files
 * and top-level definitions (every file lexed, once, the first query); its
 * hits, among the files shown only when [here] (the unit looked at, and
 * at the street its panels) *)
let search_all (t : t) : Code_search.hit array =
  match t.search_all with
  | Some a -> a
  | None ->
      let dirs = Array.to_list t.placed |> List.filter_map (fun (p : entry Treemap.placed) -> match p.node with Dir _ when p.path <> "" -> Some p.path | _ -> None) in
      let files = List.map (fun (e : entry) -> e.path) t.entries in
      let defs =
        List.concat_map
          (fun (e : entry) ->
            List.filter_map
              (fun (l, n, (cat : Highlight_code.category)) -> match cat with Def_function | Def_value | Def_type | Def_module -> Some (e.path, l, n) | _ -> None)
              (Lazy.force e.file).defs)
          t.entries
      in
      let views = List.map (fun (v : Code_guide.view) -> v.vname) (Code_guide.views t.guide) in
      let tours = List.map (fun (tr : Code_guide.tour) -> tr.name) (Code_guide.tours t.guide) in
      let a = Code_search.candidates ~views ~tours ~dirs ~files ~defs () in
      t.search_all <- Some a;
      a

(* the files shown: under the unit looked at, and the street's panels *)
let shown (t : t) : string -> bool =
  let top = t.placed.(t.focus).path in
  let panels = match at_ground t t.cam with Some e when t.street -> List.map (fun (p : Code_street.panel) -> p.path) (Code_street.panels (street_of t e)) | _ -> [] in
  fun p -> top = "" || p = top || Code_search.starts p (top ^ "/") || List.mem p panels

(* claude: the files' lines as text, whole (the grid cuts them at
 * Code_file.cols), for a text search: its spans put back at their
 * columns *)
let texts : (string, string array) Hashtbl.t = Hashtbl.create 256

let text_of (e : entry) : string array =
  match Hashtbl.find_opt texts e.path with
  | Some a -> a
  | None ->
      let f = Lazy.force e.file in
      let a =
        Array.map
          (fun spans ->
            let b = Buffer.create 80 in
            List.iter (fun (sp : Highlight_code.span) -> while Buffer.length b < sp.col do Buffer.add_char b ' ' done; Buffer.add_string b sp.text) spans;
            Buffer.contents b)
          f.lines
      in
      Hashtbl.replace texts e.path a;
      a

(* a query's hits among the map's files: names, or, a text search, lines *)
let query_hits (t : t) (q : string) : Code_search.hit list =
  match (Code_search.text_query q, Code_search.ref_query q) with
  | Some text, _ -> Code_search.text_matches (List.map (fun (e : entry) -> (e.path, text_of e)) t.entries) text
  (* claude: @name, the lines referring to it (Code_file.refs) *)
  | None, Some name ->
      Code_search.ref_matches
        (List.map
           (fun (e : entry) ->
             let f = Lazy.force e.file in
             let refs = Array.to_list f.refs |> List.concat_map (List.map (fun (r : Highlight_code.reference) -> (r.rline, String.concat "." (r.rpath @ [ r.rname ])))) in
             (e.path, refs, text_of e))
           t.entries)
        name
  | None, None ->
      if String.length q > 0 && (q.[0] = '"' || q.[0] = '@') then []
      else
        let top = t.placed.(t.focus).path in
        let near p = top <> "" && (p = top || Code_search.starts p (top ^ "/")) in
        Code_search.matches ~near (search_all t) q

let search_hits (t : t) : Code_search.hit list =
  match t.search with
  | None -> []
  | Some s ->
      if fst s.hits = (s.query, s.here) then snd s.hits
      else
        let hits = query_hits t s.query in
        let hits = if s.here then (let ok = shown t in List.filter (fun (h : Code_search.hit) -> ok h.path) hits) else hits in
        s.hits <- ((s.query, s.here), hits);
        hits

let search_named (t : t) : string list =
  match t.search with Some s -> List.filter (fun p -> (not s.here) || shown t p) (Code_search.all_named (search_all t) s.query) | None -> []

(* all a search found, to see together (shift+Enter): its directories
 * and files, a file under a directory found left out; else, if it found
 * only definitions, their files *)
let search_set (t : t) : string list =
  let hits = search_hits t in
  let units = List.filter_map (fun (h : Code_search.hit) -> match h.kind with Dir | File -> Some h.path | Def | Text | View | Tour -> None) hits in
  let hits = List.filter (fun (h : Code_search.hit) -> h.kind <> View && h.kind <> Tour) hits in
  let paths = if units <> [] then units else List.map (fun (h : Code_search.hit) -> h.path) hits in
  let paths = List.sort_uniq compare paths in
  List.filter (fun p -> not (List.exists (fun d -> d <> p && Code_search.starts p (d ^ "/")) paths)) paths

(* the hits lit where they are on the map, at any level: a directory or
 * file framed, a definition's line marked (a bar at the ground, a dot
 * above it), the chosen one brighter and named *)
let search_lit ?(glow = rgb 255 225 90) ?(dot = 3.) ?(chosen : Code_search.hit option) (t : t) (c : camera) (hits : Code_search.hit list) : shape list =
  let a = c.a in
  let ground = at_ground t c <> None in
  let index = Hashtbl.create 64 in
  Array.iteri (fun i (p : entry Treemap.placed) -> Hashtbl.replace index p.path i) t.placed;
  List.concat
    (List.filteri (fun i _ -> i < 3000) hits
    |> List.map (fun (h : Code_search.hit) ->
           let sel = chosen == Some h || chosen = Some h in
           match h.kind with
           | Dir | File -> (
               match Hashtbl.find_opt index h.path with
               | Some i when i <> t.focus && not ground -> (
                   match clip c t.placed.(i).rect with
                   | Some (x0, y0, x1, y1) when x1 - x0 >= 2 && y1 - y0 >= 2 ->
                       let x0 = float_of_int x0 and y0 = float_of_int y0 and x1 = float_of_int x1 and y1 = float_of_int y1 in
                       [ rectangle glow (x1 -. x0) (y1 -. y0) |> move (sx a ((x0 +. x1) /. 2.)) (sy a ((y0 +. y1) /. 2.)) |> fade (if sel then 0.3 else 0.14) ]
                       @ frame a glow x0 y0 x1 y1 (if sel then 3. else 1.5)
                   | _ -> [])
               | _ -> [])
           | Def | Text -> (
               match spot t c h.path h.line with
               | Some (x, y, xe) when ground ->
                   let w = Float.max 30. (xe -. x) in
                   [ rectangle glow (w +. 8.) 12. |> move (sx a (x +. (w /. 2.))) (sy a y) |> fade (if sel then 0.55 else 0.3) ]
               | Some (x, y, _) -> [ circle glow (if sel then 6. else dot) |> move (sx a x) (sy a y) |> fade (if sel then 1. else 0.85) ]
               | None -> [])
           | View | Tour -> []))

(* the box, under the title: the query typed, where it looks, the best
 * hits, the chosen one lit, and what the keys do *)
let search_box (t : t) (c : camera) (s : search) (hits : Code_search.hit list) : shape list =
  let a = c.a in
  let w = Float.min 760. (float_of_int a.pw -. 40.) in
  let x0 = (float_of_int a.pw -. w) /. 2. and y0 = 12. in
  let shown_hits = List.filteri (fun i _ -> i < 8) hits in
  let row = 24. in
  let named = search_named t in
  let h = 50. +. (row *. float_of_int (max 1 (List.length shown_hits))) +. 30. in
  let left size col x y str = label a col size (x +. (text_width size str /. 2.)) y str in
  let where = if s.here then (match t.placed.(t.focus).path with "" -> "the files shown" | p -> "in " ^ p ^ (match t.placed.(t.focus).node with Dir _ -> "/" | File _ -> "")) else "everywhere" in
  let caret = if Float.rem t.clock 1. < 0.5 then "|" else " " in
  [ rectangle (rgb 16 14 34) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.97 ]
  @ frame a yellow x0 y0 (x0 +. w) (y0 +. h) 2.
  @ [ left 20. yellow (x0 +. 14.) (y0 +. 22.) ("/ " ^ s.query ^ caret) ]
  @ [ left 13. dim (x0 +. w -. 14. -. text_width 13. (where ^ "   " ^ string_of_int (List.length hits) ^ " found")) (y0 +. 22.) (where ^ "   " ^ string_of_int (List.length hits) ^ " found") ]
  @ (if s.query = "" then [ left 15. dim (x0 +. 20.) (y0 +. 50. +. (row /. 2.)) "a directory, a file or a definition: its name, or a part of it" ]
     else if hits = [] then [ left 15. dim (x0 +. 20.) (y0 +. 50. +. (row /. 2.)) "nothing of that name" ]
     else [])
  @ List.concat
      (List.mapi
         (fun i (hit : Code_search.hit) ->
           let y = y0 +. 44. +. (float_of_int i *. row) +. (row /. 2.) in
           let kind = match hit.kind with Dir -> "dir" | File -> "file" | Def -> "def" | Text -> "line" | View -> "view" | Tour -> "tour" in
           let cut n str = if String.length str > n then String.sub str 0 n ^ "..." else str in
           let name = match hit.kind with Dir -> hit.name ^ "/" | Text -> Printf.sprintf "%s:%d" (Code_search.basename hit.path) (hit.line + 1) | _ -> hit.name in
           let where =
             match hit.kind with
             | Def -> Printf.sprintf "%s:%d" hit.path (hit.line + 1)
             | Text -> cut 70 hit.name
             | View -> ( match List.nth_opt (Code_guide.views t.guide) hit.line with Some v -> (match v.of_ with Some o -> o ^ " and " ^ Option.value v.with_ ~default:"users" | None -> String.concat ", " v.files) | None -> "")
             | Tour -> ( match List.nth_opt (Code_guide.tours t.guide) hit.line with Some tr -> Printf.sprintf "%d stops, n next, p back" (List.length tr.stops) | None -> "")
             | Dir | File -> hit.path
           in
           let where = cut 80 where in
           let col = lighter (archi t.colours hit.path) in
           (if i = s.sel then [ rectangle (rgb 60 56 110) (w -. 12.) row |> move (sx a (x0 +. (w /. 2.))) (sy a y) ] else [])
           @ [ left 12. dim (x0 +. 16.) y kind; left 16. (if i = s.sel then yellow else col) (x0 +. 56.) y name; left 13. dim (x0 +. 70. +. text_width 16. name) y where ])
         shown_hits)
  @ [
      left 12. dim (x0 +. 14.) (y0 +. h -. 14.)
        (match named with
        | _ :: _ :: _ -> Printf.sprintf "Enter: the %d directories named so, together   Esc close" (List.length named)
        | _ when hits <> [] ->
            let n = List.length (search_set t) in
            Printf.sprintf "Enter go   shift+Enter the %d %s together   ctrl+Enter a layer   Tab complete   \"text   / first: here or all" n
              (if List.exists (fun (h : Code_search.hit) -> h.kind = Dir || h.kind = File) hits then "found" else "files of these")
        | _ -> "a name, \"text, @reference, name// directories so named   Tab complete   up/down choose   / first: here or all   Esc close");
    ]

(* claude: the layers (plan_codemap_v2.md): searches kept, ctrl+Enter,
 * each lit in its colour at any level, all at once (the author:
 * "Cap.fork, Cap.exec ... a layer with different color scheme for each
 * and get all the capabilities highlighted at the same time"); their
 * legend in the map's bottom left corner *)
let layer_colours = [ (245, 85, 85); (80, 205, 245); (120, 230, 110); (250, 165, 50); (225, 115, 235); (245, 240, 95); (110, 130, 255) ]

let layer_hits (t : t) (l : layer) : Code_search.hit list =
  match l.lhits with
  | Some h -> h
  | None ->
      let h = query_hits t l.lquery in
      l.lhits <- Some h;
      h

(* the groups of layers: those kept, then each config's, their names *)
let layer_groups (t : t) : (string * layer list) list =
  let guide =
    match t.guide_layers with
    | Some g -> g
    | None ->
        let g =
          List.map
            (fun (l : Code_guide.layer) ->
              (l.lname, List.map (fun (r : Code_guide.rule) -> { lquery = (if r.is_ref then "@" else "\"") ^ r.text; lcolour = r.colour; lsay = r.rsay; lhits = None }) l.rules))
            (Code_guide.layers t.guide)
        in
        t.guide_layers <- Some g;
        g
  in
  ("kept", t.layers) :: guide

let layers_shapes (t : t) (c : camera) : shape list =
  (* claude: -1, none (List.nth_opt raises on it) *)
  match if t.layer_group < 0 then None else List.nth_opt (layer_groups t) t.layer_group with
  | None | Some (_, []) -> []
  | Some (group, layers) ->
    let a = c.a in
    let lit = List.concat_map (fun (l : layer) -> let r, g, b = l.lcolour in search_lit ~glow:(rgb r g b) ~dot:4.5 t c (layer_hits t l)) layers in
    let row = 20. in
    let line (l : layer) =
      let q = if String.length l.lquery > 0 && (l.lquery.[0] = '"' || l.lquery.[0] = '@') then String.sub l.lquery 1 (String.length l.lquery - 1) else l.lquery in
      Printf.sprintf "%s  %d%s" q (List.length (layer_hits t l)) (match l.lsay with Some s -> "   " ^ s | None -> "")
    in
    let head = Printf.sprintf "%s   (l: next)" (if group = "kept" then "layers kept" else group) in
    let n = List.length layers in
    let w = 30. +. List.fold_left (fun m l -> Float.max m (text_width 14. (line l))) (text_width 14. head) layers in
    let h = 12. +. (row *. float_of_int (n + 1)) in
    let x0 = 10. and y0 = float_of_int a.ph -. h -. 10. in
    lit
    @ [ rectangle (rgb 16 14 34) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.9 ]
    @ [ label a yellow 14. (x0 +. 12. +. (text_width 14. head /. 2.)) (y0 +. 6. +. (row /. 2.)) head ]
    @ List.concat
        (List.mapi
           (fun i (l : layer) ->
             let r, g, b = l.lcolour in
             let y = y0 +. 6. +. (float_of_int (i + 1) *. row) +. (row /. 2.) in
             let str = line l in
             [ circle (rgb r g b) 5. |> move (sx a (x0 +. 12.)) (sy a y); label a ink 14. (x0 +. 22. +. (text_width 14. str /. 2.)) y str ])
           layers)

(* claude: a match under the mouse (a search's, a layer's), its line and
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
        (match t.search with Some _ -> List.map (fun h -> (h, rgb 255 225 90, None)) (search_hits t) | None -> [])
        @ (if t.layer_group < 0 then []
           else
             match List.nth_opt (layer_groups t) t.layer_group with
             | Some (_, ls) -> List.concat_map (fun (l : layer) -> let r, g, b = l.lcolour in List.map (fun h -> (h, rgb r g b, l.lsay)) (layer_hits t l)) ls
             | None -> [])
      in
      let lines = List.filter (fun ((h : Code_search.hit), _, _) -> h.kind = Def || h.kind = Text) lit in
      if lines = [] then None
      else
        let index = Hashtbl.create 256 in
        Array.iteri (fun i (p : entry Treemap.placed) -> match p.node with File _ -> Hashtbl.replace index p.path i | Dir _ -> ()) t.placed;
        let ground = at_ground t c <> None in
        let where (h : Code_search.hit) =
          if ground then Option.map (fun (x, y, xe) -> (x, y, Float.max xe (x +. 30.))) (spot t c h.path h.line)
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
            code_card ~lit:(h.line, col) t c h.path (h.line - 3) (h.line + 3) title mx my
  | _ -> []

(* claude: a config's tour under way: its name, the stop, its words,
 * above the map's foot *)
let tour_banner (t : t) (c : camera) : shape list =
  match t.tour_on with
  | None -> []
  | Some (tr, k) ->
      let a = c.a in
      let say = match List.nth_opt tr.stops k with Some (i : Code_guide.item) -> Option.value i.say ~default:i.at | None -> "" in
      let head = Printf.sprintf "%s   stop %d of %d   (n next, p back, Esc the end)" tr.name (k + 1) (List.length tr.stops) in
      let lines = wrap 90 say in
      let w = Float.min (float_of_int a.pw -. 40.) (40. +. List.fold_left (fun m l -> Float.max m (text_width 18. l)) (text_width 14. head) lines) in
      let h = 34. +. (24. *. float_of_int (List.length lines)) in
      let x0 = (float_of_int a.pw -. w) /. 2. and y0 = float_of_int a.ph -. h -. 12. in
      [ rectangle (rgb 16 14 34) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.95 ]
      @ frame a (rgb 90 210 120) x0 y0 (x0 +. w) (y0 +. h) 2.
      @ [ label a (rgb 90 210 120) 14. (x0 +. 14. +. (text_width 14. head /. 2.)) (y0 +. 14.) head ]
      @ List.mapi (fun i l -> label a ink 18. (x0 +. 14. +. (text_width 18. l /. 2.)) (y0 +. 36. +. (24. *. float_of_int i)) l) lines

let anchor_line (t : t) (path : string) (anchor : string) : int option =
  match entry_of t path with Some e -> capital_line e anchor | None -> None

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
        !drawn_bones
  | _ -> None

(* its card: an entity's role and the start of its definition, readable;
 * a file's or a directory's role and what its config says of it *)
let bone_card (t : t) (c : camera) : shape list =
  match (hovered_bone t c, t.pointer) with
  | Some bn, Some (u, v) -> (
      let mx = to_px c u and my = to_py c v in
      match bone_line t bn with
      | Some l -> (
          match entry_of t bn.bpath with
          | Some e ->
              let _, last = extent (Lazy.force e.file) l in
              code_card t c bn.bpath l (min last (l + 11)) (Printf.sprintf "%s   %s:%d   (click: all of it)" bn.role bn.bpath (l + 1)) mx my
          | None -> [])
      | None ->
          let a = c.a in
          let said =
            match Code_guide.file_note t.guide bn.bpath with
            | Some { summary = Some s; _ } -> Some s
            | _ -> Code_guide.dir_summary t.guide bn.bpath
          in
          let lines = (bn.role :: [ bn.bpath ]) @ (match said with Some s -> wrap 60 s | None -> []) in
          let w = 24. +. List.fold_left (fun m l -> Float.max m (text_width 15. l)) 0. lines and h = 12. +. (22. *. float_of_int (List.length lines)) in
          let x0 = Float.min (mx +. 18.) (float_of_int a.pw -. w -. 4.) and y0 = Float.min (my +. 18.) (float_of_int a.ph -. h -. 4.) in
          let r, g, b = ivory in
          [ rectangle (rgb 18 16 36) w h |> move (sx a (x0 +. (w /. 2.))) (sy a (y0 +. (h /. 2.))) |> fade 0.96 ]
          @ frame a (rgb r g b) x0 y0 (x0 +. w) (y0 +. h) 1.5
          @ List.mapi (fun i l -> label a (if i = 0 then rgb r g b else if i = 1 then dim else ink) 15. (x0 +. 12. +. (text_width 15. l /. 2.)) (y0 +. 17. +. (22. *. float_of_int i)) l) lines)
  | _ -> []

let search_shapes (t : t) (c : camera) : shape list =
  match t.search with
  | None -> []
  | Some s ->
      let hits = search_hits t in
      search_lit ?chosen:(List.nth_opt hits s.sel) t c hits @ search_box t c s hits

let labels (t : t) (c : camera) (q : float) : shape list =
  let kept = names t c in
  (match at_ground t c with
  | Some e when t.street -> street_labels t c e @ line_lit t c e @ names_glow t c e
  | Some e -> notes t c e @ line_lit t c e @ names_glow t c e
  | None -> [])
  @ List.rev_map (fun n -> n.draw) kept
  (* the legend when a plate other than the skeleton is on *)
  @ (if t.xray then skeleton_shapes t c @ anatomy_shapes t c @ (if List.exists (fun s -> s <> Code_anatomy.Skeleton) !Code_anatomy.shown then legend c else []) else [])
  @ hover_card t c kept
  @ peek_shapes t c q
  @ peek_glow t c
  @ layers_shapes t c
  @ search_shapes t c
  @ match_preview t c
  @ bone_card t c
  @ tour_banner t c

(* claude: at the ground, the line under a pixel (Code_ground's layout,
 * not the treemap's): what Enter opens, what the status line says *)
let pick (t : t) (c : camera) (_ : float) (px : float) (py : float) : (string * int * int) option =
  (* the column under the pixel, in a line of a layout *)
  let col g l = let x0, _, _, _ = Code_ground.box g l in max 0 (int_of_float ((px -. x0) /. Code_ground.cell_w g l)) in
  match peek_geom t c with
  | Some pk ->
      (* a peek open: its line under the pixel, or nothing (a click there
       * closes it, Code_map) *)
      if not (inside_peek pk px py) then None
      else
        Option.map
          (fun l -> let x0, _, _, _ = Code_ground.box pk.pg l in (pk.pe.path, l, max 0 (int_of_float ((px -. pk.ix -. x0) /. Code_ground.cell_w pk.pg l))))
          (Code_ground.line_at pk.pg (px -. pk.ix) (py -. pk.iy))
      |> (function None when inside_peek pk px py -> Some (pk.pe.path, -2, 0) | r -> r)
  | None ->
  match at_ground t c with
  | Some e when t.street -> (
      let s = street_of t e in
      match Code_street.line_at s px py with
      | Some (p, l) -> ( match Code_street.ground_of s p with Some g -> Some (p, l, col g l) | None -> None)
      | None -> None)
  | Some e -> let g = ground_of t e in Option.map (fun l -> (e.path, l, col g l)) (Code_ground.line_at g px py)
  | None ->
      (* a section's title in a file's table of contents: the whole
       * section, the column -1 saying so (Code_map) *)
      Option.map (fun (p, l) -> (p, l, -1)) (List.find_map (fun n -> if within n.nbox px py then n.sect else None) (names t c))

let style : style = { sname = "v2"; paint; labels; pick; unit_at; units = true }
