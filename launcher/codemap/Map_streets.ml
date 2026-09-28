(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Map_streets.mli *)

open Playground
open Code_map_base

(*****************************************************************************)
(* The levels *)
(*****************************************************************************)

(* where each level starts, a line's height on the screen in the
 * window's pixels: the files apart (Z1), SeeSoft's colours (Z3), the
 * letters (Z4, paint_code's own threshold) *)
let t_apart = 0.22
let t_colours = 2.5

let level (lh : float) : int = if lh < t_apart then 0 else if lh < 1. then 1 else if lh < t_colours then 2 else if lh < text_px then 3 else 4

(* 0 below a, 1 above b, smooth between *)
let smooth (a : float) (b : float) (x : float) : float =
  let t = Float.min 1. (Float.max 0. ((x -. a) /. (b -. a))) in
  t *. t *. (3. -. (2. *. t))

(*****************************************************************************)
(* The files' colours *)
(*****************************************************************************)

(* codemap's archi_code, by a file's name: what a reader looks for first *)
let role (path : string) : (int * int * int) option =
  let low = String.lowercase_ascii (Filename.basename path) in
  let starts p = String.length low >= String.length p && String.sub low 0 (String.length p) = p in
  if starts "unit_" || starts "test" then Some (235, 80, 90)
  else if starts "tiny" || low = "main.c" || starts "main." then Some (255, 140, 60)
  else if starts "lexer" || starts "parse" then Some (110, 215, 120)
  else None

(* claude: half the role's, half the part's: a part made only of
 * programs (games/, all Tiny*.ml) keeps its colour *)
let file_colour (t : t) (path : string) : int * int * int =
  match role path with Some c -> mix c 0.45 (archi t.colours path) | None -> archi t.colours path

(*****************************************************************************)
(* The column hints *)
(*****************************************************************************)

(* a file's lines' lengths, from its grid (the last cell with a
 * character), made once *)
let widths : (string, int array) Hashtbl.t = Hashtbl.create 256

let widths_of (path : string) (f : Code_file.t) : int array =
  match Hashtbl.find_opt widths path with
  | Some w -> w
  | None ->
      let n = Code_file.nlines f and cols = Code_file.cols in
      let w =
        Array.init n (fun line ->
            let k = ref cols in
            while !k > 0 && Bytes.get f.grid ((line * cols) + !k - 1) = '\000' do decr k done;
            !k)
      in
      Hashtbl.replace widths path w;
      w

(* The hints, codemap's macro level: a file's columns of lines, each line
 * a bar as long as the code's, in the file's colour lightened, over its
 * background; no colour of the code. A pixel's shade is the share of the
 * lines under it (up to 4 looked at) that reach its column: from afar, a
 * column's silhouette, near, its lines one by one. [alpha] below 1: over
 * what the pixel already has (the band where the colours come in). *)
let hints ~(alpha : float) (img : Rgba_image.t) (c : camera) (r : Treemap.rect) (g : geometry) (w : int array)
    ((x0, y0, x1, y1) : int * int * int * int) (bg : int * int * int) (col : int * int * int) : unit =
  let n = Array.length w in
  let br, bgc, bb = bg in
  let rr, rg, rb = mix col 0.5 (215, 218, 235) in
  let rgba = img.rgba in
  (* for each x: its column of lines, and its character there *)
  let nx = x1 - x0 in
  let colx = Array.make nx 0 and chx = Array.make nx 0.0 in
  for i = 0 to nx - 1 do
    let u = to_u c (float_of_int (x0 + i) +. 0.5) -. r.x in
    let k = int_of_float (Float.floor (u /. g.colw)) in
    colx.(i) <- k;
    chx.(i) <- (u -. (float_of_int k *. g.colw)) /. g.cell_w
  done;
  for y = y0 to y1 - 1 do
    let v0 = (to_v c (float_of_int y) -. r.y) /. g.cell_h and v1 = (to_v c (float_of_int (y + 1)) -. r.y) /. g.cell_h in
    let l0 = int_of_float (Float.floor v0) and l1 = int_of_float (Float.floor v1) in
    let samples = max 1 (min 4 (l1 - l0 + 1)) in
    for x = x0 to x1 - 1 do
      let xi = x - x0 in
      let k = colx.(xi) and ch = chx.(xi) in
      (* a column's last characters left blank: the gap between columns *)
      let hits = ref 0 in
      if ch < float_of_int (Code_file.cols - 3) then
        for s = 0 to samples - 1 do
          let lc = l0 + (s * max 1 ((l1 - l0 + 1) / samples)) in
          let line = (k * g.lpc) + lc in
          if lc >= 0 && lc < g.lpc && line >= 0 && line < n && ch < float_of_int w.(line) then incr hits
        done;
      let cov = float_of_int !hits /. float_of_int samples in
      let i = 4 * ((y * img.width) + x) in
      let put o target base =
        let v = float_of_int base +. (cov *. float_of_int (target - base)) in
        let v = if alpha >= 1. then v else (alpha *. v) +. ((1. -. alpha) *. float_of_int (Bigarray.Array1.unsafe_get rgba (i + o))) in
        Bigarray.Array1.unsafe_set rgba (i + o) (int_of_float v)
      in
      put 0 rr br;
      put 1 rg bgc;
      put 2 rb bb;
      Bigarray.Array1.unsafe_set rgba (i + 3) 255
    done
  done

(*****************************************************************************)
(* The picture *)
(*****************************************************************************)

let paint ~(aa : bool) (t : t) (c : camera) : Rgba_image.t =
  let img = Rgba_image.create ~width:c.a.pw ~height:c.a.ph in
  fill img 0 0 c.a.pw c.a.ph dark;
  Array.iteri
    (fun i (p : entry Treemap.placed) ->
      match clip c p.rect with
      | None -> ()
      | Some ((x0, y0, x1, y1) as box) -> (
          match (p.node, t.geometry.(i)) with
          | Dir _, _ -> fill img x0 y0 x1 y1 (dir_colour t p.path p.depth)
          | File (_, _, e), Some g ->
              let col = file_colour t p.path in
              let bg = mix col 0.22 (20, 22, 30) in
              let lh = g.cell_h *. c.z in
              (* Z0: a flat block of its colour; and a file too small to show
               * anything is not lexed (Map_classic's rule) *)
              if lh < t_apart *. 0.7 || ((x1 - x0) * (y1 - y0) < 40 && not (Lazy.is_val e.file)) then fill img x0 y0 x1 y1 (mix col 0.4 bg)
              else begin
                let f = Lazy.force e.file in
                let colours = smooth (t_colours *. 0.8) (t_colours *. 1.25) lh in
                let apart = smooth (t_apart *. 0.7) (t_apart *. 1.3) lh in
                if colours > 0. then paint_code ~aa img c p.rect g f box bg
                else fill img x0 y0 x1 y1 (mix col 0.4 bg);
                (* the hints, faded in from Z0's block, out into the colours *)
                let a = apart *. (1. -. colours) in
                if a > 0.01 then hints ~alpha:a img c p.rect g (widths_of p.path f) box bg col
              end;
              (* its outline, once the files are apart *)
              if lh >= t_apart && x1 - x0 > 6 && y1 - y0 > 6 then begin
                let edge = mix col 0.6 dark in
                fill img x0 y0 x1 (y0 + 1) edge;
                fill img x0 (y1 - 1) x1 y1 edge;
                fill img x0 y0 (x0 + 1) y1 edge;
                fill img (x1 - 1) y0 x1 y1 edge
              end
          | File _, None -> ()))
    t.placed;
  img

(*****************************************************************************)
(* The names *)
(*****************************************************************************)

(* claude: how much a definition matters, its population (Code_rank,
 * the plan's step 2): codemap's weight by kind times its bucket of uses,
 * over 2.1 (the middle bucket) to be on Highlight_code.emphasis's scale
 * -- a function used 5 to 19 times as classic's 3.5, unused about 1.5,
 * used by a hundred about 5.5; a section's title as classic's *)
let emphasis (t : t) (path : string) (line : int) (name : string) (cat : Highlight_code.category) : float =
  match cat with
  | Def_module | Def_type | Def_function | Def_value -> Code_rank.score (rank_of t) path line name cat /. 2.1
  | _ -> Highlight_code.emphasis cat

(* the level, continuous: level i from i to i + 1, by a line's height *)
let level_f (lh : float) : float =
  let within a b = Float.log (lh /. a) /. Float.log (b /. a) in
  if lh < t_apart then lh /. t_apart
  else if lh < 1. then 1. +. within t_apart 1.
  else if lh < t_colours then 2. +. within 1. t_colours
  else if lh < text_px then 3. +. within t_colours text_px
  else 4.

(* claude: the labels of a map, built from its files (all lexed: the
 * populations need them anyway) and placed once (Code_labels), in the
 * classes of a street map, each smaller one lighter:
 *
 *   the countries                  the whole map: its top directories, in
 *                                  capitals, nothing else around them
 *   the regions, the districts     zooming in: the next depths
 *   the program's own file's tab   always, first
 *   the files' tabs                from Z1, when they fit
 *   the map's 3 capitals, and      from Z1: the most used definitions,
 *   each directory's 2 cities      named with their module
 *   a trick of this game           a landmark
 *   a definition, a section        Z2 and Z3, by their uses, the least
 *                                  used the lightest; at Z4 the code
 *                                  is read instead *)
let dir_classes = [| (42., 60000., (240, 240, 250)); (22., 40000., (205, 205, 228)); (16., 30000., (165, 165, 198)) |]

let build (t : t) : Code_labels.label array =
  let rank = rank_of t in
  let out = ref [] and defs = ref [] in
  let add l = out := l :: !out in
  Array.iteri
    (fun i (p : entry Treemap.placed) ->
      match (p.node, t.geometry.(i)) with
      | Dir (name, _), _ when p.depth >= 1 && p.depth <= 3 ->
          (* a country from the whole map; a region, a district, one zoom
           * level (by the zoom's, dir_level) deeper each *)
          let px, rk, color = dir_classes.(p.depth - 1) in
          let r = p.rect in
          let text = if p.depth = 1 then String.uppercase_ascii name else name ^ "/" in
          let from_level = float_of_int (p.depth - 1) -. (if p.depth = 1 then 0. else 0.2) in
          add
            (Code_labels.label Dir text ~x:(r.x +. (r.w /. 2.)) ~y:(r.y +. (r.h /. 2.)) ~left:false ~px ~rank:(rk +. (r.w *. r.h /. 100.)) ~from_level
               ~to_level:(from_level +. 1.4) ~fw:r.w ~fh:r.h color)
      | File (_, _, e), Some g ->
          let f = Lazy.force e.file and r = p.rect in
          let fw = r.w and fh = r.h in
          let main = List.mem e.path t.marked in
          add
            (Code_labels.label Tab (basename e.path) ~x:r.x ~y:r.y ~px:13. ~rank:(if main then 1e6 else 800. +. (float_of_int e.nlines /. 20.))
               ~from_level:(if main then 0. else 1.6) ~to_level:9. ~fw ~fh
               (if main then (255, 215, 70) else let rr, gg, bb = file_colour t e.path in (min 255 (rr + 60), min 255 (gg + 60), min 255 (bb + 60))));
          let at line = let x, y = line_pos r g line in (x, y +. (g.cell_h /. 2.)) in
          List.iter
            (fun line ->
              let x, y = line_pos r g line in
              add (Code_labels.label Landmark ("* " ^ Code_file.trick) ~x ~y:(y -. 19.) ~px:13. ~rank:5000. ~from_level:1.2 ~to_level:9. ~fw ~fh (230, 80, 200)))
            f.marks;
          List.iter
            (fun (line, name, cat) ->
              if line < Code_file.nlines f then begin
                let x, y = at line in
                match cat with
                | Highlight_code.Comment_section ->
                    add (Code_labels.label Section name ~x ~y ~px:13. ~rank:(600. +. (float_of_int e.nlines /. 40.)) ~from_level:2. ~to_level:3.95 ~fw ~fh (178, 182, 255))
                | _ ->
                    let s = Code_rank.score rank e.path line name cat in
                    defs := (e.path, line, s, name, cat, x, y, fw, fh) :: !defs;
                    (* the least used the lightest: they recede, still there *)
                    let color = mix (Highlight_code.rgb cat) (Float.min 1. (0.45 +. (s /. 12.))) (120, 120, 140) in
                    add
                      (Code_labels.label Def name ~x ~y ~px:(Float.min 20. (9. +. (0.7 *. s))) ~rank:(1000. +. (40. *. s)) ~from_level:2. ~to_level:3.95 ~fw ~fh
                         ~target:(e.path, line, name) color)
              end)
            f.defs
      | _ -> ())
    t.placed;
  (* the capitals, the map's; the cities, each directory's; seen from
   * afar, named with their module (TinyMario.model, Playground.number:
   * many a directory's most used is its model, or its t) *)
  let by_score = List.stable_sort (fun (_, _, a, _, _, _, _, _, _) (_, _, b, _, _, _, _, _, _) -> compare b a) !defs in
  let qualified path name = String.capitalize_ascii (Filename.remove_extension (Filename.basename path)) ^ "." ^ name in
  List.iteri
    (fun k (path, line, s, name, _, x, y, fw, fh) ->
      if k < 3 then
        add
          (Code_labels.label Capital (qualified path name) ~x ~y ~px:18. ~rank:(20000. +. s) ~from_level:0.6 ~to_level:1.6 ~fw ~fh ~target:(path, line, name)
             (255, 215, 70)))
    by_score;
  let per_dir = Hashtbl.create 64 in
  List.iter
    (fun (path, line, s, name, cat, x, y, fw, fh) ->
      let d = Filename.dirname path in
      let n = Option.value (Hashtbl.find_opt per_dir d) ~default:0 in
      if n < 2 then begin
        Hashtbl.replace per_dir d (n + 1);
        add
          (Code_labels.label City (qualified path name) ~x ~y ~px:15. ~rank:(10000. +. s) ~from_level:1. ~to_level:2.2 ~fw ~fh ~target:(path, line, name)
             (Highlight_code.rgb cat))
      end)
    by_score;
  Array.of_list !out

(* the labels placed, kept for the last few maps (a map and the menu's
 * panel's), by their layout (physically: a new layout, t, a new
 * placement) and the window's pixels a unit *)
let placed_cache : (entry Treemap.placed array * float * Code_labels.label array) list ref = ref []

let placed_labels (t : t) (q : float) : Code_labels.label array =
  match List.find_opt (fun (p, q', _) -> p == t.placed && q' = q) !placed_cache with
  | Some (_, _, ls) -> ls
  | None ->
      let ls = build t in
      let chs = Array.to_list t.geometry |> List.filter_map (Option.map (fun g -> g.cell_h)) |> List.sort compare in
      let median = match chs with [] -> 1. | _ -> List.nth chs (List.length chs / 2) in
      (* the directories by the zoom from the whole map (z 1): the
       * countries there whatever the map's size, a level each 3 times
       * closer *)
      Code_labels.place ~level:(fun z -> level_f (median *. z *. q)) ~dir_level:(fun z -> Float.max 0. (Float.log z /. Float.log 3.)) ~zmin:0.5
        ~zmax:400. ls;
      placed_cache := (t.placed, q, ls) :: List.filteri (fun i _ -> i < 3) !placed_cache;
      ls

(* a label's box on the map, if it is shown there whole: x0, y0, w, h *)
let on_map (c : camera) (l : Code_labels.label) : (float * float * float * float) option =
  let w, h = Code_labels.size l in
  let px = to_px c l.x and py = to_py c l.y in
  let x0 = if l.left then px else px -. (w /. 2.) in
  let y0 = match l.kind with Tab | Landmark -> py | _ -> py -. (h /. 2.) in
  if x0 < 0. || x0 +. w > float_of_int c.a.pw || y0 < 0. || y0 +. h > float_of_int c.a.ph then None else Some (x0, y0, w, h)

(* the labels shown at this zoom *)
let labels (t : t) (c : camera) (q : float) : shape list =
  let a = c.a in
  Array.to_list (placed_labels t q)
  |> List.filter_map (fun (l : Code_labels.label) ->
           let al = Code_labels.alpha l c.z in
           if al <= 0.02 then None
           else
             let w, h = Code_labels.size l in
             let px = to_px c l.x and py = to_py c l.y in
             let x0 = if l.left then px else px -. (w /. 2.) in
             (* only whole, on the map: never over its edges *)
             let y0 = match l.kind with Tab | Landmark -> py | _ -> py -. (h /. 2.) in
             if x0 < 0. || x0 +. w > float_of_int a.pw || y0 < 0. || y0 +. h > float_of_int a.ph then None
             else
               let r, g, b = l.color in
               let color = rgb r g b in
               match l.kind with
               | Tab | Landmark -> Some (snd (tab a ~alpha:(0.85 *. al) color l.px px py l.text) |> fade al)
               | Dir ->
                   (* a halo: the name drawn dark round itself, then over it,
                    * a street map's thick outline *)
                   let cx = x0 +. (w /. 2.) and o = Float.max 1.5 (l.px /. 14.) in
                   let dark = Playground.rgb 12 10 28 in
                   Some
                     (group
                        (List.map (fun (dx, dy) -> label a ~alpha:(0.8 *. al) dark l.px (cx +. dx) (py +. dy) l.text) [ (-.o, 0.); (o, 0.); (0., -.o); (0., o); (-.o, -.o); (o, o); (-.o, o); (o, -.o) ]
                        @ [ label a ~alpha:al color l.px cx py l.text ]))
               | _ ->
                   Some
                     (group
                        [
                          (* a halo: a dark box behind, the words readable over the code *)
                          rectangle (Playground.rgb 12 10 28) (w +. 6.) (h +. 2.) |> move (sx a (x0 +. (w /. 2.))) (sy a py) |> fade (0.55 *. al);
                          label a ~alpha:al color l.px (x0 +. (w /. 2.)) py l.text;
                        ]))

(* claude: the definition a label under a pixel names: the labels shown
 * well, a definition's, its box around the pixel *)
let pick (t : t) (c : camera) (q : float) (mx : float) (my : float) : (string * int * string) option =
  Array.fold_left
    (fun found (l : Code_labels.label) ->
      match (found, l.target) with
      | Some _, _ | _, None -> found
      | None, Some target ->
          if Code_labels.alpha l c.z < 0.5 then None
          else (
            match on_map c l with Some (x0, y0, w, h) when mx >= x0 && mx <= x0 +. w && my >= y0 && my <= y0 +. h -> Some target | _ -> None))
    None (placed_labels t q)

let style : style = { sname = "streets"; paint; labels; pick }
