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

(* the names: Map_classic's, sized and chosen by their uses; placed once,
 * level by level, next (the plan's step 3) *)
let labels (t : t) = Map_classic.labels_by ~emphasis:(emphasis t) t

let style : style = { sname = "streets"; paint; labels }
