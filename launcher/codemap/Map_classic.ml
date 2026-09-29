(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)


(* See Map_classic.mli *)

open Playground
open Code_map_base

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
              let bg = file_background t p.path in
              (* claude: a file too small to show anything is not lexed:
               * the whole repository's map opens without lexing it all *)
              if (x1 - x0) * (y1 - y0) < 40 && not (Lazy.is_val e.file) then fill img x0 y0 x1 y1 (mix (archi t.colours p.path) 0.5 bg)
              else paint_code ~aa img c p.rect g (Lazy.force e.file) box bg;
              (* claude: its outline, in its part's colour, a file told
               * apart from its neighbours (the layout's gap between them) *)
              if x1 - x0 > 6 && y1 - y0 > 6 then begin
                let edge = mix (archi t.colours p.path) 0.6 dark in
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

(* the names over the map: directories', big and faint (codemap's), and
 * their paths on a tab at their top right; files' on a tab at their top
 * left (the program's own in yellow, never left out); and, from afar,
 * what each file defines, bigger the more it matters. claude: how much
 * it matters, [emphasis] of its file, line, name and category: here, its
 * category's (Highlight_code.emphasis); the street map's, its uses
 * (Map_streets) *)
let labels_by ~(emphasis : string -> int -> string -> Highlight_code.category -> float) (t : t) (c : camera) (q : float) : shape list =
  let a = c.a in
  (* readable as painted: in the window's pixels *)
  let readable c g = readable (at_ratio c q) g in
  let candidate = candidate a in
  let dirs = ref [] and files = ref [] and defs = ref [] in
  Array.iteri
    (fun i (p : entry Treemap.placed) ->
      match clip c p.rect with
      | None -> ()
      | Some (x0, y0, x1, y1) -> (
          let w = float_of_int (x1 - x0) and h = float_of_int (y1 - y0) in
          let name = match p.node with Dir (n, _) -> n | File (n, _, _) -> n in
          let fit_size len = w /. (0.55 *. float_of_int (max 1 len)) in
          match (p.node, t.geometry.(i)) with
          | Dir (_, kids), _ when p.depth > 0 || p.path <> "" ->
              (* codemap's: the name big and faint over the directory,
               * but for one filling the map *)
              let s = Float.min (fit_size (String.length name)) (Float.min (h /. 4.) 90.) in
              if s >= 12. && w *. h < 0.4 *. float_of_int (a.pw * a.ph) then
                dirs := candidate ~rank:s ~alpha:0.35 ink s ((float_of_int x0 +. float_of_int x1) /. 2.) ((float_of_int y0 +. float_of_int y1) /. 2.) name :: !dirs;
              (* ours: its path on a tab at its top right, the way back to
               * it in the repository (claude: the right, the files' names
               * being at their left); only a directory with files of its
               * own, as the path says its parents' names *)
              let size = 13. in
              (* claude: with a / at its end, a directory's *)
              let dir = p.path ^ "/" in
              let tw = (0.5 *. size *. float_of_int (String.length dir)) +. 8. in
              let has_files = List.exists (function Treemap.File _ -> true | Dir _ -> false) kids in
              if has_files && w >= tw && h >= 40. then begin
                let tx = Float.min (float_of_int x1 -. tw -. 2.) (float_of_int a.pw -. tw) and ty = Float.max 0. (to_py c p.rect.y +. 2.) in
                let box, shape = tab a ~alpha:1. (lighter (archi t.colours p.path)) size tx ty dir in
                files := { rank = 300. -. float_of_int p.depth; box; shape } :: !files
              end
          | File (_, _, e), Some g ->
              (* claude: its name on a tab at its top left, always there
               * (the corner in view when the file is partly off the map),
               * as big as the file allows, up to 18; the program's own in
               * yellow, first *)
              let main = List.mem e.path t.marked in
              (* claude: numbered, its place in the reading order *)
              let name = match Hashtbl.find_opt t.order e.path with Some n -> Printf.sprintf "%d  %s" n name | None -> name in
              let s = Float.min (fit_size (String.length name + 2)) (Float.min ((h -. 6.) /. 2.) 18.) in
              let s = if main then Float.max s 12. else s in
              if s >= 9. then begin
                let box, shape = tab a (if main then yellow else lighter (archi t.colours p.path)) s (float_of_int x0 +. 1.) (float_of_int y0 +. 1.) name in
                files := { rank = (if main then 1000. else 100. +. s); box; shape } :: !files
              end;
              (* claude: the tricks (Code_file.marks), marked where they are *)
              if Lazy.is_val e.file && h >= 30. then
                List.iter
                  (fun line ->
                    if line < g.lpc * g.k then begin
                      let x, y = line_pos p.rect g line in
                      let box, shape = tab a ~alpha:1. (rgb 230 80 200) 13. (to_px c x) (to_py c y -. 19.) ("* " ^ Code_file.trick) in
                      files := { rank = 800.; box; shape } :: !files
                    end)
                  (Lazy.force e.file).marks;
              (* the semantic zoom: definitions written over the code *)
              if (not (readable c g)) && Lazy.is_val e.file then
                List.iter
                  (fun (line, def, cat) ->
                    let emph = emphasis e.path line def cat in
                    let size = Float.min 22. (g.cell_h *. c.z *. emph *. 1.6) in
                    if size >= 9. && line < Code_file.nlines (Lazy.force e.file) then begin
                      let col = line / g.lpc and lc = line mod g.lpc in
                      let px = to_px c (p.rect.x +. (float_of_int col *. g.colw)) and py = to_py c (p.rect.y +. ((float_of_int lc +. 0.5) *. g.cell_h)) in
                      let wd = 0.5 *. size *. float_of_int (String.length def) in
                      if py >= 0. && py < float_of_int a.ph && px +. wd > 0. && px < float_of_int a.pw then
                        let r, gg, b = Highlight_code.rgb cat in
                        defs := candidate ~rank:(emph *. size) (rgb r gg b) size (px +. (wd /. 2.)) py def :: !defs
                    end)
                  (Lazy.force e.file).defs
          | _ -> ()))
    t.placed;
  (* the directories' names faint under the rest, placed among themselves *)
  place a !dirs @ place a (!files @ !defs)

let labels = labels_by ~emphasis:(fun _ _ _ cat -> Highlight_code.emphasis cat)

(* claude: its labels are drawn, not kept: none to pick *)
let style : style = { sname = "classic"; paint; labels; pick = (fun _ _ _ _ _ -> None); unit_at = (fun _ _ _ _ _ -> None); units = false }
