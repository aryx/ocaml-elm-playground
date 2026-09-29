(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_ground.mli *)

type place = { col : int; y : float; h : float }
type t = { places : place array; cols : int; colw : float; unit : float; ox : float; oy : float }

(*****************************************************************************)
(* The weights *)
(*****************************************************************************)

let weights (f : Code_file.t) ~(important : (int * int) list) : float array =
  let n = Code_file.nlines f in
  let cols = Code_file.cols in
  let w = Array.make n 1. in
  for l = 0 to n - 1 do
    (* the line's first character, its category; blank, or all stars *)
    let first = ref None and stars = ref true in
    for c = 0 to cols - 1 do
      let ch = Bytes.get f.chars ((l * cols) + c) in
      if ch <> '\000' && ch <> ' ' then begin
        if !first = None then first := Code_file.at f l c;
        if ch <> '*' && ch <> '(' && ch <> ')' then stars := false
      end
    done;
    w.(l) <-
      (match !first with
      | None -> 0.35
      | Some _ when !stars -> 0.35
      | Some (Comment | Comment_section) -> 0.8
      | Some _ -> 1.)
  done;
  List.iter
    (fun (l, _, (cat : Highlight_code.category)) ->
      if l >= 0 && l < n then
        match cat with
        | Def_function | Def_value | Def_type | Def_module -> w.(l) <- Float.max w.(l) 3.
        | Comment_section -> w.(l) <- Float.max w.(l) 4.
        | _ -> ())
    f.defs;
  List.iter (fun (l, k) -> if l >= 0 && l < n then w.(l) <- Float.max w.(l) (2.5 +. float_of_int k)) important;
  w

(*****************************************************************************)
(* The layout *)
(*****************************************************************************)

let max_unit = 18.

let layout ?(x0 = 0.) ?(y0 = 0.) (weights : float array) ~(pw : int) ~(ph : int) : t =
  let total = Float.max 1. (Array.fold_left ( +. ) 0. weights) in
  let pw = float_of_int pw and ph = float_of_int ph in
  (* as many columns as keep 80 characters (40 units) in a column *)
  let k = max 1 (int_of_float (Float.sqrt (pw *. total /. (40. *. ph)))) in
  (* but not so many that the unit would pass its most *)
  let k = max 1 (min k (int_of_float (max_unit *. total /. ph))) in
  (* a little room, lines not being cut between columns *)
  let unit = Float.min max_unit (float_of_int k *. ph /. (total *. 1.03)) in
  let col = ref 0 and y = ref 0. in
  let places =
    Array.map
      (fun w ->
        let h = w *. unit in
        if !y +. h > ph && !y > 0. then (incr col; y := 0.);
        let p = { col = !col; y = !y; h } in
        y := !y +. h;
        p)
      weights
  in
  { places; cols = !col + 1; colw = pw /. float_of_int (max k (!col + 1)); unit; ox = x0; oy = y0 }

let scale (g : t) (q : float) : t =
  { g with places = Array.map (fun p -> { p with y = p.y *. q; h = p.h *. q }) g.places; colw = g.colw *. q; unit = g.unit *. q; ox = g.ox *. q; oy = g.oy *. q }

let pad = 8.

let box (g : t) (l : int) : float * float * float * float =
  let p = g.places.(l) in
  (g.ox +. (float_of_int p.col *. g.colw) +. pad, g.oy +. p.y, g.colw -. (2. *. pad), p.h)

let cell_w (g : t) (l : int) : float =
  let _, _, w, h = box g l in
  if h >= 7. then h /. 2. else Float.min ((h /. 2.) +. 1.5) (w /. 80.)

let line_at (g : t) (x : float) (y : float) : int option =
  let found = ref None in
  Array.iteri
    (fun l _ ->
      let x0, y0, w, h = box g l in
      if x >= x0 && x < x0 +. w && y >= y0 && y < y0 +. h then found := Some l)
    g.places;
  !found

(*****************************************************************************)
(* Painting *)
(*****************************************************************************)

let palette = Array.map Highlight_code.rgb Highlight_code.all

let paint (img : Rgba_image.t) (f : Code_file.t) (g : t) ~(bg : int * int * int) ~(aa : bool) : unit =
  let cols = Code_file.cols in
  let br, bgc, bb = bg in
  let set x y (r, gg, b) (a : int) (all : int) =
    if x >= 0 && y >= 0 && x < img.width && y < img.height then begin
      let i = 4 * ((y * img.width) + x) in
      Bigarray.Array1.unsafe_set img.rgba i (br + ((r - br) * a / all));
      Bigarray.Array1.unsafe_set img.rgba (i + 1) (bgc + ((gg - bgc) * a / all));
      Bigarray.Array1.unsafe_set img.rgba (i + 2) (bb + ((b - bb) * a / all));
      Bigarray.Array1.unsafe_set img.rgba (i + 3) 255
    end
  in
  Array.iteri
    (fun l (p : place) ->
      let x0, y0, w, h = box g l in
      let glyphs = h >= 7. in
      (* a character's cell: half as wide as high; a bar's, a column's
       * eightieth *)
      let cw = cell_w g l in
      let ch_h = if glyphs then h else Float.max 1. (h -. 1.) in
      ignore p;
      let px0 = int_of_float x0 and px1 = int_of_float (x0 +. w) in
      let py0 = int_of_float y0 and py1 = int_of_float (y0 +. ch_h) in
      let ss = if glyphs && aa then 2 else 1 in
      for y = py0 to max py0 (py1 - 1) do
        for x = px0 to px1 - 1 do
          let hits = ref 0 and ink = ref 0 in
          for ky = 0 to ss - 1 do
            for kx = 0 to ss - 1 do
              let fx = (float_of_int x +. ((float_of_int kx +. 0.5) /. float_of_int ss) -. x0) /. cw in
              let fy = (float_of_int y +. ((float_of_int ky +. 0.5) /. float_of_int ss) -. y0) /. ch_h in
              let c = int_of_float fx in
              if c >= 0 && c < cols && fy >= 0. && fy < 1. then begin
                let cell = (l * cols) + c in
                let code = Char.code (Bytes.unsafe_get f.grid cell) in
                if code <> 0 then begin
                  let hit =
                    (not glyphs)
                    ||
                    let gx = min (Vga_font.width - 1) (int_of_float ((fx -. float_of_int c) *. float_of_int Vga_font.width)) in
                    let gy = min (Vga_font.height - 1) (int_of_float (fy *. float_of_int Vga_font.height)) in
                    Vga_font.bit (Char.code (Bytes.unsafe_get f.chars cell)) gx gy
                  in
                  if hit then (incr hits; ink := code)
                end
              end
            done
          done;
          if !hits > 0 then set x y palette.(!ink - 1) !hits (ss * ss)
        done
      done)
    g.places
