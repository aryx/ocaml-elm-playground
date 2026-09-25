(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

let grey g =
  let v = int_of_float (Float.round (g *. 255.)) in
  rgb v v v

(* a stroke from a to b, [w] wide: a thin rectangle, turned *)
let segment color w (ax, ay) (bx, by) =
  let dx = bx -. ax and dy = by -. ay in
  let len = Float.sqrt ((dx *. dx) +. (dy *. dy)) in
  rectangle color (len +. w) w |> rotate (Float.atan2 dy dx *. 180. /. Float.pi) |> move ((ax +. bx) /. 2.) ((ay +. by) /. 2.)

let rec figure (f : Figure.t) =
  match f with
  | Line (a, b, s) -> [ segment black s.pen a b ]
  | Rect (b, s) ->
      let w = b.x1 -. b.x0 and h = b.y1 -. b.y0 in
      let cx = (b.x0 +. b.x1) /. 2. and cy = (b.y0 +. b.y1) /. 2. in
      (match s.fill with Some g -> [ rectangle (grey g) w h |> move cx cy ] | None -> [])
      @ [
          rectangle black (w +. s.pen) s.pen |> move cx b.y1;
          rectangle black (w +. s.pen) s.pen |> move cx b.y0;
          rectangle black s.pen (h +. s.pen) |> move b.x0 cy;
          rectangle black s.pen (h +. s.pen) |> move b.x1 cy;
        ]
  | Oval (b, s) ->
      let a = (b.x1 -. b.x0) /. 2. and r = (b.y1 -. b.y0) /. 2. in
      let cx = b.x0 +. a and cy = b.y0 +. r in
      (* the outline as short strokes round the ellipse *)
      let n = 64 in
      let pt k =
        let t = 2. *. Float.pi *. float_of_int k /. float_of_int n in
        (cx +. (a *. Float.cos t), cy +. (r *. Float.sin t))
      in
      (match s.fill with Some g -> [ oval (grey g) (2. *. a) (2. *. r) |> move cx cy ] | None -> [])
      @ List.init n (fun k -> segment black s.pen (pt k) (pt (k + 1)))
  | Text (b, s, size) ->
      let style = { Style.plain with size } in
      snd
        (String.fold_left
           (fun (x, acc) c ->
             let ch = String.make 1 c in
             (x +. Stroke_text.metrics style ch, acc @ Stroke_text.glyph black style ch ~x ~baseline:(b.y1 -. size)))
           (b.x0, []) s)
  | Group fs -> List.concat_map figure fs

