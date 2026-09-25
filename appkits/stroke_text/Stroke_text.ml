(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Stroke_text.mli *)

(* Hershey has glyphs for printable ASCII; anything else is its '?' *)
let char_of (s : string) = if String.length s = 1 then s.[0] else '?'

(* font units to the playground's, at a look's size *)
let scale_of (look : Style.t) = look.size /. Hershey.units_per_em

let metrics look s =
  let g = Hershey.glyph (char_of s) in
  float_of_int (g.right - g.left) *. scale_of look

let glyph color (look : Style.t) s ~x ~baseline =
  let g = Hershey.glyph (char_of s) in
  let k = scale_of look in
  (* a look is a pen: thicker for bold *)
  let pen = if look.bold then look.size /. 7. else look.size /. 16. in
  let slant = if look.italic then 0.2 else 0. in
  (* a point of the glyph, in the playground: Hershey's y goes down
   * with the baseline at 9, the playground's goes up from it *)
  let at (gx, gy) =
    let up = float_of_int (9 - gy) *. k in
    (x +. (float_of_int (gx - g.left) *. k) +. (up *. slant), baseline +. up)
  in
  let segment (x1, y1) (x2, y2) =
    let dx = x2 -. x1 and dy = y2 -. y1 in
    (* one pen-width longer than the segment, so that joints overlap
     * rather than leaving notches *)
    Playground.rectangle color (sqrt ((dx *. dx) +. (dy *. dy)) +. pen) pen
    |> Playground.rotate (atan2 dy dx *. 180. /. Float.pi)
    |> Playground.move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)
  in
  let rec pairs = function a :: (b :: _ as rest) -> segment a b :: pairs rest | _ -> [] in
  let strokes = List.concat_map (fun stroke -> pairs (List.map at stroke)) g.strokes in
  let advance = float_of_int (g.right - g.left) *. k in
  let rule y =
    Playground.rectangle color advance (pen *. 0.8) |> Playground.move (x +. (advance /. 2.)) y
  in
  strokes
  @ (if look.underline then [ rule (baseline -. (look.size *. 0.18)) ] else [])
  @ if look.strike then [ rule (baseline +. (look.size *. 0.25)) ] else []
