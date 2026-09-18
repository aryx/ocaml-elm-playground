(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Hershey.mli for the font format, with an example *)

type glyph = { left : int; right : int; strokes : (int * int) list list }

(*****************************************************************************)
(* Decoding *)
(*****************************************************************************)

let coordinate (c : char) : int = Char.code c - Char.code 'R'

let decode_glyph (s : string) : glyph =
  (* the characters two by two: the left/right pair, then the points *)
  let pairs = List.init (String.length s / 2) (fun i -> (s.[2 * i], s.[(2 * i) + 1])) in
  match pairs with
  | [] -> failwith "empty glyph"
  | (l, r) :: points ->
      (* split the points into strokes at each pen up, " R" *)
      let finish stroke strokes = if stroke = [] then strokes else List.rev stroke :: strokes in
      let stroke, strokes =
        List.fold_left
          (fun (stroke, strokes) (cx, cy) ->
            if cx = ' ' && cy = 'R' then ([], finish stroke strokes)
            else ((coordinate cx, coordinate cy) :: stroke, strokes))
          ([], []) points
      in
      { left = coordinate l; right = coordinate r; strokes = List.rev (finish stroke strokes) }

(*****************************************************************************)
(* The font *)
(*****************************************************************************)

(* One line per glyph, for ASCII 32 (space) to 127 in order; each line
 * starts with a 5-character number and a 3-character count *)
let font : glyph array Lazy.t =
  lazy
    (String.split_on_char '\n' Hershey_futural.jhf
    |> List.filter (fun line -> String.length line >= 10)
    |> List.map (fun line -> decode_glyph (String.sub line 8 (String.length line - 8)))
    |> Array.of_list)

let glyph (c : char) : glyph =
  let font = Lazy.force font in
  let i = Char.code c - 32 in
  if i >= 0 && i < Array.length font then font.(i) else font.(Char.code '?' - 32)

(* Capital letters go from -12 to 9, 21 units, and in usual fonts
 * capitals are about 0.7 em tall: 21 / 0.7 = 30 *)
let units_per_em = 30.

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

let layout (str : string) : (float * float) list list * float =
  let strokes, width =
    List.fold_left
      (fun (strokes, x) c ->
        let g = glyph c in
        (* move the glyph so that its left side is at x *)
        let dx = x - g.left in
        let moved = List.map (List.map (fun (px, py) -> (float (px + dx), float py))) g.strokes in
        (strokes @ moved, x + (g.right - g.left)))
      ([], 0)
      (List.init (String.length str) (String.get str))
  in
  (strokes, float width)
