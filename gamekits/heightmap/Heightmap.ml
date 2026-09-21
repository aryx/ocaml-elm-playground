(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type t = { size : int; cells : float array; top : float; sea : float }

(*****************************************************************************)
(* Making it up *)
(*****************************************************************************)

(* every step keeps the low 30 bits: those of a product are the same
 * whether it was computed on 32 bits (the web) or 63 (native) *)
let random (seed : int) (i : int) (j : int) : float =
  let mask = 0x3FFFFFFF in
  let h = ((seed * 0x27d4eb2d) + (i * 0x165667b1) + (j * 0x1b873593)) land mask in
  let h = h lxor (h lsr 15) in
  let h = (h * 0x2c1b3c6d) land mask in
  let h = h lxor (h lsr 12) in
  let h = (h * 0x297a2d39) land mask in
  let h = h lxor (h lsr 15) in
  (float_of_int h /. float_of_int mask *. 2.) -. 1.

(* Diamond-square, on a grid that wraps around (the cell after the last
 * is the first: no edges to special-case), [step] halved each time:
 *
 *    a . . . b       a . . . b        a . e . b
 *    . . . . .       . . . . .        . . . . .
 *    . . . . .  ->   . . m . .   ->   h . m . f
 *    . . . . .       . . . . .        . . . . .
 *    d . . . c       d . . . c        d . g . c
 *                   the diamond:     the square: e, f, g, h, the
 *                   m, a b c d's     average of the 4 points around
 *                   average + bump   (the next squares' m's too) + bump
 *)
let diamond_square (seed : int) (size : int) (roughness : float) : float array =
  let a = Array.make (size * size) 0. in
  let index i j = ((j land (size - 1)) * size) + (i land (size - 1)) in
  let get i j = a.(index i j) in
  let set i j v = a.(index i j) <- v in
  let rec go step bump =
    if step > 1 then begin
      let h = step / 2 in
      for y = 0 to (size / step) - 1 do
        for x = 0 to (size / step) - 1 do
          let i = x * step and j = y * step in
          let corners = get i j +. get (i + step) j +. get i (j + step) +. get (i + step) (j + step) in
          set (i + h) (j + h) ((corners /. 4.) +. (bump *. random seed (i + h) (j + h)))
        done
      done;
      for y = 0 to (size / h) - 1 do
        for x = 0 to (size / h) - 1 do
          (* the edges' middles: an odd number of halves from the corner *)
          if (x + y) land 1 = 1 then begin
            let i = x * h and j = y * h in
            let around = get (i - h) j +. get (i + h) j +. get i (j - h) +. get i (j + h) in
            set i j ((around /. 4.) +. (bump *. random seed i j))
          end
        done
      done;
      go h (bump *. roughness)
    end
  in
  go size 1.;
  a

let generate ~(seed : int) ~(size : int) ~(top : float) ~(roughness : float) : t =
  let a = diamond_square seed size roughness in
  let lo = Array.fold_left Float.min infinity a and hi = Array.fold_left Float.max neg_infinity a in
  (* 0..1, then lowered by the squared distance to the middle (0 there,
   * 1 at an edge's middle): the island *)
  let half = float_of_int size /. 2. in
  let island =
    Array.mapi
      (fun k h ->
        let dx = (float_of_int (k mod size) -. half) /. half and dy = (float_of_int (k / size) -. half) /. half in
        ((h -. lo) /. (hi -. lo)) -. (1.5 *. ((dx *. dx) +. (dy *. dy))))
      a
  in
  let high = Array.fold_left Float.max neg_infinity island in
  let sea = top /. 5. in
  (* 0 (and less) the sea's level, [high] the top *)
  { size; top; sea; cells = Array.map (fun h -> Float.max sea (sea +. (h /. high *. (top -. sea)))) island }

(*****************************************************************************)
(* Asking *)
(*****************************************************************************)

let cell (t : t) (i : int) (j : int) : float =
  if i < 0 || j < 0 || i >= t.size || j >= t.size then t.sea else t.cells.((j * t.size) + i)

let height (t : t) (x : float) (y : float) : float =
  let i = Float.to_int (Float.floor x) and j = Float.to_int (Float.floor y) in
  let fx = x -. Float.floor x and fy = y -. Float.floor y in
  let mix a b f = a +. ((b -. a) *. f) in
  mix (mix (cell t i j) (cell t (i + 1) j) fx) (mix (cell t i (j + 1)) (cell t (i + 1) (j + 1)) fx) fy

let clear (t : t) ((x1, y1, z1) : float * float * float) ((x2, y2, z2) : float * float * float) : bool =
  let n = Float.to_int (Float.hypot (x2 -. x1) (y2 -. y1)) + 1 in
  let above k =
    let f = float_of_int k /. float_of_int n in
    z1 +. ((z2 -. z1) *. f) > height t (x1 +. ((x2 -. x1) *. f)) (y1 +. ((y2 -. y1) *. f))
  in
  List.for_all above (List.init (n + 1) Fun.id)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

type kind = Sea | Sand | Grass | Forest | Rock | Snow

let kinds = [ Sea; Sand; Grass; Forest; Rock; Snow ]

let kind (t : t) (i : int) (j : int) : kind =
  let h = cell t i j in
  (* how high between the sea and the top, 0..1 *)
  let f = (h -. t.sea) /. (t.top -. t.sea) in
  if h <= t.sea then Sea else if f < 0.04 then Sand else if f < 0.35 then Grass else if f < 0.6 then Forest else if f < 0.82 then Rock else Snow

let light (t : t) (i : int) (j : int) : int =
  let slope = cell t i j -. cell t (i - 1) j in
  let steep = t.top /. 200. in
  (* going up eastward: the slope faces west, the sun *)
  if slope > steep then 0 else if slope < -.steep then 2 else 1

let color (k : kind) (light : int) : int * int * int =
  let r, g, b =
    match k with
    | Sea -> (40, 90, 170)
    | Sand -> (214, 196, 136)
    | Grass -> (86, 156, 64)
    | Forest -> (42, 104, 46)
    | Rock -> (128, 116, 104)
    | Snow -> (236, 238, 244)
  in
  let f = match (k, light) with Sea, _ -> 1. | _, 0 -> 1.18 | _, 1 -> 1. | _ -> 0.72 in
  let c x = min 255 (Float.to_int (float_of_int x *. f)) in
  (c r, c g, c b)
