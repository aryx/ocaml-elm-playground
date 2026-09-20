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
open Basics (* float arithmetics *)

type surface = { solid : bool array; angle : number }
type mode = Floor | Right_wall | Ceiling | Left_wall

let mode_of (angle : number) : mode =
  let a = Float.rem (Float.rem angle 360. + 360.) 360. in
  if a < 45. || a >= 315. then Floor else if a < 135. then Right_wall else if a < 225. then Ceiling else Left_wall

let down (mode : mode) : number * number =
  match mode with Floor -> (0., -1.) | Right_wall -> (1., 0.) | Ceiling -> (0., 1.) | Left_wall -> (-1., 0.)

(*****************************************************************************)
(* The tiles *)
(*****************************************************************************)

(* a tile's pixels, from its bottom-left corner *)
let make (size : int) (angle : number) (f : int -> int -> bool) : surface =
  { solid = Array.init (size *.. size) (fun i -> f (i mod size) (i /.. size)); angle }

let block (size : int) : surface = make size 0. (fun _ _ -> true)
let empty (size : int) : surface = make size 0. (fun _ _ -> false)

let slope (size : int) ~(from_ : int) ~(to_ : int) : surface =
  let angle = atan2 (float_of_int (to_ -.. from_)) (float_of_int size) * 180. / pi in
  make size angle (fun x y ->
      (* the height of the line over this column, its left edge at
       * [from_] and its right edge at [to_] *)
      let h = float_of_int from_ + ((float_of_int (to_ -.. from_)) * (float_of_int x +. 0.5) / float_of_int size) in
      float_of_int y < h)

let ring (size : int) ~(cx : number) ~(cy : number) ~(radius : number) ~(thickness : number) ~(inside : bool) : surface =
  (* the surface the hero walks on is the band's inner edge for a loop,
   * its outer edge for a ball; the angle is that face's normal where
   * the circle crosses the middle of the tile *)
  let mx = float_of_int size / 2. and my = float_of_int size / 2. in
  let out = atan2 (my - cy) (mx - cx) * 180. / pi in
  let facing = if inside then out + 180. else out in
  let angle = Float.rem (facing -. 90. +. 720.) 360. in
  make size angle (fun x y ->
      let d = Float.hypot (float_of_int x +. 0.5 -. cx) (float_of_int y +. 0.5 -. cy) in
      d >= radius && d <= radius +. thickness)

(*****************************************************************************)
(* The sensors *)
(*****************************************************************************)

(* [solid_at]: is this world pixel inside the ground? *)
let solid_at ~(tiles : int * int -> surface option) ~(size : int) (px : int) (py : int) : surface option =
  let floor_div a b = if a >= 0 then a /.. b else ((a +.. 1) /.. b) -.. 1 in
  let tx = floor_div px size and ty = floor_div py size in
  match tiles (tx, ty) with
  | None -> None
  | Some s ->
      let lx = px -.. (tx *.. size) and ly = py -.. (ty *.. size) in
      if s.solid.((ly *.. size) +.. lx) then Some s else None

(* the sensor looks along [down mode]: backwards out of the ground it
 * stands in, or forwards for the ground below it; at most a tile each
 * way, as Sonic's do *)
let ground ~(tiles : int * int -> surface option) ~(size : int) (mode : mode) ((x, y) : number * number) :
    (number * number) option =
  let dx, dy = down mode in
  let px = int_of_float (Float.round x) and py = int_of_float (Float.round y) in
  let at k =
    solid_at ~tiles ~size (px +.. int_of_float (dx *. float_of_int k)) (py +.. int_of_float (dy *. float_of_int k))
  in
  (* where the feet rest: the last empty pixel before the solid, on the
   * axis the mode walks on *)
  let foot k = if dx <> 0. then x +. (dx *. float_of_int k) else y +. (dy *. float_of_int k) in
  (* above the ground: down to the first solid pixel, the feet on the
   * last empty one *)
  let rec forward k = if k > size then None else match at k with Some s -> Some (foot (k -.. 1), s.angle) | None -> forward (k +.. 1) in
  (* inside the ground: up to the first empty pixel, the feet on it *)
  let rec backward k angle =
    if k > size then None else match at (-k) with Some s -> backward (k +.. 1) s.angle | None -> Some (foot (-k), angle)
  in
  match at 0 with Some s -> backward 1 s.angle | None -> forward 1
