(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_road.mli *)

open Playground
open Code_map_base

(*****************************************************************************)
(* The curve *)
(*****************************************************************************)

let bspline ?(per = 8) (pts : (float * float) array) : (float * float) list =
  let n = Array.length pts in
  if n < 3 then Array.to_list pts
  else
    let p = Array.concat [ [| pts.(0); pts.(0) |]; pts; [| pts.(n - 1); pts.(n - 1) |] ] in
    let out = ref [ pts.(0) ] in
    for i = 0 to Array.length p - 4 do
      let (x0, y0), (x1, y1), (x2, y2), (x3, y3) = (p.(i), p.(i + 1), p.(i + 2), p.(i + 3)) in
      for k = 1 to per do
        let s = float_of_int k /. float_of_int per in
        let s2 = s *. s and s3 = s *. s *. s in
        let b0 = (1. -. s) ** 3. /. 6. and b1 = ((3. *. s3) -. (6. *. s2) +. 4.) /. 6. and b2 = ((-3. *. s3) +. (3. *. s2) +. (3. *. s) +. 1.) /. 6. and b3 = s3 /. 6. in
        out := ((b0 *. x0) +. (b1 *. x1) +. (b2 *. x2) +. (b3 *. x3), (b0 *. y0) +. (b1 *. y1) +. (b2 *. y2) +. (b3 *. y3)) :: !out
      done
    done;
    List.rev !out

(* a polyline cut into [k] pieces of the same length: a road of two
 * points is a straight line, one piece, and its gradient needs many *)
let resample (k : int) (pts : (float * float) list) : (float * float) array =
  let pts = Array.of_list pts in
  let n = Array.length pts in
  if n < 2 then pts
  else
    let len = Array.make n 0. in
    for i = 1 to n - 1 do
      let (x0, y0), (x1, y1) = (pts.(i - 1), pts.(i)) in
      len.(i) <- len.(i - 1) +. Float.sqrt (((x1 -. x0) ** 2.) +. ((y1 -. y0) ** 2.))
    done;
    let total = Float.max 1e-6 len.(n - 1) in
    let j = ref 1 in
    Array.init (k + 1) (fun i ->
        let d = total *. float_of_int i /. float_of_int k in
        while !j < n - 1 && len.(!j) < d do incr j done;
        let (x0, y0), (x1, y1) = (pts.(!j - 1), pts.(!j)) in
        let s = if len.(!j) > len.(!j - 1) then (d -. len.(!j - 1)) /. (len.(!j) -. len.(!j - 1)) else 0. in
        let s = Float.min 1. (Float.max 0. s) in
        (x0 +. (s *. (x1 -. x0)), y0 +. (s *. (y1 -. y0))))

(*****************************************************************************)
(* The road *)
(*****************************************************************************)

let user_end = (90, 220, 120)
let used_end = (250, 80, 70)

(* a quad a piece, [w] pixels wide at the user, a third of it at the used *)
let road ?(colours = (user_end, used_end)) (a : area) (pts : (float * float) list) (w : float) (alpha : float) : shape list =
  let user_end, used_end = colours in
  (* pieces of at most 16 pixels (up to 400): the gradient smooth, and
   * the pieces off the map left out close to its edge *)
  let len = fst (List.fold_left (fun (l, (px, py)) (x, y) -> (l +. Float.sqrt (((x -. px) ** 2.) +. ((y -. py) ** 2.)), (x, y))) (0., List.hd pts) pts) in
  let pts = resample (min 400 (max 28 (int_of_float (len /. 16.)))) pts in
  let n = Array.length pts in
  (* only on the map: a piece whose middle is off it is left out *)
  List.init (max 0 (n - 1)) Fun.id
  |> List.filter (fun i -> let (x0, y0), (x1, y1) = (pts.(i), pts.(i + 1)) in on a ((x0 +. x1) /. 2.) ((y0 +. y1) /. 2.))
  |> List.map (fun i ->
      let (x0, y0), (x1, y1) = (pts.(i), pts.(i + 1)) in
      let s0 = float_of_int i /. float_of_int (n - 1) and s1 = float_of_int (i + 1) /. float_of_int (n - 1) in
      let dx = x1 -. x0 and dy = y1 -. y0 in
      let d = Float.max 1e-6 (Float.sqrt ((dx *. dx) +. (dy *. dy))) in
      let nx = -.dy /. d and ny = dx /. d in
      let h0 = w *. (1. -. (0.66 *. s0)) /. 2. and h1 = w *. (1. -. (0.66 *. s1)) /. 2. in
      let r, g, b = mix used_end ((s0 +. s1) /. 2.) user_end in
      polygon (rgb r g b)
        [
          (sx a (x0 +. (nx *. h0)), sy a (y0 +. (ny *. h0)));
          (sx a (x1 +. (nx *. h1)), sy a (y1 +. (ny *. h1)));
          (sx a (x1 -. (nx *. h1)), sy a (y1 -. (ny *. h1)));
          (sx a (x0 -. (nx *. h0)), sy a (y0 -. (ny *. h0)));
        ]
      |> fade alpha)
