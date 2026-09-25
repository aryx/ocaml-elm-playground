(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Lut.mli *)

type t = int array

let of_function (f : int -> float) : t = Array.init 256 (fun v -> Pixels.clamp (int_of_float (Float.round (f v))))
let identity = Array.init 256 Fun.id
let invert = Array.init 256 (fun v -> 255 - v)

let posterize (n : int) : t =
  let n = max 2 (min 255 n) in
  (* n bands, each shown as its own value, from 0 to 255 *)
  Array.init 256 (fun v -> let band = min (n - 1) (v * n / 256) in band * 255 / (n - 1))

let brightness_contrast ~(brightness : int) ~(contrast : int) : t =
  let c = float_of_int (max (-100) (min 100 contrast)) in
  (* the slope through middle grey: from 0 (all grey) to steep (all
     black or white) *)
  let slope = if c >= 0. then 100. /. Float.max 1. (100. -. c) else (100. +. c) /. 100. in
  of_function (fun v -> ((float_of_int v -. 128.) *. slope) +. 128. +. (float_of_int brightness *. 1.28))

let levels ~(black : int) ~(white : int) ~(gamma : float) : t =
  let span = float_of_int (max 1 (white - black)) in
  of_function (fun v ->
      let t = Float.min 1. (Float.max 0. (float_of_int (v - black) /. span)) in
      255. *. (t ** (1. /. gamma)))

(* Fritsch and Carlson's monotone cubic: the tangents at the points
   chosen so that between two points the curve goes one way only *)
let curves (points : (int * int) list) : t =
  let points = List.sort_uniq (fun (a, _) (b, _) -> compare a b) points in
  let points = (if List.mem_assoc 0 points then [] else [ (0, 0) ]) @ points @ if List.mem_assoc 255 points then [] else [ (255, 255) ] in
  let xs = Array.of_list (List.map (fun (x, _) -> float_of_int x) points) and ys = Array.of_list (List.map (fun (_, y) -> float_of_int y) points) in
  let n = Array.length xs in
  if n < 2 then identity
  else
    let d = Array.init (n - 1) (fun i -> (ys.(i + 1) -. ys.(i)) /. (xs.(i + 1) -. xs.(i))) in
    let m = Array.init n (fun i -> if i = 0 then d.(0) else if i = n - 1 then d.(n - 2) else if d.(i - 1) *. d.(i) <= 0. then 0. else (d.(i - 1) +. d.(i)) /. 2.) in
    (* the tangents shortened where they would overshoot *)
    for i = 0 to n - 2 do
      if d.(i) = 0. then begin
        m.(i) <- 0.;
        m.(i + 1) <- 0.
      end
      else
        let a = m.(i) /. d.(i) and b = m.(i + 1) /. d.(i) in
        let s = (a *. a) +. (b *. b) in
        if s > 9. then begin
          let t = 3. /. sqrt s in
          m.(i) <- t *. a *. d.(i);
          m.(i + 1) <- t *. b *. d.(i)
        end
    done;
    of_function (fun v ->
        let x = float_of_int v in
        let rec seg i = if i >= n - 2 || x <= xs.(i + 1) then i else seg (i + 1) in
        let i = seg 0 in
        let h = xs.(i + 1) -. xs.(i) in
        let t = Float.min 1. (Float.max 0. ((x -. xs.(i)) /. h)) in
        let t2 = t *. t and t3 = t *. t *. t in
        (((2. *. t3) -. (3. *. t2) +. 1.) *. ys.(i))
        +. ((t3 -. (2. *. t2) +. t) *. h *. m.(i))
        +. (((-2. *. t3) +. (3. *. t2)) *. ys.(i + 1))
        +. ((t3 -. t2) *. h *. m.(i + 1)))

let compose (f : t) (g : t) : t = Array.init 256 (fun v -> g.(f.(v)))
let apply_rgb (r : t) (g : t) (b : t) (img : Pixels.image) : Pixels.image = Pixels.map (fun rv gv bv a -> (r.(rv), g.(gv), b.(bv), a)) img
let apply (t : t) (img : Pixels.image) : Pixels.image = apply_rgb t t t img

let threshold (level : int) (img : Pixels.image) : Pixels.image =
  Pixels.map (fun r g b a -> let v = if Pixels.luminance r g b >= level then 255 else 0 in (v, v, v, a)) img

let desaturate (img : Pixels.image) : Pixels.image = Pixels.map (fun r g b a -> let v = Pixels.luminance r g b in (v, v, v, a)) img
