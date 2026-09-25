(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Scale.mli *)

type method_ = Nearest | Bilinear | Bicubic

(* Keys's cubic, a = -1/2: the weight of a pixel at distance d *)
let cubic (d : float) : float =
  let d = Float.abs d and a = -0.5 in
  if d <= 1. then ((a +. 2.) *. d *. d *. d) -. ((a +. 3.) *. d *. d) +. 1.
  else if d < 2. then (a *. d *. d *. d) -. (5. *. a *. d *. d) +. (8. *. a *. d) -. (4. *. a)
  else 0.

let resize (m : method_) ~(width : int) ~(height : int) (img : Pixels.image) : Pixels.image =
  let out = Rgba_image.create ~width ~height in
  let sx = float_of_int img.width /. float_of_int width and sy = float_of_int img.height /. float_of_int height in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      (* the point of the old picture the new pixel's centre falls on *)
      let fx = ((float_of_int x +. 0.5) *. sx) -. 0.5 and fy = ((float_of_int y +. 0.5) *. sy) -. 0.5 in
      for c = 0 to 3 do
        let v =
          match m with
          | Nearest -> float_of_int (Pixels.get img (int_of_float (Float.round fx)) (int_of_float (Float.round fy)) c)
          | Bilinear ->
              let x0 = int_of_float (Float.floor fx) and y0 = int_of_float (Float.floor fy) in
              let tx = fx -. float_of_int x0 and ty = fy -. float_of_int y0 in
              let p i j = float_of_int (Pixels.get img (x0 + i) (y0 + j) c) in
              (((p 0 0 *. (1. -. tx)) +. (p 1 0 *. tx)) *. (1. -. ty)) +. (((p 0 1 *. (1. -. tx)) +. (p 1 1 *. tx)) *. ty)
          | Bicubic ->
              let x0 = int_of_float (Float.floor fx) and y0 = int_of_float (Float.floor fy) in
              let sum = ref 0. in
              for j = -1 to 2 do
                for i = -1 to 2 do
                  let w = cubic (fx -. float_of_int (x0 + i)) *. cubic (fy -. float_of_int (y0 + j)) in
                  sum := !sum +. (w *. float_of_int (Pixels.get img (x0 + i) (y0 + j) c))
                done
              done;
              !sum
        in
        Pixels.set out x y c (Pixels.clamp (int_of_float (Float.round v)))
      done
    done
  done;
  out

let remap ~(width : int) ~(height : int) (f : int -> int -> int * int) (img : Pixels.image) : Pixels.image =
  let out = Rgba_image.create ~width ~height in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let sx, sy = f x y in
      for c = 0 to 3 do
        Pixels.set out x y c (Pixels.get img sx sy c)
      done
    done
  done;
  out

let flip_horizontal (img : Pixels.image) = remap ~width:img.width ~height:img.height (fun x y -> (img.width - 1 - x, y)) img
let flip_vertical (img : Pixels.image) = remap ~width:img.width ~height:img.height (fun x y -> (x, img.height - 1 - y)) img
let rotate_90 (img : Pixels.image) = remap ~width:img.height ~height:img.width (fun x y -> (y, img.height - 1 - x)) img
