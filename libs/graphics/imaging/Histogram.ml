(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Histogram.mli *)

type t = { red : int array; green : int array; blue : int array; luminance : int array }

let compute (img : Pixels.image) : t =
  let h = { red = Array.make 256 0; green = Array.make 256 0; blue = Array.make 256 0; luminance = Array.make 256 0 } in
  for i = 0 to (img.width * img.height) - 1 do
    let r = Bigarray.Array1.get img.rgba (4 * i) and g = Bigarray.Array1.get img.rgba ((4 * i) + 1) and b = Bigarray.Array1.get img.rgba ((4 * i) + 2) in
    h.red.(r) <- h.red.(r) + 1;
    h.green.(g) <- h.green.(g) + 1;
    h.blue.(b) <- h.blue.(b) + 1;
    let l = Pixels.luminance r g b in
    h.luminance.(l) <- h.luminance.(l) + 1
  done;
  h

let auto_levels ?(clip = 0.005) (h : t) : int * int =
  let total = Array.fold_left ( + ) 0 h.luminance in
  let limit = int_of_float (clip *. float_of_int total) in
  let rec from_dark v seen = if v >= 255 || seen + h.luminance.(v) > limit then v else from_dark (v + 1) (seen + h.luminance.(v)) in
  let rec from_bright v seen = if v <= 0 || seen + h.luminance.(v) > limit then v else from_bright (v - 1) (seen + h.luminance.(v)) in
  let black = from_dark 0 0 and white = from_bright 255 0 in
  if white > black then (black, white) else (0, 255)
