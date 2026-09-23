(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Texture.mli *)

type image = {
  width : int;
  height : int;
  rgba : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

let sample_bilinear (img : image) ~(u : float) ~(v : float) : int * int * int =
  let clamp lo hi x = if x < lo then lo else if x > hi then hi else x in
  (* texel i's center is at i + 0.5 *)
  let x = (u *. float_of_int img.width) -. 0.5 and y = (v *. float_of_int img.height) -. 0.5 in
  let i = int_of_float (Float.floor x) and j = int_of_float (Float.floor y) in
  let tx = x -. Float.floor x and ty = y -. Float.floor y in
  let texel i j =
    let i = clamp 0 (img.width - 1) i and j = clamp 0 (img.height - 1) j in
    let idx = ((j * img.width) + i) * 4 in
    fun k -> float_of_int (Bigarray.Array1.unsafe_get img.rgba (idx + k))
  in
  let t00 = texel i j and t10 = texel (i + 1) j and t01 = texel i (j + 1) and t11 = texel (i + 1) (j + 1) in
  let channel k =
    let top = (t00 k *. (1. -. tx)) +. (t10 k *. tx) and bottom = (t01 k *. (1. -. tx)) +. (t11 k *. tx) in
    int_of_float ((top *. (1. -. ty)) +. (bottom *. ty) +. 0.5)
  in
  (channel 0, channel 1, channel 2)

let sample_nearest (img : image) ~(u : float) ~(v : float) : int * int * int =
  let clamp01 x = if x < 0. then 0. else if x > 1. then 1. else x in
  let x = min (img.width - 1) (int_of_float (clamp01 u *. float_of_int img.width)) in
  let y = min (img.height - 1) (int_of_float (clamp01 v *. float_of_int img.height)) in
  let idx = ((y * img.width) + x) * 4 in
  let data = img.rgba in
  ( Bigarray.Array1.unsafe_get data idx,
    Bigarray.Array1.unsafe_get data (idx + 1),
    Bigarray.Array1.unsafe_get data (idx + 2) )
