(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pixels.mli *)

type image = Rgba_image.t

let copy (img : image) : image =
  let c = Rgba_image.create ~width:img.width ~height:img.height in
  Bigarray.Array1.blit img.rgba c.rgba;
  c

let get (img : image) (x : int) (y : int) (c : int) : int =
  let x = if x < 0 then 0 else if x >= img.width then img.width - 1 else x in
  let y = if y < 0 then 0 else if y >= img.height then img.height - 1 else y in
  Bigarray.Array1.get img.rgba ((4 * ((y * img.width) + x)) + c)

let set (img : image) (x : int) (y : int) (c : int) (v : int) : unit = Bigarray.Array1.set img.rgba ((4 * ((y * img.width) + x)) + c) v
let clamp (v : int) : int = if v < 0 then 0 else if v > 255 then 255 else v
let luminance (r : int) (g : int) (b : int) : int = ((299 * r) + (587 * g) + (114 * b) + 500) / 1000

let map (f : int -> int -> int -> int -> int * int * int * int) (img : image) : image =
  let out = copy img in
  let a = out.rgba in
  for i = 0 to (img.width * img.height) - 1 do
    let o = 4 * i in
    let r, g, b, al = f (Bigarray.Array1.get a o) (Bigarray.Array1.get a (o + 1)) (Bigarray.Array1.get a (o + 2)) (Bigarray.Array1.get a (o + 3)) in
    Bigarray.Array1.set a o r;
    Bigarray.Array1.set a (o + 1) g;
    Bigarray.Array1.set a (o + 2) b;
    Bigarray.Array1.set a (o + 3) al
  done;
  out
