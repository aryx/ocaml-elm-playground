(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sobel.mli *)

let magnitude (img : Pixels.image) (x : int) (y : int) (c : int) : float =
  let p dx dy = Pixels.get img (x + dx) (y + dy) c in
  let gx = -p (-1) (-1) + p 1 (-1) - (2 * p (-1) 0) + (2 * p 1 0) - p (-1) 1 + p 1 1 in
  let gy = -p (-1) (-1) - (2 * p 0 (-1)) - p 1 (-1) + p (-1) 1 + (2 * p 0 1) + p 1 1 in
  sqrt (float_of_int ((gx * gx) + (gy * gy)))

let find_edges (img : Pixels.image) : Pixels.image =
  let out = Pixels.copy img in
  for y = 0 to img.height - 1 do
    for x = 0 to img.width - 1 do
      for c = 0 to 2 do
        Pixels.set out x y c (Pixels.clamp (255 - int_of_float (magnitude img x y c)))
      done
    done
  done;
  out
