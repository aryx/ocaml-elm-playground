(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Gradient.mli *)

let linear ?selection ((ax, ay) : float * float) ((bx, by) : float * float) ((r0, g0, b0) : int * int * int) ((r1, g1, b1) : int * int * int)
    (img : Pixels.image) : Pixels.image =
  let dx = bx -. ax and dy = by -. ay in
  let len2 = Float.max 1e-9 ((dx *. dx) +. (dy *. dy)) in
  let out = Pixels.copy img in
  for y = 0 to img.height - 1 do
    for x = 0 to img.width - 1 do
      let px = float_of_int x +. 0.5 -. ax and py = float_of_int y +. 0.5 -. ay in
      let t = Float.min 1. (Float.max 0. (((px *. dx) +. (py *. dy)) /. len2)) in
      let mix a b = Pixels.clamp (int_of_float (Float.round (float_of_int a +. (float_of_int (b - a) *. t)))) in
      Pixels.set out x y 0 (mix r0 r1);
      Pixels.set out x y 1 (mix g0 g1);
      Pixels.set out x y 2 (mix b0 b1);
      Pixels.set out x y 3 255
    done
  done;
  match selection with Some m -> Composite.through m ~before:img ~after:out | None -> out
