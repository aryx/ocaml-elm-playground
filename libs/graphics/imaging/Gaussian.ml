(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Gaussian.mli *)

let kernel (sigma : float) : float array =
  let sigma = Float.max 0.1 sigma in
  let half = int_of_float (Float.ceil (3. *. sigma)) in
  let w = Array.init ((2 * half) + 1) (fun i -> let d = float_of_int (i - half) in exp (-.(d *. d) /. (2. *. sigma *. sigma))) in
  let total = Array.fold_left ( +. ) 0. w in
  Array.map (fun v -> v /. total) w

let blur ~(radius : float) (img : Pixels.image) : Pixels.image = if radius <= 0. then Pixels.copy img else Convolve.separable (kernel radius) img

let unsharp ~(amount : float) ~(radius : float) ~(threshold : int) (img : Pixels.image) : Pixels.image =
  let blurred = blur ~radius img in
  let out = Pixels.copy img in
  for y = 0 to img.height - 1 do
    for x = 0 to img.width - 1 do
      for c = 0 to 2 do
        let v = Pixels.get img x y c in
        let detail = v - Pixels.get blurred x y c in
        if abs detail >= threshold then Pixels.set out x y c (Pixels.clamp (v + int_of_float (Float.round (float_of_int detail *. amount /. 100.))))
      done
    done
  done;
  out
