(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Convolve.mli *)

type kernel = { size : int; weights : int array; divisor : int; offset : int }

let apply (k : kernel) (img : Pixels.image) : Pixels.image =
  let out = Pixels.copy img in
  let half = k.size / 2 in
  for y = 0 to img.height - 1 do
    for x = 0 to img.width - 1 do
      for c = 0 to 2 do
        let sum = ref 0 in
        for j = 0 to k.size - 1 do
          for i = 0 to k.size - 1 do
            let w = k.weights.((j * k.size) + i) in
            if w <> 0 then sum := !sum + (w * Pixels.get img (x + i - half) (y + j - half) c)
          done
        done;
        Pixels.set out x y c (Pixels.clamp ((!sum / k.divisor) + k.offset))
      done
    done
  done;
  out

let separable (weights : float array) (img : Pixels.image) : Pixels.image =
  let n = Array.length weights and half = Array.length weights / 2 in
  let pass (src : Pixels.image) ~(horizontal : bool) : Pixels.image =
    let out = Pixels.copy src in
    for y = 0 to src.height - 1 do
      for x = 0 to src.width - 1 do
        for c = 0 to 2 do
          let sum = ref 0. in
          for i = 0 to n - 1 do
            let d = i - half in
            let v = if horizontal then Pixels.get src (x + d) y c else Pixels.get src x (y + d) c in
            sum := !sum +. (weights.(i) *. float_of_int v)
          done;
          Pixels.set out x y c (Pixels.clamp (int_of_float (Float.round !sum)))
        done
      done
    done;
    out
  in
  pass (pass img ~horizontal:true) ~horizontal:false

let blur = { size = 3; weights = Array.make 9 1; divisor = 9; offset = 0 }
let blur_more = { size = 5; weights = Array.make 25 1; divisor = 25; offset = 0 }
let sharpen = { size = 3; weights = [| 0; -1; 0; -1; 5; -1; 0; -1; 0 |]; divisor = 1; offset = 0 }
let sharpen_more = { size = 3; weights = [| -1; -1; -1; -1; 9; -1; -1; -1; -1 |]; divisor = 1; offset = 0 }
let emboss = { size = 3; weights = [| -1; -1; 0; -1; 0; 1; 0; 1; 1 |]; divisor = 1; offset = 128 }
