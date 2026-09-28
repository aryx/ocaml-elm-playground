(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Add_noise.mli *)

let add ?(monochrome = false) ~(amount : int) ~(seed : int) (img : Pixels.image) : Pixels.image =
  let rng = ref (Lehmer.of_int seed) in
  let draw () =
    rng := Lehmer.next !rng;
    int_of_float (Lehmer.to_unit !rng *. float_of_int ((2 * amount) + 1)) - amount
  in
  Pixels.map
    (fun r g b a ->
      if monochrome then
        let d = draw () in
        (Pixels.clamp (r + d), Pixels.clamp (g + d), Pixels.clamp (b + d), a)
      else
        let dr = draw () in
        let dg = draw () in
        let db = draw () in
        (Pixels.clamp (r + dr), Pixels.clamp (g + dg), Pixels.clamp (b + db), a))
    img
