(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Composite.mli *)

let mix (a : int) (b : int) (m : int) : int = a + (((b - a) * m) + (if b >= a then 127 else -127)) / 255

let through (mask : Mask.t) ~(before : Pixels.image) ~(after : Pixels.image) : Pixels.image =
  let out = Pixels.copy before in
  for y = 0 to before.height - 1 do
    for x = 0 to before.width - 1 do
      let m = Mask.get mask x y in
      if m > 0 then
        for c = 0 to 3 do
          Pixels.set out x y c (mix (Pixels.get before x y c) (Pixels.get after x y c) m)
        done
    done
  done;
  out

let fill (mask : Mask.t) ((r, g, b) : int * int * int) (img : Pixels.image) : Pixels.image =
  (* opaque: paint poured on a layer's transparent pixels shows *)
  let colour = Pixels.map (fun _ _ _ _ -> (r, g, b, 255)) img in
  through mask ~before:img ~after:colour
