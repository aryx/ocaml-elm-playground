(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Hsl.mli *)

let of_rgb (r : int) (g : int) (b : int) : float * float * float =
  let r = float_of_int r /. 255. and g = float_of_int g /. 255. and b = float_of_int b /. 255. in
  let mx = Float.max r (Float.max g b) and mn = Float.min r (Float.min g b) in
  let l = (mx +. mn) /. 2. in
  if mx = mn then (0., 0., l)
  else
    let d = mx -. mn in
    let s = if l > 0.5 then d /. (2. -. mx -. mn) else d /. (mx +. mn) in
    let h = if mx = r then ((g -. b) /. d) +. if g < b then 6. else 0. else if mx = g then ((b -. r) /. d) +. 2. else ((r -. g) /. d) +. 4. in
    (h *. 60., s, l)

let to_rgb (h : float) (s : float) (l : float) : int * int * int =
  let byte v = Pixels.clamp (int_of_float (Float.round (v *. 255.))) in
  if s = 0. then (byte l, byte l, byte l)
  else
    let q = if l < 0.5 then l *. (1. +. s) else l +. s -. (l *. s) in
    let p = (2. *. l) -. q in
    let channel t =
      let t = if t < 0. then t +. 1. else if t > 1. then t -. 1. else t in
      if t < 1. /. 6. then p +. ((q -. p) *. 6. *. t)
      else if t < 1. /. 2. then q
      else if t < 2. /. 3. then p +. ((q -. p) *. ((2. /. 3.) -. t) *. 6.)
      else p
    in
    let h = Float.rem (Float.rem h 360. +. 360.) 360. /. 360. in
    (byte (channel (h +. (1. /. 3.))), byte (channel h), byte (channel (h -. (1. /. 3.))))

let hue_saturation ~(hue : float) ~(saturation : float) ~(lightness : float) (img : Pixels.image) : Pixels.image =
  (* a slider at +100 goes all the way (to full saturation, to white),
     at -100 all the way back (to grey, to black) *)
  let push v amount = if amount >= 0. then v +. ((1. -. v) *. amount /. 100.) else v *. (1. +. (amount /. 100.)) in
  Pixels.map
    (fun r g b a ->
      let h, s, l = of_rgb r g b in
      let r, g, b = to_rgb (h +. hue) (push s saturation) (push l lightness) in
      (r, g, b, a))
    img
