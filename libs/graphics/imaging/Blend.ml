(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Blend.mli *)

type mode = Normal | Multiply | Screen | Overlay | Hard_light | Darken | Lighten | Difference | Color

let modes = [ Normal; Multiply; Screen; Overlay; Hard_light; Darken; Lighten; Difference; Color ]

let name = function
  | Normal -> "Normal"
  | Multiply -> "Multiply"
  | Screen -> "Screen"
  | Overlay -> "Overlay"
  | Hard_light -> "Hard Light"
  | Darken -> "Darken"
  | Lighten -> "Lighten"
  | Difference -> "Difference"
  | Color -> "Color"

let screen b s = 1. -. ((1. -. b) *. (1. -. s))
let hard_light b s = if s <= 0.5 then b *. 2. *. s else screen b ((2. *. s) -. 1.)

let channel (mode : mode) (b : float) (s : float) : float =
  match mode with
  | Normal | Color -> s
  | Multiply -> b *. s
  | Screen -> screen b s
  | Overlay -> hard_light s b
  | Hard_light -> hard_light b s
  | Darken -> Float.min b s
  | Lighten -> Float.max b s
  | Difference -> Float.abs (b -. s)

let blend (mode : mode) ((br, bg, bb) : float * float * float) ((sr, sg, sb) : float * float * float) : float * float * float =
  match mode with
  | Color ->
      let byte v = int_of_float (Float.round (v *. 255.)) in
      let h, s, _ = Hsl.of_rgb (byte sr) (byte sg) (byte sb) and _, _, l = Hsl.of_rgb (byte br) (byte bg) (byte bb) in
      let r, g, b = Hsl.to_rgb h s l in
      (float_of_int r /. 255., float_of_int g /. 255., float_of_int b /. 255.)
  | _ -> (channel mode br sr, channel mode bg sg, channel mode bb sb)
