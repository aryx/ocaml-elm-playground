(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Shading.mli *)

type mode = Flat_color | Flat_shading | Gouraud | Phong

let make (mode : mode) (v0 : Project.vertex) (v1 : Project.vertex) (v2 : Project.vertex) :
    l0:float -> l1:float -> l2:float -> float =
  match mode with
  | Flat_color -> fun ~l0:_ ~l1:_ ~l2:_ -> 1.
  | Flat_shading ->
      let brightness = Lighting.brightness_of_normal v0.normal in
      fun ~l0:_ ~l1:_ ~l2:_ -> brightness
  | Gouraud ->
      let b0 = Lighting.brightness_of_normal v0.normal
      and b1 = Lighting.brightness_of_normal v1.normal
      and b2 = Lighting.brightness_of_normal v2.normal in
      fun ~l0 ~l1 ~l2 -> (l0 *. b0) +. (l1 *. b1) +. (l2 *. b2)
  | Phong ->
      let (n0x, n0y, n0z) = v0.normal and (n1x, n1y, n1z) = v1.normal and (n2x, n2y, n2z) = v2.normal in
      fun ~l0 ~l1 ~l2 ->
        let nx = (l0 *. n0x) +. (l1 *. n1x) +. (l2 *. n2x)
        and ny = (l0 *. n0y) +. (l1 *. n1y) +. (l2 *. n2y)
        and nz = (l0 *. n0z) +. (l1 *. n1z) +. (l2 *. n2z) in
        Lighting.brightness_of_normal (Vec3.normalize (nx, ny, nz))
