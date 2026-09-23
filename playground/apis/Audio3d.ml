(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Audio3d.mli *)

open Playground

type listener = { position : Space.vec; right : Space.vec; velocity : Space.vec }

let vec ((x, y, z) : number * number * number) : Space.vec = Space.vec x y z

let normalize (v : Space.vec) : Space.vec =
  let n = sqrt ((v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z)) in
  if n = 0. then v else Space.vec (v.x /. n) (v.y /. n) (v.z /. n)

(* ahead x up: the right, for a camera looking along -z with +y up, +x *)
let cross (a : Space.vec) (b : Space.vec) : Space.vec =
  Space.vec ((a.y *. b.z) -. (a.z *. b.y)) ((a.z *. b.x) -. (a.x *. b.z)) ((a.x *. b.y) -. (a.y *. b.x))

let listener ?(velocity = (0., 0., 0.)) (camera : Playground3d.camera) : listener =
  let eye = vec camera.eye and target = vec camera.target in
  let ahead = normalize (Space.vec (target.x -. eye.x) (target.y -. eye.y) (target.z -. eye.z)) in
  { position = eye; right = normalize (cross ahead (vec camera.up)); velocity = vec velocity }

let pan (ears : listener) (position : number * number * number) : number =
  Space.direction ~listener:ears.position ~right:ears.right (vec position)

let heard (ears : listener) ?(velocity = (0., 0., 0.)) ?(reference = 10.) ?(speed_of_sound = 343.)
    (position : number * number * number) (s : Audio.sound) : Audio.sound =
  let source = vec position in
  let d = Space.distance ears.position source in
  let doppler =
    Space.doppler ~speed_of_sound ~listener:ears.position ~listener_velocity:ears.velocity ~source
      ~source_velocity:(vec velocity)
  in
  let cutoff = Space.air_cutoff d in
  s
  |> (if cutoff < 20000. then Audio.low_pass cutoff else Fun.id)
  |> Audio.pitched doppler |> Audio.faster doppler
  |> Audio.louder (Space.attenuation ~reference d)
  |> Audio.pan (pan ears position)
