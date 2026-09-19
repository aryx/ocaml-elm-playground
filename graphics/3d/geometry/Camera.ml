(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Camera.mli *)

type t = { eye : Vec3.t; target : Vec3.t; up : Vec3.t; fov : float; near : float; far : float }

(* the world's "up" *)
let up_hint : Vec3.t = (0., 1., 0.)

let basis ?(up = up_hint) ~(eye : Vec3.t) ~(target : Vec3.t) () : Vec3.t * Vec3.t * Vec3.t =
  let forward = Vec3.normalize (Vec3.sub target eye) in
  let right = Vec3.normalize (Vec3.cross forward up) in
  let up = Vec3.cross right forward in
  (right, up, forward)

let view (camera : t) (point : Vec3.t) : Vec3.t =
  let right, up, forward = basis ~up:camera.up ~eye:camera.eye ~target:camera.target () in
  let relative = Vec3.sub point camera.eye in
  (Vec3.dot relative right, Vec3.dot relative up, Vec3.dot relative forward)

let focal (camera : t) : float = 1. /. tan (camera.fov *. Float.pi /. 180. /. 2.)

let ndc (camera : t) ~(aspect : float) ((x, y, z) : Vec3.t) : (float * float) option =
  if z < camera.near || z >= camera.far then None
  else
    let f = focal camera in
    Some (f *. x /. aspect /. z, f *. y /. z)
