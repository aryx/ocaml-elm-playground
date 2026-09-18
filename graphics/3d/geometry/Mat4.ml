(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(*****************************************************************************)
(* Mat4: the one genuinely new piece of math a GPU backend needs that
 * the software rasterizer doesn't -- see plan_opengl.md's comparison
 * table. The rasterizer projects one point at a time with a few plain
 * scalar formulas (view_space + project_vertex); a GPU vertex shader
 * instead expects a single 4x4 "model-view-projection" matrix per
 * draw call, uploaded once, that it then applies to every vertex
 * itself, in parallel. A row-major float array of 16 elements --
 * uniform_matrix4fv's [transpose] argument (set to true in the OpenGL
 * backend) tells OpenGL to transpose it into the column-major layout
 * it actually wants internally, so this code never has to think in
 * column-major.
 * claude: WebGL 1 requires [transpose] = false, so the WebGL backend
 * will have to transpose on the CPU itself (see plan_webgl.md). *)
(*****************************************************************************)

type t = float array

let up_hint : Vec3.t = (0., 1., 0.)

(* [look_at eye target] builds a view matrix using the exact same
 * right/up/forward basis as the native rasterizer's view_space (same
 * up_hint, same "which way is the camera pointing" derivation) --
 * V * point = (dot (point - eye) right, dot (point - eye) up,
 * dot (point - eye) forward), i.e. the same view-space coordinates
 * view_space computes, just packaged as a matrix a GPU can apply. *)
let look_at ~(eye : Vec3.t) ~(target : Vec3.t) : t =
  let forward = Vec3.normalize (Vec3.sub target eye) in
  let right = Vec3.normalize (Vec3.cross forward up_hint) in
  let up = Vec3.cross right forward in
  let (rx, ry, rz) = right and (ux, uy, uz) = up and (fx, fy, fz) = forward in
  [|
    rx; ry; rz; -.(Vec3.dot right eye);
    ux; uy; uz; -.(Vec3.dot up eye);
    fx; fy; fz; -.(Vec3.dot forward eye);
    0.; 0.; 0.; 1.;
  |]

(* [perspective ~fov_degrees ~aspect ~near ~far]: the exact same
 * f = 1/tan(fov/2), x scaled by f/aspect, y scaled by f formulas as
 * project_vertex's ndc_x/ndc_y (see that function's comment) -- same
 * fov/near/far camera field, same on-screen framing, on both
 * backends. The z row (derived from "NDC z must be -1 at [near] and
 * +1 at [far], for a view-space z that's positive in front of the
 * camera, matching look_at's convention above") is new: the software
 * rasterizer never needs to remap depth into any particular range, it
 * only ever directly compares raw view-space z values against each
 * other in its own hand-rolled zbuffer; a GPU's hardware depth test
 * expects normalized device coordinates instead. *)
let perspective ~(fov_degrees : float) ~(aspect : float) ~(near : float) ~(far : float) : t =
  let fov_rad = fov_degrees *. Float.pi /. 180. in
  let f = 1. /. tan (fov_rad /. 2.) in
  let a = (far +. near) /. (far -. near) in
  let b = -2. *. far *. near /. (far -. near) in
  [| f /. aspect; 0.; 0.; 0.; 0.; f; 0.; 0.; 0.; 0.; a; b; 0.; 0.; 1.; 0. |]

(* row-major 4x4 * 4x4 -- [mul a b] then applied to a point means
 * "apply b first, then a" (standard matrix composition), so
 * [mul projection view] is the usual "view, then project" order. *)
let mul (a : t) (b : t) : t =
  Array.init 16 (fun idx ->
      let r = idx / 4 and c = idx mod 4 in
      let sum = ref 0. in
      for k = 0 to 3 do
        sum := !sum +. (a.((r * 4) + k) *. b.((k * 4) + c))
      done;
      !sum)
