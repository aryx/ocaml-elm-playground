(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Energy3d.mli *)

(* claude: an immovable body (an infinite mass) or one that never turns
 * (an infinite tensor) is always still about that axis, and infinity
 * times a zero speed is a nan rather than the 0 it should be: hence the
 * guards *)
let still (v : Vec3.t) = v = (0., 0., 0.)

let linear_kinetic (b : Body3d.t) = if still b.vel then 0. else 0.5 *. b.mass *. Vec3.dot b.vel b.vel

let rotational_kinetic (b : Body3d.t) =
  if still b.spin then 0. else 0.5 *. Vec3.dot b.spin (Mat3.mul_vec (Body3d.inertia_world b) b.spin)

let kinetic b = linear_kinetic b +. rotational_kinetic b
let momentum (b : Body3d.t) = if still b.vel then (0., 0., 0.) else Vec3.scale b.mass b.vel
let angular (b : Body3d.t) = if still b.spin then (0., 0., 0.) else Mat3.mul_vec (Body3d.inertia_world b) b.spin

let angular_momentum ~around (b : Body3d.t) =
  Vec3.add (Vec3.cross (Vec3.sub b.pos around) (momentum b)) (angular b)

let gravity ~g (b : Body3d.t) =
  let _, y, _ = b.pos in
  b.mass *. g *. y
