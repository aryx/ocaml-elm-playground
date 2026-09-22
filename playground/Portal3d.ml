(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground3d

type vec = number * number * number

type t = { centre : vec; normal : vec; up : vec; width : number; height : number }

let right (p : t) : vec = Vec3.cross p.up p.normal

(* a point's coordinates in the portal's frame, and back *)
let local (p : t) (v : vec) : vec = (Vec3.dot v (right p), Vec3.dot v p.up, Vec3.dot v p.normal)

let global (p : t) ((x, y, z) : vec) : vec =
  Vec3.add (Vec3.scale x (right p)) (Vec3.add (Vec3.scale y p.up) (Vec3.scale z p.normal))

(* half a turn about up: in at one, out of the other *)
let half_turn ((x, y, z) : vec) : vec = (-.x, y, -.z)

let direction ~(from : t) ~(into : t) (d : vec) : vec = global into (half_turn (local from d))
let point ~(from : t) ~(into : t) (p : vec) : vec = Vec3.add into.centre (direction ~from ~into (Vec3.sub p from.centre))

(* Shepperd's method: the quaternion of a rotation matrix, read off its
 * largest diagonal term for accuracy. The matrix's columns are the
 * three axes. *)
let of_frame ((ax, ay, az) : vec) ((bx, by, bz) : vec) ((cx, cy, cz) : vec) : Quat.t =
  let m00 = ax and m10 = ay and m20 = az and m01 = bx and m11 = by and m21 = bz and m02 = cx and m12 = cy and m22 = cz in
  let trace = m00 +. m11 +. m22 in
  let w, x, y, z =
    if trace > 0. then
      let s = Float.sqrt (trace +. 1.) *. 2. in
      (s /. 4., (m21 -. m12) /. s, (m02 -. m20) /. s, (m10 -. m01) /. s)
    else if m00 > m11 && m00 > m22 then
      let s = Float.sqrt (1. +. m00 -. m11 -. m22) *. 2. in
      ((m21 -. m12) /. s, s /. 4., (m01 +. m10) /. s, (m02 +. m20) /. s)
    else if m11 > m22 then
      let s = Float.sqrt (1. +. m11 -. m00 -. m22) *. 2. in
      ((m02 -. m20) /. s, (m01 +. m10) /. s, s /. 4., (m12 +. m21) /. s)
    else
      let s = Float.sqrt (1. +. m22 -. m00 -. m11) *. 2. in
      ((m10 -. m01) /. s, (m02 +. m20) /. s, (m12 +. m21) /. s, s /. 4.)
  in
  Quat.normalize { Quat.w; v = (x, y, z) }

let orientation ~(from : t) ~(into : t) (q : Quat.t) : Quat.t =
  let turn = of_frame (direction ~from ~into (1., 0., 0.)) (direction ~from ~into (0., 1., 0.)) (direction ~from ~into (0., 0., 1.)) in
  Quat.normalize (Quat.mul turn q)

let carry ~(from : t) ~(into : t) (b : Physics3d.body) : Physics3d.body =
  let x, y, z = point ~from ~into (b.x, b.y, b.z) and vx, vy, vz = direction ~from ~into (b.vx, b.vy, b.vz) in
  { b with x; y; z; vx; vy; vz; orientation = orientation ~from ~into b.orientation; spin = direction ~from ~into b.spin }

let crossed (p : t) ~(before : vec) ~(after : vec) : bool =
  let side v = Vec3.dot (Vec3.sub v p.centre) p.normal in
  let a = side before and b = side after in
  a > 0. && b <= 0.
  &&
  (* where the path meets the plane, inside the rectangle *)
  let t = a /. (a -. b) in
  let x, y, _ = local p (Vec3.sub (Vec3.add before (Vec3.scale t (Vec3.sub after before))) p.centre) in
  Float.abs x <= p.width /. 2. && Float.abs y <= p.height /. 2.

(* Sutherland-Hodgman, one plane: the part of the polygon on the side
 * where [inside] is >= 0, the edges that cross it cut where it is 0 *)
let clip_plane (inside : vec -> float) (poly : vec list) : vec list =
  match poly with
  | [] -> []
  | _ ->
      let cut a b = let da = inside a and db = inside b in Vec3.add a (Vec3.scale (da /. (da -. db)) (Vec3.sub b a)) in
      let n = List.length poly in
      let arr = Array.of_list poly in
      List.concat
        (List.init n (fun i ->
             let a = arr.(i) and b = arr.((i + 1) mod n) in
             match (inside a >= 0., inside b >= 0.) with
             | true, true -> [ b ]
             | true, false -> [ cut a b ]
             | false, true -> [ cut a b; b ]
             | false, false -> []))

let clip ~(eye : vec) (p : t) (poly : vec list) : vec list =
  let r = Vec3.scale (p.width /. 2.) (right p) and u = Vec3.scale (p.height /. 2.) p.up in
  let corner sx sy = Vec3.add p.centre (Vec3.add (Vec3.scale sx r) (Vec3.scale sy u)) in
  let corners = [ corner (-1.) (-1.); corner 1. (-1.); corner 1. 1.; corner (-1.) 1. ] in
  (* the plane through the eye and one edge, facing the portal's middle *)
  let side a b =
    let n = Vec3.cross (Vec3.sub a eye) (Vec3.sub b eye) in
    let n = if Vec3.dot n (Vec3.sub p.centre eye) < 0. then Vec3.scale (-1.) n else n in
    fun v -> Vec3.dot n (Vec3.sub v eye)
  in
  let arr = Array.of_list corners in
  let sides = List.init 4 (fun i -> side arr.(i) arr.((i + 1) mod 4)) in
  let behind v = -.Vec3.dot (Vec3.sub v p.centre) p.normal in
  List.fold_left (fun poly inside -> clip_plane inside poly) poly (behind :: sides)
