(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Physics3d.mli *)

open Playground
open Playground3d

type body = {
  shape : shape3d;
  x : number;
  y : number;
  z : number;
  vx : number;
  vy : number;
  vz : number;
  orientation : Quat.t;
  spin : number * number * number;
  mass : number;
  bounciness : number;
  friction : number;
  inertia : Mat3.t;
  ax : number;
  ay : number;
  az : number;
  torque : number * number * number;
}

let tick = 1. /. 60.
let radians d = d *. Float.pi /. 180.
let degrees r = r *. 180. /. Float.pi
let to_radians (x, y, z) = (radians x, radians y, radians z)
let to_degrees (x, y, z) = (degrees x, degrees y, degrees z)

(*****************************************************************************)
(* Measuring a shape *)
(*****************************************************************************)

(* claude: every point of the shape, in world space (Playground3d's
 * shapes carry no transform: move3d and friends move the points
 * themselves), so a bounding box is a fold over the tree *)
let bounds (s : shape3d) : (number * number * number) * (number * number * number) =
  let lo = ref None and hi = ref None in
  let point (x, y, z) =
    (match !lo with
    | None -> lo := Some (x, y, z)
    | Some (a, b, c) -> lo := Some (Float.min a x, Float.min b y, Float.min c z));
    match !hi with
    | None -> hi := Some (x, y, z)
    | Some (a, b, c) -> hi := Some (Float.max a x, Float.max b y, Float.max c z)
  in
  let rec walk (s : shape3d) =
    match s.form with
    | Polygon3d (_, points) -> List.iter point points
    | TexturedPolygon3d (_, points) -> List.iter (fun (p, _) -> point p) points
    | SmoothPolygon3d (_, points) -> List.iter (fun (p, _) -> point p) points
    | Hud _ -> ()
    | Group3d shapes -> List.iter walk shapes
    | Cached3d c -> walk c.content
  in
  walk s;
  match (!lo, !hi) with Some a, Some b -> (a, b) | _ -> ((0., 0., 0.), (0., 0., 0.))

let sides_of (s : shape3d) : number * number * number =
  let (ax, ay, az), (bx, by, bz) = bounds s in
  (bx -. ax, by -. ay, bz -. az)

(*****************************************************************************)
(* Making bodies *)
(*****************************************************************************)

let body (shape : shape3d) : body =
  { shape; x = 0.; y = 0.; z = 0.; vx = 0.; vy = 0.; vz = 0.; orientation = Quat.identity; spin = (0., 0., 0.);
    mass = 1.; bounciness = 0.; friction = 0.; inertia = Body3d.box ~mass:1. (sides_of shape);
    ax = 0.; ay = 0.; az = 0.; torque = (0., 0., 0.) }

let at x y z (b : body) : body = { b with x; y; z }
let moving vx vy vz (b : body) : body = { b with vx; vy; vz }
let pointing axis deg (b : body) : body = { b with orientation = Quat.of_axis_angle axis (radians deg) }
let turning (ax, ay, az) speed (b : body) : body =
  let n = Float.hypot (Float.hypot ax ay) az in
  if n < 1e-12 then b else { b with spin = (ax /. n *. speed, ay /. n *. speed, az /. n *. speed) }

(* claude: the tensor is proportional to the mass, so [heavy] scales it
 * too -- a heavier body is harder to spin as well as to push. An
 * infinite tensor (an [upright] body) scales to an infinite one, which
 * is what it should stay. *)
let heavy mass (b : body) : body =
  let scalable = Float.is_finite mass && Float.is_finite b.mass && b.mass > 0. in
  { b with mass; inertia = (if scalable then Mat3.scale (mass /. b.mass) b.inertia else b.inertia) }

let bouncy bounciness (b : body) : body = { b with bounciness }
let rough friction (b : body) : body = { b with friction }
let immovable (b : body) : body = { b with mass = infinity; inertia = Body3d.never_turns }
(* claude: the spin it was given is kept: nothing can *change* it, which
 * is exactly what a kinematic flipper wants (see Body3d.mli) *)
let upright (b : body) : body = { b with inertia = Body3d.never_turns }
let solid_as sides (b : body) : body = { b with inertia = Body3d.box ~mass:b.mass sides }

(*****************************************************************************)
(* What pushes it *)
(*****************************************************************************)

let accelerate (ax, ay, az) (b : body) : body = { b with ax = b.ax +. ax; ay = b.ay +. ay; az = b.az +. az }
let apply (f : Force3d.t) (b : body) : body = accelerate (f (b.x, b.y, b.z) (b.vx, b.vy, b.vz)) b
let fall g (b : body) : body = accelerate (0., -.g, 0.) b
let push fx fy fz (b : body) : body = accelerate (fx /. b.mass, fy /. b.mass, fz /. b.mass) b

(* the body's own -z, turned: the direction games3d/ characters face *)
let forward (b : body) : number * number * number = Quat.rotate b.orientation (0., 0., -1.)

let thrust f (b : body) : body =
  let fx, fy, fz = forward b in
  push (f *. fx) (f *. fy) (f *. fz) b

let slow c (b : body) : body = apply (Force3d.drag ~c) b
let attracted_by (other : body) (b : body) : body =
  apply (Force3d.gravitation ~gm:other.mass ~center:(other.x, other.y, other.z)) b

let pulled_to x y z k (b : body) : body = apply (Force3d.spring ~k_over_m:k ~anchor:(x, y, z)) b

let floating ?damping ~water ~density (b : body) : body =
  let _, h, _ = sides_of b.shape in
  apply (Force3d.buoyancy ?damping ~g:9.8 ~water ~half_height:(h /. 2.) ~density ()) b

let spin_by tx ty tz (b : body) : body =
  let ox, oy, oz = b.torque in
  { b with torque = (ox +. tx, oy +. ty, oz +. tz) }

(*****************************************************************************)
(* Moving *)
(*****************************************************************************)

let step (b : body) : body =
  let engine =
    Body3d.make ~vel:(b.vx, b.vy, b.vz) ~orientation:b.orientation ~spin:(to_radians b.spin) ~mass:b.mass
      ~inertia:b.inertia (b.x, b.y, b.z)
  in
  let engine =
    Integrate3d.step Integrate3d.Semi_implicit_euler ~torque:b.torque
      ~force:(Force3d.uniform (b.ax, b.ay, b.az))
      ~dt:tick engine
  in
  let x, y, z = engine.Body3d.pos and vx, vy, vz = engine.Body3d.vel in
  { b with x; y; z; vx; vy; vz; orientation = engine.Body3d.orientation; spin = to_degrees engine.Body3d.spin;
    ax = 0.; ay = 0.; az = 0.; torque = (0., 0., 0.) }

(*****************************************************************************)
(* Looking at bodies *)
(*****************************************************************************)

let draw (b : body) : shape3d =
  let dx, dy, dz = Quat.to_euler_xyz b.orientation in
  b.shape |> rotate3d dx dy dz |> move3d b.x b.y b.z

let position (b : body) : number * number * number = (b.x, b.y, b.z)
let distance (a : body) (b : body) : number = Vec3.length (Vec3.sub (position a) (position b))
let speed (b : body) : number = Vec3.length (b.vx, b.vy, b.vz)

(* claude: the bounding box as its twelve edges (thin boxes, which
 * every backend draws and which stay visible edge-on, unlike a
 * polygon), plus the velocity as a line from the centre. No alpha: the
 * software backend has none, so a translucent hitbox like the 2D
 * debug's is not an option here. *)
let debug (b : body) : shape3d =
  let (ax, ay, az), (bx, by, bz) = bounds b.shape in
  let t = 0.01 *. Float.max 1. (Vec3.length (bx -. ax, by -. ay, bz -. az)) in
  (* claude: just outside the shape, or the edges sit exactly on its
   * faces and are hidden by them *)
  let ax = ax -. t and ay = ay -. t and az = az -. t in
  let bx = bx +. t and by = by +. t and bz = bz +. t in
  let edge (x1, y1, z1) (x2, y2, z2) =
    box green (Float.abs (x2 -. x1) +. t) (Float.abs (y2 -. y1) +. t) (Float.abs (z2 -. z1) +. t)
    |> move3d ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.) ((z1 +. z2) /. 2.)
  in
  let corners = [ (ax, ay, az); (bx, ay, az); (bx, ay, bz); (ax, ay, bz) ] in
  let top (x, _, z) = (x, by, z) in
  let box_edges =
    List.concat
      (List.mapi
         (fun i c ->
           let next = List.nth corners ((i + 1) mod 4) in
           [ edge c next; edge (top c) (top next); edge c (top c) ])
         corners)
  in
  let dx, dy, dz = Quat.to_euler_xyz b.orientation in
  let velocity =
    if speed b < 1e-6 then []
    else
      [ (let vx, vy, vz = (b.vx, b.vy, b.vz) in
         box red (Float.abs vx +. t) (Float.abs vy +. t) (Float.abs vz +. t)
         |> move3d (b.x +. (vx /. 2.)) (b.y +. (vy /. 2.)) (b.z +. (vz /. 2.))) ]
  in
  group3d ((group3d box_edges |> rotate3d dx dy dz |> move3d b.x b.y b.z) :: velocity)
