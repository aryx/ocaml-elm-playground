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
  hitbox : Hitbox3d.t;
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

let half_sides (s : shape3d) : Vec3.t =
  let w, h, d = sides_of s in
  (w /. 2., h /. 2., d /. 2.)

let body (shape : shape3d) : body =
  let hitbox = Hitbox3d.Box (half_sides shape) in
  { shape; x = 0.; y = 0.; z = 0.; vx = 0.; vy = 0.; vz = 0.; orientation = Quat.identity; spin = (0., 0., 0.);
    mass = 1.; bounciness = 0.; friction = 0.; hitbox; inertia = Hitbox3d.inertia ~mass:1. hitbox;
    ax = 0.; ay = 0.; az = 0.; torque = (0., 0., 0.) }

(* claude: a body that was made [upright] stays upright whatever hitbox
 * it is given afterwards, so that the order of the two does not matter *)
let hitbox (h : Hitbox3d.t) (b : body) : body =
  let inertia = if b.inertia = Body3d.never_turns then b.inertia else Hitbox3d.inertia ~mass:b.mass h in
  { b with hitbox = h; inertia }

let ball (b : body) : body =
  let hx, hy, hz = half_sides b.shape in
  hitbox (Hitbox3d.Sphere (Float.min hx (Float.min hy hz))) b

let pill (b : body) : body =
  let hx, hy, hz = half_sides b.shape in
  let r = Float.min hx hz in
  hitbox (Hitbox3d.Capsule (Float.max 0. (hy -. r), r)) b

let hitbox_of (b : body) : Hitbox3d.placed =
  Hitbox3d.place ~orientation:b.orientation (b.x, b.y, b.z) b.hitbox

(* the engine's body, and back: the API counts angles in degrees and
 * physics/3d in radians, and this is the only place that matters *)
let state (b : body) : Body3d.t =
  Body3d.make ~vel:(b.vx, b.vy, b.vz) ~orientation:b.orientation ~spin:(to_radians b.spin) ~mass:b.mass
    ~inertia:b.inertia (b.x, b.y, b.z)

let with_state (s : Body3d.t) (b : body) : body =
  let x, y, z = s.Body3d.pos and vx, vy, vz = s.Body3d.vel in
  { b with x; y; z; vx; vy; vz; orientation = s.Body3d.orientation; spin = to_degrees s.Body3d.spin }

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

let touching (a : body) (b : body) : bool = Collide3d.touching (hitbox_of a) (hitbox_of b)
let contact (a : body) (b : body) : Contact3d.t option = Collide3d.contact (hitbox_of a) (hitbox_of b)

(* the pair's: the bouncier one's bounciness and the geometric mean of
 * the frictions, which are Box2D's choices and the 2D API's *)
let bounce (a : body) (b : body) : body * body =
  match contact a b with
  | None -> (a, b)
  | Some c ->
      let restitution = Float.max a.bounciness b.bounciness and friction = sqrt (a.friction *. b.friction) in
      let sa, sb = Resolve3d.resolve ~restitution ~friction (state a, state b) c in
      (with_state sa a, with_state sb b)

let bounce_off (wall : body) (b : body) : body = fst (bounce b (immovable wall))

(* the body's hitbox's box in the world: what a broad phase sorts *)
let world_bounds (b : body) : Broadphase3d.box = Hitbox3d.bounds (hitbox_of b)

let broad_phase (m : Broadphase3d.method_) (bodies : body list) : Broadphase3d.result =
  Broadphase3d.pairs m (Array.of_list (List.map world_bounds bodies))

let bounce_all ?(broad_phase = Broadphase3d.Sweep_and_prune) (bodies : body list) : body list =
  let all = Array.of_list bodies in
  let boxes = Array.map world_bounds all in
  let found = Broadphase3d.pairs broad_phase boxes in
  List.iter
    (fun (i, j) ->
      let a, b = bounce all.(i) all.(j) in
      all.(i) <- a;
      all.(j) <- b)
    found.Broadphase3d.pairs;
  Array.to_list all

let ray ~from ~direction (bodies : body list) : (body * number) option =
  List.fold_left
    (fun best b ->
      match Collide3d.ray ~from ~direction (hitbox_of b) with
      | Some t -> ( match best with Some (_, u) when u <= t -> best | _ -> Some (b, t))
      | None -> best)
    None bodies

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
  let engine = state b in
  let engine =
    Integrate3d.step Integrate3d.Semi_implicit_euler ~torque:b.torque
      ~force:(Force3d.uniform (b.ax, b.ay, b.az))
      ~dt:tick engine
  in
  { (with_state engine b) with ax = 0.; ay = 0.; az = 0.; torque = (0., 0., 0.) }

(*****************************************************************************)
(* Looking at bodies *)
(*****************************************************************************)

let draw (b : body) : shape3d =
  let dx, dy, dz = Quat.to_euler_xyz b.orientation in
  b.shape |> rotate3d dx dy dz |> move3d b.x b.y b.z

let position (b : body) : number * number * number = (b.x, b.y, b.z)
let distance (a : body) (b : body) : number = Vec3.length (Vec3.sub (position a) (position b))
let speed (b : body) : number = Vec3.length (b.vx, b.vy, b.vz)

(* claude: a hitbox drawn as its wireframe -- thin boxes between
 * points, since a flat polygon vanishes edge-on and the software
 * backend has no alpha for a translucent solid. Everything is built in
 * world space from the placed hitbox, so a turned box needs no special
 * case. *)
let rod (c : color) (a : Vec3.t) (b : Vec3.t) (t : number) : shape3d =
  let d = Vec3.sub b a in
  let len = Vec3.length d in
  if len < 1e-9 then group3d []
  else
    let q =
      let axis = Vec3.cross (0., 1., 0.) d in
      if Vec3.length axis < 1e-9 then if Vec3.dot (0., 1., 0.) d > 0. then Quat.identity else Quat.of_axis_angle (1., 0., 0.) Float.pi
      else Quat.of_axis_angle axis (acos (Float.max (-1.) (Float.min 1. (Vec3.dot (Vec3.normalize d) (0., 1., 0.)))))
    in
    let dx, dy, dz = Quat.to_euler_xyz q in
    let mx, my, mz = Vec3.scale 0.5 (Vec3.add a b) in
    box c t len t |> rotate3d dx dy dz |> move3d mx my mz

(* a circle of [n] rods in the plane of two unit vectors *)
let ring (c : color) (centre : Vec3.t) (u : Vec3.t) (v : Vec3.t) (r : number) (t : number) : shape3d list =
  let n = 16 in
  let at i =
    let a = 2. *. Float.pi *. float_of_int i /. float_of_int n in
    Vec3.add centre (Vec3.add (Vec3.scale (r *. cos a) u) (Vec3.scale (r *. sin a) v))
  in
  List.init n (fun i -> rod c (at i) (at ((i + 1) mod n)) t)

let debug (b : body) : shape3d =
  let p = hitbox_of b in
  let scale_of = Float.max 0.05 (Vec3.length (sides_of b.shape)) in
  let t = 0.015 *. scale_of in
  let c = green in
  let turned v = Quat.rotate b.orientation v in
  let outline =
    match b.hitbox with
    | Hitbox3d.Box _ ->
        let corners = Hitbox3d.corners p in
        (* the 8 corners come out in a known order (x slowest, then y,
         * then z): the 12 edges are the pairs differing in one bit *)
        let nth i = List.nth corners i in
        List.filter_map
          (fun (i, j) ->
            let bits = i lxor j in
            if bits = 1 || bits = 2 || bits = 4 then Some (rod c (nth i) (nth j) t) else None)
          (List.concat_map (fun i -> List.init 8 (fun j -> (i, j))) (List.init 8 (fun i -> i)))
    | Hitbox3d.Sphere r ->
        ring c (b.x, b.y, b.z) (turned (1., 0., 0.)) (turned (0., 1., 0.)) r t
        @ ring c (b.x, b.y, b.z) (turned (0., 1., 0.)) (turned (0., 0., 1.)) r t
        @ ring c (b.x, b.y, b.z) (turned (0., 0., 1.)) (turned (1., 0., 0.)) r t
    | Hitbox3d.Capsule (_, r) ->
        let lo, hi = Hitbox3d.segment p in
        let u = turned (1., 0., 0.) and w = turned (0., 0., 1.) in
        ring c lo u w r t @ ring c hi u w r t
        @ List.map
            (fun d -> rod c (Vec3.add lo (Vec3.scale r d)) (Vec3.add hi (Vec3.scale r d)) t)
            [ u; Vec3.scale (-1.) u; w; Vec3.scale (-1.) w ]
        @ [ rod c lo hi t ]
    | Hitbox3d.Plane (n, d) ->
        let n = Vec3.normalize n in
        let u = Vec3.normalize (if Float.abs (let _, y, _ = n in y) > 0.9 then Vec3.cross n (1., 0., 0.) else Vec3.cross n (0., 1., 0.)) in
        let v = Vec3.cross n u in
        let at i j = Vec3.add (Vec3.scale d n) (Vec3.add (Vec3.scale (2. *. i) u) (Vec3.scale (2. *. j) v)) in
        (* a grid on it, since a plane has no edges of its own *)
        List.concat_map (fun i -> [ rod c (at i (-2.)) (at i 2.) t; rod c (at (-2.) i) (at 2. i) t ]) [ -2.; -1.; 0.; 1.; 2. ]
  in
  let velocity =
    if speed b < 1e-6 then [] else [ rod red (b.x, b.y, b.z) (b.x +. b.vx, b.y +. b.vy, b.z +. b.vz) t ]
  in
  group3d (outline @ velocity)
