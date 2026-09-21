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

(* The pair's: the bouncier one's bounciness and the geometric mean of
 * the frictions, which are Box2D's choices and the 2D API's.
 *
 * Every point of the manifold gets its impulse, not just one: a crate
 * landing flat on the floor touches at four corners, and answering
 * only one of them tips it. They are pushed apart once, by the deepest
 * of the points. *)
let bounce (a : body) (b : body) : body * body =
  match Collide3d.manifold (hitbox_of a) (hitbox_of b) with
  | [] -> (a, b)
  | contacts ->
      let restitution = Float.max a.bounciness b.bounciness and friction = sqrt (a.friction *. b.friction) in
      let pair = List.fold_left (fun pair c -> Resolve3d.bounce ~restitution ~friction pair c) (state a, state b) contacts in
      let deepest =
        List.fold_left
          (fun (best : Contact3d.t) (c : Contact3d.t) -> if c.Contact3d.depth > best.Contact3d.depth then c else best)
          (List.hd contacts) contacts
      in
      let sa, sb = Resolve3d.separate pair deepest in
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

(* rolling friction, the loss that stops a rolling ball (deformation,
 * not grip): a torque against the angular momentum, so the spin dies
 * away at the same rate whatever the tensor *)
let spin_slow c (b : body) : body =
  let l = Mat3.mul_vec (Body3d.inertia_world (state b)) (to_radians b.spin) in
  let tx, ty, tz = Vec3.scale (-.c) l in
  { b with torque = (let ox, oy, oz = b.torque in (ox +. tx, oy +. ty, oz +. tz)) }

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

(*****************************************************************************)
(* A world: every contact of a step, solved together *)
(*****************************************************************************)

type world = {
  bodies : body list;
  memory : Solver3d.memory;
  still : int list;
  asleep : bool list;
  solved : int;
  swept : int;
}

let world (bodies : body list) : world =
  { bodies; memory = Solver3d.nothing; still = List.map (fun _ -> 0) bodies;
    asleep = List.map (fun _ -> false) bodies; solved = 0; swept = 0 }

(* a body is ready to sleep once it has been slow for this many steps,
 * and a *group* of them sleeps together -- see below *)
let sleep_after = 60
let slow_speed = 0.05
let slow_spin = 10.

let movable (b : body) : bool = Float.is_finite b.mass

(* the radius a body is swept with: its own for a sphere or a capsule,
 * its narrowest half for a box (see [went_through]) *)
let sweep_radius (b : body) : number =
  match b.hitbox with
  | Hitbox3d.Sphere r | Hitbox3d.Capsule (_, r) -> r
  | Hitbox3d.Box (hx, hy, hz) -> Float.min hx (Float.min hy hz)
  | Hitbox3d.Plane _ -> 0.

(* One step of [dt] (a tick, or a piece of one): the pushes, the
 * contacts solved, the sweep, the moves. [last]: the pushes are used up
 * (the game's pushes last the whole tick, however many pieces it is
 * cut into). *)
let advance ~gravity ~iterations ~warm_starting ~sleeping ~broad_phase ~continuous ~(dt : number) ~(last : bool)
    (w : world) : world =
  let all = Array.of_list w.bodies in
  let still = Array.of_list w.still in
  let asleep = Array.of_list w.asleep in
  if not sleeping then Array.iteri (fun i _ -> asleep.(i) <- false) asleep;
  (* the pushes change the velocities (semi-implicit Euler: velocities
   * first, positions last, with the new ones) *)
  let awake_and_moving =
    Array.mapi
      (fun i b ->
        if (not (movable b)) || asleep.(i) then b
        else
          let b = fall gravity b in
          { b with vx = b.vx +. (b.ax *. dt); vy = b.vy +. (b.ay *. dt); vz = b.vz +. (b.az *. dt) })
      all
  in
  (* the contacts: the broad phase's pairs, each with the points of its
   * manifold *)
  let boxes = Array.map world_bounds awake_and_moving in
  let found = Broadphase3d.pairs broad_phase boxes in
  let touching = ref [] in
  let pairs =
    List.filter_map
      (fun (i, j) ->
        let a = awake_and_moving.(i) and b = awake_and_moving.(j) in
        let both_still = ((not (movable a)) || asleep.(i)) && ((not (movable b)) || asleep.(j)) in
        if both_still then None
        else
          match Collide3d.manifold (hitbox_of a) (hitbox_of b) with
          | [] -> None
          | contacts ->
              touching := (i, j) :: !touching;
              (* a sleeper wakes when something *moving* touches it --
               * not merely when it is touched, or a crate resting on
               * the one below would keep it awake for ever *)
              let disturbing k (x : body) = movable x && (not asleep.(k)) && speed x > slow_speed in
              if asleep.(i) && disturbing j b then asleep.(i) <- false;
              if asleep.(j) && disturbing i a then asleep.(j) <- false;
              Some
                { Solver3d.a = i; b = j; contacts;
                  restitution = Float.max a.bounciness b.bounciness;
                  friction = sqrt (a.friction *. b.friction) })
      found.Broadphase3d.pairs
  in
  let states, memory =
    Solver3d.solve { Solver3d.default with iterations; warm_starting } ~dt
      (Array.map state awake_and_moving) pairs w.memory
  in
  let solved_bodies = Array.mapi (fun i b -> if asleep.(i) then b else with_state states.(i) b) awake_and_moving in
  (* The sweep (continuous collision, Sweep3d). A sphere that is fast
   * this step (farther than a quarter of its radius), or near something
   * that moves, is swept along its path against everything else --
   * where it is at the start of the step, moving and turning as it will
   * -- and at the first touch:
   *
   *  - against something that nothing can push (a wall, a flipper), the
   *    touch is answered there and then: the ball's speed along the
   *    normal, relative to that surface's own speed at the point, is
   *    turned round (keeping the pair's bounciness), and the ball spends
   *    the rest of the step going the new way. Nothing is lost, and a
   *    flipper swinging into a ball at rest throws it -- where stopping
   *    the ball at the touch would have let the flipper sweep through
   *    it, since only the ball is held back;
   *  - against another moving body, it stops there, and the next step's
   *    contact is the solver's.
   *
   * The ball is treated as a point there (its spin is not touched): a
   * pinball's spin barely matters to where it goes, and the solver
   * takes over at the next step. Every pair, every step, for those
   * spheres only: fine for a table, not for a thousand marbles. *)
  let n = Array.length solved_bodies in
  let final = Array.copy solved_bodies and swept = ref 0 in
  let moves (o : body) = o.vx <> 0. || o.vy <> 0. || o.vz <> 0. || o.spin <> (0., 0., 0.) in
  if continuous then
    Array.iteri
      (fun i (b : body) ->
        match b.hitbox with
        | Hitbox3d.Sphere radius when movable b && not asleep.(i) ->
            let motion = (b.vx *. dt, b.vy *. dt, b.vz *. dt) in
            let fast = Vec3.length motion > radius /. 4. in
            let first = ref None in
            for j = 0 to n - 1 do
              let o = solved_bodies.(j) in
              if j <> i && (fast || (moves o && not asleep.(j))) then
                let shift = (o.vx *. dt, o.vy *. dt, o.vz *. dt) and turn = Vec3.scale dt (to_radians o.spin) in
                let moving = if asleep.(j) then None else Some (shift, turn) in
                match Sweep3d.sphere ~radius ~from:(b.x, b.y, b.z) ~motion ?moving (hitbox_of o) with
                | Some t when (match !first with None -> true | Some (t', _) -> t < t') -> first := Some (t, j)
                | _ -> ()
            done;
            (match !first with
            | None -> ()
            | Some (t, j) ->
                incr swept;
                let o = solved_bodies.(j) in
                let at = Vec3.add (b.x, b.y, b.z) (Vec3.scale t motion) in
                if movable o then
                  let x, y, z = at in
                  final.(i) <- { b with x; y; z }
                else begin
                  (* the obstacle where it is at the touch *)
                  let shift = (o.vx *. dt *. t, o.vy *. dt *. t, o.vz *. dt *. t) in
                  let placed = hitbox_of o in
                  let placed =
                    { placed with pos = Vec3.add placed.pos shift;
                      orientation = Quat.turned_by ~spin:(to_radians o.spin) ~dt:(dt *. t) placed.orientation }
                  in
                  let v = (b.vx, b.vy, b.vz) in
                  let v =
                    match Collide3d.contact placed (Hitbox3d.place at (Hitbox3d.Sphere radius)) with
                    | None -> v
                    | Some k ->
                        (* the surface's own speed there: its velocity,
                         * and its spin about its middle *)
                        let r = Vec3.sub k.Contact3d.point placed.pos in
                        let surface = Vec3.add (o.vx, o.vy, o.vz) (Vec3.cross (to_radians o.spin) r) in
                        let closing = Vec3.dot (Vec3.sub v surface) k.Contact3d.normal in
                        if closing >= 0. then v
                        else
                          let e = if -.closing < Solver3d.default.bounce_threshold then 0. else Float.max b.bounciness o.bounciness in
                          Vec3.sub v (Vec3.scale ((1. +. e) *. closing) k.Contact3d.normal)
                  in
                  let x, y, z = Vec3.add at (Vec3.scale ((1. -. t) *. dt) v) and vx, vy, vz = v in
                  final.(i) <- { b with x; y; z; vx; vy; vz }
                end)
        | _ -> ())
      solved_bodies;
  let swept_now = Array.init n (fun i -> final.(i) != solved_bodies.(i)) in
  let used (b : body) = if last then { b with ax = 0.; ay = 0.; az = 0.; torque = (0., 0., 0.) } else b in
  (* the moves, with the solved velocities, and the counters *)
  let bodies =
    Array.mapi
      (fun i b ->
        if asleep.(i) then used b
        else
          let b = solved_bodies.(i) in
          let sx, sy, sz = b.spin in
          let slow = speed b < slow_speed && Float.abs sx +. Float.abs sy +. Float.abs sz < slow_spin in
          still.(i) <- (if movable b && slow then still.(i) + 1 else 0);
          (* claude: and turned by its spin, as [step] turns it (a
           * first version of the world only moved bodies: the solver
           * changed their spins, and nothing ever turned by them -- a
           * domino could slide but not topple, a falling piece of
           * TinyTeardown never tumbled; found when phase 10's flippers,
           * which are nothing but a spin, would not move) *)
          let turned = Quat.turned_by ~spin:(to_radians b.spin) ~dt b.orientation in
          if swept_now.(i) then used { (final.(i)) with orientation = turned }
          else
            used
              { b with
                x = b.x +. (b.vx *. dt); y = b.y +. (b.vy *. dt); z = b.z +. (b.vz *. dt); orientation = turned })
      awake_and_moving
  in
  (* Sleeping, by *islands*: a crate sent to sleep on its own while the
   * ones above it are still settling is woken a moment later with a
   * jolt -- measured, once every sixty-one steps, which is the sleep
   * threshold plus one. So bodies that touch are put in a group
   * (union-find over this step's contacts, immovable bodies not
   * joining any, or the floor would make one island of the world), and
   * a group sleeps only when every body in it is ready. Box2D does the
   * same, and for the same reason. *)
  let parent = Array.init (Array.length bodies) Fun.id in
  let rec root i = if parent.(i) = i then i else (parent.(i) <- root parent.(i); parent.(i)) in
  List.iter
    (fun (i, j) -> if movable bodies.(i) && movable bodies.(j) then parent.(root i) <- root j)
    !touching;
  let ready = Array.mapi (fun i b -> (not (movable b)) || still.(i) >= sleep_after) bodies in
  let island_ready = Array.make (Array.length bodies) true in
  Array.iteri (fun i _ -> if not ready.(i) then island_ready.(root i) <- false) bodies;
  Array.iteri
    (fun i b -> if sleeping && movable b then asleep.(i) <- island_ready.(root i) && still.(i) >= sleep_after)
    bodies;
  { bodies = Array.to_list bodies; memory; still = Array.to_list still; asleep = Array.to_list asleep;
    solved = List.fold_left (fun n (p : Solver3d.pair) -> n + List.length p.Solver3d.contacts) 0 pairs;
    swept = w.swept + !swept }

let simulate ?(gravity = 0.) ?(iterations = Solver3d.default.iterations) ?(warm_starting = true) ?(sleeping = true)
    ?(broad_phase = Broadphase3d.Sweep_and_prune) ?(continuous = false) ?(substeps = 1) (w : world) : world =
  let dt = tick /. float_of_int (max 1 substeps) in
  let rec go w n =
    let w = advance ~gravity ~iterations ~warm_starting ~sleeping ~broad_phase ~continuous ~dt ~last:(n = 1) w in
    if n <= 1 then w else go w (n - 1)
  in
  go { w with swept = 0 } (max 1 substeps)

(* the fast body's path during its last step, from where it was a tick
 * ago, swept as a sphere of its narrowest size *)
let went_through (fast : body) (b : body) : bool =
  let motion = (fast.vx *. tick, fast.vy *. tick, fast.vz *. tick) in
  let from = Vec3.sub (fast.x, fast.y, fast.z) motion in
  Sweep3d.sphere ~radius:(sweep_radius fast) ~from ~motion (hitbox_of b) <> None
