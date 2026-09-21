(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Physics.mli *)

open Playground

type body = {
  shape : shape;
  x : number;
  y : number;
  vx : number;
  vy : number;
  angle : number;
  spin : number;
  mass : number;
  bounciness : number;
  friction : number;
  upright : bool;
  ax : number;
  ay : number;
}

let body (shape : shape) : body =
  { shape; x = 0.; y = 0.; vx = 0.; vy = 0.; angle = 0.; spin = 0.; mass = 1.; bounciness = 0.; friction = 0.; upright = false; ax = 0.; ay = 0. }

let at x y (b : body) : body = { b with x; y }
let moving vx vy (b : body) : body = { b with vx; vy }
let radians degrees = degrees *. Float.pi /. 180.

let launched speed angle (b : body) : body =
  { b with vx = speed *. cos (radians angle); vy = speed *. sin (radians angle) }

let shot_from speed distance (shooter : body) (b : body) : body =
  let c = cos (radians shooter.angle) and s = sin (radians shooter.angle) in
  {
    b with
    x = shooter.x +. (distance *. c);
    y = shooter.y +. (distance *. s);
    vx = shooter.vx +. (speed *. c);
    vy = shooter.vy +. (speed *. s);
    angle = shooter.angle;
  }

let pointing angle (b : body) : body = { b with angle }
let heavy mass (b : body) : body = { b with mass }
let bouncy bounciness (b : body) : body = { b with bounciness }
let rough friction (b : body) : body = { b with friction }
let immovable (b : body) : body = { b with mass = infinity }
let upright (b : body) : body = { b with upright = true }

(* the accumulator: every push adds an acceleration, [step] uses them up *)
let accelerate ax ay (b : body) : body = { b with ax = b.ax +. ax; ay = b.ay +. ay }
let fall g (b : body) : body = accelerate 0. (-.g) b
let push fx fy (b : body) : body = accelerate (fx /. b.mass) (fy /. b.mass) b
let thrust f (b : body) : body = push (f *. cos (radians b.angle)) (f *. sin (radians b.angle)) b
let slow c (b : body) : body = accelerate (-.c *. b.vx) (-.c *. b.vy) b
let turn spin (b : body) : body = { b with spin }

let attracted_by (other : body) (b : body) : body =
  let (ax, ay) = Force.gravitation ~gm:other.mass ~center:(other.x, other.y) (b.x, b.y) (b.vx, b.vy) in
  accelerate ax ay b
let pulled_to x y k (b : body) : body = accelerate (k *. (x -. b.x)) (k *. (y -. b.y)) b
let tick = 1. /. 60.

let step (b : body) : body =
  (* one step of the engine's semi-implicit Euler, the pushes as a
   * constant acceleration during the step *)
  let s = Integrate.semi_implicit_euler ~force:(Force.uniform (b.ax, b.ay)) ~dt:tick (Body.make ~vel:(b.vx, b.vy) (b.x, b.y)) in
  let (x, y) = s.pos and (vx, vy) = s.vel in
  { b with x; y; vx; vy; angle = b.angle +. (b.spin *. tick); ax = 0.; ay = 0. }

let wrap (screen : screen) (b : body) : body =
  let around v lo hi = if v < lo then v +. (hi -. lo) else if v > hi then v -. (hi -. lo) else v in
  { b with x = around b.x screen.left screen.right; y = around b.y screen.bottom screen.top }

let bounce_in (screen : screen) bounciness (b : body) : body =
  (* back inside, the velocity reversed if it was going further out *)
  let axis v p lo hi =
    if p < lo then (lo, if v < 0. then -.v *. bounciness else v)
    else if p > hi then (hi, if v > 0. then -.v *. bounciness else v)
    else (p, v)
  in
  let (x, vx) = axis b.vx b.x screen.left screen.right in
  let (y, vy) = axis b.vy b.y screen.bottom screen.top in
  { b with x; y; vx; vy }

(* the hitboxes of a shape tree, in the world: [world] turns a point of
 * the parent's frame into world coordinates, [scale] is the parent's
 * total scale; a shape is scaled, then rotated, then moved inside its
 * parent (Playground's order, see the software backend's
 * shape_transform) *)
let rec hitboxes_of (world : number * number -> number * number) (scale : number) (s : shape) : Shape.placed list =
  let a = radians s.angle in
  let c = cos a and sn = sin a in
  let to_world (x, y) =
    let x = x *. s.scale and y = y *. s.scale in
    world (s.x +. (x *. c) -. (y *. sn), s.y +. (x *. sn) +. (y *. c))
  in
  let polygon corners = [ Shape.Polygon_at (List.map to_world corners) ] in
  match s.form with
  | Circle (_, r) -> [ Shape.Circle_at (to_world (0., 0.), r *. s.scale *. scale) ]
  | Rectangle (_, w, h) | Image (w, h, _) -> polygon (Shape.box_corners w h)
  | Oval (_, w, h) ->
      polygon (List.init 16 (fun i -> let t = radians (22.5 *. float_of_int i) in (w /. 2. *. cos t, h /. 2. *. sin t)))
  (* like elm-playground: the first corner at the top, then clockwise *)
  | Ngon (_, n, r) ->
      polygon (List.init n (fun i -> let t = radians (90. -. (360. *. float_of_int i /. float_of_int n)) in (r *. cos t, r *. sin t)))
  | Polygon (_, corners) -> polygon corners
  (* an estimate: the playground doesn't know the font's widths *)
  | Words (_, text) ->
      polygon (Shape.box_corners (0.6 *. words_font_size *. float_of_int (String.length text)) words_font_size)
  | Group shapes -> List.concat_map (hitboxes_of to_world (scale *. s.scale)) shapes

let hitboxes (b : body) : Shape.placed list =
  let a = radians b.angle in
  let c = cos a and s = sin a in
  hitboxes_of (fun (x, y) -> (b.x +. (x *. c) -. (y *. s), b.y +. (x *. s) +. (y *. c))) 1. b.shape

(* the moment of inertia: the mass spread evenly over the hitboxes,
 * placed around the body's center, unturned (Shape.moments) *)
let inertia (b : body) : number =
  if b.upright || b.mass = infinity then infinity
  else
    let (area, j) =
      List.fold_left
        (fun (a, j) h -> let (a', j') = Shape.moments h in (a +. a', j +. j'))
        (0., 0.) (hitboxes_of Fun.id 1. b.shape)
    in
    if area = 0. then infinity else b.mass *. j /. area

(* to and from the engine's bodies, whose spins are in radians *)
let state (b : body) : Body.t =
  Body.make ~vel:(b.vx, b.vy) ~mass:b.mass ~spin:(radians b.spin) ~inertia:(inertia b) (b.x, b.y)

let with_state (s : Body.t) (b : body) : body =
  let (x, y) = s.pos and (vx, vy) = s.vel in
  { b with x; y; vx; vy; spin = s.spin *. 180. /. Float.pi }

let touching (a : body) (b : body) : bool =
  let hb = hitboxes b in
  List.exists (fun h -> List.exists (Collide.touching h) hb) (hitboxes a)

(* the deepest contact between any of their hitboxes *)
let contact (a : body) (b : body) : Contact.t option =
  let hb = hitboxes b in
  List.fold_left
    (fun best ha ->
      List.fold_left
        (fun best hb ->
          match (Collide.contact ha hb, best) with
          | Some (c : Contact.t), Some (d : Contact.t) when d.depth >= c.depth -> best
          | Some c, _ -> Some c
          | None, _ -> best)
        best hb)
    None (hitboxes a)

let bounce (a : body) (b : body) : body * body =
  match contact a b with
  | None -> (a, b)
  | Some c ->
      (* the pair's: the bouncier one's bounciness, the geometric mean
       * of the frictions (Box2D's choices) *)
      let restitution = Float.max a.bounciness b.bounciness and friction = sqrt (a.friction *. b.friction) in
      let (sa, sb) = Resolve.resolve ~restitution ~friction (state a, state b) c in
      (with_state sa a, with_state sb b)

let bounce_off (wall : body) (b : body) : body = fst (bounce b (immovable wall))

(* the bounding box of all its hitboxes (its center alone if it has
 * none: it touches nothing anyway) *)
let bounds (b : body) : Broadphase.box =
  let union ((x0, y0), (x1, y1)) ((x0', y0'), (x1', y1')) =
    ((Float.min x0 x0', Float.min y0 y0'), (Float.max x1 x1', Float.max y1 y1'))
  in
  match List.map Shape.bounds (hitboxes b) with
  | [] -> ((b.x, b.y), (b.x, b.y))
  | first :: rest -> List.fold_left union first rest

let broad_phase (m : Broadphase.method_) (bodies : body list) : Broadphase.result =
  Broadphase.pairs m (Array.of_list (List.map bounds bodies))

let bounce_all ?(broad_phase = Broadphase.Sort_and_sweep) (bodies : body list) : body list =
  let a = Array.of_list bodies in
  (* the candidate pairs, i < j, in order, each seeing the earlier
   * bounces *)
  (Broadphase.pairs broad_phase (Array.map bounds a)).pairs
  |> List.iter (fun (i, j) ->
         let (bi, bj) = bounce a.(i) a.(j) in
         a.(i) <- bi;
         a.(j) <- bj);
  Array.to_list a

type world = { bodies : body list; memory : Solver.memory; joints : Joint2d.t list }

let world (bodies : body list) : world = { bodies; memory = Solver.nothing; joints = [] }

let simulate ?(gravity = 0.) ?(iterations = Solver.default.iterations) ?(warm_starting = true) (w : world) : world =
  let moving (b : body) = b.mass <> infinity in
  (* the pushes change the velocities (semi-implicit Euler: the
   * velocities first, the positions last, with the new velocities) *)
  let bodies =
    Array.of_list w.bodies
    |> Array.map (fun b ->
           if not (moving b) then b
           else let b = fall gravity b in { b with vx = b.vx +. (b.ax *. tick); vy = b.vy +. (b.ay *. tick) })
  in
  (* the contacts: the broad phase's pairs, each with its points; two
   * bodies joined by a joint don't collide (a seesaw sits on its pivot) *)
  let joined i j = List.exists (fun (jt : Joint2d.t) -> (jt.a = i && jt.b = j) || (jt.a = j && jt.b = i)) w.joints in
  let pairs =
    (Broadphase.sort_and_sweep (Array.map bounds bodies)).pairs
    |> List.filter (fun (i, j) -> not (joined i j))
    |> List.filter_map (fun (i, j) ->
           let a = bodies.(i) and b = bodies.(j) in
           let hb = hitboxes b in
           match List.concat_map (fun ha -> List.concat_map (Collide.manifold ha) hb) (hitboxes a) with
           | [] -> None
           | _ when not (moving a || moving b) -> None
           | contacts ->
               Some
                 { Solver.a = i; b = j; contacts;
                   restitution = Float.max a.bounciness b.bounciness; friction = sqrt (a.friction *. b.friction) })
  in
  let angles = Array.map (fun b -> radians b.angle) bodies in
  let (states, memory) =
    Solver.solve { Solver.default with iterations; warm_starting } ~dt:tick ~joints:(angles, w.joints) (Array.map state bodies)
      pairs w.memory
  in
  (* the moves, with the solved velocities *)
  let bodies =
    Array.mapi
      (fun i b ->
        let b = with_state states.(i) b in
        { b with x = b.x +. (b.vx *. tick); y = b.y +. (b.vy *. tick); angle = b.angle +. (b.spin *. tick); ax = 0.; ay = 0. })
      bodies
  in
  { w with bodies = Array.to_list bodies; memory }

(* the joints, made from where the bodies are now (see Physics.mli) *)
let states (w : world) : Body.t array * float array =
  let bodies = Array.of_list w.bodies in
  (Array.map state bodies, Array.map (fun b -> radians b.angle) bodies)

let add (j : Body.t array -> float array -> Joint2d.t) (w : world) : world =
  let s, angles = states w in
  { w with joints = w.joints @ [ j s angles ] }

let pin ?motor (a : int) (b : int) ~(at : number * number) (w : world) : world =
  let motor = Option.map (fun (speed, torque) -> (radians speed, torque)) motor in
  add (fun s angles -> Joint2d.pin s angles a b ~at ?motor ()) w

let rod (a : int) (b : int) ~at_a ~at_b (w : world) : world = add (fun s angles -> Joint2d.rod s angles a b ~at_a ~at_b ()) w

let rope ?length (a : int) (b : int) ~at_a ~at_b (w : world) : world =
  add (fun s angles -> Joint2d.rope s angles a b ~at_a ~at_b ?length ()) w

let pulley (a : int) (b : int) ~at_a ~at_b ~ground_a ~ground_b (w : world) : world =
  add (fun s angles -> Joint2d.pulley s angles a b ~at_a ~at_b ~ground_a ~ground_b ()) w

let set_motor (i : int) ((speed, torque) : number * number) (w : world) : world =
  { w with
    joints =
      List.mapi
        (fun k (j : Joint2d.t) ->
          match j.kind with
          | Pin _ when k = i -> { j with kind = Pin { motor = Some (radians speed, torque) } }
          | _ -> j)
        w.joints }

let joint_length (i : int) (w : world) : number =
  let s, angles = states w in
  Joint2d.length_now s angles (List.nth w.joints i)

let debug_joints (w : world) : shape list =
  let s, angles = states w in
  List.concat_map
    (fun (j : Joint2d.t) ->
      let (ax, ay), (bx, by) = Joint2d.anchors s angles j in
      let line (x1, y1) (x2, y2) =
        let dx = x2 -. x1 and dy = y2 -. y1 in
        rectangle (rgb 240 200 60) (Float.hypot dx dy) 2. |> rotate (atan2 dy dx *. 180. /. Float.pi)
        |> Playground.move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)
      in
      match j.kind with
      | Pin _ -> [ circle (rgb 240 200 60) 4. |> Playground.move ax ay ]
      | Rod _ | Rope _ -> [ line (ax, ay) (bx, by) ]
      | Pulley { ground_a; ground_b; _ } -> [ line (ax, ay) ground_a; line ground_a ground_b; line ground_b (bx, by) ])
    w.joints

let went_through (fast : body) (b : body) : bool =
  let path = ((fast.x -. (fast.vx *. tick), fast.y -. (fast.vy *. tick)), (fast.x, fast.y)) in
  hitboxes b
  |> List.exists (function
       | Shape.Polygon_at corners -> Collide.segment_polygon path corners <> None
       | Shape.Circle_at (c, r) -> Collide.segment_circle path (c, r) <> None
       | Shape.Point_at p -> Collide.segment_circle path (p, 0.) <> None)

let debug (b : body) : shape =
  let green = rgb 0 200 0 in
  let hitbox = function
    | Shape.Point_at (x, y) -> circle green 3. |> Playground.move x y
    | Shape.Circle_at ((x, y), r) -> circle green r |> Playground.move x y
    | Shape.Polygon_at corners -> polygon green corners
  in
  let length = Float.hypot b.vx b.vy /. 4. in
  let arrow =
    group [ rectangle green length 2. |> Playground.move_x (length /. 2.) ]
    |> rotate (Float.atan2 b.vy b.vx *. 180. /. Float.pi)
    |> Playground.move b.x b.y
  in
  (* each piece faded, not the group: not every backend fades groups *)
  group (List.map (fun s -> s |> fade 0.5) (List.map hitbox (hitboxes b) @ [ arrow ]))

let draw (b : body) : shape = b.shape |> rotate b.angle |> Playground.move b.x b.y
let distance (a : body) (b : body) : number = Float.hypot (a.x -. b.x) (a.y -. b.y)
let speed (b : body) : number = Float.hypot b.vx b.vy

let outside (screen : screen) (b : body) : bool =
  b.x < screen.left || b.x > screen.right || b.y < screen.bottom || b.y > screen.top
