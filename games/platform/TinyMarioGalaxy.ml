(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Super Mario Galaxy (Nintendo EAD Tokyo, 2007), in
 * 3D as the original: Mario runs round a planetoid, over a cube's
 * edges and along a capsule, drops from under his planet onto a flat
 * platform, and jumps from its end into the pull of the tiny planet of
 * the Power Star.
 *
 *   arrows       run, relative to the camera
 *   space (z)    jump
 *   a d          turn the camera round Mario
 *
 * TinyMarioGalaxy2D is its 2D twin, and its header explains the idea:
 * gravity as level design, zones each with a down and a priority
 * instead of Newton's pull ([zone], [gravity_at]), and up as local, the
 * velocity split every frame along the ground and along up ([step]).
 * Those are the same here, the vectors having a third number. What 3D
 * adds:
 *
 *   - any solid gives a gravity: pull towards its nearest point, and a
 *     ball pulls to its centre, a capsule to its spine, a box to its
 *     surface -- walk off a cube's face and its edge turns you onto the
 *     next face, as on Galaxy's cube planets (GlobalCubeGravity in its
 *     level files, from memory, to check). One function, [nearest], for
 *     both the collisions and the gravity;
 *   - which way is Mario facing? In 2D, one sign. In 3D a direction on
 *     the ground, and the ground turns under him as he walks: the
 *     direction is carried along by laying it back on the new ground
 *     each frame ([tangent], parallel transport, the way a gyroscope's
 *     axis goes round a planet), and so is the camera's heading;
 *   - the model is turned so that its up is Mario's up and its front
 *     his facing: [euler], the angles rotate3d wants for a given frame
 *     of axes, the one piece of 3D maths here that isn't a dot product;
 *   - the camera rolls: its up follows Mario's, smoothly (the camera's
 *     [up], which the flight games use to bank), so that the planet is
 *     always under him on the screen. The arrows are read in the
 *     camera's terms, laid on Mario's ground ([wish]), which is why
 *     holding up walks him round the planet and back: the camera's
 *     heading goes round with him. (The 2D twin needs a latch, the
 *     arrow kept while held, when its camera is fixed; here the camera
 *     always follows, as Galaxy's mostly does.)
 *   - and the sun: the renderer's light is fixed in the world, from
 *     above, and the underside of a planet is its night side. Rather
 *     than move the sun, [sun_over] turns the whole world, camera
 *     included, so that the camera's up is the world's: the sun is
 *     always over Mario's head, and the picture is the same.
 *
 * What it uses: Playground3d (spheres, boxes, polygons for the capsule's
 * tube), Camera3d.follow (the eye and target smoothed), Scene2d; the
 * Mario of TinyMario64, his boxes, and its star. Not Physics3d nor
 * Character3d: their gravity is one vector for the world. Not the
 * shadow of TinyMario64: on a planet, "below" is towards its centre,
 * and the shadow is an exercise.
 *
 * Exercises: the shadow, laid on the nearest solid along Mario's down;
 * the zones drawn, faint (the 2D twin shows them); a disk's gravity
 * and the inverted sphere, walked on from the inside; a launch star;
 * the spin; the camera's heading turned slowly after Mario's facing,
 * as Galaxy's does when you run towards it.
 *)
open Playground
open Playground3d
open Basics (* float arithmetics *)

(*****************************************************************************)
(* Vectors *)
(*****************************************************************************)

type vec = number * number * number

let add ((ax, ay, az) : vec) ((bx, by, bz) : vec) : vec = (ax + bx, ay + by, az + bz)
let sub ((ax, ay, az) : vec) ((bx, by, bz) : vec) : vec = (ax - bx, ay - by, az - bz)
let mul (k : number) ((x, y, z) : vec) : vec = (k * x, k * y, k * z)
let dot ((ax, ay, az) : vec) ((bx, by, bz) : vec) : number = (ax * bx) + (ay * by) + (az * bz)
let cross ((ax, ay, az) : vec) ((bx, by, bz) : vec) : vec = ((ay * bz) - (az * by), (az * bx) - (ax * bz), (ax * by) - (ay * bx))
let len (v : vec) : number = sqrt (dot v v)
let zero : vec = (0., 0., 0.)
let norm_or (fallback : vec) (v : vec) : vec = if len v < 1e-6 then fallback else mul (1. / len v) v

(* [v] laid on the ground whose up is [up]: its part along up removed,
 * and made one long again; carried this way frame after frame, a
 * direction follows the ground round a planet *)
let tangent (up : vec) (fallback : vec) (v : vec) : vec = norm_or fallback (sub v (mul (dot v up) up))

(* [v] turned by [deg] degrees round the axis [k] (Rodrigues) *)
let turn_around (k : vec) (deg : number) (v : vec) : vec =
  let a = degrees_to_radians deg in
  add (add (mul (cos a) v) (mul (sin a) (cross k v))) (mul (dot k v * (1. - cos a)) k)

(* the point of the segment [a]-[b] closest to [p] *)
let closest (a : vec) (b : vec) (p : vec) : vec =
  let ab = sub b a in
  let l2 = dot ab ab in
  if l2 = 0. then a else add a (mul (clamp 0. 1. (dot (sub p a) ab / l2)) ab)

(* The angles rotate3d wants (around x, then y, then z) to turn a model
 * standing on y and facing -z into one standing on [up] and facing
 * [front]. Its axes go to right, up and back, the columns of the
 * rotation R = Rz(c) Ry(b) Rx(a), whose entries give the angles back:
 *
 *        | cc.cb   .       .     |     right = (cc.cb, sc.cb, -sb)
 *   R =  | sc.cb   .       .     |     up.z = cb.sa, back.z = cb.ca
 *        | -sb     cb.sa   cb.ca |
 *
 * so b from right.z, a from up.z and back.z, c from right.x and
 * right.y; unless cb = 0 (right straight along z), where only a + c is
 * known (gimbal lock): then a = 0, and c from up. *)
let euler (up : vec) (front : vec) : number * number * number =
  let back = mul (-1.) front in
  let ((rx, ry, rz) as _right) = cross up back in
  let ux, uy, uz = up and _, _, bz = back in
  let b = asin (clamp (-1.) 1. (-.rz)) in
  let a, c = if Float.abs rz < 0.999 then (atan2 uz bz, atan2 ry rx) else (0., atan2 (-.ux) uy) in
  (radians_to_degrees a, radians_to_degrees b, radians_to_degrees c)

let orient (up : vec) (front : vec) (s : shape3d) : shape3d =
  let a, b, c = euler up front in
  rotate3d a b c s

(*****************************************************************************)
(* The galaxy *)
(*****************************************************************************)

(* A solid is the points within [radius] of a segment (a ball when
 * [a] = [b], a capsule otherwise), or a box. *)
type solid = Capsule of { a : vec; b : vec; radius : number } | Block of { centre : vec; half : vec }

(* the nearest point of [s]'s core to [p], and the radius around it:
 * for the collisions, and for the gravity *)
let nearest (s : solid) ((x, y, z) as p : vec) : vec * number =
  match s with
  | Capsule { a; b; radius } -> (closest a b p, radius)
  | Block { centre = cx, cy, cz; half = hx, hy, hz } ->
      ((clamp (cx - hx) (cx + hx) x, clamp (cy - hy) (cy + hy) y, clamp (cz - hz) (cz + hz) z), 0.)

type body = { solid : solid; color : color }

(* Where the gravity is: [Around] a solid, towards its nearest point, up
 * to [reach] from its surface; [Parallel], in a box (its two corners),
 * towards a fixed [down]. *)
type zone =
  | Around of { solid : solid; reach : number; priority : int }
  | Parallel of { low : vec; high : vec; down : vec; priority : int }

let ball (centre : vec) (radius : number) : solid = Capsule { a = centre; b = centre; radius }

let home = ball zero 5.
let pill = Capsule { a = (0., 4., -12.5); b = (0., 10., -17.); radius = 2. }
let cube_planet = Block { centre = (-13., 3., 0.); half = (2.2, 2.2, 2.2) }

(* under the home planet, gravity straight down *)
let platform = Block { centre = (5.5, -18., 0.); half = (11.5, 0.5, 6.) }
let tiny = ball (24., -13., 0.) 1.8

let bodies =
  [ { solid = home; color = rgb 110 190 90 };
    { solid = pill; color = rgb 220 160 90 };
    { solid = cube_planet; color = rgb 150 110 210 };
    { solid = platform; color = rgb 150 150 200 };
    { solid = tiny; color = rgb 230 120 160 } ]

let zones =
  [ Around { solid = home; reach = 7.5; priority = 1 };
    Around { solid = pill; reach = 6.5; priority = 1 };
    Around { solid = cube_planet; reach = 6.; priority = 1 };
    Parallel { low = (-6.5, -17.5, -6.5); high = (17.5, -8.5, 6.5); down = (0., -1., 0.); priority = 2 };
    Around { solid = tiny; reach = 7.5; priority = 1 } ]

let star_bits : vec list =
  List.init 4 (fun i ->
      let a = degrees_to_radians (float_of_int i * 90.) in
      (7. * cos a, 3.5, 7. * sin a))
  @ List.init 4 (fun i -> add (0., 3.2, 0.) (closest (0., 4., -12.5) (0., 10., -17.) (0., 4. + (2. * float_of_int i), -13. - (1.5 * float_of_int i))))
  @ [ (-13., 6.2, 0.); (-13., -0.2, 0.); (-13., 3., 3.2); (-13., 3., -3.2) ]
  @ List.init 5 (fun i -> (-3. + (float_of_int i * 4.), -16.8, 0.))

(* on the far side of the tiny planet *)
let power_star : vec = (24., -13., 2.9)

(*****************************************************************************)
(* Gravity, zone by zone *)
(*****************************************************************************)

let inside ((lx, ly, lz) : vec) ((hx, hy, hz) : vec) ((x, y, z) : vec) : bool =
  x >= lx && x <= hx && y >= ly && y <= hy && z >= lz && z <= hz

(* [z] at [p]: which way it pulls, and how far [p] is from its solid (0
 * in a box), or None if [p] is out of it *)
let pull (z : zone) (p : vec) : (vec * number) option =
  match z with
  | Around { solid; reach; _ } ->
      let q, r = nearest solid p in
      let d = len (sub p q) - r in
      if d <= reach then Some (norm_or (0., -1., 0.) (sub q p), d) else None
  | Parallel { low; high; down; _ } -> if inside low high p then Some (down, 0.) else None

let priority (z : zone) : int = match z with Around { priority; _ } | Parallel { priority; _ } -> priority

(* which way is down at [p]: the zone of highest priority there, and of
 * those the nearest; None in the void *)
let gravity_at (p : vec) : vec option =
  List.fold_left
    (fun best z ->
      match (pull z p, best) with
      | None, _ -> best
      | Some (down, d), None -> Some (down, d, priority z)
      | Some (down, d), Some (_, bd, bp) ->
          if priority z > bp || (priority z = bp && d < bd) then Some (down, d, priority z) else best)
    None zones
  |> Option.map (fun (down, _, _) -> down)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  pos : vec; (* the centre of Mario's ball *)
  vel : vec;
  up : vec; (* the last down turned round, kept while adrift *)
  facing : vec; (* on the ground, carried along *)
  standing : bool;
  cam_up : vec; (* the camera's up, after Mario's *)
  cam_heading : vec; (* where the camera looks along the ground, carried along *)
  cam : camera; (* smoothed *)
  goomba : number; (* its angle round the home planet's equator *)
  squashed : int;
  bits : vec list;
  collected : int;
  adrift : int;
  dying : int;
  deaths : int;
  frames : int;
}

type scene = Title | Galaxy of play | Star of play
type model = scene Scene2d.t

(* Mario's radius: he is a ball, for the collisions *)
let size = 0.5

(* the camera behind and above Mario, rolled to his up *)
let camera_for (pos : vec) (up : vec) (heading : vec) : camera =
  camera ~eye:(add pos (add (mul 3.5 up) (mul (-8.) heading))) ~target:(add pos up) ~up ()

let start : play =
  let pos = (0., 5. + size, 0.) and up = (0., 1., 0.) and heading = (0., 0., -1.) in
  { pos; vel = zero; up; facing = heading; standing = true; cam_up = up; cam_heading = heading;
    cam = camera_for pos up heading; goomba = 180.; squashed = 0; bits = star_bits; collected = 0;
    adrift = 0; dying = 0; deaths = 0; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* what the player does this frame: arrows ahead and to the side (in
 * the camera's terms), the jump, the camera turned; the game reads it
 * off the keyboard, the tests make it up *)
type input = { ahead : number; side : number; jump : bool; turn : number }

let nothing = { ahead = 0.; side = 0.; jump = false; turn = 0. }

let gravity = 0.02
let jump_speed = 0.42 (* 0.42^2 / (2 * 0.02) = 4.4 high *)
let run_top = 0.16
let run_accel = 0.02
let run_brake = 0.025
let air_accel = 0.006
let turn_speed = 2.5
let dying_frames = 40
let lost_frames = 150
let snap = 0.3

(* [v] moved towards [target] by at most [rate] *)
let approach (rate : number) (target : vec) (v : vec) : vec =
  let d = sub target v in
  if len d <= rate then target else add v (mul (rate / len d) d)

(* Mario pushed out of [body], his velocity into it removed; standing
 * if pushed from below, below being [up]'s opposite *)
let collide (up : vec) ((pos, vel, standing) : vec * vec * bool) (body : body) : vec * vec * bool =
  let q, r = nearest body.solid pos in
  let d = len (sub pos q) in
  if d >= r + size then (pos, vel, standing)
  else
    let n = norm_or up (sub pos q) in
    let into = dot vel n in
    let vel = if into < 0. then sub vel (mul into n) else vel in
    (add q (mul (r + size) n), vel, standing || dot n up > 0.5)

(* glued back on the ground he stood on, if it is a few steps away (see
 * the 2D twin's snap_to_ground) *)
let snap_to_ground ((pos, vel) : vec * vec) : vec * vec * bool =
  let gap body =
    let q, r = nearest body.solid pos in
    len (sub pos q) - r - size
  in
  match List.find_opt (fun body -> gap body >= 0. && gap body < snap) bodies with
  | None -> (pos, vel, false)
  | Some body ->
      let q, r = nearest body.solid pos in
      let n = norm_or (0., 1., 0.) (sub pos q) in
      let out = dot vel n in
      (add q (mul (r + size) n), (if out > 0. then sub vel (mul out n) else vel), true)

let goomba_speed = 0.4

(* the goomba on the home planet's equator: its place, its up, its front *)
let goomba_at (angle : number) : vec * vec * vec =
  let a = degrees_to_radians angle in
  let up = (cos a, 0., sin a) in
  (mul (5. + 0.45) up, up, (sin a, 0., -.cos a))

let respawn (p : play) : play =
  { start with bits = p.bits; collected = p.collected; deaths = p.deaths; frames = p.frames }

let step (i : input) (p : play) : play =
  if p.dying > 0 then if p.dying = 1 then respawn p else { p with dying = p.dying -.. 1 }
  else
    let p = { p with frames = p.frames +.. 1 } in
    let down = gravity_at p.pos in
    let up = match down with Some d -> mul (-1.) d | None -> p.up in
    (* the arrows: the camera's heading, turned by a/d, laid on Mario's
     * ground, and its right *)
    let cam_heading = turn_around p.cam_up (-.i.turn * turn_speed) p.cam_heading in
    let ahead = tangent up p.facing cam_heading in
    let right = cross ahead up in
    let wish = norm_or zero (add (mul i.ahead ahead) (mul i.side right)) in
    (* the velocity in this place's terms: on the ground, and up *)
    let vn = dot p.vel up in
    let vt = sub p.vel (mul vn up) in
    let rate = if not p.standing then air_accel else if wish = zero then run_brake else run_accel in
    let vt = approach rate (mul run_top wish) vt in
    let jumped = i.jump && p.standing in
    let vn = if jumped then jump_speed else vn in
    let vel = add vt (mul vn up) in
    let vel = match down with Some d -> add vel (mul gravity d) | None -> vel in
    let pos, vel, standing = List.fold_left (collide up) (add p.pos vel, vel, false) bodies in
    let pos, vel, standing =
      if p.standing && (not jumped) && not standing then snap_to_ground (pos, vel) else (pos, vel, standing)
    in
    let facing = if wish = zero then tangent up ahead p.facing else wish in
    (* the camera: its up turned after Mario's, a tenth of the angle a
     * frame, round their common perpendicular, and its heading turned
     * with it. When his up turns right over (from under the planet onto
     * the platform), the way round matters: tipped over his head, the
     * camera would carry its heading round too, and "right" would
     * change sides; so then it rolls, round its heading, and keeps
     * looking where it looked. *)
    let cam_up, cam_heading =
      let cos_between = dot p.cam_up up in
      let axis = if cos_between < -0.9 then cam_heading else norm_or cam_heading (cross p.cam_up up) in
      let angle = 0.1 * radians_to_degrees (acos (clamp (-1.) 1. cos_between)) in
      let cam_up = turn_around axis angle p.cam_up in
      (cam_up, tangent cam_up facing (turn_around axis angle cam_heading))
    in
    let cam = Camera3d.follow 0.2 (camera_for pos cam_up cam_heading) p.cam in
    let p =
      { p with pos; vel; up; facing; standing; cam_up; cam_heading; cam;
               adrift = (if down = None then p.adrift +.. 1 else 0) }
    in
    let near q = len (sub q p.pos) < size + 0.6 in
    let got, bits = List.partition near p.bits in
    let p = { p with bits; collected = p.collected +.. List.length got } in
    (* the goomba: stomped if Mario comes down on it, down being its
     * own; otherwise it hurts *)
    let g, gup, _ = goomba_at p.goomba in
    let p = if p.squashed > 0 then { p with squashed = p.squashed +.. 1 } else { p with goomba = p.goomba + goomba_speed } in
    if p.squashed = 0 && len (sub g p.pos) < size + 0.5 then
      if dot p.vel gup < -0.05 && dot (sub p.pos g) gup > 0. then
        { p with squashed = 1; vel = add (sub p.vel (mul (dot p.vel gup) gup)) (mul 0.3 gup); standing = false }
      else { p with dying = dying_frames; deaths = p.deaths +.. 1 }
    else if p.adrift > lost_frames then { p with dying = dying_frames; deaths = p.deaths +.. 1 }
    else p

let got_star (p : play) : bool = p.dying = 0 && len (sub power_star p.pos) < size + 0.9

let input_of (scenes : model) (k : keyboard) : input =
  let key name = if Set_.mem name k.keys then 1. else 0. in
  { ahead = to_y k; side = to_x k; jump = Scene2d.pressed (fun k -> k.kspace || Set_.mem "z" k.keys) scenes;
    turn = key "d" - key "a" }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  match scenes.scene with
  | Title | Star _ ->
      if Scene2d.pressed (fun k -> k.kspace) scenes then Scene2d.go (Galaxy start) scenes else scenes
  | Galaxy p ->
      let p = step (input_of scenes computer.keyboard) p in
      if got_star p then Scene2d.go (Star p) scenes else { scenes with scene = Galaxy p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let gold = rgb 250 210 40
let space_color = black

(* a tube round the segment [a]-[b]: twelve quads *)
let tube (color : color) (a : vec) (b : vec) (r : number) : shape3d =
  let axis = norm_or (0., 1., 0.) (sub b a) in
  let e1 = tangent axis (1., 0., 0.) (if Float.abs (dot axis (0., 1., 0.)) < 0.9 then (0., 1., 0.) else (1., 0., 0.)) in
  let e2 = cross axis e1 in
  let at k (o : vec) =
    let t = degrees_to_radians (float_of_int k * 30.) in
    add o (mul r (add (mul (cos t) e1) (mul (sin t) e2)))
  in
  group3d (List.init 12 (fun k -> polygon3d color [ at k a; at (k +.. 1) a; at (k +.. 1) b; at k b ]))

let body_shape (b : body) : shape3d =
  match b.solid with
  | Capsule { a; b = b'; radius } when a = b' ->
      let x, y, z = a in
      sphere b.color radius |> move3d x y z
  | Capsule { a; b = b'; radius } ->
      let ax, ay, az = a and bx, by, bz = b' in
      group3d [ sphere b.color radius |> move3d ax ay az; sphere b.color radius |> move3d bx by bz; tube b.color a b' radius ]
  | Block { centre = x, y, z; half = hx, hy, hz } -> box b.color (2. * hx) (2. * hy) (2. * hz) |> move3d x y z

let galaxy : shape3d = cached3d (List.map body_shape bodies)

(* far stars, all round *)
let sky : shape3d =
  cached3d
    (List.init 90 (fun i ->
         let a = float_of_int ((i *.. 7919) mod 360) and b = float_of_int ((i *.. 104729) mod 170) - 85. in
         let a = degrees_to_radians a and b = degrees_to_radians b in
         cube white 0.6 |> move3d (150. * cos b * cos a) (150. * sin b) (150. * cos b * sin a)))

(* space: a black box round the eye, seen from inside *)
let space (cam : camera) : shape3d =
  let x, y, z = cam.eye in
  box space_color 500. 500. 500. |> move3d x y z

(* Mario, standing on y and facing -z: shoes, overalls, shirt, head,
 * cap and its visor (TinyMario64's) *)
let mario_model : shape3d =
  let red = rgb 220 30 30 and blue = rgb 40 70 200 and skin = rgb 250 190 140 and brown = rgb 110 60 20 in
  group3d
    [ box brown 0.28 0.18 0.4 |> move3d (-0.17) 0.09 (-0.05); box brown 0.28 0.18 0.4 |> move3d 0.17 0.09 (-0.05);
      box blue 0.6 0.4 0.4 |> move_y3d 0.38; box red 0.66 0.3 0.42 |> move_y3d 0.72;
      box skin 0.46 0.4 0.44 |> move_y3d 1.05; box red 0.5 0.14 0.5 |> move_y3d 1.3;
      box red 0.4 0.06 0.25 |> move3d 0. 1.25 (-0.33) ]

let mario (p : play) : shape3d =
  let x, y, z = sub p.pos (mul size p.up) in
  mario_model |> orient p.up p.facing |> move3d x y z

let goomba (p : play) : shape3d =
  let g, up, front = goomba_at p.goomba in
  let brown = rgb 150 90 40 and dark = rgb 60 40 20 in
  let model =
    if p.squashed > 0 then box brown 0.9 0.15 0.9 |> move_y3d (-0.35)
    else
      group3d
        [ box dark 0.3 0.15 0.4 |> move3d (-0.2) (-0.38) 0.; box dark 0.3 0.15 0.4 |> move3d 0.2 (-0.38) 0.;
          box brown 0.8 0.6 0.7; box white 0.15 0.2 0.05 |> move3d (-0.18) 0.1 (-0.36);
          box white 0.15 0.2 0.05 |> move3d 0.18 0.1 (-0.36) ]
  in
  let x, y, z = g in
  model |> orient up front |> move3d x y z

(* a star: an octahedron of gold, turning (TinyMario64's) *)
let star (turn : number) : shape3d =
  let top = (0., 0.7, 0.) and bottom = (0., -0.7, 0.) in
  let ring = [ (0.5, 0., 0.); (0., 0., 0.5); (-0.5, 0., 0.); (0., 0., -0.5) ] in
  group3d
    (List.concat
       (List.mapi
          (fun i a ->
            let b = List.nth ring ((i +.. 1) mod 4) in
            [ polygon3d gold [ top; a; b ]; polygon3d (rgb 230 170 20) [ bottom; b; a ] ])
          ring))
  |> rotate3d 0. turn 0.

let world (p : play) : shape3d list =
  let turn = float_of_int p.frames * 3. in
  [ space p.cam; sky; galaxy; mario p; goomba p;
    (let x, y, z = power_star in
     star turn |> move3d x y z) ]
  @ List.map (fun (x, y, z) -> cube (rgb 120 220 250) 0.3 |> rotate3d turn turn 0. |> move3d x y z) p.bits

(* The renderer's sun is fixed in the world, over +y: under a planet,
 * Mario would be in its shadow, on its night side. So the world is
 * turned instead, so that the camera's up is +y, and the sun is always
 * over Mario's head (Galaxy lights each planet as its designer wants;
 * this is the one-line version). The turn is the camera's frame (right,
 * up, back) read as rows, R's transpose, the inverse of a rotation: a
 * point p goes to (p.right, p.up, p.back), and a shape by [euler]'s
 * three turns undone, in the reverse order. *)
let sun_over (up : vec) (front : vec) ((cam, shapes) : camera * shape3d list) : camera * shape3d list =
  let a, b, c = euler up front in
  let back = mul (-1.) front in
  let right = cross up back in
  let turn p = (dot p right, dot p up, dot p back) in
  ( { cam with eye = turn cam.eye; target = turn cam.target; up = turn cam.up },
    List.map (fun s -> s |> rotate3d 0. 0. (-.c) |> rotate3d 0. (-.b) 0. |> rotate3d (-.a) 0. 0.) shapes )

let view (computer : computer) (model : model) : camera * shape3d list =
  let screen = computer.screen in
  match model.scene with
  | Title ->
      let cam = Camera3d.orbit ~distance:36. ~height:8. ~look:0. (spin 12. computer.time) (5., 2., 0.) in
      ( cam,
        world start
        @ List.map hud
            ([ text gold 7. "TINY MARIO GALAXY" |> move_y 320.;
               text white 2.5 "arrows: run   space: jump   a/d: camera" |> move_y 250. ]
            @ Scene2d.blink 1. model [ text yellow 3. "PRESS SPACE" |> move_y 190. ]) )
  | Galaxy p ->
      let cam, shapes = sun_over p.cam_up p.cam_heading (p.cam, world p) in
      ( cam,
        shapes
        @ [ hud
              (text white 2.5 (Printf.sprintf "star bits %d   deaths %d" p.collected p.deaths)
              |> move (screen.left + 200.) (screen.top - 40.)) ]
        @ if p.adrift > 30 && p.dying = 0 then [ hud (text white 4. "ADRIFT" |> move_y 150.) ] else [] )
  | Star p ->
      let cam, shapes = sun_over p.cam_up p.cam_heading (p.cam, world p) in
      ( cam,
        shapes
        @ List.map hud
            ([ text gold 6. "YOU GOT A POWER STAR" |> move_y 200.;
               text white 2.5
                 (Printf.sprintf "%d star bits, %d deaths, %.1f seconds" p.collected p.deaths (float_of_int p.frames / 60.))
               |> move_y 130. ]
            @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y 70. ]) )

let help =
  {|TinyMarioGalaxy
  arrows       run, relative to the camera
  space (z)    jump
  a d          turn the camera round Mario
|}

let app = game3d view update initial_model

(* flat shading; the back faces drawn too, for space seen from inside *)
let main =
  print_string help;
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false }
    ~flags:(Playground_platform.flags ()) app
