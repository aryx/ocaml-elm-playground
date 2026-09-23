(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Physics3d, the Evan-style API over physics/3d *)

open Playground
open Playground3d
open Physics3d

(* [ball] is a verb here and a good name for a body: keep both *)
let ball_hitbox = ball

let t = Testo.create
let close = Alcotest.(check (float 1e-9))
let rec repeat n f b = if n = 0 then b else repeat (n - 1) f (f b)

(* Physics3d.mli's first example, for one second: 2 metres along x, and
 * down by g dt^2 (1 + 2 + ... + 60) = 9.8 * 1830 / 3600, which is
 * semi-implicit Euler's first-order error over the exact 4.9 *)
let thrown_ball () =
  let ball = body (sphere red 0.2) |> at 0. 3. 0. |> moving 2. 0. 0. in
  let b = repeat 60 (fun b -> b |> fall 9.8 |> step) ball in
  close "x" 2. b.x;
  close "y" (3. -. (9.8 /. 3600. *. 1830.)) b.y;
  close "and the speed down is exact" (-9.8) b.vy

(* the pushes add up in any order, and [step] uses them up *)
let accumulator () =
  let b = body (cube red 1.) in
  let one = b |> fall 10. |> push 3. 0. 0. |> step and two = b |> push 3. 0. 0. |> fall 10. |> step in
  Alcotest.(check (float 1e-12)) "order doesn't matter" one.vx two.vx;
  Alcotest.(check (float 1e-12)) "used up" 0. one.ax;
  Alcotest.(check (float 1e-12)) "twice the mass, half the push" (1.5 *. tick) (b |> heavy 2. |> push 3. 0. 0. |> step).vx

(* drag's top speed, as in 2D: g / c *)
let top_speed () =
  let b = repeat 3000 (fun b -> b |> fall 9.8 |> slow 2. |> step) (body (cube red 1.)) in
  close "falling at 4.9 m/s" (-4.9) b.vy

(* a body faces its own -z, the direction 3D games' characters face;
 * [pointing] turns it, [thrust] pushes it that way *)
let directions () =
  let b = body (cube red 1.) in
  let fx, fy, fz = forward b in
  close "by default, -z" (-1.) fz;
  close "and nothing sideways" 0. fx;
  close "nor up" 0. fy;
  let turned = b |> pointing (0., 1., 0.) 90. in
  let fx, _, fz = forward turned in
  Alcotest.(check (float 1e-9)) "a quarter turn about y faces -x" (-1.) fx;
  Alcotest.(check (float 1e-9)) "exactly" 0. fz;
  let pushed = turned |> thrust 6. |> step in
  Alcotest.(check (float 1e-9)) "and thrust goes that way" (-0.1) pushed.vx

(* [turning] is degrees per second, like every other angle in the
 * playground, whatever the engine does underneath *)
let turning_a_body () =
  let b = body (cube red 1.) |> turning (0., 1., 0.) 90. |> step in
  let _, sy, _ = b.spin in
  close "the spin stays 90 degrees a second" 90. sy;
  let _, angle = Quat.to_axis_angle b.orientation in
  close "and one tick of it is 1.5 degrees" 1.5 (angle *. 180. /. Float.pi);
  (* a full second of it is a quarter turn: what it says on the tin *)
  let after = repeat 60 step (body (cube red 1.) |> turning (0., 1., 0.) 90.) in
  let _, angle = Quat.to_axis_angle after.orientation in
  Alcotest.(check (float 1e-6)) "a second, a quarter turn" 90. (angle *. 180. /. Float.pi)

(* gravitation, with the 2D API's own numbers: a sun of 1e6, a body 100
 * away, pulled at 100 *)
let gravitation () =
  let sun = body (sphere yellow 1.) |> heavy 1e6 in
  let b = body (sphere red 0.2) |> at 100. 0. 0. |> attracted_by sun in
  close "towards the sun" (-100.) b.ax;
  close "and only towards it" 0. b.az;
  let planet = body (sphere red 0.2) |> at 100. 0. 0. |> moving 0. 0. (-100.) in
  let after = repeat 377 (fun b -> b |> attracted_by sun |> step) planet in
  Alcotest.(check (float 0.05)) "one orbit later, still 100 out" 100. (distance sun after)

(* a body is given the tensor of its own bounding box, so a torque
 * spins it -- and an upright one is exactly the body no torque can
 * turn *)
let spinning () =
  let cube_body = body (cube red 1.) in
  let sides = Physics3d.bounds cube_body.shape in
  let (ax, ay, az), (bx, by, bz) = sides in
  close "a unit cube measures 1 across" 1. (bx -. ax);
  close "1 up" 1. (by -. ay);
  close "1 deep" 1. (bz -. az);
  let spun = cube_body |> spin_by 0. 1. 0. |> step in
  let _, sy, _ = spun.spin in
  (* I = m (w^2 + d^2) / 12 = 1/6 for a unit cube, so a torque of 1
   * gives 6 rad/s^2, and a tick of it 0.1 rad/s = 5.7 degrees/s *)
  Alcotest.(check (float 1e-6)) "a torque of 1 on a unit cube" (6. *. tick *. 180. /. Float.pi) sy;
  let post = body (box gray 0.2 2. 0.2) |> upright |> spin_by 0. 10. 0. |> step in
  Alcotest.(check bool) "an upright body ignores every torque" true (post.spin = (0., 0., 0.));
  (* but keeps a spin it was given: a flipper the game drives *)
  let flipper = body (box gray 0.2 0.1 1.) |> upright |> turning (0., 1., 0.) 720. |> spin_by 0. 99. 0. |> step in
  let _, sy, _ = flipper.spin in
  close "a driven spin is untouched" 720. sy

(* Archimedes, through the API: a body settles with [density] of its
 * height under water *)
let floating_body () =
  let barrel = body (box brown 0.6 1. 0.6) |> at 0. 3. 0. in
  let rested = repeat 6000 (fun b -> b |> floating ~water:0. ~density:0.6 |> step) barrel in
  (* its centre sits at half_height - density * height above the line *)
  Alcotest.(check (float 1e-3)) "60% of it under water" (0.5 -. 0.6) rested.y;
  let rock = repeat 600 (fun b -> b |> floating ~water:0. ~density:2.5 |> step) barrel in
  Alcotest.(check bool) "and a rock keeps going down" true (rock.y < -1.)

(* [draw] puts the shape where the body is, turned; [debug] draws
 * something for every body *)
let drawing () =
  let b = body (cube red 2.) |> at 1. 2. 3. |> pointing (0., 1., 0.) 90. in
  let (ax, ay, az), (bx, by, bz) = Physics3d.bounds (draw b) in
  close "drawn where the body is (x)" 1. ((ax +. bx) /. 2.);
  close "(y)" 2. ((ay +. by) /. 2.);
  close "(z)" 3. ((az +. bz) /. 2.);
  Alcotest.(check (float 1e-9)) "a cube turned a quarter is still 2 across" 2. (bx -. ax);
  let (dx, _, _), (ex, _, _) = Physics3d.bounds (debug (b |> moving 1. 0. 0.)) in
  Alcotest.(check bool) "the debug drawing covers the body and its velocity" true (ex -. dx > 2.)

(* what a body *is* to a collision is not what it looks like: a sphere
 * is a box to the engine until you say [ball], and the two answer
 * differently in exactly the corner where it matters *)
let hitboxes () =
  let cube = body (Playground3d.cube red 1.) in
  let round_thing = body (sphere blue 0.5) |> at 0.9 0.9 0. in
  Alcotest.(check bool) "as boxes, their corners overlap" true (touching cube round_thing);
  Alcotest.(check bool) "as a ball, it misses the cube's corner" false (touching cube (ball round_thing));
  (* and the tensor follows the hitbox *)
  let as_ball = ball (body (sphere blue 0.5)) in
  let i = as_ball.inertia in
  Alcotest.(check (float 1e-9)) "a ball's tensor, 2/5 m r^2" (0.4 *. 0.25) i.Mat3.m00;
  Alcotest.(check (float 1e-9)) "the same about every axis" i.Mat3.m00 i.Mat3.m22;
  (* a pill is a capsule standing up *)
  (match (pill (body (Playground3d.box gray 0.6 2. 0.6))).hitbox with
  | Hitbox3d.Capsule (half, r) ->
      Alcotest.(check (float 1e-9)) "as wide as the narrow sides" 0.3 r;
      Alcotest.(check (float 1e-9)) "and its segment is what is left" 0.7 half
  | _ -> Alcotest.fail "pill should give a capsule");
  (* upright survives being given a hitbox, whichever order *)
  Alcotest.(check bool) "upright then ball is still upright" true ((body (sphere red 1.) |> upright |> ball).inertia = Body3d.never_turns);
  (* the hitbox goes where the body goes, and turns with it *)
  let turned = body (Playground3d.box red 2. 0.5 0.5) |> at 1. 2. 3. |> pointing (0., 1., 0.) 90. in
  let placed = hitbox_of turned in
  Alcotest.(check (float 1e-9)) "placed where the body is" 3. (let _, _, z = placed.Hitbox3d.pos in z);
  let _, _, hz = Vec3.sub (snd (Hitbox3d.bounds placed)) (fst (Hitbox3d.bounds placed)) in
  Alcotest.(check (float 1e-6)) "turned a quarter, its length is along z" 2. hz

(* a contact says how far in and which way out *)
let contacts () =
  let floor = body (Playground3d.box gray 10. 1. 10.) |> at 0. (-0.5) 0. |> immovable in
  let crate = body (Playground3d.cube brown 1.) |> at 0. 0.4 0. in
  match contact floor crate with
  | None -> Alcotest.fail "the crate's bottom is at -0.1, under the floor's top"
  | Some c ->
      Alcotest.(check (float 1e-6)) "0.1 through the floor" 0.1 c.Contact3d.depth;
      let _, ny, _ = c.Contact3d.normal in
      Alcotest.(check (float 1e-6)) "pushed up, out of the floor" 1. ny

(* what picking, aiming and a ground check are made of *)
let rays () =
  let near = body (Playground3d.cube red 1.) |> at 0. 0. (-3.) in
  let far = body (Playground3d.cube blue 1.) |> at 0. 0. (-8.) in
  let aside = body (Playground3d.cube green 1.) |> at 5. 0. (-5.) in
  match ray ~from:(0., 0., 0.) ~direction:(0., 0., -1.) [ far; aside; near ] with
  | None -> Alcotest.fail "two of them are straight ahead"
  | Some (hit, distance) ->
      Alcotest.(check (float 1e-6)) "the nearer one, at its face" 2.5 distance;
      Alcotest.(check bool) "and it is the near one" true (hit.z = near.z);
      Alcotest.(check bool) "nothing that way" true (ray ~from:(0., 0., 0.) ~direction:(0., 1., 0.) [ far; aside; near ] = None);
      (* a ground check: down, from just above the floor *)
      let floor = body (Playground3d.box gray 10. 1. 10.) |> at 0. (-0.5) 0. in
      (match ray ~from:(0., 0.6, 0.) ~direction:(0., -1., 0.) [ floor ] with
      | None -> Alcotest.fail "the floor is right below"
      | Some (_, d) -> Alcotest.(check (float 1e-6)) "0.6 above the ground" 0.6 d)

(* the API's side of phase 5: one call, and the pair has bounced *)
let bouncing () =
  let floor = body (Playground3d.box gray 10. 1. 10.) |> at 0. (-0.5) 0. |> immovable in
  let ball = body (sphere red 0.5) |> ball_hitbox |> at 0. 0.4 0. |> moving 0. (-3.) 0. |> bouncy 0.8 in
  let after = bounce_off floor ball in
  Alcotest.(check (float 1e-6)) "it comes back at 0.8 of the speed" 2.4 after.vy;
  Alcotest.(check bool) "and is no longer inside the floor" true (after.y >= 0.5 -. 1e-9);
  let dead = bounce_off floor (ball |> bouncy 0.) in
  Alcotest.(check (float 1e-9)) "clay does not come back" 0. dead.vy;
  (* two balls head on, equal and elastic: they swap *)
  let left = body (sphere red 0.5) |> ball_hitbox |> at (-0.4) 0. 0. |> moving 2. 0. 0. |> bouncy 1. in
  let right = body (sphere blue 0.5) |> ball_hitbox |> at 0.4 0. 0. |> bouncy 1. in
  let left, right = bounce left right in
  Alcotest.(check (float 1e-6)) "the first stops" 0. left.vx;
  Alcotest.(check (float 1e-6)) "the second leaves at 2" 2. right.vx;
  (* bodies that miss are handed back untouched *)
  let far = body (sphere blue 0.5) |> ball_hitbox |> at 9. 0. 0. in
  let a, b = bounce left far in
  Alcotest.(check bool) "nothing happens to a pair that misses" true (a.vx = left.vx && b.x = far.x);
  (* and all of them at once *)
  match bounce_all [ left; right; far ] with
  | [ _; _; _ ] -> ()
  | _ -> Alcotest.fail "bounce_all gives back as many bodies as it took"

let tests =
  [ t "Physics3d, a thrown ball" thrown_ball;
    t "Physics3d, the pushes add up and are used up" accumulator;
    t "Physics3d, drag's top speed" top_speed;
    t "Physics3d, which way a body faces, and thrust" directions;
    t "Physics3d, turning, in degrees a second" turning_a_body;
    t "Physics3d, gravitation and an orbit" gravitation;
    t "Physics3d, a torque, the tensor of its own box, and upright" spinning;
    t "Physics3d, a barrel floats at its density" floating_body;
    t "Physics3d, draw and debug" drawing;
    t "Physics3d, a hitbox is not a drawing" hitboxes;
    t "Physics3d, a contact" contacts;
    t "Physics3d, rays: picking, aiming, a ground check" rays;
    t "Physics3d, bouncing" bouncing ]
