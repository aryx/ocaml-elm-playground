(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A brick wall and a row of dominoes, which is to say: things that
 * stay where they are put.
 *
 *   space     throw a ball at the wall
 *   s         the solver, on and off
 *   i         how many iterations it runs
 *   w         warm starting, on and off
 *   space x2  (after a throw) set it all up again
 *
 * Bouncing and stacking are different problems, and this is the
 * example where that stops being a claim. Answer each pair of bodies
 * once, as Physics3d.bounce does, and every answer undoes a bit of
 * another -- push the top brick up and the one below goes down, into
 * the one below that. Press "s" and watch the wall come apart into a
 * heap of bricks that never quite stop moving. It is not that the
 * contacts are wrong; each one, on its own, is right.
 *
 * With the solver ("s" again) the same contacts are gone over ten
 * times a step, each pass correcting its impulse a little, and the
 * whole wall converges -- Gauss-Seidel, on contacts (Erin Catto,
 * Box2D Lite, 2006; physics/3d/Solver3d.mli). "i" turns the iterations
 * down: at one, the wall sags almost as badly as with no solver at
 * all; by four it mostly stands; by ten it is a wall.
 *
 * "w" is warm starting, which is the other half and invisible until
 * you look: each contact starts from the impulse it needed last step,
 * so the iterations only fix the difference. Without it the wall
 * settles lower and takes longer about it.
 *
 * The pale bricks are asleep. A body that has been still for a second
 * stops being simulated until something moving touches it -- and they
 * sleep in *groups*, everything that touches together, because a
 * brick sent to sleep alone under a wall that is still settling gets
 * woken a moment later with a jolt (measured: every sixty-one steps,
 * which is the threshold plus one).
 *
 * Metres and kilograms: the bricks are 40 cm long and weigh 2 kg, the
 * ball 20 cm across and 6 kg, and it arrives at 9 m/s.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The scene *)
(*****************************************************************************)

let brick_w = 0.4
let brick_h = 0.2
let brick_d = 0.2
let rows = 4
let columns = 6

let floor_body : Physics3d.body =
  Physics3d.body (box (rgb 120 130 115) 20. 0.4 20.)
  |> Physics3d.at 0. (-0.2) 0.
  |> Physics3d.immovable
  |> Physics3d.rough 0.8

(* a running bond: the odd rows have one brick fewer, which staggers
 * them by half a brick and leaves no half hanging off the end *)
let bricks_in (row : int) : int = if row mod 2 = 0 then columns else columns - 1

let brick (i : int) (row : int) : Physics3d.body =
  let n = bricks_in row in
  let shade = 150 + (((i * 7) + (row * 13)) mod 60) in
  Physics3d.body (box (rgb shade (shade - 60) (shade - 90)) brick_w brick_h brick_d)
  |> Physics3d.at
       ((float_of_int i -. (float_of_int (n - 1) /. 2.)) *. brick_w)
       ((float_of_int row *. (brick_h +. 0.002)) +. (brick_h /. 2.))
       0.
  |> Physics3d.heavy 2.
  |> Physics3d.rough 0.9
  |> Physics3d.bouncy 0.

let dominoes : Physics3d.body list =
  List.init 8 (fun i ->
      Physics3d.body (box (rgb 235 235 240) 0.06 0.3 0.15)
      |> Physics3d.at (-1.2 +. (float_of_int i *. 0.19)) 0.15 1.4
      |> Physics3d.heavy 0.4
      |> Physics3d.rough 0.5
      |> Physics3d.bouncy 0.)

let wall : Physics3d.body list =
  List.concat (List.init rows (fun row -> List.init (bricks_in row) (fun i -> brick i row)))

let thrown () : Physics3d.body =
  Physics3d.body (sphere (rgb 60 70 90) 0.2)
  |> Physics3d.ball
  |> Physics3d.at 0.2 0.7 5.5
  |> Physics3d.moving 0. 0.6 (-9.)
  |> Physics3d.heavy 6.
  |> Physics3d.rough 0.4

type model = {
  world : Physics3d.world;
  solver : bool;
  iterations : int;
  warm : bool;
  thrown_yet : bool;
  keys_down : string list;
}

let fresh (solver : bool) (iterations : int) (warm : bool) : model =
  { world = Physics3d.world ((floor_body :: wall) @ dominoes); solver; iterations; warm; thrown_yet = false;
    keys_down = [] }

let initial_model = fresh true 10 true

(* one step of the two engines, for the same bodies *)
let step (m : model) : Physics3d.world =
  if m.solver then Physics3d.simulate ~gravity:9.8 ~iterations:m.iterations ~warm_starting:m.warm m.world
  else
    let bodies =
      m.world.Physics3d.bodies
      |> List.map (fun (b : Physics3d.body) ->
             if Float.is_finite b.Physics3d.mass then b |> Physics3d.fall 9.8 |> Physics3d.step else b)
      |> Physics3d.bounce_all
    in
    { m.world with Physics3d.bodies; asleep = List.map (fun _ -> false) bodies; solved = 0 }

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  let down key = Set_.mem key k.keys in
  let pressed key = down key && not (List.mem key m.keys_down) in
  let keys_down = List.filter down [ "s"; "i"; "w"; "space" ] @ (if k.kspace then [ "space" ] else []) in
  let m = { m with keys_down } in
  if pressed "s" then { (fresh (not m.solver) m.iterations m.warm) with keys_down }
  else if pressed "i" then
    { (fresh m.solver (match m.iterations with 1 -> 4 | 4 -> 10 | 10 -> 20 | _ -> 1) m.warm) with keys_down }
  else if pressed "w" then { (fresh m.solver m.iterations (not m.warm)) with keys_down }
  else if k.kspace && not m.thrown_yet then
    { m with
      world =
        { m.world with
          Physics3d.bodies = m.world.Physics3d.bodies @ [ thrown () ];
          still = m.world.Physics3d.still @ [ 0 ];
          asleep = m.world.Physics3d.asleep @ [ false ] };
      thrown_yet = true }
  else if k.kspace && m.thrown_yet && m.world.Physics3d.solved >= 0 && down "space" && List.length m.keys_down > 1 then m
  else { m with world = step m }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text color size str = words color str |> scale size
let cam = Camera3d.from_far ~fov:40. ~offset:(2.6, 2.4, 6.) (0., 0.5, 0.3)

(* A body that is still awake wears a small bright cube: the software
 * backend has no alpha, so drawing the sleepers pale is not an option,
 * and the interesting state is the awake one anyway -- the markers go
 * out one by one as the wall settles. *)
let awake_marker (b : Physics3d.body) : shape3d =
  cube (rgb 250 210 60) 0.05 |> move3d b.Physics3d.x (b.Physics3d.y +. 0.2) b.Physics3d.z

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let sleeping = m.world.Physics3d.asleep in
  let bodies = List.map Physics3d.draw m.world.Physics3d.bodies in
  let markers =
    List.concat
      (List.mapi
         (fun i (b : Physics3d.body) ->
           if Float.is_finite b.Physics3d.mass && not (List.nth sleeping i) then [ awake_marker b ] else [])
         m.world.Physics3d.bodies)
  in
  let asleep_count = List.length (List.filter Fun.id sleeping) in
  ( cam,
    bodies @ markers
    @ List.map hud
        [ text black 2.2
            (if m.solver then Printf.sprintf "the solver: %d iterations, warm starting %s" m.iterations (if m.warm then "on" else "off")
             else "no solver: every pair answered once, and the wall comes apart")
          |> move_y (screen.top -. 45.);
          text (if m.solver then rgb 30 110 60 else rgb 175 80 50) 2.
            (Printf.sprintf "%d contact points solved this step; %d asleep, the rest still marked"
               m.world.Physics3d.solved asleep_count)
          |> move_y (screen.top -. 80.);
          text darkGray 2. "space: throw a ball    s: the solver    i: iterations    w: warm starting"
          |> move_y (screen.bottom +. 20.) ] )

let app = game3d view update initial_model

let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
