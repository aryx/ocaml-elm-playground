(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/2d/Collide.manifold and Solver: the box on the floor of
 * Solver.mli, warm starting, and the conservation laws *)

let t = Testo.create
let vec = Alcotest.(pair (float 1e-9) (float 1e-9))

let test_manifold () =
  let floor = Shape.Polygon_at [ (-10., -2.); (10., -2.); (10., 0.); (-10., 0.) ] in
  (* a 2 x 1 box, 0.1 into the floor *)
  let box = Shape.place (0., 0.4) (Shape.Box (2., 1.)) in
  (match Collide.manifold box floor with
  | [ c1; c2 ] ->
      Alcotest.check vec "down, from the box to the floor" (0., -1.) c1.normal;
      Alcotest.(check (float 1e-9)) "each corner 0.1 deep" 0.1 c1.depth;
      Alcotest.(check (float 1e-9)) "... both" 0.1 c2.depth;
      Alcotest.(check (float 1e-9)) "the two bottom corners" 2. (Float.abs (fst c1.point -. fst c2.point))
  | l -> Alcotest.failf "lying flat: 2 points, not %d" (List.length l));
  (* tilted, on one corner *)
  let tilted = Shape.place ~angle:0.3 (0., 0.7) (Shape.Box (2., 1.)) in
  Alcotest.(check int) "on a corner: 1 point" 1 (List.length (Collide.manifold tilted floor))

(* Solver.mli's example: a box of mass 1 (2 x 1, I = 5/12) on the floor,
 * g = 10, dt = 0.1 *)
let box = Body.make ~inertia:(5. /. 12.) (0., 0.5)
let floor = Body.make ~mass:infinity (0., -1.)

let on_floor : Solver.pair =
  let c x : Contact.t = { normal = (0., -1.); depth = 0.; point = (x, 0.) } in
  { a = 0; b = 1; contacts = [ c (-1.); c 1. ]; restitution = 0.; friction = 0.5 }

let options = { Solver.default with slop = 0.01; bounce_threshold = 1.; matching = 0.1 }

(* [n] steps of gravity then the solver, the box not moving (it
 * shouldn't) *)
let settle (o : Solver.options) (n : int) : Body.t array * Solver.memory =
  let rec go i (bodies, memory) =
    if i = 0 then (bodies, memory)
    else
      let bodies = Array.map (fun (b : Body.t) -> if b.mass = infinity then b else { b with vel = Vec2.add b.vel (0., -1.) }) bodies in
      go (i - 1) (Solver.solve o ~dt:0.1 bodies [ on_floor ] memory)
  in
  go n ([| box; floor |], Solver.nothing)

let test_resting_box () =
  let (bodies, memory) = settle options 20 in
  Alcotest.check vec "the box still" (0., 0.) bodies.(0).vel;
  Alcotest.(check (float 1e-9)) "not turning" 0. bodies.(0).spin;
  Alcotest.(check (list (float 1e-6))) "m g dt = 1, half at each corner" [ 0.5; 0.5 ] (Solver.impulses memory (0, 1))

let test_warm_starting () =
  (* one iteration per step: warm started, the impulses of the step
   * before are already right; cold, one iteration isn't enough *)
  let one = { options with iterations = 1 } in
  let (warm, _) = settle one 20 and (cold, _) = settle { one with warm_starting = false } 20 in
  Alcotest.(check (float 1e-6)) "warm: the box still" 0. (Vec2.length warm.(0).vel);
  if Vec2.length cold.(0).vel < 1e-3 && Float.abs cold.(0).spin < 1e-3 then
    Alcotest.fail "cold, one iteration: should still be off"

let random_body (st : Random.State.t) : Body.t =
  let f lo hi = lo +. Random.State.float st (hi -. lo) in
  Body.make ~vel:(f (-10.) 10., f (-10.) 10.) ~mass:(f 0.1 10.) ~spin:(f (-3.) 3.) ~inertia:(f 0.1 10.) (f (-5.) 5., f (-5.) 5.)

(* whatever the impulses, equal and opposite: momentum and angular
 * momentum kept *)
let test_conservation () =
  let st = Random.State.make [| 17 |] in
  for _ = 1 to 300 do
    let a = random_body st and b = random_body st in
    let angle = Random.State.float st (2. *. Float.pi) in
    let contact () : Contact.t =
      { normal = (cos angle, sin angle); depth = Random.State.float st 2.; point = (Random.State.float st 4. -. 2., Random.State.float st 4. -. 2.) }
    in
    let pair : Solver.pair = { a = 0; b = 1; contacts = [ contact (); contact () ]; restitution = 0.5; friction = 0.5 } in
    let (after, _) = Solver.solve Solver.default ~dt:(1. /. 60.) [| a; b |] [ pair ] Solver.nothing in
    let p (bs : Body.t array) = Vec2.add (Energy.momentum bs.(0)) (Energy.momentum bs.(1)) in
    let l (bs : Body.t array) =
      Energy.angular_momentum ~around:(0., 0.) bs.(0) +. Energy.angular_momentum ~around:(0., 0.) bs.(1)
    in
    let (dx, dy) = Vec2.sub (p after) (p [| a; b |]) in
    if Float.abs dx > 1e-6 || Float.abs dy > 1e-6 then Alcotest.failf "momentum changed by (%g, %g)" dx dy;
    let dl = l after -. l [| a; b |] in
    if Float.abs dl > 1e-6 then Alcotest.failf "angular momentum changed by %g" dl
  done

let tests =
  Testo.categorize "Solver"
    [
      t "manifold: two points lying flat, one on a corner" test_manifold;
      t "the resting box: 0.5 at each corner" test_resting_box;
      t "warm starting: one iteration is enough" test_warm_starting;
      t "momentum and angular momentum kept" test_conservation;
    ]
