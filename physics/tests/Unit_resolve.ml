(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/2d/Resolve: the worked example, and the laws: momentum always
 * conserved, kinetic energy with a restitution of 1 *)

let t = Testo.create
let vec = Alcotest.(pair (float 1e-9) (float 1e-9))

(* the first ball at 2 into the second, at rest, touching along x *)
let head_on = (Body.make ~vel:(2., 0.) (0., 0.), Body.make (1., 0.))
let along_x : Contact.t = { normal = (1., 0.); depth = 0.; point = (0.5, 0.) }

let test_worked_example () =
  [ (1., 2., (0., 0.), (2., 0.), 2.); (0., 1., (1., 0.), (1., 0.), 1.); (0.5, 1.5, (0.5, 0.), (1.5, 0.), 1.25) ]
  |> List.iter (fun (e, j, va, vb, energy) ->
         let (a, b) = head_on in
         let name = Printf.sprintf "e = %g" e in
         Alcotest.(check (float 1e-9)) (name ^ ": j") j (Resolve.impulse ~restitution:e a b along_x.normal);
         let (a, b) = Resolve.bounce ~restitution:e ~friction:0. head_on along_x in
         Alcotest.check vec (name ^ ": the first's velocity") va a.vel;
         Alcotest.check vec (name ^ ": the second's") vb b.vel;
         Alcotest.check vec (name ^ ": the momentum, 2 as before") (2., 0.)
           (Vec2.add (Energy.momentum a) (Energy.momentum b));
         Alcotest.(check (float 1e-9)) (name ^ ": the kinetic energy") energy (Energy.kinetic a +. Energy.kinetic b))

let test_separating () =
  let (a, b) = head_on in
  let away = ({ a with vel = (-1., 0.) }, b) in
  Alcotest.(check (float 0.)) "moving apart: no impulse" 0.
    (Resolve.impulse ~restitution:1. (fst away) (snd away) along_x.normal)

(* a wall: an infinite mass, 1/m = 0 *)
let test_immovable () =
  let ball = Body.make ~vel:(3., -4.) (0., 0.) and wall = Body.make ~mass:infinity (0., -1.) in
  let down : Contact.t = { normal = (0., -1.); depth = 0.2; point = (0., -0.5) } in
  let (ball', wall') = Resolve.resolve ~restitution:1. ~friction:0. (ball, wall) down in
  Alcotest.check vec "the ball bounces off, like a mirror" (3., 4.) ball'.vel;
  Alcotest.check vec "the wall doesn't move" (0., 0.) wall'.vel;
  Alcotest.check vec "the ball takes all the separation" (0., 0.2) ball'.pos;
  Alcotest.check vec "the wall none" (0., -1.) wall'.pos

(* a ball hitting the floor while sliding right: friction slows the
 * sliding by at most mu j *)
let test_friction () =
  let floor = Body.make ~mass:infinity (0., -1.) in
  let down : Contact.t = { normal = (0., -1.); depth = 0.; point = (0., -0.5) } in
  (* restitution 0: j = 4 (the vertical speed, stopped) *)
  let (ball, _) = Resolve.bounce ~restitution:0. ~friction:0.25 (Body.make ~vel:(3., -4.) (0., 0.), floor) down in
  Alcotest.check vec "mu j = 1 taken off the sliding" (2., 0.) ball.vel;
  let (ball, _) = Resolve.bounce ~restitution:0. ~friction:1. (Body.make ~vel:(3., -4.) (0., 0.), floor) down in
  Alcotest.check vec "mu j = 4 > 3: the sliding stops, not reversed" (0., 0.) ball.vel;
  (* a moving floor (a paddle) drags the ball along *)
  let paddle = { floor with vel = (5., 0.) } in
  let (ball, _) = Resolve.bounce ~restitution:0. ~friction:1. (Body.make ~vel:(3., -4.) (0., 0.), paddle) down in
  Alcotest.check vec "carried by the paddle, up to its speed" (5., 0.) ball.vel

let random_body (st : Random.State.t) : Body.t =
  let f lo hi = lo +. Random.State.float st (hi -. lo) in
  Body.make ~vel:(f (-10.) 10., f (-10.) 10.) ~mass:(f 0.1 10.) (f (-5.) 5., f (-5.) 5.)

let test_conservation () =
  let st = Random.State.make [| 11 |] in
  for _ = 1 to 1000 do
    let a = random_body st and b = random_body st in
    let angle = Random.State.float st (2. *. Float.pi) in
    let c : Contact.t = { normal = (cos angle, sin angle); depth = 0.; point = (0., 0.) } in
    let e = Random.State.float st 1. and mu = Random.State.float st 1. in
    let before = Vec2.add (Energy.momentum a) (Energy.momentum b) in
    let energy = Energy.kinetic a +. Energy.kinetic b in
    let (a', b') = Resolve.bounce ~restitution:e ~friction:mu (a, b) c in
    let (px, py) = Vec2.sub (Vec2.add (Energy.momentum a') (Energy.momentum b')) before in
    if Float.abs px > 1e-9 || Float.abs py > 1e-9 then Alcotest.failf "momentum changed by (%g, %g)" px py;
    let energy' = Energy.kinetic a' +. Energy.kinetic b' in
    if energy' > energy +. 1e-9 then Alcotest.failf "energy created: %g -> %g" energy energy';
    let (a', b') = Resolve.bounce ~restitution:1. ~friction:0. (a, b) c in
    let energy' = Energy.kinetic a' +. Energy.kinetic b' in
    if Float.abs (energy' -. energy) > 1e-9 then Alcotest.failf "e = 1, energy changed: %g -> %g" energy energy'
  done

let tests =
  Testo.categorize "Resolve"
    [
      t "the worked example: e = 1, 0, 0.5" test_worked_example;
      t "separating: nothing to do" test_separating;
      t "an immovable wall" test_immovable;
      t "friction, and a moving paddle" test_friction;
      t "momentum conserved, energy never created, 1000 random collisions" test_conservation;
    ]
