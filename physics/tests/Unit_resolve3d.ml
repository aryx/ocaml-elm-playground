(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/3d/Resolve3d: the worked examples the 2D engine already had,
 * which must come out the same here, and the two conservation laws,
 * which hold by construction and so should hold to the last bits *)

let t = Testo.create
let close = Alcotest.(check (float 1e-9))

let check_vec ?(tol = 1e-9) name (ax, ay, az) (bx, by, bz) =
  Alcotest.(check (float tol)) (name ^ " x") ax bx;
  Alcotest.(check (float tol)) (name ^ " y") ay by;
  Alcotest.(check (float tol)) (name ^ " z") az bz

let contact ~normal ~point : Contact3d.t = Contact3d.make ~normal ~depth:0.01 ~point

(* physics/2d/Resolve.mli's first example, one dimension up: two balls
 * of mass 1, the first at 2 m/s into the second, at rest *)
let head_on () =
  let a = Body3d.make ~vel:(2., 0., 0.) (-1., 0., 0.) in
  let b = Body3d.make (1., 0., 0.) in
  let c = contact ~normal:(1., 0., 0.) ~point:(0., 0., 0.) in
  let after e =
    let a', b' = Resolve3d.bounce ~restitution:e ~friction:0. (a, b) c in
    let (vax, _, _) = a'.Body3d.vel and (vbx, _, _) = b'.Body3d.vel in
    (vax, vbx)
  in
  let va, vb = after 1. in
  close "e = 1: it stops" 0. va;
  close "and the other leaves at 2 (Newton's cradle)" 2. vb;
  let va, vb = after 0. in
  close "e = 0: they go on together, at 1" 1. va;
  close "" 1. vb;
  let va, vb = after 0.5 in
  close "e = 0.5: 0.5" 0.5 va;
  close "and 1.5" 1.5 vb;
  (* the momentum is 2 every time; the energy, 2 before, is only kept
   * by e = 1 *)
  let energy e =
    let a', b' = Resolve3d.bounce ~restitution:e ~friction:0. (a, b) c in
    Energy3d.kinetic a' +. Energy3d.kinetic b'
  in
  close "e = 1 keeps the energy" 2. (energy 1.);
  close "e = 0 loses half of it" 1. (energy 0.);
  close "e = 0.5 keeps 1.25" 1.25 (energy 0.5)

(* and Resolve.mli's second: a ball into the *end* of a rod, which
 * makes it spin. The 2D answer was 0.4 and 1.2 rad/s; the tensor
 * version has to agree. *)
let off_centre () =
  let ball = Body3d.make ~vel:(0., 1., 0.) (0., -1., 0.) in
  (* a rod of mass 1 and length 2 along x: I = m L^2 / 12 = 1/3 about
   * the two axes across it, and a token amount along its own length
   * (a real rod has almost none, and exactly none would make its
   * inverse infinite) *)
  let rod_tensor = Mat3.diagonal 0.001 (1. /. 3.) (1. /. 3.) in
  let rod = Body3d.make ~mass:1. ~inertia:rod_tensor (-1., 0., 0.) in
  let c = contact ~normal:(0., 1., 0.) ~point:(0., 0., 0.) in
  let ball', rod' = Resolve3d.bounce ~restitution:1. ~friction:0. (ball, rod) c in
  let _, ball_vy, _ = ball'.Body3d.vel and _, rod_vy, _ = rod'.Body3d.vel in
  let _, _, rod_spin = rod'.Body3d.spin in
  Alcotest.(check (float 1e-6)) "the rod's end takes 0.4 of the speed" 0.4 rod_vy;
  Alcotest.(check (float 1e-6)) "the ball keeps 0.6" 0.6 ball_vy;
  Alcotest.(check (float 1e-6)) "and the rod spins at 1.2 rad/s" 1.2 rod_spin;
  (* hit in the middle instead, and it does not spin at all: it takes
   * the whole speed, like a ball *)
  let mid = Body3d.make ~mass:1. ~inertia:rod_tensor (0., 0., 0.) in
  let ball', rod' = Resolve3d.bounce ~restitution:1. ~friction:0. (ball, mid) c in
  let _, ball_vy, _ = ball'.Body3d.vel and _, rod_vy, _ = rod'.Body3d.vel in
  Alcotest.(check (float 1e-6)) "hit in the middle: the rod takes all of it" 1. rod_vy;
  Alcotest.(check (float 1e-6)) "and the ball stops" 0. ball_vy;
  check_vec ~tol:1e-9 "with no spin at all" (0., 0., 0.) rod'.Body3d.spin

(* an infinite mass takes none of the impulse and does not move: no
 * special case anywhere, just 1/m = 0 *)
let against_a_wall () =
  let ball = Body3d.make ~vel:(0., -3., 0.) ~inertia:(Body3d.solid_sphere ~mass:1. ~radius:0.5) (0., 0.5, 0.) in
  let floor = Body3d.make ~mass:infinity (0., -1., 0.) in
  let c = contact ~normal:(0., -1., 0.) ~point:(0., 0., 0.) in
  (* the normal points from the ball towards the floor *)
  let ball', floor' = Resolve3d.bounce ~restitution:1. ~friction:0. (ball, floor) c in
  check_vec ~tol:1e-9 "it bounces back as fast" (0., 3., 0.) ball'.Body3d.vel;
  check_vec ~tol:1e-9 "and the floor has not moved" (0., 0., 0.) floor'.Body3d.vel;
  let ball', _ = Resolve3d.bounce ~restitution:0. ~friction:0. (ball, floor) c in
  check_vec ~tol:1e-9 "e = 0: it stops dead" (0., 0., 0.) ball'.Body3d.vel;
  (* and one already moving away is left alone *)
  let leaving = { ball with Body3d.vel = (0., 2., 0.) } in
  close "nothing to do when they are separating" 0. (Resolve3d.impulse ~restitution:1. leaving floor c)

(* friction at the bottom of a sliding ball spins it: the reason a
 * bowling ball stops skidding and starts rolling *)
let friction_spins_it () =
  let ball =
    Body3d.make ~vel:(4., -1., 0.) ~inertia:(Body3d.solid_sphere ~mass:1. ~radius:0.5) (0., 0.5, 0.)
  in
  let floor = Body3d.make ~mass:infinity (0., -1., 0.) in
  let c = contact ~normal:(0., -1., 0.) ~point:(0., 0., 0.) in
  let smooth, _ = Resolve3d.bounce ~restitution:0.5 ~friction:0. (ball, floor) c in
  check_vec ~tol:1e-9 "with no friction it keeps sliding as fast" (4., 0.5, 0.) smooth.Body3d.vel;
  check_vec ~tol:1e-9 "and does not turn" (0., 0., 0.) smooth.Body3d.spin;
  let rough, _ = Resolve3d.bounce ~restitution:0.5 ~friction:0.4 (ball, floor) c in
  let vx, _, _ = rough.Body3d.vel and _, _, sz = rough.Body3d.spin in
  Alcotest.(check bool) "with friction it is slowed" true (vx < 4. && vx > 3.);
  Alcotest.(check bool) "and it is now turning forwards" true (sz < -0.1);
  (* the tangential impulse never exceeds mu times the normal one *)
  let j = Resolve3d.impulse ~restitution:0.5 ball floor c in
  let lost = 4. -. vx in
  Alcotest.(check bool) "Coulomb's clamp holds" true (lost <= 0.4 *. j /. 1. +. 1e-9)

(* the positional correction: bodies found overlapping are pushed
 * apart, the lighter one more, and an immovable one not at all *)
let separating () =
  let a = Body3d.make ~mass:1. (0., 0., 0.) and b = Body3d.make ~mass:3. (1., 0., 0.) in
  let c = Contact3d.make ~normal:(1., 0., 0.) ~depth:0.4 ~point:(0.5, 0., 0.) in
  let a', b' = Resolve3d.separate (a, b) c in
  let ax, _, _ = a'.Body3d.pos and bx, _, _ = b'.Body3d.pos in
  close "the light one moves 3/4 of the way" (-0.3) ax;
  close "the heavy one a quarter" 1.1 bx;
  close "and the gap is closed exactly once" 0.4 (bx -. ax -. 1.);
  let wall = Body3d.make ~mass:infinity (1., 0., 0.) in
  let a', wall' = Resolve3d.separate (a, wall) c in
  let ax, _, _ = a'.Body3d.pos and wx, _, _ = wall'.Body3d.pos in
  close "against a wall, the body takes all of it" (-0.4) ax;
  close "and the wall stays" 1. wx

(*****************************************************************************)
(* The laws *)
(*****************************************************************************)

(* A thousand random collisions: whatever the masses, tensors, spins,
 * restitution and friction, the pair's momentum and angular momentum
 * come out unchanged -- they are conserved by the shape of the
 * formula, not by its numbers -- and the energy never grows. *)
let the_laws () =
  Random.init 21;
  let rnd a b = a +. Random.float (b -. a) in
  let v () = (rnd (-3.) 3., rnd (-3.) 3., rnd (-3.) 3.) in
  let worst_p = ref 0. and worst_l = ref 0. and worst_gain = ref 0. and elastic_worst = ref 0. in
  for i = 1 to 1000 do
    let body () =
      let mass = rnd 0.2 5. in
      Body3d.make ~vel:(v ()) ~spin:(v ())
        ~orientation:(Quat.of_axis_angle (v ()) (Random.float 6.28))
        ~mass
        ~inertia:(Body3d.box ~mass (rnd 0.2 2., rnd 0.2 2., rnd 0.2 2.))
        (v ())
    in
    let a = body () and b = body () in
    let c = Contact3d.make ~normal:(v ()) ~depth:0.01 ~point:(v ()) in
    let elastic = i mod 3 = 0 in
    let restitution = if elastic then 1. else Random.float 1. in
    let friction = if elastic then 0. else Random.float 1. in
    let momentum (x, y) = Vec3.add (Energy3d.momentum x) (Energy3d.momentum y) in
    let angular (x, y) =
      Vec3.add (Energy3d.angular_momentum ~around:(0., 0., 0.) x) (Energy3d.angular_momentum ~around:(0., 0., 0.) y)
    in
    let energy (x, y) = Energy3d.kinetic x +. Energy3d.kinetic y in
    let before = (a, b) in
    let after = Resolve3d.bounce ~restitution ~friction before c in
    let scale = Float.max 1. (Vec3.length (momentum before)) in
    worst_p := Float.max !worst_p (Vec3.length (Vec3.sub (momentum after) (momentum before)) /. scale);
    let lscale = Float.max 1. (Vec3.length (angular before)) in
    worst_l := Float.max !worst_l (Vec3.length (Vec3.sub (angular after) (angular before)) /. lscale);
    let gain = (energy after -. energy before) /. Float.max 1. (energy before) in
    worst_gain := Float.max !worst_gain gain;
    if elastic then elastic_worst := Float.max !elastic_worst (Float.abs gain)
  done;
  Alcotest.(check bool) "momentum: conserved to the last bits" true (!worst_p < 1e-12);
  Alcotest.(check bool) "angular momentum too, about any point" true (!worst_l < 1e-12);
  Alcotest.(check bool) "energy is never created" true (!worst_gain < 1e-12);
  Alcotest.(check bool) "and with e = 1 and no friction, never lost either" true (!elastic_worst < 1e-12)

let tests =
  [ t "Resolve3d, two balls head on" head_on;
    t "Resolve3d, a ball into the end of a rod" off_centre;
    t "Resolve3d, against an immovable wall" against_a_wall;
    t "Resolve3d, friction spins a sliding ball" friction_spins_it;
    t "Resolve3d, pushing them apart" separating;
    t "Resolve3d, a thousand collisions keep the laws" the_laws ]
