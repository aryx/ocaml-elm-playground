(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/3d/Solver3d, through playground3d/Physics3d's world: the
 * difference between bouncing things off each other and stacking them.
 * One pass per pair leaves a crate shivering on the floor for ever;
 * the solver puts it down and, a second later, stops simulating it at
 * all. *)

open Playground3d

let t = Testo.create

let floor () =
  Physics3d.body (box Playground.gray 20. 1. 20.) |> Physics3d.at 0. (-0.5) 0. |> Physics3d.immovable
  |> Physics3d.rough 0.6

let crate y =
  Physics3d.body (cube Playground.brown 0.5) |> Physics3d.at 0. y 0. |> Physics3d.rough 0.6 |> Physics3d.bouncy 0.

let run ?(steps = 300) ?(sleeping = true) ?(warm_starting = true) ?(iterations = 10) w =
  let w = ref w in
  for _ = 1 to steps do
    w := Physics3d.simulate ~gravity:9.8 ~sleeping ~warm_starting ~iterations !w
  done;
  !w

let nth_body (w : Physics3d.world) i = List.nth w.Physics3d.bodies i

(* A crate dropped on the floor comes to rest 5 mm into it -- the slop
 * the solver tolerates so that resting bodies keep touching -- and
 * stops moving entirely. *)
let a_crate_comes_to_rest () =
  let w = run (Physics3d.world [ floor (); crate 0.3 ]) in
  let b = nth_body w 1 in
  Alcotest.(check (float 1e-4)) "resting on the floor, one slop deep" 0.245 b.Physics3d.y;
  Alcotest.(check bool) "and not moving at all" true (Physics3d.speed b < 1e-6);
  Alcotest.(check bool) "asleep, so it is not even simulated" true (List.nth w.Physics3d.asleep 1);
  Alcotest.(check int) "and its contacts cost nothing" 0 w.Physics3d.solved

(* the same crate, answered one pair at a time as Physics3d.bounce
 * does: it stays up, and it never stops twitching *)
let one_pass_shivers () =
  let bodies = ref [ floor (); crate 0.3 ] in
  for _ = 1 to 300 do
    bodies :=
      !bodies
      |> List.map (fun (b : Physics3d.body) ->
             if Float.is_finite b.Physics3d.mass then b |> Physics3d.fall 9.8 |> Physics3d.step else b)
      |> Physics3d.bounce_all
  done;
  let b = List.nth !bodies 1 in
  Alcotest.(check bool) "it is still about where it should be" true (Float.abs (b.Physics3d.y -. 0.25) < 0.02);
  Alcotest.(check bool) "but it is still moving, a few centimetres a second" true (Physics3d.speed b > 0.01);
  (* which is the whole argument for the solver *)
  let solved = nth_body (run (Physics3d.world [ floor (); crate 0.3 ])) 1 in
  Alcotest.(check bool) "against a thousand times less, solved" true (Physics3d.speed solved *. 1000. < Physics3d.speed b)

(* Five crates, stacked. They stay stacked, they stay put, and they go
 * to sleep together. *)
let a_tower_stands () =
  let tower = Physics3d.world (floor () :: List.init 5 (fun i -> crate (0.25 +. (float_of_int i *. 0.5)))) in
  let w = run ~steps:600 tower in
  List.iteri
    (fun i (b : Physics3d.body) ->
      if i > 0 then begin
        let wanted = 0.25 +. (float_of_int (i - 1) *. 0.5) in
        (* each contact below it may keep the solver's slop, 5 mm, of
         * overlap: crate 5 rests up to 2.5 cm low, and does (2.1) *)
        Alcotest.(check bool)
          (Printf.sprintf "crate %d is within the slop of the contacts under it (%.3f)" i b.Physics3d.y)
          true
          (Float.abs (b.Physics3d.y -. wanted) < (float_of_int i *. 0.005) +. 0.001);
        Alcotest.(check bool)
          (Printf.sprintf "crate %d has hardly crept sideways (%.4f)" i b.Physics3d.x)
          true
          (Float.abs b.Physics3d.x < 0.01);
        Alcotest.(check bool) (Printf.sprintf "crate %d is asleep" i) true (List.nth w.Physics3d.asleep i)
      end)
    w.Physics3d.bodies;
  Alcotest.(check int) "and the whole tower costs nothing to keep" 0 w.Physics3d.solved

(* Sleeping is by islands: everything that touches sleeps together. A
 * crate sent to sleep on its own under a tower that is still settling
 * is woken a moment later with a jolt -- which is what this checks is
 * *not* happening, by counting how often the bottom crate is woken. *)
let they_sleep_together () =
  let tower = Physics3d.world (floor () :: List.init 5 (fun i -> crate (0.25 +. (float_of_int i *. 0.5)))) in
  let w = ref tower and wakings = ref 0 in
  for _ = 1 to 600 do
    let before = List.nth (!w).Physics3d.still 1 in
    w := Physics3d.simulate ~gravity:9.8 !w;
    if List.nth (!w).Physics3d.still 1 = 0 && before > 0 then incr wakings
  done;
  Alcotest.(check bool)
    (Printf.sprintf "the bottom crate is woken only while the tower lands (%d times)" !wakings)
    true (!wakings <= 4);
  (* and nothing sleeps while the pile is still moving *)
  let early = run ~steps:20 tower in
  Alcotest.(check bool) "nothing sleeps in the first twenty steps" true (not (List.exists Fun.id early.Physics3d.asleep));
  (* something dropped on a sleeping tower wakes it *)
  let settled = run ~steps:600 tower in
  let with_visitor =
    Physics3d.world (settled.Physics3d.bodies @ [ crate 6. |> Physics3d.moving 0. (-8.) 0. ])
  in
  let after = run ~steps:60 { with_visitor with Physics3d.asleep = settled.Physics3d.asleep @ [ false ] } in
  Alcotest.(check bool) "a crate dropped on the tower wakes it up" true
    (not (List.nth after.Physics3d.asleep 5))

(* Warm starting is the difference between a stack that settles in a
 * few steps and one that takes many: the impulses of the previous step
 * are nearly right, so the iterations only fix the difference. *)
let warm_starting_settles_faster () =
  let tower () = Physics3d.world (floor () :: List.init 4 (fun i -> crate (0.25 +. (float_of_int i *. 0.5)))) in
  let sunk (w : Physics3d.world) =
    List.mapi (fun i (b : Physics3d.body) -> if i = 0 then 0. else Float.abs (b.Physics3d.y -. (0.25 +. (float_of_int (i - 1) *. 0.5)))) w.Physics3d.bodies
    |> List.fold_left Float.max 0.
  in
  let warm = sunk (run ~steps:120 ~sleeping:false (tower ())) in
  let cold = sunk (run ~steps:120 ~sleeping:false ~warm_starting:false (tower ())) in
  Alcotest.(check bool) (Printf.sprintf "cold sinks further (%.4f against %.4f)" cold warm) true (cold > warm);
  (* and one iteration is not enough for a tower, where ten are: after
   * two seconds it is still bouncing, its crates at up to 0.6 m/s where
   * the ten-iteration tower's are still. Not measured by how far it
   * sank: a bouncing crate is sometimes *above* where it started (a
   * first version of this test measured the sinking, and passed only
   * while nothing in a world could turn) *)
  let fastest (w : Physics3d.world) = List.fold_left (fun m b -> Float.max m (Physics3d.speed b)) 0. w.Physics3d.bodies in
  let still_ = fastest (run ~steps:120 ~sleeping:false (tower ())) in
  let lazy_ = fastest (run ~steps:120 ~sleeping:false ~iterations:1 (tower ())) in
  Alcotest.(check bool) (Printf.sprintf "one iteration: still moving (%.3f m/s against %.4f)" lazy_ still_) true
    (lazy_ > 100. *. still_)

(* A world turns its bodies, as [Physics3d.step] does: a free box given
 * a quarter turn a second is a quarter turned after a second; and a
 * domino tipped past its edge falls over rather than standing there
 * tipped (a first version of [simulate] never turned anything) *)
let a_world_turns_its_bodies () =
  let spun = Physics3d.body (cube Playground.brown 0.5) |> Physics3d.turning (0., 1., 0.) 90. in
  let w = ref (Physics3d.world [ spun ]) in
  for _ = 1 to 60 do w := Physics3d.simulate !w done;
  let fx, _, fz = Physics3d.forward (List.hd !w.Physics3d.bodies) in
  Alcotest.(check (float 1e-3)) "a quarter turn: -z is now -x" (-1.) fx;
  Alcotest.(check (float 1e-3)) "and no longer -z" 0. fz;
  (* 0.06 wide and 0.3 tall, it tips past its edge at 11 degrees: 20 *)
  let domino =
    Physics3d.body (box Playground.white 0.06 0.3 0.15) |> Physics3d.pointing (0., 0., 1.) (-20.)
    |> Physics3d.at 0. 0.155 0. |> Physics3d.rough 0.6
  in
  let w = run ~steps:120 (Physics3d.world [ floor (); domino ]) in
  let d = List.nth w.Physics3d.bodies 1 in
  let _, uy, _ = Quat.rotate d.Physics3d.orientation (0., 1., 0.) in
  Alcotest.(check bool) (Printf.sprintf "the domino lies down (its up is now %.2f up)" uy) true (uy < 0.5)

let tests =
  [ t "Solver3d, a crate comes to rest and stays there" a_crate_comes_to_rest;
    t "Solver3d, and one answered pair at a time shivers" one_pass_shivers;
    t "Solver3d, a tower of five stands" a_tower_stands;
    t "Solver3d, they sleep in islands, not one by one" they_sleep_together;
    t "Solver3d, warm starting, and how many iterations" warm_starting_settles_faster;
    t "Solver3d, a world turns its bodies: a domino topples" a_world_turns_its_bodies ]
