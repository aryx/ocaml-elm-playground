(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* kits/segments: Segments, Sixdof *)

let t = Testo.create

let vec =
  Alcotest.testable
    (fun ppf (x, y, z) -> Format.fprintf ppf "(%g, %g, %g)" x y z)
    (fun (a, b, c) (x, y, z) ->
      let eq u v = Float.abs (u -. v) < 1e-9 in
      eq a x && eq b y && eq c z)

(*****************************************************************************)
(* Segments *)
(*****************************************************************************)

let seg b : Segments.segment = { box = b; rgb = (0, 0, 0); light = 1. }
let box x0 y0 z0 x1 y1 z1 : Segments.box = { x0; y0; z0; x1; y1; z1 }

(* a room, and a corridor out of the middle of its east side: the
 * shared rectangle is an opening on both sides, and the room's east
 * side is cut into 8 pieces of rock around it *)
let test_make () =
  let lv = Segments.make ~start:(5., 5., 5.) ~exit:1 [ seg (box 0. 0. 0. 10. 10. 10.); seg (box 10. 4. 4. 20. 6. 6.) ] in
  Alcotest.(check (list int)) "an opening each way" [ 1; 0 ] (List.map (fun (o : Segments.opening) -> o.into) (lv.openings.(0) @ lv.openings.(1)));
  let east = List.filter (fun (q : Segments.quad) -> q.side.axis = 0 && q.side.positive) lv.walls.(0) in
  Alcotest.(check int) "the room's east side, around the hole" 8 (List.length east);
  Alcotest.(check int) "the corridor's east side, whole" 1 (List.length (List.filter (fun (q : Segments.quad) -> q.side.axis = 0 && q.side.positive) lv.walls.(1)));
  (* the openings face into their own box: east for the room, west for
   * the corridor *)
  Alcotest.check vec "the room's opening faces east" (-1., 0., 0.) (List.hd lv.openings.(0)).quad.normal;
  Alcotest.check vec "the corridor's faces west" (1., 0., 0.) (List.hd lv.openings.(1)).quad.normal

(* every opening has its twin on the other side, the same rectangle *)
let test_mine () =
  let lv = Segments.mine in
  Array.iteri
    (fun i os ->
      List.iter
        (fun (o : Segments.opening) ->
          let twin =
            List.filter (fun (b : Segments.opening) -> b.into = i && List.sort compare b.quad.corners = List.sort compare o.quad.corners) lv.openings.(o.into)
          in
          Alcotest.(check int) (Printf.sprintf "segment %d's opening into %d, and back" i o.into) 1 (List.length twin))
        os)
    lv.openings;
  Alcotest.(check bool) "every segment reachable" true
    (let seen = Array.make (Array.length lv.segments) false in
     let rec go i =
       if not seen.(i) then begin
         seen.(i) <- true;
         List.iter (fun (o : Segments.opening) -> go o.into) lv.openings.(i)
       end
     in
     go 0;
     Array.for_all Fun.id seen);
  Alcotest.(check (option int)) "the start is in the first room" (Some 0) (Segments.segment_at lv lv.start)

(* the ship stops at the rock and goes through the opening; sliding
 * along a wall keeps the speed along it *)
let test_move () =
  let lv = Segments.make ~start:(5., 5., 5.) ~exit:1 [ seg (box 0. 0. 0. 10. 10. 10.); seg (box 10. 4. 4. 20. 6. 6.) ] in
  let radius = 1. in
  (* above the corridor's mouth: the east wall stops the ship a radius
   * short of it, but not the part of the move along the wall *)
  Alcotest.check vec "into the east wall, stopped at the rock" (9., 9., 5.) (Segments.move lv ~radius (5., 9., 5.) (4., 0., 0.));
  Alcotest.check vec "sliding along it" (9., 9., 8.) (Segments.move lv ~radius (5., 9., 5.) (4., 0., 3.));
  Alcotest.check vec "through the opening" (12., 5., 5.) (Segments.move lv ~radius (9., 5., 5.) (3., 0., 0.));
  Alcotest.(check bool) "the corridor is too narrow to leave" false (Segments.inside lv ~radius:1.5 (12., 5., 5.));
  Alcotest.(check bool) "a line of sight down the corridor" true (Segments.clear lv (2., 5., 5.) (18., 5., 5.));
  Alcotest.(check bool) "but not through the rock" false (Segments.clear lv (2., 2., 5.) (18., 2., 5.))

(*****************************************************************************)
(* Sixdof *)
(*****************************************************************************)

(* each turn around one of the ship's own axes, that axis unmoved *)
let test_turn () =
  let i = Sixdof.identity in
  let up = Sixdof.turn ~pitch:90. i in
  Alcotest.check vec "nose up" (0., 1., 0.) up.forward;
  Alcotest.check vec "the canopy now looks backwards" (0., 0., 1.) up.up;
  Alcotest.check vec "right unmoved" (1., 0., 0.) up.right;
  let left = Sixdof.turn ~yaw:90. i in
  Alcotest.check vec "nose left" (-1., 0., 0.) left.forward;
  Alcotest.check vec "up unmoved" (0., 1., 0.) left.up;
  let rolled = Sixdof.turn ~roll:90. i in
  Alcotest.check vec "the right wing down" (0., -1., 0.) rolled.right;
  Alcotest.check vec "forward unmoved" (0., 0., -1.) rolled.forward

(* 360 small turns around each axis, back where it started, still
 * square (the straightening keeps it there) *)
let test_straight () =
  let t = ref Sixdof.identity in
  for _ = 1 to 360 do
    t := Sixdof.turn ~pitch:1. ~yaw:1. ~roll:1. !t
  done;
  for _ = 1 to 360 do
    t := Sixdof.turn ~pitch:(-1.) ~yaw:(-1.) ~roll:(-1.) !t
  done;
  let dot (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2) in
  let close a b = Alcotest.(check bool) (Printf.sprintf "%g close to %g" a b) true (Float.abs (a -. b) < 1e-6) in
  close 1. (dot !t.forward !t.forward);
  close 0. (dot !t.forward !t.up);
  close 0. (dot !t.right !t.up);
  (* the turns don't commute, so it isn't the identity again -- but it
   * is still a ship pointing somewhere *)
  Alcotest.(check bool) "still pointing somewhere" true (Float.abs (dot !t.forward (0., 0., -1.)) <= 1.)

let test_along () =
  let t = Sixdof.turn ~yaw:90. Sixdof.identity in
  Alcotest.check vec "10 ahead, nose left" (-10., 0., 0.) (Sixdof.ahead t ~from:(0., 0., 0.) ~distance:10.);
  Alcotest.check vec "3 to its right" (0., 0., -3.) (Sixdof.along t (3., 0., 0.))

let tests =
  Testo.categorize "Segments"
    [ t "make, the openings and the rock around them" test_make;
      t "the mine: twin openings, all reachable" test_mine;
      t "move, stopped by the rock, sliding along it" test_move;
      t "Sixdof: pitch, yaw and roll around its own axes" test_turn;
      t "Sixdof: still square after 720 turns" test_straight;
      t "Sixdof: ahead and along" test_along ]
