(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Portal3d: the pair's motion, crossing, and seeing
 * through *)

let t = Testo.create

(* a portal in the floor, and one in a wall 5 m up *)
let floor = { Portal3d.centre = (0., 0., 0.); normal = (0., 1., 0.); up = (0., 0., -1.); width = 2.; height = 2. }
let wall = { Portal3d.centre = (0., 5., -8.); normal = (0., 0., 1.); up = (0., 1., 0.); width = 2.; height = 2. }

let near (what : string) (a : float * float * float) (b : float * float * float) =
  Alcotest.(check bool) what true (Vec3.length (Vec3.sub a b) < 1e-9)

(* Falling at 9 m/s into the floor portal, out of the wall one at 9 m/s
 * straight out of the wall: turned, not scaled *)
let speedy_thing () =
  near "out of the wall, as fast" (0., 0., 9.) (Portal3d.direction ~from:floor ~into:wall (0., -9., 0.));
  near "and back again" (0., -9., 0.) (Portal3d.direction ~from:wall ~into:floor (0., 0., 9.))

(* in front of one, behind the other; and there and back is nowhere *)
let sides () =
  let _, _, z = Portal3d.point ~from:floor ~into:wall (0.3, 0.5, 0.2) in
  Alcotest.(check bool) "half a metre above the floor portal is half a metre behind the wall one" true
    (Float.abs (z -. -8.5) < 1e-9);
  let p = (0.4, 1.3, -0.7) in
  near "there and back" p (Portal3d.point ~from:wall ~into:floor (Portal3d.point ~from:floor ~into:wall p))

(* an orientation goes through as a direction does: a body's axes come
 * out turned exactly as its velocity *)
let orientation () =
  let q = Quat.of_axis_angle (0.3, 0.8, 0.2) 1.1 in
  let q' = Portal3d.orientation ~from:floor ~into:wall q in
  List.iter
    (fun axis ->
      near "an axis, turned as a direction" (Portal3d.direction ~from:floor ~into:wall (Quat.rotate q axis)) (Quat.rotate q' axis))
    [ (1., 0., 0.); (0., 1., 0.); (0., 0., 1.) ]

(* through the rectangle, from its front: crossed; beside it, or from
 * behind: not *)
let crossing () =
  Alcotest.(check bool) "down through it" true (Portal3d.crossed floor ~before:(0.2, 0.1, 0.3) ~after:(0.2, -0.1, 0.3));
  Alcotest.(check bool) "down beside it" false (Portal3d.crossed floor ~before:(1.5, 0.1, 0.) ~after:(1.5, -0.1, 0.));
  Alcotest.(check bool) "up from behind" false (Portal3d.crossed floor ~before:(0., -0.1, 0.) ~after:(0., 0.1, 0.))

(* A polygon behind the wall portal, seen from 4 m in front of it: what
 * is kept is inside the view through the portal and behind it; a
 * polygon off to the side is not seen at all *)
let seeing_through () =
  let eye = (0., 5., -4.) in
  let big = [ (-10., 0., -12.); (10., 0., -12.); (10., 10., -12.); (-10., 10., -12.) ] in
  let kept = Portal3d.clip ~eye wall big in
  Alcotest.(check bool) "something is seen" true (List.length kept >= 3);
  List.iter
    (fun (x, y, z) ->
      (* seen from the eye 4 m out, the 2 m portal spans 4 m more at 8 m *)
      Alcotest.(check bool) "behind the portal, and within its window" true
        (z <= -8. && Float.abs x <= 2.0001 && Float.abs (y -. 5.) <= 2.0001))
    kept;
  let aside = [ (6., 4., -12.); (8., 4., -12.); (8., 6., -12.); (6., 6., -12.) ] in
  Alcotest.(check int) "off to the side: nothing" 0 (List.length (Portal3d.clip ~eye wall aside))

let tests =
  [ t "Portal3d, speedy thing goes in, speedy thing comes out" speedy_thing;
    t "Portal3d, in front of one is behind the other" sides;
    t "Portal3d, an orientation goes through as a direction" orientation;
    t "Portal3d, crossing" crossing;
    t "Portal3d, seeing through" seeing_through ]
