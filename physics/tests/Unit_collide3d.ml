(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/3d/Hitbox3d and physics/3d/Collide3d: the worked examples of
 * their .mlis, the ray tests against answers worked out by hand, and
 * the separating axis theorem checked against a ground truth -- twice,
 * because the interesting question is not whether SAT works but what
 * happens when its nine edge-edge axes are left out *)

open Hitbox3d

let t = Testo.create
let close = Alcotest.(check (float 1e-9))
let near = Alcotest.(check (float 1e-6))

let check_vec ?(tol = 1e-9) name (ax, ay, az) (bx, by, bz) =
  Alcotest.(check (float tol)) (name ^ " x") ax bx;
  Alcotest.(check (float tol)) (name ^ " y") ay by;
  Alcotest.(check (float tol)) (name ^ " z") az bz

let deg d = d *. Float.pi /. 180.

(*****************************************************************************)
(* Hitbox3d *)
(*****************************************************************************)

let shapes_and_their_numbers () =
  (* volume *)
  near "a sphere of radius 2" (4. /. 3. *. Float.pi *. 8.) (volume (Sphere 2.));
  near "a box 2 x 1 x 4" 8. (volume (Box (1., 0.5, 2.)));
  near "a capsule is a cylinder plus a sphere" (Float.pi +. (4. /. 3. *. Float.pi)) (volume (Capsule (0.5, 1.)));
  (* a capsule whose segment has no length *is* a sphere, tensor and
   * all: the sharpest check the capsule's formula can get *)
  let as_capsule = inertia ~mass:3. (Capsule (0., 0.7)) in
  let as_sphere = inertia ~mass:3. (Sphere 0.7) in
  near "a capsule of no length has a sphere's tensor" as_sphere.Mat3.m00 as_capsule.Mat3.m00;
  near "about every axis" as_sphere.Mat3.m11 as_capsule.Mat3.m11;
  (* a long capsule is easy to spin about its own length *)
  let rod = inertia ~mass:1. (Capsule (2., 0.1)) in
  Alcotest.(check bool) "a rod spins easily about its length" true (rod.Mat3.m11 < rod.Mat3.m00 /. 100.);
  near "and the same across either way" rod.Mat3.m00 rod.Mat3.m22

let placing_and_reading () =
  let b = place ~orientation:(Quat.of_axis_angle (0., 1., 0.) (deg 90.)) (1., 2., 3.) (Box (2., 0.5, 0.5)) in
  (* turned a quarter about y, a box 4 long in x is now 4 long in z *)
  let (lx, ly, lz), (hx, hy, hz) = bounds b in
  near "its x reach is now its thickness" 1. (hx -. lx);
  near "and its z reach its length" 4. (hz -. lz);
  near "y is untouched" 1. (hy -. ly);
  near "centred where it was put" 1. ((lx +. hx) /. 2.);
  near "" 2. ((ly +. hy) /. 2.);
  near "" 3. ((lz +. hz) /. 2.);
  Alcotest.(check int) "a box has eight corners" 8 (List.length (corners b));
  (* support: the farthest point along a direction -- a *corner*, and
   * with the box's long axis now along z, four of them tie for
   * farthest, so what is worth checking is how far it reaches *)
  let far = support b (0., 0., 1.) in
  let _, _, fz = far in
  near "it reaches 2 along z from the centre" 5. fz;
  Alcotest.(check bool) "and it is one of the corners" true
    (List.exists (fun c -> Vec3.length (Vec3.sub c far) < 1e-9) (corners b));
  let s = place (0., 0., 0.) (Sphere 2.) in
  check_vec "a sphere's support is its radius that way" (0., 2., 0.) (support s (0., 5., 0.));
  let cap = place (0., 0., 0.) (Capsule (1., 0.25)) in
  let a, bb = segment cap in
  check_vec "a capsule's segment, its own y" (0., -1., 0.) a;
  check_vec "" (0., 1., 0.) bb

(*****************************************************************************)
(* Closest points *)
(*****************************************************************************)

let closest_points () =
  (* the .mli's example: two segments crossing at right angles, a unit
   * apart *)
  let p, q = Collide3d.closest_between_segments ((0., 0., 0.), (1., 0., 0.)) ((0.5, 1., -1.), (0.5, 1., 1.)) in
  check_vec "on the first" (0.5, 0., 0.) p;
  check_vec "on the second" (0.5, 1., 0.) q;
  (* parallel segments: any pair of facing points will do, and the
   * distance is what matters *)
  let p, q = Collide3d.closest_between_segments ((0., 0., 0.), (1., 0., 0.)) ((0., 2., 0.), (1., 2., 0.)) in
  near "parallel, two apart" 2. (Vec3.length (Vec3.sub q p));
  (* past the end of a segment: its endpoint *)
  check_vec "off the end" (1., 0., 0.) (Collide3d.closest_on_segment ((0., 0., 0.), (1., 0., 0.)) (5., 0., 0.));
  (* a triangle's regions: a corner, an edge, the face *)
  let tri = ((0., 0., 0.), (2., 0., 0.), (0., 2., 0.)) in
  check_vec "over the face" (0.5, 0.5, 0.) (Collide3d.closest_on_triangle tri (0.5, 0.5, 3.));
  check_vec "past a corner" (0., 0., 0.) (Collide3d.closest_on_triangle tri (-1., -1., 0.));
  check_vec "beside an edge" (1., 0., 0.) (Collide3d.closest_on_triangle tri (1., -1., 0.));
  (* a box's nearest point, in its own frame *)
  let b = place ~orientation:(Quat.of_axis_angle (0., 1., 0.) (deg 45.)) (0., 0., 0.) (Box (1., 1., 1.)) in
  let q = Collide3d.closest_on_box b (3., 0., 0.) in
  near "a box turned 45 degrees reaches sqrt 2 towards +x" (sqrt 2.) (Vec3.length q)

(*****************************************************************************)
(* Pairs *)
(*****************************************************************************)

let sphere_pairs () =
  (* Contact3d.mli's example *)
  (match Collide3d.spheres ((0., 0., 0.), 3.) ((2.4, 3.2, 0.), 2.) with
  | None -> Alcotest.fail "these overlap"
  | Some c ->
      near "depth" 1. c.Contact3d.depth;
      check_vec ~tol:1e-6 "the normal, from the first towards the second" (0.6, 0.8, 0.) c.Contact3d.normal;
      near "and the point is between their surfaces" 2.5 (Vec3.length c.Contact3d.point));
  (* exactly 5 apart, radii 3 and 2: touching, which counts as apart *)
  Alcotest.(check bool) "just touching is not overlapping" true (Collide3d.spheres ((0., 0., 0.), 3.) ((3., 4., 0.), 2.) = None);
  Alcotest.(check bool) "well apart" true (Collide3d.spheres ((0., 0., 0.), 1.) ((10., 0., 0.), 1.) = None)

let sphere_against_box () =
  let b = place (0., 0., 0.) (Box (1., 1., 1.)) in
  (match Collide3d.sphere_box ((0., 1.5, 0.), 0.6) b with
  | None -> Alcotest.fail "0.6 of sphere reaches 1.5 - 1 = 0.5 away"
  | Some c ->
      near "depth" 0.1 c.Contact3d.depth;
      check_vec "the normal points down at the box" (0., -1., 0.) c.Contact3d.normal;
      check_vec "and the point is on its face" (0., 1., 0.) c.Contact3d.point);
  Alcotest.(check bool) "a sphere just out of reach" true (Collide3d.sphere_box ((0., 1.7, 0.), 0.6) b = None);
  (* the second case: a centre inside the box, where "the closest point
   * on the box" is the centre itself and says nothing *)
  match Collide3d.sphere_box ((0., 0.8, 0.), 0.2) b with
  | None -> Alcotest.fail "a sphere inside the box is certainly touching it"
  | Some c ->
      (* the centre is nearest the +y face, so the sphere's way out is
       * up -- and the normal, which pushes the *box*, points down *)
      check_vec "the box is pushed off the nearest face" (0., -1., 0.) c.Contact3d.normal;
      near "and the depth reaches through it" 0.4 c.Contact3d.depth

(* the ground truth for two boxes: a separating axis, checked straight
 * on the corners, independently of what Collide3d does *)
let separating_axis (a : placed) (b : placed) : Vec3.t option =
  let fa = face_axes a and fb = face_axes b in
  let axes = fa @ fb @ List.concat_map (fun u -> List.map (fun v -> Vec3.cross u v) fb) fa in
  let shadow p axis =
    let ds = List.map (fun c -> Vec3.dot c axis) (corners p) in
    (List.fold_left Float.min infinity ds, List.fold_left Float.max neg_infinity ds)
  in
  List.find_opt
    (fun axis ->
      Vec3.length axis > 1e-6
      &&
      let axis = Vec3.normalize axis in
      let alo, ahi = shadow a axis and blo, bhi = shadow b axis in
      ahi < blo -. 1e-9 || bhi < alo -. 1e-9)
    axes

let point_in_box (p : placed) (q : Vec3.t) : bool =
  match p.shape with
  | Box (hx, hy, hz) ->
      let lx, ly, lz = Quat.rotate (Quat.conjugate p.orientation) (Vec3.sub q p.pos) in
      Float.abs lx <= hx && Float.abs ly <= hy && Float.abs lz <= hz
  | _ -> false

(* The nine edge-edge axes, and what leaving them out costs. This pair
 * was found by trying random ones (20,000 of them, 3 found): no face
 * of either box separates them, so a 6-axis test calls it a collision,
 * and the cross of one edge of each shows they are apart -- which the
 * certificate below checks without asking Collide3d anything. *)
let the_nine_axes () =
  let a = place ~orientation:(Quat.of_axis_angle (-0.2481, -0.9472, -0.2033) 1.8013) (0., 0., 0.) (Box (1., 0.2, 0.2)) in
  let b = place ~orientation:(Quat.of_axis_angle (0.6495, 0.0376, 0.7594) 0.5296) (-0.5085, 0.5111, -0.9856) (Box (1., 0.2, 0.2)) in
  (match separating_axis a b with
  | None -> Alcotest.fail "the certificate says they overlap: the pair is no longer a good example"
  | Some axis ->
      (* and it is one of the nine, not a face of either *)
      let is_face ax = List.exists (fun f -> Float.abs (Float.abs (Vec3.dot (Vec3.normalize ax) f) -. 1.) < 1e-6) (face_axes a @ face_axes b) in
      Alcotest.(check bool) "the separating axis is an edge cross, not a face" false (is_face axis));
  Alcotest.(check bool) "with the nine: apart, correctly" true (Collide3d.boxes a b = None);
  Alcotest.(check bool) "without them: a collision that is not there" true (Collide3d.boxes ~edge_axes:false a b <> None)

(* and the other way round: boxes that really do overlap are found,
 * whichever axes are tried, and the contact pushes them apart *)
let boxes_overlap () =
  let a = place (0., 0., 0.) (Box (1., 1., 1.)) in
  let b = place ~orientation:(Quat.of_axis_angle (0., 1., 0.) (deg 45.)) (2.2, 0., 0.) (Box (1., 1., 1.)) in
  match Collide3d.boxes a b with
  | None -> Alcotest.fail "a box turned 45 degrees reaches sqrt 2 = 1.414, so 2.2 apart overlaps"
  | Some c ->
      near "they overlap by 1 + 1.414 - 2.2" (1. +. sqrt 2. -. 2.2) c.Contact3d.depth;
      check_vec ~tol:1e-6 "pushed apart along x" (1., 0., 0.) c.Contact3d.normal;
      Alcotest.(check bool) "the certificate agrees there is no separating axis" true (separating_axis a b = None)

(* a thousand random pairs: the answer is never "apart" when a point
 * can be found inside both, and never "together" when an axis
 * separates them *)
let boxes_against_the_truth () =
  Random.init 12;
  let rnd () = Random.float 2. -. 1. in
  let disagreements = ref 0 and overlaps = ref 0 in
  for _ = 1 to 1000 do
    let a = place ~orientation:(Quat.of_axis_angle (rnd (), rnd (), rnd ()) (Random.float 6.28)) (0., 0., 0.) (Box (1., 0.4, 0.7)) in
    let b =
      place
        ~orientation:(Quat.of_axis_angle (rnd (), rnd (), rnd ()) (Random.float 6.28))
        (rnd (), rnd (), rnd ())
        (Box (0.8, 0.8, 0.3))
    in
    let says_touching = Collide3d.boxes a b <> None in
    if says_touching then incr overlaps;
    (match separating_axis a b with
    | Some _ when says_touching -> incr disagreements
    | None when not says_touching -> incr disagreements
    | _ -> ());
    (* and a point inside both settles it either way *)
    if not says_touching then
      for _ = 1 to 40 do
        let lx, ly, lz = (rnd (), rnd (), rnd ()) in
        let p =
          match a.shape with
          | Box (hx, hy, hz) -> Vec3.add a.pos (Quat.rotate a.orientation (lx *. hx, ly *. hy, lz *. hz))
          | _ -> a.pos
        in
        if point_in_box b p then incr disagreements
      done
  done;
  Alcotest.(check bool) "the random pairs did overlap sometimes" true (!overlaps > 100);
  Alcotest.(check int) "and SAT and the certificate never disagreed" 0 !disagreements

let capsule_pairs () =
  (* two crossed capsules, their segments a unit apart, radii 0.4 and
   * 0.4: they overlap by 0.8 - 1 < 0... just missing *)
  let up = place (0., 0., 0.) (Capsule (1., 0.4)) in
  let across =
    place ~orientation:(Quat.of_axis_angle (0., 0., 1.) (deg 90.)) (0., 1., 0.) (Capsule (1., 0.4))
  in
  (match Collide3d.capsules up across with
  | None -> Alcotest.fail "their segments meet: the ends touch"
  | Some c -> near "deep, since the segments cross" 0.8 c.Contact3d.depth);
  let far = place ~orientation:(Quat.of_axis_angle (0., 0., 1.) (deg 90.)) (0., 2., 0.) (Capsule (1., 0.4)) in
  Alcotest.(check bool) "moved up out of reach" true (Collide3d.capsules up far = None);
  (* a sphere and a capsule, from the side *)
  match Collide3d.sphere_capsule ((1., 0.5, 0.), 0.7) up with
  | None -> Alcotest.fail "1 away, radii 0.7 + 0.4"
  | Some c ->
      near "depth" 0.1 c.Contact3d.depth;
      check_vec ~tol:1e-6 "the normal, sphere to capsule" (-1., 0., 0.) c.Contact3d.normal

let capsule_against_box () =
  let b = place (0., 0., 0.) (Box (2., 0.5, 2.)) in
  (* a capsule standing on the box's top face *)
  let standing = place (0., 1.3, 0.) (Capsule (0.5, 0.3)) in
  Alcotest.(check bool) "its bottom cap ends exactly on the box's top: apart" true (Collide3d.box_capsule b standing = None);
  (match Collide3d.box_capsule b (place (0., 1.25, 0.) (Capsule (0.5, 0.3))) with
  | None -> Alcotest.fail "0.05 into the face"
  | Some c ->
      check_vec ~tol:1e-6 "pushed straight up off the face" (0., 1., 0.) c.Contact3d.normal;
      near "just into it" 0.05 c.Contact3d.depth);
  let lower = place (0., 1.1, 0.) (Capsule (0.5, 0.3)) in
  (match Collide3d.box_capsule b lower with
  | None -> Alcotest.fail "0.2 into the face"
  | Some c -> near "sunk by 0.2" 0.2 c.Contact3d.depth);
  Alcotest.(check bool) "well above it" true (Collide3d.box_capsule b (place (0., 3., 0.) (Capsule (0.5, 0.3))) = None)

let against_a_plane () =
  let floor = ((0., 1., 0.), 0.) in
  let ball = place (0., 0.4, 0.) (Sphere 0.5) in
  (match Collide3d.plane_hitbox floor ball with
  | None -> Alcotest.fail "its bottom is at -0.1"
  | Some c ->
      near "0.1 through the floor" 0.1 c.Contact3d.depth;
      check_vec "up, out of the half-space" (0., 1., 0.) c.Contact3d.normal);
  Alcotest.(check bool) "resting exactly on it counts as apart" true
    (Collide3d.plane_hitbox floor (place (0., 0.5, 0.) (Sphere 0.5)) = None);
  (* a box tipped onto a corner: the deepest corner is what touches *)
  let tipped = place ~orientation:(Quat.of_axis_angle (0., 0., 1.) (deg 45.)) (0., 0.5, 0.) (Box (0.5, 0.5, 0.5)) in
  match Collide3d.plane_hitbox floor tipped with
  | None -> Alcotest.fail "a 45-degree box reaches 0.707 down"
  | Some c -> near "its corner is 0.207 under" ((sqrt 0.5) -. 0.5) c.Contact3d.depth

let the_dispatcher () =
  let ball = place (0., 0., 0.) (Sphere 1.) in
  let b = place (1.5, 0., 0.) (Box (1., 1., 1.)) in
  (match Collide3d.contact ball b with
  | None -> Alcotest.fail "they overlap by 0.5"
  | Some c -> check_vec ~tol:1e-6 "sphere first: the normal points at the box" (1., 0., 0.) c.Contact3d.normal);
  (match Collide3d.contact b ball with
  | None -> Alcotest.fail "the same pair, the other way up"
  | Some c -> check_vec ~tol:1e-6 "box first: the normal turns round" (-1., 0., 0.) c.Contact3d.normal);
  Alcotest.(check bool) "touching says the same" true (Collide3d.touching ball b);
  Alcotest.(check bool) "and a plane works with anything" true
    (Collide3d.touching (place (0., 0., 0.) (Plane ((0., 1., 0.), 0.))) (place (0., 0.2, 0.) (Sphere 0.5)))

(*****************************************************************************)
(* Rays *)
(*****************************************************************************)

let rays () =
  let from = (0., 0., 5.) and direction = (0., 0., -1.) in
  (* a sphere of radius 1 at the origin: hit at 4 *)
  near "ray to sphere" 4. (Option.get (Collide3d.ray_sphere ~from ~direction ((0., 0., 0.), 1.)));
  Alcotest.(check bool) "a ray pointing away misses" true (Collide3d.ray_sphere ~from ~direction:(0., 0., 1.) ((0., 0., 0.), 1.) = None);
  Alcotest.(check bool) "and one that passes beside it" true
    (Collide3d.ray_sphere ~from:(3., 0., 5.) ~direction ((0., 0., 0.), 1.) = None);
  near "a ray starting inside hits at 0" 0. (Option.get (Collide3d.ray_sphere ~from:(0., 0., 0.) ~direction ((0., 0., 0.), 1.)));
  (* a box, turned: the slab test in its own frame *)
  let b = place ~orientation:(Quat.of_axis_angle (0., 1., 0.) (deg 45.)) (0., 0., 0.) (Box (1., 1., 1.)) in
  near "ray to a box turned 45 degrees: sqrt 2 out" (5. -. sqrt 2.) (Option.get (Collide3d.ray_box ~from ~direction b));
  Alcotest.(check bool) "past its corner" true (Collide3d.ray_box ~from:(1.9, 0., 5.) ~direction b = None);
  (* a capsule: the side, then the caps *)
  let cap = place (0., 0., 0.) (Capsule (1., 0.5)) in
  near "ray to a capsule's side" 4.5 (Option.get (Collide3d.ray_capsule ~from ~direction cap));
  near "and to its cap, from above" 0.5 (Option.get (Collide3d.ray_capsule ~from:(0., 2., 0.) ~direction:(0., -1., 0.) cap));
  Alcotest.(check bool) "beside it" true (Collide3d.ray_capsule ~from:(2., 0., 5.) ~direction cap = None);
  (* Moller-Trumbore *)
  let tri = ((-1., -1., 0.), (1., -1., 0.), (0., 1., 0.)) in
  near "ray to a triangle" 5. (Option.get (Collide3d.ray_triangle ~from ~direction tri));
  Alcotest.(check bool) "outside its edges" true (Collide3d.ray_triangle ~from:(0.9, 0.9, 5.) ~direction tri = None);
  Alcotest.(check bool) "a ray in its plane misses" true
    (Collide3d.ray_triangle ~from:(0., 0., 0.) ~direction:(1., 0., 0.) tri = None);
  (* the dispatcher, and a plane *)
  near "ray to a plane" 4. (Option.get (Collide3d.ray ~from ~direction:(0., -1., 0.) (place (0., 0., 0.) (Plane ((0., 1., 0.), -4.)))));
  near "and to a sphere through it" 4. (Option.get (Collide3d.ray ~from ~direction (place (0., 0., 0.) (Sphere 1.))))

(* a ray that hits a sphere at a known angle: the distance is the
 * analytic one, for a hundred directions *)
let rays_against_the_analytic_answer () =
  Random.init 3;
  for _ = 1 to 100 do
    let r = 0.5 +. Random.float 2. in
    let c = (Random.float 4. -. 2., Random.float 4. -. 2., Random.float 4. -. 2.) in
    let from = (Random.float 20. -. 10., Random.float 20. -. 10., Random.float 20. -. 10.) in
    (* aim at a point on the sphere, so the answer is known *)
    let dir = Vec3.normalize (Random.float 2. -. 1., Random.float 2. -. 1., Random.float 2. -. 1.) in
    let target = Vec3.add c (Vec3.scale r dir) in
    let direction = Vec3.sub target from in
    (* only when the ray starts outside: from within, the first hit is
     * where it started, and the test below would be asking the wrong
     * question *)
    if Vec3.length (Vec3.sub from c) > r +. 0.1 then
      match Collide3d.ray_sphere ~from ~direction (c, r) with
      | None -> Alcotest.fail "a ray aimed at a point of the sphere must hit it"
      | Some hit ->
          let p = Vec3.add from (Vec3.scale hit (Vec3.normalize direction)) in
          Alcotest.(check (float 1e-6)) "the hit is on the sphere" r (Vec3.length (Vec3.sub p c))
  done

let tests =
  [ t "Hitbox3d, the volumes and the tensors" shapes_and_their_numbers;
    t "Hitbox3d, placed: bounds, corners, support" placing_and_reading;
    t "Collide3d, the closest points" closest_points;
    t "Collide3d, spheres" sphere_pairs;
    t "Collide3d, a sphere and a box, from outside and from within" sphere_against_box;
    t "Collide3d, the nine edge-edge axes, and the bug without them" the_nine_axes;
    t "Collide3d, boxes that do overlap" boxes_overlap;
    t "Collide3d, a thousand random pairs against a certificate" boxes_against_the_truth;
    t "Collide3d, capsules" capsule_pairs;
    t "Collide3d, a capsule on a box" capsule_against_box;
    t "Collide3d, against a plane" against_a_plane;
    t "Collide3d, the dispatcher" the_dispatcher;
    t "Collide3d, the rays" rays;
    t "Collide3d, a hundred rays against the analytic sphere" rays_against_the_analytic_answer ]
