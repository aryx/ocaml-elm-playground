(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/pushpull: SketchUp's model -- Skp_model.mli's worked
 * example, the house, with Euler's formula checked at each step; the
 * faces of a box all facing out; a face that slides; a window, and the
 * Euler-Poincare formula; a circle pushed into a cylinder whose seams
 * are soft; lines closing a face, an edge erased with its faces; half
 * a top pulled up into a step, the wall under it merged; the BSP
 * tree's order; the inference of an axis; the camera. *)

module M = Skp_model

let t = Testo.create
let v3 = Alcotest.(triple (float 1e-6) (float 1e-6) (float 1e-6))

let counts (m : M.t) = (List.length m.verts, List.length m.edges, List.length m.faces)
let vef = Alcotest.(triple int int int)
let euler (m : M.t) = List.length m.verts - List.length m.edges + List.length m.faces
let rings (m : M.t) = List.fold_left (fun n (f : M.face) -> n + List.length f.holes) 0 m.faces
let vertex (m : M.t) p = fst (List.find (fun (_, q) -> Vec3.length (Vec3.sub p q) < 1e-6) m.verts)
let only = function [ x ] -> x | _ -> Alcotest.fail "one expected"

(* the face whose normal is n *)
let facing (m : M.t) n = only (List.filter (fun f -> Vec3.length (Vec3.sub (M.normal m f) n) < 1e-6) m.faces)

let rect x0 y0 x1 y1 = [ (x0, y0, 0.); (x1, y0, 0.); (x1, y1, 0.); (x0, y1, 0.) ]

(* a box 6 x 4 x 3, pulled up from the ground *)
let box () =
  let m = M.add_polygon M.empty (rect (-3.) (-2.) 3. 2.) in
  let f = only m.faces in
  (m, M.push_pull m f.id (-3.))

let test_house () =
  let m, box = box () in
  Alcotest.check v3 "on the ground, a face faces down" (0., 0., -1.) (M.normal m (only m.faces));
  Alcotest.check vef "a box" (8, 12, 6) (counts box);
  Alcotest.(check int) "V - E + F" 2 (euler box);
  let split = M.add_edge box (0., -2., 3.) (0., 2., 3.) in
  Alcotest.check vef "the line across the top: two edges and the top split" (10, 15, 7) (counts split);
  Alcotest.(check int) "V - E + F" 2 (euler split);
  let house = M.move split [ vertex split (0., -2., 3.); vertex split (0., 2., 3.) ] (0., 0., 2.) in
  Alcotest.check vef "lifted: the same numbers" (10, 15, 7) (counts house);
  let front = facing house (0., -1., 0.) in
  Alcotest.(check int) "the front wall, a pentagon by itself" 5 (List.length front.outer);
  let slope = Vec3.normalize (-2., 0., 3.) in
  ignore (facing house slope);
  ignore (facing house (let x, y, z = slope in (-.x, y, z)))

(* the house's right slope pushed in: the gables notched *)
let test_notch () =
  let _, box = box () in
  let m = M.add_edge box (0., -2., 3.) (0., 2., 3.) in
  let m = M.move m [ vertex m (0., -2., 3.); vertex m (0., 2., 3.) ] (0., 0., 2.) in
  let slope = facing m (Vec3.normalize (2., 0., 3.)) in
  let m = M.push_pull m slope.id (-0.5) in
  Alcotest.(check int) "V - E + F" 2 (euler m);
  Alcotest.(check int) "the front gable, a pentagon with a notch" 7 (List.length (facing m (0., -1., 0.)).outer);
  Alcotest.(check int) "no face twice in its plane" 1 (List.length (List.filter (fun f -> Vec3.dot (M.normal m f) (0., 1., 0.) > 0.99) m.faces))

(* each face's normal points away from the middle of the box *)
let test_faces_out () =
  let _, box = box () in
  List.iter
    (fun (f : M.face) ->
      let c = Vec3.centroid (List.map (M.pos box) f.outer) in
      Alcotest.(check bool) "facing out" true (Vec3.dot (M.normal box f) (Vec3.sub c (0., 0., 1.5)) > 0.))
    box.faces

let test_slides () =
  let _, box = box () in
  let top = facing box (0., 0., 1.) in
  let taller = M.push_pull box top.id 1. in
  Alcotest.check vef "the top slides: nothing new" (8, 12, 6) (counts taller);
  Alcotest.(check bool) "at 4" true (List.for_all (fun v -> let _, _, z = M.pos taller v in z = 4.) top.outer)

let test_window () =
  let _, box = box () in
  let win = [ (0.8, -2., 1.); (2.2, -2., 1.); (2.2, -2., 2.); (0.8, -2., 2.) ] in
  let m = M.add_polygon box win in
  Alcotest.check vef "a face inside the wall" (12, 16, 7) (counts m);
  Alcotest.(check int) "and a hole in it" 1 (rings m);
  let inner = only (List.filter (fun (f : M.face) -> f.holes = [] && Vec3.dot (M.normal m f) (0., -1., 0.) > 0.99) m.faces) in
  Alcotest.(check bool) "facing as the wall does, where the window is" true (M.inside m inner (1.5, -2., 1.5));
  let m = M.push_pull m inner.id (-0.2) in
  Alcotest.check vef "pushed in: the recess" (16, 24, 11) (counts m);
  Alcotest.(check int) "V - E + F - R" 2 (euler m - rings m);
  let front = List.filter (fun f -> Vec3.length (Vec3.sub (M.normal m f) (0., -1., 0.)) < 1e-6) m.faces in
  Alcotest.(check (list int)) "facing front: the wall, and the recess's floor 0.2 behind" [ 1; 0 ]
    (List.map (fun (f : M.face) -> List.length f.holes) front |> List.sort (Fun.flip compare));
  Alcotest.(check bool) "the floor 0.2 behind" true
    (List.exists (fun (f : M.face) -> f.holes = [] && List.for_all (fun v -> let _, y, _ = M.pos m v in Float.abs (y +. 1.8) < 1e-9) f.outer) front);
  (* the side wall pulled out 2: the front of what it makes merges into
     the front wall, which keeps its window *)
  let side = only (List.filter (fun f -> Vec3.dot (M.normal m f) (1., 0., 0.) > 0.99 && M.inside m f (3., 0., 1.5)) m.faces) in
  let m = M.push_pull m side.id 2. in
  Alcotest.(check int) "V - E + F - R" 2 (euler m - rings m);
  let wall = only (List.filter (fun (f : M.face) -> f.holes <> []) m.faces) in
  Alcotest.(check bool) "the front wall, one face as far as the new corner" true (M.inside m wall (4.5, -2., 0.5) && M.inside m wall (-2.5, -2., 0.5))

let test_cylinder () =
  let circle = List.init 24 (fun i -> let a = float_of_int i *. Float.pi /. 12. in (cos a, sin a, 0.)) in
  let m = M.add_polygon ~curve:true M.empty circle in
  let m = M.push_pull m (only m.faces).id (-2.) in
  Alcotest.check vef "24 sides, a top, a bottom" (48, 72, 26) (counts m);
  Alcotest.(check int) "the seams between the sides are soft" 24 (List.length (List.filter (fun (e : M.edge) -> e.soft) m.edges))

let test_lines_close_a_face () =
  let pts = rect 0. 0. 2. 1. in
  let m = M.add_edge (M.add_edge (M.add_edge M.empty (List.nth pts 0) (List.nth pts 1)) (List.nth pts 1) (List.nth pts 2)) (List.nth pts 2) (List.nth pts 3) in
  Alcotest.(check int) "three lines: no face" 0 (List.length m.faces);
  let m = M.add_edge m (List.nth pts 3) (List.nth pts 0) in
  Alcotest.(check int) "the fourth closes it" 1 (List.length m.faces);
  let a = vertex m (0., 0., 0.) and b = vertex m (2., 0., 0.) in
  let m = M.erase_edge m a b in
  Alcotest.check vef "an edge erased: its face goes, the rest stays" (4, 3, 0) (counts m)

let test_step () =
  let _, box = box () in
  let m = M.add_edge box (0., -2., 3.) (0., 2., 3.) in
  let half = only (List.filter (fun (f : M.face) -> M.inside m f (1.5, 0., 3.) && Vec3.dot (M.normal m f) (0., 0., 1.) > 0.5) m.faces) in
  let m = M.push_pull m half.id 1. in
  Alcotest.(check int) "V - E + F" 2 (euler m);
  Alcotest.(check int) "the right wall grew: one face, 4 corners" 4 (List.length (facing m (1., 0., 0.)).outer);
  Alcotest.(check int) "the front, an L: its corners, the healed one gone" 6 (List.length (facing m (0., -1., 0.)).outer)

let test_bsp () =
  let square z = List.map (fun (x, y) -> ((x, y, z), true)) [ (0., 0.); (1., 0.); (1., 1.); (0., 1.) ] in
  let tree = Bsp.build [ { Bsp.corners = square 0.; data = 0 }; { corners = square 1.; data = 1 } ] in
  let order eye = List.map (fun (p : int Bsp.poly) -> p.data) (Bsp.back_to_front ~eye tree) in
  Alcotest.(check (list int)) "from above" [ 0; 1 ] (order (0.5, 0.5, 5.));
  Alcotest.(check (list int)) "from below" [ 1; 0 ] (order (0.5, 0.5, -5.));
  (* a square standing across them is cut in two by the first *)
  let wall = List.map (fun (x, z) -> ((x, 0.5, z), true)) [ (0., -1.); (1., -1.); (1., 2.); (0., 2.) ] in
  let tree = Bsp.build [ { Bsp.corners = square 0.; data = 0 }; { corners = wall; data = 2 } ] in
  Alcotest.(check int) "cut in two" 3 (Bsp.size tree);
  let front, back = Bsp.split ((0., 0., 1.), 0.) wall in
  Alcotest.(check (list bool)) "the cut side is not an edge" [ false; true; true; true ] (List.map snd front |> List.sort compare);
  Alcotest.(check int) "the half below, 4 corners" 4 (List.length back)

let area : Skp_view.area = { cx = 0.; cy = 0.; w = 800.; h = 600. }

let test_view () =
  let v = { Skp_view.start with target = (0., 0., 0.); distance = 10.; azimuth = 0.; elevation = 0. } in
  Alcotest.check v3 "the eye along x" (10., 0., 0.) (Skp_view.eye v);
  (match Skp_view.project v area (0., 0., 1.) with
  | Some (x, y) -> Alcotest.(check bool) "straight above the middle" true (Float.abs x < 1e-6 && y > 0.)
  | None -> Alcotest.fail "in front");
  Alcotest.(check bool) "behind the eye: nowhere" true (Skp_view.project v area (20., 0., 0.) = None)

let test_infer () =
  let v = Skp_view.start in
  let x, y = Option.get (Skp_view.project v area (3., 0., 0.)) in
  let mouse = (x, y +. 1.) in
  let found = Skp_infer.find ~project:(Skp_view.project v area) ~ray:(Skp_view.ray v area mouse) ~from:(0., 0., 0.) M.empty mouse in
  Alcotest.(check string) "the axis" "On Red Axis" (Skp_infer.name found.kind);
  Alcotest.check (Alcotest.triple (Alcotest.float 0.05) (Alcotest.float 1e-9) (Alcotest.float 1e-9)) "on it, 3 along" (3., 0., 0.) found.point;
  let _, box = box () in
  let corner = (3., -2., 3.) in
  let x, y = Option.get (Skp_view.project v area corner) in
  let found = Skp_infer.find ~project:(Skp_view.project v area) ~ray:(Skp_view.ray v area (x +. 3., y)) box (x +. 3., y) in
  Alcotest.(check string) "a corner" "Endpoint" (Skp_infer.name found.kind);
  (* the far bottom corner is behind the box *)
  let hidden = (-3., 2., 0.) in
  let x, y = Option.get (Skp_view.project v area hidden) in
  let found = Skp_infer.find ~project:(Skp_view.project v area) ~ray:(Skp_view.ray v area (x, y)) box (x, y) in
  Alcotest.(check bool) "hidden: not an endpoint" true (found.kind <> Skp_infer.Endpoint)

let tests =
  [
    t "pushpull: the house, Skp_model.mli's worked example" test_house;
    t "pushpull: a box pulled from the ground faces out" test_faces_out;
    t "pushpull: the top of a box slides" test_slides;
    t "pushpull: a window, a hole and a recess" test_window;
    t "pushpull: a circle pushed, its seams soft" test_cylinder;
    t "pushpull: lines close a face, an edge erased" test_lines_close_a_face;
    t "pushpull: half a top pulled up, the wall merged" test_step;
    t "pushpull: a roof pushed in, the gables notched" test_notch;
    t "pushpull: the BSP tree's order, and its cuts" test_bsp;
    t "pushpull: the camera" test_view;
    t "pushpull: inference, an axis, an endpoint, a hidden one" test_infer;
  ]
