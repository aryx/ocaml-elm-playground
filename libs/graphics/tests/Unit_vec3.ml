(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/geometry: Vec3 and Mat4 *)

let t = Testo.create

let vec = Alcotest.(triple (float 1e-9) (float 1e-9) (float 1e-9))

(* the example in Vec3.ml: a counterclockwise triangle in the xy plane *)
let test_face_normal_triangle () =
  Alcotest.check vec "(0,0,0), (1,0,0), (0,1,0)" (0., 0., 1.)
    (Vec3.face_normal [ (0., 0., 0.); (1., 0., 0.); (0., 1., 0.) ])

(* The bug Vec3.face_normal's comment tells about: a sphere's top quad,
 * whose first two corners are both the pole. The first-three-points
 * formula gives (0, 0, 0) there; Newell's method the real normal,
 * pointing up and out. *)
let test_face_normal_pole () =
  let pole = (0., 1., 0.) in
  let quad = [ pole; pole; (0.5, 0.8, 0.3); (0.5, 0.8, -0.3) ] in
  let first_three =
    match quad with
    | p0 :: p1 :: p2 :: _ -> Vec3.normalize (Vec3.cross (Vec3.sub p1 p0) (Vec3.sub p2 p0))
    | _ -> assert false
  in
  Alcotest.check vec "the old formula: (0, 0, 0)" (0., 0., 0.) first_three;
  let n = Vec3.face_normal quad in
  Alcotest.(check (float 1e-9)) "Newell: a unit normal" 1. (Vec3.length n);
  Alcotest.(check bool) "pointing up, out of the sphere" true (Vec3.dot n (0., 1., 0.) > 0.5)

(* a view matrix takes the camera's eye to the origin, and the target
 * straight ahead, on +z *)
let test_look_at () =
  let m = Mat4.look_at ~eye:(3., 2., 5.) ~target:(0., 0., 0.) () in
  let apply (x, y, z) =
    let row i = (m.(i * 4) *. x) +. (m.((i * 4) + 1) *. y) +. (m.((i * 4) + 2) *. z) +. m.((i * 4) + 3) in
    (row 0, row 1, row 2)
  in
  Alcotest.check vec "eye -> origin" (0., 0., 0.) (apply (3., 2., 5.));
  let x, y, z = apply (0., 0., 0.) in
  Alcotest.check vec "target -> straight ahead" (0., 0., sqrt 38.) (x, y, z)

(* the translation, in the last column of a row-major matrix, ends up
 * in the last row, where a column-major GPU expects it *)
let test_transpose () =
  let m = Mat4.look_at ~eye:(3., 2., 5.) ~target:(0., 0., 0.) () in
  let tm = Mat4.transpose m in
  let mat = Alcotest.(array (float 1e-12)) in
  Alcotest.check mat "last row = last column" [| m.(3); m.(7); m.(11); m.(15) |] (Array.sub tm 12 4);
  Alcotest.check mat "twice: the same" m (Mat4.transpose tm)

let tests =
  Testo.categorize "Vec3/Mat4"
    [
      t "face_normal of a triangle" test_face_normal_triangle;
      t "face_normal at a sphere's pole" test_face_normal_pole;
      t "look_at" test_look_at;
      t "transpose" test_transpose;
    ]
