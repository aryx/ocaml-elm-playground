(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/Zbuffer *)

let t = Testo.create

(* Zbuffer.mli's example, at one pixel of the overlap: A at depth 5, B
 * at 3, in both orders *)
let test_overlap () =
  let zb = Zbuffer.create ~width:4 ~height:3 in
  Alcotest.(check bool) "A first: drawn" true (Zbuffer.test_and_set zb ~x:2 ~y:1 5.);
  Alcotest.(check bool) "then B, nearer: drawn" true (Zbuffer.test_and_set zb ~x:2 ~y:1 3.);
  Zbuffer.clear zb;
  Alcotest.(check bool) "B first: drawn" true (Zbuffer.test_and_set zb ~x:2 ~y:1 3.);
  Alcotest.(check bool) "then A, farther: not drawn" false (Zbuffer.test_and_set zb ~x:2 ~y:1 5.);
  Alcotest.(check bool) "another pixel: its own depth" true (Zbuffer.test_and_set zb ~x:1 ~y:2 5.)

let tests = Testo.categorize "Zbuffer" [ t "two overlapping triangles" test_overlap ]
