(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_indexed.mli *)

let points = Alcotest.(list (pair int int))

let tests =
  Testo.categorize "Indexed"
    [
      Testo.create "Indexed.mli's line, Bresenham's" (fun () ->
          Alcotest.check points "line" [ (0, 0); (1, 1); (2, 1); (3, 2); (4, 2) ] (Indexed.line_dots (0, 0) (4, 2)));
      Testo.create "the bucket: the same colour, touching, and no further" (fun () ->
          (* a wall of colour 2 down the middle: the fill stays on its side *)
          let p = Indexed.change (Indexed.create 5 3 0) (fun p -> for y = 0 to 2 do Indexed.dot p 2 y 2 done) in
          let q = Indexed.change p (fun q -> Indexed.fill q 7 (0, 0)) in
          Alcotest.(check (list int)) "left filled, wall kept, right untouched" [ 7; 7; 2; 0; 0 ] (List.init 5 (fun x -> Indexed.get q x 1));
          Alcotest.(check int) "the old picture unchanged" 0 (Indexed.get p 0 0));
      Testo.create "a cut piece: its transparent colour not painted" (fun () ->
          let src = Indexed.change (Indexed.create 3 1 4) (fun p -> Indexed.dot p 1 0 9) in
          let piece = Indexed.cut src (0, 0) (2, 0) in
          let q = Indexed.change (Indexed.create 3 1 1) (fun q -> Indexed.stamp q (Piece (piece, 4)) 0 (1, 0)) in
          Alcotest.(check (list int)) "only the 9" [ 1; 9; 1 ] (List.init 3 (fun x -> Indexed.get q x 0)));
      Testo.create "symmetry: four turns of a point" (fun () ->
          Alcotest.check points "turns" [ (11, 10); (10, 11); (9, 10); (10, 9) ] (Indexed.symmetric ~order:4 ~centre:(10, 10) (11, 10)));
      Testo.create "Cycling.mli's range of three" (fun () ->
          let pal = [| (0, 0, 0); (1, 1, 1); (10, 0, 0) (* A *); (20, 0, 0) (* B *); (30, 0, 0) (* C *) |] in
          let r : Ilbm.range = { low = 2; high = 4; rate = 16384; active = true; reverse = false } in
          let reds p = List.map (fun (r, _, _) -> r) (Array.to_list p) in
          Alcotest.(check (list int)) "one step" [ 0; 1; 30; 10; 20 ] (reds (Cycling.turn pal r 1));
          Alcotest.(check (list int)) "three: as it was" [ 0; 1; 10; 20; 30 ] (reds (Cycling.turn pal r 3));
          Alcotest.(check (list int)) "reversed" [ 0; 1; 20; 30; 10 ] (reds (Cycling.turn pal { r with reverse = true } 1));
          (* 60 steps a second: after a second, 60 steps, which is 0 modulo 3 *)
          Alcotest.(check (list int)) "a second" [ 0; 1; 10; 20; 30 ] (reds (Cycling.palette_at pal [ r ] 1.)));
    ]
