(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* playground/Tilemap *)

let t = Testo.create

let point = Alcotest.(pair (float 1e-9) (float 1e-9))
let cell = Alcotest.(pair int int)

(* Tilemap.mli's example map *)
let map = Tilemap.of_strings 10. [ "#....#"; "#.@..#"; "######" ]

let test_size () =
  Alcotest.(check int) "cols" 6 (Tilemap.cols map);
  Alcotest.(check int) "rows" 3 (Tilemap.rows map);
  let b = Tilemap.bounds map in
  Alcotest.(check (list (float 1e-9))) "bounds" [ -30.; 30.; -15.; 15. ] [ b.left; b.right; b.bottom; b.top ];
  let short = Tilemap.of_strings 10. [ "##"; "#" ] in
  Alcotest.(check (option char)) "short rows padded with ' '" (Some ' ') (Tilemap.get short 1 1)

let test_cells () =
  Alcotest.check point "center (2, 1)" (-5., 0.) (Tilemap.center map 2 1);
  Alcotest.check cell "cell (-9, 4)" (2, 1) (Tilemap.cell map (-9.) 4.);
  Alcotest.check cell "left of the map" (-1, 0) (Tilemap.cell map (-31.) 14.);
  Alcotest.check cell "on a border: the right, lower cell" (2, 1) (Tilemap.cell map (-10.) 5.);
  Alcotest.(check (option char)) "tile_at (-9, 4)" (Some '@') (Tilemap.tile_at map (-9.) 4.);
  Alcotest.(check (option char)) "tile_at (100, 0)" None (Tilemap.tile_at map 100. 0.)

let test_find_set () =
  Alcotest.(check (list string)) "to_strings, padded" [ "##"; "# " ] (Tilemap.to_strings (Tilemap.of_strings 1. [ "##"; "#" ]));
  Alcotest.(check (list cell)) "find '@'" [ (2, 1) ] (Tilemap.find map '@');
  let map' = Tilemap.set map 2 1 '.' in
  Alcotest.(check (list cell)) "set: no more '@'" [] (Tilemap.find map' '@');
  Alcotest.(check (list cell)) "the original map unchanged" [ (2, 1) ] (Tilemap.find map '@');
  Alcotest.(check (list cell)) "set outside: the same map" [ (2, 1) ] (Tilemap.find (Tilemap.set map 9 9 '@') '@')

let test_hits () =
  let wall = ( = ) '#' in
  Alcotest.(check bool) "on the '@' cell, touching the floor" false (Tilemap.hits wall map (-5.) 0. 10. 10.);
  Alcotest.(check bool) "entering the floor" true (Tilemap.hits wall map (-5.) (-1.) 10. 10.);
  Alcotest.(check bool) "entering the left wall" true (Tilemap.hits wall map (-16.) 0. 10. 10.);
  Alcotest.(check bool) "outside: nothing solid" false (Tilemap.hits wall map 100. 100. 10. 10.)

let rec count (shape : Playground.shape) : int =
  match shape.form with
  | Group shapes -> List.fold_left (fun acc s -> acc + count s) 0 shapes
  | _ -> 1

let test_view () =
  let tile _ = Playground.square Playground.black 10. in
  Alcotest.(check int) "all but the empty ' ' tiles" 18 (count (Tilemap.view tile map));
  let big = Tilemap.of_strings 50. (List.init 20 (fun _ -> String.make 200 '#')) in
  let screen = Playground.to_screen 1000. 1000. in
  let cam : Camera2d.t = { x = 1234.; y = 0.; zoom = 1. } in
  Alcotest.(check int) "culling: 21x20 of 4000" (21 * 20)
    (count (Tilemap.view_visible (Camera2d.visible screen cam) tile big))

let tests =
  Testo.categorize "Tilemap"
    [
      t "size" test_size;
      t "cells" test_cells;
      t "find and set" test_find_set;
      t "hits" test_hits;
      t "view and view_visible" test_view;
    ]
