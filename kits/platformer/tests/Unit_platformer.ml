(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* kits/platformer: Tile_move and Ladder *)

let t = Testo.create
let pt = Alcotest.(pair (float 1e-9) (float 1e-9))
let solid c = c = '#'
let is_ladder c = c = 'H'

(* move_by's worked example: tiles of 10, a wall from x = 50; a 20 x 20
 * box at (0, 0) moving (100, 0) stops at (40, 0) *)
let test_move_by () =
  (* 14 columns centered on 0, from x = -70: the 13th, the first '#',
   * from x = 50 *)
  let map = Tilemap.of_strings 10. [ "            ##"; "            ##"; "            ##"; "            ##" ] in
  let (x, y), hit = Tile_move.move_by solid map (20., 20.) (0., 0.) (100., 0.) in
  Alcotest.check pt "stopped" (40., 0.) (x, y);
  Alcotest.(check bool) "hit" true hit

(* climb's worked example: the ladder on the floor, up from its foot to
 * its top, where it stands; next to it, above nothing, it doesn't *)
let test_ladder () =
  let map = Tilemap.of_strings 10. [ "    "; "  H "; "  H "; "####" ] in
  Alcotest.check pt "at the top" (5., 15.) (Ladder.climb solid is_ladder map (6., 10.) (5., -5.) 100.);
  Alcotest.(check bool) "standing on the top" true (Ladder.standing solid is_ladder map (6., 10.) 5. 15.);
  Alcotest.(check bool) "not in the air" false (Ladder.standing solid is_ladder map (6., 10.) (-15.) 15.);
  Alcotest.(check bool) "on top" true (Ladder.on_top is_ladder map (6., 10.) 5. 15.);
  Alcotest.(check bool) "on it, not on top" false (Ladder.on_top is_ladder map (6., 10.) 5. 0.);
  Alcotest.check pt "down to the floor" (5., -5.) (Ladder.climb solid is_ladder map (6., 10.) (5., 15.) (-100.));
  Alcotest.check pt "half a tile off, still climbs" (5., 15.) (Ladder.climb solid is_ladder map (6., 10.) (8., -5.) 100.)

let tests = Testo.categorize "kit_platformer" [ t "Tile_move, move_by" test_move_by; t "Ladder, climb" test_ladder ]
