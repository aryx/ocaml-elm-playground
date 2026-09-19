(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* kits/sectors: Sectors *)

let t = Testo.create

let square loops : Sectors.sector =
  { floor = 0.; ceiling = 100.; light = 1.; floor_rgb = (0, 0, 0); ceiling_rgb = (0, 0, 0); wall_rgb = (0, 0, 0); loops }

let test_make () =
  let lv = Sectors.make [ square [ Sectors.rect 0. 0. 10. 10. ]; square [ Sectors.rect 10. 0. 20. 10. ] ] ~start:(5., 5., 0.) ~exit:1 in
  Alcotest.(check int) "7 lines" 7 (Array.length lv.lines);
  let two_sided = List.filter (fun (l : Sectors.line) -> l.back <> None) (Array.to_list lv.lines) in
  Alcotest.(check (list (list (float 0.)))) "one two-sided, the first square in front" [ [ 10.; 10.; 10.; 0.; 0.; 1. ] ]
    (List.map (fun (l : Sectors.line) -> [ l.x1; l.y1; l.x2; l.y2; float_of_int l.front; float_of_int (Option.get l.back) ]) two_sided);
  let lv = Sectors.make [ square [ Sectors.rect 0. 0. 10. 10. ]; square [ Sectors.rect 10. 0. 20. 5. ] ] ~start:(5., 5., 0.) ~exit:1 in
  Alcotest.(check int) "cut: 8 lines" 8 (Array.length lv.lines);
  Alcotest.(check int) "one two-sided" 1 (Array.length (Array.of_list (List.filter (fun (l : Sectors.line) -> l.back <> None) (Array.to_list lv.lines))))

let test_outpost () =
  let lv = Sectors.outpost in
  let x, y, _ = lv.start in
  Alcotest.(check (list int)) "sectors" [ 0; 1; 13; 12; 6; 14 ]
    [ Sectors.sector_at lv x y; Sectors.sector_at lv 384. 256.; Sectors.sector_at lv 1056. 232.; Sectors.sector_at lv 1000. 300.;
      Sectors.sector_at lv 384. 900.; Sectors.sector_at lv 784. 256. ];
  (* every two-sided line has its two sectors on its two sides *)
  Array.iter
    (fun (l : Sectors.line) ->
      let mx = (l.x1 +. l.x2) /. 2. and my = (l.y1 +. l.y2) /. 2. in
      (* 1 unit to the right of the line, and to its left *)
      let nx = (l.y2 -. l.y1) /. Float.hypot (l.x2 -. l.x1) (l.y2 -. l.y1) and ny = -.(l.x2 -. l.x1) /. Float.hypot (l.x2 -. l.x1) (l.y2 -. l.y1) in
      Alcotest.(check int) "front on the right" l.front (Sectors.sector_at lv (mx +. nx) (my +. ny));
      Option.iter (fun b -> Alcotest.(check int) "back on the left" b (Sectors.sector_at lv (mx -. nx) (my -. ny))) l.back)
    lv.lines

(* walking by steps of 4 *)
let walk (x, y) (dx, dy) n =
  let lv = Sectors.outpost in
  let p = ref (x, y) in
  for _ = 1 to n do p := Sectors.move lv (Sectors.sector_at lv) !p (dx, dy) done;
  !p

let test_move () =
  Alcotest.(check (pair (float 0.) (float 0.))) "16 short of the pillar" (336., 256.) (walk (300., 256.) (4., 0.) 30);
  Alcotest.(check (pair (float 0.) (float 0.))) "the window's sill too high a step" (752., 256.) (walk (700., 256.) (4., 0.) 30);
  let _, y = walk (384., 480.) (0., 4.) 100 in
  Alcotest.(check bool) "up the stairs, upstairs" true (y > 800.)

let test_distance () =
  let l : Sectors.line = { x1 = 0.; y1 = 0.; x2 = 10.; y2 = 0.; front = 0; back = None } in
  Alcotest.(check (list (float 1e-9))) "to the segment" [ 5.; 5. ] [ Sectors.distance l 5. 5.; Sectors.distance l 13. 4. ]

let tests =
  Testo.categorize "kit_sectors" [ t "make" test_make; t "outpost" test_outpost; t "move" test_move; t "distance" test_distance ]
