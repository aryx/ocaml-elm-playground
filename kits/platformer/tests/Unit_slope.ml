(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* kits/platformer/Slope: the tiles' shapes, the sensors, and a hero
 * carried round a loop by the modes alone *)

let t = Testo.create
let size = 16

(* a world of one tile kind at the tiles [where] says *)
let world (where : int * int -> Slope.surface option) = Slope.ground ~tiles:where ~size

let test_tiles () =
  let s = Slope.slope size ~from_:0 ~to_:16 in
  Alcotest.(check (float 1e-6)) "a 45 degrees hill" 45. s.angle;
  (* the first column is nearly empty, the last nearly full *)
  let column x = List.length (List.filter (fun y -> s.solid.((y * size) + x)) (List.init size Fun.id)) in
  Alcotest.(check int) "the left column" 1 (column 0);
  Alcotest.(check int) "the right column" 16 (column 15);
  Alcotest.(check (float 1e-6)) "a flat tile is 0 degrees" 0. (Slope.slope size ~from_:16 ~to_:16).angle;
  Alcotest.(check (float 1e-6)) "a gentle one" 26.565051177077997 (Slope.slope size ~from_:0 ~to_:8).angle;
  Alcotest.(check bool) "a block is all solid" true (Array.for_all Fun.id (Slope.block size).solid);
  Alcotest.(check bool) "the sky is empty" true (Array.for_all not (Slope.empty size).solid)

let test_modes () =
  Alcotest.(check bool) "flat: a floor" true (Slope.mode_of 0. = Floor);
  Alcotest.(check bool) "a slope is still a floor" true (Slope.mode_of 30. = Floor);
  Alcotest.(check bool) "a quarter turn: a wall" true (Slope.mode_of 90. = Right_wall);
  Alcotest.(check bool) "upside down: the ceiling" true (Slope.mode_of 180. = Ceiling);
  Alcotest.(check bool) "three quarters: the other wall" true (Slope.mode_of 270. = Left_wall);
  Alcotest.(check bool) "and round again" true (Slope.mode_of 350. = Floor);
  Alcotest.(check (pair (float 0.) (float 0.))) "down on a floor" (0., -1.) (Slope.down Floor);
  Alcotest.(check (pair (float 0.) (float 0.))) "down on a right wall" (1., 0.) (Slope.down Right_wall)

(* the ground is the tiles below y = 0, flat *)
let flat_world (_tx, ty) = if ty < 0 then Some (Slope.block size) else None

let test_sensor () =
  let ground = world flat_world in
  (* above the ground: the sensor looks down and finds it *)
  (match ground Floor (8., 6.) with
  | Some (y, angle) ->
      Alcotest.(check (float 0.)) "the feet rest at 0" 0. y;
      Alcotest.(check (float 0.)) "flat" 0. angle
  | None -> Alcotest.fail "the sensor found nothing");
  (* inside the ground: the sensor walks back out of it *)
  (match ground Floor (8., -5.) with
  | Some (y, _) -> Alcotest.(check (float 0.)) "pushed back up to 0" 0. y
  | None -> Alcotest.fail "the sensor found nothing");
  (* too high above it: nothing within a tile *)
  Alcotest.(check bool) "nothing that far up" true (ground Floor (8., 40.) = None);
  (* a hill: its surface rises with x *)
  let hill (tx, ty) = if ty < 0 then Some (Slope.block size) else if ty = 0 && tx = 0 then Some (Slope.slope size ~from_:0 ~to_:16) else None in
  let at x = match world hill Floor (x, 8.) with Some (y, a) -> (y, a) | None -> Alcotest.fail "no ground on the hill" in
  let y0, a0 = at 1. and y1, a1 = at 14. in
  Alcotest.(check bool) "it rises" true (y1 > y0);
  Alcotest.(check (float 1e-6)) "at 45 degrees all along" a0 a1;
  Alcotest.(check (float 1e-6)) "the angle is the tile's" 45. a0

(* a loop of [ring] tiles: a hero placed all around it, in the mode its
 * angle asks for, always finds the ground under its feet -- no case for
 * the loop anywhere *)
let test_loop () =
  let radius = 48. in
  let center = (0., 0.) in
  (* the loop: a ring of ground one tile thick, sky on both sides *)
  let tiles (tx, ty) =
    let cx = fst center -. float_of_int (tx * size) and cy = snd center -. float_of_int (ty * size) in
    Some (Slope.ring size ~cx ~cy ~radius ~thickness:(float_of_int size) ~inside:true)
  in
  List.iter
    (fun degrees ->
      let a = degrees *. Float.pi /. 180. in
      (* the hero just inside the ring, its feet toward it *)
      let x = fst center +. ((radius -. 6.) *. cos a) and y = snd center +. ((radius -. 6.) *. sin a) in
      (* the mode it walks in there: the wall is straight out from the
       * center, so "down" for the hero points that way *)
      let out = (cos a, sin a) in
      let mode =
        List.fold_left
          (fun best m ->
            let bx, by = Slope.down m and cx, cy = Slope.down best in
            if (bx *. fst out) +. (by *. snd out) > (cx *. fst out) +. (cy *. snd out) then m else best)
          Slope.Floor [ Slope.Floor; Slope.Right_wall; Slope.Ceiling; Slope.Left_wall ]
      in
      match Slope.ground ~tiles ~size mode (x, y) with
      | Some _ -> ()
      | None -> Alcotest.failf "no ground at %g degrees round the loop" degrees)
    [ 0.; 45.; 90.; 135.; 180.; 225.; 270.; 315. ]

let tests =
  Testo.categorize "Slope"
    [ t "the tiles' shapes and angles" test_tiles; t "the four modes" test_modes; t "the sensors" test_sensor; t "a loop, mode by mode" test_loop ]
