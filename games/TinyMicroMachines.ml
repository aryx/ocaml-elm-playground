(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Micro Machines (Codemasters, 1991, after Galoob's toy
 * cars): tiny cars racing seen from above, on a breakfast table, among
 * the cereal bowls, and off its edge if you're not careful. Red on the
 * arrows; blue on w/a/s/d, or the computer. v changes the camera.
 *
 * Its head-to-head mode is the one here, and it's a lovely rule: one
 * screen, the camera on the leader, and when a car falls behind so far
 * that it leaves the screen, the leader wins a point, and both restart
 * side by side. No laps, no finish line: first to 4 points. The race is
 * about the screen's edge.
 *
 * The cars slide: a car goes where it points only little by little
 * (its velocity turns towards its heading by a fraction each frame, the
 * "grip", the racing kit's Topdown.drive), so fast turns drift, the feel
 * of toy cars on a polished table. Off the track, on the table, they're
 * slower; off the table, they fall, and come back on the track after a
 * moment.
 *
 * The track is its waypoints ([waypoints]): the road's cells are drawn
 * between them (a Tilemap), the computer drives from one to the next
 * (Topdown.computer), and who leads is who's further along them.
 *
 * The camera (Camera2d) follows the leader, looking ahead of it, where
 * it goes: Micro Machines' own trick, since what matters is what's
 * coming; north stays up. v turns the camera with the leader instead
 * (Camera2d's angle): its car always points up and the table turns
 * under it -- the view from the driving seat, flattened. Compare: the
 * fixed one is easier to read, which is why the original kept it.
 *)
open Playground

(*****************************************************************************)
(* The table and the track *)
(*****************************************************************************)

let tile = 100.
let cols = 28
let rows = 22

(* the track's center line, cell after cell, a loop *)
let waypoints : (int * int) list =
  [ (3, 3); (24, 3); (24, 10); (14, 10); (14, 16); (24, 16); (24, 19); (3, 19); (3, 12); (9, 12); (9, 7); (3, 7) ]

(* the road: every cell between two waypoints, and the one next to it
 * (a road two cells wide), on the table's cells ('.') *)
let table : Tilemap.t =
  let map = Tilemap.of_strings tile (List.init rows (fun _ -> String.make cols '.')) in
  let n = List.length waypoints in
  List.fold_left
    (fun map i ->
      let c1, r1 = List.nth waypoints i and c2, r2 = List.nth waypoints ((i + 1) mod n) in
      let cells =
        if r1 = r2 then List.init (abs (c2 - c1) + 1) (fun k -> (min c1 c2 + k, r1))
        else List.init (abs (r2 - r1) + 1) (fun k -> (c1, min r1 r2 + k))
      in
      List.fold_left (fun m (c, r) -> Tilemap.set (Tilemap.set m c r '#') (c + 1) (r + 1) '#' |> fun m ->
        Tilemap.set (Tilemap.set m (c + 1) r '#') c (r + 1) '#') map cells)
    map
    (List.init n Fun.id)
  |> fun m -> Tilemap.set (Tilemap.set m 3 5 'S') 4 5 'S'

let bounds = Tilemap.bounds table

(* the waypoints in the world: the centers of their 2x2 cells; passed
 * within 160, the corner cut from 350 *)
let track : Topdown.track =
  let point (c, r) = let x, y = Tilemap.center table c r in (x +. (tile /. 2.), y -. (tile /. 2.)) in
  { points = Array.of_list (List.map point waypoints); reach = 160.; corner = 350. }

let on_road (x : number) (y : number) : bool = match Tilemap.tile_at table x y with Some ('#' | 'S') -> true | _ -> false
let on_table (x : number) (y : number) : bool = Tilemap.tile_at table x y <> None

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type car = { body : Topdown.t; falling : int (* > 0: fallen off the table, for that long *) }

type race = {
  red : car;
  blue : car;
  red_points : int;
  blue_points : int;
  cam : Camera2d.t;
  ready : int; (* > 0: waiting before the start, "READY" *)
  computer : bool;
}

type scene = Title | Racing of race | Winner of race
type model = { scenes : scene Scene2d.t; turning : bool (* the camera turns with the leader *) }

let points_to_win = 4

(* a car on waypoint [i], facing the next one, [side] across the road *)
let car_at (i : int) (side : number) : car = { body = Topdown.start track i side; falling = 0 }

let restart (r : race) (i : int) : race =
  let red = car_at i 35. and blue = car_at i (-35.) in
  { r with red; blue; ready = 60; cam = { r.cam with x = red.body.x; y = red.body.y } }

let new_race (computer : bool) : race =
  restart
    { red = car_at 0 0.; blue = car_at 0 0.; red_points = 0; blue_points = 0; cam = Camera2d.origin; ready = 0; computer }
    0

let initial_model = { scenes = Scene2d.start Title; turning = false }

(*****************************************************************************)
(* Driving *)
(*****************************************************************************)

(* One frame of a car, [gas] from -1 (reverse) to 1, [steer] -1 (right)
 * to 1 (left): Topdown's toy car, 700 fast on the road, 300 on the
 * table; falling once off it *)
let drive (gas : number) (steer : number) (c : car) : car =
  if c.falling > 0 then c
  else
    let top = if on_road c.body.x c.body.y then 700. else 300. in
    let body = Topdown.drive Topdown.toy top gas steer c.body |> Topdown.follow track in
    if on_table body.x body.y then { c with body } else { body; falling = 60 }

(* back on the track after a fall: at the last waypoint passed *)
let recover (c : car) : car =
  if c.falling = 1 then { body = { (Topdown.start track (c.body.next - 1) 0.) with next = c.body.next }; falling = 0 }
  else if c.falling > 0 then { c with falling = c.falling - 1 }
  else c

let progress (c : car) : number = Topdown.progress track c.body

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

let update_race (screen : screen) (k : keyboard) (turning : bool) (r : race) : race =
  if r.ready > 0 then { r with ready = r.ready - 1 }
  else
    let red = drive (axis k.kup k.kdown) (axis k.kleft k.kright) r.red |> recover in
    let blue =
      if r.computer then (let gas, steer = Topdown.computer track r.blue.body in drive gas steer r.blue) |> recover
      else drive (axis k.kw k.ks) (axis k.ka k.kd) r.blue |> recover
    in
    let leader = if progress red >= progress blue then red else blue in
    let l = leader.body in
    (* the camera: on the leader, looking ahead of it (a third of a
     * second of its velocity), smoothed; turned with it if asked *)
    let cam = Camera2d.follow 0.1 (l.x +. (0.35 *. l.vx)) (l.y +. (0.35 *. l.vy)) r.cam in
    let cam = Camera2d.turn_toward 0.08 (if turning then l.heading -. 90. else 0.) cam in
    let r = { r with red; blue; cam } in
    (* the head-to-head rule: the other car off the screen (or off the
     * table), a point for the leader, and a new start where it is *)
    let trailing = if leader == red then blue else red in
    let sx, sy = Camera2d.to_screen cam trailing.body.x trailing.body.y in
    let off = Float.abs sx > (screen.width /. 2.) +. 30. || Float.abs sy > (screen.height /. 2.) +. 30. || trailing.falling > 0 in
    if not off then r
    else
      let r = if leader == red then { r with red_points = r.red_points + 1 } else { r with blue_points = r.blue_points + 1 } in
      restart r (l.next - 1)

let update (computer : computer) (m : model) : model =
  let s = Scene2d.update computer m.scenes in
  let key name = Scene2d.pressed (fun k -> Set_.mem name k.keys) s in
  let turning = if key "v" then not m.turning else m.turning in
  let scenes =
    match s.scene with
    | Title ->
        if key "1" then Scene2d.go (Racing (new_race true)) s
        else if key "2" then Scene2d.go (Racing (new_race false)) s
        else s
    | Racing r ->
        let r = update_race computer.screen computer.keyboard turning r in
        if r.red_points >= points_to_win || r.blue_points >= points_to_win then Scene2d.go (Winner r) s
        else { s with scene = Racing r }
    | Winner _ -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go Title s else s
  in
  { scenes; turning }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let red_car_color = rgb 220 30 30
let blue_car_color = rgb 30 90 220

(* a toy car from above, pointing up: body, windscreen, wheels *)
let car_shape (color : color) : shape =
  Sprite.pixels 4.
    [ ('B', color); ('W', rgb 170 220 250); ('K', rgb 20 20 20); ('L', rgb 255 240 150) ]
    [ ".LBBBBL."; "KBBBBBBK"; "KBBBBBBK"; ".BWWWWB."; ".BWWWWB."; ".BBBBBB."; ".BBBBBB."; ".BWWWWB.";
      "KBBBBBBK"; "KBBBBBBK"; ".BBBBBB." ]

let view_car (color : color) (c : car) : shape =
  let shape = car_shape color |> rotate (c.body.heading -. 90.) |> move c.body.x c.body.y in
  if c.falling > 0 then shape |> scale (float_of_int c.falling /. 60.) else shape

(* the breakfast table: wood, the cereal bowls, a spoon, a glass *)
let decor : shape list =
  let bowl x y = group [ circle (rgb 240 240 240) 110.; circle (rgb 250 230 170) 90.; circle (rgb 220 170 70) 20. |> move 20. 10.; circle (rgb 220 170 70) 18. |> move (-25.) (-15.) ] |> move x y in
  let grain i = rectangle (rgb 175 125 75) (float_of_int cols *. tile) 6. |> move_y (bounds.bottom +. 60. +. (float_of_int i *. 170.)) in
  [ rectangle (rgb 190 140 90) (float_of_int cols *. tile) (float_of_int rows *. tile) ]
  @ List.init 13 grain
  @ [ bowl (-400.) 300.; bowl 700. (-300.); bowl (-1000.) (-850.);
      group [ oval (rgb 200 200 210) 60. 90.; rectangle (rgb 200 200 210) 26. 260. |> move_y (-160.) ] |> rotate 30. |> move 900. 650.;
      circle (rgb 200 230 250) 70. |> move (-1150.) 800. ]

let road (c : char) : shape =
  match c with
  | '#' -> square (rgb 110 110 115) tile
  | 'S' -> group [ square white tile; square black (tile /. 2.) |> move (-25.) 25.; square black (tile /. 2.) |> move 25. (-25.) ]
  | _ -> group []

let text color size str = words color str |> scale size

let lights (color : color) (n : int) : shape list =
  List.init points_to_win (fun i -> circle (if i < n then color else rgb 60 60 60) 12. |> move (float_of_int i *. 32.) 0.)

let view_race (screen : screen) (turning : bool) (r : race) : shape list =
  let world = decor @ [ Tilemap.view_visible (Camera2d.visible screen r.cam) road table; view_car blue_car_color r.blue; view_car red_car_color r.red ] in
  [ rectangle (rgb 60 50 45) screen.width screen.height; Camera2d.view r.cam world ]
  @ List.map (move (screen.left +. 40.) (screen.top -. 40.)) (lights red_car_color r.red_points)
  @ List.map (move (screen.right -. 140.) (screen.top -. 40.)) (lights blue_car_color r.blue_points)
  @ [ text white 2. ("v: camera " ^ if turning then "(turning)" else "(fixed)") |> move_y (screen.bottom +. 25.) ]
  @ if r.ready > 0 then [ text yellow 6. "READY" ] else []

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  let s = m.scenes in
  match s.scene with
  | Title ->
      [ rectangle (rgb 190 140 90) screen.width screen.height;
        text red_car_color 7. "TINY MICRO MACHINES" |> move_y 250.;
        car_shape red_car_color |> scale 3. |> rotate (-20.) |> move (-80.) 80.;
        car_shape blue_car_color |> scale 3. |> rotate 20. |> move 80. 80.;
        text white 3. "1: against the computer   2: two players" |> move_y (-60.);
        text white 2.5 "red: arrows   blue: w/a/s/d   v: camera" |> move_y (-120.);
        text white 2.5 "leave the other car off the screen to win a point" |> move_y (-170.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS 1 OR 2" |> move_y (-260.) ]
  | Racing r -> view_race screen m.turning r
  | Winner r ->
      view_race screen m.turning r
      @ [ (if r.red_points > r.blue_points then text red_car_color 7. "RED WINS!"
           else text blue_car_color 7. (if r.computer then "THE COMPUTER WINS!" else "BLUE WINS!"))
          |> move_y 150. ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-150.) ]

let app = game view update initial_model

let main = Playground_platform.run_app app
