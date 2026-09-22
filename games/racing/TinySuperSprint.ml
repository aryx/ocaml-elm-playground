(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Super Sprint (Atari Games, 1986): four little cars
 * on one screen, a figure eight with a bridge, three laps. Red on the
 * arrows, blue on w/a/s/d (or a drone); the other cars are drones, the
 * computer's.
 *
 * Twelve years after Gran Trak 10 (TinyGranTrak10.ml), the same view
 * from above, but crowded: the cars bump each other ([Topdown.push]),
 * bounce off the walls instead of crashing ([Topdown.bounce]), and
 * the track crosses itself. That is the picture everybody remembers:
 * the road going under a bridge and coming back over it.
 *
 *              \\ //              the car on the bridge is drawn after
 *            ===\\/===  bridge    the deck, the one under it before:
 *            ====\/===            hidden. And each car's walls are
 *               //\\              measured only against the segments
 *              //  \\             around it (Topdown.distance_from):
 *                                 at the crossing, the other road
 *                                 doesn't exist for it, it goes on.
 *
 * Which of the two it is on is only which segment it's on: the car
 * passing waypoint 3 is on the bridge until it passes waypoint 4. Two
 * cars on different levels don't bump.
 *
 * The golden wrenches: one on the track at a time, taken by whoever
 * drives over it (drones too). After each race, three wrenches buy an
 * upgrade: traction (the grip), acceleration, top speed -- red with
 * left, up, right, blue with a, w, d. The drones get a level better
 * every race, so a player who doesn't pick up wrenches falls behind:
 * the arcade's way to make you play for the next race, not only win
 * this one.
 *
 * What it uses: the racing kit's Topdown -- the car ([drive]), the
 * waypoints (laps, places, the drones' driving: [computer]), the walls
 * ([distance_from], [bounce]), the bumps ([push]) and the road's
 * drawing ([ribbon]); Scene2d for the title, the race and the results;
 * Sprite for the cars; Audio for the wrench and the knocks. No
 * Camera2d but its zoom: one screen, fitted to the window.
 *
 * What it doesn't: Super Sprint's eight tracks and their shortcuts
 * behind gates that open and close, its oil and water, its tornadoes,
 * the car that explodes on a wall too fast and is brought back by a
 * helicopter. One track, and the bridge.
 *
 * Exercises: a second track, and choosing it; the helicopter; a gate
 * across a shortcut, opening and closing every few seconds; a third
 * player (the cabinet had three wheels).
 *)
open Playground

(*****************************************************************************)
(* The track *)
(*****************************************************************************)

let world_width = 1000.
let world_height = 640.

let road_width = 100.

(* a figure eight, the left loop first, from the start halfway down its
 * straight (the grid behind it on the straight too); the segment from
 * waypoint 3 to 4 goes over the bridge, the one from 9 to 10 under it,
 * both through (0, 0) *)
let track : Topdown.track =
  { points =
      [| (-430., 60.); (-430., -150.); (-300., -230.); (-160., -160.); (160., 160.); (300., 230.); (430., 150.);
         (430., -150.); (300., -230.); (160., -160.); (-160., 160.); (-300., 230.); (-430., 150.) |];
    reach = 90.;
    corner = 220. }

let bridge = 3
let segments = Array.length track.points

(* the segment the car is on, and whether that's the bridge *)
let on_bridge (c : Topdown.t) : bool = (c.next - 1) mod segments = bridge

(* a wall: further than half the road from the segments around the car
 * (the one before, its own, the one after) *)
let in_wall (c : Topdown.t) (x : number) (y : number) : bool =
  Topdown.distance_from track (c.next - 2) 3 x y > (road_width /. 2.) -. 12.

(* where the wrenches appear, one after the other *)
let wrench_spots = [| (430., 0.); (-230., -195.); (365., 190.); (-430., -60.); (365., -190.); (-365., 190.) |]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type upgrades = { traction : int; accel : int; top : int }

type driver = Red | Blue | Drone

type car = { body : Topdown.t; driver : driver; color : color; wrenches : int; up : upgrades }

type race = {
  cars : car list;
  wrench : int; (* the spot the wrench is on *)
  number : int; (* the race, the first 1 *)
  ready : int; (* > 0: waiting before the start *)
}

type scene = Title | Racing of race | Results of race
type model = scene Scene2d.t

let laps = 3
let max_level = 4

(* the grid: two by two behind the start line, [row] rows back *)
let grid (row : int) (side : number) : Topdown.t =
  let c = Topdown.start track 0 side in
  let a = c.heading *. Float.pi /. 180. and back = 30. +. (60. *. float_of_int row) in
  { c with x = c.x -. (back *. cos a); y = c.y -. (back *. sin a) }

let none = { traction = 0; accel = 0; top = 0 }

let first_race (two_players : bool) : race =
  let car driver color = { body = Topdown.start track 0 0.; driver; color; wrenches = 0; up = none } in
  { cars =
      [ car Red (rgb 220 40 40); car (if two_players then Blue else Drone) (rgb 40 90 230); car Drone (rgb 240 200 40);
        car Drone (rgb 40 170 80) ];
    wrench = 0;
    number = 0;
    ready = 0 }

(* the next race: the cars back on the grid, the drones a level up *)
let next_race (r : race) : race =
  let n = r.number + 1 in
  let level = min max_level (n - 1) in
  let place i (c : car) =
    let up = if c.driver = Drone then { traction = level; accel = level; top = level } else c.up in
    { c with body = grid (i / 2) (if i mod 2 = 0 then 25. else -25.); up }
  in
  { r with cars = List.mapi place r.cars; number = n; ready = 90 }

let initial_model = Scene2d.start Title

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

let knock = Audio.sfx { Sfx.hit with decay = 0.08; volume = 0.3 }

(* the car's feel: its upgrades on top of a toy car; its top speed *)
let params (u : upgrades) : Topdown.params =
  { Topdown.toy with accel = 700. +. (150. *. float_of_int u.accel); grip = 0.09 +. (0.03 *. float_of_int u.traction) }

let top_speed (u : upgrades) : number = 430. +. (50. *. float_of_int u.top)

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

let controls (k : keyboard) (c : car) : number * number =
  match c.driver with
  | Red -> (axis k.kup k.kdown, axis k.kleft k.kright)
  | Blue -> (axis k.kw k.ks, axis k.ka k.kd)
  | Drone -> Topdown.computer track c.body

let drive (k : keyboard) (c : car) : car =
  let gas, steer = controls k c in
  let before = c.body in
  let after = Topdown.drive (params c.up) (top_speed c.up) gas steer before in
  let body = Topdown.bounce (in_wall before) before after |> Topdown.follow track in
  if body.speed <> after.speed && Float.abs after.speed > 250. && c.driver <> Drone then Audio.play knock;
  { c with body }

(* every pair of cars on the same level, pushed apart *)
let bump (cars : car list) : car list =
  let a = Array.of_list cars in
  for i = 0 to Array.length a - 1 do
    for j = i + 1 to Array.length a - 1 do
      if on_bridge a.(i).body = on_bridge a.(j).body then begin
        let bi, bj = Topdown.push 14. a.(i).body a.(j).body in
        a.(i) <- { (a.(i)) with body = bi };
        a.(j) <- { (a.(j)) with body = bj }
      end
    done
  done;
  Array.to_list a

(* the wrench, to the first car over it *)
let pick_wrench (r : race) : race =
  let wx, wy = wrench_spots.(r.wrench mod Array.length wrench_spots) in
  match List.find_opt (fun (c : car) -> Float.hypot (c.body.x -. wx) (c.body.y -. wy) < 30.) r.cars with
  | None -> r
  | Some taker ->
      if taker.driver <> Drone then Audio.play Audio.coin;
      { r with
        wrench = r.wrench + 1;
        cars = List.map (fun c -> if c == taker then { c with wrenches = c.wrenches + 1 } else c) r.cars }

let progress (c : car) : number = Topdown.progress track c.body

(* the cars, the leader first *)
let places (r : race) : car list = List.stable_sort (fun a b -> compare (progress b) (progress a)) r.cars

let update_race (k : keyboard) (r : race) : race =
  if r.ready > 0 then { r with ready = r.ready - 1 }
  else pick_wrench { r with cars = List.map (drive k) r.cars |> bump }

let finished (r : race) : bool = List.exists (fun (c : car) -> Topdown.lap track c.body >= laps) r.cars

(* three wrenches for a level, if there's one left *)
let buy (field : upgrades -> int) (set : upgrades -> upgrades) (c : car) : car =
  if c.wrenches >= 3 && field c.up < max_level then { c with wrenches = c.wrenches - 3; up = set c.up } else c

let shop (pressed : string -> bool) (c : car) : car =
  let left, up, right = match c.driver with Red -> ("ArrowLeft", "ArrowUp", "ArrowRight") | Blue -> ("a", "w", "d") | Drone -> ("", "", "") in
  if c.driver = Drone then c
  else if pressed left then buy (fun u -> u.traction) (fun u -> { u with traction = u.traction + 1 }) c
  else if pressed up then buy (fun u -> u.accel) (fun u -> { u with accel = u.accel + 1 }) c
  else if pressed right then buy (fun u -> u.top) (fun u -> { u with top = u.top + 1 }) c
  else c

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let pressed name = Scene2d.pressed (fun k -> Set_.mem name k.keys) s in
  match s.scene with
  | Title ->
      if pressed "1" then Scene2d.go (Racing (next_race (first_race false))) s
      else if pressed "2" then Scene2d.go (Racing (next_race (first_race true))) s
      else s
  | Racing r ->
      let r = update_race computer.keyboard r in
      if finished r then Scene2d.go (Results r) s else { s with scene = Racing r }
  | Results r ->
      if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (Racing (next_race r)) s
      else { s with scene = Results { r with cars = List.map (shop pressed) r.cars } }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let car_shape (color : color) : shape =
  Sprite.pixels 3.5
    [ ('B', color); ('W', rgb 170 220 250); ('K', rgb 20 20 20) ]
    [ ".BBBBBB."; "KBBBBBBK"; "KBBBBBBK"; ".BWWWWB."; ".BBBBBB."; ".BBBBBB."; ".BWWWWB."; "KBBBBBBK"; "KBBBBBBK"; ".BBBBBB." ]

let view_car (c : car) : shape = car_shape c.color |> rotate (c.body.heading -. 90.) |> move c.body.x c.body.y

let wrench_shape : shape =
  Sprite.pixels 3. [ ('Y', rgb 250 210 40) ] [ "Y...Y"; "YY.YY"; ".YYY."; "..Y.."; "..Y.."; "..Y.."; ".YYY."; ".Y.Y." ]

(* the deck over the crossing, along the bridge's segment, its rails
 * red *)
let deck : shape =
  let length = 250. and width = road_width +. 24. in
  group
    [ rectangle (rgb 30 70 30) length width |> move 8. (-8.);
      rectangle (rgb 120 120 128) length width;
      rectangle (rgb 200 50 40) length 6. |> move_y (width /. 2.);
      rectangle (rgb 200 50 40) length 6. |> move_y (-.width /. 2.) ]
  |> rotate 45.

let ground : shape list =
  let x, y = Topdown.point track 0 in
  [ rectangle (rgb 70 150 60) world_width world_height;
    Topdown.ribbon (rgb 235 235 235) (road_width +. 12.) track;
    Topdown.ribbon (rgb 85 85 92) (road_width -. 4.) track;
    group
      (List.init 10 (fun i ->
           square (if i mod 2 = 0 then white else black) 10. |> move ((float_of_int i *. 10.) -. 45.) 0.))
    |> move x y ]

let text color size str = words color str |> scale size

let view_race (r : race) : shape list =
  let wx, wy = wrench_spots.(r.wrench mod Array.length wrench_spots) in
  let below, above = List.partition (fun (c : car) -> not (on_bridge c.body)) r.cars in
  ground
  @ [ wrench_shape |> move wx wy ]
  @ List.map view_car below
  @ [ deck ]
  @ List.map view_car above
  @ List.concat
      (List.mapi
         (fun i (c : car) ->
           let x = -330. +. (float_of_int i *. 220.) in
           [ square c.color 16. |> move (x -. 60.) 300.;
             text white 2.
               (Printf.sprintf "L%d  %dW" (min laps (Topdown.lap track c.body + 1)) c.wrenches)
             |> move (x +. 5.) 300. ])
         r.cars)
  @ if r.ready > 0 then [ text yellow 6. (if r.ready > 30 then "READY" else "GO!") ] else []

let place_names = [| "1ST"; "2ND"; "3RD"; "4TH" |]

let view_results (s : model) (r : race) : shape list =
  let driver (c : car) = match c.driver with Red -> "RED" | Blue -> "BLUE" | Drone -> "DRONE" in
  let bar n = String.make n '#' ^ String.make (max_level - n) '.' in
  let row i (c : car) =
    let y = 120. -. (float_of_int i *. 55.) in
    [ square c.color 24. |> move (-360.) y;
      text white 2.5 (Printf.sprintf "%s  %-5s" place_names.(i) (driver c)) |> move (-230.) y;
      text white 2.
        (Printf.sprintf "%d wrenches   traction %s  accel %s  top %s" c.wrenches (bar c.up.traction) (bar c.up.accel)
           (bar c.up.top))
      |> move 140. y ]
  in
  view_race r
  @ [ rectangle (rgb 20 20 30) 860. 440. |> move_y 20.; text yellow 4. (Printf.sprintf "RACE %d" r.number) |> move_y 200. ]
  @ List.concat (List.mapi row (places r))
  @ [ text white 2. "3 wrenches: an upgrade -- red: left, up, right   blue: a, w, d" |> move_y (-120.) ]
  @ Scene2d.blink 1. s [ text yellow 3. "SPACE: NEXT RACE" |> move_y (-170.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  let zoom = Float.min (screen.width /. world_width) (screen.height /. world_height) in
  let cam = { Camera2d.origin with zoom } in
  [ rectangle black screen.width screen.height ]
  @
  match s.scene with
  | Title ->
      [ Camera2d.view cam
          (ground
          @ [ deck;
              rectangle (rgb 20 20 30) 820. 300. |> move_y 10.;
              text (rgb 250 210 40) 7. "TINY SUPER SPRINT" |> move_y 110.;
              text white 3. "1: against the drones   2: two players" |> move_y 30.;
              text white 2.5 "red: arrows   blue: w/a/s/d" |> move_y (-20.);
              text white 2.5 "pick up the golden wrenches, three laps" |> move_y (-60.) ]
          @ Scene2d.blink 1. s [ text yellow 3. "PRESS 1 OR 2" |> move_y (-110.) ]) ]
  | Racing r -> [ Camera2d.view cam (view_race r) ]
  | Results r -> [ Camera2d.view cam (view_results s r) ]

let app = game view update initial_model

let main = Playground_platform.run_app app
