(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Ignition (UDS, Virgin, 1997): three laps of a
 * country road, four toys racing on it -- a sports car, a police car, a
 * school bus -- seen from above, as in Micro Machines, but the world
 * in 3D: the road climbs and drops, and a ramp throws you in the air.
 * Left/right on the title to choose a vehicle, up/down for one or two
 * players, space to start; the arrows drive, and player 2 drives on
 * w/a/s/d, the screen split down the middle (Ignition's split was
 * vertical, not the usual top and bottom).
 *
 * TinyMicroMachines.ml draws the same kind of race flat: a car is a
 * point on a plane with a heading (the racing kit's Topdown), seen
 * from straight above. Ignition kept that view and that driving, and
 * gave the ground a height; that is all this game adds too, and the
 * lesson is how little it takes:
 *
 *   - the car is Topdown's, on Offroad's ground (the racing kit): the
 *     slope pulling it, the takeoff where the ground falls away faster
 *     than it would fall, walls where it is too steep;
 *   - the ground is one function of where a point is, and it is made
 *     from the road: the course is a closed spline (the racing kit's
 *     Track3d), [locate] gives any point's distance along it and across
 *     it, and the height is the road's hills near it, blended into
 *     the countryside away from it:
 *
 *        the road's hills            the countryside
 *        (hills, the ramps)          (fields, gentle)
 *     ___/'''\__/|___  ...blend...  ~~~~~~~~~~~~~~~~~~
 *        on the road     a few units away     far from it
 *
 *     so the road is always drivable, and the terrain meets it;
 *   - the view from above is what shows the height: the camera is
 *     tilted a little (not straight down), so a slope is a shade, a
 *     ramp a wedge with a side, a flying car higher than its shadow.
 *
 * Uses: the racing kit's Topdown (the driving, the waypoints, laps,
 * the computer's drivers), Offroad (the ground) and Track3d (the course
 * as a spline, the road drawn along it), Scene2d, and split3d for the
 * two players. Not the heightmap kit: the ground is a function, and the
 * mesh samples it; not Physics (the car is a point, its pitch and roll
 * only drawn).
 *
 * Exercises: the bus heavier than the cars (Topdown.push swaps the
 * velocities of equal masses); Ignition's other vehicles and tracks; a
 * shortcut off the road; the camera turning with the car.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The course *)
(*****************************************************************************)

(* half the road *)
let road = 7.

(* the course's (x, y) seen from above is the world's (x, -z): north
 * up (as in TinyBigRedRacing); Track3d is in the world's (x, z) *)
let at (x : number) (y : number) (h : number) : number * number * number = (x, h, -.y)

let ribbon : Track3d.t =
  Track3d.build
    (List.map
       (fun (x, y) -> Track3d.control ~width:road x (-.y))
       [ (50., 50.); (130., 35.); (210., 55.); (225., 120.); (180., 150.); (200., 210.); (120., 222.); (60., 190.); (85., 130.);
         (38., 100.) ])

let length = Track3d.length ribbon

(* The road's hills: a function of where, not of how far along. On a
 * curve's inside, the distance along the road ([locate]'s) jumps where
 * the nearest piece of the middle line changes, and a height made from
 * it has steps there, which the cars hop off (a headless race showed
 * them in the air most of the time) *)
let hills (x : number) (y : number) : number = 4. +. (3.5 *. sin (x /. 31.) *. cos (y /. 43.))

(* the ramps: a wedge up the road, 8 long, 1.6 high at its lip, on the
 * two straights (from 30 to 120 along, and from 370 to 440), where a
 * car at full speed lands on the road 30 further (from its lip it rises
 * at a fifth of its speed, 9 per second, and flies 0.6 s) *)
let ramps = [ 40.; 375. ]
let ramp_length = 8.
let ramp_height = 1.6

let ramp (s : number) : number =
  List.fold_left (fun h r -> if s >= r && s < r +. ramp_length then h +. (ramp_height *. (s -. r) /. ramp_length) else h) 0. ramps

(* the countryside, away from the road *)
let country (x : number) (y : number) : number =
  2. +. (2.5 *. sin (x /. 37.) *. cos (y /. 29.)) +. (1.5 *. sin ((x +. (2. *. y)) /. 53.))

let smooth (t : number) : number =
  let t = Float.max 0. (Float.min 1. t) in
  t *. t *. (3. -. (2. *. t))

(* The ground: the road's hills near it, the countryside away from it,
 * blended between 3 and 15 beyond the road's edge; the ramps on the
 * road itself *)
let height ?(ramps = true) (x : number) (y : number) : number =
  let s, off = Track3d.locate ribbon x (-.y) in
  let t = smooth ((Float.abs off -. road -. 3.) /. 12.) in
  let h = (hills x y *. (1. -. t)) +. (country x y *. t) in
  if ramps && Float.abs off < road then h +. ramp s else h

(* the world: a square of 256, the cars kept 4 inside it *)
let side = 256.
let ground : Offroad.ground = { height = (fun x y -> height x y); gravity = 30.; lo = 4.; hi = side -. 4. }

(* the waypoints, every 12 along the middle, for Topdown's laps and
 * the computer's drivers *)
let track : Topdown.track =
  let n = int_of_float (length /. 12.) in
  let points =
    Array.init n (fun i ->
        let p = Track3d.at ribbon (float_of_int i *. length /. float_of_int n) in
        (p.px, -.p.pz))
  in
  { points; reach = 14.; corner = 25. }

(*****************************************************************************)
(* Drawing the world, once *)
(*****************************************************************************)

(* The terrain: every 4 units a height, two triangles a square; the
 * fields in patches of 24 (a hash per patch picks the crop), the road
 * drawn over it *)
let cell = 4.
let cells = int_of_float (side /. cell) + 1

let hash (i : int) (j : int) : int = ((i * 73856093) lxor (j * 19349663)) land 0xffff

let field (x : number) (y : number) : color =
  match hash (int_of_float (x /. 24.)) (int_of_float (y /. 24.)) mod 4 with
  | 0 -> rgb 90 160 60
  | 1 -> rgb 150 170 70
  | 2 -> rgb 140 110 70
  | _ -> rgb 70 140 55

let terrain : shape3d list =
  let hs = Array.init (cells * cells) (fun k -> height ~ramps:false (float_of_int (k mod cells) *. cell) (float_of_int (k / cells) *. cell)) in
  let p i j = at (float_of_int i *. cell) (float_of_int j *. cell) hs.((j * cells) + i) in
  let n = cells - 1 in
  List.concat
    (List.init (n * n) (fun k ->
         let i = k mod n and j = k / n in
         let color = field (float_of_int i *. cell) (float_of_int j *. cell) in
         [ polygon3d color [ p i j; p (i + 1) j; p (i + 1) (j + 1) ]; polygon3d color [ p i j; p (i + 1) (j + 1); p i (j + 1) ] ]))

(* A strip of the road from [a] to [b] across, on segment [i], on its
 * hills (not at Track3d's own height: the ribbon was built flat), just
 * above the terrain *)
let strip (color : color) (i : int) (a : number) (b : number) : shape3d =
  let step = Track3d.step ribbon in
  let p s off =
    let x, _, z = Track3d.across ribbon s off in
    (x, hills x (-.z) +. 0.15, z)
  in
  let s1 = float_of_int i *. step and s2 = float_of_int (i + 1) *. step in
  polygon3d color [ p s1 a; p s1 b; p s2 b; p s2 a ]

let road_shapes : shape3d list =
  List.concat
    (List.init (Track3d.segments ribbon) (fun i ->
         let kerb = if i / 2 mod 2 = 0 then rgb 220 40 40 else white in
         [ strip (rgb 90 90 95) i (-.road) road; strip kerb i (-1.15 *. road) (-.road); strip kerb i road (1.15 *. road) ]
         @ if i mod 3 = 0 then [ strip white i (-0.2) 0.2 ] else []))

(* A ramp: its top rising to the lip, its sides and its back, where the
 * cars fly off *)
let ramp_shape (r : number) : shape3d =
  let p s off up =
    let x, _, z = Track3d.across ribbon s off in
    (x, hills x (-.z) +. 0.15 +. up, z)
  in
  let e = r +. ramp_length in
  let w = road in
  let side = rgb 60 60 60 in
  group3d
    (* its top, yellow with two black stripes across *)
    [ polygon3d yellow [ p r (-.w) 0.; p r w 0.; p e w ramp_height; p e (-.w) ramp_height ];
      polygon3d black [ p (r +. 2.) (-.w) 0.45; p (r +. 2.) w 0.45; p (r +. 3.) w 0.65; p (r +. 3.) (-.w) 0.65 ];
      polygon3d black [ p (r +. 5.) (-.w) 1.05; p (r +. 5.) w 1.05; p (r +. 6.) w 1.25; p (r +. 6.) (-.w) 1.25 ];
      polygon3d side [ p e (-.w) 0.; p e w 0.; p e w ramp_height; p e (-.w) ramp_height ];
      polygon3d side [ p r (-.w) 0.; p e (-.w) 0.; p e (-.w) ramp_height ]; polygon3d side [ p r w 0.; p e w 0.; p e w ramp_height ] ]

(* the chequered line, at s = 0 *)
let start_line : shape3d list =
  List.concat
    (List.init 8 (fun i ->
         List.init 2 (fun j ->
             let w = 2. *. road /. 8. in
             let p u v =
               let x, _, z = Track3d.across ribbon v u in
               (x, hills x (-.z) +. 0.2, z)
             in
             let u = -.road +. (float_of_int i *. w) and v = float_of_int j *. w in
             polygon3d (if (i + j) mod 2 = 0 then white else black) [ p u v; p (u +. w) v; p (u +. w) (v +. w); p u (v +. w) ])))

(* trees and houses, in the fields, away from the road *)
let tree : shape3d = group3d [ box (rgb 100 70 40) 0.6 2. 0.6 |> move_y3d 1.; box (rgb 40 110 45) 3. 2.6 3. |> rotate3d 0. 45. 0. |> move_y3d 3. ]

let house : shape3d =
  let roof = rgb 170 60 40 in
  group3d
    [ box (rgb 235 230 215) 6. 3.5 5. |> move_y3d 1.75;
      polygon3d roof [ (-3.3, 3.5, 2.8); (3.3, 3.5, 2.8); (3.3, 5.5, 0.); (-3.3, 5.5, 0.) ];
      polygon3d roof [ (3.3, 3.5, -2.8); (-3.3, 3.5, -2.8); (-3.3, 5.5, 0.); (3.3, 5.5, 0.) ] ]

let scenery : shape3d list =
  List.concat
    (List.init 256 (fun k ->
         let i = k mod 16 and j = k / 16 in
         let x = (float_of_int i +. 0.5) *. 16. +. float_of_int (hash i j mod 9 - 4)
         and y = (float_of_int j +. 0.5) *. 16. +. float_of_int (hash j i mod 9 - 4) in
         let _, off = Track3d.locate ribbon x (-.y) in
         let px, py, pz = at x y (height x y) in
         if Float.abs off < road +. 9. then []
         else
           match hash i j mod 10 with
           | 0 -> [ house |> rotate3d 0. (float_of_int (hash i j mod 4 * 90)) 0. |> move3d px py pz ]
           | 1 | 2 | 3 -> [ tree |> move3d px py pz ]
           | _ -> []))

let world : shape3d = cached3d (terrain @ road_shapes @ List.map ramp_shape ramps @ start_line @ scenery)

(*****************************************************************************)
(* Vehicles *)
(*****************************************************************************)

type vehicle = {
  vname : string;
  (* facing -z; the time, for the police car's lights *)
  model : number -> shape3d;
  params : Topdown.params;
  (* the top speed on the road, and off it *)
  top : number;
  rough : number;
  radius : number;
}

let wheel (x : number) (z : number) : shape3d = box (rgb 25 25 25) 0.5 1.1 1.1 |> move3d x 0.55 z

let sports_car : vehicle =
  { vname = "SPORTS CAR";
    model =
      (fun _ ->
        let red = rgb 220 40 30 in
        group3d
          [ box red 2. 0.6 4.2 |> move_y3d 0.7; box (rgb 40 40 50) 1.6 0.5 1.6 |> move3d 0. 1.2 0.3; box red 2. 0.15 0.5 |> move3d 0. 1.2 1.9;
            wheel (-1.05) (-1.3); wheel 1.05 (-1.3); wheel (-1.05) 1.3; wheel 1.05 1.3 ]);
    params = { accel = 46.; friction = 0.6; grip = 0.1; steering = 3.4; steering_speed = 12. };
    top = 46.;
    rough = 24.;
    radius = 2.2 }

let police_car : vehicle =
  { vname = "POLICE CAR";
    model =
      (fun time ->
        let blink = int_of_float (time *. 6.) mod 2 = 0 in
        group3d
          [ box white 2.1 0.7 4.4 |> move_y3d 0.75; box (rgb 30 30 40) 2. 0.3 1.2 |> move3d 0. 0.8 (-1.6);
            box white 1.8 0.6 2. |> move3d 0. 1.4 0.3;
            box (if blink then rgb 40 80 255 else rgb 20 30 90) 0.7 0.3 0.4 |> move3d (-0.4) 1.85 0.3;
            box (if blink then rgb 90 20 20 else rgb 255 40 40) 0.7 0.3 0.4 |> move3d 0.4 1.85 0.3; wheel (-1.1) (-1.4); wheel 1.1 (-1.4);
            wheel (-1.1) 1.4; wheel 1.1 1.4 ]);
    params = { accel = 42.; friction = 0.6; grip = 0.13; steering = 3.2; steering_speed = 12. };
    top = 44.;
    rough = 26.;
    radius = 2.3 }

let school_bus : vehicle =
  { vname = "SCHOOL BUS";
    model =
      (fun _ ->
        let yellow = rgb 245 190 20 in
        group3d
          [ box yellow 2.6 2.4 8. |> move_y3d 1.9; box (rgb 40 40 50) 2.65 0.7 6. |> move3d 0. 2.3 0.6;
            box yellow 2.4 1.2 1.4 |> move3d 0. 1.2 (-4.6); box (rgb 20 20 20) 2.65 0.2 8.05 |> move_y3d 1.2; wheel (-1.3) (-3.);
            wheel 1.3 (-3.); wheel (-1.3) 2.6; wheel 1.3 2.6 ]);
    params = { accel = 32.; friction = 0.6; grip = 0.2; steering = 2.5; steering_speed = 12. };
    top = 38.;
    rough = 26.;
    radius = 3.2 }

let vehicles = [| sports_car; police_car; school_bus |]

(*****************************************************************************)
(* The cars *)
(*****************************************************************************)

let laps = 3

(* a player (0 or 1), or the computer, by its skill *)
type driver = Human of int | Computer of number

type car = {
  ride : Offroad.t;
  vehicle : int;
  driver : driver;
  (* once over the line: the place and the time *)
  result : (int * int) option;
}

(* two rows of two, behind the line *)
let grid (players : int) (chosen : int) : car list =
  let drivers = List.init 4 (fun i -> if i < players then Human i else Computer (0.9 +. (0.03 *. float_of_int i))) in
  List.mapi
    (fun i driver ->
      let b = Topdown.start track 0 (if i mod 2 = 0 then 3.5 else -3.5) in
      let a = b.heading *. Float.pi /. 180. and back = 4. +. (8. *. float_of_int (i / 2)) in
      let b = { b with x = b.x -. (back *. cos a); y = b.y -. (back *. sin a) } in
      { ride = Offroad.start ground b; vehicle = (chosen + i) mod Array.length vehicles; driver; result = None })
    drivers

let axis (a : bool) (b : bool) : number = (if a then 1. else 0.) -. if b then 1. else 0.

(* the player's keys, or the computer, which also drives a player's car
 * once it has finished *)
let drive (k : keyboard) (c : car) : car =
  let v = vehicles.(c.vehicle) in
  let b = c.ride.body in
  let gas, steer =
    match (c.driver, c.result) with
    | Human 0, None -> (axis k.kup k.kdown, axis k.kleft k.kright)
    | Human _, None -> (axis k.kw k.ks, axis k.ka k.kd)
    | _ -> Topdown.computer track b
  in
  let skill = match c.driver with Computer s -> s | Human _ -> 1. in
  let _, off = Track3d.locate ribbon b.x (-.b.y) in
  let top = (if Float.abs off < road *. 1.15 then v.top else v.rough) *. skill in
  { c with ride = Offroad.drive ground track { v.params with accel = v.params.accel *. skill } top gas steer c.ride }

(* every two cars pushed apart (Offroad.push: not one flying over the
 * other) *)
let rec push_all (cars : car list) : car list =
  match cars with
  | [] -> []
  | c :: rest ->
      let c, rest =
        List.fold_left
          (fun (c, acc) o ->
            let r = (vehicles.(c.vehicle).radius +. vehicles.(o.vehicle).radius) /. 2. in
            let a, b = Offroad.push r c.ride o.ride in
            ({ c with ride = a }, { o with ride = b } :: acc))
          (c, []) rest
      in
      c :: push_all (List.rev rest)

let place (cars : car list) (c : car) : int =
  let p = Topdown.progress track c.ride.body in
  1 + List.length (List.filter (fun o -> o != c && Topdown.progress track o.ride.body > p) cars)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type race = { players : int; chosen : int; cars : car list; time : int }

type scene = Title of int * int (* vehicle, players *) | Racing of race

type model = scene Scene2d.t

let initial_model : model = Scene2d.start (Title (0, 1))

let countdown = 180

let humans (r : race) : car list = List.filter (fun c -> match c.driver with Human _ -> true | Computer _ -> false) r.cars

let over (r : race) : bool = List.for_all (fun c -> c.result <> None) (humans r)

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let pressed f = Scene2d.pressed f s in
  match s.scene with
  | Title (v, players) ->
      let n = Array.length vehicles in
      if pressed (fun k -> k.kspace) then Scene2d.go (Racing { players; chosen = v; cars = grid players v; time = 0 }) s
      else if pressed (fun k -> k.kright) then Scene2d.go (Title ((v + 1) mod n, players)) s
      else if pressed (fun k -> k.kleft) then Scene2d.go (Title ((v + n - 1) mod n, players)) s
      else if pressed (fun k -> k.kup || k.kdown) then Scene2d.go (Title (v, 3 - players)) s
      else s
  | Racing r when over r && pressed (fun k -> k.kspace) -> Scene2d.go (Title (r.chosen, r.players)) s
  | Racing r ->
      let cars = if r.time < countdown then r.cars else push_all (List.map (drive computer.keyboard) r.cars) in
      let cars =
        List.map
          (fun c ->
            if c.result = None && Topdown.lap track c.ride.body >= laps then { c with result = Some (place cars c, r.time - countdown) } else c)
          cars
      in
      { s with scene = Racing { r with cars; time = r.time + 1 } }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* the car pitched and rolled by the ground (Offroad.pose), and, in the
 * air, its shadow on the ground below *)
let draw_car (time : number) (c : car) : shape3d list =
  let b = c.ride.body in
  let pitch, roll = Offroad.pose ground c.ride in
  let x, h, z = at b.x b.y c.ride.h in
  let car = vehicles.(c.vehicle).model time |> rotate3d pitch 0. roll |> rotate3d 0. (b.heading -. 90.) 0. |> move3d x h z in
  if not c.ride.air then [ car ]
  else
    let a = b.heading *. Float.pi /. 180. in
    let fx = cos a and fy = sin a in
    let g = height b.x b.y +. 0.2 in
    let p u v = at (b.x +. (fx *. v) +. (fy *. u)) (b.y +. (fy *. v) -. (fx *. u)) g in
    [ car; polygon3d (rgb 20 20 20) [ p (-1.1) (-2.2); p 1.1 (-2.2); p 1.1 2.2; p (-1.1) 2.2 ] |> fade3d 0.5 ]

(* From above, north up, tilted a little towards the south so that
 * slopes and heights show; further for a half screen *)
let camera_on (players : int) (c : car) : camera =
  let x, h, z = at c.ride.body.x c.ride.body.y c.ride.h in
  let up, south = if players = 1 then (48., 30.) else (62., 38.) in
  camera ~eye:(x, h +. up, z +. south) ~target:(x, h, z) ~fov:45. ~far:2000. ()

let text color size str = words color str |> scale size

let ordinal (n : int) : string = match n with 1 -> "1ST" | 2 -> "2ND" | 3 -> "3RD" | n -> string_of_int n ^ "TH"

let clock (frames : int) : string = Printf.sprintf "%d:%02d.%d" (frames / 3600) (frames / 60 mod 60) (frames mod 60 / 6)

let hud_for (screen : screen) (r : race) (s : model) (c : car) : shape list =
  let lap = min laps (Topdown.lap track c.ride.body + 1) in
  let go =
    if r.time < countdown then [ text yellow 10. (string_of_int (3 - (r.time / 60))) |> move_y 150. ]
    else if r.time < countdown + 60 then [ text (rgb 250 60 40) 10. "GO!" |> move_y 150. ]
    else []
  in
  match c.result with
  | None ->
      [ text white 3. (Printf.sprintf "LAP %d/%d" lap laps) |> move (screen.left +. 100.) (screen.top -. 40.);
        text white 3. (clock (max 0 (r.time - countdown))) |> move (screen.left +. 100.) (screen.top -. 80.);
        text yellow 5. (ordinal (place r.cars c)) |> move (screen.right -. 80.) (screen.top -. 50.) ]
      @ go
  | Some (n, t) ->
      [ text yellow 7. (ordinal n ^ " PLACE!") |> move_y 200.; text white 4. (clock t) |> move_y 130. ]
      @ if over r then Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y 70. ] else []

let view (computer : computer) (s : model) : view list =
  (* the police car's lights blink with the scene's clock *)
  let time = s.elapsed in
  let sky cam = Camera3d.floor ~color:(rgb 80 150 60) ~ground:0. cam :: Camera3d.sky ~sky:(rgb 150 200 240) ~horizon:(rgb 80 150 60) ~ground:0. cam in
  match s.scene with
  | Title (v, players) ->
      let cars = grid players v in
      let me = List.hd cars in
      let x, h, z = at me.ride.body.x me.ride.body.y me.ride.h in
      let cam = Camera3d.orbit ~distance:16. ~height:8. ~look:1. (spin 10. computer.time) (x, h, z) in
      let hud_shapes =
        [ text (rgb 250 60 40) 7. "TINY IGNITION" |> move_y 300.; text white 4. ("< " ^ vehicles.(v).vname ^ " >") |> move_y 230.;
          text yellow 3. (if players = 1 then "1 PLAYER" else "2 PLAYERS (P2: W A S D)") |> move_y 180.;
          text white 2. "left/right: vehicle   up/down: players   arrows: drive" |> move_y (-300.) ]
        @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 130. ]
      in
      [ { camera = cam; area = whole; shapes = sky cam @ (world :: List.concat_map (draw_car time) cars) @ List.map hud hud_shapes } ]
  | Racing r ->
      let cars = List.concat_map (draw_car time) r.cars in
      let areas = if r.players = 1 then [ whole ] else [ { x = 0.; y = 0.; w = 0.5; h = 1. }; { x = 0.5; y = 0.; w = 0.5; h = 1. } ] in
      List.map2
        (fun area c ->
          let cam = camera_on r.players c in
          let screen = area_screen computer.screen area in
          { camera = cam; area; shapes = sky cam @ (world :: cars) @ List.map hud (hud_for screen r s c) })
        areas (humans r)

let app = split3d view update initial_model

(* flat shading, each triangle lit by its slope; the back faces drawn
 * too, for the sky (seen from below, see Camera3d.sky) *)
let main =
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false } app
