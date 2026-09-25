(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Big Red Racing (Big Red Software, Domark, 1996):
 * three laps against three computer drivers, over whole landscapes in
 * flat-shaded polygons, seen from high behind the car. Three courses,
 * each with its own vehicle, as in the original (where every course
 * decided which class of vehicle raced it): a buggy over green hills,
 * a monster truck down a red canyon, a lunar buggy on the Moon.
 * Left/right on the title to choose, space to start; up to accelerate,
 * down to brake, left/right to steer, v to change the view.
 *
 * It is TinySuperOffRoad.ml's driving on a whole world instead of a
 * stadium: the car is the racing kit's Topdown (a point with a heading
 * and a velocity, sliding by its grip), and the ground under it has a
 * height, which adds the same three rules -- the slope's pull, the
 * takeoff over a crest, the landing. What's new is that the world is
 * the terrain, a grid of heights (the heightmap kit's Heightmap, as in
 * TinyComanche3d.ml), and the course is *in* it, not laid on it:
 *
 *   - the road is only where the terrain is painted: each quad of the
 *     grid colored by how far it is from the course's center line
 *     (Topdown.distance), the whole landscape drawn the same way, a
 *     single cached mesh of 8000 triangles;
 *   - a course is its heights, a function of where a point is and how
 *     far it is from the road: the canyon is the ground raised steeply
 *     beyond a distance from the road, its walls nothing but terrain;
 *     a wall is where the ground is higher than the car can climb in
 *     a frame (the car bounces off it, Topdown.bounce), so there is no
 *     other kind of wall anywhere;
 *   - and gravity is a number of the course. On the Moon it is a sixth
 *     of the Earth's, which does two things at once: over a crest the
 *     ground falls away faster than a sixth of gravity can follow, so
 *     every crater's rim is a ramp and the buggy flies for seconds; and
 *     its tyres press on the ground with a sixth of the weight, so they
 *     grip less, and it slides (the lunar buggy's low grip). Whether
 *     Big Red Racing's Moon had its gravity changed, I don't know; this
 *     one has.
 *
 * The takeoff, as in TinySuperOffRoad, is one comparison: where the
 * car would be one frame on if it were flying, against the ground there.
 * The same crest at the same speed stays on the ground on Earth and
 * leaves it on the Moon:
 *
 *        Earth: gravity pulls it     Moon: the ground falls away
 *        down onto the far side      faster than it falls
 *              __                          __ . . . . .
 *          ___/  \___                  ___/  \___
 *
 * Uses: the racing kit's Topdown (the driving, the waypoints, laps and
 * who leads, the computer's drivers, the bounces off walls and each
 * other), the heightmap kit's Heightmap (a grid of heights and the
 * height between them; not its island generator: each course makes
 * its own heights), Scene2d, and Camera3d's sky and floor. Not Track3d
 * (the road is not a ribbon over the world, it is the world), not
 * Physics (the car is a point, its pitch and roll only drawn).
 *
 * Exercises: the other vehicles of the original (a hovercraft that
 * crosses water, a helicopter over everything); two cars to choose per
 * course; Mars, at 0.38 g; the camera smoothed (Camera3d.follow).
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* Vehicles and courses *)
(*****************************************************************************)

type vehicle = {
  vname : string;
  (* the car, facing -z, in the given paint *)
  model : color -> shape3d;
  params : Topdown.params;
  (* the top speed on the road, and off it *)
  top : number;
  rough : number;
}

let wheel (r : number) (x : number) (z : number) : shape3d = box (rgb 30 30 30) 0.6 (2. *. r) (2. *. r) |> move3d x r z

let buggy : vehicle =
  { vname = "BUGGY";
    model =
      (fun paint ->
        group3d
          [ box paint 1.9 0.5 3.6 |> move_y3d 0.8; box (rgb 60 60 60) 1.6 0.9 0.2 |> move3d 0. 1.5 0.3;
            box paint 1.8 0.2 0.5 |> move3d 0. 1.9 1.6; wheel 0.55 (-1.1) (-1.2); wheel 0.55 1.1 (-1.2);
            wheel 0.6 (-1.1) 1.2; wheel 0.6 1.1 1.2 ]);
    params = { accel = 40.; friction = 0.6; grip = 0.12; steering = 3.2; steering_speed = 12. };
    top = 42.;
    rough = 28. }

let monster_truck : vehicle =
  { vname = "MONSTER TRUCK";
    model =
      (fun paint ->
        group3d
          [ box paint 2.2 1.1 3.8 |> move_y3d 2.1; box paint 2. 0.9 1.6 |> move3d 0. 3. 0.5;
            box (rgb 150 200 240) 2.05 0.6 0.1 |> move3d 0. 3. (-0.3); wheel 1.1 (-1.3) (-1.3); wheel 1.1 1.3 (-1.3);
            wheel 1.1 (-1.3) 1.3; wheel 1.1 1.3 1.3 ]);
    params = { accel = 32.; friction = 0.6; grip = 0.18; steering = 2.6; steering_speed = 12. };
    top = 38.;
    rough = 32. }

(* the tyres press on the ground with a sixth of the weight: a sixth of
 * the grip, and less of the engine's pull reaching the ground *)
let lunar_buggy : vehicle =
  { vname = "LUNAR BUGGY";
    model =
      (fun paint ->
        let gold = rgb 220 180 60 in
        group3d
          [ box (rgb 190 190 190) 2. 0.3 3.8 |> move_y3d 0.9; box paint 1.2 0.6 0.9 |> move3d 0. 1.3 0.6;
            box gold 1.6 0.2 1. |> move3d 0. 1.1 (-1.); box (rgb 200 200 200) 0.1 1.4 0.1 |> move3d 0.6 1.8 1.4;
            box (rgb 230 230 230) 0.8 0.1 0.8 |> rotate3d 30. 0. 0. |> move3d 0.6 2.5 1.4; wheel 0.6 (-1.2) (-1.3);
            wheel 0.6 1.2 (-1.3); wheel 0.6 (-1.2) 1.3; wheel 0.6 1.2 1.3 ]);
    params = { accel = 20.; friction = 0.3; grip = 0.05; steering = 3.; steering_speed = 8. };
    top = 28.;
    rough = 26. }

(* the world is a square of [cells] x [cells] heights, [cell] apart *)
let cells = 64
let cell = 4.
let side = float_of_int (cells - 1) *. cell

(* half the road *)
let road = 9.

let smooth (t : number) : number =
  let t = Float.max 0. (Float.min 1. t) in
  t *. t *. (3. -. (2. *. t))

(* a mound where the road jumps, [r] around *)
let kicker (x : number) (y : number) ((kx, ky, height, r) : number * number * number * number) : number =
  let q = Float.hypot (x -. kx) (y -. ky) /. r in
  height *. Float.exp (-.(q *. q))

type course = {
  cname : string;
  points : (number * number) list;
  vehicle : vehicle;
  (* the Earth's is 30 (world units, a car 4 long, per second squared:
   * arcade gravity, three times the real one, for short jumps) *)
  gravity : number;
  (* [ground x y d]: the height at (x, y), [d] from the road's middle *)
  ground : number -> number -> number -> number;
  (* [paint h steep on_road]: the color of a quad, [h] high, rising
   * [steep] across its side *)
  paint : number -> number -> bool -> color;
  sky : color;
  horizon : color;
}

let highlands : course =
  { cname = "HIGHLANDS";
    points = [ (60., 45.); (200., 40.); (222., 110.); (165., 138.); (205., 205.); (120., 218.); (45., 190.); (85., 120.) ];
    vehicle = buggy;
    gravity = 30.;
    ground =
      (fun x y d ->
        let rolling = 8. +. (5. *. sin (x /. 23.) *. cos (y /. 31.)) +. (3. *. sin ((x -. y) /. 17.)) in
        let hills = smooth ((d -. 14.) /. 40.) *. (10. +. (12. *. (0.5 +. (0.5 *. sin ((x /. 19.) +. (y /. 13.)))))) in
        rolling +. hills +. kicker x y (130., 42., 2.5, 5.) +. kicker x y (82., 214., 2.5, 5.));
    paint =
      (fun h steep on_road ->
        if on_road then rgb 170 140 100
        else if steep > 2.5 then rgb 120 115 105
        else if h > 22. then rgb 110 120 70
        else rgb 70 (140 + int_of_float h) 55);
    sky = rgb 150 195 240;
    horizon = rgb 90 130 80 }

let canyon : course =
  { cname = "RED CANYON";
    points =
      [ (50., 40.); (130., 62.); (212., 40.); (215., 130.); (150., 120.); (112., 170.); (205., 212.); (60., 215.); (38., 128.) ];
    vehicle = monster_truck;
    gravity = 30.;
    ground =
      (fun x y d ->
        let floor = 3. +. (2. *. sin (x /. 29.) *. sin (y /. 21.)) in
        let wall = smooth ((d -. 13.) /. 7.) *. (26. +. (6. *. sin ((x /. 11.) +. (y /. 17.)))) in
        floor +. wall +. kicker x y (175., 50., 3., 5.) +. kicker x y (214., 90., 3., 5.) +. kicker x y (130., 214., 3., 5.));
    (* the walls in bands by height: the canyon's strata *)
    paint =
      (fun h steep on_road ->
        if on_road then rgb 215 170 120
        else if steep < 1. then rgb 200 140 90
        else match int_of_float (h /. 4.) mod 3 with 0 -> rgb 170 70 40 | 1 -> rgb 200 100 50 | _ -> rgb 150 60 45);
    sky = rgb 170 200 230;
    horizon = rgb 180 110 70 }

(* (x, y, radius, depth) *)
let craters =
  [ (128., 38., 12., 2.); (218., 150., 13., 2.); (120., 214., 12., 2.); (160., 120., 30., 5.); (70., 110., 18., 4.);
    (60., 150., 10., 2.); (210., 225., 14., 3.) ]

(* a bowl, and its rim thrown up around it *)
let crater (x : number) (y : number) ((cx, cy, r, depth) : number * number * number * number) : number =
  let u = Float.hypot (x -. cx) (y -. cy) /. r in
  let rim = (u -. 1.) /. 0.4 in
  (if u < 1. then -.depth *. (1. -. (u *. u)) else 0.) +. (depth *. 0.4 *. Float.exp (-.(rim *. rim)))

let moon : course =
  { cname = "THE MOON";
    points = [ (50., 60.); (128., 35.); (205., 60.); (220., 150.); (165., 213.); (80., 210.); (35., 140.) ];
    vehicle = lunar_buggy;
    gravity = 30. /. 6.;
    ground =
      (fun x y _d ->
        6. +. (1.5 *. sin (x /. 37.) *. cos (y /. 41.)) +. List.fold_left (fun h c -> h +. crater x y c) 0. craters);
    paint =
      (fun h steep on_road ->
        let g = 120 + int_of_float (h *. 4.) - int_of_float (steep *. 10.) in
        if on_road then rgb (g - 50) (g - 50) (g - 45) else rgb g g g);
    sky = black;
    horizon = rgb 60 60 60 }

let courses = [| highlands; canyon; moon |]

(*****************************************************************************)
(* The world: a course's heights, and its mesh *)
(*****************************************************************************)

type world = { course : course; track : Topdown.track; map : Heightmap.t; mesh : shape3d }

(* the height anywhere, the four heights around mixed *)
let height (map : Heightmap.t) (x : number) (y : number) : number = Heightmap.height map (x /. cell) (y /. cell)

(* the ground's rise per unit, along x and along y *)
let slope (map : Heightmap.t) (x : number) (y : number) : number * number =
  let e = 0.5 in
  ((height map (x +. e) y -. height map (x -. e) y) /. (2. *. e), (height map x (y +. e) -. height map x (y -. e)) /. (2. *. e))

(* the world's y is up; the course's (x, y), seen from above, is the
 * world's (x, -z), so that north is still up from above (as in
 * TinyComanche3d) *)
let at (x : number) (y : number) (h : number) : number * number * number = (x, h, -.y)

(* a checkered line across the road at the start, just above the ground *)
let start_line (track : Topdown.track) (map : Heightmap.t) : shape3d list =
  let x1, y1 = Topdown.point track 0 and x2, y2 = Topdown.point track 1 in
  let a = atan2 (y2 -. y1) (x2 -. x1) in
  let fx = cos a and fy = sin a in
  let rx = fy and ry = -.fx in
  let n = 8 in
  let w = 2. *. road /. float_of_int n in
  List.concat
    (List.init n (fun i ->
         List.init 2 (fun j ->
             let p u v =
               let x = x1 +. (rx *. u) +. (fx *. v) and y = y1 +. (ry *. u) +. (fy *. v) in
               at x y (height map x y +. 0.15)
             in
             let u = -.road +. (float_of_int i *. w) and v = float_of_int j *. w in
             polygon3d (if (i + j) mod 2 = 0 then white else black) [ p u v; p (u +. w) v; p (u +. w) (v +. w); p u (v +. w) ])))

(* The terrain as triangles: each quad of the grid split in two, its
 * color the course's, by its height, its steepness, and whether its
 * middle is on the road. The road is nothing else.
 *
 *     (i, j+1) +----+ (i+1, j+1)
 *              |  / |
 *              | /  |
 *       (i, j) +----+ (i+1, j)
 *)
let build (course : course) : world =
  let track = { Topdown.points = Array.of_list course.points; reach = 20.; corner = 30. } in
  let heights =
    Array.init (cells * cells) (fun k ->
        let x = float_of_int (k mod cells) *. cell and y = float_of_int (k / cells) *. cell in
        course.ground x y (Topdown.distance track x y))
  in
  let map = { Heightmap.size = cells; cells = heights; top = Array.fold_left Float.max 0. heights; sea = 0. } in
  let quad i j =
    let p i j = at (float_of_int i *. cell) (float_of_int j *. cell) (Heightmap.cell map i j) in
    let hs = [ Heightmap.cell map i j; Heightmap.cell map (i + 1) j; Heightmap.cell map (i + 1) (j + 1); Heightmap.cell map i (j + 1) ] in
    let lo = List.fold_left Float.min infinity hs and hi = List.fold_left Float.max neg_infinity hs in
    let mx = (float_of_int i +. 0.5) *. cell and my = (float_of_int j +. 0.5) *. cell in
    let color = course.paint ((lo +. hi) /. 2.) (hi -. lo) (Topdown.distance track mx my < road) in
    [ polygon3d color [ p i j; p (i + 1) j; p (i + 1) (j + 1) ]; polygon3d color [ p i j; p (i + 1) (j + 1); p i (j + 1) ] ]
  in
  let n = cells - 1 in
  let mesh = List.concat (List.init (n * n) (fun k -> quad (k mod n) (k / n))) in
  { course; track; map; mesh = cached3d (mesh @ start_line track map) }

(* built the first time a course is raced or shown *)
let worlds : world Lazy.t array = Array.map (fun c -> lazy (build c)) courses

(*****************************************************************************)
(* The cars *)
(*****************************************************************************)

let dt = 1. /. 60.
let laps = 3

type driver = Player | Computer of number (* skill, 1 the best *)

type car = {
  body : Topdown.t;
  (* the height, and how fast it changes *)
  h : number;
  vh : number;
  air : bool;
  driver : driver;
  paint : color;
}

(* The car moved from [before] to [after]: bounced off the walls -- the
 * ground higher than a car [h] high can climb in a frame, or steeper
 * than it can climb at all, uphill -- and stopped
 * at the map's edge (not a wall for Topdown.bounce: a car already in a
 * wall moves freely, to get out, and off the map it would stay in one);
 * then its next waypoint, if it reached this one. *)
let bounce (w : world) (h : number) (before : Topdown.t) (after : Topdown.t) : Topdown.t =
  let steep x y =
    let gx, gy = slope w.map x y in
    Float.hypot gx gy > 1.2
  in
  let b = Topdown.bounce (fun x y -> height w.map x y > h +. 1.2 || (steep x y && height w.map x y > h +. 0.3)) before after in
  let clamp v = Float.max cell (Float.min (side -. cell) v) in
  let b = if clamp b.x = b.x && clamp b.y = b.y then b else { b with x = clamp b.x; y = clamp b.y; vx = 0.; vy = 0.; speed = 0. } in
  Topdown.follow w.track b

(* On the ground: driven, then gravity along the slope (the part along
 * the car slowing it or speeding it up, the part across pushing it
 * sideways); then either still on the ground, or off it: where the car
 * would be one frame on if it flew, above the ground there. *)
let on_ground (w : world) (gas : number) (steer : number) (c : car) : car =
  let v = w.course.vehicle and g = w.course.gravity in
  let b = c.body in
  let skill = match c.driver with Computer k -> k | Player -> 1. in
  let top = (if Topdown.distance w.track b.x b.y < road then v.top else v.rough) *. skill in
  let after = Topdown.drive { v.params with accel = v.params.accel *. skill } top gas steer b in
  let gx, gy = slope w.map b.x b.y in
  let a = after.heading *. Float.pi /. 180. in
  let along = (gx *. cos a) +. (gy *. sin a) in
  let after =
    { after with
      speed = after.speed -. (g *. along *. dt);
      vx = after.vx -. (g *. (gx -. (along *. cos a)) *. dt);
      vy = after.vy -. (g *. (gy -. (along *. sin a)) *. dt) }
  in
  let body = bounce w c.h b after in
  let ground = height w.map body.x body.y in
  let flying = c.h +. (c.vh *. dt) -. (g *. dt *. dt /. 2.) in
  if ground < flying -. 0.03 then { c with body; h = flying; vh = c.vh -. (g *. dt); air = true }
  else { c with body; h = ground; vh = (ground -. c.h) /. dt; air = false }

(* in the air: no wheel, no gas; falling; landing when the ground comes
 * up to it, a hard landing costing speed *)
let in_air (w : world) (c : car) : car =
  let b = c.body in
  let moved = { b with x = b.x +. (b.vx *. dt); y = b.y +. (b.vy *. dt) } in
  let body = bounce w c.h b moved in
  let vh = c.vh -. (w.course.gravity *. dt) in
  let h = c.h +. (vh *. dt) in
  let ground = height w.map body.x body.y in
  if h > ground then { c with body; h; vh }
  else
    let hard = vh < -15. in
    { c with body = { body with speed = (if hard then body.speed *. 0.7 else body.speed) }; h = ground; vh = 0.; air = false }

let axis (a : bool) (b : bool) : number = (if a then 1. else 0.) -. if b then 1. else 0.

(* the player's keys, or the computer, which also drives the player's
 * car once it has finished *)
let drive (w : world) (k : keyboard) (finished : bool) (c : car) : car =
  let gas, steer =
    match c.driver with
    | Player when not finished -> (axis k.kup k.kdown, axis k.kleft k.kright)
    | _ -> Topdown.computer w.track c.body
  in
  if c.air then in_air w c else on_ground w gas steer c

(* two cars at about the same height, pushed apart *)
let rec push_all (cars : car list) : car list =
  match cars with
  | [] -> []
  | c :: rest ->
      let c, rest =
        List.fold_left
          (fun (c, acc) (o : car) ->
            if Float.abs (c.h -. o.h) < 2. then
              let b1, b2 = Topdown.push 2.2 c.body o.body in
              ({ c with body = b1 }, { o with body = b2 } :: acc)
            else (c, o :: acc))
          (c, []) rest
      in
      c :: push_all (List.rev rest)

(* the four abreast, a little behind the line *)
let grid (w : world) : car list =
  List.mapi
    (fun i (driver, paint) ->
      let body = Topdown.start w.track 0 (float_of_int ((3 * i) - 5) *. 1.3) in
      let body = { body with x = body.x -. (2. *. cos (body.heading *. Float.pi /. 180.)); y = body.y -. (2. *. sin (body.heading *. Float.pi /. 180.)) } in
      { body; h = height w.map body.x body.y; vh = 0.; air = false; driver; paint })
    [ (Computer 0.88, rgb 40 80 220); (Player, rgb 220 30 30); (Computer 0.93, rgb 240 200 30); (Computer 0.97, rgb 40 170 60) ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type view = High | Low

type race = {
  course : int;
  cars : car list;
  (* frames since the countdown started *)
  time : int;
  view : view;
  (* the player's place and time, once over the line *)
  result : (int * int) option;
}

type scene = Title of int | Racing of race

type model = scene Scene2d.t

let initial_model : model = Scene2d.start (Title 0)

let countdown = 180

let player (r : race) : car = List.find (fun c -> c.driver = Player) r.cars

(* 1 + the cars further along *)
let place (w : world) (r : race) : int =
  let p = Topdown.progress w.track (player r).body in
  1 + List.length (List.filter (fun c -> c.driver <> Player && Topdown.progress w.track c.body > p) r.cars)

let new_race (course : int) : race =
  { course; cars = grid (Lazy.force worlds.(course)); time = 0; view = High; result = None }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let pressed f = Scene2d.pressed f s in
  match s.scene with
  | Title i ->
      let n = Array.length courses in
      if pressed (fun k -> k.kspace) then Scene2d.go (Racing (new_race i)) s
      else if pressed (fun k -> k.kright) then Scene2d.go (Title ((i + 1) mod n)) s
      else if pressed (fun k -> k.kleft) then Scene2d.go (Title ((i + n - 1) mod n)) s
      else s
  | Racing r when r.result <> None && pressed (fun k -> k.kspace) -> Scene2d.go (Title r.course) s
  | Racing r ->
      let w = Lazy.force worlds.(r.course) in
      let view = if pressed (fun k -> Set_.mem "v" k.keys) then (match r.view with High -> Low | Low -> High) else r.view in
      let cars = if r.time < countdown then r.cars else push_all (List.map (drive w computer.keyboard (r.result <> None)) r.cars) in
      let r = { r with cars; view; time = r.time + 1 } in
      let r =
        if r.result = None && Topdown.lap w.track (player r).body >= laps then { r with result = Some (place w r, r.time - countdown) }
        else r
      in
      { s with scene = Racing r }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* The car on the ground, pitched and rolled by the slope under it
 * (only drawn: the car is still a point); in the air, nose up or down
 * as it rises or falls, and its shadow on the ground below it. The
 * course's headings turn counterclockwise from +x, Camera3d's clockwise
 * from -z: the model, facing -z, is turned by the heading less 90. *)
let draw_car (w : world) (c : car) : shape3d list =
  let b = c.body in
  let a = b.heading *. Float.pi /. 180. in
  let fx = cos a and fy = sin a in
  let gx, gy = slope w.map b.x b.y in
  let along = if c.air then c.vh /. Float.max 10. (Float.abs b.speed) else (gx *. fx) +. (gy *. fy) in
  let right = if c.air then 0. else (gx *. fy) -. (gy *. fx) in
  let deg v = Float.atan v *. 180. /. Float.pi in
  let x, h, z = at b.x b.y c.h in
  let car = w.course.vehicle.model c.paint |> rotate3d (deg along) 0. (deg right) |> rotate3d 0. (b.heading -. 90.) 0. |> move3d x h z in
  if not c.air then [ car ]
  else
    let ground = height w.map b.x b.y +. 0.1 in
    let p u v = at (b.x +. (fx *. v) +. (fy *. u)) (b.y +. (fy *. v) -. (fx *. u)) ground in
    [ car; polygon3d (rgb 20 20 20) [ p (-1.) (-2.); p 1. (-2.); p 1. 2.; p (-1.) 2. ] |> fade3d 0.5 ]

(* Big Red Racing's view, from high behind, or a lower one; the eye kept
 * above the ground behind the car (a hill, the canyon's wall) *)
let camera_for (w : world) (view : view) (c : car) : camera =
  let b = c.body in
  let a = b.heading *. Float.pi /. 180. in
  let fx = cos a and fy = sin a in
  let back, up = match view with High -> (16., 11.) | Low -> (10., 4.) in
  let ex = b.x -. (fx *. back) and ey = b.y -. (fy *. back) in
  let eh = Float.max (c.h +. up) (height w.map ex ey +. 2.) in
  camera ~eye:(at ex ey eh) ~target:(at (b.x +. (fx *. 6.)) (b.y +. (fy *. 6.)) (c.h +. 1.)) ~far:2000. ()

(* On the Moon, the Earth and the stars: far, so drawn around the eye,
 * moving with it, as if at infinity *)
let space (cam : camera) : shape3d list =
  let ex, ey, ez = cam.eye in
  let far (dx, dy, dz) d shape =
    let n = Float.sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz)) in
    shape |> move3d (ex +. (d *. dx /. n)) (ey +. (d *. dy /. n)) (ez +. (d *. dz /. n))
  in
  far (0.6, 0.07, -0.7) 600. (sphere (rgb 70 120 220) 45.)
  :: List.init 120 (fun k ->
         let r i = Heightmap.random 11 k i in
         far (r 0, 0.02 +. (0.3 *. Float.abs (r 1)), r 2) 700. (cube white 4.))

let text color size str = words color str |> scale size

let ordinal (n : int) : string = match n with 1 -> "1ST" | 2 -> "2ND" | 3 -> "3RD" | n -> string_of_int n ^ "TH"

let clock (frames : int) : string = Printf.sprintf "%d:%02d.%d" (frames / 3600) (frames / 60 mod 60) (frames mod 60 / 6)

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  let i, r = match s.scene with Title i -> (i, new_race i) | Racing r -> (r.course, r) in
  let w = Lazy.force worlds.(i) in
  let course = w.course in
  let me = player r in
  let cam =
    match s.scene with
    | Title _ ->
        (* turning round the grid, above the ground (the canyon's walls) *)
        let a = spin 10. computer.time *. Float.pi /. 180. in
        let ex = me.body.x +. (14. *. sin a) and ey = me.body.y -. (14. *. cos a) in
        let eh = Float.max (me.h +. 6.) (height w.map ex ey +. 3.) in
        camera ~eye:(at ex ey eh) ~target:(at me.body.x me.body.y (me.h +. 1.)) ~far:2000. ()
    | Racing r -> camera_for w r.view me
  in
  let sky = Camera3d.floor ~color:course.horizon ~ground:(-0.1) cam :: Camera3d.sky ~sky:course.sky ~horizon:course.horizon ~ground:(-0.1) cam in
  let hud_shapes =
    match s.scene with
    | Title _ ->
        [ text (rgb 230 30 30) 7. "TINY BIG RED RACING" |> move_y 300.;
          text white 4. ("< " ^ course.cname ^ " >") |> move_y 230.;
          text yellow 2.5
            (course.vehicle.vname ^ if course.gravity < 10. then "   GRAVITY 1/6" else "")
          |> move_y 185.;
          text white 2. "left/right: course   up: accelerate   down: brake   v: view" |> move_y (-300.) ]
        @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 130. ]
    | Racing r -> (
        let lap = min laps (Topdown.lap w.track me.body + 1) in
        let status =
          [ text white 3. (Printf.sprintf "LAP %d/%d" lap laps) |> move (screen.left +. 110.) (screen.top -. 40.);
            text white 3. (clock (max 0 (r.time - countdown))) |> move (screen.left +. 110.) (screen.top -. 80.);
            text yellow 5. (ordinal (place w r)) |> move (screen.right -. 80.) (screen.top -. 50.) ]
        in
        let go =
          if r.time < countdown then [ text yellow 10. (string_of_int (3 - (r.time / 60))) |> move_y 120. ]
          else if r.time < countdown + 60 then [ text (rgb 230 30 30) 10. "GO!" |> move_y 120. ]
          else []
        in
        match r.result with
        | None -> status @ go
        | Some (n, t) ->
            [ text yellow 8. (ordinal n ^ " PLACE!") |> move_y 200.; text white 4. (clock t) |> move_y 120. ]
            @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y 60. ])
  in
  let cars = List.concat_map (draw_car w) r.cars in
  (cam, sky @ (if course.gravity < 10. then space cam else []) @ (w.mesh :: cars) @ List.map hud hud_shapes)

let app = game3d view update initial_model

(* flat shading, each triangle lit by its slope; the back faces drawn
 * too, for the sky (seen from below, see Camera3d.sky) *)
let main =
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false } app
