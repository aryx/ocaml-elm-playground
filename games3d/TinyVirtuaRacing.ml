(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Virtua Racing (Yu Suzuki, Sega AM2, 1992): a stage,
 * from the start line to the GOAL arch, as fast as you can, on the same
 * course as games2.5d/TinyOutRun.ml -- but drawn with polygons. Up to
 * accelerate, down to brake, left/right to steer, v to change the view.
 *
 * Virtua Racing was the arcade's first great polygon racer: flat-shaded,
 * no textures, 60 frames per second, on a board (Model 1) built for it.
 * Its look is exactly playground3d's: every surface one flat color, lit
 * by the sun. And its four views, from the cockpit to far behind, were
 * its famous novelty: the "V.R." buttons (here, the v key).
 *
 * Next to TinyOutRun, the lesson is what polygons changed:
 *   - the road really turns: Road.centerline walks the course, each
 *     segment's curve turning its heading, and each segment of road is a
 *     quad in space between two edges of the center line; in TinyOutRun
 *     a curve is only a sideways shift of the picture;
 *   - so the camera can be anywhere: behind the car, above it, in it --
 *     TinyOutRun's camera can only be where its trick works;
 *   - hills hide what's behind them because of the z-buffer, not because
 *     of a special case (TinyOutRun's [visible]).
 * The course and the car (its speed, steering, the curves' push, the
 * grass) are the racing kit's (kits/racing/), shared by the two games.
 *
 * The road is static: it's cut in chunks of 40 segments, each a cached3d
 * group (kept in GPU buffers on the GPU backends), and only the chunks
 * around the car are drawn.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The course, in space *)
(*****************************************************************************)

let segment_length = 2.
let road_width = 6. (* half of it *)
let rumble_length = 3

let track = Road.build segment_length Road.coast
let params = Car.params segment_length

(* how many degrees a segment of curve 1 turns: the road winds without
 * coming back near itself *)
let points = Road.centerline 0.8 track

let radians d = d *. Float.pi /. 180.
let forward (h : number) = (sin (radians h), 0., -.cos (radians h))
let right_of (h : number) = (cos (radians h), 0., sin (radians h))

(* the point at [offset] across the road (in world units, right
 * positive), at the i-th edge of the center line *)
let across (i : int) (offset : number) : number * number * number =
  let p = points.(i) in
  let rx, _, rz = right_of p.heading in
  (p.x +. (offset *. rx), p.y, p.z +. (offset *. rz))

(* a strip of the i-th segment, from [a] to [b] across the road:
 * counterclockwise seen from above (near left, near right, far right,
 * far left), so that its face is up *)
let strip (color : color) (i : int) (a : number) (b : number) : shape3d =
  polygon3d color [ across i a; across i b; across (i + 1) b; across (i + 1) a ]

let segment_shapes (i : int) : shape3d list =
  let light = i / rumble_length mod 2 = 0 in
  let grass = if light then rgb 70 160 60 else rgb 60 140 50 in
  let rumble = if light then rgb 240 240 240 else rgb 200 40 40 in
  let road = if light then rgb 110 110 110 else rgb 100 100 100 in
  let w = road_width in
  [ strip grass i (-6. *. w) (-1.15 *. w); strip rumble i (-1.15 *. w) (-.w); strip road i (-.w) w;
    strip rumble i w (1.15 *. w); strip grass i (1.15 *. w) (6. *. w) ]
  @ if light then [ strip white i (-0.03 *. w) (0.03 *. w) ] else []

(* the scenery, as low-poly as Virtua Racing's, turned to face the road *)
let tree : shape3d =
  group3d [ box (rgb 110 70 30) 0.5 2.4 0.5 |> move_y3d 1.2; box (rgb 40 120 40) 2.4 2. 2.4 |> rotate3d 0. 45. 0. |> move_y3d 3. ]

let bush : shape3d = box (rgb 50 130 50) 1.6 1. 1.6 |> move_y3d 0.5

let sign : shape3d =
  group3d
    [ box (rgb 90 90 90) 0.3 2. 0.3 |> move3d (-1.5) 1. 0.; box (rgb 90 90 90) 0.3 2. 0.3 |> move3d 1.5 1. 0.;
      box white 4. 1.4 0.3 |> move_y3d 2.6; box red 3.4 0.5 0.35 |> move_y3d 2.6 ]

let place (i : int) (offset : number) (shape : shape3d) : shape3d =
  let x, y, z = across i offset in
  shape |> rotate3d 0. (-.points.(i).heading) 0. |> move3d x y z

(* the same places as TinyOutRun's palms, bushes and signs *)
let scenery (i : int) : shape3d list =
  let w = road_width in
  if i mod 8 = 0 then [ place i (-1.4 *. w) tree; place i (1.4 *. w) tree ]
  else if i mod 8 = 4 then [ place i ((if i mod 16 = 4 then -1.8 else 1.8) *. w) bush ]
  else if i mod 150 = 75 then [ place i (1.3 *. w) sign ]
  else []

(* the start line, and the GOAL arch at the end *)
let segments = Array.length track.segments

let start_line : shape3d list = [ strip white 0 (-.road_width) road_width ]

let goal : shape3d list =
  let w = road_width in
  let arch =
    group3d
      [ box (rgb 230 230 230) 1. 7. 1. |> move3d (-.w -. 1.) 3.5 0.; box (rgb 230 230 230) 1. 7. 1. |> move3d (w +. 1.) 3.5 0.;
        box (rgb 30 60 200) ((2. *. w) +. 3.) 1.6 0.6 |> move_y3d 7. ]
  in
  [ strip white (segments - 2) (-.w) w; place (segments - 2) 0. arch ]

let chunk_size = 40

let chunks : shape3d array =
  Array.init ((segments + chunk_size - 1) / chunk_size) (fun c ->
      let first = c * chunk_size in
      let last = min segments (first + chunk_size) - 1 in
      let shapes = List.concat (List.init (last - first + 1) (fun k -> segment_shapes (first + k) @ scenery (first + k))) in
      let last_chunk = (segments - 1) / chunk_size in
      cached3d ((if c = 0 then start_line else []) @ shapes @ if c = last_chunk then goal else []))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type view = Chase | Far | Cockpit | Above

type race = { car : Car.t; time : int; view : view }

type scene = Title | Racing of race | Finished of race

type model = scene Scene2d.t

let new_race = { car = Car.start; time = 0; view = Chase }
let initial_model : model = Scene2d.start Title

let finish_line = Road.length track -. (2. *. segment_length)

let next_view = function Chase -> Far | Far -> Cockpit | Cockpit -> Above | Above -> Chase

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let pressed_v = Scene2d.pressed (fun k -> Set_.mem "v" k.keys) s in
  match s.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (Racing new_race) s else s
  | Racing r ->
      let car = Car.drive params track computer.keyboard r.car in
      let r = { car; time = r.time + 1; view = (if pressed_v then next_view r.view else r.view) } in
      if car.position >= finish_line then Scene2d.go (Finished r) s else { s with scene = Racing r }
  | Finished r ->
      (* coasting to a stop after the line *)
      let car = Car.drive params track { computer.keyboard with kup = false; kdown = true } r.car in
      let s = { s with scene = Finished { r with car } } in
      if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* the car, from its low-poly parts, facing -z *)
let car_model : shape3d =
  let red = rgb 210 30 30 and black = rgb 30 30 30 in
  let wheel = box black 0.5 0.7 0.9 in
  group3d
    [ box red 2. 0.6 4. |> move_y3d 0.6; box (rgb 150 200 240) 1.5 0.5 1.6 |> move3d 0. 1.15 0.4;
      box red 2.2 0.15 0.6 |> move3d 0. 1.2 1.9; wheel |> move3d (-1.1) 0.35 (-1.3); wheel |> move3d 1.1 0.35 (-1.3);
      wheel |> move3d (-1.1) 0.35 1.3; wheel |> move3d 1.1 0.35 1.3 ]

(* where the car is: on the center line at its position, then across *)
let car_place (car : Car.t) : (number * number * number) * number =
  let s = Float.min car.position (Road.length track -. 0.001) in
  let i = int_of_float (s /. segment_length) in
  let f = (s -. (float_of_int i *. segment_length)) /. segment_length in
  let p = points.(i) and q = points.(i + 1) in
  let heading = q.heading in
  let rx, _, rz = right_of heading in
  let off = car.x *. road_width in
  ((p.x +. ((q.x -. p.x) *. f) +. (off *. rx), p.y +. ((q.y -. p.y) *. f), p.z +. ((q.z -. p.z) *. f) +. (off *. rz)), heading)

(* the four views, all behind the car (Camera3d.behind), from near and
 * low to high and far; the cockpit's eye in front of the car's center *)
let camera_for (view : view) ((x, y, z) : number * number * number) (heading : number) : camera =
  let at back height ahead look = Camera3d.behind ~back ~height ~ahead ~look { x; y; z; heading } in
  match view with
  | Chase -> at 9. 3.5 8. 1.
  | Far -> at 20. 8. 10. 1.
  | Cockpit -> at (-0.5) 1.4 20. 1.2
  | Above -> at 12. 30. 12. 0.

(* the land around, below the road (lowest at height 0), a blue sky, a
 * hazy horizon (Camera3d.sky: seen from below, see [main]) *)
let sky_and_land (cam : camera) : shape3d list =
  Camera3d.floor ~color:(rgb 70 140 60) ~ground:(-0.1) cam
  :: Camera3d.sky ~sky:(rgb 150 205 250) ~horizon:(rgb 90 150 80) ~ground:(-0.1) cam

let text color size str = words color str |> scale size

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  let r = match s.scene with Title -> new_race | Racing r | Finished r -> r in
  let pos, heading = car_place r.car in
  let heading_shown = heading +. (r.car.steer *. 6.) in
  let cam =
    match s.scene with
    | Title ->
        (* turning around the car on the start line *)
        Camera3d.orbit ~distance:10. ~height:4. ~look:1. (spin 12. computer.time) pos
    | _ -> camera_for r.view pos heading
  in
  let x, y, z = pos in
  let car = car_model |> rotate3d 0. (-.heading_shown) 0. |> move3d x y z in
  let chunk = int_of_float (r.car.position /. segment_length) / chunk_size in
  let road = List.filter_map (fun c -> if c >= 0 && c < Array.length chunks then Some chunks.(c) else None) (List.init 6 (fun k -> chunk - 1 + k)) in
  let hud_shapes =
    match s.scene with
    | Title ->
        [ text (rgb 250 60 40) 7. "TINY VIRTUA RACING" |> move_y 300.;
          text white 2.5 "up: accelerate   down: brake   left/right: steer   v: view" |> move_y 230. ]
        @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 170. ]
    | Racing r ->
        [ text white 3. (Printf.sprintf "%3.0f km/h" (r.car.speed /. params.max_speed *. 290.)) |> move (screen.right -. 150.) (screen.top -. 40.);
          text white 3. (Printf.sprintf "TIME %d.%d" (r.time / 60) (r.time mod 60 / 6)) |> move (screen.left +. 150.) (screen.top -. 40.) ]
    | Finished r ->
        [ text yellow 8. "GOAL!" |> move_y 250.; text white 4. (Printf.sprintf "TIME %d.%d" (r.time / 60) (r.time mod 60 / 6)) |> move_y 160. ]
        @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y 100. ]
  in
  (cam, sky_and_land cam @ road @ [ car ] @ List.map hud hud_shapes)

let app = game3d view update initial_model

(* flat shading, Virtua Racing's look; the back faces drawn too, for the
 * sky (see [sky_and_land]) *)
let main =
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false } app
