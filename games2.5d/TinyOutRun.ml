(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Out Run (Yu Suzuki, Sega, 1986): a red convertible on
 * a road of curves and hills, palm trees going by. Up to accelerate,
 * down to brake, left/right to steer; stay on the road (the grass slows
 * you down), and beware the curves, which throw you outwards.
 *
 * The road looks 3D, but it's all 2D shapes, the way the arcade did it,
 * a "pseudo-3D" older than polygons: Pole Position (Namco, 1982) and Out
 * Run drew roads with sprite hardware and tricks, no 3D maths beyond
 * dividing by the distance. The road is a list of segments, each a slice
 * of the road with its curve and its height ([track]); a segment's two
 * edges are projected like 3D points ([project]), and the slice between
 * them drawn as trapezoids: grass, rumble strips, road, lane marks.
 *
 *          far segments: thin, high up
 *              ___
 *             /   \           a curve is not a turn of the road in
 *            /     \          space: each segment is shifted
 *           /       \         sideways a bit more than the one
 *          /_________\        before ([dx] grows by the curve):
 *        near segments: wide   the road bends, and that's enough
 *
 * Three more tricks, each a few lines: stripes alternating light and
 * dark every few segments (the speed you feel is them rushing at you),
 * hills hiding what's behind them (a segment is drawn only if it rises
 * above the nearer ones, [visible]), and the fog, colors fading into
 * the sky with the distance.
 *
 * The algorithm follows Jake Gordon's "How to build a racing game"
 * (2012, a JavaScript Out Run in four parts), which itself follows Lou
 * Gorenfeld's "Lou's Pseudo 3d Page", the classic explanation of how the
 * arcade games did it.
 *
 * Exercises: the other cars (sprites on the road, to overtake), the
 * forks of Out Run's route (choose left or right at the end of a stage),
 * the countdown timer and checkpoints, the tilted road in curves, the
 * radio (plan_audio_teaching.md).
 *)
open Playground

(*****************************************************************************)
(* The track, and the car: the racing kit *)
(*****************************************************************************)

(* the course and the car's rules are the racing kit's (kits/racing/,
 * Road and Car), shared with games3d/TinyVirtuaRacing.ml *)
let segment_length = 200. (* world units along the road *)
let road_width = 2000. (* half the road's width, in world units *)
let rumble_length = 3 (* segments per stripe *)

let track = Road.build segment_length Road.coast
let track_length = Road.length track
let params = Car.params segment_length

type roadside = Palm | Bush | Sign

(* what stands beside a segment: the side (-1 left, 1 right) times the
 * distance from the center, in road widths *)
let roadside (index : int) : (number * roadside) list =
  if index mod 8 = 0 then [ (-1.4, Palm); (1.4, Palm) ]
  else if index mod 8 = 4 then [ ((if index mod 16 = 4 then -1.8 else 1.8), Bush) ]
  else if index mod 150 = 75 then [ (1.3, Sign) ]
  else []

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type race = {
  car : Car.t; (* its position wrapped: 0 .. track_length *)
  sky : number; (* the background's offset, turning with the curves *)
  time : int; (* frames since the start *)
  laps : int;
}

type scene = Title | Racing of race
type model = scene Scene2d.t

let new_race = { car = Car.start; sky = 0.; time = 0; laps = 0 }
let initial_model : model = Scene2d.start Title

let update_race (k : keyboard) (r : race) : race =
  let seg = Road.segment_at track r.car.position in
  let percent = r.car.speed /. params.max_speed in
  let car = Car.drive params track k r.car in
  (* a loop: past the end, a lap more, and the start again *)
  let laps = if car.position >= track_length then r.laps + 1 else r.laps in
  { car = { car with position = Float.rem car.position track_length };
    sky = r.sky +. (seg.curve *. percent *. 0.002); time = r.time + 1; laps }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  match s.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (Racing new_race) s else s
  | Racing r -> { s with scene = Racing (update_race computer.keyboard r) }

(*****************************************************************************)
(* Projection -- the trick of this game, in 23 lines (the slices it
 * gives are drawn by [view] below; see the header) *)
(*****************************************************************************)

let camera_height = 1000.
let field_of_view = 100. (* degrees, across *)
let camera_depth = 1. /. tan (field_of_view /. 2. *. Float.pi /. 180.)
let draw_distance = 150 (* segments *)

(* A point of the road, projected: its position on the screen (the
 * playground's, (0, 0) at the center, y up), and the road's half width
 * there. The camera is at (camera_x, camera_y, camera_z); a point
 * (x, y, z) in front of it, at distance dz, is scaled by
 * camera_depth / dz: twice as far, half as big. *)
type projected = { sx : number; sy : number; sw : number; scale : number }

let project (screen : screen) (x, y, z) (camera_x, camera_y, camera_z) : projected =
  let scale = camera_depth /. (z -. camera_z) in
  { sx = scale *. (x -. camera_x) *. screen.width /. 2.;
    sy = scale *. (y -. camera_y) *. screen.width /. 2.;
    sw = scale *. road_width *. screen.width /. 2.;
    scale }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

type rgb3 = int * int * int

let sky_rgb = (110, 190, 250)

(* fog: the color faded into the sky's, more with the distance *)
let fog (n : int) ((r, g, b) : rgb3) : color =
  let f = Float.min 1. ((float_of_int n /. float_of_int draw_distance) ** 2.) in
  let mix a b = int_of_float ((float_of_int a *. (1. -. f)) +. (float_of_int b *. f)) in
  let sr, sg, sb = sky_rgb in
  rgb (mix r sr) (mix g sg) (mix b sb)

let quad color (x1, y1, w1) (x2, y2, w2) : shape =
  polygon color [ (x1 -. w1, y1); (x1 +. w1, y1); (x2 +. w2, y2); (x2 -. w2, y2) ]

(* a segment's slice: grass across the whole screen, then the rumble
 * strips, the road, the lane marks, in the stripe's light or dark *)
let view_segment (screen : screen) (n : int) (seg : Road.segment) (p1 : projected) (p2 : projected) : shape list =
  let light = seg.index / rumble_length mod 2 = 0 in
  let grass = if light then (70, 160, 60) else (60, 140, 50) in
  let rumble = if light then (240, 240, 240) else (200, 40, 40) in
  let road = if light then (110, 110, 110) else (100, 100, 100) in
  let edge (p : projected) k = (p.sx, p.sy, p.sw *. k) in
  [ polygon (fog n grass)
      [ (-.screen.width, p1.sy); (screen.width, p1.sy); (screen.width, p2.sy); (-.screen.width, p2.sy) ];
    quad (fog n rumble) (edge p1 1.15) (edge p2 1.15);
    quad (fog n road) (edge p1 1.) (edge p2 1.) ]
  @ if light then [ quad (fog n (230, 230, 230)) (edge p1 0.03) (edge p2 0.03) ] else []

let palm_art =
  [ "..GG..GG.."; ".GGGGGGGG."; "GG.GGGG.GG"; "G..GTTG..G"; "....TT...."; "....TT...."; "....TT....";
    "...TT....."; "...TT....."; "...TT....."; "..TTT....."; ".TTTT....." ]

let bush_art = [ "..GGGG.."; ".GGGGGG."; "GGGGGGGG"; "GGGGGGGG" ]
let sign_art = [ "WWWWWWWWWW"; "WRRWRRWRRW"; "WWWWWWWWWW"; "....TT...."; "....TT...." ]

let roadside_shape (kind : roadside) : shape * number =
  let pal = [ ('G', rgb 40 130 40); ('T', rgb 120 80 40); ('W', white); ('R', red) ] in
  match kind with
  | Palm -> (Sprite.pixels 1. pal palm_art, 12.)
  | Bush -> (Sprite.pixels 1. pal bush_art, 4.)
  | Sign -> (Sprite.pixels 1. pal sign_art, 5.)

(* A roadside sprite, sized by the segment's scale, standing on the
 * ground beside the road: [height] is its height in pixels of its art;
 * a world unit is scale * width/2 pixels, and an art pixel is 150 world
 * units *)
let view_sprite (screen : screen) (p : projected) ((side, kind) : number * roadside) : shape =
  let shape, height = roadside_shape kind in
  let pixel = p.scale *. screen.width /. 2. *. 150. in
  shape |> scale pixel |> move (p.sx +. (side *. p.sw)) (p.sy +. (height *. pixel /. 2.))

(* the player's car, from behind: two frames for steering *)
let car_art =
  [ "....RRRRRRRR....."; "...RWWWWWWWWR...."; "..RRRRRRRRRRRR..."; ".RRRRRRRRRRRRRR.."; "RRYRRRRRRRRRRYRR.";
    "RRRRRRRRRRRRRRRR."; "KKK.RRRRRRRR.KKK."; "KKK..........KKK." ]

let car = Sprite.pixels 8. [ ('R', rgb 220 30 30); ('W', rgb 150 200 250); ('Y', yellow); ('K', rgb 30 30 30) ] car_art

(* the hills of the background, scrolling with the curves *)
let hills (screen : screen) (sky : number) : shape =
  let offset = Float.rem (sky *. screen.width) screen.width in
  group
    (List.concat_map
       (fun k ->
         List.init 5 (fun i ->
             oval (rgb 90 150 120) 500. 200. |> move ((float_of_int i *. 300.) -. 600. +. offset +. (k *. screen.width)) 0.))
       [ -1.; 0.; 1. ])

let view_road (screen : screen) (r : race) : shape list =
  let base = Road.segment_at track r.car.position in
  let percent = Float.rem r.car.position segment_length /. segment_length in
  let player_y = base.y1 +. ((base.y2 -. base.y1) *. percent) in
  let camera_y = player_y +. camera_height in
  (* near to far: project, accumulate the curve, find what's visible *)
  let rec go n x dx clip acc =
    if n >= draw_distance then acc
    else
      let seg = track.segments.((base.index + n) mod Array.length track.segments) in
      (* past the end of the track, it starts again: one lap further *)
      let loop = if seg.index < base.index then track_length else 0. in
      let z1 = (float_of_int seg.index *. segment_length) +. loop in
      let cam = (r.car.x *. road_width, camera_y, r.car.position) in
      let p1 = project screen (x, seg.y1, z1) cam in
      let p2 = project screen (x +. dx, seg.y2, z1 +. segment_length) cam in
      let behind = z1 -. r.car.position <= camera_depth in
      (* hidden behind a hill: its far edge not above what's drawn *)
      let visible = (not behind) && p2.sy > clip in
      let acc = if visible then (n, seg, p1, p2) :: acc else acc in
      go (n + 1) (x +. dx) (dx +. seg.curve) (if visible then p2.sy else clip) acc
  in
  let visible = go 0 0. (-.(base.curve *. percent)) (-.screen.height) [] in
  (* far to near, each segment's road then its sprites, so that nearer
   * things cover farther ones, like a painter *)
  List.concat_map (fun (n, seg, p1, p2) -> view_segment screen n seg p1 p2 @ List.map (view_sprite screen p1) (roadside seg.index)) visible

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  let r = match s.scene with Title -> new_race | Racing r -> r in
  let sr, sg, sb = sky_rgb in
  let background = [ rectangle (rgb sr sg sb) screen.width screen.height; hills screen r.sky ] in
  let car_shape = (if r.car.steer < 0. then car |> rotate 3. else if r.car.steer > 0. then car |> rotate (-3.) else car) |> move 0. (screen.bottom +. 90.) in
  let text color size str = words color str |> scale size in
  let hud =
    match s.scene with
    | Title ->
        [ text (rgb 250 60 40) 7. "TINY OUT RUN" |> move_y 250.;
          text white 2.5 "up: accelerate   down: brake   left/right: steer" |> move_y 170. ]
        @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 110. ]
    | Racing r ->
        [ text white 3. (Printf.sprintf "%3.0f km/h" (r.car.speed /. params.max_speed *. 290.)) |> move (screen.right -. 150.) (screen.top -. 40.);
          text white 3. (Printf.sprintf "TIME %d.%d" (r.time / 60) (r.time mod 60 / 6)) |> move (screen.left +. 150.) (screen.top -. 40.);
          text white 3. (Printf.sprintf "LAP %d" (r.laps + 1)) |> move_y (screen.top -. 40.) ]
  in
  background @ view_road screen r @ [ car_shape ] @ hud

let app = game view update initial_model

let main = Playground_platform.run_app app
