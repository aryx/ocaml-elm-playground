(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of SSX (EA Canada, 2000): a snowboard race down a
 * mountain against three riders, and in the air, tricks. Up to tuck
 * (faster), down to brake, left/right to carve; off a kicker, let go of
 * the arrows, then left/right spin, up/down flip, space grabs; land
 * straight and the trick is
 * scored and fills the boost meter, which Shift spends. Land crooked and
 * it's a wipeout.
 *
 * That loop is SSX: tricks are not a separate game, they are the fuel
 * of the race. Big air costs nothing (the mountain gives it), a trick
 * risks a wipeout, and a landed one pays back in speed. Here the three
 * parts are three rules:
 *
 *   - the rider is a car without an engine: the racing kit's Offroad
 *     (a Topdown car on ground with a height), which already pulls it
 *     down the slope and throws it off a kicker's lip; the board is a
 *     car with little grip (it carves, it slides), tucking is a little
 *     gas, and gravity does the rest;
 *   - in the air ([Offroad.t]'s [air]), the keys no longer steer: they
 *     turn the rider, a spin and a flip in degrees, and a grab held;
 *   - on landing, the angles are checked: within 50 degrees of a whole
 *     turn (half a turn for a spin: landing backwards, "switch", is
 *     fine), the trick is named and scored, e.g. 720 BACKFLIP + GRAB;
 *     otherwise the rider falls, and most of the speed is gone:
 *
 *        spin 350, flip 0      ->  "360", 200 points
 *        spin 540, flip 370    ->  "540 BACKFLIP", 600 points
 *        spin 300              ->  WIPEOUT
 *
 * The mountain is the racing kit's Road (the sections of TinyOutRun's
 * course, here all going down), walked into space by Track3d.of_road,
 * as TinyVirtuaRacing does: a stage, not a circuit. The ground is one
 * function made from it (as in TinyIgnition): the piste's height along
 * it, the kickers on it, and snow banks rising beyond its edges, which
 * Offroad's walls keep the riders in.
 *
 * Uses: the racing kit's Road, Track3d (the mountain), Topdown (the
 * waypoints, the computer's riders) and Offroad (the riding), Scene2d,
 * Camera3d. Not Physics (the rider is a point: the spin and the flip
 * are drawn, the landing only compares numbers).
 *
 * Exercises: SSX's Uber tricks (a full meter unlocks bigger ones);
 * rails to grind; the computer's riders shoving you; several courses;
 * the trick's points shown where it happened.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The mountain *)
(*****************************************************************************)

let segment_length = 4.

(* half the piste *)
let piste = 11.

(* all going down: [hill n h] descends [h] segment lengths over [n]
 * segments; the last one flat, a run-out past the finish *)
let road : Road.t =
  Road.build segment_length
    [ Road.hill 10 (-1.); Road.curve_hill 30 1.5 (-8.); Road.hill 20 (-7.); Road.curve_hill 30 (-2.) (-8.);
      Road.hill 15 (-3.); Road.curve_hill 40 2. (-12.); Road.hill 25 (-9.); Road.curve_hill 30 (-1.5) (-8.);
      Road.hill 20 (-4.); Road.curve_hill 30 1.8 (-9.); Road.hill 20 (-7.); Road.curve_hill 30 (-1.8) (-8.); Road.straight 20 ]

let ribbon : Track3d.t = Track3d.of_road ~width:piste ~degrees_per_curve:2. road

let length = Track3d.length ribbon
let finish = length -. (20. *. segment_length)

(* the kickers: a wedge 10 long, its lip 2.5 high, each just before a
 * steep section, so that the landing is downhill and the air long *)
let kickers = [ 150.; 350.; 570.; 985. ]
let kicker_length = 10.
let kicker_height = 2.5

let kicker (s : number) : number =
  List.fold_left
    (fun h k -> if s >= k && s < k +. kicker_length then h +. (kicker_height *. (s -. k) /. kicker_length) else h)
    0. kickers

let smooth (t : number) : number =
  let t = Float.max 0. (Float.min 1. t) in
  t *. t *. (3. -. (2. *. t))

(* The piste's height along it, smooth. Track3d joins its samples (a
 * segment apart) by straight lines, and each change of slope between
 * two of them is an edge a rider at 50 per second hops off (Offroad's
 * takeoff): through the same samples, a Catmull-Rom curve instead, its
 * slope continuous (the spline Track3d.build uses across, here along) *)
let profile (s : number) : number =
  let n = Track3d.segments ribbon in
  let y k = (Track3d.at ribbon (float_of_int (max 0 (min n k)) *. segment_length)).py in
  let k = int_of_float (Float.floor (s /. segment_length)) in
  let t = (s /. segment_length) -. float_of_int k in
  let p0 = y (k - 1) and p1 = y k and p2 = y (k + 1) and p3 = y (k + 2) in
  0.5
  *. ((2. *. p1) +. ((p2 -. p0) *. t)
     +. (((2. *. p0) -. (5. *. p1) +. (4. *. p2) -. p3) *. t *. t)
     +. (((3. *. p1) -. p0 -. (3. *. p2) +. p3) *. t *. t *. t))

(* The ground at [s] along and [off] across: the piste, flat across,
 * then the snow banks rising 14 over 8 beyond its edges (too steep to
 * ride up: Offroad's walls) *)
let surface (s : number) (off : number) : number = profile s +. (14. *. smooth ((Float.abs off -. piste) /. 8.))

(* the course's (x, y) seen from above is the world's (x, -z), as in
 * TinyIgnition *)
let height (x : number) (y : number) : number =
  let s, off = Track3d.locate ribbon x (-.y) in
  surface s off +. if Float.abs off < piste then kicker s else 0.

let ground : Offroad.ground = { height; gravity = 30.; lo = -10000.; hi = 10000. }

(* the waypoints, every 16 along the middle *)
let track : Topdown.track =
  let n = int_of_float (length /. 16.) in
  { points =
      Array.init n (fun i ->
          let p = Track3d.at ribbon (float_of_int i *. 16.) in
          (p.px, -.p.pz));
    reach = 14.;
    corner = 30. }

(*****************************************************************************)
(* Drawing the mountain *)
(*****************************************************************************)

(* a strip of segment [i] from [a] to [b] across, on the ground *)
let strip (color : color) (i : int) (a : number) (b : number) : shape3d =
  let p s off =
    let x, _, z = Track3d.across ribbon s off in
    (x, surface s off +. 0.05, z)
  in
  let s1 = float_of_int i *. segment_length and s2 = float_of_int (i + 1) *. segment_length in
  polygon3d color [ p s1 a; p s1 b; p s2 b; p s2 a ]

(* a pine, a cone of four faces on a trunk *)
let pine : shape3d =
  let green = rgb 30 90 50 in
  let tip = (0., 7., 0.) and c x z = (x, 1.5, z) in
  group3d
    [ box (rgb 90 60 40) 0.6 1.5 0.6 |> move_y3d 0.75; polygon3d green [ c (-2.) 2.; c 2. 2.; tip ]; polygon3d green [ c 2. 2.; c 2. (-2.); tip ];
      polygon3d green [ c 2. (-2.); c (-2.) (-2.); tip ]; polygon3d green [ c (-2.) (-2.); c (-2.) 2.; tip ] ]

let place (s : number) (off : number) (shape : shape3d) : shape3d =
  let x, _, z = Track3d.across ribbon s off in
  shape |> move3d x (surface s off) z

let kicker_shape (k : number) : shape3d =
  let p s off up =
    let x, _, z = Track3d.across ribbon s off in
    (x, surface s 0. +. 0.08 +. up, z)
  in
  let e = k +. kicker_length and w = piste *. 0.6 in
  let side = rgb 180 200 230 in
  group3d
    [ polygon3d (rgb 245 250 255) [ p k (-.w) 0.; p k w 0.; p e w kicker_height; p e (-.w) kicker_height ];
      polygon3d (rgb 60 120 220) [ p (e -. 0.6) (-.w) (kicker_height -. 0.15); p (e -. 0.6) w (kicker_height -. 0.15); p e w kicker_height; p e (-.w) kicker_height ];
      polygon3d side [ p e (-.w) 0.; p e w 0.; p e w kicker_height; p e (-.w) kicker_height ];
      polygon3d side [ p k (-.w) 0.; p e (-.w) 0.; p e (-.w) kicker_height ]; polygon3d side [ p k w 0.; p e w 0.; p e w kicker_height ] ]

(* a gate across the piste, its banner [color] *)
let gate (s : number) (color : color) : shape3d =
  let pole = box (rgb 60 60 60) 0.6 8. 0.6 |> move_y3d 4. in
  let p = Track3d.at ribbon s in
  group3d [ pole |> move_x3d (-.piste); pole |> move_x3d piste; box color ((2. *. piste) +. 1.) 2. 0.4 |> move_y3d 8. ]
  |> rotate3d 0. (-.p.heading) 0.
  |> move3d p.px (profile s) p.pz

let segment_shapes (i : int) : shape3d list =
  let s = float_of_int i *. segment_length in
  let snow = if i mod 2 = 0 then rgb 235 240 250 else rgb 225 232 245 in
  let bank = if i mod 2 = 0 then rgb 210 220 240 else rgb 200 212 235 in
  [ strip snow i (-.piste) piste; strip bank i piste (piste +. 8.); strip bank i (-.piste -. 8.) (-.piste);
    strip white i (piste +. 8.) (piste +. 30.); strip white i (-.piste -. 30.) (-.piste -. 8.) ]
  @ (if i mod 3 = 0 then [ place s (piste +. 12. +. float_of_int (i * 7 mod 11)) pine ] else [])
  @ if i mod 3 = 1 then [ place s (-.piste -. 12. -. float_of_int (i * 5 mod 13)) pine ] else []

(* the mountain in chunks of 20 segments, each built once (kept in GPU
 * buffers on the GPU backends), only those around the camera drawn *)
let chunk_size = 20

let chunks : shape3d array =
  let n = Track3d.segments ribbon in
  Array.init ((n + chunk_size - 1) / chunk_size) (fun c ->
      let first = c * chunk_size in
      let last = min n (first + chunk_size) - 1 in
      let s1 = float_of_int first *. segment_length and s2 = float_of_int (last + 1) *. segment_length in
      cached3d
        (List.concat (List.init (last - first + 1) (fun k -> segment_shapes (first + k)))
        @ List.filter_map (fun k -> if k >= s1 && k < s2 then Some (kicker_shape k) else None) kickers
        @ (if first = 0 then [ gate 8. (rgb 40 90 220) ] else [])
        @ if finish >= s1 && finish < s2 then [ gate finish (rgb 230 40 40) ] else []))

(*****************************************************************************)
(* The riders *)
(*****************************************************************************)

(* the board: little grip (it slides), tucking a little gas; the top
 * speed where gravity and the snow's drag meet *)
let board : Topdown.params = { accel = 10.; friction = 0.15; grip = 0.08; steering = 3.; steering_speed = 10. }
let top = 60.

(* a player, or the computer, by its skill *)
type driver = Player | Computer of number

type rider = {
  ride : Offroad.t;
  driver : driver;
  paint : color;
  (* how far down, for who leads and the finish (Track3d.locate's) *)
  s : number;
  (* in the air: the spin and the flip, in degrees, the frames grabbed *)
  spin : number;
  flip : number;
  grab : int;
  (* the arrows let go since the takeoff: until then, the ones held to
   * tuck and carve into the kicker do no trick *)
  armed : bool;
  (* frames of wipeout left *)
  down : int;
  (* 0 to 1, spent by Shift *)
  boost : number;
  (* the last trick, and for how many more frames it is shown *)
  said : string;
  saying : int;
  score : int;
  (* the time over the finish line *)
  time : int option;
}

let start_rider (i : int) (driver : driver) (paint : color) : rider =
  let side = [| -6.; -2.; 2.; 6. |].(i) in
  let b = Topdown.start track 1 side in
  { ride = Offroad.start ground b; driver; paint; s = 16.; spin = 0.; flip = 0.; grab = 0; armed = false; down = 0; boost = 0.; said = ""; saying = 0;
    score = 0; time = None }

(* the nearest angle to [a] that is a multiple of [step], and how far *)
let round_to (step : number) (a : number) : number * number =
  let r = Float.round (a /. step) *. step in
  (r, Float.abs (a -. r))

(* E.g. a spin of 540 and a flip of 360, grabbed: "540 BACKFLIP +
 * GRAB"; the points, 100 a half turn of spin, 300 a flip, 5 a frame
 * grabbed *)
let trick_name (spin : number) (flip : number) (grab : int) : string * int =
  let halves = int_of_float (Float.abs spin /. 180.) and flips = int_of_float (Float.abs flip /. 360.) in
  let spin_name = if halves = 0 then [] else [ string_of_int (halves * 180) ] in
  let flip_name =
    if flips = 0 then []
    else [ (if flips = 2 then "DOUBLE " else if flips > 2 then "TRIPLE " else "") ^ if flip > 0. then "BACKFLIP" else "FRONTFLIP" ]
  in
  let grab_name = if grab > 10 then [ "+ GRAB" ] else [] in
  let points = (100 * halves) + (300 * flips) + (5 * grab) in
  (String.concat " " (spin_name @ flip_name @ grab_name), points)

(* on landing: straight enough, the trick; crooked, a wipeout *)
let touch_down (r : rider) : rider =
  let spin, off_spin = round_to 180. r.spin and flip, off_flip = round_to 360. r.flip in
  if off_spin > 50. || off_flip > 50. then
    { r with ride = { r.ride with body = { r.ride.body with speed = r.ride.body.speed *. 0.2 } }; down = 90; said = "WIPEOUT!"; saying = 90;
      spin = 0.; flip = 0.; grab = 0 }
  else
    let name, points = trick_name spin flip r.grab in
    let r = { r with spin = Float.rem spin 360.; flip = 0.; grab = 0 } in
    if points = 0 then r
    else { r with said = Printf.sprintf "%s  %d" name points; saying = 90; score = r.score + points; boost = Float.min 1. (r.boost +. (float_of_int points /. 1500.)) }

let axis (a : bool) (b : bool) : number = (if a then 1. else 0.) -. if b then 1. else 0.

(* One frame of a rider. On the ground: the keys (or the computer)
 * carve, tuck and brake, Shift spends the boost; in the air, they turn
 * the rider. The computer spins a 360 off every kicker, which it always
 * lands (at these airs, its spin is over long before the ground). *)
let update_rider (k : keyboard) (r : rider) : rider =
  let r = { r with saying = max 0 (r.saying - 1) } in
  if r.time <> None then
    (* past the line: slowing to a stop on the run-out *)
    let ride = { r.ride with body = { r.ride.body with speed = r.ride.body.speed *. 0.97 } } in
    { r with ride = Offroad.drive ground track board top 0. 0. ride }
  else
    let skill = match r.driver with Computer s -> s | Player -> 1. in
    let air = r.ride.air in
    let r =
      if not air then r
      else
        match r.driver with
        | Player ->
            let armed = r.armed || not (k.kup || k.kdown || k.kleft || k.kright) in
            let turn a b = if armed then axis a b else 0. in
            { r with
              armed;
              spin = r.spin +. (9. *. turn k.kright k.kleft);
              flip = r.flip +. (6. *. turn k.kdown k.kup);
              grab = (if k.kspace then r.grab + 1 else r.grab) }
        | Computer _ ->
            (* a 360 off a kicker (it knows where they are: a drop or a
             * bump gives too short an air to finish it), always finished *)
            let off_kicker = List.exists (fun k -> r.s > k && r.s < k +. kicker_length +. 15.) kickers in
            if (off_kicker || r.spin > 0.) && r.spin < 360. then { r with spin = r.spin +. 9. } else r
    in
    let gas, steer =
      if r.down > 0 then (0., 0.)
      else
        match r.driver with
        | Player -> (Float.max 0. (axis k.kup false) -. (if k.kdown && r.ride.body.speed > 2. then 1. else 0.), axis k.kleft k.kright)
        | Computer _ -> Topdown.computer track r.ride.body
    in
    let boosting = r.driver = Player && k.kshift && r.boost > 0. && not air in
    let p = if boosting then { board with accel = board.accel +. 30. } else board in
    let ride = Offroad.drive ground track p ((if boosting then top *. 1.3 else top) *. skill) (Float.max (-1.) gas) steer r.ride in
    let s, _ = Track3d.locate ~near:r.s ribbon ride.body.x (-.ride.body.y) in
    let r = { r with ride; s; armed = r.armed && ride.air; down = max 0 (r.down - 1); boost = (if boosting then Float.max 0. (r.boost -. 0.006) else r.boost) } in
    if air && not ride.air then touch_down r else r

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type race = { riders : rider list; time : int }

type scene = Title | Racing of race

type model = scene Scene2d.t

let initial_model : model = Scene2d.start Title

let countdown = 180

let new_race () : race =
  { riders =
      List.mapi
        (fun i (d, c) -> start_rider i d c)
        [ (Computer 0.9, rgb 40 170 60); (Player, rgb 230 40 40); (Computer 0.94, rgb 40 90 230); (Computer 0.97, rgb 240 180 30) ];
    time = 0 }

let player (r : race) : rider = List.find (fun x -> x.driver = Player) r.riders

let place (r : race) (me : rider) : int =
  match me.time with
  | Some t -> 1 + List.length (List.filter (fun (x : rider) -> match x.time with Some t' -> t' < t | None -> false) r.riders)
  | None -> 1 + List.length (List.filter (fun (x : rider) -> x != me && (x.time <> None || x.s > me.s)) r.riders)

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  match s.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (Racing (new_race ())) s else s
  | Racing r when (player r).time <> None && Scene2d.pressed (fun k -> k.kspace) s -> Scene2d.go Title s
  | Racing r ->
      let riders = if r.time < countdown then r.riders else List.map (update_rider computer.keyboard) r.riders in
      let riders = List.map (fun (x : rider) -> if x.time = None && x.s >= finish then { x with time = Some (r.time - countdown) } else x) riders in
      { s with scene = Racing { riders; time = r.time + 1 } }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* The rider, facing -z: the board along the way, the body across it (a
 * snowboarder stands sideways); crouched to the board when grabbing *)
let rider_model (paint : color) (grabbing : bool) : shape3d =
  let body = if grabbing then 0.6 else 1. in
  group3d
    [ box (rgb 40 40 60) 0.55 0.12 2.3 |> move_y3d 0.1; box (rgb 50 50 70) 0.45 (0.8 *. body) 0.8 |> move_y3d (0.2 +. (0.4 *. body));
      box paint 0.6 0.8 0.5 |> move_y3d (0.2 +. (0.8 *. body) +. 0.4); box (rgb 240 200 170) 0.4 0.4 0.4 |> move_y3d (0.2 +. (0.8 *. body) +. 1.);
      box paint 0.25 0.25 1.6 |> move_y3d (if grabbing then 0.4 else 0.2 +. (0.8 *. body) +. 0.6) ]

let draw_rider (r : rider) : shape3d list =
  let b = r.ride.body in
  let pitch, roll = if r.ride.air then (0., 0.) else Offroad.pose ground r.ride in
  let x, h, z = (b.x, r.ride.h, -.b.y) in
  let fallen = if r.down > 0 then 80. else 0. in
  let rider =
    rider_model r.paint (r.grab > 0 && r.ride.air)
    |> rotate3d r.flip (-.r.spin) fallen
    |> rotate3d pitch 0. roll
    |> rotate3d 0. (b.heading -. 90.) 0.
    |> move3d x h z
  in
  if not r.ride.air then [ rider ]
  else
    let g = height b.x b.y +. 0.1 in
    [ rider; polygon3d (rgb 120 130 160) [ (x -. 0.6, g, z -. 1.2); (x +. 0.6, g, z -. 1.2); (x +. 0.6, g, z +. 1.2); (x -. 0.6, g, z +. 1.2) ] |> fade3d 0.5 ]

(* behind the rider, looking down the mountain; the eye kept above the
 * snow *)
let camera_on (r : rider) : camera =
  let b = r.ride.body in
  let a = b.heading *. Float.pi /. 180. in
  let fx = cos a and fy = sin a in
  let ex = b.x -. (fx *. 10.) and ey = b.y -. (fy *. 10.) in
  let eh = Float.max (r.ride.h +. 4.5) (height ex ey +. 2.) in
  camera ~eye:(ex, eh, -.ey) ~target:(b.x +. (fx *. 8.), r.ride.h -. 1., -.(b.y +. (fy *. 8.))) ~fov:65. ~far:2000. ()

let text color size str = words color str |> scale size

let ordinal (n : int) : string = match n with 1 -> "1ST" | 2 -> "2ND" | 3 -> "3RD" | n -> string_of_int n ^ "TH"

let clock (frames : int) : string = Printf.sprintf "%d:%02d.%d" (frames / 3600) (frames / 60 mod 60) (frames mod 60 / 6)

let boost_bar (screen : screen) (b : number) : shape list =
  let w = 200. in
  [ rectangle (rgb 40 40 60) (w +. 8.) 22. |> move (screen.left +. 130.) (screen.bottom +. 40.);
    rectangle (rgb 255 140 30) (w *. b) 14. |> move (screen.left +. 30. +. (w *. b /. 2.)) (screen.bottom +. 40.);
    text white 2. "BOOST" |> move (screen.left +. 130.) (screen.bottom +. 70.) ]

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  let r = match s.scene with Title -> new_race () | Racing r -> r in
  let me = player r in
  let cam =
    match s.scene with
    | Title -> Camera3d.orbit ~distance:14. ~height:5. ~look:1. (spin 10. computer.time) (me.ride.body.x, me.ride.h, -.me.ride.body.y)
    | Racing _ -> camera_on me
  in
  let chunk = int_of_float (me.s /. segment_length) / chunk_size in
  let mountain = List.filter_map (fun c -> if c >= 0 && c < Array.length chunks then Some chunks.(c) else None) (List.init 6 (fun k -> chunk - 1 + k)) in
  let sky = Camera3d.floor ~color:white ~ground:(-400.) cam :: Camera3d.sky ~sky:(rgb 110 170 240) ~horizon:(rgb 230 238 250) ~ground:(-400.) cam in
  let hud_shapes =
    match s.scene with
    | Title ->
        [ text (rgb 250 120 20) 8. "TINY SSX" |> move_y 300.; text white 2.5 "up: tuck   down: brake   left/right: carve   shift: boost" |> move_y 230.;
          text white 2.5 "in the air: left/right spin, up/down flip, space grab" |> move_y 195.;
          text yellow 2.5 "land straight, or wipe out" |> move_y 160. ]
        @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 100. ]
    | Racing r -> (
        let said = if me.saying > 0 then [ text (if me.said = "WIPEOUT!" then rgb 250 60 40 else yellow) 4. me.said |> move_y 180. ] else [] in
        let go =
          if r.time < countdown then [ text yellow 10. (string_of_int (3 - (r.time / 60))) |> move_y 150. ]
          else if r.time < countdown + 60 then [ text (rgb 250 120 20) 10. "GO!" |> move_y 150. ]
          else []
        in
        let status =
          [ text white 3. (clock (max 0 (r.time - countdown))) |> move (screen.left +. 110.) (screen.top -. 40.);
            text white 3. (Printf.sprintf "%d PTS" me.score) |> move (screen.left +. 110.) (screen.top -. 80.);
            text yellow 5. (ordinal (place r me)) |> move (screen.right -. 80.) (screen.top -. 50.) ]
          @ boost_bar screen me.boost
        in
        match me.time with
        | None -> status @ said @ go
        | Some t ->
            [ text yellow 7. (ordinal (place r me) ^ " PLACE!") |> move_y 220.;
              text white 4. (Printf.sprintf "%s   %d PTS" (clock t) me.score) |> move_y 150. ]
            @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y 90. ])
  in
  (cam, sky @ mountain @ List.concat_map draw_rider r.riders @ List.map hud hud_shapes)

let app = game3d view update initial_model

(* flat shading, each face lit by its slope; the back faces drawn too,
 * for the sky (seen from below, see Camera3d.sky) *)
let main =
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false } app
