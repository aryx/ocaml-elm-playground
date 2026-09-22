(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Gran Trak 10 (Atari, 1974; Larry Emmons and Allan
 * Alcorn -- names from memory, to check): the first racing game seen
 * from above, the whole track on one black and white screen, one car,
 * and the clock. Arrows: up the gas, down the brake, left and right the
 * wheel; 1, 2, 3 and r the gear lever.
 *
 * The cabinet had a real wheel, two pedals and a four-position gear
 * lever, and the gearbox is the game: first pulls hard but tops out
 * early, third is fast but barely moves the car from a stop. Start in
 * first, shift up as the engine screams, shift down for the hairpin.
 * Here a gear is the car's [accel] and top speed ([gears]), and the
 * engine is heard: its pitch is how far the car is into its gear's top
 * speed, so it climbs, and drops when you shift up (Audio.keep_playing,
 * changed every frame).
 *
 * The walls don't bounce you back, they stop you: a crash, the car
 * spinning for most of a second, and on your way again, pointing along
 * the track. The oil slicks ('o' on the track) are ice: no grip, no
 * steering, the car keeps sliding the way it went. Ninety seconds; a
 * point per checkpoint passed, the laps counted, the best one timed.
 * (The original's score and its time, set by the operator, from
 * memory.)
 *
 * What it uses: the racing kit's Topdown -- the car and its slide
 * ([drive], the grip turned down on the oil), the track as a loop of
 * waypoints (the checkpoints, [follow]), its walls where the center line
 * is further than half the road ([distance]), and its drawing
 * ([ribbon], white under black: the borders); Scene2d for the title and
 * the end; Sprite for the car; Audio for the engine and the crash. No
 * Camera2d but its zoom: the whole track fits on the screen, scaled to
 * it. Not Topdown.bounce: a wall here is a crash, as in 1974.
 *
 * A bit of history. Gran Trak 10 is where the top-down racing games
 * come from (Sprint 2, 1976, then Super Sprint and Super Off Road, see
 * TinySuperSprint.ml and TinySuperOffRoad.ml); its graphics were in a
 * diode matrix, a ROM wired by hand, and it lost Atari money, sold under
 * cost by mistake (both from memory, to check). Speed Race (Taito,
 * 1974) scrolled instead, the road coming down the screen.
 *
 * Exercises: a second car (Gran Trak 20, 1974, had two); an
 * automatic gearbox, and how much less fun it is; the oil slicks
 * moving between laps; the time extended for each lap, as later
 * racers did (the "checkpoint" of Out Run, 1986).
 *)
open Playground

(*****************************************************************************)
(* The track *)
(*****************************************************************************)

(* the world, the size of the screen it's drawn on (scaled to fit) *)
let world_width = 980.
let world_height = 680.

let road_width = 110.

(* the center line, clockwise from the start: a long straight, the
 * right-hand bend, a hairpin up into the middle, the left-hand side *)
let track : Topdown.track =
  { points =
      [| (-300., 220.); (300., 220.); (400., 130.); (400., -180.); (320., -240.); (150., -240.); (80., -160.);
         (80., -10.); (0., 60.); (-80., -10.); (-80., -170.); (-160., -240.); (-330., -240.); (-410., -160.);
         (-410., 130.) |];
    reach = 90.;
    corner = 200. }

(* the oil slicks: where, and how wide *)
let slicks = [ (40., 220., 30.); (400., -40., 26.); (-410., -40., 28.) ]

let on_oil (x : number) (y : number) : bool = List.exists (fun (ox, oy, r) -> Float.hypot (x -. ox) (y -. oy) < r) slicks
let in_wall (x : number) (y : number) : bool = Topdown.distance track x y > (road_width /. 2.) -. 10.

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type gear = Reverse | First | Second | Third

(* each gear's pull and top speed: first 3 times third's pull, a third
 * of its top *)
let gears (g : gear) : number * number =
  match g with Reverse -> (500., 360.) | First -> (1200., 240.) | Second -> (700., 450.) | Third -> (400., 700.)

type race = {
  car : Topdown.t;
  gear : gear;
  crash : int; (* > 0: spinning after a crash, for that many frames *)
  frames : int; (* since the start *)
  lap_start : int; (* the frame the lap started *)
  best : int option; (* the best lap, in frames *)
}

type scene = Title | Racing of race | Over of race
type model = scene Scene2d.t

let seconds = 90

let new_race : race =
  { car = Topdown.start track 0 0.; gear = First; crash = 0; frames = 0; lap_start = 0; best = None }

let initial_model = Scene2d.start Title

let score (r : race) : int = r.car.next - 1

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

let crash_sound = Audio.sfx { Sfx.explosion with decay = 0.5; volume = 0.5 }

(* pointing to the next checkpoint, after a crash *)
let heading_to_next (c : Topdown.t) : number =
  let x, y = Topdown.point track c.next in
  atan2 (y -. c.y) (x -. c.x) *. 180. /. Float.pi

let shift (k : keyboard) (g : gear) : gear =
  let key name = Set_.mem name k.keys in
  if key "1" then First else if key "2" then Second else if key "3" then Third else if key "r" then Reverse else g

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

(* one frame of the car: the gas pushes forward, or backward in
 * reverse; the brake only slows it down, never backs it up *)
let drive (k : keyboard) (r : race) : race =
  let accel, top = gears r.gear in
  let oil = on_oil r.car.x r.car.y in
  let params = { Topdown.toy with accel; grip = (if oil then 0.01 else 0.15); steering = (if oil then 0. else 3.5) } in
  let braking = k.kdown && Float.abs r.car.speed > 5. in
  let gas =
    if braking then (if r.car.speed > 0. then -3. else 3.)
    else if k.kup then (if r.gear = Reverse then -1. else 1.)
    else 0.
  in
  let car = Topdown.drive params top gas (axis k.kleft k.kright) r.car |> Topdown.follow track in
  (* the brake stops at 0 *)
  let car = if braking && car.speed *. r.car.speed < 0. then { car with speed = 0. } else car in
  if in_wall car.x car.y then begin
    Audio.play crash_sound;
    { r with car = { r.car with vx = 0.; vy = 0.; speed = 0. }; crash = 50 }
  end
  else { r with car }

let update_race (k : keyboard) (r : race) : race =
  let r = { r with frames = r.frames + 1; gear = shift k r.gear } in
  let lap = Topdown.lap track r.car in
  let r =
    if r.crash > 1 then { r with crash = r.crash - 1; car = { r.car with heading = r.car.heading +. 14. } }
    else if r.crash = 1 then { r with crash = 0; car = { r.car with heading = heading_to_next r.car } }
    else drive k r
  in
  (* a lap done: its time *)
  let r =
    if Topdown.lap track r.car > lap then
      let t = r.frames - r.lap_start in
      { r with lap_start = r.frames; best = Some (match r.best with Some b -> min b t | None -> t) }
    else r
  in
  (* the engine: from 60 to 200 hertz as the car goes from 0 to its
   * gear's top speed *)
  let _, top = gears r.gear in
  let revs = Float.min 1. (Float.abs r.car.speed /. top) in
  Audio.keep_playing "engine" (Audio.square (60. +. (140. *. revs)) |> Audio.low_pass 800. |> Audio.louder 0.25);
  r

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  match s.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (Racing new_race) s else s
  | Racing r ->
      let r = update_race computer.keyboard r in
      if r.frames >= seconds * 60 then Scene2d.go (Over r) s else { s with scene = Racing r }
  | Over _ -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* the car from above, pointing up, in white on black like the 1974
 * screen *)
let car_shape : shape =
  Sprite.pixels 5. [ ('W', white); ('G', rgb 140 140 140) ]
    [ "W.WW.W"; "WWWWWW"; "W.WW.W"; "..WW.."; ".WWWW."; ".WGGW."; ".WWWW."; "W.WW.W"; "WWWWWW"; "W.WW.W" ]

let text color size str = words color str |> scale size

let time (frames : int) : string = Printf.sprintf "%d.%d" (frames / 60) (frames mod 60 / 6)

let field : shape list =
  let x0, y0 = Topdown.point track 0 and x1, y1 = Topdown.point track 1 in
  let a = atan2 (y1 -. y0) (x1 -. x0) *. 180. /. Float.pi in
  [ Topdown.ribbon white (road_width +. 8.) track;
    Topdown.ribbon black (road_width -. 8.) track;
    (* the start line *)
    rectangle white 6. (road_width -. 8.) |> rotate a |> move x0 y0 ]
  @ List.map (fun (x, y, r) -> group [ circle (rgb 90 90 90) r; circle (rgb 50 50 50) (r *. 0.6) |> move 4. 3. ] |> move x y) slicks

let view_race (r : race) : shape list =
  let car = car_shape |> rotate (r.car.heading -. 90.) |> move r.car.x r.car.y in
  let gear = match r.gear with Reverse -> "R" | First -> "1" | Second -> "2" | Third -> "3" in
  let left = max 0 ((seconds * 60) - r.frames) in
  field
  @ [ car;
      text white 3. (Printf.sprintf "SCORE %d" (score r)) |> move (-300.) 310.;
      text white 3. ("TIME " ^ string_of_int ((left + 59) / 60)) |> move 0. 310.;
      text white 3. ("GEAR " ^ gear) |> move 300. 310.;
      text white 2. (Printf.sprintf "LAP %d   BEST %s" (Topdown.lap track r.car + 1) (match r.best with Some b -> time b | None -> "-"))
      |> move 0. (-318.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  let zoom = Float.min (screen.width /. world_width) (screen.height /. world_height) in
  let cam = { Camera2d.origin with zoom } in
  [ rectangle black screen.width screen.height ]
  @
  match s.scene with
  | Title ->
      [ Camera2d.view cam field;
        rectangle black 820. 360.;
        text white 6. "TINY GRAN TRAK 10" |> move_y 140.;
        car_shape |> scale 3. |> rotate (-90.) |> move_y 50.;
        text white 2.5 "up: gas   down: brake   left, right: wheel" |> move_y (-30.);
        text white 2.5 "1, 2, 3, r: the gear lever -- start in first!" |> move_y (-70.) ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-140.) ]
  | Racing r -> [ Camera2d.view cam (view_race r) ]
  | Over r ->
      [ Camera2d.view cam (view_race r); text white 6. "TIME UP" |> move_y 60.;
        text white 4. (Printf.sprintf "SCORE %d" (score r)) |> move_y (-10.) ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-80.) ]

let app = game view update initial_model

let main = Playground_platform.run_app app
