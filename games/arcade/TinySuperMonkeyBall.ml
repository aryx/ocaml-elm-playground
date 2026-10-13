(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Super Monkey Ball (Toshihiro Nagoshi, Amusement
 * Vision, Sega, 2001; Monkey Ball in the arcades a few months before,
 * its stick shaped like a banana): a monkey in a ball, on stages
 * floating over the sea with no rail around them, and a goal to roll
 * through before the clock runs out. Arrows to tilt the stage; the
 * ball rolls down wherever down now is. Bananas on the way, three
 * monkeys, sixty seconds a stage.
 *
 * It was one of the GameCube's launch games, and the purest: one stick
 * and no button. Marble Madness (TinyMarbleMadness.ml) pushes the
 * ball; this one, like the wooden labyrinths with two knobs, and like
 * Cameltry (TinyCameltry.ml) in 2D, moves the world under it. (Names
 * and dates from memory, to check.)
 *
 * The trick of this game -- the stage never tilts. A stage turned
 * about the ball would have to push it, lift it, and carry everything
 * on it; instead the stage stays still, a world of immovable boxes,
 * and two lies are told that agree with each other ([up_of]):
 *
 *      what you see                    what is simulated
 *
 *          o                                o
 *    ------+--___                    -------+-------  the floor, still
 *                ---  far end down          |\
 *    (the camera turned back,               | \  gravity, turned
 *     about the ball)                         v   forward
 *
 *   - gravity is turned: it pulls along -up, where [up] is the
 *     vertical leaned back by the tilt, so that a level floor is a
 *     slope to the ball ([roll]). Cameltry's sentence, a dimension
 *     up: turning the maze is only turning gravity;
 *   - the camera is turned the other way, about the ball: its own
 *     "up" is that same [up], and it sits behind and above the ball
 *     as measured with it ([camera_at]). What it then shows is a
 *     stage tilted under a ball, pivoting on the ball, which is what
 *     the original shows.
 *
 * Everything else follows from the engine. The ball is a rough sphere
 * of Physics3d, the stage its boxes ([slab], [ramp]: the same value is
 * the collision and the drawing), and nothing here says how a ball
 * rolls: the 5/7 of TinyMarbleMadness's header is the engine's, the
 * ball leaves a crest when it is fast enough, and lands, and bounces.
 * The rules around it are a page: the goal's gate, the bananas, the
 * fall out, the clock.
 *
 * The camera looks the way the ball goes ([heading], turned a little
 * each frame towards the velocity), and the stick is read from behind
 * the ball: up is always "away from me". This is why the game needs no
 * camera button, and why pulling back to brake is safe but pulling
 * back for long swings the view round.
 *
 * What it uses: Physics3d (a [world] to [simulate]: the slabs, the
 * ball; [push] for the part of gravity along the floor; [ray] to keep
 * the camera out of the floor), Camera3d ([forward], [orbit] for the
 * title, [floor] for the sea, and [sky], hence both sides of the faces
 * drawn), Scene2d (title, play, the three ways a stage ends, game
 * over), Audio's ready-made sounds, cached3d for each stage. Not Physics3d's
 * sleeping (one body moves, and a tilt is not a touch), nor its
 * continuous collision (a ball of 0.5 m at 25 m/s goes 0.4 m a tick).
 * No kit: TinyMarbleMadness's course is a height map, this one is
 * boxes.
 *
 * The flag stage=4 starts at the fourth stage (the original's
 * practice mode):
 *   dune exec games/arcade/TinySuperMonkeyBall.exe -- stage=4
 * or ?stage=4 on the web.
 *
 * Exercises: the monkey (the ball is opaque here: two colours, to see
 * it roll); the ball's shadow, which the jump of the last stage
 * misses; a sea and a sky that stay level while the stage tilts (here
 * they tilt with it, which gives the trick away); moving platforms and bumpers (Physics3d's kinematic bodies: an immovable
 * body [moving]); a banked curve ([slab ~axis ~degrees] is ready for
 * it); the original's bonus for the time left and the extra monkey
 * every 100 bananas; the stick as the mouse's place on the screen, for
 * a tilt as fine as the GameCube's stick; the party games (Monkey
 * Target is a glider, Monkey Bowling a bowling alley).
 *)
open Playground
open Playground3d

type vec = Vec3.t

let rad (deg : number) : number = deg *. Float.pi /. 180.
let deg (rad : number) : number = rad *. 180. /. Float.pi

(*****************************************************************************)
(* The stages *)
(*****************************************************************************)

let thick = 0.5 (* a slab's thickness *)
let light = rgb 250 220 120
let dark = rgb 230 150 60
let side = rgb 150 80 40

(* [slab ~axis ~degrees top w d]: a piece of floor [w] wide (x) and [d]
 * deep (z), the middle of its top at [top], turned [degrees] about
 * [axis] (a slope, a banked curve). An immovable body whose shape is
 * what is drawn: a checkerboard of tiles about 1 m wide over a box. *)
let slab ?(axis = (1., 0., 0.)) ?(degrees = 0.) (top : vec) (w : number) (d : number) : Physics3d.body =
  let nx = max 1 (Float.to_int (Float.round w)) and nz = max 1 (Float.to_int (Float.round d)) in
  let tw = w /. float_of_int nx and td = d /. float_of_int nz and y = thick /. 2. in
  let tile i j =
    let x0 = (float_of_int i *. tw) -. (w /. 2.) and z0 = (float_of_int j *. td) -. (d /. 2.) in
    polygon3d (if (i + j) mod 2 = 0 then light else dark) [ (x0, y, z0); (x0, y, z0 +. td); (x0 +. tw, y, z0 +. td); (x0 +. tw, y, z0) ]
  in
  (* the box a hair lower than the tiles, not to fight them for the
   * same pixels *)
  let under = box side w (thick -. 0.02) d |> move_y3d (-0.01) in
  let shape = group3d (under :: List.concat (List.init nx (fun i -> List.init nz (tile i)))) in
  let b = Physics3d.body shape |> Physics3d.immovable |> Physics3d.rough 0.8 |> Physics3d.pointing axis degrees in
  let x, y, z = Vec3.sub top (Vec3.scale (thick /. 2.) (Quat.rotate b.orientation (0., 1., 0.))) in
  b |> Physics3d.at x y z

(* [ramp a b w]: a slope [w] wide whose top goes from [a] to [b], the
 * middles of its two ends, along z (a.x = b.x) or along x. E.g. from
 * (0, 0, -6) to (0, -3, -16): 10 m north and 3 m down, a slab 10.44 m
 * long turned -16.7 degrees about x. *)
let ramp ((ax, ay, az) : vec) ((bx, by, bz) : vec) (w : number) : Physics3d.body =
  let mid = ((ax +. bx) /. 2., (ay +. by) /. 2., (az +. bz) /. 2.) in
  if ax = bx then slab ~degrees:(deg (atan ((ay -. by) /. (bz -. az)))) mid w (Float.hypot (by -. ay) (bz -. az))
  else slab ~axis:(0., 0., 1.) ~degrees:(deg (atan ((by -. ay) /. (bx -. ax)))) mid (Float.hypot (by -. ay) (bx -. ax)) w

(* A stage: the ball starts over the origin, looking north (-z); [goal]
 * is the middle of the gate's foot, the gate across x. *)
type stage = { name : string; slabs : Physics3d.body list; goal : vec; bananas : vec list }

(* [row from step n]: n bananas in a line *)
let row (from : vec) (step : vec) (n : int) : vec list =
  List.init n (fun i -> Vec3.add from (Vec3.scale (float_of_int i) step))

let stages : stage array =
  [| (* a field, to learn the stick; the original's first stage too *)
     { name = "PLAIN"; slabs = [ slab (0., 0., -7.) 8. 18. ]; goal = (0., 0., -14.); bananas = row (0., 0., -4.) (0., 0., -2.) 4 };
     (* two islands and 12 m of a bridge 2 m wide: a ball 1 m wide has
      * half a metre to spare on each side *)
     { name = "BRIDGE";
       slabs = [ slab (0., 0., -2.) 6. 8.; slab (0., 0., -12.) 2. 12.; slab (0., 0., -22.) 6. 8. ];
       goal = (0., 0., -24.);
       bananas = row (0., 0., -8.) (0., 0., -2.) 5 };
     (*    D     the goal on D; bananas at the inside of each corner,
      *    +---+ where cutting it short is tempting
      *        C
      *    +---+
      *    A       *)
     { name = "HAIRPINS";
       slabs =
         [ slab (0., 0., -3.) 4. 10.; slab (5., 0., -10.) 14. 4.; slab (10., 0., -17.) 4. 10.; slab (5., 0., -24.) 14. 4.;
           slab (0., 0., -31.) 4. 10. ];
       goal = (0., 0., -34.);
       bananas = [ (2.5, 0., -8.5); (5., 0., -10.); (7.5, 0., -11.5); (7.5, 0., -22.5); (5., 0., -24.); (2.5, 0., -25.5) ] };
     (* down 3 m and up 2: the speed gained going down is what climbs
      * the narrower slope after it, and what overshoots the last island *)
     { name = "VALLEY";
       slabs =
         [ slab (0., 0., -2.) 5. 8.; ramp (0., 0., -6.) (0., -3., -16.) 5.; slab (0., -3., -19.) 5. 6.;
           ramp (0., -3., -22.) (0., -1., -30.) 3.; slab (0., -1., -34.) 5. 8. ];
       goal = (0., -1., -36.);
       bananas = row (0., -0.6, -8.) (0., -0.6, -2.) 4 @ row (0., -2.5, -24.) (0., 0.5, -2.) 3 };
     (* a slope, a kicker at its foot, and 6 m of nothing: too slow
      * falls short, and the island is wide because nobody steers in
      * the air *)
     { name = "LEAP";
       slabs =
         [ slab (0., 0., -2.) 4. 8.; ramp (0., 0., -6.) (0., -4., -18.) 4.; ramp (0., -4., -18.) (0., -3.4, -21.) 4.;
           slab (0., -6., -33.) 9. 12. ];
       goal = (0., -6., -36.);
       bananas = row (0., -0.9, -9.) (0., -1., -3.) 3 @ row (0., -2.5, -22.5) (0., -0.3, -1.5) 3 } |]

(* fallen below this: out *)
let lowest (s : stage) : number = List.fold_left (fun low (b : Physics3d.body) -> Float.min low b.y) 0. s.slabs -. 8.

(*****************************************************************************)
(* The ball *)
(*****************************************************************************)

let radius = 0.5
(* three times the Earth's: a stage is crossed in seconds *)
let gravity = 30.
let max_tilt = 20. (* degrees, the stick pushed all the way *)
let drag = 0.25 (* per second: the top speed, about 30 m/s on a floor tilted all the way *)

(* a sphere of two halves, facets in its own frame, to see it roll *)
let ball_shape : shape3d =
  let lats = 8 and lons = 12 in
  let point lat lon =
    let theta = Float.pi *. float_of_int lat /. float_of_int lats in
    let phi = 2. *. Float.pi *. float_of_int lon /. float_of_int lons in
    (radius *. sin theta *. cos phi, radius *. cos theta, radius *. sin theta *. sin phi)
  in
  group3d
    (List.concat
       (List.init lats (fun lat ->
            List.init lons (fun lon ->
                polygon3d
                  (if lat < lats / 2 then rgb 225 50 50 else white)
                  [ point lat lon; point lat (lon + 1); point (lat + 1) (lon + 1); point (lat + 1) lon ]))))

let new_ball : Physics3d.body =
  Physics3d.body ball_shape |> Physics3d.ball |> Physics3d.rough 0.8 |> Physics3d.bouncy 0.3 |> Physics3d.at 0. radius 0.

(*****************************************************************************)
(* The tilt -- the trick of this game *)
(*****************************************************************************)

(* [up_of heading (pitch, roll)]: the vertical as the tilted stage has
 * it, a unit vector. [pitch] and [roll] are the stick, -1 to 1, read
 * from behind a ball going towards [heading]: pitch 1 leans the
 * vertical back by [max_tilt] (the far end of the floor goes down),
 * roll 1 leans it left (the right side goes down). E.g. heading 0
 * (north, -z) and the stick up: (0, cos 20, sin 20). *)
let up_of (heading : number) ((pitch, roll) : number * number) : vec =
  let fx, fz = Camera3d.forward heading in
  let p = tan (rad (max_tilt *. pitch)) and r = tan (rad (max_tilt *. roll)) in
  (* the right of forward (fx, fz) is (-fz, fx) *)
  Vec3.normalize ((-.p *. fx) +. (r *. fz), 1., (-.p *. fz) -. (r *. fx))

(* One tick of the world under a gravity along -[up]. Physics3d's
 * gravity is a number, straight down: it gets the part of ours along
 * y, and the ball, the only thing that moves, is pushed by the rest.
 * The slabs come first in the world, the ball last. *)
let roll (up : vec) (w : Physics3d.world) : Physics3d.world =
  let ux, uy, uz = up in
  let last = List.length w.bodies - 1 in
  let tilted i (b : Physics3d.body) =
    if i < last then b
    else
      b
      |> Physics3d.push (-.gravity *. b.mass *. ux) 0. (-.gravity *. b.mass *. uz)
      |> Physics3d.slow drag |> Physics3d.spin_slow drag
  in
  Physics3d.simulate ~gravity:(gravity *. uy) ~sleeping:false { w with bodies = List.mapi tilted w.bodies }

(* The camera a tilted stage is seen from: behind the ball and above
 * it, where "above" is [up] and "behind" is the heading made square to
 * it -- so that the floor leans on the screen as gravity leans in the
 * world.
 *
 * One correction, the camera's own collision: leaning forward lowers
 * the eye to 0.6 m over the ball's floor, and going down a slope the
 * floor behind the ball is higher than that -- the eye was inside the
 * last stage's slope, showing its underside. So a ray is cast down on
 * the [slabs] from over the eye, and the eye kept [clear] above what it
 * meets. *)
let clear = 1.2

let camera_at (slabs : Physics3d.body list) ((x, y, z) : vec) (heading : number) (up : vec) : camera =
  let fx, fz = Camera3d.forward heading in
  let f = (fx, 0., fz) in
  let f = Vec3.normalize (Vec3.sub f (Vec3.scale (Vec3.dot f up) up)) in
  let from back height = Vec3.add (x, y, z) (Vec3.add (Vec3.scale (-.back) f) (Vec3.scale height up)) in
  let ex, ey, ez = from 6.5 3. in
  let ey =
    match Physics3d.ray ~from:(ex, ey +. 10., ez) ~direction:(0., -1., 0.) slabs with
    | Some (_, d) -> Float.max ey (ey +. 10. -. d +. clear)
    | None -> ey
  in
  camera ~eye:(ex, ey, ez) ~target:(from (-3.) 0.5) ~up ~far:2400. ()

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  stage : int; (* in [stages] *)
  world : Physics3d.world; (* its slabs, then the ball *)
  tilt : number * number; (* pitch and roll, -1 to 1, following the stick *)
  heading : number; (* where the camera looks: degrees, 0 north, 90 east *)
  cam : camera;
  left : vec list; (* the stage's bananas not taken yet *)
  time_left : int; (* frames *)
  bananas : int;
  monkeys : int;
}

type ending = Goal | Fall_out | Time_over

type scene = Title | Playing of play | Ended of ending * play | Game_over of play | All_clear of play
type model = scene Scene2d.t

let ball (p : play) : Physics3d.body = List.nth p.world.bodies (List.length p.world.bodies - 1)

let stage_time = 60 * 60
let ready = 90 (* frames of READY before the clock and the stick start *)

let start ~(stage : int) ~(bananas : int) ~(monkeys : int) : play =
  let s = stages.(stage) in
  { stage; world = Physics3d.world (s.slabs @ [ new_ball ]); tilt = (0., 0.); heading = 0.;
    cam = camera_at s.slabs (0., radius, 0.) 0. (0., 1., 0.); left = s.bananas; time_left = stage_time; bananas; monkeys }

let initial_model : model = Scene2d.start Title

(* stage=4: the stage a game starts at, 1 by default *)
let first_stage (flags : flags) : int =
  match Option.bind (List.assoc_opt "stage" flags) int_of_string_opt with
  | Some n when n >= 1 && n <= Array.length stages -> n - 1
  | _ -> 0

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let stick (k : keyboard) : number * number =
  let key b = if b then 1. else 0. in
  (key k.kup -. key k.kdown, key k.kright -. key k.kleft)

(* [a] turned [fraction] of the way to [b], the short way round *)
let turn_to (fraction : number) (b : number) (a : number) : number =
  let d = Float.rem (b -. a) 360. in
  let d = if d > 180. then d -. 360. else if d < -180. then d +. 360. else d in
  a +. (fraction *. d)

(* [advance ~filming stick p]: one frame. The tilt follows the stick a
 * tenth of the way each frame (a floor does not jump, and it makes the
 * arrows a stick); the heading follows the ball's velocity, when it has
 * one worth following. When not [filming], the camera stays where it
 * was and only watches: the ball falling away. *)
let advance ~(filming : bool) ((sp, sr) : number * number) (p : play) : play =
  let tp, tr = p.tilt in
  let tilt = (tp +. (0.1 *. (sp -. tp)), tr +. (0.1 *. (sr -. tr))) in
  let up = up_of p.heading tilt in
  let world = roll up p.world in
  let p = { p with tilt; world } in
  let b = ball p in
  let heading = if Float.hypot b.vx b.vz > 1.5 then turn_to 0.04 (deg (atan2 b.vx (-.b.vz))) p.heading else p.heading in
  let at = (b.x, b.y, b.z) in
  { p with heading; cam = (if filming then camera_at stages.(p.stage).slabs at heading up else { p.cam with target = at }) }

let near (d : number) ((x, y, z) : vec) (b : Physics3d.body) : bool = Vec3.length (Vec3.sub (x, y, z) (b.x, b.y, b.z)) < d

(* through the gate: between its posts, 3 m apart, under its bar *)
let through_gate ((gx, gy, gz) : vec) (b : Physics3d.body) : bool =
  Float.abs (b.x -. gx) < 1.5 -. radius && Float.abs (b.z -. gz) < 0.5 && b.y > gy && b.y < gy +. 2.5

let update_play (frames : int) (keyboard : keyboard) (p : play) : scene =
  let playing = frames >= ready in
  let p = advance ~filming:true (if playing then stick keyboard else (0., 0.)) p in
  let b = ball p and s = stages.(p.stage) in
  (* a banana floats at the ball's height over where it is given *)
  let taken, left = List.partition (fun (x, y, z) -> near 0.9 (x, y +. radius, z) b) p.left in
  if taken <> [] then Audio.play Audio.coin;
  let p = { p with left; bananas = p.bananas + List.length taken; time_left = (if playing then p.time_left - 1 else p.time_left) } in
  if through_gate s.goal b then (Audio.play Audio.powerup; Ended (Goal, p))
  else if b.y < lowest s then (Audio.play Audio.explosion; Ended (Fall_out, p))
  else if p.time_left <= 0 then (Audio.play Audio.laser; Ended (Time_over, p))
  else Playing p

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (start ~stage:(first_stage computer.flags) ~bananas:0 ~monkeys:3)) s else s
  | Playing p -> (
      match update_play s.frames computer.keyboard p with Playing p -> { s with scene = Playing p } | ended -> Scene2d.go ended s)
  | Ended (ending, p) ->
      (* the ball rolls on, the stage coming back level *)
      let p = advance ~filming:(ending <> Fall_out) (0., 0.) p in
      if s.frames < 150 then { s with scene = Ended (ending, p) }
      else
        let again stage monkeys = Playing (start ~stage ~bananas:p.bananas ~monkeys) in
        Scene2d.go
          (match ending with
          | Goal -> if p.stage + 1 < Array.length stages then again (p.stage + 1) p.monkeys else All_clear p
          | Fall_out | Time_over -> if p.monkeys > 1 then again p.stage (p.monkeys - 1) else Game_over p)
          s
  | Game_over _ | All_clear _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let sea = rgb 30 110 170

(* the sea far below and the sky, round the camera (whose far plane is
 * pushed back for them: [camera_at]) *)
let backdrop (ground : number) (cam : camera) : shape3d list =
  Camera3d.floor ~color:sea ~ground cam :: Camera3d.sky ~sky:(rgb 120 180 240) ~horizon:sea ~ground cam
let text color size str = words color str |> scale size

(* the goal: two posts 3 m apart and a bar, across x *)
let gate ((x, y, z) : vec) : shape3d =
  let post dx = box (rgb 60 90 220) 0.25 2.5 0.25 |> move3d dx 1.25 0. in
  group3d [ post (-1.5); post 1.5; box (rgb 240 240 250) 3.6 0.6 0.3 |> move_y3d 2.8 ] |> move3d x y z

let stage_shapes : shape3d array =
  Array.map (fun (s : stage) -> cached3d (gate s.goal :: List.map Physics3d.draw s.slabs)) stages

(* a banana: two yellow sticks at an angle, turning *)
let banana (time : time) ((x, y, z) : vec) : shape3d =
  let half a = box (rgb 255 225 40) 0.16 0.45 0.16 |> move_y3d 0.2 |> rotate3d 0. 0. a in
  group3d [ half 35.; half 145. ] |> rotate3d 0. (spin 2. time) 0. |> move3d x (y +. radius) z

let view_play (computer : computer) (p : play) : camera * shape3d list =
  let screen = computer.screen and b = ball p in
  let label color size str x y = hud (text color size str |> move x y) in
  let t = max 0 p.time_left in
  ( p.cam,
    backdrop (lowest stages.(p.stage) -. 30.) p.cam
    @ [ stage_shapes.(p.stage); Physics3d.draw b ]
    @ List.map (banana computer.time) p.left
    @ [ label (if t < 600 then red else yellow) 5. (Printf.sprintf "%02d:%02d" (t / 60) (t mod 60 * 100 / 60)) 0. (screen.top -. 50.);
        label white 2.5 (Printf.sprintf "STAGE %d  %s" (p.stage + 1) stages.(p.stage).name) (screen.left +. 150.) (screen.top -. 40.);
        label yellow 3. (Printf.sprintf "BANANAS %d" p.bananas) (screen.right -. 150.) (screen.top -. 40.);
        label white 2.5 (Printf.sprintf "MONKEYS %d" p.monkeys) (screen.left +. 110.) (screen.bottom +. 40.);
        (* the original's speedometer, in miles per hour as it was *)
        label white 2.5 (Printf.sprintf "%.0f MPH" (Physics3d.speed b *. 2.237)) (screen.right -. 110.) (screen.bottom +. 40.) ] )

let view (computer : computer) (s : model) : camera * shape3d list =
  let over p lines =
    let cam, shapes = view_play computer p in
    (cam, shapes @ List.map hud lines)
  in
  let press y = Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y y ] in
  match s.scene with
  | Title ->
      let cam = { (Camera3d.orbit ~distance:36. ~height:18. ~look:0. (spin 14. computer.time) (5., 0., -17.)) with far = 2400. } in
      ( cam,
        backdrop (-38.) cam
        @ [ stage_shapes.(2); Physics3d.draw new_ball ]
        @ List.map (banana computer.time) stages.(2).bananas
        @ List.map hud
            ([ text yellow 7. "TINY MONKEY BALL" |> move_y 330.;
               text white 2.5 "arrows: tilt the stage, and roll through the goal" |> move_y 260.;
               text white 2.5 "no rail, sixty seconds, three monkeys" |> move_y 225. ]
            @ press 170.) )
  | Playing p -> over p (if s.frames < ready then [ text white 6. "READY" |> move_y 150. ] else if s.frames < ready + 45 then [ text yellow 7. "GO!" |> move_y 150. ] else [])
  | Ended (Goal, p) -> over p [ text yellow 8. "GOAL!" |> move_y 150. ]
  | Ended (Fall_out, p) -> over p [ text red 7. "FALL OUT" |> move_y 150. ]
  | Ended (Time_over, p) -> over p [ text red 7. "TIME OVER" |> move_y 150. ]
  | Game_over p -> over p ([ text red 7. "GAME OVER" |> move_y 200. ] @ press 130.)
  | All_clear p ->
      over p ([ text yellow 6. "ALL STAGES CLEAR" |> move_y 220.; text white 3. (Printf.sprintf "%d bananas" p.bananas) |> move_y 150. ] @ press 90.)

let app = game3d view update initial_model

(* flat shading: each tile and facet of the ball its own shade; both
 * sides of a face drawn, for the sky, seen from below (Camera3d.sky) *)
let main = Program.main __MODULE__ (fun () ->
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false }
    ~flags:(Playground_platform.flags ()) app)
