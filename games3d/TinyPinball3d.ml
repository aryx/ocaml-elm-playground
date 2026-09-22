(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* games/TinyPinball.ml's table again, in 3D and on the physics engine:
 * the same plunger, flippers, bumpers, slingshots and drop targets, the
 * same keys (hold space to pull the plunger, left and right for the
 * flippers, up to nudge), and "c" to switch off what this game is for.
 *
 * The 2D table was already at a real pinball's scale -- a pixel is a
 * millimetre, the ball 26 mm across, the table 1.5 m/s^2 of tilt -- so
 * here it is in metres, and it is plain what the engine is up against:
 * the plunger launches at 2.3 m/s and the flippers throw at 3, which is
 * 4 to 5 cm between two frames, against walls 1 cm thick and a ball
 * 1.3 cm in radius. Tested on a plain step, where the ball *is*, it
 * goes through the walls from 1.5 m/s (physics/tests/
 * Unit_sweep3d.ml): the table would not hold it for a second. The 2D
 * game cuts each frame into four substeps; four substeps hold a 3D
 * ball up to 6 m/s.
 *
 * This one sweeps instead: Physics3d.simulate ~continuous
 * (physics/3d/Sweep3d.mli, plan_physics3d_teaching.md phase 10). The
 * ball's path through the step is tested, not its position, by
 * conservative advancement -- advance by the gap divided by the
 * fastest the gap can close, again and again, never past anything --
 * and at the first touch it is bounced off there and then, and goes on
 * for the rest of the step. "c" turns it off, and the corner of the
 * screen counts the walls the ball went through (Physics3d.
 * went_through); on, it counts the touches the sweep caught.
 *
 * The subtler half is the flippers. A flipper here is a *kinematic*
 * body: immovable, so the ball cannot push it, and turned by the game
 * at 1400 degrees a second, which it tells the engine as a spin; the
 * engine turns it, and the ball it meets gets its surface's speed at
 * the contact. But at that speed its tip moves 3 cm a frame, more than
 * the ball's radius: a flipper is a wall that tunnels through the ball.
 * The sweep is the ball's, and still catches it, because it measures
 * the gap against the flipper *moving and turning* -- the closing
 * speed bounded by its turning speed times its reach. Switch the sweep
 * off, and a flipper swung at a ball resting on it goes straight past.
 *
 * What it uses: Physics3d (a world simulated with ~continuous, bodies,
 * went_through), Scene2d, Audio. Not Physics3d's sleeping (one ball,
 * always moving), nor its solver's piles; what it takes from the solver
 * is the ball rolling on the table, and the glass over it.
 *
 * Exercises: multiball (the sweep is per ball; two balls meeting are
 * the solver's business); a ramp, which a 2D table cannot have and a 3D
 * one wants -- a slab tilted up, and the ball rolling up it; the
 * flippers as hinged bodies with a motor, once the engine has joints
 * (phase 11), rather than driven by the game.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The table: games/TinyPinball.ml's, in millimetres *)
(*****************************************************************************)

(* coupling: from here to [ready], games/TinyPinball.ml's table and
 * numbers, as they are there: in its pixels, which are millimetres.
 * [to3] turns them into the engine's metres. *)

type point = number * number

type wall = { a : point; b : point; bounce : number; kick : number; score : int; target : int option }
type bumper = { c : point; r : number }

let wall ?(bounce = 0.42) ?(kick = 0.) ?(score = 0) ?target (a : point) (b : point) : wall = { a; b; bounce; kick; score; target }

let chain ?bounce ?kick (pts : point list) : wall list =
  let rec go = function p :: (q :: _ as rest) -> wall ?bounce ?kick p q :: go rest | _ -> [] in
  go pts

let arc ((cx, cy) : point) (r : number) (a0 : number) (a1 : number) (n : int) : point list =
  List.init (n + 1) (fun i ->
      let t = (a0 +. ((a1 -. a0) *. float_of_int i /. float_of_int n)) *. Float.pi /. 180. in
      (cx +. (r *. cos t), cy +. (r *. sin t)))

let left_x = -240.
let right_x = 240.
let lane_x = 300.
let drain_y = -450.
let pivot_y = -330.
let pivot_x = 120.

let table_walls : wall list =
  chain [ (left_x, drain_y); (left_x, 170.) ]
  @ chain (arc (30., 170.) 270. 180. 0. 18)
  @ chain [ (lane_x, 170.); (lane_x, drain_y) ]
  @ chain [ (right_x, -150.); (right_x, 140.) ]
  @ chain ~bounce:0.6 ~kick:520. [ (left_x, -150.); (-175., -290.) ]
  @ chain [ (-175., -290.); (-175., -395.) ]
  @ chain ~bounce:0.6 ~kick:520. [ (right_x, -150.); (175., -290.) ]
  @ chain [ (175., -290.); (175., -395.) ]
  @ List.concat
      (List.mapi
         (fun i y ->
           [ wall ~bounce:0.5 ~score:500 ~target:i (-190., y) (-190., y +. 44.);
             wall ~bounce:0.5 ~score:500 ~target:(i + 3) (190., y) (190., y +. 44.) ])
         [ -20.; 40.; 100. ])

let bumpers : bumper list = [ { c = (-120., 150.); r = 38. }; { c = (0., 250.); r = 38. }; { c = (120., 150.); r = 38. } ]
let bumper_score = 100
let bumper_kick = 620.
let bumper_bounce = 0.55
let ball_r = 13.
let gravity = 1500.
let flipper_len = 112.
let flipper_speed = 1400.

type flipper = { pivot : point; rest : number; up : number; angle : number; spin : number }

let new_flipper (left : bool) : flipper =
  if left then { pivot = (-.pivot_x, pivot_y); rest = -28.; up = 32.; angle = -28.; spin = 0. }
  else { pivot = (pivot_x, pivot_y); rest = 208.; up = 148.; angle = 208.; spin = 0. }

let ready : point = ((right_x +. lane_x) /. 2., -400.)

(* The table's (x, y), y up the table, in millimetres, as the engine's
 * (x, z) in metres: the table lies flat, x across it and -z up it, the
 * drain towards the player at +z. It is the pull down the table
 * ([tilt]) that makes it a slope, not its geometry. *)
let m (px : number) : number = px /. 1000.
let to3 ((x, y) : point) : number * number = (m x, -.m y)

(*****************************************************************************)
(* The table as bodies *)
(*****************************************************************************)

let tilt = m gravity (* 1.5 m/s^2 down the table *)
let radius = m ball_r
let wall_height = 0.03

(* A wall: a box 1 cm thick and 3 cm high along the segment, standing
 * on the table. Turned about y by the segment's own angle, since the
 * table's y is the engine's -z. *)
let wall_body (color : color) (w : wall) : Physics3d.body =
  let (ax, az) = to3 w.a and (bx, bz) = to3 w.b in
  let len = Float.hypot (bx -. ax) (bz -. az) +. 0.01 in
  let angle = atan2 (-.(bz -. az)) (bx -. ax) *. 180. /. Float.pi in
  Physics3d.body (box color len wall_height 0.01)
  |> Physics3d.pointing (0., 1., 0.) angle
  |> Physics3d.at ((ax +. bx) /. 2.) (wall_height /. 2.) ((az +. bz) /. 2.)
  |> Physics3d.immovable |> Physics3d.bouncy w.bounce |> Physics3d.rough 0.1

let bumper_body (bu : bumper) : Physics3d.body =
  let x, z = to3 bu.c in
  Physics3d.body (sphere red (m bu.r)) |> Physics3d.ball |> Physics3d.at x 0.015 z |> Physics3d.immovable
  |> Physics3d.bouncy bumper_bounce

(* the table, and a glass over it, as on a real machine: a ball kicked
 * hard enough jumps, and the glass is what keeps it on the table *)
let floor_body = Physics3d.body (box (rgb 20 30 80) 0.7 0.02 1.) |> Physics3d.at 0.03 (-0.01) 0. |> Physics3d.immovable |> Physics3d.rough 0.1
let glass_body = Physics3d.body (box white 0.7 0.02 1.) |> Physics3d.at 0.03 0.05 0. |> Physics3d.immovable

(* A flipper at [angle], turning at [spin]: the game places it, and
 * tells the engine how it moves over the next step -- turning about its
 * own middle at [spin], and its middle going round the pivot, so the
 * two together are a turn about the pivot. Immovable: the ball cannot
 * push it back. *)
let flipper_body (f : flipper) : Physics3d.body =
  let px, pz = to3 f.pivot in
  let t = f.angle *. Float.pi /. 180. in
  let rx = m flipper_len /. 2. *. cos t and rz = -.(m flipper_len /. 2. *. sin t) in
  let w = f.spin *. Float.pi /. 180. in
  Physics3d.body (box white (m flipper_len) 0.02 0.016)
  |> Physics3d.pointing (0., 1., 0.) f.angle
  |> Physics3d.at (px +. rx) 0.01 (pz +. rz)
  |> Physics3d.immovable
  |> Physics3d.turning (0., 1., 0.) f.spin
  |> Physics3d.moving (w *. rz) 0. (-.w *. rx)
  |> Physics3d.bouncy 0.3 |> Physics3d.rough 0.3

let ball_body ((x, z) : number * number) : Physics3d.body =
  Physics3d.body (sphere (rgb 200 200 210) radius) |> Physics3d.ball |> Physics3d.heavy 0.08 |> Physics3d.at x radius z
  |> Physics3d.bouncy 0.42 |> Physics3d.rough 0.1

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = Plunger of number | Live | Drained of int

(* what the ball can touch, in the order of the world's bodies after the
 * ball: the two flippers, the table, the glass, the walls up, the
 * bumpers *)
type thing = Left | Right | Floor | Glass | Wall of wall | Bumper of point

type game = {
  world : Physics3d.world;
  things : thing list;
  play : play;
  left : flipper;
  right : flipper;
  down : int list;
  near : thing list; (* what the ball was touching, last tick: a hit is a new one *)
  score : int;
  balls : int;
  nudges : int list;
  tilted : bool;
  bumped : (point * int) list;
  frames : int;
  continuous : bool;
  through : int; (* walls gone through, with the sweep off *)
  caught : int; (* touches the sweep caught, with it on *)
}

type scene = Title | Playing of game | Game_over of int
type model = { scenes : scene Scene2d.t; hi_score : int }

let live_walls (down : int list) : wall list =
  List.filter (fun (w : wall) -> match w.target with None -> true | Some i -> not (List.mem i down)) table_walls

let wall_color (w : wall) : color =
  if w.target <> None then rgb 250 170 40 else if w.kick > 0. then rgb 240 70 70 else rgb 150 160 190

(* the world, from the ball and the table as it is: rebuilt when a
 * target drops, which is rare *)
let build (ball : Physics3d.body) (left : flipper) (right : flipper) (down : int list) : Physics3d.world * thing list =
  let walls = live_walls down in
  let bodies =
    [ ball; flipper_body left; flipper_body right; floor_body; glass_body ]
    @ List.map (fun w -> wall_body (wall_color w) w) walls
    @ List.map bumper_body bumpers
  in
  (Physics3d.world bodies, [ Left; Right; Floor; Glass ] @ List.map (fun w -> Wall w) walls @ List.map (fun b -> Bumper b.c) bumpers)

let new_game () : game =
  let left = new_flipper true and right = new_flipper false in
  let world, things = build (ball_body (to3 ready)) left right [] in
  { world; things; play = Plunger 0.; left; right; down = []; near = []; score = 0; balls = 3; nudges = []; tilted = false;
    bumped = []; frames = 0; continuous = true; through = 0; caught = 0 }

let initial_model = { scenes = Scene2d.start Title; hi_score = 0 }

let ball (g : game) : Physics3d.body = List.hd g.world.bodies
let with_ball (g : game) (b : Physics3d.body) : game = { g with world = { g.world with bodies = b :: List.tl g.world.bodies } }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* a flipper turns towards where the player holds it, at a fixed speed
 * (games/TinyPinball.ml's) *)
let step_flipper (held : bool) (f : flipper) : flipper =
  let wanted = if held then f.up else f.rest in
  let most = flipper_speed *. Physics3d.tick in
  let move = Float.max (-.most) (Float.min most (wanted -. f.angle)) in
  { f with angle = f.angle +. move; spin = move /. Physics3d.tick }

(* the flippers where the game says, and moving as it says, for the
 * next step: the world's second and third bodies replaced *)
let flap (left_held : bool) (right_held : bool) (g : game) : game =
  let left = step_flipper left_held g.left and right = step_flipper right_held g.right in
  let bodies =
    match g.world.bodies with
    | b :: _ :: _ :: rest -> b :: flipper_body { g.left with spin = left.spin } :: flipper_body { g.right with spin = right.spin } :: rest
    | bodies -> bodies
  in
  { g with left; right; world = { g.world with bodies } }

(* within this of something, the ball is touching it, for scoring: the
 * sweep bounces it off at the touch, so by the end of the step it has
 * often left again *)
let reach = 0.004

let touches (b : Physics3d.body) (o : Physics3d.body) : Contact3d.t option =
  Collide3d.contact (Physics3d.hitbox_of o) (Hitbox3d.place (b.x, b.y, b.z) (Hitbox3d.Sphere (radius +. reach)))

(* the ball pushed away from what it hit, by a rubber's own kick *)
let kick (speed : number) (k : Contact3d.t) (b : Physics3d.body) : Physics3d.body =
  let nx, _, nz = k.normal in
  Physics3d.moving (b.vx +. (speed *. nx)) b.vy (b.vz +. (speed *. nz)) b

let score_hits (g : game) : game =
  let b = ball g in
  let others = List.combine g.things (List.tl g.world.bodies) in
  let now = List.filter_map (fun (t, o) -> match t with Floor | Glass -> None | _ -> Option.map (fun k -> (t, k)) (touches b o)) others in
  let fresh = List.filter (fun (t, _) -> not (List.mem t g.near)) now in
  let g = { g with near = List.map fst now } in
  if g.tilted then g
  else
    List.fold_left
      (fun g (t, k) ->
        match t with
        | Bumper c ->
            Audio.play Audio.blip;
            let g = with_ball g (kick (m bumper_kick) k (ball g)) in
            { g with score = g.score + bumper_score; bumped = (c, 0) :: List.filter (fun (c', _) -> c' <> c) g.bumped }
        | Wall w -> (
            let g = if w.kick > 0. then (Audio.play Audio.hit; with_ball g (kick (m w.kick) k (ball g))) else g in
            let g = { g with score = g.score + w.score } in
            match w.target with
            | Some i when not (List.mem i g.down) ->
                Audio.play Audio.coin;
                let down = i :: g.down in
                let down, bonus = if List.length down < 6 then (down, 0) else (Audio.play Audio.explosion; ([], 5000)) in
                let world, things = build (ball g) g.left g.right down in
                { g with down; world; things; score = g.score + bonus }
            | _ -> g)
        | Left | Right | Floor | Glass -> g)
      g fresh

(* Did the ball go through [o] this tick, from [before] to [after]: the
 * straight path between them meets it, and the ball ends on its other
 * side (a wall is a box, its thin side its own z). Not
 * Physics3d.went_through, which works out where the ball was from where
 * it is going now -- right for a bullet, wrong for a ball that has just
 * bounced, whose velocity has turned round: every bounce counted as a
 * tunnel in a first version of this counter. *)
let crossed (before : Physics3d.body) (after : Physics3d.body) (o : Physics3d.body) : bool =
  let n = Quat.rotate o.orientation (0., 0., 1.) in
  let side (b : Physics3d.body) = Vec3.dot (Vec3.sub (b.x, b.y, b.z) (o.x, o.y, o.z)) n in
  let motion = (after.x -. before.x, after.y -. before.y, after.z -. before.z) in
  side before *. side after < 0.
  && Sweep3d.sphere ~radius ~from:(before.x, before.y, before.z) ~motion (Physics3d.hitbox_of o) <> None

(* one tick of the ball: the table's tilt, the world stepped (swept, or
 * not), and the walls it went through if it was not *)
let roll (g : game) : game =
  let g = with_ball g (Physics3d.push 0. 0. (tilt *. (ball g).mass) (ball g)) in
  let before = ball g in
  let world = Physics3d.simulate ~gravity:9.8 ~sleeping:false ~continuous:g.continuous g.world in
  let b = List.hd world.bodies in
  let walls = List.filteri (fun i _ -> match List.nth g.things i with Wall _ | Left | Right -> true | _ -> false) (List.tl world.bodies) in
  let through = List.length (List.filter (crossed before b) walls) in
  score_hits { g with world; through = g.through + through; caught = g.caught + world.swept }

let step_plunger (computer : computer) (g : game) (charge : number) : game =
  let x, z = to3 ready in
  if computer.keyboard.kspace then
    let charge = Float.min 1. (charge +. (Physics3d.tick /. 0.8)) in
    with_ball { g with play = Plunger charge } (ball_body (x, z +. (0.026 *. charge)))
  else if charge = 0. then g
  else begin
    Audio.play Audio.laser;
    with_ball { g with play = Live } (ball_body (x, z) |> Physics3d.moving 0. 0. (-.(0.9 +. (1.4 *. charge))))
  end

(* three nudges within three seconds: tilted, the flippers dead *)
let nudge (g : game) : game =
  let nudges = g.frames :: List.filter (fun f -> g.frames - f < 180) g.nudges in
  Audio.play Audio.hit;
  if List.length nudges >= 3 then { g with nudges; tilted = true }
  else let b = ball g in with_ball { g with nudges } (Physics3d.moving (b.vx +. 0.06) b.vy (b.vz -. 0.19) b)

let next_ball (g : game) : game =
  if g.balls <= 1 then { g with balls = 0 }
  else with_ball { g with balls = g.balls - 1; play = Plunger 0.; tilted = false; nudges = []; near = [] } (ball_body (to3 ready))

let update_game (computer : computer) (scenes : scene Scene2d.t) (g : game) : game =
  let g = { g with frames = g.frames + 1; bumped = List.filter_map (fun (c, n) -> if n < 18 then Some (c, n + 1) else None) g.bumped } in
  let g = if Scene2d.pressed (fun k -> Set_.mem "c" k.keys) scenes then { g with continuous = not g.continuous } else g in
  let held k = (not g.tilted) && k in
  let g = flap (held computer.keyboard.kleft) (held computer.keyboard.kright) g in
  let g = if Scene2d.pressed (fun k -> k.kup) scenes && g.play = Live then nudge g else g in
  match g.play with
  | Plunger charge -> step_plunger computer g charge
  | Drained n -> if n > 90 then next_ball g else { g with play = Drained (n + 1) }
  | Live ->
      let g = roll g in
      let b = ball g in
      let lane = m right_x and drain = -.m drain_y in
      if b.x > lane && b.z > 0.39 then with_ball { g with play = Plunger 0. } (ball_body (to3 ready))
      else if b.z > drain || b.y < -0.5 then begin
        Audio.play Audio.explosion;
        { g with play = Drained 0 }
      end
      else g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  let fire = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Title -> if fire then { model with scenes = Scene2d.go (Playing (new_game ())) scenes } else { model with scenes }
  | Playing g ->
      let g = update_game computer scenes g in
      let hi_score = max model.hi_score g.score in
      if g.balls = 0 then { hi_score; scenes = Scene2d.go (Game_over g.score) scenes }
      else { hi_score; scenes = { scenes with scene = Playing g } }
  | Game_over _ -> if fire || scenes.elapsed > 10. then { model with scenes = Scene2d.go Title scenes } else { model with scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

(* the table from the player's end, looking up it *)
let cam : camera = camera ~eye:(0.03, 0.62, 0.78) ~target:(0.03, 0., -0.08) ~fov:52. ()

let table_shapes (g : game) : shape3d list =
  let bodies = List.tl g.world.bodies in
  List.concat
    (List.map2
       (fun t (o : Physics3d.body) ->
         match t with
         | Glass -> []
         | Bumper c ->
             let lit = List.mem_assoc c g.bumped in
             [ sphere (if lit then rgb 255 240 120 else rgb 220 60 60) (m 38.) |> move3d o.x 0.015 o.z ]
         | _ -> [ Physics3d.draw o ])
       g.things bodies)

let hud_shapes (screen : screen) (g : game) : shape list =
  let sweep =
    if g.continuous then Printf.sprintf "sweep on (c): %d touches caught" g.caught
    else Printf.sprintf "sweep OFF (c): %d walls gone through" g.through
  in
  [ text white 2.4 (Printf.sprintf "SCORE %d" g.score) |> move (screen.left +. 120.) (screen.top -. 35.);
    text white 2.4 (Printf.sprintf "BALLS %d" g.balls) |> move (screen.right -. 110.) (screen.top -. 35.);
    text (if g.continuous then rgb 140 230 140 else rgb 255 120 120) 2. sweep |> move_y (screen.bottom +. 30.) ]
  @ (if g.tilted then [ text red 6. "TILT" ] else [])
  @ match g.play with Drained _ -> [ text yellow 5. "DRAINED" |> move_y 100. ] | _ -> []

let background (screen : screen) = rectangle (rgb 10 10 30) screen.width screen.height

(* the room: a dark wall behind the table and a floor under it *)
let room : shape3d list =
  [ box (rgb 12 12 28) 40. 40. 0.1 |> move3d 0. 0. (-6.); box (rgb 25 20 30) 40. 0.1 40. |> move3d 0. (-0.9) 0. ]

let view (computer : computer) (model : model) : camera * shape3d list =
  let screen = computer.screen in
  match model.scenes.scene with
  | Title ->
      let g = new_game () in
      ( cam,
        room @ table_shapes g @ [ Physics3d.draw (ball g) ]
        @ List.map hud
            ([ text white 6. "TINY PINBALL 3D" |> move_y 330.;
               text white 2.2 "space: plunger   left/right: flippers   up: nudge   c: the sweep" |> move_y 270. ]
            @ Scene2d.blink 1. model.scenes [ text yellow 3.5 "PRESS SPACE" |> move_y 220. ]) )
  | Playing g -> (cam, (room @ table_shapes g @ [ Physics3d.draw (ball g) ]) @ List.map hud (hud_shapes screen g))
  | Game_over score ->
      ( cam,
        List.map hud
          [ background screen; text white 6. "GAME OVER";
            text white 3. (Printf.sprintf "SCORE %d   BEST %d" score model.hi_score) |> move_y (-70.) ] )

let app = game3d view update initial_model

let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
