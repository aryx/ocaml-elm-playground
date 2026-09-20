(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy pinball table, the old-school kind: one screen seen from
 * above, a plunger, two flippers, three pop bumpers, two slingshots,
 * two banks of drop targets, and a drain that takes your ball. Hold
 * space to pull the plunger and let go to launch, left and right
 * arrows for the flippers, up to nudge the table (too much and it
 * tilts).
 *
 * The flipper is from 1947 (Gottlieb's Humpty Dumpty, six of them,
 * facing outwards); the video pinball we are copying is the Amiga and
 * DOS generation -- Pinball Dreams and Pinball Fantasies (Digital
 * Illusions, 1992-93), and Space Cadet (Cinematronics/Maxis, 1995),
 * the one that came with Windows. Before them Bill Budge's Pinball
 * Construction Set (1983) had already made the point this file makes:
 * a table is *data* -- walls, bumpers, targets -- and the game is a
 * ball falling through it. (Names and dates from memory, to check.)
 *
 * The one mechanic is the flipper: it does not bat the ball, it
 * *carries* it. What a flipper adds is the speed of its own surface at
 * the point of contact -- a lever turning at w radians per second
 * gives the point r away from its pivot a speed w r, so the tip throws
 * the ball much harder than the base does, which is why aiming with a
 * flipper means choosing where along it to catch the ball:
 *
 *      pivot o=============*=======>  the tip moves fastest
 *             \      slow  |  fast
 *              \           v
 *               the ball leaves with (bounce) + (the surface's speed)
 *
 * The table is a list of segments, a list of circles and two flippers
 * (see "The table" below); everything else -- what bounces, what
 * scores, what kicks -- is a field on those, so the table can be
 * changed without touching a line of physics.
 *
 * Two engines, chosen like Asteroid's and TinyMario's, with the flag
 * physics=engine (?physics=engine in a browser; see Playground.flags):
 *
 *  - by default, ours, 30 lines: the closest point of a segment to the
 *    ball, and if it is nearer than the radius, the ball is pushed out
 *    and its velocity reflected about the normal, keeping [bounce] of
 *    the speed it came in with (Newton's law of impact, the same
 *    formula as physics/2d/Resolve.mli's, for a wall of infinite mass);
 *  - with physics=engine, the playground's Physics layer: the walls,
 *    the bumpers and the flippers as [immovable] bodies, the ball as a
 *    circle body, and Physics.bounce_off resolving each touch --
 *    Collide (the contact point and normal), Contact and Resolve (the
 *    impulse, with restitution *and* friction, and the flipper's
 *    surface speed taken from Body.point_velocity, which ours has to
 *    add by hand).
 *
 * Both integrate the ball themselves, in [substeps] (4) per frame,
 * rather than with Physics.step, and that is the pinball's lesson.
 * Physics.tick is a fixed 1/60 s; the plunger launches at up to 2300
 * pixels a second and a flipper throws at about 3000, i.e. 50 pixels
 * between two frames, four times the ball's radius, so it can be on
 * one side of a wall at one frame and the other side at the next,
 * having overlapped nothing:
 *
 *      frame n      flipper      frame n+1
 *         o   ------ ===== ------>   o        nothing ever overlapped
 *
 * Run it with substeps=1 to watch exactly that: the ball is through
 * the table and gone within a second of the launch. With 4, it moves
 * 12 pixels a step, just under its radius, and the table holds it.
 * The real answers are smaller steps, ours, or a swept test --
 * Physics.went_through, which TinySoldat's bullets use, and which
 * physics/2d offers as a question, not as a response (see
 * plan_physics_remaining.md).
 *
 * The subtler half of the same lesson is that a *wall* can be the fast
 * one: a flipper turning at 1400 degrees a second moves its tip 45
 * pixels in a frame, three ball radii, so a flipper swung once a frame
 * sweeps straight past a ball resting on it and throws nothing at all
 * (it did, until the flippers were moved into the substep loop too).
 *
 * What it uses: no kit (there is one pinball); Scene2d for the title
 * and game over, Audio, and Physics for the second engine. Not
 * Physics.world: a world solves many contacts at once, for piles that
 * rest on each other, and a pinball table has exactly one moving
 * thing. Not Random: a table is deterministic, which is why a good
 * player can repeat a shot.
 *
 * Exercises: a ball saver and multiball (the model holds one ball; a
 * list of them changes four lines, and the flippers then have to know
 * which one they hit); ramps and a habitrail (a lane the ball follows
 * with no physics at all, like the real ones); a skill shot on the
 * plunger's charge; a bonus multiplier held between balls; the tilt's
 * plumb bob as a real pendulum (physics/2d's Springs); the table read
 * from strings, the way Tilemap games read their maps, which is what
 * Pinball Construction Set really was.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The table *)
(*****************************************************************************)

type point = number * number

(* A wall: a segment the ball bounces off. [bounce] is how much of its
 * speed it keeps (the restitution), [kick] a speed added along the
 * normal whatever the ball's own (a slingshot's rubber, which throws a
 * ball that barely touched it), [score] what a touch is worth, and
 * [target] the drop target this wall is, if it is one. *)
type wall = { a : point; b : point; bounce : number; kick : number; score : int; target : int option }

(* A pop bumper: the mushroom that kicks and scores. *)
type bumper = { c : point; r : number }

let wall ?(bounce = 0.42) ?(kick = 0.) ?(score = 0) ?target (a : point) (b : point) : wall = { a; b; bounce; kick; score; target }

(* a chain of walls through the points, e.g. the dome over the table *)
let chain ?bounce ?kick (pts : point list) : wall list =
  let rec go = function p :: (q :: _ as rest) -> wall ?bounce ?kick p q :: go rest | _ -> [] in
  go pts

(* an arc of [n] segments of the circle of center (cx, cy) and radius
 * [r], from [a0] to [a1] degrees *)
let arc ((cx, cy) : point) (r : number) (a0 : number) (a1 : number) (n : int) : point list =
  List.init (n +.. 1) (fun i ->
      let t = degrees_to_radians (a0 + ((a1 - a0) * float_of_int i / float_of_int n)) in
      (cx + (r * cos t), cy + (r * sin t)))

(* The cabinet: the playfield between the left wall and the lane the
 * plunger shoots up, the dome over both, and the outlanes down to the
 * flippers. (0, 0) is the middle of the screen, y up.
 *
 *      +--------------------+-+   320    the dome, and the lane
 *      |   ()  ()  ()       | |          the bumpers
 *      | []             []  | |          the drop targets
 *      |  \             /   | |   -140   the slingshots
 *      |   \___     ___/    |_|
 *      |    ==\   /==       |     -330   the flippers
 *      +-------\ /----------+     -450   the drain
 *)
let left_x = -240.
let right_x = 240. (* the playfield's right wall; the plunger lane is behind it *)
let lane_x = 300.
let drain_y = -450.
let pivot_y = -330.
let pivot_x = 120.

let table_walls : wall list =
  (* the sides and the dome over both the playfield and the lane: a
   * ball shot up the lane rides under the dome and is turned back into
   * the table, which is what the lane is for *)
  chain [ (left_x, drain_y); (left_x, 170.) ]
  @ chain (arc (30., 170.) 270. 180. 0. 18)
  @ chain [ (lane_x, 170.); (lane_x, drain_y) ]
  (* the lane's inner wall, stopping short of the dome so the ball can
   * leave at the top *)
  @ chain [ (right_x, -150.); (right_x, 140.) ]
  (* the slingshots: the rubber above each flipper, the only walls that
   * kick, and below them the walls along the drain *)
  @ chain ~bounce:0.6 ~kick:520. [ (left_x, -150.); (-175., -290.) ]
  @ chain [ (-175., -290.); (-175., -395.) ]
  @ chain ~bounce:0.6 ~kick:520. [ (right_x, -150.); (175., -290.) ]
  @ chain [ (175., -290.); (175., -395.) ]
  (* the two banks of three drop targets, facing the middle *)
  @ List.concat
      (List.mapi
         (fun i y -> [ wall ~bounce:0.5 ~score:500 ~target:i (-190., y) (-190., y + 44.); wall ~bounce:0.5 ~score:500 ~target:(i +.. 3) (190., y) (190., y + 44.) ])
         [ -20.; 40.; 100. ])

let bumpers : bumper list = [ { c = (-120., 150.); r = 38. }; { c = (0., 250.); r = 38. }; { c = (120., 150.); r = 38. } ]
let bumper_score = 100
let bumper_kick = 620.
let bumper_bounce = 0.55

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type ball = { x : number; y : number; vx : number; vy : number }

(* A flipper turns between [rest] and [up] degrees; [angle] is where it
 * is now and [spin] how fast it is turning, degrees per second, which
 * is what it throws the ball with. *)
type flipper = { pivot : point; rest : number; up : number; angle : number; spin : number }

type play =
  | Plunger of number (* how far it is pulled, 0 to 1 *)
  | Live
  | Drained of int (* frames since *)

type engine = Ours | Engine

type game = {
  ball : ball;
  play : play;
  left : flipper;
  right : flipper;
  down : int list; (* the drop targets that are down *)
  score : int;
  balls : int; (* left to play, this one included *)
  nudges : (int * number) list; (* when, and which way: the tilt watches them *)
  tilted : bool;
  bumped : (point * int) list; (* a bumper lit, and frames since *)
  frames : int;
  engine : engine;
  substeps : int;
}

type scene = Title | Playing of game | Game_over of int
type model = { scenes : scene Scene2d.t; hi_score : int }

let ball_r = 13.
let gravity = 1500. (* pixels per second, per second: the table's tilt *)
let flipper_len = 112.
let flipper_speed = 1400. (* degrees per second *)

let new_flipper (left : bool) : flipper =
  if left then { pivot = (-.pivot_x, pivot_y); rest = -28.; up = 32.; angle = -28.; spin = 0. }
  else { pivot = (pivot_x, pivot_y); rest = 208.; up = 148.; angle = 208.; spin = 0. }

(* on the plunger, at the bottom of the lane *)
let ready_ball : ball = { x = (right_x + lane_x) / 2.; y = -400.; vx = 0.; vy = 0. }

let new_game (engine : engine) (substeps : int) : game =
  { ball = ready_ball; play = Plunger 0.; left = new_flipper true; right = new_flipper false; down = []; score = 0; balls = 3; nudges = [];
    tilted = false; bumped = []; frames = 0; engine; substeps }

let initial_model = { scenes = Scene2d.start Title; hi_score = 0 }

(* the two flags: which engine bounces the ball, and how many steps a
 * frame is cut into (see the header) *)
let engine_of (flags : flags) : engine = match List.assoc_opt "physics" flags with Some "engine" -> Engine | _ -> Ours
let substeps_of (flags : flags) : int = match List.assoc_opt "substeps" flags with Some s -> (try max 1 (int_of_string s) with _ -> 4) | None -> 4

(*****************************************************************************)
(* The geometry both engines need *)
(*****************************************************************************)

(* the point of the segment [a,b] nearest to [p]: t along it, clamped
 * to its ends, so a segment behaves like a rod with round caps *)
let closest ((ax, ay) : point) ((bx, by) : point) ((px, py) : point) : point =
  let dx = bx - ax and dy = by - ay in
  let l2 = (dx * dx) + (dy * dy) in
  let t = if l2 = 0. then 0. else clamp 0. 1. ((((px - ax) * dx) + ((py - ay) * dy)) / l2) in
  (ax + (t * dx), ay + (t * dy))

let flipper_tip (f : flipper) : point =
  let px, py = f.pivot in
  let t = degrees_to_radians f.angle in
  (px + (flipper_len * cos t), py + (flipper_len * sin t))

(* the speed of the flipper's surface at [p], what it carries the ball
 * away with: a lever turning at w radians per second moves the point r
 * from its pivot at w x r, i.e. w times r, across r *)
let surface_velocity (f : flipper) ((px, py) : point) : number * number =
  let ox, oy = f.pivot in
  let w = degrees_to_radians f.spin in
  (0. - (w * (py - oy)), w * (px - ox))

(*****************************************************************************)
(* What the ball touched *)
(*****************************************************************************)

type hit = Hit_wall of wall | Hit_bumper of point | Hit_flipper


(* the walls the ball can touch now: the table's, minus the drop
 * targets that are already down *)
let live_walls (g : game) : wall list =
  List.filter (fun (w : wall) -> match w.target with None -> true | Some i -> not (List.mem i g.down)) table_walls

(* a flipper as a segment, from its pivot to its tip, with the ball's
 * radius around it: the fat rod the ball rolls along *)
let flipper_wall (f : flipper) : wall = wall ~bounce:0.3 f.pivot (flipper_tip f)

(*****************************************************************************)
(* Engine 1: ours *)
(*****************************************************************************)

(* the ball pushed out of what it overlaps, and its velocity reflected
 * about the normal: it keeps [bounce] of the speed it came in with,
 * plus [kick] (the rubber's own), plus the speed of the surface it hit
 * (a flipper's; a wall does not move). The same impulse as
 * physics/2d/Resolve.mli's, with 1/m = 0 for the wall. *)
let reflect (b : ball) ((nx, ny) : number * number) (depth : number) (bounce : number) (kick : number) ((sx, sy) : number * number) : ball =
  let b = { b with x = b.x + (nx * depth); y = b.y + (ny * depth) } in
  let vn = ((b.vx - sx) * nx) + ((b.vy - sy) * ny) in
  if vn > 0. then b (* already on its way out: bouncing again would suck it back *)
  else
    let j = (1. + bounce) * (0. - vn) in
    { b with vx = b.vx + ((j + kick) * nx); vy = b.vy + ((j + kick) * ny) }

(* the ball against one segment: the nearest point of it, and if that
 * is nearer than the radius, the normal (from the wall to the ball)
 * and how deep the overlap is *)
let against_segment (b : ball) (a : point) (c : point) : (number * number * number) option =
  let cx, cy = closest a c (b.x, b.y) in
  let dx = b.x - cx and dy = b.y - cy in
  let d = Float.hypot dx dy in
  if d >= ball_r then None else if d < 1e-6 then Some (0., 1., ball_r) else Some (dx / d, dy / d, ball_r - d)

let against_circle (b : ball) ((cx, cy) : point) (r : number) : (number * number * number) option =
  let dx = b.x - cx and dy = b.y - cy in
  let d = Float.hypot dx dy in
  if d >= r + ball_r then None else if d < 1e-6 then Some (0., 1., r) else Some (dx / d, dy / d, r + ball_r - d)

let ours_substep (g : game) (dt : number) (b : ball) : ball * hit list =
  let b = { b with vy = b.vy - (gravity * dt) } in
  let b = { b with x = b.x + (b.vx * dt); y = b.y + (b.vy * dt) } in
  let wall_hit (b, hits) (w : wall) =
    match against_segment b w.a w.b with
    | None -> (b, hits)
    | Some (nx, ny, depth) -> (reflect b (nx, ny) depth w.bounce w.kick (0., 0.), Hit_wall w :: hits)
  in
  let bumper_hit (b, hits) (bu : bumper) =
    match against_circle b bu.c bu.r with
    | None -> (b, hits)
    | Some (nx, ny, depth) -> (reflect b (nx, ny) depth bumper_bounce bumper_kick (0., 0.), Hit_bumper bu.c :: hits)
  in
  let flipper_hit (b, hits) (f : flipper) =
    let w = flipper_wall f in
    match against_segment b w.a w.b with
    | None -> (b, hits)
    | Some (nx, ny, depth) ->
        (* where it touched decides how hard it is thrown: the tip of
         * a flipper moves much faster than its base *)
        let touch = closest w.a w.b (b.x, b.y) in
        (reflect b (nx, ny) depth w.bounce 0. (surface_velocity f touch), Hit_flipper :: hits)
  in
  let acc = List.fold_left wall_hit (b, []) (live_walls g) in
  let acc = List.fold_left bumper_hit acc bumpers in
  let b, hits = List.fold_left flipper_hit acc [ g.left; g.right ] in
  (b, hits)

(*****************************************************************************)
(* Engine 2: the playground's Physics *)
(*****************************************************************************)

(* A wall as a body: a thin box, turned along the segment. The engine
 * sees a 10-pixel-thick box where we see a line, so the ball rests 5
 * pixels further out; the table is drawn from the same segments, so
 * the difference is where the drawing and the physics disagree -- the
 * flag hitboxes shows it. *)
let wall_body (w : wall) : Physics.body =
  let ax, ay = w.a and bx, by = w.b in
  let len = Float.max 1. (Float.hypot (bx - ax) (by - ay)) in
  Physics.body (rectangle black len 10.)
  |> Physics.at ((ax + bx) / 2.) ((ay + by) / 2.)
  |> Physics.pointing (radians_to_degrees (atan2 (by - ay) (bx - ax)))
  |> Physics.immovable |> Physics.bouncy w.bounce |> Physics.rough 0.1

let bumper_body (bu : bumper) : Physics.body =
  Physics.body (circle black bu.r) |> Physics.at (fst bu.c) (snd bu.c) |> Physics.immovable |> Physics.bouncy bumper_bounce

(* the flipper as a body turning at [spin] degrees per second: the
 * engine reads the speed of the *contact point*
 * (physics/2d/Body.point_velocity), so it carries the ball by itself,
 * which is the one thing ours has to do by hand *)
let flipper_body (f : flipper) : Physics.body =
  let px, py = f.pivot in
  let t = degrees_to_radians f.angle in
  Physics.body (rectangle black flipper_len 16.)
  |> Physics.at (px + (flipper_len / 2. * cos t)) (py + (flipper_len / 2. * sin t))
  |> Physics.pointing f.angle |> Physics.turn f.spin |> Physics.immovable |> Physics.bouncy 0.3 |> Physics.rough 0.3

let ball_body (b : ball) : Physics.body =
  Physics.body (circle white ball_r) |> Physics.at b.x b.y |> Physics.moving b.vx b.vy |> Physics.bouncy 0.42 |> Physics.rough 0.1 |> Physics.upright

(* [kick] is the table's rubber, not the engine's: Physics has no such
 * verb, so it goes on along the direction the bounce pushed the ball *)
let add_kick (kick : number) (before : Physics.body) (after : Physics.body) : Physics.body =
  let dvx = after.vx - before.vx and dvy = after.vy - before.vy in
  let d = Float.hypot dvx dvy in
  if kick = 0. || d < 1e-6 then after else { after with vx = after.vx + (kick * dvx / d); vy = after.vy + (kick * dvy / d) }

let engine_substep (g : game) (dt : number) (b : ball) : ball * hit list =
  let b = { b with vy = b.vy - (gravity * dt) } in
  let b = { b with x = b.x + (b.vx * dt); y = b.y + (b.vy * dt) } in
  let touch (body, hits) (hit, kick, other) =
    if not (Physics.touching other body) then (body, hits) else (add_kick kick body (Physics.bounce_off other body), hit :: hits)
  in
  let obstacles =
    List.map (fun (w : wall) -> (Hit_wall w, w.kick, wall_body w)) (live_walls g)
    @ List.map (fun (bu : bumper) -> (Hit_bumper bu.c, bumper_kick, bumper_body bu)) bumpers
    @ List.map (fun (f : flipper) -> (Hit_flipper, 0., flipper_body f)) [ g.left; g.right ]
  in
  let body, hits = List.fold_left touch (ball_body b, []) obstacles in
  ({ x = body.x; y = body.y; vx = body.vx; vy = body.vy }, hits)

(*****************************************************************************)
(* The ball, one frame *)
(*****************************************************************************)

(* a flipper turns towards where the player is holding it, at a fixed
 * speed; [spin], how fast it is turning, is what carries the ball *)
let step_flipper (dt : number) (held : bool) (f : flipper) : flipper =
  let wanted = if held then f.up else f.rest in
  let most = flipper_speed * dt in
  let move = clamp (0. - most) most (wanted - f.angle) in
  { f with angle = f.angle + move; spin = move / dt }

let flap (dt : number) (left_held : bool) (right_held : bool) (g : game) : game =
  { g with left = step_flipper dt left_held g.left; right = step_flipper dt right_held g.right }

(* One frame: [substeps] of it (see the header), the ball *and the
 * flippers* advancing by each. A flipper is a wall that moves, and a
 * fast wall tunnels through the ball exactly as a fast ball tunnels
 * through a wall: at 1400 degrees a second its tip travels 45 pixels
 * between two frames, three times the ball's radius, so a flipper
 * moved once a frame sweeps past a ball resting on it and throws
 * nothing. *)
let step_ball (g : game) (left_held : bool) (right_held : bool) : game * hit list =
  let dt = Physics.tick / float_of_int g.substeps in
  let substep = match g.engine with Ours -> ours_substep | Engine -> engine_substep in
  let rec go g hits n =
    if n = 0 then (g, hits)
    else
      let g = flap dt left_held right_held g in
      let ball, h = substep g dt g.ball in
      go { g with ball } (h @ hits) (n -.. 1)
  in
  go g [] g.substeps

(*****************************************************************************)
(* The score *)
(*****************************************************************************)

let all_targets = 6

let score_hits (g : game) (hits : hit list) : game =
  List.fold_left
    (fun g hit ->
      match hit with
      | Hit_flipper -> g
      | Hit_bumper c ->
          Audio.play Audio.blip;
          { g with score = g.score +.. bumper_score; bumped = (c, 0) :: List.filter (fun (c', _) -> c' <> c) g.bumped }
      | Hit_wall w -> (
          let g = { g with score = g.score +.. w.score } in
          match w.target with
          | None ->
              if w.kick > 0. then Audio.play Audio.hit;
              g
          | Some i when List.mem i g.down -> g
          | Some i ->
              Audio.play Audio.coin;
              let down = i :: g.down in
              (* the whole bank down: the bonus, and they all come back
               * up, as a real table's do *)
              if List.length down < all_targets then { g with down }
              else begin
                Audio.play Audio.explosion;
                { g with down = []; score = g.score +.. 5000 }
              end))
    g hits

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* The plunger: space pulls it back (a second to the end), letting go
 * launches the ball up the lane, as hard as it was pulled. *)
let step_plunger (computer : computer) (g : game) (charge : number) : game =
  if computer.keyboard.kspace then
    let charge = Float.min 1. (charge + (Physics.tick / 0.8)) in
    { g with play = Plunger charge; ball = { ready_ball with y = ready_ball.y - (26. * charge) } }
  else if charge = 0. then g
  else begin
    Audio.play Audio.laser;
    { g with play = Live; ball = { ready_ball with vy = 900. + (1400. * charge) } }
  end

(* Nudging the table pushes the ball, and the table notices: three
 * nudges within three seconds and it tilts, which on a real machine
 * cuts the power to the flippers until the ball is gone. *)
let nudge (g : game) : game =
  let nudges = (g.frames, 1.) :: g.nudges in
  let recent = List.filter (fun (f, _) -> g.frames -.. f < 180) nudges in
  Audio.play Audio.hit;
  if List.length recent >= 3 then { g with nudges; tilted = true }
  else { g with nudges; ball = { g.ball with vy = g.ball.vy + 190.; vx = g.ball.vx + 60. } }

let next_ball (g : game) : game =
  if g.balls <= 1 then { g with balls = 0 } else { g with balls = g.balls -.. 1; ball = ready_ball; play = Plunger 0.; tilted = false; nudges = [] }

let update_game (computer : computer) (scenes : scene Scene2d.t) (g : game) : game =
  let g = { g with frames = g.frames +.. 1; bumped = List.filter_map (fun (c, n) -> if n < 18 then Some (c, n +.. 1) else None) g.bumped } in
  (* the flippers are dead while the table is tilted, which is the
   * point of tilting *)
  let held k = (not g.tilted) && k in
  let left_held = held computer.keyboard.kleft and right_held = held computer.keyboard.kright in
  let g = if Scene2d.pressed (fun k -> k.kup) scenes && g.play = Live then nudge g else g in
  match g.play with
  (* with no ball in play there is nothing to collide with, so the
   * flippers can be flapped a frame at a time (the player does) *)
  | Plunger charge -> step_plunger computer (flap Physics.tick left_held right_held g) charge
  | Drained n ->
      let g = flap Physics.tick left_held right_held g in
      if n > 90 then next_ball g else { g with play = Drained (n +.. 1) }
  | Live ->
      let g, hits = step_ball g left_held right_held in
      let ball = g.ball in
      let g = if g.tilted then g else score_hits g hits in
      if List.exists (fun h -> h = Hit_flipper) hits then Audio.play Audio.step;
      (* a plunge too soft to get the ball over the dome: it dribbles
       * back down the lane, and the player plunges again rather than
       * losing it *)
      if ball.x > right_x && ball.y < -390. then { g with play = Plunger 0.; ball = ready_ball }
      else if ball.y < drain_y then begin
        Audio.play Audio.explosion;
        { g with play = Drained 0 }
      end
      else g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  let fire = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Title ->
      if fire then { model with scenes = Scene2d.go (Playing (new_game (engine_of computer.flags) (substeps_of computer.flags))) scenes }
      else { model with scenes }
  | Playing g ->
      let g = update_game computer scenes g in
      let hi_score = max model.hi_score g.score in
      if g.balls = 0 then { hi_score; scenes = Scene2d.go (Game_over g.score) scenes } else { hi_score; scenes = { scenes with scene = Playing g } }
  | Game_over _ -> if fire || scenes.elapsed > 10. then { model with scenes = Scene2d.go Title scenes } else { model with scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let steel = rgb 190 200 215
let neon = rgb 90 210 255
let rubber = rgb 255 90 120

(* a wall as the bar it is: its length, turned along it *)
let view_wall (color : color) (thickness : number) (w : wall) : shape =
  let ax, ay = w.a and bx, by = w.b in
  rectangle color (Float.max 4. (Float.hypot (bx - ax) (by - ay))) thickness
  |> rotate (radians_to_degrees (atan2 (by - ay) (bx - ax)))
  |> move ((ax + bx) / 2.) ((ay + by) / 2.)

let view_bumper (g : game) (bu : bumper) : shape list =
  let lit = List.exists (fun (c, _) -> c = bu.c) g.bumped in
  [ circle (if lit then white else rgb 40 60 110) (bu.r + 6.) |> move (fst bu.c) (snd bu.c);
    circle (if lit then yellow else neon) bu.r |> move (fst bu.c) (snd bu.c);
    circle (rgb 20 30 60) (bu.r / 2.) |> move (fst bu.c) (snd bu.c) ]

let view_flipper (f : flipper) : shape list =
  let px, py = f.pivot in
  let t = degrees_to_radians f.angle in
  [ rectangle steel flipper_len 16. |> rotate f.angle |> move (px + (flipper_len / 2. * cos t)) (py + (flipper_len / 2. * sin t));
    circle steel 11. |> move px py; circle (rgb 90 100 120) 5. |> move px py ]

let view_ball (b : ball) : shape list =
  [ circle (rgb 230 235 245) ball_r |> move b.x b.y; circle white 5. |> move (b.x - 4.) (b.y + 4.) ]

let view_table (g : game) : shape list =
  (* the playfield, then the walls on it *)
  [ rectangle (rgb 12 16 34) (lane_x - left_x) 900. |> move ((left_x + lane_x) / 2.) (-10.);
    circle (rgb 12 16 34) 270. |> move 30. 170. ]
  @ List.map
      (fun (w : wall) ->
        let color = if w.kick > 0. then rubber else if w.target <> None then rgb 255 180 60 else steel in
        view_wall color (if w.target <> None then 12. else 8.) w)
      (live_walls g)
  @ List.concat_map (view_bumper g) bumpers
  @ List.concat_map view_flipper [ g.left; g.right ]
  (* the plunger, pulled down as it is charged *)
  @ (match g.play with
    | Plunger charge ->
        [ rectangle (rgb 200 60 60) 20. 60. |> move ready_ball.x (-440. - (26. * charge)); rectangle steel 8. 90. |> move ready_ball.x (-470.) ]
    | _ -> [])
  @ (match g.play with Drained _ -> [] | _ -> view_ball g.ball)

let view_hud (model : model) (g : game) : shape list =
  [ text neon 2.5 "SCORE" |> move (-420.) 420.; text white 3. (Printf.sprintf "%d" g.score) |> move (-420.) 380.;
    text neon 2.5 "HIGH" |> move 420. 420.; text white 3. (Printf.sprintf "%d" (max model.hi_score g.score)) |> move 420. 380.;
    text neon 2.2 (Printf.sprintf "BALL %d" (4 -.. g.balls)) |> move (-420.) (-380.);
    text (rgb 120 130 150) 1.8 (match g.engine with Ours -> "physics: ours" | Engine -> "physics=engine") |> move (-420.) (-430.) ]
  @ (if g.tilted then [ text (rgb 255 80 80) 5. "TILT" |> move_y (-120.) ] else [])
  @ (match g.play with Drained n when n > 20 -> [ text (rgb 255 180 60) 3. "BALL LOST" |> move_y 60. ] | _ -> [])
  @ if g.frames < 150 then [ text (rgb 120 130 150) 1.8 "space: plunger   left/right: flippers   up: nudge" |> move_y (-480.) ] else []

let view_title (scenes : scene Scene2d.t) (model : model) : shape list =
  [ text neon 7. "TINY PINBALL" |> move_y 300.; text white 2.2 "a table is data: walls, bumpers, targets, and a ball falling through it" |> move_y 230. ]
  @ List.concat
      (List.mapi
         (fun i ((key : string), (what : string)) ->
           let y = 100. - (float_of_int i * 55.) in
           [ text (rgb 120 220 255) 2.4 key |> move (-260.) y; text white 2.4 what |> move 90. y ])
         [ ("space", "pull the plunger, let go to launch"); ("left/right", "the flippers"); ("up", "nudge the table (three times: TILT)");
           ("scores", "bumpers 100   drop targets 500   the bank 5000") ])
  @ [ text (rgb 255 180 60) 2.
        "flags: physics=engine (the Physics layer bounces it)   substeps=1 (watch it fall through)"
      |> move_y (-180.);
      text white 2.2 (Printf.sprintf "HIGH %d" model.hi_score) |> move_y (-250.) ]
  @ Scene2d.blink 1. scenes [ text yellow 3. "PRESS SPACE" |> move_y (-350.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and scenes = model.scenes in
  rectangle (rgb 6 8 18) screen.width screen.height
  ::
  (match scenes.scene with
  | Title -> view_title scenes model
  | Playing g -> view_table g @ view_hud model g
  | Game_over score ->
      [ text (rgb 255 80 80) 6. "GAME OVER" |> move_y 60.; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-40.) ]
      @ Scene2d.blink 1. scenes [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ])

(* the keys and flags, printed at launch, to remember them (on the web,
 * in the browser's console) *)
let help =
  {|TinyPinball
  keys:  space       pull the plunger, let go to launch (start, restart)
         left/right  the flippers
         up          nudge the table (three times in three seconds: TILT)
  flags: physics=engine  the playground's Physics layer bounces the ball
         substeps=n      steps per frame (4); with 1 the ball tunnels
  e.g.   dune exec games/TinyPinball.exe -- physics=engine
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
