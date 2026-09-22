(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy Angry Birds (Rovio, 2009, on Box2D; after Crush the Castle,
 * Armor Games, 2009, a Flash game on Box2D too): pull the ball back
 * in the slingshot, let go, and knock down the tower to break the
 * green targets inside, with 3 balls.
 *
 *   mouse: press near the sling, pull back, release
 *   keys:  left/right the angle, up/down the power, space shoots
 *   r:     the level again
 *
 * What it teaches is the whole physics plan at once
 * (docs/claude_notes/plan_physics_teaching.md): the ball's parabola
 * (gravity and the time step, sections 2-4 of notes_2d_physics.md),
 * with the arc drawn before the shot as the engine's own steps, so the
 * dots are exactly where the ball will go; and a tower that must stand
 * still until hit, then tumble -- rotation, friction, and above all
 * stacking (the solver of Physics.world, phases 7 and 8). A target
 * breaks when hit hard: when its velocity jumps by more than 250
 * pixels per second in one step. That jump is the impulse it took
 * divided by its mass (J = m dv, section 10): a hard hit, or a hard
 * fall.
 *
 * The flag solver=off (or the key s) plays it with the engine before
 * stacking (phase 7's bounce_all): the tower doesn't wait for the
 * ball, it slumps by itself -- what the solver is for. The flag
 * hitboxes draws what the physics sees.
 *
 * What it uses: no kit; the Playground, Scene2d (the keys pressed),
 * and the Physics layer: a world ([Physics.world], [simulate]) for the
 * tower, the ball and the floor, and [step] alone for the arc.
 * Underneath, all of physics/2d/ but Force.gravitation and Energy:
 * Body and Integrate, Shape, Collide (manifolds), Contact, Broadphase,
 * Resolve (rotation, friction) and Solver.
 *
 * Left as exercises: more levels (from strings, like TinyCameltry's
 * maze), materials (wood, stone, glass: their masses, frictions, and
 * how hard they must be hit to break), birds with powers, the camera
 * following the ball over a wider level (Camera2d), the score's stars.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The level *)
(*****************************************************************************)

type kind = Wall | Block | Target | Ball

let wall (w : number) (h : number) (x : number) (y : number) : Physics.body =
  Physics.body (rectangle (rgb 90 140 70) w h) |> Physics.at x y |> Physics.immovable |> Physics.rough 0.8

(* the ground's top at -400, a wall far right *)
let walls = [ wall 1000. 100. 0. (-450.); wall 40. 1000. 520. 0. ]

(* wood: its mass its area (a 40 x 40 block: 1) *)
let block (w : number) (h : number) (x : number) (y : number) : Physics.body =
  Physics.body (rectangle (rgb 190 140 80) w h) |> Physics.at x y |> Physics.heavy (w * h / 1600.) |> Physics.rough 0.7

(* two floors of pillars and planks, the ground's top at -400 *)
let blocks : Physics.body list =
  [ block 20. 100. 200. (-350.); block 20. 100. 340. (-350.); block 180. 20. 270. (-290.);
    block 20. 100. 220. (-230.); block 20. 100. 320. (-230.); block 140. 20. 270. (-170.);
    block 40. 40. 270. (-140.); block 20. 60. 430. (-370.) ]

let target_shape : shape =
  group [ circle (rgb 90 200 90) 16.; circle white 4. |> move (-6.) 4.; circle white 4. |> move 6. 4. ]

let target (x : number) (y : number) : Physics.body =
  Physics.body target_shape |> Physics.at x y |> Physics.heavy 0.6 |> Physics.rough 0.5

(* on the ground inside, on the first floor, and on the top *)
let targets = [ target 270. (-384.); target 270. (-264.); target 270. (-104.) ]

let sling = (-350., -300.)
let ball_shape = circle (rgb 200 50 50) 16.

let ball : Physics.body =
  Physics.body ball_shape |> Physics.at (fst sling) (snd sling) |> Physics.heavy 4. |> Physics.rough 0.5 |> Physics.bouncy 0.2

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type aim = { angle : number; (* degrees *) power : number (* 0 to 1 *) }

type play = {
  world : Physics.world;
  (* each body's kind, in the world's order: walls, blocks, targets,
   * balls (the solver knows bodies by their place in the list: a new
   * ball goes at the end, and the others keep their places) *)
  kinds : kind list;
  aim : aim;
  (* the mouse pulling the ball back *)
  dragging : bool;
  balls_left : int;
  (* the targets broken so far, and where recently (for a puff) *)
  broken : int;
  puffs : (number * number * int) list;
  (* frames since the last shot *)
  since_shot : int;
  solver : bool;
}

type scene = Title | Playing of play | Over of bool (* won *)

type model = scene Scene2d.t

let start (solver : bool) : play =
  let bodies = walls @ blocks @ targets in
  {
    world = Physics.world bodies;
    kinds = List.map (fun _ -> Wall) walls @ List.map (fun _ -> Block) blocks @ List.map (fun _ -> Target) targets;
    aim = { angle = 35.; power = 0.7 };
    dragging = false;
    balls_left = 3;
    broken = 0;
    puffs = [];
    since_shot = 0;
    solver;
  }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let gravity = 800.
let max_speed = 1000.
let max_pull = 120.

let launched (a : aim) : Physics.body = ball |> Physics.launched (a.power * max_speed) a.angle

(* the mouse's pull, as an aim: pulling back and down shoots forward
 * and up *)
let aim_of_mouse (m : mouse) : aim =
  let dx = fst sling - m.mx and dy = snd sling - m.my in
  { angle = Float.atan2 dy dx * 180. / Float.pi; power = min 1. (Float.hypot dx dy / max_pull) }

(* without the solver: phase 7's engine, one contact at a time *)
let without_solver (bodies : Physics.body list) : Physics.body list =
  bodies
  |> List.map (fun (b : Physics.body) -> if b.mass = infinity then b else b |> Physics.fall gravity |> Physics.step)
  |> Physics.bounce_all

let simulate (solver : bool) (w : Physics.world) : Physics.world =
  if solver then w |> Physics.simulate ~gravity else { w with bodies = without_solver w.bodies }

(* a target breaks when its velocity jumped by more than 250 pixels per
 * second in one step: J = m dv, a hard hit *)
let hit_hard (before : Physics.body) (after : Physics.body) : bool =
  (* what gravity alone adds in a step doesn't count *)
  Float.hypot (after.vx - before.vx) (after.vy - before.vy + (gravity / 60.)) > 250.

let update_play (computer : computer) (scenes : model) (p : play) : play =
  let keys = computer.keyboard and m = computer.mouse in
  let pressed k = Scene2d.pressed k scenes in
  let aim =
    {
      angle = p.aim.angle + ((if keys.kleft then 1. else 0.) - if keys.kright then 1. else 0.);
      power = max 0.2 (min 1. (p.aim.power + ((if keys.kup then 0.01 else 0.) - if keys.kdown then 0.01 else 0.)));
    }
  in
  let near_sling = Float.hypot (m.mx - fst sling) (m.my - snd sling) < 50. in
  let dragging = m.mdown && (p.dragging || near_sling) in
  let aim = if dragging then aim_of_mouse m else aim in
  let shoot = p.balls_left > 0 && (pressed (fun k -> k.kspace) || (p.dragging && not m.mdown)) in
  let (world, kinds) =
    if shoot then ({ p.world with bodies = p.world.bodies @ [ launched aim ] }, p.kinds @ [ Ball ]) else (p.world, p.kinds)
  in
  let world' = simulate p.solver world in
  (* the targets hit hard break, the balls gone off the screen go *)
  let keep = List.map2 (fun k (before, after) -> match k with
      | Target -> not (hit_hard before after)
      | Ball -> not (Physics.outside computer.screen after)
      | Wall | Block -> true) kinds (List.combine world.bodies world'.bodies)
  in
  let broken_now =
    List.concat (List.map2 (fun k (b, kept) -> if k = Target && not kept then [ (b.Physics.x, b.Physics.y, 0) ] else []) kinds (List.combine world'.bodies keep))
  in
  let filter l = List.filteri (fun i _ -> List.nth keep i) l in
  {
    p with
    world = { world' with bodies = filter world'.bodies };
    kinds = filter kinds;
    aim;
    dragging;
    balls_left = (if shoot then p.balls_left -.. 1 else p.balls_left);
    broken = p.broken +.. List.length broken_now;
    puffs = broken_now @ List.filter_map (fun (x, y, n) -> if n < 30 then Some (x, y, n +.. 1) else None) p.puffs;
    since_shot = (if shoot then 0 else p.since_shot +.. 1);
  }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let solver_flag = List.assoc_opt "solver" computer.flags <> Some "off" in
  let pressed k = Scene2d.pressed k scenes in
  match scenes.scene with
  | Title | Over _ -> if pressed (fun k -> k.kspace) || computer.mouse.mclick then Scene2d.go (Playing (start solver_flag)) scenes else scenes
  | Playing p ->
      if pressed (fun k -> Set_.mem "r" k.keys) then Scene2d.go (Playing (start p.solver)) scenes
      else
        let p = if pressed (fun k -> Set_.mem "s" k.keys) then { p with solver = not p.solver } else p in
        let p = update_play computer scenes p in
        if not (List.mem Target p.kinds) then Scene2d.go (Over true) scenes
        (* out of balls, and the last one had 4 seconds to do its work *)
        else if p.balls_left = 0 && p.since_shot > 240 then Scene2d.go (Over false) scenes
        else { scenes with scene = Playing p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the arc: the ball's next 60 steps, the engine's own, a dot every 4 *)
let arc (a : aim) : shape list =
  let rec go n (b : Physics.body) acc =
    if n = 0 then acc
    else
      let b = b |> Physics.fall gravity |> Physics.step in
      go (n -.. 1) b (if n mod 4 = 0 then (circle white 3. |> move b.x b.y) :: acc else acc)
  in
  go 60 (launched a) []

let view_play (computer : computer) (p : play) : shape list =
  let (sx, sy) = sling in
  let aiming = p.balls_left > 0 in
  (* the ball in the sling, pulled back along the aim *)
  let pulled = p.aim.power * max_pull * 0.5 in
  let a = p.aim.angle * Float.pi / 180. in
  let (bx, by) = (sx - (pulled * cos a), sy - (pulled * sin a)) in
  [ rectangle (rgb 110 70 40) 12. 100. |> move sx (sy - 60.) ]
  @ (if aiming then arc p.aim @ [ ball_shape |> move bx by ] else [])
  @ List.map Physics.draw p.world.bodies
  @ List.map (fun (x, y, n) -> circle white (16. + float_of_int n) |> fade (1. - (float_of_int n / 30.)) |> move x y) p.puffs
  @ (if List.mem_assoc "hitboxes" computer.flags then List.map Physics.debug p.world.bodies else [])
  @ [ text black 2.5 (Printf.sprintf "balls %d   broken %d/3" p.balls_left p.broken) |> move_y 450.;
      text black 2. (if p.solver then "" else "solver off (s): the tower slumps by itself") |> move_y 415. ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 170 210 240) screen.width screen.height
  :: List.map Physics.draw walls
  @
  match model.scene with
  | Title ->
      [ text black 6. "TINY SLINGSHOT" |> move_y 200.;
        text black 2. "mouse: pull back from the sling, release" |> move_y 80.;
        text black 2. "or keys: left/right angle, up/down power, space" |> move_y 40.;
        text black 2. "break the 3 green targets with 3 balls" |> move_y 0. ]
      @ Scene2d.blink 1. model [ text black 3. "PRESS SPACE" |> move_y (-200.) ]
  | Playing p -> view_play computer p
  | Over won ->
      [ text black 5. (if won then "ALL BROKEN!" else "TRY AGAIN") |> move_y 100. ]
      @ Scene2d.blink 1. model [ text black 3. "PRESS SPACE" |> move_y (-200.) ]

let help =
  {|TinySlingshot
  mouse: press near the sling, pull back, release
  keys:  left/right  the angle     up/down  the power
         space       shoot (start, restart)
         r           the level again
         s           the solver off/on (off: the tower slumps by itself)
  flags: solver=off  without the solver
         hitboxes    draw what the physics sees
  e.g.   dune exec games/puzzle/TinySlingshot.exe -- hitboxes
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
