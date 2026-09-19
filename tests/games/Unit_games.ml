(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See dune: each test plays a game without drawing it, and checks its
 * model. These were first throwaway simulations, written to find the
 * bugs the comments mention; kept, they keep them fixed. *)

open Playground

let t = Testo.create

(* the computer at frame [i] (1/60 s each), with a keyboard *)
let computer ?(keyboard = initial_computer.keyboard) (i : int) : computer =
  { initial_computer with keyboard; time = Time (float_of_int i /. 60.); screen = to_screen 1000. 1000. }

let press (key : string) (k : keyboard) : keyboard = { k with keys = Set_.add key k.keys }

(*****************************************************************************)
(* TinySokoban *)
(*****************************************************************************)

(* the first level's shortest solution, found by a breadth-first search:
 * uldurrd *)
let sokoban_solution () =
  let open TinySokoban in
  let p = load 0 in
  let dirs = [ (0, -1); (-1, 0); (0, 1); (0, -1); (1, 0); (1, 0); (0, 1) ] in
  let b = List.fold_left (fun b d -> match step b d with Some b -> b | None -> Alcotest.fail "a step blocked") p.board dirs in
  Alcotest.(check bool) "solved" true (solved b);
  Alcotest.(check (pair int int)) "moves, pushes" (7, 2) (b.moves, b.pushes)

(*****************************************************************************)
(* TinyPacman *)
(*****************************************************************************)

(* the ghosts leave their house, one after the other: Pinky is out,
 * hunting, 5 seconds into the game (READY! 2 s, 1 s waiting, leaving) *)
let pacman_ghosts_leave () =
  let open TinyPacman in
  let g = ref (new_game ()) in
  for i = 1 to 300 do g := update_game (computer i) !g done;
  let pinky = List.find (fun gh -> gh.name = Pinky) !g.ghosts in
  Alcotest.(check bool) "Pinky hunting" true (pinky.state = Hunting)

(* a power pellet turns the hunting ghosts blue, and back the other way *)
let pacman_blue () =
  let open TinyPacman in
  let g = new_game () in
  let g =
    { g with pause = None; pac = { (mover_at (3, 1)) with dir = Left; wanted = Left };
      ghosts = List.map (fun gh -> if gh.name = Blinky then { gh with m = { (mover_at (7, 1)) with dir = Left } } else gh) g.ghosts }
  in
  let g = ref g in
  for i = 1 to 20 do g := update_game (computer ~keyboard:{ initial_computer.keyboard with kleft = true } i) !g done;
  let blinky = List.find (fun gh -> gh.name = Blinky) !g.ghosts in
  Alcotest.(check bool) "blue" true blinky.blue;
  Alcotest.(check bool) "blue for a while" true (!g.blue_frames > 300);
  Alcotest.(check int) "the pellet's 50, and the dots on the way" 70 !g.score

(*****************************************************************************)
(* TinyBomberman *)
(*****************************************************************************)

(* a bomb's fire reaching another bomb sets it off at once (chain
 * reaction), and stops at the first block, burning it *)
let bomberman_chain () =
  let open TinyBomberman in
  let g = { (new_game ()) with range = 2; bombs = [ { col = 1; row = 1; timer = 0 }; { col = 3; row = 1; timer = 999 } ] } in
  let g' = explode g in
  Alcotest.(check int) "both exploded" 0 (List.length g'.bombs);
  Alcotest.(check (option char)) "the block after the second bomb burned" (Some ' ') (Tilemap.get g'.map 4 1);
  Alcotest.(check (option char)) "the one after it still there" (Some '+') (Tilemap.get g'.map 5 1);
  let g'' = explode { g with range = 1; bombs = [ { col = 8; row = 7; timer = 0 } ] } in
  Alcotest.(check (option char)) "the exit revealed" (Some 'e') (Tilemap.get g''.map 9 7)

(*****************************************************************************)
(* TinyMicroMachines *)
(*****************************************************************************)

(* the computer drives two laps in 50 seconds, on the road, never
 * falling off the table (it used to aim across the table on the long
 * straights, see computer_drive) *)
let micro_machines_computer () =
  let open TinyMicroMachines in
  let c = ref (car_at 0 0.) and falls = ref 0 and offroad = ref 0 in
  for _ = 1 to 3000 do
    let gas, steer = computer_drive !c in
    c := drive gas steer !c |> recover;
    if !c.falling = 60 then incr falls;
    if not (on_road !c.x !c.y) then incr offroad
  done;
  Alcotest.(check bool) "two laps" true (!c.next > 2 * List.length waypoints);
  Alcotest.(check int) "falls" 0 !falls;
  Alcotest.(check bool) "hardly off the road" true (!offroad < 60)

(*****************************************************************************)
(* TinyMario64 *)
(*****************************************************************************)

(* running to the first platform and jumping onto it: landed, at its
 * height, 2 *)
let mario64_jump () =
  let open TinyMario64 in
  let s = ref initial_model in
  for i = 1 to 180 do
    let keyboard =
      { initial_computer.keyboard with kspace = i = 1 || (i >= 145 && i <= 165); kleft = i >= 2 && i <= 63; kup = i >= 64 && i <= 180 }
    in
    s := update (computer ~keyboard i) !s
  done;
  match !s.scene with
  | Playing l ->
      Alcotest.(check (float 1e-9)) "on the platform's top" 2. l.mario.y;
      Alcotest.(check bool) "on the ground" true l.mario.on_ground
  | _ -> Alcotest.fail "not playing"

(*****************************************************************************)
(* TinyMarble *)
(*****************************************************************************)

(* the first ramp, "vvvv" twice between the 9s and the 7s: its edges at
 * 9, 8 and 7; halfway down its first row, 8.5 *)
let marble_ramp () =
  let open TinyMarble in
  let x = 2.5 *. cell in
  let at r = Option.get (ground x (r *. cell)) in
  Alcotest.(check (float 1e-9)) "top" 9. (at 4.);
  Alcotest.(check (float 1e-9)) "middle" 8. (at 5.);
  Alcotest.(check (float 1e-9)) "bottom" 7. (at 6.);
  Alcotest.(check (float 1e-9)) "halfway down the first row" 8.5 (at 4.5);
  let _, sz = slope x (4.5 *. cell) in
  Alcotest.(check (float 1e-9)) "going down south, 1 per tile of 2" (-0.5) sz

(* rolling south until landing: from how high the ball fell *)
let marble_fall_from (c, r) : number =
  let open TinyMarble in
  let rec go b i =
    if i > 600 then Alcotest.fail "never landed"
    else match step (0., 1.) b with _, Some h -> h | b, None -> go b (i + 1)
  in
  go (ball_at (c, r)) 0

(* the shortcut down the cliff, from the 7s to the 2s: 5 high, broken;
 * off the end of the bridge, from the 2s to the 1s: 1 high, fine *)
let marble_falls () =
  let open TinyMarble in
  let cliff = marble_fall_from (2, 7) and step = marble_fall_from (4, 19) in
  Alcotest.(check (float 0.1)) "the cliff" 5. cliff;
  Alcotest.(check bool) "breaks" true (cliff > max_fall);
  Alcotest.(check (float 0.1)) "the step" 1. step;
  Alcotest.(check bool) "doesn't break" true (step <= max_fall)

(* collide's worked example: the steelie (2) at 0.1 hits the marble (1)
 * at rest: the marble goes off at 0.133, the steelie on at 0.033 *)
let marble_steelie () =
  let open TinyMarble in
  let me = ball_at (5, 13) in
  let steelie = { me with x = me.x -. 0.9; vx = 0.1 } in
  let me, steelie = collide me 1. steelie 2. in
  Alcotest.(check (float 1e-9)) "the marble" (0.4 /. 3.) me.vx;
  Alcotest.(check (float 1e-9)) "the steelie" (0.1 /. 3.) steelie.vx

(* left alone at the top of the first ramp, the marble rolls down it by
 * itself, onto the 7s, faster than a push on the flat would take it in
 * the same time *)
let marble_rolls_down () =
  let open TinyMarble in
  let b = { (ball_at (2, 3)) with z = 4. *. cell +. 0.01 } in
  let rec go b i = if i = 0 then b else go (fst (step (0., 0.) b)) (i - 1) in
  let b = go b 60 in
  Alcotest.(check (float 1e-9)) "on the 7s" 7. b.y;
  Alcotest.(check bool) "on the ground" true b.on_ground;
  Alcotest.(check bool) "rolling on" true (b.vz > 0.1)

(* a robot on the trackball (the mouse) drives from waypoint to waypoint
 * (tiles, as (column, row)), steering towards the next one and braking
 * its own speed: it reaches the goal in about 10 seconds, never broken,
 * never fallen: the course can be won (a weaker braking, 12 times the
 * speed, overshot the first plateau, and the one after the lane) *)
let marble_robot () =
  let open TinyMarble in
  let waypoints = [ (2.5, 7.); (7., 7.5); (12., 7.5); (12., 13.); (4.5, 13.); (4.5, 19.5); (5., 21.); (8., 22.8) ] in
  let s = ref initial_model and todo = ref waypoints and broken = ref 0 and fallen = ref 0 and i = ref 0 in
  while !i < 60 * 45 && (match !s.scene with Finished _ | Time_up _ -> false | _ -> true) do
    incr i;
    let mouse =
      match (!s.scene, !todo) with
      | Racing r, (c, row) :: rest ->
          let tx = c *. cell and tz = row *. cell in
          if Float.hypot (tx -. r.me.x) (tz -. r.me.z) < 0.8 *. cell then todo := (if rest = [] then !todo else rest);
          let wx = (tx -. r.me.x) -. (25. *. r.me.vx) and wz = (tz -. r.me.z) -. (25. *. r.me.vz) in
          let n = Float.max 1e-6 (Float.hypot wx wz) in
          let wx = wx /. n and wz = wz /. n in
          let right = (wx -. wz) /. sqrt 2. and up = -.(wx +. wz) /. sqrt 2. in
          { initial_computer.mouse with mdx = 8. *. right; mdy = 8. *. up }
      | _ -> initial_computer.mouse
    in
    let keyboard = { initial_computer.keyboard with kspace = !i = 1 } in
    s := update { (computer ~keyboard !i) with mouse } !s;
    match !s.scene with Racing { fate = Broken 0; _ } -> incr broken | Racing r when r.me.y < 0. -> incr fallen | _ -> ()
  done;
  Alcotest.(check int) "never broken" 0 !broken;
  Alcotest.(check int) "never fallen" 0 !fallen;
  match !s.scene with
  | Finished r -> Alcotest.(check bool) "with 20 seconds to spare" true (r.time_left > 20 * 60)
  | _ -> Alcotest.fail (Printf.sprintf "not at the goal: waypoints left %d" (List.length !todo))

(*****************************************************************************)
(* TinyXpilot *)
(*****************************************************************************)

(* intercept's worked example: a ship 300 to the right going up at 160,
 * a shot at 200: they meet after 2.5 s, 500 away (3-4-5) *)
let xpilot_intercept () =
  let t = Option.get (TinyXpilot.intercept 300. 0. 0. 160. 200.) in
  Alcotest.(check (float 1e-9)) "t" 2.5 t;
  Alcotest.(check (option (float 1e-9))) "outrun" None (TinyXpilot.intercept 300. 0. 250. 0. 200.)

(* rope_pull's worked example: the ball 120 right of the ship, both
 * still: 600 to the right; slack at 100, nothing *)
let xpilot_rope () =
  let open TinyXpilot in
  let ship = Physics.body ship_shape and ball = (new_ball solo Red).ball in
  let fx, fy = rope_pull ship (ball |> Physics.at 120. 0.) in
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "stretched by 10" (600., 0.) (fx, fy);
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "slack" (0., 0.) (rope_pull ship (ball |> Physics.at 100. 0.))

(* hit_walls' worked example: landing on the floor at 60 pixels per
 * second, a jolt of 84; at 250, 350: above crash *)
let xpilot_crash () =
  let open TinyXpilot in
  let x, y = List.assoc Blue solo.bases in
  (* pointing up, its bottom 2 pixels into the floor *)
  let ship vy = (new_ship solo Blue).body |> Physics.at x (y -. 10.) |> Physics.moving 0. vy in
  let _, soft = hit_walls solo (ship (-60.)) and _, hard = hit_walls solo (ship (-250.)) in
  Alcotest.(check (float 1.)) "landing" 84. soft;
  Alcotest.(check bool) "fine" true (soft < crash);
  Alcotest.(check (float 1.)) "ramming" 350. hard;
  Alcotest.(check bool) "crashed" true (hard > crash)

(* two players: the red ball set on the blue treasure box is a point for
 * blue, and the ball goes back home *)
let xpilot_duel_score () =
  let open TinyXpilot in
  let g = new_game 2 in
  let x, y = List.assoc Blue g.lv.treasures in
  let balls = List.map (fun b -> if b.owner = Red then { b with ball = b.ball |> Physics.at x (y +. h +. 10.) } else b) g.balls in
  let g = update_game (computer 1) (Scene2d.start (Playing g)) { g with balls } in
  Alcotest.(check (list int)) "scores" [ 1; 0 ] (List.map (fun (p : pilot) -> p.score) g.pilots);
  let red = List.find (fun b -> b.owner = Red) g.balls in
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "home" (List.assoc Red g.lv.homes) (red.ball.x, red.ball.y)

(* red's shot on blue: blue explodes, unless its shield is up (s) *)
let xpilot_duel_shot () =
  let open TinyXpilot in
  let g = new_game 2 in
  let blue = (List.hd g.pilots).ship.body in
  let shot = { shot = Physics.body (circle red 3.) |> Physics.at blue.x blue.y; ttl = 90 } in
  let g = { g with pilots = List.map (fun (p : pilot) -> if p.team = Red then { p with shots = [ shot ] } else p) g.pilots } in
  let after keyboard = List.hd (update_game (computer ~keyboard 1) (Scene2d.start (Playing g)) g).pilots in
  Alcotest.(check bool) "exploded" true ((after initial_computer.keyboard).ship.dead <> None);
  Alcotest.(check bool) "shielded" true ((after { initial_computer.keyboard with ks = true }).ship.dead = None)

(* A robot pilot, through the keyboard: it wants to go to a tile's
 * center, at up to 150 pixels per second; the acceleration it needs
 * (towards the wanted velocity, plus gravity's) says where to point the
 * ship, and it thrusts when pointing about there; shield up when a
 * cannon's shot comes near *)
let xpilot_robot (g : TinyXpilot.game) ((col, row) : int * int) : keyboard =
  let open TinyXpilot in
  let p = List.hd g.pilots in
  let s = p.ship.body in
  let tx, ty = Tilemap.center g.lv.map col row in
  let dx = tx -. s.x and dy = ty -. s.y in
  let d = Float.hypot dx dy in
  let v = Float.min 150. (1.2 *. d) in
  let wx = if d > 1. then v *. dx /. d else 0. and wy = if d > 1. then v *. dy /. d else 0. in
  let ax = 3. *. (wx -. s.vx) and ay = (3. *. (wy -. s.vy)) +. gravity in
  let want = atan2 ay ax *. 180. /. Float.pi in
  let err = Float.rem (want -. s.angle +. 540.) 360. -. 180. in
  let danger = List.exists (fun (b : shot) -> Float.hypot (b.shot.x -. s.x) (b.shot.y -. s.y) < 90.) g.bullets in
  { initial_computer.keyboard with
    kleft = err > 4.; kright = err < -4.; kup = Float.abs err < 30. && Float.hypot ax ay > 40.; kdown = danger }

(* the robot flies from the base down to the ball, catches it, and
 * brings it back up onto the treasure box, in 72 seconds: the ball game
 * can be won, the rope holds, the cannons can be survived. It taught
 * the game's numbers: with a thrust of 260 and a tank of 100, hauling
 * the ball (twice the ship's mass) emptied the tank before the lower
 * fuel station; and its route: the rope wraps around a pyramid cut
 * across, and the ball, dragged along the floor, jams on the fuel
 * station (so it flies high over it) *)
let xpilot_ball () =
  let open TinyXpilot in
  let there = [ (5, 3); (12, 4); (34, 4); (37, 5); (37, 11); (20, 11); (6, 12); (5, 16); (4, 17) ] in
  let back = [ (5, 15); (6, 11); (20, 11); (32, 11); (33, 16); (34, 17); (37, 11); (37, 5); (34, 3); (12, 4); (9, 1); (2, 3) ] in
  (* by the lower fuel station, waiting for a full tank *)
  let refuel = (34, 17) in
  let s = ref initial_model and todo = ref there and going_back = ref false and i = ref 0 in
  let deaths = ref 0 and delivered = ref 0 in
  while !i < 60 * 120 && !delivered = 0 do
    incr i;
    let keyboard =
      match !s.scene with
      | Playing g -> (
          let p = List.hd g.pilots in
          if p.deaths > !deaths then (todo := there; going_back := false);
          deaths := p.deaths;
          (match !todo with
          | (c, r) :: rest ->
              let tx, ty = Tilemap.center g.lv.map c r in
              if Float.hypot (tx -. p.ship.body.x) (ty -. p.ship.body.y) < 30. && rest <> [] && ((c, r) <> refuel || p.ship.fuel > 145.) then todo := rest
          | [] -> ());
          if List.exists (fun b -> b.holder = Some Blue) g.balls && not !going_back then (going_back := true; todo := back);
          match !todo with p :: _ -> xpilot_robot g p | [] -> initial_computer.keyboard)
      | _ -> { initial_computer.keyboard with kspace = !i = 1 }
    in
    s := update (computer ~keyboard !i) !s;
    match !s.scene with Won g | Playing g -> delivered := (List.hd g.pilots).score | Title -> ()
  done;
  Alcotest.(check int) "delivered" 1 !delivered;
  Alcotest.(check int) "deaths" 0 !deaths

(*****************************************************************************)
(* TinyTron (the light cycles kit) *)
(*****************************************************************************)

(* the computer outlasts a player going straight on: every round it
 * (two in 5 seconds: the straight line crashes into the wall after
 * about 2 seconds, then a pause) *)
let tron_computer () =
  let s = ref Lightcycles.initial_model in
  for i = 1 to 300 do
    let keyboard = if i = 1 then press "1" initial_computer.keyboard else initial_computer.keyboard in
    s := Lightcycles.update (computer ~keyboard i) !s
  done;
  match !s.scene with
  | Playing g | Winner g ->
      Alcotest.(check int) "blue's points" 0 g.score1;
      Alcotest.(check bool) "the computer's points" true (g.score2 >= 1)
  | Title -> Alcotest.fail "still on the title"

let tests =
  Testo.categorize "games"
    [ t "TinySokoban, level 1 solved" sokoban_solution;
      t "TinyPacman, the ghosts leave the house" pacman_ghosts_leave;
      t "TinyPacman, a power pellet" pacman_blue;
      t "TinyBomberman, a chain reaction" bomberman_chain;
      t "TinyMicroMachines, the computer drives laps" micro_machines_computer;
      t "TinyMario64, a jump onto a platform" mario64_jump;
      t "TinyMarble, the ramp's heights" marble_ramp;
      t "TinyMarble, the cliff breaks the marble, the step doesn't" marble_falls;
      t "TinyMarble, the steelie knocks the marble" marble_steelie;
      t "TinyMarble, rolling down a ramp" marble_rolls_down;
      t "TinyMarble, a robot drives to the goal" marble_robot;
      t "TinyXpilot, cannons aim ahead" xpilot_intercept;
      t "TinyXpilot, the rope pulls when stretched" xpilot_rope;
      t "TinyXpilot, landing vs. crashing" xpilot_crash;
      t "TinyXpilot, a robot brings a ball home" xpilot_ball;
      t "TinyXpilot, two players: a ball home" xpilot_duel_score;
      t "TinyXpilot, two players: a shot, a shield" xpilot_duel_shot;
      t "TinyTron, the computer outlasts a straight line" tron_computer ]
