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
      t "TinyTron, the computer outlasts a straight line" tron_computer ]
