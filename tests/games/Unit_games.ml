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
  let b = List.fold_left (fun b d -> match step b d with Some b -> b | None -> Alcotest.fail "a step blocked") p.boards.now dirs in
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
 * straights, see Topdown.computer) *)
let micro_machines_computer () =
  let open TinyMicroMachines in
  let c = ref (car_at 0 0.) and falls = ref 0 and offroad = ref 0 in
  for _ = 1 to 3000 do
    let gas, steer = Topdown.computer track !c.body in
    c := drive gas steer !c |> recover;
    if !c.falling = 60 then incr falls;
    if not (on_road !c.body.x !c.body.y) then incr offroad
  done;
  Alcotest.(check bool) "two laps" true (!c.body.next > 2 * List.length waypoints);
  Alcotest.(check int) "falls" 0 !falls;
  Alcotest.(check bool) "hardly off the road" true (!offroad < 60)

(*****************************************************************************)
(* TinyKart *)
(*****************************************************************************)

(* Mode 7's two ways, a screen pixel to the ground and back: 90 high,
 * with a focal of 866 (a 1000-pixel screen, 60 degrees), the row 90
 * pixels under the horizon sees 866 ahead of the eye (250 behind the
 * kart), where a pixel is a unit *)
let kart_mode7 () =
  let open TinyKart in
  let e = eye (Playground.to_screen 1000. 1000.) 0. 0. 0. in
  let x, y = Option.get (to_ground e 0. (horizon -. 90.)) in
  Alcotest.(check (list (float 0.01))) "866 ahead" [ 866.03 -. 250.; 0. ] [ x; y ];
  let sx, sy, scale = Option.get (TinyKart.to_screen e x y) in
  Alcotest.(check (list (float 1e-6))) "and back" [ 0.; horizon -. 90.; 1. ] [ sx; sy; scale ];
  (* right of the screen is right of the way we look, -y when looking +x *)
  let x, y = Option.get (to_ground e 100. (horizon -. 45.)) in
  Alcotest.(check bool) "to the right" true (y < 0.);
  let sx, sy, _ = Option.get (TinyKart.to_screen e x y) in
  Alcotest.(check (list (float 1e-6))) "and back" [ 100.; horizon -. 45. ] [ sx; sy ];
  Alcotest.(check bool) "the sky" true (to_ground e 0. (horizon +. 1.) = None)

(* the computer drives all four karts (the player's too, as after the
 * finish): the player's kart does its 3 laps in under two minutes,
 * hardly ever on the grass, and so do the others, slower *)
let kart_race () =
  let open TinyKart in
  let r = ref (new_race ()) and frames = ref 0 and grass = ref 0 in
  let player () = (List.hd !r.karts).car in
  while Topdown.lap track (player ()) < laps && !frames < 60 * 120 do
    incr frames;
    r := update_race initial_computer.keyboard true !r;
    if top_speed (player ()).x (player ()).y < 300. then incr grass
  done;
  Alcotest.(check int) "3 laps" laps (Topdown.lap track (player ()));
  Alcotest.(check bool) "hardly on the grass" true (!grass < 60);
  List.iter (fun k -> Alcotest.(check bool) "the others lapping" true (Topdown.lap track k.car >= 2)) !r.karts

(*****************************************************************************)
(* TinyDoom *)
(*****************************************************************************)

(* The node builder's tree: every subsector (a leaf) convex, its segs
 * all of one sector; and the sector the tree finds at a point, the one
 * the level's polygons say, everywhere (every 16 units, except within
 * 1 of a line, where it's either) *)
let doom_bsp () =
  let open TinyDoom in
  let rec leaves t = match t with Leaf (segs, _) -> [ segs ] | Node (_, f, b, _) -> leaves f @ leaves b in
  List.iter
    (fun segs ->
      Alcotest.(check bool) "convex" true (convex segs);
      Alcotest.(check int) "one sector" 1 (List.length (List.sort_uniq compare (List.map (fun s -> s.front) segs))))
    (leaves bsp);
  let near_line x y = Array.exists (fun (l : Sectors.line) -> Sectors.distance l x y < 1.) level.lines in
  for i = 0 to 1152 / 16 do
    for j = 0 to 1024 / 16 do
      let x = float_of_int (i * 16) +. 0.5 and y = float_of_int (j * 16) +. 0.5 in
      let s = Sectors.sector_at level x y in
      if Sectors.inside (sector s) x y && not (near_line x y) then
        Alcotest.(check int) (Printf.sprintf "the sector at (%g, %g)" x y) s (TinyDoom.sector_at bsp x y)
    done
  done

(* a frame at the start: every column drawn to the end (the walls
 * close them all), without all the segs *)
let doom_frame () =
  let open TinyDoom in
  let v = frame (Playground.to_screen 1000. 1000.) initial_model in
  Alcotest.(check int) "every column closed" columns v.closed;
  Alcotest.(check bool) "not every seg" true (List.length v.drawn < snd (count bsp) / 2)

(* A robot walks to the exit, waypoint after waypoint (turning towards
 * the next, walking when facing it): around the pillar, up the stairs,
 * along the corridor, down the stairs, into the dark room; climbing the
 * steps is the kit's (at most 24 at a time), the floor followed *)
let doom_exit () =
  let open TinyDoom in
  let route = [ (300., 400.); (384., 700.); (384., 880.); (700., 896.); (960., 896.); (960., 560.); (960., 400.); (1056., 232.) ] in
  let m = ref initial_model and todo = ref route and i = ref 0 and highest = ref 0. in
  while !m.exited = None && !i < 60 * 60 do
    incr i;
    (match !todo with (x, y) :: rest when Float.hypot (x -. !m.x) (y -. !m.y) < 20. -> todo := rest | _ -> ());
    let k = initial_computer.keyboard in
    let keyboard =
      match !todo with
      | (x, y) :: _ ->
          let wanted = atan2 (y -. !m.y) (x -. !m.x) *. 180. /. Float.pi in
          let d = Float.rem (Float.rem (wanted -. !m.angle +. 180.) 360. +. 360.) 360. -. 180. in
          if d > 4. then { k with kleft = true } else if d < -4. then { k with kright = true } else { k with kup = true }
      | [] -> k
    in
    m := update (computer ~keyboard !i) !m;
    highest := Float.max !highest !m.z
  done;
  Alcotest.(check bool) "exited" true (!m.exited <> None);
  Alcotest.(check (float 0.5)) "upstairs on the way" 64. !highest

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

(* clip's worked example: the square (-1, 1) (1, 1) (1, -1) (-1, -1) cut
 * to x >= 0, its right half; entirely outside, nothing *)
let xpilot_clip () =
  let open TinyXpilot in
  let square = [ (-1., 1.); (1., 1.); (1., -1.); (-1., -1.) ] in
  let pts = Alcotest.(list (pair (float 1e-9) (float 1e-9))) in
  (* the same corners in the same order around, from the first expected *)
  let rec from p l n = match l with q :: rest when q <> p && n > 0 -> from p (rest @ [ q ]) (n - 1) | _ -> l in
  let right = clip { left = 0.; right = 5.; bottom = -5.; top = 5. } square in
  Alcotest.check pts "right half" [ (0., 1.); (1., 1.); (1., -1.); (0., -1.) ] (from (0., 1.) right (List.length right));
  Alcotest.check pts "outside" [] (clip { left = 2.; right = 5.; bottom = -5.; top = 5. } square)

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
(* TinyGalaga *)
(*****************************************************************************)

(* after its five waves, all 40 enemies have flown in and taken their
 * places (or dive, some of them already) *)
let galaga_formation () =
  let open TinyGalaga in
  let g = ref (new_game ()) in
  for i = 1 to 1000 do g := update_game (computer i) (Scene2d.start (Playing !g)) !g done;
  Alcotest.(check int) "all there" 40 (List.length !g.enemies);
  Alcotest.(check bool) "arrived" true (arrived !g)

(* a robot under the nearest enemy in the formation, firing as fast as
 * it can, stepping aside from the bullets and the divers, clears the
 * first stage (in about 40 s, not hit once; chasing the nearest enemy,
 * diving ones included, it was rammed three times) *)
let galaga_robot () =
  let open TinyGalaga in
  let s = ref initial_model and stage2 = ref false and i = ref 0 in
  while !i < 60 * 180 && not !stage2 do
    incr i;
    let keyboard =
      match !s.scenes.scene with
      | Playing g -> (
          if g.stage = 2 then stage2 := true;
          (* a bullet or a diver coming down near: out of its way *)
          let threats =
            List.map (fun (b : Shots.t) -> (b.x, b.y)) g.bullets
            @ List.filter_map (fun e -> match e.flight with Diving _ -> Some (e.x, e.y) | _ -> None) g.enemies
          in
          let close = List.filter (fun (x, y) -> Float.abs (x -. g.fx) < 60. && y < -150.) threats in
          let fire = !i mod 2 = 0 in
          match close with
          | (x, _) :: _ ->
              let left = x > g.fx || g.fx > 400. in
              { initial_computer.keyboard with kleft = left; kright = not left; kspace = fire }
          | [] -> (
              let targets = List.filter (fun e -> flying e && (match e.flight with Diving _ -> false | _ -> true)) g.enemies in
              match List.sort (fun a b -> compare (Float.abs (a.x -. g.fx)) (Float.abs (b.x -. g.fx))) targets with
              | e :: _ -> { initial_computer.keyboard with kleft = e.x < g.fx -. 8.; kright = e.x > g.fx +. 8.; kspace = fire }
              | [] -> { initial_computer.keyboard with kspace = fire }))
      | _ -> { initial_computer.keyboard with kspace = !i mod 2 = 0 }
    in
    s := update (computer ~keyboard !i) !s
  done;
  Alcotest.(check bool) "stage 2" true !stage2

(*****************************************************************************)
(* TinyDonkeyKong *)
(*****************************************************************************)

(* height's worked example: the bottom girder, -440 at its left end,
 * -420 at its right, is at -430 in the middle *)
let kong_height () = Alcotest.(check (float 1e-9)) "middle" (-430.) (TinyDonkeyKong.height TinyDonkeyKong.girders.(0) 0.)

(* a jump in place: up 57.8 pixels (see jump_speed), and back on the
 * girder, walking, 35 frames later; walking off the end of the second girder,
 * a fall of 100 pixels: deadly *)
let kong_jump () =
  let open TinyDonkeyKong in
  let none = initial_computer.keyboard in
  let rec go h n top = match h.state with Walking _ when n > 0 -> (n, top) | _ when n > 100 -> (n, top) | _ -> go (step_hero none false h) (n + 1) (Float.max top h.y) in
  let h = step_hero none true start_hero in
  let frames, top = go h 1 h.y in
  Alcotest.(check int) "frames in the air" 35 frames;
  Alcotest.(check (float 1e-6)) "height" 57.8 (top -. start_hero.y);
  let edge = { start_hero with x = 398.; y = height girders.(1) 398.; state = Walking 1 } in
  let rec fall h n = match h.state with Dying _ | Walking 0 -> h | _ when n > 200 -> h | _ -> fall (step_hero { none with kright = true } false h) (n + 1) in
  Alcotest.(check bool) "dead" true (match (fall edge 0).state with Dying _ -> true | _ -> false)

(* a robot climbs to Pauline: to the next unbroken ladder up, up it,
 * jumping over the barrels rolling at it: the stage can be won, not
 * hit once *)
let kong_robot () =
  let open TinyDonkeyKong in
  let s = ref initial_model and rescued = ref false and deaths = ref 0 and i = ref 0 in
  while !i < 60 * 90 && not !rescued do
    incr i;
    let keyboard =
      match !s.scenes.scene with
      | Playing g -> (
          let h = g.hero and k = initial_computer.keyboard in
          match h.state with
          | Walking n ->
              let l = List.find (fun l -> l.below = n && not l.broken) ladders in
              let coming b = (match b.bstate with Rolling m -> m = n | _ -> false) && Float.abs (b.bx -. h.x) < 60. && (b.bx -. h.x) *. downhill girders.(n) < 0. in
              if List.exists coming g.barrels then { k with kspace = true }
              else if Float.abs (l.lx -. h.x) < 4. then { k with kup = true }
              else { k with kleft = l.lx < h.x; kright = l.lx > h.x }
          | Climbing _ -> { k with kup = true }
          | _ -> k)
      | Rescued _ -> rescued := true; initial_computer.keyboard
      | _ -> { initial_computer.keyboard with kspace = !i = 1 }
    in
    s := update (computer ~keyboard !i) !s;
    match !s.scenes.scene with Playing { hero = { state = Dying 0; _ }; _ } -> incr deaths | _ -> ()
  done;
  Alcotest.(check bool) "rescued" true !rescued;
  Alcotest.(check int) "deaths" 0 !deaths

(*****************************************************************************)
(* TinyLodeRunner *)
(*****************************************************************************)

(* frames of the game, keys held (none after the first frames) *)
let lode_play (g : TinyLodeRunner.game) (n : int) (keys : int -> keyboard) : TinyLodeRunner.game =
  let open TinyLodeRunner in
  let s = ref (Scene2d.start (Playing g)) and g = ref g in
  for i = 1 to n do
    let c = computer ~keyboard:(keys i) i in
    s := Scene2d.update c !s;
    g := update_game c !s !g
  done;
  !g

(* from the start, right to the ladder, up it, and a hole dug on the
 * right: the brick at (6, 11) gone; 5 s later, back *)
let lode_dig () =
  let open TinyLodeRunner in
  let g = { (new_game 3) with guards = [] } in
  let keys i = { initial_computer.keyboard with kright = i <= 40; kup = i > 40 && i <= 80; keys = (if i = 85 then Set_.singleton "x" else Set_.empty) } in
  let g = lode_play g 90 keys in
  Alcotest.(check (option char)) "dug" (Some ' ') (Tilemap.get g.map 6 11);
  let g = lode_play g 300 (fun _ -> initial_computer.keyboard) in
  Alcotest.(check (option char)) "grown back" (Some '#') (Tilemap.get g.map 6 11)

(* a guard over a hole falls in and is stuck (a player would fall
 * through); the brick growing back over the player is deadly *)
let lode_trap () =
  let open TinyLodeRunner in
  let g = new_game 3 in
  let at c r = Tilemap.center g.map c r in
  let guard = runner_at (at 6 10) in
  let g = { g with guards = [ guard ]; map = Tilemap.set g.map 6 11 ' '; holes = [ (6, 11, 200) ]; player = runner_at (at 2 12) } in
  let g = lode_play g 30 (fun _ -> initial_computer.keyboard) in
  let r = List.hd g.guards in
  Alcotest.(check bool) "trapped" true (r.trapped > 0);
  Alcotest.(check (pair int int)) "in the hole" (6, 11) (Tilemap.cell g.map r.x r.y);
  (* a pit: the player falls through holes, so a floor under this one *)
  let g = { g with guards = []; player = runner_at (at 6 11); map = Tilemap.set g.map 6 12 '@' } in
  let g = lode_play g 200 (fun _ -> initial_computer.keyboard) in
  Alcotest.(check bool) "crushed" true (g.dead > 0)

(* the last gold taken: the escape ladder appears *)
let lode_escape () =
  let open TinyLodeRunner in
  let g = new_game 3 in
  let gold = List.filter (fun (c, r) -> not (c = 15 && r = 12)) (Tilemap.find g.map '$') in
  let map = List.fold_left (fun m (c, r) -> Tilemap.set m c r ' ') g.map gold in
  let g = { g with map; guards = []; gold = total_gold - 1; player = runner_at (Tilemap.center map 14 12) } in
  let g = lode_play g 20 (fun _ -> { initial_computer.keyboard with kright = true }) in
  Alcotest.(check int) "all the gold" total_gold g.gold;
  Alcotest.(check (option char)) "the escape ladder" (Some 'H') (Tilemap.get g.map 23 0)

(*****************************************************************************)
(* TinyRick *)
(*****************************************************************************)

(* A robot plays the two rooms, one step after the other, each a
 * condition to reach and the keys to hold meanwhile: away from the
 * boulder, down the hole; the native shot, the spikes jumped, down the
 * ladder; the next native shot; the wall blown up (and away from the
 * blast); the last native shot; the dart jumped; the exit. Not once
 * dead. It found that the spikes killed a jump over them at its start
 * (the whole tile was deadly, now only its bottom, see on_spikes), and
 * taught itself one bullet at a time (6 in all) and to shoot the
 * native that walks back through the blown wall. *)
let rick_robot () =
  let open TinyRick in
  let k = initial_computer.keyboard in
  (* one bullet at a time: 6 in all *)
  let fire i (g : game) = { k with kspace = i mod 2 = 0 && g.bullets = [] } in
  (* the nearest native on Rick's floor, in his room *)
  let native (g : game) =
    List.filter (fun n -> Float.abs (n.ny -. g.rick.y) < 30. && room_of n.nx n.ny = room_of g.rick.x g.rick.y) g.natives
    |> List.sort (fun a b -> compare (Float.abs (a.nx -. g.rick.x)) (Float.abs (b.nx -. g.rick.x)))
    |> function n :: _ -> Some n | [] -> None
  in
  (* facing it, shooting *)
  let shoot i (g : game) =
    match native g with
    | Some n when (n.nx -. g.rick.x) *. g.rick.facing < 0. -> if n.nx < g.rick.x then { k with kleft = true } else { k with kright = true }
    | _ -> fire i g
  in
  let clear (g : game) = native g = None in
  (* (done?, keys) *)
  let steps : ((game -> bool) * (int -> game -> keyboard)) list =
    [ ((fun g -> g.rick.y < 0.), fun _ _ -> { k with kright = true });
      (clear, shoot);
      ((fun g -> g.rick.x < -470.), fun _ g -> { k with kleft = true; kup = g.rick.x < -325. && g.rick.x > -335. });
      ((fun g -> Float.abs (g.rick.x +. 775.) < 3.), fun _ g -> if g.rick.x > -775. then { k with kleft = true } else { k with kright = true });
      ((fun g -> g.rick.y < -270.), fun _ _ -> { k with kdown = true });
      (clear, shoot);
      ((fun g -> g.rick.x > 225.), fun _ _ -> { k with kright = true });
      ((fun g -> g.sticks <> []), fun _ _ -> { k with keys = Set_.singleton "x" });
      ((fun g -> g.sticks = [] && g.blasts = []), fun _ g -> { k with kleft = g.rick.x > 100. });
      (clear, shoot);
      ((fun g -> g.rick.x > 600.), fun _ g -> { k with kright = true; kup = List.exists (fun (d : Shots.t) -> d.x > g.rick.x && d.x -. g.rick.x < 110.) g.darts });
      ((fun _ -> false), fun _ _ -> { k with kright = true }) ]
  in
  let s = ref initial_model and todo = ref steps and i = ref 0 and escaped = ref false and lives = ref 6 in
  while !i < 60 * 90 && not !escaped do
    incr i;
    let keyboard =
      match !s.scene with
      | Playing g -> (
          lives := g.lives;
          (match !todo with (finished, _) :: rest when finished g -> todo := rest | _ -> ());
          match !todo with (_, keys) :: _ -> keys !i g | [] -> k)
      | Escaped _ -> escaped := true; k
      | _ -> { k with kspace = !i = 1 }
    in
    s := update (computer ~keyboard !i) !s
  done;
  Alcotest.(check int) "steps left" 1 (List.length !todo);
  Alcotest.(check bool) "escaped" true !escaped;
  Alcotest.(check int) "lives" 6 !lives

(*****************************************************************************)
(* TinyGradius *)
(*****************************************************************************)

(* the bar: 5 capsules, the cursor on OPTION; taken, an option, and the
 * cursor back to nothing; a second SPEED..., up to 4 *)
let gradius_bar () =
  let open TinyGradius in
  let p = take { no_power with cursor = 4 } in
  Alcotest.(check int) "an option" 1 p.options;
  Alcotest.(check int) "the cursor reset" (-1) p.cursor;
  let p = List.fold_left (fun p _ -> take { p with cursor = 0 }) no_power (List.init 6 Fun.id) in
  Alcotest.(check int) "speed, at most 4" 4 p.speed;
  Alcotest.(check bool) "laser replaces double" false (take { (take { no_power with cursor = 2 }) with cursor = 3 }).double

(* a robot flies the stage: in the middle of the cave a bit ahead,
 * away from the bullets coming near, firing, taking SPEED once and the
 * OPTIONs; at the boss, in line with its core *)
let gradius_robot () =
  let open TinyGradius in
  let s = ref initial_model and i = ref 0 and cleared = ref false and deaths = ref 0 in
  while !i < 60 * 120 && not !cleared do
    incr i;
    let keyboard =
      match !s.scene with
      | Playing g ->
          if g.dead = 1 then incr deaths;
          let col = int_of_float ((g.sx +. 150. -. bounds.left) /. tile) in
          let col = max 0 (min (cols - 1) col) in
          let gap_mid = (ground_top col +. (bounds.top -. (float_of_int (digit ceiling col) *. tile))) /. 2. in
          let target_y = match g.boss with Boss b -> b.by | _ -> gap_mid in
          let danger = List.find_opt (fun (b : Shots.t) -> Float.abs (b.x -. g.sx) < 90. && Float.abs (b.y -. g.sy) < 40.) g.bullets in
          let ty = match danger with Some b -> if b.y > g.sy then g.sy -. 60. else g.sy +. 60. | None -> target_y in
          let tx = g.cam -. 300. in
          let want = (g.power.cursor = 0 && g.power.speed = 0) || g.power.cursor = 4 in
          { initial_computer.keyboard with kup = ty > g.sy +. 4.; kdown = ty < g.sy -. 4.; kright = tx > g.sx +. 4.; kleft = tx < g.sx -. 4.;
            kspace = !i mod 2 = 0; keys = (if want && !i mod 2 = 1 then Set_.singleton "x" else Set_.empty) }
      | Clear _ -> cleared := true; initial_computer.keyboard
      | _ -> { initial_computer.keyboard with kspace = !i mod 2 = 0 }
    in
    s := update (computer ~keyboard !i) !s
  done;
  Alcotest.(check bool) "stage clear" true !cleared;
  Alcotest.(check bool) "at most one ship lost" true (!deaths <= 1)

(*****************************************************************************)
(* TinyZelda *)
(*****************************************************************************)

(* a robot's quest, from tile to tile: the sword, the key, down into the
 * dungeon, through the locked door, to the Triforce; a monster coming
 * near, it turns to face it and swings *)
let zelda_robot () =
  let open TinyZelda in
  let route = [ (9, 3); (9, 2); (9, 5); (20, 5); (20, 7); (23, 7); (23, 5); (23, 12); (18, 12); (18, 19); (25, 19); (25, 16); (30, 16); (33, 16); (39, 16) ] in
  let s = ref initial_model and todo = ref route and i = ref 0 and won = ref false and hits = ref 0 in
  let k = initial_computer.keyboard in
  let towards dx dy = if Float.abs dx > Float.abs dy then (if dx > 0. then { k with kright = true } else { k with kleft = true }) else if dy > 0. then { k with kup = true } else { k with kdown = true } in
  while !i < 60 * 120 && not !won do
    incr i;
    let keyboard =
      match !s.scene with
      | Playing g -> (
          if g.hurt = 59 then incr hits;
          let here = room_of g.x g.y in
          let near = List.find_opt (fun m -> room_of m.mx m.my = here && Float.hypot (m.mx -. g.x) (m.my -. g.y) < 100.) g.monsters in
          match (near, !todo) with
          | Some m, _ when g.sword ->
              let dx = m.mx -. g.x and dy = m.my -. g.y in
              let want = if Float.abs dx > Float.abs dy then (Float.of_int (compare dx 0.), 0.) else (0., Float.of_int (compare dy 0.)) in
              if want = g.facing then { k with kspace = !i mod 2 = 0 } else towards dx dy
          | _, (c, r) :: rest ->
              let tx, ty = Tilemap.center g.map c r in
              if Float.abs (tx -. g.x) < 3. && Float.abs (ty -. g.y) < 3. then todo := rest;
              (* one axis, then the other *)
              if Float.abs (tx -. g.x) >= 3. then towards (tx -. g.x) 0. else towards 0. (ty -. g.y)
          | _, [] -> k)
      | Won _ -> won := true; k
      | _ -> { k with kspace = !i = 1 }
    in
    s := update (computer ~keyboard !i) !s
  done;
  Alcotest.(check bool) "the Triforce" true !won;
  Alcotest.(check bool) "hit at most twice" true (!hits <= 2)

(*****************************************************************************)
(* TinyRogue *)
(*****************************************************************************)

(* the shortest way, by breadth-first search on the squares one can walk
 * (and the target's), from the player to a target square: its first
 * step *)
let rogue_path (g : TinyRogue.game) (target : int * int) : (int * int) option =
  let open TinyRogue in
  let seen = Hashtbl.create 100 in
  let q = Queue.create () in
  Queue.add ((g.px, g.py), None) q;
  Hashtbl.replace seen (g.px, g.py) ();
  let result = ref None in
  while !result = None && not (Queue.is_empty q) do
    let (c, r), first = Queue.pop q in
    if (c, r) = target then result := first
    else
      List.iter
        (fun (dx, dy) ->
          let n = (c + dx, r + dy) in
          if (not (Hashtbl.mem seen n)) && (walkable g (fst n) (snd n) || n = target) then begin
            Hashtbl.replace seen n ();
            Queue.add (n, (match first with None -> Some (dx, dy) | f -> f)) q
          end)
        [ (1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (-1, 1); (1, -1); (-1, -1) ]
  done;
  !result

(* every level of 20 dungeons: the stairs (or the Amulet) reachable from
 * the start *)
let rogue_connected () =
  let open TinyRogue in
  for seed = 1 to 20 do
    List.iter
      (fun depth ->
        let g = enter depth (new_game seed) in
        let goal = fst (List.find (fun (_, it) -> it = Stairs || it = Amulet) g.level.items) in
        Alcotest.(check bool) (Printf.sprintf "seed %d, level %d" seed depth) true (rogue_path { g with level = { g.level with monsters = [] } } goal <> None))
      [ 1; 2; 3 ]
  done

(* a robot knowing the map: to the stairs, then down; to the Amulet at
 * the bottom; fighting what's next to it, drinking a potion when low:
 * it gets the Amulet *)
let rogue_robot () =
  let open TinyRogue in
  let won = ref 0 in
  List.iter
    (fun seed ->
      let g = ref (look (new_game seed)) and over = ref false and n = ref 0 in
      while (not !over) && !n < 2000 do
        incr n;
        let g0 = !g in
        let next_to = List.find_opt (fun m -> abs (m.mx - g0.px) <= 1 && abs (m.my - g0.py) <= 1) g0.level.monsters in
        let goal = fst (List.find (fun (_, it) -> it = Stairs || it = Amulet) g0.level.items) in
        let action =
          if g0.hp <= 5 && g0.potions > 0 then Quaff
          else
            match next_to with
            | Some m -> Move (m.mx - g0.px, m.my - g0.py)
            | None -> if on_stairs g0 then Descend else (match rogue_path g0 goal with Some d -> Move (fst d, snd d) | None -> Rest)
        in
        let g1, killer = turn g0 action in
        g := g1;
        if killer <> None then over := true;
        if on_amulet g1 then (incr won; over := true)
      done)
    [ 1; 2; 3; 4; 5 ];
  Alcotest.(check bool) "the Amulet, at least 3 times in 5" true (!won >= 3)

(*****************************************************************************)
(* TinyStreetFighter *)
(*****************************************************************************)

(* quarter_circle's worked examples *)
let sf_quarter_circle () =
  let open TinyStreetFighter in
  let none = { back = false; forward = false; up = false; down = false } in
  let d = { none with down = true } and df = { none with down = true; forward = true } and f = { none with forward = true } in
  Alcotest.(check bool) "down, down-forward, forward" true (quarter_circle 100 [ (97, f); (93, df); (90, d) ]);
  Alcotest.(check bool) "forward then down" false (quarter_circle 100 [ (97, d); (93, df); (90, f) ]);
  Alcotest.(check bool) "too long ago" false (quarter_circle 100 [ (97, f); (93, df); (70, d) ])

(* the motion, frame by frame, down, down-forward, forward, and punch:
 * a fireball's startup *)
let sf_fireball () =
  let open TinyStreetFighter in
  let none = { back = false; forward = false; up = false; down = false } in
  let inputs =
    List.map (fun d -> { dir = d; punch = false; kick = false }) [ { none with down = true }; { none with down = true; forward = true }; { none with forward = true } ]
    @ [ { dir = { none with forward = true }; punch = true; kick = false } ]
  in
  let f, _ = List.fold_left (fun (f, n) i -> (step_fighter n i f, n + 1)) (new_fighter 0. 1. 0, 1) inputs in
  Alcotest.(check bool) "a fireball" true (match f.state with Attacking (Fireball, _) -> true | _ -> false)

(* the blocks: a kick, blocked holding back; a low kick hits a standing
 * guard, not a crouching one; a jump kick the other way round *)
let sf_blocks () =
  let open TinyStreetFighter in
  let none = { back = false; forward = false; up = false; down = false } in
  let back = { dir = { none with back = true }; punch = false; kick = false } and back_down = { dir = { none with back = true; down = true }; punch = false; kick = false } in
  let attacker a y = { (new_fighter 0. 1. 0) with state = Attacking (a, (move_of a).startup + 1); y } in
  let defender = new_fighter 120. (-1.) 0 in
  let result a y i = let _, d, _ = strike (attacker a y) defender i in match d.state with Blocking _ -> `Blocked | Hit _ -> `Hit | _ -> `Missed in
  Alcotest.(check bool) "kick, standing guard" true (result Kick 0. back = `Blocked);
  Alcotest.(check bool) "low kick, standing guard" true (result Low_kick 0. back = `Hit);
  Alcotest.(check bool) "low kick, crouching guard" true (result Low_kick 0. back_down = `Blocked);
  Alcotest.(check bool) "jump kick, crouching guard" true (result Jump_kick 120. back_down = `Hit);
  Alcotest.(check bool) "jump kick, standing guard" true (result Jump_kick 120. back = `Blocked)

(* the computer knocks out a player who does nothing *)
let sf_computer () =
  let open TinyStreetFighter in
  let g = ref (new_game false) and i = ref 0 in
  while !g.over = 0 && !i < 60 * 60 do
    incr i;
    g := update_fight (computer !i) (Scene2d.start (Fight !g)) !g
  done;
  Alcotest.(check int) "knocked out" 0 !g.p1.hp;
  Alcotest.(check bool) "the computer standing" true (!g.p2.hp > 0)

(*****************************************************************************)
(* TinyFinalFight *)
(*****************************************************************************)

(* the combo: three punches, each in the last one's recovery, on a thug
 * on the player's line: knocked down; the same thug a little deeper in
 * the street: missed *)
let ff_combo () =
  let open TinyFinalFight in
  let p = new_player () in
  let t = { (thug (p.x +. 70.) p.z false red) with wait = 999 } in
  let run (t : fighter) =
    let g = ref { (new_game ()) with player = p; thugs = [ t ]; wave = List.length waves; cam = 500. } in
    for i = 1 to 40 do
      let keyboard = { initial_computer.keyboard with kspace = List.mem i [ 1; 7; 13 ] } in
      let s = Scene2d.update (computer ~keyboard i) (Scene2d.start (Street !g)) in
      g := update_street (computer ~keyboard i) s !g
    done;
    List.hd !g.thugs
  in
  let t1 = run t in
  Alcotest.(check bool) "knocked down" true (match t1.state with Knocked _ -> true | _ -> false);
  Alcotest.(check int) "4 + 5 + 8" (24 - 17) t1.hp;
  Alcotest.(check int) "off the line: untouched" 24 (run { t with z = t.z +. 30. }).hp

(* a robot clears the street: to the nearest thug's line, at arm's
 * length, punching every 6 frames (the combo), spinning when two are
 * near; walking on when the wave is down *)
let ff_robot () =
  let open TinyFinalFight in
  let s = ref initial_model and i = ref 0 and cleared_ = ref false in
  let k = initial_computer.keyboard in
  while !i < 60 * 180 && not !cleared_ do
    incr i;
    let keyboard =
      match !s.scene with
      | Street g -> (
          let p = g.player in
          let alive = List.filter (fun t -> t.hp > 0) g.thugs in
          match List.sort (fun a b -> compare (Float.abs (a.x -. p.x) +. Float.abs (a.z -. p.z)) (Float.abs (b.x -. p.x) +. Float.abs (b.z -. p.z))) alive with
          | [] -> { k with kright = true }
          | t :: _ ->
              let near = List.filter (fun t -> Float.abs (t.x -. p.x) < 110. && Float.abs (t.z -. p.z) < 18.) alive in
              if List.length near >= 2 && p.hp > 30 then { k with keys = (if !i mod 10 = 0 then Set_.singleton "z" else Set_.empty) }
              else
                let side = if t.x > p.x then 1. else -1. in
                let reach = if t.big then 95. else 65. in
                let tx = t.x -. (side *. reach) in
                let dz = t.z -. p.z and dx = tx -. p.x in
                if Float.abs dz > 4. || Float.abs dx > 6. then { k with kup = dz > 4.; kdown = dz < -4.; kright = dx > 6.; kleft = dx < -6. }
                else if p.facing <> side then (if side > 0. then { k with kright = true } else { k with kleft = true })
                else { k with kspace = !i mod 6 = 0 })
      | Cleared _ -> cleared_ := true; k
      | _ -> { k with kspace = !i = 1 }
    in
    s := update (computer ~keyboard !i) !s
  done;
  Alcotest.(check bool) "street cleared" true !cleared_

(*****************************************************************************)
(* TinyBabaIsYou *)
(*****************************************************************************)

(* the shortest solution of a level, by a breadth-first search over the
 * boards (a board: what's where, the objects' ids aside) *)
let baba_solve (level : int) : int option =
  let open TinyBabaIsYou in
  let key (b : board) = List.sort compare (List.map (fun o -> (o.kind, o.c, o.r)) b.objs) in
  let seen = Hashtbl.create 10000 and q = Queue.create () in
  let start = load level in
  Hashtbl.replace seen (key start) ();
  Queue.add (start, 0) q;
  let found = ref None in
  while !found = None && (not (Queue.is_empty q)) && Hashtbl.length seen < 300_000 do
    let b, n = Queue.pop q in
    List.iter
      (fun d ->
        let b' = turn b d in
        if won b' && !found = None then found := Some (n + 1)
        else if (not (Hashtbl.mem seen (key b'))) && not (no_you b') then begin
          Hashtbl.replace seen (key b') ();
          Queue.add (b', n + 1) q
        end)
      [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
  done;
  !found

(* every level can be won (in 10, 14, 12 and 15 moves); the rules read
 * off the first one *)
let baba_levels () =
  let open TinyBabaIsYou in
  let rs = rules (load 0) in
  Alcotest.(check int) "four rules" 4 (List.length rs);
  Alcotest.(check bool) "BABA IS YOU" true (List.mem (Baba, Prop You) rs);
  List.iteri
    (fun i _ ->
      match baba_solve i with
      | Some n -> Printf.printf "level %d: %d moves\n" (i + 1) n
      | None -> Alcotest.fail (Printf.sprintf "level %d unsolvable" (i + 1)))
    levels

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
      t "TinyKart, Mode 7 there and back" kart_mode7;
      t "TinyKart, the computer drives the race" kart_race;
      t "TinyDoom, the BSP: convex subsectors, the right sectors" doom_bsp;
      t "TinyDoom, a frame" doom_frame;
      t "TinyDoom, a robot finds the exit" doom_exit;
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
      t "TinyXpilot, clipping for the split screen" xpilot_clip;
      t "TinyGalaga, the formation" galaga_formation;
      t "TinyGalaga, a robot clears stage 1" galaga_robot;
      t "TinyDonkeyKong, a girder's height" kong_height;
      t "TinyDonkeyKong, a jump, a fall" kong_jump;
      t "TinyDonkeyKong, a robot rescues Pauline" kong_robot;
      t "TinyLodeRunner, digging" lode_dig;
      t "TinyLodeRunner, a guard trapped, the player crushed" lode_trap;
      t "TinyLodeRunner, the escape ladder" lode_escape;
      t "TinyRick, a robot escapes the temple" rick_robot;
      t "TinyGradius, the power-up bar" gradius_bar;
      t "TinyGradius, a robot clears the stage" gradius_robot;
      t "TinyZelda, a robot's quest" zelda_robot;
      t "TinyRogue, the dungeons connected" rogue_connected;
      t "TinyRogue, a robot gets the Amulet" rogue_robot;
      t "TinyStreetFighter, the quarter circle" sf_quarter_circle;
      t "TinyStreetFighter, blocking high and low" sf_blocks;
      t "TinyStreetFighter, the fireball's motion" sf_fireball;
      t "TinyStreetFighter, the computer fights" sf_computer;
      t "TinyFinalFight, the combo, on the line" ff_combo;
      t "TinyFinalFight, a robot clears the street" ff_robot;
      t "TinyBabaIsYou, every level solvable" baba_levels;
      t "TinyTron, the computer outlasts a straight line" tron_computer ]
