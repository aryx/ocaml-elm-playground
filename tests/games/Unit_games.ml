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
(* TinyComanche *)
(*****************************************************************************)

(* A robot pops the balloons, the nearest first: turning towards it,
 * flying when facing it, climbing or descending to its height; the
 * ground always under the helicopter (the kit's [height], plus the
 * clearance) *)
let comanche_balloons () =
  let open TinyComanche in
  let m = ref initial_model and i = ref 0 in
  while !m.finished = None && !i < 60 * 120 do
    incr i;
    let k = initial_computer.keyboard in
    let nearest = List.sort (fun (x1, y1, _) (x2, y2, _) -> compare (Float.hypot (x1 -. !m.x) (y1 -. !m.y)) (Float.hypot (x2 -. !m.x) (y2 -. !m.y))) !m.left in
    let keyboard =
      match nearest with
      | (x, y, z) :: _ ->
          let wanted = atan2 (y -. !m.y) (x -. !m.x) *. 180. /. Float.pi in
          let d = Float.rem (Float.rem (wanted -. !m.angle +. 180.) 360. +. 360.) 360. -. 180. in
          let k = { k with kw = z > !m.alt +. 1.; ks = z < !m.alt -. 1. } in
          if d > 5. then { k with kleft = true } else if d < -5. then { k with kright = true }
          else if Float.hypot (x -. !m.x) (y -. !m.y) < 20. && !m.speed > 0.5 then { k with kdown = true }
          else { k with kup = true }
      | [] -> k
    in
    m := update (computer ~keyboard !i) !m;
    Alcotest.(check bool) "above the ground" true (!m.alt >= Heightmap.height map !m.x !m.y +. clearance -. 1e-9)
  done;
  Alcotest.(check bool) "all popped" true (!m.finished <> None)

(*****************************************************************************)
(* TinyDescent *)
(*****************************************************************************)

(* The mine holds the ship in: flying straight ahead for ten seconds,
 * it ends up against the rock, still inside, in some cell. Then a shot
 * down the corridor destroys the robot waiting in the next room -- and
 * with all of them gone, the exit ends the game. *)
let descent_mine () =
  let open TinyDescent in
  let fly keys n m =
    let s = ref m in
    for i = 1 to n do
      s := update (computer ~keyboard:(keys i) i) !s
    done;
    !s
  in
  let ahead i = { initial_computer.keyboard with kw = true; kspace = i > 30 } in
  let m = fly (fun _ -> { initial_computer.keyboard with kw = true }) 600 initial_model in
  Alcotest.(check bool) "still in the mine" true (Segments.segment_at level m.p <> None);
  Alcotest.(check bool) "it went somewhere" true (Float.abs (let x, _, _ = m.p in x -. 20.) > 50.);
  let m = fly ahead 60 initial_model in
  Alcotest.(check int) "one robot shot down the corridor" 3 (List.length m.robots);
  (* in the exit, with no robots left: out *)
  let m = fly (fun _ -> initial_computer.keyboard) 1 { initial_model with p = (150., 72., 110.); robots = [] } in
  Alcotest.(check bool) "escaped" true (match m.over with Some (_, true) -> true | _ -> false)

(*****************************************************************************)
(* TinyQuake *)
(*****************************************************************************)

(* The level pipeline: qbsp's tree says what is rock and what is air,
 * vis's set is smaller than the whole level and works both ways, and
 * light's patches all landed in an air leaf. *)
let quake_tools () =
  let open TinyQuake in
  List.iter
    (fun (name, p, solid) -> Alcotest.(check bool) name solid (solid_at p))
    [ ("inside a wall", (500., 50., 500.), true); ("under the floor", (200., -20., 200.), true);
      ("the start room", (200., 60., 200.), false); ("where the player starts", add start_at (0., 8., 0.), false) ];
  let here = leaf_id (add start_at (0., eye_height, 0.)) in
  Alcotest.(check bool) "the player starts in an air leaf" true (here >= 0);
  let set = pvs here in
  let seen = Array.fold_left (fun n v -> if v then n + 1 else n) 0 set in
  Alcotest.(check bool) "the set is smaller than the level" true (seen > 0 && seen < leaves);
  Alcotest.(check bool) "seeing works both ways" true (Array.for_all Fun.id (Array.init leaves (fun other -> set.(other) = sees other here)));
  Alcotest.(check bool) "every lamp is in the air" true (List.for_all (fun (l : lamp) -> not (solid_at l.where)) lamps);
  Alcotest.(check bool) "the runes and the exit too" true (List.for_all (fun r -> not (solid_at r)) (exit_at :: runes))

(* Walking: gravity puts the player on the floor and the walls stop
 * him; the three runes then the exit end the game *)
let quake_walk () =
  let open TinyQuake in
  let play keys n m =
    let s = ref m in
    for i = 1 to n do
      s := update (computer ~keyboard:(keys i) i) !s
    done;
    !s
  in
  let still = { initial_computer.keyboard with kw = false } in
  let m = play (fun _ -> still) 60 { initial_model with p = add start_at (0., 100., 0.) } in
  let _, y, _ = m.p in
  Alcotest.(check (float 1.)) "fallen back to the floor" 0. y;
  let m = play (fun _ -> { still with kw = true }) 400 initial_model in
  Alcotest.(check bool) "still in the air, somewhere else" true ((not (solid_at m.p)) && Float.abs (let x, _, _ = m.p in x -. 128.) > 100.);
  let m = play (fun _ -> still) 2 { initial_model with p = exit_at; runes = [] } in
  Alcotest.(check bool) "out" true (m.over <> None)

(*****************************************************************************)
(* TinyMinecraft *)
(*****************************************************************************)

(* The world: [shown] holds exactly the blocks with a side touching
 * air, and nothing else -- checked on the generated world, then
 * exactly on a solid 3 x 3 x 3 cube, whose center is buried until a
 * neighbour is taken away *)
let minecraft_world () =
  let open TinyMinecraft in
  let m = world in
  Alcotest.(check bool) "the world is not empty" true (Hashtbl.length m.blocks > 0);
  Alcotest.(check bool) "every shown block exists and is exposed" true
    (Hashtbl.fold (fun p _ ok -> ok && Hashtbl.mem m.blocks p && exposed m p) m.shown true);
  Alcotest.(check bool) "and every exposed block is shown" true
    (Hashtbl.fold (fun p _ ok -> ok && ((not (exposed m p)) || Hashtbl.mem m.shown p)) m.blocks true);
  let cube = create () in
  List.iter (fun p -> add_block cube p Stone)
    (List.concat_map (fun x -> List.concat_map (fun y -> List.map (fun z -> (x, y, z)) [ -1; 0; 1 ]) [ -1; 0; 1 ]) [ -1; 0; 1 ]);
  Alcotest.(check bool) "the middle of a solid cube is buried" false (exposed cube (0, 0, 0) || Hashtbl.mem cube.shown (0, 0, 0));
  remove_block cube (1, 0, 0);
  Alcotest.(check bool) "taking a neighbour away exposes and shows it" true (exposed cube (0, 0, 0) && Hashtbl.mem cube.shown (0, 0, 0));
  add_block cube (1, 0, 0) Stone;
  Alcotest.(check bool) "putting it back buries it again" false (exposed cube (0, 0, 0) || Hashtbl.mem cube.shown (0, 0, 0));
  (* the sight line, along +x from outside the cube: its near face, and
   * the empty cell in front of it, where a new block would go *)
  Alcotest.(check (option (pair (triple int int int) (option (triple int int int)))))
    "what the crosshair is on" (Some ((-1, 0, 0), Some (-2, 0, 0)))
    (hit_test cube ~position:(-5., 0., 0.) ~vector:(1., 0., 0.) ());
  Alcotest.(check bool) "nothing, aimed over the cube" true (hit_test cube ~position:(-5., 10., 0.) ~vector:(1., 0., 0.) () = None)

(* The player, in a world worked out by hand: a stone floor at y = -2
 * for x, z in -5..5, and a wall at z = -3, in front of a player
 * starting at the origin, looking along -z at it:
 *
 *     y
 *     0  . . . W . . .       W: the wall (z = -3)
 *    -1  . . . W . . .       P: the player's eyes (0, 0, 0),
 *    -2  F F F F F F F          body from y 0 down to -1
 *        -6 -5 -4 -3 -2 -1 0  z  (P at z = 0)
 *)
let minecraft_player () =
  let open TinyMinecraft in
  let w = create () in
  for x = -5 to 5 do
    for z = -5 to 5 do
      add_block w (x, -2, z) Stone
    done;
    add_block w (x, -1, -3) Stone;
    add_block w (x, 0, -3) Stone
  done;
  let none : TinyMinecraft.input = { forward = 0; right = 0; jump = false } in
  (* [seconds] of frames at 60 fps *)
  let run ?(input = none) (seconds : float) (p : TinyMinecraft.player) : TinyMinecraft.player =
    let rec loop n p = if n = 0 then p else loop (n - 1) (step w ~dt:(1. /. 60.) input p) in
    loop (int_of_float (seconds *. 60.)) p
  in
  let y_of (p : TinyMinecraft.player) = let _, y, _ = p.position in y in
  let z_of (p : TinyMinecraft.player) = let _, _, z = p.position in z in
  let close a b = Float.abs (a -. b) < 1e-6 in
  let sx, sy, sz = sight_vector initial_player in
  Alcotest.(check bool) "yaw 0 looks along -z" true (close sx 0. && close sy 0. && close sz (-1.));
  let sx, _, sz = sight_vector { initial_player with yaw = 90. } in
  Alcotest.(check bool) "yaw 90 looks along +x, turning right" true (close sx 1. && close sz 0.);
  (* standing: gravity pulls, the floor pushes back, and the eyes end
   * up a quarter block (the collision's [pad]) into the cell above it *)
  let rest = run 2. initial_player in
  Alcotest.(check bool) "standing on the floor, not through it" true (y_of rest > -0.5 && y_of rest < 0. && rest.dy = 0.);
  let fallen = run 3. { initial_player with position = (0., 5., 0.) } in
  Alcotest.(check bool) "falling from 5 blocks up lands there too" true (close (y_of fallen) (y_of rest) && fallen.dy = 0.);
  (* a jump: one frame of space, then the highest it gets -- the jump
   * speed is worked out for exactly one block *)
  let rec highest n p best =
    if n = 0 then (best, p)
    else
      let p = step w ~dt:(1. /. 60.) none p in
      highest (n - 1) p (Float.max best (y_of p))
  in
  let peak, landed = highest 120 (step w ~dt:(1. /. 60.) { none with jump = true } rest) (y_of rest) in
  Alcotest.(check bool) "a jump rises about a block, and lands back" true
    (peak -. y_of rest > 0.9 && peak -. y_of rest < 1.1 && close (y_of landed) (y_of rest));
  let walked = run ~input:{ none with forward = 1 } 3. rest in
  Alcotest.(check (float 0.01)) "walking into the wall, stopped a quarter block into it" (-2.25) (z_of walked);
  let hovering = run 1. { initial_player with position = (0., 3., 0.); flying = true } in
  Alcotest.(check bool) "flying: no gravity" true (close (y_of hovering) 3.);
  let climbed = run ~input:{ none with forward = 1 } 0.2 { initial_player with position = (0., 3., 0.); flying = true; pitch = 45. } in
  Alcotest.(check bool) "flying forward while looking up climbs" true (y_of climbed > 3.5)

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
(* TinyMarbleMadness *)
(*****************************************************************************)

(* the first ramp, "vvvv" twice between the 9s and the 7s: its edges at
 * 9, 8 and 7; halfway down its first row, 8.5 *)
let marble_ramp () =
  let open TinyMarbleMadness in
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
  let open TinyMarbleMadness in
  let rec go b i =
    if i > 600 then Alcotest.fail "never landed"
    else match step (0., 1.) b with _, Some h -> h | b, None -> go b (i + 1)
  in
  go (ball_at (c, r)) 0

(* the shortcut down the cliff, from the 7s to the 2s: 5 high, broken;
 * off the end of the bridge, from the 2s to the 1s: 1 high, fine *)
let marble_falls () =
  let open TinyMarbleMadness in
  let cliff = marble_fall_from (2, 7) and step = marble_fall_from (4, 19) in
  Alcotest.(check (float 0.1)) "the cliff" 5. cliff;
  Alcotest.(check bool) "breaks" true (cliff > max_fall);
  Alcotest.(check (float 0.1)) "the step" 1. step;
  Alcotest.(check bool) "doesn't break" true (step <= max_fall)

(* collide's worked example: the steelie (2) at 0.1 hits the marble (1)
 * at rest: the marble goes off at 0.133, the steelie on at 0.033 *)
let marble_steelie () =
  let open TinyMarbleMadness in
  let me = ball_at (5, 13) in
  let steelie = { me with x = me.x -. 0.9; vx = 0.1 } in
  let me, steelie = collide me 1. steelie 2. in
  Alcotest.(check (float 1e-9)) "the marble" (0.4 /. 3.) me.vx;
  Alcotest.(check (float 1e-9)) "the steelie" (0.1 /. 3.) steelie.vx

(* left alone at the top of the first ramp, the marble rolls down it by
 * itself, onto the 7s, faster than a push on the flat would take it in
 * the same time *)
let marble_rolls_down () =
  let open TinyMarbleMadness in
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
  let open TinyMarbleMadness in
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
(* TinyMissileCommand *)
(*****************************************************************************)

(* a robot aims every counter-missile where a warhead will be when the
 * counter-missile gets there, and survives the first two waves (aiming
 * 40 frames ahead, it lost 5 cities in the first one: most of its
 * ammo missed) *)
let missile_robot () =
  let open TinyMissileCommand in
  let s = ref (update (computer ~keyboard:{ initial_computer.keyboard with kspace = true } 1) initial_model) in
  let waves = ref 1 and cities = ref 6 in
  for i = 2 to 60 * 60 do
    let mouse, keyboard =
      match !s.scene with
      | Playing g when i mod 12 = 0 -> (
          (* the lowest warhead not already aimed at *)
          let aimed (m : missile) = List.exists (fun (c : missile) -> Float.hypot (fst c.target -. m.shot.x) (snd c.target -. m.shot.y) < 150.) g.mine in
          match List.sort (fun (a : missile) b -> compare a.shot.y b.shot.y) (List.filter (fun m -> not (aimed m)) g.theirs) with
          | m :: _ when m.shot.y > ground +. 120. ->
              (* where it will be when the counter-missile gets there: the
               * flight time from the nearest base, refined 3 times *)
              let ahead t = (m.shot.x +. (t *. m.shot.vx), m.shot.y +. (t *. m.shot.vy)) in
              let flight (x, y) =
                let bx = List.fold_left (fun b x' -> if Float.abs (x' -. x) < Float.abs (b -. x) then x' else b) 1e9 base_x in
                Float.hypot (x -. bx) (y -. base_top) /. 12.
              in
              let x, y = ahead (flight (ahead (flight (ahead (flight (ahead 0.)))))) in
              ({ initial_computer.mouse with mx = x; my = y; mdx = 1.; mclick = true }, initial_computer.keyboard)
          | _ -> (initial_computer.mouse, initial_computer.keyboard))
      | _ -> (initial_computer.mouse, initial_computer.keyboard)
    in
    s := update { (computer ~keyboard i) with mouse } !s;
    match !s.scene with
    | Playing g ->
        waves := g.wave;
        cities := List.length (List.filter Fun.id g.cities)
    | _ -> ()
  done;
  Printf.printf "wave %d, %d cities\n" !waves !cities;
  Alcotest.(check bool) "past wave 2" true (!waves >= 3);
  Alcotest.(check bool) "cities left" true (!cities >= 3)

(* a warhead in an explosion explodes, and its explosion catches the
 * next one: the chain reaction *)
let missile_chain () =
  let open TinyMissileCommand in
  let g = { (new_game ()) with pause = 0; to_come = 0 } in
  let warhead x = { from = (x, 500.); target = (x, ground); shot = Shots.straight x 0. 0. (-0.1) } in
  let g = { g with theirs = [ warhead 0.; warhead 40.; warhead 80. ]; explosions = [ { ex = 0.; ey = 0.; age = 29 } ] } in
  let g = ref g in
  for i = 1 to 60 do g := update_game (computer i) (Scene2d.start Title) !g done;
  Alcotest.(check int) "all three" 0 (List.length !g.theirs);
  Alcotest.(check int) "the score" 75 !g.score

(*****************************************************************************)
(* TinyLemmings *)
(*****************************************************************************)

(* a level played by a plan: the k-th job given (once) to the first
 * walker [where] says; the lemmings saved *)
let lemmings_play (level : int) (plan : (int * (TinyLemmings.lemming -> bool)) list) : int =
  let open TinyLemmings in
  let g = ref (new_game level) and i = ref 0 in
  let todo = ref plan in
  while not (over !g) do
    incr i;
    g := update_game (computer !i) (Scene2d.start Title) !g;
    match !todo with
    | (k, where) :: rest -> (
        match List.find_opt (fun (_, l) -> l.job = Walker && where l) (List.mapi (fun n l -> (n, l)) !g.lemmings) with
        | Some (n, _) ->
            g := assign !g n k;
            todo := rest
        | None -> ())
    | [] -> ()
  done;
  saved !g

let lemmings_levels () =
  let open TinyLemmings in
  (* nothing done: level 1's lemmings walk forever, level 3's fall *)
  Alcotest.(check int) "level 1, no job" 0 (lemmings_play 0 []);
  Alcotest.(check int) "level 3, no job" 0 (lemmings_play 2 []);
  (* a digger through the floor; a basher through the wall; a builder
   * over the gap *)
  let saved1 = lemmings_play 0 [ (3, fun l -> l.x > 80) ] in
  let saved2 = lemmings_play 1 [ (2, fun l -> l.x >= 105) ] in
  let saved3 = lemmings_play 2 [ (1, fun l -> l.x >= 97) ] in
  Printf.printf "saved: %d, %d, %d\n" saved1 saved2 saved3;
  List.iteri (fun i n -> Alcotest.(check bool) (Printf.sprintf "level %d" (i + 1)) true (n >= (List.nth levels i).need)) [ saved1; saved2; saved3 ]

(* a fall of 64 cells is survived, not one of 65 *)
let lemmings_splat () =
  let open TinyLemmings in
  let fall h =
    let t = Bytes.make (cols * rows) '\000' in
    for c = 0 to cols - 1 do set t c 140 dirt done;
    let l = ref { x = 10; y = 139 - h; dir = 1; job = Faller 0 } in
    for _ = 1 to 100 do l := step t [] 0 !l done;
    !l.job
  in
  Alcotest.(check bool) "64 cells" true (fall 64 = Walker);
  Alcotest.(check bool) "65 cells" true (fall 65 = Dead)

(*****************************************************************************)
(* TinyPuzzleBobble *)
(*****************************************************************************)

(* on the hexagonal grid, a cell's neighbours are all one bubble away:
 * 64 pixels, a bit more between rows (56 and 32: 64.5) *)
let bobble_hex () =
  let open TinyPuzzleBobble in
  let g = new_round 0 0 1 in
  List.iter
    (fun cell ->
      let x, y = center g cell in
      Alcotest.(check int) "six" 6 (List.length (neighbours cell));
      List.iter
        (fun n ->
          let x', y' = center g n in
          Alcotest.(check bool) "one bubble away" true (Float.abs (Float.hypot (x -. x') (y -. y') -. 64.) < 1.))
        (neighbours cell))
    [ (2, 3); (3, 3); (4, 1) ]

(* a red shot completes three reds; the blue hanging below them falls,
 * the green next to the ceiling stays *)
let bobble_drop () =
  let open TinyPuzzleBobble in
  let g = { (new_round 0 0 1) with board = [ ((0, 0), 0); ((0, 1), 0); ((1, 0), 2); ((0, 3), 1) ]; current = 0 } in
  let x, y = center g (0, 2) in
  let g = stick g { x; y; vx = 0.; vy = 1.; color = 0 } in
  Alcotest.(check (list (pair (pair int int) int))) "the green only" [ ((0, 3), 1) ] g.board;
  Alcotest.(check int) "3 popped, 1 fallen" (30 + 20) g.score

(* a robot tries every angle with the aiming guide's [path] and shoots
 * where the most bubbles go (pop or fall), or else next to the most of
 * its color; it clears the three rounds *)
let bobble_robot () =
  let open TinyPuzzleBobble in
  let s = ref initial_model and cleared = ref 0 and i = ref 0 in
  while !cleared < List.length rounds && !i < 60 * 60 * 5 do
    incr i;
    let space = ref (!i mod 2 = 0) in
    (match !s.scene with
    | Playing g when g.shot = None && !space ->
        let value angle =
          let f = List.nth (path g angle) (List.length (path g angle) - 1) in
          let cell = snap g f in
          let after = stick g { f with color = g.current } in
          let same = List.length (List.filter (fun n -> List.assoc_opt n g.board = Some g.current) (neighbours cell)) in
          (List.length g.board + 1 - List.length after.board, same)
        in
        let angles = List.init 301 (fun k -> 15. +. (0.5 *. float_of_int k)) in
        let best = List.fold_left (fun a b -> if value b > value a then b else a) 90. angles in
        s := { !s with scene = Playing { g with angle = best } }
    | Clear g -> cleared := g.round + 1
    | Game_over g -> Alcotest.fail (Printf.sprintf "game over, round %d" (g.round + 1))
    | _ -> ());
    s := update (computer ~keyboard:{ initial_computer.keyboard with kspace = !space } !i) !s
  done;
  Printf.printf "rounds cleared: %d, in %d frames\n" !cleared !i;
  Alcotest.(check int) "all the rounds" (List.length rounds) !cleared

(*****************************************************************************)
(* TinyTowerDefense (ai/'s Pathfind) *)
(*****************************************************************************)

(* every tower makes the way longer, and the one that would close it is
 * refused: the search as the referee *)
let tower_maze () =
  let open TinyTowerDefense in
  let g = new_game () in
  let length (g : game) = List.length (way g.field entrance) - 1 in
  Alcotest.(check int) "straight across" 19 (length g);
  (* a wall of towers down the middle, all but the last cell *)
  let g = List.fold_left (fun g y -> build { g with gold = 1000 } (10, y)) g [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12 ] in
  Alcotest.(check int) "around the bottom" 31 (length g);
  Alcotest.(check bool) "the last cell would close it" false (can_build { g with gold = 1000 } (10, 13));
  let g' = build { g with gold = 1000 } (10, 13) in
  Alcotest.(check int) "so nothing was built" 31 (length g');
  Alcotest.(check int) "and the gold is untouched" 1000 g'.gold

(* a monster walking finds its way again when a tower lands in front of
 * it, and a tower that would trap it is refused *)
let tower_repath () =
  let open TinyTowerDefense in
  let g = { (new_game ()) with gold = 1000; pause = 0 } in
  let g = ref g in
  for i = 1 to 200 do g := update_game (computer i) (Scene2d.start Title) !g done;
  let m = List.hd !g.monsters in
  let ahead = (fst (cell_of m) + 2, snd (cell_of m)) in
  let before = List.length m.path in
  let g' = build !g ahead in
  let m' = List.hd g'.monsters in
  Alcotest.(check bool) "it goes around now" true (List.length m'.path > before);
  Alcotest.(check bool) "not through the tower" false (List.mem ahead m'.path);
  (* boxed in on three sides: the fourth tower is refused *)
  let x, y = cell_of m' in
  let g'' = List.fold_left (fun g c -> build { g with gold = 1000 } c) g' [ (x, y - 1); (x, y + 1); (x - 1, y) ] in
  Alcotest.(check bool) "the last way out stays open" false (can_build { g'' with gold = 1000 } (x + 1, y))

(* towers along the way kill the first waves *)
let tower_waves () =
  let open TinyTowerDefense in
  let g = ref { (new_game ()) with gold = 200 } in
  List.iter (fun c -> g := build !g c) [ (4, 6); (4, 8); (8, 6); (8, 8); (12, 6); (12, 8); (16, 6); (16, 8) ];
  for i = 1 to 60 * 90 do g := update_game (computer i) (Scene2d.start Title) !g done;
  Printf.printf "wave %d, %d lives, %d gold, score %d\n" !g.wave !g.lives !g.gold !g.score;
  Alcotest.(check bool) "past wave 3" true (!g.wave >= 3);
  Alcotest.(check bool) "still alive" true (!g.lives > 0);
  Alcotest.(check bool) "monsters killed" true (!g.score > 0)

(*****************************************************************************)
(* TinySonic (kits/platformer's Slope) *)
(*****************************************************************************)

(* holding right for [frames], reporting where he got to *)
let sonic_run ?(keys = fun _ -> initial_computer.keyboard) (frames : int) : TinySonic.game =
  let open TinySonic in
  let g = ref (new_game ()) in
  for i = 1 to frames do
    g := update_game (computer ~keyboard:(keys i) i) (Scene2d.start Title) !g
  done;
  !g

let right (_ : int) : keyboard = { initial_computer.keyboard with kright = true }

let sonic_walks () =
  let open TinySonic in
  let g = sonic_run ~keys:right 240 in
  Printf.printf "at %.0f, %.0f: speed %.2f, angle %.0f, grounded %b, rings %d\n" g.sonic.x g.sonic.y g.sonic.gsp g.sonic.angle
    g.sonic.grounded g.taken;
  Alcotest.(check bool) "he ran right" true (g.sonic.x > 400.);
  Alcotest.(check bool) "still in the world" true (g.sonic.y > 0. && g.sonic.y < 200.);
  Alcotest.(check bool) "fast, on the ground or over a bump" true
    (Float.abs (if g.sonic.grounded then g.sonic.gsp else g.sonic.vx) > 4.);
  Alcotest.(check bool) "picking up rings" true (g.taken > 0)

(* the loop: running at it fast enough, he goes round -- the modes
 * change under him, nothing else *)
let sonic_loop () =
  let open TinySonic in
  let g = ref (new_game ()) and modes = ref [] and highest = ref 0. and i = ref 0 in
  (* he holds right all the way to the sign post, as a player would *)
  while !i < 60 * 30 && !g.sonic.x < fst goal_at do
    incr i;
    g := update_game (computer ~keyboard:(right !i) !i) (Scene2d.start Title) !g;
    if not (List.mem !g.sonic.mode !modes) then modes := !g.sonic.mode :: !modes;
    if !g.sonic.x > 900. && !g.sonic.x < 1200. then highest := Float.max !highest !g.sonic.y
  done;
  Printf.printf "modes round the loop: %d; highest in it %.0f; ended at %.0f, %.0f\n" (List.length !modes) !highest !g.sonic.x
    !g.sonic.y;
  Alcotest.(check bool) "he went up the loop's wall" true (List.mem Slope.Right_wall !modes || List.mem Slope.Left_wall !modes);
  Alcotest.(check bool) "and along its ceiling" true (List.mem Slope.Ceiling !modes);
  Alcotest.(check bool) "over the top of it" true (!highest > 150.);
  Alcotest.(check bool) "he came out the other side" true (!g.sonic.x > 1200.);
  Alcotest.(check bool) "still in the world" true (!g.sonic.y > 0. && !g.sonic.y < 400.);
  Alcotest.(check bool) "and on the ground" true !g.sonic.grounded

let tests_sonic = [ t "TinySonic, he runs right" sonic_walks; t "TinySonic, the loop" sonic_loop ]

(*****************************************************************************)
(* TinyWarcraft2 (ai/'s Pathfind: a flow field) *)
(*****************************************************************************)

(* one Dijkstra from where the crowd is sent, and every unit walks
 * downhill on it: five of them, from five places, all arrive *)
let warcraft_crowd () =
  let open TinyWarcraft2 in
  let g = new_game () in
  let target = (8, 8) in
  let crowd = List.mapi (fun i (c : int * int) -> { (new_unit (100 + i) Footman Us c) with goal = Some target }) [ (2, 2); (4, 13); (6, 6); (2, 11); (5, 5) ] in
  let g = ref { g with units = crowd; our_field = send g.map target } in
  for _ = 1 to 1500 do
    g := { !g with units = List.map (fun u -> follow !g u) !g.units }
  done;
  List.iter (fun (u : unit_) -> Alcotest.(check (pair int int)) "arrived" target (cell_of u)) !g.units;
  (* the field knows the whole map, so it works from anywhere *)
  Alcotest.(check bool) "one field, the whole map" true (List.length !g.our_field > 300)

(* peasants mine gold and chop wood on their own: the purse fills, the
 * mine and the forest go down *)
let warcraft_gather () =
  let open TinyWarcraft2 in
  let start = new_game () in
  let ours = List.filter (fun (u : unit_) -> u.side = Us && u.job = Peasant) start.units in
  let g = ref { start with units = { (List.hd ours) with carrying = Wood 0 } :: List.tl ours } in
  let left what = List.fold_left (fun n i -> match (!g.map.(i), what) with (Mine k, `Gold) -> n + k | (Forest k, `Wood) -> n + k | _ -> n) 0 (List.init (cols * rows) Fun.id) in
  let gold0 = left `Gold and wood0 = left `Wood in
  for i = 1 to 60 * 90 do g := update_game (computer i) (Scene2d.start Title) !g done;
  Printf.printf "gold %d, wood %d; mine %d -> %d, forest %d -> %d\n" !g.gold !g.wood gold0 (left `Gold) wood0 (left `Wood);
  Alcotest.(check bool) "gold mined" true (!g.gold > 120);
  Alcotest.(check bool) "wood chopped" true (!g.wood > 60);
  Alcotest.(check bool) "the mine is smaller" true (left `Gold < gold0);
  Alcotest.(check bool) "the forest is smaller" true (left `Wood < wood0)

(* the fog: what a unit has walked past stays known, the far side never
 * is *)
let warcraft_fog () =
  let open TinyWarcraft2 in
  let g = ref (new_game ()) in
  for i = 1 to 120 do g := update_game (computer i) (Scene2d.start Title) !g done;
  Alcotest.(check bool) "home is seen" true !g.seen.(index our_hall);
  Alcotest.(check bool) "their hall is not" false !g.seen.(index their_hall);
  (* a footman sent across the map *)
  let target = (20, 4) in
  g := { !g with our_field = send !g.map target;
         units = List.map (fun (u : unit_) -> if u.job = Footman && u.side = Us then { u with goal = Some target } else u) !g.units };
  for i = 1 to 60 * 60 do g := update_game (computer i) (Scene2d.start Title) !g done;
  Alcotest.(check bool) "what it walked past is remembered" true !g.seen.(index (12, 4));
  Alcotest.(check bool) "home is still known" true !g.seen.(index our_hall)

(*****************************************************************************)
(* TinyDune2 (ai/'s Pathfind) *)
(*****************************************************************************)

(* an order is a path around the rocks, and the unit walks it to the end *)
let dune2_order () =
  let open TinyDune2 in
  let g = new_game () in
  let tank = List.nth g.units 1 in
  let target = (18, 8) in
  let tank = order g.terrain tank target in
  Alcotest.(check bool) "there is a way" true (tank.path <> []);
  Alcotest.(check bool) "it ends at the order" true (List.nth tank.path (List.length tank.path - 1) = target);
  Alcotest.(check bool) "no rock on it" true (List.for_all (fun c -> passable g.terrain c) tank.path);
  (* walked to the end *)
  let u = ref tank in
  for _ = 1 to 2000 do u := walk !u done;
  Alcotest.(check bool) "arrived" true (cell_of !u = target)

(* a harvester finds the nearest spice by itself, digs it, brings it
 * home: the credits go up and the patch goes down *)
let dune2_harvest () =
  let open TinyDune2 in
  let g = ref { (new_game ()) with units = [ List.hd (new_game ()).units ] } in
  let spice_left (g : game) =
    List.fold_left (fun n i -> match g.terrain.(i) with Spice k -> n + k | _ -> n) 0 (List.init (cols * rows) Fun.id)
  in
  let before = spice_left !g in
  for i = 1 to 60 * 60 do g := update_game (computer i) (Scene2d.start Title) !g done;
  Printf.printf "credits %d, spice %d -> %d\n" !g.credits before (spice_left !g);
  Alcotest.(check bool) "it earned credits" true (!g.credits > 150);
  Alcotest.(check bool) "it dug the spice" true (spice_left !g < before)

(* left alone, the enemy's tanks come and take our refinery down *)
let dune2_war () =
  let open TinyDune2 in
  let start = new_game () in
  (* our tank taken away: only harvesters at home *)
  let g = ref { start with units = List.filter (fun (u : TinyDune2.unit_) -> not (u.side = Us && u.kind = Tank)) start.units } in
  let over = ref None in
  let i = ref 0 in
  while !over = None && !i < 60 * 60 * 6 do
    incr i;
    g := update_game (computer !i) (Scene2d.start Title) !g;
    if not (List.exists (fun (b : building) -> b.bside = Us) !g.buildings) then over := Some Them
    else if not (List.exists (fun (b : building) -> b.bside = Them) !g.buildings) then over := Some Us
  done;
  Printf.printf "over after %d frames: %s\n" !i (match !over with Some Them -> "they won" | Some Us -> "we won" | None -> "nobody");
  Alcotest.(check bool) "they take our base" true (!over = Some Them)

(* tanks of ours sent at their refinery take it down *)
let dune2_attack () =
  let open TinyDune2 in
  let g = ref { (new_game ()) with credits = 1000 } in
  (* four tanks bought and ordered across the map *)
  for i = 1 to 4 do
    g := update_game (computer ~keyboard:(press "b" initial_computer.keyboard) i) (Scene2d.start Title) !g;
    g := update_game (computer i) (Scene2d.start Title) !g
  done;
  g := { !g with units = List.map (fun (u : unit_) -> if u.side = Us && u.kind = Tank then order !g.terrain u their_base else u) !g.units };
  let i = ref 0 and won = ref false in
  while (not !won) && !i < 60 * 60 * 4 do
    incr i;
    g := update_game (computer !i) (Scene2d.start Title) !g;
    (* they keep coming: order ours at their base again when idle *)
    g := { !g with units = List.map (fun (u : unit_) -> if u.side = Us && u.kind = Tank && u.path = [] && u.cooldown = 0 then order !g.terrain u their_base else u) !g.units };
    won := not (List.exists (fun (b : building) -> b.bside = Them) !g.buildings)
  done;
  Printf.printf "we won after %d frames, %d tanks left\n" !i (List.length (List.filter (fun (u : unit_) -> u.side = Us && u.kind = Tank) !g.units));
  Alcotest.(check bool) "their refinery is gone" true !won

(*****************************************************************************)
(* AiOthello (an example, ai/'s Minimax) *)
(*****************************************************************************)

(* the start: black's 4 moves, each flipping one disk *)
let othello_rules () =
  let open AiOthello in
  Alcotest.(check (list int)) "4 moves" [ 19; 26; 37; 44 ] (legal start);
  let p = play start (Put 19) in
  Alcotest.(check bool) "d4 flipped" true (p.board.(27) = Black);
  Alcotest.(check (pair int int)) "4 black, 1 white" (4, 1) (count p Black, count p White)

(* the computer against itself, 20 moves: at each, alpha-beta finds
 * minimax's move and value, looking at fewer positions *)
let othello_alphabeta () =
  let open AiOthello in
  let p = ref start and a_total = ref 0 and m_total = ref 0 in
  for _ = 1 to 20 do
    let a = Minimax.alphabeta othello ~depth !p and m = Minimax.minimax othello ~depth !p in
    Alcotest.(check (float 0.)) "the value" m.value a.value;
    Alcotest.(check bool) "the move" true (a.best = m.best);
    a_total := !a_total + a.nodes;
    m_total := !m_total + m.nodes;
    match a.best with Some mv -> p := play !p mv | None -> ()
  done;
  Printf.printf "alpha-beta: %d positions, minimax: %d\n" !a_total !m_total;
  Alcotest.(check bool) "fewer positions" true (!a_total < !m_total)

(* the computer (white) beats a greedy player, the one flipping the most
 * disks each time *)
let othello_greedy () =
  let open AiOthello in
  let greedy p =
    match moves p with
    | [ Pass ] -> Pass
    | ms -> List.fold_left (fun (best, n) m -> match m with Put i when List.length (flips p.board p.turn i) > n -> (m, List.length (flips p.board p.turn i)) | _ -> (best, n)) (List.hd ms, -1) ms |> fst
  in
  let p = ref start in
  while moves !p <> [] do
    p := play !p (if !p.turn = Black then greedy !p else Option.get (Minimax.alphabeta othello ~depth !p).best)
  done;
  Printf.printf "greedy %d, computer %d\n" (count !p Black) (count !p White);
  Alcotest.(check bool) "the computer wins" true (count !p White > count !p Black)

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

(*****************************************************************************)
(* TinyDungeonMaster *)
(*****************************************************************************)

(* The dungeon is winnable, and only in this order: the iron key is
 * within reach at the start but the stairs are not; the key opens the
 * iron door, which opens the north half -- still not the stairs; the
 * lever there raises the portcullis, which is the only way in. *)
let dungeon_master_winnable () =
  let open TinyDungeonMaster in
  (* every cell the hero can walk to from where it stands *)
  let reach (g : game) = List.map fst (Pathfind.field (problem g) (g.x, g.y)) in
  let one (c : char) (map : Tilemap.t) =
    match Tilemap.find map c with [ cell ] -> cell | _ -> Alcotest.failf "not one %c in the dungeon" c
  in
  let g = new_game () in
  let key = one 'k' g.map and stairs = one '>' g.map in
  Alcotest.(check bool) "the key is reachable" true (List.mem key (reach g));
  Alcotest.(check bool) "the stairs are not" false (List.mem stairs (reach g));
  (* at the door, with the key *)
  let g = hand { g with x = 6; y = 9; facing = North; keys = 1 } in
  Alcotest.(check bool) "the key opened the door" false (wall (Tilemap.get g.map 6 8));
  Alcotest.(check int) "and was used up" 0 g.keys;
  Alcotest.(check bool) "the stairs are still shut in" false (List.mem stairs (reach g));
  (* at the lever, beyond it *)
  let g = hand { g with x = 12; y = 7; facing = North } in
  Alcotest.(check (option char)) "the lever stays pulled" (Some 'l') (Tilemap.get g.map 12 6);
  Alcotest.(check bool) "and the stairs can be reached" true (List.mem stairs (reach g))

(* The rule the fights are built on: a monster that has just struck must
 * wait [attack_rest] frames and one that has just moved [move_rest], so
 * a hero who steps out of reach after each blow makes it spend its
 * clock closing the distance again instead of hitting. That is Dungeon
 * Master's dance, and it is the whole of it. *)
let dungeon_master_dance () =
  let open TinyDungeonMaster in
  let g = { (new_game ()) with x = 3; y = 14; monsters = [ { mx = 4; my = 14; hp = monster_hp; cool = 0; hurt = 0 } ] } in
  let g = step_monsters g in
  Alcotest.(check int) "it strikes" (max_hp - claw) g.hp;
  Alcotest.(check int) "then it must wait" attack_rest (List.hd g.monsters).cool;
  (* out of its reach while it waits *)
  let g = step { g with rest = 0 } West in
  Alcotest.(check (pair int int)) "a step away" (2, 14) (g.x, g.y);
  let g = ref g in
  for _ = 1 to attack_rest + 1 do
    g := step_monsters !g
  done;
  let m = List.hd !g.monsters in
  Alcotest.(check int) "no second blow" (max_hp - claw) !g.hp;
  Alcotest.(check (pair int int)) "it closed the distance instead" (3, 14) (m.mx, m.my);
  Alcotest.(check int) "and must wait again" move_rest m.cool

(*****************************************************************************)
(* PuzzleScriptSokoban, PuzzleScriptBoulders *)
(*****************************************************************************)

(* The three levels of examples/PuzzleScriptSokoban.ml are our own, so
 * something had better check that they can be solved. A board is a
 * value, so the search is the plain breadth-first one, over the boards
 * a turn can reach -- which is also the shortest way to see that the
 * one rule of that file really is the game. *)
let puzzlescript_sokoban_levels () =
  let open Puzzlescript in
  let g = PuzzleScriptSokoban.sokoban in
  let solve (level : int) : int option =
    let start = board g level in
    let seen = Hashtbl.create 1000 and waiting = Queue.create () in
    Hashtbl.replace seen (to_strings g start) ();
    Queue.push (start, 0) waiting;
    let answer = ref None in
    while !answer = None && not (Queue.is_empty waiting) do
      let b, moves = Queue.pop waiting in
      if won g b then answer := Some moves
      else
        List.iter
          (fun d ->
            let b' = turn g (Some d) b in
            let key = to_strings g b' in
            if not (Hashtbl.mem seen key) then begin
              Hashtbl.replace seen key ();
              Queue.push (b', moves + 1) waiting
            end)
          every
    done;
    !answer
  in
  Alcotest.(check (list (option int))) "one push, then seven moves, then fifteen" [ Some 1; Some 7; Some 15 ]
    (List.mapi (fun i _ -> solve i) PuzzleScriptSokoban.levels)

(* The cave of examples/PuzzleScriptBoulders.ml, played: down the left
 * side for the first diamond, then right along the bottom for the
 * second. The boulder keeps its dirt under it the whole way, so the run
 * is the same every time. *)
let puzzlescript_boulders_run () =
  let open Puzzlescript in
  let g = PuzzleScriptBoulders.boulders in
  let moves = [ Down; Down; Down ] @ List.init 7 (fun _ -> Right) in
  let b = List.fold_left (fun b d -> turn g (Some d) b) (board g 0) moves in
  Alcotest.(check bool) "every diamond taken" true (won g b);
  Alcotest.(check bool) "and the boulder never fell" true (List.mem 'o' (at b 4 2))

(*****************************************************************************)
(* TinyBlockout *)
(*****************************************************************************)

(* Turning a solid is turning its bounding box, an integer trick with no
 * centre and no rounding (see the header of games3d/TinyBlockout.ml).
 * The proof that it really is a rotation: four quarter turns about any
 * axis put every cube back where it was, and no cube is ever lost. *)
let blockout_turns () =
  let open TinyBlockout in
  let sorted cs = List.sort compare cs in
  List.iter
    (fun (name, turn) ->
      List.iter
        (fun (p : piece) ->
          Alcotest.(check int) (name ^ ", every cube kept") (List.length p.cells) (List.length (turn p.cells));
          Alcotest.(check bool)
            (name ^ ", four quarter turns are none")
            true
            (sorted (turn (turn (turn (turn p.cells)))) = sorted p.cells))
        pieces)
    [ ("about x", turn_x); ("about y", turn_y); ("about z", turn_z) ]

(* A layer is a whole floor of the well, not a line: fill one and it
 * goes, and what was above it comes down a level. *)
let blockout_layer () =
  let open TinyBlockout in
  let stack = Array.make (cols * levels * rows) None in
  List.iter (fun (x, z) -> stack.(index x (levels - 1) z) <- Some red) floor_cells;
  stack.(index 2 (levels - 2) 2) <- Some blue;
  let s, gone = clear_layers stack in
  Alcotest.(check int) "one layer went" 1 gone;
  Alcotest.(check bool) "the cube above came down to the floor" true (s.(index 2 (levels - 1) 2) = Some blue);
  Alcotest.(check int) "and it is all that is left" 1 (Array.fold_left (fun n c -> if c = None then n else n + 1) 0 s);
  (* one hole is enough to keep a layer *)
  let stack = Array.make (cols * levels * rows) None in
  List.iter (fun (x, z) -> if (x, z) <> (0, 0) then stack.(index x (levels - 1) z) <- Some red) floor_cells;
  Alcotest.(check int) "a layer with a hole stays" 0 (snd (clear_layers stack))

(* The pit is cols x rows across, so a piece may not be turned or slid
 * through its wall: BlockOut refuses the move rather than nudging the
 * piece, and refusing means the game is unchanged. *)
let blockout_walls () =
  let open TinyBlockout in
  let g = start_game () in
  let long = { cells = [ (0, 0, 0); (1, 0, 0); (2, 0, 0); (3, 0, 0) ]; color = red } in
  let g = { g with piece = long; at = (0, 0, 0) } in
  Alcotest.(check bool) "a 4-long bar fits across a 5-wide pit" true (free g (world g));
  Alcotest.(check bool) "but not one cell further right" true (try_at g (2, 0, 0) long.cells = None);
  (* turned upright about z it is 4 tall, which the pit has room for *)
  let upright = turn_z long.cells in
  Alcotest.(check bool) "and it may stand up" true (try_at g (0, 0, 0) upright <> None)

(*****************************************************************************)
(* TinyTombRaider *)
(*****************************************************************************)

(* The tomb can be got out of, and this is the route: every move the
 * raider has, in order, from the entrance to the idol and back. It is
 * worth reading as the level's answer written down -- which is the
 * point of a game whose moves are fixed lengths (see the header of
 * games3d/TinyTombRaider.ml). Each move is asked for and must be
 * granted: a refusal here means the tomb cannot be finished. *)
let tomb_raider_route () =
  let open TinyTombRaider in
  let fwd g = attempt g `Forward and right g = attempt g `Right and left g = attempt g `Left in
  let hands g = attempt g `Hands and jump2 g = jump g 2 in
  let times n move = List.init n (fun _ -> move) in
  let route =
    List.concat
      [ times 12 fwd (* east down the entrance corridor *);
        [ right ] @ times 3 fwd (* south, into the chamber's door *);
        [ right; fwd ] (* west, up to the ledge *);
        [ fwd; fwd ] (* her hands catch it, then she pulls up *);
        [ fwd; fwd ] (* along the ledge, then down off its far end *);
        [ left; fwd; fwd ] (* south to the row the chasm can be crossed on *);
        [ right ] @ times 3 fwd (* west to its edge *);
        [ jump2 ] (* the running jump: two squares, over the chasm *);
        [ right; hands ] (* face the block and push it beside the plinth *);
        [ fwd; fwd ] (* grab the block, pull up onto it *);
        [ left; fwd; fwd ] (* from it, grab the plinth and pull up: the idol *);
        [ right; right; fwd ] (* about turn, down onto the block *);
        [ left; fwd ] (* off it, onto the floor *);
        [ right; jump2 ] (* the chasm again, eastwards *);
        times 5 fwd (* east across the chamber *);
        [ right; fwd; fwd ] (* up the ledge again *);
        [ left; fwd; fwd ] (* down it and out of the chamber *);
        [ left ] @ times 3 fwd (* north, up the corridor *);
        [ left ] @ times 12 fwd (* west, the whole way out *) ]
  in
  let g =
    List.fold_left
      (fun g move ->
        match move g with
        | Some d -> finish g d
        | None -> Alcotest.failf "the tomb refused a move at (%d, %d)" g.at.cx g.at.cz)
      (new_game ()) route
  in
  Alcotest.(check bool) "the idol is off its plinth" false g.idol;
  Alcotest.(check bool) "and she is out with it" true g.out;
  Alcotest.(check (option string)) "alive" None g.dead

(* The two jumps are the two lengths there are, and the chasm is exactly
 * wide enough to tell them apart: that is the whole of the design (see
 * the header). A standing jump into it lands in the spikes. *)
let tomb_raider_jumps () =
  let open TinyTombRaider in
  (* on the chamber floor, at the chasm's edge, looking across it *)
  let g = { (new_game ()) with at = { cx = 6; cz = 6; y = 0; facing = West; hanging = false } } in
  (match jump g 2 with
  | Some d ->
      let g = finish g d in
      Alcotest.(check (pair int int)) "the running jump crosses it" (4, 6) (g.at.cx, g.at.cz);
      Alcotest.(check (option string)) "and she lives" None g.dead
  | None -> Alcotest.fail "the running jump was refused");
  match jump g 1 with
  | Some d ->
      let g = finish g d in
      Alcotest.(check (pair int int)) "the standing jump falls short" (5, 6) (g.at.cx, g.at.cz);
      Alcotest.(check (option string)) "into the spikes" (Some "the spikes") g.dead
  | None -> Alcotest.fail "the standing jump was refused"

(*****************************************************************************)
(* TinyRobotron *)
(*****************************************************************************)

let robotron_enemy (kind : TinyRobotron.kind) (x : number) (y : number) : TinyRobotron.enemy = { kind; x; y; vx = 0.; vy = 0.; timer = 999 }

(* the two sticks are independent, which is the whole game: the arrows
 * right and a (shoot left) in the same frame move the man right and
 * send a shot the other way *)
let robotron_twin_stick () =
  let open TinyRobotron in
  let g = new_game () in
  let keyboard = { initial_computer.keyboard with kright = true; ka = true } in
  let g' = update_game (computer ~keyboard 1) g in
  Alcotest.(check bool) "he runs right" true (g'.mx > g.mx);
  match g'.shots with
  | [ s ] ->
      Alcotest.(check bool) "the shot flies left" true (s.vx < 0.);
      Alcotest.(check (float 1e-9)) "and straight" 0. s.vy
  | l -> Alcotest.failf "%d shots, expected 1" (List.length l)

(* A grunt has no idea where it is going: it walks at the man, and an
 * electrode on the way kills it. The hulk is the other way round --
 * nothing kills it, and it is the humans who die of it. *)
let robotron_walks_into_things () =
  let open TinyRobotron in
  let steps (n : int) (g : game) : game =
    let rec go g n = if n = 0 then g else go (walk_into_electrodes (step_enemies g)) (n - 1) in
    go g n
  in
  let g = { (new_game ()) with mx = 0.; my = 0.; enemies = [ robotron_enemy Grunt 300. 0.; robotron_enemy Electrode 200. 0. ]; humans = [] } in
  let g = steps 120 g in
  Alcotest.(check int) "the grunt died on the electrode" 1 (List.length g.enemies);
  Alcotest.(check bool) "which is still standing" true (List.for_all (fun (e : enemy) -> e.kind = Electrode) g.enemies);
  (* the hulk walks west over the family *)
  let g = { (new_game ()) with enemies = [ { (robotron_enemy Hulk 200. 0.) with vx = -1.7 } ]; humans = [ { hx = 0.; hy = 0.; hvx = 0.; hvy = 0. } ] } in
  let g = steps 120 g in
  Alcotest.(check int) "one of the family is gone" 0 (List.length g.humans);
  Alcotest.(check int) "and the hulk is still there" 1 (List.length g.enemies)

(* the brain reaches a human and rebuilds it: one human less, one prog
 * more, and the prog hunts the man faster than a grunt walks *)
let robotron_brain_rebuilds () =
  let open TinyRobotron in
  let g = { (new_game ()) with enemies = [ robotron_enemy Brain 200. 0. ]; humans = [ { hx = 0.; hy = 0.; hvx = 0.; hvy = 0. } ] } in
  let rec go g n = if n = 0 then g else go (step_enemies g) (n - 1) in
  let g = go g 200 in
  Alcotest.(check int) "the human is gone" 0 (List.length g.humans);
  Alcotest.(check bool) "a prog took its place" true (List.exists (fun (e : enemy) -> e.kind = Prog) g.enemies)

(* a robot player, shooting the nearest robot and backing away from it,
 * clears the first wave (8 grunts, no electrodes yet) in about 10
 * seconds; the humans it walks over on the way are worth 1000 and up *)
let robotron_robot () =
  let open TinyRobotron in
  let sign (d : number) : bool * bool = (d > 20., d < -20.) in
  let s = ref (new_game ()) and i = ref 0 in
  while !i < 60 * 60 && !s.wave = 1 do
    incr i;
    let g = !s in
    let nearest =
      List.sort (fun (a : enemy) (b : enemy) -> compare (Float.hypot (a.x -. g.mx) (a.y -. g.my)) (Float.hypot (b.x -. g.mx) (b.y -. g.my))) g.enemies
    in
    let keyboard =
      match nearest with
      | [] -> initial_computer.keyboard
      | e :: _ ->
          (* shoot at it, and run the other way (into the arena) *)
          let kd, ka = sign (e.x -. g.mx) and kw, ks = sign (e.y -. g.my) in
          let away (m : number) (o : number) : bool * bool = if Float.abs m > 380. then (m < 0., m > 0.) else (o < m, o > m) in
          let kright, kleft = away g.mx e.x and kup, kdown = away g.my e.y in
          { initial_computer.keyboard with kd; ka; kw; ks; kright; kleft; kup; kdown }
    in
    s := update_game (computer ~keyboard !i) !s
  done;
  Alcotest.(check int) "wave 2" 2 !s.wave;
  Alcotest.(check bool) "not once caught" true (!s.lives = 3)

(*****************************************************************************)
(* TinyPinball *)
(*****************************************************************************)

(* a game with the ball put somewhere, for a test that wants one *)
let pinball_at ?(engine = TinyPinball.Ours) ?(substeps = 4) (x : number) (y : number) (vx : number) (vy : number) : TinyPinball.game =
  let g = TinyPinball.new_game engine substeps in
  { g with ball = { x; y; vx; vy }; play = TinyPinball.Live }

let pinball_run (frames : int) ?(keyboard = fun (_ : int) -> initial_computer.keyboard) (g : TinyPinball.game) : TinyPinball.game =
  let s = ref g in
  for i = 1 to frames do
    s := TinyPinball.update_game (computer ~keyboard:(keyboard i) i) (Scene2d.start (TinyPinball.Playing !s)) !s
  done;
  !s

(* the highest the ball gets while the game runs *)
let pinball_apex (frames : int) ?(keyboard = fun (_ : int) -> initial_computer.keyboard) (g : TinyPinball.game) : number =
  let s = ref g and top = ref g.ball.y in
  for i = 1 to frames do
    s := TinyPinball.update_game (computer ~keyboard:(keyboard i) i) (Scene2d.start (TinyPinball.Playing !s)) !s;
    top := Float.max !top !s.ball.y
  done;
  !top

(* The flipper does not bat the ball, it carries it: the same ball
 * falling on the same spot of the left flipper barely comes back up if
 * the flipper stays down, and is thrown the height of the table if it
 * is swung. *)
let pinball_flipper_throws () =
  let drop () = pinball_at (-60.) (-250.) 0. (-400.) in
  let resting = pinball_apex 40 (drop ()) in
  let flipped = pinball_apex 40 ~keyboard:(fun i -> { initial_computer.keyboard with kleft = i > 12 }) (drop ()) in
  Alcotest.(check bool) "the flipper down, it stays at the bottom" true (resting < -150.);
  Alcotest.(check bool) "swung, it is thrown up the table" true (flipped > 100.)

(* Both engines play the same table: a ball rolled into the left wall
 * comes back with the same part of its speed (the restitution is the
 * table's, not the engine's). They are not the same code -- ours
 * reflects a velocity about a normal, the engine resolves an impulse
 * against a box 10 pixels thick -- so they are compared on the physics,
 * not pixel by pixel. *)
let pinball_both_engines () =
  let open TinyPinball in
  let into_the_wall (engine : engine) = (pinball_run 20 (pinball_at ~engine (-100.) 0. (-800.) 0.)).ball in
  let ours = into_the_wall Ours and theirs = into_the_wall Engine in
  Alcotest.(check bool) "ours comes back off the wall" true (ours.vx > 100.);
  Alcotest.(check bool) "so does the engine's" true (theirs.vx > 100.);
  Alcotest.(check bool) "at the same speed, give or take" true (Float.abs (ours.vx -. theirs.vx) < 80.);
  Alcotest.(check bool) "and from the same place, give or take a ball" true (Float.abs (ours.x -. theirs.x) < 30.)

(* The pinball's lesson: at 1/60 s a fast ball jumps clean over a wall
 * without ever overlapping it. A flipper throws the ball at about 3000
 * pixels a second, i.e. 50 a frame, four times its radius; dropped at
 * 4000 on the flipper it goes straight through with substeps=1, and
 * bounces off it with the four steps of the default. *)
let pinball_tunnels () =
  let open TinyPinball in
  let drop (substeps : int) = (pinball_run 30 (pinball_at ~substeps (-60.) (-100.) 0. (-4000.))).ball.y in
  Alcotest.(check bool) "substeps=1: gone through the flipper" true (drop 1 < drain_y);
  Alcotest.(check bool) "substeps=4: still on the table" true (drop 4 > drain_y)

(* A game played by a robot that flips whenever the ball is low: the
 * ball bounces off the table's things and scores, and -- the thing
 * worth checking every time the table or the physics changes -- it
 * never leaves the cabinet. *)
let pinball_stays_on_the_table () =
  let open TinyPinball in
  let g = ref (new_game Ours 4) and escaped = ref None and i = ref 0 in
  while !i < 1800 do
    incr i;
    let b = !g.ball in
    let keyboard =
      { initial_computer.keyboard with kspace = !i < 50; kleft = b.y < -250. && b.x < 0.; kright = b.y < -250. && b.x > 0. }
    in
    g := update_game (computer ~keyboard !i) (Scene2d.start (Playing !g)) !g;
    let b = !g.ball in
    if !escaped = None && (b.x < -260. || b.x > 320. || b.y > 500.) then escaped := Some (b.x, b.y)
  done;
  (match !escaped with Some (x, y) -> Alcotest.failf "the ball left the table at (%.0f, %.0f)" x y | None -> ());
  Alcotest.(check bool) "it hit things on the way" true (!g.score > 0)

(* three balls, and the game is over *)
let pinball_three_balls () =
  let open TinyPinball in
  let drained (g : game) = pinball_run 140 { g with ball = { x = 0.; y = drain_y -. 10.; vx = 0.; vy = -100. }; play = Live } in
  let g = drained (new_game Ours 4) in
  Alcotest.(check int) "the second ball" 2 g.balls;
  let g = drained (drained g) in
  Alcotest.(check int) "none left" 0 g.balls

(*****************************************************************************)
(* TinyBoomerangFu3d *)
(*****************************************************************************)

(* The one rule the whole game hangs on: a thrown boomerang comes back
 * to where its owner *is*, not to where the throw started. So: throw
 * north, then walk east the whole time it is away, and it still finds
 * you. *)
let boomerang_returns () =
  let open TinyBoomerangFu3d in
  let me = List.hd (new_game ()).players in
  let me, thrown = step_player { go = Some (0., -1.); throw = true; dash_now = false } me in
  let r = match thrown with Some r -> r | None -> Alcotest.fail "nothing left the hand" in
  Alcotest.(check bool) "the hand is empty" false me.holds;
  let from_x = me.px in
  let rec fly n me rangs =
    if n = 0 then (me, rangs)
    else
      let me, _ = step_player { go = Some (1., 0.); throw = false; dash_now = false } me in
      let rangs, caught = step_rangs [ me ] rangs in
      if List.mem me.idx caught then ({ me with holds = true }, rangs) else fly (n - 1) me rangs
  in
  let me, rangs = fly 200 me [ r ] in
  Alcotest.(check bool) "it is caught again" true me.holds;
  Alcotest.(check int) "and nothing is left in the air" 0 (List.length rangs);
  Alcotest.(check bool) "having chased a thrower who kept moving" true (me.px -. from_x > 5.)

(* Who a flight cuts, which is three rules in one function ([cuts]):
 * everyone on the way out, nobody but its owner's enemies on the way
 * back, and its own thrower only once it has both got away and come off
 * something. *)
let boomerang_cuts () =
  let open TinyBoomerangFu3d in
  let g = new_game () in
  let me = List.nth g.players 0 and you = List.nth g.players 1 in
  let flight = { rx = 0.; rz = 0.; rvx = 0.5; rvz = 0.; owner = me.idx; leg = Out; bounced = false; away = false; age = 10 } in
  let over (p : player) (r : rang) = { r with rx = p.px; rz = p.pz } in
  let n rangs players = List.length (cuts players rangs) in
  Alcotest.(check int) "a throw does not cut the one who threw it" 0 (n [ over me flight ] [ me ]);
  Alcotest.(check int) "unless it has got away and come off the fence" 1
    (n [ over me { flight with bounced = true; away = true } ] [ me ]);
  Alcotest.(check int) "the way back is a catch, never a cut" 0
    (n [ over me { flight with leg = Back; bounced = true; away = true } ] [ me ]);
  Alcotest.(check int) "but it cuts anyone else, either way round" 2
    (n [ over you flight; over you { flight with leg = Back } ] [ me; you ])

(* The other half of the trade: holding the boomerang, the dash is a
 * slash; without it, the same dash is only a dodge. *)
let boomerang_slash () =
  let open TinyBoomerangFu3d in
  let g = new_game () in
  let me = { (List.nth g.players 0) with px = 0.; pz = 0.; dash = 5 } in
  let you = { (List.nth g.players 1) with px = 0.9; pz = 0. } in
  Alcotest.(check int) "a dash with it in hand cuts" 1 (List.length (cuts [ me; you ] []));
  Alcotest.(check int) "the same dash, unarmed, does not" 0 (List.length (cuts [ { me with holds = false }; you ] []));
  Alcotest.(check int) "and it has no reach" 0 (List.length (cuts [ me; { you with px = 3.5 } ] []));
  Alcotest.(check int) "standing still with it in hand does not either" 0
    (List.length (cuts [ { me with dash = 0 }; you ] []))

(* The pits swallow whoever walks into one -- and the computer, which
 * looks where it is going ([way_ok]), must not: a round played out with
 * nobody at the keyboard is decided by boomerangs, and nobody falls. *)
let boomerang_pits () =
  let open TinyBoomerangFu3d in
  let me = { (List.hd (new_game ()).players) with px = 5.; pz = 0. } in
  let rec walk n p =
    if n = 0 || not (alive p) then p else walk (n - 1) (fst (step_player { go = Some (1., 0.); throw = false; dash_now = false } p))
  in
  Alcotest.(check bool) "walk into a pit and you fall" true (match (walk 40 me).state with Falling _ -> true | _ -> false);
  let g = ref (new_game ()) in
  for _ = 1 to 1800 do
    g := step_game initial_model initial_computer.keyboard !g
  done;
  Alcotest.(check bool) "the round was decided" true (!g.ended <> None);
  Alcotest.(check int) "and the computer kept out of the pits" 0
    (List.length (List.filter (fun p -> match p.state with Falling _ -> true | _ -> false) !g.players))

(*****************************************************************************)
(* TinyPortal2D *)
(*****************************************************************************)

let portal_run (frames : int) ?(keyboard = fun (_ : int) -> initial_computer.keyboard) (g : TinyPortal2D.game) : TinyPortal2D.game =
  let s = ref g in
  for i = 1 to frames do
    s := TinyPortal2D.update_game (computer ~keyboard:(keyboard i) i) (Scene2d.start (TinyPortal2D.In_game !s)) !s
  done;
  !s

(* aim the gun from the player at a point, as the mouse does *)
let portal_shot (g : TinyPortal2D.game) ((x, y) : number * number) : TinyPortal2D.portal =
  match TinyPortal2D.shoot g.map (g.player.x, g.player.y) (x -. g.player.x, y -. g.player.y) with
  | Some p -> p
  | None -> Alcotest.failf "nothing to put a portal on towards (%.0f, %.0f)" x y

(* The transform, on its own: what goes in comes out as fast, turned by
 * the angle between the two portals. Two floor portals are a half turn
 * (fall in, come up); a floor and a wall are a quarter (fall in, fly
 * sideways), which is the fling. *)
let portal_transform () =
  let open TinyPortal2D in
  let floor_a = { px = 0.; py = 0.; nx = 0.; ny = 1.; cell = (0, 0) } in
  let floor_b = { px = 300.; py = 0.; nx = 0.; ny = 1.; cell = (1, 0) } in
  let wall_b = { px = 300.; py = 0.; nx = -1.; ny = 0.; cell = (2, 0) } in
  let falling = Physics.body (circle white 10.) |> Physics.at 0. 10. |> Physics.moving 0. (-900.) in
  let up = go_through floor_a floor_b falling in
  Alcotest.(check (float 1e-6)) "it comes out of the floor going up" 900. (Float.round up.vy);
  Alcotest.(check (float 1e-6)) "and not sideways" 0. (Float.round up.vx);
  let sideways = go_through floor_a wall_b falling in
  Alcotest.(check (float 1e-6)) "out of the wall, it flies along it" (-900.) (Float.round sideways.vx);
  Alcotest.(check (float 1e-6)) "as fast as it fell, no more" 900. (Float.round (Float.hypot sideways.vx sideways.vy))

(* The gun: portals stick to the white walls only, on the face the shot
 * came in by. *)
let portal_gun () =
  let open TinyPortal2D in
  let g = load 0 in
  let left = portal_shot g (-500., g.player.y) in
  Alcotest.(check (float 1e-9)) "the left wall's face points right" 1. left.nx;
  let right = portal_shot g (500., g.player.y) in
  Alcotest.(check (float 1e-9)) "the right wall's face points left" (-1.) right.nx;
  Alcotest.(check bool) "and the dark ceiling takes none" true (shoot g.map (g.player.x, g.player.y) (0., 1.) = None)

(* Chamber 1: a portal on each side wall, walk into one, come out of
 * the other, and the goo is behind you. *)
let portal_chamber1 () =
  let open TinyPortal2D in
  let g = load 0 in
  let g = { g with blue = Some (portal_shot g (-500., g.player.y)); orange = Some (portal_shot g (500., g.player.y)) } in
  let g = portal_run 60 ~keyboard:(fun _ -> { initial_computer.keyboard with kleft = true }) g in
  Alcotest.(check bool) "through, and on the other side" true (g.player.x > 100.);
  let g = portal_run 150 ~keyboard:(fun _ -> { initial_computer.keyboard with kleft = true }) g in
  Alcotest.(check bool) "at the exit" true (match g.play with Won _ -> true | _ -> false)

(* [portal_play until control g]: the game driven a frame at a time by
 * a player that looks at where it is, up to [until] frames or until it
 * has nothing left to do *)
let portal_play (until : int) (control : TinyPortal2D.game -> keyboard option) (g : TinyPortal2D.game) : TinyPortal2D.game =
  let s = ref g and i = ref 0 and stop = ref false in
  (* the scenes carried from frame to frame, not made anew each time:
     the game asks Scene2d whether a key *went* down (picking the cube
     up), which needs the frame before *)
  let scenes = ref (Scene2d.start (TinyPortal2D.In_game g)) in
  while (not !stop) && !i < until do
    incr i;
    match control !s with
    | None -> stop := true
    | Some keyboard ->
        let c = computer ~keyboard !i in
        scenes := Scene2d.update c !scenes;
        s := TinyPortal2D.update_game c !scenes !s
  done;
  !s

let portal_keys ?(left = false) ?(right = false) ?(grab = false) () : keyboard =
  let k = { initial_computer.keyboard with kleft = left; kright = right } in
  if grab then { k with keys = Set_.add "e" k.keys } else k

(* Chamber 2, the fling: from the edge of the platform the white floor
 * below and to the right is in plain view; two holes in it, then step
 * off, fall the height of the room into one and come out of the other
 * going up just as fast -- to a ledge no jump reaches. *)
let portal_fling () =
  let open TinyPortal2D in
  let g = load 1 in
  (* to the right edge of the platform *)
  let g = portal_play 120 (fun g -> if g.player.x > 40. then None else Some (portal_keys ~right:true ())) g in
  let g = { g with blue = Some (portal_shot g (225., -240.)); orange = Some (portal_shot g (300., -240.)) } in
  (match (g.blue, g.orange) with
  | Some b, Some o ->
      Alcotest.(check bool) "both holes are in the floor" true (b.ny = 1. && o.ny = 1.);
      Alcotest.(check bool) "and they are two different tiles" true (b.cell <> o.cell)
  | _ -> Alcotest.fail "no portals");
  (* up to about the height he fell from: the platform he left is at 97 *)
  let fell = portal_play 150 (fun g -> if g.player.y > 60. then None else Some (portal_keys ~right:true ())) g in
  Alcotest.(check bool) "flung back up to the height he fell from" true (fell.player.y > 60.);
  (* over the ledge, then along it to the way out *)
  let exit_x = match Tilemap.find g.map 'E' with (col, row) :: _ -> fst (Tilemap.center g.map col row) | [] -> 0. in
  let won =
    portal_play 300
      (* steering at the top, not on the way up: coming out of the hole
         he goes straight up, and pressing right too early walks him
         into the underside of the platform he is aiming for *)
      (fun g ->
        match g.play with
        | Won _ -> None
        | _ -> Some (portal_keys ~right:(g.player.vy <= 0. && g.player.x < exit_x) ~left:(g.player.vy <= 0. && g.player.x > exit_x +. 10.) ()))
      fell
  in
  Alcotest.(check bool) "and steered onto the ledge with the way out" true (match won.play with Won _ -> true | _ -> false)

(* Chamber 3: the cube fetched through the two white floors, and put on
 * the button, which opens the door. The player falls in one hole and
 * comes up out of the other, so walking is what gets him out of it. *)
let portal_cube () =
  let open TinyPortal2D in
  let g = load 2 in
  let g = { g with blue = Some (portal_shot g (-350., -160.)); orange = Some { px = 225.; py = -100.; nx = 0.; ny = 1.; cell = (14, 6) } } in
  (* left into the hole, and out in the other room; then away from it,
     towards the cube *)
  (* right, into the hole he shot at his feet, and up out of the one in
     the other room *)
  (* he lands first, then walks left into the hole at his feet *)
  let g = portal_play 40 (fun _ -> Some (portal_keys ())) g in
  let g = portal_play 120 (fun g -> if g.player.x > 150. then None else Some (portal_keys ~left:true ())) g in
  Alcotest.(check bool) "in the room with the cube" true (g.player.x > 150.);
  let cube_x (g : game) = match g.cube with Some c -> c.x | None -> 0. in
  let g = portal_play 150 (fun g -> if g.held then None else Some (portal_keys ~left:(g.player.x > cube_x g +. 20.) ~grab:(Float.abs (g.player.x -. cube_x g) < 50.) ())) g in
  Alcotest.(check bool) "carrying it" true g.held;
  (* back east into the hole in this room, up out of the one in the
     other, and west to the button *)
  let g = portal_play 200 (fun g -> if g.player.x < 0. then None else Some (portal_keys ~right:true ())) g in
  Alcotest.(check bool) "back in the first room, with the cube" true (g.player.x < 0. && g.held);
  let button_x = match Tilemap.find g.map 'B' with (col, row) :: _ -> fst (Tilemap.center g.map col row) | [] -> 0. in
  (* the cube is carried in front of him, so what has to be over the
     button is the cube, not the player *)
  let g =
    portal_play 250
      (fun g ->
        if (not g.held) && g.door_open then None
        else Some (portal_keys ~left:(cube_x g > button_x +. 10.) ~right:(cube_x g < button_x -. 10.) ~grab:(Float.abs (cube_x g -. button_x) < 12.) ()))
      g
  in
  Alcotest.(check bool) "the cube is on the button, the door is open" true g.door_open

(*****************************************************************************)
(* TinyGauntlet2 *)
(*****************************************************************************)

(* does [s] contain [needle]? (for the voice's lines) *)
let contains (needle : string) (s : string) : bool =
  let n = String.length needle and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  go 0

let gauntlet_game ?(field = false) ?(level = 0) () : TinyGauntlet2.game =
  TinyGauntlet2.load (List.nth TinyGauntlet2.heroes 0) field level (0, 0, 0)

let gauntlet_play (frames : int) ?(keyboard = fun (_ : int) -> initial_computer.keyboard) (g : TinyGauntlet2.game) : TinyGauntlet2.game =
  let s = ref g and scenes = ref (Scene2d.start (TinyGauntlet2.Playing g)) in
  for i = 1 to frames do
    let c = computer ~keyboard:(keyboard i) i in
    scenes := Scene2d.update c !scenes;
    s := TinyGauntlet2.update_game c !scenes !s
  done;
  !s

(* The generator is the game: left alone for ten seconds the room
 * fills, and the only thing that stops it is shooting the tile
 * itself -- killing what has come out changes nothing. *)
let gauntlet_generators () =
  let open TinyGauntlet2 in
  let g = gauntlet_play 600 (gauntlet_game ()) in
  Alcotest.(check bool) "the room fills by itself" true (List.length g.monsters >= 4);
  (* the same, with the generators taken out at the start *)
  let quiet = gauntlet_play 600 { (gauntlet_game ()) with gens = [] } in
  Alcotest.(check int) "with the taps shut, nothing comes" 0 (List.length quiet.monsters)

(* Health is the clock: it goes down by itself, ten points a second,
 * whatever the hero does. *)
let gauntlet_health_is_the_clock () =
  let open TinyGauntlet2 in
  let g = { (gauntlet_game ()) with gens = [] } in
  let after = gauntlet_play 60 g in
  Alcotest.(check int) "a second costs ten points" 690 after.health;
  (* and food buys it back *)
  let fed = gauntlet_play 60 { g with x = fst (center_of g.map (List.hd (Tilemap.find g.map 'F'))); y = snd (center_of g.map (List.hd (Tilemap.find g.map 'F'))) } in
  Alcotest.(check bool) "food is worth more than the second it takes" true (fed.health > after.health)

(* "Elf shot the food!": the hero's own shot destroys the thing
 * keeping him alive, and the voice says so. *)
let gauntlet_shot_the_food () =
  let open TinyGauntlet2 in
  let g = { (gauntlet_game ()) with gens = [] } in
  let fx, fy = center_of g.map (List.hd (Tilemap.find g.map 'F')) in
  (* stand to the right of the food, facing it, and fire *)
  let g = { g with x = fx +. 120.; y = fy; facing = (-1., 0.) } in
  let loaves (g : game) = List.length (Tilemap.find g.map 'F') in
  let before = loaves g in
  let g = gauntlet_play 30 ~keyboard:(fun i -> { initial_computer.keyboard with kspace = i = 1 }) g in
  Alcotest.(check int) "one loaf less" (before - 1) (loaves g);
  let said = match g.says with Some (what, _) -> what | None -> "" in
  Alcotest.(check bool) "and the voice says who did it" true (contains "SHOT THE FOOD" said)

(* The two chases, on the layout that tells them apart: a pen whose
 * only opening faces *away* from the hero. Walking towards him is
 * walking into its back wall, so the arcade's greedy monsters stay
 * there for ever, while one Dijkstra from the hero sends them out the
 * other side and round. This is the exact shape where a flow field
 * earns its search -- on an open floor, or round a single pillar, the
 * greedy walk gets there too. The numbers are in the game's header. *)
let gauntlet_two_chases () =
  let open TinyGauntlet2 in
  let pen = [ (7, 14); (7, 15); (7, 16); (8, 14); (8, 16) ] in
  let stats (field : bool) =
    let g = { (gauntlet_game ~field ()) with gens = [] } in
    let map = List.fold_left (fun m (col, row) -> Tilemap.set m col row '#') g.map pen in
    let hx, hy = center_of map (3, 15) in
    let mx, my = center_of map (8, 15) in
    let g =
      { g with map; x = hx; y = hy; monsters = List.init 3 (fun i -> { kind = Grunt; mx; my = my +. (float_of_int i *. 2.); life = 3; cool = 60 }) }
    in
    let g = gauntlet_play 900 g in
    let ds = List.map (fun (m : monster) -> Float.hypot (m.mx -. g.x) (m.my -. g.y)) g.monsters in
    (List.length (List.filter (fun d -> d < 80.) ds), List.fold_left ( +. ) 0. ds /. float_of_int (max 1 (List.length ds)))
  in
  let dumb_there, dumb_mean = stats false and smart_there, smart_mean = stats true in
  Alcotest.(check int) "the greedy monsters are still in the pen" 0 dumb_there;
  Alcotest.(check bool) "the field brings them out and round" true (smart_there > 0);
  Alcotest.(check bool) "and much closer" true (smart_mean < dumb_mean /. 2.)

(* The dungeon is bigger than the screen, so the view scrolls: walking
 * east for two seconds takes the camera east too, and it never shows
 * anything outside the level. (Written after the camera spent a
 * commit pinned to a corner, because Camera2d.follow takes its
 * fraction first and it was being handed the hero's x.) *)
let gauntlet_scrolls () =
  let open TinyGauntlet2 in
  let g = { (gauntlet_game ()) with gens = [] } in
  let start_cam = g.cam.x and start_hero = g.x in
  let g = gauntlet_play 120 ~keyboard:(fun _ -> { initial_computer.keyboard with kright = true }) g in
  (* 198 pixels: he walks until the wall of the first room stops him *)
  Alcotest.(check bool) "the hero went east" true (g.x > start_hero +. 150.);
  Alcotest.(check bool) "and the camera followed him" true (g.cam.x > start_cam +. 60.);
  Alcotest.(check bool) "without leaving the dungeon" true
    (let b = Tilemap.bounds g.map in
     let half = 1000. /. (2. *. zoom) in
     g.cam.x >= b.left +. half -. 1. && g.cam.x <= b.right -. half +. 1.)

(* A robot with the map walks the dungeon: it takes the key, opens the
 * door and finds the way down. It is the level's own test -- a
 * dungeon whose exit cannot be reached is not a dungeon. *)
let gauntlet_robot_escapes () =
  let open TinyGauntlet2 in
  let g = { (gauntlet_game ()) with gens = []; health = 9999 } in
  let walkable (col, row) = match Tilemap.get g.map col row with Some c -> not (c = '#' || c = 'b') | None -> false in
  let goal (g : game) =
    match (Tilemap.find g.map 'K', Tilemap.find g.map 'X') with
    | k :: _, _ when g.keys = 0 -> k
    | _, x :: _ -> x
    | _ -> cell_of g.map g.x g.y
  in
  let s = ref g and scenes = ref (Scene2d.start (Playing g)) and out = ref false and i = ref 0 in
  while (not !out) && !i < 3000 do
    incr i;
    let g = !s in
    let here = cell_of g.map g.x g.y in
    let keyboard =
      match Orders.path ~walkable ~from:here (goal g) with
      | _ :: next :: _ ->
          let tx, ty = center_of g.map next in
          { initial_computer.keyboard with kright = tx > g.x +. 4.; kleft = tx < g.x -. 4.; kup = ty > g.y +. 4.; kdown = ty < g.y -. 4. }
      | _ -> initial_computer.keyboard
    in
    let c = computer ~keyboard !i in
    scenes := Scene2d.update c !scenes;
    s := update_game c !scenes !s;
    if escaped !s then out := true
  done;
  Alcotest.(check bool) "the robot found the way down" true !out;
  Alcotest.(check bool) "having opened the door with the key it picked up" true (!s.keys = 0 && !i > 60)

(*****************************************************************************)
(* TinyKickOff2 *)
(*****************************************************************************)

let kickoff_play (frames : int) ?(keyboard = fun (_ : int) -> initial_computer.keyboard) (g : TinyKickOff2.game) : TinyKickOff2.game =
  let s = ref g in
  for i = 1 to frames do
    s := TinyKickOff2.update_game (computer ~keyboard:(keyboard i) i) !s
  done;
  !s

(* the game with the whistle already gone and the ball at a player's
 * feet, ready to dribble *)
let kickoff_dribbling ?(glued = false) () : TinyKickOff2.game =
  let open TinyKickOff2 in
  (* one player and the ball, and nobody to take it off him: what is
     being measured is the ball, not the other side *)
  let g = { (new_game glued) with kickoff = 0 } in
  let me = { (List.nth g.players g.mine) with px = 0.; py = -200.; dir = (0., 1.) } in
  { g with players = [ me ]; mine = 0; ball = Free_ball.still 0. (-200. +. 22.) }

(* The one idea: dribbling up the pitch, a free ball runs ahead of the
 * player and has to be caught up, where a glued one is his feet. The
 * numbers are in the game's header. *)
let kickoff_free_ball () =
  let open TinyKickOff2 in
  let up _ = { initial_computer.keyboard with kup = true } in
  let gap (glued : bool) =
    let s = ref (kickoff_dribbling ~glued ()) and worst = ref 0. and touches = ref 0 in
    for i = 1 to 180 do
      let before = (List.nth !s.players !s.mine).touch in
      s := update_game (computer ~keyboard:(up i) i) !s;
      let me = List.nth !s.players !s.mine in
      if me.touch > before then incr touches;
      worst := Float.max !worst (Float.hypot (!s.ball.x -. me.px) (!s.ball.y -. me.py))
    done;
    (!worst, !touches)
  in
  let free_gap, free_touches = gap false and glued_gap, _ = gap true in
  (* 22: the two radii, which is what "at his feet" means here *)
  Alcotest.(check bool) "glued, the ball is his feet" true (glued_gap < 24.);
  Alcotest.(check bool) "free, it runs away from him" true (free_gap > 40.);
  Alcotest.(check bool) "but not so far that he cannot catch it" true (free_gap < 120.);
  Alcotest.(check bool) "and he has to touch it again and again" true (free_touches >= 3)

(* The aftertouch: the same shot, bent by holding a direction while it
 * is in the air, finishes somewhere else entirely. *)
let kickoff_aftertouch () =
  let open TinyKickOff2 in
  let shot (bend : bool) =
    let g = { (kickoff_dribbling ()) with power = 1. } in
    (* let go of the kick at the first frame, then hold right or nothing *)
    (* 40 frames, while it is still on the pitch: once it is in the
       goal the referee puts it on the centre spot and both shots
       measure the same nothing *)
    let g = kickoff_play 40 ~keyboard:(fun i -> if i = 1 then initial_computer.keyboard else { initial_computer.keyboard with kright = bend }) g in
    g.ball.x
  in
  let straight = shot false and bent = shot true in
  Alcotest.(check bool) "the bent ball ends up well to the side" true (bent -. straight > 80.)

(* Through the posts is a goal, and the game starts again in the middle *)
let kickoff_goal () =
  let open TinyKickOff2 in
  (* an empty net: with a keeper on his line this is a save, which is
     the keeper's test, not the referee's. One player is kept, far
     away, because the game always has someone to run *)
  let g = { (new_game false) with kickoff = 0 } in
  let g = { g with players = [ { (List.nth g.players 4) with px = 0.; py = -400. } ]; mine = 0 } in
  let g = { g with ball = { (Free_ball.still 0. (half_h -. 30.)) with vy = 9. }; last = Some South } in
  let g = kickoff_play 20 g in
  Alcotest.(check int) "one nil" 1 g.south;
  Alcotest.(check bool) "and the ball is back on the centre spot" true (Float.hypot g.ball.x g.ball.y < 2.)

(* Out at the side is a throw-in, to the other team, and the ball
 * comes back on the pitch *)
let kickoff_throw_in () =
  let open TinyKickOff2 in
  let g = { (new_game false) with kickoff = 0 } in
  let g = { g with ball = { (Free_ball.still (half_w -. 10.) 0.) with vx = 9. }; last = Some South } in
  let g = kickoff_play 20 g in
  Alcotest.(check bool) "the ball is on the pitch again" true (Float.abs g.ball.x < half_w);
  Alcotest.(check bool) "and it is theirs" true (g.last = Some North)

(* The shape of a team: nobody is told the plan, but when the ball
 * goes up the pitch the whole side goes with it. *)
let kickoff_formation_slides () =
  let open TinyKickOff2 in
  let outfield (g : game) =
    let them = List.filter (fun (p : player) -> p.side = South && not (keeper p)) g.players in
    List.fold_left ( +. ) 0. (List.map (fun (p : player) -> p.py) them) /. float_of_int (List.length them)
  in
  let g = { (new_game false) with kickoff = 0 } in
  let before = outfield g in
  let g = { g with ball = Free_ball.still 0. (half_h -. 120.) } in
  let g = kickoff_play 120 g in
  let after = outfield g in
  Alcotest.(check bool) "the side moved up with the ball" true (after > before +. 80.)

(*****************************************************************************)
(* TinySpeedball2 *)
(*****************************************************************************)

let speedball_play (frames : int) ?(keyboard = fun (_ : int) -> initial_computer.keyboard) (g : TinySpeedball2.game) : TinySpeedball2.game =
  let s = ref g in
  for i = 1 to frames do
    s := TinySpeedball2.update_game (computer ~keyboard:(keyboard i) i) !s
  done;
  !s

(* a game already under way, with nobody on the metal but the ball *)
let speedball_empty () : TinySpeedball2.game =
  let open TinySpeedball2 in
  let g = { (new_game ()) with restarting = 0 } in
  { g with players = [ { (List.nth g.players 4) with px = 0.; py = -600. } ]; mine = 0 }

(* The arena scores: a ball sent into a dome comes back off it, and the
 * side that touched it last is paid for the hit. A pinball table's
 * bumper, in a game about goals. *)
let speedball_arena_pays () =
  let open TinySpeedball2 in
  let dome = List.find (fun (f : fixture) -> f.what = Dome) (speedball_empty ()).fixtures in
  let g = speedball_empty () in
  let g = { g with ball = { (Free_ball.still dome.fx (dome.fy -. 120.)) with vy = 7. }; last = Some Red } in
  let after = speedball_play 30 g in
  Alcotest.(check bool) "the hit scored" true (after.red > 0);
  Alcotest.(check bool) "and the ball came back the way it went in" true (after.ball.vy < 0.)

(* The x2 plate doubles what its side scores while it is lit: the same
 * dome, hit twice, is worth twice as much the second time. *)
let speedball_multiplier () =
  let open TinySpeedball2 in
  let dome = List.find (fun (f : fixture) -> f.what = Dome) (speedball_empty ()).fixtures in
  let hit (double : bool) =
    let g = speedball_empty () in
    let g = if double then { g with double = [ (Red, 600) ] } else g in
    let g = { g with ball = { (Free_ball.still dome.fx (dome.fy -. 120.)) with vy = 7. }; last = Some Red } in
    (speedball_play 30 g).red
  in
  let plain = hit false and doubled = hit true in
  Alcotest.(check int) "the plate doubles it" (plain * 2) doubled

(* No out of play: the walls give the ball back, and it keeps four
 * fifths of its speed. This is why the game never stops. *)
let speedball_walls () =
  let open TinySpeedball2 in
  let g = speedball_empty () in
  (* at y = 120, a lane with no furniture in it: the plates sit at y = 0
     and would send the ball back themselves *)
  let g = { g with ball = { (Free_ball.still (half_w -. 40.) 120.) with vx = 10. } } in
  let after = speedball_play 20 g in
  Alcotest.(check bool) "it is still in the arena" true (Float.abs after.ball.x < half_w);
  Alcotest.(check bool) "coming back" true (after.ball.vx < 0.);
  Alcotest.(check bool) "a fifth slower" true (Float.abs after.ball.vx < 9. && Float.abs after.ball.vx > 5.)

(* The difference with the football: run near the ball and you have
 * it, with no button pressed and no chasing it. It then travels with
 * you rather than rolling away, which is what "carried" means. *)
let speedball_carries () =
  let open TinySpeedball2 in
  let g = speedball_empty () in
  let me = List.nth g.players 0 in
  (* the ball a little ahead of him, and he walks north into it *)
  let g = { g with ball = Free_ball.still me.px (me.py +. 60.) } in
  let g = speedball_play 40 ~keyboard:(fun _ -> { initial_computer.keyboard with kup = true }) g in
  Alcotest.(check bool) "he picked it up by running into it" true (g.carrier = Some 0);
  let me = List.nth g.players 0 in
  Alcotest.(check bool) "and it is in his hands, not running away" true (Free_ball.near 30. (me.px, me.py) g.ball);
  (* running on, the ball stays with him *)
  let far = speedball_play 60 ~keyboard:(fun _ -> { initial_computer.keyboard with kup = true }) g in
  let me = List.nth far.players 0 in
  Alcotest.(check bool) "still his, fifty pixels later" true (far.carrier = Some 0 && Free_ball.near 30. (me.px, me.py) far.ball)

(* The view eases after the ball instead of being nailed to it. The
 * ball jumps -- into a carrier's hands, back to the centre spot after
 * a goal -- and a camera that copies the jump makes the arena lurch,
 * which is what it did when the camera was computed from the ball in
 * the view. One frame moves it a tenth of the way; forty get it
 * there. *)
let speedball_camera_is_smooth () =
  let open TinySpeedball2 in
  let g = speedball_empty () in
  (* the ball suddenly two hundred pixels away, as a goal or a catch
     moves it -- and well inside the arena, since near a wall the
     camera is clamped and *should* stop short of the ball *)
  let g = { g with ball = Free_ball.still 0. 200. } in
  let gap (g : game) = Float.abs (g.cam.y -. g.ball.y) in
  let before = gap g in
  let one = speedball_play 1 g in
  Alcotest.(check bool) "one frame does not jump the whole way" true (gap one > before /. 2.);
  Alcotest.(check bool) "but it does move" true (gap one < before);
  let later = speedball_play 60 g in
  Alcotest.(check bool) "and a second later it has caught up" true (gap later < 30.)

(* Violence is a move: space with no ball puts the nearest opponent on
 * the floor, pays ten for it, and takes the ball off him. *)
let speedball_tackle () =
  let open TinySpeedball2 in
  let g = { (new_game ()) with restarting = 0 } in
  let me = { (List.nth g.players 4) with px = 0.; py = 0. } in
  let victim = { (List.nth g.players 9) with px = 20.; py = 0. } in
  let g = { g with players = [ me; victim ]; mine = 0; ball = Free_ball.still 0. 500. } in
  (* and he is the one carrying the ball *)
  let g = { g with carrier = Some 1; ball = Free_ball.still victim.px victim.py } in
  let after = speedball_play 3 ~keyboard:(fun _ -> { initial_computer.keyboard with kspace = true }) g in
  Alcotest.(check bool) "he is on the floor" true ((List.nth after.players 1).down > 0);
  Alcotest.(check int) "and that is ten points" 10 after.red;
  Alcotest.(check bool) "the ball came out of his hands" true (after.carrier = None);
  Alcotest.(check bool) "and is loose, moving" true (Free_ball.speed after.ball > 1.)

(* A match plays itself. The player's man runs north the whole time and
 * does nothing else -- he has to be moving, because he is usually the
 * nearest to the ball and so the one holding it, and a man standing
 * still with the ball in his hands is a game that never restarts. *)
let speedball_plays_itself () =
  let open TinySpeedball2 in
  let g = speedball_play 1800 ~keyboard:(fun _ -> { initial_computer.keyboard with kup = true }) { (new_game ()) with restarting = 0 } in
  Alcotest.(check bool) "somebody scored something" true (g.red + g.blue >= 10);
  (* in play: in somebody's hands, or loose and moving. A carried ball
     has no speed of its own, which is the point of carrying it *)
  Alcotest.(check bool) "and the ball is in play" true (g.carrier <> None || Free_ball.speed g.ball > 0.5)

(* Through the mouth is ten, and the ball goes back to the middle --
 * the only thing that stops this game. *)
let speedball_goal () =
  let open TinySpeedball2 in
  let g = speedball_empty () in
  let g = { g with ball = { (Free_ball.still 0. (half_h -. 40.)) with vy = 9. }; last = Some Red } in
  let after = speedball_play 20 g in
  Alcotest.(check bool) "ten at least" true (after.red >= 10);
  Alcotest.(check bool) "and back to the middle" true (Float.hypot after.ball.x after.ball.y < 2.)

(*****************************************************************************)
(* TinySensibleSoccer *)
(*****************************************************************************)

let sensible_play (frames : int) ?(keyboard = fun (_ : int) -> initial_computer.keyboard) (g : TinySensibleSoccer.game) : TinySensibleSoccer.game =
  let s = ref g in
  for i = 1 to frames do
    s := TinySensibleSoccer.update_game (computer ~keyboard:(keyboard i) i) !s
  done;
  !s

(* one player, the ball at his feet, the whistle gone *)
let sensible_alone () : TinySensibleSoccer.game =
  let open TinySensibleSoccer in
  let g = { (new_game ()) with kickoff = 0 } in
  let me = { (List.nth g.players g.mine) with px = 0.; py = -300.; dir = (0., 1.) } in
  { g with players = [ me ]; mine = 0; ball = Free_ball.still 0. (-300. +. 20.) }

(* The third answer to the question the other two ask: the ball is not
 * his, but it never gets far. The number belongs next to
 * TinyKickOff2's 48 and the glued ball's 22. *)
let sensible_close_control () =
  let open TinySensibleSoccer in
  let s = ref (sensible_alone ()) and worst = ref 0. in
  for i = 1 to 180 do
    s := update_game (computer ~keyboard:{ initial_computer.keyboard with kup = true } i) !s;
    let me = List.nth !s.players 0 in
    worst := Float.max !worst (Float.hypot (!s.ball.x -. me.px) (!s.ball.y -. me.py))
  done;
  Printf.eprintf "DBG sensible: the ball gets %.0f ahead\n%!" !worst;
  Alcotest.(check bool) "further than his feet" true (!worst > 24.);
  Alcotest.(check bool) "but nothing like Kick Off's 48" true (!worst < 40.)

(* The ball has a height: hold the kick and it goes up, comes down, and
 * while it is above head height nobody can touch it. *)
let sensible_loft () =
  let open TinySensibleSoccer in
  let g = { (sensible_alone ()) with power = 1. } in
  (* let go of the kick at the first frame *)
  let up = sensible_play 20 g in
  Alcotest.(check bool) "it went up" true (up.z > 30.);
  Alcotest.(check bool) "over everybody's head" true (up.z > head_height);
  let later = sensible_play 120 g in
  Alcotest.(check bool) "and came back down" true (later.z < 30.)

(* A tap stays on the grass, where a held kick does not: the same
 * button, two passes. *)
let sensible_tap_stays_down () =
  let open TinySensibleSoccer in
  let tap = sensible_play 20 { (sensible_alone ()) with power = 0.2 } in
  Alcotest.(check bool) "a tap never leaves the grass" true (tap.z < 1.);
  Alcotest.(check bool) "but it does move the ball" true (Free_ball.speed tap.ball > 1.)

(* Aftertouch is the game: a lofted ball bends far more than one on the
 * grass, which is what makes Sensible's shots curl. *)
let sensible_aftertouch () =
  let open TinySensibleSoccer in
  let shot (bend : bool) (power : number) =
    let g = { (sensible_alone ()) with power } in
    let g = sensible_play 45 ~keyboard:(fun i -> if i = 1 then initial_computer.keyboard else { initial_computer.keyboard with kright = bend }) g in
    g.ball.x
  in
  let air = shot true 1. -. shot false 1. and ground = shot true 0.2 -. shot false 0.2 in
  Printf.eprintf "DBG sensible aftertouch: lofted %.0f, along the grass %.0f\n%!" air ground;
  Alcotest.(check bool) "a lofted ball bends a long way" true (air > 100.);
  Alcotest.(check bool) "further than one on the grass" true (air > ground *. 1.3)

(* The way a player actually lofts one: run with the ball, hold the
 * button while it is running a stride ahead of him, let go. The charge
 * has to survive the ball being out of reach for a frame here and
 * there -- it is dribbling, so it always is -- or nothing can ever be
 * held long enough to leave the grass. *)
let sensible_charge_while_dribbling () =
  let open TinySensibleSoccer in
  let k = initial_computer.keyboard in
  let held = sensible_play 34 ~keyboard:(fun _ -> { k with kup = true; kspace = true }) (sensible_alone ()) in
  Alcotest.(check bool) "the charge built while he ran with it" true (held.power > 0.9);
  let after = sensible_play 14 ~keyboard:(fun _ -> { k with kup = true }) held in
  Alcotest.(check bool) "and letting go lofted it" true (after.z > 20.)

(* Through the posts is a goal, and the game restarts in the middle *)
let sensible_goal () =
  let open TinySensibleSoccer in
  let g = { (new_game ()) with kickoff = 0; players = [ { (List.nth (new_game ()).players 4) with px = 0.; py = -400. } ]; mine = 0 } in
  let g = { g with ball = { (Free_ball.still 0. (half_h -. 30.)) with vy = 8. }; last = Some Home } in
  let after = sensible_play 20 g in
  Alcotest.(check int) "one nil" 1 after.home;
  Alcotest.(check bool) "and back to the centre spot" true (Float.hypot after.ball.x after.ball.y < 2.)

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
      t "TinyComanche, a robot pops the balloons" comanche_balloons;
      t "TinyDescent, the mine holds the ship, a robot shot, the exit" descent_mine;
      t "TinyQuake, qbsp, vis and light" quake_tools;
      t "TinyQuake, walking the level" quake_walk;
      t "TinyMinecraft, the world and what is shown" minecraft_world;
      t "TinyMinecraft, standing, jumping, walking, flying" minecraft_player;
      t "TinyMario64, a jump onto a platform" mario64_jump;
      t "TinyMarbleMadness, the ramp's heights" marble_ramp;
      t "TinyMarbleMadness, the cliff breaks the marble, the step doesn't" marble_falls;
      t "TinyMarbleMadness, the steelie knocks the marble" marble_steelie;
      t "TinyMarbleMadness, rolling down a ramp" marble_rolls_down;
      t "TinyMarbleMadness, a robot drives to the goal" marble_robot;
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
      t "TinyMissileCommand, a robot survives two waves" missile_robot;
      t "TinyMissileCommand, a chain reaction" missile_chain;
      t "TinyLemmings, a job per level" lemmings_levels;
      t "TinyLemmings, the fall that splats" lemmings_splat;
      t "TinyPuzzleBobble, the hexagonal grid" bobble_hex;
      t "TinyPuzzleBobble, popped and fallen" bobble_drop;
      t "TinyPuzzleBobble, a robot clears the rounds" bobble_robot;
      t "TinyTowerDefense, the maze and the referee" tower_maze;
      t "TinyTowerDefense, a monster finds its way again" tower_repath;
      t "TinyTowerDefense, towers hold the first waves" tower_waves;
      t "TinySonic, he runs right" sonic_walks;
      t "TinySonic, the loop" sonic_loop;
      t "TinyWarcraft2, a crowd on one flow field" warcraft_crowd;
      t "TinyWarcraft2, gold and wood" warcraft_gather;
      t "TinyWarcraft2, the fog of war" warcraft_fog;
      t "TinyDune2, an order is a path" dune2_order;
      t "TinyDune2, a harvester finds the spice" dune2_harvest;
      t "TinyDune2, the enemy takes an undefended base" dune2_war;
      t "TinyDune2, tanks take their refinery" dune2_attack;
      t "AiOthello, the rules" othello_rules;
      t "AiOthello, alpha-beta agrees with minimax" othello_alphabeta;
      t "AiOthello, the computer beats a greedy player" othello_greedy;
      t "TinyTron, the computer outlasts a straight line" tron_computer;
      t "TinyDungeonMaster, the key, the door, the lever, the stairs" dungeon_master_winnable;
      t "TinyDungeonMaster, the dance" dungeon_master_dance;
      t "PuzzleScriptSokoban, every level solvable" puzzlescript_sokoban_levels;
      t "PuzzleScriptBoulders, a run through the cave" puzzlescript_boulders_run;
      t "TinyBlockout, four quarter turns are none" blockout_turns;
      t "TinyBlockout, a layer goes and the rest comes down" blockout_layer;
      t "TinyBlockout, the pit refuses what does not fit" blockout_walls;
      t "TinyTombRaider, the tomb can be got out of" tomb_raider_route;
      t "TinyTombRaider, the two jumps, and the chasm between them" tomb_raider_jumps;
      t "TinyRobotron, the two sticks" robotron_twin_stick;
      t "TinyRobotron, a grunt on an electrode, a hulk on the family" robotron_walks_into_things;
      t "TinyRobotron, the brain rebuilds a human" robotron_brain_rebuilds;
      t "TinyRobotron, a robot clears the first wave" robotron_robot;
      t "TinyPinball, the flipper carries the ball" pinball_flipper_throws;
      t "TinyPinball, ours and the engine agree" pinball_both_engines;
      t "TinyPinball, substeps=1 falls through the table" pinball_tunnels;
      t "TinyPinball, the ball never leaves the table" pinball_stays_on_the_table;
      t "TinyPinball, three balls and it is over" pinball_three_balls;
      t "TinyBoomerangFu3d, the boomerang comes back to a moving thrower" boomerang_returns;
      t "TinyBoomerangFu3d, who a flight cuts" boomerang_cuts;
      t "TinyBoomerangFu3d, the dash is a slash only with it in hand" boomerang_slash;
      t "TinyBoomerangFu3d, the pits, and the computer that avoids them" boomerang_pits;
      t "TinyPortal2D, the transform keeps the speed" portal_transform;
      t "TinyPortal2D, the gun sticks to white walls only" portal_gun;
      t "TinyPortal2D, chamber 1: through the side walls" portal_chamber1;
      t "TinyPortal2D, chamber 2: the fling" portal_fling;
      t "TinyPortal2D, chamber 3: the cube on the button" portal_cube;
      t "TinyGauntlet2, the generators fill the room" gauntlet_generators;
      t "TinyGauntlet2, health is the clock" gauntlet_health_is_the_clock;
      t "TinyGauntlet2, shot the food" gauntlet_shot_the_food;
      t "TinyGauntlet2, the two chases" gauntlet_two_chases;
      t "TinyGauntlet2, the dungeon scrolls" gauntlet_scrolls;
      t "TinyGauntlet2, a robot walks out of the dungeon" gauntlet_robot_escapes;
      t "TinyKickOff2, the ball is not glued to your feet" kickoff_free_ball;
      t "TinyKickOff2, the aftertouch bends it" kickoff_aftertouch;
      t "TinyKickOff2, a goal, and the centre spot" kickoff_goal;
      t "TinyKickOff2, out at the side is a throw-in" kickoff_throw_in;
      t "TinyKickOff2, the formation slides with the ball" kickoff_formation_slides;
      t "TinySpeedball2, the arena pays" speedball_arena_pays;
      t "TinySpeedball2, the x2 plate doubles it" speedball_multiplier;
      t "TinySpeedball2, the walls give the ball back" speedball_walls;
      t "TinySpeedball2, the ball is carried, not chased" speedball_carries;
      t "TinySpeedball2, the tackle" speedball_tackle;
      t "TinySpeedball2, the camera eases after the ball" speedball_camera_is_smooth;
      t "TinySpeedball2, a goal is ten" speedball_goal;
      t "TinySpeedball2, a match plays itself" speedball_plays_itself;
      t "TinySensibleSoccer, close control: the third answer" sensible_close_control;
      t "TinySensibleSoccer, the ball has a height" sensible_loft;
      t "TinySensibleSoccer, a tap stays on the grass" sensible_tap_stays_down;
      t "TinySensibleSoccer, aftertouch bends a lofted ball" sensible_aftertouch;
      t "TinySensibleSoccer, the charge survives the dribble" sensible_charge_while_dribbling;
      t "TinySensibleSoccer, a goal" sensible_goal ]
