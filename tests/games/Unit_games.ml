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
  let b = List.fold_left (fun b d -> match Sokoban.step b d with Some b -> b | None -> Alcotest.fail "a step blocked") p.boards.now dirs in
  Alcotest.(check bool) "solved" true (Sokoban.solved b);
  Alcotest.(check (pair int int)) "moves, pushes" (7, 2) (b.moves, b.pushes)

(* the levels of TinySokoban.xsb, as the header says: their shortest
 * solutions are 7, 37 and 23 moves (the kit's solver, which is what
 * TinySokobanEd's s asks) *)
let sokoban_levels_solvable () =
  let moves rows = match Sokoban.solve rows with Sokoban.Moves m -> String.length m | _ -> -1 in
  Alcotest.(check (list int)) "shortest solutions" [ 7; 37; 23 ] (List.map moves TinySokoban.levels)

(* the editor's round trip: the file it exports is the one the game was
 * built with, byte for byte, when nothing was changed *)
let sokoban_editor_round_trip () =
  Alcotest.(check string) "TinySokoban.xsb" Sokoban_levels.xsb (Sokoban.to_xsb TinySokoban.levels)

(*****************************************************************************)
(* TinyPacman *)
(*****************************************************************************)

(* the ghosts leave their house, one after the other: Pinky is out,
 * hunting, 5 seconds into the game (READY! 2 s, 1 s waiting, leaving) *)
let pacman_ghosts_leave ?(ai_engine = false) () =
  let open TinyPacman in
  let g = ref (new_game ~ai_engine ()) in
  for i = 1 to 300 do g := update_game (computer i) !g done;
  let pinky = List.find (fun gh -> gh.name = Pinky) !g.ghosts in
  Alcotest.(check bool) "Pinky hunting" true (pinky.state = Hunting)

(* a power pellet turns the hunting ghosts blue, and back the other way *)
let pacman_blue ?(ai_engine = false) () =
  let open TinyPacman in
  let g = new_game ~ai_engine () in
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

(* a blue ghost on Pac-Man's tile is eaten: 200 points, once, and it
 * goes home as eyes (the machine's a frame later, the points at once) *)
let pacman_eaten ?(ai_engine = false) () =
  let open TinyPacman in
  let g = new_game ~ai_engine () in
  let blue (gh : ghost) =
    if gh.name = Blinky then { gh with m = g.pac; blue = true; mind = Fsm.start Mind.Frightened } else gh
  in
  let g = ref { g with pause = None; blue_frames = 300; ghosts = List.map blue g.ghosts } in
  for i = 1 to 3 do g := update_game (computer i) !g done;
  let blinky = List.find (fun gh -> gh.name = Blinky) !g.ghosts in
  Alcotest.(check bool) "eyes" true (blinky.state = Eyes);
  Alcotest.(check int) "200 points, not twice" 200 !g.score

(* the waves as a machine agree with the clock: in chase at the same
 * frames, 6000 of them (all seven changes of the schedule) *)
let pacman_waves () =
  let open TinyPacman in
  let run = ref (Fsm.start (Scatter 1)) in
  for frames = 1 to 6000 do
    run := Fsm.step wave_rules () !run;
    let machine = match !run.state with Chase _ -> true | Scatter _ -> false in
    if machine <> chasing frames then Alcotest.failf "frame %d: the machine and the clock disagree" frames
  done;
  Alcotest.(check bool) "chasing for good at the end" true (!run.state = Chase 4)

(*****************************************************************************)
(* TinySoldat *)
(*****************************************************************************)

(* claude: its bots, both ways (the flag ai=engine): 900 frames of a
 * three-way fight with the player (soldier 0) standing still. Either
 * way the two bots leave their corners and someone dies; the
 * difference is what they know -- by hand, each one has its enemies'
 * positions through the walls from the first frame; on ai/, it has to
 * see them, and patrols until it does *)
let soldat_fight ?(ai_engine = false) () =
  let open TinySoldat in
  let scenes = Scene2d.start Title in
  let p = ref (start ~ai_engine ()) in
  let spawn_of i = let b = body_of !p i in (b.x, b.y) in
  let blue_spawn = spawn_of 1 and green_spawn = spawn_of 2 in
  (* how far each one ever gets from where it started: a patrolling bot
   * turns every two seconds, so where it *ends* says nothing *)
  let roamed = [| 0.; 0.; 0. |] in
  for i = 1 to 900 do
    p := update_play (computer i) scenes !p;
    List.iter
      (fun (j, (x0, y0)) ->
        let (x, y) = spawn_of j in
        roamed.(j) <- Float.max roamed.(j) (Float.hypot (x -. x0) (y -. y0)))
      [ (1, blue_spawn); (2, green_spawn) ]
  done;
  Alcotest.(check bool) "BLUE left its corner" true (roamed.(1) > 100.);
  Alcotest.(check bool) "GREEN left its corner" true (roamed.(2) > 100.);
  let kills = Array.fold_left (fun n (s : soldier) -> n + s.kills) 0 !p.soldiers in
  Alcotest.(check bool) "somebody was killed" true (kills > 0)

(* claude: what the ai/ layer takes away: a bot that has seen nobody
 * knows nothing, where the hand-written one knows where everyone is.
 * One frame in, the ai=engine bots' senses hold no enemy position (the
 * three spawn apart, with the map between them) *)
let soldat_senses () =
  let open TinySoldat in
  let scenes = Scene2d.start Title in
  let p = ref (start ~ai_engine:true ()) in
  for i = 1 to 3 do
    p := update_play (computer i) scenes !p
  done;
  let knows i = match Bot.last_senses !p.minds.(i) with Some (s : senses) -> s.enemy.position <> None | None -> false in
  Alcotest.(check bool) "BLUE has seen nobody yet" false (knows 1);
  Alcotest.(check bool) "GREEN neither" false (knows 2)

(*****************************************************************************)
(* TinyBoomerangFu's cooks, on ai/ *)
(*****************************************************************************)

(* claude: the three things the ai/ layer takes away from a bot: sight
   through a wall, a decision the same frame, and a state machine's
   hysteresis hidden in an if *)

(* a cook behind a stone is out of sight; one across a hole is not --
   a hole is a hole, not a wall *)
let boomerang_sight () =
  let open TinyBoomerangFu in
  Alcotest.(check bool) "straight along the north" true (in_sight garden (-2., -9.) (2., -9.));
  Alcotest.(check bool) "through the stone at (-9, -5)" false (in_sight garden (-9., -7.) (-9., -3.));
  Alcotest.(check bool) "over the hole at (-5, -7)" true (in_sight garden (-7., -7.) (-3., -7.))

(* dodge, hunt, keep away -- and the guard that keeps it dodging for
   eight frames after the boomerang's line is clear, which is what
   stops it stepping back into it *)
let boomerang_modes () =
  let open TinyBoomerangFu in
  let senses ~armed ~incoming (run : mode Fsm.run) : senses =
    { arena_no = 0; level = Normal; at = (0., 0.); facing = 0.; armed; cool = 0; think = 0; seed = 1; last_way = (0., 1.);
      watched = []; enemy = Sense.unknown; incoming; mind = run }
  in
  let step ~armed ~incoming run = Fsm.step modes (senses ~armed ~incoming run) run in
  let hunting = Fsm.start Hunt in
  let dodging = step ~armed:true ~incoming:(Some (1., 0.5, 1., 0.)) hunting in
  Alcotest.(check bool) "a boomerang beats everything else" true (dodging.state = Dodge);
  (* the line is clear again at once, and it keeps stepping aside *)
  let after_1 = step ~armed:true ~incoming:None dodging in
  Alcotest.(check bool) "still dodging one frame later" true (after_1.state = Dodge);
  let rec settle run n = if n = 0 then run else settle (step ~armed:true ~incoming:None run) (n - 1) in
  Alcotest.(check bool) "and only after eight" true ((settle after_1 8).state = Hunt);
  (* and the other two: thrown, keep away; caught, hunt again *)
  Alcotest.(check bool) "unarmed, it keeps away" true ((step ~armed:false ~incoming:None hunting).state = Away);
  Alcotest.(check bool) "with it back, it hunts" true
    ((step ~armed:true ~incoming:None (Fsm.start Away)).state = Hunt)

(* the delay: it acts on what it saw six frames ago, so when the world
   changes under it, it keeps answering the old world for six frames.
   (A bot that has only just started acts on the oldest senses it has,
   which is this frame's -- Bot.mli: the delay is a memory, not a
   blindfold.) *)
let boomerang_delay () =
  let open TinyBoomerangFu in
  let at (i : int) (x : number) (z : number) (g : game) : game =
    { g with players = List.map (fun (p : player) -> if p.idx = i then { p with px = x; pz = z } else p) g.players }
  in
  (* the strawberry in the middle of the north side, ready to throw (a
     fresh cook hesitates before its first throw and circles
     meanwhile), and its enemy out east, beyond its range, so it walks
     at it *)
  let ready (g : game) : game =
    { g with players = List.map (fun (p : player) -> if p.idx = 1 then { p with think = 0 } else p) g.players }
  in
  let g = ready (at 1 0. (-9.) (at 0 9. (-9.) (new_game ()))) in
  (* eight frames of hunting an enemy to the east *)
  let rec frames g n r last =
    if n = 0 then (r, last)
    else
      let (it, r) = Bot.step mind (g, 1) r in
      frames g (n - 1) r it
  in
  let (running, east) = frames g 8 (Bot.start idle) idle in
  let towards (i : intent) = match i.go with Some (dx, _) -> dx | None -> 0. in
  Alcotest.(check bool) "it heads east" true (towards east > 0.);
  (* the enemy jumps to the other side; it goes on heading east *)
  let g = at 0 (-9.) (-9.) g in
  let rec steps g n r acc = if n = 0 then List.rev acc else
    let (it, r) = Bot.step mind (g, 1) r in
    steps g (n - 1) r (towards it :: acc)
  in
  let after = steps g 12 running [] in
  let first_six = List.filteri (fun i _ -> i < 6) after in
  Alcotest.(check bool) "six frames still heading the old way" true (List.for_all (fun dx -> dx > 0.) first_six);
  Alcotest.(check bool) "and then it turns round" true (List.exists (fun dx -> dx < 0.) after)

(*****************************************************************************)
(* AiConnect4 *)
(*****************************************************************************)

(* claude: what each trick of Deepening saves, on the opening
   position searched 7 moves ahead -- the numbers Deepening.mli quotes.
   Each line adds one thing to the line above *)
let connect4_nodes () =
  let open AiConnect4 in
  let table = Zobrist.table () in
  let plain = (Minimax.alphabeta connect4 ~depth start).nodes in
  let ordered = (Minimax.alphabeta ordered_rules ~depth start).nodes in
  let deepened = (Deepening.search ~order:middle_first connect4 ~depth start).nodes in
  let tabled = (Deepening.search ~order:middle_first ~key ~table connect4 ~depth start).nodes in
  Alcotest.(check int) "alpha-beta, the columns left to right" 65724 plain;
  Alcotest.(check int) "the middle columns first" 9449 ordered;
  (* iterative deepening costs a third more here: the game's own hint
     already orders the moves well, so the shallower passes buy little
     and are paid for in full (Deepening.mli says so) *)
  Alcotest.(check int) "+ iterative deepening, 1 to 7" 12818 deepened;
  Alcotest.(check int) "+ the transposition table" 7742 tabled

(* claude: the tricks must not change the answer: every way of
   searching plays the same opening move (the middle column, which is
   what everybody knows about Connect 4) *)
let connect4_same_move () =
  let open AiConnect4 in
  let table = Zobrist.table () in
  let plain = (Minimax.alphabeta connect4 ~depth start).best in
  let tabled = (Deepening.search ~order:middle_first ~key ~table connect4 ~depth start).best in
  Alcotest.(check (option int)) "alpha-beta and the tricks agree" plain tabled;
  Alcotest.(check (option int)) "and it is the middle column" (Some 3) tabled

(*****************************************************************************)
(* AiGo *)
(*****************************************************************************)

(* claude: Go's rules are the subtle part of that game, and the search
   knows none of them -- so they are what there is to check. A board
   written out as nine rows, "." empty, "b" black, "w" white *)
let go_board (rows : string list) (turn : AiGo.stone) : AiGo.position =
  let open AiGo in
  let board = Array.make points Empty in
  List.iteri
    (fun r row ->
      String.iteri
        (fun c ch ->
          board.(((size - 1 - r) * size) + c) <- (match ch with 'b' -> Black | 'w' -> White | _ -> Empty))
        row)
    rows;
  { board; turn; ko = None; passes = 0 }

let go_rows (p : AiGo.position) : string list =
  let open AiGo in
  List.init size (fun r ->
      String.init size (fun c ->
          match p.board.(((size - 1 - r) * size) + c) with Empty -> '.' | Black -> 'b' | White -> 'w'))

let go_captures () =
  let open AiGo in
  (* a white stone with one liberty left, on the edge; black takes it *)
  let p = go_board [ ".........";  ".........";  ".........";  ".........";  ".........";  ".........";  ".b.......";  "bw.......";  ".b......." ] Black in
  let after = Option.get (put p ((1 * size) + 2)) in
  Alcotest.(check (list string)) "the white stone is taken off"
    [ ".........";  ".........";  ".........";  ".........";  ".........";  ".........";  ".b.......";  "b.b......";  ".b......." ]
    (go_rows after);
  (* the same point for white would be suicide: no liberty, nothing taken *)
  let suicide = go_board [ ".........";  ".........";  ".........";  ".........";  ".........";  ".........";  ".b.......";  "b.b......";  ".b......." ] White in
  Alcotest.(check bool) "white cannot fill it: suicide" true (put suicide ((1 * size) + 1) = None);
  (* but black may: it is its own eye, and legal *)
  Alcotest.(check bool) "black may" true (put { suicide with turn = Black } ((1 * size) + 1) <> None)

let go_ko () =
  let open AiGo in
  (* the ko shape: black takes one white stone, white may not take it
     straight back *)
  (* black plays the empty point, taking the white stone beside it; the
     three stones around black's are white, so black's is alone with
     that one liberty -- the shape where taking back would repeat for
     ever *)
  let p = go_board [ ".........";  ".........";  ".........";  ".........";  ".........";  ".........";  ".wb......";  "w.wb.....";  ".wb......" ] Black in
  let taken = Option.get (put p ((1 * size) + 1)) in
  Alcotest.(check bool) "black took the white stone" true (taken.board.((1 * size) + 2) = Empty);
  Alcotest.(check (option int)) "and the point it took from is forbidden" (Some ((1 * size) + 2)) taken.ko;
  Alcotest.(check bool) "white cannot take back at once" true (put taken ((1 * size) + 2) = None)

let go_scoring () =
  let open AiGo in
  (* black owns the bottom left corner: four stones around three empty
     points; white has a stone of its own *)
  (* five black stones walling off the four points of the corner *)
  let p = go_board [ ".........";  ".........";  ".........";  ".........";  ".........";  ".......w.";  "bbb......";  "..b......";  "..b......" ] Black in
  Alcotest.(check (float 0.)) "black: five stones and four points" 9. (area p Black);
  Alcotest.(check (float 0.)) "white: one stone, no territory" 1. (area p White);
  Alcotest.(check (float 0.)) "black leads, less komi" (9. -. 1. -. 6.5) (final_score p)

(* claude: and the search plays a legal move, in the time a turn has *)
let go_plays () =
  let open AiGo in
  let t0 = Unix.gettimeofday () in
  let r = Mcts.search ~seed:1 ~playout go ~playouts:1200 start in
  let seconds = Unix.gettimeofday () -. t0 in
  Printf.eprintf "go: 1200 playouts in %.2f s\n" seconds;
  match r.best with
  | Some (Put i) -> Alcotest.(check bool) "a legal point" true (put start i <> None)
  | Some Pass -> Alcotest.fail "it passed on an empty board"
  | None -> Alcotest.fail "no move at all"

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
(* TinyMarioKart *)
(*****************************************************************************)

(* Mode 7's two ways, a screen pixel to the ground and back: 90 high,
 * with a focal of 866 (a 1000-pixel screen, 60 degrees), the row 90
 * pixels under the horizon sees 866 ahead of the eye (250 behind the
 * kart), where a pixel is a unit *)
let kart_mode7 () =
  let open TinyMarioKart in
  let e = eye (Playground.to_screen 1000. 1000.) 0. 0. 0. in
  let x, y = Option.get (to_ground e 0. (e.horizon -. 90.)) in
  Alcotest.(check (list (float 0.01))) "866 ahead" [ 866.03 -. 250.; 0. ] [ x; y ];
  let sx, sy, scale = Option.get (TinyMarioKart.to_screen e x y) in
  Alcotest.(check (list (float 1e-6))) "and back" [ 0.; e.horizon -. 90.; 1. ] [ sx; sy; scale ];
  (* right of the screen is right of the way we look, -y when looking +x *)
  let x, y = Option.get (to_ground e 100. (e.horizon -. 45.)) in
  Alcotest.(check bool) "to the right" true (y < 0.);
  let sx, sy, _ = Option.get (TinyMarioKart.to_screen e x y) in
  Alcotest.(check (list (float 1e-6))) "and back" [ 100.; e.horizon -. 45. ] [ sx; sy ];
  Alcotest.(check bool) "the sky" true (to_ground e 0. (e.horizon +. 1.) = None)

(* the computer drives all four karts (the player's too, as after the
 * finish: a place already given): the player's kart does its 3 laps in
 * under two minutes, hardly ever on the grass, and so do the others,
 * slower *)
let kart_race () =
  let open TinyMarioKart in
  let r = ref { (new_race 1) with places = [ Some 1 ] } and frames = ref 0 and grass = ref 0 in
  let player () = (List.hd !r.karts).car in
  while Topdown.lap track (player ()) < laps && !frames < 60 * 120 do
    incr frames;
    r := update_race initial_computer.keyboard !r;
    if top_speed (player ()).x (player ()).y < 300. then incr grass
  done;
  Alcotest.(check int) "3 laps" laps (Topdown.lap track (player ()));
  Alcotest.(check bool) "hardly on the grass" true (!grass < 60);
  List.iter (fun k -> Alcotest.(check bool) "the others lapping" true (Topdown.lap track k.car >= 2)) !r.karts

(* Two players: the first two karts, each driven by its own keys (the
 * arrows, w a s d), each finishing on its own; and in a half of the
 * split screen, a hill taller than the half's sky is cut at its top. *)
let kart_two_players () =
  let open TinyMarioKart in
  let r = { (new_race 2) with ready = 0 } in
  Alcotest.(check int) "two players" 2 (List.length r.view_angles);
  let wasd = { initial_computer.keyboard with kw = true } in
  let r' = ref r in
  for _ = 1 to 60 do r' := update_race wasd !r' done;
  let moved i = Float.hypot ((List.nth !r'.karts i).car.x -. (List.nth r.karts i).car.x) ((List.nth !r'.karts i).car.y -. (List.nth r.karts i).car.y) in
  Alcotest.(check bool) (Printf.sprintf "w drives the second (%.0f)" (moved 1)) true (moved 1 > 100.);
  Alcotest.(check bool) (Printf.sprintf "not the first (%.0f)" (moved 0)) true (moved 0 < 20.);
  let one_done = { !r' with places = [ Some 2; None ] } in
  Alcotest.(check bool) "one finished, the race goes on" true (List.nth (update_race wasd one_done).places 1 = None);
  let half = Playground.to_screen 1000. 500. in
  let e = eye half 0. 0. 0. in
  Alcotest.(check (float 1e-9)) "the half's horizon: 15% of its height" 75. e.horizon;
  match (hill half Playground.red 220. 0. e.horizon).form with
  | Playground.Polygon (_, points) ->
      Alcotest.(check (float 1e-9)) "a tall hill, cut at the half's top" half.top
        (List.fold_left (fun m (_, y) -> Float.max m y) neg_infinity points)
  | _ -> Alcotest.fail "a tall hill should be cut" 

(*****************************************************************************)
(* TinyMarioKart64 *)
(*****************************************************************************)

(* the computer drives all eight karts (the player's too, as after the
 * finish: a place already given), up the hill, off the ramp, through the traffic and whatever
 * they throw at each other: the player's kart does its 3 laps in under
 * two minutes and a half, hardly ever off the road, and the others are
 * not far behind -- which is what the rubber band is for *)
let mario_kart_race () =
  let open TinyMarioKart64 in
  let r = ref { (new_race 1) with places = [| Some 1 |] } and frames = ref 0 and off_road = ref 0 and flew = ref false in
  let player () = !r.karts.(0) in
  while (player ()).lap < laps && !frames < 60 * 150 do
    incr frames;
    r := step_race [| no_pad |] !r;
    let k = player () in
    if top_speed_at (Track3d.at track k.s).width k.offset < 30. then incr off_road;
    if k.air > 1. then flew := true
  done;
  Alcotest.(check int) "3 laps" laps (player ()).lap;
  Alcotest.(check bool) "hardly off the road" true (!off_road < 240);
  Alcotest.(check bool) "over the ramp" true !flew;
  Array.iter (fun (k : kart) -> Alcotest.(check bool) "the others lapping" true (k.lap >= 2)) !r.karts

(* the powerslide: hold it into a corner and the charge builds (2 a
 * frame while the wheel stays in the slide), let go and the charge is
 * a boost, which makes the kart faster than its top speed. Driven from
 * the middle of the start straight, where there is room to slide. *)
let mario_kart_mini_turbo () =
  let open TinyMarioKart64 in
  let start = (new_race 1).karts.(0) in
  let s0 = 40. in
  let x, y = plane_at s0 0. in
  let heading = 90. -. (Track3d.at track s0).heading in
  let k = ref { start with car = { start.car with x; y; heading; speed = 40. }; s = s0; offset = 0. } in
  for _ = 1 to 30 do
    k := step_kart true 1. 1. road_speed !k
  done;
  Alcotest.(check bool) "charged" true (match !k.drift with Sliding (_, charge) -> charge >= 55 | Straight -> false);
  k := step_kart false 1. 0. road_speed !k;
  Alcotest.(check bool) "the mini-turbo" true (!k.boost > 0);
  let fast = ref 0. in
  for _ = 1 to 25 do
    k := step_kart false 1. 0. road_speed !k;
    fast := Float.max !fast !k.car.speed
  done;
  Alcotest.(check bool) "faster than flat out" true (!fast > road_speed)

(* the items go by place: the leader never draws what would take him
 * further ahead, the back of the field does *)
let mario_kart_items () =
  let open TinyMarioKart64 in
  let seeds = [ 0; 1; 2 ] in
  Alcotest.(check bool) "nothing to catch up with, in front" true
    (List.for_all (fun s -> roll 1 s <> Mushroom && roll 1 s <> Red_shell) seeds);
  Alcotest.(check bool) "something to catch up with, at the back" true
    (List.exists (fun s -> roll 8 s = Mushroom) seeds);
  (* and the rubber band: behind the player, faster; ahead of him, slower *)
  let k = (new_race 1).karts.(1) in
  Alcotest.(check bool) "faster when behind" true (rubber (along k +. 100.) k > 1.);
  Alcotest.(check bool) "slower when ahead" true (rubber (along k -. 100.) k < 1.)

(* Up to four players: one or two race the seven computers' karts,
 * three or four race each other alone, as on the N64; each pad drives
 * its own kart; and the screen is a view per player, in quadrants. *)
let mario_kart_players () =
  let open TinyMarioKart64 in
  Alcotest.(check int) "two players among eight karts" 8 (Array.length (new_race 2).karts);
  let r = { (new_race 4) with ready = 0 } in
  Alcotest.(check int) "four players, four karts" 4 (Array.length r.karts);
  Alcotest.(check int) "four colors" 4 (List.length (List.sort_uniq compare (Array.to_list (Array.map (fun (k : kart) -> k.color) r.karts))));
  let go = { no_pad with gas = 1. } and stop = { no_pad with gas = -1. } in
  let r' = ref r in
  for _ = 1 to 90 do r' := step_race [| go; stop; go; stop |] !r' done;
  let moved i = along !r'.karts.(i) -. along r.karts.(i) in
  Alcotest.(check bool) (Printf.sprintf "the gas moves 1 and 3 (%.0f, %.0f)" (moved 0) (moved 2)) true (moved 0 > 20. && moved 2 > 20.);
  (* the brake held from a standstill backs away, as down does alone *)
  Alcotest.(check bool) (Printf.sprintf "the brake keeps 2 and 4 back (%.1f, %.1f)" (moved 1) (moved 3)) true (moved 1 < 0. && moved 3 < 0.);
  let views = TinyMarioKart64.view (Playground.initial_computer) (Scene2d.start (Racing r)) in
  Alcotest.(check int) "four views" 4 (List.length views);
  Alcotest.(check bool) "in the four quadrants" true (List.map (fun (v : Playground3d.view) -> v.area) views = Playground3d.split 4)

(* Block Fort: two heights where a bridge crosses the floor, the one a
 * kart stands on found by its own height; a fort's side stops a kart,
 * and a ramp takes it to the top. *)
let mario_kart_block_fort () =
  let open TinyMarioKart64 in
  let under_bridge = (0., fort_center) in
  Alcotest.(check (float 1e-9)) "under the bridge: the floor" 0. (level (fst under_bridge) (snd under_bridge) 0.);
  Alcotest.(check (float 1e-9)) "on it: the bridge" fort_height (level (fst under_bridge) (snd under_bridge) fort_height);
  let mid_ramp = fort_center +. fort_half +. (ramp_length_bf /. 2.) in
  Alcotest.(check (float 1e-6)) "half way up a ramp" (fort_height /. 2.) (level mid_ramp fort_center 2.);
  Alcotest.(check bool) "a fort's side, from the floor" true (solid_at fort_center fort_center 0.);
  Alcotest.(check bool) "its top, from above" false (solid_at fort_center fort_center fort_height);
  let bt = new_battle 2 in
  let place (f : fighter) x y heading = { f with kart = { f.kart with car = { f.kart.car with x; y; heading; vx = 0.; vy = 0.; speed = 0. } } } in
  let gas = { no_pad with gas = 1. } in
  let drive n f = let f = ref f in for _ = 1 to n do f := step_fighter gas !f done; !f in
  (* at the fort's east side, facing it (west): it bounces, and stays on the floor *)
  let at_wall = drive 60 (place bt.fighters.(0) (fort_center +. fort_half +. 16.) 0. 180.) in
  Alcotest.(check bool) (Printf.sprintf "stopped by the fort (x %.1f)" at_wall.kart.car.x) true
    (at_wall.kart.car.x >= fort_center +. fort_half -. 0.5 && at_wall.h = 0.);
  (* at the foot of the north-east fort's ramp, facing up it: on top *)
  let climbed = drive 90 (place bt.fighters.(0) (fort_center +. fort_half +. ramp_length_bf +. 3.) fort_center 180.) in
  Alcotest.(check (float 1e-6)) (Printf.sprintf "up the ramp, on the fort (x %.1f)" climbed.kart.car.x) fort_height climbed.h

(* A shell comes back off a wall; a hit pops a balloon, with a moment
 * when nothing else can; the last kart with a balloon wins. *)
let mario_kart_balloons () =
  let open TinyMarioKart64 in
  let bt = new_battle 2 in
  let shell = { bsx = arena -. 1.; bsy = -40.; bsh = 0.; vx = 62.; vy = 0.; seeking = false; blife = 300; by_ = 0 } in
  (match step_bshell bt.fighters shell with
  | Some s, _ -> Alcotest.(check bool) "off the wall, and back" true (s.vx < 0.)
  | None, _ -> Alcotest.fail "the shell should bounce");
  let f = bt.fighters.(1) in
  let hit = pop f in
  Alcotest.(check int) "a balloon popped" 2 hit.balloons;
  Alcotest.(check int) "and not twice at once" 2 (pop hit).balloons;
  Alcotest.(check bool) "two left: no winner yet" true (battle_winner bt = None);
  let last = { bt with fighters = [| bt.fighters.(0); { f with balloons = 0 } |] } in
  Alcotest.(check bool) "one left: the winner" true (battle_winner last = Some 0)

(* the circuit itself: the ribbon's two ways, a distance along and an
 * offset across, there and back; and a lap that climbs and comes home
 * to the height it started from *)
let mario_kart_ribbon () =
  let open TinyMarioKart64 in
  let s = 120. and offset = 4.5 in
  let x, y, z = Track3d.across track s offset in
  let back_s, back_offset = Track3d.locate ~near:s track x z in
  Alcotest.(check (float 0.2)) "the same distance along" s back_s;
  Alcotest.(check (float 0.2)) "the same offset across" offset back_offset;
  (* the banked corner really leans: the outside of it is higher than
   * the inside, by more than the road is thick *)
  let bank_s = lap_length *. 0.38 in
  let _, left, _ = Track3d.across track bank_s (-9.) and _, right, _ = Track3d.across track bank_s 9. in
  Alcotest.(check bool) "the outside of the bank is higher" true (left -. right > 2.);
  (* and the height at the start is the height after a lap *)
  let _, y0, _ = Track3d.across track 0. 0. and _, y1, _ = Track3d.across track lap_length 0. in
  Alcotest.(check (float 0.001)) "the lap closes" y0 y1;
  ignore y

(*****************************************************************************)
(* TinyVirtuaFighter *)
(*****************************************************************************)

(* the keyframes are the frame data: the fist is where the hitbox is,
 * exactly while the move can hit. Written by eye instead, an animation
 * drifts from the rules and a fighter seems to hit before it does. *)
let virtua_fighter_keyframes () =
  let open TinyVirtuaFighter in
  let m = move_of Punch in
  let reach (frame : int) : number =
    let _, _, z = Skeleton.hand fighter_height 0. (poses_of Punch frame) in
    (* built facing -z, so the fist reaches out as this grows *)
    -.z
  in
  (* frame 0 is the guard; by frame 1 the arm is already a quarter of
   * the way out, which is the interpolation doing its job *)
  let guard_reach = reach 0 and active_reach = reach (m.startup + 1) and back = reach (Frame_data.length m) in
  Alcotest.(check bool) "the fist is out while the move is active" true (active_reach > guard_reach +. 0.3);
  Alcotest.(check bool) "and back afterwards" true (back < active_reach -. 0.3);
  (* the hitbox and the fist agree about where a punch lands *)
  Alcotest.(check bool) "the hitbox is where the fist is" true (Float.abs (m.hitbox.x -. active_reach) < 0.35)

(* a punch that lands takes health, the same punch blocked takes far
 * less and pushes less: holding back is the whole defence *)
let virtua_fighter_blocking () =
  let open TinyVirtuaFighter in
  let attacker = { (new_fighter (-0.5) 1.) with state = Attacking (Punch, (move_of Punch).startup + 1) } in
  let open_up = new_fighter 0.5 (-1.) in
  let back = { no_input with dir = { back = true; forward = false; down = false } } in
  let _, hit, landed = strike attacker open_up no_input in
  let _, guarded, landed_guard = strike attacker open_up back in
  Alcotest.(check bool) "it lands" true landed;
  Alcotest.(check bool) "it lands on the guard too" true landed_guard;
  Alcotest.(check bool) "blocking costs less health" true (guarded.health > hit.health);
  Alcotest.(check bool) "and gives less ground" true (guarded.x < hit.x);
  (* a low kick goes under a standing guard *)
  let sweeper = { (new_fighter (-0.5) 1.) with state = Attacking (Low_kick, (move_of Low_kick).startup + 1) } in
  let _, swept, _ = strike sweeper open_up back in
  Alcotest.(check bool) "a low kick is not blocked standing" true (swept.health < guarded.health)

(* the ring is the second way to lose: pushed past its edge, a fighter
 * goes down and the round is over whatever its health says *)
let virtua_fighter_ring_out () =
  let open TinyVirtuaFighter in
  (* a kick landing on someone already at the edge pushes them off it,
   * with their health barely touched: the ring is a second way to lose *)
  let attacker = { (new_fighter (ring_half -. 0.8) 1.) with state = Attacking (Kick, (move_of Kick).startup + 1) } in
  let victim = new_fighter (ring_half -. 0.05) (-1.) in
  let _, pushed, landed = strike attacker victim no_input in
  Alcotest.(check bool) "the kick lands" true landed;
  Alcotest.(check bool) "and pushes them off the ring" true (off_ring pushed);
  Alcotest.(check bool) "with most of their health" true (pushed.health > 80);
  (* and a round in that state is over, won by the one still on it *)
  let r = new_round 0 0 in
  let r = { r with b = { r.b with x = ring_half +. 0.1 } } in
  let r = step_round initial_computer.keyboard r in
  Alcotest.(check bool) "the round is decided" true (r.over > 0);
  Alcotest.(check bool) "on the ring, not on damage" true (r.ring_out && r.b.health > 80);
  Alcotest.(check int) "the one still standing on it wins it" 1 r.won_a

(*****************************************************************************)
(* TinyStarFox *)
(*****************************************************************************)

(* on rails: the stage carries the ship down the canyon whatever the
 * player does, and the player can only move it across -- inside the
 * canyon, whatever the player does *)
let star_fox_rails () =
  let open TinyStarFox in
  let r = ref (new_run ()) in
  let keys = { initial_computer.keyboard with kleft = true; kup = true } in
  for _ = 1 to 300 do
    r := step_run keys !r
  done;
  Alcotest.(check (float 0.01)) "carried at the stage's speed" (start_s +. (300. *. ship_speed /. 60.)) !r.s;
  let width = (Track3d.at track !r.s).width in
  Alcotest.(check bool) "not through the wall" true (Float.abs !r.ship.offset <= width -. 2. +. 0.001);
  Alcotest.(check bool) "not above the canyon" true (!r.ship.height <= ceiling +. 0.001)

(* a bolt fired down the canyon meets the enemy in front of it: the
 * enemy is flying the shmup kit's 2D path across the cross-section, so
 * the ship is put where that path puts the enemy *)
let star_fox_bolt () =
  let open TinyStarFox in
  let r = new_run () in
  let target = List.hd waves in
  let x, y = enemy_across target in
  let r =
    { r with
      s = target.es -. 40.;
      ship = { r.ship with offset = x; height = y };
      enemies = [ { target with speed = 0. } ] }
  in
  let r = ref (fire r) in
  for _ = 1 to 60 do
    r := step_bolts !r
  done;
  Alcotest.(check int) "one enemy down" 100 !r.score;
  Alcotest.(check bool) "and it stays down" true (List.for_all (fun (e : enemy) -> not e.alive) !r.enemies)

(*****************************************************************************)
(* TinyAloneInTheDark *)
(*****************************************************************************)

(* the tile under a point, and the doorway between the hall and the
 * corridor *)
let alone_door () : number * number =
  let open TinyAloneInTheDark in
  match Tilemap.find map '.' with
  | cells -> (
      (* the one below the hall: the lowest of them *)
      let col, row = List.fold_left (fun (c, r) (c2, r2) -> if r2 > r then (c2, r2) else (c, r)) (List.hd cells) cells in
      match Tilemap.center map col row with x, y -> (x, y))

(* The cut, and where it happens: standing in the doorway, the camera is
 * still the hall's (a doorway belongs to no room, so the room you came
 * from is kept); one step past it, it is the corridor's -- instantly,
 * with nothing in between. Change it *in* the doorway instead, and a
 * player standing on the threshold flickers between the two shots. *)
let alone_cut () =
  let open TinyAloneInTheDark in
  let dx, dy = alone_door () in
  let h = new_house () in
  (* in the doorway, facing south, coming from the hall *)
  let h = { h with carnby = { h.carnby with x = dx; y = dy; heading = -90. }; room = 'a' } in
  let still = step_house initial_computer.keyboard h in
  Alcotest.(check char) "in the doorway: the hall's camera still" 'a' still.room;
  let h = ref still in
  for _ = 1 to 40 do
    h := step_house { initial_computer.keyboard with kup = true } !h
  done;
  Alcotest.(check char) "past it: the corridor's" 'c' !h.room

(* Tank controls: up walks the way Carnby faces, whatever the camera is
 * doing -- the same key moves him east facing east and west facing
 * west, in the same room, under the same shot *)
let alone_tank () =
  let open TinyAloneInTheDark in
  let h = new_house () in
  let walk heading =
    let h = { h with carnby = { h.carnby with heading } } in
    let after = step_house { initial_computer.keyboard with kup = true } h in
    after.carnby.x -. h.carnby.x
  in
  Alcotest.(check bool) "facing east, up goes east" true (walk 0. > 0.);
  Alcotest.(check bool) "facing west, up goes west" true (walk 180. < 0.)

(* the study's door holds without the key, and opens for it *)
let alone_door_locked () =
  let open TinyAloneInTheDark in
  let lx, ly =
    match Tilemap.find map 'L' with [ (col, row) ] -> Tilemap.center map col row | _ -> Alcotest.fail "no door"
  in
  (* just north of the door, facing south into it *)
  let before = { (new_house ()) with room = 'c' } in
  let before = { before with carnby = { before.carnby with x = lx; y = ly +. 2.; heading = -90. } } in
  let walk (h : house) =
    let h = ref h in
    for _ = 1 to 90 do
      h := step_house { initial_computer.keyboard with kup = true } !h
    done;
    !h
  in
  let locked = walk before in
  Alcotest.(check bool) "without the key: still outside" true (locked.carnby.y > ly);
  let opened = walk { before with has_key = true } in
  Alcotest.(check bool) "with it: the door opens" true opened.unlocked;
  Alcotest.(check bool) "and he walks through" true (opened.carnby.y < ly)

(*****************************************************************************)
(* TinyElite *)
(*****************************************************************************)

(* the galaxy is the original's: the seed twisted system by system gives
 * Tibedied first, and Lave -- where every game began -- eighth, with
 * the government, economy and tech level the manual gave it *)
let elite_galaxy () =
  let open TinyElite in
  let first = systems 10 in
  Alcotest.(check string) "system 0" "Tibedied" (List.nth first 0).name;
  Alcotest.(check (list string)) "the next ones" [ "Qube"; "Leleer"; "Biarge"; "Xequerin" ]
    (List.map (fun (s : system) -> s.name) (List.filteri (fun i _ -> i >= 1 && i <= 4) first));
  Alcotest.(check string) "Lave" "Lave" lave.name;
  Alcotest.(check string) "a dictatorship" "Dictatorship" lave.government;
  Alcotest.(check string) "rich agricultural" "Rich Agricultural" lave.economy;
  Alcotest.(check int) "tech level 5" 5 lave.tech

(* The 6502's rotation is not quite a rotation: each small turn
 * stretches the two vectors it mixes by 1 + t^4/4, so a thousand turns
 * without straightening leave them measurably out of square -- about
 * 4e-4 in floating point, far more in Elite's 8-bit fixed point, which
 * is why it tidied every few frames. One TIDY and they are square
 * again, to the last digit. *)
let elite_tidy () =
  let open TinyElite in
  let o = ref upright in
  for _ = 1 to 1000 do
    o := roll_by 0.035 (pitch_by 0.022 !o)
  done;
  let drift = Float.abs (length !o.nose -. 1.) +. Float.abs (dot !o.nose !o.roof) in
  Alcotest.(check bool) "drifted" true (drift > 1e-5);
  let t = tidy !o in
  Alcotest.(check (float 1e-9)) "unit nose" 1. (length t.nose);
  Alcotest.(check (float 1e-9)) "unit roof" 1. (length t.roof);
  Alcotest.(check (float 1e-9)) "square" 0. (dot t.nose t.roof);
  Alcotest.(check (float 1e-9)) "side square too" 0. (dot t.side t.nose)

(* Hidden lines: the station seen straight on to one of its squares,
 * from far off, shows that square and the four triangles round it --
 * five faces, twelve of its twenty-four edges -- and nothing of the
 * far side. Battlezone would draw all twenty-four. *)
let elite_hidden_lines () =
  let open TinyElite in
  Alcotest.(check int) "twenty-four edges" 24 (List.length coriolis.edges);
  let shown = visible_edges coriolis (v 0. 0. 100000.) upright in
  Alcotest.(check int) "twelve of them facing you" 12 (List.length shown)

(* docking: at the slot, slow, pointing in, and rolled to match it; the
 * same approach rolled a quarter turn is a crash, because the slot is a
 * letterbox and it turns *)
let elite_docking () =
  let open TinyElite in
  let facing_you = { nose = v 0. 0. (-1.); roof = v 0. 1. 0.; side = v (-1.) 0. 0. } in
  let at_slot (o : orientation) =
    { (new_flight ()) with station = { pos = v 0. 0. (coriolis_size +. 40.); o; speed = 0. }; speed = 3. }
  in
  Alcotest.(check bool) "lined up: docked" true (docking (at_slot facing_you));
  Alcotest.(check bool) "rolled a quarter turn: not" false (docking (at_slot (tidy (roll_by 1.57 facing_you))))

(* Elite's hidden lines are backface culling: the edges TinyElite draws
 * (LL9, an edge kept when either of its faces is turned towards you)
 * are the edges of the faces graphics/3d's Cull keeps of TinyElite3d's
 * wound polygons, the eye at the origin -- for the station seen from
 * a few places, turned a few ways *)
let elite3d_culling_is_ll9 () =
  let round (x, y, z) = (Float.round (x *. 1000.), Float.round (y *. 1000.), Float.round (z *. 1000.)) in
  let pair a b = let a = round a and b = round b in (min a b, max a b) in
  List.iter
    (fun ((x, y, z), pitch, roll) ->
      let o2 = TinyElite.(tidy (roll_by roll (pitch_by pitch upright))) in
      let o3 = TinyElite3d.(tidy (roll_by roll (pitch_by pitch upright))) in
      let ll9 =
        TinyElite.(visible_edges coriolis (v x y z) o2)
        |> List.map (fun ((a : TinyElite.vec), (b : TinyElite.vec)) -> pair (a.x, a.y, a.z) (b.x, b.y, b.z))
        |> List.sort_uniq compare
      in
      let pos = TinyElite3d.v x y z in
      let culled =
        List.concat_map
          (fun face ->
            let pts = List.map (fun i -> TinyElite3d.place pos o3 TinyElite3d.coriolis.corners.(i)) face in
            let wound = TinyElite3d.wound pos pts in
            if not (Cull.faces_camera ~eye:(0., 0., 0.) wound) then []
            else
              (* back from the engine's frame to Elite's, and round the face *)
              let back = List.map (fun (x, y, z) -> (x, y, -.z)) wound in
              List.mapi (fun k a -> pair a (List.nth back ((k + 1) mod List.length back))) back)
          TinyElite3d.coriolis.faces
        |> List.sort_uniq compare
      in
      Alcotest.(check int) "as many edges" (List.length ll9) (List.length culled);
      Alcotest.(check bool) "the same edges" true (ll9 = culled))
    [ ((0., 0., 700.), 0.6, 0.3); ((120., -60., 3400.), 0., 0.3); ((300., 200., 500.), 1.2, -0.7); ((-50., 30., 400.), 2.5, 1.9) ]

(*****************************************************************************)
(* TinyTeardown *)
(*****************************************************************************)

(* Greedy meshing: a voxel is its six faces, and so is a bar of four in
 * one material; two materials side by side are not merged *)
let teardown_mesh () =
  let open TinyTeardown in
  let quads mat dims = List.length (mesh mat dims corner) in
  Alcotest.(check int) "one voxel" 6 (quads (fun _ _ _ -> Brick) (1, 1, 1));
  Alcotest.(check int) "a bar of four" 6 (quads (fun _ _ _ -> Brick) (4, 1, 1));
  Alcotest.(check int) "half brick, half wood" 10 (quads (fun i _ _ -> if i < 2 then Brick else Wood) (4, 1, 1))

(* The standing level as boxes: every solid cell in exactly one *)
let teardown_boxes () =
  let open TinyTeardown in
  let g = level () in
  let solid = Array.fold_left (fun n m -> if m <> Air then n + 1 else n) 0 g in
  let covered = List.fold_left (fun n ((i, j, k), (i2, j2, k2)) -> n + ((i2 - i) * (j2 - j) * (k2 - k))) 0 (boxes g) in
  Alcotest.(check int) "the boxes hold every solid cell once" solid covered;
  Alcotest.(check bool) "and far fewer boxes than cells" true (List.length (boxes g) * 20 < solid)

(* The hammer's ray: from the yard, straight at the house's south wall,
 * whose face is at z = -2 *)
let teardown_cast () =
  let open TinyTeardown in
  match cast (level ()) (0., 1., 5.) (0., 0., -1.) 20. with
  | Some ((_, j, k), t) ->
      Alcotest.(check bool) "7 metres" true (Float.abs (t -. 7.) < 1e-6);
      Alcotest.(check (pair int int)) "the wall's cell" (4, 31) (j, k)
  | None -> Alcotest.fail "the wall is there"

(* The water tower: its four legs knocked out, the tank is one loose
 * piece, and three seconds later it is down -- on the stumps of its
 * legs, what is left of them above the blows hanging under it -- and
 * the crate that was on it with it *)
let teardown_tower () =
  let open TinyTeardown in
  let legs = [ (3., 0.); (5.5, 0.); (3., 2.5); (5.5, 2.5) ] in
  let g = List.fold_left (fun g (x, z) -> blow g (x +. 0.25, 1., z +. 0.25)) (new_game ()) legs in
  Alcotest.(check int) "one piece came down" 1 g.broken;
  let g = ref g in
  for _ = 1 to 180 do g := { !g with world = Physics3d.simulate ~gravity:9.8 !g.world } done;
  let g = !g in
  let (tank : Physics3d.body) = List.hd (pieces g) and (crate : Physics3d.body) = List.nth (crates g) 2 in
  Alcotest.(check bool) "the tank down" true (tank.y < 2.2);
  Alcotest.(check bool) "the crate on it, not through it" true (crate.y > tank.y && crate.y < 2.8)

(* The heist, walls down beforehand: the tower's legs knocked out, then
 * the player at each crate in turn. The first one taken sets off the
 * alarm; with the three taken, the car is the way out. *)
let teardown_heist () =
  let open TinyTeardown in
  let legs = [ (3., 0.); (5.5, 0.); (3., 2.5); (5.5, 2.5) ] in
  let g = ref (List.fold_left (fun g (x, z) -> blow g (x +. 0.25, 1., z +. 0.25)) (new_game ()) legs) in
  for _ = 1 to 180 do g := { !g with world = Physics3d.simulate ~gravity:9.8 !g.world } done;
  Alcotest.(check bool) "no alarm yet" true (!g.alarm = None);
  List.iteri
    (fun n (x, _, z) ->
      g := take_loot { !g with player = { !g.player with x; z } };
      Alcotest.(check int) "one more crate taken" (2 - n) !g.n_crates;
      Alcotest.(check bool) "the alarm is on" true (!g.alarm <> None))
    loot_at;
  Alcotest.(check bool) "not away yet" false (at_car !g);
  Alcotest.(check bool) "at the car" true (at_car { !g with player = { !g.player with x = 7.; z = 7. } })

(*****************************************************************************)
(* TinyBattlezone *)
(*****************************************************************************)

(* The divide: straight ahead is the middle of the screen, and a point
 * twice as far is drawn twice as close to it; the near plane cuts a
 * segment going behind the eye where it crosses it *)
let battlezone_projection () =
  let open TinyBattlezone in
  let screen = Playground.to_screen 1000. 1000. in
  let eye = look (0., 1., 0.) (0., 1., -1.) in
  let at p = project screen (to_eye eye p) in
  let near_ x y = Float.abs (x -. y) < 1e-6 in
  let x, y = at (0., 1., -10.) in
  Alcotest.(check bool) "ahead: the middle" true (near_ x 0. && near_ y 0.);
  let x10, y10 = at (1., 0., -10.) and x20, y20 = at (1., 0., -20.) in
  Alcotest.(check bool) "twice as far, half as far out" true (near_ x10 (2. *. x20) && near_ y10 (2. *. y20));
  Alcotest.(check bool) "right is right, below is below" true (x10 > 0. && y10 < 0.);
  match clip (to_eye eye (0., 1., -5.), to_eye eye (0., 1., 5.)) with
  | Some ((_, _, z1), (_, _, z2)) -> Alcotest.(check bool) "cut at the near plane" true (z1 = 5. && near_ z2 near)
  | None -> Alcotest.fail "the part ahead is kept"

(* Shells fly at the height of a tank's hull, below the eye: at the eye's
 * height a shell is drawn on the horizon whatever its distance, above
 * the hull it goes through *)
let battlezone_shell_height () =
  let open TinyBattlezone in
  let ys = List.concat_map (fun ((_, y1, _), (_, y2, _)) -> [ y1; y2 ]) shell_edges in
  Alcotest.(check bool) "within the hull (0 to 0.6)" true (List.for_all (fun y -> y >= 0. && y <= 0.6) ys)

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

(* physics=engine: the same world, walked by the engine's capsule
 * (Character3d): it stands on the floor (its top at y = -1.5), walks
 * into the wall and stops a radius short of it (the wall's face at
 * z = -2.5), and a single block is a wall without a jump and a stair
 * with one, the step offset (0.6) being less than a block, as in
 * Minecraft *)
let minecraft_engine () =
  let open TinyMinecraft in
  let floor_and extra =
    let w = create () in
    for x = -5 to 5 do
      for z = -5 to 5 do
        add_block w (x, -2, z) Stone
      done;
      List.iter (fun (y, z) -> add_block w (x, y, z) Stone) extra
    done;
    w
  in
  let run w ?(jump = false) (seconds : float) =
    let input : TinyMinecraft.input = { forward = 1; right = 0; jump } in
    let p = { initial_player with position = (0., -1.5 +. eye, 0.) } in
    let rec loop n (c, p) = if n = 0 then (c, p) else loop (n - 1) (step_engine w input c p) in
    loop (int_of_float (seconds *. 60.)) (walker_of p, p)
  in
  let c, _ = run (floor_and [ (-1, -3); (0, -3) ]) 2. in
  Alcotest.(check (float 1e-3)) "standing on the floor" (-1.5) c.y;
  Alcotest.(check (float 1e-3)) "stopped by the wall, a radius short" (-2.2) c.z;
  let c, _ = run (floor_and [ (-1, -3) ]) 2. in
  Alcotest.(check (float 1e-3)) "a block, no jump: a wall" (-1.5) c.y;
  let c, _ = run (floor_and [ (-1, -3) ]) ~jump:true 2. in
  Alcotest.(check bool) "a block, jumping: on it, or beyond it" true (c.z < -3.)

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

(* physics=engine: the same keys, the same jump onto the same platform
 * (its top 2 m up), Mario's body moved by the engine's capsule
 * (Character3d) and the game feel still the game's *)
let mario64_engine () =
  let open TinyMario64 in
  let s = ref (Scene2d.start (Playing (new_level ~engine:Engine ()))) in
  for i = 2 to 180 do
    let keyboard =
      { initial_computer.keyboard with kspace = i >= 145 && i <= 165; kleft = i >= 2 && i <= 63; kup = i >= 64 && i <= 180 }
    in
    s := update (computer ~keyboard i) !s
  done;
  match !s.scene with
  | Playing l ->
      Alcotest.(check (float 1e-3)) "on the platform's top" 2. l.mario.y;
      Alcotest.(check bool) "on the ground" true l.mario.on_ground
  | _ -> Alcotest.fail "not playing"

(*****************************************************************************)
(* TinyPinball3d *)
(*****************************************************************************)

(* a game in play, the ball at (x, z) mm on the table moving (vx, vz)
 * m/s, the sweep on or off *)
let pinball3d_with ~continuous ((x, y) : number * number) ((vx, vz) : number * number) : TinyPinball3d.game =
  let open TinyPinball3d in
  let g = new_game () in
  with_ball { g with play = Live; continuous } (ball_body (to3 (x, y)) |> Physics3d.moving vx 0. vz)

let pinball3d_run (keyboard : keyboard) (frames : int) (g : TinyPinball3d.game) : TinyPinball3d.game =
  let open TinyPinball3d in
  let g = ref g in
  for i = 1 to frames do
    g := update_game (computer ~keyboard i) (Scene2d.start (Playing !g)) !g
  done;
  !g

(* A ball near the tip of the left flipper, a centimetre above its
 * face, and the flipper raised: the sweep catches the flipper *turning*
 * into the ball, and it is thrown up the table. Without the sweep, the
 * flipper's face goes past the ball between two steps -- out there it
 * moves 4.5 cm a frame, more than the ball's width and its own
 * together -- and throws nothing. (A ball *resting* on the flipper is
 * thrown either way: it is touching at the start of the step, and that
 * contact is the solver's; a first version of this test put it there
 * and measured the same throw twice.) *)
let pinball3d_flipper () =
  let open TinyPinball3d in
  (* 100 mm out from the pivot, and 8 + 13 + 10 mm above the face *)
  let near_tip = (-32. +. (0.47 *. 31.), -377. +. (0.88 *. 31.)) in
  let flip continuous =
    let g = pinball3d_with ~continuous near_tip (0., 0.) in
    let g = pinball3d_run { initial_computer.keyboard with kleft = true } 8 g in
    (ball g).vz
  in
  let swept = flip true and not_swept = flip false in
  Alcotest.(check bool) (Printf.sprintf "swept: thrown up the table (%.2f m/s)" swept) true (swept < -1.);
  Alcotest.(check bool) (Printf.sprintf "not swept: the flipper goes through it (%.2f m/s)" not_swept) true (not_swept > -0.5)

(* The ball shot at 3 m/s straight at the left wall (1 cm thick): kept
 * on the table with the sweep, and through the wall without, the
 * game's counter counting it *)
let pinball3d_wall () =
  let open TinyPinball3d in
  let shoot continuous = pinball3d_run initial_computer.keyboard 20 (pinball3d_with ~continuous (-200., 0.) (-3., 0.)) in
  let kept = shoot true and lost = shoot false in
  Alcotest.(check bool) "swept: still on the table" true ((ball kept).x > m left_x);
  Alcotest.(check bool) "not swept: through the wall" true ((ball lost).x < m left_x);
  Alcotest.(check bool) (Printf.sprintf "and counted (%d)" lost.through) true (lost.through > 0)

(*****************************************************************************)
(* TinyHalfLife2 *)
(*****************************************************************************)

let hl2_run ?(keys = []) (frames : int) (g : TinyHalfLife2.game) : TinyHalfLife2.game =
  let keyboard = { initial_computer.keyboard with keys = List.fold_left (fun s k -> Set_.add k s) Set_.empty keys } in
  let g = ref g in
  for i = 1 to frames do g := TinyHalfLife2.update_game (computer ~keyboard i) !g done;
  !g

(* The gravity gun: aimed at the bottom crate of the pile, z grabs it,
 * and it is soon held 1.8 m in front of the eye; x launches it at
 * 12 m/s *)
let hl2_gun () =
  let open TinyHalfLife2 in
  (* standing 3 m from the pile, looking down at its middle crate *)
  let g = { (new_game ()) with me = Character3d.make (-0.6) 0. (-5.); pitch = -.(atan2 1.3 3. *. 180. /. Float.pi) } in
  let g = hl2_run 1 g in
  let g = hl2_run ~keys:[ "z" ] 1 g in
  let held = match g.held with Some i -> i | None -> Alcotest.fail "the crate is grabbed" in
  let g = hl2_run 40 g in
  let ex, ey, ez = eye g and lx, ly, lz = look g in
  let b = nth g held in
  let off = Float.hypot (Float.hypot (b.x -. (ex +. (1.8 *. lx))) (b.y -. (ey +. (1.8 *. ly)))) (b.z -. (ez +. (1.8 *. lz))) in
  Alcotest.(check bool) (Printf.sprintf "held in front of the eye (%.2f m off)" off) true (off < 0.25);
  (* aimed up and away first: launched down at the pile it came from, a
   * first version of this test hit the other crates in the same step *)
  let g = hl2_run 20 { g with pitch = 20. } in
  let g = hl2_run ~keys:[ "x" ] 1 g in
  Alcotest.(check bool) "let go" true (g.held = None);
  let speed = Physics3d.speed (nth g held) in
  Alcotest.(check bool) (Printf.sprintf "launched (%.1f m/s)" speed) true (speed > 10. && speed < 13.)

(* A crate at 12 m/s into a zombie: it goes faster than it walks, and is
 * a ragdoll from then on, knocked out into the yard and lying there *)
let hl2_zombie () =
  let open TinyHalfLife2 in
  let g = new_game () in
  let z = nth g first_zombie in
  let thrown = List.length statics in
  let g = { g with world = set thrown (fun b -> b |> Physics3d.at (z.x -. 1.) 1. z.z |> Physics3d.moving 12. 0. 0.) g.world } in
  let g = hl2_run 120 g in
  Alcotest.(check bool) "the zombie is down" true (List.hd g.zombies = Fallen);
  (* the ragdoll's ten bodies are the last ones, its torso first *)
  let torso = List.nth g.world.bodies (List.length g.world.bodies - Ragdoll3d.count) in
  Alcotest.(check bool) (Printf.sprintf "a ragdoll, lying (its torso %.2f m up)" torso.y) true (torso.y < 0.4)

(* The barrels, half as dense as water, float half out of it: their
 * middles at the water's level, 0.7 m *)
let hl2_barrels () =
  let open TinyHalfLife2 in
  let g = hl2_run 240 (new_game ()) in
  let first_barrel = plank_index + 2 in
  List.iter
    (fun i ->
      let b = nth g i in
      Alcotest.(check bool) (Printf.sprintf "barrel %d afloat (%.2f m)" i b.y) true (Float.abs (b.y -. water) < 0.08))
    [ first_barrel; first_barrel + 1; first_barrel + 2 ]

(* The seesaw: a crate dropped on its high end sends the crate on its
 * low end up *)
let hl2_seesaw () =
  let open TinyHalfLife2 in
  let g = new_game () in
  let g = hl2_run 30 g in
  let dropped = List.length statics in
  let g = { g with world = set dropped (fun b -> b |> Physics3d.at (-2.8) 3. 3. |> Physics3d.moving 0. (-4.) 0.) g.world } in
  let fastest = ref 0. and g = ref g in
  for _ = 1 to 60 do
    g := hl2_run 1 !g;
    fastest := Float.max !fastest (nth !g (plank_index + 1)).vy
  done;
  Alcotest.(check bool) (Printf.sprintf "thrown up (%.1f m/s)" !fastest) true (!fastest > 1.)

(*****************************************************************************)
(* TinyPortal *)
(*****************************************************************************)

let portal3d_panel (c : number * number * number) : TinyPortal.panel =
  List.find (fun (p : TinyPortal.panel) -> Vec3.length (Vec3.sub p.centre c) < 1e-6) TinyPortal.panels

let portal3d_run ?(keys = []) (frames : int) (g : TinyPortal.game) : TinyPortal.game =
  let keyboard = { initial_computer.keyboard with kw = List.mem "w" keys } in
  let g = ref g in
  for i = 1 to frames do g := TinyPortal.update_game (computer ~keyboard i) !g done;
  !g

(* Speedy thing goes in: a portal in the floor 3 m ahead, one on the
 * wall above the ledge; walk into the floor, fall through it, come out
 * of the wall flung into the room, and land on the ledge 4 m up, which
 * no jump reaches *)
let portal3d_fling () =
  let open TinyPortal in
  let g = new_game () in
  let g =
    rebuild
      { g with blue = Some (portal_on g (portal3d_panel (1., 0., 3.))); orange = Some (portal_on g (portal3d_panel (1., 5., -8.)));
        me = Character3d.make 1. 0. 6. }
  in
  (* walking at it until through (up the wall), then letting go *)
  let g = ref g and n = ref 0 in
  while !g.me.y < 3. && !n < 180 do
    g := portal3d_run ~keys:[ "w" ] 1 !g;
    incr n
  done;
  Alcotest.(check bool) "through, and out of the wall" true (!g.me.y > 3.);
  Alcotest.(check bool) "flung out into the room" true (snd !g.fling > 1.);
  let g = portal3d_run 120 !g in
  Alcotest.(check bool) (Printf.sprintf "on the ledge (%.2f m up, z %.2f)" g.me.y g.me.z) true
    (Float.abs (g.me.y -. 4.) < 0.05 && g.me.z < -4. && g.me.grounded)

(* The cube on the button opens the door, and through the door is the
 * end of the chamber *)
let portal3d_door () =
  let open TinyPortal in
  let g = new_game () in
  let bx, bz = button_at in
  let cube = List.hd g.world.bodies |> Physics3d.at bx 0.3 bz in
  (* put there from outside the game, so the solids are worked out again
   * here: in play the cube lands on the button during an update, which
   * notices the door change and does it *)
  let g = portal3d_run 2 (rebuild { g with world = { g.world with bodies = cube :: List.tl g.world.bodies } }) in
  Alcotest.(check bool) "the door is open" true (door_open g);
  let g = { g with yaw = 180.; me = Character3d.make 0. 0. 6. } in
  let s = ref (Scene2d.start (Playing g)) in
  for i = 1 to 90 do
    s := update (computer ~keyboard:{ initial_computer.keyboard with kw = true } i) !s
  done;
  Alcotest.(check bool) "out through it: the chamber complete" true (match !s.scene with Complete _ -> true | _ -> false)

(*****************************************************************************)
(* TinyShufflePuck *)
(*****************************************************************************)

(* The projection's worked example: your end of the table fills the
 * bottom of the screen, 900 pixels wide; the centre line at -60; the
 * far end at 135, 450 pixels wide; and the mouse's inverse gives the
 * same point of the table back *)
let shufflepuck_view () =
  let open TinyShufflePuck in
  let near = Alcotest.(check (float 1e-6)) in
  let x, y, _ = project (0.5, 0., 0.) in
  near "your end, its right corner, across" 450. x;
  near "your end, along the bottom" (-450.) y;
  let _, y, _ = project (0., 1., 0.) in
  near "the centre line" (-60.) y;
  let x, y, _ = project (0.5, 2., 0.) in
  near "the far end" 135. y;
  near "half as wide" 225. x;
  let sx, sy, _ = project (0.3, 1.4, 0.) in
  let x, y = unproject (sx, sy) in
  near "back across" 0.3 x;
  near "back along" 1.4 y

let shufflepuck_rally ?(who = 1) (puck : TinyShufflePuck.puck) : TinyShufflePuck.rally =
  let open TinyShufflePuck in
  (* you out of the way, in a corner of your end *)
  { (new_rally who) with puck; you = { px = -0.4; py = 0.1; pvx = 0.; pvy = 0. } }

let shufflepuck_run (frames : int) (f : TinyShufflePuck.rally -> unit) (r : TinyShufflePuck.rally) : TinyShufflePuck.rally =
  let r = ref r in
  for i = 1 to frames do
    r := TinyShufflePuck.step_rally (computer i) !r;
    f !r
  done;
  !r

(* The rails hold a puck at 4 m/s, 7 cm a frame: it is moved in eight
 * steps a frame and bounced by the engine at each *)
let shufflepuck_rails () =
  let open TinyShufflePuck in
  let worst = ref 0. in
  ignore
    (shufflepuck_run 120
       (fun r -> worst := Float.max !worst (Float.abs r.puck.x))
       (shufflepuck_rally { x = 0.; y = 1.; vx = 4.; vy = 0.2 }));
  Alcotest.(check bool) (Printf.sprintf "never past a rail (%.3f m out at most)" !worst) true (!worst <= half_width)

(* A bank shot at the goal, off the right rail: Robo-9, who sees where
 * it will cross his line, is there; Nervous Ned, who follows where the
 * puck is, and late, is on the wrong side when it comes off the rail *)
let shufflepuck_block () =
  (* from (0.3, 0.6), at the goal's mirror image across the right rail *)
  let dx = (2. *. (TinyShufflePuck.half_width -. TinyShufflePuck.puck_r)) -. 0.3 and dy = 2. -. 0.6 in
  let d = Float.hypot dx dy in
  let shot = { TinyShufflePuck.x = 0.3; y = 0.6; vx = 3. *. dx /. d; vy = 3. *. dy /. d } in
  let robo = shufflepuck_run 150 ignore (shufflepuck_rally ~who:1 shot) in
  let ned = shufflepuck_run 150 ignore (shufflepuck_rally ~who:0 shot) in
  Alcotest.(check int) "Robo-9 stops it" 0 robo.mine;
  Alcotest.(check int) "Ned lets it in" 1 ned.mine

(* How they shoot: the puck left still on her side, Bank Betty sends it
 * at your goal off a rail; Robo-9 straight at it, touching none *)
let shufflepuck_aim () =
  let banks who =
    let still = { TinyShufflePuck.x = 0.1; y = 1.5; vx = 0.; vy = 0. } in
    let hit = ref false and railed = ref false in
    ignore
      (shufflepuck_run 180
         (fun r ->
           if r.puck.vy < -0.5 then hit := true;
           if !hit && r.puck.y > 0.5 && Float.abs r.puck.x > TinyShufflePuck.half_width -. TinyShufflePuck.puck_r -. 0.01 then
             railed := true)
         (shufflepuck_rally ~who still));
    (!hit, !railed)
  in
  let hit, railed = banks 2 in
  Alcotest.(check bool) "Betty hits it" true hit;
  Alcotest.(check bool) "off a rail" true railed;
  let hit, railed = banks 1 in
  Alcotest.(check bool) "Robo-9 hits it" true hit;
  Alcotest.(check bool) "straight, off no rail" false railed

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
 * speed, overshot the first plateau, and the one after the lane);
 * [start] is the model it starts from, [brake] how many frames ahead
 * it looks *)
let marble_robot_from ?(brake = 25.) (start : TinyMarbleMadness.model) =
  let open TinyMarbleMadness in
  let waypoints = [ (2.5, 7.); (7., 7.5); (12., 7.5); (12., 13.); (4.5, 13.); (4.5, 19.5); (5., 21.); (8., 22.8) ] in
  let s = ref start and todo = ref waypoints and broken = ref 0 and fallen = ref 0 and i = ref 0 in
  while !i < 60 * 45 && (match !s.scene with Finished _ | Time_up _ -> false | _ -> true) do
    incr i;
    let mouse =
      match (!s.scene, !todo) with
      | Racing r, (c, row) :: rest ->
          let tx = c *. cell and tz = row *. cell in
          if Float.hypot (tx -. r.me.x) (tz -. r.me.z) < 0.8 *. cell then todo := (if rest = [] then !todo else rest);
          let wx = (tx -. r.me.x) -. (brake *. r.me.vx) and wz = (tz -. r.me.z) -. (brake *. r.me.vz) in
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

let marble_robot () = marble_robot_from TinyMarbleMadness.initial_model

(* physics=engine: the same robot, the same waypoints, the engine
 * rolling the marble: the course can be won that way too -- braking
 * earlier, since the engine's marble leaves the crest of the long ramp
 * at speed and lands on the narrow plateau after it still going (at 25
 * frames ahead, it rolled off that plateau's far side) *)
let marble_engine_robot () =
  let open TinyMarbleMadness in
  marble_robot_from ~brake:40. (Scene2d.start (Racing (new_race ~engine:Engine ())))

(* physics=engine: the course as boxes is the course. A ray straight
 * down onto the engine's boxes meets the height [ground] reads, at a
 * quarter and three quarters across every tile, ramps and all (a ramp
 * tilted the wrong way round would be off by its whole drop) *)
let marble_engine_course () =
  let open TinyMarbleMadness in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      List.iter
        (fun (fx, fz) ->
          let x = (float_of_int c +. fx) *. cell and z = (float_of_int r +. fz) *. cell in
          match (ground x z, Physics3d.ray ~from:(x, 20., z) ~direction:(0., -1., 0.) course_bodies) with
          | None, None -> ()
          | Some g, Some (_, d) -> Alcotest.(check (float 1e-6)) (Printf.sprintf "tile (%d, %d)" c r) g (20. -. d)
          | _ -> Alcotest.fail (Printf.sprintf "tile (%d, %d): a box where there is no tile, or none where there is" c r))
        [ (0.25, 0.25); (0.75, 0.75); (0.25, 0.75) ]
    done
  done

(* physics=engine: the 5/7 comes out by itself. Let go on the first
 * ramp (a slope of 1 in 2), with no push and no drag, the engine's
 * marble gains speed down it at 5/7 g sin(a) -- the number [step]
 * writes down, and which nothing on the engine's side mentions *)
let marble_engine_five_sevenths () =
  let open TinyMarbleMadness in
  let start = { (ball_at (2, 3)) with z = (4. *. cell) +. 0.6 } in
  let start = { start with y = Option.get (ground start.x start.z) } in
  let w = ref (Physics3d.world (course_bodies @ [ marble_body 1. start; marble_body 2. (ball_at steelie_home) ])) in
  let speed () = let b, _ = engine_bodies !w in Physics3d.speed b in
  for _ = 1 to 5 do w := Physics3d.simulate ~gravity:engine_gravity ~sleeping:false !w done;
  let v0 = speed () in
  for _ = 1 to 15 do w := Physics3d.simulate ~gravity:engine_gravity ~sleeping:false !w done;
  let measured = (speed () -. v0) /. (15. /. 60.) in
  let expected = 5. /. 7. *. engine_gravity *. (0.5 /. Float.hypot 1. 0.5) in
  Alcotest.(check bool) "5/7 g sin(a), within 3%" true (Float.abs (measured -. expected) < 0.03 *. expected)

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
(* TinySonic (gamekits/platformer's Slope) *)
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
(* AiChess (an example, ai/'s Minimax) *)
(*****************************************************************************)

(* perft: every position 1, 2, 3 moves ahead, counted, against the
 * numbers every chess programmer checks theirs with (the Chess
 * Programming Wiki's "Perft Results") *)
let chess_perft () =
  let open AiChess in
  let check name fen counts =
    let p = of_fen fen in
    List.iteri (fun i n -> Alcotest.(check int) (Printf.sprintf "%s, %d moves ahead" name (i + 1)) n (perft p (i + 1))) counts
  in
  check "the start" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" [ 20; 400; 8902 ];
  (* castling both ways for both, pins, en passant, promotions *)
  check "Kiwipete" "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" [ 48; 2039 ];
  (* an endgame where en passant can uncover a check on its own king *)
  check "position 3" "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1" [ 14; 191; 2812 ];
  (* promotions that capture, white in check *)
  check "position 4" "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1" [ 6; 264; 9467 ]

(* the three moves that do more than move a piece *)
let chess_special_moves () =
  let open AiChess in
  let mv from dest = { from; dest; promotion = None } in
  (* e5xd6 en passant: the pawn taken is on d5 *)
  let p = of_fen "4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1" in
  Alcotest.(check bool) "e5xd6 is legal" true (List.mem (mv 28 19) (legal p));
  let q = play p (mv 28 19) in
  Alcotest.(check bool) "d5 is empty" true (q.board.(27) = None);
  (* e1-g1: the rook jumps to f1, and neither may castle again *)
  let p = of_fen "4k3/8/8/8/8/8/8/R3K2R w KQ - 0 1" in
  let q = play p (mv 60 62) in
  Alcotest.(check bool) "the rook on f1" true (q.board.(61) = Some { color = White; kind = Rook } && q.board.(63) = None);
  Alcotest.(check bool) "no more castling" false (q.white_short || q.white_long);
  (* a7-a8: four moves, one per piece *)
  let p = of_fen "4k3/P7/8/8/8/8/8/4K3 w - - 0 1" in
  Alcotest.(check int) "four promotions" 4 (List.length (List.filter (fun m -> m.from = 8) (legal p)));
  Alcotest.(check bool) "a queen" true ((play p { from = 8; dest = 0; promotion = Some Queen }).board.(0) = Some { color = White; kind = Queen })

(* the search: mate in one for either side, and a queen left hanging *)
let chess_search () =
  let open AiChess in
  let best fen = Option.get (search ~ordered:true ~quiescence:true ~depth (of_fen fen)).best in
  let m = best "6k1/5ppp/8/8/8/8/5PPP/R5K1 w - - 0 1" in
  Alcotest.(check (pair int int)) "white: Ra8 mate" (56, 0) (m.from, m.dest);
  let m = best "r5k1/5ppp/8/8/8/8/5PPP/6K1 b - - 0 1" in
  Alcotest.(check (pair int int)) "black: Ra1 mate" (0, 56) (m.from, m.dest);
  let m = best "4k3/8/2n5/8/1Q6/8/8/4K3 b - - 0 1" in
  Alcotest.(check (pair int int)) "the knight takes the queen" (18, 33) (m.from, m.dest)

(* the horizon effect: one move ahead, Qxe5 wins a pawn -- unless the
 * search goes on to see d6xe5 *)
let chess_quiescence () =
  let open AiChess in
  let p = of_fen "4k3/8/3p4/4p3/8/8/8/4QK2 w - - 0 1" in
  let best quiescence = Option.get (search ~ordered:true ~quiescence ~depth:1 p).best in
  let takes m = m.from = 60 && m.dest = 28 in
  Alcotest.(check bool) "without quiescence, Qxe5" true (takes (best false));
  Alcotest.(check bool) "with it, not" false (takes (best true))

(* move ordering: the same value, fewer positions *)
let chess_ordering () =
  let open AiChess in
  let p = of_fen "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" in
  let o = search ~ordered:true ~quiescence:false ~depth p and u = search ~ordered:false ~quiescence:false ~depth p in
  Printf.printf "ordered: %d positions, unordered: %d\n" o.nodes u.nodes;
  Alcotest.(check (float 0.)) "the same value" u.value o.value;
  Alcotest.(check bool) "fewer positions" true (o.nodes < u.nodes)

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
  (* every cell the hero can walk to from where it stands: one flow
     field from where it stands, and a cell is on it or it is not *)
  let reaches (g : game) (cell : int * int) = Ai.steps_to_go (Ai.flow ~walkable:(open_cell g) (g.x, g.y)) cell <> None in
  let one (c : char) (map : Tilemap.t) =
    match Tilemap.find map c with [ cell ] -> cell | _ -> Alcotest.failf "not one %c in the dungeon" c
  in
  let g = new_game () in
  let key = one 'k' g.map and stairs = one '>' g.map in
  Alcotest.(check bool) "the key is reachable" true (reaches g key);
  Alcotest.(check bool) "the stairs are not" false (reaches g stairs);
  (* at the door, with the key *)
  let g = hand { g with x = 6; y = 9; facing = North; keys = 1 } in
  Alcotest.(check bool) "the key opened the door" false (wall (Tilemap.get g.map 6 8));
  Alcotest.(check int) "and was used up" 0 g.keys;
  Alcotest.(check bool) "the stairs are still shut in" false (reaches g stairs);
  (* at the lever, beyond it *)
  let g = hand { g with x = 12; y = 7; facing = North } in
  Alcotest.(check (option char)) "the lever stays pulled" (Some 'l') (Tilemap.get g.map 12 6);
  Alcotest.(check bool) "and the stairs can be reached" true (reaches g stairs)

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
 * centre and no rounding (see the header of TinyBlockout.ml).
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
 * TinyTombRaider.ml). Each move is asked for and must be
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
(* TinyZeldaLinkPast *)
(*****************************************************************************)

(* the three-quarter view's one sort: a tree's trunk on row 5, Link a
 * cell north of it (under its canopy) is drawn before it, hidden; a
 * cell south, after it, in front *)
let lttp_behind_tree () =
  let open TinyZeldaLinkPast in
  let tx, ty = Tilemap.center level 8 5 in
  let tree_base = ty -. (tile /. 2.) in
  let tree = standing 40 (circle green 10.) tx tree_base in
  let link y = standing 0 (circle red 10.) tx (y -. 12.) in
  let north = link (ty +. tile) and south = link (ty -. tile) in
  Alcotest.(check bool) "north: Link first" true (draw_standing [ tree; north ] = [ north.picture; tree.picture ]);
  Alcotest.(check bool) "south: the tree first" true (draw_standing [ south; tree ] = [ tree.picture; south.picture ])

(* Link in cell (col, row), facing [facing], in a game started *)
let lttp_at (col : int) (row : int) (facing : number * number) : TinyZeldaLinkPast.model =
  let open TinyZeldaLinkPast in
  let x, y = Tilemap.center level col row in
  let g = new_game () in
  Scene2d.go (Playing { g with x; y; facing; cam = camera x y g.cam }) initial_model

(* [lttp_play s n]: [n] frames, space pressed at the first *)
let lttp_play (s : TinyZeldaLinkPast.model) (n : int) : TinyZeldaLinkPast.model =
  let s = ref s in
  for i = 1 to n do
    s := TinyZeldaLinkPast.update (computer ~keyboard:{ initial_computer.keyboard with kspace = i = 1 } i) !s
  done;
  !s

(* the bushes before the bridge: one cut by a swing, the way across *)
let lttp_bush () =
  let open TinyZeldaLinkPast in
  Alcotest.(check bool) "a bush there" true (Tilemap.get level 15 24 = Some '*');
  match (lttp_play (lttp_at 15 25 (0., 1.)) 20).scene with
  | Playing g -> Alcotest.(check bool) "the bush above cut" true (Tilemap.get g.map 15 24 <> Some '*')
  | _ -> Alcotest.fail "not playing"

(* the pedestal: the sword stays in it without the three pendants, and
 * comes with them *)
let lttp_pedestal () =
  let open TinyZeldaLinkPast in
  let s = lttp_at 19 3 (0., 1.) in
  (match (lttp_play s 2).scene with Playing _ -> () | _ -> Alcotest.fail "the sword came without the pendants");
  let s = match s.scene with Playing g -> { s with scene = Playing { g with pendants = [ '1'; '2'; '3' ] } } | _ -> s in
  match (lttp_play s 2).scene with Won _ -> () | _ -> Alcotest.fail "the sword stayed, with the three pendants"

(*****************************************************************************)
(* TinyZeldaOcarina *)
(*****************************************************************************)

(* in the temple's room, just come in by its south door *)
let ocarina_room () : TinyZeldaOcarina.play =
  let open TinyZeldaOcarina in
  let p = new_play () in
  { p with area = Dungeon; link = { p.link with x = 0.; z = half -. 0.8; heading = 0. } }

(* Link at [d] in front of the Stalfos (which stands at the room's
 * middle, facing +z, towards him), facing it *)
let ocarina_facing (d : number) : TinyZeldaOcarina.play =
  let open TinyZeldaOcarina in
  let p = ocarina_room () in
  { p with
    link = { p.link with x = 0.; z = d; heading = 0. };
    stalfos = { p.stalfos with body = { p.stalfos.body with x = 0.; z = 0.; heading = 180. } } }

(* Locked on, a key held sideways is an orbit: the distance stays what
 * it was, and Link faces the Stalfos all the way round. Unlocked, the
 * same key runs him off along the camera's right, facing where he
 * goes. *)
let ocarina_orbit () =
  let open TinyZeldaOcarina in
  let p = ocarina_facing 2.5 in
  let foe = p.stalfos.body in
  let right = { fwd = 0.; side = 1. } in
  let locked = ref p in
  for _ = 1 to 60 do
    locked := { !locked with link = move_link true (Some foe) right !locked }
  done;
  let l = !locked in
  Alcotest.(check bool) "locked: still at arm's length" true (Float.abs (dist l.link foe -. 2.5) < 1e-6);
  Alcotest.(check bool) "and facing it" true (off_facing l.link foe < 1e-6);
  (* 60 steps of 0.11 at 2.5: 2.64 radians, 151 degrees round *)
  Alcotest.(check bool) "having gone most of the way to its back" true (Float.abs (diff l.link.heading 0.) > 140.);
  let free = ref p in
  for _ = 1 to 60 do
    free := { !free with link = move_link false None right !free }
  done;
  let f = !free in
  Alcotest.(check bool) "free: further off" true (dist f.link foe > 5.);
  Alcotest.(check bool) "and facing away from it" true (off_facing f.link foe > 60.)

(* the shield: a slash from the front clangs off it; from behind, or
 * while the Stalfos is bent over after its chop, it lands *)
let ocarina_shield () =
  let open TinyZeldaOcarina in
  let p = ocarina_facing 2. in
  let s = p.stalfos in
  Alcotest.(check bool) "from the front: blocked" true (strike p s = Blocked);
  let behind = { s with body = { s.body with heading = 0. } } in
  Alcotest.(check bool) "from behind: a hit" true (strike p behind = Hit);
  let after_chop = { s with state = Chop (chop.startup + chop.active + 5) } in
  Alcotest.(check bool) "after its chop: a hit" true (strike p after_chop = Hit);
  let far = ocarina_facing 4. in
  Alcotest.(check bool) "out of reach: nothing" true (strike far behind = Miss)

(* The fight is two numbers: circling at arm's length, locked on, Link
 * goes round faster than the Stalfos turns, so its back comes round.
 * Played through the whole update, the Stalfos stalking and chopping. *)
let ocarina_circle_to_its_back () =
  let open TinyZeldaOcarina in
  let keys = { initial_computer.keyboard with kright = true; keys = Set_.of_list [ "z" ] } in
  let p = ref (ocarina_facing 2.5) in
  let reached = ref false in
  for _ = 1 to 150 do
    p := step_play keys false !p;
    if !p.target <> None && off_facing !p.stalfos.body !p.link > 90. then reached := true
  done;
  Alcotest.(check bool) "behind it, still locked on" true !reached

(* The room can be won with the lock-on alone: hold z, circle, and
 * slash when the Stalfos' back is towards you. Played through the
 * whole update from the south door, until the bars are up. *)
let ocarina_won_by_circling () =
  let open TinyZeldaOcarina in
  let p = ref (ocarina_room ()) in
  let frames = ref 0 in
  while !frames < 3000 && not (door_open !p) do
    let q = !p in
    let close = dist q.link q.stalfos.body < reach in
    let keys =
      { initial_computer.keyboard with kup = not close; kright = close; keys = Set_.of_list [ "z" ] }
    in
    let swing = close && q.swing = 0 && off_facing q.stalfos.body q.link > 90. in
    p := step_play keys swing q;
    incr frames
  done;
  Alcotest.(check bool) "the bars are up" true (door_open !p);
  Alcotest.(check bool) "and Link is alive" true (!p.hearts > 0)

(* locking on reaches 10, and once locked it holds to 16 *)
let ocarina_lock_ranges () =
  let open TinyZeldaOcarina in
  let p = ocarina_facing 12. in
  Alcotest.(check bool) "at 12, it does not lock" true (lock_on true p = None);
  Alcotest.(check bool) "but a lock held from nearer holds" true (lock_on true { p with target = Some 0 } = Some 0);
  Alcotest.(check bool) "and z let go lets go" true (lock_on false { p with target = Some 0 } = None)

(* The temple's door is a loading zone: walking into it on the field
 * puts Link at the room's south door, behind a moment of black during
 * which nothing moves; walking back out of that door puts him on the
 * field again, before the temple. Two places, two coordinates, and a
 * pair of positions for a door. *)
let ocarina_loading_zone () =
  let open TinyZeldaOcarina in
  let walk (keys : keyboard) (p : play) (until : play -> bool) : play =
    let p = ref p and n = ref 0 in
    while !n < 200 && not (until !p) do
      p := step_play keys false !p;
      incr n
    done;
    !p
  in
  let up = { initial_computer.keyboard with kup = true } in
  let p = new_play () in
  let p = { p with link = { p.link with x = 0.; z = temple_z +. 3.; heading = 0. } } in
  let inside = walk up p (fun p -> p.area = Dungeon) in
  Alcotest.(check bool) "in the room" true (inside.area = Dungeon);
  Alcotest.(check bool) "at its south door" true (Float.abs (inside.link.z -. (half -. 0.8)) < 1e-6);
  let loaded = step_play up false inside in
  Alcotest.(check bool) "while it loads, Link does not move" true (loaded.link = inside.link);
  let down = { initial_computer.keyboard with kdown = true } in
  let out = walk down inside (fun p -> p.area = Field) in
  Alcotest.(check bool) "back on the field" true (out.area = Field);
  Alcotest.(check bool) "before the temple" true (Float.abs (out.link.z -. (temple_z +. 2.5)) < 1e-6)

(* The clock runs on the field only; at night Stalchildren climb out
 * of the ground, and at dawn they sink back and are gone. *)
let ocarina_night () =
  let open TinyZeldaOcarina in
  let still = initial_computer.keyboard in
  let run (n : int) (p : play) : play =
    let p = ref p in
    for _ = 1 to n do
      p := step_play still false !p
    done;
    !p
  in
  let room = run 100 (ocarina_room ()) in
  Alcotest.(check bool) "in the room, time stands still" true (room.clock = (new_play ()).clock);
  let dusk = { (new_play ()) with clock = 0.5 } in
  let night = run 240 dusk in
  Alcotest.(check bool) "at night, they come" true (night.stalchildren <> []);
  (* a frame before dawn (a frame is 1/3600 of the day) *)
  let dawn = run 2 { night with clock = 0.9499 } in
  Alcotest.(check bool) "at dawn, they sink" true
    (List.for_all (fun (f : foe) -> match f.state with Dying _ -> true | _ -> false) dawn.stalchildren);
  Alcotest.(check bool) "and are gone" true ((run 70 dawn).stalchildren = [])

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
(* TinyBoomerangFu *)
(*****************************************************************************)

(* The one rule the whole game hangs on: a thrown boomerang comes back
 * to where its owner *is*, not to where the throw started. So: throw
 * north, then walk east the whole time it is away, and it still finds
 * you. *)
let boomerang_returns () =
  let open TinyBoomerangFu in
  let me = List.hd (new_game ()).players in
  let me, thrown = step_player garden { idle with go = Some (0., -1.); throw = true } me in
  let r = match thrown with Some r -> r | None -> Alcotest.fail "nothing left the hand" in
  Alcotest.(check bool) "the hand is empty" false me.holds;
  let from_x = me.px in
  let rec fly n me rangs =
    if n = 0 then (me, rangs)
    else
      let me, _ = step_player garden { idle with go = Some (1., 0.) } me in
      let rangs, caught = step_rangs garden [ me ] rangs in
      if List.mem me.idx caught then ({ me with holds = true }, rangs) else fly (n - 1) me rangs
  in
  let me, rangs = fly 200 me [ r ] in
  Alcotest.(check bool) "it is caught again" true me.holds;
  Alcotest.(check int) "and nothing is left in the air" 0 (List.length rangs);
  Alcotest.(check bool) "having chased a thrower who kept moving" true (me.px -. from_x > 5.)

(* Who a flight cuts, which is three rules in one function ([cuts]):
 * everyone on the way out, nobody but its owner's enemies on the way
 * back, and its own thrower only once it has both got away and come off
 * something. And only at the height of a body: a jump clears one. *)
let boomerang_cuts () =
  let open TinyBoomerangFu in
  let g = new_game () in
  let me = List.nth g.players 0 and you = List.nth g.players 1 in
  let flight = { rx = 0.; ry = rang_height; rz = 0.; rvx = 0.5; rvz = 0.; owner = me.idx; leg = Out; bounced = false; away = false; age = 10 } in
  let over (p : player) (r : rang) = { r with rx = p.px; rz = p.pz } in
  let n rangs players = List.length (cuts players rangs) in
  Alcotest.(check int) "a throw does not cut the one who threw it" 0 (n [ over me flight ] [ me ]);
  Alcotest.(check int) "unless it has got away and come off the fence" 1
    (n [ over me { flight with bounced = true; away = true } ] [ me ]);
  Alcotest.(check int) "the way back is a catch, never a cut" 0
    (n [ over me { flight with leg = Back; bounced = true; away = true } ] [ me ]);
  Alcotest.(check int) "but it cuts anyone else, either way round" 2
    (n [ over you flight; over you { flight with leg = Back } ] [ me; you ]);
  Alcotest.(check int) "unless they are in the top of a jump" 0 (n [ over you flight ] [ { you with py = 1.2 } ])

(* The other half of the trade: holding the boomerang, the dash is a
 * slash; without it, the same dash is only a dodge. *)
let boomerang_slash () =
  let open TinyBoomerangFu in
  let g = new_game () in
  let me = { (List.nth g.players 0) with px = 0.; pz = 0.; dash = 5 } in
  let you = { (List.nth g.players 1) with px = 0.9; pz = 0. } in
  Alcotest.(check int) "a dash with it in hand cuts" 1 (List.length (cuts [ me; you ] []));
  Alcotest.(check int) "the same dash, unarmed, does not" 0 (List.length (cuts [ { me with holds = false }; you ] []));
  Alcotest.(check int) "and it has no reach" 0 (List.length (cuts [ me; { you with px = 3.5 } ] []));
  Alcotest.(check int) "standing still with it in hand does not either" 0
    (List.length (cuts [ { me with dash = 0 }; you ] []))

(* The holes swallow whoever walks into one, and a jump carries you
 * over: the garden's hole at (7, -7) is one cell, two units, and a
 * jump flies 3.7. The terrace is a cliff from the grass, and a jump
 * gets you up onto it. *)
let boomerang_holes () =
  let open TinyBoomerangFu in
  let me = { (List.hd (new_game ()).players) with px = 3.; pz = -7. } in
  let rec walk ?(jump_at = Float.infinity) n p =
    if n = 0 || not (alive p) then p
    else walk ~jump_at (n - 1) (fst (step_player garden { idle with go = Some (1., 0.); jump = p.px >= jump_at && p.px < jump_at +. 0.2 } p))
  in
  Alcotest.(check bool) "walk into a hole and you fall" true (match (walk 60 me).state with Falling _ -> true | _ -> false);
  let over = walk ~jump_at:5.4 60 me in
  Alcotest.(check bool) "jump, and you are over it" true (alive over && over.px > 8.);
  (* west of the terrace, at (-7, 3) (the steps onto it are north, at
     (-3, -5)), walking east into its cliff *)
  let low = { me with px = -7.; pz = 3. } in
  let stuck = walk 60 low in
  Alcotest.(check bool) "the cliff stops you" true (stuck.px < -6. && stuck.py = 0.);
  let up = walk ~jump_at:(-6.8) 60 low in
  Alcotest.(check bool) "a jump puts you on the terrace" true (up.px > -5. && up.py = 1.)

(* The river is crossed by its bridges, which the computer finds with
 * A* ([route]): walking from one bank to the other the way it closes
 * in on its man ([approach]), it gets there dry *)
let boomerang_bridge () =
  let open TinyBoomerangFu in
  let me = { (List.hd (new_game ~arena_no:1 ()).players) with think = 0 } in
  let there = (9., -9.) in
  Alcotest.(check bool) "no straight way across" false (snd (route river (me.px, me.pz) there));
  let rec walk n p =
    if n = 0 || not (alive p) || d2 p.px p.pz (fst there) (snd there) < 1. then p
    else
      let way, ahead = approach river (p.px, p.pz) there 0. in
      walk (n - 1) (fst (step_player river { idle with go = Some (clear_way river p ?ahead way) } p))
  in
  let p = walk 600 me in
  Alcotest.(check bool) "it did not fall in" true (alive p);
  Alcotest.(check bool) "and it is across" true (d2 p.px p.pz (fst there) (snd there) < 1.)

(* the computer, which looks where it is going ([way_ok]), and whose
 * feet do so every frame ([footing]), must not fall: a round played
 * out with nobody at the keyboard, on either arena and at every level,
 * ends with nobody in a hole, off an edge or in the water *)
let boomerang_pits () =
  let open TinyBoomerangFu in
  List.iter
    (fun level ->
      List.iter
        (fun arena_no ->
          let g = ref (new_game ~level ~arena_no ()) in
          while !g.ended = None do
            g := step_game initial_model initial_computer.keyboard !g
          done;
          Alcotest.(check int) "the computer kept out of the holes" 0
            (List.length (List.filter (fun p -> match p.state with Falling _ -> true | _ -> false) !g.players)))
        [ 0; 1 ])
    [ Easy; Normal; Hard ]

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

(*****************************************************************************)
(* TinyJoust *)
(*****************************************************************************)

(* the scene wrapper a play needs, for the flap's rising edge *)
let joust_scenes (p : TinyJoust.play) : TinyJoust.model = Scene2d.start (TinyJoust.Playing p)

(* [n] frames of the play alone, no scene changes, the keyboard of
 * frame i given by [keyboard] *)
let joust_play ?(keyboard = fun _ -> initial_computer.keyboard) (n : int) (p : TinyJoust.play) : TinyJoust.play =
  let p = ref p and m = ref (joust_scenes p) in
  for i = 1 to n do
    let c = computer ~keyboard:(keyboard i) i in
    m := Scene2d.update c !m;
    p := TinyJoust.update_play c !m !p
  done;
  !p

(* The one rule of the game, both ways round: of two riders who touch,
 * the higher one wins, and the loser leaves an egg. *)
let joust_higher_wins () =
  let open TinyJoust in
  let meeting (dy : number) =
    let p = { (start ()) with player = { (flyer Player 0 0. dy) with age = 1 }; others = [ flyer Buzzard 0 0. 0. ] } in
    joust_play 1 p
  in
  let won = meeting 20. in
  Alcotest.(check int) "the higher lance scores" 500 won.score;
  Alcotest.(check int) "and keeps its lives" 3 won.lives;
  (match won.others with
  | [ e ] -> Alcotest.(check bool) "the loser left an egg, a tier up" true (e.role = Egg && e.tier = 1)
  | l -> Alcotest.fail (Printf.sprintf "expected one egg, got %d" (List.length l)));
  let lost = meeting (-20.) in
  Alcotest.(check int) "the lower one pays a life" 2 lost.lives;
  Alcotest.(check int) "and scores nothing" 0 lost.score;
  Alcotest.(check bool) "the buzzard flies on" true (List.exists (fun (f : flyer) -> f.role = Buzzard) lost.others);
  (* level with each other, neither wins: the engine bounces them apart *)
  let level = meeting 4. in
  Alcotest.(check int) "level: no score" 0 level.score;
  Alcotest.(check int) "level: no life lost" 3 level.lives

(* A flap is a key *press*, not a key held: holding the button gives
 * one flap and then nothing, hammering it climbs. That is the whole
 * feel of the game, and it comes from Scene2d.pressed. *)
let joust_flap_is_a_press () =
  let open TinyJoust in
  let k = initial_computer.keyboard in
  let run held =
    (* a buzzard in the far corner keeps the wave alive without ever
     * reaching him in 40 frames *)
    let p = { (start ()) with others = [ flyer Buzzard 0 400. 350. ] } in
    let after = joust_play 40 ~keyboard:(fun i -> if held i then { k with kspace = true } else k) p in
    after.player.b.y
  in
  let hammered = run (fun i -> i mod 6 = 0) and holding = run (fun _ -> true) and still = run (fun _ -> false) in
  Printf.eprintf "DBG joust flap: hammered %.0f, holding %.0f, still %.0f\n%!" hammered holding still;
  Alcotest.(check bool) "hammering climbs" true (hammered > still +. 150.);
  Alcotest.(check bool) "holding is one flap" true (holding < hammered -. 100.)

(* No collision code in the game at all: one Physics.bounce_all does
 * every bird against every ledge. A bird dropped over one stays on it. *)
let joust_ledges_hold () =
  let open TinyJoust in
  let p = { (start ()) with player = flyer Player 0 (-300.) 200.; others = [ flyer Buzzard 0 400. 350. ] } in
  let after = joust_play 120 p in
  Printf.eprintf "DBG joust ledge: y %.1f\n%!" after.player.b.y;
  Alcotest.(check int) "no life lost on the way down" 3 after.lives;
  (* the first ledge under him is the middle-left one, whose top is at
   * -138: he rests on it half his own height above, and not a pixel
   * of that is written in the game *)
  Alcotest.(check bool) "he came to rest on the ledge" true (after.player.b.y > -132. && after.player.b.y < -118.)

(* An egg left alone hatches, and what comes out is a tier faster than
 * what laid it. *)
let joust_egg_hatches () =
  let open TinyJoust in
  let p = { (start ()) with player = flyer Player 0 (-400.) 300.; others = [ flyer Egg 1 250. (-100.) ] } in
  let just_before = joust_play (hatch_after - 10) p in
  (match just_before.others with
  | [ e ] -> Alcotest.(check bool) "still in the shell" true (e.role = Egg)
  | _ -> Alcotest.fail "the egg went");
  let after = joust_play (hatch_after + 4) p in
  match after.others with
  | [ f ] -> Alcotest.(check bool) "hatched, a tier up" true (f.role = Buzzard && f.tier = 1)
  | l -> Alcotest.fail (Printf.sprintf "expected one buzzard, got %d" (List.length l))

(* Collected instead, it pays 250 -- and with the wave cleared, the
 * next one is laid, one buzzard bigger. *)
let joust_egg_collected () =
  let open TinyJoust in
  let p = { (start ()) with player = flyer Player 0 250. (-100.); others = [ flyer Egg 1 250. (-100.) ] } in
  let after = joust_play 1 p in
  Alcotest.(check int) "the egg pays" 250 after.score;
  Alcotest.(check int) "the wave was cleared" 2 after.wave;
  Alcotest.(check int) "and the next is laid" 4 (List.length after.others)

(* Under everything is the lava, and it keeps what falls in it. *)
let joust_lava () =
  let open TinyJoust in
  let p = { (start ()) with player = flyer Player 0 0. (-300.); others = [ flyer Buzzard 0 400. 350. ] } in
  let after = joust_play 60 p in
  Alcotest.(check int) "over the pit, with nothing under him" 2 after.lives;
  Alcotest.(check bool) "and back on his own ledge" true (after.dead > 0 && after.player.b.y > lava_top)

(*****************************************************************************)
(* TinyDefender *)
(*****************************************************************************)

let defender_play ?(keyboard = fun _ -> initial_computer.keyboard) (n : int) (p : TinyDefender.play) : TinyDefender.play =
  let p = ref p and m = ref (Scene2d.start (TinyDefender.Playing p)) in
  for i = 1 to n do
    let c = computer ~keyboard:(keyboard i) i in
    m := Scene2d.update c !m;
    p := TinyDefender.update_play c !m !p
  done;
  !p

(* a play with the ship parked at (x, y), and nothing else moving *)
let defender_at (x : number) (y : number) (p : TinyDefender.play) : TinyDefender.play =
  { p with ship = { p.ship with b = p.ship.b |> Physics.at x y }; cam = x }

(* The planet is a cylinder, and [near] is the only place that knows:
 * two things either side of the seam are next to each other. *)
let defender_cylinder () =
  let open TinyDefender in
  Alcotest.(check (float 0.01)) "across the seam, the short way" 20. (apart (10., 0.) (world_w -. 10., 0.));
  Alcotest.(check (float 0.01)) "and the copy to draw" (world_w +. 100.) (near (world_w -. 100.) 100.);
  (* flying right off the end comes back at the start, and the camera
   * goes with it rather than sweeping the whole planet backwards *)
  let k = initial_computer.keyboard in
  let p = defender_at (world_w -. 60.) 100. (start ()) in
  let after = defender_play 60 ~keyboard:(fun _ -> { k with kright = true }) p in
  Alcotest.(check bool) "the ship came round" true (after.ship.b.x < 400.);
  Alcotest.(check bool) "the camera came with it" true (apart (after.cam, 0.) (after.ship.b.x, 0.) < 400.)

(* A lander finds the nearest human, carries him to the top, and is a
 * mutant from then on: the abduction you miss is the enemy you will
 * have to fight. *)
let defender_abduction () =
  let open TinyDefender in
  let human = List.nth humans_start 0 in
  let p = { (defender_at 3000. 100. (start ())) with enemies = [ { ex = human.hx; ey = 140.; kind = Lander; holds = None; cool = 100000 } ] } in
  let grabbing = defender_play 260 p in
  Alcotest.(check bool) "he has him" true ((List.nth grabbing.humans 0).hstate = Grabbed);
  Alcotest.(check bool) "and is on his way up" true (List.for_all (fun (e : enemy) -> e.holds = Some 0) grabbing.enemies);
  let after = defender_play 620 p in
  Alcotest.(check int) "one human short" 9 (List.length (List.filter alive after.humans));
  Alcotest.(check bool) "and a mutant instead" true (List.for_all (fun (e : enemy) -> e.kind = Mutant) after.enemies);
  Alcotest.(check bool) "which now wants the ship" true
    (apart (after.ship.b.x, after.ship.b.y) ((List.hd after.enemies).ex, (List.hd after.enemies).ey)
     < apart (after.ship.b.x, after.ship.b.y) (human.hx, 270.))

(* Catching one is the game: fly into him as he falls, take him down,
 * and the ground gives you 500. *)
let defender_rescue () =
  let open TinyDefender in
  let h = List.nth humans_start 3 in
  let x = h.hx in
  let falling = List.mapi (fun i (hu : human) -> if i = 3 then { hu with hy = ground x +. 60.; hstate = Falling } else hu) humans_start in
  let p = { (defender_at x (ground x +. 60.) (start ())) with humans = falling; enemies = [] } in
  let caught = defender_play 1 p in
  Alcotest.(check bool) "caught in the air" true (caught.carried = Some 3 && (List.nth caught.humans 3).hstate = Held);
  let k = initial_computer.keyboard in
  let home = defender_play 25 ~keyboard:(fun _ -> { k with kdown = true }) p in
  Alcotest.(check int) "and flown down to the ground" 500 home.score;
  Alcotest.(check bool) "standing again" true ((List.nth home.humans 3).hstate = Standing && home.carried = None)

(* Let him fall from high enough and he does not get up; the same fall
 * from just above the rocks he walks away from. *)
let defender_drop () =
  let open TinyDefender in
  let dropped (height : number) =
    let h = List.nth humans_start 6 in
    let humans = List.mapi (fun i (hu : human) -> if i = 6 then { hu with hy = ground h.hx +. height; hstate = Falling } else hu) humans_start in
    let p = { (defender_at 3000. 200. (start ())) with humans; enemies = [] } in
    (List.nth (defender_play 120 p).humans 6).hstate
  in
  Alcotest.(check bool) "from the sky: dead" true (dropped 500. = Dead);
  Alcotest.(check bool) "from a stride up: standing" true (dropped 50. = Standing)

(* The planet is the ten of them: lose them all and the ground goes,
 * and every lander left turns at once. *)
let defender_planet_goes () =
  let open TinyDefender in
  let p =
    { (defender_at 3000. 200. (start ())) with
      humans = List.map (fun (h : human) -> { h with hstate = Dead }) humans_start;
      enemies = [ { ex = 3200.; ey = 150.; kind = Lander; holds = None; cool = 100000 } ] }
  in
  let after = defender_play 1 p in
  Alcotest.(check bool) "no planet left" true (not after.planet);
  Alcotest.(check bool) "and nothing but mutants" true (List.for_all (fun (e : enemy) -> e.kind = Mutant) after.enemies)

(* The smart bomb is exactly what you can see -- which is why the
 * scanner, not the screen, is where the game is played. *)
let defender_smart_bomb () =
  let open TinyDefender in
  let lander x = { ex = x; ey = 150.; kind = Lander; holds = None; cool = 100000 } in
  let p = { (defender_at 3000. 200. (start ())) with enemies = [ lander 3100.; lander 4800. ] } in
  let k = initial_computer.keyboard in
  let after = defender_play 1 ~keyboard:(fun _ -> press "b" k) p in
  Alcotest.(check int) "the one on screen is gone" 1 (List.length after.enemies);
  Alcotest.(check bool) "the one over the horizon is not" true ((List.hd after.enemies).ex > 4000.);
  Alcotest.(check int) "it cost a bomb" 2 after.bombs;
  Alcotest.(check int) "and paid" 150 after.score

(*****************************************************************************)
(* TinyZaxxon *)
(*****************************************************************************)

let zaxxon_play ?(keyboard = fun _ -> initial_computer.keyboard) (n : int) (p : TinyZaxxon.play) : TinyZaxxon.play =
  let p = ref p and m = ref (Scene2d.start (TinyZaxxon.Playing p)) in
  for i = 1 to n do
    let c = computer ~keyboard:(keyboard i) i in
    m := Scene2d.update c !m;
    p := TinyZaxxon.update_play c !m !p
  done;
  !p

(* The whole renderer is two lines, and the one property the game is
 * played on is that altitude moves a thing *straight up* the screen
 * and moves its shadow not at all -- so the gap between the two is the
 * altitude, in pixels. *)
let zaxxon_projection () =
  let open TinyZaxxon in
  Alcotest.(check (pair (float 0.01) (float 0.01))) "the origin" (-120., -330.) (project 0. 0. 0. 0.);
  (* across the fortress: right and a little down *)
  Alcotest.(check (pair (float 0.01) (float 0.01))) "100 across" (-35., -360.) (project 0. 100. 0. 0.);
  (* along it: up and to the right *)
  Alcotest.(check (pair (float 0.01) (float 0.01))) "100 along" (-86., -288.) (project 0. 0. 0. 100.);
  (* and the reading the game is played by *)
  List.iter
    (fun (x, y, z) ->
      let px, py = project 500. x y z and sx, sy = project 500. x 0. z in
      Alcotest.(check (float 0.01)) "the shadow is directly below" px sx;
      Alcotest.(check (float 0.01)) "and the gap is the altitude" y (py -. sy))
    [ (0., 60., 700.); (-180., 0., 120.); (150., 170., 3000.) ];
  (* the scroll is a subtraction, and nothing else *)
  Alcotest.(check (pair (float 0.01) (float 0.01))) "300 further on, 300 of scroll: the same place"
    (project 0. 40. 20. 900.) (project 300. 40. 20. 1200.)

(* The fortress asks two questions in turn, and the altimeter answers
 * both: the first wall is low enough to clear without touching the
 * stick, the second has to be climbed. *)
let zaxxon_over_the_wall () =
  let open TinyZaxxon in
  let k = initial_computer.keyboard in
  let straight = zaxxon_play 200 (start ()) in
  Alcotest.(check int) "over the low one, hands off" 3 straight.lives;
  Alcotest.(check bool) "and past it" true (straight.camz +. 260. > 700.);
  let too_low = zaxxon_play 360 (start ()) in
  Alcotest.(check int) "into the next one, which is higher" 2 too_low.lives;
  let climbing = zaxxon_play 360 ~keyboard:(fun _ -> { k with kup = true }) (start ()) in
  Alcotest.(check int) "climbed, and through" 3 climbing.lives

(* A fuel tank is not points, it is the next thirty seconds. *)
let zaxxon_fuel_tank () =
  let open TinyZaxxon in
  let k = initial_computer.keyboard in
  let tank = List.find (fun (t : thing) -> t.kind = Fuel) things in
  let p = { (start ()) with px = tank.tx; py = 20.; fuel = 50. } in
  let after = zaxxon_play 30 ~keyboard:(fun i -> if i = 2 then { k with kspace = true } else k) p in
  Alcotest.(check int) "the tank is worth 150" 150 after.score;
  Alcotest.(check bool) "and is 25 of fuel" true (after.fuel > 73.);
  Alcotest.(check bool) "it is not there any more" true
    (List.exists (fun (t : thing) -> t.kind = Fuel && not t.alive) after.things)

(* And running out of it is the same as flying into a wall. *)
let zaxxon_out_of_fuel () =
  let open TinyZaxxon in
  let after = zaxxon_play 30 { (start ()) with fuel = 0.5 } in
  Alcotest.(check int) "down with an empty tank" 2 after.lives

(* The end of the fortress is not the end of the game: round again, and
 * the fortress comes at you faster. *)
let zaxxon_end_of_run () =
  let open TinyZaxxon in
  let after = zaxxon_play 1 { (start ()) with camz = 3545. } in
  Alcotest.(check int) "a second run" 2 after.run;
  Alcotest.(check int) "and a thousand for the first" 1000 after.score;
  Alcotest.(check bool) "faster than the first" true (speed 2 > speed 1);
  Alcotest.(check bool) "back at the start of it" true (after.camz = 0. && List.for_all (fun (t : thing) -> t.alive) after.things)

(*****************************************************************************)
(* TinyZaxxon *)
(*****************************************************************************)

let zaxxon_play ?(keyboard = fun _ -> initial_computer.keyboard) (n : int) (p : TinyZaxxon.play) : TinyZaxxon.play =
  let p = ref p and m = ref (Scene2d.start (TinyZaxxon.Playing p)) in
  for i = 1 to n do
    let c = computer ~keyboard:(keyboard i) i in
    m := Scene2d.update c !m;
    p := TinyZaxxon.update_play c !m !p
  done;
  !p

(* The whole renderer is two lines, and the one property the game is
 * played on is that altitude moves a thing *straight up* the screen
 * and moves its shadow not at all -- so the gap between the two is the
 * altitude, in pixels. *)
let zaxxon_projection () =
  let open TinyZaxxon in
  Alcotest.(check (pair (float 0.01) (float 0.01))) "the origin" (-120., -330.) (project 0. 0. 0. 0.);
  (* across the fortress: right and a little down *)
  Alcotest.(check (pair (float 0.01) (float 0.01))) "100 across" (-35., -360.) (project 0. 100. 0. 0.);
  (* along it: up and to the right *)
  Alcotest.(check (pair (float 0.01) (float 0.01))) "100 along" (-86., -288.) (project 0. 0. 0. 100.);
  (* and the reading the game is played by *)
  List.iter
    (fun (x, y, z) ->
      let px, py = project 500. x y z and sx, sy = project 500. x 0. z in
      Alcotest.(check (float 0.01)) "the shadow is directly below" px sx;
      Alcotest.(check (float 0.01)) "and the gap is the altitude" y (py -. sy))
    [ (0., 60., 700.); (-180., 0., 120.); (150., 170., 3000.) ];
  (* the scroll is a subtraction, and nothing else *)
  Alcotest.(check (pair (float 0.01) (float 0.01))) "300 further on, 300 of scroll: the same place"
    (project 0. 40. 20. 900.) (project 300. 40. 20. 1200.)

(* The first wall is wide open at the height you start at, so flying it
 * straight through takes no input at all; climb above the hole and the
 * fortress takes the fighter. *)
let zaxxon_through_the_hole () =
  let open TinyZaxxon in
  let k = initial_computer.keyboard in
  let straight = zaxxon_play 200 (start ()) in
  Alcotest.(check int) "through the first wall" 3 straight.lives;
  Alcotest.(check bool) "and past it" true (straight.camz +. 260. > 700.);
  let climbing = zaxxon_play 200 ~keyboard:(fun _ -> { k with kup = true }) (start ()) in
  Alcotest.(check int) "over the hole is into the wall" 2 climbing.lives

(* A fuel tank is not points, it is the next thirty seconds. *)
let zaxxon_fuel_tank () =
  let open TinyZaxxon in
  let k = initial_computer.keyboard in
  let tank = List.find (fun (t : thing) -> t.kind = Fuel) things in
  let p = { (start ()) with px = tank.tx; py = 20.; fuel = 50. } in
  let after = zaxxon_play 30 ~keyboard:(fun i -> if i = 2 then { k with kspace = true } else k) p in
  Alcotest.(check int) "the tank is worth 150" 150 after.score;
  Alcotest.(check bool) "and is 25 of fuel" true (after.fuel > 73.);
  Alcotest.(check bool) "it is not there any more" true
    (List.exists (fun (t : thing) -> t.kind = Fuel && not t.alive) after.things)

(* And running out of it is the same as flying into a wall. *)
let zaxxon_out_of_fuel () =
  let open TinyZaxxon in
  let after = zaxxon_play 30 { (start ()) with fuel = 0.5 } in
  Alcotest.(check int) "down with an empty tank" 2 after.lives

(* The end of the fortress is not the end of the game: round again, and
 * the fortress comes at you faster. *)
let zaxxon_end_of_run () =
  let open TinyZaxxon in
  let after = zaxxon_play 1 { (start ()) with camz = 3545. } in
  Alcotest.(check int) "a second run" 2 after.run;
  Alcotest.(check int) "and a thousand for the first" 1000 after.score;
  Alcotest.(check bool) "faster than the first" true (speed 2 > speed 1);
  Alcotest.(check bool) "back at the start of it" true (after.camz = 0. && List.for_all (fun (t : thing) -> t.alive) after.things)

(* Once you are past a tower it stands between you and the eye, and the
 * game draws you through it. Whether it does is a walk from the
 * fighter along the one direction this projection flattens to nothing:
 * where that walk crosses the wall's plane is where you are hidden, or
 * not. *)
let zaxxon_hidden_behind_a_wall () =
  let open TinyZaxxon in
  (* the doorway: towers to the ceiling either side of a gap *)
  let w = List.nth walls 2 in
  Alcotest.(check bool) "still coming at it: nothing in the way" false (hidden_by (0., 40., w.wz -. 100.) w);
  Alcotest.(check bool) "in the gap: seen between the towers" false (hidden_by (0., 40., w.wz +. 2.) w);
  Alcotest.(check bool) "past it: a tower is over you" true (hidden_by (0., 40., w.wz +. 200.) w);
  (* and far enough past, the line of sight clears the top of it *)
  Alcotest.(check bool) "well past: out from under it" false (hidden_by (0., 40., w.wz +. 400.) w)

(*****************************************************************************)
(* TinyDiablo *)
(*****************************************************************************)

let diablo_play ?(mouse = fun _ -> initial_computer.mouse) ?(keyboard = fun _ -> initial_computer.keyboard) (n : int)
    (p : TinyDiablo.play) : TinyDiablo.play =
  let p = ref p and m = ref (Scene2d.start (TinyDiablo.Playing p)) in
  for i = 1 to n do
    let c = { (computer ~keyboard:(keyboard i) i) with mouse = mouse i } in
    m := Scene2d.update c !m;
    p := TinyDiablo.update_play c !m !p
  done;
  !p

(* A dungeon is a pure function of its seed -- the same one is the same
 * dungeon for ever -- and whatever it rolls, the stairs can be walked
 * to from where you come in. A generator that can lock the stairs
 * behind a wall is not a generator, it is a coin toss. *)
let diablo_dungeon_holds_together () =
  let open TinyDiablo in
  let floors cells =
    let n = ref 0 in
    Array.iter (Array.iter (fun c -> if c <> Rock then incr n)) cells;
    !n
  in
  let a, _, _ = dig 24601 7 and b, _, _ = dig 24601 7 and c, _, _ = dig 1234 7 in
  Alcotest.(check int) "the same seed, the same dungeon" (floors a) (floors b);
  Alcotest.(check bool) "another seed, another dungeon" true (floors a <> floors c);
  (* twenty of them, each walked from the first room to the stairs *)
  for k = 1 to 20 do
    let cells, rooms, _ = dig (1000 + (k * 7919)) 7 in
    let start = match rooms with r :: _ -> centre r | [] -> (2, 2) in
    let stairs = ref None in
    Array.iteri (fun i col -> Array.iteri (fun j c -> if c = Stairs then stairs := Some (i, j)) col) cells;
    match !stairs with
    | None -> Alcotest.fail "a dungeon with no way down"
    | Some goal ->
        Alcotest.(check bool) (Printf.sprintf "dungeon %d: the stairs can be reached" k) true
          (goal = start || path_to cells start goal <> [])
  done

(* Clicking is not a direction, it is a place: the pixel becomes a cell
 * (the projection backwards), the cell becomes a path found by A
 * star, and the
 * path is walked. *)
let diablo_click_walks_there () =
  let open TinyDiablo in
  let p = start () in
  (* a cell a few steps away in the first room *)
  let here = cell_of p.px p.pz in
  let there = (fst here + 2, snd here + 1) in
  let path = path_to p.cells here there in
  Alcotest.(check bool) "there is a way" true (path <> []);
  let after = diablo_play 120 { p with path } in
  Alcotest.(check bool) "and he walked it" true (apart after.px after.pz (float_of_int (fst there)) (float_of_int (snd there)) < 0.2);
  Alcotest.(check bool) "with nothing left to walk" true (after.path = [])

(* A monster killed drops what it carries, and what is on the floor is
 * picked up by walking over it. *)
let diablo_kills_and_loots () =
  let open TinyDiablo in
  let p = start () in
  let m = { mx = p.px +. 0.6; mz = p.pz; hp = 4.; kind = Imp; cool = 100; hurt = 0 } in
  let after = diablo_play 40 { p with monsters = [ m ]; target = Some 0 } in
  Alcotest.(check int) "the imp is dead" 0 (List.length after.monsters);
  Alcotest.(check bool) "and paid for itself" true (after.gold > p.gold || after.items <> [] || after.potions > p.potions)

(* The stairs only go down, and what you carry goes with you. *)
let diablo_stairs_go_down () =
  let open TinyDiablo in
  let p = start () in
  let stairs = ref (0, 0) in
  Array.iteri (fun i col -> Array.iteri (fun j c -> if c = Stairs then stairs := (i, j)) col) p.cells;
  let i, j = !stairs in
  let on_them = { p with px = float_of_int i; pz = float_of_int j; gold = 130; potions = 2; monsters = [] } in
  let after = diablo_play 1 on_them in
  Alcotest.(check int) "a level deeper" 2 after.depth;
  Alcotest.(check int) "the gold came too" 130 after.gold;
  Alcotest.(check int) "and the potions" 2 after.potions;
  Alcotest.(check bool) "a new dungeon, with monsters in it" true (after.monsters <> [])

(*****************************************************************************)
(* TinyHades *)
(*****************************************************************************)

let hades_play ?(keyboard = fun _ -> initial_computer.keyboard) (n : int) (p : TinyHades.play) : TinyHades.play =
  let p = ref p and m = ref (Scene2d.start (TinyHades.Playing p)) in
  for i = 1 to n do
    let c = computer ~keyboard:(keyboard i) i in
    m := Scene2d.update c !m;
    p := TinyHades.update_play c !m !p
  done;
  !p

(* A boon is not a weapon, it is a number in the model changed for the
 * rest of the run. *)
let hades_a_boon_is_a_number () =
  let open TinyHades in
  let p = start () in
  Alcotest.(check bool) "fury hits harder" true ((take_boon Fury p).damage > p.damage);
  Alcotest.(check bool) "reach reaches further" true ((take_boon Reach p).reach > p.reach);
  Alcotest.(check bool) "swift dashes sooner" true ((take_boon Swift p).dash_wait < p.dash_wait);
  let v = take_boon Vitality p in
  Alcotest.(check bool) "vitality is more life, and some of it now" true (v.max_hp > p.max_hp && v.hp > p.hp)

(* The dash is the defence: for its first frames nothing lands. That
 * one rule is what separates an action game of the 2010s from one of
 * the 1990s. *)
let hades_the_dash_is_the_defence () =
  let open TinyHades in
  let p = start () in
  let beside = { fx = p.px +. 1.2; fz = p.pz; hp = 30.; kind = Shade; cool = 0; hurt = 0 } in
  let hit = hades_play 1 { p with foes = [ beside ]; shots = [] } in
  Alcotest.(check bool) "standing there, it lands" true (hit.hp < p.hp);
  let dashing = hades_play 1 { p with foes = [ beside ]; shots = []; dash = 14 } in
  Alcotest.(check bool) "dashing through it, nothing lands" true (dashing.hp = p.hp)

(* A chamber cleared offers three, and taking one opens the door. *)
let hades_between_chambers () =
  let open TinyHades in
  let k = initial_computer.keyboard in
  let cleared = hades_play 1 { (start ()) with foes = [] } in
  Alcotest.(check int) "three on offer" 3 (List.length cleared.offer);
  Alcotest.(check bool) "and nothing else moves until one is taken" true (cleared.chamber = 1);
  let taken = hades_play 3 ~keyboard:(fun i -> if i = 2 then press "1" k else k) cleared in
  Alcotest.(check int) "the next chamber" 2 taken.chamber;
  Alcotest.(check bool) "with something in it" true (taken.foes <> []);
  Alcotest.(check bool) "and the boon kept" true (taken.offer = [])

(* Dying is not a reset: it pays for the next run, which is the whole
 * of the roguelite and the reason this game is not TinyRogue. *)
let hades_death_pays_for_the_next_run () =
  let open TinyHades in
  let k = initial_computer.keyboard in
  let p = start () in
  let doomed = { p with hp = 1.; foes = [ { fx = p.px; fz = p.pz +. 1.; hp = 30.; kind = Brute; cool = 0; hurt = 0 } ] } in
  let m = ref (Scene2d.go (Playing doomed) (Scene2d.start Title)) in
  for i = 1 to 3 do m := TinyHades.update (computer i) !m done;
  (match !m.scene with
  | Dead _ -> ()
  | _ -> Alcotest.fail "the brute should have finished him");
  for i = 4 to 6 do m := TinyHades.update (computer ~keyboard:(if i = 5 then { k with kspace = true } else k) i) !m done;
  match !m.scene with
  | Playing next ->
      Alcotest.(check int) "a second run" 2 next.run;
      Alcotest.(check bool) "kept something from the first" true (next.kept > 0.);
      Alcotest.(check bool) "and starts with more life than the first did" true (next.max_hp > p.max_hp)
  | _ -> Alcotest.fail "space should have started the next run"

(*****************************************************************************)
(* TinyMonumentValley *)
(*****************************************************************************)

let mv_screen = to_screen 1000. 1000.

(* The claim the whole game rests on: with no perspective and the view
 * direction (1, 1, 1), a block and another three along, three up and
 * three away are drawn on the same pixel. Nothing in the picture can
 * tell them apart -- so the game lets the figure step between them. *)
let mv_three_apart_is_one_pixel () =
  let open TinyMonumentValley in
  let l = levels.(0) in
  let cam = monument_camera l 0 in
  let blocks = Array.of_list (placed l 0) in
  let a = blocks.(4) and b = blocks.(5) in
  Alcotest.(check (float 0.001)) "three along" 3. (b.bx -. a.bx);
  Alcotest.(check (float 0.001)) "three up" 3. (b.by -. a.by);
  Alcotest.(check (float 0.001)) "three away" 3. (b.bz -. a.bz);
  (match (on_screen cam mv_screen a, on_screen cam mv_screen b) with
  | Some (ax, ay), Some (bx, by) ->
      Alcotest.(check bool) "and yet one pixel" true (Float.hypot (ax -. bx) (ay -. by) < 1.)
  | _ -> Alcotest.fail "both should be on screen");
  Alcotest.(check bool) "so the figure may step across" true (connected cam mv_screen a b);
  (* and two blocks that are neither neighbours nor lined up are not *)
  Alcotest.(check bool) "unlike two unrelated stones" false (connected cam mv_screen blocks.(0) blocks.(6))

(* The first monument is walkable end to end only through that step. *)
let mv_the_first_monument_is_walked () =
  let open TinyMonumentValley in
  let l = levels.(0) in
  let cam = monument_camera l 0 in
  let blocks = placed l 0 in
  let path = route cam mv_screen blocks l.start l.goal in
  Alcotest.(check bool) "there is a way" true (path <> []);
  Alcotest.(check bool) "and it goes through the impossible step" true (List.mem 5 path)

(* Turning the piece does not build anything: it changes which of the
 * impossible connections the camera happens to be making. At rest the
 * bridge lines up with the east terrace, a quarter turn later with the
 * north one -- and the goal is north. *)
let mv_turning_changes_what_connects () =
  let open TinyMonumentValley in
  let l = levels.(1) in
  let way (turns : int) =
    let cam = monument_camera l turns in
    route cam mv_screen (placed l turns) l.start l.goal
  in
  Alcotest.(check bool) "pointing east, no way to the goal" true (way 0 = []);
  Alcotest.(check bool) "turned a quarter, there is one" true (way 1 <> []);
  (* the bridge really did move: it is the world that turned, not the rule *)
  let before = Array.of_list (placed l 0) and after = Array.of_list (placed l 1) in
  Alcotest.(check bool) "the bridge swung" true (before.(6).bx <> after.(6).bx || before.(6).bz <> after.(6).bz);
  Alcotest.(check bool) "the masonry did not" true (before.(0).bx = after.(0).bx && before.(0).bz = after.(0).bz)

(* Walking is the same number of frames a step whether the step is real
 * or not, which is what sells it. *)
let mv_a_step_is_a_step () =
  let open TinyMonumentValley in
  let p = ref (start ()) in
  let l = levels.(0) in
  let cam = monument_camera l 0 in
  let path = route cam mv_screen (placed l 0) l.start l.goal in
  p := { !p with path };
  let m = ref (Scene2d.start (TinyMonumentValley.Playing !p)) in
  let steps = List.length path in
  for i = 1 to (steps * 17) + 5 do
    let c = computer i in
    m := Scene2d.update c !m;
    p := TinyMonumentValley.update_play c !m !p
  done;
  Alcotest.(check int) "she arrived" l.goal !p.here;
  Alcotest.(check bool) "with nothing left to walk" true (!p.path = [])

(*****************************************************************************)
(* TinyCeleste *)
(*****************************************************************************)

(* A little room to try each rule in: a floor, a wall on each side, and
 * a block of ceiling in the middle for the corner correction. *)
let celeste_room =
  Tilemap.of_strings 40.
    [ "##########";
      "#........#";
      "#...##...#";
      "#........#";
      "#........#";
      "#........#";
      "##########" ]

let celeste_at (x : float) (y : float) (feel : TinyCeleste.feel) : TinyCeleste.play =
  { (TinyCeleste.enter 0 0 feel) with map = celeste_room; x; y; vx = 0.; vy = 0.; airborne = 0 }

let celeste_run (inputs : TinyCeleste.input list) (p : TinyCeleste.play) : TinyCeleste.play =
  List.fold_left (fun p i -> TinyCeleste.step i p) p inputs

(* the floor's top, and where a climber standing on it is *)
let celeste_floor = -100.
let celeste_standing = celeste_floor +. 17.

(* Coyote time: a jump a few frames after leaving the ground still
 * counts. Off, the same press does nothing. *)
let celeste_coyote () =
  let open TinyCeleste in
  (* away from the ceiling block, which is only for the corner test *)
  let falling feel = { (celeste_at (-120.) 0. feel) with airborne = 3; vy = -2.7 } in
  let jump = { nothing with jump = true; jump_held = true } in
  Alcotest.(check bool) "on: still a jump, three frames late" true ((step jump (falling all_on)).vy > 10.);
  Alcotest.(check bool) "off: just a fall" true ((step jump (falling { all_on with coyote = false })).vy < 0.)

(* The buffer: a jump pressed just before landing is done on landing.
 * Off, it was too early and is lost. *)
let celeste_buffer () =
  let open TinyCeleste in
  let landing feel = { (celeste_at (-120.) (celeste_standing +. 12.) feel) with airborne = 20; vy = -6. } in
  let press = { nothing with jump = true; jump_held = true } in
  let highest feel =
    let p = ref (landing feel) and top = ref neg_infinity in
    List.iter (fun i -> p := step i !p; top := Float.max !top !p.y) (press :: List.init 20 (fun _ -> nothing));
    !top
  in
  Alcotest.(check bool) "on: the early press became a jump" true (highest all_on > celeste_standing +. 40.);
  Alcotest.(check bool) "off: it was lost" true (highest { all_on with buffer = false } < celeste_standing +. 20.)

(* Variable height: the same jump, let go of early, is lower. Off, the
 * button's length says nothing. *)
let celeste_variable_jump () =
  let open TinyCeleste in
  let apex feel hold =
    let p = ref (celeste_at (-120.) celeste_standing feel) and top = ref neg_infinity in
    for f = 0 to 40 do
      p := step { nothing with jump = f = 0; jump_held = f < hold } !p;
      top := Float.max !top !p.y
    done;
    !top -. celeste_standing
  in
  Printf.eprintf "DBG celeste apex: held %.0f, tapped %.0f\n%!" (apex all_on 40) (apex all_on 3);
  Alcotest.(check bool) "on: a tap is a hop" true (apex all_on 3 < apex all_on 40 *. 0.6);
  Alcotest.(check bool) "off: a tap is a leap" true
    (Float.abs (apex { all_on with variable = false } 3 -. apex { all_on with variable = false } 40) < 1.)

(* Corner correction: a jump that clips a ceiling's corner by a few
 * pixels is slid round it. Off, it stops dead. *)
let celeste_corners () =
  let open TinyCeleste in
  (* the ceiling block spans x -40..40; a climber 24 wide centred at 50
   * overlaps it by 2 pixels on his left *)
  let rise feel =
    let p = ref (celeste_at 50. celeste_standing feel) and top = ref neg_infinity in
    for f = 0 to 25 do
      p := step { nothing with jump = f = 0; jump_held = true } !p;
      top := Float.max !top !p.y
    done;
    !top
  in
  Printf.eprintf "DBG celeste corner: on %.0f, off %.0f\n%!" (rise all_on) (rise { all_on with corners = false });
  (* the block's underside is at y = 20: a head above it got past *)
  Alcotest.(check bool) "on: past the ceiling" true (rise all_on > 40.);
  Alcotest.(check bool) "off: stopped under it" true (rise { all_on with corners = false } < 20.)

(* One dash, spent in the air, and not another until the feet touch. *)
let celeste_one_dash () =
  let open TinyCeleste in
  let p = { (celeste_at 0. 0. all_on) with airborne = 10 } in
  let dashed = step { nothing with dash = true; dx = 1. } p in
  Alcotest.(check bool) "the dash is spent" true ((not dashed.dash_ready) && dashed.dashing > 0);
  let after = celeste_run (List.init 12 (fun _ -> nothing)) dashed in
  let again = step { nothing with dash = true; dx = -1. } after in
  Alcotest.(check int) "and there is no second one" 0 again.dashing;
  let landed = celeste_run (List.init 60 (fun _ -> nothing)) again in
  Alcotest.(check bool) "until the ground gives it back" true landed.dash_ready

(* A wall jump throws you away from the wall, and up. *)
let celeste_wall_jump () =
  let open TinyCeleste in
  (* against the right wall, whose inside is at x = 160 *)
  let p = { (celeste_at 148. 20. all_on) with airborne = 15; vy = -3. } in
  let off = step { nothing with jump = true; jump_held = true; dx = 1. } p in
  Alcotest.(check bool) "away from the wall" true (off.vx < 0.);
  Alcotest.(check bool) "and up" true (off.vy > 0.)

(*****************************************************************************)
(* TinyBraid *)
(*****************************************************************************)

(* [until i stop p]: the same input, frame after frame, until [stop] (at
 * most 600 frames) *)
let braid_until (i : TinyBraid.input) (stop : TinyBraid.play -> bool) (p : TinyBraid.play) : TinyBraid.play =
  let rec go n p = if n = 0 || stop p then p else go (n - 1) (TinyBraid.step i p) in
  go 600 p

let braid_frames (n : int) (i : TinyBraid.input) (p : TinyBraid.play) : TinyBraid.play =
  List.fold_left (fun p i -> TinyBraid.step i p) p (List.init n (fun _ -> i))

(* shift held until the start: [TinyBraid.rewind] with the room's rules
 * given, to try them changed *)
let rec braid_rewind_all (r : TinyBraid.room) (p : TinyBraid.play) : TinyBraid.play =
  match p.history.past with [] -> p | _ -> braid_rewind_all r (TinyBraid.rewind r p)

let braid_tim (p : TinyBraid.play) : TinyBraid.tim = p.history.now.tim

(* Forgiveness: running into the pit kills; rewinding undoes it, frame
 * for frame, and a monster stomped walks again. *)
let braid_forgiveness () =
  let open TinyBraid in
  let right = { nothing with dx = 1. } in
  let p = enter 0 in
  let early = braid_frames 20 right p in
  let dead = braid_until right (fun p -> (braid_tim p).dead) early in
  Alcotest.(check bool) "the spikes kill" true (braid_tim dead).dead;
  let frozen = braid_frames 30 right dead in
  Alcotest.(check bool) "and dead, time stops" true (braid_tim frozen = braid_tim dead);
  let back = braid_until { nothing with rewind = true } (fun p -> p.history.now.clock = early.history.now.clock) frozen in
  Alcotest.(check bool) "rewound, it is the frame it was" true (back.history.now = early.history.now);
  (* a monster stomped, one frame, rewound one frame *)
  let w = p.history.now in
  let m = List.hd w.monsters in
  let above = { p with history = Undo.start { w with tim = { w.tim with x = m.mx; y = m.my +. 25.; vy = -3. } } } in
  let stomped = step nothing above in
  Alcotest.(check bool) "stomped" false (List.hd stomped.history.now.monsters).alive;
  Alcotest.(check bool) "and it walks again" true (List.hd (step { nothing with rewind = true } stomped).history.now.monsters).alive

(* And the room can be got through by a robot that runs and jumps at
 * the pits (stomping the first monster on the way down), and that,
 * should a monster get it, would rewind a second and wait a third of
 * one: no lives, no restart, just the list. *)
let braid_robot () =
  let open TinyBraid in
  let rec go n deaths p =
    let t = braid_tim p in
    if n = 0 || reached_exit p then (p, deaths)
    else if t.dead then go (n - 1) (deaths + 1) (braid_frames 20 nothing (braid_frames 60 { nothing with rewind = true } p))
    else
      let pit = Tilemap.tile_at forgiveness.map (t.x +. 30.) (t.y -. 30.) = Some '^' in
      go (n - 1) deaths (step { nothing with dx = 1.; jump = t.ground && pit } p)
  in
  let p, deaths = go 3000 0 (enter 0) in
  Printf.eprintf "DBG braid robot: %d deaths, %d frames kept\n%!" deaths (List.length p.history.past);
  Alcotest.(check bool) "out" true (reached_exit p)

(* Mystery: the key at the bottom of the drop, taken, and brought up by
 * rewinding, because it is green; not green, it goes back down. *)
let braid_mystery () =
  let open TinyBraid in
  let right = { nothing with dx = 1. } in
  let p = enter 1 in
  let down = braid_until right (fun p -> p.history.now.key = Held) p in
  Alcotest.(check bool) "the key, taken at the bottom" true ((braid_tim down).y < 0.);
  let back = braid_rewind_all mystery down in
  Alcotest.(check bool) "rewound to the start" true ((braid_tim back).y > 0.);
  Alcotest.(check bool) "and the key still in hand" true (back.history.now.key = Held);
  let not_green = braid_rewind_all { mystery with green = false } down in
  Alcotest.(check bool) "not green, it would be back down there" true (not_green.history.now.key = Lying);
  (* up there, round the hole: a jump over it, then the door *)
  let at_hole = braid_until right (fun p -> (braid_tim p).x > -190.) back in
  let over = braid_frames 1 { right with jump = true } at_hole in
  let out = braid_until right reached_exit over in
  Alcotest.(check bool) "the door opened for the key" true (out.history.now.key = Used);
  Alcotest.(check bool) "and the exit is behind it" true (reached_exit out)

(* Place: standing still, time stands still; walking across the lift
 * raises it, and from its top the ledge is a jump away. *)
let braid_place () =
  let open TinyBraid in
  let right = { nothing with dx = 1. } in
  let p = enter 2 in
  let still = braid_frames 60 nothing (braid_frames 5 nothing p) in
  Alcotest.(check bool) "standing, the clock stops" true (still.history.now.clock = (braid_frames 5 nothing p).history.now.clock);
  let top = braid_until right (fun p -> (braid_tim p).x > 150.) p in
  Printf.eprintf "DBG braid lift: x %.0f y %.0f\n%!" (braid_tim top).x (braid_tim top).y;
  Alcotest.(check bool) "carried up by walking" true ((braid_tim top).y > 120.);
  let out = braid_until right reached_exit (braid_frames 1 { right with jump = true } top) in
  Alcotest.(check bool) "and out" true (reached_exit out)

(* Decision: the door stays open only while the plate is pressed. Run
 * for it, and it closes before you get there; stand on the plate,
 * rewind, and the shadow stands there while you run. *)
let braid_decision () =
  let open TinyBraid in
  let right = { nothing with dx = 1. } in
  let on_plate = braid_frames 20 nothing (braid_until right (fun p -> (braid_tim p).x > -300.) (enter 3)) in
  let run p = braid_until right reached_exit p in
  let alone = run on_plate in
  Printf.eprintf "DBG braid alone: x %.0f\n%!" (braid_tim alone).x;
  Alcotest.(check bool) "alone, the door is shut in front of you" false (reached_exit alone);
  let waited = braid_frames 150 nothing on_plate in
  let rewound = braid_frames 150 { nothing with rewind = true } waited in
  let helped = run rewound in
  Alcotest.(check bool) "with the shadow on the plate, out" true (reached_exit helped)

(*****************************************************************************)
(* TinyVVVVVV *)
(*****************************************************************************)

let vv_frames (n : int) (i : TinyVVVVVV.input) (p : TinyVVVVVV.play) : TinyVVVVVV.play =
  List.fold_left (fun p i -> TinyVVVVVV.step i p) p (List.init n (fun _ -> i))

let vv_until (i : TinyVVVVVV.input) (stop : TinyVVVVVV.play -> bool) (p : TinyVVVVVV.play) : TinyVVVVVV.play =
  let rec go n p = if n = 0 || stop p then p else go (n - 1) (TinyVVVVVV.step i p) in
  go 600 p

(* Viridian standing on the tile (col, row) of room [r] *)
let vv_on (r : int) (col : int) (row : int) : TinyVVVVVV.play =
  let open TinyVVVVVV in
  let x, y = Tilemap.center rooms.(r).map col row in
  vv_frames 3 nothing { start with room = r; x; y }

(* The flip: from the floor to the ceiling, all the way; and not again
 * until he stands on something. *)
let vvvvvv_flip () =
  let open TinyVVVVVV in
  let p = vv_frames 5 nothing start in
  Alcotest.(check bool) "on the floor" true (standing p);
  let up = step { nothing with flip = true } p in
  Alcotest.(check bool) "flipped" true up.flipped;
  let again = step { nothing with flip = true } up in
  Alcotest.(check bool) "no flip back in the air" true again.flipped;
  let ceiling = vv_frames 60 nothing again in
  Alcotest.(check bool) "on the ceiling" true (standing ceiling && ceiling.y > 150.)

(* The first room: up, over the wall on the ceiling, down between the
 * spikes, and out on the right. *)
let vvvvvv_first_room () =
  let open TinyVVVVVV in
  let right = { dx = 1.; flip = false } in
  let p = step { nothing with flip = true } (vv_frames 5 nothing start) in
  let p = vv_until nothing standing (vv_frames 1 nothing p) in
  let p = vv_frames 10 nothing (vv_until right (fun p -> p.x > 190.) p) in
  let p = vv_until nothing standing (vv_frames 1 nothing (step { nothing with flip = true } p)) in
  Alcotest.(check bool) "down past the spikes" true ((not p.flipped) && p.y < 0.);
  let p = vv_until right (fun p -> p.room = 1 || p.dead > 0) p in
  Alcotest.(check int) "into the next room" 1 p.room;
  Alcotest.(check int) "without dying" 0 p.deaths

(* A spike takes him back to the checkpoint touched last. *)
let vvvvvv_checkpoint () =
  let open TinyVVVVVV in
  let p = vv_on 1 2 13 in
  Alcotest.(check int) "the checkpoint is his" 1 p.save.s_room;
  let dead = vv_until { dx = 1.; flip = false } (fun p -> p.dead > 0) p in
  Alcotest.(check int) "the spikes" 1 dead.deaths;
  let back = vv_frames dying_frames nothing dead in
  let cx, _ = Tilemap.center rooms.(1).map 2 13 in
  Alcotest.(check bool) "back at the checkpoint" true (back.dead = 0 && back.room = 1 && back.x = cx)

(* The gravity line: walk off into the pit, and it throws you up to the
 * ceiling, clear over the pit. *)
let vvvvvv_gravity_line () =
  let open TinyVVVVVV in
  let right = { dx = 1.; flip = false } in
  let p = vv_until right (fun p -> p.flipped || p.dead > 0) (vv_on 2 2 13) in
  Alcotest.(check bool) "flipped by the line" true p.flipped;
  let p = vv_until right standing p in
  Printf.eprintf "DBG vvvvvv line: on the ceiling at x %.0f\n%!" p.x;
  Alcotest.(check bool) "on the ceiling, past the pit" true (p.y > 150. && p.x > 160.);
  let p = vv_until nothing standing (vv_frames 1 nothing (step { nothing with flip = true } p)) in
  let p = vv_until right (fun p -> p.room = 3 || p.dead > 0) p in
  Alcotest.(check int) "and out" 3 p.room;
  Alcotest.(check int) "without dying" 0 p.deaths

(* A room that wraps: fall off the bottom, in at the top, onto the
 * ledge with the teleporter. *)
let vvvvvv_wrap () =
  let open TinyVVVVVV in
  let x, _ = Tilemap.center rooms.(3).map 12 8 in
  let p = vv_until nothing arrived { start with room = 3; x; y = -300. } in
  Alcotest.(check bool) "home" true (arrived p)

(*****************************************************************************)
(* TinyMarioGalaxy2D *)
(*****************************************************************************)

let mg_turning : TinyMarioGalaxy2D.config = { turning = true; latch = true }

(* [i] played until [stop], or [n] frames *)
let mg_until ?(c = mg_turning) ?(n = 600) (i : TinyMarioGalaxy2D.input) (stop : TinyMarioGalaxy2D.play -> bool)
    (p : TinyMarioGalaxy2D.play) : TinyMarioGalaxy2D.play =
  let rec go n p = if n = 0 || stop p then p else go (n - 1) (TinyMarioGalaxy2D.step c i p) in
  go n p

let mg_frames ?c (n : int) (i : TinyMarioGalaxy2D.input) (p : TinyMarioGalaxy2D.play) : TinyMarioGalaxy2D.play =
  mg_until ?c ~n i (fun _ -> false) p

(* the goomba out of the way *)
let mg_start : TinyMarioGalaxy2D.play = { TinyMarioGalaxy2D.start with squashed = 1 }

let mg_down (p : float * float) : string =
  match TinyMarioGalaxy2D.gravity_at p with Some (x, y) -> Printf.sprintf "(%.2f, %.2f)" (x +. 0.) (y +. 0.) | None -> "none"

(* Down is towards the planet from all round it, a fixed direction in
 * the platform's box, which wins where it overlaps the home planet's
 * pull, and nothing in the void. *)
let galaxy_zones () =
  Alcotest.(check string) "on top of home" "(0.00, -1.00)" (mg_down (0., 150.));
  Alcotest.(check string) "under home" "(0.00, 1.00)" (mg_down (0., -150.));
  Alcotest.(check string) "right of home" "(-1.00, 0.00)" (mg_down (150., 0.));
  Alcotest.(check string) "the box, over the home planet's pull" "(0.00, -1.00)" (mg_down (0., -250.));
  Alcotest.(check string) "the tiny planet's own" "(1.00, 0.00)" (mg_down (500., -300.));
  Alcotest.(check string) "the void" "none" (mg_down (0., -700.))

(* Standing under the planet, upside down, and staying there. *)
let galaxy_underneath () =
  let open TinyMarioGalaxy2D in
  let p = mg_frames 60 nothing { mg_start with pos = (0., -.(home.radius +. size)); up = (0., -1.) } in
  Alcotest.(check bool) "standing" true p.standing;
  Alcotest.(check bool) "still under" true (snd p.pos < -.home.radius)

(* Right held, the camera fixed: all the way round the planet, the arrow
 * kept for what it meant when pressed; re-read each frame, it means
 * nothing on the planet's side, and Mario stops there. *)
let galaxy_latch () =
  let open TinyMarioGalaxy2D in
  let right = { dx = 1.; jump = false } in
  let fixed = { turning = false; latch = true } in
  let under = mg_until ~c:fixed right (fun p -> snd p.pos < -100.) mg_start in
  Alcotest.(check bool) "under, on foot" true (snd under.pos < -100. && under.standing);
  let back = mg_until ~c:fixed right (fun p -> snd p.pos > 100.) under in
  Alcotest.(check bool) "back on top" true (snd back.pos > 100. && back.deaths = 0);
  let stuck = mg_frames ~c:{ fixed with latch = false } 300 right mg_start in
  Alcotest.(check bool) "stopped on the side" true (fst stuck.pos > 100. && Float.abs (snd stuck.pos) < 40.);
  (* and the turning camera needs no latch: right is always along *)
  let turning = mg_until ~c:{ turning = true; latch = false } right (fun p -> snd p.pos < -100.) mg_start in
  Alcotest.(check bool) "under, the camera turned" true (snd turning.pos < -100.)

(* Coming down on the goomba squashes it, down being the goomba's own
 * down; walking into it hurts. *)
let galaxy_goomba () =
  let open TinyMarioGalaxy2D in
  let g, up = goomba_at start.goomba in
  let p = { start with pos = add g (mul 40. up); vel = mul (-3.) up; up; standing = false } in
  let stomped = mg_until nothing (fun p -> p.squashed > 0 || p.dying > 0) p in
  Alcotest.(check (pair int int)) "squashed, unhurt" (1, 0) (stomped.squashed, stomped.deaths);
  let hurt = mg_frames 2 nothing { start with pos = add g (mul 25. (clockwise up)); up } in
  Alcotest.(check int) "hurt" 1 hurt.deaths

(* Off the platform's edge, nothing pulls: adrift, lost, and back on the
 * home planet. *)
let galaxy_void () =
  let open TinyMarioGalaxy2D in
  let p = { mg_start with pos = (-100., -440.); standing = false } in
  let lost = mg_until { dx = -1.; jump = false } (fun p -> p.dying > 0) p in
  Alcotest.(check int) "lost" 1 lost.deaths;
  Alcotest.(check bool) "adrift first" true (lost.adrift > lost_frames);
  let back = mg_frames dying_frames nothing lost in
  Alcotest.(check bool) "home" true (back.pos = start.pos && back.dying = 0)

(* The galaxy played through: right held round the home planet, a
 * standing jump from its underside into the platform's box, right held
 * along it, a jump off its end into the tiny planet's pull, and round
 * it to the Power Star. *)
let galaxy_played_through () =
  let open TinyMarioGalaxy2D in
  let right = { dx = 1.; jump = false } and jump = { dx = 1.; jump = true } in
  let angle (p : play) = degrees (sub p.pos home.a) in
  let p = mg_frames 20 nothing (mg_until right (fun p -> angle p < -80.) mg_start) in
  let p = mg_until nothing (fun p -> p.standing && snd p.pos < -400.) (step mg_turning { nothing with jump = true } p) in
  let p = mg_until right (fun p -> fst p.pos > 380.) p in
  let p = mg_until right (fun p -> got_star p || p.dying > 0) (step mg_turning jump p) in
  Alcotest.(check bool) "the Power Star" true (got_star p);
  Alcotest.(check int) "without dying" 0 p.deaths

(*****************************************************************************)
(* TinyMarioGalaxy *)
(*****************************************************************************)

let g3_until ?(n = 900) (i : TinyMarioGalaxy.input) (stop : TinyMarioGalaxy.play -> bool) (p : TinyMarioGalaxy.play) :
    TinyMarioGalaxy.play =
  let rec go n p = if n = 0 || stop p then p else go (n - 1) (TinyMarioGalaxy.step i p) in
  go n p

let g3_frames (n : int) (i : TinyMarioGalaxy.input) (p : TinyMarioGalaxy.play) : TinyMarioGalaxy.play =
  g3_until ~n i (fun _ -> false) p

let fst3 ((x, _, _) : float * float * float) : float = x
let snd3 ((_, y, _) : float * float * float) : float = y

let g3_start : TinyMarioGalaxy.play = { TinyMarioGalaxy.start with squashed = 1 }

(* to the hundredth, and -0 made 0 *)
let g3_vec ((x, y, z) : float * float * float) : string =
  let r v = (Float.round (v *. 100.) /. 100.) +. 0. in
  Printf.sprintf "(%.2f, %.2f, %.2f)" (r x) (r y) (r z)

(* [euler]'s angles, given to rotate3d, take the model's axes where they
 * should go: x to the right, y to up, -z to the front; even in the
 * gimbal lock, the right straight along z. *)
let galaxy3d_euler () =
  let open TinyMarioGalaxy in
  let check (up : vec) (front : vec) =
    match (orient up front (Playground3d.polygon3d Playground.red [ (1., 0., 0.); (0., 1., 0.); (0., 0., -1.) ])).form with
    | Playground3d.Polygon3d (_, [ r; u; f ]) ->
        let name = g3_vec up ^ " " ^ g3_vec front in
        Alcotest.(check string) (name ^ ": right") (g3_vec (cross front up)) (g3_vec r);
        Alcotest.(check string) (name ^ ": up") (g3_vec up) (g3_vec u);
        Alcotest.(check string) (name ^ ": front") (g3_vec front) (g3_vec f)
    | _ -> Alcotest.fail "not a polygon"
  in
  check (0., 1., 0.) (0., 0., -1.);
  check (0., -1., 0.) (0., 0., 1.);
  check (0.6, 0.8, 0.) (0., 0., -1.);
  check (0., 0.8, 0.6) (1., 0., 0.);
  check (1., 0., 0.) (0., 1., 0.) (* the gimbal lock *)

(* The cube's gravity: straight into a face, and round an edge towards
 * the edge; the box over the home planet's pull *)
let galaxy3d_zones () =
  let open TinyMarioGalaxy in
  let down p = match gravity_at p with Some d -> g3_vec d | None -> "none" in
  Alcotest.(check string) "a face" "(0.00, 0.00, -1.00)" (down (-13., 3., 3.5));
  Alcotest.(check string) "an edge" "(-0.71, -0.71, 0.00)" (down (-10., 6., 0.));
  Alcotest.(check string) "the box" "(0.00, -1.00, 0.00)" (down (0., -9., 0.));
  Alcotest.(check string) "the void" "none" (down (0., -30., 0.))

(* Up held: over the planet's side, under it, and back on top, the
 * camera's heading carried round with him. *)
let galaxy3d_round () =
  let open TinyMarioGalaxy in
  let up = { nothing with ahead = 1. } in
  let under = g3_until up (fun p -> snd3 p.pos < -5.) g3_start in
  Alcotest.(check bool) "under, on foot" true (under.standing && snd3 under.pos < -5.);
  let back = g3_until up (fun p -> snd3 p.pos > 5.) under in
  Alcotest.(check bool) "back on top" true (snd3 back.pos > 5. && back.standing && back.deaths = 0)

(* Coming down on the goomba squashes it, down being its own. *)
let galaxy3d_goomba () =
  let open TinyMarioGalaxy in
  let g, up, _ = goomba_at start.goomba in
  let p = { start with pos = add g (mul 1.5 up); vel = mul (-0.1) up; up; standing = false } in
  let stomped = g3_until nothing (fun p -> p.squashed > 0 || p.dying > 0) p in
  Alcotest.(check (pair int int)) "squashed, unhurt" (1, 0) (stomped.squashed, stomped.deaths)

(* the arrows a player holds to run towards [target]: its direction in
 * the camera's terms *)
let g3_toward ?(jump = false) (target : float * float * float) (p : TinyMarioGalaxy.play) : TinyMarioGalaxy.input =
  let open TinyMarioGalaxy in
  let ahead = tangent p.up p.facing p.cam_heading in
  let right = cross ahead p.up in
  let dir = tangent p.up zero (sub target p.pos) in
  { nothing with ahead = dot dir ahead; side = dot dir right; jump }

let g3_steer ?(n = 900) (target : float * float * float) (stop : TinyMarioGalaxy.play -> bool) (p : TinyMarioGalaxy.play) :
    TinyMarioGalaxy.play =
  let rec go n p = if n = 0 || stop p then p else go (n - 1) (TinyMarioGalaxy.step (g3_toward target p) p) in
  go n p

(* The galaxy played through: up held to the home planet's underside, a
 * standing jump into the platform's box, along it, a jump off its end
 * into the tiny planet's pull, round it to the Power Star. *)
let galaxy3d_played_through () =
  let open TinyMarioGalaxy in
  let p = g3_frames 20 nothing (g3_until { nothing with ahead = 1. } (fun p -> snd3 p.pos < -5.3) g3_start) in
  let p = g3_until nothing (fun p -> p.standing && snd3 p.pos < -15.) (step { nothing with jump = true } p) in
  Alcotest.(check bool) "on the platform" true (snd3 p.pos < -15. && p.deaths = 0);
  let edge = (17., -17., 0.) in
  let p = g3_steer edge (fun p -> fst3 p.pos > 15. || p.dying > 0) p in
  let p = step (g3_toward ~jump:true edge p) p in
  let p = g3_steer power_star (fun p -> got_star p || p.dying > 0) p in
  Alcotest.(check bool) "the Power Star" true (got_star p);
  Alcotest.(check int) "without dying" 0 p.deaths

(*****************************************************************************)
(* TinyKarel *)
(*****************************************************************************)

let karel_run (text : string) (world : Karel.world) : Karel.run =
  match TinyKarel.parse text with
  | Ok p -> Karel.execute ~definitions:p.definitions world p.main
  | Error (line, msg) -> Alcotest.failf "line %d: %s" line msg

(* Each level's solution does its task; the skeleton, a move, doesn't. *)
let karel_solutions () =
  let open TinyKarel in
  List.iter
    (fun (l : level) ->
      let r = karel_run l.solution (level_world l) in
      Alcotest.(check bool) (l.name ^ ": turned off") true (Karel.status r = Karel.Finished);
      Alcotest.(check bool) (l.name ^ ": done") true (l.goal (Karel.current r));
      Alcotest.(check bool) (l.name ^ ": not by the skeleton") false (l.goal (Karel.current (karel_run skeleton (level_world l)))))
    levels

(* The right-hand rule, in a maze it has never seen. *)
let karel_other_maze () =
  let maze = List.nth TinyKarel.levels 3 in
  let other =
    Karel.world
      [ ". . . . .";
        "  -   -  ";
        ". .|. .|.";
        "    -    ";
        ".|. .|. .";
        "  -   -  ";
        ". .|. .|1";
        "      -  ";
        "> . .|. ." ]
  in
  Alcotest.(check int) "picked up" 0 (Karel.beepers_left (Karel.current (karel_run maze.solution other)))

(* A mistake, and its line. *)
let karel_errors () =
  let err text = match TinyKarel.parse text with Ok _ -> None | Error e -> Some e in
  let program body = "BEGINNING-OF-PROGRAM\nBEGINNING-OF-EXECUTION\n" ^ body ^ "\nEND-OF-EXECUTION\nEND-OF-PROGRAM" in
  Alcotest.(check (option (pair int string))) "no such instruction" (Some (4, "no instruction turnright"))
    (err (program "move;\nturnright"));
  Alcotest.(check (option (pair int string))) "no such condition" (Some (3, "no condition wall-ahead"))
    (err (program "WHILE wall-ahead DO move"));
  Alcotest.(check (option (pair int string))) "a missing TIMES" (Some (3, "TIMES expected, not move"))
    (err (program "ITERATE 3 move"));
  Alcotest.(check (option (pair int string))) "lower case, and a comment" None (err (program "{ go } iterate 2 times move"))

(*****************************************************************************)
(* TinyStoneAge *)
(*****************************************************************************)

(* the level [rows] with the dinosaur on its 'S' *)
let sa_play (rows : string list) : TinyStoneAge.play =
  let open TinyStoneAge in
  let map = Tilemap.of_strings tile_size rows in
  let col, row = List.hd (Tilemap.find map 'S') in
  { (load 0) with map = Tilemap.set map col row '#'; col; row }

let sa_step (d : int * int) (p : TinyStoneAge.play) : TinyStoneAge.play =
  match TinyStoneAge.step d p with Some p -> TinyStoneAge.settle p | None -> p

let sa_right = (1, 0)

(* A crumbling stone falls when he steps off it, and he won't step into
 * the void. *)
let stoneage_crumble () =
  let open TinyStoneAge in
  let p = sa_play [ "Sc#" ] in
  let p = sa_step sa_right (sa_step sa_right p) in
  Alcotest.(check (list string)) "fallen" [ "#.#" ] (Tilemap.to_strings p.map);
  Alcotest.(check bool) "no way back" true (step (-1, 0) p = None)

(* An arrow block carries him over the void to the next block, and
 * stays there; the other way, it is an ordinary block. *)
let stoneage_ride () =
  let open TinyStoneAge in
  let p = sa_step sa_right (sa_play [ "S>...#" ]) in
  let p = sa_step sa_right p in
  Alcotest.(check (list string)) "carried" [ "#...>#" ] (Tilemap.to_strings p.map);
  Alcotest.(check (pair int int)) "on it" (4, 0) (p.col, p.row);
  Alcotest.(check bool) "not back the way it came" true (step (-1, 0) p = None)

(* A lock wants its key, and keeps it. *)
let stoneage_keys () =
  let open TinyStoneAge in
  Alcotest.(check bool) "locked" true (step sa_right (sa_play [ "SR#" ]) = None);
  let p = sa_step sa_right (sa_step sa_right (sa_play [ "SrR" ])) in
  Alcotest.(check (pair int (list char))) "through, the key spent" (2, []) (p.col, p.keys)

(* Every level can be finished, and in time: the search's shortest way,
 * at two seconds a move, fits the clock. The lengths are the header's. *)
let stoneage_levels () =
  let open TinyStoneAge in
  let lengths =
    List.mapi
      (fun i (l : level) ->
        match solve (load i) with
        | None -> Alcotest.fail (l.name ^ ": no way to the cave")
        | Some path ->
            Alcotest.(check bool) (l.name ^ ": in time") true (List.length path * 2 <= l.seconds);
            List.length path)
      levels
  in
  Alcotest.(check (list int)) "the shortest ways" [ 11; 9; 11; 14; 14 ] lengths

(*****************************************************************************)
(* TinySuperMeatBoy *)
(*****************************************************************************)

(* [policy] played in a room, frame after frame, until Bandage Girl or
 * a death, or [n] frames *)
let smb_play ?(n = 1200) (policy : TinySuperMeatBoy.run -> TinySuperMeatBoy.input) (r : TinySuperMeatBoy.run) :
    TinySuperMeatBoy.run =
  let open TinySuperMeatBoy in
  let rec go n r =
    if n = 0 || r.play.dead || rescued levels.(r.level) r.play then r else go (n - 1) (advance (policy r) r)
  in
  go n r

(* run right, and jump at [xs], holding the jump while rising *)
let smb_run_jumping (xs : float list) (r : TinySuperMeatBoy.run) : TinySuperMeatBoy.input =
  let p = r.play in
  let jump = p.airborne = 0 && List.exists (fun x -> p.x > x && p.x < x +. 12.) xs in
  { dx = 1.; jump; jump_held = jump || p.vy > 0. }

(* A try is its inputs: fed to [step] again from the start, they give
 * the same run, to the same death. *)
let smb_replay_is_the_inputs () =
  let open TinySuperMeatBoy in
  let dead = smb_play (fun _ -> { nothing with dx = 1. }) (enter 0 0) in
  Alcotest.(check bool) "into the saw" true dead.play.dead;
  let again = List.fold_left (fun p i -> step hello_world i p) (start_of hello_world) (List.rev dead.inputs) in
  Alcotest.(check bool) "the same run, to the pixel" true (again = dead.play)

(* Dying costs a quarter of a second: back at the start, the try kept,
 * the smears too. *)
let smb_death_is_cheap () =
  let open TinySuperMeatBoy in
  let dead = smb_play (fun _ -> { nothing with dx = 1. }) (enter 0 0) in
  let back = List.fold_left (fun r i -> advance i r) dead (List.init dying_frames (fun _ -> nothing)) in
  Alcotest.(check bool) "at the start" true (back.play = start_of hello_world && back.splat = 0);
  Alcotest.(check int) "one death" 1 back.deaths;
  Alcotest.(check int) "one try kept" 1 (List.length back.tries);
  Alcotest.(check bool) "and the smears stay" true (List.length back.smears > 5)

(* Hello World: a jump over the pit, one onto the step. *)
let smb_hello_world () =
  let open TinySuperMeatBoy in
  let r = smb_play (smb_run_jumping [ -190.; 120. ]) (enter 0 0) in
  Alcotest.(check bool) "Bandage Girl" true (rescued hello_world r.play)

(* Wall to Wall: up the shaft by jumping from wall to wall, the saw
 * coming up behind. *)
let smb_wall_to_wall () =
  let open TinySuperMeatBoy in
  let policy (r : run) =
    let p = r.play in
    let side = wall_side wall_to_wall.map p in
    if p.y > 170. then { dx = 1.; jump = side <> 0. && p.airborne > 0 && p.vy < 3.; jump_held = p.vy > 0. }
    else
      let jump = p.airborne = 0 || (side <> 0. && p.vy < 3.) in
      { dx = p.facing; jump; jump_held = jump || p.vy > 0. }
  in
  let r = smb_play policy (enter 1 0) in
  Printf.eprintf "DBG smb shaft: x %.0f y %.0f dead %b frame %d\n%!" r.play.x r.play.y r.play.dead r.play.frame;
  Alcotest.(check bool) "Bandage Girl" true (rescued wall_to_wall r.play)

(* Pendulums: running straight at them dies; waiting for them to be up
 * gets through. *)
let smb_pendulums () =
  let open TinySuperMeatBoy in
  let after wait = smb_play (fun r -> if r.play.frame < wait then nothing else { nothing with dx = 1. }) (enter 2 0) in
  let waits = List.filter (fun w -> rescued pendulums (after w).play) (List.init 23 (fun k -> k * 5)) in
  Printf.eprintf "DBG smb pendulums: %d of 23 waits get through\n%!" (List.length waits);
  Alcotest.(check bool) "some wait gets through" true (waits <> []);
  Alcotest.(check bool) "not every one" true (List.length waits < 23)

(* The replay: every try from the start at once, the dead ones dying
 * where they died, the last one at Bandage Girl. *)
let smb_every_try () =
  let open TinySuperMeatBoy in
  let dead = smb_play (fun _ -> { nothing with dx = 1. }) (enter 0 0) in
  let back = List.fold_left (fun r i -> advance i r) dead (List.init dying_frames (fun _ -> nothing)) in
  let won = smb_play (smb_run_jumping [ -190.; 120. ]) back in
  Printf.eprintf "DBG smb first try: %d frames\n%!" (List.length (List.hd won.tries));
  Printf.eprintf "DBG smb winning try: %d frames, jumps at %s\n%!" (List.length won.inputs)
    (String.concat "," (List.filteri (fun _ s -> s <> "") (List.mapi (fun k (i : TinySuperMeatBoy.input) -> if i.jump then string_of_int k else "") (List.rev won.inputs))));
  let rec play rp = if replay_over rp then rp else play (replay_step rp) in
  let rp = play (ghosts won) in
  match rp.ghosts with
  | [ (first, _); (last, _) ] ->
      Alcotest.(check bool) "the first try dies again" true first.dead;
      Alcotest.(check bool) "the last one makes it" true (rescued hello_world last)
  | g -> Alcotest.failf "%d ghosts, not 2" (List.length g)

(*****************************************************************************)
(* TinyMetalGearSolid *)
(*****************************************************************************)

(* A base with one guard standing on [at], facing [facing] (its patrol
 * that one cell, so it stays), and Snake on [snake]: cells of the top
 * yard. *)
let mgs_with (at : int * int) (facing : float) (snake : int * int) : TinyMetalGearSolid.game =
  let open TinyMetalGearSolid in
  let g = start () in
  let gx, gy = center at and x, y = center snake in
  let guard = { (List.hd g.guards) with gx; gy; facing; patrol = [| at |]; goal = at; noise = at } in
  { g with guards = [ guard ]; snake = { g.snake with x; y } }

let mgs_frames (n : int) (i : TinyMetalGearSolid.input) (g : TinyMetalGearSolid.game) : TinyMetalGearSolid.game =
  List.fold_left (fun g i -> TinyMetalGearSolid.step i g) g (List.init n (fun _ -> i))

let mgs_mode (g : TinyMetalGearSolid.game) : TinyMetalGearSolid.mode = (List.hd g.guards).run.state

(* Seeing: in front and near, seen; the same distance behind the
 * crates, or behind the guard's back, not. *)
let mgs_seeing () =
  let open TinyMetalGearSolid in
  Alcotest.(check bool) "in front: '!'" true (mgs_mode (step nothing (mgs_with (18, 5) 0. (22, 5))) = Alert);
  Alcotest.(check bool) "behind the crates: nothing" true (mgs_mode (step nothing (mgs_with (21, 4) 0. (25, 4))) = Patrol);
  Alcotest.(check bool) "behind its back: nothing" true (mgs_mode (step nothing (mgs_with (18, 5) 180. (22, 5))) = Patrol)

(* The box: still, a box in plain sight is not Snake; moving, it is
 * worth a look ('?'), not an alert. *)
let mgs_box () =
  let open TinyMetalGearSolid in
  let boxed = step { nothing with box = true } (mgs_with (18, 5) 0. (22, 5)) in
  Alcotest.(check bool) "the box hides him" true (mgs_mode boxed = Patrol);
  Alcotest.(check bool) "still, it stays hidden" true (mgs_mode (mgs_frames 60 nothing boxed) = Patrol);
  Alcotest.(check bool) "moving, a '?'" true (mgs_mode (step { nothing with dy = 1. } boxed) = Suspicious)

(* The knock: the guard hears it, walks there by A* (round the wall
 * between), finds nothing, and goes back to its patrol. *)
let mgs_knock () =
  let open TinyMetalGearSolid in
  (* Snake in his room, against its wall; the guard beyond it, facing away *)
  let knocked = step { nothing with knock = true } (mgs_with (12, 3) 0. (8, 3)) in
  Alcotest.(check bool) "heard: '?'" true (mgs_mode knocked = Suspicious);
  Alcotest.(check bool) "a way there, round the wall" true (List.length (List.hd knocked.guards).path > 4);
  (* and Snake gone to the far corner of his room, out of its sight *)
  let x, y = center (1, 8) in
  let knocked = { knocked with snake = { knocked.snake with x; y } } in
  let there = mgs_frames 300 nothing knocked in
  let guard = List.hd there.guards in
  Alcotest.(check bool) "it came to the noise" true (cell_of guard.gx guard.gy = (8, 3));
  Alcotest.(check bool) "and looks around" true (mgs_mode there = Suspicious);
  Alcotest.(check bool) "nothing there: the patrol again" true (mgs_mode (mgs_frames 150 nothing there) = Patrol)

(* The choke: from behind, the guard sleeps; from in front, he sees you
 * first. *)
let mgs_choke () =
  let open TinyMetalGearSolid in
  let behind = mgs_with (20, 5) 0. (20, 5) in
  let behind = { behind with snake = { behind.snake with x = behind.snake.x -. 28. } } in
  Alcotest.(check bool) "from behind: asleep" true (mgs_mode (step { nothing with choke = true } behind) = Asleep);
  let front = mgs_with (20, 5) 180. (20, 5) in
  let front = { front with snake = { front.snake with x = front.snake.x -. 28. } } in
  Alcotest.(check bool) "from in front: '!'" true (mgs_mode (step { nothing with choke = true } front) = Alert);
  Alcotest.(check bool) "and he wakes up" true (mgs_mode (mgs_frames 1000 nothing (step { nothing with choke = true } behind)) <> Asleep)

(* The alert: one guard sees him, all of them know (the radio); he
 * gets out of sight, and they search (evasion), then give up. One
 * alert counted. *)
let mgs_alert () =
  let open TinyMetalGearSolid in
  let g = start () in
  let one = List.hd g.guards in
  (* in his room's doorway, in front of the first guard, facing west *)
  let x, y = center (7, 5) in
  let seen = step nothing { g with snake = { g.snake with x; y }; guards = { one with facing = 180. } :: List.tl g.guards } in
  Alcotest.(check bool) "every guard on alert" true (List.for_all (fun (gd : guard) -> gd.run.state = Alert) seen.guards);
  Alcotest.(check int) "one alert" 1 seen.alerts;
  (* he vanishes (to a corner of the bottom yard, far from all of them) *)
  let sx, sy = center (1, 24) in
  let hidden = { seen with snake = { seen.snake with x = sx; y = sy } } in
  let rec until n g = if n = 0 || not (alert g) then (g, n) else until (n - 1) (step nothing g) in
  let evading, _ = until 600 hidden in
  Alcotest.(check bool) "lost him: evasion" true (evasion evading && not (alert evading));
  let calm = mgs_frames 700 nothing evading in
  Alcotest.(check bool) "then back to their patrols" true (List.for_all (fun (gd : guard) -> gd.run.state = Patrol) calm.guards);
  Alcotest.(check int) "still one alert" 1 calm.alerts

(* Caught: a guard on alert next to him. And there is a way out: A*
 * finds the elevator from the start. *)
let mgs_caught_and_way_out () =
  let open TinyMetalGearSolid in
  let g = mgs_with (20, 5) 180. (20, 5) in
  let g = { g with snake = { g.snake with x = g.snake.x -. 20. } } in
  Alcotest.(check bool) "caught" true (caught (step nothing g));
  let s = List.hd (Tilemap.find base 'S') and e = List.hd (Tilemap.find base 'E') in
  Alcotest.(check bool) "a way to the elevator" true (List.length (route s e) > 30)

(*****************************************************************************)
(* TinyGTA *)
(*****************************************************************************)

let gta_frames (n : int) (i : TinyGTA.input) (c : TinyGTA.city) : TinyGTA.city =
  List.fold_left (fun c i -> TinyGTA.step i c) c (List.init n (fun _ -> i))

(* you standing next to car [k] *)
let gta_next_to (k : int) (c : TinyGTA.city) : TinyGTA.city =
  let b = (List.nth c.cars k).body in
  { c with you = { x = b.x +. 20.; y = b.y; car = None } }

let gta_index (d : TinyGTA.driver) (c : TinyGTA.city) : int =
  let rec go i = function [] -> -1 | (car : TinyGTA.car) :: rest -> if car.driver = d then i else go (i + 1) rest in
  go 0 c.cars

(* The traffic keeps to the roads: a minute on, no car in a building,
 * every one of them having driven through several crossroads. *)
let gta_traffic () =
  let open TinyGTA in
  let c = start () in
  let seen = Array.make (List.length c.cars) [] in
  let c = ref c in
  for _ = 1 to 3600 do
    c := step nothing !c;
    List.iteri (fun k (car : car) -> if not (List.mem car.toward seen.(k)) then seen.(k) <- car.toward :: seen.(k)) !c.cars
  done;
  Alcotest.(check bool) "no car in a building" true (List.for_all (fun (car : car) -> not (blocked car.body.x car.body.y)) !c.cars);
  List.iteri
    (fun k (car : car) ->
      if car.driver = Civilian then Alcotest.(check bool) "it went places" true (List.length seen.(k) >= 4))
    !c.cars

(* Any car yours: a parked one is only taken; one with its driver in it
 * is stolen, a star, and a police car after you. *)
let gta_cars_are_yours () =
  let open TinyGTA in
  let c = start () in
  let parked = gta_index Nobody c in
  let taken = step { nothing with enter = true } (gta_next_to parked c) in
  Alcotest.(check bool) "in the parked car" true (taken.you.car = Some parked && (List.nth taken.cars parked).driver = Player);
  Alcotest.(check int) "no crime" 0 taken.wanted;
  let out = step { nothing with enter = true } taken in
  Alcotest.(check bool) "and out again" true (out.you.car = None && (List.nth out.cars parked).driver = Nobody);
  let k = gta_index Civilian c in
  let stolen = step { nothing with enter = true } (gta_next_to k c) in
  Alcotest.(check bool) "in someone's car" true (stolen.you.car = Some k);
  Alcotest.(check int) "a star" 1 stolen.wanted;
  Alcotest.(check bool) "and a police car" true (gta_index Cop stolen >= 0)

(* Driving over someone is a crime too. *)
let gta_run_over () =
  let open TinyGTA in
  let c = start () in
  let p = List.hd c.peds in
  let x, y = ped_at p in
  let k = gta_index Nobody c in
  let car = List.nth c.cars k in
  let car = { car with driver = Player; body = { car.body with x = x -. 12.; y; speed = 300.; vx = 300.; vy = 0.; heading = 0. } } in
  let c = { c with cars = List.mapi (fun i x -> if i = k then car else x) c.cars; you = { x = x -. 12.; y; car = Some k } } in
  let after = run_over c in
  Alcotest.(check bool) "knocked down" false (List.hd after.peds).alive;
  Alcotest.(check int) "a star" 1 after.wanted

(* Out of the police's sight ten seconds, a star less; caught on foot
 * by one, busted. *)
let gta_police () =
  let open TinyGTA in
  let c = crime (start ()) in
  let cop = gta_index Cop c in
  Alcotest.(check int) "wanted" 1 c.wanted;
  let calm = List.fold_left (fun c _ -> cool_down c) c (List.init 601 Fun.id) in
  Alcotest.(check int) "they gave up" 0 calm.wanted;
  Alcotest.(check int) "and went home" (-1) (gta_index Cop calm);
  let b = (List.nth c.cars cop).body in
  Alcotest.(check bool) "on foot, next to one: busted" true (busted { c with you = { x = b.x +. 20.; y = b.y; car = None } });
  (* and a police car, from the far end of the city, finds you standing
   * in the street: through the roads, then straight at you *)
  let rec wait n c = if n = 0 || busted c then (c, n) else wait (n - 1) (step nothing c) in
  let _, left = wait 1800 c in
  Printf.eprintf "DBG gta police: busted after %d frames\n%!" (1800 - left);
  Alcotest.(check bool) "they come for you" true (left > 0)

(* The phones: answered, a mission; its place reached in a car, paid,
 * and the next phone rings. And every phone and place is on a
 * pavement, not in a building. *)
let gta_missions () =
  let open TinyGTA in
  Array.iter
    (fun ((px, py), (tx, ty)) ->
      Alcotest.(check bool) "phones outside" false (blocked px py);
      Alcotest.(check bool) "places outside" false (blocked tx ty))
    phones;
  let c = start () in
  let (px, py), (tx, ty) = phones.(0) in
  let answered = missions { c with you = { x = px; y = py; car = None } } in
  Alcotest.(check bool) "a mission" true (answered.mission <> None);
  let there = missions { answered with you = { x = tx; y = ty; car = Some 0 } } in
  Alcotest.(check bool) "paid" true (there.money > 1000);
  Alcotest.(check int) "the next phone" 1 there.phone;
  let late = missions { answered with mission = Some (0, 0) } in
  Alcotest.(check bool) "too late: nothing, the next phone" true (late.money = 0 && late.phone = 1)

(*****************************************************************************)
(* TinyZork *)
(*****************************************************************************)

let zork_play (lines : string list) (g : TinyZork.game) : TinyZork.game =
  List.fold_left (fun g l -> TinyZork.command l g) g lines

let zork_last (g : TinyZork.game) : string = List.nth g.out (List.length g.out - 1)

(* The parser: noise words dropped, synonyms folded, two-word verbs
 * joined, the second object after its preposition. *)
let zork_parser () =
  let open TinyZork in
  let say verb obj with_ = Say { Adventure.verb; obj; with_ } in
  Alcotest.(check bool) "pick up the brass lantern" true (parse "Pick up the brass lantern" = say "take" (Some "lamp") None);
  Alcotest.(check bool) "put the egg in the case" true
    (parse "put the jewel-encrusted egg in the trophy case" = say "put" (Some "egg") (Some "case"));
  Alcotest.(check bool) "kill troll with sword" true (parse "kill the troll with the sword" = say "attack" (Some "troll") (Some "sword"));
  Alcotest.(check bool) "go north" true (parse "go north" = Move "north");
  Alcotest.(check bool) "n" true (parse "n" = Move "north");
  Alcotest.(check bool) "climb tree" true (parse "climb tree" = Move "up");
  Alcotest.(check bool) "an unknown word, said back" true (parse "frobozz the lamp" = Unknown "frobozz")

(* The whole game, walked through: the mailbox, the window, the lamp and
 * the sword, the rug and the trap door, the troll, the painting, the
 * egg up the tree; both treasures in the case. *)
let zork_walkthrough () =
  let open TinyZork in
  let g =
    zork_play
      [ "open mailbox"; "take leaflet"; "read leaflet"; "north"; "east"; "open window"; "west"; "west"; "take lamp";
        "take sword"; "move rug"; "open trap door"; "turn on lamp"; "down"; "north"; "kill troll with sword"; "east";
        "east"; "take painting"; "west"; "west"; "south"; "up"; "put painting in case" ]
      (start ())
  in
  Alcotest.(check int) "the painting, 50" 50 g.world.score;
  let g =
    zork_play
      [ "east"; "east"; "north"; "north"; "climb tree"; "take egg"; "climb down"; "south"; "east"; "west"; "west";
        "put egg in case" ]
      g
  in
  Printf.eprintf "DBG zork:\n%s\n%!" (String.concat "\n" g.out);
  Alcotest.(check int) "the egg too, 100" 100 g.world.score;
  Alcotest.(check bool) "won" true (Adventure.has g.world "won");
  Alcotest.(check string) "and told so" "****  You have won  ****" (zork_last g)

(* The dark: in the cellar without the lamp lit nothing can be seen,
 * and going on is the grue. *)
let zork_grue () =
  let open TinyZork in
  let g = zork_play [ "north"; "east"; "open window"; "west"; "west"; "move rug"; "open trap door"; "down" ] (start ()) in
  Alcotest.(check string) "pitch black" "It is pitch black. You are likely to be eaten by a grue." (zork_last g);
  Alcotest.(check string) "nothing to see" "It's too dark to see!" (zork_last (command "take lamp" g));
  let g = command "north" g in
  Alcotest.(check bool) "eaten" true g.dead;
  Alcotest.(check string) "and told so" "****  You have died  ****" (zork_last g)

(* The troll: in the way until killed, and not with bare hands. *)
let zork_troll () =
  let open TinyZork in
  let g =
    zork_play [ "north"; "east"; "open window"; "west"; "west"; "take lamp"; "move rug"; "open trap door"; "turn on lamp"; "down"; "north" ]
      (start ())
  in
  Alcotest.(check string) "in the way" "The troll fends you off with a menacing gesture." (zork_last (command "east" g));
  Alcotest.(check string) "bare hands" "Attacking the troll with your bare hands is suicidal." (zork_last (command "kill troll" g));
  Alcotest.(check string) "the sword, not carried" "You see no sword here." (zork_last (command "kill troll with sword" g))

(*****************************************************************************)
(* TinyManiacMansion *)
(*****************************************************************************)

(* a verb clicked, then objects, then Dave left to walk there and do it *)
let mm_do (verb : string) (objs : string list) (p : TinyManiacMansion.play) : TinyManiacMansion.play =
  let open TinyManiacMansion in
  let p = List.fold_left (fun p o -> click_object o p) { p with verb; first = None } objs in
  let rec go n p = if n = 0 || (p.path = [] && p.doing = None) then p else go (n - 1) (step_walk p) in
  go 2000 p

(* Walkboxes: across the hall, round its table, by the front strip:
 * the middle of each edge crossed, then the spot. *)
let mm_walkboxes () =
  let open TinyManiacMansion in
  let pts = route "hall" (-300., -120.) (300., -120.) in
  Alcotest.(check (list (pair (float 0.01) (float 0.01)))) "round the table" [ (-295., -160.); (295., -160.); (300., -120.) ] pts;
  Alcotest.(check (list (pair (float 0.01) (float 0.01)))) "in one box, straight there" [ (-200., -130.) ] (route "hall" (-300., -120.) (-200., -130.));
  Alcotest.(check (list (pair (float 0.01) (float 0.01)))) "a click on the table: the floor nearest" [ (0., -160.) ] (route "hall" (-100., -180.) (0., -120.))

(* The whole game, verb by verb: the key under the mat, the door, the
 * soda for the tentacle, the flashlight for the dark, the odd book;
 * into the lab. *)
let mm_walkthrough () =
  let open TinyManiacMansion in
  let p = start () in
  let p = mm_do "Push" [ "mat" ] p in
  Alcotest.(check string) "a key" "Hey, there's a key under here!" p.saying;
  let p = mm_do "Pick up" [ "key" ] p |> mm_do "Open" [ "front door" ] in
  Alcotest.(check string) "locked" "It's locked." p.saying;
  let p = mm_do "Use" [ "key"; "front door" ] p |> mm_do "Open" [ "front door" ] |> mm_do "Walk to" [ "front door" ] in
  Alcotest.(check string) "in the hall" "hall" p.world.here;
  let p = mm_do "Walk to" [ "library door" ] p in
  Alcotest.(check string) "the tentacle" "Purple Tentacle: Stop right there, kid! Nobody goes in the library." p.saying;
  let p =
    p |> mm_do "Walk to" [ "kitchen door" ] |> mm_do "Open" [ "fridge" ] |> mm_do "Pick up" [ "soda" ] |> mm_do "Open" [ "drawer" ]
    |> mm_do "Pick up" [ "flashlight" ] |> mm_do "Walk to" [ "hall door" ] |> mm_do "Give" [ "soda"; "tentacle" ]
  in
  Alcotest.(check bool) "the tentacle gone" true (Adventure.where p.world "tentacle" = Adventure.Nowhere);
  let p = mm_do "Walk to" [ "library door" ] p in
  Alcotest.(check string) "in the library" "library" p.world.here;
  Alcotest.(check (list string)) "dark: only the way out" [ "library exit" ] (clickable p.world);
  let p = mm_do "Turn on" [ "flashlight" ] p in
  Alcotest.(check bool) "light: the book" true (List.mem "odd book" (clickable p.world));
  let p = mm_do "Pull" [ "odd book" ] p |> mm_do "Walk to" [ "secret door" ] in
  Alcotest.(check string) "into the lab" "lab" p.world.here

(*****************************************************************************)
(* TinyHamurabi *)
(*****************************************************************************)

(* A year, with the dice chosen: everyone fed, all the land sown, a
 * yield of 3, rats on an even throw of 2 (half the store), one
 * newcomer's die, no plague. *)
let hamurabi_year () =
  let open TinyHamurabi in
  let dice = { d_yield = 3; d_rats = 2; d_come = 1; d_plague = false; d_price = 20 } in
  match harvest dice { bought = 0; food = 2000; sown = 1000 } start_city with
  | Impeached _ -> Alcotest.fail "impeached"
  | Reigning c ->
      (* 2800 - 2000 eaten - 500 of seed + 3000 harvested = 3300, half to the rats *)
      Alcotest.(check int) "the rats' half" 1650 c.rats;
      Alcotest.(check int) "the store" 1650 c.grain;
      (* 1 * (20 * 1000 + 1650) / 100 / 100 + 1 *)
      Alcotest.(check int) "newcomers" 3 c.arrived;
      Alcotest.(check int) "the people" 103 c.people;
      Alcotest.(check int) "next year's price" 20 c.price;
      Alcotest.(check int) "year 2" 2 c.year

(* More than 45% starved in one year: the reign ends. The plague halves
 * the people. *)
let hamurabi_impeached_and_plague () =
  let open TinyHamurabi in
  let dice = { d_yield = 3; d_rats = 1; d_come = 1; d_plague = false; d_price = 20 } in
  (match harvest dice { bought = 0; food = 1000; sown = 0 } start_city with
   | Impeached (_, starved) -> Alcotest.(check int) "half starved" 50 starved
   | Reigning _ -> Alcotest.fail "still reigning");
  (match harvest dice { bought = 0; food = 1200; sown = 0 } start_city with
   | Reigning c -> Alcotest.(check int) "40 starved is survivable" 40 c.starved
   | Impeached _ -> Alcotest.fail "impeached at 40%");
  match harvest { dice with d_plague = true } { bought = 0; food = 2000; sown = 0 } start_city with
  | Reigning c -> Alcotest.(check bool) "the plague" true (c.plague && c.people < 100)
  | Impeached _ -> Alcotest.fail "impeached"

(* The questions check what is typed against the store, and say what
 * is lacking. *)
let hamurabi_think_again () =
  let open TinyHamurabi in
  let g = start 1 in
  let last g = List.nth g.out (List.length g.out - 2) in
  Alcotest.(check string) "not enough grain" "HAMURABI: THINK AGAIN. YOU HAVE ONLY 2800 BUSHELS OF GRAIN." (last (answer "1000" g));
  Alcotest.(check string) "not a number" "HAMURABI: A NUMBER, PLEASE." (last (answer "lots" g));
  let g = answer "0" g |> answer "2000" in
  Alcotest.(check string) "not enough land" "HAMURABI: THINK AGAIN. YOU OWN ONLY 1000 ACRES." (last (answer "1200" g));
  let g = { g with city = { g.city with people = 50 } } in
  Alcotest.(check string) "not enough hands" "HAMURABI: BUT YOU HAVE ONLY 50 PEOPLE TO TEND THE FIELDS!" (last (answer "900" g))

(* Ten years played through the questions, a careful king: no land
 * traded, the seed for all the land the people can farm kept first,
 * the rest eaten -- but never so little that 45% starve. The reign
 * ends in the verdict, not in impeachment. *)
let hamurabi_ten_years () =
  let open TinyHamurabi in
  let rec reign n g =
    if n = 0 || g.asked = Over then g
    else
      let c = g.city in
      let seed = min c.acres (10 * c.people) / 2 in
      let food = min c.grain (max (min (20 * c.people) (c.grain - seed)) (11 * c.people)) in
      let sown = min c.acres (min (10 * c.people) (2 * (c.grain - food))) in
      reign (n - 1) (g |> answer "0" |> answer (string_of_int food) |> answer (string_of_int sown))
  in
  let g = reign 20 (start 1968) in
  Printf.eprintf "DBG hamurabi:\n%s\n%!" (String.concat "\n" g.out);
  Alcotest.(check bool) "over" true (g.asked = Over);
  Alcotest.(check bool) "ten years, not impeached" true (g.city.year = 11);
  Alcotest.(check bool) "and judged" true
    (List.exists (fun l -> String.length l > 20 && String.sub l 0 20 = "IN YOUR 10-YEAR TERM") g.out)

(*****************************************************************************)
(* TinyTennisForTwo *)
(*****************************************************************************)

(* frames of [step] until the rally is over, or [n] frames *)
let tennis_until_point (n : int) (i : TinyTennisForTwo.input) (g : TinyTennisForTwo.game) : TinyTennisForTwo.game =
  let rec go n (g : TinyTennisForTwo.game) =
    match g.rally with TinyTennisForTwo.Point _ -> g | Flying -> if n = 0 then g else go (n - 1) (TinyTennisForTwo.step i g)
  in
  go n g

(* The serve: hit at 45 degrees, over the net, bouncing on the other
 * side; flat at 5 degrees, into the net, and the point to the
 * receiver. *)
let tennis_serve_and_net () =
  let open TinyTennisForTwo in
  let g = { start with computer = false } in
  let served = step { nothing with hit_l = true } g in
  Alcotest.(check bool) "the ball goes" true (served.ball.vx > 0.);
  let rec until_bounce n (g : game) = if n = 0 || g.ball.bounces > 0 || g.rally <> Flying then g else until_bounce (n - 1) (step nothing g) in
  let bounced = until_bounce 300 served in
  Alcotest.(check bool) "over the net, a bounce on the right" true (bounced.ball.x > 0. && bounced.ball.bounces = 1);
  let flat = tennis_until_point 300 nothing (step { nothing with hit_l = true } { g with knob_l = 5. }) in
  Alcotest.(check bool) "into the net: the right's point" true (flat.rally = Point (Right, 60));
  Alcotest.(check bool) "dead against the net" true (Float.abs flat.ball.x <= 1.)

(* Nobody hits it back: it bounces twice on the right, the left's
 * point; and after the pause, counted, and the right serves. *)
let tennis_two_bounces () =
  let open TinyTennisForTwo in
  let g = { start with computer = false } in
  let over = tennis_until_point 600 nothing (step { nothing with hit_l = true } g) in
  Alcotest.(check bool) "the left's point" true (over.rally = Point (Left, 60));
  let rec wait n g = if n = 0 then g else wait (n - 1) (step nothing g) in
  let next = wait 61 over in
  Alcotest.(check int) "counted" 1 next.score_l;
  Alcotest.(check bool) "the right serves" true (next.server = Right && next.ball.x > 0.)

(* The computer returns a serve: the ball comes back over the net, hit
 * by the right. *)
let tennis_computer_returns () =
  let open TinyTennisForTwo in
  let g = step { nothing with hit_l = true } start in
  let rec until_back n (g : game) =
    if n = 0 || g.rally <> Flying || (g.ball.hitter = Right && g.ball.crossed) then g else until_back (n - 1) (step nothing g)
  in
  let back = until_back 600 g in
  Printf.eprintf "DBG tennis: x %.0f, hitter right %b, crossed %b\n%!" back.ball.x (back.ball.hitter = Right) back.ball.crossed;
  Alcotest.(check bool) "hit back over the net" true (back.ball.hitter = Right && back.ball.crossed && back.ball.x < 0.)

(*****************************************************************************)
(* TinyTonyHawk *)
(*****************************************************************************)

(* frames of [step], the input chosen by [play] each frame, until
 * [stop] or [n] frames *)
let hawk_until (n : int) (stop : TinyTonyHawk.run -> bool) (play : TinyTonyHawk.run -> TinyTonyHawk.input) (g : TinyTonyHawk.run) :
    TinyTonyHawk.run * int =
  let rec go k g = if k = n || stop g then (g, k) else go (k + 1) (TinyTonyHawk.step (play g) g) in
  go 0 g

let hawk_in_air (g : TinyTonyHawk.run) : bool = match g.skater with Air _ -> true | _ -> false
let hawk_pumping (_ : TinyTonyHawk.run) : TinyTonyHawk.input = { TinyTonyHawk.nothing with down = true }

(* pumped up to an air of at least [vy] off the lip *)
let hawk_high_air (vy : float) : TinyTonyHawk.run =
  fst (hawk_until 3000 (fun g -> match g.skater with Air a -> a.vy >= vy | _ -> false) hawk_pumping TinyTonyHawk.start)

(* The ramp is one curve: its pieces meet; dropped in from a lip with
 * no input, the skater rolls to the other side and back, almost up
 * to the lips (the friction's loss) but never over them. *)
let hawk_ramp () =
  let open TinyTonyHawk in
  let near (x1, y1) (x2, y2) = Float.abs (x1 -. x2) < 0.01 && Float.abs (y1 -. y2) < 0.01 in
  Alcotest.(check bool) "flat to curve" true (near (pos flat) (pos (flat +. 0.0001)));
  Alcotest.(check bool) "curve to vert" true (near (pos s_curve) (pos (s_curve +. 0.0001)));
  Alcotest.(check bool) "the lip" true (near (pos (-.s_lip)) (-.(flat +. r), lip_height));
  let top = ref (-1000.) in
  let g, _ =
    hawk_until 400 hawk_in_air (fun g -> if g.s > 0. then top := Float.max !top (snd (pos g.s)); nothing) start
  in
  Printf.eprintf "DBG hawk: highest on the right %.1f (lip %.0f)\n%!" !top lip_height;
  Alcotest.(check bool) "never in the air" false (hawk_in_air g);
  Alcotest.(check bool) "almost up to the lip" true (!top > lip_height -. 40. && !top < lip_height)

(* Pumping (down held) takes the skater over a lip, and back down on
 * it: a landing without a trick, nothing to bank; and pumping on, the
 * airs get higher. *)
let hawk_pump_to_air () =
  let open TinyTonyHawk in
  let g, k = hawk_until 1500 hawk_in_air hawk_pumping start in
  Printf.eprintf "DBG hawk: in the air after %d frames\n%!" k;
  let top = ref 0. in
  let _ = hawk_until 1500 (fun _ -> false) (fun g -> (match g.skater with Air a ->  top := Float.max !top a.height | _ -> ()); hawk_pumping g) start in
  Printf.eprintf "DBG hawk: the highest air %.0f\n%!" !top;
  Alcotest.(check bool) "higher and higher" true (!top > 150.);
  Alcotest.(check bool) "in the air" true (hawk_in_air g);
  let g, _ = hawk_until 200 (fun g -> not (hawk_in_air g)) (fun _ -> nothing) g in
  Alcotest.(check bool) "landed" true (g.skater = Rolling && g.combo = [])

(* the chain's arithmetic: a kickflip, an indy, and a kickflip again,
 * worth half *)
let hawk_combo_score () =
  let open TinyTonyHawk in
  let kf = { name = "kickflip"; points = 100 } and indy = { name = "indy"; points = 130 } in
  Alcotest.(check int) "the second kickflip" 50 (trick_value [ indy; kf ] kf);
  Alcotest.(check int) "(100 + 130 + 50) x 3" 840 (combo_score [ kf; indy; kf ])

(* In the air: a kickflip and a half turn land, the chain banked (the
 * revert's window let go by); a quarter turn does not. *)
let hawk_landing () =
  let open TinyTonyHawk in
  let air = hawk_high_air 9. in
  let spin deg (g : run) =
    match g.skater with Air a when Float.abs a.spun < deg -> { nothing with dir = 1.; flip = a.height > 20. && a.vy > 0. && a.flip = None && g.combo = [] } | _ -> nothing
  in
  let landed, _ = hawk_until 300 (fun g -> not (hawk_in_air g)) (spin 180.) air in
  Printf.eprintf "DBG hawk: landed, %s\n%!" (String.concat " + " (List.map (fun (t : trick) -> t.name) landed.combo));
  Alcotest.(check (list string)) "a kickflip and a 180" [ "180"; "kickflip" ] (List.map (fun (t : trick) -> t.name) landed.combo);
  let banked, _ = hawk_until 20 (fun g -> g.combo = []) (fun _ -> nothing) landed in
  Alcotest.(check int) "banked: (100 + 100) x 2" 400 banked.score;
  let fell, _ = hawk_until 300 (fun g -> not (hawk_in_air g)) (spin 90.) air in
  Alcotest.(check bool) "a quarter turn: a bail" true (match fell.skater with Bailed _ -> true | _ -> false)

(* The chain across the half-pipe: a kickflip, a revert on landing, a
 * manual on the flat, balanced, then up the other side and a grab:
 * five tricks, one chain. *)
let hawk_revert_manual () =
  let open TinyTonyHawk in
  let air = hawk_high_air 8. in
  let robot (g : run) : input =
    let a = Float.abs g.s in
    match g.skater with
    | Air x when g.combo = [] || List.length g.combo = 3 ->
        if x.vy > 0. && x.height > 40. && x.flip = None then { nothing with flip = List.length g.combo = 0; grab = List.length g.combo = 3 } else nothing
    | Air x -> { nothing with grab = x.grab <> None && x.grab <> Some ("indy", 20) && x.vy > -.5. }
    | Manual b -> { nothing with dir = -.(sign (b.needle +. (b.speed *. 10.))) }
    | Rolling when g.landed > 0 -> { nothing with revert = true }
    | Rolling when a <= flat -> { nothing with up = true; up_held = true }
    | _ -> { nothing with down = true }
  in
  let g, k = hawk_until 1000 (fun g -> List.length g.combo = 4 && not (hawk_in_air g) || g.score > 0 || match g.skater with Bailed _ -> true | _ -> false) robot air in
  Printf.eprintf "DBG hawk chain after %d frames: %s, score %d\n%!" k (String.concat " + " (List.rev_map (fun (t : trick) -> t.name) g.combo)) g.score;
  Alcotest.(check (list string)) "one chain" [ "indy"; "manual"; "revert"; "kickflip" ] (List.map (fun (t : trick) -> t.name) g.combo)

(*****************************************************************************)
(* TinyMazeWar *)
(*****************************************************************************)

(* a game with only the eyes given, (col, row, dir) each, you first *)
let mazewar_with (eyes : (int * int * int) list) : TinyMazeWar.game =
  let open TinyMazeWar in
  { (start ()) with eyes = Array.of_list (List.mapi (fun i (c, r, d) -> eye (Printf.sprintf "e%d" i) c r d) eyes) }

let mazewar_cmds (n : int) (you : TinyMazeWar.command) : TinyMazeWar.command array =
  Array.init n (fun k -> if k = 0 then you else TinyMazeWar.Nothing)

(* The view's frames: a division by the distance. *)
let mazewar_frames () =
  let open TinyMazeWar in
  Alcotest.(check (float 0.001)) "frame 0: the view's edge" box_half (frame 0);
  Alcotest.(check (float 0.001)) "frame 1: a third" (box_half /. 3.) (frame 1);
  Alcotest.(check (float 0.001)) "frame 2: a fifth" (box_half /. 5.) (frame 2)

(* Moving: a step along the corridor, none into a wall. Shooting: the
 * first one down the corridor, not the one behind a wall. *)
let mazewar_move_and_shoot () =
  let open TinyMazeWar in
  (* you at (1, 1) facing east, along row 1; e1 at (5, 1), e2 at (1, 3)
   * down the other corridor *)
  let g = mazewar_with [ (1, 1, 1); (5, 1, 3); (1, 3, 0) ] in
  let moved = tick (mazewar_cmds 3 Forward) g in
  Alcotest.(check int) "a step" 2 moved.eyes.(0).c;
  let blocked = tick (mazewar_cmds 3 Forward) (mazewar_with [ (1, 1, 0); (5, 1, 3); (1, 3, 0) ]) in
  Alcotest.(check int) "not into the wall" 1 blocked.eyes.(0).r;
  let shot = tick (mazewar_cmds 3 Fire) g in
  Alcotest.(check bool) "e1 shot" true (shot.eyes.(1).dead > 0);
  Alcotest.(check int) "a kill" 1 shot.eyes.(0).kills;
  Alcotest.(check int) "e2 untouched" 0 shot.eyes.(2).dead;
  (* the wall between (5, 1) and (5, 3): e1 can't see e2 *)
  Alcotest.(check bool) "no one down a walled way" true (down_the_corridor g 5 1 2 = None);
  let again = tick (mazewar_cmds 3 Fire) shot in
  Alcotest.(check int) "reloading: no second shot yet" 1 again.eyes.(0).kills

(* The dead come back, after a while, somewhere free and away from the
 * others. *)
let mazewar_respawn () =
  let open TinyMazeWar in
  let g = tick (mazewar_cmds 2 Fire) (mazewar_with [ (1, 1, 1); (5, 1, 3) ]) in
  let rec wait n g = if n = 0 then g else wait (n - 1) (tick (mazewar_cmds 2 Nothing) g) in
  let back = wait 100 g in
  let e = back.eyes.(1) in
  Alcotest.(check int) "alive again" 0 e.dead;
  Alcotest.(check bool) "on a free cell" false (wall e.c e.r);
  Alcotest.(check bool) "away from you" true (abs (e.c - 1) + abs (e.r - 1) >= 5)

(* A robot facing you down a corridor shoots you. And robots left to
 * themselves hunt each other: in a minute, someone has been shot. *)
let mazewar_robots () =
  let open TinyMazeWar in
  let g = mazewar_with [ (1, 1, 1); (5, 1, 3) ] in
  let rec until_shot n g = if n = 0 || g.eyes.(0).dead > 0 then g else until_shot (n - 1) (tick [| Nothing; robot g 1 |] g) in
  Alcotest.(check bool) "it shoots you" true ((until_shot 60 g).eyes.(0).deaths = 1);
  let g = ref (start ()) in
  for _ = 1 to 3600 do
    let cur = !g in
    g := { (tick (Array.init (Array.length cur.eyes) (fun k -> robot cur k)) cur) with rng = next cur.rng }
  done;
  let kills = Array.fold_left (fun a e -> a + e.kills) 0 !g.eyes in
  Printf.eprintf "DBG mazewar: %d kills in a minute of robots\n%!" kills;
  Alcotest.(check bool) "someone got shot" true (kills > 0)

(*****************************************************************************)
(* TinyFez *)
(*****************************************************************************)

let fez_frames (n : int) (i : TinyFez.input) (p : TinyFez.play) : TinyFez.play =
  List.fold_left (fun p i -> TinyFez.step i p) p (List.init n (fun _ -> i))

(* run to column [u] (in the view), then stand still *)
let fez_walk_to (u : int) (p : TinyFez.play) : TinyFez.play =
  let open TinyFez in
  let rec go k p =
    let target = center_u u in
    if k = 0 || Float.abs (p.u -. target) < 3. then fez_frames 5 nothing p
    else go (k - 1) (step { nothing with dx = (if p.u < target then 1. else -1.) } p)
  in
  go 300 p

(* a jump [cells] columns across (12 frames of running a column), then
 * waiting to land *)
let fez_hop (dir : float) (cells : int) (p : TinyFez.play) : TinyFez.play =
  let open TinyFez in
  let p = step { nothing with dx = dir; jump = true } p in
  let p = fez_frames ((cells * 12) - 1) { nothing with dx = dir } p in
  let rec down k p = if k = 0 || p.ground then p else down (k - 1) (step nothing p) in
  fez_frames 3 nothing (down 120 p)

let fez_turn (dir : int) (p : TinyFez.play) : TinyFez.play = fez_frames 20 TinyFez.nothing (TinyFez.step { TinyFez.nothing with turn = dir } p)

(* The views: each a quarter turn of the others, and [of_grid] undoes
 * [to_grid]. *)
let fez_views () =
  let open TinyFez in
  for view = 0 to 3 do
    for u = 0 to n - 1 do
      for d = 0 to n - 1 do
        let x, z = to_grid view u d in
        Alcotest.(check (pair int int)) "back again" (u, d) (of_grid view x z)
      done
    done
  done;
  (* the tower's east ledge, (5, 3): across at 5 from the front, in the
   * middle of the right view, and at 1 from the back *)
  Alcotest.(check (pair int int)) "front" (5, 3) (of_grid 0 5 3);
  Alcotest.(check (pair int int)) "right" (3, 1) (of_grid 1 5 3);
  Alcotest.(check (pair int int)) "back" (1, 3) (of_grid 2 5 3)

(* Fez's rules: in front of the tower (behind him: background) Gomez
 * walks across it; at the tower's depth or behind it, it is a wall. *)
let fez_background_and_walls () =
  let open TinyFez in
  let p = fez_frames 10 nothing (start ()) in
  Alcotest.(check int) "on the island's front row" 2 p.depth;
  let across = fez_walk_to 4 p in
  Alcotest.(check bool) "walked across the tower's column" true (cell_u across = 4);
  (* the walls from the front: the tower's column, at the tower's depth
   * or behind it; not in front of it *)
  let tower_at depth = Tilemap.get (walls 0 depth) 3 (h - 1 - 4) = Some '#' in
  Alcotest.(check bool) "at its depth, a wall" true (tower_at 3);
  Alcotest.(check bool) "in front, background" false (tower_at 2)

(* The whole level, played: the east ledge; turned, the south ledge
 * and its bit; turned back, the west ledge; turned, the north ledge
 * and its bit (in front of the tower, which the turn put behind him);
 * turned back, the high east ledge, and the tower's top and its bit. *)
let fez_the_cube () =
  let open TinyFez in
  let p = fez_frames 10 nothing (start ()) in
  let show what (p : play) =
    Printf.eprintf "DBG fez %s: view %d, u %d (%.0f), y %.0f, depth %d on %d, ground %b, bits %d\n%!" what p.view (cell_u p) p.u p.y
      p.depth p.on_depth p.ground (List.length p.bits);
    p
  in
  let p = p |> fez_walk_to 4 |> show "at 4" |> fez_hop 1. 1 |> show "east" |> fez_turn 1 |> show "turned" |> fez_hop (-1.) 2 |> show "south" in
  Alcotest.(check int) "the south bit" 2 (List.length p.bits);
  let p = p |> fez_turn (-1) |> show "back" |> fez_hop (-1.) 2 |> show "west" |> fez_turn 1 |> show "turned" in
  Alcotest.(check bool) "turned, brought in front of the tower" true (p.depth < 3);
  let p = p |> fez_hop 1. 2 |> show "north" in
  Alcotest.(check int) "the north bit" 1 (List.length p.bits);
  let p = p |> fez_turn (-1) |> fez_hop 1. 2 |> fez_hop (-1.) 2 in
  Printf.eprintf "DBG fez: view %d, u %.0f, y %.0f, depth %d, bits %d\n%!" p.view p.u p.y p.depth (List.length p.bits);
  Alcotest.(check int) "the cube is whole" 0 (List.length p.bits)

(*****************************************************************************)
(* TinyPerspective *)
(*****************************************************************************)

let perspective_screen = Playground.to_screen 1000. 1000.

let perspective_frames (n : int) (i : TinyPerspective.input) (p : TinyPerspective.play) : TinyPerspective.play =
  List.fold_left (fun p i -> TinyPerspective.step perspective_screen i p) p (List.init n (fun _ -> i))

(* The runner starts on the start ledge's picture, standing; from the
 * first camera, the gap is a fall. *)
let perspective_start () =
  let open TinyPerspective in
  let p = perspective_frames 30 nothing (enter perspective_screen 0) in
  Printf.eprintf "DBG perspective start: x %.0f y %.0f ground %b\n%!" p.x p.y p.ground;
  Alcotest.(check bool) "standing" true p.ground;
  let ran = perspective_frames 300 { nothing with dx = 1. } p in
  Printf.eprintf "DBG perspective ran from the start: x %.0f y %.0f\n%!" ran.x ran.y;
  Alcotest.(check bool) "the gap, a fall" true (fell perspective_screen ran)

(* The far bridge: the camera lowered to the ledges' height, every top
 * at that height is on the horizon, and the far bridge fills the gap:
 * the runner runs across to the goal. *)
let perspective_bridge () =
  let open TinyPerspective in
  let p = perspective_frames 30 nothing (enter perspective_screen 0) in
  let p = step perspective_screen { nothing with switch = true } p in
  let rec lower n p = if n = 0 || p.pitch <= 0. then p else lower (n - 1) (step perspective_screen { nothing with dy = -1. } p) in
  let p = lower 100 p in
  let p = step perspective_screen { nothing with switch = true } p in
  Printf.eprintf "DBG perspective bridge: pitch %.0f, %s, x %.0f y %.0f\n%!" p.pitch p.message p.x p.y;
  let rec run n p = if n = 0 || at_goal p || fell perspective_screen p then p else run (n - 1) (step perspective_screen { nothing with dx = 1. } p) in
  let p = run 400 p in
  Printf.eprintf "DBG perspective ran: x %.0f y %.0f\n%!" p.x p.y;
  Alcotest.(check bool) "at the goal" true (at_goal p)

(* a robot runner: right, jumping at walls and at edges *)
let perspective_robot (n : int) (p : TinyPerspective.play) : TinyPerspective.play =
  let open TinyPerspective in
  let rec go n p =
    if n = 0 || at_goal p || fell perspective_screen p then p
    else
      let jump =
        match p.mode with
        | Runner map ->
            let feet = p.y -. (snd size /. 2.) in
            p.ground
            && (Tile_move.hits solid map size (p.x +. 6.) p.y
               || not (Tile_move.hits solid map (4., 4.) (p.x +. 16.) (feet -. 4.)))
        | Camera -> false
      in
      go (n - 1) (step perspective_screen { nothing with dx = 1.; jump } p)
  in
  go n p

(* The stairs: from the first camera, the robot does not get up them;
 * turned a little and lowered below the pillars' tops, their pictures
 * are steps a jump high -- one of the cameras a search over yaw, pitch
 * and distance found, about one in nine. *)
let perspective_stairs () =
  let open TinyPerspective in
  let first = perspective_robot 500 (perspective_frames 5 nothing (enter perspective_screen 1)) in
  Alcotest.(check bool) "not from the first camera" false (at_goal first);
  let p = enter perspective_screen 1 in
  let p = to_runner perspective_screen (stand perspective_screen { p with mode = Camera; yaw = -20.; pitch = -6.; dist = 24. }) in
  Alcotest.(check bool) "the picture taken" true (match p.mode with Runner _ -> true | Camera -> false);
  Alcotest.(check bool) "up the stairs" true (at_goal (perspective_robot 500 (perspective_frames 5 nothing p)))

(*****************************************************************************)
(* TinyPrinceOfPersia *)
(*****************************************************************************)

(* The distances are the tables' sums: a stride is a tile, a standing
 * jump two, a running jump three, and the jumps come down where they
 * took off; climbing up rises by what hanging lowers. *)
let pop_tables () =
  let open TinyPrinceOfPersia in
  let sum field m = Array.fold_left (fun a fr -> a +. field fr) 0. (anims m).frames in
  let dx fr = fr.dx and dy fr = fr.dy in
  Alcotest.(check (float 0.01)) "a stride" tile_w (sum dx Run);
  Alcotest.(check (float 0.01)) "a standing jump" (2. *. tile_w) (sum dx Stand_jump);
  Alcotest.(check (float 0.01)) "a running jump" (3. *. tile_w) (sum dx Run_jump);
  Alcotest.(check (float 0.01)) "down where it took off" 0. (sum dy Stand_jump);
  Alcotest.(check (float 0.01)) "down where it took off, running" 0. (sum dy Run_jump);
  Alcotest.(check (float 0.01)) "climbing up" hang_drop (sum dy Climb_up);
  Alcotest.(check (float 0.01)) "climbing down" (-.hang_drop) (sum dy Climb_down);
  Alcotest.(check (float 0.01)) "jumping up to the ledge, then hanging" tile_h (sum dy Jump_grab +. hang_drop)

(* [pop_play script frames]: the game played from its title, [script]
 * the keys held, as -script says them (e.g. "right:10-40"; space at
 * frame 1 starts the game); the scenes seen, one per frame *)
let pop_play (script : string) (frames : int) : TinyPrinceOfPersia.scene list =
  let open TinyPrinceOfPersia in
  let held = List.map (fun e -> Scanf.sscanf e "%[^:]:%d-%d" (fun k a b -> (k, a, b))) (String.split_on_char ',' script) in
  let s = ref initial_model and seen = ref [] in
  for i = 1 to frames do
    let down k = List.exists (fun (k', a, b) -> k' = k && a <= i && i <= b) held in
    let keyboard = { initial_computer.keyboard with kspace = i = 1; kup = down "up"; kdown = down "down"; kleft = down "left"; kright = down "right"; kshift = down "Shift" } in
    s := update (computer ~keyboard i) !s;
    seen := !s.scene :: !seen
  done;
  List.rev !seen

(* the golden frames' way through the dungeon: a standing jump over the
 * first gap, a running jump over the spikes, down the hole onto the
 * plate, through the gate before it closes, a careful step to the
 * shaft's edge, hanging, the drop, a standing jump over the second
 * spikes, up to the ledge, and the door; never hurt *)
let pop_way_out =
  "right:10-40,up:54-58,right:54-58,right:100-290,up:193-197,left:330-550,left:600-601,Shift:600-601,down:630-634,down:690-694,\
   right:745-768,up:790-794,right:790-794,right:840-1060,up:1070-1074,up:1110-1114,right:1150-1190,up:1200-1240"

let pop_robot () =
  let open TinyPrinceOfPersia in
  let seen = pop_play pop_way_out 1300 in
  let games = List.filter_map (function Playing g -> Some g | _ -> None) seen in
  Alcotest.(check bool) "hung from a ledge" true (List.exists (fun g -> g.prince.move = Hang) games);
  Alcotest.(check int) "never hurt" 3 (List.fold_left (fun m g -> min m g.prince.hp) 3 games);
  Alcotest.(check bool) "escaped" true (List.exists (function Escaped _ -> true | _ -> false) seen)

(* the same way, but waiting too long off the plate: the gate closes
 * and stops him; running into the shaft instead of hanging from its
 * edge is a fall of two floors, a triangle *)
let pop_gate_and_fall () =
  let open TinyPrinceOfPersia in
  let last seen = match List.rev seen with Playing g :: _ -> g | _ -> Alcotest.fail "not playing" in
  let g = last (pop_play "right:10-40,up:54-58,right:54-58,right:100-290,up:193-197,left:330-370,left:800-1200" 1200) in
  Alcotest.(check int) "stopped by the gate" 5 (col_at g.prince.x);
  let g = last (pop_play "right:10-40,up:54-58,right:54-58,right:100-290,up:193-197,left:330-600" 800) in
  Alcotest.(check int) "down the shaft" 3 (row_at g.prince.y);
  Alcotest.(check int) "a triangle less" 2 g.prince.hp

(*****************************************************************************)
(* TinyDDR *)
(*****************************************************************************)

(* The chart is the melody: a step on each note, at the note's start,
 * the arrow following the tune's shape. *)
let ddr_chart_is_the_tune () =
  let open TinyDDR in
  let beat = 60. /. 128. in
  (match steps with
  | a :: b :: c :: d :: _ ->
      (* two silent bars (8 beats) before the first note *)
      Alcotest.(check (float 0.001)) "the first step, after the count-in" (8. *. beat) a.at;
      Alcotest.(check (float 0.001)) "then a note a beat" (9. *. beat) b.at;
      (* c e g e: a first arrow, up, up, and down *)
      Alcotest.(check bool) "c e g e is left, up, up, down" true
        (a.lane = Left && b.lane = Up && c.lane = Up && d.lane = Down)
  | _ -> Alcotest.fail "a chart of fewer than four steps");
  (* the melody's eight bars hold 4 + 3 + 4 + 3 + 4 + 3 + 4 + 1 notes *)
  Alcotest.(check int) "one step per note of the melody" 26 (List.length steps);
  Alcotest.(check bool) "all within the song" true (List.for_all (fun s -> s.at < song_length) steps)

(* On the beat is PERFECT, a little off still counts, too far off is
 * not a step at all. *)
let ddr_judging () =
  let open TinyDDR in
  let is j e = Alcotest.(check bool) (Printf.sprintf "%.0f ms" (e *. 1000.)) true (judge e = j) in
  is (Some Perfect) 0.;
  is (Some Perfect) (-0.025);
  is (Some Great) 0.05;
  is (Some Good) (-0.09);
  is (Some Almost) 0.13;
  is None 0.2;
  (* and a step let go by is a miss, which ends the combo *)
  let first = List.hd steps in
  let d = start_dance 0. 0. in
  let hit = dance_step first.at [ first.lane ] d in
  Alcotest.(check int) "on the beat: a hundred" 100 hit.score;
  Alcotest.(check int) "and a combo of one" 1 hit.combo;
  let after = dance_step (first.at +. 3.) [] hit in
  Alcotest.(check bool) "the next ones went by: misses" true
    (List.exists (fun (_, j) -> j = Some Miss) after.judged && after.combo = 0)

(* The game measures its machine through its player: someone who
 * dances steadily to what they hear is late by the latency, and the
 * average of their errors is the calibration to set. *)
let ddr_average_error_is_the_calibration () =
  let open TinyDDR in
  let late = 0.040 in
  let d =
    List.fold_left (fun d (s : step) -> dance_step (s.at +. late) [ s.lane ] d) (start_dance 0. 0.) steps
  in
  (match average_error d with
  | Some e -> Alcotest.(check (float 0.001)) "forty milliseconds late, on average" late e
  | None -> Alcotest.fail "no step was hit");
  Alcotest.(check bool) "and every step was a hit" true (List.for_all (fun (_, j) -> j <> None && j <> Some Miss) d.judged)

(* The clock is the music's: Audio.position is how much of the song the
 * sound card has been fed, and the dance's time is that less the
 * calibration. A frame the game spends late does not move it. *)
let ddr_the_clock_is_the_music () =
  let open TinyDDR in
  Audio.stop "song";
  ignore (Audio.pull 735);
  Audio.loop "song" song;
  for _ = 1 to 120 do ignore (Audio.pull 735) done;
  (match Audio.position "song" with
  | Some p ->
      Alcotest.(check (float 1e-6)) "two seconds of song fed to the card" 2. p;
      Alcotest.(check (float 1e-6)) "heard 50 ms later" 1.95 (song_time ~position:p ~offset:0.05)
  | None -> Alcotest.fail "the song should be playing");
  Audio.stop "song";
  ignore (Audio.pull 735)

(*****************************************************************************)
(* TinyRockBand *)
(*****************************************************************************)

(* The parts are the voices of one tune: the guitar plays the melody,
 * the bass the bass line, the keys the chords -- several frets at once
 * -- and the drums the beat, on a pedal and four pads. *)
let rockband_parts_are_voices () =
  let open TinyRockBand in
  let guitar = chart Guitar Expert and bass = chart Bass Expert and keys = chart Keys Expert in
  let drums = chart Drums Expert in
  (* the melody's eight bars: 4 + 3 + 4 + 3 + 4 + 3 + 4 + 1 notes; the
   * bass's: two a bar but the last *)
  Alcotest.(check int) "the guitar plays the melody" 26 (List.length guitar);
  Alcotest.(check int) "the bass plays the bass line" 15 (List.length bass);
  let at_once (l : int Rhythm.note list) =
    List.exists (fun (n : int Rhythm.note) -> List.length (List.filter (fun (m : int Rhythm.note) -> m.at = n.at) l) >= 2) l
  in
  Alcotest.(check bool) "the keys play chords: frets together" true (at_once keys);
  Alcotest.(check bool) "five frets, no more" true
    (List.for_all (fun (n : int Rhythm.note) -> n.lane >= 0 && n.lane <= 4) (guitar @ bass @ keys @ drums));
  let on lane = List.exists (fun (n : int Rhythm.note) -> n.lane = lane) drums in
  Alcotest.(check bool) "the drums: kick, snare, hi-hat, toms, crash" true (List.for_all on [ 0; 1; 2; 3; 4 ])

(* Every part has its difficulty. The keys' chords were three keys at
 * once; on Medium they are one, on Easy on three keys only. *)
let rockband_difficulty () =
  let open TinyRockBand in
  let widest l =
    List.fold_left
      (fun w (n : int Rhythm.note) -> max w (List.length (List.filter (fun (m : int Rhythm.note) -> m.at = n.at) l)))
      0 l
  in
  Alcotest.(check int) "the keys on Expert: triads" 3 (widest (chart Keys Expert));
  Alcotest.(check int) "on Medium: one key at a time" 1 (widest (chart Keys Medium));
  Alcotest.(check bool) "on Easy: three keys" true
    (List.for_all (fun (n : int Rhythm.note) -> n.lane <= 2) (chart Keys Easy))

(* The drums are reduced their own way: the pedal only from Hard, one
 * pad at a time below Expert, Easy only on the beats. *)
let rockband_drums_reduced () =
  let open TinyRockBand in
  let kicks l = List.length (List.filter (fun (n : int Rhythm.note) -> n.lane = 0) l) in
  let count level = List.length (chart Drums level) in
  Alcotest.(check bool) "Expert has the pedal" true (kicks (chart Drums Expert) > 0);
  Alcotest.(check int) "Medium: no pedal" 0 (kicks (chart Drums Medium));
  Alcotest.(check bool) "each level fewer hits" true
    (count Easy < count Medium && count Medium < count Hard && count Hard < count Expert)

(* A guitar note is two hands: the fret alone plays nothing, the strum
 * with the fret down plays it. *)
let rockband_guitar_strums () =
  let open TinyRockBand in
  let n = List.hd (chart Guitar Expert) in
  let p = start Guitar Expert 0. 0. in
  let fret_only = play_step n.at ~strum:false ~held:[ n.lane ] ~pressed:[ n.lane ] p in
  Alcotest.(check int) "the fret alone: nothing" 0 fret_only.perf.score;
  let strummed = play_step n.at ~strum:true ~held:[ n.lane ] ~pressed:[] p in
  Alcotest.(check int) "fret, then strum, on the beat" 100 strummed.perf.score

(* A keyboard has no strum: the key is the note. *)
let rockband_keys_no_strum () =
  let open TinyRockBand in
  let n = List.hd (chart Keys Expert) in
  let p = start Keys Expert 0. 0. in
  let strummed = play_step n.at ~strum:true ~held:[ n.lane ] ~pressed:[] p in
  Alcotest.(check int) "a strum on a keyboard: nothing" 0 strummed.perf.score;
  let keyed = play_step n.at ~strum:false ~held:[ n.lane ] ~pressed:[ n.lane ] p in
  Alcotest.(check int) "the key, on the beat" 100 keyed.perf.score

(* A drum is struck: the pad pressed, or the pedal -- the space bar,
 * the strum of the others -- on its own. *)
let rockband_drums_struck () =
  let open TinyRockBand in
  let kick = List.find (fun (n : int Rhythm.note) -> n.lane = 0) (chart Drums Expert) in
  let p = start Drums Expert 0. 0. in
  let pedal = play_step kick.at ~strum:true ~held:[] ~pressed:[] p in
  Alcotest.(check bool) "the pedal plays the kick" true (pedal.perf.score >= 100);
  let snare = List.find (fun (n : int Rhythm.note) -> n.lane = 1) (chart Drums Expert) in
  let pad = play_step snare.at ~strum:false ~held:[ 1 ] ~pressed:[ 1 ] p in
  Alcotest.(check int) "a pad, struck on the beat" 100 pad.perf.score

(* A long note held for its length goes on scoring; let go of, it stops. *)
let rockband_sustain () =
  let open TinyRockBand in
  let long = List.find (fun (n : int Rhythm.note) -> n.length >= Rhythm.sustain_min) (chart Guitar Expert) in
  let hold frames holding =
    let p = ref (play_step long.at ~strum:true ~held:[ long.lane ] ~pressed:[] (start Guitar Expert 0. 0.)) in
    for f = 1 to frames do
      p := play_step (long.at +. (float_of_int f /. 60.)) ~strum:false ~held:(if holding then [ long.lane ] else []) ~pressed:[] !p
    done;
    !p.sustain
  in
  Alcotest.(check bool) "held: the note keeps scoring" true (hold 40 true > 30);
  Alcotest.(check int) "let go: it stops" 0 (hold 40 false)

(* The crowd is one meter for the band: a song left unplayed empties it,
 * and the band is booed off. *)
let rockband_crowd () =
  let open TinyRockBand in
  let p = start Guitar Medium 0. 0. in
  let ignored = play_step (song_length +. 1.) ~strum:false ~held:[] ~pressed:[] p in
  Alcotest.(check bool) "every note a miss" true
    (List.for_all (fun (_, j) -> j = Some Rhythm.Miss) ignored.perf.judged);
  Alcotest.(check bool) "and the band booed off" true (booed ignored)

(*****************************************************************************)
(* TinyGuitarHero *)
(*****************************************************************************)

(* The highway is Out Run's road straightened: one division by the
 * distance. The line is 630 px under the horizon and 500 wide, and a
 * thing further up the road is higher, smaller, and nearer the middle. *)
let gh_the_road () =
  let open TinyGuitarHero in
  let x0, y0, s0 = project 2.5 0. in
  Alcotest.(check (float 1e-6)) "the line: 630 px under the horizon" (horizon -. 630.) y0;
  Alcotest.(check (float 1e-6)) "and 500 wide" 250. x0;
  let x1, y1, s1 = project 2.5 10. in
  Alcotest.(check bool) "further: higher" true (y1 > y0);
  Alcotest.(check bool) "smaller" true (s1 < s0);
  Alcotest.(check bool) "and the lanes closer together" true (x1 < x0);
  let _, y_far, _ = project 0. 1e9 in
  Alcotest.(check bool) "and at the end of it all, the horizon" true (Float.abs (y_far -. horizon) < 1e-3)

(* The difficulty is the same part, reduced: Expert has the power
 * chords, Hard their outline, and Easy is single notes on three frets. *)
let gh_difficulty () =
  let open TinyGuitarHero in
  let chords part =
    List.exists (fun (n : int Rhythm.note) -> List.length (List.filter (fun (m : int Rhythm.note) -> m.at = n.at) part) >= 2) part
  in
  Alcotest.(check bool) "expert: power chords" true (chords (part Rhythm.Expert));
  let easy = part Rhythm.Easy in
  Alcotest.(check bool) "easy: no chords" false (chords easy);
  Alcotest.(check bool) "on three frets" true (List.for_all (fun (n : int Rhythm.note) -> n.lane < 3) easy);
  (* one note per chord on easy and medium: the same number of notes as
   * the tune has chords and notes *)
  Alcotest.(check int) "easy and medium, as many notes" (List.length (part Rhythm.Medium)) (List.length easy);
  Alcotest.(check bool) "fewer than expert" true (List.length easy < List.length (part Rhythm.Expert))

(* A note is two hands: the fret held does nothing until the strum. *)
let gh_strum () =
  let open TinyGuitarHero in
  let n = List.hd (part Rhythm.Expert) in
  let p = start Rhythm.Expert 0. 0. in
  Alcotest.(check int) "the fret alone" 0 (play_step n.at ~strum:false ~held:[ n.lane ] p).perf.score;
  Alcotest.(check int) "fret, then strum, on the beat" 100 (play_step n.at ~strum:true ~held:[ n.lane ] p).perf.score

(* A long note held after the strum goes on scoring. The song's half
 * notes last 0.91 s at 132 beats a minute, over the kit's 0.75. *)
let gh_sustain () =
  let open TinyGuitarHero in
  let long = List.find (fun (n : int Rhythm.note) -> n.length >= Rhythm.sustain_min) (part Rhythm.Expert) in
  let run holding =
    let p = ref (play_step long.at ~strum:true ~held:[ long.lane ] (start Rhythm.Expert 0. 0.)) in
    for f = 1 to 40 do
      p := play_step (long.at +. (float_of_int f /. 60.)) ~strum:false ~held:(if holding then [ long.lane ] else []) !p
    done;
    !p.sustain
  in
  Alcotest.(check bool) "held" true (run true > 30);
  Alcotest.(check int) "let go" 0 (run false)

(*****************************************************************************)
(* TinySimCity *)
(*****************************************************************************)

(* a city with [tool] used on each of [cells], the months left alone *)
let simcity_with (city : TinySimCity.city) (tool : TinySimCity.tool) (cells : (int * int) list) =
  List.fold_left (fun city c -> TinySimCity.build { city with tool } c) city cells

let simcity_months (n : int) (city : TinySimCity.city) =
  let c = ref city in
  for _ = 1 to n do c := TinySimCity.month !c done;
  !c

let row y x0 x1 = List.init (x1 - x0 + 1) (fun k -> (x0 + k, y))

(* A plant powers what touches it, and power runs on through the zones
 * and the lines; a road between two zones stops it. *)
let simcity_power () =
  let open TinySimCity in
  let city = simcity_with (new_city ()) Plant_tool [ (2, 2) ] in
  let city = simcity_with city (Zone_tool Residential) [ (3, 2); (4, 2); (6, 2) ] in
  let city = simcity_with city Road_tool [ (5, 2) ] in
  let powered c = city.powered.(index c) in
  Alcotest.(check bool) "the zone beside the plant" true (powered (3, 2));
  Alcotest.(check bool) "the next one, through the first" true (powered (4, 2));
  Alcotest.(check bool) "not across a road" false (powered (6, 2));
  let city = simcity_with city Wire_tool [ (5, 3); (6, 3) ] in
  let city = simcity_with city (Zone_tool Residential) [ (4, 3) ] in
  Alcotest.(check bool) "a line around the road" true city.powered.(index (6, 2))

(* the smallest town that grows: a plant, a road, homes along it *)
let simcity_town ?(road = true) () =
  let open TinySimCity in
  let city = simcity_with (new_city ()) Plant_tool [ (2, 5) ] in
  let city = simcity_with city (Zone_tool Residential) (row 5 3 8) in
  if road then simcity_with city Road_tool (row 6 2 9) else city

(* A zone with power and a road grows, month after month, while the city
 * wants homes; without a road it never does. *)
let simcity_growth () =
  let open TinySimCity in
  let level (city : city) c = match city.tiles.(index c) with Zone (_, l) -> l | _ -> -1 in
  let grown = simcity_months 24 (simcity_town ()) in
  Alcotest.(check bool) "homes built" true (List.exists (fun c -> level grown c > 0) (row 5 3 8));
  Alcotest.(check bool) "people living in them" true ((census grown.tiles).residents > 0);
  let stranded = simcity_months 24 (simcity_town ~road:false ()) in
  Alcotest.(check int) "no road, no one" 0 (census stranded.tiles).residents

(* The valves, SimCity's R C I bars: an empty town wants homes; homes
 * without jobs want factories and shops, not more homes. *)
let simcity_valves () =
  let open TinySimCity in
  let empty = valves { residents = 0; shops = 0; factories = 0 } 7 in
  Alcotest.(check bool) "an empty town wants homes" true (empty.r > 0);
  let dormitory = valves { residents = 200; shops = 0; factories = 0 } 7 in
  Alcotest.(check bool) "homes and no jobs: no more homes" true (dormitory.r < 0);
  Alcotest.(check bool) "but factories" true (dormitory.i > 0);
  Alcotest.(check bool) "and shops" true (dormitory.c > 0)

(* Industry fouls the air around it, and the air spreads, a blur: bad
 * next door, gone two tiles away. Homes beside a coal plant will not
 * grow there. *)
let simcity_smog () =
  let open TinySimCity in
  let tiles = Array.copy (new_city ()).tiles in
  tiles.(index (10, 10)) <- Zone (Industrial, 3);
  let air = ref (Array.make (Array.length tiles) 0.) in
  for _ = 1 to 60 do air := spread tiles !air done;
  let at c = !air.(index c) in
  Alcotest.(check bool) "the factory's own air" true (at (10, 10) > 0.5);
  Alcotest.(check bool) "next door: smog" true (at (11, 10) > smog);
  Alcotest.(check bool) "two tiles away: clean" true (at (12, 10) < smog);
  let city = simcity_with (new_city ()) Plant_tool [ (4, 5) ] in
  let city = simcity_with city (Zone_tool Residential) [ (5, 5) ] in
  let city = simcity_with city Road_tool (row 6 2 9) in
  let city = simcity_months 24 city in
  Alcotest.(check bool) "no home beside the plant" true (city.tiles.(index (5, 5)) = Zone (Residential, 0))

(* Every point of tax pushes every bar down: at 20%, nothing grows. *)
let simcity_tax () =
  let open TinySimCity in
  let taxed = simcity_months 24 { (simcity_town ()) with tax = 20 } in
  Alcotest.(check int) "nobody comes" 0 (census taxed.tiles).residents;
  let fair = simcity_months 24 (simcity_town ()) in
  Alcotest.(check bool) "at 7%, they do, and pay" true ((census fair.tiles).residents > 0 && taxes (census fair.tiles) 7 > 0)

(* The dice are a hash of the place and the month: the same city, the
 * same months, the same town. *)
let simcity_replays () =
  let a = simcity_months 36 (simcity_town ()) and b = simcity_months 36 (simcity_town ()) in
  Alcotest.(check bool) "the same tiles" true (a.tiles = b.tiles)

(*****************************************************************************)
(* TinyCivilization *)
(*****************************************************************************)

(* The made world: about half land, and the two starts far apart on
 * the same continent, so that the rival can walk to you. The map is
 * printed in the log. *)
let civ_world () =
  let open TinyCivilization in
  let char_of = function Ocean -> '~' | Grass -> '"' | Plains -> '.' | Forest -> 'f' | Hills -> 'h' in
  let us, them = starts in
  for y = 0 to rows - 1 do
    prerr_endline
      (String.init cols (fun x -> if (x, y) = us then 'R' else if (x, y) = them then 'B' else char_of world.(index (x, y))))
  done;
  let land_tiles = Array.fold_left (fun n t -> if t <> Ocean then n + 1 else n) 0 world in
  let fraction = float_of_int land_tiles /. float_of_int (cols * rows) in
  Alcotest.(check bool) (Printf.sprintf "about half land (%.2f)" fraction) true (fraction > 0.35 && fraction < 0.6);
  Alcotest.(check bool) "both starts on land" true (on_land us && on_land them);
  Alcotest.(check bool) (Printf.sprintf "far apart (%d)" (distance us them)) true (distance us them >= 15);
  let reached = Array.make (cols * rows) false in
  let rec walk = function
    | [] -> ()
    | c :: rest ->
        if on_land c && not reached.(index c) then begin
          reached.(index c) <- true;
          walk (List.tl (around 1 c) @ rest)
        end
        else walk rest
  in
  walk [ us ];
  Alcotest.(check bool) "one can walk to the other" true reached.(index them)

(* The tree: six advances to start with; each learnt opens its
 * children; Philosophy waits for all three of its parents. *)
let civ_tree () =
  let open TinyCivilization in
  Alcotest.(check int) "six roots" 6 (List.length (available []));
  Alcotest.(check bool) "Alphabet opens Writing and Code of Laws" true
    (List.mem Writing (available [ Alphabet ]) && List.mem Code_of_laws (available [ Alphabet ]));
  Alcotest.(check int) "Philosophy: three advances deep" 3 (depth Philosophy);
  let all_but_currency = List.filter (fun t -> t <> Currency && t <> Philosophy) techs in
  Alcotest.(check bool) "not without Currency" false (List.mem Philosophy (available all_but_currency));
  Alcotest.(check bool) "with it, yes" true (List.mem Philosophy (available (Currency :: all_but_currency)))

(* A city on the Roman start: its own tile and one around it, food
 * filling the box to a second citizen, shields making warriors. *)
let civ_city () =
  let open TinyCivilization in
  let us, _ = starts in
  let c = { name = "Rome"; owner = Us; at = us; size = 1; food = 0; shields = 0; making = Warriors } in
  Alcotest.(check int) "size 1 works two tiles" 2 (List.length (worked c));
  let f, s, _ = output c in
  Alcotest.(check bool) (Printf.sprintf "food to spare (%d), shields (%d)" f s) true (f > 2 && s > 0);
  let c = ref c and made = ref [] in
  for _ = 1 to 30 do
    let c', unit = city_turn !c in
    c := c';
    Option.iter (fun k -> made := k :: !made) unit
  done;
  Alcotest.(check bool) "grown" true (!c.size >= 2);
  Alcotest.(check bool) "warriors made" true (List.mem Warriors !made)

(* A weighted coin: a catapult against warriors wins six times in seven;
 * warriors against a phalanx fortified on hills in a city, rarely. And
 * the defender lost, the whole stack dies. *)
let civ_combat () =
  let open TinyCivilization in
  let g = new_game () in
  let at = (20, 12) and next = (21, 12) in
  let u id side kind at : unit_ = { id; side; kind; at; moves = 1 } in
  let g = { g with units = [ u 10 Us Catapult at; u 11 Them Warriors next; u 12 Them Settlers next ] } in
  let odds_c = odds g (List.hd g.units) next in
  Alcotest.(check (float 1e-9)) "catapult against warriors" (6. /. 7.) odds_c;
  (* the first roll of the dice that the catapult wins *)
  let rec first_win g = let r, g' = roll g in if r < odds_c then g else first_win g' in
  let g = first_win g in
  let after = advance g (List.hd g.units) next in
  Alcotest.(check int) "both of theirs gone" 0 (List.length (List.filter (fun (u : unit_) -> u.side = Them) after.units));
  let g2 = { g with units = [ u 20 Us Warriors at ]; cities = [ { name = "Ur"; owner = Them; at = next; size = 3; food = 0; shields = 0; making = Warriors } ] } in
  let g2 = { g2 with units = g2.units @ [ u 21 Them Phalanx next ] } in
  let hills = world.(index next) = Hills in
  Alcotest.(check (float 1e-9)) "warriors against a phalanx in a city" (1. /. (1. +. (2. *. 1.5 *. if hills then 1.5 else 1.)))
    (odds g2 (List.hd g2.units) next)

(* Settlers found a city where they stand -- but not two tiles from
 * another. *)
let civ_found () =
  let open TinyCivilization in
  let g = new_game () in
  let settlers = List.hd g.units in
  let g = found g settlers in
  Alcotest.(check int) "a city" 1 (List.length g.cities);
  Alcotest.(check bool) "the settlers settled" false (List.exists (fun (u : unit_) -> u.kind = Settlers && u.side = Us) g.units);
  let x, y = settlers.at in
  Alcotest.(check bool) "not two tiles away" false (can_found g (x + 2, y));
  Alcotest.(check bool) "three is fine, on land" true ((not (on_land (x + 3, y))) || can_found g (x + 3, y))

(* The rival plays on its own: left alone for eighty turns it has more
 * cities, advances, and an army -- the same ones every time. *)
let civ_rival () =
  let open TinyCivilization in
  let play () =
    let g = new_game () in
    let g = ref (found g (List.hd g.units)) in
    for _ = 1 to 80 do g := end_turn !g done;
    !g
  in
  let g = play () in
  let theirs = List.filter (fun (c : city) -> c.owner = Them) g.cities in
  prerr_endline
    (Printf.sprintf "after 80 turns: %d Babylonian cities, %d advances, %d units; winner %s" (List.length theirs)
       (List.length g.them.known) (List.length (List.filter (fun (u : unit_) -> u.side = Them) g.units))
       (match g.winner with Some Us -> "Us" | Some Them -> "Them" | None -> "none"));
  Alcotest.(check bool) "more than one city" true (List.length theirs >= 2);
  Alcotest.(check bool) "advances learnt" true (List.length g.them.known >= 2);
  let g' = play () in
  Alcotest.(check bool) "the same game again" true (g.units = g'.units && g.cities = g'.cities)

(*****************************************************************************)
(* TinyMarioWorld *)
(*****************************************************************************)

let mw_run (c : TinyMarioWorld.course) (h : TinyMarioWorld.hands) (n : int) (m : TinyMarioWorld.mario) =
  let m = ref m in
  for _ = 1 to n do m := TinyMarioWorld.step_mario c h !m done;
  !m

(* Mario dropped at column [tx] of a course, [dy] pixels over its tile
 * row [ty], and let fall to the ground *)
let mw_at (c : TinyMarioWorld.course) (tx : int) (ty : int) : TinyMarioWorld.mario =
  let open TinyMarioWorld in
  let m =
    { x = (float_of_int tx +. 0.5) *. 16.; y = (float_of_int ty *. 16.) +. 40.; vx = 0.; vy = 0.; grounded = false; angle = 0.;
      facing = 1.; sliding = false; p = 0.; flight = Walking }
  in
  mw_run c no_hands 60 m

(* the first column of [ch] in the course's bottom rows *)
let mw_find (c : TinyMarioWorld.course) (ch : char) : int * int =
  let found = ref None in
  for ty = c.rows - 1 downto 0 do
    for tx = c.cols - 1 downto 0 do
      if TinyMarioWorld.char_at c (tx, ty) = ch then found := Some (tx, ty)
    done
  done;
  Option.get !found

(* The sensors find the slope under the feet: walking up a 45 degree
 * hill he rises as much as he goes along, and stands at its angle. *)
let mw_slope () =
  let open TinyMarioWorld in
  let c = course Donut in
  let tx, ty = mw_find c '/' in
  let m = mw_at c tx (ty + 1) in
  Alcotest.(check bool) "on the ground" true m.grounded;
  Alcotest.(check (float 0.5)) "at the hill's angle" 45. m.angle;
  let m' = mw_run c { no_hands with right = true } 10 m in
  Alcotest.(check (float 1.)) "as high again as along" (m'.x -. m.x) (m'.y -. m.y)

(* Up a hill the top speed shrinks; down it, it doesn't *)
let mw_uphill () =
  let open TinyMarioWorld in
  let m = { (mw_at (course Donut) 3 3) with angle = 45.; vx = 1. } in
  Alcotest.(check bool) "uphill: less" true (uphill m run_max < run_max);
  Alcotest.(check (float 1e-9)) "downhill: the same" run_max (uphill { m with vx = -1. } run_max)

(* Crouch at the top of the long slope: he slides down it, faster and
 * faster, and the three galoombas at its foot go down with him. *)
let mw_slide () =
  let open TinyMarioWorld in
  let c = course Donut in
  let screen = { Playground.initial_computer.screen with width = 1000.; height = 1000. } in
  (* the long slope: the last '\' of the course's lowest rows *)
  let tx = ref 0 in
  for x = 0 to c.cols - 1 do if char_at c (x, 1) = '\\' then tx := x done;
  let top = !tx - 4 in
  let p = start_play Donut in
  let p = { p with mario = mw_at c top 6 } in
  Alcotest.(check bool) "standing on the slope" true (p.mario.grounded && p.mario.angle < -40.);
  let p = ref p and fastest = ref 0. in
  for _ = 1 to 150 do
    p := play_step screen { no_hands with down = true } !p;
    fastest := Float.max !fastest !p.mario.vx
  done;
  Alcotest.(check bool) (Printf.sprintf "faster than running (%.2f)" !fastest) true (!fastest > run_max);
  Alcotest.(check bool) "alive" false !p.dead;
  Alcotest.(check int) "the three galoombas knocked out" 0
    (List.length (List.filter (fun g -> g.gx > float_of_int (top * 16) && g.gx < float_of_int ((top + 20) * 16)) !p.galoombas))

(* A second at full run fills the P meter; then a jump is a takeoff,
 * and he rises as long as jump is held. *)
let mw_takeoff () =
  let open TinyMarioWorld in
  let c = course Cloud in
  let run = { no_hands with right = true; run = true } in
  let m = mw_run c run 100 (mw_at c 3 3) in
  Alcotest.(check (float 1e-9)) "the P meter full" 1. m.p;
  let m = step_mario c { run with jump = true; holding = true } m in
  Alcotest.(check bool) "taking off" true (match m.flight with Rising _ -> true | _ -> false);
  let high = mw_run c { run with holding = true } 40 m in
  Alcotest.(check bool) (Printf.sprintf "risen (%.0f px)" (high.y -. m.y)) true (high.y -. m.y > 100.);
  let walked = step_mario c { run with jump = true; holding = true } { m with flight = Walking; grounded = true; p = 0.5; vy = 0. } in
  Alcotest.(check bool) "without it, only a jump" true (walked.flight = Walking)

(* In the air the cape trades: a dive makes speed of height, a climb
 * height of speed; and a dive and a climb end lower than they began. *)
let mw_trade () =
  let open TinyMarioWorld in
  let c = course Cloud in
  let m = { (mw_at c 3 3) with y = 280.; grounded = false; flight = Soaring; vx = 3.; vy = 0.; facing = 1. } in
  let dived = mw_run c { no_hands with right = true } 20 m in
  Alcotest.(check bool) "a dive: faster" true (dived.vx > m.vx);
  Alcotest.(check bool) "and lower" true (dived.y < m.y);
  let climbed = mw_run c { no_hands with left = true } 20 dived in
  Alcotest.(check bool) "a climb: slower" true (climbed.vx < dived.vx);
  Alcotest.(check bool) "and higher" true (climbed.y > dived.y);
  let rec pump m k = if k = 0 then m else pump (mw_run c { no_hands with left = true } 30 (mw_run c { no_hands with right = true } 20 m)) (k - 1) in
  let pumped = pump m 3 in
  Alcotest.(check bool) (Printf.sprintf "no free height (%.0f -> %.0f)" m.y pumped.y) true (pumped.y < m.y)

(* The map is a graph: at first one path, from home to Donut Hills; the
 * goal opens Cloud Gap, the keyhole the Star Road, and that road
 * reaches the castle without Cloud Gap. *)
let mw_map () =
  let open TinyMarioWorld in
  let fresh = new_progress in
  Alcotest.(check int) "one path to begin with" 1 (List.length (opened fresh));
  let at_donut = walk_map fresh (1., 0.) in
  Alcotest.(check bool) "right: to Donut Hills" true (at_donut.at = Course Donut);
  Alcotest.(check bool) "up: nothing yet" true ((walk_map at_donut (0., 1.)).at = Course Donut);
  let secret = finish at_donut Donut Secret in
  let on_star = walk_map secret (0., 1.) in
  Alcotest.(check bool) "the keyhole: up to the Star Road" true (on_star.at = Course Star);
  let on_star = finish on_star Star Normal in
  Alcotest.(check bool) "and on to the castle, Cloud Gap never played" true
    ((walk_map on_star (1., 0.)).at = Course Castle && not (List.mem (Cloud, Normal) on_star.found))

(* Donut Hills played to its secret exit, by a little pilot that looks at
 * Mario each frame: walk, hop the galoomba on the hill, slide down the
 * long slope through the three at its foot, run the runway until the P
 * meter is full, take off, glide onto the island in the sky, and walk
 * to the keyhole. The keys it pressed are printed as a -script for the
 * golden frames. *)
let mw_pilot (p : TinyMarioWorld.play) (frame : int) (phase : int ref) (hold : int ref) : TinyMarioWorld.hands =
  let open TinyMarioWorld in
  let c = course Donut in
  let m = p.mario in
  let tx = int_of_float (m.x /. 16.) in
  let slope_top = let t = ref 0 in for x = 0 to c.cols - 1 do if char_at c (x, 1) = '\\' then t := x done; !t - 4 in
  let key_x = fst (mw_find c 'K') in
  ignore frame;
  (match !phase with
   | 0 when tx >= slope_top -> phase := 1
   | 1 when (not m.sliding) && m.grounded && Float.abs m.vx < 0.05 && tx > slope_top + 4 -> phase := 2
   | 2 when m.flight <> Walking -> phase := 3
   | 3 when m.grounded && m.y > 150. -> phase := 4
   | _ -> ());
  let galoomba_ahead = List.exists (fun g -> g.gx > m.x && g.gx -. m.x < 40. && Float.abs (g.gy -. m.y) < 40.) p.galoombas in
  match !phase with
  | 0 ->
      let jump = m.grounded && galoomba_ahead && !hold = 0 in
      if jump then hold := 14 else hold := max 0 (!hold - 1);
      { no_hands with right = true; jump; holding = !hold > 0 }
  | 1 -> { no_hands with down = true }
  | 2 ->
      (* the pit before the runway: hop it running, then run on *)
      let pit_ahead = m.grounded && feet c { m with x = m.x +. 24. } = None in
      let past_pit = tx > slope_top + 20 in
      let go = m.grounded && m.p >= 1. && tx >= key_x - 14 in
      if pit_ahead && not past_pit && !hold = 0 then (hold := 14; { no_hands with right = true; run = true; jump = true; holding = true })
      else begin
        hold := max 0 (!hold - 1);
        { no_hands with right = true; run = true; jump = go; holding = go || !hold > 0 }
      end
  | 3 -> (
      match m.flight with
      | Rising _ -> { no_hands with right = true; run = true; holding = true }
      (* over the island: pull back, climb, stall, and drop onto it *)
      | _ -> if tx >= key_x - 3 then { no_hands with left = true } else no_hands)
  | _ -> { no_hands with right = tx < key_x; left = tx > key_x }

let mw_keyhole () =
  let open TinyMarioWorld in
  let screen = { Playground.initial_computer.screen with width = 1000.; height = 1000. } in
  let p = ref (start_play Donut) and phase = ref 0 and hold = ref 0 and frame = ref 0 and log = ref [] in
  while !p.ended = None && (not !p.dead) && !frame < 3000 do
    incr frame;
    let before = !phase in
    let h = mw_pilot !p !frame phase hold in
    log := h :: !log;
    p := play_step screen h !p;
    if !phase <> before then
      Printf.eprintf "frame %d: phase %d at (%.0f, %.0f) p=%.2f\n" !frame !phase !p.mario.x !p.mario.y !p.mario.p
  done;
  Printf.eprintf "end at frame %d: (%.0f, %.0f) dead=%b\n" !frame !p.mario.x !p.mario.y !p.dead;
  (* the keys, as a -script from the frame the course starts (6) *)
  let hands = Array.of_list (List.rev !log) in
  let ranges name get =
    let out = ref [] and start = ref (-1) in
    Array.iteri
      (fun i h ->
        if get h && !start < 0 then start := i
        else if (not (get h)) && !start >= 0 then (out := Printf.sprintf "%s:%d-%d" name (!start + 6) (i + 5) :: !out; start := -1))
      hands;
    if !start >= 0 then out := Printf.sprintf "%s:%d-%d" name (!start + 6) (Array.length hands + 5) :: !out;
    List.rev !out
  in
  let jumps = List.concat (List.mapi (fun i (h : hands) -> if h.jump then [ i ] else []) (Array.to_list hands)) in
  Printf.eprintf "SCRIPT %s\n"
    (String.concat ","
       (ranges "right" (fun h -> h.right) @ ranges "left" (fun h -> h.left) @ ranges "down" (fun h -> h.down)
       @ ranges "x" (fun h -> h.run) @ ranges "space" (fun h -> h.holding || h.jump)));
  ignore jumps;
  Alcotest.(check bool) "not dead" false !p.dead;
  Alcotest.(check bool) "the secret exit" true (!p.ended = Some Secret)

(*****************************************************************************)
(* TinyRType *)
(*****************************************************************************)

let rtype_run (h : TinyRType.hands) (n : int) (g : TinyRType.game) : TinyRType.game =
  let g = ref g in
  for _ = 1 to n do g := TinyRType.step h !g done;
  !g

(* the waves not yet come: a game at frame 0 stays quiet for 100 frames *)
let rtype_quiet () : TinyRType.game = TinyRType.new_game ()

(* The Force: sent, it flies ahead and stays out, following the ship's
 * height; called back, it docks -- in front if it comes back to the
 * front of the ship, behind if the ship has gone past it. *)
let rtype_force () =
  let open TinyRType in
  let g = step { no_hands with send = true } (rtype_quiet ()) in
  Alcotest.(check bool) "sent" true (match g.force with Flying _ -> true | _ -> false);
  let g = rtype_run no_hands 40 g in
  Alcotest.(check bool) "out, and staying out" true (g.force = Loose && g.fx -. g.sx > 300.);
  let g = rtype_run { no_hands with dy = 1. } 20 g in
  Alcotest.(check bool) (Printf.sprintf "following the ship's height (%.0f towards %.0f)" g.fy g.sy) true (g.sy > 50. && g.fy > 30. && g.fy <= g.sy);
  let back = rtype_run no_hands 40 (step { no_hands with send = true } g) in
  Alcotest.(check bool) "called back: docked in front" true (back.force = Front);
  (* flying into it docks it (touching the Force picks it up); round
   * it, ahead of it, and a call brings it in behind *)
  let touched = rtype_run { no_hands with dx = 1. } 90 g in
  Alcotest.(check bool) "flown into: docked" true (touched.force = Front);
  let past = { g with sx = g.fx +. 200. } in
  let behind = rtype_run no_hands 40 (step { no_hands with send = true } past) in
  Alcotest.(check bool) "called back: docked behind" true (behind.force = Back)

(* Docked in front, the Force takes a bullet coming at the ship *)
let rtype_shield () =
  let open TinyRType in
  let g = rtype_quiet () in
  let g = { g with bullets = [ Shots.straight (g.sx +. 120.) g.sy (-6.) 0. ] } in
  let g = rtype_run no_hands 30 g in
  Alcotest.(check int) "alive" 0 g.dead;
  Alcotest.(check int) "the bullet gone" 0 (List.length g.bullets)

(* A tap fires a pellet; held a second, the charge fires a beam of level
 * 2 when let go, which goes through two brutes in a row *)
let rtype_beam () =
  let open TinyRType in
  let g = rtype_quiet () in
  let tapped = step { no_hands with fire = true; held = true } g in
  Alcotest.(check bool) "a tap: pellets" true (List.for_all (fun (s : shot) -> s.kind = Pellet) tapped.shots && tapped.shots <> []);
  let held = rtype_run { no_hands with held = true } 60 { g with shots = [] } in
  Alcotest.(check int) "a second held: level 2" 2 (beam_level held.charge);
  let brute x id = { id; path = Path.make [ (0., 0.); (1., 0.) ]; s = 1.; wait = 0; kind = Brute; x; y = held.sy; hp = 12 } in
  let fired = step { no_hands with released = true } { held with shots = []; enemies = [] } in
  Alcotest.(check bool) "let go: a beam" true (List.exists (fun (s : shot) -> match s.kind with Beam (2, _) -> true | _ -> false) fired.shots);
  (* the brutes set in the beam's way, and kept there *)
  let g = ref { fired with enemies = [ brute (fired.sx +. 150.) 1000; brute (fired.sx +. 260.) 1001 ] } in
  for _ = 1 to 20 do
    g := { !g with enemies = List.map (fun (e : enemy) -> { e with x = e.x +. scroll }) !g.enemies };
    g := shoot { !g with shots = List.map (fun (s : shot) -> { s with shot = Shots.advance s.shot }) !g.shots }
  done;
  Alcotest.(check bool) "through both" true (List.for_all (fun (e : enemy) -> e.hp < 12) !g.enemies && List.length !g.enemies = 2)

(* The battleship: a turret shot goes, the hull stops a shot, the core
 * counts its hits *)
let rtype_battleship () =
  let open TinyRType in
  let g = { (rtype_quiet ()) with cam = last_cam } in
  let col, row = List.hd (Tilemap.find g.ship 't') in
  let tx, ty = Tilemap.center g.ship col row in
  let g1 = shoot { g with shots = [ { shot = Shots.straight tx ty 16. 0.; kind = Pellet } ] } in
  Alcotest.(check bool) "a turret shot: gone" true (Tilemap.get g1.ship col row = Some ' ');
  let hcol, hrow = List.hd (Tilemap.find g.ship '#') in
  let hx, hy = Tilemap.center g.ship hcol hrow in
  let g2 = shoot { g with shots = [ { shot = Shots.straight hx hy 16. 0.; kind = Pellet } ] } in
  Alcotest.(check int) "the hull stops a shot" 0 (List.length g2.shots);
  let ccol, crow = List.hd (Tilemap.find g.ship 'C') in
  let cx, cy = Tilemap.center g.ship ccol crow in
  let g3 = shoot { g with shots = [ { shot = Shots.straight cx cy 16. 0.; kind = Beam (3, []) } ] } in
  Alcotest.(check int) "a level 3 beam on the core" (core_hits - 12) g3.core

(* A pilot flies the stage: on the left of the screen, charging beams
 * and letting them go; out of the way of bullets and enemies coming
 * near; along the battleship level with the top of its core, above the
 * bridge tower, until the core goes. The keys it pressed are printed
 * as a -script. *)
let rtype_pilot () =
  let open TinyRType in
  let g = ref (new_game ()) and i = ref 0 and deaths = ref 0 and log = ref [] in
  let core_y = let c, r = List.hd (Tilemap.find stage 'C') in snd (Tilemap.center stage c r) in
  let ship_left = bounds.left +. (float_of_int empty_cols *. tile) in
  while !i < 60 * 60 && !g.won = 0 do
    incr i;
    let g0 = !g in
    let over_ship = g0.cam +. 500. > ship_left in
    let threat =
      List.find_opt (fun (b : Shots.t) -> Float.abs (b.x -. g0.sx) < 110. && Float.abs (b.y -. g0.sy) < 45.) g0.bullets
      |> Option.map (fun (b : Shots.t) -> b.y)
      |> (function
          | Some y -> Some y
          | None ->
              List.find_opt (fun (e : enemy) -> visible e && Float.abs (e.x -. g0.sx) < 130. && Float.abs (e.y -. g0.sy) < 60.) g0.enemies
              |> Option.map (fun (e : enemy) -> e.y))
    in
    let target_y =
      match threat with
      | Some y when over_ship -> core_y +. (if y > g0.sy then -. 0. else 60.)
      | Some y -> if y > g0.sy then g0.sy -. 70. else g0.sy +. 70.
      | None -> if over_ship then core_y else 0.
    in
    let tx = g0.cam -. 320. in
    let dy = if target_y > g0.sy +. 4. then 1. else if target_y < g0.sy -. 4. then -1. else 0. in
    let dx = if tx > g0.sx +. 4. then 1. else if tx < g0.sx -. 4. then -1. else 0. in
    (* the fire button: held 50 frames, let go for 2 *)
    let phase = !i mod 52 in
    let held = phase < 50 in
    let h = { dx; dy; fire = phase = 0; held; released = phase = 50; send = false } in
    log := h :: !log;
    g := step h g0;
    if !g.dead = 1 && g0.dead = 0 then incr deaths
  done;
  Printf.eprintf "pilot: frame %d, won %d, deaths %d, core %d, score %d\n" !i !g.won !deaths !g.core !g.score;
  let hands = Array.of_list (List.rev !log) in
  let ranges name get =
    let out = ref [] and start = ref (-1) in
    Array.iteri
      (fun i h ->
        if get h && !start < 0 then start := i
        else if (not (get h)) && !start >= 0 then (out := Printf.sprintf "%s:%d-%d" name (!start + 2) (i + 1) :: !out; start := -1))
      hands;
    if !start >= 0 then out := Printf.sprintf "%s:%d-%d" name (!start + 2) (Array.length hands + 1) :: !out;
    List.rev !out
  in
  Printf.eprintf "SCRIPT %s\n"
    (String.concat ","
       (ranges "up" (fun h -> h.dy > 0.) @ ranges "down" (fun h -> h.dy < 0.) @ ranges "right" (fun h -> h.dx > 0.)
       @ ranges "left" (fun h -> h.dx < 0.) @ ranges "space" (fun h -> h.held)));
  Alcotest.(check bool) "the core down" true (!g.won > 0);
  Alcotest.(check bool) "at most one ship lost" true (!deaths <= 1)

(*****************************************************************************)
(* TinyIncredibleMachine *)
(*****************************************************************************)

(* a puzzle run with some parts for [frames]: the frame it was solved,
 * if it was, and where the target ended *)
let tim_run (p : TinyIncredibleMachine.puzzle) (parts : TinyIncredibleMachine.placed list) (frames : int) =
  let open TinyIncredibleMachine in
  let m = ref (build p parts) and at = ref None in
  for i = 1 to frames do
    m := step p !m;
    if !at = None && solved !m then at := Some i
  done;
  let target = List.nth !m.world.bodies (List.nth (ball_indices !m) p.target) in
  (!at, (target.x, target.y), !m)

(* Every puzzle: its solution solves it, in under 15 seconds; without
 * any part, it isn't solved *)
let tim_solutions () =
  let open TinyIncredibleMachine in
  List.iter
    (fun (p : puzzle) ->
      let at, (x, y), _ = tim_run p p.solution 900 in
      let _, (x0, y0), _ = tim_run p [] 900 in
      Printf.eprintf "%s: solved at %s, target at (%.0f, %.0f); without parts at (%.0f, %.0f)\n" p.title
        (match at with Some f -> string_of_int f | None -> "never") x y x0 y0;
      Alcotest.(check bool) (p.title ^ ": solved") true (at <> None);
      let at0, _, _ = tim_run p [] 900 in
      Alcotest.(check bool) (p.title ^ ": not without parts") true (at0 = None))
    puzzles

(* The same machine, run twice: the same frames, the same world *)
let tim_deterministic () =
  let open TinyIncredibleMachine in
  let p = List.nth puzzles 2 in
  let _, a, m1 = tim_run p p.solution 400 and _, b, m2 = tim_run p p.solution 400 in
  Alcotest.(check bool) "the target in the same place" true (a = b);
  Alcotest.(check bool) "every body too" true
    (List.for_all2 (fun (x : Physics.body) (y : Physics.body) -> x.x = y.x && x.y = y.y && x.angle = y.angle) m1.world.bodies m2.world.bodies)

(*****************************************************************************)
(* TinyWorms *)
(*****************************************************************************)

(* a flat terrain, earth under y = 0, and whatever else [f] adds *)
let worms_flat ?(f = fun (_ : TinyWorms.terrain) -> ()) () : TinyWorms.terrain =
  let open TinyWorms in
  let t = Bytes.make (cols * rows) (Char.chr air) in
  for r = row_of 0. to rows - 1 do
    for c = 0 to cols - 1 do Bytes.set_uint8 t ((r * cols) + c) earth done
  done;
  f t;
  t

let worms_game (t : TinyWorms.terrain) (x : float) : TinyWorms.game =
  let open TinyWorms in
  let g = new_game () in
  (* the ground found from under the test girders (at 200) *)
  let w = { g.worms.(0) with x; y = ground_below t x 100. +. radius; vx = 0.; vy = 0.; airborne = false } in
  { g with terrain = t; worms = [| w; { g.worms.(1) with x = 400.; y = ground_below t 400. 100. +. radius } |]; turn = 0; phase = Moving None; wind = 0. }

let worms_hands = { TinyWorms.dir = 0.; aim_by = 0.; jump = false; pick = None; held = false; pressed = false; released = false }

let worms_run (h : TinyWorms.hands) (n : int) (g : TinyWorms.game) : TinyWorms.game =
  let g = ref g in
  for _ = 1 to n do g := TinyWorms.step h !g done;
  !g

(* A crater: a circle of air, nothing outside it touched *)
let worms_crater () =
  let open TinyWorms in
  let t = carve (worms_flat ()) 0. 0. 40. in
  Alcotest.(check bool) "the middle, air" false (solid_at t 0. (-20.));
  Alcotest.(check bool) "the edge, air" false (solid_at t 36. (-4.));
  Alcotest.(check bool) "just beyond, earth" true (solid_at t 46. (-4.));
  Alcotest.(check bool) "below it, earth" true (solid_at t 0. (-46.))

(* A worm walks on the flat, is stopped by a wall, falls off an edge *)
let worms_walk () =
  let open TinyWorms in
  let wall t = for r = row_of 60. to row_of 0. do Bytes.set_uint8 t ((r * cols) + col_of 50.) earth done in
  let g = worms_game (worms_flat ~f:wall ()) 0. in
  let g = worms_run { worms_hands with dir = 1. } 60 g in
  let w = g.worms.(0) in
  Alcotest.(check bool) (Printf.sprintf "walked, up to the wall (x %.0f)" w.x) true (w.x > 30. && w.x < 50.);
  Alcotest.(check bool) "still on the ground" false w.airborne;
  let edge t = for r = row_of 0. to rows - 1 do for c = col_of 100. to cols - 1 do Bytes.set_uint8 t ((r * cols) + c) air done done in
  let g = worms_run { worms_hands with dir = 1. } 90 (worms_game (worms_flat ~f:edge ()) 60.) in
  Alcotest.(check bool) (Printf.sprintf "off the edge, falling (y %.0f)" g.worms.(0).y) true (g.worms.(0).y < -20.)

(* The rope: fired straight up at a girder, it hooks it; climbing lifts
 * the worm, a push swings it, and let go, it flies on *)
let worms_rope () =
  let open TinyWorms in
  let girder_ t = for c = col_of (-100.) to col_of 100. do Bytes.set_uint8 t ((row_of 200. * cols) + c) girder done in
  let g = worms_game (worms_flat ~f:girder_ ()) 0. in
  let g = { g with weapon = Rope; worms = [| { g.worms.(0) with aim = 80. }; g.worms.(1) |] } in
  let g = step { worms_hands with pressed = true } g in
  (match g.phase with Roping (_, (_, hy)) -> Alcotest.(check (float 5.)) "hooked on the girder" 200. hy | _ -> Alcotest.fail "the rope should hook");
  let g = worms_run { worms_hands with aim_by = 1. } 30 g in
  Alcotest.(check bool) (Printf.sprintf "climbed off the ground (y %.0f)" g.worms.(0).y) true (g.worms.(0).y > 30.);
  let g = worms_run { worms_hands with dir = 1. } 30 g in
  let x = g.worms.(0).x in
  Alcotest.(check bool) (Printf.sprintf "swung (x %.0f)" x) true (Float.abs x > 5.);
  let g = step { worms_hands with pressed = true } g in
  Alcotest.(check bool) "let go: flying, with the swing's speed" true (g.worms.(0).airborne && Float.abs g.worms.(0).vx > 10.)

(* A grenade dropped on the flat bounces up, slower; its fuse blows *)
let worms_grenade () =
  let open TinyWorms in
  let t = worms_flat () in
  let b = ref (Physics.body (Playground.circle Playground.black 5.) |> Physics.at 0. 60. |> Physics.moving 0. (-200.)) in
  let bounced = ref None in
  for _ = 1 to 40 do
    let b' = bounce t !b in
    if !bounced = None && !b.vy < 0. && b'.vy > 0. then bounced := Some (!b.vy, b'.vy);
    b := b'
  done;
  (match !bounced with
  | Some (down, up) -> Alcotest.(check bool) (Printf.sprintf "back up, slower (%.0f then %.0f)" down up) true (up > 0. && up < -.down)
  | None -> Alcotest.fail "it should bounce");
  let g = worms_game t 0. in
  let g = { g with phase = Thrown (Physics.body (Playground.circle Playground.black 5.) |> Physics.at (-200.) 10., 1) } in
  let g = worms_run worms_hands 2 g in
  Alcotest.(check bool) "the fuse: a blast" true (match g.phase with Blast _ -> true | _ -> false)

(* A blast near a worm hurts it and throws it away; the water drowns *)
let worms_blast () =
  let open TinyWorms in
  let g = worms_game (worms_flat ()) 0. in
  let g = explode g (-30.) 5. in
  let w = g.worms.(0) in
  Alcotest.(check bool) "hurt" true (w.health < 100.);
  Alcotest.(check bool) "thrown away, up and to the right" true (w.airborne && w.vx > 0. && w.vy > 0.);
  Alcotest.(check bool) "the far worm untouched" true (g.worms.(1).health = 100.);
  Alcotest.(check bool) "in the water: drowned" true (drowned { w with y = water -. 10. })

(*****************************************************************************)
(* TinyXCOM *)
(*****************************************************************************)

let xcom_soldier (at : int * int) (facing : int * int) : TinyXCOM.unit_ =
  { id = 0; side = Squad; name = "T"; at; facing; tu = 50; max_tu = 50; hp = 40; accuracy = 65; reactions = 55 }

let xcom_alien (at : int * int) (facing : int * int) : TinyXCOM.unit_ =
  { id = 10; side = Aliens; name = "A"; at; facing; tu = 54; max_tu = 54; hp = 30; accuracy = 60; reactions = 60 }

let xcom_game (rows : string list) (units : TinyXCOM.unit_ list) : TinyXCOM.game =
  { (TinyXCOM.new_game ()) with map = Tilemap.of_strings TinyXCOM.tile rows; units; selected = 0; seen = [] }

let xcom_open = [ "##############"; "#............#"; "#............#"; "#............#"; "##############" ]

(* A step is 4 time units, 6 diagonally; a walk goes as far as they pay
 * for, and stops there *)
let xcom_time_units () =
  let open TinyXCOM in
  Alcotest.(check int) "straight" 4 (step_cost (1, 1) (2, 1));
  Alcotest.(check int) "diagonal" 6 (step_cost (1, 1) (2, 2));
  let s = xcom_soldier (1, 2) (1, 0) in
  Alcotest.(check int) "snap" 13 (shot_cost s Snap);
  Alcotest.(check int) "aimed" 25 (shot_cost s Aimed);
  let g = xcom_game xcom_open [ s ] in
  Alcotest.(check int) "all the way with 50" 8 (List.length (affordable g s (9, 2)));
  Alcotest.(check (list (pair int int))) "two steps with 10" [ (2, 2); (3, 2) ] (affordable g { s with tu = 10 } (9, 2));
  Alcotest.(check (list (pair int int))) "not into a wall" [] (affordable g s (0, 2))

(* Seeing: a cone of 90 degrees ahead; a wall hides what is behind it, a
 * hedge doesn't (it only stops you walking) *)
let xcom_sight () =
  let open TinyXCOM in
  let rows = [ "##########"; "#..o.....#"; "#...#....#"; "#........#"; "##########" ] in
  let m = Tilemap.of_strings tile rows in
  let s = xcom_soldier (1, 2) (1, 0) in
  Alcotest.(check bool) "ahead" true (sees m s (3, 2));
  Alcotest.(check bool) "behind the wall" false (sees m s (7, 2));
  Alcotest.(check bool) "past the hedge" true (sees m (xcom_soldier (1, 1) (1, 0)) (6, 1));
  Alcotest.(check bool) "the hedge blocks walking" true (blocks_move m (3, 1));
  Alcotest.(check bool) "not behind itself" false (sees m { s with at = (6, 2) } (3, 3));
  Alcotest.(check bool) "turned round, it does" true (sees m { s with at = (6, 3); facing = (-1, 0) } (3, 3));
  Alcotest.(check bool) "out of range" false (sees (Tilemap.of_strings tile [ String.make 30 '.' ]) { s with at = (0, 0) } (12, 0))

(* The chance shown before shooting: an aimed shot better than a snap,
 * less far away, less behind a hedge *)
let xcom_hit_chance () =
  let open TinyXCOM in
  let g = xcom_game xcom_open [] in
  let s = xcom_soldier (1, 2) (1, 0) in
  let a at = xcom_alien at (-1, 0) in
  Alcotest.(check int) "snap, near" 39 (hit_chance g.map s (a (5, 2)) Snap);
  Alcotest.(check int) "aimed, near" 71 (hit_chance g.map s (a (5, 2)) Aimed);
  Alcotest.(check int) "aimed, far" 63 (hit_chance g.map s (a (11, 2)) Aimed);
  let hedged = Tilemap.set g.map 10 2 'o' in
  Alcotest.(check (option (pair int int))) "the hedge is between" (Some (10, 2)) (cover_between hedged s.at (11, 2));
  Alcotest.(check int) "aimed, far, behind a hedge" 38 (hit_chance hedged s (a (11, 2)) Aimed);
  Alcotest.(check int) "never under 5" 5 (hit_chance hedged { s with accuracy = 0 } (a (11, 2)) Snap)

(* A miss the hedge was in the way of hits the hedge, which goes or
 * stays by the dice; a shot either wounds or doesn't, whatever the dice *)
let xcom_cover_wears () =
  let open TinyXCOM in
  let g = xcom_game xcom_open [ xcom_soldier (1, 2) (1, 0); xcom_alien (11, 2) (-1, 0) ] in
  let g = { g with map = Tilemap.set g.map 10 2 'o' } in
  let outcomes =
    List.init 100 (fun rolls ->
        let g = { g with rolls } in
        let g' = shoot g (unit_by g 0) (unit_by g 10) Snap in
        ((unit_by g' 10).hp < 30, Tilemap.get g'.map 10 2 = Some '.'))
  in
  Alcotest.(check bool) "some hit" true (List.exists fst outcomes);
  Alcotest.(check bool) "some wear the hedge away" true (List.exists snd outcomes);
  Alcotest.(check bool) "some leave it" true (List.exists (fun (hit, gone) -> not hit && not gone) outcomes);
  Alcotest.(check bool) "a hit never takes the hedge" false (List.exists (fun (hit, gone) -> hit && gone) outcomes);
  let g' = shoot g (unit_by g 0) (unit_by g 10) Snap in
  Alcotest.(check int) "the time units spent" 37 (unit_by g' 0).tu

(* Reaction fire: the alien who sees you step, with more of its turn
 * kept back than you, shoots in the middle of your walk -- which stops
 * it; with no time units left, it can't *)
let xcom_reaction_fire () =
  let open TinyXCOM in
  let g = xcom_game xcom_open [ xcom_soldier (1, 2) (1, 0); xcom_alien (9, 2) (-1, 0) ] in
  let g', stopped = walk_step g 0 (2, 2) in
  Alcotest.(check int) "a shot" 1 (List.length g'.shots);
  Alcotest.(check bool) "the walk stops" true stopped;
  Alcotest.(check bool) "the alien spent its time" true ((unit_by g' 10).tu < 54);
  let tired = set_unit g { (unit_by g 10) with tu = 10 } in
  let g', _ = walk_step tired 0 (2, 2) in
  Alcotest.(check int) "no time, no shot" 0 (List.length g'.shots);
  let away = set_unit g { (unit_by g 10) with facing = (1, 0) } in
  let g', _ = walk_step away 0 (2, 2) in
  Alcotest.(check int) "looking away, no shot" 0 (List.length g'.shots)

let xcom_run_aliens (g : TinyXCOM.game) : TinyXCOM.game =
  let rec go n (g : TinyXCOM.game) = match g.phase with Aliens_turn _ when n > 0 -> go (n - 1) (TinyXCOM.step g) | _ -> g in
  go 5000 (TinyXCOM.end_turn g)

(* The aliens' turn: they come towards the squad, keeping a snap shot's
 * time units back for your turn, and hand it back; the same turn twice
 * is the same *)
let xcom_aliens_turn () =
  let open TinyXCOM in
  let g = reveal (new_game ()) in
  let g' = xcom_run_aliens g in
  Alcotest.(check bool) "the squad's turn again" true (g'.phase = Squad_turn || g'.phase = Lost);
  Alcotest.(check int) "turn 2" 2 g'.turn;
  let at (g : game) = List.map (fun (u : unit_) -> u.at) (List.filter (fun (u : unit_) -> u.side = Aliens) g.units) in
  Alcotest.(check bool) "they moved" true (at g <> at g');
  let far (g : game) = List.fold_left (fun acc (a : unit_) -> acc + fst a.at) 0 (enemies g Squad) in
  Alcotest.(check bool) "towards the squad" true (far g' < far g);
  Alcotest.(check bool) "the same twice" true ((xcom_run_aliens g).units = g'.units)

(* A squad that only ends its turn still fights, by reaction fire, and
 * the battle ends one way or the other *)
let xcom_battle_ends () =
  let open TinyXCOM in
  let rec go n (g : game) = if n = 0 || g.phase = Won || g.phase = Lost then g else go (n - 1) (xcom_run_aliens g) in
  let g = go 40 (reveal (new_game ())) in
  Alcotest.(check bool) "over" true (g.phase = Won || g.phase = Lost)

(*****************************************************************************)
(* TinyMetroid *)
(*****************************************************************************)

(* The checker's verdict on the world: one item a round, in the
 * designed order, and Kraid only with all four *)
let metroid_progression () =
  let open TinyMetroid in
  Alcotest.(check (list (list string)))
    "one key a round"
    [ [ "o" ]; [ "m" ]; [ "j" ]; [ "b" ] ]
    (List.map (List.map (function Morph_ball -> "o" | Missiles -> "m" | High_jump -> "j" | Bombs -> "b")) (progression level));
  let kraid = List.hd (Tilemap.find level 'K') in
  Alcotest.(check bool) "Kraid with everything" true (reaches level [ Morph_ball; Missiles; High_jump; Bombs ] kraid);
  Alcotest.(check bool) "not without the bombs" false (reaches level [ Morph_ball; Missiles; High_jump ] kraid);
  let tank = List.hd (Tilemap.find level 'e') in
  Alcotest.(check bool) "the tank needs the bombs" false (reaches level [ Morph_ball; Missiles; High_jump ] tank);
  Alcotest.(check bool) "and has them" true (reaches level [ Morph_ball; Missiles; High_jump; Bombs ] tank)

(* The checker catches a broken world: the red door left out, the boots
 * come with the missiles, in the same round *)
let metroid_broken () =
  let open TinyMetroid in
  let doorless = List.fold_left (fun m (c, r) -> Tilemap.set m c r ' ') level [ (41, 26); (41, 27); (41, 28) ] in
  Alcotest.(check int) "three rounds, not four" 3 (List.length (progression doorless));
  Alcotest.(check bool) "the boots in the second" true (List.mem High_jump (List.nth (progression doorless) 1))

let metroid_run (c : TinyMetroid.controls) (n : int) (g : TinyMetroid.game) : TinyMetroid.game =
  let g = ref g in
  for _ = 1 to n do g := TinyMetroid.step c !g done;
  !g

(* The game's jump against the checker's: 3 tiles and not 4, 5 with the
 * boots and not 6 *)
let metroid_jump () =
  let open TinyMetroid in
  let g = new_game () in
  let rise (s : samus) =
    let rec go (g : game) best n = if n = 0 then best else let g = step { idle with jump = true } g in go g (Float.max best (g.samus.y -. s.y)) (n - 1) in
    go (step { idle with jump = true; jump_now = true } { g with samus = s }) 0. 80 /. tile
  in
  let h = rise g.samus in
  Alcotest.(check bool) (Printf.sprintf "3 to 4 tiles (%.2f)" h) true (h >= 3. && h < 4.);
  let h = rise { g.samus with has = [ High_jump ] } in
  Alcotest.(check bool) (Printf.sprintf "5 to 6 tiles (%.2f)" h) true (h >= 5. && h < 6.)

(* The first lock: the tunnel out of the start room lets only the ball
 * through *)
let metroid_tunnel () =
  let open TinyMetroid in
  let g = new_game () in
  let g = metroid_run { idle with dir = 1. } 300 g in
  Alcotest.(check bool) "standing, stopped by the wall" true (g.samus.x < fst (center (18, 28)));
  let g = { g with samus = { g.samus with has = [ Morph_ball ] } } in
  let g = step { idle with down_now = true } g in
  Alcotest.(check bool) "rolled up" true g.samus.ball;
  let g = metroid_run { idle with dir = 1. } 200 g in
  Alcotest.(check bool) "through" true (g.samus.x > fst (center (22, 28)));
  let g = step { idle with up_now = true } g in
  Alcotest.(check bool) "standing again, out of it" false g.samus.ball

(* A red door: the beam bounces off it, a missile opens it, all of it *)
let metroid_door () =
  let open TinyMetroid in
  let g = new_game () in
  let x, y = on_floor 70. (39, 28) in
  let g = { g with samus = { g.samus with x; y; facing = 1.; has = [ Missiles ]; missiles = 10 }; enemies = [] } in
  let g = metroid_run idle 40 (step { idle with fire_now = true } g) in
  Alcotest.(check (option char)) "the beam: still shut" (Some 'R') (Tilemap.get g.map 41 27);
  let g = step { idle with switch_now = true } g in
  let g = metroid_run idle 40 (step { idle with fire_now = true } g) in
  Alcotest.(check (list (option char))) "a missile: open" [ Some ' '; Some ' '; Some ' ' ] (List.map (Tilemap.get g.map 41) [ 26; 27; 28 ]);
  Alcotest.(check int) "one missile spent" 9 g.samus.missiles

(* A bomb laid in the ball breaks the blocks around it, and only after
 * its fuse *)
let metroid_bomb () =
  let open TinyMetroid in
  let g = new_game () in
  let x, y = on_floor 24. (5, 11) in
  let g = { g with samus = { g.samus with x; y; ball = true; has = [ Morph_ball; Bombs ] }; enemies = [] } in
  let g = step { idle with fire_now = true } g in
  Alcotest.(check (option char)) "ticking" (Some 'x') (Tilemap.get g.map 5 12);
  let g = metroid_run idle 45 g in
  Alcotest.(check (list (option char))) "blown" [ Some ' '; Some ' '; Some ' ' ] (List.map (fun c -> Tilemap.get g.map c 12) [ 4; 5; 6 ]);
  Alcotest.(check bool) "and the ball falls to the tank" true (g.samus.y < y)

(* Kraid feels only missiles *)
let metroid_kraid () =
  let open TinyMetroid in
  let g = new_game () in
  let b = g.boss in
  let g = { g with samus = { g.samus with x = b.kx -. 200.; y = b.ky -. 20.; facing = 1.; has = [ Missiles ]; missiles = 10 }; enemies = [] } in
  let g = metroid_run idle 30 (step { idle with fire_now = true } g) in
  Alcotest.(check int) "the beam: nothing" 8 g.boss.khp;
  let g = step { idle with switch_now = true } g in
  let g = metroid_run idle 30 (step { idle with fire_now = true } g) in
  Alcotest.(check int) "a missile: one" 7 g.boss.khp

(* The climbs the checker counts on, done with the game's own jump:
 * straight up, then across at the top (the tile the feet end on); and
 * the ledge that needs the boots, without them *)
let metroid_climbs () =
  let open TinyMetroid in
  let leap (g : game) wait dir frames =
    let g = step { idle with jump = true; jump_now = true } g in
    let g = metroid_run { idle with jump = true } (wait - 1) g in
    let g = metroid_run { idle with jump = true; dir } frames g in
    metroid_run idle 60 g
  in
  let from has (c, r) = let g = new_game () in let x, y = on_floor 70. (c, r) in { g with samus = { g.samus with x; y; has }; enemies = [] } in
  let feet (g : game) = Tilemap.cell g.map g.samus.x (g.samus.y -. 30.) in
  List.iter
    (fun (what, has, start, wait, dir, frames, expected) ->
      Alcotest.(check (pair int int)) what expected (feet (leap (from has start) wait dir frames)))
    [ ("onto the missiles' ledge", [], (35, 28), 10, 1., 20, (37, 25));
      ("onto the high ledge, with the boots", [ High_jump ], (24, 28), 14, 1., 15, (26, 24));
      ("not without", [], (24, 28), 14, 1., 15, (24, 28));
      ("up the shaft", [ High_jump ], (26, 24), 10, 1., 15, (27, 20));
      ("up the shaft, again", [ High_jump ], (27, 20), 10, -1., 20, (25, 16));
      ("and again", [ High_jump ], (25, 16), 10, 1., 15, (27, 12));
      ("out, into the corridor", [ High_jump ], (27, 12), 10, 1., 10, (28, 11)) ]

(*****************************************************************************)
(* TinyFreeCell (the cards kit) *)
(*****************************************************************************)

let names cards = List.map Cards.name cards

(* deal 1, as players know it: its first column, from the last card up,
 * and the 7 and 6 cards of the columns *)
let freecell_deal () =
  let open TinyFreeCell in
  let g = new_game 1 in
  Alcotest.(check (list string)) "column 1" [ "6S"; "6D"; "3S"; "4C"; "2S"; "KD"; "JD" ] (names g.columns.(0));
  Alcotest.(check (list int)) "the columns" [ 7; 7; 7; 7; 6; 6; 6; 6 ] (Array.to_list (Array.map List.length g.columns))

(* a card into a free cell, a card onto one of the other color one
 * higher, and what the rules refuse *)
let freecell_moves () =
  let open TinyFreeCell in
  let g = new_game 1 in
  Alcotest.(check bool) "the 9C onto nothing" true (move_cards g (Column 1) 1 (Foundation 0) = None);
  Alcotest.(check bool) "two cards that are no run" true (move_cards g (Column 0) 2 (Cell 0) = None);
  let g = Option.get (move_cards g (Column 0) 1 (Cell 0)) in
  Alcotest.(check (option string)) "the 6S parked" (Some "6S") (Option.map Cards.name g.cells.(0));
  Alcotest.(check bool) "not into a full cell" true (move_cards g (Column 1) 1 (Cell 0) = None);
  (* the 6D, uncovered, onto the 8C: not one lower *)
  Alcotest.(check bool) "the 6D onto the 8C" true (move_cards g (Column 0) 1 (Column 6) = None);
  Alcotest.(check int) "a move counted" 1 g.moves

(* the header's supermove: 2 free cells and 1 empty column move 6 cards
 * onto a column, 3 onto the empty one *)
let freecell_supermove () =
  let open TinyFreeCell in
  let g = new_game 1 in
  let g = { g with cells = [| Some (Cards.of_index 0); Some (Cards.of_index 1); None; None |] } in
  let columns = Array.copy g.columns in
  columns.(7) <- [];
  let g = { g with columns } in
  Alcotest.(check int) "onto a column" 6 (capacity g (Column 0));
  Alcotest.(check int) "onto the empty one" 3 (capacity g (Column 7))

(*****************************************************************************)
(* TinySolitaire (the cards kit) *)
(*****************************************************************************)

(* deal 1 the Klondike way: 1 to 7 cards, the last face up, 24 in the
 * stock *)
let solitaire_deal () =
  let open TinySolitaire in
  let g = new_game 1 in
  Alcotest.(check (list int)) "the columns" [ 1; 2; 3; 4; 5; 6; 7 ] (Array.to_list (Array.map List.length g.columns));
  Alcotest.(check (list string)) "their last cards" [ "JD"; "5H"; "QC"; "9D"; "AH"; "5C"; "QH" ]
    (Array.to_list (Array.map (fun col -> Cards.name (fst (List.hd col))) g.columns));
  Alcotest.(check int) "face up" 7 (Array.fold_left (fun n col -> n + List.length (List.filter snd col)) 0 g.columns);
  Alcotest.(check int) "the stock" 24 (List.length g.stock)

(* the ace home, and the card under it turned over; the stock through
 * the waste and back *)
let solitaire_moves () =
  let open TinySolitaire in
  let g = new_game 1 in
  Alcotest.(check bool) "the ace not onto clubs" true (move_cards g (Column 4) 1 (Foundation 0) = None);
  let g = Option.get (move_cards g (Column 4) 1 (Foundation 2)) in
  Alcotest.(check int) "hearts: the ace" 1 g.foundations.(2);
  Alcotest.(check bool) "the card under it face up" true (snd (List.hd g.columns.(4)));
  Alcotest.(check bool) "only a king into an empty column" true
    (let columns = Array.copy g.columns in
     columns.(0) <- [];
     move_cards { g with columns } (Column 1) 1 (Column 0) = None);
  let g = List.fold_left (fun g _ -> turn g) g (List.init 24 Fun.id) in
  Alcotest.(check (pair int int)) "all turned over" (0, 24) (List.length g.stock, List.length g.waste);
  let g = turn g in
  Alcotest.(check (pair int int)) "and back" (24, 0) (List.length g.stock, List.length g.waste)

(*****************************************************************************)
(* TinyCoreWar *)
(*****************************************************************************)

(* the labels: the Dwarf as Dewdney printed it, with numbers *)
let corewar_assemble () =
  let open TinyCoreWar in
  let d = dwarf in
  Alcotest.(check bool) "ADD #4, 3" true (List.nth d.code 0 = { op = ADD; amode = Immediate; a = 4; bmode = Direct; b = 3 });
  Alcotest.(check bool) "MOV 2, @2" true (List.nth d.code 1 = { op = MOV; amode = Direct; a = 2; bmode = Indirect; b = 2 });
  Alcotest.(check bool) "JMP -2" true ((List.nth d.code 2).a = -2);
  Alcotest.(check bool) "SPL @3, 0" true (List.nth mice.code 4 = { op = SPL; amode = Indirect; a = 3; bmode = Direct; b = 0 });
  Alcotest.(check int) "Mice's END start" 1 mice.entry

(* a mistake is shown with its line, counted from 1 as the editor shows
 * them, comments and blank lines included *)
let corewar_mistakes () =
  let open TinyCoreWar in
  let mistake text = match assemble text with Error e -> Some e | Ok _ -> None in
  Alcotest.(check (option (pair int string))) "a label no line has" (Some (3, "no label bmb"))
    (mistake "; a comment\n\n  ADD #4, bmb\nbomb DAT #0");
  Alcotest.(check (option (pair int string))) "a label alone" (Some (2, "a label with no instruction"))
    (mistake "MOV 0, 1\nHALT");
  Alcotest.(check (option (pair int string))) "three operands" (Some (1, "one or two operands")) (mistake "MOV 0, 1, 2");
  Alcotest.(check (option (pair int string))) "nothing" (Some (1, "no instruction")) (mistake "; only a comment");
  Alcotest.(check (option (pair int string))) "END's label" (Some (2, "no label go")) (mistake "MOV 0, 1\nEND go")

(* the Imp walks a cell a cycle; the Dwarf's first bomb lands 7 cells
 * from its start, the next 4 further *)
let corewar_imp_and_dwarf () =
  let open TinyCoreWar in
  let m = run (load (imp, dwarf)) 10 in
  Alcotest.(check (list int)) "the Imp" [ 10 ] m.warriors.(0).queue;
  Alcotest.(check bool) "a bomb at 407" true (m.core.(407).op = DAT && m.owner.(407) = 2);
  Alcotest.(check bool) "and at 411" true (m.core.(411).op = DAT && m.owner.(411) = 2)

(* a program running into a DAT dies; the Imp turns the Dwarf into an
 * Imp, a draw; the Mice multiply and kill the Dwarf *)
let corewar_fights () =
  let open TinyCoreWar in
  let dead = { code = [ dat0 ]; entry = 0 } in
  Alcotest.(check bool) "DAT kills" true (result (run (load (dead, imp)) 5) = Wins 1);
  Alcotest.(check bool) "Dwarf and Imp: a draw" true (result (run (load (dwarf, imp)) max_cycles) = Draw);
  let m = run (load (mice, dwarf)) max_cycles in
  Printf.printf "Mice against Dwarf: %s, cycle %d, %d mice\n"
    (match result m with Wins 0 -> "Mice" | Wins _ -> "Dwarf" | _ -> "a draw")
    m.cycles (List.length m.warriors.(0).queue);
  Alcotest.(check bool) "the Mice win" true (result m = Wins 0)

(*****************************************************************************)
(* TinyLunarLander *)
(*****************************************************************************)

(* the module put upright 60 units above the middle of a pad of the
 * first moon (seed=1), or above a point of the moon that is no pad's,
 * then flown by [pilot] until it touches down *)
let lander_down ?(mult = 3) ?(off_pad = false) (pilot : TinyLunarLander.lander -> bool) : TinyLunarLander.game =
  let open TinyLunarLander in
  let g = new_game 1 in
  let x =
    if off_pad then
      (* the first point, and the one after, both off every pad *)
      let i = List.find (fun i -> List.for_all (fun p -> i < p.first - 1 || i > p.first + p.len + 1) g.moon.pads) (List.init segments Fun.id) in
      point_x i +. (seg_w /. 2.)
    else
      let p = List.find (fun p -> p.mult = mult) g.moon.pads in
      (point_x p.first +. point_x (p.first + p.len)) /. 2.
  in
  let g = { g with lander = { x; y = ground_at g.moon x +. feet_y +. 60.; vx = 0.; vy = 0.; angle = 0.; firing = false } } in
  let rec go n (g : game) =
    if n = 0 then Alcotest.fail "never touched down"
    else match g.touched with Some _ -> g | None -> go (n - 1) (update_game { initial_computer.keyboard with kup = pilot g.lander } g)
  in
  go 1000 g

(* a pilot keeping the fall at 10 units a second lands well, and is
 * paid the pad's multiplier; the camera went close on the way *)
let lander_good () =
  let open TinyLunarLander in
  let g = lander_down (fun l -> l.vy < -10.) in
  Alcotest.(check bool) "a good landing, x3" true (match g.touched with Some (Good, 3, _) -> true | _ -> false);
  Alcotest.(check int) "50 x 3" 150 g.score;
  Alcotest.(check bool) "close up" true g.close;
  let g = lander_down ~mult:5 (fun l -> l.vy < -22.) in
  Alcotest.(check bool) "a hard landing, x5" true (match g.touched with Some (Hard, 5, _) -> true | _ -> false)

(* no engine: 60 units fallen, 49 units a second at the end, a crash;
 * slowly but on a slope, a crash too *)
let lander_crashes () =
  let open TinyLunarLander in
  let g = lander_down (fun _ -> false) in
  Alcotest.(check bool) "falling" true (match g.touched with Some (Crashed, 0, _) -> true | _ -> false);
  let g = lander_down ~off_pad:true (fun l -> l.vy < -10.) in
  Alcotest.(check bool) "on a slope" true (match g.touched with Some (Crashed, 0, _) -> true | _ -> false)

(* the tank is 25 seconds of thrust, then the engine stops for good *)
let lander_fuel () =
  let open TinyLunarLander in
  let k = { initial_computer.keyboard with kup = true } in
  let g = ref (new_game 1) in
  (* straight up, stopped by the ceiling, never touching down *)
  g := { !g with lander = { !g.lander with angle = 0.; vx = 0. } };
  for _ = 1 to 60 * 26 do g := update_game k !g done;
  Alcotest.(check (float 0.)) "empty" 0. !g.fuel;
  Alcotest.(check bool) "the engine out" false !g.lander.firing

(*****************************************************************************)
(* TinyFrogger *)
(*****************************************************************************)

(* the traffic is a formula of time: every lane is back where it was one
 * of its periods later, and row 4's car is where [spans]'s comment says *)
let frogger_lanes () =
  let open TinyFrogger in
  lanes
  |> List.iter (fun (l : lane) ->
         let later = 3. +. (l.period /. Float.abs l.speed) in
         List.iter2
           (fun (a, _) (b, _) -> Alcotest.(check (float 0.001)) "the same place a period later" a b)
           (spans 1 3. l) (spans 1 later l));
  Alcotest.(check (list (pair (float 0.001) (float 0.001))))
    "row 4 at 2 s" [ (-110., -50.) ] (spans 1 2. (List.nth lanes 3))

(* the game played frame by frame, [presses] the frames an arrow (or
 * space) goes down, as the golden frames' scripts do *)
let frogger_play (presses : (int * string) list) (frames : int) : TinyFrogger.game =
  let k0 = initial_computer.keyboard in
  let rec go i (m : TinyFrogger.model) =
    if i > frames then m
    else
      let keyboard =
        match List.assoc_opt i presses with
        | Some "up" -> { k0 with kup = true }
        | Some "space" -> { k0 with kspace = true }
        | _ -> k0
      in
      go (i + 1) (TinyFrogger.update (computer ~keyboard i) m)
  in
  match (go 1 TinyFrogger.initial_model).scenes.scene with
  | Playing g -> g
  | _ -> Alcotest.fail "not playing"

(* the golden frame's crossing: between the cars, onto a log, onto the
 * long log; then, waiting there, carried off the edge: a frog lost *)
let frogger_crossing () =
  let presses =
    (1, "space") :: List.map (fun f -> (f, "up")) [ 10; 20; 30; 40; 109; 119; 135; 145; 250 ]
  in
  let g = frogger_play presses 280 in
  Alcotest.(check int) "on the long log" 9 g.frog.row;
  Alcotest.(check bool) "alive" true (g.dying = None);
  Alcotest.(check int) "no frog lost" 2 g.lives;
  Alcotest.(check int) "10 a row" 90 g.score;
  let g = frogger_play presses 600 in
  Alcotest.(check int) "carried off: a frog lost" 1 g.lives;
  Alcotest.(check int) "the next frog at the start" 0 g.frog.row

(* a frog landing in the bays' row: home in an empty bay, dead against
 * the hedge or in a bay taken; the fifth one home, the next level *)
let frogger_bays () =
  let open TinyFrogger in
  let at ?(homes = new_game.homes) x = { new_game with homes; frog = { new_frog with x; row = home_row } } in
  let g = arrive (at 0.) in
  Alcotest.(check bool) "the middle bay filled" true (List.nth g.homes 2);
  Alcotest.(check int) "the next frog at the start" start_row g.frog.row;
  Alcotest.(check bool) "the hedge kills" true ((arrive (at 90.)).dying <> None);
  Alcotest.(check bool) "a bay taken kills" true ((arrive (at ~homes:g.homes 0.)).dying <> None);
  let g = arrive (at ~homes:[ true; true; false; true; true ] 0.) in
  Alcotest.(check int) "the five: the next level" 2 g.level;
  Alcotest.(check bool) "the bays emptied" true (List.for_all not g.homes)

let tests =
  Testo.categorize "games"
    [ t "TinySokoban, level 1 solved" sokoban_solution;
      t "TinySokoban, every level solvable" sokoban_levels_solvable;
      t "TinySokoban.xsb, as the editor writes it" sokoban_editor_round_trip;
      t "TinyPacman, the ghosts leave the house" pacman_ghosts_leave;
      t "TinyPacman, a power pellet" pacman_blue;
      t "TinyPacman, a ghost eaten" pacman_eaten;
      t "TinyPacman ai=engine, the ghosts leave the house" (pacman_ghosts_leave ~ai_engine:true);
      t "TinyPacman ai=engine, a power pellet" (pacman_blue ~ai_engine:true);
      t "TinyPacman ai=engine, a ghost eaten" (pacman_eaten ~ai_engine:true);
      t "TinyPacman ai=engine, the waves agree with the clock" pacman_waves;
      t "TinySoldat, the bots fight" soldat_fight;
      t "TinySoldat ai=engine, the bots fight" (soldat_fight ~ai_engine:true);
      t "TinySoldat ai=engine, a bot knows only what it has seen" soldat_senses;
      t "TinyBoomerangFu, a stone blocks sight" boomerang_sight;
      t "TinyBoomerangFu, dodge, hunt, keep away" boomerang_modes;
      t "TinyBoomerangFu, the computer answers late" boomerang_delay;
      t "AiConnect4, what each trick saves" connect4_nodes;
      t "AiConnect4, the tricks do not change the move" connect4_same_move;
      t "AiGo, a capture and a suicide" go_captures;
      t "AiGo, the ko rule" go_ko;
      t "AiGo, area scoring" go_scoring;
      t "AiGo, MCTS plays a legal move" go_plays;
      t "TinyBomberman, a chain reaction" bomberman_chain;
      t "TinyMicroMachines, the computer drives laps" micro_machines_computer;
      t "TinyMarioKart, Mode 7 there and back" kart_mode7;
      t "TinyMarioKart, the computer drives the race" kart_race;
      t "TinyMarioKart, two players" kart_two_players;
      t "TinyMarioKart64, the computer drives the race" mario_kart_race;
      t "TinyMarioKart64, Block Fort" mario_kart_block_fort;
      t "TinyMarioKart64, the balloons" mario_kart_balloons;
      t "TinyMarioKart64, up to four players" mario_kart_players;
      t "TinyMarioKart64, the powerslide and its mini-turbo" mario_kart_mini_turbo;
      t "TinyMarioKart64, the items by place" mario_kart_items;
      t "TinyMarioKart64, the ribbon there and back" mario_kart_ribbon;
      t "TinyVirtuaFighter, the keyframes are the frame data" virtua_fighter_keyframes;
      t "TinyVirtuaFighter, blocking, and what goes under it" virtua_fighter_blocking;
      t "TinyVirtuaFighter, the ring is the other way to lose" virtua_fighter_ring_out;
      t "TinyStarFox, on rails and inside the canyon" star_fox_rails;
      t "TinyStarFox, a bolt down the canyon" star_fox_bolt;
      t "TinyAloneInTheDark, the cut is past the doorway" alone_cut;
      t "TinyAloneInTheDark, tank controls ignore the camera" alone_tank;
      t "TinyAloneInTheDark, the study door and its key" alone_door_locked;
      t "TinyElite, the galaxy from its seed" elite_galaxy;
      t "TinyElite, small turns and TIDY" elite_tidy;
      t "TinyElite, the hidden lines" elite_hidden_lines;
      t "TinyElite, docking" elite_docking;
      t "TinyBattlezone, the divide and the near plane" battlezone_projection;
      t "TinyBattlezone, shells at the height of a hull" battlezone_shell_height;
      t "TinyElite3d, backface culling is Elite's hidden lines" elite3d_culling_is_ll9;
      t "TinyTeardown, greedy meshing" teardown_mesh;
      t "TinyTeardown, the level as boxes" teardown_boxes;
      t "TinyTeardown, the hammer's ray" teardown_cast;
      t "TinyTeardown, the water tower comes down" teardown_tower;
      t "TinyTeardown, the heist" teardown_heist;
      t "TinyDoom, the BSP: convex subsectors, the right sectors" doom_bsp;
      t "TinyDoom, a frame" doom_frame;
      t "TinyDoom, a robot finds the exit" doom_exit;
      t "TinyComanche, a robot pops the balloons" comanche_balloons;
      t "TinyDescent, the mine holds the ship, a robot shot, the exit" descent_mine;
      t "TinyQuake, qbsp, vis and light" quake_tools;
      t "TinyQuake, walking the level" quake_walk;
      t "TinyMinecraft, the world and what is shown" minecraft_world;
      t "TinyMinecraft, standing, jumping, walking, flying" minecraft_player;
      t "TinyMinecraft, physics=engine: the capsule on the blocks" minecraft_engine;
      t "TinyMario64, a jump onto a platform" mario64_jump;
      t "TinyMario64, physics=engine: the same jump" mario64_engine;
      t "TinyPinball3d, the sweep and the flipper" pinball3d_flipper;
      t "TinyPinball3d, the sweep and a wall" pinball3d_wall;
      t "TinyHalfLife2, the gravity gun" hl2_gun;
      t "TinyHalfLife2, a zombie hit goes limp" hl2_zombie;
      t "TinyHalfLife2, the barrels float" hl2_barrels;
      t "TinyHalfLife2, the seesaw" hl2_seesaw;
      t "TinyPortal, speedy thing goes in" portal3d_fling;
      t "TinyPortal, the button and the door" portal3d_door;
      t "TinyShufflePuck, the view" shufflepuck_view;
      t "TinyShufflePuck, the rails hold" shufflepuck_rails;
      t "TinyShufflePuck, a bank shot: Robo-9 blocks it, Ned does not" shufflepuck_block;
      t "TinyShufflePuck, a bank and a straight shot" shufflepuck_aim;
      t "TinyMarbleMadness, the ramp's heights" marble_ramp;
      t "TinyMarbleMadness, the cliff breaks the marble, the step doesn't" marble_falls;
      t "TinyMarbleMadness, the steelie knocks the marble" marble_steelie;
      t "TinyMarbleMadness, rolling down a ramp" marble_rolls_down;
      t "TinyMarbleMadness, a robot drives to the goal" marble_robot;
      t "TinyMarbleMadness, physics=engine: the boxes are the course" marble_engine_course;
      t "TinyMarbleMadness, physics=engine: 5/7 by itself" marble_engine_five_sevenths;
      t "TinyMarbleMadness, physics=engine: the robot wins too" marble_engine_robot;
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
      t "TinyZeldaLinkPast, behind a tree or in front" lttp_behind_tree;
      t "TinyZeldaLinkPast, a bush cut" lttp_bush;
      t "TinyZeldaLinkPast, the pedestal and the pendants" lttp_pedestal;
      t "TinyRogue, the dungeons connected" rogue_connected;
      t "TinyRogue, a robot gets the Amulet" rogue_robot;
      t "TinyStreetFighter, the quarter circle" sf_quarter_circle;
      t "TinyStreetFighter, blocking high and low" sf_blocks;
      t "TinyStreetFighter, the fireball's motion" sf_fireball;
      t "TinyStreetFighter, the computer fights" sf_computer;
      t "TinyFinalFight, the combo, on the line" ff_combo;
      t "TinyFinalFight, a robot clears the street" ff_robot;
      Testutil_heavy.t "TinyBabaIsYou, every level solvable" baba_levels;
      t "TinyMissileCommand, a robot survives two waves" missile_robot;
      t "TinyMissileCommand, a chain reaction" missile_chain;
      t "TinyLemmings, a job per level" lemmings_levels;
      t "TinyLemmings, the fall that splats" lemmings_splat;
      t "TinyPuzzleBobble, the hexagonal grid" bobble_hex;
      t "TinyPuzzleBobble, popped and fallen" bobble_drop;
      Testutil_heavy.t "TinyPuzzleBobble, a robot clears the rounds" bobble_robot;
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
      Testutil_heavy.t "AiOthello, alpha-beta agrees with minimax" othello_alphabeta;
      t "AiOthello, the computer beats a greedy player" othello_greedy;
      t "AiChess, perft" chess_perft;
      t "AiChess, en passant, castling, promotion" chess_special_moves;
      t "AiChess, mates in one and a hanging queen" chess_search;
      t "AiChess, quiescence against the horizon effect" chess_quiescence;
      t "AiChess, move ordering" chess_ordering;
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
      t "TinyZeldaOcarina, locked on, sideways is an orbit" ocarina_orbit;
      t "TinyZeldaOcarina, the Stalfos' shield" ocarina_shield;
      t "TinyZeldaOcarina, circling reaches its back" ocarina_circle_to_its_back;
      t "TinyZeldaOcarina, the lock's two ranges" ocarina_lock_ranges;
      t "TinyZeldaOcarina, won by circling" ocarina_won_by_circling;
      t "TinyZeldaOcarina, the temple's door is a loading zone" ocarina_loading_zone;
      t "TinyZeldaOcarina, the night and its Stalchildren" ocarina_night;
      t "TinyRobotron, the two sticks" robotron_twin_stick;
      t "TinyRobotron, a grunt on an electrode, a hulk on the family" robotron_walks_into_things;
      t "TinyRobotron, the brain rebuilds a human" robotron_brain_rebuilds;
      t "TinyRobotron, a robot clears the first wave" robotron_robot;
      t "TinyPinball, the flipper carries the ball" pinball_flipper_throws;
      t "TinyPinball, ours and the engine agree" pinball_both_engines;
      t "TinyPinball, substeps=1 falls through the table" pinball_tunnels;
      t "TinyPinball, the ball never leaves the table" pinball_stays_on_the_table;
      t "TinyPinball, three balls and it is over" pinball_three_balls;
      t "TinyBoomerangFu, the boomerang comes back to a moving thrower" boomerang_returns;
      t "TinyBoomerangFu, who a flight cuts" boomerang_cuts;
      t "TinyBoomerangFu, the dash is a slash only with it in hand" boomerang_slash;
      t "TinyBoomerangFu, the holes, the jump and the terrace" boomerang_holes;
      t "TinyBoomerangFu, the computer crosses the river by a bridge" boomerang_bridge;
      t "TinyBoomerangFu, the computer does not fall" boomerang_pits;
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
      t "TinySensibleSoccer, a goal" sensible_goal;
      t "TinyJoust, the higher lance wins" joust_higher_wins;
      t "TinyJoust, a flap is a press, not a key held" joust_flap_is_a_press;
      t "TinyJoust, the ledges hold, with no collision code" joust_ledges_hold;
      t "TinyJoust, an egg hatches a tier up" joust_egg_hatches;
      t "TinyJoust, an egg collected, and the next wave" joust_egg_collected;
      t "TinyJoust, the lava keeps what falls in it" joust_lava;
      t "TinyDefender, the planet is a cylinder" defender_cylinder;
      t "TinyDefender, the abduction, and the mutant it makes" defender_abduction;
      t "TinyDefender, catching a falling human" defender_rescue;
      t "TinyDefender, dropped from too high" defender_drop;
      t "TinyDefender, the planet goes with the last human" defender_planet_goes;
      t "TinyDefender, the smart bomb is what you can see" defender_smart_bomb;
      t "TinyZaxxon, the projection, and the shadow that reads it" zaxxon_projection;
      t "TinyZaxxon, over the wall, or into it" zaxxon_over_the_wall;
      t "TinyZaxxon, a fuel tank is thirty seconds" zaxxon_fuel_tank;
      t "TinyZaxxon, out of fuel" zaxxon_out_of_fuel;
      t "TinyZaxxon, the end of the fortress" zaxxon_end_of_run;
      t "TinyZaxxon, hidden behind a wall you have passed" zaxxon_hidden_behind_a_wall;
      t "TinyDiablo, the dungeon holds together" diablo_dungeon_holds_together;
      t "TinyDiablo, a click is a place, and a path to it" diablo_click_walks_there;
      t "TinyDiablo, what it dropped" diablo_kills_and_loots;
      t "TinyDiablo, the stairs only go down" diablo_stairs_go_down;
      t "TinyHades, a boon is a number" hades_a_boon_is_a_number;
      t "TinyHades, the dash is the defence" hades_the_dash_is_the_defence;
      t "TinyHades, three boons between chambers" hades_between_chambers;
      t "TinyHades, dying pays for the next run" hades_death_pays_for_the_next_run;
      t "TinyMonumentValley, three apart is one pixel" mv_three_apart_is_one_pixel;
      t "TinyMonumentValley, the first monument is walked" mv_the_first_monument_is_walked;
      t "TinyMonumentValley, turning changes what connects" mv_turning_changes_what_connects;
      t "TinyMonumentValley, a step is a step" mv_a_step_is_a_step;
      t "TinyCeleste, coyote time" celeste_coyote;
      t "TinyCeleste, the jump buffer" celeste_buffer;
      t "TinyCeleste, a tap is a hop" celeste_variable_jump;
      t "TinyCeleste, corner correction" celeste_corners;
      t "TinyCeleste, one dash until you land" celeste_one_dash;
      t "TinyCeleste, the wall jump" celeste_wall_jump;
      t "TinyBraid, forgiveness: death undone" braid_forgiveness;
      t "TinyBraid, forgiveness: a robot that rewinds gets out" braid_robot;
      t "TinyBraid, mystery: the green key stays in hand" braid_mystery;
      t "TinyBraid, place: time is where you stand" braid_place;
      t "TinyBraid, decision: the shadow holds the plate" braid_decision;
      t "TinyVVVVVV, the flip goes all the way" vvvvvv_flip;
      t "TinyVVVVVV, the first room played through" vvvvvv_first_room;
      t "TinyVVVVVV, back at the checkpoint" vvvvvv_checkpoint;
      t "TinyVVVVVV, the gravity line" vvvvvv_gravity_line;
      t "TinyVVVVVV, off the bottom, in at the top" vvvvvv_wrap;
      t "TinyMarioGalaxy2D, a down per zone" galaxy_zones;
      t "TinyMarioGalaxy2D, standing underneath" galaxy_underneath;
      t "TinyMarioGalaxy2D, the arrow kept while held" galaxy_latch;
      t "TinyMarioGalaxy2D, the goomba's own down" galaxy_goomba;
      t "TinyMarioGalaxy2D, adrift in the void" galaxy_void;
      t "TinyMarioGalaxy2D, the galaxy played through" galaxy_played_through;
      t "TinyMarioGalaxy, euler's angles" galaxy3d_euler;
      t "TinyMarioGalaxy, a cube's gravity" galaxy3d_zones;
      t "TinyMarioGalaxy, round the planet" galaxy3d_round;
      t "TinyMarioGalaxy, the goomba's own down" galaxy3d_goomba;
      t "TinyMarioGalaxy, the galaxy played through" galaxy3d_played_through;
      t "TinyKarel, the levels' solutions" karel_solutions;
      t "TinyKarel, another maze" karel_other_maze;
      t "TinyKarel, mistakes and their lines" karel_errors;
      t "TinyStoneAge, the crumbling stone" stoneage_crumble;
      t "TinyStoneAge, the ride over the void" stoneage_ride;
      t "TinyStoneAge, keys and locks" stoneage_keys;
      t "TinyStoneAge, every level can be done" stoneage_levels;
      t "TinySuperMeatBoy, a try is its inputs" smb_replay_is_the_inputs;
      t "TinySuperMeatBoy, dying costs nothing" smb_death_is_cheap;
      t "TinySuperMeatBoy, Hello World" smb_hello_world;
      t "TinySuperMeatBoy, wall to wall" smb_wall_to_wall;
      t "TinySuperMeatBoy, pendulums" smb_pendulums;
      t "TinySuperMeatBoy, every try at once" smb_every_try;
      t "TinyMetalGearSolid, seen in the cone, not behind walls" mgs_seeing;
      t "TinyMetalGearSolid, the box" mgs_box;
      t "TinyMetalGearSolid, the knock" mgs_knock;
      t "TinyMetalGearSolid, the choke, from behind" mgs_choke;
      t "TinyMetalGearSolid, alert, evasion, infiltration" mgs_alert;
      t "TinyMetalGearSolid, caught, and a way out" mgs_caught_and_way_out;
      t "TinyGTA, the traffic keeps to the roads" gta_traffic;
      t "TinyGTA, any car is yours" gta_cars_are_yours;
      t "TinyGTA, running someone over" gta_run_over;
      t "TinyGTA, the police, and the stars" gta_police;
      t "TinyGTA, the phones" gta_missions;
      t "TinyZork, the parser" zork_parser;
      t "TinyZork, a walkthrough to the win" zork_walkthrough;
      t "TinyZork, the grue" zork_grue;
      t "TinyZork, the troll" zork_troll;
      t "TinyManiacMansion, the walkboxes" mm_walkboxes;
      t "TinyManiacMansion, a walkthrough to the lab" mm_walkthrough;
      t "TinyHamurabi, a year" hamurabi_year;
      t "TinyHamurabi, impeachment and the plague" hamurabi_impeached_and_plague;
      t "TinyHamurabi, think again" hamurabi_think_again;
      t "TinyHamurabi, ten years" hamurabi_ten_years;
      t "TinyTennisForTwo, the serve, and the net" tennis_serve_and_net;
      t "TinyTennisForTwo, two bounces" tennis_two_bounces;
      t "TinyTennisForTwo, the computer returns" tennis_computer_returns;
      t "TinyTonyHawk, the ramp" hawk_ramp;
      t "TinyTonyHawk, pumping to the air" hawk_pump_to_air;
      t "TinyTonyHawk, the chain's score" hawk_combo_score;
      t "TinyTonyHawk, the landing" hawk_landing;
      t "TinyTonyHawk, revert and manual" hawk_revert_manual;
      t "TinyMazeWar, the frames" mazewar_frames;
      t "TinyMazeWar, a step, and a shot" mazewar_move_and_shoot;
      t "TinyMazeWar, back from the dead" mazewar_respawn;
      t "TinyMazeWar, the robots" mazewar_robots;
      t "TinyFez, the four views" fez_views;
      t "TinyFez, background and walls" fez_background_and_walls;
      t "TinyFez, the cube" fez_the_cube;
      t "TinyPerspective, the start" perspective_start;
      t "TinyPerspective, the far bridge" perspective_bridge;
      t "TinyPerspective, the stairs" perspective_stairs;
      t "TinyPrinceOfPersia, the distances are the tables' sums" pop_tables;
      t "TinyPrinceOfPersia, a robot escapes the dungeon" pop_robot;
      t "TinyPrinceOfPersia, the gate closes, the shaft hurts" pop_gate_and_fall;
      t "TinyDDR, the chart is the tune" ddr_chart_is_the_tune;
      t "TinyDDR, judging a step" ddr_judging;
      t "TinyDDR, the average error is the calibration" ddr_average_error_is_the_calibration;
      t "TinyDDR, the clock is the music's" ddr_the_clock_is_the_music;
      t "TinyRockBand, the parts are the voices" rockband_parts_are_voices;
      t "TinyRockBand, a guitar is fret and strum" rockband_guitar_strums;
      t "TinyRockBand, a keyboard has no strum" rockband_keys_no_strum;
      t "TinyRockBand, a drum is struck" rockband_drums_struck;
      t "TinyRockBand, a difficulty per part" rockband_difficulty;
      t "TinyRockBand, the drums reduced their own way" rockband_drums_reduced;
      t "TinyRockBand, a long note held goes on scoring" rockband_sustain;
      t "TinyRockBand, the crowd" rockband_crowd;
      t "TinyGuitarHero, the road, straightened" gh_the_road;
      t "TinyGuitarHero, the same part, reduced" gh_difficulty;
      t "TinyGuitarHero, fret and strum" gh_strum;
      t "TinyGuitarHero, a long note held" gh_sustain;
      t "TinySimCity, power: zones pass it on, roads don't" simcity_power;
      t "TinySimCity, a zone grows with power and a road" simcity_growth;
      t "TinySimCity, the valves" simcity_valves;
      t "TinySimCity, the smog" simcity_smog;
      t "TinySimCity, the tax rate" simcity_tax;
      t "TinySimCity, the same city replays the same" simcity_replays;
      t "TinyCivilization, the world" civ_world;
      t "TinyCivilization, the tree of advances" civ_tree;
      t "TinyCivilization, a city grows and builds" civ_city;
      t "TinyCivilization, combat, and the stack that dies" civ_combat;
      t "TinyCivilization, cities keep their distance" civ_found;
      t "TinyCivilization, the rival" civ_rival;
      t "TinyMarioWorld, the feet on a slope" mw_slope;
      t "TinyMarioWorld, slower uphill" mw_uphill;
      t "TinyMarioWorld, the slide" mw_slide;
      t "TinyMarioWorld, the P meter and the takeoff" mw_takeoff;
      t "TinyMarioWorld, flight is a trade" mw_trade;
      t "TinyMarioWorld, the map and its secret" mw_map;
      t "TinyMarioWorld, the keyhole in the sky" mw_keyhole;
      t "TinyRType, the Force sent and called back" rtype_force;
      t "TinyRType, the Force as a shield" rtype_shield;
      t "TinyRType, the beam" rtype_beam;
      t "TinyRType, the battleship" rtype_battleship;
      t "TinyRType, a pilot takes the battleship down" rtype_pilot;
      t "TinyIncredibleMachine, every puzzle solved by its solution" tim_solutions;
      t "TinyIncredibleMachine, the same machine runs the same" tim_deterministic;
      t "TinyWorms, the craters" worms_crater;
      t "TinyWorms, walking" worms_walk;
      t "TinyWorms, the ninja rope" worms_rope;
      t "TinyWorms, the grenade bounces" worms_grenade;
      t "TinyWorms, the blast and the water" worms_blast;
      t "TinyXCOM, time units" xcom_time_units;
      t "TinyXCOM, what a soldier sees" xcom_sight;
      t "TinyXCOM, the chance to hit" xcom_hit_chance;
      t "TinyXCOM, cover wears away" xcom_cover_wears;
      t "TinyXCOM, reaction fire" xcom_reaction_fire;
      t "TinyXCOM, the aliens' turn" xcom_aliens_turn;
      t "TinyXCOM, a battle ends" xcom_battle_ends;
      t "TinyMetroid, the checker: one key a round" metroid_progression;
      t "TinyMetroid, the checker catches a broken world" metroid_broken;
      t "TinyMetroid, the jump against the checker's" metroid_jump;
      t "TinyMetroid, the tunnel and the ball" metroid_tunnel;
      t "TinyMetroid, a red door" metroid_door;
      t "TinyMetroid, a bomb" metroid_bomb;
      t "TinyMetroid, Kraid feels only missiles" metroid_kraid;
      t "TinyMetroid, the climbs, with the game's jump" metroid_climbs;
      t "TinyFreeCell, deal 1" freecell_deal;
      t "TinyFreeCell, the moves" freecell_moves;
      t "TinyFreeCell, the supermove" freecell_supermove;
      t "TinySolitaire, deal 1 the Klondike way" solitaire_deal;
      t "TinySolitaire, a card home, the stock round" solitaire_moves;
      t "TinyCoreWar, the assembler" corewar_assemble;
      t "TinyCoreWar, the assembler's mistakes, and their lines" corewar_mistakes;
      t "TinyCoreWar, the Imp walks, the Dwarf bombs" corewar_imp_and_dwarf;
      t "TinyCoreWar, three fights" corewar_fights;
      t "TinyFrogger, the lanes are a formula of time" frogger_lanes;
      t "TinyFrogger, across the road, onto the logs, off the edge" frogger_crossing;
      t "TinyFrogger, the bays" frogger_bays;
      t "TinyLunarLander, good and hard landings" lander_good;
      t "TinyLunarLander, crashes" lander_crashes;
      t "TinyLunarLander, the tank" lander_fuel ]
