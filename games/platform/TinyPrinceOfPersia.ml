(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Prince of Persia (Jordan Mechner, Broderbund, 1989):
 * a prince in the dungeons of a palace, four rooms of ledges, a floor
 * that gives way, spikes, a gate held open by a pressure plate, and the
 * door out. Left/right to run (and to turn), up to jump up (or to
 * climb, under a ledge; or to go through the door), up with left/right
 * held for a standing jump, up while running for a running jump, down
 * at an edge to climb down and hang, then down to let go or up to climb
 * back, shift with left/right for a careful step. A fall of one floor
 * is nothing, of two a wound (the triangles at the bottom), of three
 * death; spikes kill unless you step through them carefully.
 *
 * Jordan Mechner wrote it alone, on an Apple II, from 1985 to 1989,
 * after Karateka (1984). To make the prince move like a man, he filmed
 * his younger brother David running, jumping and climbing in a parking
 * lot, in white clothes, and traced the frames of the film over, one
 * by one: rotoscoping, Max Fleischer's process of 1915 for his
 * cartoons. That's why the prince's moves still look right, with a
 * dozen frames each: they are a real body's. His journals of those
 * years are published (The Making of Prince of Persia, 2011), and the
 * Apple II source code was found and released in 2012. (Names and
 * dates from memory, to check.)
 *
 * The trick of this game, the idea that is new here: the movement is
 * the animation. Every move -- starting to run, running, stopping,
 * turning, the standing jump, the running jump, jumping up, climbing
 * up, climbing down, hanging, falling, the careful step -- is a table
 * of frames ([anims]), each frame a pose and how far it moves the
 * prince (dx forward, dy up). A move once started is played to its
 * end: the player is *committed*, and the keys are read only at the
 * frames the table allows ([read]): standing, hanging, and twice per
 * stride of the run. The distances are the tables' sums: a standing
 * jump always carries the prince two tiles, a running one three, a
 * careful step never goes over the edge. And the grid of tiles decides
 * how a move ends: a wall stops it ([blocked]), missing floor turns it
 * into a fall ([tick]). The frames are played at 15 a second ([tick]
 * every 4 updates; the original: 12), and the prince moves in the
 * jumps of his frames, as he did.
 *
 * It's the opposite of TinyCeleste.ml, where a key changes the
 * velocity in the same frame and the body follows the input: Celeste
 * is precise, Prince of Persia is heavy -- you plan a jump from the
 * right place (a careful step to the edge, then up), and a run started
 * can't be stopped on a coin. The weight is the game, as the controls
 * of Another World (Éric Chahi, 1991) and of Flashback (Paul Cuisset,
 * Delphine, 1992), its heirs, and "cinematic platformers" since
 * (Limbo, Inside).
 *
 * The level is PoP's own geometry: rooms of 10 x 3 tiles, a tile 100
 * wide and 250 high, each with a floor at its bottom (or none: a hole),
 * the screen flipping from room to room (Camera2d.flip, as in
 * TinyRick). The prince is always on a floor, hanging from one, or in
 * the air: his row is where his feet are ([row_at]).
 *
 * What it uses: Stickman (the brawler kit's stick figures,
 * gamekits/brawler/: a pose as the angles of the joints, which is what
 * a rotoscoped frame becomes without an artist; its [hand] gives how
 * low the prince hangs under a ledge), Tilemap (only as the grid of
 * the level's characters, [get], [set], [find]: its tiles are square,
 * PoP's aren't, so the geometry is the game's own), Camera2d (the
 * flip-screen), Scene2d. Not the platformer kit's Tile_move: moving a
 * pixel at a time until a wall is what the animation tables replace,
 * and Stickman's [at] (keyframes interpolated) isn't used either: the
 * frames are shown as they are, a flipbook.
 *
 * Left as exercises: the guards and the sword fight (Karateka's
 * engine in PoP: advance, retreat, strike, parry, each a table of
 * frames again), the 60 minutes to save the princess (a clock in the
 * model, shown when a level starts), the potions (a red one heals a
 * triangle, a big one adds one), grabbing a ledge at the end of a
 * running jump or while falling (shift in the original: here only
 * standing under a ledge), a loose floor also shaken by the prince
 * landing near it, more levels (the original has 12, and the palace
 * above).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The dungeon *)
(*****************************************************************************)

(* four rooms of 10 x 3 tiles: _ a floor, ' ' no floor (a hole), # a
 * wall, ^ spikes, L a loose floor, P a pressure plate, G a gate (at the
 * tile's left edge, opened by the plate), E the door out, S the start;
 * r (rubble) is a loose floor that has fallen *)
let dungeon =
  [ "#S__ __L____^____ _#";
    "#__ _G___________P_#";
    "### ################";
    "#_____^__ ##### __E#";
    "#########_______####";
    "####################" ]

let tile_w = 100.
let tile_h = 250.
let room_size = (1000., 750.)
let level = Tilemap.of_strings tile_w dungeon
let bounds : Camera2d.rect = { left = 0.; right = tile_w * float_of_int (Tilemap.cols level); top = 0.; bottom = -.(tile_h * float_of_int (Tilemap.rows level)) }

let has_floor (c : char option) : bool = match c with Some ('_' | '#' | '^' | 'L' | 'P' | 'G' | 'E' | 'S' | 'r') -> true | _ -> false

(* the floor of row [r] is at the bottom of its tiles; row 0 is the top
 * one *)
let floor_y (r : int) : number = -.(tile_h * float_of_int (r +.. 1))
let col_at (x : number) : int = int_of_float (floor (x / tile_w))

(* the row the prince is in, his feet at [y]: standing on its floor,
 * jumping above it, or hanging under the floor of the row above *)
let row_at (y : number) : int = int_of_float (floor (-.(y + 10.) / tile_h))

(*****************************************************************************)
(* The prince's moves: poses *)
(*****************************************************************************)

(* the stick figure's poses (see Stickman.mli: each limb's two angles
 * from straight down, positive forward; the torso's lean) *)
let pose lean front_arm back_arm front_leg back_leg : Stickman.pose = { lean; front_arm; back_arm; front_leg; back_leg }

let stand = Stickman.stand
let step_a = pose 5. (-10., 10.) (10., 30.) (25., 5.) (-15., -20.)
let step_b = pose 3. (5., 20.) (-5., 15.) (10., 0.) (-5., -30.)
let set_off1 = pose 8. (-15., 20.) (20., 50.) (25., 0.) (-15., -30.)
let set_off2 = pose 12. (-30., 10.) (35., 80.) (35., 5.) (-25., -55.)
let run_a = pose 15. (-40., 10.) (45., 100.) (45., 10.) (-35., -75.)
let run_b = pose 15. (0., 60.) (10., 50.) (-5., -5.) (25., -50.)
let run_c = pose 15. (45., 100.) (-40., 10.) (-35., -75.) (45., 10.)
let run_d = pose 15. (10., 50.) (0., 60.) (25., -50.) (-5., -5.)
let brake1 = pose (-10.) (30., 60.) (-20., 20.) (35., 30.) (-10., -40.)
let brake2 = pose (-5.) (20., 40.) (-10., 15.) (20., 15.) (-10., -20.)
let sideways = pose 0. (0., 5.) (0., 5.) (0., 0.) (0., 0.)
let crouch = pose 25. (40., 70.) (20., 60.) (70., -15.) (55., -30.)
let deep = pose 45. (60., 20.) (40., 10.) (95., -10.) (80., -40.)
let take_off = pose 20. (120., 150.) (-50., -20.) (30., 10.) (-30., -40.)
let flying = pose 10. (140., 160.) (-70., -40.) (70., 20.) (-20., -80.)
let landing = pose 15. (60., 90.) (-20., 20.) (40., 20.) (-10., -30.)
let reach = pose 0. (170., 178.) (165., 175.) (5., -5.) (-5., -10.)
let hanging = pose 0. (180., 180.) (178., 180.) (5., 0.) (-5., -5.)
let pull = pose 15. (150., 40.) (140., 30.) (20., 0.) (0., -10.)
let knee = pose 35. (60., 10.) (50., 0.) (100., 0.) (-10., -20.)
let rise = pose 20. (30., 20.) (20., 10.) (40., 0.) (-10., -10.)
let falling = pose (-5.) (130., 100.) (-120., -100.) (20., -10.) (-15., -30.)

(* the prince, standing, is this high; hanging, his feet are [hang_drop]
 * under his hands, that is under the ledge *)
let height = 150.
let hang_drop = snd (Stickman.hand height hanging)

(*****************************************************************************)
(* The prince's moves: the tables *)
(*****************************************************************************)

type move =
  | Stand | Set_off | Run | Stop | Turn | Step
  | Stand_jump | Run_jump | Jump_up | Jump_grab | Hang | Climb_up | Climb_down
  | Drop | Fall_off | Fall | Land_soft | Land_hard | Dead

(* a frame: the pose shown, how far it moves the prince (forward, the
 * way he faces, and up), and whether the keys are read at it *)
type frame = { p : Stickman.pose; dx : number; dy : number; read : bool }

(* a move: its frames, and the move after its last one *)
type anim = { frames : frame array; next : move }

let f ?(read = false) (p : Stickman.pose) (dx : number) (dy : number) : frame = { p; dx; dy; read }

(* the climbs' rise, a share of [hang_drop] per frame *)
let climb = [| 0.12; 0.24; 0.3; 0.24; 0.1; 0. |]

(* the tables. The sums are the game's distances: a stride 100 (a
 * tile), a standing jump 200 (two), a running jump 300 (three), a
 * careful step 40; the jumps come down where they took off *)
let anims (m : move) : anim =
  match m with
  | Stand -> { frames = [| f ~read:true stand 0. 0. |]; next = Stand }
  | Set_off -> { frames = [| f set_off1 10. 0.; f set_off2 15. 0.; f run_a 20. 0.; f run_b 25. 0. |]; next = Run }
  | Run -> { frames = [| f ~read:true run_c 25. 0.; f run_d 25. 0.; f ~read:true run_a 25. 0.; f run_b 25. 0. |]; next = Run }
  | Stop -> { frames = [| f brake1 15. 0.; f brake2 8. 0.; f step_b 0. 0. |]; next = Stand }
  | Turn -> { frames = [| f sideways 0. 0.; f sideways 0. 0.; f step_b 0. 0. |]; next = Stand }
  | Step -> { frames = [| f step_a 8. 0.; f step_b 8. 0.; f step_a 8. 0.; f step_b 8. 0.; f stand 8. 0. |]; next = Stand }
  | Stand_jump ->
      { frames = [| f crouch 0. 0.; f crouch 0. 0.; f take_off 35. 20.; f flying 40. 15.; f flying 40. 5.; f flying 40. (-5.); f landing 35. (-15.); f crouch 10. (-20.) |]; next = Stand }
  | Run_jump ->
      { frames = [| f take_off 30. 0.; f take_off 40. 25.; f flying 40. 20.; f flying 40. 10.; f flying 40. (-10.); f flying 40. (-20.); f landing 40. (-25.); f run_a 30. 0. |]; next = Run }
  | Jump_up -> { frames = [| f crouch 0. 0.; f reach 0. 30.; f reach 0. 20.; f reach 0. (-20.); f crouch 0. (-30.) |]; next = Stand }
  (* up to the ledge: the rise that puts the hands on it, the last frame
   * snapping him under it ([finish]) *)
  | Jump_grab -> let up = (tile_h - hang_drop) / 2. in { frames = [| f crouch 0. 0.; f reach 0. up; f reach 0. up |]; next = Hang }
  | Hang -> { frames = [| f ~read:true hanging 0. 0. |]; next = Hang }
  | Climb_up ->
      let poses = [| pull; pull; knee; knee; rise; stand |] and dxs = [| 0.; 0.; 10.; 20.; 20.; 10. |] in
      { frames = Array.init 6 (fun i -> f poses.(i) dxs.(i) (climb.(i) * hang_drop)); next = Stand }
  (* climbing up played backwards: he turns first ([start]) *)
  | Climb_down ->
      let poses = [| rise; knee; knee; pull; pull; hanging |] and dxs = [| -10.; -20.; -20.; -10.; 0.; 0. |] in
      { frames = Array.init 6 (fun i -> f poses.(i) dxs.(i) (-.climb.(5 -.. i) * hang_drop)); next = Hang }
  | Drop -> { frames = [| f hanging 0. (-10.); f falling 0. (-20.); f falling 0. (-30.); f falling 0. (-40.) |]; next = Fall }
  | Fall_off -> { frames = [| f falling 20. (-10.); f falling 15. (-20.); f falling 10. (-30.); f falling 5. (-40.) |]; next = Fall }
  | Fall -> { frames = [| f falling 0. (-50.) |]; next = Fall }
  | Land_soft -> { frames = [| f crouch 0. 0.; f crouch 0. 0.; f brake2 0. 0. |]; next = Stand }
  | Land_hard -> { frames = Array.init 8 (fun i -> f (if i < 6 then deep else crouch) 0. 0.); next = Stand }
  | Dead -> { frames = [| f stand 0. 0. |]; next = Dead }

(* the moves on the ground: a floor must be under him *)
let grounded (m : move) : bool = match m with Stand | Set_off | Run | Stop | Turn | Step | Land_soft | Land_hard -> true | _ -> false
let falling_move (m : move) : bool = match m with Drop | Fall_off | Fall -> true | _ -> false

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type prince = {
  x : number; (* his feet *)
  y : number;
  facing : number; (* 1. right, -1. left *)
  move : move;
  frame : int; (* in the move's table *)
  fall_from : number; (* the y his fall started at *)
  hp : int;
}

(* a piece of loose floor falling *)
type debris = { bx : number; by : number; vy : number }

type game = {
  map : Tilemap.t;
  prince : prince;
  loose : ((int * int) * int) list; (* the loose floors shaking, frames since stepped on *)
  debris : debris list;
  gate : number; (* how open the gates are, 0. to 1. *)
  held : int; (* frames the gates stay open *)
  hurt : int; (* frames of the red flash *)
  dead : int; (* frames since he died, 0 if alive *)
  frames : int;
  out : bool; (* through the door *)
}

type scene = Title | Playing of game | Escaped of int | Game_over
type model = scene Scene2d.t

let max_hp = 3

let new_game () : game =
  let c, r = List.hd (Tilemap.find level 'S') in
  let prince = { x = (float_of_int c + 0.5) * tile_w; y = floor_y r; facing = 1.; move = Stand; frame = 0; fall_from = 0.; hp = max_hp } in
  { map = Tilemap.set level c r '_'; prince; loose = []; debris = []; gate = 0.; held = 0; hurt = 0; dead = 0; frames = 0; out = false }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The grid decides *)
(*****************************************************************************)

let tile (g : game) (c : int) (r : int) : char option = Tilemap.get g.map c r
let gate_closed (g : game) : bool = g.gate < 0.7

(* [blocked g r x x' s]: the prince in row [r] moving from [x] to [x']
 * (going [s], 1. or -1.) would push his front (20 ahead of his feet)
 * into a wall, off the level, or through a closed gate: where he stops,
 * or None *)
let blocked (g : game) (r : int) (x : number) (x' : number) (s : number) : number option =
  let front = col_at (x + (s * 20.)) and front' = col_at (x' + (s * 20.)) in
  if front = front' then None
  else
    let wall = match tile g front' r with Some '#' | None -> true | _ -> false in
    let gate = tile g (max front front') r = Some 'G' && gate_closed g in
    if wall || gate then Some ((float_of_int (max front front') * tile_w) - (s * 21.)) else None

(* the boundary between his tile and the next one, the way he faces *)
let edge (p : prince) : number = float_of_int (col_at p.x +.. if p.facing > 0. then 1 else 0) * tile_w

(* standing under a hole in the ceiling, a ledge in front, up there *)
let can_grab (g : game) (p : prince) : bool =
  let c = col_at p.x and r = row_at p.y in
  let ahead = c +.. int_of_float p.facing in
  (not (has_floor (tile g c (r -.. 1)))) && has_floor (tile g ahead (r -.. 1)) && tile g ahead (r -.. 1) <> Some '#'

(* at the edge of a floor, facing the hole *)
let can_climb_down (g : game) (p : prince) : bool =
  let ahead = tile g (col_at p.x +.. int_of_float p.facing) (row_at p.y) in
  (not (has_floor ahead)) && ahead <> Some '#' && ahead <> None && Float.abs (edge p - p.x) < 60.

(* [landing g c from y']: the first floor his feet cross falling from
 * [from] down to [y'] in column [c], if any *)
let landing (g : game) (c : int) (from : number) (y' : number) : int option =
  let rec go r = if r >= Tilemap.rows g.map then None else let fy = floor_y r in if fy < y' then None else if fy < from && has_floor (tile g c r) then Some r else go (r +.. 1) in
  go (max 0 (row_at from))

(*****************************************************************************)
(* Update: a frame of a move *)
(*****************************************************************************)

(* [start p m]: [m]'s first frame, and what starting it does at once *)
let start (p : prince) (m : move) : prince =
  let p = { p with move = m; frame = 0 } in
  match m with
  | Turn -> { p with facing = -.p.facing }
  (* on the edge, then turned to face the ledge he'll hang from *)
  | Climb_down -> { p with x = edge p - (p.facing * 40.); facing = -.p.facing }
  | Drop | Fall_off -> { p with fall_from = p.y }
  | _ -> p

(* the last frame done: where it leaves him *)
let finish (g : game) (p : prince) : prince =
  match p.move with
  (* hands on the ledge: under it, [hang_drop] below *)
  | Jump_grab -> let e = edge p in start { p with x = e - (p.facing * 20.); y = floor_y (row_at p.y -.. 1) - hang_drop } Hang
  | Stand_jump | Run_jump | Jump_up when not (has_floor (tile g (col_at p.x) (row_at p.y))) -> start p Fall_off
  | m -> start p (anims m).next

(* the keys, read at the frames that allow it: which move to start *)
let decide (g : game) (keys : keyboard) (p : prince) : move option =
  let dir = to_x keys in
  match p.move with
  | Stand ->
      if keys.kup && dir = p.facing then Some Stand_jump
      else if keys.kup then Some (if can_grab g p then Jump_grab else Jump_up)
      else if keys.kdown && can_climb_down g p then Some Climb_down
      else if dir = p.facing && keys.kshift then Some Step
      else if dir = p.facing then Some Set_off
      else if dir = -.p.facing then Some Turn
      else None
  | Run -> if keys.kup then Some Run_jump else if dir <> p.facing then Some Stop else None
  | Hang -> if keys.kup then Some Climb_up else if keys.kdown then Some Drop else None
  | _ -> None

(* hurt by a fall, or by a stone *)
let wound (g : game) : game =
  let p = g.prince in
  if p.hp <= 1 then { g with prince = start { p with hp = 0 } Dead; dead = 1; hurt = 20 } else { g with prince = { p with hp = p.hp -.. 1 }; hurt = 20 }

let die (g : game) : game = { g with prince = start { g.prince with hp = 0 } Dead; dead = 1; hurt = 20 }

(* one frame of the prince's move (15 a second) *)
let tick (keys : keyboard) (g : game) : game =
  let p = g.prince in
  let r = row_at p.y in
  (* the door, and the keys where the table reads them *)
  if p.move = Stand && keys.kup && tile g (col_at p.x) r = Some 'E' then { g with out = true }
  else
    let p = if (anims p.move).frames.(p.frame).read then match decide g keys p with Some m -> start p m | None -> p else p in
    let fr = (anims p.move).frames.(p.frame) in
    let s = p.facing * (if fr.dx < 0. then -1. else 1.) in
    let x' = p.x + (p.facing * fr.dx) in
    (* a careful step stops at the edge *)
    let x' = if p.move = Step && col_at x' <> col_at p.x && not (has_floor (tile g (col_at x') r)) then edge p - (p.facing * 1.) else x' in
    let climbing = p.move = Climb_up || p.move = Climb_down in
    let x', bumped = if fr.dx = 0. || climbing then (x', false) else match blocked g r p.x x' s with Some x -> (x, true) | None -> (x', false) in
    let y' = p.y + fr.dy in
    let entered = col_at x' <> col_at p.x in
    let p' = { p with x = x'; y = y' } in
    (* where the grid ends the move: a wall, no floor, a floor to land on *)
    if falling_move p.move then
      match landing g (col_at x') p.y y' with
      | Some r' ->
          let p' = { p' with y = floor_y r' } in
          let drop = p.fall_from - p'.y in
          let g = { g with prince = p' } in
          if tile g (col_at x') r' = Some '^' || drop > 2.5 * tile_h then die g
          else if drop > 1.5 * tile_h then let g = wound g in if g.dead > 0 then g else { g with prince = start g.prince Land_hard }
          else { g with prince = start p' Land_soft }
      | None -> { g with prince = (if p'.frame +.. 1 >= Array.length (anims p.move).frames then finish g p' else { p' with frame = p'.frame +.. 1 }) }
    else if grounded p.move && not (has_floor (tile g (col_at x') r)) then { g with prince = start p' Fall_off }
    else if grounded p.move && entered && p.move <> Step && tile g (col_at x') r = Some '^' then die { g with prince = p' }
    else if bumped && (p.move = Set_off || p.move = Run) then { g with prince = start p' Stand }
    else
      let last = p'.frame +.. 1 >= Array.length (anims p.move).frames in
      let landed_on_spikes = last && (p.move = Stand_jump || p.move = Run_jump) && tile g (col_at x') r = Some '^' in
      if landed_on_spikes then die { g with prince = p' }
      else { g with prince = (if last then finish g p' else { p' with frame = p'.frame +.. 1 }) }

(*****************************************************************************)
(* Update: the dungeon *)
(*****************************************************************************)

(* standing on a tile, not over it in a jump *)
let on_tile (g : game) : (int * int * char) option =
  let p = g.prince in
  if grounded p.move then let c = col_at p.x and r = row_at p.y in Option.map (fun t -> (c, r, t)) (tile g c r) else None

(* a loose floor stepped on shakes, then falls; its stones land on the
 * floor below as rubble, on the prince if he's there *)
let loose_floors (g : game) : game =
  let loose = match on_tile g with Some (c, r, 'L') when not (List.mem_assoc (c, r) g.loose) -> ((c, r), 0) :: g.loose | _ -> g.loose in
  let loose = List.map (fun (cr, n) -> (cr, n +.. 1)) loose in
  let falling, loose = List.partition (fun (_, n) -> n >= 30) loose in
  let map = List.fold_left (fun m ((c, r), _) -> Tilemap.set m c r ' ') g.map falling in
  let debris = List.map (fun ((c, r), _) -> { bx = (float_of_int c + 0.5) * tile_w; by = floor_y r; vy = 0. }) falling @ g.debris in
  let g = { g with map; loose } in
  List.fold_left
    (fun g d ->
      let y' = d.by + d.vy in
      let c = col_at d.bx in
      match landing g c d.by y' with
      | Some r ->
          let g = { g with map = Tilemap.set g.map c r 'r' } in
          if g.dead = 0 && grounded g.prince.move && col_at g.prince.x = c && row_at g.prince.y = r then wound g else g
      | None -> { g with debris = { d with by = y'; vy = d.vy - 1.2 } :: g.debris })
    { g with debris = [] } debris

(* the plate opens the gates; they stay open a while, then close slowly *)
let gates (g : game) : game =
  let held = match on_tile g with Some (_, _, 'P') -> 240 | _ -> g.held -.. 1 in
  let gate = if held > 0 then Float.min 1. (g.gate + 0.05) else Float.max 0. (g.gate - (1. / 150.)) in
  { g with held; gate }

let update_game (computer : computer) (g : game) : game =
  let g = { g with frames = g.frames +.. 1; hurt = max 0 (g.hurt -.. 1) } in
  let g = if g.dead > 0 then { g with dead = g.dead +.. 1 } else if g.frames mod 4 = 0 then tick computer.keyboard g else g in
  gates (loose_floors g)

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer g in
      if g.out then Scene2d.go (Escaped g.frames) s
      else if g.dead > 90 then Scene2d.go Game_over s
      else { s with scene = Playing g }
  | Escaped _ -> if space then Scene2d.go Title s else s
  | Game_over -> if space then Scene2d.go (Playing (new_game ())) s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let stone = rgb 150 140 120
let wall_c = rgb 95 90 110
let dark = rgb 35 38 60

let slab (color : color) : shape = group [ rectangle color tile_w 20. |> move_y 10.; rectangle (rgb 190 180 160) tile_w 4. |> move_y 18. ]

(* a tile, drawn with its floor's middle at (0, 0) *)
let tile_shape (g : game) (c : int) (r : int) (t : char) : shape =
  let spike i = triangle (rgb 210 210 220) 12. |> move ((float_of_int i * 30.) - 30.) 28. in
  match t with
  | '_' -> slab stone
  | '#' ->
      group
        ([ rectangle wall_c tile_w tile_h |> move_y (tile_h / 2.) ]
        @ List.init 5 (fun i -> rectangle (rgb 70 65 85) tile_w 3. |> move_y (float_of_int i * 50.))
        @ List.init 5 (fun i -> rectangle (rgb 70 65 85) 3. 50. |> move ((if i mod 2 = 0 then -25. else 25.)) ((float_of_int i * 50.) + 25.)))
  | '^' -> group (slab stone :: List.init 3 spike)
  | 'L' ->
      let shaking = match List.assoc_opt (c, r) g.loose with Some n -> if n mod 4 < 2 then 3. else -3. | None -> 0. in
      group [ slab (rgb 185 150 90); rectangle (rgb 70 55 35) 3. 22. |> rotate 30. |> move 10. 10.; rectangle (rgb 70 55 35) 3. 22. |> rotate (-40.) |> move (-25.) 10. ] |> move_y shaking
  | 'P' ->
      let pressed = match on_tile g with Some (c', r', _) when c' = c && r' = r -> 22. | _ -> 26. in
      group [ slab stone; rectangle (rgb 200 190 150) 60. 8. |> move_y pressed ]
  | 'G' ->
      let h = (tile_h - 30.) * (1. - g.gate) in
      group [ slab stone; group (rectangle (rgb 60 60 70) 14. h :: List.init (int_of_float (h / 30.)) (fun i -> rectangle (rgb 170 170 180) 18. 3. |> move_y ((h / 2.) - 10. - (float_of_int i * 30.)))) |> move (-.(tile_w / 2.)) (tile_h - (h / 2.)) ]
  | 'E' ->
      group [ slab stone; rectangle (rgb 200 170 60) 100. 190. |> move_y 115.; rectangle black 80. 175. |> move_y 108.; rectangle (rgb 110 90 40) 60. 10. |> move_y 30. ]
  | 'r' -> group [ slab stone; rectangle (rgb 165 150 115) 30. 10. |> rotate 15. |> move (-20.) 25.; rectangle (rgb 165 150 115) 24. 8. |> rotate (-20.) |> move 25. 24. ]
  | _ -> group []

(* the dungeon: its wall behind, torches, the tiles *)
let torches = [ (250., -120.); (1450., -120.); (750., -370.); (550., -870.); (1350., -870.) ]

let view_dungeon (g : game) : shape list =
  let back = rectangle dark (bounds.right - bounds.left) (bounds.top - bounds.bottom) |> move ((bounds.left + bounds.right) / 2.) ((bounds.top + bounds.bottom) / 2.) in
  let flame i = let t = float_of_int (g.frames +.. (i *.. 7)) in group [ rectangle (rgb 90 60 30) 8. 30.; oval orange 18. (26. + (4. * sin (t / 3.))) |> move_y 26.; oval yellow 8. 14. |> move_y 22. ] in
  let tiles =
    List.concat
      (List.init (Tilemap.rows g.map) (fun r ->
           List.init (Tilemap.cols g.map) (fun c ->
               match tile g c r with Some t -> tile_shape g c r t |> move ((float_of_int c + 0.5) * tile_w) (floor_y r) | None -> group [])))
  in
  (back :: List.mapi (fun i (x, y) -> flame i |> move x y) torches) @ tiles

(* the prince in white, his far limbs grey; dead, lying down *)
let view_prince (g : game) : shape =
  let p = g.prince in
  let body = Stickman.draw white (rgb 170 170 185) height p.facing (anims p.move).frames.(p.frame).p in
  if p.move = Dead then body |> rotate (-90. * p.facing) |> move p.x (p.y + 8.) else body |> move p.x p.y

let view_game (g : game) : shape list =
  let p = g.prince in
  let cam = Camera2d.flip bounds room_size p.x (p.y + 10.) Camera2d.origin in
  let world = view_dungeon g @ List.map (fun d -> rectangle (rgb 165 150 115) 60. 14. |> move d.bx (d.by + 10.)) g.debris @ [ view_prince g ] in
  let health = List.init max_hp (fun i -> triangle (if i < p.hp then red else rgb 70 20 20) 16. |> rotate 180. |> move (-460. + (float_of_int i * 40.)) (-420.)) in
  (* the room only: the next ones are off screen *)
  let band y = rectangle black 1000. 125. |> move_y y in
  [ Camera2d.view cam world; band 437.5; band (-437.5) ] @ health @ (if g.hurt > 0 then [ rectangle red 1000. 750. |> fade 0.3 ] else [])

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_game (new_game ())
      @ [ rectangle black 860. 300. |> fade 0.85 |> move_y 40.; text (rgb 250 200 60) 6. "TINY PRINCE OF PERSIA" |> move_y 140.;
          text white 2.2 "left/right run or turn   up jump up, climb, or the door" |> move_y 80.;
          text white 2.2 "up + left/right standing jump   up while running: running jump" |> move_y 45.;
          text white 2.2 "down at an edge: hang, then down to drop   shift: careful step" |> move_y 10. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-60.) ]
  | Playing g -> view_game g
  | Escaped frames ->
      [ text (rgb 250 200 60) 6. "YOU ESCAPED!"; text white 3. (Printf.sprintf "IN %d SECONDS" (frames /.. 60)) |> move_y (-80.) ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ]
  | Game_over -> [ text red 6. "YOU DIED"; text white 3. "space to try again" |> move_y (-80.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
