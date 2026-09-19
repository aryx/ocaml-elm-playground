(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Rick Dangerous (Simon Phipps, Core Design, 1989): an
 * explorer in a fedora, in a temple full of traps, and the famous start
 * -- a boulder rolling after you down a corridor. Left/right to run, up
 * to jump (or climb, at a ladder), down to climb down, space to shoot
 * (6 bullets), x to drop a stick of dynamite (6, then run: it blows up
 * walls, natives, and you). Two rooms: get out of the first one alive,
 * through the second one's wall and past its dart trap, to the exit.
 *
 * Rick Dangerous was a game of dying: every trap is hidden until it
 * kills you once, and the next time you know. Its players learned the
 * rooms by heart, with 6 lives and a checkpoint at each room's door; it
 * was hard, and loved for it, on the Amiga, the Atari ST, the C64 and
 * the ZX Spectrum. Core Design went on to make Tomb Raider, whose Lara
 * Croft is Rick's heir, and Indiana Jones's, whom Rick is a parody of.
 * (Names and dates from memory, to check.)
 *
 * The new idea here is traps as tiles: a tile of the map can be a
 * spike ('^', deadly to touch), an invisible trigger ('t': stepping on
 * it fires the dart shooter 'd' of its row, once), a wall only dynamite
 * breaks ('b'). The level is data again, and the dangers are in it
 * ([solo_map]), the code only saying what each tile does
 * ([on_spikes], [spring_traps], [explode]).
 *
 * And the screen is a room: the camera doesn't follow Rick, it jumps
 * from room to room when he crosses a door (Camera2d.room and flip), the
 * "flip-screen" of the 8-bit computers (and of Zelda's dungeons, see
 * plan_games.md section 12), which could redraw a screen but not scroll
 * one. Dying sends Rick back to the door he came in by.
 *
 * What it uses: the platformer kit (kits/platformer/: Tile_move, for
 * Rick, the natives and the boulder against the rock, one pixel at a
 * time; Ladder, climbing, and ladders' tops as floors, [on_top] only:
 * jumping across a ladder, Rick isn't caught by it), the shoot 'em up
 * kit's Shots (kits/shmup/: Rick's bullets and the darts), Tilemap (the
 * temple, changed by the treasures taken, the walls blown, the traps
 * sprung), Camera2d (a room at a time: room, flip), Sprite (Rick and the natives),
 * Scene2d. Not Physics: the jump is TinyMario's two lines, and the
 * boulder only rolls.
 *
 * Exercises: more rooms (the original's first level has about 20), the
 * spikes that come out of the floor as you pass, a native who throws
 * spears, Rick's poke (the stick he pushes traps with), the boulder
 * rolling down stairs.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The temple *)
(*****************************************************************************)

(* two rooms of 20 x 16 tiles, side by side: # rock, b a wall dynamite
 * breaks, H a ladder, ^ spikes, t a trigger (invisible) and d the dart
 * shooter of its row, * a treasure, a ammunition, E the exit; R Rick, O
 * the boulder, N natives *)
let solo_map =
  [ "########################################";
    "#                  #####################";
    "#                  #####################";
    "# O   R     *     ######################";
    "################ #######################";
    "#                 ######################";
    "#   H             ######################";
    "#   H             ######################";
    "#   H   N  ^^     ######################";
    "####H###################################";
    "#   H              #####################";
    "#   H              ##    b             #";
    "#   H                    b             #";
    "#   H    a     N         b  t  N  *   Ed";
    "########################################";
    "########################################" ]

let tile = 50.
let level = Tilemap.of_strings tile solo_map
let solid (c : char) : bool = c = '#' || c = 'b' || c = 'd'
let is_ladder (c : char) : bool = c = 'H'
let rick_size = (30., 44.)
let native_size = (30., 44.)
let boulder_size = (90., 90.)

let places (c : char) : (number * number) list = List.map (fun (col, row) -> Tilemap.center level col row) (Tilemap.find level c)

(* the temple to play: the people and the boulder are not tiles *)
let start_map : Tilemap.t =
  List.fold_left (fun m (col, row) -> Tilemap.set m col row ' ') level (List.concat_map (Tilemap.find level) [ 'R'; 'O'; 'N' ])

(* standing on the floor of the tile at (x, y), a body [h] high *)
let on_floor (h : number) ((x, y) : number * number) : number * number = (x, y - (tile / 2.) + (h / 2.))

(* the rooms: 20 x 16 tiles, side by side *)
let room_size = (1000., 800.)
let room_of (x : number) (y : number) : int * int = Camera2d.room (Tilemap.bounds level) room_size x y

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type rick = {
  x : number;
  y : number;
  vy : number;
  climbing : bool;
  facing : number; (* 1. right, -1. left *)
  steps : int;
  dead : int; (* frames since he died, 0 if alive *)
}

type native = { nx : number; ny : number; dir : number }
type boulder = { bx : number; by : number; spin : number; rolling : bool }

type game = {
  map : Tilemap.t;
  rick : rick;
  natives : native list;
  boulder : boulder;
  bullets : Shots.t list;
  darts : Shots.t list;
  sticks : (number * number * int) list; (* dynamite: where, frames before it blows up *)
  blasts : (number * number * int) list; (* where, frames since *)
  ammo : int;
  dynamite : int;
  score : int;
  lives : int;
  checkpoint : number * number; (* where he comes back: the door of the room *)
  room : int * int;
  frames : int;
}

type scene = Title | Playing of game | Escaped of int | Game_over of int
type model = scene Scene2d.t

let start = on_floor (snd rick_size) (List.hd (places 'R'))
let new_rick ((x, y) : number * number) : rick = { x; y; vy = 0.; climbing = false; facing = 1.; steps = 0; dead = 0 }

let new_boulder () : boulder =
  let x, y = on_floor (snd boulder_size) (List.hd (places 'O')) in
  { bx = x; by = y; spin = 0.; rolling = true }

let new_game () : game =
  { map = start_map; rick = new_rick start; natives = List.map (fun p -> let nx, ny = on_floor (snd native_size) p in { nx; ny; dir = -1. }) (places 'N');
    boulder = new_boulder (); bullets = []; darts = []; sticks = []; blasts = []; ammo = 6; dynamite = 6; score = 0; lives = 6;
    checkpoint = start; room = room_of (fst start) (snd start); frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Rick *)
(*****************************************************************************)

let run_speed = 4.
let jump_speed = 13. (* up to about 2 tiles *)
let gravity = 0.8

let supported (map : Tilemap.t) (x : number) (y : number) : bool =
  Tile_move.on_ground solid map rick_size x y || Ladder.on_top is_ladder map rick_size x y

(* falling or rising by [vy]: stopped by the rock, and, going down, by
 * the top of a ladder (Tile_move.move_by would fall through it: a
 * ladder isn't solid) *)
let rise_or_fall (map : Tilemap.t) ((x, y) : number * number) (vy : number) : (number * number) * bool =
  if vy >= 0. then Tile_move.move_by solid map rick_size (x, y) (0., vy)
  else
    let n = int_of_float (ceil (Float.abs vy)) in
    let rec go i y =
      if i >= n then (y, false)
      else
        let y' = y + (vy / float_of_int n) in
        if Tile_move.hits solid map rick_size x y' then (y, true) else if Ladder.on_top is_ladder map rick_size x y' then (y', true) else go (succ i) y'
    in
    let y, landed = go 0 y in
    ((x, y), landed)

(* [step_rick map keys jump r]: on a ladder, up and down, and off it
 * sideways; otherwise running, jumping (up, not at a ladder), falling,
 * and onto a ladder with up (at one) or down (on its top) *)
let step_rick (map : Tilemap.t) (keys : keyboard) (jump : bool) (r : rick) : rick =
  let dir = to_x keys in
  let facing = if dir <> 0. then dir else r.facing in
  let steps = if dir <> 0. then r.steps +.. 1 else r.steps in
  let at_ladder = Ladder.reach is_ladder map rick_size r.x r.y <> None in
  if r.climbing && dir = 0. && at_ladder then
    let x, y = Ladder.climb solid is_ladder map rick_size (r.x, r.y) (3. * to_y keys) in
    { r with x; y; vy = 0.; steps = (if to_y keys <> 0. then r.steps +.. 1 else r.steps) }
  else if (keys.kup && Ladder.reach is_ladder map rick_size r.x r.y <> None && not (Ladder.on_top is_ladder map rick_size r.x r.y))
          || (keys.kdown && Ladder.on_top is_ladder map rick_size r.x r.y)
  then { r with climbing = true; x = Option.get (Ladder.reach is_ladder map rick_size r.x r.y) }
  else
    let (x, _), _ = Tile_move.move_by solid map rick_size (r.x, r.y) (dir * run_speed, 0.) in
    let on = supported map x r.y in
    let vy = if jump && on then jump_speed else if on && r.vy <= 0. then 0. else Float.max (-15.) (r.vy - gravity) in
    let (x, y), hit = rise_or_fall map (x, r.y) vy in
    { r with x; y; vy = (if hit then 0. else vy); climbing = false; facing; steps }

(*****************************************************************************)
(* The others *)
(*****************************************************************************)

(* a native walks, and turns at a wall or at the edge of its floor *)
let step_native (map : Tilemap.t) (n : native) : native =
  let (x, _), hit = Tile_move.move_by solid map native_size (n.nx, n.ny) (n.dir * 2., 0.) in
  let edge = not (Tile_move.on_ground solid map native_size (x + (n.dir * 16.)) n.ny) in
  if hit || edge then { n with dir = -.n.dir } else { n with nx = x }

(* the boulder rolls right, falls, and stops against a wall *)
let step_boulder (map : Tilemap.t) (b : boulder) : boulder =
  if not b.rolling then b
  else
    let (x, _), hit = Tile_move.move_by solid map boulder_size (b.bx, b.by) (4.5, 0.) in
    let (x, y), _ = Tile_move.move_by solid map boulder_size (x, b.by) (0., -8.) in
    { bx = x; by = y; spin = b.spin - 6.; rolling = not hit }

(* the tile under Rick's center, and under his feet *)
let tile_under (map : Tilemap.t) (r : rick) : char list =
  List.filter_map (fun dy -> Tilemap.tile_at map r.x (r.y + dy)) [ 0.; -.(snd rick_size / 2.) + 2. ]

(* his feet in the spikes: in a '^' tile, and down among the spikes, at
 * the bottom 18 pixels of the tile (a jump over them leaves the tile's
 * upper part at once, but his feet are still in the tile) *)
let on_spikes (map : Tilemap.t) (r : rick) : bool =
  let feet = r.y - (snd rick_size / 2.) in
  let col, row = Tilemap.cell map r.x (feet + 1.) in
  Tilemap.get map col row = Some '^' && feet - (snd (Tilemap.center map col row) - (tile / 2.)) < 18.

(* a trigger stepped on: the dart shooter of its row fires, at Rick, and
 * the trigger is spent *)
let spring_traps (g : game) : game =
  let col, row = Tilemap.cell g.map g.rick.x g.rick.y in
  if Tilemap.get g.map col row <> Some 't' then g
  else
    let shooters = List.filter (fun (_, r) -> r = row) (Tilemap.find g.map 'd') in
    let darts =
      List.map
        (fun (c, r) ->
          let x, _ = Tilemap.center g.map c r in
          let side = if g.rick.x < x then -1. else 1. in
          Shots.straight (x + (side * tile / 2.)) g.rick.y (side * 10.) 0.)
        shooters
    in
    { g with map = Tilemap.set g.map col row '.'; darts = darts @ g.darts }

(* dynamite blowing up: the breakable walls around, the natives, and Rick
 * if he's still there *)
let blast_radius = 90.

let explode (g : game) ((x, y) : number * number) : game =
  let near px py = Float.hypot (px - x) (py - y) < blast_radius in
  let walls = List.filter (fun (c, r) -> let cx, cy = Tilemap.center g.map c r in near cx cy) (Tilemap.find g.map 'b') in
  { g with map = List.fold_left (fun m (c, r) -> Tilemap.set m c r ' ') g.map walls; natives = List.filter (fun n -> not (near n.nx n.ny)) g.natives;
    rick = (if g.rick.dead = 0 && near g.rick.x g.rick.y then { g.rick with dead = 1 } else g.rick); blasts = (x, y, 0) :: g.blasts }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let flying (map : Tilemap.t) (s : Shots.t) : bool = match Tilemap.tile_at map s.x s.y with Some c -> not (solid c) | None -> false

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let g = { g with frames = g.frames +.. 1; blasts = List.filter_map (fun (x, y, n) -> if n < 30 then Some (x, y, n +.. 1) else None) g.blasts } in
  let pressed k = Scene2d.pressed k scenes in
  let g =
    if g.rick.dead > 0 then
      (* back at the door, the boulder too if it's the first room's *)
      if g.rick.dead < 60 then { g with rick = { g.rick with dead = g.rick.dead +.. 1 } }
      else { g with rick = new_rick g.checkpoint; lives = g.lives -.. 1; darts = []; bullets = []; boulder = (if g.checkpoint = start then new_boulder () else g.boulder) }
    else
      let rick = step_rick g.map computer.keyboard (pressed (fun k -> k.kup)) g.rick in
      let room = room_of rick.x rick.y in
      let g = { g with rick; room; checkpoint = (if room <> g.room then (rick.x, rick.y) else g.checkpoint) } in
      (* treasures, ammunition *)
      let col, row = Tilemap.cell g.map rick.x rick.y in
      let g =
        match Tilemap.get g.map col row with
        | Some '*' -> { g with map = Tilemap.set g.map col row ' '; score = g.score +.. 1000 }
        | Some 'a' -> { g with map = Tilemap.set g.map col row ' '; ammo = g.ammo +.. 6 }
        | _ -> g
      in
      let g = spring_traps g in
      (* shooting, dropping dynamite *)
      let g =
        if pressed (fun k -> k.kspace) && g.ammo > 0 then
          { g with ammo = g.ammo -.. 1; bullets = Shots.straight (rick.x + (rick.facing * 20.)) (rick.y + 6.) (rick.facing * 14.) 0. :: g.bullets }
        else g
      in
      if pressed (fun k -> Set_.mem "x" k.keys) && g.dynamite > 0 && supported g.map rick.x rick.y then
        { g with dynamite = g.dynamite -.. 1; sticks = (rick.x, rick.y - 10., 90) :: g.sticks }
      else g
  in
  (* the bullets and darts, the natives, the boulder *)
  let bullets = List.filter (flying g.map) (List.map Shots.advance g.bullets) in
  let shot n = List.exists (Shots.near 25. (n.nx, n.ny)) bullets in
  let g =
    { g with bullets = List.filter (fun b -> not (List.exists (fun n -> Shots.near 25. (n.nx, n.ny) b) g.natives)) bullets;
      natives = List.map (step_native g.map) (List.filter (fun n -> not (shot n)) g.natives);
      score = g.score +.. (100 *.. List.length (List.filter shot g.natives));
      darts = List.filter (flying g.map) (List.map Shots.advance g.darts); boulder = step_boulder g.map g.boulder }
  in
  let g = List.fold_left (fun g (x, y, n) -> if n <= 0 then explode g (x, y) else g) { g with sticks = List.filter_map (fun (x, y, n) -> if n <= 0 then None else Some (x, y, n -.. 1)) g.sticks } (List.filter (fun (_, _, n) -> n <= 0) g.sticks) in
  (* what kills Rick *)
  let r = g.rick in
  let deadly =
    r.dead = 0
    && (on_spikes g.map r
       || List.exists (Shots.near 22. (r.x, r.y)) g.darts
       || List.exists (fun n -> Float.hypot (n.nx - r.x) (n.ny - r.y) < 30.) g.natives
       || Float.hypot (g.boulder.bx - r.x) (g.boulder.by - r.y) < 60.)
  in
  if deadly then { g with rick = { r with dead = 1 } } else g

let escaped (g : game) : bool = g.rick.dead = 0 && List.mem 'E' (tile_under g.map g.rick)

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if escaped g then Scene2d.go (Escaped g.score) s
      else if g.lives = 0 then Scene2d.go (Game_over g.score) s
      else { s with scene = Playing g }
  | Escaped _ | Game_over _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let tile_shape (c : char) : shape =
  let rock = rgb 110 80 50 in
  match c with
  | '#' -> group [ square rock tile; square (rgb 125 92 58) (tile - 6.); rectangle rock tile 2. |> move_y 8.; rectangle rock 2. 20. |> move 10. (-6.) ]
  | 'b' -> group [ square (rgb 160 120 80) tile; rectangle (rgb 90 60 40) tile 3.; rectangle (rgb 90 60 40) 3. (tile / 2.) |> move 0. 12.; rectangle (rgb 90 60 40) 3. (tile / 2.) |> move 12. (-12.) ]
  | 'H' -> group [ rectangle (rgb 170 120 60) 4. tile |> move_x (-14.); rectangle (rgb 170 120 60) 4. tile |> move_x 14.; rectangle (rgb 170 120 60) 28. 4. |> move_y (-12.); rectangle (rgb 170 120 60) 28. 4. |> move_y 12. ]
  | '^' -> group (List.init 3 (fun i -> triangle (rgb 200 200 210) 9. |> move ((float_of_int i * 16.) - 16.) (-18.)))
  | 'd' -> group [ square (rgb 120 120 130) tile; circle black 6. |> move (-12.) 2.; rectangle black 12. 4. |> move (-18.) (-12.) ]
  | '*' -> group [ oval (rgb 250 200 40) 22. 30. |> move_y (-8.); circle (rgb 255 240 150) 5. |> move (-4.) (-2.) ]
  | 'a' -> group [ rectangle (rgb 80 100 60) 30. 20. |> move_y (-15.); words white "AMMO" |> scale 0.8 |> move_y (-15.) ]
  | 'E' -> group [ rectangle black 40. tile; oval black 40. 30. |> move_y 20.; words (rgb 255 220 120) "EXIT" |> move_y 30. ]
  | _ -> group []

(* Rick in his fedora, facing right, two frames of a run; a native *)
let rick_rows =
  let top = [ "...KKKK..."; "..KKKKKKK."; "...SSSS..."; "...SSKS..."; "...SSSS..."; "..TTTTTT.."; ".STTTTTTS."; ".STTTTTTS."; "..TTTTTT.."; "..BBBBBB.." ] in
  [ top @ [ "..BB..BB.."; "..BB..BB.."; "..KK..KK.."; ".KKK..KKK." ]; top @ [ "...BBBB..."; "...BBBB..."; "...KKKK..."; "..KKKKK..." ] ]

let native_rows =
  let top = [ "....KK...."; "...KKKK..."; "...NNNN..."; "...NWNN..."; "...NNNN..."; "..NNNNNN.."; ".N.NNNN.N."; ".N.NNNN.N."; "...RRRR..." ] in
  [ top @ [ "...N..N..."; "...N..N..."; "..N....N.."; ".NN....NN." ]; top @ [ "...N..N..."; "...N..N..."; "...N..N..."; "..NN..NN.." ] ]

let palette = [ ('K', rgb 60 40 20); ('S', rgb 240 190 140); ('T', rgb 200 170 110); ('B', rgb 110 70 40); ('N', rgb 140 90 50); ('W', white); ('R', red) ]
let sprites (rows : string list list) (left : bool) : shape list = List.map (fun r -> Sprite.pixels 3. palette (if left then Sprite.flip r else r)) rows

let view_game (g : game) : shape list =
  let r = g.rick in
  let rick =
    if r.dead > 0 then [ Sprite.cycle 0 (sprites rick_rows (r.facing < 0.)) |> rotate (float_of_int r.dead * 15.) |> fade (1. - (float_of_int r.dead / 60.)) |> move r.x r.y ]
    else [ Sprite.cycle (r.steps /.. 6) (sprites rick_rows (r.facing < 0.)) |> move r.x r.y ]
  in
  let world =
    [ Tilemap.view tile_shape g.map ]
    @ List.map (fun n -> Sprite.cycle (g.frames /.. 10) (sprites native_rows (n.dir < 0.)) |> move n.nx n.ny) g.natives
    @ [ group [ circle (rgb 130 120 110) 45.; circle (rgb 150 140 130) 35. |> move (-8.) 8.; rectangle (rgb 100 90 80) 60. 6. ] |> rotate g.boulder.spin |> move g.boulder.bx g.boulder.by ]
    @ List.map (fun (s : Shots.t) -> rectangle yellow 10. 3. |> move s.x s.y) g.bullets
    @ List.map (fun (s : Shots.t) -> rectangle (rgb 220 220 200) 22. 3. |> move s.x s.y) g.darts
    @ List.map (fun (x, y, n) -> group [ rectangle red 8. 20.; rectangle (if n mod 10 < 5 then yellow else white) 2. 6. |> move_y 12. ] |> move x y) g.sticks
    @ List.map (fun (x, y, n) -> circle (if n mod 6 < 3 then orange else yellow) (20. + (float_of_int n * 3.)) |> fade (1. - (float_of_int n / 30.)) |> move x y) g.blasts
    @ rick
  in
  let cam = Camera2d.flip (Tilemap.bounds level) room_size g.rick.x g.rick.y Camera2d.origin in
  [ Camera2d.view cam world;
    text white 2.5 (Printf.sprintf "LIVES %d   BULLETS %d   DYNAMITE %d   SCORE %d" g.lives g.ammo g.dynamite g.score) |> move_y 450. ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_game (new_game ())
      @ [ rectangle black 800. 260. |> fade 0.85 |> move_y 60.; text (rgb 250 200 40) 7. "TINY RICK" |> move_y 140.;
          text white 2.3 "left/right run   up jump or climb   down climb down" |> move_y 70.;
          text white 2.3 "space shoot   x dynamite (then run!)" |> move_y 35. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-20.) ]
  | Playing g -> view_game g
  | Escaped score -> [ text (rgb 250 200 40) 6. "YOU ESCAPED!"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ]
  | Game_over score -> [ text red 6. "GAME OVER"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
