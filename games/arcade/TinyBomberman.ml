(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Bomberman (Hudson Soft, 1983; the NES version, 1985):
 * a grid of pillars and soft blocks, bombs exploding in a cross, and
 * balloons to blow up. Arrows to move, space to drop a bomb. Blow up
 * the balloons, find the exit hidden under a block, and walk into it;
 * another block hides a power-up, a longer fire. Don't stand in the
 * fire, and don't touch the balloons.
 *
 * The second maze game, and the maze kit's second user (gamekits/maze/):
 * the bomber moves like Pac-Man (Grid_move: along the corridors, the
 * turn asked for early remembered), the balloons wander like Pac-Man's
 * blue ghosts (Chase.at_random: at each tile, a random way, but never
 * back). What's new here is the explosion ([explode]): the fire spreads
 * from the bomb in the four directions, tile by tile, up to its range,
 * stopped by a pillar, and by the first soft block, which it destroys;
 * a bomb it reaches explodes too, at once: chain reactions, the game's
 * best moments, in a loop until no bomb is left to catch fire.
 *
 *            #                 the fire of a bomb (o) of range 2:
 *         +  F  #              stopped by the pillar above, destroying
 *      .  F  F  o  F  F  .     the block on the left of its path, and
 *            F                 reaching the bomb on the right, which
 *            F                 explodes in turn
 *
 * The stage is ours, in the classic layout: 15x13, pillars on every
 * even column and row. No randomness: the balloons' random ways come
 * from Pac-Man's own generator (Chase.next_random), so a game can be
 * replayed.
 *
 * Exercises: the power-up for more bombs at once, the remote detonator,
 * the other enemies (faster, walking through blocks), a timer, the
 * multiplayer battle mode that made the series famous (Saturn Bomberman,
 * 1996: ten players), with plan_networking_teaching.md.
 *)
open Playground

(*****************************************************************************)
(* The stage *)
(*****************************************************************************)

(* '#' a pillar or a wall, '+' a soft block, and two blocks hiding
 * something: 'E' the exit, 'F' a fire power-up (once revealed: 'e',
 * 'f'); 'P' where the bomber starts, 'B' the balloons *)
let stage_rows =
  [ "###############";
    "#P  ++ + ++   #";
    "# #+# #+#+#+# #";
    "#  ++   +  + +#";
    "#+# # #+# #+#+#";
    "#+ +  +++ + + #";
    "# #+#+# # #+#+#";
    "#   + +  E+   #";
    "#+# #+# #+# # #";
    "# ++ + +F +B+ #";
    "# # # #+#+# #+#";
    "#   +  B +  +B#";
    "###############" ]

let t = 60 (* a tile, in pixels *)
let stage = Tilemap.of_strings (float_of_int t) stage_rows
let grid : Grid_move.grid = { tile = t; cols = Tilemap.cols stage; rows = Tilemap.rows stage }
let bounds = Tilemap.bounds stage

let start = match Tilemap.find stage 'P' with p :: _ -> p | [] -> (1, 1)

let is_block (c : char option) : bool = c = Some '+' || c = Some 'E' || c = Some 'F'
let is_floor (c : char option) : bool = c = Some ' ' || c = Some 'e' || c = Some 'f'

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type bomb = { col : int; row : int; timer : int }

type game = {
  map : Tilemap.t; (* the blocks left, and what they revealed *)
  bomber : Grid_move.mover;
  bombs : bomb list;
  fire : ((int * int) * int) list; (* burning tiles, and for how long *)
  balloons : Grid_move.mover list;
  range : int; (* how far the fire goes *)
  lives : int;
  score : int;
  dying : int; (* > 0: caught, for that long *)
  rng : int;
  frames : int;
}

type scene = Title | Playing of game | Game_over of int | Cleared of int
type model = scene Scene2d.t

(* the stage, with the bomber and the balloons taken out of the map *)
let new_game () : game =
  let map =
    List.fold_left (fun m (c, r) -> Tilemap.set m c r ' ') stage (Tilemap.find stage 'P' @ Tilemap.find stage 'B')
  in
  { map; bomber = Grid_move.mover_at grid start; bombs = []; fire = [];
    balloons = List.map (Grid_move.mover_at grid) (Tilemap.find stage 'B');
    range = 1; lives = 3; score = 0; dying = 0; rng = 1; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Bombs and fire *)
(*****************************************************************************)

let bomb_at (g : game) (cr : int * int) : bool = List.exists (fun b -> (b.col, b.row) = cr) g.bombs

(* the fire of the bomb at (col, row): its tile, and in each direction
 * the tiles up to [range], stopping before a pillar, and at the first
 * block (included: it burns) *)
let fire_of (g : game) (col, row) : (int * int) list =
  let rec spread (dc, dr) n acc =
    if n > g.range then acc
    else
      let cr = (col + (dc * n), row + (dr * n)) in
      match Tilemap.get g.map (fst cr) (snd cr) with
      | c when is_block c -> cr :: acc
      | c when is_floor c -> spread (dc, dr) (n + 1) (cr :: acc)
      | _ -> acc
  in
  (col, row) :: List.concat_map (fun d -> spread d 1 []) [ (0, -1); (0, 1); (-1, 0); (1, 0) ]

(* Explosions: every bomb whose timer is out explodes; its fire sets off
 * the bombs it reaches, which explode in the same frame, and so on
 * (chain reactions); the blocks in the fire burn, revealing what they
 * hid *)
let rec explode (g : game) : game =
  match List.partition (fun b -> b.timer <= 0) g.bombs with
  | [], _ -> g
  | exploding, rest ->
      let tiles = List.concat_map (fun b -> fire_of g (b.col, b.row)) exploding in
      let bombs = List.map (fun b -> if List.mem (b.col, b.row) tiles then { b with timer = 0 } else b) rest in
      let burn m (c, r) =
        match Tilemap.get m c r with
        | Some '+' -> Tilemap.set m c r ' '
        | Some 'E' -> Tilemap.set m c r 'e'
        | Some 'F' -> Tilemap.set m c r 'f'
        | _ -> m
      in
      let score = g.score + (10 * List.length (List.filter (fun (c, r) -> is_block (Tilemap.get g.map c r)) tiles)) in
      explode { g with bombs; map = List.fold_left burn g.map tiles; fire = List.map (fun cr -> (cr, 30)) tiles @ g.fire; score }

let burning (g : game) (cr : int * int) : bool = List.exists (fun (f, _) -> f = cr) g.fire

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the bomber can walk on the floor, but not onto a bomb -- except the
 * one just dropped under him, to walk away from it *)
let bomber_open (g : game) (cr : int * int) : bool =
  is_floor (Tilemap.get g.map (fst cr) (snd cr)) && not (bomb_at g cr)

let balloon_open (g : game) (cr : int * int) : bool =
  is_floor (Tilemap.get g.map (fst cr) (snd cr)) && not (bomb_at g cr)

(* the arrow held (one is) *)
let wanted (k : keyboard) : Grid_move.dir =
  if k.kup then Up else if k.kdown then Down else if k.kleft then Left else Right

let update_game (s : model) (computer : computer) (g : game) : game =
  let g = { g with frames = g.frames + 1; rng = Chase.next_random g.rng } in
  if g.dying > 0 then
    if g.dying > 1 then { g with dying = g.dying - 1 }
    else
      (* back to the start, the blocks as they are *)
      { g with dying = 0; lives = g.lives - 1; bomber = Grid_move.mover_at grid start; bombs = []; fire = [] }
  else
    let k = computer.keyboard in
    (* claude: with no arrow held, the bomber stops at the next center
     * (unlike Pac-Man, who runs on) *)
    let held = k.kup || k.kdown || k.kleft || k.kright in
    let b = g.bomber in
    let b = if held then { b with wanted = wanted k } else { b with wanted = Stop } in
    let b =
      if held then Grid_move.move_player grid ~open_:(fun cr -> bomber_open g cr || cr = Grid_move.tile_of grid g.bomber) 3 b
      else if Grid_move.at_center grid b then { b with dir = Stop }
      else Grid_move.slide grid ~choose:(fun m -> { m with dir = Stop }) 3 b
    in
    let here = Grid_move.tile_of grid b in
    let g = { g with bomber = b } in
    (* a bomb where the bomber stands, two at most at once (the
     * original starts with one, and a power-up adds more): with two,
     * chain reactions *)
    let g =
      if Scene2d.pressed (fun k -> k.kspace) s && List.length g.bombs < 2 && not (bomb_at g here) then
        { g with bombs = { col = fst here; row = snd here; timer = 150 } :: g.bombs }
      else g
    in
    (* the power-up *)
    let g =
      if Tilemap.get g.map (fst here) (snd here) = Some 'f' then
        { g with range = g.range + 1; map = Tilemap.set g.map (fst here) (snd here) ' ' }
      else g
    in
    let g = { g with bombs = List.map (fun b -> { b with timer = b.timer - 1 }) g.bombs } |> explode in
    let g = { g with fire = List.filter_map (fun (cr, n) -> if n > 1 then Some (cr, n - 1) else None) g.fire } in
    let balloons =
      List.mapi (fun i m -> Grid_move.slide grid ~choose:(Chase.at_random grid ~open_:(balloon_open g) (g.rng + (i * 7))) 2 m) g.balloons
    in
    (* the fire gets the balloons in it *)
    let dead, balloons = List.partition (fun m -> burning g (Grid_move.tile_of grid m)) balloons in
    let g = { g with balloons; score = g.score + (100 * List.length dead) } in
    let caught = burning g here || List.exists (fun m -> Grid_move.tile_of grid m = here) g.balloons in
    if caught then { g with dying = 90 } else g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game s computer g in
      let col, row = Grid_move.tile_of grid g.bomber in
      if g.lives = 0 then Scene2d.go (Game_over g.score) s
      else if g.balloons = [] && Tilemap.get g.map col row = Some 'e' && g.dying = 0 then Scene2d.go (Cleared g.score) s
      else { s with scene = Playing g }
  | Game_over _ | Cleared _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let block_art =
  [ "############"; "#.....#....."; "#.....#....."; "############"; "...#.....#.."; "...#.....#..";
    "############"; "#.....#....."; "#.....#....."; "############"; "...#.....#.."; "...#.....#.." ]

let block = Sprite.pixels 5. [ ('#', rgb 120 70 40); ('.', rgb 190 120 70) ] block_art

let tile (c : char) : shape =
  match c with
  | '#' -> group [ square (rgb 110 110 120) 60.; square (rgb 160 160 170) 48. ]
  | '+' | 'E' | 'F' -> block
  | 'e' -> group [ square (rgb 60 60 60) 50.; rectangle (rgb 20 20 20) 30. 44. |> move_y (-3.) ]
  | 'f' -> group [ square (rgb 250 120 30) 44.; triangle yellow 16. |> rotate 90. ]
  | _ -> group []

(* the bomber: white helmet and suit, pink face; two frames when walking *)
let bomber_frames =
  let top = [ "...WWWW..."; "..WWWWWW.."; "..WPPPPW.."; "..WPKPKW.."; "...PPPP..."; ".BBWWWWBB." ] in
  List.map
    (Sprite.pixels 5. [ ('W', white); ('P', rgb 250 180 160); ('K', black); ('B', rgb 60 90 220) ])
    [ top @ [ ".B.WWWW.B."; "...BBBB..."; "..PP..PP.." ]; top @ [ ".B.WWWW.B."; "...BBBB..."; "...PP.PP.." ] ]

let balloon =
  Sprite.pixels 5. [ ('O', rgb 250 140 60); ('W', white); ('K', black) ]
    [ "...OOOO..."; ".OOOOOOOO."; "OOWWOOWWOO"; "OOWKOOWKOO"; "OOOOOOOOOO"; ".OOOOOOOO."; "..OO..OO.."; ".O..OO..O." ]

let at (m : Grid_move.mover) (shape : shape) : shape =
  let x, y = Grid_move.to_world grid bounds m in
  shape |> move x y

let cell ((col, row) : int * int) (shape : shape) : shape =
  let x, y = Tilemap.center stage col row in
  shape |> move x y

let text color size str = words color str |> scale size

let view_game (g : game) : shape list =
  let bomber =
    if g.dying > 0 then (if g.frames / 6 mod 2 = 0 then List.hd bomber_frames else group []) |> rotate (float_of_int g.dying *. 8.)
    else Sprite.cycle (if g.bomber.dir = Stop then 0 else g.frames / 8) bomber_frames
  in
  [ rectangle (rgb 60 140 60) (bounds.right -. bounds.left) (bounds.top -. bounds.bottom); Tilemap.view tile g.map ]
  @ List.map
      (fun b ->
        let pulse = 1. +. (0.08 *. sin (float_of_int b.timer /. 4.)) in
        cell (b.col, b.row) (group [ circle black 22.; rectangle (rgb 250 200 40) 4. 12. |> move 10. 22. ] |> scale pulse))
      g.bombs
  @ List.map (fun (cr, _) -> cell cr (group [ square (rgb 250 110 20) 60.; square (rgb 255 220 60) 34. ])) g.fire
  @ List.map (fun m -> at m balloon) g.balloons
  @ [ at g.bomber bomber;
      text white 3. (Printf.sprintf "LIVES %d   SCORE %d   FIRE %d" g.lives g.score g.range) |> move_y (bounds.top +. 40.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 30 30 40) screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      [ text white 7. "TINY BOMBERMAN" |> move_y 250.;
        List.hd bomber_frames |> scale 2. |> move (-60.) 60.; balloon |> scale 2. |> move 60. 60.;
        text gray 2.5 "arrows: move   space: bomb" |> move_y (-100.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-200.) ]
  | Playing g -> view_game g
  | Game_over score ->
      [ text red 7. "GAME OVER"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-100.) ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-200.) ]
  | Cleared score ->
      [ text yellow 7. "STAGE CLEAR!"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-100.) ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-200.) ])

let app = game view update initial_model

let main = Playground_platform.run_app app
