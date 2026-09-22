(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Lode Runner (Doug Smith, Broderbund, 1983): take all
 * the gold, then the escape ladder appears; climb it to the top. You
 * can't jump, and you can't fight: you dig. Arrows to run and climb (and
 * down to let go of a bar), z to dig a hole on your left, x on your
 * right. Guards chase you; one that falls in a hole is stuck there for
 * a while (walk over its head), and the brick grows back: whatever is
 * in the hole then dies -- a guard comes back from the top, you lose a
 * life.
 *
 * Doug Smith wrote it as a student, on a VAX then an Apple II; it came
 * with a level editor, one of the first, and its players made more
 * levels than its 150. Its digging made it a puzzle game as much as an
 * action one: a hole is a trap, a stair, and a clock. (Names and dates
 * from memory, to check.)
 *
 * The new idea here is a map that changes: a dug brick is gone from the
 * Tilemap (a new map, the model's, not the level's), and remembered
 * with its timer ([holes]) until it grows back. And the guards' pursuit
 * ([guard_wants]): on the player's row, run at them; otherwise, take the
 * nearest ladder going their way, up or down; otherwise, run towards
 * them anyway, dropping off edges -- no pathfinding, as dumb as the
 * original's (and still dangerous, since there are several).
 *
 * What it uses: the platformer kit (gamekits/platformer/: Tile_move, the
 * bodies against the bricks, one pixel at a time; Ladder, climbing, and
 * the ladders' tops as floors), Tilemap (the level, and its holes),
 * Sprite (the runner and the guards), Scene2d (title, play, cleared,
 * game over). Not Camera2d: one screen, as the original. Not Physics:
 * falls are at a constant speed, as on the Apple II.
 *
 * Exercises: the guards carrying gold (and dropping it), a guard's
 * respawn anywhere on the top row, more levels (a level editor!), the
 * guards' smarter pursuit (a breadth-first search on the ladders and
 * floors, see TinySokoban's solver).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The level *)
(*****************************************************************************)

(* # a brick (diggable), @ concrete, H a ladder, - a bar, $ gold, P the
 * player, G a guard, S the escape ladder, hidden until the gold is all
 * taken *)
let level_rows =
  [ "                       S";
    "  $          G         S";
    "###H#####--------###H##S";
    "   H                H  S";
    "   H   $       $    H  S";
    "#####H#######H######H##H";
    "     H       H         H";
    "  $  H   G   H    $    H";
    "@@@######H####@@@###H##H";
    "         H          H   ";
    "    $    H     G    H $ ";
    "#####H###H##########H###";
    "  P  H         $        ";
    "@@@@@@@@@@@@@@@@@@@@@@@@" ]

let tile = 40.
let level = Tilemap.of_strings tile level_rows
let solid (c : char) : bool = c = '#' || c = '@'
let is_ladder (c : char) : bool = c = 'H'
let size = (28., 34.) (* a runner, smaller than a tile *)

let places (c : char) : (number * number) list = List.map (fun (col, row) -> Tilemap.center level col row) (Tilemap.find level c)

(* the level to play: the runners are not tiles, and S is empty *)
let start_map : Tilemap.t =
  List.fold_left (fun m (col, row) -> Tilemap.set m col row ' ') level (List.concat_map (Tilemap.find level) [ 'P'; 'G'; 'S' ])

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type runner = { x : number; y : number; trapped : int (* frames in a hole, 0 if not *); home : number * number }

type game = {
  map : Tilemap.t;
  player : runner;
  guards : runner list;
  holes : (int * int * int) list; (* dug bricks: column, row, frames before they grow back *)
  gold : int; (* taken *)
  lives : int;
  dead : int; (* frames since the player died, 0 if alive *)
  frames : int;
}

type scene = Title | Playing of game | Cleared of int | Game_over
type model = scene Scene2d.t

let total_gold = List.length (places '$')
let runner_at ((x, y) : number * number) : runner = { x; y; trapped = 0; home = (x, y) }

let new_game (lives : int) : game =
  { map = start_map; player = runner_at (List.hd (places 'P')); guards = List.map runner_at (places 'G'); holes = []; gold = 0; lives; dead = 0; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Running, climbing, hanging, falling *)
(*****************************************************************************)

type want = Left | Right | Up | Down | Stay

let player_speed = 3.
let guard_speed = 2.
let fall_speed = 5.

let cell_center (map : Tilemap.t) (x : number) (y : number) : number * number =
  let col, row = Tilemap.cell map x y in
  Tilemap.center map col row

let on_bar (map : Tilemap.t) (x : number) (y : number) : bool = Tilemap.tile_at map x y = Some '-'

(* the height of a runner on the row whose tile's center is at [cy]:
 * standing on the floor below, its center 3 pixels under the tile's
 * (the tile 40 high, the runner 34); hanging from a bar, at it *)
let row_y (map : Tilemap.t) (x : number) (cy : number) : number = if on_bar map x cy then cy else cy - (tile / 2.) + (snd size / 2.)

(* [step_runner map speed held want (x, y)]: one frame of a runner, player or
 * guard. Nothing holding it up (a floor, a ladder, a bar, or [held], a
 * guard's head): it falls, straight down its column. Otherwise it does
 * what it wants: climbs, lets go of a bar (down), or runs sideways,
 * once on a row (a runner between two rows, on a ladder, gets there
 * first: Lode Runner's grid, with smooth moves). *)
let step_runner (map : Tilemap.t) (speed : number) (held : bool) (want : want) ((x, y) : number * number) : number * number =
  let cx, cy = cell_center map x y in
  if not (held || Ladder.standing solid is_ladder map size x y || on_bar map x y) then fst (Tile_move.move_by solid map size (cx, y) (0., -.fall_speed))
  else
    let y = if on_bar map x y then cy else y in
    let ry = row_y map x cy in
    match want with
    | Up -> Ladder.climb solid is_ladder map size (x, y) speed
    | Down when on_bar map x y && Ladder.reach is_ladder map size x y = None -> (x, y - (tile / 2.) - 1.)
    | Down -> Ladder.climb solid is_ladder map size (x, y) (-.speed)
    | Left | Right when Float.abs (y - ry) > speed -> (x, y + if ry > y then speed else -.speed)
    | Left -> fst (Tile_move.move_by solid map size (x, ry) (-.speed, 0.))
    | Right -> fst (Tile_move.move_by solid map size (x, ry) (speed, 0.))
    | Stay -> (x, y)

(*****************************************************************************)
(* Digging *)
(*****************************************************************************)

let regrow = 300 (* frames: 5 s *)

(* [dig g side]: the brick below the player on that side (-1 left, 1
 * right), if there's one with nothing above it, is gone for a while *)
let dig (g : game) (side : int) : game =
  let col, row = Tilemap.cell g.map g.player.x g.player.y in
  let col = col +.. side in
  match (Tilemap.get g.map col (row +.. 1), Tilemap.get g.map col row) with
  | Some '#', Some ' ' -> { g with map = Tilemap.set g.map col (row +.. 1) ' '; holes = (col, row +.. 1, regrow) :: g.holes }
  | _ -> g

let in_hole (g : game) (r : runner) : (int * int) option =
  let col, row = Tilemap.cell g.map r.x r.y in
  if List.exists (fun (c, rw, _) -> c = col && rw = row) g.holes then Some (col, row) else None

(* the holes growing back: a brick again, whoever is in it dies *)
let regrow_holes (g : game) : game =
  let holes = List.map (fun (c, r, n) -> (c, r, n -.. 1)) g.holes in
  let grown = List.filter (fun (_, _, n) -> n <= 0) holes in
  let map = List.fold_left (fun m (c, r, _) -> Tilemap.set m c r '#') g.map grown in
  let crushed (r : runner) = List.exists (fun (c, rw, _) -> Tilemap.cell g.map r.x r.y = (c, rw)) grown in
  { g with map; holes = List.filter (fun (_, _, n) -> n > 0) holes;
    guards = List.map (fun r -> if crushed r then runner_at r.home else r) g.guards;
    dead = (if g.dead = 0 && crushed g.player then 1 else g.dead) }

(*****************************************************************************)
(* The guards *)
(*****************************************************************************)

(* [guard_wants g r]: on the player's row, towards them; else to the
 * nearest ladder of this row going their way (up: a ladder on the row;
 * down: one just below), then along it; else towards them, falling off
 * whatever edge is on the way *)
let guard_wants (g : game) (r : runner) : want =
  let p = g.player in
  let towards x = if x < r.x - 2. then Left else if x > r.x + 2. then Right else Stay in
  if Float.abs (p.y - r.y) < tile / 2. then towards p.x
  else
    let col, row = Tilemap.cell g.map r.x r.y in
    let up = p.y > r.y in
    let ladder c = match Tilemap.get g.map c (if up then row else row +.. 1) with Some 'H' -> true | _ -> false in
    let cols = List.filter ladder (List.init (Tilemap.cols g.map) Fun.id) in
    match List.sort (fun a b -> compare (abs (a -.. col)) (abs (b -.. col))) cols with
    | c :: _ when c = col -> if up then Up else Down
    | c :: _ -> towards (fst (Tilemap.center g.map c row))
    | [] -> towards p.x

(* a guard: falling in a hole, stuck in it (the player would fall
 * through), for 2 s, then out of it, up and sideways (onto the floor
 * next to it); otherwise chasing *)
let step_guard (g : game) (r : runner) : runner =
  match in_hole g r with
  | Some (col, row) ->
      let x, cy = Tilemap.center g.map col row in
      if r.trapped < 120 then { r with x; y = row_y g.map x cy; trapped = r.trapped +.. 1 }
      else
        let side = if g.player.x < r.x then -1 else 1 in
        let x, y = Tilemap.center g.map (col +.. side) (row -.. 1) in
        { r with x; y; trapped = 0 }
  | _ ->
      let want = guard_wants g r in
      let x, y = step_runner g.map guard_speed false want (r.x, r.y) in
      (* stuck on a ladder going nowhere: run at the player instead *)
      let x, y = if (x, y) = (r.x, r.y) && (want = Up || want = Down) then step_runner g.map guard_speed false (if g.player.x < r.x then Left else Right) (r.x, r.y) else (x, y) in
      { r with x; y; trapped = 0 }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let g = { g with frames = g.frames +.. 1 } in
  if g.dead > 0 then { g with dead = g.dead +.. 1 }
  else
    let k = computer.keyboard in
    let want = if k.kleft then Left else if k.kright then Right else if k.kup then Up else if k.kdown then Down else Stay in
    (* standing on a trapped guard's head *)
    let p = g.player in
    let held = List.exists (fun r -> r.trapped > 0 && Float.abs (r.x - p.x) < tile / 2. && p.y - r.y > 0. && p.y - r.y <= tile + 1.) g.guards in
    let x, y = step_runner g.map player_speed held want (p.x, p.y) in
    let g = { g with player = { p with x; y } } in
    let g = if Scene2d.pressed (fun k -> Set_.mem "z" k.keys) scenes then dig g (-1) else if Scene2d.pressed (fun k -> Set_.mem "x" k.keys) scenes then dig g 1 else g in
    (* gold; all of it: the escape ladder *)
    let col, row = Tilemap.cell g.map x y in
    let g = if Tilemap.get g.map col row = Some '$' then { g with map = Tilemap.set g.map col row ' '; gold = g.gold +.. 1 } else g in
    let g =
      if g.gold = total_gold && List.exists (fun (c, r) -> Tilemap.get g.map c r = Some ' ') (Tilemap.find level 'S') then
        { g with map = List.fold_left (fun m (c, r) -> Tilemap.set m c r 'H') g.map (Tilemap.find level 'S') }
      else g
    in
    let g = { g with guards = List.map (step_guard g) g.guards } |> regrow_holes in
    let caught = List.exists (fun r -> r.trapped = 0 && Float.hypot (r.x - g.player.x) (r.y - g.player.y) < 24.) g.guards in
    if caught && g.dead = 0 then { g with dead = 1 } else g

(* on the top row, all the gold taken *)
let escaped (g : game) : bool = g.gold = total_gold && snd (Tilemap.cell g.map g.player.x g.player.y) = 0

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game 3)) s else s
  | Playing g ->
      let g = update_game computer s g in
      if escaped g then Scene2d.go (Cleared g.frames) s
      else if g.dead > 90 then if g.lives > 1 then Scene2d.go (Playing (new_game (g.lives -.. 1))) s else Scene2d.go Game_over s
      else { s with scene = Playing g }
  | Cleared _ | Game_over -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let tile_shape (c : char) : shape =
  match c with
  | '#' -> group [ square (rgb 170 60 40) tile; rectangle (rgb 90 30 20) tile 3. |> move_y 0.; rectangle (rgb 90 30 20) 3. (tile / 2.) |> move 0. 10.; rectangle (rgb 90 30 20) 3. (tile / 2.) |> move 10. (-10.) ]
  | '@' -> group [ square (rgb 140 140 150) tile; square (rgb 110 110 120) (tile - 8.) ]
  | 'H' -> group [ rectangle white 3. tile |> move_x (-12.); rectangle white 3. tile |> move_x 12.; rectangle white 24. 3. |> move_y (-10.); rectangle white 24. 3. |> move_y 10. ]
  | '-' -> rectangle white tile 3. |> move_y 12.
  | '$' -> group [ rectangle (rgb 240 200 40) 22. 14. |> move_y (-8.); rectangle (rgb 255 230 120) 14. 6. |> move_y (-4.) ]
  | _ -> group []

(* the runner and the guards, two frames of a run *)
let runner_rows =
  [ [ "..##.."; "..##.."; ".####."; "#.##.#"; "..##.."; ".#..#."; "#....#" ]; [ "..##.."; "..##.."; ".####."; ".####."; "..##.."; "..##.."; ".#..#." ] ]

let runner (color : color) (frames : int) : shape = Sprite.cycle (frames /.. 8) (List.map (Sprite.pixels 5. [ ('#', color) ]) runner_rows)

let view_game (g : game) : shape list =
  let p = g.player in
  [ Tilemap.view tile_shape g.map ]
  @ List.map (fun (r : runner) -> runner (rgb 240 80 80) (if r.trapped > 0 then 0 else g.frames) |> move r.x r.y) g.guards
  @ [ (if g.dead > 0 then runner white 0 |> rotate (float_of_int g.dead * 12.) |> fade (1. - (float_of_int g.dead / 90.)) else runner white g.frames) |> move p.x p.y;
      text white 2.5 (Printf.sprintf "GOLD %d / %d   LIVES %d" g.gold total_gold g.lives) |> move_y 330. ]
  @ if g.gold = total_gold then [ text yellow 2.5 "THE ESCAPE LADDER! TO THE TOP" |> move_y (-330.) ] else []

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      [ Tilemap.view tile_shape start_map |> fade 0.3; rectangle black 760. 330. |> fade 0.85 |> move_y 40.;
        text (rgb 240 200 40) 7. "TINY LODE RUNNER" |> move_y 150.;
        text white 2.5 "arrows run and climb   z dig left   x dig right" |> move_y 60.;
        text white 2.5 "take all the gold, then climb to the top" |> move_y 20. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-80.) ]
  | Playing g -> view_game g
  | Cleared frames ->
      [ text (rgb 240 200 40) 6. "LEVEL CLEARED"; text white 3. (Printf.sprintf "%.1f s" (float_of_int frames / 60.)) |> move_y (-80.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ]
  | Game_over -> [ text red 6. "GAME OVER" ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-100.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
