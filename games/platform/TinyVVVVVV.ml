(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of VVVVVV (Terry Cavanagh, 2010): Captain Viridian
 * cannot jump. He can only turn gravity upside down, and walk on the
 * ceiling, and turn it back.
 *
 *   left right         walk
 *   space (up, down)   flip gravity -- only standing on something
 *
 * VVVVVV (six Vs: the crew's six names all start with one -- Viridian,
 * Violet, Vitellary, Vermilion, Verdigris, Victoria) is the game people
 * mean by "the one where gravity flips". It wasn't the first: Irem's
 * Metal Storm (NES, 1991) let its robot walk on the ceiling. But
 * VVVVVV built a whole game on that one move and nothing else, a world
 * of a few hundred single-screen rooms, each with a pun for a name, and
 * its source was published for its tenth anniversary. (Names and dates
 * from memory, to check.)
 *
 * The lesson is what happens to a platformer when the jump is taken
 * away and one bit replaces it:
 *
 *   - a jump is an arc whose height the player chooses (see
 *     TinyCeleste.ml, where the button held says how high); a flip has
 *     no height to choose: it goes all the way across the room, until
 *     something stops it. The player chooses *when*, and how far to
 *     drift sideways on the way; the room chooses the rest;
 *   - and it can't be undone in the air: a flip is allowed only from a
 *     surface ([standing]). Once you let go of the floor you are
 *     committed, and the rooms are puzzles about that commitment;
 *   - every surface is a floor. So the spikes are on both sides ('^'
 *     and 'v'), and the ceiling is where half the level is;
 *   - gravity lines ('-') flip you whether you like it or not, so the
 *     room can take the one decision away from you too (and they save
 *     you, over a pit);
 *   - and some rooms wrap ([wraps]): fall off the bottom, come back in
 *     at the top, which makes a ledge in mid-air reachable by falling
 *     past it (VVVVVV's own "Veni, Vidi, Vici!" is the famous one).
 *
 * All of it is one sign: gravity is [falling p], -1 or 1, and the rest
 * of the code does not know which way is down. Viridian upside down is
 * his picture's rows in reverse order (List.rev: Sprite.flip mirrors
 * left-right, reversing the rows mirrors top-bottom).
 *
 * Death is cheap, as in TinyCeleste: touch a spike and you are back at
 * the last checkpoint ('C') in half a second, with the gravity you had
 * there. VVVVVV's checkpoints are everywhere, and that is what makes its
 * hard rooms fair.
 *
 * What it uses: gamekits/platformer's Tile_move (x then y, a pixel at a
 * time; it does not care which way gravity points, so the only thing
 * written here is [standing], the ground test on the side gravity pulls
 * to, where Tile_move.on_ground only looks down), Tilemap (the rooms, as
 * strings), Scene2d, Sprite (Viridian, pixel art as VVVVVV's is; his
 * frown when he dies is two rows swapped; artwork=shapes draws him as a
 * circle and a box). Not Camera2d: VVVVVV flips from screen to screen,
 * each room exactly one screen, and so does this. Not Physics.
 *
 * Exercises: the trinkets, shiny and out of the way; the enemies,
 * which in VVVVVV just move back and forth in straight lines; moving
 * and disappearing platforms, and conveyors; the crew to rescue; the
 * Super Gravitron (bouncing between two gravity lines, dodging, for as
 * long as you last); rooms that wrap left-right too; and more rooms,
 * which are strings.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The ship *)
(*****************************************************************************)

(* a room is 24 x 18 tiles, one screen *)
let tile = 40.

type room = { name : string; map : Tilemap.t; color : color; wraps : bool }

let room ?(wraps = false) (name : string) (color : color) (rows : string list) : room =
  { name; map = Tilemap.of_strings tile rows; color; wraps }

let wall = "########################"
let open_row = "........................"

(* flip up, walk over the wall on the ceiling, and flip down between the
 * two beds of spikes *)
let first =
  room "Don't Jump, Flip" (rgb 90 210 230)
    ([ wall; wall; wall; wall; "#..................vvvv." ]
    @ List.init 4 (fun _ -> "#.......................")
    @ List.init 4 (fun _ -> "#.........##............")
    @ [ "#..P......##.^^^........" ]
    @ List.init 4 (fun _ -> wall))

(* spikes above, spikes below, taken in turns *)
let second =
  room "Both Sides Now" (rgb 230 110 200)
    ([ wall; wall; wall; wall; "...........vvvv........." ]
    @ List.init 8 (fun _ -> open_row)
    @ [ "..C..^^^^........^^^^..." ]
    @ List.init 4 (fun _ -> wall))

(* walk off into the pit: the line throws you up to the ceiling *)
let third =
  room "Line of Least Resistance" (rgb 120 220 110)
    ([ wall; wall; wall; wall; "...................vvvv." ]
    @ List.init 8 (fun _ -> open_row)
    @ [ "..C.....................";
        "########........########";
        "########--------########";
        "########........########";
        "########^^^^^^^^########" ])

(* no floor in the middle: fall off the bottom, come back at the top,
 * and land on the ledge you could not reach *)
let fourth =
  room ~wraps:true "Around and Around" (rgb 240 210 90)
    (List.init 4 (fun _ -> "#####..............#####")
    @ List.init 3 (fun _ -> ".......................#")
    @ [ "............E..........#"; "..........#####........#" ]
    @ List.init 4 (fun _ -> ".......................#")
    @ [ "..C....................#" ]
    @ List.init 4 (fun _ -> "#####..............#####"))

let rooms = [| first; second; third; fourth |]

let solid (c : char) : bool = c = '#'
let spiky (c : char) : bool = c = '^' || c = 'v'
let checkpoint (c : char) : bool = c = 'C'
let teleporter (c : char) : bool = c = 'E'

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* where to come back to, and which way up *)
type save = { s_room : int; s_x : number; s_y : number; s_flipped : bool }

type play = {
  room : int;
  x : number;
  y : number;
  vx : number;
  vy : number;
  flipped : bool; (* gravity pulls up *)
  facing : number;
  in_line : bool; (* on a gravity line: it flips you once, when you reach it *)
  save : save;
  dead : int; (* frames of dying left *)
  deaths : int;
  frames : int;
}

type scene = Title | Aboard of play | Home of play
type model = scene Scene2d.t

(* Viridian's box *)
let size = (22., 38.)

let map_of (p : play) : Tilemap.t = rooms.(p.room).map

let start : play =
  let x, y =
    match Tilemap.find first.map 'P' with (c, r) :: _ -> Tilemap.center first.map c r | [] -> (0., 0.)
  in
  { room = 0; x; y; vx = 0.; vy = 0.; flipped = false; facing = 1.; in_line = false;
    save = { s_room = 0; s_x = x; s_y = y; s_flipped = false }; dead = 0; deaths = 0; frames = 0 }

let respawn (p : play) : play =
  { p with room = p.save.s_room; x = p.save.s_x; y = p.save.s_y; flipped = p.save.s_flipped; vx = 0.; vy = 0.;
           in_line = false; dead = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Gravity, one sign *)
(*****************************************************************************)

let gravity = 1.1
let max_fall = 13.
let run_top = 7.
let run_accel = 1.4
let run_brake = 1.8
let dying_frames = 30

(* which way is down: -1, or 1 upside down *)
let falling (p : play) : number = if p.flipped then 1. else -1.

(* on something, on the side gravity pulls to: the floor, or the
 * ceiling; the only place a flip can be made from *)
let standing (p : play) : bool = Tile_move.hits solid (map_of p) size p.x (p.y + falling p)

(* out of the screen's side into the next room; and, in a room that
 * wraps, out of the bottom and in at the top *)
let through_edges (p : play) : play =
  let (b : Camera2d.rect) = Tilemap.bounds (map_of p) in
  let w = b.right - b.left and h = b.top - b.bottom in
  let p =
    if p.x > b.right && p.room +.. 1 < Array.length rooms then { p with room = p.room +.. 1; x = p.x - w }
    else if p.x < b.left && p.room > 0 then { p with room = p.room -.. 1; x = p.x + w }
    else p
  in
  if not rooms.(p.room).wraps then p
  else if p.y > b.top then { p with y = p.y - h }
  else if p.y < b.bottom then { p with y = p.y + h }
  else p

(* A gravity line flips you when you reach it, and not again until you
 * have left it. It is drawn across the middle of its tile, and the
 * middle of a box 38 high is in the tile just when the box touches that
 * line. *)
let gravity_line (p : play) : play =
  let on = Tilemap.tile_at (map_of p) p.x p.y = Some '-' in
  if on && not p.in_line then { p with flipped = not p.flipped; vy = 0.; in_line = true }
  else { p with in_line = on }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* what the player does this frame: the game reads it off the keyboard,
 * the tests make it up *)
type input = { dx : number; flip : bool }

let nothing = { dx = 0.; flip = false }

let step (i : input) (p : play) : play =
  if p.dead > 0 then if p.dead = 1 then respawn p else { p with dead = p.dead -.. 1 }
  else
    let p = { p with frames = p.frames +.. 1 } in
    (* the flip, the only move: from a surface only, never in the air *)
    let p = if i.flip && standing p then { p with flipped = not p.flipped; vy = 0. } else p in
    let target = i.dx * run_top in
    let rate = if i.dx = 0. then run_brake else run_accel in
    let vx = if p.vx < target then Float.min target (p.vx + rate) else Float.max target (p.vx - rate) in
    let vy = Float.max (-.max_fall) (Float.min max_fall (p.vy + (falling p * gravity))) in
    (* x first, then y, a pixel at a time (Tile_move) *)
    let map = map_of p in
    let (x, y), hit_x = Tile_move.move_by solid map size (p.x, p.y) (vx, 0.) in
    let (x, y), hit_y = Tile_move.move_by solid map size (x, y) (0., vy) in
    let p =
      { p with x; y; vx = (if hit_x then 0. else vx); vy = (if hit_y then 0. else vy);
               facing = (if i.dx <> 0. then i.dx else p.facing) }
    in
    let p = gravity_line (through_edges p) in
    (* a checkpoint touched: come back here, on its tile, this way up *)
    let p =
      if Tile_move.hits checkpoint (map_of p) size p.x p.y then
        match
          List.find_opt
            (fun (c, r) ->
              let cx, cy = Tilemap.center (map_of p) c r in
              Float.abs (cx - p.x) < tile && Float.abs (cy - p.y) < tile)
            (Tilemap.find (map_of p) 'C')
        with
        | Some (c, r) ->
            let s_x, s_y = Tilemap.center (map_of p) c r in
            { p with save = { s_room = p.room; s_x; s_y; s_flipped = p.flipped } }
        | None -> p
      else p
    in
    if Tile_move.hits spiky (map_of p) size p.x p.y then
      { p with dead = dying_frames; deaths = p.deaths +.. 1; vx = 0.; vy = 0. }
    else p

let arrived (p : play) : bool = p.dead = 0 && Tile_move.hits teleporter (map_of p) size p.x p.y

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  let flip = pressed (fun k -> k.kspace || k.kup || k.kdown) in
  match scenes.scene with
  | Title | Home _ -> if pressed (fun k -> k.kspace) then Scene2d.go (Aboard start) scenes else scenes
  | Aboard p ->
      let p = step { dx = to_x computer.keyboard; flip } p in
      if arrived p then Scene2d.go (Home p) scenes else { scenes with scene = Aboard p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let space = rgb 8 8 20
let spike_color = rgb 200 200 210
let crew = rgb 132 224 240
let dark = rgb 20 20 30

(* the stars going by behind the ship, as in VVVVVV's backgrounds *)
let stars (frames : int) : shape list =
  List.init 50 (fun i ->
      let speed = float_of_int ((i mod 3) +.. 1) in
      let x0 = float_of_int ((i *.. 7919) mod 960) and y = float_of_int ((i *.. 104729) mod 720) - 360. in
      let x = Float.rem (x0 - (float_of_int frames * speed)) 960. in
      let x = (if x < 0. then x + 960. else x) - 480. in
      square (rgb 90 90 130) (speed * 1.5) |> move x y)

let tile_shape (r : room) (active : bool) (c : char) : shape =
  let spikes = List.init 3 (fun i -> triangle spike_color 8. |> move ((float_of_int i * 13.) - 13.) (-.tile / 2. + 7.)) in
  match c with
  | '#' -> group [ square r.color tile; square (rgb 10 10 30) (tile - 12.) |> fade 0.2 ]
  | '^' -> group spikes
  | 'v' -> group spikes |> rotate 180.
  | '-' -> rectangle white tile 3.
  | 'C' ->
      group [ rectangle (if active then white else rgb 110 110 130) 26. 34.; text dark 2.2 "C" ]
  | 'E' -> group [ circle r.color 34. |> fade 0.5; circle white 18. |> fade 0.8 ]
  | _ -> group []

(* Viridian, 10 x 16 pixels, facing right; dying, his smile's two rows
 * are swapped into a frown *)
let viridian_rows =
  [ "..CCCCCC..";
    ".CCCCCCCC.";
    "CCCCCCCCCC";
    "CCCKCCCKCC";
    "CCCKCCCKCC";
    "CCCCCCCCCC";
    "CCKCCCCCKC";
    "CCCKKKKKCC";
    ".CCCCCCCC.";
    "..CCCCCC..";
    ".CCCCCCCC.";
    "CCCCCCCCCC";
    "C.CCCCCC.C";
    "..CCCCCC..";
    "..CC..CC..";
    "..CC..CC.." ]

let frown (rows : string list) : string list =
  List.mapi (fun i row -> if i = 6 then List.nth rows 7 else if i = 7 then List.nth rows 6 else row) rows

let viridian (computer : computer) (p : play) : shape =
  let color = if p.dead > 0 then rgb 230 120 120 else crew in
  let up = if p.flipped then -1. else 1. in
  (if not (Sprite.artwork ~default:true computer.flags) then
     group
       [ rectangle color 20. 18. |> move_y (-9. * up);
         circle color 11. |> move_y (8. * up);
         circle dark 2.5 |> move (4. * p.facing) (10. * up) ]
   else
     (* upside down, the mouth is turned back, or he would frown on
      * every ceiling: frown xor flipped *)
     let rows = if p.dead > 0 <> p.flipped then frown viridian_rows else viridian_rows in
     let rows = if p.facing < 0. then Sprite.flip rows else rows in
     let rows = if p.flipped then List.rev rows else rows in
     Sprite.pixels 2.4 [ ('C', color); ('K', dark) ] rows)
  |> move p.x p.y

let view_play (computer : computer) (p : play) : shape list =
  let screen = computer.screen in
  let r = rooms.(p.room) in
  (* the checkpoint you would come back to, lit *)
  let lit = if p.save.s_room = p.room && Tilemap.tile_at r.map p.save.s_x p.save.s_y = Some 'C' then [ tile_shape r true 'C' |> move p.save.s_x p.save.s_y ] else [] in
  [ rectangle space screen.width screen.height ]
  @ stars p.frames
  @ [ Tilemap.view (tile_shape r false) r.map ]
  @ lit
  @ [ viridian computer p;
      rectangle black screen.width 36. |> move_y (screen.bottom + 18.);
      text white 2. r.name |> move_y (screen.bottom + 18.);
      text white 1.8 (Printf.sprintf "deaths %d" p.deaths) |> move (screen.left + 90.) (screen.top - 20.);
      text (rgb 150 150 180) 1.6 "arrows walk    space flips gravity, standing on something"
      |> move_y (screen.top - 20.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let backdrop = rectangle space screen.width screen.height :: stars model.frames in
  match model.scene with
  | Title ->
      backdrop
      @ [ text crew 7. "VVVVVV" |> move_y 220.;
          text white 2. "you cannot jump" |> move_y 120.;
          text white 2. "space flips gravity -- from the floor, or from the ceiling" |> move_y 80.;
          text white 2. "arrows walk, and steer while you fall" |> move_y 40.;
          text (rgb 150 150 180) 2. "spikes on both sides.  C is a checkpoint" |> move_y (-10.);
          (* one bar, stood on from both sides *)
          rectangle crew 300. 8. |> move_y (-110.);
          viridian computer { start with x = -60.; y = -87. };
          viridian computer { start with x = 60.; y = -133.; flipped = true; facing = -1. } ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-250.) ]
  | Aboard p -> view_play computer p
  | Home p ->
      backdrop
      @ [ text crew 5. "TELEPORTED HOME" |> move_y 100.;
          text white 2.5 (Printf.sprintf "%d deaths, %.1f seconds" p.deaths (float_of_int p.frames / 60.))
          |> move_y 20. ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-200.) ]

let help =
  {|TinyVVVVVV
  left right         walk, and steer while falling
  space (up, down)   flip gravity -- only when standing, on the floor or the ceiling
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
