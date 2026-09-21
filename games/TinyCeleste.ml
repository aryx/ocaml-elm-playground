(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Celeste (Maddy Thorson and Noel Berry, 2018): climb
 * a mountain one screen at a time, with a jump, a dash in any of eight
 * directions, and the walls to jump off.
 *
 *   arrows   run, and aim the dash
 *   space    jump (hold it longer to jump higher)
 *   x        dash -- once, until your feet touch the ground again;
 *            the hair says whether you have it (red) or not (blue)
 *   1 2 3 4  turn off, one at a time, the four ways the game is on
 *            your side, and feel it get worse
 *
 * Celeste started as a PICO-8 game made in four days for a game jam
 * in 2015 (now "Celeste Classic"), and became in 2018 the platformer
 * people point to when they mean *controls that feel right*. Its
 * designers wrote down how, which almost no one does: Maddy Thorson's
 * notes on how Celeste and TowerFall move things ("Celeste and
 * TowerFall Physics", 2017 -- the pixel-at-a-time movement
 * gamekits/platformer/Tile_move.mli already cites), and the game's own
 * Player code, published for anyone to learn from. It is also
 * remembered for its Assist Mode (slow the game down, a dash that never
 * runs out: a hard game that lets you choose how hard), and for a story
 * about anxiety told through a mountain. (Names and dates from memory,
 * to check.)
 *
 * What it has that no game in this directory has yet is **game feel**,
 * and the lesson is that game feel is not one thing but a handful of
 * small, named lies, each a few frames long, each told in the player's
 * favour:
 *
 *   - coyote time ([coyote_frames]): you can still jump for a few
 *     frames after running off a ledge -- because the eye says you
 *     pressed in time, even when the pixel says you were already over
 *     the drop (named after the cartoon coyote, who hangs in the air
 *     until he looks down);
 *   - jump buffering ([buffer_frames]): a jump pressed a few frames
 *     *before* landing is kept and done on landing, instead of being
 *     lost because it came too early;
 *   - variable jump height: let go of the button on the way up and the
 *     rise is cut ([cut]), so a tap is a hop and a hold is a leap --
 *     one button, a range of jumps;
 *   - corner correction ([corner_slide]): a jump that clips the corner
 *     of a ceiling by a few pixels is nudged round it rather than
 *     stopped dead, because a hit that close looks like a miss.
 *
 * Each is a separate function, and each can be switched off in the game
 * (1 to 4): play a room, turn one off, play it again. None of them is
 * visible when it works. That is the point, and why they are worth
 * seeing one at a time: every one is the game taking the player's side
 * against the exactness of its own rules. Compare games/TinyMario.ml,
 * which has none of them and is exactly right, and harder than it
 * looks.
 *
 * And the two moves that are Celeste's own:
 *
 *   - the dash: a fixed speed in one of eight directions for a few
 *     frames, gravity switched off, and only one until you land. One
 *     dash a jump is the whole design space of the game -- the rooms are
 *     puzzles about where to spend it;
 *   - the wall jump: falling against a wall slows you ([wall_slide]),
 *     and jumping off it throws you away from it, with a moment in
 *     which you cannot steer back ([wall_lock]), so that a shaft is
 *     climbed by bouncing between its sides.
 *
 * Death is instant and cheap: a spike puts you back at the room's
 * start in a fifth of a second, and the deaths are counted and shown.
 * Celeste's own line is that the count is not a score, it is how much
 * you practised.
 *
 * What it uses: gamekits/platformer's Tile_move (the movement: one pixel at
 * a time, x and then y, which is Celeste's own way of moving -- the
 * kit's header cites it, and this is the first game here written after
 * it), Tilemap (the three rooms, as strings), Scene2d. Not Physics:
 * nothing here is physical, every number is a feel, tuned. Not
 * Camera2d: a room is exactly one screen, as the original's are.
 *
 * The feel lives in this file, and gamekits/platformer is where it would
 * move when a second game wants it: games/TinyMario.ml is the obvious
 * one, and would be a better game for coyote time and a buffer.
 *
 * Exercises: climbing with stamina (Celeste lets you hold on to a wall,
 * for a while); the strawberries, optional and harder to reach, that
 * are the real game for those who want it; the moving blocks and the
 * wind; the Assist Mode itself, which is three numbers in the model;
 * and more rooms, which are strings.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The mountain *)
(*****************************************************************************)

let tile = 40.
let cols = 24
let rows = 18

(* A room is its four walls and what is put in it: [things] are
 * (row, from column, to column, what). *)
let room (things : (int * int * int * char) list) : Tilemap.t =
  Tilemap.of_strings tile
    (List.init rows (fun r ->
         String.init cols (fun c ->
             if r = 0 || r = rows -.. 1 || c = 0 || c = cols -.. 1 then '#'
             else
               match List.find_opt (fun (row, c0, c1, _) -> row = r && c >= c0 && c <= c1) things with
               | Some (_, _, _, ch) -> ch
               | None -> '.')))

(* the climb: ledges three tiles apart, a jump's height, so that the
 * first room is about jumping well *)
let climb =
  room
    [ (16, 2, 2, 'P'); (14, 6, 10, '='); (11, 12, 16, '='); (8, 18, 22, '='); (5, 11, 15, '='); (1, 11, 14, 'E') ]

(* the gap: too far to jump, not too far to jump and dash -- the room
 * is a question about where to spend the dash *)
let gap =
  room
    [ (16, 2, 2, 'P'); (16, 7, 14, '^'); (13, 17, 22, '='); (10, 20, 22, '='); (7, 16, 18, '='); (4, 20, 22, '=');
      (1, 19, 22, 'E') ]

(* the shaft: four tiles wide and fourteen high, climbed by bouncing
 * off its walls *)
let shaft =
  room
    (List.concat (List.init 13 (fun i -> [ (i +.. 3, 9, 9, '#'); (i +.. 3, 14, 14, '#') ]))
    @ [ (16, 11, 11, 'P'); (1, 10, 13, 'E'); (16, 1, 8, '^'); (16, 15, 22, '^') ])

let rooms = [| climb; gap; shaft |]

let solid (c : char) : bool = c = '#' || c = '='
let spiky (c : char) : bool = c = '^'
let exit (c : char) : bool = c = 'E'

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* the four lies, each of which can be switched off *)
type feel = { coyote : bool; buffer : bool; variable : bool; corners : bool }

let all_on = { coyote = true; buffer = true; variable = true; corners = true }

type play = {
  room : int;
  map : Tilemap.t;
  x : number;
  y : number;
  vx : number;
  vy : number;
  facing : number;
  (* frames since the feet last touched the ground, 0 while they do *)
  airborne : int;
  (* frames since jump was last pressed: a press remembered *)
  since_press : int;
  held : bool; (* the jump key, last frame *)
  dash_ready : bool;
  dashing : int; (* frames of dash left *)
  dash_vx : number;
  dash_vy : number;
  wall_lock : int; (* frames after a wall jump in which you cannot steer back *)
  dead : int; (* frames of dying left *)
  deaths : int;
  feel : feel;
}

type scene = Title | Climbing of play | Summit of int (* the deaths *)
type model = scene Scene2d.t

let size = (24., 34.)

let start_of (map : Tilemap.t) : number * number =
  match Tilemap.find map 'P' with (c, r) :: _ -> Tilemap.center map c r | [] -> (0., 0.)

let enter (room : int) (deaths : int) (feel : feel) : play =
  let map = rooms.(room) in
  let x, y = start_of map in
  { room; map; x; y; vx = 0.; vy = 0.; facing = 1.; airborne = 0; since_press = 99; held = false;
    dash_ready = true; dashing = 0; dash_vx = 0.; dash_vy = 0.; wall_lock = 0; dead = 0; deaths; feel }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The feel: every number here is tuned, not derived *)
(*****************************************************************************)

let gravity = 0.9
let max_fall = 13.
let run_top = 6.5
let run_accel = 1.3
let run_brake = 1.6
let air_brake = 0.5
let jump_speed = 15.5
let dash_speed = 15.
let dash_frames = 9
let coyote_frames = 6
let buffer_frames = 6

(* the lie about time after: still a jump, just after the ledge *)
let can_jump (p : play) : bool = p.airborne = 0 || (p.feel.coyote && p.airborne <= coyote_frames)

(* the lie about time before: a press kept until it can be used *)
let wants_jump (pressed : bool) (p : play) : bool = pressed || (p.feel.buffer && p.since_press <= buffer_frames)

(* the lie about intent: let go on the way up and the rise is cut, so
 * how long the button is held says how high to go *)
let cut (held : bool) (p : play) : play =
  if p.feel.variable && p.held && (not held) && p.vy > 0. && p.dashing = 0 then { p with vy = p.vy * 0.45 } else p

(* the lie about space: a head that clips the corner of a ceiling by a
 * few pixels is slid round it, since a hit that close looks like a miss *)
let corner_slide (p : play) (dy : number) : number option =
  if not p.feel.corners then None
  else
    List.find_map
      (fun dx ->
        if not (Tile_move.hits solid p.map size (p.x + dx) (p.y + dy)) then Some dx else None)
      [ 1.; -1.; 2.; -2.; 3.; -3.; 4.; -4.; 5.; -5.; 6.; -6. ]

(* against a wall, on the left (-1) or the right (1), or not *)
let wall_side (p : play) : number =
  if Tile_move.hits solid p.map size (p.x + 1.) p.y then 1.
  else if Tile_move.hits solid p.map size (p.x - 1.) p.y then -1.
  else 0.

(* falling against a wall you are pushing into is slower *)
let wall_slide (dx : number) (p : play) : number =
  let side = wall_side p in
  if side <> 0. && dx = side && p.vy < 0. then Float.max p.vy (-3.5) else p.vy

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* what the player does this frame, which is all [step] needs: the game
 * reads it off the keyboard, the tests make it up *)
type input = { dx : number; dy : number; jump : bool; jump_held : bool; dash : bool }

let nothing = { dx = 0.; dy = 0.; jump = false; jump_held = false; dash = false }

let step (i : input) (p : play) : play =
  if p.dead > 0 then (if p.dead = 1 then enter p.room (p.deaths +.. 1) p.feel else { p with dead = p.dead -.. 1 })
  else
    let p = { p with since_press = (if i.jump then 0 else p.since_press +.. 1) } in
    (* the dash: a fixed speed in one of eight directions, gravity off,
     * one per jump *)
    let p =
      if i.dash && p.dash_ready && p.dashing = 0 then
        let dx, dy = if i.dx = 0. && i.dy = 0. then (p.facing, 0.) else (i.dx, i.dy) in
        let n = Float.hypot dx dy in
        { p with dash_ready = false; dashing = dash_frames; dash_vx = dx / n * dash_speed; dash_vy = dy / n * dash_speed }
      else p
    in
    let p =
      if p.dashing > 0 then
        let ending = p.dashing = 1 in
        { p with vx = p.dash_vx * (if ending then 0.55 else 1.); vy = p.dash_vy * (if ending then 0.4 else 1.);
                 dashing = p.dashing -.. 1 }
      else
        (* running: quick to start, quick to stop, never instant *)
        let target = if p.wall_lock > 0 then p.vx else i.dx * run_top in
        let rate = if i.dx = 0. then (if p.airborne = 0 then run_brake else air_brake) else run_accel in
        let vx = if p.vx < target then Float.min target (p.vx + rate) else Float.max target (p.vx - rate) in
        let vy = Float.max (-.max_fall) (p.vy - gravity) in
        let p = { p with vx; vy } in
        { p with vy = wall_slide i.dx p }
    in
    (* jumping, off the ground or off a wall *)
    let p =
      if p.dashing = 0 && wants_jump i.jump p && can_jump p then
        { p with vy = jump_speed; airborne = coyote_frames +.. 1; since_press = 99 }
      else if p.dashing = 0 && i.jump && p.airborne > 0 && wall_side p <> 0. then
        let away = -.wall_side p in
        { p with vy = jump_speed * 0.9; vx = away * 9.; wall_lock = 7; facing = away; since_press = 99 }
      else p
    in
    let p = cut i.jump_held p in
    (* moving, one pixel at a time: x first, then y, so that a wall
     * stops only the running (gamekits/platformer's Tile_move) *)
    let (x, y), hit_x = Tile_move.move_by solid p.map size (p.x, p.y) (p.vx, 0.) in
    let p = { p with x; y; vx = (if hit_x then 0. else p.vx) } in
    let p =
      let (x, y), hit_y = Tile_move.move_by solid p.map size (p.x, p.y) (0., p.vy) in
      if hit_y && p.vy > 0. then
        match corner_slide p p.vy with
        | Some dx -> { p with x = p.x + dx; y = p.y + p.vy }
        | None -> { p with x; y; vy = 0. }
      else { p with x; y; vy = (if hit_y then 0. else p.vy) }
    in
    let on_ground = Tile_move.on_ground solid p.map size p.x p.y in
    let p =
      { p with
        airborne = (if on_ground then 0 else p.airborne +.. 1);
        dash_ready = p.dash_ready || (on_ground && p.dashing = 0);
        wall_lock = max 0 (p.wall_lock -.. 1);
        facing = (if i.dx <> 0. && p.wall_lock = 0 then i.dx else p.facing);
        held = i.jump_held }
    in
    if Tile_move.hits spiky p.map size p.x p.y then { p with dead = 12; vx = 0.; vy = 0. } else p

let reached_exit (p : play) : bool = p.dead = 0 && Tile_move.hits exit p.map size p.x p.y

let toggles (scenes : model) (f : feel) : feel =
  let pressed k = Scene2d.pressed (fun kb -> Set_.mem k kb.keys) scenes in
  { coyote = (if pressed "1" then not f.coyote else f.coyote);
    buffer = (if pressed "2" then not f.buffer else f.buffer);
    variable = (if pressed "3" then not f.variable else f.variable);
    corners = (if pressed "4" then not f.corners else f.corners) }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  match scenes.scene with
  | Title | Summit _ -> if pressed (fun k -> k.kspace) then Scene2d.go (Climbing (enter 0 0 all_on)) scenes else scenes
  | Climbing p ->
      let k = computer.keyboard in
      let input =
        { dx = to_x k; dy = to_y k; jump = pressed (fun k -> k.kspace); jump_held = k.kspace;
          dash = pressed (fun k -> Set_.mem "x" k.keys) }
      in
      let p = step input { p with feel = toggles scenes p.feel } in
      if reached_exit p then
        if p.room +.. 1 < Array.length rooms then Scene2d.go (Climbing (enter (p.room +.. 1) p.deaths p.feel)) scenes
        else Scene2d.go (Summit p.deaths) scenes
      else { scenes with scene = Climbing p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let night = rgb 30 26 58
let rock = rgb 96 84 124
let snow = rgb 214 220 240
let spikes = rgb 220 220 235
let summit_glow = rgb 250 220 120

let tile_shape (c : char) : shape =
  match c with
  | '#' -> square rock tile
  (* a ledge, which is what gets the snow *)
  | '=' -> group [ square rock tile; rectangle snow tile 6. |> move_y ((tile / 2.) - 3.) ]
  | '^' ->
      group (List.init 3 (fun i -> triangle spikes 8. |> move ((float_of_int i * 13.) - 13.) (-.tile / 2. + 7.)))
  | 'E' -> square summit_glow tile |> fade 0.35
  | _ -> group []

(* Madeline: the hair is red when the dash is there to be spent, and
 * blue when it has been -- the one piece of interface Celeste needs *)
let climber (p : play) : shape =
  let hair = if p.dash_ready then rgb 220 60 60 else rgb 80 150 230 in
  let body = if p.dashing > 0 then white else rgb 240 220 200 in
  group
    [ rectangle (rgb 60 70 120) 20. 20. |> move_y (-7.);
      circle body 9. |> move_y 9.;
      circle hair 8. |> move (-4. * p.facing) 13.;
      circle hair 6. |> move (-11. * p.facing) 9. ]
  |> move p.x p.y

let view_play (computer : computer) (p : play) : shape list =
  let screen = computer.screen in
  let on b = if b then "on" else "OFF" in
  [ rectangle night screen.width screen.height; Tilemap.view tile_shape p.map ]
  @ (if p.dead > 0 then [ circle (rgb 220 60 60) (float_of_int (12 -.. p.dead) * 5.) |> fade 0.6 |> move p.x p.y ]
     else [ climber p ])
  @ [ text snow 2.2 (Printf.sprintf "room %d of %d    deaths %d" (p.room +.. 1) (Array.length rooms) p.deaths)
      |> move_y (screen.top - 40.);
      text (rgb 170 170 200) 1.7
        (Printf.sprintf "1 coyote time %s    2 jump buffer %s    3 variable jump %s    4 corner correction %s"
           (on p.feel.coyote) (on p.feel.buffer) (on p.feel.variable) (on p.feel.corners))
      |> move_y (screen.bottom + 55.);
      text (rgb 130 130 160) 1.7 "arrows run    space jumps (hold it)    x dashes, once" |> move_y (screen.bottom + 25.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  match model.scene with
  | Title ->
      [ rectangle night screen.width screen.height;
        text snow 6. "TINY CELESTE" |> move_y 200.;
        text snow 2. "arrows run    space jumps -- hold it to go higher" |> move_y 70.;
        text snow 2. "x dashes in any of eight directions, once until you land" |> move_y 30.;
        text (rgb 220 60 60) 2. "red hair: the dash is there.  blue: it is spent" |> move_y (-20.);
        text (rgb 170 170 200) 2. "1 2 3 4 switch off the ways the game is on your side" |> move_y (-70.) ]
      @ Scene2d.blink 1. model [ text snow 3. "PRESS SPACE" |> move_y (-200.) ]
  | Climbing p -> view_play computer p
  | Summit deaths ->
      [ rectangle night screen.width screen.height;
        text summit_glow 5. "THE SUMMIT" |> move_y 100.;
        text snow 2.5 (Printf.sprintf "%d deaths on the way up" deaths) |> move_y 20.;
        text (rgb 170 170 200) 2. "not a score: how much you practised" |> move_y (-25.) ]
      @ Scene2d.blink 1. model [ text snow 3. "PRESS SPACE" |> move_y (-200.) ]

let help =
  {|TinyCeleste
  arrows   run, and aim the dash
  space    jump -- hold it to jump higher
  x        dash, once until you land (red hair: you have it)
  1 2 3 4  switch off coyote time, the jump buffer, the variable jump,
           the corner correction -- and feel the game get worse
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
