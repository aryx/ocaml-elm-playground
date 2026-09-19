(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Breakout (Atari, 1976), Pong turned on its side and
 * played alone: a paddle at the bottom, a wall of 8 rows of bricks at
 * the top, a ball breaking a brick at each hit. Left and right arrows,
 * or the mouse, to move the paddle; space to serve. Three balls; clear
 * the wall, then a second one.
 *
 * Breakout was Nolan Bushnell's idea, designed with Steve Bristow, and
 * its prototype was built by Steve Wozniak, over four nights, for Steve
 * Jobs, then at Atari: Atari paid a bonus for every chip saved below
 * 50, and Wozniak's board had about 45, so tightly designed that
 * Atari's engineers couldn't understand it, and built their own. A year
 * later Wozniak gave the Apple II color graphics, paddles and a speaker
 * so that Breakout could be written in BASIC, in software: "Little
 * Brick Out". A game that took a board of chips could now be a program
 * -- what every game of this directory is. Then Arkanoid (Taito, 1986)
 * added capsules falling from the bricks (a wider paddle, three balls,
 * a laser), and the genre has had power-ups since. (Stories from
 * memory, to check.)
 *
 * The original's rules, kept here:
 *
 *  - the bricks: yellow, green, orange and red, 2 rows of each, worth
 *    1, 3, 5 and 7 points: 448 points a wall, and the game ends after
 *    two walls (896 is the best score). The monitor was black and
 *    white; the colors were strips of cellophane glued on the glass, one
 *    per pair of rows, as for Space Invaders' green bottom (see
 *    games/TinyInvaders.ml);
 *
 *  - the ball speeds up four times: after 4 hits, after 12, and at its
 *    first orange brick, and its first red one (see [speed]);
 *
 *  - once the ball has broken through the wall and hit the back wall,
 *    the paddle shrinks to half its width. Breaking through is the
 *    game's most famous moment, which no rule describes: behind the
 *    wall, the ball bounces between the back wall and the bricks' top,
 *    breaking them from above, and the points pour in by themselves;
 *    good players dig a tunnel on purpose.
 *
 * The paddle doesn't reflect the ball, the way a mirror would: where
 * the ball lands on it decides where it goes ([paddle_bounce]), straight
 * up in the middle, at a slant on the sides. It's a rule, not physics,
 * and it's the whole game: with a mirror, the player could move the
 * ball but never aim it. Pong did the same, its paddle cut into 8
 * segments, each with its angle (Allan Alcorn, 1972); games/TinyPong.ml
 * shows the other way, the physics engine's friction dragging the ball.
 *
 * What it uses: Tilemap (the wall is typed as strings, a brick being 2
 * tiles, "Rr": the map draws it, says which brick a point is in, and a
 * brick broken is its 2 tiles set to ' '), Scene2d (title, play, game
 * over), Audio (a pitch per row, like the original's beeps). Not
 * Physics: the ball's motion is 2 additions, and its bounces are rules
 * (the paddle's above, a wall's or brick's plain reversal); nor
 * Tilemap.hits, which says whether a box hits a tile, where Breakout
 * needs which bricks (see [bricks_under]). No Camera2d: the whole world
 * is on the screen.
 *
 * Left as exercises: Arkanoid's capsules; bricks needing two hits, or
 * never breaking (a new letter in the wall's strings); more walls, typed
 * as strings; the original's two players, taking turns; the ball
 * stuck in a loop between unbreakable bricks, which Arkanoid breaks by
 * nudging its angle.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The field and the wall *)
(*****************************************************************************)

(* In pixels; the screen is 1000 x 1000, the field 700 wide, inside
 * walls on the left, the right and the top ("the back wall"). *)
let field_left = -350.
let field_right = 350.
let field_top = 420.

let paddle_y = -400.
let paddle_height = 16.

(* the ball is a square, as the original's: on a board of chips, a
 * square is what's cheap to draw (a few rows of the screen, a few
 * columns) *)
let ball_size = 12.
let half = ball_size / 2.

(* A brick is two tiles of 25, 50 x 25: 14 bricks a row. A capital
 * letter is a brick's left half (the one [view_bricks] draws, the whole
 * brick), the same letter in lowercase its right half. 3 empty rows at
 * the top, where the ball goes once it has broken through. *)
let wall : Tilemap.t =
  Tilemap.of_strings 25.
    [ "                            ";
      "                            ";
      "                            ";
      "RrRrRrRrRrRrRrRrRrRrRrRrRrRr";
      "RrRrRrRrRrRrRrRrRrRrRrRrRrRr";
      "OoOoOoOoOoOoOoOoOoOoOoOoOoOo";
      "OoOoOoOoOoOoOoOoOoOoOoOoOoOo";
      "GgGgGgGgGgGgGgGgGgGgGgGgGgGg";
      "GgGgGgGgGgGgGgGgGgGgGgGgGgGg";
      "YyYyYyYyYyYyYyYyYyYyYyYyYyYy";
      "YyYyYyYyYyYyYyYyYyYyYyYyYyYy" ]

(* a Tilemap is centered on (0, 0): the wall's map is moved up, its top
 * against the back wall; a world point (x, y) is at (x, y - wall_y) in
 * the map *)
let wall_y = field_top - (float_of_int (Tilemap.rows wall) * Tilemap.size wall / 2.)

let points (brick : char) : int = match brick with 'R' -> 7 | 'O' -> 5 | 'G' -> 3 | 'Y' -> 1 | _ -> 0
let color (brick : char) : color = match brick with 'R' -> red | 'O' -> orange | 'G' -> green | _ -> yellow

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type ball = { x : number; y : number; vx : number; vy : number }

type game = {
  bricks : Tilemap.t;
  paddle_x : number;
  (* the mouse's x at the last frame: when it changes, the mouse moves
   * the paddle (else the arrows do) *)
  mouse_x : number;
  ball : ball option; (* None: waiting on the paddle for the serve *)
  balls : int; (* left, the one in play included *)
  score : int;
  (* for the speed-ups, counted again for each ball *)
  hits : int;
  reached_orange : bool;
  reached_red : bool;
  (* the paddle, halved once the ball reached the back wall *)
  shrunk : bool;
  second_wall : bool;
}

type scene = Title | Playing of game | Game_over of int

type model = { scenes : scene Scene2d.t; hi_score : int }

let new_game (mouse_x : number) : game =
  { bricks = wall; paddle_x = 0.; mouse_x; ball = None; balls = 3; score = 0;
    hits = 0; reached_orange = false; reached_red = false; shrunk = false; second_wall = false }

let initial_model : model = { scenes = Scene2d.start Title; hi_score = 0 }

let paddle_width (g : game) : number = if g.shrunk then 50. else 100.

(* 360 pixels per second, plus 60 for each of the four speed-ups
 * reached: at most 600, 10 pixels a tick, less than the ball's size and
 * a brick's height, so that the ball can't jump over a brick between
 * two ticks (tunneling, see games/TinyPong.ml) *)
let speed (g : game) : number =
  let ups = List.length (List.filter Fun.id [ g.hits >= 4; g.hits >= 12; g.reached_orange; g.reached_red ]) in
  360. + (60. * float_of_int ups)

(*****************************************************************************)
(* The bricks the ball hits *)
(*****************************************************************************)

(* the brick the world point (x, y) is in, if any: its left cell, and
 * its letter, in capital *)
let brick_at (map : Tilemap.t) (x : number) (y : number) : (int * int * char) option =
  let col, row = Tilemap.cell map x (y - wall_y) in
  match Tilemap.get map col row with
  | Some ('A' .. 'Z' as c) -> Some (col, row, c)
  | Some ('a' .. 'z' as c) -> Some (col -.. 1, row, Char.uppercase_ascii c)
  | _ -> None

(* The bricks under the ball, centered on (x, y): the ones its 4 corners
 * are in. The ball being smaller than a tile, its corners are enough:
 * it can't overlap a tile without a corner in it. At most 4 bricks,
 * often 1, sometimes 2 (the ball on the border between two), each
 * once.
 *
 *     +--------+--------+
 *     |  Rr brick 1     |    two corners in brick 1,
 *     |      +--+       |    two in brick 2: both break
 *     +------|--|-------+
 *     |      +--+ brick 2   (Tilemap.hits would say "yes", but not
 *     +-----------------+    which ones)
 *
 * The corners are taken a hair inside the ball (0.01), so that a ball
 * touching a brick, but not entering it, doesn't break it. *)
let bricks_under (map : Tilemap.t) (x : number) (y : number) : (int * int * char) list =
  let h = half - 0.01 in
  [ (-.h, -.h); (h, -.h); (-.h, h); (h, h) ]
  |> List.filter_map (fun (dx, dy) -> brick_at map (x + dx) (y + dy))
  |> List.sort_uniq compare

(* the pitch of each row's beep: higher for the bricks worth more *)
let brick_sound (brick : char) : Audio.sound =
  let freq = match brick with 'R' -> 1047. | 'O' -> 880. | 'G' -> 659. | _ -> 523. in
  Audio.square freq |> Audio.lasting 0.06 |> Audio.fading

(* the bricks broken: both tiles cleared, their points, the counts of
 * the speed-ups *)
let break_bricks (bricks : (int * int * char) list) (g : game) : game =
  List.fold_left
    (fun g (col, row, c) ->
      Audio.play (brick_sound c);
      { g with
        bricks = Tilemap.set (Tilemap.set g.bricks col row ' ') (col +.. 1) row ' ';
        score = g.score +.. points c;
        hits = g.hits +.. 1;
        reached_orange = g.reached_orange || c = 'O';
        reached_red = g.reached_red || c = 'R' })
    g bricks

let cleared (map : Tilemap.t) : bool = List.for_all (fun s -> String.trim s = "") (Tilemap.to_strings map)

(*****************************************************************************)
(* The ball *)
(*****************************************************************************)

let dt = 1. / 60.

(* Where the ball lands on the paddle decides where it goes: from the
 * paddle's center (offset 0) to one of its ends (offset -1 or 1), an
 * angle from straight up to 60 degrees to that side. E.g. at the speed
 * 400, a ball landing on the center goes back up at (0, 400), one
 * landing on the right end at (346, 200): 400 * (sin 60, cos 60).
 *
 *          \   |   /
 *        60 \  |  / 60
 *            \ | /
 *     [=======*=======]   the paddle
 *     -1      0       1   the offset
 *
 * Whatever direction it came from: that's the rule's point. *)
let paddle_bounce (g : game) (b : ball) : ball =
  let offset = clamp (-1.) 1. ((b.x - g.paddle_x) / (paddle_width g / 2.)) in
  let angle = degrees_to_radians (offset * 60.) in
  let s = speed g in
  { b with vx = s * sin angle; vy = s * cos angle; y = paddle_y + (paddle_height / 2.) + half }

let on_paddle (g : game) (x : number) (y : number) : bool =
  Float.abs (x - g.paddle_x) < (paddle_width g / 2.) + half
  && y - half < paddle_y + (paddle_height / 2.)
  && y > paddle_y

(* One tick of the ball, one axis at a time, as TinyMario moves Mario:
 * first sideways (a side wall, or a brick's side, reverses vx), then
 * up or down (the back wall, a brick's top or bottom, the paddle). One
 * axis at a time, a bounce always knows which way to go: a ball
 * hitting a corner is stopped by the first axis that hits, and an axis
 * that hits is simply not moved (the ball stays where it was, at most
 * 10 pixels from the brick: not visible). *)
let move_ball (g : game) (b : ball) : game * ball =
  (* velocity rescaled to the current speed, the direction kept *)
  let s = sqrt ((b.vx * b.vx) + (b.vy * b.vy)) in
  let b = { b with vx = b.vx * speed g / s; vy = b.vy * speed g / s } in
  (* sideways *)
  let x = b.x + (b.vx * dt) in
  let g, b =
    match bricks_under g.bricks x b.y with
    | _ when x - half < field_left || x + half > field_right ->
        Audio.play Audio.blip;
        (g, { b with vx = -.b.vx })
    | [] -> (g, { b with x })
    | bricks -> (break_bricks bricks g, { b with vx = -.b.vx })
  in
  (* up or down *)
  let y = b.y + (b.vy * dt) in
  match bricks_under g.bricks b.x y with
  | _ when y + half > field_top ->
      (* the back wall: the paddle shrinks at the first time *)
      Audio.play Audio.blip;
      ({ g with shrunk = true }, { b with vy = -.b.vy })
  | _ when b.vy < 0. && on_paddle g b.x y ->
      Audio.play Audio.blip;
      (g, paddle_bounce g b)
  | [] -> (g, { b with y })
  | bricks -> (break_bricks bricks g, { b with vy = -.b.vy })

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let lost_sound = Audio.square 300. |> Audio.sliding 80. |> Audio.lasting 0.6 |> Audio.fading

(* the paddle: the mouse, if it moved (Breakout's controller was a knob,
 * turned to place the paddle: the mouse is its heir), else the arrows *)
let move_paddle (computer : computer) (g : game) : game =
  let mx = computer.mouse.mx in
  let x = if mx <> g.mouse_x then mx else g.paddle_x + (600. * dt * to_x computer.keyboard) in
  let w = paddle_width g in
  { g with paddle_x = clamp (field_left + (w / 2.)) (field_right - (w / 2.)) x; mouse_x = mx }

let serve_pressed (scenes : scene Scene2d.t) : bool = Scene2d.pressed (fun k -> k.kspace) scenes

let update_game (computer : computer) (scenes : scene Scene2d.t) (g : game) : game =
  let g = move_paddle computer g in
  match g.ball with
  | None when serve_pressed scenes ->
      (* up, 30 degrees to the right or to the left, a ball out of two *)
      let angle = degrees_to_radians (if g.balls mod 2 = 1 then 30. else -30.) in
      let s = speed g in
      { g with ball = Some { x = g.paddle_x; y = paddle_y + (paddle_height / 2.) + half; vx = s * sin angle; vy = s * cos angle } }
  | None -> g
  | Some b ->
      let g, b = move_ball g b in
      if b.y < -520. then begin
        Audio.play lost_sound;
        { g with ball = None; balls = g.balls -.. 1; hits = 0; reached_orange = false; reached_red = false }
      end
      else if cleared g.bricks && not g.second_wall then
        (* the second wall, the paddle back to its size *)
        { g with bricks = wall; ball = None; second_wall = true; shrunk = false }
      else { g with ball = Some b }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  match scenes.scene with
  | Title ->
      if serve_pressed scenes then { model with scenes = Scene2d.go (Playing (new_game computer.mouse.mx)) scenes }
      else { model with scenes }
  | Playing g ->
      let g = update_game computer scenes g in
      let hi_score = max model.hi_score g.score in
      if g.balls = 0 || cleared g.bricks then { hi_score; scenes = Scene2d.go (Game_over g.score) scenes }
      else { hi_score; scenes = { scenes with scene = Playing g } }
  | Game_over _ ->
      if serve_pressed scenes || scenes.elapsed > 10. then { model with scenes = Scene2d.go Title scenes }
      else { model with scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* a brick, drawn from its left tile: moved right by half a tile to be
 * centered on the two, and a bit smaller, to leave a gap around it *)
let view_bricks (map : Tilemap.t) : shape =
  Tilemap.view
    (fun c -> if c >= 'A' && c <= 'Z' then rectangle (color c) 46. 21. |> move_x 12.5 else group [])
    map
  |> move_y wall_y

let side_walls : shape list =
  [ rectangle gray 20. 900. |> move (field_left - 10.) (field_top - 450.);
    rectangle gray 20. 900. |> move (field_right + 10.) (field_top - 450.);
    rectangle gray (field_right - field_left + 40.) 20. |> move_y (field_top + 10.) ]

let view_game (g : game) : shape list =
  let ball_shape (x : number) (y : number) = square white ball_size |> move x y in
  side_walls
  @ [ view_bricks g.bricks; rectangle white (paddle_width g) paddle_height |> move g.paddle_x paddle_y ]
  @ (match g.ball with
    | Some b -> [ ball_shape b.x b.y ]
    | None -> [ ball_shape g.paddle_x (paddle_y + (paddle_height / 2.) + half) ])
  (* the balls left in reserve, the one in play or on the paddle excluded *)
  @ List.init (g.balls -.. 1) (fun i -> square white ball_size |> move (field_right - (float_of_int i * 25.)) 470.)

let header (model : model) (score : int) : shape list =
  [ text white 3. (Printf.sprintf "SCORE %03d" score) |> move (-250.) 470.;
    text white 3. (Printf.sprintf "HI %03d" model.hi_score) |> move 50. 470. ]

(* the title: the points of each color, bricks next to their value *)
let view_title (scenes : scene Scene2d.t) : shape list =
  [ text white 6. "TINY BREAKOUT" |> move_y 250.;
    text white 2.5 "left/right or the mouse: the paddle; space: serve" |> move_y 150. ]
  @ List.concat
      (List.mapi
         (fun i c ->
           let y = 60. - (float_of_int i * 50.) in
           [ rectangle (color c) 46. 21. |> move (-80.) y;
             text white 3. (Printf.sprintf "= %d POINT%s" (points c) (if points c = 1 then "" else "S")) |> move 60. y ])
         [ 'R'; 'O'; 'G'; 'Y' ])
  @ Scene2d.blink 1. scenes [ text white 3. "PRESS SPACE" |> move_y (-250.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let scenes = model.scenes in
  rectangle black screen.width screen.height
  ::
  (match scenes.scene with
  | Title -> header model 0 @ view_title scenes
  | Playing g -> header model g.score @ view_game g
  | Game_over score ->
      header model score
      @ [ text white 6. (if score >= 896 then "YOU WIN" else "GAME OVER") ]
      @ Scene2d.blink 1. scenes [ text white 3. "PRESS SPACE" |> move_y (-150.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
