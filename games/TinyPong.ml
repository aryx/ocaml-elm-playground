(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Pong (Atari, 1972: Allan Alcorn, for Nolan Bushnell), with a physics
 * engine instead of Pong's rules: the ball, the walls and the paddles
 * are bodies, and the ball does what the collisions say
 * (playground/Physics.mli's bounce, docs/claude_notes/notes_2d_physics.md
 * section 10):
 *
 *   ball |> step |> bounce_off top |> bounce_off bottom
 *        |> bounce_off left_paddle |> bounce_off right_paddle
 *
 * games/Pong.ml, the port of Elm's Pong, does it by hand: a paddle hit
 * reverses vx, the angle never changes. Here three of its TODOs come
 * for free, from the physics:
 *
 *  - a moving paddle drags the ball along (friction: both are [rough]):
 *    move the paddle while hitting to send the ball up or down;
 *  - a ball hitting a paddle's corner flies off at an angle (the
 *    contact's normal points from the corner to the ball's center);
 *  - the paddles are bumpers: a bounciness above 1 (1.05) gives the
 *    ball 5% more speed at every hit, so rallies speed up.
 *
 * One player: w and s, against the computer (the right paddle,
 * following the ball, at a limited speed). The flag players=2
 * (?players=2 on the web) gives the right paddle to a second player,
 * on the up and down arrows; the flag hitboxes draws what the physics
 * sees (Physics.debug).
 *
 * Tunneling: a body moving more than its collision zone per tick can
 * jump through a wall without ever touching it (see the notes, section
 * 9). The ball's speed is capped at 1100 pixels per second, 18 pixels a
 * tick, below the 44 of the paddle's zone (its 20 of width plus the
 * ball's radius on each side).
 *
 * Left as exercises: a winning score, sounds (plan_audio_teaching.md),
 * a spinning ball (rotation, phase 7 of plan_physics_teaching.md: then
 * the ball's spin would curve its bounces too).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

let white_rect w h = rectangle white w h

(* the field: 900 x 600, walls above and below *)
let top = Physics.body (white_rect 900. 20.) |> Physics.at 0. 310. |> Physics.immovable
let bottom = Physics.body (white_rect 900. 20.) |> Physics.at 0. (-310.) |> Physics.immovable

(* the paddles move by themselves (the players'), not by collisions *)
let paddle x = Physics.body (white_rect 20. 120.) |> Physics.at x 0. |> Physics.immovable |> Physics.bouncy 1.05 |> Physics.rough 0.5

let ball = Physics.body (circle white 12.) |> Physics.bouncy 1. |> Physics.rough 0.5

type game = {
  ball : Physics.body;
  left : Physics.body;
  right : Physics.body;
  (* frames before the serve, 0 when the ball is in play *)
  serving : int;
  serves : int;
  left_score : int;
  right_score : int;
}

type scene = Title | Playing of game

type model = scene Scene2d.t

(* the [n]th serve, towards the right ([dir] 1) or the left (-1): the
 * angles cycle, no Random *)
let serve (dir : number) (n : int) (g : game) : game =
  let angle = List.nth [ 20.; -30.; 10.; -15. ] (n mod 4) in
  { g with ball = ball |> Physics.launched 450. (if dir > 0. then angle else 180. - angle); serving = 60; serves = n +.. 1 }

let start_game : game =
  serve 1. 0 { ball; left = paddle (-420.); right = paddle 420.; serving = 60; serves = 0; left_score = 0; right_score = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* a paddle at [speed] pixels per second (up positive), kept in the
 * field; stopped at the edges, so that its velocity, which friction
 * gives the ball, is the real one *)
let move_paddle (speed : number) (p : Physics.body) : Physics.body =
  let p = p |> Physics.moving 0. speed |> Physics.step in
  if p.y > 240. then p |> Physics.at p.x 240. |> Physics.moving 0. 0.
  else if p.y < -240. then p |> Physics.at p.x (-240.) |> Physics.moving 0. 0.
  else p

let keys (up : bool) (down : bool) : number = (if up then 600. else 0.) - if down then 600. else 0.

(* the computer: towards the ball, at most 380 pixels per second *)
let computer_player (b : Physics.body) (p : Physics.body) : number = max (-380.) (min 380. ((b.y - p.y) * 6.))

let max_speed = 1100.

let capped (b : Physics.body) : Physics.body =
  let s = Physics.speed b in
  if s > max_speed then b |> Physics.moving (b.vx * max_speed / s) (b.vy * max_speed / s) else b

let update_game (computer : computer) (g : game) : game =
  let k = computer.keyboard in
  let two_players = List.assoc_opt "players" computer.flags = Some "2" in
  let left = move_paddle (keys k.kw k.ks) g.left in
  let right = move_paddle (if two_players then keys k.kup k.kdown else computer_player g.ball g.right) g.right in
  if g.serving > 0 then { g with left; right; serving = g.serving -.. 1 }
  else
    let ball =
      g.ball |> Physics.step
      |> Physics.bounce_off top |> Physics.bounce_off bottom
      |> Physics.bounce_off left |> Physics.bounce_off right
      |> capped
    in
    let g = { g with ball; left; right } in
    (* a point, and the next serve towards the one who lost it *)
    if ball.x < -470. then serve (-1.) g.serves { g with right_score = g.right_score +.. 1 }
    else if ball.x > 470. then serve 1. g.serves { g with left_score = g.left_score +.. 1 }
    else g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  match scenes.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) scenes then Scene2d.go (Playing start_game) scenes else scenes
  | Playing g -> { scenes with scene = Playing (update_game computer g) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (size : number) (s : string) : shape = words white s |> scale size

(* the net: a dashed line *)
let net : shape list = List.init 15 (fun i -> rectangle (rgb 120 120 120) 6. 20. |> move_y (280. - (40. * float_of_int i)))

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height :: List.map Physics.draw [ top; bottom ]
  @
  match model.scene with
  | Title ->
      [ text 6. "TINY PONG" |> move_y 150.;
        text 2. "w/s: your paddle; moving it while hitting drags the ball" |> move_y 50.;
        text 2. "(players=2: the right paddle on up/down)" |> move_y 10. ]
      @ Scene2d.blink 1. model [ text 3. "PRESS SPACE" |> move_y (-150.) ]
  | Playing g ->
      net
      @ List.map Physics.draw [ g.left; g.right; g.ball ]
      @ (if List.mem_assoc "hitboxes" computer.flags then List.map Physics.debug [ top; bottom; g.left; g.right; g.ball ] else [])
      @ [ text 5. (string_of_int g.left_score) |> move (-100.) 400.; text 5. (string_of_int g.right_score) |> move 100. 400. ]

let app = game view update initial_model
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
