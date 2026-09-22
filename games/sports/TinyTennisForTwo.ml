(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Tennis for Two (William Higinbotham and Robert
 * Dvorak, Brookhaven National Laboratory, 1958): tennis seen from the
 * side, on an oscilloscope.
 *
 *   left player   w s   turn the knob (the angle of the shot)
 *                 d     hit
 *   right player  up down, and left to hit -- or the computer, until
 *                 2 is pressed (1 gives it back)
 *
 * For the laboratory's visitors' day of October 1958, Higinbotham, a
 * physicist of the Manhattan Project, put a game on the analog
 * computer that plotted missile trajectories: the ball's path is what
 * the machine was built to compute, the net and the court two lines on
 * a five-inch oscilloscope, and each player a box with a knob and a
 * button. People queued to play it; it was taken apart after the next
 * year's visitors' day, and never patented. It is the first video game
 * made to entertain, four years before Spacewar!, and Pong (1972) is
 * the same game seen from above, with the gravity taken out. (Names
 * and dates from memory, to check.)
 *
 * The whole game is a ballistic trajectory and its bounces
 * ([fly]), which is why it was easy on a computer made for ballistics:
 *
 *                 . - - .
 *              .           .      each frame: vy -= gravity; the
 *           .      |        .     ball moves by (vx, vy); on the
 *        o         |net       o   court it bounces, vy turned back and
 *   ===============+==============     both slowed ([bounce_keep]);
 *        left            right         against the net it drops dead
 *
 * The rules are the ones two players at the knobs would agree on --
 * the original kept no score at all:
 *
 *   - a player may hit only while the ball is on their side, once
 *     (in the air too: the volley was the game's great trick);
 *   - the shot's angle is the knob's; its speed is fixed;
 *   - the point is lost by the side where the ball bounces twice, by
 *     the one whose shot stops in the net or bounces back on their own
 *     side, and, for a shot out, by the hitter if it never bounced on
 *     the other side, by the receiver if it did.
 *
 * The oscilloscope: a dot moving fast enough draws a line (the
 * phosphor glows on a moment after the beam has passed), which is how
 * the court and the net were drawn on the same screen as the ball, and
 * why the ball leaves a trail ([trail]).
 *
 * What it uses: Scene2d. Not Physics: one point under gravity is a
 * few lines, and a physics engine's ball would roll on the court where
 * this one's bounces must stop. Not gamekits/sports' Free_ball: seen
 * from the side, the ball's height is simply its y, and the
 * shadow-and-z of TinySensibleSoccer (seen from above) are not needed.
 *
 * Left undone, exercises: the air's drag, which the original's analog
 * computer had and a knob to turn it on; a court on the Moon or on
 * Jupiter (a different gravity: the 1959 version had that switch);
 * two knobs per player as Pong would later have, for the height of the
 * racket.
 *)
open Playground

(*****************************************************************************)
(* The court *)
(*****************************************************************************)

let half = 400. (* the court from -half to half *)
let ground = -150.
let net_height = 50.
let gravity = 0.22
let shot_speed = 10.
let bounce_keep = 0.7 (* of the vertical speed, after a bounce *)

type side = Left | Right

let other (s : side) : side = match s with Left -> Right | Right -> Left
let side_of (x : float) : side = if x < 0. then Left else Right

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type ball = {
  x : float;
  y : float;
  vx : float;
  vy : float;
  hitter : side; (* who hit it last (or serves it) *)
  crossed : bool; (* over the net since the last hit *)
  bounces : int; (* on the side it is on, since it got there *)
  hit_here : bool; (* the side it is on has hit it already *)
}

type rally = Flying | Point of side * int (* won by, frames left of the pause *)

type game = {
  ball : ball;
  knob_l : float; (* degrees above the horizontal *)
  knob_r : float;
  score_l : int;
  score_r : int;
  server : side;
  rally : rally;
  computer : bool; (* the right player *)
  trail : (float * float) list; (* the ball's last positions, newest first *)
}

let serve (s : side) : ball =
  let x = if s = Left then -.half +. 60. else half -. 60. in
  { x; y = ground +. 110.; vx = 0.; vy = 0.; hitter = s; crossed = true; bounces = 0; hit_here = false }

let start : game =
  { ball = serve Left; knob_l = 45.; knob_r = 45.; score_l = 0; score_r = 0; server = Left; rally = Flying; computer = true;
    trail = [] }

(*****************************************************************************)
(* The flight, and the rules *)
(*****************************************************************************)

(* A shot: the fixed speed, the knob's angle, towards the other side. *)
let hit (s : side) (knob : float) (b : ball) : ball =
  let a = knob *. Float.pi /. 180. in
  let dir = if s = Left then 1. else -1. in
  { b with vx = dir *. shot_speed *. Float.cos a; vy = shot_speed *. Float.sin a; hitter = s; crossed = false; hit_here = true;
    bounces = 0 }

(* whether side [s] may hit now: on its side, not hit there yet *)
let may_hit (s : side) (b : ball) : bool = side_of b.x = s && not b.hit_here

(* One frame of flight: the trajectory, the net, the bounces; and the
 * point, if one is over (Some winner). *)
let fly (b : ball) : ball * side option =
  let vy = b.vy -. gravity in
  let x = b.x +. b.vx and y = b.y +. vy in
  (* the net: crossing x = 0 below its top stops the ball dead *)
  if (b.x < 0.) <> (x < 0.) && y < ground +. net_height then ({ b with vx = 0.; vy = 0.; x = (if b.x < 0. then -1. else 1.) }, Some (other b.hitter))
  else
    let b =
      if (b.x < 0.) <> (x < 0.) then { b with x; y; vy; crossed = true; bounces = 0; hit_here = false } else { b with x; y; vy }
    in
    if Float.abs b.x > half +. 20. then
      (* out: good for the hitter if it had bounced over there *)
      (b, Some (if b.crossed && b.bounces >= 1 then b.hitter else other b.hitter))
    else if b.y <= ground && b.vy < 0. then
      let b = { b with y = ground; vy = -.b.vy *. bounce_keep; vx = b.vx *. 0.9; bounces = b.bounces + 1 } in
      let here = side_of b.x in
      if (not b.crossed) && here = b.hitter && b.vx <> 0. then (b, Some (other b.hitter)) (* back on its own side *)
      else if b.bounces >= 2 then (b, Some (other here))
      else (b, None)
    else (b, None)

(* The computer, on the right: serves at once; in a rally, waits for
 * the ball to come down to a good height, then hits it, steeper the
 * nearer the net. *)
let computer_play (b : ball) : float * bool =
  let knob = 30. +. (40. *. (1. -. (b.x /. half))) in
  let serving = b.vx = 0. && b.vy = 0. in
  (knob, may_hit Right b && (serving || (b.vy < 0. && b.y < ground +. 90. && b.x > 60.)))

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* what the players do this frame: each knob turned (-1, 0, 1) and each
 * button pressed *)
type input = { turn_l : float; hit_l : bool; turn_r : float; hit_r : bool }

let nothing = { turn_l = 0.; hit_l = false; turn_r = 0.; hit_r = false }
let clamp_knob (k : float) : float = Float.max 5. (Float.min 85. k)

let step (i : input) (g : game) : game =
  match g.rally with
  | Point (winner, 0) ->
      let server = other g.server in
      { g with rally = Flying; server; ball = serve server; trail = [];
               score_l = (g.score_l + if winner = Left then 1 else 0); score_r = (g.score_r + if winner = Right then 1 else 0) }
  | Point (winner, n) -> { g with rally = Point (winner, n - 1) }
  | Flying ->
      let knob_r, hit_r = if g.computer then computer_play g.ball else (clamp_knob (g.knob_r +. i.turn_r), i.hit_r) in
      let g = { g with knob_l = clamp_knob (g.knob_l +. i.turn_l); knob_r } in
      let b = g.ball in
      let b = if i.hit_l && may_hit Left b then hit Left g.knob_l b else if hit_r && may_hit Right b then hit Right g.knob_r b else b in
      (* the ball waits in the server's hand until it is hit *)
      let waiting = b.vx = 0. && b.vy = 0. && b.hit_here = false && b.crossed in
      if waiting then { g with ball = b }
      else
        let b, over = fly b in
        let trail = List.filteri (fun k _ -> k < 14) ((b.x, b.y) :: g.trail) in
        match over with Some winner -> { g with ball = b; trail; rally = Point (winner, 60) } | None -> { g with ball = b; trail }

type model = game Scene2d.t

let initial_model : model = Scene2d.start start

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  let key k = Set_.mem k computer.keyboard.keys in
  let k = computer.keyboard in
  let g = scenes.scene in
  let g = if pressed (fun k -> Set_.mem "2" k.keys) then { g with computer = false } else if pressed (fun k -> Set_.mem "1" k.keys) then { g with computer = true } else g in
  let i =
    { turn_l = (if key "w" then 1. else if key "s" then -1. else 0.); hit_l = pressed (fun k -> Set_.mem "d" k.keys);
      turn_r = (if k.kup then 1. else if k.kdown then -1. else 0.); hit_r = pressed (fun k -> k.kleft) }
  in
  { scenes with scene = step i g }

(*****************************************************************************)
(* View: the oscilloscope *)
(*****************************************************************************)

let phosphor = rgb 120 255 170

(* a line of the beam: a thin rectangle from (x1, y1) to (x2, y2) *)
let beam (x1 : float) (y1 : float) (x2 : float) (y2 : float) : shape =
  let len = Float.hypot (x2 -. x1) (y2 -. y1) in
  rectangle phosphor len 3. |> rotate (Float.atan2 (y2 -. y1) (x2 -. x1) *. 180. /. Float.pi) |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let knob_shape (angle : float) : shape =
  let a = angle *. Float.pi /. 180. in
  group [ circle (rgb 60 60 60) 28.; circle (rgb 30 30 30) 22.; beam 0. 0. (22. *. Float.cos a) (22. *. Float.sin a) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let g = model.scene in
  let trail = List.mapi (fun k (x, y) -> circle phosphor (5. -. (float_of_int k *. 0.25)) |> fade (0.6 *. (1. -. (float_of_int k /. 14.))) |> move x y) g.trail in
  [ rectangle (rgb 40 42 48) screen.width screen.height;
    (* the round screen of the oscilloscope, in its box *)
    rectangle (rgb 90 90 95) 1000. 640.;
    circle (rgb 5 18 10) 440.;
    beam (-.half) ground half ground;
    beam 0. ground 0. (ground +. net_height) ]
  @ trail
  @ [ circle phosphor 5. |> move g.ball.x g.ball.y;
      knob_shape g.knob_l |> move (-.half) (-.290.);
      knob_shape (180. -. g.knob_r) |> move half (-.290.);
      words phosphor (Printf.sprintf "%d" g.score_l) |> scale 3. |> move (-.200.) 200.;
      words phosphor (Printf.sprintf "%d" g.score_r) |> scale 3. |> move 200. 200.;
      words (rgb 200 200 200) "w s knob, d hit" |> scale 1.4 |> move (-.(half -. 60.)) (-.340.);
      words (rgb 200 200 200) (if g.computer then "computer (2: a player)" else "up down knob, left hit") |> scale 1.4 |> move (half -. 60.) (-.340.) ]
  @ match g.rally with Point (w, _) -> [ words phosphor (if w = Left then "point, left" else "point, right") |> scale 2. |> move_y 120. ] | Flying -> []

let help = {|TinyTennisForTwo
  left player:  w s turn the knob (the shot's angle), d hits
  right player: the computer; press 2 for a second player (up down, left hits), 1 for the computer again
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
