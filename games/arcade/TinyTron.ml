(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Tron's light cycles (Bally Midway, 1982, after the
 * Disney film of the same year): two cycles race on a grid, each leaving
 * a wall of light behind it, and the first to crash -- into a wall, a
 * trail, its own included -- loses the round. First to 3 rounds wins.
 * Player 1 (blue) steers with the arrows; player 2 (orange) with
 * w/a/s/d, or it's the computer.
 *
 * The idea is older than the film: Blockade (Gremlin, 1976), the first
 * game of the kind, had two players leaving trails on a grid, and
 * Surround (Atari 2600, 1977) brought it home; Snake is its one-player
 * descendant (see Snake.ml). And it's a classic first clone: the
 * whole game is a grid, two positions, two directions, and one rule.
 *
 * The rules are the light cycles kit's (gamekits/lightcycles/, the model
 * and the update); this file is only the 2D view, and
 * TinyTron3d.ml the 3D one. The arena is a Tilemap of
 * characters (' ' free, '#' the walls, '1' and '2' the trails); the
 * trails are drawn with Sprite.pixels on its rows: a row's cells of the
 * same color are one rectangle, so a long trail across the arena costs
 * a few shapes, not a hundred.
 *
 * The computer (Lightcycles.computer_turn) looks at each way it can go,
 * and takes the one leading to the most room: the number of free cells
 * it could still reach from there (a flood fill), keeping straight on
 * when it's as good. Cutting the other player off from space is then
 * what it does without being told, and what it fears. The same idea
 * won the 2010 Google AI Challenge, whose game was Tron: its best bots
 * searched a few moves ahead (minimax), scoring positions by the cells
 * each player reaches first (a Voronoi partition of the arena) -- an
 * exercise.
 *)
open Playground
open Lightcycles (* the game: its rules, the computer, the scenes *)

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let blue = rgb 60 200 255
let orange = rgb 255 150 40

let text color size str = words color str |> scale size

(* [result]: the round's result, not on the winner's screen *)
let view_round ?(result = true) (g : game) : shape list =
  let r = g.round in
  let half = float_of_int size *. cell /. 2. in
  let grid_lines =
    List.concat
      (List.init 9 (fun i ->
           let x = -.half +. (float_of_int (i + 1) *. cell *. 9.) in
           [ rectangle (rgb 20 30 60) 1. (2. *. half) |> move_x x; rectangle (rgb 20 30 60) (2. *. half) 1. |> move_y x ]))
  in
  let trails =
    Sprite.pixels cell [ ('#', rgb 40 60 120); ('1', blue); ('2', orange) ] (Tilemap.to_strings r.arena)
  in
  let head (c : cycle) color =
    let x, y = Tilemap.center r.arena c.col c.row in
    square color (cell *. 1.6) |> move x y
  in
  let result =
    if not result then []
    else
    match r.over with
    | Some (1, 0) -> [ text blue 5. "BLUE WINS THE ROUND" ]
    | Some (0, 1) -> [ text orange 5. "ORANGE WINS THE ROUND" ]
    | Some _ -> [ text white 5. "BOTH CRASH" ]
    | None -> []
  in
  grid_lines @ [ trails; head r.p1 white; head r.p2 (rgb 255 230 180) ]
  @ [ text blue 3. (Printf.sprintf "BLUE %d" g.score1) |> move (-300.) 475.;
      text orange 3. (Printf.sprintf "%s %d" (if g.computer then "COMPUTER" else "ORANGE") g.score2) |> move 300. 475. ]
  @ result

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      [ text blue 8. "TINY TRON" |> move_y 250.;
        text white 3. "1: against the computer" |> move_y 60.;
        text white 3. "2: two players" |> move_y 0.;
        text gray 2.5 "blue: arrows   orange: w/a/s/d" |> move_y (-80.) ]
      @ Scene2d.blink 1. s [ text orange 3. "PRESS 1 OR 2" |> move_y (-200.) ]
  | Playing g -> view_round g
  | Winner g ->
      view_round ~result:false g
      @ [ (if g.score1 > g.score2 then text blue 7. "BLUE WINS!" else text orange 7. (if g.computer then "THE COMPUTER WINS!" else "ORANGE WINS!"))
          |> move_y 150. ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-150.) ])

let app = game view update initial_model

let main = Playground_platform.run_app app
