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
 * Player 1 (blue) steers with the arrows and boosts with space; player
 * 2 (orange) with w/a/s/d and e, or it's the computer. Two riders, or
 * four -- then the last one riding wins -- on the open grid or around
 * the obstacles of the arenas that follow it round after round.
 *
 * The idea is older than the film: Blockade (Gremlin, 1976), the first
 * game of the kind, had two players leaving trails on a grid, and
 * Surround (Atari 2600, 1977) brought it home; Snake is its one-player
 * descendant (see Snake.ml). And it's a classic first clone: the
 * whole game is a grid, two positions, two directions, and one rule.
 *
 * The rules are the light cycles kit's (gamekits/lightcycles/, the model
 * and the update); this file is its title (how many riders, how well
 * the computer plays) and the 2D view, and TinyTron3d.ml the 3D one. The arena is a Tilemap of
 * characters (' ' free, '#' the walls, '1' and '2' the trails); the
 * trails are drawn with Sprite.pixels on its rows: a row's cells of the
 * same color are one rectangle, so a long trail across the arena costs
 * a few shapes, not a hundred.
 *
 * The computer, at EASY (Lightcycles.computer_turn), looks at each way
 * it can go, and takes the one leading to the most room: the number of
 * free cells it could still reach from there (a flood fill), keeping
 * straight on when it's as good. Cutting the other player off from
 * space is then what it does without being told, and what it fears.
 * (At EASY it counts only 40 cells of room.) At HARD
 * (Lightcycles.search_turn) it does what won the 2010 Google AI
 * Challenge, whose game was Tron: it searches two moves each ahead
 * with alpha-beta (ai/'s Minimax), against the nearest rider, scoring
 * a position by the cells each reaches first (a Voronoi partition of
 * the arena) and the room each has left. The difference shows at the
 * end of a round: the flood fill
 * notices it is being walled in when there is no way out any more;
 * the search sees the wall coming, and races for the door.
 *
 * Exercises: the computer boosting (when is a second of speed worth
 * four of waiting?), a search looking at all the riders at once (the
 * "paranoid" search: everyone against me), the arenas that shrink.
 *)
open Playground
open Lightcycles (* the game: its rules, the computers *)

(*****************************************************************************)
(* The title's choices *)
(*****************************************************************************)

type level = Easy | Normal | Hard

(* the computer at each level: the flood fill short-sighted (it counts
 * 40 cells of room at most: a trap is seen late), the flood fill, and
 * the search two moves each ahead. Measured, in duels on the three
 * arenas: the search at that depth beat the flood fill six times out
 * of six; one move each ahead it only drew with it, and three moves
 * ahead it did worse than two, at twice the cost -- looking further
 * mostly looks further into the score's own blind spots *)
let brain_of = function Easy -> Room 40 | Normal -> Room 600 | Hard -> Search 4

type scene = Title of int * level (* riders *) | Playing of game | Winner of game
type model = scene Scene2d.t

let initial_model : model = Scene2d.start (Title (4, Normal))

let settings (riders : int) (level : level) (humans : int) : settings =
  { riders; humans; brain = brain_of level; arenas = layouts }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let key name = Scene2d.pressed (fun k -> Set_.mem name k.keys) s in
  let pressed f = Scene2d.pressed f s in
  match s.scene with
  | Title (riders, level) ->
      let riders = if pressed (fun k -> k.kup) then 4 else if pressed (fun k -> k.kdown) then 2 else riders in
      let levels = [| Easy; Normal; Hard |] in
      let i = match level with Easy -> 0 | Normal -> 1 | Hard -> 2 in
      let i = if pressed (fun k -> k.kleft) then max 0 (i - 1) else if pressed (fun k -> k.kright) then min 2 (i + 1) else i in
      if key "1" then Scene2d.go (Playing (new_game (settings riders levels.(i) 1))) s
      else if key "2" then Scene2d.go (Playing (new_game (settings riders levels.(i) 2))) s
      else { s with scene = Title (riders, levels.(i)) }
  | Playing g ->
      let g = update_game computer.keyboard g in
      if winner g <> None then Scene2d.go (Winner g) s else { s with scene = Playing g }
  | Winner g ->
      if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (Title (g.settings.riders, Normal)) s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let blue = rgb 60 200 255
let orange = rgb 255 150 40
let green = rgb 90 230 110
let pink = rgb 240 90 200
let colors = [| blue; orange; green; pink |]
let names = [| "BLUE"; "ORANGE"; "GREEN"; "PINK" |]

let text color size str = words color str |> scale size

(* who a rider is on the scoreboard: its color, or "COMPUTER" in a duel
 * against it *)
let name (g : game) (i : int) : string = if g.settings.riders = 2 && i = 1 && g.settings.humans = 1 then "COMPUTER" else names.(i)

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
    Sprite.pixels cell
      [ ('#', rgb 40 60 120); ('1', blue); ('2', orange); ('3', green); ('4', pink) ]
      (Tilemap.to_strings r.arena)
  in
  let head (c : cycle) =
    let x, y = Tilemap.center r.arena c.col c.row in
    if c.alive then [ square (if c.boosting then yellow else white) (cell *. 1.6) |> move x y ] else []
  in
  let result =
    if not result then []
    else
      match r.over with
      | Some points -> (
          match List.find_map (fun (i, p) -> if p = 1 then Some i else None) (List.mapi (fun i p -> (i, p)) points) with
          | Some i -> [ text colors.(i) 5. (name g i ^ " WINS THE ROUND") ]
          | None -> [ text white 5. (if g.settings.riders = 2 then "BOTH CRASH" else "ALL CRASH") ])
      | None -> []
  in
  (* the scoreboard, and under each human's score its boost *)
  let n = g.settings.riders in
  let scores =
    List.concat
      (List.mapi
         (fun i (c : cycle) ->
           let x = (float_of_int i -. (float_of_int (n - 1) /. 2.)) *. (if n = 2 then 600. else 240.) in
           [ text colors.(i) 3. (Printf.sprintf "%s %d" (name g i) (List.nth g.scores i)) |> move x 475. ]
           @
           if i < g.settings.humans then
             let w = 120. *. float_of_int c.energy /. float_of_int boost_max in
             [ rectangle (rgb 40 40 60) 120. 6. |> move x 452.; rectangle colors.(i) w 6. |> move (x -. 60. +. (w /. 2.)) 452. ]
           else [])
         r.cycles)
  in
  grid_lines @ [ trails ] @ List.concat_map head r.cycles @ scores
  @ [ text gray 2. (Printf.sprintf "ROUND %d -- %s" g.round_no (fst (List.nth g.settings.arenas ((g.round_no - 1) mod List.length g.settings.arenas)))) |> move_y (-475.) ]
  @ result

let level_name = function Easy -> "EASY" | Normal -> "NORMAL" | Hard -> "HARD"

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title (riders, level) ->
      let chosen b = if b then yellow else gray in
      [ text blue 8. "TINY TRON" |> move_y 300.;
        text (chosen (riders = 4)) 3. "4 RIDERS" |> move_y 170.;
        text (chosen (riders = 2)) 3. "2 RIDERS" |> move_y 120. ]
      @ List.mapi
          (fun i l -> text (chosen (l = level)) (if l = level then 2.8 else 2.4) (level_name l) |> move ((float_of_int i -. 1.) *. 180.) 40.)
          [ Easy; Normal; Hard ]
      @ [ text gray 2.2 "up / down: riders   left / right: the computer's level" |> move_y (-20.);
          text white 3. "1: against the computer" |> move_y (-90.);
          text white 3. "2: two players" |> move_y (-140.);
          text gray 2.5 "blue: arrows, space boost   orange: w/a/s/d, e boost" |> move_y (-210.) ]
      @ Scene2d.blink 1. s [ text orange 3. "PRESS 1 OR 2" |> move_y (-290.) ]
  | Playing g -> view_round g
  | Winner g ->
      let w = Option.value (winner g) ~default:0 in
      view_round ~result:false g
      @ [ text colors.(w) 7. (if w = 0 then "BLUE WINS!" else if name g w = "COMPUTER" then "THE COMPUTER WINS!" else names.(w) ^ " WINS!") |> move_y 150. ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-150.) ])

let app = game view update initial_model

let main = Playground_platform.run_app app
