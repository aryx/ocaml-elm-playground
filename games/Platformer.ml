(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A little Super Mario Bros.: a level bigger than the screen, typed as
 * strings (see Tilemap), seen through a camera following the player (see
 * Camera2d), with parallax hills and clouds behind, and a HUD in front.
 * Arrows to run, up to jump; take the coins ('$'), bump the '?' blocks
 * from below, reach the flag ('F').
 *
 * Two flags (see Playground.flags) to compare the ways a camera can
 * follow the player (see Camera2d.mli), and to see more of the level:
 *
 *   dune exec games/Platformer.exe -- camera=lock zoom=0.5
 *   http://localhost:8001/games/js/Platformer.html?camera=lerp
 *
 * camera= is window (the default), lock, or lerp. *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The level *)
(*****************************************************************************)

(* '#' ground, 'B' a brick, '?' a block with a coin inside ('X' once
 * bumped), '$' a coin, 'F' the flag, '@' where the player starts *)
let level =
  Tilemap.of_strings 50.
    [
      "                                                            ";
      "                                                            ";
      "                                                 $$$        ";
      "                                                #####       ";
      "                                                            ";
      "                                                            ";
      "                                                     ###    ";
      "                                                            ";
      "                                                            ";
      "                                             $ $            ";
      "                                            #######         ";
      "                                                            ";
      "                                     $                      ";
      "                                   #####                    ";
      "                                                            ";
      "                              $ $                           ";
      "                             #####                          ";
      "                                                            ";
      "               ?B?B?                    $$$                 ";
      "                                       #####                ";
      "                                                            ";
      "      $  $                   #                    #         ";
      "  @         #       $$     # #       $            #      F  ";
      "####################  ###########   ######   ###############";
      "####################  ###########   ######   ###############";
    ]

let solid (c : char) : bool = c = '#' || c = 'B' || c = '?' || c = 'X'

let tile (c : char) : shape =
  match c with
  | '#' -> group [ square (rgb 120 60 20) 50.; square (rgb 160 90 40) 42. ]
  | 'B' -> group [ square (rgb 180 80 30) 50.; rectangle (rgb 90 40 10) 50. 4. ]
  | '?' -> group [ square (rgb 240 180 30) 50.; words black "?" |> scale 3. ]
  | 'X' -> square (rgb 130 100 60) 50.
  | '$' -> oval yellow 20. 30.
  | 'F' ->
      group
        [ rectangle darkGray 6. 150. |> move_y 50.;
          triangle green 30. |> rotate (-90.) |> move 20. 100. ]
  | _ -> group []

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* the player is a 40x40 box, smaller than a tile, so it fits in a
 * one-tile gap *)
let player_size = 40.

type model = {
  map : Tilemap.t; (* changes when a coin is taken, a block bumped *)
  x : number;
  y : number;
  vy : number;
  coins : int;
  won : bool;
  cam : Camera2d.t;
}

let start : number * number =
  match Tilemap.find level '@' with
  | (col, row) :: _ -> Tilemap.center level col row
  | [] -> (0., 0.)

let initial_model =
  let x, y = start in
  let col, row = Tilemap.cell level x y in
  { map = Tilemap.set level col row ' ';
    x; y; vy = 0.; coins = 0; won = false;
    cam = Camera2d.origin |> Camera2d.look_at x y }

(*****************************************************************************)
(* Moving against the tiles *)
(*****************************************************************************)

let blocked (map : Tilemap.t) (x : number) (y : number) : bool =
  Tilemap.hits solid map x y player_size player_size

(* Move (x, y) by (dx, dy), at most one pixel at a time, stopping before
 * the first step entering a solid tile; returns where we stopped, and
 * whether we hit something. That's how Celeste and TowerFall move their
 * characters (Maddy Thorson, "Celeste and TowerFall Physics", 2017): slow
 * in theory, but a character moves only a few pixels per frame, and it
 * can't go through a thin wall even when fast (a big step could jump
 * over it: the "tunneling" of physics engines). Called once for x, then
 * once for y, so that running into a wall while falling stops only the
 * running, and the player slides down along the wall. *)
let move_by (map : Tilemap.t) (x, y) (dx, dy) : (number * number) * bool =
  let n = int_of_float (ceil (Float.abs dx + Float.abs dy)) in
  let rec go i (x, y) =
    if i >= n then ((x, y), false)
    else
      let x' = x + (dx / float_of_int n) and y' = y + (dy / float_of_int n) in
      if blocked map x' y' then ((x, y), true) else go (succ i) (x', y')
  in
  go 0 (x, y)

(* the tile over the player's head, bumped from below: a '?' gives a coin *)
let bump (model : model) : model =
  let col, row = Tilemap.cell model.map model.x (model.y + (player_size / 2.) + 1.) in
  match Tilemap.get model.map col row with
  | Some '?' -> { model with map = Tilemap.set model.map col row 'X'; coins = succ model.coins }
  | _ -> model

(* the tile the player's center is in: a coin, the flag *)
let touch (model : model) : model =
  let col, row = Tilemap.cell model.map model.x model.y in
  match Tilemap.get model.map col row with
  | Some '$' -> { model with map = Tilemap.set model.map col row ' '; coins = succ model.coins }
  | Some 'F' -> { model with won = true }
  | _ -> model

(*****************************************************************************)
(* The camera *)
(*****************************************************************************)

let flag (computer : computer) (name : string) : string option = List.assoc_opt name computer.flags

(* see Camera2d.mli for these three ways, and the camera= flag above *)
let move_camera (computer : computer) (model : model) : Camera2d.t =
  let zoom = Option.value (Option.bind (flag computer "zoom") float_of_string_opt) ~default:1. in
  let cam = { model.cam with zoom } in
  let cam =
    match flag computer "camera" with
    | Some "lock" -> Camera2d.look_at model.x model.y cam
    | Some "lerp" -> Camera2d.follow 0.1 model.x model.y cam
    | _ -> Camera2d.window 200. 300. model.x model.y cam
  in
  Camera2d.clamp computer.screen (Tilemap.bounds model.map) cam

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (computer : computer) (model : model) : model =
  let on_ground = blocked model.map model.x (model.y - 1.) in
  let vx = 6. * to_x computer.keyboard in
  let vy =
    if on_ground && computer.keyboard.kup then 19.
    else max (-15.) (model.vy - 0.8) (* gravity, and a terminal speed *)
  in
  let (x, _), _ = move_by model.map (model.x, model.y) (vx, 0.) in
  let (x, y), hit = move_by model.map (x, model.y) (0., vy) in
  let model = { model with x; y; vy = (if hit then 0. else vy) } in
  let model = if hit && vy > 0. then bump model else model in
  let model = touch model in
  (* fallen in a pit: back to the start *)
  let model =
    if model.y < (Tilemap.bounds model.map).bottom - 200. then
      { model with x = fst start; y = snd start; vy = 0. }
    else model
  in
  { model with cam = move_camera computer model }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let hills = List.init 8 (fun i -> oval (rgb 90 170 80) 500. 300. |> move ((float_of_int i * 400.) - 1400.) (-450.))
let clouds = List.init 8 (fun i -> oval white 160. 60. |> move ((float_of_int i * 350.) - 1300.) (250. + (float_of_int (i mod 3) * 60.)))

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let cam = model.cam in
  let world =
    [ Tilemap.view_visible (Camera2d.visible screen cam) tile model.map;
      square red player_size |> move model.x model.y ]
  in
  let hud =
    [ words black (Printf.sprintf "coins: %d" model.coins) |> scale 3. |> move (screen.left + 120.) (screen.top - 40.);
      words black ("camera: " ^ Option.value (flag computer "camera") ~default:"window")
      |> scale 2. |> move (screen.right - 150.) (screen.top - 40.) ]
    @ if model.won then [ words red "You win!" |> scale 8. ] else []
  in
  (* the sky, glued to the screen, then layers further and further *)
  [ rectangle (rgb 174 238 238) screen.width screen.height;
    Camera2d.view (Camera2d.parallax 0.3 cam) hills;
    Camera2d.view (Camera2d.parallax 0.6 cam) clouds;
    Camera2d.view cam world ]
  @ hud

let app = game view update initial_model

let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
