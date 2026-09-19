(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* games2.5d/TinyWolf.ml again, in real 3D: the same map, the same controls,
 * the same treasure, but each wall cell is a box, drawn by playground3d
 * like any 3D scene. Left/right to turn, up/down to walk.
 *
 * Put the two side by side (the golden frames of both are the same walk)
 * and the lesson is what 3D costs and buys. The raycaster draws one
 * rectangle per screen column, 200 of them whatever the map; here each
 * box is 6 faces, 12 triangles, projected, clipped, and filled pixel by
 * pixel with a z-buffer deciding what's in front (graphics/3d). In
 * exchange nothing is assumed: the walls could be of any height, the
 * floor could slope, the eye could look up and fly, rooms could be above
 * rooms -- everything Wolfenstein 3D (1992) couldn't do and Quake (1996)
 * could, on a Pentium, with a very clever software renderer.
 *
 * The walls never change: one cached3d group, built once (on the GPU
 * backends, kept in GPU buffers from frame to frame). The treasure is
 * real 3D too, cubes of gold turning, instead of billboards.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The map, and walking: as in games2.5d/TinyWolf.ml *)
(*****************************************************************************)

(* coupling: the same map as games2.5d/TinyWolf.ml's *)
let map =
  Tilemap.of_strings 1.
    [ "################";
      "#......#.......#";
      "#.$....#...$...#";
      "#......R.......#";
      "#..GG.....BB...#";
      "#..G......B....#";
      "#......@.......#";
      "#.....RRR......#";
      "#..$...........#";
      "###.####.#######";
      "#......#.......#";
      "#.$....#...$...#";
      "#......#.......#";
      "#..........B...#";
      "#......$.......#";
      "################" ]

let solid (c : char option) : bool = match c with Some ('.' | '$' | '@') -> false | _ -> true
let tile (x : number) (y : number) : char option = Tilemap.get map (int_of_float (floor x)) (int_of_float (floor y))

let wall_color (c : char) : color =
  match c with 'R' -> rgb 180 50 40 | 'G' -> rgb 40 150 60 | 'B' -> rgb 40 70 190 | _ -> rgb 130 130 140

(* x and y on the map, as in TinyWolf; in 3D, the map's y is the
 * world's z, and the world's y is up *)
type model = { x : number; y : number; angle : number; treasures : (number * number) list; found : int }

let initial_model : model =
  let col, row = match Tilemap.find map '@' with p :: _ -> p | [] -> (1, 1) in
  { x = float_of_int col +. 0.5; y = float_of_int row +. 0.5; angle = -90.;
    treasures = List.map (fun (c, r) -> (float_of_int c +. 0.5, float_of_int r +. 0.5)) (Tilemap.find map '$');
    found = 0 }

let walk (m : model) (dx : number) (dy : number) : model =
  let r = 0.2 in
  let free x y = not (solid (tile x y)) in
  let x = if free (m.x +. dx +. (if dx > 0. then r else -.r)) m.y then m.x +. dx else m.x in
  let y = if free x (m.y +. dy +. (if dy > 0. then r else -.r)) then m.y +. dy else m.y in
  { m with x; y }

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  let angle = m.angle +. (if k.kleft then -2.5 else 0.) +. if k.kright then 2.5 else 0. in
  let a = angle *. Float.pi /. 180. in
  let speed = (if k.kup then 0.06 else 0.) -. if k.kdown then 0.04 else 0. in
  let m = walk { m with angle } (speed *. cos a) (speed *. sin a) in
  let near (tx, ty) = Float.hypot (tx -. m.x) (ty -. m.y) < 0.5 in
  let taken, treasures = List.partition near m.treasures in
  { m with treasures; found = m.found + List.length taken }

(*****************************************************************************)
(* The 3D world *)
(*****************************************************************************)

(* every wall cell a 1x1x1 box standing on the floor, the floor and the
 * ceiling (a plane turned over, so that its face looks down at us) *)
let world : shape3d =
  let cols = Tilemap.cols map and rows = Tilemap.rows map in
  let walls =
    List.concat
      (List.init rows (fun r ->
           List.init cols (fun c ->
               match Tilemap.get map c r with
               | cell when solid cell ->
                   [ box (wall_color (Option.value cell ~default:'#')) 1. 1. 1.
                     |> move3d (float_of_int c +. 0.5) 0.5 (float_of_int r +. 0.5) ]
               | _ -> [])
           |> List.concat))
  in
  let w = float_of_int cols and d = float_of_int rows in
  cached3d
    ([ plane (rgb 110 100 90) w d |> move3d (w /. 2.) 0. (d /. 2.);
       plane (rgb 60 60 70) w d |> rotate3d 180. 0. 0. |> move3d (w /. 2.) 1. (d /. 2.) ]
    @ walls)

let view (computer : computer) (m : model) : camera * shape3d list =
  let a = m.angle *. Float.pi /. 180. in
  (* the eye halfway up the walls, like the raycaster's; the field of
   * view of its camera plane, 0.66 each side for 1 ahead, i.e. 2 atan
   * 0.66, 67 degrees (on a square screen, across and up alike) *)
  let eye = (m.x, 0.5, m.y) in
  let fov = 2. *. atan 0.66 *. 180. /. Float.pi in
  let cam = camera ~eye ~target:(m.x +. cos a, 0.5, m.y +. sin a) ~fov () in
  let turning = spin 3. computer.time in
  let treasures =
    List.map (fun (tx, ty) -> cube (rgb 255 210 40) 0.25 |> rotate3d 0. turning 0. |> move3d tx 0.25 ty) m.treasures
  in
  let total = m.found + List.length m.treasures in
  let screen = computer.screen in
  let huds =
    [ hud (words white (Printf.sprintf "TREASURE %d / %d" m.found total) |> scale 3. |> move (screen.right -. 220.) (screen.top -. 40.)) ]
    @ if m.treasures = [] then [ hud (words yellow "ALL THE TREASURE IS YOURS!" |> scale 4.) ] else []
  in
  (cam, (world :: treasures) @ huds)

let app = game3d view update initial_model

let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
