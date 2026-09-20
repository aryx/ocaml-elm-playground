(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Wolfenstein 3D (id Software, 1992), in the *2D*
 * playground: a first-person walk through a maze, looking for the
 * treasure. Left/right to turn, up/down to walk.
 *
 * There is no 3D here at all -- that's the point. The world is a grid
 * of walls (a Tilemap), every wall is as high as the others, and the
 * eye is always halfway up them; then what a column of the screen shows
 * is only a question of distance: how far, along that column's ray, is
 * the first wall? The wall's slice is drawn as one rectangle, as tall as
 * 1 / distance ([cast], then [view_walls]):
 *
 *     the map, from above               the screen
 *      #########                      |   |||   |
 *      #   \|/ #   one ray per        |  |||||  |   near walls: tall
 *      #    @  #   screen column      | ||||||| |   far walls: short
 *
 * John Carmack's raycaster ran at full speed on a 286 because of this:
 * 320 rays per frame, each walking the grid a cell at a time, instead of
 * polygons. Its limits are its rules: no stairs, no slopes, no rooms
 * above rooms, walls only at right angles -- Doom (1993) lifted some,
 * with sectors, and Quake (1996) all of them, with real 3D. Compare with
 * games3d/TinyWolfenstein3d.ml: the same map, as boxes, drawn by the 3D
 * rasterizer, with a z-buffer.
 *
 * The walk through the grid is the DDA of Lode Vandevenne's classic
 * tutorial (lodev.org/cgtutor/raycasting.html), the same as John
 * Amanatides and Andrew Woo's "A Fast Voxel Traversal Algorithm for Ray
 * Tracing" (1987), which TinyMinecraft's block picking also uses, in 3D.
 * Before Wolfenstein, the same idea gave Hovertank 3D and Catacomb 3-D
 * (id, 1991), and, at a whole cell per step, 3D Monster Maze (1981).
 *
 * The treasure is drawn like Wolfenstein's objects, as "billboards",
 * flat pictures always facing you, sized by distance, and hidden by the
 * walls column by column: the rays' distances ([hits] in [view]) are a
 * one-dimensional z-buffer.
 *
 * Exercises: textured walls (Lode's part II: which column of the
 * texture a ray hits is the fractional part of the hit point), doors,
 * enemies (billboards that move), floor and ceiling textures.
 *)
open Playground

(*****************************************************************************)
(* The map *)
(*****************************************************************************)

(* the walls, by their color ('#', 'R', 'G', 'B'), the floor '.', the
 * treasure '$', and where you start, '@' (coupling: the same map is in
 * games3d/TinyWolfenstein3d.ml) *)
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

(* positions are in cells, from the map's top-left corner, x to the
 * right, y *down* (the rows' order); the cell (col, row) is the square
 * from (col, row) to (col + 1, row + 1) *)
let tile (x : number) (y : number) : char option = Tilemap.get map (int_of_float (floor x)) (int_of_float (floor y))

let wall_rgb (c : char) : int * int * int =
  match c with 'R' -> (180, 50, 40) | 'G' -> (40, 150, 60) | 'B' -> (40, 70, 190) | _ -> (130, 130, 140)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  x : number;
  y : number;
  angle : number; (* degrees, 0 looking right (+x), 90 down (+y) *)
  treasures : (number * number) list; (* the ones left *)
  found : int;
}

let initial_model : model =
  let col, row = match Tilemap.find map '@' with p :: _ -> p | [] -> (1, 1) in
  { x = float_of_int col +. 0.5; y = float_of_int row +. 0.5; angle = -90.;
    treasures = List.map (fun (c, r) -> (float_of_int c +. 0.5, float_of_int r +. 0.5)) (Tilemap.find map '$');
    found = 0 }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* walking, one axis at a time, so that walking into a wall at an angle
 * slides along it; [r] keeps the eye a little away from the walls (else
 * a wall right in front of the eye fills the screen) *)
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
(* Raycasting -- the trick of this game, in 61 lines (see the header) *)
(*****************************************************************************)

(* the field of view: the "camera plane", perpendicular to where we
 * look, 0.66 long each side for 1 ahead: about 66 degrees, Lode's (and
 * Wolfenstein's, roughly) *)
let plane_length = 0.66

(* where a ray hit: the distance, perpendicular to the camera plane
 * (so no "fisheye": a straight wall looks straight), whether it hit a
 * vertical side of a cell (an x side) or a horizontal one, and the wall *)
type hit = { dist : number; x_side : bool; wall : char }

(* The DDA walk: from (x, y), in direction (dx, dy), cross the grid's
 * lines one at a time, always the nearest next one, vertical or
 * horizontal, until a cell is a wall.
 *
 *     +-----+-----+-----+       the ray crosses x lines every
 *     |     |     | ### |       [delta_x] of its length, y lines
 *     |   / |     | ### |       every [delta_y]: side_x and side_y
 *     +--/--+-----+-----+       are the lengths to the next ones;
 *     | @   |     |     |       step to the nearer, add its delta
 *     +-----+-----+-----+
 *)
let cast (x : number) (y : number) (dx : number) (dy : number) : hit =
  let delta_x = if dx = 0. then infinity else Float.abs (1. /. dx) in
  let delta_y = if dy = 0. then infinity else Float.abs (1. /. dy) in
  let col = int_of_float (floor x) and row = int_of_float (floor y) in
  let step_x, side_x = if dx < 0. then (-1, (x -. float_of_int col) *. delta_x) else (1, (float_of_int col +. 1. -. x) *. delta_x) in
  let step_y, side_y = if dy < 0. then (-1, (y -. float_of_int row) *. delta_y) else (1, (float_of_int row +. 1. -. y) *. delta_y) in
  let rec go col row side_x side_y =
    let x_side = side_x < side_y in
    let col, row, side_x, side_y =
      if x_side then (col + step_x, row, side_x +. delta_x, side_y) else (col, row + step_y, side_x, side_y +. delta_y)
    in
    match Tilemap.get map col row with
    | c when solid c ->
        (* the distance to the line just crossed *)
        let dist = if x_side then side_x -. delta_x else side_y -. delta_y in
        { dist; x_side; wall = Option.value c ~default:'#' }
    | _ -> go col row side_x side_y
  in
  go col row side_x side_y

(* the direction we look, and the camera plane *)
let camera (m : model) : (number * number) * (number * number) =
  let a = m.angle *. Float.pi /. 180. in
  let dx = cos a and dy = sin a in
  ((dx, dy), (-.dy *. plane_length, dx *. plane_length))

(* the number of rays: one per 5 pixels of a 1000-pixel screen *)
let columns = 200

(* the ray of column i: from the left edge of the plane (-1) to the
 * right (+1) *)
let ray (m : model) (i : int) : number * number =
  let (dx, dy), (px, py) = camera m in
  let c = (2. *. (float_of_int i +. 0.5) /. float_of_int columns) -. 1. in
  (dx +. (px *. c), dy +. (py *. c))

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* a wall's color, darker on its x sides *)
let wall_color (c : char) (x_side : bool) : color =
  let r, g, b = wall_rgb c in
  if x_side then rgb (r * 7 / 10) (g * 7 / 10) (b * 7 / 10) else rgb r g b

(* How many pixels 1 unit at distance 1 takes on the screen: across,
 * the camera plane's 2 * 0.66 units span the screen's width; up, the
 * same, else the picture is stretched (Lode's tutorial uses the
 * screen's height instead, simpler, stretched on a square screen). *)
let pixels_per_unit (screen : screen) : number = screen.width /. (2. *. plane_length)

(* One slice per column, as tall as pixels_per_unit / distance. The x
 * sides darker than the y sides, Wolfenstein's cheap lighting: corners
 * stand out. *)
let view_walls (screen : screen) (hits : hit array) : shape list =
  let w = screen.width /. float_of_int columns in
  Array.to_list
    (Array.mapi
       (fun i h ->
         let height = Float.min (pixels_per_unit screen /. h.dist) (2. *. screen.height) in
         let color = wall_color h.wall h.x_side in
         rectangle color (w +. 1.) height |> move_x (screen.left +. (w *. (float_of_int i +. 0.5))))
       hits)

let treasure_art =
  [ "........"; "..####.."; ".#yyyy#."; ".#yyyy#."; "..#yy#.."; "...##..."; "..####.."; "........" ]

let treasure_color (c : char) : color option =
  match c with '#' -> Some (rgb 190 140 20) | 'y' -> Some (rgb 255 220 60) | _ -> None

(* A billboard: the treasure at (tx, ty) as seen from the camera (its
 * depth, and where it is across the screen), then drawn column by
 * column, each column only where it's nearer than that column's wall.
 *
 * In the camera's frame, a point's coordinates are its components along
 * the plane (across) and the direction (depth): solving
 *   (tx, ty) - eye = across * plane + depth * dir
 * for (across, depth) is inverting a 2x2 matrix. *)
let view_treasure (screen : screen) (m : model) (hits : hit array) ((tx, ty) : number * number) : shape list =
  let (dx, dy), (px, py) = camera m in
  let sx = tx -. m.x and sy = ty -. m.y in
  let inv = 1. /. ((px *. dy) -. (dx *. py)) in
  let across = inv *. ((dy *. sx) -. (dx *. sy)) in
  let depth = inv *. ((-.py *. sx) +. (px *. sy)) in
  if depth <= 0.1 then []
  else
    let w = screen.width /. float_of_int columns in
    (* its center on the screen, in columns, and its size: half a wall *)
    let center = float_of_int columns /. 2. *. (1. +. (across /. depth)) in
    let size = pixels_per_unit screen /. depth *. 0.5 in
    let size_cols = size /. w in
    let first = int_of_float (floor (center -. (size_cols /. 2.))) in
    let last = int_of_float (ceil (center +. (size_cols /. 2.))) in
    let floor_y = -.(pixels_per_unit screen /. depth /. 2.) in
    List.init (max 0 (last - first + 1)) (fun k -> first + k)
    |> List.filter (fun i -> i >= 0 && i < columns && depth < hits.(i).dist)
    |> List.concat_map (fun i ->
           (* the art's column this screen column shows *)
           let u = (float_of_int i +. 0.5 -. (center -. (size_cols /. 2.))) /. size_cols in
           let tc = min 7 (max 0 (int_of_float (u *. 8.))) in
           let px = size /. 8. in
           List.concat
             (List.mapi
                (fun r row ->
                  match treasure_color row.[tc] with
                  | None -> []
                  | Some color ->
                      [ rectangle color (w +. 1.) (px +. 1.)
                        |> move (screen.left +. (w *. (float_of_int i +. 0.5))) (floor_y +. size -. ((float_of_int r +. 0.5) *. px)) ])
                treasure_art))

(* the map from above, with the rays: what the raycaster does, seen *)
let view_minimap (screen : screen) (m : model) (hits : hit array) : shape list =
  let cell = 10. in
  let ox = screen.left +. 20. and oy = screen.top -. 60. in
  let at x y = (ox +. (x *. cell), oy -. (y *. cell)) in
  let line color (x1, y1) (x2, y2) =
    let dx = x2 -. x1 and dy = y2 -. y1 in
    rectangle color (Float.hypot dx dy) 1. |> rotate (atan2 dy dx *. 180. /. Float.pi) |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)
  in
  let walls =
    Sprite.pixels cell [ ('#', gray); ('R', gray); ('G', gray); ('B', gray) ] (Tilemap.to_strings map)
    |> move (ox +. (8. *. cell)) (oy -. (8. *. cell))
  in
  let rays =
    List.init (columns / 10) (fun k ->
        let i = k * 10 in
        let rx, ry = ray m i in
        line yellow (at m.x m.y) (at (m.x +. (rx *. hits.(i).dist)) (m.y +. (ry *. hits.(i).dist))))
  in
  (walls :: rays)
  @ List.map (fun (tx, ty) -> let x, y = at tx ty in square (rgb 255 220 60) 4. |> move x y) m.treasures
  @ [ (let x, y = at m.x m.y in circle red 3. |> move x y) ]

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  let hits = Array.init columns (fun i -> let rx, ry = ray m i in cast m.x m.y rx ry) in
  (* the treasure, farthest first: the nearer ones drawn over them *)
  let dist (tx, ty) = Float.hypot (tx -. m.x) (ty -. m.y) in
  let treasures = List.sort (fun a b -> compare (dist b) (dist a)) m.treasures in
  let total = m.found + List.length m.treasures in
  [ rectangle (rgb 60 60 70) screen.width (screen.height /. 2.) |> move_y (screen.height /. 4.);
    rectangle (rgb 110 100 90) screen.width (screen.height /. 2.) |> move_y (-.screen.height /. 4.) ]
  @ view_walls screen hits
  @ List.concat_map (view_treasure screen m hits) treasures
  @ view_minimap screen m hits
  @ [ words white (Printf.sprintf "TREASURE %d / %d" m.found total) |> scale 3. |> move (screen.right -. 220.) (screen.top -. 40.) ]
  @ if m.treasures = [] then [ words yellow "ALL THE TREASURE IS YOURS!" |> scale 4. ] else []

let app = game view update initial_model

let main = Playground_platform.run_app app
