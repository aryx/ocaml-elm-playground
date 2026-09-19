(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Comanche: Maximum Overkill (NovaLogic, 1992), in the
 * *2D* playground: a helicopter over an island, to pop the 6 balloons
 * floating over it. Left/right to turn, up/down to speed up and slow
 * down, w/s to climb and descend.
 *
 * Flight simulators drew their hills with a few big polygons, flat
 * colors; Comanche showed real hills, with every slope and valley, on a
 * 386. Its "Voxel Space" (Kyle Freeman) had no polygons: the terrain is
 * a height map and a color map (kits/heightmap/Heightmap.mli), and the
 * screen is drawn column by column, each a line across the map, from
 * the nearest to the farthest ([view_terrain]):
 *
 *      from above                         a column, from the side
 *                  far                   eye o--__
 *       \ - - - - - - - - - /                |     --__     row: where
 *        \  - - - - - - -  /     at each     |  ###    --__ a cell's
 *         \   - - - - -   /      distance,   | #####        top shows
 *          \    - - -    /       a line of   +------------------
 *           \     -     /        the map,       near --> far
 *                eye             a sample per column
 *
 * At each distance z, the map's cell under each column, its height
 * seen at the screen row horizon + (altitude - height) / z * focal; if
 * that's above what the column already shows, the cell's color fills
 * the column from there down to it. What the column shows is one
 * number, the "y-buffer" ([ybuffer]): the nearest first, a hill hides
 * what's behind it by being drawn first, and the farther cells only
 * add above it. Doom's trick again (games/TinyDoom.ml: the nearest
 * first, the columns' clip arrays), but with no BSP tree: the order is
 * the distance itself, and the clip only has a bottom.
 *
 * The cells are not mixed ([Heightmap.cell], not [height]): each is a
 * column of one color, the "voxel" of the name (a volume pixel), big
 * blocks near the eye. Farther, the distance between two lines grows
 * ([dz]): fewer samples where a cell is smaller than a pixel. And the
 * far cells fade to the sky's color, 4 steps of fog, which hid where
 * the map stops.
 *
 * As in games/TinyKart.ml, a "pixel" is 5 x 5 real ones: the view is a
 * picture of characters (one per color: a kind of ground, its light,
 * its fog), drawn by Sprite.pixels. The balloons are billboards: a
 * circle sized by its distance, drawn over the terrain if nothing is
 * between it and the eye ([Heightmap.clear]).
 *
 * games3d/TinyComanche3d.ml flies over the same island, the grid as
 * triangles with a z-buffer: the flight simulators' way, now the GPUs'.
 *
 * Uses: the Heightmap kit, Sprite. Not Tilemap (a grid of heights, not
 * of tiles), not Camera2d, not Scene2d (no title screen).
 *
 * References: Sebastian Macke, "Voxel Space" (github.com/s-macke/
 * VoxelSpace, 2017): Comanche's algorithm in 20 lines, the loop of
 * [view_terrain]; Fabien Sanglard's notes on Comanche.
 *
 * Exercises: looking up and down (Comanche moved the horizon, a
 * "shear": [horizon] changed, no rotation), the balloons half hidden (a
 * y-buffer per distance), the helicopter's shadow, textures in the
 * color map, a map that wraps around (Comanche's did).
 *)
open Playground

(*****************************************************************************)
(* The island *)
(*****************************************************************************)

(* coupling: games3d/TinyComanche3d.ml's island and balloons, the same *)
let map = Heightmap.generate ~seed:7 ~size:256 ~top:80. ~roughness:0.55

(* 6 balloons around the middle, 12 above the ground *)
let balloons : (number * number * number) list =
  List.init 6 (fun k ->
      let a = float_of_int k *. Float.pi /. 3. in
      let x = 128. +. (60. *. cos a) and y = 128. +. (60. *. sin a) in
      (x, y, Heightmap.height map x y +. 12.))

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

(* coupling: games3d/TinyComanche3d.ml's model and update, the same *)
type model = {
  x : number;
  y : number;
  alt : number;
  (* degrees, 0 east, 90 north *)
  angle : number;
  (* map units a frame *)
  speed : number;
  left : (number * number * number) list;
  frames : int;
  finished : int option;
}

let initial_model : model =
  { x = 128.; y = 8.; alt = 40.; angle = 90.; speed = 0.; left = balloons; frames = 0; finished = None }

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

(* the rotor's clearance above the ground *)
let clearance = 5.

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  match m.finished with
  | Some _ -> if k.kspace then initial_model else m
  | None ->
      let angle = m.angle +. (2.5 *. axis k.kleft k.kright) in
      let speed =
        if k.kup then Float.min 1.5 (m.speed +. 0.03) else if k.kdown then Float.max (-0.3) (m.speed -. 0.05) else m.speed *. 0.99
      in
      let a = angle *. Float.pi /. 180. in
      let inside v = Float.max 0. (Float.min 255. v) in
      let x = inside (m.x +. (speed *. cos a)) and y = inside (m.y +. (speed *. sin a)) in
      let alt = Float.min 150. (Float.max (Heightmap.height map x y +. clearance) (m.alt +. (0.6 *. axis k.kw k.ks))) in
      let near (bx, by, bz) = Float.hypot (Float.hypot (bx -. x) (by -. y)) (bz -. alt) < 5. in
      let left = List.filter (fun b -> not (near b)) m.left in
      { x; y; alt; angle; speed; left; frames = m.frames + 1; finished = (if left = [] then Some m.frames else None) }

(*****************************************************************************)
(* Voxel Space *)
(*****************************************************************************)

(* the view's "pixels", 5 real ones wide and high; the panel below it *)
let pixel = 5.
let panel = 200.

(* how far we see, in map cells, and the fog's steps *)
let far = 300.
let fogs = 4

let sky = (150, 190, 230)

let kind_index (k : Heightmap.kind) : int =
  let rec find i = function [] -> 0 | k' :: ks -> if k' = k then i else find (i + 1) ks in
  find 0 Heightmap.kinds

(* one character per (kind, light, fog), from 'A' on *)
let code (kind : int) (light : int) (fog : int) : char = Char.chr (65 + (((kind * 3) + light) * fogs) + fog)

let palette : (char * color) list =
  let sr, sg, sb = sky in
  List.concat_map
    (fun kind ->
      List.concat_map
        (fun light ->
          List.init fogs (fun fog ->
              let r, g, b = Heightmap.color kind light in
              (* 0: the color, the last step: mostly the sky *)
              let f = float_of_int fog /. float_of_int fogs in
              let mix c s = int_of_float ((float_of_int c *. (1. -. f)) +. (float_of_int s *. f)) in
              (code (kind_index kind) light fog, rgb (mix r sr) (mix g sg) (mix b sb))))
        [ 0; 1; 2 ])
    Heightmap.kinds

(* each cell's character without the fog, computed once; outside, the
 * sea's *)
let codes : char array =
  Array.init (map.size * map.size) (fun k ->
      let i = k mod map.size and j = k / map.size in
      code (kind_index (Heightmap.kind map i j)) (Heightmap.light map i j) 0)

let code_at (i : int) (j : int) : char =
  if i < 0 || j < 0 || i >= map.size || j >= map.size then code (kind_index Sea) 1 0 else codes.((j * map.size) + i)

(* the eye's frame: its direction and its right, in the map *)
type eye = { ex : number; ey : number; alt : number; fx : number; fy : number; rx : number; ry : number }

let eye (m : model) : eye =
  let a = m.angle *. Float.pi /. 180. in
  { ex = m.x; ey = m.y; alt = m.alt; fx = cos a; fy = sin a; rx = sin a; ry = -.cos a }

(* the view's size in pixels; a 90 degrees field of view (the column
 * [cols / 2] from the middle sees as far right as ahead), the horizon
 * a third of the way down *)
let cols (screen : screen) = int_of_float (screen.width /. pixel)
let rows (screen : screen) = int_of_float ((screen.height -. panel) /. pixel)
let focal (screen : screen) = float_of_int (cols screen) /. 2.
let horizon (screen : screen) = float_of_int (rows screen) *. 0.35

(* The terrain, Macke's loop: at each distance [z], the line across the
 * map from the view's left edge to its right, a sample per column. *)
let view_terrain (screen : screen) (e : eye) : shape =
  let cols = cols screen and rows = rows screen in
  let focal = focal screen and horizon = horizon screen in
  let picture = Array.init rows (fun _ -> Bytes.make cols ' ') in
  (* the highest row drawn in each column so far (rows count down from
   * the top): the bottom, at first *)
  let ybuffer = Array.make cols rows in
  let rec line z dz =
    if z < far then begin
      (* the line's left end, and a column's step along it *)
      let lx = e.ex +. (z *. e.fx) -. (z *. e.rx) and ly = e.ey +. (z *. e.fy) -. (z *. e.ry) in
      let sx = 2. *. z *. e.rx /. float_of_int cols and sy = 2. *. z *. e.ry /. float_of_int cols in
      let fog = min (fogs - 1) (int_of_float (z /. far *. float_of_int (fogs + 1))) in
      for c = 0 to cols - 1 do
        let i = Float.to_int (Float.floor (lx +. ((float_of_int c +. 0.5) *. sx)))
        and j = Float.to_int (Float.floor (ly +. ((float_of_int c +. 0.5) *. sy))) in
        let top = int_of_float (horizon +. ((e.alt -. Heightmap.cell map i j) /. z *. focal)) in
        let top = max 0 top in
        if top < ybuffer.(c) then begin
          let ch = Char.chr (Char.code (code_at i j) + fog) in
          for r = top to ybuffer.(c) - 1 do
            Bytes.set picture.(r) c ch
          done;
          ybuffer.(c) <- top
        end
      done;
      (* farther, the lines further apart *)
      line (z +. dz) (dz +. 0.01)
    end
  in
  line 1. 0.25;
  Sprite.pixels pixel palette (Array.to_list (Array.map Bytes.to_string picture))
  |> move (screen.left +. (float_of_int cols *. pixel /. 2.)) (screen.top -. (float_of_int rows *. pixel /. 2.))

(* the sky, behind the terrain: the fog's color, where the far cells
 * fade to *)
let view_sky (screen : screen) : shape list =
  let r, g, b = sky in
  [ rectangle (rgb r g b) screen.width (screen.height -. panel) |> move_y (screen.top -. ((screen.height -. panel) /. 2.)) ]

(* A balloon, a billboard: where its middle projects, as the terrain's
 * cells do, its size by its distance; the farthest first. *)
let view_balloons (screen : screen) (e : eye) (bs : (number * number * number) list) : shape list =
  let focal = focal screen and horizon = horizon screen in
  bs
  |> List.filter_map (fun (bx, by, bz) ->
         let dx = bx -. e.ex and dy = by -. e.ey in
         let ahead = (dx *. e.fx) +. (dy *. e.fy) and side = (dx *. e.rx) +. (dy *. e.ry) in
         if ahead < 1. || ahead > far || Float.abs side > ahead || not (Heightmap.clear map (e.ex, e.ey, e.alt) (bx, by, bz)) then None
         else
           let x = screen.left +. ((float_of_int (cols screen) /. 2.) +. (side /. ahead *. focal)) *. pixel in
           let y = screen.top -. ((horizon +. ((e.alt -. bz) /. ahead *. focal)) *. pixel) in
           let r = 2.5 /. ahead *. focal *. pixel in
           Some
             ( ahead,
               group
                 [ rectangle (rgb 60 60 60) (Float.max 1. (r /. 10.)) (2. *. r) |> move_y (-.2. *. r);
                   circle (rgb 220 40 40) r;
                   circle (rgb 255 150 150) (r /. 3.) |> move (-.r /. 3.) (r /. 3.) ]
               |> move x y ))
  |> List.sort (fun (a, _) (b, _) -> compare b a)
  |> List.map snd

(*****************************************************************************)
(* The panel *)
(*****************************************************************************)

(* the island from above, a pixel every 4 cells, drawn once *)
let minimap_cells = 4
let minimap_pixel = 2.5

let minimap : shape =
  let n = map.size / minimap_cells in
  (* the rows from the north down *)
  Sprite.pixels minimap_pixel palette
    (List.init n (fun r -> String.init n (fun c -> code_at (c * minimap_cells) ((n - 1 - r) * minimap_cells))))

let text color size str = words color str |> scale size

(* coupling: games3d/TinyComanche3d.ml's panel, the same *)
let view_panel (screen : screen) (m : model) : shape list =
  let side = float_of_int (map.size / minimap_cells) *. minimap_pixel in
  let ox = screen.right -. 20. -. (side /. 2.) and oy = screen.bottom +. (panel /. 2.) in
  let at x y = (ox +. ((x /. float_of_int minimap_cells) -. (side /. minimap_pixel /. 2.)) *. minimap_pixel, oy +. ((y /. float_of_int minimap_cells) -. (side /. minimap_pixel /. 2.)) *. minimap_pixel) in
  let dot color r (x, y) = circle color r |> move x y in
  let time = float_of_int (Option.value m.finished ~default:m.frames) /. 60. in
  let stat i str = text (rgb 120 230 120) 2.5 str |> move (screen.left +. 200.) (screen.bottom +. 160. -. (float_of_int i *. 40.)) in
  [ rectangle (rgb 40 44 40) screen.width panel |> move_y (screen.bottom +. (panel /. 2.));
    minimap |> move ox oy;
    stat 0 (Printf.sprintf "TIME %d:%02d" (int_of_float time / 60) (int_of_float time mod 60));
    stat 1 (Printf.sprintf "BALLOONS %d / %d" (List.length balloons - List.length m.left) (List.length balloons));
    stat 2 (Printf.sprintf "ALTITUDE %d" (int_of_float m.alt));
    stat 3 (Printf.sprintf "SPEED %d" (int_of_float (m.speed *. 100.))) ]
  @ List.map (fun (x, y, _) -> dot (rgb 220 40 40) 3. (at x y)) m.left
  @ [ dot yellow 4. (at m.x m.y) ]

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  let e = eye m in
  view_sky screen @ [ view_terrain screen e ] @ view_balloons screen e m.left @ view_panel screen m
  @
  match m.finished with
  | Some _ -> [ text yellow 6. "ALL POPPED!" |> move_y 300.; text white 3. "space: again" |> move_y 220. ]
  | None -> if m.frames < 180 then [ text yellow 4. "POP THE 6 BALLOONS" |> move_y 300. ] else []

let app = game view update initial_model

let main = Playground_platform.run_app app
