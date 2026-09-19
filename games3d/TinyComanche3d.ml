(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* games2.5d/TinyComanche.ml, the same flight over the same island, but the
 * terrain as triangles, drawn with a z-buffer by playground3d: the way
 * flight simulators drew their ground (Flight Simulator, Falcon, a few
 * big flat-colored polygons), and every 3D game since, on a GPU. The
 * two, side by side, are the lesson:
 *
 *  - TinyComanche: no polygons; a column of the screen is a line of the
 *    map, sampled from near to far, a y-buffer for what's hidden. Every
 *    cell of the map counts, at no cost per cell: the cost is per
 *    column and per distance step, whatever the terrain. But the eye
 *    can only turn around the vertical and look straight ahead (no
 *    roll: the columns must stay vertical), and nothing can be under
 *    something else.
 *  - TinyComanche3d: the height map made into triangles, once
 *    ([mesh]), a quad every 4 cells split in two (256 x 256 cells, a
 *    triangle each would be 130000 of them: too many for the software
 *    rasterizer), each lit by its slope (the Flat shading: the light
 *    computed from the triangle's normal, where Voxel Space's color map
 *    had it painted in). The camera can do anything, and the z-buffer
 *    sorts it out, pixel by pixel; the cost is per triangle and per
 *    pixel.
 *
 * Comanche's successors went the second way: by 1997 the 3D cards drew
 * triangles faster than a CPU drew voxels. Voxel Space came back in
 * compute shaders and demos.
 *
 * Uses: the Heightmap kit, Sprite (the minimap), Camera3d's sky. Not
 * Camera3d's cameras (the camera is the eye, looking a bit down, like
 * TinyComanche's horizon).
 *
 * Exercises: the whole map's cells (and a mesh far coarser away: the
 * "level of detail" of terrain engines, e.g. ROAM, Mark Duchaineau et
 * al., 1997), fog (a color toward the sky by the depth), a roll when
 * turning, what Voxel Space couldn't do.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The island *)
(*****************************************************************************)

(* coupling: games2.5d/TinyComanche.ml's island and balloons, the same *)
let map = Heightmap.generate ~seed:7 ~size:256 ~top:80. ~roughness:0.55

let balloons : (number * number * number) list =
  List.init 6 (fun k ->
      let a = float_of_int k *. Float.pi /. 3. in
      let x = 128. +. (60. *. cos a) and y = 128. +. (60. *. sin a) in
      (x, y, Heightmap.height map x y +. 12.))

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

(* coupling: games2.5d/TinyComanche.ml's model and update, the same *)
type model = {
  x : number;
  y : number;
  alt : number;
  angle : number;
  speed : number;
  left : (number * number * number) list;
  frames : int;
  finished : int option;
}

let initial_model : model =
  { x = 128.; y = 8.; alt = 40.; angle = 90.; speed = 0.; left = balloons; frames = 0; finished = None }

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.
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
(* The island in 3D *)
(*****************************************************************************)

(* the map's (x, y) at height h: in 3D, y is up, and the map's y is -z
 * (so that from above, north is still up) *)
let at (x : number) (y : number) (h : number) : number * number * number = (x, h, -.y)

let color_of (kind : Heightmap.kind) : color =
  let r, g, b = Heightmap.color kind 1 in
  rgb r g b

(* a quad's side, in cells *)
let step = 4

(* The mesh: every [step] cells, the quad from (i, j) to (i + step,
 * j + step), two triangles counterclockwise seen from above; the
 * color, its middle cell's kind. The quads all in the sea are left out:
 * one big quad for the whole sea, just under them (not at the same
 * height: the z-buffer couldn't tell which one is in front, and would
 * mix them, "z-fighting").
 *
 *     (i, j+s) +----+ (i+s, j+s)
 *              |  / |
 *              | /  |
 *       (i, j) +----+ (i+s, j)
 *)
let mesh : shape3d list =
  let n = map.size / step in
  let quad i j =
    let p i j = at (float_of_int i) (float_of_int j) (Heightmap.cell map i j) in
    let corners = [ (i, j); (i + step, j); (i + step, j + step); (i, j + step) ] in
    if List.for_all (fun (i, j) -> Heightmap.cell map i j <= map.sea) corners then []
    else
      let color = color_of (Heightmap.kind map (i + (step / 2)) (j + (step / 2))) in
      [ polygon3d color [ p i j; p (i + step) j; p (i + step) (j + step) ];
        polygon3d color [ p i j; p (i + step) (j + step); p i (j + step) ] ]
  in
  let sea = map.sea -. 0.3 and far = 1000. in
  polygon3d (color_of Sea) [ at (-.far) (-.far) sea; at far (-.far) sea; at far far sea; at (-.far) far sea ]
  :: List.concat (List.init (n * n) (fun k -> quad (k mod n * step) (k / n * step)))

(* built once: on the GPU backends, kept in GPU buffers *)
let world : shape3d = cached3d mesh

let view_balloon ((x, y, z) : number * number * number) : shape3d =
  let x, y, z = at x y z in
  group3d [ sphere (rgb 220 40 40) 2.5; box (rgb 60 60 60) 0.2 5. 0.2 |> move_y3d (-5.) ] |> move3d x y z

(*****************************************************************************)
(* The panel *)
(*****************************************************************************)

let minimap_cells = 4
let minimap_pixel = 2.5
let panel = 200.

(* the island from above, a pixel every 4 cells, a character by kind
 * and light, from 'A' on *)
let minimap : shape =
  let n = map.size / minimap_cells in
  let code i j =
    let rec index k = function [] -> 0 | k' :: ks -> if k' = Heightmap.kind map i j then k else index (k + 1) ks in
    Char.chr (65 + (index 0 Heightmap.kinds * 3) + Heightmap.light map i j)
  in
  let palette =
    List.concat
      (List.mapi
         (fun k kind -> List.init 3 (fun light -> let r, g, b = Heightmap.color kind light in (Char.chr (65 + (k * 3) + light), rgb r g b)))
         Heightmap.kinds)
  in
  Sprite.pixels minimap_pixel palette
    (List.init n (fun r -> String.init n (fun c -> code (c * minimap_cells) ((n - 1 - r) * minimap_cells))))

let text color size str = words color str |> scale size

(* coupling: games2.5d/TinyComanche.ml's panel, the same *)
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

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let a = m.angle *. Float.pi /. 180. in
  (* looking down a bit: the horizon about where TinyComanche's is *)
  let cam =
    camera ~eye:(at m.x m.y m.alt) ~target:(at (m.x +. cos a) (m.y +. sin a) (m.alt -. 0.44)) ~fov:90. ~near:0.5 ~far:1500. ()
  in
  let huds =
    List.map hud (view_panel screen m)
    @
    match m.finished with
    | Some _ -> [ hud (text yellow 6. "ALL POPPED!" |> move_y 300.); hud (text white 3. "space: again" |> move_y 220.) ]
    | None -> if m.frames < 180 then [ hud (text yellow 4. "POP THE 6 BALLOONS" |> move_y 300.) ] else []
  in
  let sky = Camera3d.sky ~sky:(rgb 150 190 230) ~horizon:(color_of Sea) ~ground:(map.sea -. 0.5) cam in
  (cam, (world :: sky) @ List.map view_balloon m.left @ huds)

let app = game3d view update initial_model

(* flat shading, each triangle lit by its slope; the back faces drawn
 * too, for the sky (seen from below, see Camera3d.sky) *)
let main =
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false } app
