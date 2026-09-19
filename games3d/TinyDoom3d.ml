(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* games2.5d/TinyDoom.ml again, in real 3D: the same level (the Sectors kit's),
 * the same controls, the same exit, but drawn by playground3d like any
 * 3D scene. Left/right to turn, up/down to walk.
 *
 * Put the two side by side (their golden frames are the same walks) and
 * the lesson is what a z-buffer makes unnecessary. TinyDoom needs its
 * BSP tree to draw the walls nearest first, and its per-column clip
 * arrays to draw each pixel once; here the level is turned into
 * polygons once ([world]: each sector's floor and ceiling, each line's
 * wall, or its steps above and below for a two-sided line), all of them
 * drawn every frame in any order, and for each pixel the z-buffer keeps
 * the nearest (graphics/3d/Zbuffer.mli). No tree, no clipping by
 * columns, no front-to-back order: the sector you're in is found by
 * trying every sector's polygon (Sectors.sector_at), not by walking a
 * tree either. What it costs: every polygon of the level projected and
 * filled every frame, even the ones behind walls -- the status line
 * counts them, against TinyDoom's "SEGS DRAWN".
 *
 * Doom (1993) couldn't afford that on a 386; Quake (1996) did, on a
 * Pentium, with a z-buffer for its moving models (and still a BSP, to
 * draw its walls without overdraw). A GPU does it for millions of
 * polygons a frame, which is why modern engines use a z-buffer, and
 * trees only to skip what's out of sight.
 *
 * The light is Doom's too, the sector's level and the "fake contrast",
 * but not darker with the distance (a z-buffer could, as fog: an
 * exercise); so no shading by the 3D backend (No_lighting).
 *)
open Playground
open Playground3d

let level = Sectors.outpost
let sector (i : int) : Sectors.sector = level.sectors.(i)

(*****************************************************************************)
(* The model, as in games2.5d/TinyDoom.ml *)
(*****************************************************************************)

(* coupling: TinyDoom's model and update, with the sector found by the
 * level's polygons instead of the BSP *)
type model = { x : number; y : number; z : number; angle : number; frames : int; exited : int option }

let initial_model : model =
  let x, y, angle = level.start in
  { x; y; z = (sector (Sectors.sector_at level x y)).floor; angle; frames = 0; exited = None }

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  match m.exited with
  | Some _ -> if k.kspace then initial_model else m
  | None ->
      let angle = m.angle +. (3. *. axis k.kleft k.kright) in
      let speed = if k.kup then 8. else if k.kdown then -6. else 0. in
      let a = angle *. Float.pi /. 180. in
      let x, y = Sectors.move level (Sectors.sector_at level) (m.x, m.y) (speed *. cos a, speed *. sin a) in
      let here = Sectors.sector_at level x y in
      let floor = (sector here).floor in
      let z = if Float.abs (floor -. m.z) < 1. then floor else m.z +. ((floor -. m.z) *. 0.3) in
      { x; y; z; angle; frames = m.frames + 1; exited = (if here = level.exit then Some m.frames else None) }

(*****************************************************************************)
(* The level in 3D *)
(*****************************************************************************)

(* the plan's (x, y) at height h: in 3D, y is up, and the plan's y is -z
 * (so that from above, north is still up) *)
let at (x : number) (y : number) (h : number) : number * number * number = (x, h, -.y)

let shade ((r, g, b) : int * int * int) (light : number) : color =
  let c x = min 255 (int_of_float (float_of_int x *. light)) in
  rgb (c r) (c g) (c b)

(* A wall of the line from (x1, y1) to (x2, y2), from height h1 to h2,
 * seen from its front (its right): counterclockwise from there, the
 * first point on the left. *)
let wall (color : color) (x1, y1) (x2, y2) (h1 : number) (h2 : number) : shape3d list =
  if h2 <= h1 then [] else [ polygon3d color [ at x1 y1 h1; at x2 y2 h1; at x2 y2 h2; at x1 y1 h2 ] ]

(* A line's walls, from its front (and, two-sided, from its back too):
 * a wall from floor to ceiling, or the steps up to the other side's
 * floor and down to its ceiling (a closed sector behind, the pillar,
 * has its ceiling at its floor: the step is the whole wall). *)
let walls (l : Sectors.line) : shape3d list =
  let side (f : Sectors.sector) (back : Sectors.sector option) p1 p2 =
    let contrast = if l.y1 = l.y2 then 0.85 else if l.x1 = l.x2 then 1.1 else 1. in
    let color = shade f.wall_rgb (f.light *. contrast) in
    match back with
    | None -> wall color p1 p2 f.floor f.ceiling
    | Some b -> wall color p1 p2 (Float.max b.ceiling f.floor) f.ceiling @ wall color p1 p2 f.floor (Float.min b.floor f.ceiling)
  in
  let p1 = (l.x1, l.y1) and p2 = (l.x2, l.y2) in
  match l.back with
  | None -> side (sector l.front) None p1 p2
  | Some b -> side (sector l.front) (Some (sector b)) p1 p2 @ side (sector b) (Some (sector l.front)) p2 p1

(* A sector's floor, seen from above, and its ceiling, from below (its
 * outline clockwise from above, so reversed for the floor); its holes
 * are other sectors, whose floors and ceilings cover them. *)
let flats (s : Sectors.sector) : shape3d list =
  match s.loops with
  | outline :: _ when s.ceiling > s.floor ->
      [ polygon3d (shade s.floor_rgb s.light) (List.rev_map (fun (x, y) -> at x y s.floor) outline);
        polygon3d (shade s.ceiling_rgb s.light) (List.map (fun (x, y) -> at x y s.ceiling) outline) ]
  | _ -> []

let polygons : shape3d list =
  List.concat_map walls (Array.to_list level.lines) @ List.concat_map flats (Array.to_list level.sectors)

(* built once: on the GPU backends, kept in GPU buffers *)
let world : shape3d = cached3d polygons

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let line (color : color) (width : number) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle color (Float.hypot dx dy) width |> rotate (atan2 dy dx *. 180. /. Float.pi) |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

(* coupling: TinyDoom's automap, without the segs drawn: here, all *)
let view_minimap (screen : screen) (m : model) : shape list =
  let k = 0.24 in
  let ox = screen.right -. 150. -. (576. *. k) and oy = screen.bottom +. 150. -. (512. *. k) in
  let at x y = (ox +. (x *. k), oy +. (y *. k)) in
  let color (l : Sectors.line) =
    match l.back with
    | None -> rgb 200 40 40
    | Some b ->
        let f = sector l.front and b = sector b in
        if f.floor <> b.floor then rgb 150 110 60 else if f.ceiling <> b.ceiling then rgb 200 200 60 else rgb 90 90 90
  in
  let a = m.angle *. Float.pi /. 180. in
  let ray d = at (m.x +. (120. *. cos (a +. d))) (m.y +. (120. *. sin (a +. d))) in
  [ rectangle black 300. 270. |> move (ox +. (576. *. k)) (oy +. (512. *. k)) |> fade 0.6 ]
  @ Array.to_list (Array.map (fun (l : Sectors.line) -> line (color l) 2. (at l.x1 l.y1) (at l.x2 l.y2)) level.lines)
  @ [ line gray 1. (at m.x m.y) (ray (Float.pi /. 4.)); line gray 1. (at m.x m.y) (ray (-.Float.pi /. 4.));
      (let x, y = at m.x m.y in circle green 4. |> move x y) ]

let text color size str = words color str |> scale size

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let a = m.angle *. Float.pi /. 180. in
  (* the eye 41 above the floor; a 90 degrees field of view, Doom's *)
  let ez = m.z +. 41. in
  let cam = camera ~eye:(at m.x m.y ez) ~target:(at (m.x +. cos a) (m.y +. sin a) ez) ~fov:90. ~near:1. ~far:4000. () in
  let time = float_of_int (Option.value m.exited ~default:m.frames) /. 60. in
  let stat i str = hud (text (rgb 200 30 30) 2.5 str |> move (screen.left +. 210.) (screen.bottom +. 250. -. (float_of_int i *. 45.))) in
  let huds =
    (* whole seconds, unlike TinyDoom's tenths: on the OpenGL backend,
     * every change of the HUD's text is rendered on the CPU (see its
     * draw_hud), a hitch 10 times a second with tenths *)
    [ stat 0 (Printf.sprintf "TIME %d:%02d" (int_of_float time / 60) (int_of_float time mod 60));
      stat 1 (Printf.sprintf "POLYGONS DRAWN %d / %d" (List.length polygons) (List.length polygons));
      stat 2 (Printf.sprintf "SECTOR %d" (Sectors.sector_at level m.x m.y)) ]
    @ List.map hud (view_minimap screen m)
    @
    match m.exited with
    | Some _ -> [ hud (text yellow 6. "EXIT!" |> move_y 250.); hud (text white 3. "space: again" |> move_y 170.) ]
    | None -> if m.frames < 180 then [ hud (text yellow 4. "FIND THE EXIT" |> move_y 250.) ] else []
  in
  (cam, world :: huds)

let app = game3d view update initial_model

let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = No_lighting } app
