(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* games2.5d/TinyDescent.ml again, in real 3D: the same mine (the
 * Segments kit's), the same ship and the same robots, but drawn by
 * playground3d -- every wall of every cell handed over at once, and a
 * z-buffer deciding what shows. Arrows to aim, a/d to roll, w/s to
 * fly, space to fire.
 *
 * The pair is the lesson, as it is for the TinyDoom and TinyComanche
 * pairs:
 *
 *   - TinyDescent walks the mine through its openings ([render]
 *     there): a cell is drawn only if a portal shows it, each one
 *     clipped to the window it is seen through, and the whole mine is
 *     painted farthest first. No depth is kept per pixel; the cost is
 *     per cell and per polygon, and the status bar shows how few cells
 *     that is (3 of 12, looking down a corridor).
 *   - TinyDescent3d gives the renderer all 12 cells, every frame, and
 *     a depth test throws away what is hidden, pixel by pixel. Nothing
 *     knows the mine is made of cells at all. The cost is per pixel,
 *     and the whole mine is projected whether or not it can be seen --
 *     which is exactly the trade the GPUs won: by 1996 a depth test
 *     per pixel was free, and a portal walk was code to maintain.
 *
 * Both ways need the same two things to fly in every direction: a
 * camera with a roll (playground3d's camera takes an [up], here the
 * ship's own, where the other games leave it (0, 1, 0)) and no floor
 * to stand on (gamekits/segments/Sixdof.mli).
 *
 * The mine is built once into a [cached3d] shape, so the GPU backends
 * keep it in their buffers; only the ship, the robots and the shots
 * change from frame to frame.
 *
 * Uses: the Segments kit (the mine, Sixdof for the ship). Not
 * Camera3d (its cameras all keep their feet on the ground).
 *
 * Exercises: the portal walk here too, as a way to send fewer
 * polygons (what a real engine does with a z-buffer, "occlusion
 * culling"); a texture on the rock; the robots lit by a light on the
 * ship.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The mine, and flying in it *)
(*****************************************************************************)

(* coupling: games2.5d/TinyDescent.ml's model and update, the same *)
let level = Segments.mine
let radius = 4.

type vec = Segments.vec

let add (x1, y1, z1) (x2, y2, z2) : vec = (x1 +. x2, y1 +. y2, z1 +. z2)
let sub (x1, y1, z1) (x2, y2, z2) : vec = (x1 -. x2, y1 -. y2, z1 -. z2)
let times k (x, y, z) : vec = (k *. x, k *. y, k *. z)
let dot (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)
let length v = sqrt (dot v v)

let normalize (v : vec) : vec =
  let n = length v in
  if n = 0. then v else times (1. /. n) v

type shot = { sp : vec; sv : vec; player : bool; life : int }
type robot = { rp : vec; cool : int }

type model = {
  p : vec;
  ship : Sixdof.t;
  speed : number;
  cool : int;
  shield : int;
  robots : robot list;
  shots : shot list;
  frames : int;
  over : (int * bool) option;
}

let robots : robot list =
  List.map (fun p -> { rp = p; cool = 60 }) [ (100., 12., 20.); (100., 76., 20.); (100., 76., 110.); (20., 12., 100.) ]

let start_ship : Sixdof.t = Sixdof.turn ~yaw:(-90.) Sixdof.identity

let initial_model : model =
  { p = level.start; ship = start_ship; speed = 0.; cool = 0; shield = 100; robots; shots = []; frames = 0; over = None }

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

let fly_shot (s : shot) : shot option =
  let sp = add s.sp s.sv in
  if s.life = 0 || Segments.segment_at level sp = None then None else Some { s with sp; life = s.life - 1 }

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  match m.over with
  | Some _ -> if k.kspace then initial_model else m
  | None ->
      let ship =
        Sixdof.turn ~pitch:(2. *. axis k.kup k.kdown) ~yaw:(2. *. axis k.kleft k.kright) ~roll:(2. *. axis k.kd k.ka) m.ship
      in
      let speed = if k.kw then Float.min 2.4 (m.speed +. 0.12) else if k.ks then Float.max (-1.2) (m.speed -. 0.12) else m.speed *. 0.9 in
      let p = Segments.move level ~radius m.p (times speed ship.forward) in
      let firing = k.kspace && m.cool = 0 in
      let mine_ = if firing then [ { sp = add p (times 5. ship.forward); sv = times 6. ship.forward; player = true; life = 120 } ] else [] in
      let robots =
        List.map
          (fun (r : robot) ->
            let sees = length (sub p r.rp) < 260. && Segments.clear level r.rp p in
            { r with cool = (if r.cool > 0 then r.cool - 1 else if sees then 90 else 0) })
          m.robots
      in
      let theirs =
        List.filter_map
          (fun (r : robot) ->
            if r.cool = 90 then Some { sp = r.rp; sv = times 3.2 (normalize (sub p r.rp)); player = false; life = 150 } else None)
          robots
      in
      let shots = List.filter_map fly_shot (mine_ @ theirs @ m.shots) in
      let hit (s : shot) (target : vec) (size : number) = length (sub s.sp target) < size in
      let robots = List.filter (fun (r : robot) -> not (List.exists (fun s -> s.player && hit s r.rp 8.) shots)) robots in
      let taken = List.filter (fun s -> (not s.player) && hit s p (radius +. 2.)) shots in
      let shots =
        List.filter (fun s -> not (List.exists (fun (r : robot) -> s.player && hit s r.rp 8.) m.robots || List.memq s taken)) shots
      in
      let shield = m.shield - (10 * List.length taken) in
      let escaped = robots = [] && Segments.segment_at level p = Some level.exit in
      { p; ship; speed; cool = (if firing then 12 else max 0 (m.cool - 1)); shield; robots; shots;
        frames = m.frames + 1; over = (if escaped then Some (m.frames, true) else if shield <= 0 then Some (m.frames, false) else None) }

(*****************************************************************************)
(* The mine in 3D *)
(*****************************************************************************)

(* coupling: games2.5d/TinyDescent.ml's wall_color, without its fade
 * with the distance (no fog here) *)
let wall_color (s : Segments.segment) (q : Segments.quad) : color =
  let r, g, b = s.rgb in
  let contrast = match (q.side.axis, q.side.positive) with 1, false -> 1.35 | 1, true -> 0.7 | 0, _ -> 0.88 | _ -> 1.05 in
  let c x = int_of_float (Float.min 255. (float_of_int x *. s.light *. contrast)) in
  rgb (c r) (c g) (c b)

(* every wall of every cell, corners already counterclockwise seen
 * from inside (the kit's): built once, kept in the GPU's buffers *)
let mine : shape3d =
  cached3d
    (List.concat
       (List.mapi
          (fun i quads -> List.map (fun (q : Segments.quad) -> polygon3d (wall_color level.segments.(i) q) q.corners) quads)
          (Array.to_list level.walls)))

let polygons : int = Array.fold_left (fun n quads -> n + List.length quads) 0 level.walls

(* a robot: an octahedron, turning *)
let view_robot (frames : int) (r : robot) : shape3d =
  let size = 7. in
  let tips = [ (size, 0., 0.); (-.size, 0., 0.); (0., size, 0.); (0., -.size, 0.); (0., 0., size); (0., 0., -.size) ] in
  let faces = [ (0, 2, 4); (2, 1, 4); (1, 3, 4); (3, 0, 4); (2, 0, 5); (1, 2, 5); (3, 1, 5); (0, 3, 5) ] in
  let tip i = List.nth tips i in
  group3d
    (List.map
       (fun (a, b, c) ->
         let points = [ tip a; tip b; tip c ] in
         let middle = times (1. /. 3.) (List.fold_left add (0., 0., 0.) points) in
         let up = (dot (normalize middle) (0., 1., 0.) +. 1.) /. 2. in
         let c x = int_of_float (float_of_int x *. (0.5 +. (0.5 *. up))) in
         polygon3d (rgb (c 210) (c 170) (c 60)) points)
       faces)
  |> rotate3d 0. (float_of_int frames *. 1.5) 0.
  |> (fun s ->
       let x, y, z = r.rp in
       move3d x y z s)

let view_shot (s : shot) : shape3d =
  let x, y, z = s.sp in
  cube (if s.player then rgb 250 240 120 else rgb 250 90 60) (if s.player then 2. else 2.6) |> move3d x y z

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let panel = 180.
let text color size str = words color str |> scale size

(* coupling: games2.5d/TinyDescent.ml's panel, with the cells it can't
 * count (they are all drawn, always) *)
let view_panel (screen : screen) (m : model) : shape list =
  let time = float_of_int (match m.over with Some (f, _) -> f | None -> m.frames) /. 60. in
  let stat i str = text (rgb 120 220 160) 2.5 str |> move (screen.left +. 200.) (screen.bottom +. 140. -. (float_of_int i *. 38.)) in
  let bar =
    let w = 300. *. float_of_int (max 0 m.shield) /. 100. in
    [ rectangle (rgb 30 60 45) 300. 26. |> move (screen.right -. 220.) (screen.bottom +. 120.);
      rectangle (rgb 90 230 150) w 26. |> move (screen.right -. 370. +. (w /. 2.)) (screen.bottom +. 120.) ]
  in
  [ rectangle (rgb 24 26 30) screen.width panel |> move_y (screen.bottom +. (panel /. 2.));
    stat 0 (Printf.sprintf "TIME %d:%02d" (int_of_float time / 60) (int_of_float time mod 60));
    stat 1 (Printf.sprintf "ROBOTS LEFT %d" (List.length m.robots));
    stat 2 (Printf.sprintf "CELLS DRAWN %d / %d" (Array.length level.segments) (Array.length level.segments));
    stat 3 (Printf.sprintf "POLYGONS %d" polygons);
    text (rgb 120 220 160) 2.5 "SHIELD" |> move (screen.right -. 220.) (screen.bottom +. 70.) ]
  @ bar

let view_sight (screen : screen) : shape list =
  let y = (screen.top +. screen.bottom +. panel) /. 2. in
  let c = rgb 120 230 160 in
  [ rectangle c 26. 2. |> move (-40.) y; rectangle c 26. 2. |> move 40. y;
    rectangle c 2. 26. |> move_y (y +. 40.); rectangle c 2. 26. |> move_y (y -. 40.) ]

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  (* the camera is the ship: looking where its nose points, and turning
   * with it -- [up] is what the other games leave alone *)
  (* 90 degrees over the view of games2.5d/TinyDescent, which is the
   * window minus the panel; here the camera fills the whole window, so
   * the angle is wider by that much, and the two pictures are at the
   * same scale *)
  let fov = 2. *. atan (screen.height /. (screen.height -. panel)) *. 180. /. Float.pi in
  let cam = camera ~eye:m.p ~target:(Sixdof.ahead m.ship ~from:m.p ~distance:10.) ~up:m.ship.up ~fov ~near:1. ~far:600. () in
  let huds =
    List.map hud (view_sight screen @ view_panel screen m)
    @ (match m.over with
      | Some (_, true) -> [ hud (text yellow 6. "ESCAPED!" |> move_y 250.); hud (text white 3. "space: again" |> move_y 170.) ]
      | Some (_, false) -> [ hud (text (rgb 250 80 60) 6. "DESTROYED" |> move_y 250.); hud (text white 3. "space: again" |> move_y 170.) ]
      | None ->
          if m.robots = [] then [ hud (text yellow 4. "ESCAPE: FIND THE EXIT" |> move_y 300.) ]
          else if m.frames < 240 then
            [ hud (text yellow 4. "DESTROY THE 4 ROBOTS" |> move_y 300.);
              hud (text white 2.5 "arrows: aim   a/d: roll   w/s: fly   space: fire" |> move_y 230.) ]
          else [])
  in
  (cam, (mine :: List.map (view_robot m.frames) m.robots) @ List.map view_shot m.shots @ huds)

let app = game3d view update initial_model

(* the colors are the mine's own, as in games2.5d/TinyDescent: no
 * lighting, so the two pictures differ only in how what is hidden is
 * decided *)
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = No_lighting } app
