(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Descent (Parallax Software, 1995), in the *2D*
 * playground: a ship flying through a mine, upside down if it likes,
 * to destroy the 4 robots and reach the exit. Arrows to aim (up/down
 * the nose, left/right around), a/d to roll, w/s to fly forwards and
 * backwards, space to fire.
 *
 * Descent was the first game where you could fly in every direction
 * and look anywhere, two years after Doom, whose player could not even
 * look up. The ship's directions are its own (kits/segments/Sixdof.mli),
 * and there is no floor.
 *
 * Yet its renderer has no z-buffer either. The mine is a set of closed
 * boxes ("segments", kits/segments/Segments.mli) that touch on
 * rectangles, and everything not in the box you are in is seen through
 * one of those rectangles, a "portal":
 *
 *      +--------+---------+        from the eye in cell 0, cell 1 is
 *      |        |         |        seen only inside the rectangle A,
 *      |  eye   A    1    B   2    and cell 2 only inside A and B
 *      |   o    |         |        both: each portal narrows the
 *      |        |         |        window the next cell shows in
 *      +--------+---------+
 *
 * So the drawing is a walk ([render]): start from the cell holding the
 * eye with the whole screen as its window, and for each of its portals,
 * visit the cell behind it with the window narrowed to the portal's
 * rectangle on screen (the two intersected, [window_of]). Cells come
 * out nearest first, as Doom's BSP walk does (games2.5d/TinyDoom.ml),
 * so they are drawn in reverse, farthest first, each clipped to its
 * window, and the nearer cells paint over the farther ones: the
 * painter's algorithm, correct here because the cells are convex and
 * only touch through their portals. The status bar counts the cells
 * drawn: most of the mine is never looked at.
 *
 * Real 3D on the 2D playground, then: the projection is here, three
 * dot products and a divide by the depth ([to_eye], [to_screen]), the
 * near-plane clipping is here ([clip]), and so is every polygon. What
 * isn't here is a depth per pixel: the order does that work, which is
 * what 1995's CPUs could afford. games3d/TinyDescent3d.ml is the same
 * mine handed to playground3d, with a z-buffer and no portals at all.
 *
 * Uses: the Segments kit (the mine, and Sixdof for the ship). Not
 * Tilemap, not Camera2d (the camera is the ship's own three
 * directions), not Sprite (every shape is a polygon).
 *
 * References: the Descent source code (Parallax, released 1997):
 * render.c's render_mine and its window rectangles, segment.c. Descent
 * also sorted the objects inside a cell, drew "transparent" wall
 * textures, and had a 3D automap.
 *
 * Exercises: inertia (Descent's ship keeps its speed sideways when you
 * stop thrusting), sliding left/right and up/down (its other two
 * degrees of freedom), a robot that chases you through the openings,
 * the reactor to destroy before the exit opens, lighting by the
 * distance of each corner instead of the cell (Descent's per-vertex
 * light).
 *)
open Playground

(*****************************************************************************)
(* The mine, and flying in it *)
(*****************************************************************************)

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
  (* the frame it ended on, and whether the exit was reached *)
  over : (int * bool) option;
}

(* one in the junction room, one upstairs, one in the reactor room, one
 * in the north room *)
let robots : robot list =
  List.map (fun p -> { rp = p; cool = 60 }) [ (100., 12., 20.); (100., 76., 20.); (100., 76., 110.); (20., 12., 100.) ]

(* facing the corridor east of the start room *)
let start_ship : Sixdof.t = Sixdof.turn ~yaw:(-90.) Sixdof.identity

let initial_model : model =
  { p = level.start; ship = start_ship; speed = 0.; cool = 0; shield = 100; robots; shots = []; frames = 0; over = None }

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

(* a shot dies in the rock, or of old age *)
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
      (* the guns: one shot every 12 frames *)
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
      (* what hit what *)
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
(* Seeing: from the world to the screen *)
(*****************************************************************************)

(* the status bar below the view *)
let panel = 180.

(* A point of the mine in the ship's own coordinates: how far to its
 * right, how far up, and how far ahead ([depth]) -- three dot products
 * (the "view transform" of a 3D engine, done here by hand). *)
type eye = { at : vec; ship : Sixdof.t; focal : number; middle : number }

let to_eye (e : eye) (p : vec) : vec =
  let r = sub p e.at in
  (dot r e.ship.right, dot r e.ship.up, dot r e.ship.forward)

(* nothing nearer than this is drawn: at depth 0 the divide below
 * explodes, and behind the ship the picture would come out upside
 * down (see the "c" key of the software 3D backend) *)
let near = 1.

(* the perspective: twice as far, half as big *)
let to_screen (e : eye) ((x, y, d) : vec) : number * number = (e.focal *. x /. d, e.middle +. (e.focal *. y /. d))

(* Sutherland-Hodgman, once: the polygon cut by one straight edge,
 * [inside] telling which side to keep, [cut] making the point where an
 * edge crosses. Going round the outline, each corner contributes what
 * of it survives:
 *
 *     in  -> in :  the corner
 *     in  -> out:  the crossing
 *     out -> in :  the crossing, then the corner
 *     out -> out:  nothing
 *
 * (Ivan Sutherland, Gary Hodgman, "Reentrant Polygon Clipping", CACM
 * 1974; graphics/3d/Clip.mli does the same for triangles.) *)
let clip (inside : 'a -> bool) (cut : 'a -> 'a -> 'a) (points : 'a list) : 'a list =
  let n = List.length points in
  if n = 0 then []
  else
    List.concat
      (List.mapi
         (fun i a ->
           let b = List.nth points ((i + 1) mod n) in
           match (inside a, inside b) with
           | true, true -> [ b ]
           | true, false -> [ cut a b ]
           | false, true -> [ cut a b; b ]
           | false, false -> [])
         points)

let clip_near (points : vec list) : vec list =
  let inside (_, _, d) = d >= near in
  let cut (x1, y1, d1) (x2, y2, d2) : vec =
    let t = (near -. d1) /. (d2 -. d1) in
    (x1 +. (t *. (x2 -. x1)), y1 +. (t *. (y2 -. y1)), near)
  in
  clip inside cut points

(* a window on screen: left, bottom, right, top *)
type window = { wx0 : number; wy0 : number; wx1 : number; wy1 : number }

let clip_window (w : window) (points : (number * number) list) : (number * number) list =
  let edge keep coord limit points =
    let inside p = if keep then coord p >= limit else coord p <= limit in
    let cut ((x1, y1) as a) ((x2, y2) as b) =
      let c1 = coord a and c2 = coord b in
      let t = if c2 = c1 then 0. else (limit -. c1) /. (c2 -. c1) in
      (x1 +. (t *. (x2 -. x1)), y1 +. (t *. (y2 -. y1)))
    in
    clip inside cut points
  in
  points
  |> edge true fst w.wx0 |> edge false fst w.wx1 |> edge true snd w.wy0 |> edge false snd w.wy1

let meet (a : window) (b : window) : window option =
  let w = { wx0 = Float.max a.wx0 b.wx0; wy0 = Float.max a.wy0 b.wy0; wx1 = Float.min a.wx1 b.wx1; wy1 = Float.min a.wy1 b.wy1 } in
  if w.wx0 < w.wx1 && w.wy0 < w.wy1 then Some w else None

let around (a : window) (b : window) : window =
  { wx0 = Float.min a.wx0 b.wx0; wy0 = Float.min a.wy0 b.wy0; wx1 = Float.max a.wx1 b.wx1; wy1 = Float.max a.wy1 b.wy1 }

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

type view = {
  e : eye;
  mutable shapes : shape list;
  mutable cells : int;
  mutable polygons : int;
}

(* a polygon of the mine: to the ship's coordinates, cut at the near
 * plane, projected, cut to the window, drawn *)
let draw (v : view) (w : window) (color : color) (points : vec list) : unit =
  let cut = clip_near (List.map (to_eye v.e) points) in
  if cut <> [] then begin
    let flat = clip_window w (List.map (to_screen v.e) cut) in
    if List.length flat >= 3 then begin
      v.polygons <- v.polygons + 1;
      v.shapes <- polygon color flat :: v.shapes
    end
  end

(* the rock's color: the cell's own, its light, a little contrast by
 * the way the wall faces (as Doom's "fake contrast"), and darker with
 * the distance *)
let wall_color (s : Segments.segment) (q : Segments.quad) (depth : number) : color =
  let r, g, b = s.rgb in
  (* the floor the brightest, the ceiling the darkest: a mine lit from
   * below, and the eye can then tell which way is which even upside
   * down (Doom's "fake contrast" made walls differ the same way) *)
  let contrast = match (q.side.axis, q.side.positive) with 1, false -> 1.35 | 1, true -> 0.7 | 0, _ -> 0.88 | _ -> 1.05 in
  let fade = 1. /. (1. +. (depth /. 260.)) in
  let c x = int_of_float (Float.min 255. (float_of_int x *. s.light *. contrast *. (0.35 +. (0.65 *. fade)))) in
  rgb (c r) (c g) (c b)

(* seen from the eye: a face whose inside is turned towards us *)
let facing (e : eye) (q : Segments.quad) : bool =
  match q.corners with [] -> false | c :: _ -> dot q.normal (sub e.at c) > 0.

(* where a portal is on screen: the box around its corners (Descent's
 * window rectangles were boxes too), narrowed to the window the cell
 * itself is seen in *)
let window_of (v : view) (w : window) (q : Segments.quad) : window option =
  let cut = clip_near (List.map (to_eye v.e) q.corners) in
  if cut = [] then None
  else
    let flat = List.map (to_screen v.e) cut in
    let xs = List.map fst flat and ys = List.map snd flat in
    let mn l = List.fold_left Float.min infinity l and mx l = List.fold_left Float.max neg_infinity l in
    meet w { wx0 = mn xs; wy0 = mn ys; wx1 = mx xs; wy1 = mx ys }

(* An octahedron for a robot, 8 triangles, the ones facing us drawn
 * (their outward normal is their middle, from the robot's own middle) *)
let draw_robot (v : view) (w : window) (r : robot) (spin : number) : unit =
  let size = 7. in
  let turned (x, y, z) =
    let a = spin *. Float.pi /. 180. in
    ((x *. cos a) -. (z *. sin a), y, (x *. sin a) +. (z *. cos a))
  in
  let tips = [ (size, 0., 0.); (-.size, 0., 0.); (0., size, 0.); (0., -.size, 0.); (0., 0., size); (0., 0., -.size) ] in
  let tip i = add r.rp (turned (List.nth tips i)) in
  let faces = [ (0, 2, 4); (2, 1, 4); (1, 3, 4); (3, 0, 4); (2, 0, 5); (1, 2, 5); (3, 1, 5); (0, 3, 5) ] in
  List.iter
    (fun (a, b, c) ->
      let points = [ tip a; tip b; tip c ] in
      let middle = times (1. /. 3.) (List.fold_left add (0., 0., 0.) points) in
      let out = sub middle r.rp in
      if dot out (sub v.e.at middle) > 0. then begin
        (* lit by how much the face looks up *)
        let up = (dot (normalize out) (0., 1., 0.) +. 1.) /. 2. in
        let c x = int_of_float (float_of_int x *. (0.5 +. (0.5 *. up))) in
        draw v w (rgb (c 210) (c 170) (c 60)) points
      end)
    faces

(* a shot: a small square turned towards the eye *)
let draw_shot (v : view) (w : window) (s : shot) : unit =
  let size = if s.player then 1.2 else 1.6 in
  let r = times size v.e.ship.right and u = times size v.e.ship.up in
  let corners = [ add (add s.sp r) u; add (sub s.sp r) u; sub (sub s.sp r) u; sub (add s.sp r) u ] in
  draw v w (if s.player then rgb 250 240 120 else rgb 250 90 60) corners

(* The walk: the cell the ship is in, then the cells its portals show,
 * each with the window it is seen in ([window_of]); breadth first, so
 * they come out nearest first, and a cell reached twice (a loop in the
 * mine) keeps the box around its two windows. Then everything is drawn
 * backwards: the farthest cell first. *)
let render (v : view) (m : model) (full : window) : unit =
  let n = Array.length level.segments in
  let windows = Array.make n None in
  let order = ref [] in
  let queue = Queue.create () in
  let start = Option.value (Segments.segment_at level m.p) ~default:0 in
  windows.(start) <- Some full;
  order := [ start ];
  Queue.add start queue;
  let steps = ref 0 in
  while (not (Queue.is_empty queue)) && !steps < 200 do
    incr steps;
    let i = Queue.pop queue in
    let w = Option.get windows.(i) in
    List.iter
      (fun (o : Segments.opening) ->
        match window_of v w o.quad with
        | None -> ()
        | Some nw -> (
            match windows.(o.into) with
            | None ->
                windows.(o.into) <- Some nw;
                order := o.into :: !order;
                Queue.add o.into queue
            | Some old ->
                let both = around old nw in
                if both <> old then begin
                  windows.(o.into) <- Some both;
                  Queue.add o.into queue
                end))
      level.openings.(i)
  done;
  (* farthest first *)
  List.iter
    (fun i ->
      let w = Option.get windows.(i) in
      v.cells <- v.cells + 1;
      let s = level.segments.(i) in
      List.iter
        (fun (q : Segments.quad) ->
          if facing v.e q then draw v w (wall_color s q (length (sub (List.hd q.corners) v.e.at))) q.corners)
        level.walls.(i);
      List.iter (fun (r : robot) -> if Segments.segment_at level r.rp = Some i then draw_robot v w r (float_of_int m.frames *. 1.5)) m.robots;
      List.iter (fun (s : shot) -> if Segments.segment_at level s.sp = Some i then draw_shot v w s) m.shots)
    !order

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text color size str = words color str |> scale size

let frame (screen : screen) (m : model) : view =
  let height = screen.height -. panel in
  let middle = (screen.top +. screen.bottom +. panel) /. 2. in
  (* a 90 degrees field of view: the top of the view is as far up as it
   * is ahead *)
  let e = { at = m.p; ship = m.ship; focal = height /. 2.; middle } in
  let v = { e; shapes = []; cells = 0; polygons = 0 } in
  render v m { wx0 = screen.left; wy0 = screen.bottom +. panel; wx1 = screen.right; wy1 = screen.top };
  (* the playground draws a list front to back: the last shape lands on
   * top. [render] collects them the other way round (each new one at
   * the head), so the list is turned around here -- the farthest cell
   * first, the ship's own cell last. *)
  v.shapes <- List.rev v.shapes;
  v

let view_panel (screen : screen) (m : model) (v : view) : shape list =
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
    stat 2 (Printf.sprintf "CELLS DRAWN %d / %d" v.cells (Array.length level.segments));
    stat 3 (Printf.sprintf "POLYGONS %d" v.polygons);
    text (rgb 120 220 160) 2.5 "SHIELD" |> move (screen.right -. 220.) (screen.bottom +. 70.) ]
  @ bar

(* the gunsight, in the middle of the view *)
let view_sight (screen : screen) : shape list =
  let y = (screen.top +. screen.bottom +. panel) /. 2. in
  let c = rgb 120 230 160 in
  [ rectangle c 26. 2. |> move (-40.) y; rectangle c 26. 2. |> move 40. y;
    rectangle c 2. 26. |> move_y (y +. 40.); rectangle c 2. 26. |> move_y (y -. 40.) ]

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  let v = frame screen m in
  v.shapes @ view_sight screen
  @ view_panel screen m v
  @ (match m.over with
    | Some (_, true) -> [ text yellow 6. "ESCAPED!" |> move_y 250.; text white 3. "space: again" |> move_y 170. ]
    | Some (_, false) -> [ text (rgb 250 80 60) 6. "DESTROYED" |> move_y 250.; text white 3. "space: again" |> move_y 170. ]
    | None ->
        if m.robots = [] then [ text yellow 4. "ESCAPE: FIND THE EXIT" |> move_y 300. ]
        else if m.frames < 240 then
          [ text yellow 4. "DESTROY THE 4 ROBOTS" |> move_y 300.;
            text white 2.5 "arrows: aim   a/d: roll   w/s: fly   space: fire" |> move_y 230. ]
        else [])

let app = game view update initial_model

let main = Playground_platform.run_app app
