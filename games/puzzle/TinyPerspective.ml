(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Perspective (DigiPen Institute of Technology, a
 * student game, 2012): place a camera in a 3D world, then play a 2D
 * platformer on the picture it takes.
 *
 *   tab          switch between the camera (3D) and the runner (2D)
 *   camera:      arrows turn round the world, w s nearer and farther
 *   runner:      left right run, space jump
 *
 * Perspective was made by a team of students, played by many more on
 * video, and shown at the Independent Games Festival's student
 * showcase: a small game whose one idea has stayed. The world is 3D,
 * the little runner is 2D, and the runner's world is the picture: what
 * the camera draws where he stands is where he stands. (Names and dates
 * from memory, to check.)
 *
 * It is the third game of a family here. TinyCrush and TinyFez flatten
 * the world along an axis, orthographically: an object is as wide on
 * the screen whatever its distance, and the puzzles are about what
 * lines up. This one flattens it through a lens, with perspective, the
 * way an eye does: the far is small and the near is big, so the camera
 * builds the level -- a long bridge far away is a short plank next to
 * a near ledge, if the camera puts it there:
 *
 *        eye  .                      the far bridge, small on the
 *              \  near ledge         picture, drawn right between
 *               \====      ======    the two near ledges: in 2D,
 *                \       far bridge  one floor
 *                 \  .......................
 *
 * The runner's world is built from the picture: each box of the world
 * projected through the camera ([project], Playground3d's own, so
 * exactly what is drawn), the convex hull of its eight corners filled
 * into a grid of the screen's cells ([picture_map]), and the running
 * and jumping gamekits/platformer's Tile_move on that grid, in the
 * screen's coordinates. The runner has no depth: he is on the screen.
 * Only at the switch to the camera is he given one, the point of the
 * box drawn under his feet ([unproject]: the ray through his feet met
 * with its top), and the camera turns round that point, so that he
 * stays on his ledge while the world turns round him. And at the
 * switch back to him, one rule: if the new picture puts him inside
 * something, the switch is refused -- what is drawn over him is
 * solid.
 *
 * What it uses: Playground3d (boxes, the camera, [project], the HUD
 * the runner is drawn on), gamekits/platformer's Tile_move and
 * Tilemap (the runner's world, a grid of the screen), Scene2d.
 *
 * Left undone, exercises: the free camera of the original (flown, not
 * turned round a point); objects that move in 3D while the runner
 * waits; hazards that kill in 2D only when seen; a picture with the
 * near plane cutting a box, which [project] does not clip (a box behind
 * the eye is left out of the runner's world).
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The levels *)
(*****************************************************************************)

(* a box: x0, x1, y0, y1, z0, z1, y up; and what it is *)
type kind = Stone | Start | Goal
type box = { x0 : float; x1 : float; y0 : float; y1 : float; z0 : float; z1 : float; kind : kind }

let b kind x0 x1 y0 y1 z0 z1 = { x0; x1; y0; y1; z0; z1; kind }

type level = { name : string; hint : string; boxes : box list; yaw : float; pitch : float; dist : float }

(* two ledges too far apart to jump, and far behind them, a long bridge,
 * its top at the ledges' height: seen from above, the far bridge is
 * higher on the picture than the near ledges; lowered to their height,
 * the camera puts every top at that height on one line, the horizon --
 * and the far bridge, small, fills the gap between them *)
let bridge =
  { name = "the far bridge"; hint = "tab: the camera. Lower it (down) until the far bridge meets the ledges, then tab back and run.";
    boxes =
      [ b Start (-6.) (-2.5) (-1.) 0. (-1.) 1.; b Goal 2.5 6. (-1.) 0. (-1.) 1.; b Stone (-12.) 12. (-1.) 0. (-26.) (-24.) ];
    yaw = 0.; pitch = 12.; dist = 24. }

(* a goal on a high shelf; three pillars at three depths, each higher
 * and farther: from above, the far ones stand high above the near; from
 * below their tops, and a little to the side, perspective shrinks the
 * far heights into steps a jump high -- a staircase *)
let stairs =
  { name = "the stairs"; hint = "turn the camera a little, and lower it below the pillars' tops: a staircase";
    boxes =
      [ b Start (-8.) (-4.) (-1.) 0. (-1.) 1.; b Stone (-2.) 0. (-1.) 2. (-6.) (-4.); b Stone 1. 3. (-1.) 4. (-14.) (-12.);
        b Stone 4. 6. (-1.) 6.5 (-24.) (-22.); b Goal 8. 12. 7. 8. (-34.) (-30.) ];
    yaw = 0.; pitch = 5.; dist = 16. }

let levels = [| bridge; stairs |]

(*****************************************************************************)
(* The camera, and the picture as a world *)
(*****************************************************************************)

(* turned round the point [focus] -- where the runner stands --, [yaw]
 * degrees round, [pitch] degrees up, [dist] away *)
let camera_of (focus : float * float * float) (yaw : float) (pitch : float) (dist : float) : camera =
  let r a = a *. Float.pi /. 180. in
  let cx, cy, cz = focus in
  let eye =
    ( cx +. (dist *. Float.cos (r pitch) *. Float.sin (r yaw)),
      cy +. (dist *. Float.sin (r pitch)),
      cz +. (dist *. Float.cos (r pitch) *. Float.cos (r yaw)) )
  in
  camera ~eye ~target:(cx, cy, cz) ~fov:60. ~far:500. ()

let corners (bx : box) : (float * float * float) list =
  List.concat_map (fun x -> List.concat_map (fun y -> List.map (fun z -> (x, y, z)) [ bx.z0; bx.z1 ]) [ bx.y0; bx.y1 ]) [ bx.x0; bx.x1 ]

(* the convex hull of points, counterclockwise (Andrew's monotone chain) *)
let hull (points : (float * float) list) : (float * float) list =
  let pts = List.sort_uniq compare points in
  let cross (ox, oy) (ax, ay) (bx, by) = ((ax -. ox) *. (by -. oy)) -. ((ay -. oy) *. (bx -. ox)) in
  let half ps =
    List.fold_left
      (fun acc p ->
        let rec pop = function a :: (b :: _ as rest) when cross b a p <= 0. -> pop rest | acc -> acc in
        p :: pop acc)
      [] ps
  in
  match pts with
  | [] | [ _ ] -> pts
  | _ ->
      let lower = half pts and upper = half (List.rev pts) in
      List.rev (List.tl lower) @ List.rev (List.tl upper)

(* where a box is on the picture: its projected hull, or nothing if a
 * corner is behind the eye *)
let silhouette (cam : camera) (screen : screen) (bx : box) : (float * float) list option =
  let ps = List.map (project cam screen) (corners bx) in
  if List.exists Option.is_none ps then None else Some (hull (List.filter_map Fun.id ps))

let inside (poly : (float * float) list) ((x, y) : float * float) : bool =
  match poly with
  | [] -> false
  | first :: _ ->
      let rec edges = function a :: (b :: _ as rest) -> (a, b) :: edges rest | [ last ] -> [ (last, first) ] | [] -> [] in
      List.for_all (fun ((ax, ay), (bx, by)) -> ((bx -. ax) *. (y -. ay)) -. ((by -. ay) *. (x -. ax)) >= 0.) (edges poly)

(* [unproject cam screen (sx, sy) y]: the point at height [y] that the
 * screen point (sx, sy) shows -- the ray from the eye through it, met
 * with that horizontal plane; [project] undone. The picture's focal
 * length is the half screen's height over tan (fov / 2), for both axes
 * (Playground3d's projection). None if the ray runs level or upwards
 * away from the plane. *)
let unproject (cam : camera) (screen : screen) ((sx, sy) : float * float) (y : float) : (float * float * float) option =
  let sub (a, b, c) (d, e, f) = (a -. d, b -. e, c -. f) in
  let norm (a, b, c) = let l = Float.sqrt ((a *. a) +. (b *. b) +. (c *. c)) in (a /. l, b /. l, c /. l) in
  let cross (a, b, c) (d, e, f) = ((b *. f) -. (c *. e), (c *. d) -. (a *. f), (a *. e) -. (b *. d)) in
  let fwd = norm (sub cam.target cam.eye) in
  let right = norm (cross fwd (0., 1., 0.)) in
  let up = cross right fwd in
  let f = screen.height /. 2. /. Float.tan (cam.fov *. Float.pi /. 360.) in
  let (fx, fy, fz), (rx, ry, rz), (ux, uy, uz) = (fwd, right, up) in
  let dx = fx +. (rx *. sx /. f) +. (ux *. sy /. f) and dy = fy +. (ry *. sx /. f) +. (uy *. sy /. f)
  and dz = fz +. (rz *. sx /. f) +. (uz *. sy /. f) in
  let ex, ey, ez = cam.eye in
  if Float.abs dy < 1e-6 then None
  else
    let t = (y -. ey) /. dy in
    if t <= 0. then None else Some (ex +. (t *. dx), y, ez +. (t *. dz))

(* the top edge of a silhouette above the screen's [x]: the highest
 * point of the polygon on that vertical, where the runner stands on
 * it *)
let top_at (poly : (float * float) list) (x : float) : float option =
  match poly with
  | [] -> None
  | first :: _ ->
      let rec edges = function a :: (b :: _ as rest) -> (a, b) :: edges rest | [ last ] -> [ (last, first) ] | [] -> [] in
      List.filter_map
        (fun ((ax, ay), (bx, by)) ->
          if (x < Float.min ax bx) || (x > Float.max ax bx) || ax = bx then None else Some (ay +. ((x -. ax) *. (by -. ay) /. (bx -. ax))))
        (edges poly)
      |> List.fold_left (fun acc y -> match acc with Some m when m >= y -> acc | _ -> Some y) None

let cell = 10.

(* The runner's world: the screen cut in cells, a cell solid if its
 * centre is inside a box's silhouette -- '#' stone (and the start),
 * 'E' the goal. *)
let picture_map (cam : camera) (screen : screen) (boxes : box list) : Tilemap.t =
  let shapes = List.filter_map (fun bx -> Option.map (fun s -> (bx.kind, s)) (silhouette cam screen bx)) boxes in
  let cols = int_of_float (screen.width /. cell) and rows = int_of_float (screen.height /. cell) in
  Tilemap.of_strings cell
    (List.init rows (fun r ->
         String.init cols (fun c ->
             let x = screen.left +. ((float_of_int c +. 0.5) *. cell) and y = screen.top -. ((float_of_int r +. 0.5) *. cell) in
             match List.find_opt (fun (_, s) -> inside s (x, y)) shapes with
             | Some (Goal, _) -> 'E'
             | Some _ -> '#'
             | None -> '.')))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type mode = Camera | Runner of Tilemap.t (* the picture he runs on *)

type play = {
  level : int;
  focus : float * float * float; (* what the camera turns round: a point on the top of *)
  on_box : int; (* this box, the runner's *)
  yaw : float;
  pitch : float;
  dist : float;
  mode : mode;
  (* the runner, on the screen *)
  x : float;
  y : float;
  vy : float;
  ground : bool;
  facing : float;
  message : string;
}

type scene = Title | Playing of play | Done
type model = scene Scene2d.t

let size = (18., 28.)

(* the runner starts standing on the start box, in the level's first
 * picture *)
(* the runner standing on his box's picture: in the middle of the
 * screen (the camera looks at his point), his feet on the top edge of
 * its silhouette *)
let stand (screen : screen) (p : play) : play =
  let cam = camera_of p.focus p.yaw p.pitch p.dist in
  match Option.bind (silhouette cam screen (List.nth levels.(p.level).boxes p.on_box)) (fun s -> top_at s 0.) with
  | Some top ->
      (* on the grid's cells: the cell boundary at or above the edge *)
      let g = screen.top -. (cell *. Float.of_int (int_of_float ((screen.top -. top) /. cell))) in
      { p with x = 0.; y = g +. (snd size /. 2.) +. 0.5; vy = 0. }
  | None -> { p with x = 0.; y = (snd size /. 2.) +. 1. }

let enter (screen : screen) (i : int) : play =
  let l = levels.(i) in
  let start_i = List.mapi (fun i bx -> (i, bx)) l.boxes |> List.find (fun (_, bx) -> bx.kind = Start) |> fst in
  let start = List.nth l.boxes start_i in
  let focus = ((start.x0 +. start.x1) /. 2., start.y1, (start.z0 +. start.z1) /. 2.) in
  let cam = camera_of focus l.yaw l.pitch l.dist in
  stand screen
    { level = i; focus; on_box = start_i; yaw = l.yaw; pitch = l.pitch; dist = l.dist;
      mode = Runner (picture_map cam screen l.boxes); x = 0.; y = 0.; vy = 0.; ground = false; facing = 1.; message = l.hint }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let solid (c : char) : bool = c = '#'

(* what the player does this frame *)
type input = { dx : float; dy : float; zoom : float; jump : bool; switch : bool }

let nothing = { dx = 0.; dy = 0.; zoom = 0.; jump = false; switch = false }

(* to the camera: it turns round where the runner stands -- the top of
 * the box drawn under his feet (the nearest, if several are), found in
 * 3D; the picture recentred on it, and the runner with it *)
let to_camera (screen : screen) (p : play) : play =
  let cam = camera_of p.focus p.yaw p.pitch p.dist in
  (* the middle of the grid's cell under his feet: solid, since he
   * stands on it, so inside some silhouette *)
  let feet = (p.x, p.y -. (snd size /. 2.) -. (cell /. 2.)) in
  let under =
    List.filteri (fun _ (_, bx) -> match silhouette cam screen bx with Some s -> inside s feet | None -> false)
      (List.mapi (fun i bx -> (i, bx)) levels.(p.level).boxes)
  in
  let dist2 (x, y, z) = let ex, ey, ez = cam.eye in ((x -. ex) ** 2.) +. ((y -. ey) ** 2.) +. ((z -. ez) ** 2.) in
  let center bx = ((bx.x0 +. bx.x1) /. 2., (bx.y0 +. bx.y1) /. 2., (bx.z0 +. bx.z1) /. 2.) in
  match List.sort (fun (_, a) (_, b) -> compare (dist2 (center a)) (dist2 (center b))) under with
  | [] -> { p with message = "he must stand on something" }
  | (i, bx) :: _ ->
      (* on its top, where the ray through his feet meets it -- or its
       * middle, seen level *)
      let focus =
        match unproject cam screen feet bx.y1 with
        | Some (x, y, z) -> (Float.max bx.x0 (Float.min bx.x1 x), y, Float.max bx.z0 (Float.min bx.z1 z))
        | None -> let cx, _, cz = center bx in (cx, bx.y1, cz)
      in
      stand screen { p with mode = Camera; focus; on_box = i; message = "" }

(* to the runner: the picture taken, unless it puts him inside something *)
let to_runner (screen : screen) (p : play) : play =
  let map = picture_map (camera_of p.focus p.yaw p.pitch p.dist) screen levels.(p.level).boxes in
  if Tile_move.hits solid map size p.x p.y then { p with message = "something is in his way, on this picture" }
  else { p with mode = Runner map; message = "" }

let step (screen : screen) (i : input) (p : play) : play =
  match p.mode with
  | Camera ->
      if i.switch then to_runner screen p
      else
        (* the camera turned, the runner riding his box's picture *)
        stand screen
          { p with yaw = p.yaw +. (i.dx *. 1.5); pitch = Float.max (-20.) (Float.min 70. (p.pitch +. (i.dy *. 1.)));
                   dist = Float.max 6. (Float.min 60. (p.dist +. (i.zoom *. 0.4))) }
  | Runner map ->
      if i.switch && p.ground then to_camera screen p
      else
        let vy = if i.jump && p.ground then 10. else Float.max (-12.) (p.vy -. 0.6) in
        let (x, y), _ = Tile_move.move_by solid map size (p.x, p.y) (i.dx *. 3.5, 0.) in
        let (x, y), hit_y = Tile_move.move_by solid map size (x, y) (0., vy) in
        { p with x; y; vy = (if hit_y then 0. else vy); ground = Tile_move.on_ground solid map size x y;
                 facing = (if i.dx <> 0. then i.dx else p.facing) }

let at_goal (p : play) : bool =
  match p.mode with Runner map -> Tile_move.hits (fun c -> c = 'E') map (fst size +. 4., snd size +. 4.) p.x p.y | Camera -> false

let fell (screen : screen) (p : play) : bool = p.y < screen.bottom -. 40.

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  let screen = computer.screen in
  match scenes.scene with
  | Title | Done -> if pressed (fun k -> k.kspace) then Scene2d.go (Playing (enter screen 0)) scenes else scenes
  | Playing p ->
      let k = computer.keyboard in
      let key s = Set_.mem s k.keys in
      let i =
        { dx = to_x k; dy = to_y k; zoom = (if key "s" then 1. else if key "w" then -1. else 0.); jump = pressed (fun k -> k.kspace);
          switch = pressed (fun k -> Set_.mem "Tab" k.keys) }
      in
      let p = step screen i p in
      if fell screen p then { scenes with scene = Playing { (enter screen p.level) with message = "he fell off the picture" } }
      else if at_goal p then
        if p.level + 1 < Array.length levels then Scene2d.go (Playing (enter screen (p.level + 1))) scenes else Scene2d.go Done scenes
      else { scenes with scene = Playing p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let box_shape (bx : box) : shape3d =
  let color = match bx.kind with Stone -> rgb 170 160 190 | Start -> rgb 120 170 230 | Goal -> rgb 250 200 80 in
  box color (bx.x1 -. bx.x0) (bx.y1 -. bx.y0) (bx.z1 -. bx.z0) |> move3d ((bx.x0 +. bx.x1) /. 2.) ((bx.y0 +. bx.y1) /. 2.) ((bx.z0 +. bx.z1) /. 2.)

(* the runner: a stick of a figure, 2D, drawn on the screen *)
let runner (p : play) : shape =
  let ink = match p.mode with Camera -> rgb 150 150 160 | Runner _ -> rgb 30 30 40 in
  group
    [ circle ink 6. |> move_y 8.; rectangle ink 4. 14. |> move_y (-3.); rectangle ink 3. 10. |> rotate 20. |> move (-3.) (-12.);
      rectangle ink 3. 10. |> rotate (-20.) |> move 3. (-12.); rectangle ink 12. 3. |> rotate (p.facing *. 20.) |> move 0. 0. ]
  |> move p.x p.y

let text (color : color) (sz : float) (s : string) : shape = words color s |> scale sz

let view (computer : computer) (model : model) : camera * shape3d list =
  let screen = computer.screen in
  let still = camera ~eye:(0., 0., 30.) ~target:(0., 0., 0.) ~far:500. () in
  match model.scene with
  | Title ->
      ( still,
        [ hud (text (rgb 40 40 60) 6. "TINY PERSPECTIVE" |> move_y 200.);
          hud (text (rgb 40 40 60) 2. "tab: move the camera, and the runner's world is what it sees" |> move_y 60.);
          hud (text (rgb 40 40 60) 2. "far is small, near is big: the camera builds the level" |> move_y 25.) ]
        @ List.map hud (Scene2d.blink 1. model [ text (rgb 40 40 60) 3. "PRESS SPACE" |> move_y (-200.) ]) )
  | Done -> (still, [ hud (text (rgb 40 40 60) 4. "THE LAST PICTURE" |> move_y 60.) ])
  | Playing p ->
      let l = levels.(p.level) in
      let cam = camera_of p.focus p.yaw p.pitch p.dist in
      ( cam,
        List.map box_shape l.boxes
        @ [ hud (runner p);
            hud (text (rgb 40 40 60) 2.2 (Printf.sprintf "%s   (%s)" l.name (match p.mode with Camera -> "the camera" | Runner _ -> "the runner"))
                 |> move_y (screen.top -. 40.));
            hud (text (rgb 60 60 90) 1.6 p.message |> move_y (screen.bottom +. 60.));
            hud (text (rgb 90 90 120) 1.5 "tab switch   camera: arrows turn, w s nearer farther   runner: arrows, space"
                 |> move_y (screen.bottom +. 30.)) ] )

let help = {|TinyPerspective
  tab switches between the camera and the runner
  camera: arrows turn round the world, w s nearer and farther
  runner: left right run, space jump
|}

let app = game3d view update initial_model

let main =
  print_string help;
  Playground3d_platform.run_app3d app
