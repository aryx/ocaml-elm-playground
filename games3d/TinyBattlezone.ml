(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Battlezone (Ed Rotberg, Atari, 1980), the first 3D
 * arcade hit: you look through your tank's periscope at a plain of
 * pyramids and blocks, under mountains, and duel enemy tanks drawn in
 * green lines on black. Left/right to turn, up/down to move, space to
 * fire (one shell at a time); watch the radar, and the "ENEMY TO LEFT"
 * warnings.
 *
 * Battlezone's screen was a vector monitor: no pixels, an electron beam
 * drawing lines from point to point (the monitor of Asteroids, with a
 * "math box" to compute the 3D). So its 3D is the simplest there is:
 * every object is a list of line segments (see [box_edges]), each end
 * projected onto the screen (Playground3d.project) and the two joined by
 * a line. No faces, so no hidden surfaces to remove: you see through
 * everything, and that's the look. The one thing to get right is a
 * segment going behind you: projecting its far end would draw garbage,
 * so it's cut where it crosses a plane just in front of the eye first
 * ([clip], near-plane clipping, the 3D pipeline's first step; see
 * graphics/3d's Clip for triangles). Everything drawn is then 2D lines,
 * given to playground3d as a HUD, so it looks the same on every backend.
 *
 * The US Army asked Atari for a version to train gunners, the "Bradley
 * Trainer" (1981): one of the first military uses of a video game.
 * Rotberg himself didn't like working on it.
 *
 * Exercises: the flying saucer, the fast "super tanks" and the guided
 * missiles of the original, the volcano erupting on the horizon, a
 * second player over the network (plan_networking_teaching.md).
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* Vectors, and the models: lists of segments *)
(*****************************************************************************)

type v3 = number * number * number
type segment = v3 * v3

let ( +| ) (x1, y1, z1) (x2, y2, z2) = (x1 +. x2, y1 +. y2, z1 +. z2)
let ( -| ) (x1, y1, z1) (x2, y2, z2) = (x1 -. x2, y1 -. y2, z1 -. z2)
let ( *| ) k (x, y, z) = (k *. x, k *. y, k *. z)
let dot (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

let radians (deg : number) = deg *. Float.pi /. 180.

(* the direction a heading (in degrees) faces, on the ground: 0 is -z,
 * 90 is +x *)
let forward (heading : number) : v3 = (sin (radians heading), 0., -.cos (radians heading))

(* the 12 edges of a w x h x d box, standing on the ground (y from 0 to
 * h), centered on the y axis *)
let box_edges ?(y0 = 0.) (w : number) (h : number) (d : number) : segment list =
  let x = w /. 2. and z = d /. 2. in
  let c i j k = ((if i then x else -.x), (if j then y0 +. h else y0), if k then z else -.z) in
  [ (c false false false, c true false false); (c true false false, c true false true);
    (c true false true, c false false true); (c false false true, c false false false);
    (c false true false, c true true false); (c true true false, c true true true);
    (c true true true, c false true true); (c false true true, c false true false);
    (c false false false, c false true false); (c true false false, c true true false);
    (c true false true, c true true true); (c false false true, c false true true) ]

(* a pyramid of base w and height h: 4 base edges, 4 to the top *)
let pyramid_edges (w : number) (h : number) : segment list =
  let x = w /. 2. in
  let base = [ (-.x, 0., -.x); (x, 0., -.x); (x, 0., x); (-.x, 0., x) ] in
  let top = (0., h, 0.) in
  List.mapi (fun i p -> (p, List.nth base ((i + 1) mod 4))) base @ List.map (fun p -> (p, top)) base

(* an enemy tank: a hull, a turret, a gun pointing forward (-z) *)
let tank_edges : segment list =
  box_edges 2.4 0.6 3.6 @ box_edges ~y0:0.6 1.2 0.5 1.4 @ [ ((0., 0.85, -0.7), (0., 0.85, -2.4)) ]

(* [place heading (x, z) segments]: a model turned to [heading] and moved
 * to (x, z) on the ground *)
let place (heading : number) ((tx, tz) : number * number) (segs : segment list) : segment list =
  let a = radians heading in
  let tr (x, y, z) = (tx +. (x *. cos a) -. (z *. sin a), y, tz +. (x *. sin a) +. (z *. cos a)) in
  List.map (fun (p, q) -> (tr p, tr q)) segs

(* the obstacles, where they stand: pyramids and blocks, as in the
 * original, which shells don't go through *)
type obstacle = { ox : number; oz : number; pyramid : bool }

let obstacles =
  [ { ox = 10.; oz = -30.; pyramid = true }; { ox = -25.; oz = -20.; pyramid = false };
    { ox = 35.; oz = 10.; pyramid = false }; { ox = -40.; oz = 30.; pyramid = true };
    { ox = 5.; oz = 45.; pyramid = true }; { ox = -10.; oz = -60.; pyramid = false };
    { ox = 55.; oz = -45.; pyramid = true }; { ox = -60.; oz = -55.; pyramid = true } ]

let obstacle_edges (o : obstacle) : segment list =
  place 0. (o.ox, o.oz) (if o.pyramid then pyramid_edges 4. 4. else box_edges 3. 3. 3.)

(* the mountains, all around, far away: they turn with you but never get
 * closer, as in the original (which drew them as a 2D band, scrolled) *)
let mountains : segment list =
  let n = 48 and r = 400. in
  let heights = [| 0.; 20.; 45.; 15.; 0.; 30.; 60.; 25.; 10.; 0.; 35.; 12. |] in
  let point i =
    let a = 2. *. Float.pi *. float_of_int i /. float_of_int n in
    (r *. sin a, heights.(i mod Array.length heights), -.r *. cos a)
  in
  List.init n (fun i -> (point i, point (i + 1)))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type enemy = { ex : number; ez : number; eh : number; cooldown : int }

(* a shell: where, its heading, the frames left before it's spent *)
type shell = { sx : number; sz : number; sh : number; life : int }

type game = {
  x : number;
  z : number;
  heading : number;
  enemy : enemy;
  shot : shell option; (* ours: one at a time *)
  enemy_shot : shell option;
  explosion : (number * number * int) option; (* where, and its age *)
  hit : int; (* > 0: we were hit, the screen cracked, for that long *)
  score : int;
  lives : int;
  spawns : int; (* how many enemies appeared: the next one's place *)
  frames : int;
}

type scene = Title | Playing of game | Game_over of int

type model = scene Scene2d.t

(* where the n-th enemy appears, around us: no randomness, a fixed
 * sequence of bearings, so a game can be replayed *)
let spawn (n : int) (x : number) (z : number) : enemy =
  let bearings = [| 20.; -70.; 160.; -130.; 90.; -20.; 200.; 50. |] in
  let a = bearings.(n mod Array.length bearings) in
  let fx, _, fz = forward a in
  { ex = x +. (60. *. fx); ez = z +. (60. *. fz); eh = a +. 180.; cooldown = 120 }

let new_game () : game =
  { x = 0.; z = 0.; heading = 0.; enemy = spawn 0 0. 0.; shot = None; enemy_shot = None; explosion = None;
    hit = 0; score = 0; lives = 3; spawns = 1; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let distance (x1 : number) (z1 : number) (x2 : number) (z2 : number) : number =
  Float.sqrt (((x1 -. x2) ** 2.) +. ((z1 -. z2) ** 2.))

let blocked (x : number) (z : number) : bool = List.exists (fun o -> distance x z o.ox o.oz < 3.) obstacles

(* the heading from (x1, z1) to (x2, z2), and an angle in -180..180 *)
let bearing x1 z1 x2 z2 = atan2 (x2 -. x1) (-.(z2 -. z1)) *. 180. /. Float.pi
let normalize (a : number) : number = Float.rem (Float.rem (a +. 180.) 360. +. 360.) 360. -. 180.

let move_shell (s : shell) : shell option =
  let fx, _, fz = forward s.sh in
  let s = { s with sx = s.sx +. fx; sz = s.sz +. fz; life = s.life - 1 } in
  if s.life = 0 || blocked s.sx s.sz then None else Some s

(* The enemy turns towards us, closes in, and fires when it's aimed *)
let move_enemy (g : game) : game =
  let e = g.enemy in
  let wanted = bearing e.ex e.ez g.x g.z in
  let turn = normalize (wanted -. e.eh) in
  let eh = e.eh +. Float.max (-1.) (Float.min 1. turn) in
  let fx, _, fz = forward eh in
  let far = distance e.ex e.ez g.x g.z > 25. in
  let ex, ez = if far then (e.ex +. (0.08 *. fx), e.ez +. (0.08 *. fz)) else (e.ex, e.ez) in
  let ex, ez = if blocked ex ez then (e.ex, e.ez) else (ex, ez) in
  let aimed = Float.abs turn < 3. in
  if aimed && e.cooldown = 0 && g.enemy_shot = None then
    { g with enemy = { ex; ez; eh; cooldown = 180 }; enemy_shot = Some { sx = ex; sz = ez; sh = eh; life = 120 } }
  else { g with enemy = { ex; ez; eh; cooldown = max 0 (e.cooldown - 1) } }

let update_game (computer : computer) (s : model) (g : game) : game =
  let g = { g with frames = g.frames + 1; explosion = Option.map (fun (x, z, age) -> (x, z, age + 1)) g.explosion } in
  if g.hit > 0 then { g with hit = g.hit - 1 }
  else
    let k = computer.keyboard in
    let heading = g.heading +. (if k.kleft then -1.5 else 0.) +. if k.kright then 1.5 else 0. in
    let fx, _, fz = forward heading in
    let speed = (if k.kup then 0.2 else 0.) -. if k.kdown then 0.12 else 0. in
    let x, z = (g.x +. (speed *. fx), g.z +. (speed *. fz)) in
    let x, z = if blocked x z then (g.x, g.z) else (x, z) in
    let fire = Scene2d.pressed (fun k -> k.kspace) s && g.shot = None in
    let shot = if fire then Some { sx = x; sz = z; sh = heading; life = 150 } else Option.bind g.shot move_shell in
    let g = move_enemy { g with x; z; heading; shot } in
    let enemy_shot = Option.bind g.enemy_shot move_shell in
    let g = { g with enemy_shot } in
    (* our shell in the enemy: it explodes, the next one appears *)
    let g =
      match g.shot with
      | Some sh when distance sh.sx sh.sz g.enemy.ex g.enemy.ez < 2. ->
          { g with
            shot = None;
            score = g.score + 1000;
            explosion = Some (g.enemy.ex, g.enemy.ez, 0);
            enemy = spawn g.spawns g.x g.z;
            spawns = g.spawns + 1 }
      | _ -> g
    in
    (* its shell in us: the screen cracks, and it comes back from elsewhere *)
    match g.enemy_shot with
    | Some sh when distance sh.sx sh.sz g.x g.z < 1.5 ->
        { g with enemy_shot = None; lives = g.lives - 1; hit = 120; enemy = spawn g.spawns g.x g.z; spawns = g.spawns + 1 }
    | _ -> g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if g.lives = 0 && g.hit = 0 then Scene2d.go (Game_over g.score) s else { s with scene = Playing g }
  | Game_over _ -> if space || s.elapsed > 10. then Scene2d.go Title s else s

(*****************************************************************************)
(* Drawing lines *)
(*****************************************************************************)

let vector_green = rgb 60 255 60
let vector_red = rgb 255 60 60

(* a 2D line, as a thin rectangle *)
let line (color : color) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle color (Float.sqrt ((dx *. dx) +. (dy *. dy))) 2.
  |> rotate (atan2 dy dx *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

(* Near-plane clipping: the part of a segment in front of the plane at
 * distance [near] ahead of the eye. With d(p) the distance of p in front
 * of the plane, a segment from p (in front) to q (behind) is cut at
 * p + (q - p) * d(p) / (d(p) - d(q)), where d is 0.
 *
 *        eye   near plane
 *         >      |    p
 *                |   /       only p..r is drawn
 *                |  /
 *                | r
 *               /|
 *             q  |
 *)
let near = 0.5

let clip (eye : v3) (fwd : v3) ((p, q) : segment) : segment option =
  let d v = dot (v -| eye) fwd -. near in
  let dp = d p and dq = d q in
  let cut a b da db = a +| ((da /. (da -. db)) *| (b -| a)) in
  if dp < 0. && dq < 0. then None
  else if dp < 0. then Some (cut q p dq dp, q)
  else if dq < 0. then Some (p, cut p q dp dq)
  else Some (p, q)

let draw (color : color) (cam : camera) (screen : screen) (fwd : v3) (segs : segment list) : shape list =
  List.filter_map
    (fun seg ->
      match clip cam.eye fwd seg with
      | None -> None
      | Some (p, q) -> (
          match (project cam screen p, project cam screen q) with
          | Some a, Some b -> Some (line color a b)
          | _ -> None))
    segs

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the radar: the enemy's blip, where it is relative to our heading
 * (ahead is up), and the sweep turning *)
let radar (screen : screen) (g : game) : shape list =
  let cx = 0. and cy = screen.top -. 90. and r = 60. in
  let circle_lines =
    List.init 24 (fun i ->
        let a i = 2. *. Float.pi *. float_of_int i /. 24. in
        line vector_red (cx +. (r *. cos (a i)), cy +. (r *. sin (a i))) (cx +. (r *. cos (a (i + 1))), cy +. (r *. sin (a (i + 1)))))
  in
  let sweep = radians (float_of_int (g.frames * 4)) in
  let rel = radians (bearing g.x g.z g.enemy.ex g.enemy.ez -. g.heading) in
  let d = Float.min 1. (distance g.x g.z g.enemy.ex g.enemy.ez /. 80.) in
  circle_lines
  @ [ line vector_red (cx, cy) (cx +. (r *. sin sweep), cy +. (r *. cos sweep));
      square vector_red 6. |> move (cx +. (r *. d *. sin rel)) (cy +. (r *. d *. cos rel)) ]

(* the gunsight, in the middle *)
let sight : shape list =
  [ line vector_green (-40., 0.) (-15., 0.); line vector_green (15., 0.) (40., 0.);
    line vector_green (-40., 0.) (-40., 12.); line vector_green (40., 0.) (40., 12.);
    line vector_green (0., 25.) (0., 50.) ]

(* hit: the periscope's glass cracks *)
let cracks : shape list =
  List.map
    (fun (a, b) -> line white a b)
    [ ((0., 0.), (-200., 150.)); ((-200., 150.), (-320., 170.)); ((0., 0.), (180., 200.)); ((0., 0.), (250., -120.));
      ((250., -120.), (380., -110.)); ((0., 0.), (-150., -220.)); ((-150., -220.), (-160., -330.)); ((0., 0.), (60., 260.)) ]

(* the tank's pieces flying apart *)
let explosion_edges ((x, z, age) : number * number * int) : segment list =
  let t = float_of_int age /. 10. in
  List.mapi
    (fun i (p, q) ->
      let a = radians (float_of_int (i * 47)) in
      let off = (t *. sin a, t *. (2. -. (0.3 *. t)), t *. cos a) in
      (p +| off, q +| off))
    (place 0. (x, z) tank_edges)

let view_game (computer : computer) (g : game) : camera * shape3d list =
  let screen = computer.screen in
  let fwd = forward g.heading in
  let eye = (g.x, 1., g.z) in
  let cam = camera ~eye ~target:(eye +| fwd) () in
  let draw = draw vector_green cam screen fwd in
  let e = g.enemy in
  let shells = List.filter_map Fun.id [ g.shot; g.enemy_shot ] in
  let world =
    draw mountains
    @ List.concat_map (fun o -> draw (obstacle_edges o)) obstacles
    @ draw (place e.eh (e.ex, e.ez) tank_edges)
    @ List.concat_map (fun (sh : shell) -> draw (place sh.sh (sh.sx, sh.sz) (box_edges ~y0:0.8 0.3 0.3 0.3))) shells
    @ (match g.explosion with Some ((_, _, age) as ex) when age < 90 -> draw (explosion_edges ex) | _ -> [])
  in
  let rel = normalize (bearing g.x g.z e.ex e.ez -. g.heading) in
  let warning =
    if Float.abs rel < 30. then []
    else if Float.abs rel > 135. then [ "ENEMY TO REAR" ]
    else if rel < 0. then [ "ENEMY TO LEFT" ]
    else [ "ENEMY TO RIGHT" ]
  in
  let huds =
    world @ sight @ radar screen g
    @ List.map (fun w -> text vector_red 3. w |> move_y (screen.top -. 190.)) warning
    @ [ text vector_red 3. (Printf.sprintf "SCORE %d" g.score) |> move (screen.left +. 160.) (screen.top -. 40.);
        text vector_red 3. (Printf.sprintf "LIVES %d" g.lives) |> move (screen.right -. 140.) (screen.top -. 40.) ]
    @ if g.hit > 0 then cracks else []
  in
  (cam, [ hud (rectangle black screen.width screen.height); hud (group huds) ])

(* the title: a tank turning slowly, seen from the front *)
let view_title (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  let eye = (0., 2.5, 7.) in
  let cam = camera ~eye ~target:(0., 0.5, 0.) () in
  let fwd = (0., 0., -1.) in
  let tank = draw vector_green cam screen fwd (place (float_of_int (s.frames mod 360)) (0., 0.) tank_edges) in
  let shapes =
    [ text vector_green 6. "TINY BATTLEZONE" |> move_y 300.;
      text vector_red 2.5 "left/right: turn   up/down: move   space: fire" |> move_y (-250.) ]
    @ Scene2d.blink 1. s [ text vector_green 3. "PRESS SPACE" |> move_y (-330.) ]
  in
  (cam, [ hud (rectangle black screen.width screen.height); hud (group (tank @ shapes)) ])

let view (computer : computer) (s : model) : camera * shape3d list =
  match s.scene with
  | Title -> view_title computer s
  | Playing g -> view_game computer g
  | Game_over score ->
      let screen = computer.screen in
      ( camera ~eye:(0., 1., 0.) ~target:(0., 1., -1.) (),
        [ hud (rectangle black screen.width screen.height);
          hud
            (group
               ([ text vector_red 6. "GAME OVER"; text vector_green 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ]
               @ Scene2d.blink 1. s [ text vector_green 3. "PRESS SPACE" |> move_y (-200.) ])) ] )

let app = game3d view update initial_model

let main = Playground3d_platform.run_app3d app
