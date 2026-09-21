(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* games2.5d/TinyBattlezone.ml again, with solid faces: the same plain,
 * the same pyramids and blocks, the same enemy closing in, but every
 * object a set of flat-shaded polygons drawn by playground3d, where the
 * 1980 game had only lines. Left/right to turn, up/down to move, space
 * to fire.
 *
 * The two side by side (their golden frames are the same battle) show
 * what a line drawing gets for free. Lines have nothing to hide: a
 * tank behind a pyramid shows through it, and nobody minds, because a
 * vector screen draws no surfaces for it to be behind. Faces must hide
 * each other, and here a z-buffer does it (graphics/3d's Zbuffer: the
 * depth of each pixel drawn so far, and a new one kept only if it is
 * nearer) -- so the pyramid now hides the tank, and the game changes
 * with it: an obstacle is cover, the radar is how you know what is
 * behind it. Activision's Battlezone (1998) is this, and so are all the
 * tank games since.
 *
 * The rules are the 2D game's, copied (see the coupling: comment); only
 * [view] is new, and it has no projection and no clipping of its own:
 * a camera at the periscope, and playground3d does the rest.
 *
 * Uses: Playground3d (its camera, box, polygon3d) and Camera3d.sky, and
 * Scene2d for the title and game over; the HUD is 2D shapes.
 *
 * Exercises: a turret that turns on its own and a hull that lags behind
 * it; tracks in the sand (a trail of flat dark quads, faded out); the
 * enemy hiding behind the blocks when its shell is not ready, now that
 * they hide it.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The battle: games2.5d/TinyBattlezone.ml's rules *)
(*****************************************************************************)

(* coupling: from here to the end of [update], a copy of
 * games2.5d/TinyBattlezone.ml's (the obstacles, the enemy's spawns and
 * aim, the shells), so that the same keys play the same battle *)

type v3 = number * number * number

let radians (deg : number) = deg *. Float.pi /. 180.

(* the direction a heading (in degrees) faces, on the ground: 0 is -z,
 * 90 is +x *)
let forward (heading : number) : v3 = (sin (radians heading), 0., -.cos (radians heading))

(* the obstacles, where they stand: pyramids and blocks, as in the
 * original, which shells don't go through *)
type obstacle = { ox : number; oz : number; pyramid : bool }

let obstacles =
  [ { ox = 10.; oz = -30.; pyramid = true }; { ox = -25.; oz = -20.; pyramid = false };
    { ox = 35.; oz = 10.; pyramid = false }; { ox = -40.; oz = 30.; pyramid = true };
    { ox = 5.; oz = 45.; pyramid = true }; { ox = -10.; oz = -60.; pyramid = false };
    { ox = 55.; oz = -45.; pyramid = true }; { ox = -60.; oz = -55.; pyramid = true } ]

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

(*****************************************************************************)
(* View: faces, and a camera *)
(*****************************************************************************)

let sand = rgb 70 80 55
let tank_green = rgb 70 150 70

(* [placed heading (x, z) shape]: a model facing -z turned to [heading]
 * and moved to (x, z) on the ground. rotate3d turns by the other hand
 * from our heading (which goes from -z towards +x), hence the minus *)
let placed (heading : number) ((x, z) : number * number) (s : shape3d) : shape3d =
  s |> rotate3d 0. (-.heading) 0. |> move3d x 0. z

(* the same tank as the 2D game's, a hull, a turret and a gun, as boxes *)
let tank (color : color) : shape3d =
  group3d
    [ box color 2.4 0.6 3.6 |> move_y3d 0.3;
      box color 1.2 0.5 1.4 |> move_y3d 0.85;
      box (rgb 40 90 40) 0.15 0.15 1.7 |> move3d 0. 0.85 (-1.55) ]

(* four triangles on a square base w wide, h high *)
let pyramid (color : color) (w : number) (h : number) : shape3d =
  let x = w /. 2. in
  let base = [| (-.x, 0., x); (x, 0., x); (x, 0., -.x); (-.x, 0., -.x) |] in
  group3d (List.init 4 (fun i -> polygon3d color [ base.(i); base.((i + 1) mod 4); (0., h, 0.) ]))

let obstacle (o : obstacle) : shape3d =
  (if o.pyramid then pyramid (rgb 150 140 110) 4. 4. else box (rgb 120 120 130) 3. 3. 3. |> move_y3d 1.5)
  |> move3d o.ox 0. o.oz

(* the mountains, as in the 2D game: a ring 400 away with the same
 * heights, each piece a triangle up from the ground *)
let mountains : shape3d list =
  let n = 48 and r = 400. in
  let heights = [| 0.; 20.; 45.; 15.; 0.; 30.; 60.; 25.; 10.; 0.; 35.; 12. |] in
  let point i y =
    let a = 2. *. Float.pi *. float_of_int i /. float_of_int n in
    (r *. sin a, y, -.r *. cos a)
  in
  List.init n (fun i ->
      let h = heights.(i mod Array.length heights) and h' = heights.((i + 1) mod Array.length heights) in
      polygon3d (rgb 90 80 100) [ point i 0.; point (i + 1) 0.; point (i + 1) h'; point i h ])

(* the night: a ring of dark walls round the eye, 900 away, behind the
 * mountains. Camera3d.sky would not do: its sky is a ceiling 10 above
 * the eye, and a mountain 60 high 400 away is above that ceiling *)
let sky ((x, z) : number * number) : shape3d list =
  let n = 24 and r = 900. in
  let at i y =
    let a = 2. *. Float.pi *. float_of_int i /. float_of_int n in
    (x +. (r *. sin a), y, z -. (r *. cos a))
  in
  List.init n (fun i -> polygon3d (rgb 20 20 40) [ at i (-1.); at (i + 1) (-1.); at (i + 1) 700.; at i 700. ])

(* the world never changes: built once *)
let world : shape3d = cached3d (List.map obstacle obstacles @ mountains)

let shell (sh : shell) : shape3d = cube (rgb 255 230 90) 0.3 |> move3d sh.sx 0.45 sh.sz

(* the tank's pieces flying apart, as in the 2D game *)
let explosion ((x, z, age) : number * number * int) : shape3d list =
  let t = float_of_int age /. 10. in
  List.init 8 (fun i ->
      let a = float_of_int (i * 47) *. Float.pi /. 180. in
      box tank_green 0.6 0.3 0.6
      |> rotate3d (float_of_int (age * 7)) (float_of_int (i * 40)) 0.
      |> move3d (x +. (t *. sin a)) (0.3 +. (t *. (2. -. (0.3 *. t)))) (z +. (t *. cos a)))

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the radar, as in the 2D game but filled: the enemy's blip, relative
 * to our heading (ahead is up) *)
let radar (screen : screen) (g : game) : shape list =
  let cy = screen.top -. 90. and r = 60. in
  let rel = radians (bearing g.x g.z g.enemy.ex g.enemy.ez -. g.heading) in
  let d = Float.min 1. (distance g.x g.z g.enemy.ex g.enemy.ez /. 80.) in
  [ circle (rgb 20 40 20) r |> fade 0.8 |> move_y cy;
    square (rgb 255 60 60) 8. |> move (r *. d *. sin rel) (cy +. (r *. d *. cos rel)) ]

let sight : shape list =
  List.map
    (fun (w, h, x, y) -> rectangle (rgb 60 255 60) w h |> move x y)
    [ (25., 2., -27.5, 0.); (25., 2., 27.5, 0.); (2., 12., -40., 6.); (2., 12., 40., 6.); (2., 25., 0., 37.5) ]

let periscope (g : game) : camera =
  let fx, _, fz = forward g.heading in
  camera ~eye:(g.x, 1., g.z) ~target:(g.x +. fx, 1., g.z +. fz) ~far:3000. ()

let view_game (computer : computer) (g : game) : camera * shape3d list =
  let screen = computer.screen in
  let cam = periscope g in
  let e = g.enemy in
  let things =
    (placed e.eh (e.ex, e.ez) (tank tank_green) :: List.map shell (List.filter_map Fun.id [ g.shot; g.enemy_shot ]))
    @ match g.explosion with Some ((_, _, age) as ex) when age < 90 -> explosion ex | _ -> []
  in
  let huds =
    sight @ radar screen g
    @ [ text (rgb 255 60 60) 3. (Printf.sprintf "SCORE %d" g.score) |> move (screen.left +. 160.) (screen.top -. 40.);
        text (rgb 255 60 60) 3. (Printf.sprintf "LIVES %d" g.lives) |> move (screen.right -. 140.) (screen.top -. 40.) ]
    (* hit: the periscope goes red, where the 2D game cracks it *)
    @ if g.hit > 0 then [ rectangle red screen.width screen.height |> fade 0.3 ] else []
  in
  (cam, (Camera3d.floor ~color:sand cam :: world :: sky (g.x, g.z)) @ things @ List.map hud huds)

(* the title: the tank turning slowly, seen from the front and above *)
let view_title (computer : computer) (s : model) : camera * shape3d list =
  ignore computer;
  let cam = camera ~eye:(0., 2.5, 7.) ~target:(0., 0.5, 0.) ~far:3000. () in
  ( cam,
    (Camera3d.floor ~color:sand cam :: placed (float_of_int (s.frames mod 360)) (0., 0.) (tank tank_green) :: sky (0., 0.))
    @ List.map hud
        ([ text (rgb 60 255 60) 6. "TINY BATTLEZONE 3D" |> move_y 300.;
           text (rgb 255 60 60) 2.5 "left/right: turn   up/down: move   space: fire" |> move_y (-250.) ]
        @ Scene2d.blink 1. s [ text (rgb 60 255 60) 3. "PRESS SPACE" |> move_y (-330.) ]) )

let view (computer : computer) (s : model) : camera * shape3d list =
  match s.scene with
  | Title -> view_title computer s
  | Playing g -> view_game computer g
  | Game_over score ->
      let screen = computer.screen in
      ( camera ~eye:(0., 1., 0.) ~target:(0., 1., -1.) ~far:3000. (),
        sky (0., 0.)
        @ List.map hud
          ([ rectangle black screen.width screen.height; text (rgb 255 60 60) 6. "GAME OVER";
             text (rgb 60 255 60) 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ]
          @ Scene2d.blink 1. s [ text (rgb 60 255 60) 3. "PRESS SPACE" |> move_y (-200.) ]) )

let app = game3d view update initial_model

(* flat shading, as the 1998 remake; the back faces drawn, for the sky
 * (seen from below, see Camera3d.sky) *)
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false } app
