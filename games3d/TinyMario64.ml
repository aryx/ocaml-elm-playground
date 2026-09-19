(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Super Mario 64 (Shigeru Miyamoto, Nintendo, 1996):
 * floating platforms, five stars to collect, running and jumping in 3D.
 * Arrows to run, space to jump (hold it to jump higher), a and d to
 * turn the camera around Mario.
 *
 * Super Mario 64 was the game that showed how a platformer works in 3D,
 * and three of its answers are here:
 *   - the controls are relative to the camera: up is "away from me",
 *     wherever the camera looks from, not north ([wanted_move]); Mario
 *     turns to face where he runs;
 *   - the camera is a character: Lakitu, filming Mario from his cloud,
 *     which the player turns (the C-buttons; here a and d), and which
 *     follows smoothly (Camera3d.follow) rather than rigidly;
 *   - a shadow under Mario, on whatever is below him ([shadow]): in 3D,
 *     it's how you judge where a jump will land -- depth is hard to see
 *     on a flat screen, a shadow on the platform shows it.
 *
 * And the platformer's "game feel" (Steve Swink, Game Feel, 2008; Maddy
 * Thorson's notes on Celeste): a jump still works a few frames after
 * running off a ledge ("coyote time", [coyote]), a jump pressed just
 * before landing happens on landing ("jump buffering", [buffer]), and
 * releasing the button early cuts the jump short (variable height).
 * Each is a few lines; without them the controls feel unfair.
 *
 * No physics engine (see plan_physics_teaching.md): Mario's feet land on
 * the platforms' tops, his sides are stopped by their sides, his head by
 * their undersides, box against box, one axis at a time.
 *
 * Exercises: moving platforms, a camera avoiding walls (Camera3d.mli),
 * the triple jump, wall jumps, enemies to stomp.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The course *)
(*****************************************************************************)

(* a platform: its center on the ground (x, z), its width (x) and depth
 * (z), its top's height, its thickness, its color *)
type platform = { px : number; pz : number; w : number; d : number; top : number; thick : number; color : color }

let grass = rgb 90 180 70
let dirt = rgb 170 120 70

let platforms =
  [ { px = 0.; pz = 0.; w = 24.; d = 24.; top = 0.; thick = 3.; color = grass };
    { px = -8.; pz = -8.; w = 4.; d = 4.; top = 2.; thick = 1.; color = dirt };
    { px = -8.; pz = -14.; w = 4.; d = 4.; top = 4.; thick = 1.; color = dirt };
    { px = -2.; pz = -16.; w = 4.; d = 4.; top = 6.; thick = 1.; color = dirt };
    { px = 4.; pz = -16.; w = 4.; d = 4.; top = 8.; thick = 1.; color = rgb 200 90 60 };
    { px = 10.; pz = -10.; w = 4.; d = 4.; top = 5.; thick = 1.; color = dirt };
    { px = 8.; pz = 4.; w = 3.; d = 3.; top = 2.5; thick = 1.; color = dirt } ]

(* the stars, where they float *)
let star_places = [ (-8., 5., -14.); (4., 9., -16.); (10., 6., -10.); (8., 3.5, 4.); (-10., 1., 10.) ]

let world : shape3d =
  cached3d (List.map (fun p -> box p.color p.w p.thick p.d |> move3d p.px (p.top -. (p.thick /. 2.)) p.pz) platforms)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type mario = {
  x : number;
  y : number; (* his feet *)
  z : number;
  vy : number;
  heading : number; (* where he faces, degrees (Camera3d's: 0 is -z) *)
  on_ground : bool;
  coyote : int; (* frames since he left the ground *)
  buffer : int; (* frames since space was pressed, while in the air *)
}

type level = {
  mario : mario;
  stars : (number * number * number) list; (* the ones left *)
  cam_yaw : number; (* where the camera looks from, turned with a/d *)
  cam : camera option; (* the camera, smoothed *)
  frames : int;
}

type scene = Title | Playing of level | Won of level
type model = scene Scene2d.t

let start = { x = 0.; y = 0.; z = 6.; vy = 0.; heading = 0.; on_ground = true; coyote = 0; buffer = 99 }
let new_level () = { mario = start; stars = star_places; cam_yaw = 0.; cam = None; frames = 0 }
let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Moving *)
(*****************************************************************************)

let height = 1.2 (* Mario's *)
let radius = 0.35 (* half his width *)
let gravity = 0.03
let jump_speed = 0.45 (* up to 0.45^2 / (2 * 0.03) = 3.4 high *)
let run_speed = 0.13

(* the platforms Mario's body overlaps, standing at (x, y, z) *)
let inside (p : platform) (x : number) (y : number) (z : number) : bool =
  Float.abs (x -. p.px) < (p.w /. 2.) +. radius
  && Float.abs (z -. p.pz) < (p.d /. 2.) +. radius
  && y < p.top -. 0.01 && y +. height > p.top -. p.thick

let blocked x y z = List.exists (fun p -> inside p x y z) platforms

(* The direction the arrows ask for, relative to the camera: up is the
 * camera's forward on the ground, right its right *)
let wanted_move (k : keyboard) (cam_yaw : number) : (number * number) option =
  let ahead = (if k.kup then 1. else 0.) -. if k.kdown then 1. else 0. in
  let side = (if k.kright then 1. else 0.) -. if k.kleft then 1. else 0. in
  if ahead = 0. && side = 0. then None
  else
    let fx, fz = Camera3d.forward cam_yaw in
    let rx, rz = (-.fz, fx) in
    let mx = (ahead *. fx) +. (side *. rx) and mz = (ahead *. fz) +. (side *. rz) in
    let n = Float.hypot mx mz in
    Some (mx /. n, mz /. n)

let step_mario (s : model) (k : keyboard) (cam_yaw : number) (m : mario) : mario =
  let space_pressed = Scene2d.pressed (fun k -> k.kspace) s in
  (* running, one axis at a time, stopped by the platforms' sides *)
  let m =
    match wanted_move k cam_yaw with
    | None -> m
    | Some (dx, dz) ->
        let heading = atan2 dx (-.dz) *. 180. /. Float.pi in
        let x = if blocked (m.x +. (run_speed *. dx)) m.y m.z then m.x else m.x +. (run_speed *. dx) in
        let z = if blocked x m.y (m.z +. (run_speed *. dz)) then m.z else m.z +. (run_speed *. dz) in
        { m with x; z; heading }
  in
  (* jumping: on the ground, or up to 6 frames after leaving it (coyote
   * time), or on landing if space was pressed up to 6 frames before
   * (buffering) *)
  let buffer = if space_pressed then 0 else m.buffer + 1 in
  let can_jump = m.on_ground || m.coyote < 6 in
  let m, buffer =
    if can_jump && buffer < 6 then ({ m with vy = jump_speed; on_ground = false; coyote = 99 }, 99) else (m, buffer)
  in
  (* space released on the way up: a shorter jump *)
  let vy = if m.vy > 0.15 && not k.kspace then 0.15 else m.vy in
  let vy = vy -. gravity in
  let y = m.y +. vy in
  (* landing on a top (feet crossing it going down), or the head
   * stopped by an underside (going up) *)
  let over (p : platform) = Float.abs (m.x -. p.px) < (p.w /. 2.) +. radius && Float.abs (m.z -. p.pz) < (p.d /. 2.) +. radius in
  let landing = List.find_opt (fun p -> over p && vy <= 0. && m.y >= p.top -. 0.001 && y <= p.top) platforms in
  let bumping = List.find_opt (fun p -> over p && vy > 0. && m.y +. height <= p.top -. p.thick && y +. height > p.top -. p.thick) platforms in
  match (landing, bumping) with
  | Some p, _ -> { m with y = p.top; vy = 0.; on_ground = true; coyote = 0; buffer }
  | None, Some p -> { m with y = p.top -. p.thick -. height; vy = 0.; on_ground = false; coyote = m.coyote + 1; buffer }
  | None, None -> { m with y; vy; on_ground = false; coyote = (if m.on_ground then 0 else m.coyote + 1); buffer }

let camera_for (l : level) : camera =
  Camera3d.behind ~back:9. ~height:5. ~ahead:0. ~look:1. { x = l.mario.x; y = l.mario.y; z = l.mario.z; heading = l.cam_yaw }

let update_level (s : model) (k : keyboard) (l : level) : level =
  let cam_yaw = l.cam_yaw +. (if Set_.mem "a" k.keys then -2.5 else 0.) +. if Set_.mem "d" k.keys then 2.5 else 0. in
  let mario = step_mario s k cam_yaw l.mario in
  (* fallen off the course: back to the start *)
  let mario = if mario.y < -30. then start else mario in
  let near (x, y, z) = Float.hypot (Float.hypot (x -. mario.x) (z -. mario.z)) (y -. (mario.y +. 0.6)) < 1.2 in
  let l = { l with mario; cam_yaw; stars = List.filter (fun st -> not (near st)) l.stars; frames = l.frames + 1 } in
  let wanted = camera_for l in
  { l with cam = Some (match l.cam with None -> wanted | Some cam -> Camera3d.follow 0.15 wanted cam) }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  match s.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (Playing (new_level ())) s else s
  | Playing l ->
      let l = update_level s computer.keyboard l in
      if l.stars = [] then Scene2d.go (Won l) s else { s with scene = Playing l }
  | Won _ -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* Mario, facing -z: shoes, overalls, shirt, head, cap and its visor *)
let mario_model : shape3d =
  let red = rgb 220 30 30 and blue = rgb 40 70 200 and skin = rgb 250 190 140 and brown = rgb 110 60 20 in
  group3d
    [ box brown 0.28 0.18 0.4 |> move3d (-0.17) 0.09 (-0.05); box brown 0.28 0.18 0.4 |> move3d 0.17 0.09 (-0.05);
      box blue 0.6 0.4 0.4 |> move_y3d 0.38; box red 0.66 0.3 0.42 |> move_y3d 0.72;
      box skin 0.46 0.4 0.44 |> move_y3d 1.05; box red 0.5 0.14 0.5 |> move_y3d 1.3;
      box red 0.4 0.06 0.25 |> move3d 0. 1.25 (-0.33) ]

(* a star: an octahedron of gold, turning *)
let star (turn : number) : shape3d =
  let gold = rgb 255 210 40 in
  let top = (0., 0.7, 0.) and bottom = (0., -0.7, 0.) in
  let ring = [ (0.5, 0., 0.); (0., 0., 0.5); (-0.5, 0., 0.); (0., 0., -0.5) ] in
  let faces =
    List.concat
      (List.mapi
         (fun i a ->
           let b = List.nth ring ((i + 1) mod 4) in
           [ polygon3d gold [ top; a; b ]; polygon3d (rgb 230 170 20) [ bottom; b; a ] ])
         ring)
  in
  group3d faces |> rotate3d 0. turn 0.

(* The shadow: a dark square on the highest top below Mario's feet *)
let shadow (m : mario) : shape3d list =
  let under = List.filter (fun p -> Float.abs (m.x -. p.px) < p.w /. 2. && Float.abs (m.z -. p.pz) < p.d /. 2. && p.top <= m.y +. 0.01) platforms in
  match List.sort (fun a b -> compare b.top a.top) under with
  | p :: _ ->
      let y = p.top +. 0.02 and r = 0.4 in
      [ polygon3d (rgb 30 50 20) [ (m.x -. r, y, m.z -. r); (m.x +. r, y, m.z -. r); (m.x +. r, y, m.z +. r); (m.x -. r, y, m.z +. r) ] ]
  | [] -> []

let text color size str = words color str |> scale size

let scenery (cam : camera) = Camera3d.floor ~color:(rgb 60 120 200) ~ground:(-30.) cam :: Camera3d.sky ~sky:(rgb 150 205 250) ~horizon:(rgb 70 135 210) ~ground:(-30.) cam

let view_level (computer : computer) (l : level) : camera * shape3d list =
  let cam = match l.cam with Some c -> c | None -> camera_for l in
  let m = l.mario in
  let turn = spin 3. computer.time in
  ( cam,
    scenery cam @ [ world ] @ shadow m
    @ [ mario_model |> rotate3d 0. (-.m.heading) 0. |> move3d m.x m.y m.z ]
    @ List.map (fun (x, y, z) -> star turn |> move3d x y z) l.stars )

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      let cam = Camera3d.orbit ~distance:30. ~height:18. ~look:2. (spin 15. computer.time) (0., 0., -5.) in
      ( cam,
        scenery cam @ [ world ] @ List.map (fun (x, y, z) -> star (spin 3. computer.time) |> move3d x y z) star_places
        @ List.map hud
            ([ text (rgb 230 40 40) 7. "TINY MARIO 64" |> move_y 320.;
               text white 2.5 "arrows: run   space: jump (hold: higher)   a/d: camera" |> move_y 250. ]
            @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 190. ]) )
  | Playing l ->
      let cam, shapes = view_level computer l in
      let found = List.length star_places - List.length l.stars in
      (cam, shapes @ [ hud (text yellow 3. (Printf.sprintf "STARS %d / %d" found (List.length star_places)) |> move (screen.left +. 170.) (screen.top -. 40.)) ])
  | Won l ->
      let cam, shapes = view_level computer l in
      ( cam,
        shapes
        @ List.map hud
            ([ text yellow 6. "ALL THE STARS!" |> move_y 200. ] @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y 120. ]) )

let app = game3d view update initial_model

(* flat shading; the back faces drawn too, for the sky (Camera3d.sky) *)
let main =
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false } app
