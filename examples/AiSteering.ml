(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* One creature, seven characters: Craig Reynolds's steering behaviours
 * (1999), a key each, on the same body with the same top speed and the
 * same limit on how hard it turns.
 *
 *   1 seek     straight at the mouse, at full speed
 *   2 flee     straight away from it
 *   3 arrive   to the mouse, slowing down, and stopping there
 *   4 pursue   the orange prey, aiming where it will be
 *   5 wander   an idle stroll
 *   6 avoid    wandering among rocks
 *   7 follow   along a road
 *
 * The red arrow is the steering force -- what the behaviour adds to the
 * body this frame: the difference between the velocity it wants and the
 * velocity it has, never more than the body can turn. The trail shows
 * what that does over a few seconds: seek's loops round the mouse
 * (it overshoots, like a homing missile), arrive's straight line that
 * stops, wander's lazy S.
 *
 * What it uses: playground/Ai (seek, flee, arrive, chase, wandering,
 * avoiding, following, facing: each a verb on a Physics.body, next to
 * fall and push), over ai/Steering; Physics for the body. The overlays
 * (the slowing circle, the predicted point, the corridor ahead) are
 * drawn here from the same numbers as ai/Steering's defaults. *)
open Playground

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

type mode = Seek | Flee | Arrive | Pursue | Wander | Avoid | Follow

let modes =
  [ ("1", Seek, "seek: straight at the mouse -- it overshoots and loops back, a homing missile");
    ("2", Flee, "flee: straight away from the mouse");
    ("3", Arrive, "arrive: to the mouse, slower within the circle, stopping on it");
    ("4", Pursue, "pursue: where the prey will be, not where it is");
    ("5", Wander, "wander: seeking a point that drifts round a circle ahead");
    ("6", Avoid, "avoid: wandering, turned away from the rock in its corridor");
    ("7", Follow, "follow: kept within the road's width, cutting its corners") ]

let names = [ "seek"; "flee"; "arrive"; "pursue"; "wander"; "avoid"; "follow" ]

type model = {
  mode : mode;
  creature : Physics.body;
  force : number * number; (* the steering force, for the arrow *)
  trail : (number * number) list;
}

let creature_shape = polygon (rgb 40 90 200) [ (18., 0.); (-12., 11.); (-6., 0.); (-12., -11.) ]

let initial_model =
  { mode = Seek;
    creature = Physics.body creature_shape |> Physics.at (-300.) (-200.) |> Physics.moving 150. 0.;
    force = (0., 0.);
    trail = [] }

(* the prey: round an ellipse, 0.5 radians a second *)
let prey (time : number) : Physics.body =
  let a = time *. 0.5 in
  Physics.body (circle orange 12.)
  |> Physics.at (260. *. cos a) (180. *. sin a)
  |> Physics.moving (-130. *. sin a) (90. *. cos a)

let rocks = [ (-150., 100., 50.); (120., -60., 60.); (230., 190., 40.); (-60., -215., 60.); (0., 250., 45.) ]
let road = [ (-320., -220.); (320., -220.); (320., 220.); (-320., 220.); (-320., -220.) ]

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let seconds (computer : computer) : number = match computer.time with Time t -> t

let update (computer : computer) (m : model) : model =
  let mode =
    List.fold_left (fun mode (k, md, _) -> if Set_.mem k computer.keyboard.keys then md else mode) m.mode modes
  in
  let mx = computer.mouse.mx and my = computer.mouse.my and t = seconds computer in
  let b = m.creature in
  let pushed =
    match mode with
    | Seek -> Ai.seek mx my b
    | Flee -> Ai.flee mx my b
    | Arrive -> Ai.arrive mx my b
    | Pursue -> Ai.chase (prey t) b
    | Wander -> Ai.wandering t b
    | Avoid -> b |> Ai.wandering t |> Ai.avoiding rocks
    | Follow -> Ai.following road b
  in
  let creature = pushed |> Physics.step |> Ai.facing |> Physics.wrap computer.screen in
  let trail = List.filteri (fun i _ -> i < 120) ((creature.x, creature.y) :: m.trail) in
  let trail = if mode <> m.mode then [] else trail in
  { mode; creature; force = (pushed.ax, pushed.ay); trail }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* a line from a to b, [w] thick *)
let segment (color : color) (w : number) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle color (Float.hypot dx dy) w
  |> rotate (Float.atan2 dy dx *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let arrow (color : color) (from : number * number) ((dx, dy) : number * number) : shape =
  let x, y = from in
  let tip = (x +. dx, y +. dy) and angle = Float.atan2 dy dx *. 180. /. Float.pi in
  group
    [ segment color 3. from tip;
      polygon color [ (0., 0.); (-12., 6.); (-12., -6.) ] |> rotate angle |> move (fst tip) (snd tip) ]

(* a circle's outline, as dots *)
let ring (color : color) (r : number) : shape =
  group (List.init 48 (fun i -> let a = float_of_int i *. Float.pi /. 24. in circle color 2. |> move (r *. cos a) (r *. sin a)))

let cross (color : color) : shape = group [ rectangle color 20. 3. |> rotate 45.; rectangle color 20. 3. |> rotate (-45.) ]

let view (computer : computer) (m : model) : shape list =
  let b = m.creature and screen = computer.screen in
  let mx = computer.mouse.mx and my = computer.mouse.my and t = seconds computer in
  let heading = let s = Float.hypot b.vx b.vy in if s = 0. then (1., 0.) else (b.vx /. s, b.vy /. s) in
  let hx, hy = heading in
  let overlay =
    match m.mode with
    | Seek | Flee -> [ cross (rgb 200 40 40) |> move mx my ]
    | Arrive -> [ cross (rgb 200 40 40) |> move mx my; ring (rgb 200 120 120) 100. |> move mx my ]
    | Pursue ->
        let p = prey t in
        (* ai/Steering.pursue's guess: the prey's distance over the top
         * speed (the Ai layer's 200) is the time to catch it *)
        let time = Float.hypot (p.x -. b.x) (p.y -. b.y) /. 200. in
        let px = p.x +. (time *. p.vx) and py = p.y +. (time *. p.vy) in
        [ Physics.draw p; segment (rgb 230 160 60) 2. (p.x, p.y) (px, py); cross orange |> move px py ]
    | Wander -> [ ring (rgb 120 140 200) 40. |> move (b.x +. (80. *. hx)) (b.y +. (80. *. hy)) ]
    | Avoid ->
        (* the corridor ahead: 100 long, the creature's size (10) wide
         * on each side (ai/Steering.avoid's defaults) *)
        List.map (fun (x, y, r) -> circle (rgb 150 140 130) r |> move x y) rocks
        @ [ rectangle (rgb 120 140 200) 100. 20.
            |> fade 0.4
            |> rotate (Float.atan2 hy hx *. 180. /. Float.pi)
            |> move (b.x +. (50. *. hx)) (b.y +. (50. *. hy)) ]
    | Follow ->
        let rec pairs = function a :: (c :: _ as rest) -> (a, c) :: pairs rest | _ -> [] in
        List.map (fun (a, c) -> segment (rgb 200 200 190) 40. a c) (pairs road)
        @ List.map (fun (a, c) -> segment (rgb 150 150 140) 2. a c) (pairs road)
  in
  let fx, fy = m.force in
  let description = match List.find_opt (fun (_, md, _) -> md = m.mode) modes with Some (_, _, d) -> d | None -> "" in
  [ rectangle (rgb 245 243 236) screen.width screen.height ]
  @ overlay
  @ List.mapi (fun i (x, y) -> circle (rgb 120 150 220) 2.5 |> fade (1. -. (float_of_int i /. 120.)) |> move x y) m.trail
  @ [ Physics.draw b; arrow (rgb 220 40 40) (b.x, b.y) (fx *. 0.25, fy *. 0.25) ]
  @ [ text (rgb 60 60 60) 2. (String.concat "   " (List.mapi (fun i n -> Printf.sprintf "%d %s" (i + 1) n) names))
      |> move_y (screen.top -. 30.);
      text (rgb 30 60 140) 2. description |> move_y (screen.top -. 60.);
      text (rgb 200 40 40) 1.6 "the red arrow: the steering force" |> move_y (screen.bottom +. 25.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app
