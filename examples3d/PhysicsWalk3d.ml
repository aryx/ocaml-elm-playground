(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A player walking, which is to say not a rigid body: playground3d/
 * Character3d, the capsule controller, on a course with one lane per
 * thing a controller has to get right.
 *
 *   arrows   walk (up is along the course, left towards the ramps)
 *   space    jump
 *   o        the step offset: 0.4, 0.6, 0, 0.2 m
 *   l        the slope limit: 45, 60, 30 degrees
 *   d        the capsule, as the engine sees it
 *   r        back to the start
 *
 * The lanes, from the nearest to the camera to the farthest:
 *
 *   - a low ceiling, 2.1 m up: jump under it and the head stops the
 *     jump, instead of the character going through or sticking;
 *   - stairs of 0.2 m, a platform, then a 0.5 m step at its end. The
 *     stairs are walked up without jumping, and the 0.5 m step is a
 *     wall -- until "o" raises the offset to 0.6, and it is a stair.
 *     This is the plan's measured switch: the same step, a wall at 0.4
 *     and a stair at 0.6 (physics/tests/Unit_character3d.ml);
 *   - a 30 degree ramp, walked up;
 *   - a 50 degree ramp, a wall to the feet at the 45 degree limit;
 *     jump onto it and the character slides back down. "l" to 60, and
 *     it is walked up.
 *
 * Metres: the character is 1.8 m tall and 0.6 m wide, and walks at
 * 3 m/s.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The course *)
(*****************************************************************************)

(* a box of solid, its bottom at [y] *)
let block ?(y = 0.) (color : color) (x1, z1) (x2, z2) (h : number) : Physics3d.body =
  Physics3d.body (box color (x2 -. x1) h (z2 -. z1))
  |> Physics3d.at ((x1 +. x2) /. 2.) (y +. (h /. 2.)) ((z1 +. z2) /. 2.)
  |> Physics3d.immovable

(* a ramp rising towards +x at [degrees], from the ground at x = [x0],
 * across the lane [z1..z2]: a box turned about z, its top face
 * through (x0, 0) *)
let ramp (color : color) (degrees : number) (x0 : number) (z1, z2) : Physics3d.body =
  let a = degrees *. Float.pi /. 180. and len = 6. and thick = 1. in
  Physics3d.body (box color len thick (z2 -. z1))
  |> Physics3d.pointing (0., 0., 1.) degrees
  |> Physics3d.at
       (x0 +. (len /. 2. *. cos a) +. (thick /. 2. *. sin a))
       ((len /. 2. *. sin a) -. (thick /. 2. *. cos a))
       ((z1 +. z2) /. 2.)
  |> Physics3d.immovable

let stone = rgb 160 160 150
let wood = rgb 170 130 80

let course : Physics3d.body list =
  [ block ~y:(-1.) (rgb 110 150 100) (-10., -12.) (20., 8.) 1.;
    (* the low ceiling, over the first lane *)
    block ~y:2.1 stone (2., 4.) (8., 6.) 0.3;
    block stone (2., 3.6) (8., 4.) 2.1;
    block stone (2., 6.) (8., 6.4) 2.1 ]
  (* the stairs, the platform and the 0.5 m step *)
  @ List.init 5 (fun i -> block wood (2. +. (0.5 *. float_of_int i), -1.) (5., 1.) (0.2 *. float_of_int (i + 1)))
  @ [ block wood (5., -1.) (9., 1.) 1.; block (rgb 190 90 70) (9., -1.) (11., 1.) 1.5 ]
  @ [ ramp (rgb 120 140 190) 30. 2. (-5., -3.); ramp (rgb 190 110 140) 50. 2. (-9., -7.) ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

let offsets = [| 0.4; 0.6; 0.; 0.2 |]
let limits = [| 45.; 60.; 30. |]

type model = { me : Character3d.t; offset : int; limit : int; keys_down : string list }

let start (offset : int) (limit : int) : Character3d.t =
  Character3d.make ~step:offsets.(offset) ~slope:limits.(limit) 0. 0. 0.

let initial_model : model = { me = start 0 0; offset = 0; limit = 0; keys_down = [] }

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  let down key = Set_.mem key k.keys in
  let pressed key = down key && not (List.mem key m.keys_down) in
  let keys_down = List.filter down [ "o"; "l"; "r" ] in
  let offset = if pressed "o" then (m.offset + 1) mod Array.length offsets else m.offset in
  let limit = if pressed "l" then (m.limit + 1) mod Array.length limits else m.limit in
  let me = if pressed "r" then start offset limit else { m.me with step = offsets.(offset); slope = limits.(limit) } in
  let axis a b = (if a then 3. else 0.) -. if b then 3. else 0. in
  let me = Character3d.walk ~jump:(if k.kspace then 4.5 else 0.) course (axis k.kup k.kdown, axis k.kright k.kleft) me in
  { me; offset; limit; keys_down }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen and me = m.me in
  let cam = Camera3d.from_far ~fov:40. ~offset:(-9., 7., 11.) (me.x +. 2., me.y +. 1., me.z -. 1.) in
  let body = box (rgb 50 90 200) (2. *. me.radius) me.height (2. *. me.radius) |> move3d me.x (me.y +. (me.height /. 2.)) me.z in
  let debug = if Set_.mem "d" computer.keyboard.keys then [ Physics3d.debug (Character3d.capsule me) ] else [] in
  let standing =
    if me.grounded then Printf.sprintf "standing, on %.0f degrees" me.ground else Printf.sprintf "in the air, %.1f m/s" me.vy
  in
  ( cam,
    (List.map Physics3d.draw course @ [ body ]) @ debug
    @ List.map hud
        [ text black 2.2
            (Printf.sprintf "step offset %.1f m (o)    slope limit %.0f degrees (l)" me.step me.slope)
          |> move_y (screen.top -. 40.);
          text (rgb 40 90 40) 2. standing |> move_y (screen.top -. 75.);
          text darkGray 1.8 "arrows: walk   space: jump   d: the capsule   r: start again"
          |> move_y (screen.bottom +. 25.) ] )

let app = game3d view update initial_model

let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
