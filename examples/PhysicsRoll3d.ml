(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A race down a ramp between three bodies that all weigh the same, and
 * the one that wins is the one that does not roll.
 *
 *   space    let them go again
 *   r        rolling friction, on and off
 *
 * Galileo timed balls down inclined planes (Two New Sciences, 1638) to
 * slow falling down enough to measure, and the number he was after is
 * the one written over the middle lane here. A body let go on a slope
 * of angle a accelerates at
 *
 *                  g sin a
 *      a =  -------------------          k = I / m r^2, from its own
 *               1 + k                        inertia tensor
 *
 * so a sliding body (k = 0, nothing to spin) gets the whole g sin a, a
 * solid sphere (k = 2/5) gets 5/7 of it, and a capsule lying across
 * the slope, whose mass sits a little further from its axis, gets
 * slightly less again. Two sevenths of the sphere's energy has gone
 * into its spin, and the ice-smooth ball wins by exactly that.
 *
 * Every number over the lanes comes from the body's own tensor
 * (physics/3d/Hitbox3d.inertia), and the number under it is what the
 * simulation measured this run: the engine knows nothing about slopes,
 * only tensors, contact points and friction impulses
 * (Resolve3d.mli), and it arrives at 5/7 by itself. That same 5/7 is
 * derived by hand in TinyMarbleMadness.ml's header, which
 * makes this a cross-check between a game written from a textbook and
 * an engine written from Newton.
 *
 * "r" adds rolling friction, which is the other kind and easy to
 * confuse with this one: the grip at the contact is what makes a ball
 * roll, and rolling friction is the loss that later stops it.
 *
 * Metres and seconds; the slope is 18 degrees.
 *)
open Playground
open Playground3d

let angle = 18.
let radians d = d *. Float.pi /. 180.
let g = 9.8
let radius = 0.22

(* the slope descends towards +x, so its normal leans back *)
let normal = (sin (radians angle), cos (radians angle), 0.)
let downhill = (cos (radians angle), -.sin (radians angle), 0.)

let ramp : Physics3d.body =
  Physics3d.body (group3d [])
  |> Physics3d.hitbox (Hitbox3d.Plane (normal, 0.))
  |> Physics3d.immovable
  |> Physics3d.rough 1.

(* what the ramp looks like: a long slab whose top face is that plane *)
let ramp_shape : shape3d =
  let thickness = 0.3 in
  box (rgb 150 145 135) 12. thickness 3.4
  |> rotate3d 0. 0. (-.angle)
  |> move3d 0. (-.thickness /. 2. *. cos (radians angle)) 0.
  |> move3d (thickness /. 2. *. sin (radians angle)) 0. 0.

type racer = { name : string; body : Physics3d.body; color : color; k : number }

(* k = I / m r^2 about the axis it rolls on, straight from its tensor
 * -- 0 when nothing can spin it *)
let start_of (i : int) (name : string) (color : color) (shape : shape3d) (hitbox : Hitbox3d.t) (friction : number) : racer =
  let up_x, up_y, _ = normal in
  let along = float_of_int (i - 1) *. 1.15 in
  let body =
    Physics3d.body shape
    |> Physics3d.hitbox hitbox
    |> Physics3d.at ((-4. *. cos (radians angle)) +. (radius *. up_x)) ((4. *. sin (radians angle)) +. (radius *. up_y)) along
    |> Physics3d.rough friction
    |> Physics3d.bouncy 0.
  in
  let tensor = Hitbox3d.inertia ~mass:1. hitbox in
  let k = if friction <= 0. then 0. else tensor.Mat3.m11 /. (radius *. radius) in
  { name; body; color; k }

let racers () : racer list =
  [ start_of 0 "ice (nothing spins it)" (rgb 150 210 235) (sphere (rgb 150 210 235) radius) (Hitbox3d.Sphere radius) 0.;
    start_of 1 "a solid sphere" (rgb 220 150 70) (sphere (rgb 220 150 70) radius) (Hitbox3d.Sphere radius) 0.5;
    start_of 2 "a capsule across the slope"
      (rgb 130 190 120)
      (group3d
         [ sphere (rgb 130 190 120) radius |> move_y3d (-0.35); sphere (rgb 130 190 120) radius |> move_y3d 0.35;
           box (rgb 130 190 120) (radius *. 2.) 0.7 (radius *. 2.) ])
      (Hitbox3d.Capsule (0.35, radius)) 0.5 ]

(* the capsule's segment is its local y, and it has to lie across the
 * slope, along z *)
let lying_down (r : racer) : racer =
  if r.name.[0] = 'a' && String.length r.name > 2 && r.name.[2] = 'c' then
    { r with body = Physics3d.pointing (1., 0., 0.) 90. r.body }
  else r

type model = { racers : racer list; frames : int; rolling_friction : bool; r_was_down : bool }

let start (rolling_friction : bool) : model =
  { racers = List.map lying_down (racers ()); frames = 0; rolling_friction; r_was_down = false }

let initial_model = start false

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  let r = Set_.mem "r" k.keys in
  if k.kspace then start m.rolling_friction
  else if r && not m.r_was_down then start (not m.rolling_friction)
  else
    let step (racer : racer) =
      let b = racer.body |> Physics3d.fall g in
      let b = if m.rolling_friction then Physics3d.spin_slow 1.2 b else b in
      { racer with body = b |> Physics3d.step |> Physics3d.bounce_off ramp }
    in
    { m with racers = List.map step m.racers; frames = m.frames + 1; r_was_down = r }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text color size str = words color str |> scale size
(* from the side *and* above, so that the three lanes are three
 * lanes and not one *)
let cam = camera ~eye:(5.5, 4.2, 7.5) ~target:(0.2, -0.3, 0.) ~fov:44. ()

(* what the formula says it should be, from its own tensor *)
let predicted (r : racer) : number = g *. sin (radians angle) /. (1. +. r.k)

(* and what it did: from rest, so speed over time *)
let measured (m : model) (r : racer) : number =
  if m.frames < 10 then 0.
  else
    let v = (r.body.Physics3d.vx, r.body.Physics3d.vy, r.body.Physics3d.vz) in
    Vec3.dot v downhill /. (float_of_int m.frames /. 60.)

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let labels =
    List.concat
      (List.mapi
         (fun i (r : racer) ->
           let want = predicted r and got = measured m r in
           (* down the left, under the ramp: over the ramp they would
            * be grey on grey *)
           let y = screen.bottom +. 320. -. (float_of_int i *. 92.) in
           [ text r.color 2. r.name |> move (screen.left +. 260.) y;
             text darkGray 1.8 (Printf.sprintf "k = %.2f, so %.2f m/s^2" r.k want) |> move (screen.left +. 250.) (y -. 28.);
             text (if m.frames > 30 && Float.abs (got -. want) < 0.05 *. want then rgb 20 120 60 else rgb 120 90 60) 1.8
               (Printf.sprintf "measured %.2f" got)
             |> move (screen.left +. 200.) (y -. 54.) ])
         m.racers)
  in
  ( cam,
    (ramp_shape :: List.map (fun (r : racer) -> Physics3d.draw r.body) m.racers)
    @ List.map hud
        (labels
        @ [ text black 2.2 "the same slope, the same weight: the one that does not roll wins"
            |> move_y (screen.top -. 45.);
            text darkGray 2.
              (Printf.sprintf "a = g sin %.0f / (1 + k),  k = I / m r^2 from each body's own tensor" angle)
            |> move_y (screen.top -. 80.);
            text darkGray 2.
              (if m.rolling_friction then "rolling friction on: the spinning ones are losing it again  (r)"
               else "space: again    r: rolling friction, the loss that stops a roll")
            |> move_y (screen.bottom +. 20.) ]) )

let app = game3d view update initial_model
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
