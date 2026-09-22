(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Five balls of different bounciness, dropped together, and a crate
 * dropped on its corner.
 *
 *   space    drop them again
 *   f        friction under the crate, on and off
 *
 * The twin of examples/PhysicsBounce.ml, one dimension up, and its
 * lesson is the same one Newton measured in 1666: a ball bounces back
 * at e times the speed it arrived with, so it returns to e^2 of the
 * height it fell from. The pale bar beside each ball is drawn at that
 * height, from its e and nothing else, before it is dropped. What the
 * simulation does is come back and touch it.
 *
 * The crate is the part with no 2D version, and the reason phase 5 of
 * plan_physics3d_teaching.md needed the tensor: it lands on a corner,
 * so the impulse arrives well away from its centre, and what a
 * collision gives it is mostly *spin* (Resolve3d.mli's lever arms).
 * With "f" off it slides and turns for ever, since nothing is taking
 * its energy; with friction it settles onto a face.
 *
 * What this example is honest about: each pair is solved once per
 * frame, which is all a bouncing ball needs. A *stack* of crates needs
 * the same contacts solved again and again as their neighbours move,
 * and four contact points per pair rather than one, which is phase 8.
 * Drop these on each other and they will jostle.
 *
 * Metres and seconds; the balls are 50 cm across.
 *)
open Playground
open Playground3d

let drop_y = 3.
let radius = 0.25

(* bounciness, name, colour *)
let kinds =
  [ (0., "clay", rgb 150 120 100); (0.3, "wood", rgb 190 150 90); (0.6, "tennis", rgb 200 220 80);
    (0.8, "rubber", rgb 90 180 220); (0.95, "superball", rgb 230 90 140) ]

(* the height a ball of this bounciness must come back to: e^2 of the
 * fall, measured from where it touches *)
let comes_back_to (e : number) : number = radius +. (e *. e *. (drop_y -. radius))

(* what each ball has done since it was dropped: [apex] is how high it
 * has been *since its first bounce*, which is the number the bar is
 * about -- measured from the drop it would always be the drop *)
type climb = { apex : number; bounced : bool }

type model = {
  balls : Physics3d.body list;
  climb : climb list;
  crate : Physics3d.body;
  rough : bool;
  frames : int;
}

let floor_body : Physics3d.body =
  Physics3d.body (box (rgb 130 150 120) 20. 0.2 20.)
  |> Physics3d.at 0. (-0.1) 0.
  |> Physics3d.hitbox (Hitbox3d.Plane ((0., 1., 0.), 0.))
  |> Physics3d.immovable

let drop (rough : bool) : model =
  { balls =
      List.mapi
        (fun i (e, _, color) ->
          Physics3d.body (sphere color radius)
          |> Physics3d.ball
          |> Physics3d.at ((float_of_int i -. 2.) *. 1.1) drop_y 0.
          |> Physics3d.bouncy e
          |> Physics3d.rough (if rough then 0.4 else 0.))
        kinds;
    climb = List.map (fun _ -> { apex = 0.; bounced = false }) kinds;
    crate =
      Physics3d.body (box (rgb 180 130 80) 0.7 0.7 0.7)
      |> Physics3d.at 3.6 2.2 0.
      |> Physics3d.pointing (1., 0.3, 0.7) 40.
      |> Physics3d.bouncy 0.35
      |> Physics3d.rough (if rough then 0.5 else 0.);
    rough; frames = 0 }

let initial_model = drop true

let fall_and_bounce (b : Physics3d.body) : Physics3d.body =
  b |> Physics3d.fall 9.8 |> Physics3d.step |> Physics3d.bounce_off floor_body

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  if k.kspace then drop m.rough
  else if Set_.mem "f" k.keys then drop (not m.rough)
  else
    let balls = List.map fall_and_bounce m.balls in
    let climb =
      List.map2
        (fun (was : Physics3d.body) (is : Physics3d.body) -> (was, is))
        m.balls balls
      |> List.map2
           (fun (c : climb) ((was : Physics3d.body), (is : Physics3d.body)) ->
             (* the bottom of the *first* bounce: it was going down and
              * is now going up. From there on, the highest it reaches
              * -- and e^2 is about that first return, not the smaller
              * ones after it. *)
             if (not c.bounced) && was.Physics3d.vy < 0. && is.Physics3d.vy > 0. then
               { apex = is.Physics3d.y; bounced = true }
             else if c.bounced then { c with apex = Float.max c.apex is.Physics3d.y }
             else c)
           m.climb
    in
    { m with
      balls;
      climb;
      crate = fall_and_bounce m.crate;
      frames = m.frames + 1 }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text color size str = words color str |> scale size
let cam = camera ~eye:(0.4, 2.2, 9.5) ~target:(0.4, 1.2, 0.) ~fov:45. ()

(* the bar each ball has to come back to, drawn from its bounciness
 * before it is dropped *)
let target_bar (i : int) (e : number) : shape3d =
  box (rgb 90 90 110) 0.55 0.02 0.05 |> move3d ((float_of_int i -. 2.) *. 1.1) (comes_back_to e) 0.35

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let labels =
    List.concat
      (List.map2
         (fun ((e, name, _), (c : climb)) (b : Physics3d.body) ->
           match project cam screen (b.Physics3d.x, comes_back_to e +. 0.35, 0.35) with
           | None -> []
           | Some (x, y) ->
               let wanted = comes_back_to e in
               [ text black 1.7 name |> move x (y +. 20.);
                 text
                   (if c.bounced && Float.abs (c.apex -. wanted) < 0.06 then rgb 20 120 60 else rgb 90 90 100)
                   1.7
                   (if c.bounced then Printf.sprintf "e %.2f: %.2f, reached %.2f" e wanted c.apex
                    else Printf.sprintf "e %.2f: back to %.2f m" e wanted)
                 |> move x y ])
         (List.combine kinds m.climb) m.balls)
  in
  ( cam,
    (Physics3d.draw floor_body :: List.mapi (fun i (e, _, _) -> target_bar i e) kinds)
    @ List.map Physics3d.draw m.balls
    @ [ Physics3d.draw m.crate ]
    @ List.map hud
        (labels
        @ [ text black 2.2 "a ball comes back to e^2 of its fall: the bar is drawn there, from e alone"
            |> move_y (screen.top -. 45.);
            text darkGray 2. (Printf.sprintf "the crate lands on a corner, so the impulse spins it%s"
                                (if m.rough then " (friction on)" else " (no friction: it never settles)"))
            |> move_y (screen.bottom +. 45.);
            text darkGray 2. "space: drop them again    f: friction" |> move_y (screen.bottom +. 15.) ]) )

let app = game3d view update initial_model
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
