(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Springs and ropes: soft things (physics/2d/Springs.mli and
 * Particles.mli, docs/claude_notes/notes_2d_physics.md section 6).
 *
 *   left:   a mass on a spring (Hooke): d switches its damping, from
 *           bouncing forever to settling
 *   middle: a chain of masses and springs: x makes the springs 10 times
 *           stiffer, too stiff for the time step (k / m over 14,400 at
 *           60 steps a second), and it explodes
 *   right:  a rope the Hitman way (Jakobsen): particles and sticks,
 *           stiff but never exploding; the mouse drags its end (let go
 *           to throw it)
 *   space:  a kick sideways to all three      r: again
 *
 * The lesson of the middle and the right: the chain's springs turn a
 * stretch into a force, into a velocity, into a move, a step late each
 * time, and a stiff one overshoots more and more; the rope's sticks set
 * the positions directly, however stiff (Particles.mli).
 *
 * Like examples/Orbit.ml, this reaches under the Physics layer, into
 * the engine (physics/2d/): comparing its pieces is the point. What it
 * uses: Body, Springs (the mass and the chain), Particles (the rope),
 * and Scene2d for the keys.
 *)
open Playground
open Basics (* float arithmetics *)

let gravity = (0., -800.)
let dt = 1. / 60.

(* the ceiling at 380 *)
let ceiling = 380.

(* a mass on a spring of rest length 150, k / m = 60 *)
let mass_on_spring ~(damped : bool) : Body.t array * Springs.spring list =
  ( [| Body.make ~mass:infinity (-330., ceiling); Body.make (-330., ceiling - 150.) |],
    [ { Springs.a = 0; b = 1; rest = 150.; k = 60.; damping = (if damped then 2. else 0.) } ] )

(* 8 masses, 40 apart; k / m = 2,000, or 20,000: too stiff *)
let chain ~(stiff : bool) : Body.t array * Springs.spring list =
  Springs.chain ~from:(0., ceiling) ~towards:(0., ceiling - 280.) 8 ~k:(if stiff then 20000. else 2000.) ~damping:5.

let rope () = Particles.rope ~from:(330., ceiling) ~towards:(330., ceiling - 300.) 21

type state = {
  spring : Body.t array * Springs.spring list;
  chain : Body.t array * Springs.spring list;
  rope : Particles.particle array * Particles.stick list;
  damped : bool;
  stiff : bool;
}

type model = state Scene2d.t

let start (damped : bool) (stiff : bool) : state = { spring = mass_on_spring ~damped; chain = chain ~stiff; rope = rope (); damped; stiff }
let initial_model : model = Scene2d.start (start false false)

let kick_bodies (bodies : Body.t array) = Array.map (fun (b : Body.t) -> if b.mass = infinity then b else { b with vel = Vec2.add b.vel (300., 0.) }) bodies

(* a particle's velocity is pos - old: moving old back is a kick *)
let kick_rope (ps : Particles.particle array) =
  Array.map (fun (p : Particles.particle) -> if p.pinned then p else { p with old = Vec2.sub p.old (5., 0.) }) ps

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let s = scenes.scene in
  let pressed l = Scene2d.pressed (fun k -> Set_.mem l k.keys) scenes in
  if pressed "r" then Scene2d.go (start s.damped s.stiff) scenes
  else if pressed "d" then Scene2d.go (start (not s.damped) s.stiff) scenes
  else if pressed "x" then Scene2d.go (start s.damped (not s.stiff)) scenes
  else
    let kick = Scene2d.pressed (fun k -> k.kspace) scenes in
    let step_springs (bodies, springs) =
      let bodies = if kick then kick_bodies bodies else bodies in
      (Springs.step ~gravity ~dt bodies springs, springs)
    in
    let (ps, sticks) = s.rope in
    let ps = if kick then kick_rope ps else ps in
    (* the rope's end held by the mouse, on the right half *)
    let m = computer.mouse in
    let last = Array.length ps -.. 1 in
    let held = m.mdown && m.mx > 150. in
    let ps = Array.mapi (fun i (p : Particles.particle) -> if i = last then { p with pinned = held } else p) ps in
    let ps = ps |> Particles.step ~drag:0.01 ~accel:gravity ~dt in
    let ps = if held then (ps.(last) <- { (ps.(last)) with pos = (m.mx, m.my) }; ps) else ps in
    let ps = ps |> Particles.relax ~iterations:20 sticks in
    { scenes with scene = { s with spring = step_springs s.spring; chain = step_springs s.chain; rope = (ps, sticks) } }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* a segment from p to q: a thin rectangle, turned; nothing if a point
 * flew off (the exploded chain) *)
let segment (color : color) (width : number) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape list =
  let far v = Float.is_nan v || Float.abs v > 5000. in
  if far x1 || far y1 || far x2 || far y2 then []
  else
    let length = Float.hypot (x2 - x1) (y2 - y1) in
    [ rectangle color length width |> rotate (Float.atan2 (y2 - y1) (x2 - x1) * 180. / Float.pi) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.) ]

(* a spring drawn as a zigzag of [teeth] teeth, [width] wide *)
let zigzag ?(teeth = 12) ?(width = 10.) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape list =
  let n = 2 *.. teeth in
  let length = Float.hypot (x2 - x1) (y2 - y1) in
  let (ux, uy) = if length = 0. then (0., 0.) else ((x2 - x1) / length, (y2 - y1) / length) in
  let point i =
    let t = float_of_int i / float_of_int n in
    let side = if i = 0 || i = n then 0. else if i mod 2 = 0 then width else -.width in
    (x1 + (t * (x2 - x1)) - (side * uy), y1 + (t * (y2 - y1)) + (side * ux))
  in
  List.concat (List.init n (fun i -> segment (rgb 90 90 100) 2. (point i) (point (i +.. 1))))

let dot (color : color) (r : number) ((x, y) : number * number) : shape list =
  if Float.is_nan x || Float.abs x > 5000. || Float.abs y > 5000. then [] else [ circle color r |> move x y ]

let text (s : string) : shape = words black s |> scale 2.

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and s = model.scene in
  let (sb, _) = s.spring and (cb, springs) = s.chain and (ps, sticks) = s.rope in
  (rectangle (rgb 235 235 225) screen.width screen.height :: [ rectangle (rgb 100 100 110) 1000. 20. |> move_y (ceiling + 10.) ])
  (* the mass on its spring *)
  @ zigzag sb.(0).pos sb.(1).pos
  @ dot (rgb 200 80 60) 22. sb.(1).pos
  (* the chain *)
  @ List.concat_map (fun (sp : Springs.spring) -> zigzag ~teeth:3 ~width:6. cb.(sp.a).pos cb.(sp.b).pos) springs
  @ List.concat (Array.to_list (Array.map (fun (b : Body.t) -> dot (rgb 60 120 200) 9. b.pos) cb))
  (* the rope *)
  @ List.concat_map (fun (st : Particles.stick) -> segment (rgb 150 100 50) 5. ps.(st.a).pos ps.(st.b).pos) sticks
  @ dot (rgb 150 100 50) 8. ps.(Array.length ps -.. 1).pos
  @ [ text (if s.damped then "damped (d)" else "undamped (d)") |> move (-330.) 450.;
      text (if s.stiff then "too stiff! (x)" else "a chain of springs (x)") |> move 0. 450.;
      text "a rope of sticks (mouse)" |> move 330. 450.;
      text "space: kick   r: again" |> move_y (-450.) ]

let help =
  {|Elastic
  keys:  d      the spring's damping on/off
         x      the chain too stiff (it explodes) / back
         space  a kick sideways      r  again
  mouse: drag the rope's end (right half), let go to throw it
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
