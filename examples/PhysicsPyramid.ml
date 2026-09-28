(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A pyramid of 28 boxes that must stand still: stacking
 * (physics/2d/Solver.mli, docs/claude_notes/notes_2d_physics.md
 * section 12), the test every physics engine is judged by, and what
 * an Angry Birds tower needs to wait for its bird.
 *
 * Each box presses on the ones below, which press on the floor: all the
 * contacts depend on each other, and fixing them one at a time
 * ([Physics.bounce_all]) the pyramid jitters and sinks. A [world]
 * solves them together, going over all of them again and again (the
 * iterations), each starting from its impulse of the previous step
 * (warm starting). Switch them to see what each one does:
 *
 *   s      the solver off/on: off, the engine before stacking (the
 *          plan's phase 7): each body [step]ped, then [bounce_all],
 *          each contact fixed alone, once, at one point
 *   i      the iterations per step: 1, 4, 10 (the default), 20
 *   w      warm starting on/off
 *   space  throw a ball at the pyramid
 *   r      build it again
 *
 * (and the flag solver=off, to start without it:
 *   dune exec examples/PhysicsPyramid.exe -- solver=off
 * or ?solver=off on the web; the keys are printed at launch.)
 *
 * Without the solver, the pyramid jitters, sinks and slides apart; with
 * it but 1 iteration and no warm starting, it sags into a heap; with
 * 10 and warm starting, it stands still, until the ball.
 *
 * The classic scene of every engine's demos: Box2D Lite's "Pyramid
 * Stacking" (Erin Catto, 2006: 12 rows of boxes, and a bomb launched
 * with space, like our ball),
 *   https://github.com/erincatto/box2d-lite/blob/master/samples/main.cpp
 * Box2D's testbed "Pyramid",
 *   https://github.com/erincatto/box2d/blob/v2.4.1/testbed/tests/pyramid.cpp
 * and Chipmunk's "PyramidStack" (Scott Lembcke),
 *   https://github.com/slembcke/Chipmunk2D/blob/master/demo/PyramidStack.c
 * (the same idea here, written from scratch, not their code).
 *
 * What it uses: the Physics layer's world ([Physics.world],
 * [simulate]); underneath, all of physics/2d/ but Force.gravitation:
 * Broadphase (sort and sweep), Collide.manifold (two points per
 * resting edge), Solver (the sequential impulses), Resolve's formulas
 * with rotation, Scene2d for the keys.
 *)
open Playground
open Basics (* float arithmetics *)

let wall (w : number) (h : number) (x : number) (y : number) : Physics.body =
  Physics.body (rectangle (rgb 100 100 110) w h) |> Physics.at x y |> Physics.immovable |> Physics.rough 0.8

(* the floor's top at -400 *)
let walls = [ wall 1000. 100. 0. (-450.); wall 40. 1000. (-500.) 0.; wall 40. 1000. 500. 0. ]

(* 7 rows, 7 boxes at the bottom to 1 at the top, each row resting on
 * the one below, 2 pixels between neighbours *)
let pyramid : Physics.body list =
  List.concat
    (List.init 7 (fun row ->
         let n = 7 -.. row in
         List.init n (fun i ->
             let x = (float_of_int i - (float_of_int (n -.. 1) / 2.)) * 42. and y = -380. + (40. * float_of_int row) in
             let shade = 120 +.. (15 *.. row) in
             Physics.body (rectangle (rgb shade (shade -.. 60) 40) 40. 40.) |> Physics.at x y |> Physics.rough 0.6)))

let ball : Physics.body =
  Physics.body (circle (rgb 60 90 200) 25.) |> Physics.at (-450.) (-150.) |> Physics.moving 800. 250. |> Physics.heavy 4. |> Physics.rough 0.6

type settings = { solver : bool; iterations : int; warm_starting : bool }
type state = { world : Physics.world; settings : settings }
type model = state Scene2d.t

let build (settings : settings) : state = { world = Physics.world (walls @ pyramid); settings }
let defaults = { solver = true; iterations = 10; warm_starting = true }
let initial_model : model = Scene2d.start (build defaults)

(* without the solver: phase 7's engine, one contact at a time *)
let without_solver (bodies : Physics.body list) : Physics.body list =
  bodies
  |> List.map (fun (b : Physics.body) -> if b.mass = infinity then b else b |> Physics.fall 800. |> Physics.step)
  |> Physics.bounce_all

let next_iterations = function 1 -> 4 | 4 -> 10 | 10 -> 20 | _ -> 1
let letter (l : string) (k : keyboard) = Set_.mem l k.keys

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let s = scenes.scene in
  let pressed k = Scene2d.pressed k scenes in
  (* solver=off: without it, from the start *)
  let s =
    if scenes.frames = 1 && List.assoc_opt "solver" computer.flags = Some "off" then
      { s with settings = { s.settings with solver = false } }
    else s
  in
  let settings =
    if pressed (letter "s") then { s.settings with solver = not s.settings.solver }
    else if pressed (letter "i") then { s.settings with iterations = next_iterations s.settings.iterations }
    else if pressed (letter "w") then { s.settings with warm_starting = not s.settings.warm_starting }
    else s.settings
  in
  if pressed (letter "r") then Scene2d.go (build settings) scenes
  else
    (* the ball added at the end: the others keep their places, and
     * their contacts' memory *)
    let world = if pressed (fun k -> k.kspace) then { s.world with bodies = s.world.bodies @ [ ball ] } else s.world in
    let world =
      if settings.solver then world |> Physics.simulate ~gravity:800. ~iterations:settings.iterations ~warm_starting:settings.warm_starting
      else { world with bodies = without_solver world.bodies }
    in
    { scenes with scene = { world; settings } }

let text (s : string) : shape = words black s |> scale 2.

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let s = model.scene.settings in
  (rectangle (rgb 235 235 225) screen.width screen.height :: List.map Physics.draw model.scene.world.bodies)
  @ [ text
        (if s.solver then
           Printf.sprintf "solver on (s): %d iterations (i), warm starting %s (w)" s.iterations
             (if s.warm_starting then "on" else "off")
         else "solver off (s): one contact at a time, once")
      |> move_y 450.;
      text "space: throw a ball   r: again" |> move_y 415. ]

let help =
  {|Pyramid
  keys:  s      the solver off/on (off: one contact at a time, phase 7)
         i      iterations per step: 1, 4, 10, 20
         w      warm starting off/on
         space  throw a ball
         r      build the pyramid again
  flags: solver=off  without the solver from the start
  e.g.   dune exec examples/PhysicsPyramid.exe -- solver=off
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
