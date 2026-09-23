(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The first universe of How to Design Programs' 2htdp/universe: a ball
 * passed from world to world (see playground/Universe.mli). This is a
 * world: it rests until the universe tells it "go", then lets its ball
 * fall, and says "done" when the ball reaches the bottom; the universe
 * (UniverseBallServer.ml) then gives the ball to the next world. Run
 * the universe, then as many worlds as you like, each in its window or
 * its browser tab:
 *
 *   dune exec examples/UniverseBallServer.exe
 *   dune exec examples/UniverseBall.exe             (twice, or more)
 *   http://localhost:8001/examples/web/UniverseBall.html?port=4567
 *
 * host= and port= to reach a universe on another computer. *)
open Playground
open Bigbang

let size = 300.
let scene = empty_scene size size

type world = Resting | Falling of number

let draw (w : world) : image =
  match w with
  | Resting -> place_image (text "resting" 24. gray) (size /. 2.) (size /. 2.) scene
  | Falling y -> place_image (circle 12. Solid red) (size /. 2.) y scene

(* the ball falls 3 a tick; at the bottom, the world tells the universe *)
let fall (w : world) : world Universe.package =
  match w with
  | Falling y when y +. 3. >= size -> (Resting, [ "done" ])
  | Falling y -> (Falling (y +. 3.), [])
  | Resting -> (Resting, [])

(* the universe's "go": the ball is ours *)
let catch (w : world) (message : string) : world Universe.package = if message = "go" then (Falling 0., []) else (w, [])

let app network = Universe.big_bang Resting ~to_draw:draw ~on_tick:fall ~on_receive:catch ~network ()
let main = Cap.main (fun caps -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app caps))
