(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* How to Design Programs's first world program, in its Prologue: a
 * rocket coming down, and landing (see Bigbang.mli). The
 * world is a number, how far the rocket is from the top of the scene;
 * each tick adds 3; it stops at the ground. Space starts it over.
 *
 * The rocket is itself images composed, the way HtDP draws: a triangle
 * above a rectangle, with fins beside. *)
open Playground
open Bigbang

let width = 400.
let height = 600.
let scene = empty_scene width height

let rocket : image =
  let fin = triangle 16. Solid (rgb 200 40 40) in
  above (triangle 30. Solid (rgb 200 40 40)) (beside fin (beside (rectangle 30. 70. Solid (rgb 180 180 190)) fin))

(* where the rocket's center is when it stands on the ground *)
let landed = height -. (Bigbang.height rocket /. 2.) -. 20.

let draw (y : number) : image =
  place_image rocket (width /. 2.) y (place_image (rectangle width 20. Solid (rgb 90 160 70)) (width /. 2.) (height -. 10.) scene)

let app =
  big_bang 0.
    ~to_draw:draw
    ~on_tick:(fun y -> Float.min landed (y +. 3.))
    ~on_key:(fun y key -> if key = " " then 0. else y)
    ()

let main = Playground_platform.run_app app
