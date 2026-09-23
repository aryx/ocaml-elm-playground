(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Squash and stretch, the first of Disney's twelve principles (Thomas
 * and Johnston, The Illusion of Life, 1981), and the hit flash: three
 * balls bouncing the same bounce, side by side.
 *
 *   left     dry: round all the time -- a billiard ball on stone
 *   middle   squashed when it lands, springing back taller, its area
 *            kept -- rubber
 *   right    the same, a face, and white for a moment when it lands:
 *            the flash that says "hit"
 *
 * Each is built standing on (0, 0) -- its bottom there -- then
 * stretched, then moved where it is: so it squashes against the ground,
 * not in the air. The face is a group (a disc, two eyes, a mouth), and
 * the whole group is squashed and whitened as one.
 *
 * With the flag juice=off (dune exec examples/JuiceSquash.exe --
 * juice=off, or JuiceSquash.html?juice=off), the three are the same.
 *
 * What it uses: playground/Juice (squash, stretch, whiten, during for
 * the flash; the model is only the effects' clock, Juice.t), over
 * juice/Squash. The bounce is a parabola of that clock, not Physics:
 * every bounce the same, so the three stay together. *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The bounce *)
(*****************************************************************************)

let period = 1.25
let height = 380.
let ground = -250.

(* when it last landed, and how high it is now: a parabola between
 * two landings *)
let bounce (fx : Juice.t) : time * number =
  let (Time now) = Juice.now fx in
  let landed = Float.of_int (int_of_float (now / period)) * period in
  let u = (now - landed) / period in
  (Time landed, height * 4. * u * (1. - u))

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let radius = 50.

let ball (color : color) : shape = circle color radius |> move_up radius

let face : shape =
  group
    [
      circle yellow radius;
      circle black 7. |> move (-18.) 12.;
      circle black 7. |> move 18. 12.;
      rectangle black 36. 6. |> move 0. (-20.);
    ]
  |> move_up radius

(* the model is only the effects' clock *)
let view (_ : computer) (fx : Juice.t) : shape list =
  let landed, up = bounce fx in
  let squash = Juice.squash 0.4 0.5 landed fx in
  (* white for 0.08 s after landing *)
  let flashing = Juice.during 0.08 landed fx in
  let at x shape = shape |> move x (ground + up) in
  [
    rectangle (rgb 40 44 52) 1000. 1000.;
    rectangle (rgb 120 120 130) 1000. 6. |> move_y (ground - 3.);
    words white "dry" |> scale 2. |> move (-300.) 350.;
    words white "squash" |> scale 2. |> move 0. 350.;
    words white "squash + flash" |> scale 2. |> move 300. 350.;
    at (-300.) (ball orange);
    at 0. (ball orange |> Juice.stretch squash);
    at 300. (face |> Juice.stretch squash |> fun s -> if flashing then Juice.whiten s else s);
  ]

let update (computer : computer) (fx : Juice.t) : Juice.t = Juice.step computer fx

let app = game view update (Juice.none ~seed:1)
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
