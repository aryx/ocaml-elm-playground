(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Every easing curve, plotted, and played: Robert Penner's curves
 * (2002), the ones every tween library copies, side by side.
 *
 * Each panel is one curve: on the left, its graph (the time across,
 * how far along up; the box is 0 to 1 both ways, and [back] and
 * [elastic] leave it), with a dot moving along it; on the right, a
 * ball going from one end of its track to the other along that curve,
 * in 2 seconds, then resting 1 second, then again. All the balls leave
 * together and arrive together: only *how* they go differs. The top
 * one, [linear], is the machine they are all compared to.
 *
 * Read down a column for the families (gentle to wild), across a row
 * for the three shapes of one family: [in] starts slow, [out] is the
 * same curve run backwards (it ends slow: see how its graph is the
 * [in] graph turned half a turn), [in_out] both.
 *
 * With the flag juice=off (dune exec examples/JuiceCurves.exe --
 * juice=off, or JuiceCurves.html?juice=off), every ball is at its end
 * at once: no juice.
 *
 * What it uses: playground/Juice (the curves, Juice.curve for the
 * graphs, Juice.tween for every ball and dot), over juice/Ease and
 * juice/Tween. *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The curves *)
(*****************************************************************************)

let families : (string * (Juice.ease * Juice.ease * Juice.ease)) list =
  [
    ("quad", (Juice.in_quad, Juice.out_quad, Juice.in_out_quad));
    ("cubic", (Juice.in_cubic, Juice.out_cubic, Juice.in_out_cubic));
    ("sine", (Juice.in_sine, Juice.out_sine, Juice.in_out_sine));
    ("back", (Juice.in_back, Juice.out_back, Juice.in_out_back));
    ("elastic", (Juice.in_elastic, Juice.out_elastic, Juice.in_out_elastic));
    ("bounce", (Juice.in_bounce, Juice.out_bounce, Juice.in_out_bounce));
  ]

(* 2 s going, 1 s resting *)
let going = 2.
let cycle = 3.

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let graph_size = 80.
let track = 170.

(* a line from one point to another, as a thin rotated rectangle *)
let segment (color : color) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  rectangle color (Float.hypot (x2 - x1) (y2 - y1) + 1.) 2.
  |> rotate (atan2 (y2 - y1) (x2 - x1) * 180. / Float.pi)
  |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

(* the graph of [ease], its box's bottom left corner at (0, 0) *)
let graph (ease : Juice.ease) (computer : computer) (started : time) : shape =
  let steps = 40 in
  let points =
    List.init (steps +.. 1) (fun i ->
        let t = float_of_int i / float_of_int steps in
        (t * graph_size, Juice.curve ease t * graph_size))
  in
  let rec lines = function p :: (q :: _ as rest) -> segment darkGray p q :: lines rest | _ -> [] in
  let time = Juice.tween Juice.linear 0. 1. going started computer in
  let value = Juice.tween ease 0. 1. going started computer in
  group
    ([ rectangle (rgb 238 238 238) graph_size graph_size |> move (graph_size / 2.) (graph_size / 2.) ]
    @ lines points
    @ [ circle red 4. |> move (time * graph_size) (value * graph_size) ])

let panel (name : string) (ease : Juice.ease) (computer : computer) (started : time) : shape =
  let value = Juice.tween ease 0. 1. going started computer in
  group
    [
      words black name |> scale 1.3 |> move (-40. + (track / 2.)) 30.;
      graph ease computer started |> move (-150.) (-40.);
      rectangle (rgb 210 210 210) track 4. |> move (-40. + (track / 2.)) 0.;
      circle blue 12. |> move (-40. + (value * track)) 0.;
    ]

let view (computer : computer) () : shape list =
  let (Time now) = computer.time in
  let started = Time (Float.of_int (int_of_float (now / cycle)) * cycle) in
  let columns = [ -330.; 0.; 330. ] in
  let row y (name, (i, o, io)) =
    List.map2
      (fun x (prefix, ease) -> panel (prefix ^ name) ease computer started |> move x y)
      columns
      [ ("in_", i); ("out_", o); ("in_out_", io) ]
  in
  (panel "linear" Juice.linear computer started |> move 0. 400.)
  :: List.concat (List.mapi (fun k family -> row (270. - (float_of_int k * 125.)) family) families)

let update (_ : computer) () = ()

let app = game view update ()
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
