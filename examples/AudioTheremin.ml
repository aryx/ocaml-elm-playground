(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A theremin (Leon Theremin, 1920: the instrument played without
 * touching it, one hand for the pitch, the other for the volume): hold
 * the mouse button, left to right the pitch (110 to 880 Hz, three
 * octaves, as a real one's is spread along its antenna: each octave
 * the same width), bottom to top the volume.
 *
 * The whole instrument is one line (Audio.mli):
 *
 *   Audio.keep_playing "theremin" (Audio.tone pitch |> Audio.louder volume)
 *
 * called at every frame the button is held: a continuous sound, its
 * phase going on from frame to frame, its pitch and volume changing
 * smoothly, stopping by itself when the button is let go.
 *
 * What it uses: the Playground and Audio (tone, louder, keep_playing).
 *)
open Playground
open Basics (* float arithmetics *)

(* x from -500 to 500: 110 Hz (A2) to 880 Hz (A5), 3 octaves, each the
 * same width: 110 * 2^(3 (x + 500) / 1000) *)
let pitch (x : number) : number = 110. * (2. ** (3. * (x + 500.) / 1000.))
let volume (y : number) : number = max 0. (min 1. ((y + 500.) / 1000.))

let update (computer : computer) () : unit =
  let m = computer.mouse in
  if m.mdown then Audio.keep_playing "theremin" (Audio.tone (pitch m.mx) |> Audio.louder (2. * volume m.my))

let view (computer : computer) () : shape list =
  let m = computer.mouse and screen = computer.screen in
  (* the octaves' A's, marked along the bottom *)
  let marks =
    List.concat_map
      (fun i ->
        let x = -500. + (float_of_int i * 1000. / 3.) in
        (* the labels at the edges nudged in, to be seen whole *)
        let label_x = if i = 0 then x + 25. else if i = 3 then x - 25. else x in
        [ rectangle (rgb 80 80 100) 2. screen.height |> move_x x; words (rgb 150 150 170) (Printf.sprintf "A%d" (i +.. 2)) |> scale 2. |> move label_x (-460.) ])
      [ 0; 1; 2; 3 ]
  in
  (rectangle (rgb 20 20 35) screen.width screen.height :: marks)
  @ [ circle (if m.mdown then rgb 250 200 80 else rgb 120 120 140) (10. + (30. * volume m.my)) |> move m.mx m.my;
      words white (Printf.sprintf "%.0f Hz, volume %.0f%%" (pitch m.mx) (100. * volume m.my)) |> scale 2.5 |> move_y 450.;
      words (rgb 180 180 200) "hold the mouse button: left-right the pitch, down-up the volume" |> scale 1.6 |> move_y 410. ]

let app = game view update ()
let main = Playground_platform.run_app app
