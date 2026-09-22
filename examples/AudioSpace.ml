(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Sound in space: a car driving past you, heard with two ears
 * (audio/Space.mli). You are in the middle, facing up the screen; the
 * road runs left to right in front of you; the car goes by, around and
 * around, its engine a sawtooth through a low-pass, played every frame
 * (Audio.keep_playing) and changed by where the car is:
 *
 *   - panned: the sine of its angle from straight ahead, from the left
 *     speaker to the right one as it crosses (the constant power law:
 *     as loud in the middle as at the sides);
 *   - distance: half as loud each time it's twice as far (the inverse
 *     distance law, full volume within 20 m);
 *   - Doppler: higher coming, lower going, the pitch dropping as it
 *     passes -- by 3 semitones at 30 m/s, the "neeee-owww".
 *
 * Keys 1, 2, 3 turn each of the three off and on: hear what each one
 * does alone (and the software backend's "m", with -debug-keys, turns
 * stereo off altogether). Up and down: the speed. Space: the horn, a
 * one-shot, placed and shifted when it's blown (Audio.pan,
 * Audio.pitched and Audio.faster: a Doppler shift squeezes the whole
 * sound, not only its pitch).
 *
 * The world's scale: 10 pixels a meter, so the speed of sound, 343 m/s,
 * is 3430 pixels a second, and 30 m/s (108 km/h) 300.
 *
 * What it uses: the Playground, Scene2d (the keys pressed), Audio
 * (sawtooth, low_pass, louder, pan, pitched, faster, keep_playing,
 * play) and audio/'s Space directly (the formulas, shown on screen).
 *)
open Playground
open Basics (* float arithmetics *)

type state = {
  x : number; (* the car, on the road *)
  speed : number; (* pixels a second *)
  panning : bool;
  distance : bool;
  doppler : bool;
}

type model = state Scene2d.t

let initial_model : model = Scene2d.start { x = -600.; speed = 300.; panning = true; distance = true; doppler = true }
let road = 150.
let pixels_a_meter = 10.
let speed_of_sound = 343. * pixels_a_meter
let engine_hz = 110.

(* the listener, at the centre, facing up: its right is +x *)
let listener = Space.vec 0. 0. 0.
let right = Space.vec 1. 0. 0.
let still = Space.vec 0. 0. 0.

(* what the three effects make of the car at [s.x]: its pan, its gain,
 * its pitch factor (each 0, 1, 1 when off) *)
let heard (s : state) : number * number * number =
  let car = Space.vec s.x road 0. in
  let pan = if s.panning then Space.direction ~listener ~right car else 0. in
  let gain = if s.distance then Space.attenuation ~reference:(20. * pixels_a_meter) (Space.distance listener car) else 1. in
  let doppler =
    if s.doppler then
      Space.doppler ~speed_of_sound ~listener ~listener_velocity:still ~source:car ~source_velocity:(Space.vec s.speed 0. 0.)
    else 1.
  in
  (pan, gain, doppler)

let horn = Audio.together [ Audio.square 392.; Audio.square 494. ] |> Audio.lasting 0.4 |> Audio.low_pass 2000. |> Audio.louder 0.5

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let s = scenes.scene and k = computer.keyboard in
  let pressed key = Scene2d.pressed (fun k -> Set_.mem key k.keys) scenes in
  let s =
    { s with
      panning = (if pressed "1" then not s.panning else s.panning);
      distance = (if pressed "2" then not s.distance else s.distance);
      doppler = (if pressed "3" then not s.doppler else s.doppler);
      speed = max 50. (min 1000. (if k.kup then s.speed + 5. else if k.kdown then s.speed - 5. else s.speed));
    }
  in
  (* a fixed step, 1/60 s: the same run every time *)
  let x = s.x + (s.speed / 60.) in
  let s = { s with x = (if x > 700. then -700. else x) } in
  let (pan, gain, doppler) = heard s in
  Audio.keep_playing "engine"
    (Audio.sawtooth (engine_hz * doppler) |> Audio.low_pass 900. |> Audio.louder (1.2 * gain) |> Audio.pan pan);
  if Scene2d.pressed (fun k -> k.kspace) scenes then
    Audio.play (horn |> Audio.pitched doppler |> Audio.faster doppler |> Audio.louder gain |> Audio.pan pan);
  { scenes with scene = s }

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and s = model.scene in
  let (pan, gain, doppler) = heard s in
  let text size color str = words color str |> scale size in
  let on_off b = if b then "on" else "off" in
  let semitones = 12. * Float.log2 doppler in
  [ rectangle (rgb 30 60 30) screen.width screen.height;
    rectangle (rgb 70 70 75) screen.width 80. |> move_y road;
    (* you: a head, two ears *)
    circle (rgb 240 200 160) 22.;
    circle (rgb 220 170 130) 7. |> move_x (-24.);
    circle (rgb 220 170 130) 7. |> move_x 24.;
    (* the car, and the line from it to you *)
    rectangle (rgb 220 60 50) 70. 34. |> move s.x road;
    rectangle (rgb 255 255 255) (Float.hypot s.x road) 1.
    |> rotate (Float.atan2 road s.x * 180. / Float.pi)
    |> move (s.x / 2.) (road / 2.)
    |> fade 0.4;
    text 2.5 white "a car going by" |> move_y 420.;
    text 1.8 (rgb 220 220 230) (Printf.sprintf "speed %.0f m/s (up, down)   space: the horn" (s.speed / pixels_a_meter)) |> move_y 370.;
    text 1.8 (rgb 250 210 120)
      (Printf.sprintf "1  panning %s: %+.2f  (left %.2f, right %.2f)" (on_off s.panning) pan (fst (Space.pan pan)) (snd (Space.pan pan)))
    |> move_y (-150.);
    text 1.8 (rgb 250 210 120)
      (Printf.sprintf "2  distance %s: %.0f m, gain %.2f (%.1f dB)" (on_off s.distance)
         (Float.hypot s.x road / pixels_a_meter) gain (20. * log10 gain))
    |> move_y (-190.);
    text 1.8 (rgb 250 210 120)
      (Printf.sprintf "3  Doppler %s: x %.3f (%+.2f semitones), the engine at %.1f Hz" (on_off s.doppler) doppler semitones
         (engine_hz * doppler))
    |> move_y (-230.) ]

let help =
  {|Space
  keys:  1, 2, 3   panning, distance, Doppler: off and on
         up, down  the car's speed
         space     the horn
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
