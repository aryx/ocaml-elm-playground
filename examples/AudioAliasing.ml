(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Aliasing, heard and seen: a square wave's spectrum, from 0 to 22,050
 * Hz (the Nyquist frequency at 44,100 samples a second), its harmonics
 * in yellow, its aliases in red.
 *
 * A square wave at f is a sum of sines at f, 3f, 5f, ... forever
 * (audio/Oscillator.mli). Computed the simple way, a formula of the
 * phase (1 for the first half of a period, -1 for the second), its
 * harmonics above Nyquist don't disappear: they fold back below it
 * (audio/Signal.mli), where they are not harmonics of f any more -- the
 * red bars, all over the spectrum, out of tune with the note: a thin
 * whistle over it, worse the higher the note (hold right, and listen to
 * it get harsher). Space switches to the band-limited square
 * (PolyBLEP: the two samples on each jump pulled towards the middle):
 * the red bars low in the spectrum sink by 40 dB, the ones near Nyquist
 * much less (a two-sample correction can't tell them apart from the
 * top harmonics). The same mistake as a jagged edge in a picture, and
 * the same cure: in-between values where the jump is.
 *
 * Hold the mouse button to hear it, left to right the pitch (220 to
 * 7040 Hz, five octaves), or hold the left and right arrows (a
 * semitone a frame). The spectrum is computed here, each frame, from
 * the same oscillator the sound uses (audio/Spectrum.mli, an FFT of
 * 4096 samples): what is shown is what is heard. One trick keeps it
 * honest: the pitch is kept on a multiple of 44,100 / 4096 = 10.77 Hz
 * (the FFT's bins; a few cents off the pitch asked for, at most), so
 * the 4096 samples hold a whole number of periods, every harmonic and
 * every alias falls on one bin, nothing leaks into the bins around it
 * (Spectrum.mli: no window needed), and each bar is either a harmonic
 * or an alias, never a smear of both.
 *
 * What it uses: the Playground, Scene2d (space pressed), Audio (square,
 * naive, keep_playing), and audio/'s Oscillator and Spectrum directly.
 *)
open Playground
open Basics (* float arithmetics *)

type state = { frequency : number; naive : bool }
type model = state Scene2d.t

(* not 1000 Hz: 44,100 is 44.1 times 1000, and a 1000 Hz square's
 * aliases land 100 Hz from its harmonics, hard to tell apart *)
let initial_model : model = Scene2d.start { frequency = 1250.; naive = true }

(* the FFT's size, and its bins' spacing, 10.77 Hz *)
let n = 4096
let bin_hz = float_of_int Signal.rate / float_of_int n

(* the frequency on the nearest bin (see the top) *)
let on_bin (f : number) : number = Float.round (f / bin_hz) * bin_hz

(* x from -500 to 500: 220 Hz (A3) to 7040 Hz (A8), each octave the same
 * width *)
let pitch (x : number) : number = 220. * (2. ** (5. * (max (-500.) (min 500. x) + 500.) / 1000.))
let semitone = 2. ** (1. / 12.)

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let s = scenes.scene and k = computer.keyboard and m = computer.mouse in
  let frequency =
    if m.mdown then pitch m.mx
    else if k.kright then min 7040. (s.frequency * semitone)
    else if k.kleft then max 220. (s.frequency / semitone)
    else s.frequency
  in
  let naive = if Scene2d.pressed (fun k -> k.kspace) scenes then not s.naive else s.naive in
  if m.mdown || k.kright || k.kleft then
    Audio.keep_playing "square" (Audio.square (on_bin frequency) |> (if naive then Audio.naive else Fun.id) |> Audio.louder 0.6);
  { scenes with scene = { frequency; naive } }

(* the spectrum: 4096 samples of the square, bins 0 to 2048 (0 to
 * 22,050 Hz), drawn as 256 bars of 8 bins, each its loudest, from -80
 * to 0 dB *)
let bars = 256
let width = 860.
let height = 500.

let spectrum (s : state) : shape list =
  let f = on_bin s.frequency in
  let x = Oscillator.render ~band_limited:(not s.naive) Square ~frequency:f (float_of_int n / float_of_int Signal.rate + 0.001) in
  let mags = Spectrum.of_signal ~window:false (Array.sub x 0 n) in
  let per_bar = (Array.length mags -.. 1) /.. bars in
  (* the harmonics on the bins multiple of this one, the odd multiples *)
  let fundamental = int_of_float (Float.round (f / bin_hz)) in
  let bottom = -.height / 2. in
  List.init bars (fun b ->
      let m = ref 0. and loudest = ref 0 in
      for k = b *.. per_bar to ((b +.. 1) *.. per_bar) -.. 1 do
        if mags.(k) > !m then (m := mags.(k); loudest := k)
      done;
      let db = if !m <= 0. then -80. else max (-80.) (20. * log10 !m) in
      let h = (db + 80.) / 80. * height in
      let harmonic = !loudest mod fundamental = 0 && (!loudest /.. fundamental) mod 2 = 1 in
      let w = width / float_of_int bars in
      rectangle (if harmonic then rgb 250 200 80 else rgb 230 70 60) (max 1. (w - 1.)) (max 1. h)
      |> move ((-.width / 2.) + ((float_of_int b + 0.5) * w)) (bottom + (h / 2.)))

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and s = model.scene in
  let label text = words (rgb 170 170 190) text |> scale 1.5 in
  let mark hz text =
    let x = (-.width / 2.) + (width * hz / Signal.nyquist) in
    [ rectangle (rgb 70 70 90) 1. height |> move_x x; label text |> move x ((-.height / 2.) - 20.) ]
  in
  let db y text = [ rectangle (rgb 70 70 90) width 1. |> move_y y; label text |> move ((-.width / 2.) - 35.) y ] in
  (rectangle (rgb 20 20 30) screen.width screen.height :: db (-.height / 2.) "-80 dB")
  @ db 0. "-40 dB" @ db (height / 2.) "0 dB"
  @ List.concat_map (fun (hz, t) -> mark hz t) [ (5000., "5 kHz"); (10000., "10 kHz"); (15000., "15 kHz"); (20000., "20 kHz") ]
  @ spectrum s
  @ [ words white (Printf.sprintf "a %.0f Hz square, %s" (on_bin s.frequency) (if s.naive then "naive" else "band-limited (PolyBLEP)"))
      |> scale 2.5 |> move_y 380.;
      words (rgb 250 200 80) "yellow: its harmonics" |> scale 1.8 |> move (-200.) 330.;
      words (rgb 230 70 60) "red: aliases, folded back from above 22,050 Hz" |> scale 1.8 |> move 150. 330.;
      label "space: naive / band-limited;  hold the mouse (left-right) or the arrows: the pitch" |> move_y (-.screen.height / 2. + 30.) ]

let help =
  {|Aliasing
  keys:  space        naive or band-limited
         left, right  the pitch, a semitone a frame (and the sound)
  mouse: held, left to right, the pitch (and the sound)
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
