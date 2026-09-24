(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of the Roland TB-303 Bass Line (1981): a monophonic
 * synthesizer with its own 16-step sequencer, made for guitarists to
 * practise with, which failed, and then, its knobs turned while it
 * played, became acid house. The voice and its sequencer are
 * Tb303_voice.ml (over Diode_ladder.mli and Sequencer.mli); this is
 * its panel and its pattern.
 *
 * The real 303 is famously hard to program (a keypad, pitches and times
 * entered separately, blind); here the pattern is a grid to see and
 * click: the 16 steps as columns, a piano roll of an octave above C2 --
 * a click sets the step's note, again the same cell makes it a rest --
 * and under it each step's octave (down, as written, up), accent and
 * slide, toggled by a click. The step playing is lit. The knobs, the
 * 303's in its order (Tuning, Cut Off Freq, Resonance, Env Mod, Decay,
 * Accent), then the waveform, the tempo and the volume, turn by
 * dragging; run starts the pattern (space too). The letters play along,
 * a s d f g h j k the white keys from C, w e t y u the black ones, z and
 * x an octave. Under the grid, the scope and the spectrum: the squelch
 * seen.
 *
 * Uses: Tb303_voice (the voice, its patterns), Sequencer (the audio
 * clock's steps), Diode_ladder, Audio's instruments (the voice played
 * live), Gui (the knobs, the switch, the buttons), Spectrum. Not: the
 * effects rack, Scene2d, Sprite, File_menu.
 *
 * Exercises: several patterns and a song (the 303's pattern chains);
 * the gate's length and the slide's time as knobs (the Devil Fish's
 * mods); swing (every other 16th late); the effects rack after it (a
 * delay and a distortion: the acid house of 1988 was mostly that);
 * saving patterns with the File menu.
 *)
open Playground
open Basics (* float arithmetics *)

(* the letters: each the semitone above the octave's C *)
let letters =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

type model = {
  patch : Tb303_voice.patch;
  preset : int;
  octave : int;
  held : string list;
  space : bool; (* space held at the last frame *)
}

let presets = Tb303_voice.presets
let initial_model : model = { patch = snd (List.hd presets); preset = 0; octave = 3; held = []; space = false }

(* the voice lives with the sound: the mixer pulls its blocks, and its
 * sequencer steps in them *)
let voice = Tb303_voice.create initial_model.patch
let inst : Instrument.t = Tb303_voice.instrument voice

(*****************************************************************************)
(* The panel *)
(*****************************************************************************)

let panel_theme : Theme.t =
  {
    Theme.default with
    text = rgb 30 30 30;
    accent = rgb 230 80 40;
    edge = rgb 90 90 95;
    face = rgb 160 160 165;
    face_hot = rgb 180 180 185;
    face_down = rgb 130 130 135;
    text_size = 15.;
    dial = 40.;
    dial_face = rgb 25 25 25;
    pointer = rgb 240 240 240;
  }

type place = { name : string; x : number; label : string }

let knob_y = 385.

let places =
  [
    { name = "tuning"; x = -400.; label = "TUNING" };
    { name = "cutoff"; x = -300.; label = "CUT OFF FREQ" };
    { name = "resonance"; x = -200.; label = "RESONANCE" };
    { name = "env.mod"; x = -100.; label = "ENV MOD" };
    { name = "decay"; x = 0.; label = "DECAY" };
    { name = "accent"; x = 100.; label = "ACCENT" };
    { name = "waveform"; x = 200.; label = "WAVEFORM" };
    { name = "tempo"; x = 300.; label = "TEMPO" };
    { name = "volume"; x = 400.; label = "VOLUME" };
  ]

let control (computer : computer) (p : Tb303_voice.patch) (pl : place) : Tb303_voice.patch =
  match List.find_opt (fun (k : Tb303_voice.knob) -> k.name = pl.name) Tb303_voice.knobs with
  | None -> p
  | Some k ->
      let v = k.get p in
      let v' =
        match k.control with
        | Knob (from, to_) -> Gui.knob computer ~at:(pl.x, knob_y) ~from ~to_ v
        | Switch -> if Gui.rocker computer ~at:(pl.x, knob_y) (v >= 0.5) then 1. else 0.
        (* short labels, to fit around the switch *)
        | Selector labels ->
            let labels = if pl.name = "waveform" then [ "saw"; "sq" ] else labels in
            float_of_int (Gui.selector computer ~at:(pl.x, knob_y) labels (int_of_float v))
      in
      if v' <> v then k.put p v' else p

(*****************************************************************************)
(* The pattern's grid *)
(*****************************************************************************)

(* 16 columns; 13 rows of pitch, C2 to C3, then the octave, the accent
 * and the slide rows *)
let base = 36 (* C2 *)
let columns = 16
let cell_w = 55.
let cell_h = 24.
let grid_left = -440.
let pitch_top = 262.
let column_x (c : int) : number = grid_left + ((float_of_int c + 0.5) * cell_w)
let pitch_y (r : int) : number = pitch_top - ((float_of_int (12 -.. r) + 0.5) * cell_h) (* row r = C2 + r semitones *)
let octave_y = pitch_top - (13.5 * cell_h) - 8.
let accent_y = octave_y - cell_h - 4.
let slide_y = accent_y - cell_h - 4.

(* a step's note split into its row (0 to 12) and its octave (-1, 0, 1) *)
let row_and_octave (n : int) : int * int =
  let d = n -.. base in
  if d >= 0 && d <= 12 then (d, 0) else if d < 0 then ((d +.. 12) mod 12, -1) else ((d -.. 12) mod 13, 1)

let under (x : number) (y : number) (row_y : number) : int option =
  if Float.abs (y - row_y) > cell_h / 2. then None
  else
    let c = int_of_float (Float.of_int (int_of_float ((x - grid_left) / cell_w))) in
    if x < grid_left || c < 0 || c >= columns then None else Some c

(* a click on the grid: the pattern with that step changed *)
let click (p : Sequencer.step array) (x : number) (y : number) : Sequencer.step array =
  let p = Array.init columns (fun i -> if i < Array.length p then p.(i) else Sequencer.rest) in
  let set c s =
    let q = Array.copy p in
    q.(c) <- s;
    q
  in
  let pitch_row = List.find_opt (fun r -> Float.abs (y - pitch_y r) <= cell_h / 2.) (List.init 13 (fun r -> r)) in
  match pitch_row with
  | Some r -> (
      match under x y (pitch_y r) with
      | None -> p
      | Some c -> (
          let s = p.(c) in
          match s.note with
          (* the same cell again: a rest *)
          | Some n when fst (row_and_octave n) = r -> set c { s with note = None }
          | Some n -> set c { s with note = Some (base +.. r +.. (12 *.. snd (row_and_octave n))) }
          | None -> set c { s with note = Some (base +.. r) }))
  | None -> (
      let toggle row_y f = Option.map (fun c -> set c (f p.(c))) (under x y row_y) in
      let octave (s : Sequencer.step) =
        match s.note with
        | None -> s
        | Some n ->
            let r, o = row_and_octave n in
            let o = if o = 1 then -1 else o +.. 1 in
            { s with note = Some (base +.. r +.. (12 *.. o)) }
      in
      match toggle octave_y octave with
      | Some q -> q
      | None -> (
          match toggle accent_y (fun s -> { s with accent = not s.accent }) with
          | Some q -> q
          | None -> Option.value (toggle slide_y (fun s -> { s with slide = not s.slide })) ~default:p))

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "tb303" (fun () -> inst));
  Gui.set_theme Theme.default;
  let preset = Gui.menu computer ~at:(330., 482.) (List.map fst presets) m.preset in
  let patch = if preset <> m.preset then snd (List.nth presets preset) else m.patch in
  Gui.set_theme panel_theme;
  let patch = List.fold_left (control computer) patch places in
  let run_pressed = Gui.button computer ~at:(-400., 318.) (if Tb303_voice.running voice then "STOP" else "RUN") in
  let space = computer.keyboard.kspace in
  if run_pressed || (space && not m.space) then Tb303_voice.run voice (not (Tb303_voice.running voice));
  let mouse = computer.mouse in
  let patch = if mouse.mclick then { patch with pattern = click patch.pattern mouse.mx mouse.my } else patch in
  (* the letters: a line played over it *)
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) and released k = List.mem k m.held && not (List.mem k now) in
  let octave = if pressed "z" then max 1 (m.octave -.. 1) else if pressed "x" then min 5 (m.octave +.. 1) else m.octave in
  List.iter
    (fun (k, semitone) ->
      let n = (12 *.. (m.octave +.. 1)) +.. semitone in
      if pressed k then inst.note_on n 1.;
      if released k then inst.note_off n)
    letters;
  Tb303_voice.set_patch voice patch;
  { patch; preset; octave; held = now; space }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 30 30 30
let text (s : string) : shape = words ink s |> scale 1.1

let grid_view (p : Tb303_voice.patch) : shape list =
  let playing = if Tb303_voice.running voice then Some (Tb303_voice.step voice) else None in
  let names = [| "C"; "C#"; "D"; "Eb"; "E"; "F"; "F#"; "G"; "Ab"; "A"; "Bb"; "B"; "C" |] in
  let cell color x y = rectangle color (cell_w - 3.) (cell_h - 3.) |> move x y in
  let rows =
    List.concat
      (List.init 13 (fun r ->
           (text names.(r) |> move (grid_left - 20.) (pitch_y r))
           :: List.init columns (fun c ->
                  let black = List.mem (r mod 12) [ 1; 3; 6; 8; 10 ] in
                  cell (if black then rgb 150 150 155 else rgb 175 175 180) (column_x c) (pitch_y r))))
  in
  let steps =
    List.concat
      (List.init columns (fun c ->
           let s = if c < Array.length p.pattern then p.pattern.(c) else Sequencer.rest in
           let lit = playing = Some c in
           (if lit then [ rectangle (rgb 250 220 120) (cell_w - 3.) (13. * cell_h) |> move (column_x c) (pitch_top - (6.5 * cell_h)) ]
            else [])
           @ (match s.note with
             | None -> [ text "-" |> move (column_x c) (pitch_y 6) ]
             | Some n ->
                 let r, o = row_and_octave n in
                 [
                   cell (if s.accent then rgb 230 80 40 else rgb 40 40 45) (column_x c) (pitch_y r);
                   cell (rgb 200 200 205) (column_x c) octave_y;
                   text (match o with -1 -> "DOWN" | 1 -> "UP" | _ -> "") |> move (column_x c) octave_y;
                 ])
           @ [
               cell (if s.accent then rgb 230 80 40 else rgb 200 200 205) (column_x c) accent_y;
               cell (if s.slide then rgb 60 110 200 else rgb 200 200 205) (column_x c) slide_y;
               text (string_of_int (c +.. 1)) |> move (column_x c) (pitch_top + 12.);
             ]))
  in
  [ rectangle (rgb 120 120 125) (float_of_int columns * cell_w + 6.) (13. * cell_h + 6.) |> move 0. (pitch_top - (6.5 * cell_h)) ]
  @ rows @ steps
  @ [ text "OCTAVE" |> move (grid_left - 30.) octave_y; text "ACCENT" |> move (grid_left - 30.) accent_y; text "SLIDE" |> move (grid_left - 30.) slide_y ]

let scope_view (samples : Signal.t) : shape list =
  let cx = -235. and cy = -260. and w = 440. and h = 150. in
  let start = ref 0 in
  (try
     for i = 1 to 1023 do
       if samples.(i -.. 1) < 0. && samples.(i) >= 0. then (
         start := i;
         raise Exit)
     done
   with Exit -> ());
  let point j = (cx - (w / 2.) + (float_of_int j * w / 256.), cy + (Float.max (-1.) (Float.min 1. samples.(!start +.. (j *.. 4))) * h / 2.)) in
  let segment (x1, y1) (x2, y2) =
    rectangle (rgb 120 255 140) (Float.hypot (x2 - x1) (y2 - y1) + 1.) 2.
    |> rotate (Float.atan2 (y2 - y1) (x2 - x1) * 180. / Float.pi)
    |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)
  in
  (rectangle (rgb 15 25 15) w h |> move cx cy) :: List.init 255 (fun j -> segment (point j) (point (j +.. 1)))

let spectrum_view (samples : Signal.t) : shape list =
  let cx = 235. and cy = -260. and w = 440. and h = 150. in
  let mags = Spectrum.of_signal samples in
  let n = 2 *.. (Array.length mags -.. 1) in
  let bars = 60 in
  let freq b = 20. * (1000. ** (float_of_int b / float_of_int bars)) in
  let bar b =
    let lo = freq b and hi = freq (b +.. 1) in
    let top = ref 0. in
    Array.iteri
      (fun k v ->
        let f = Spectrum.bin_frequency ~n k in
        if f >= lo && f < hi && v > !top then top := v)
      mags;
    let db = if !top <= 0. then -80. else Float.max (-80.) (20. * log10 !top) in
    let bh = (db + 80.) / 80. * h in
    let bw = w / float_of_int bars in
    rectangle (rgb 230 80 40) (bw - 1.) (Float.max 1. bh) |> move (cx - (w / 2.) + ((float_of_int b + 0.5) * bw)) (cy - (h / 2.) + (bh / 2.))
  in
  (rectangle (rgb 25 20 18) w h |> move cx cy) :: List.init bars bar

let view (computer : computer) (m : model) : shape list =
  let samples = Tb303_voice.recent voice in
  [ rectangle (rgb 215 215 220) computer.screen.width computer.screen.height ]
  @ [ words black "TinyTB303" |> scale 2.4 |> move (-380.) 482.; words black "preset" |> scale 1.5 |> move 230. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-110.) 482. ]
  @ [ rectangle (rgb 195 195 200) 960. 175. |> move 0. 372.; words (rgb 230 80 40) "Bass Line" |> scale 1.6 |> move 380. 312. ]
  @ List.map (fun pl -> text pl.label |> move pl.x (knob_y - (if pl.name = "waveform" then 30. else 38.))) places
  @ grid_view m.patch
  @ scope_view samples @ spectrum_view samples
  @ [
      words (rgb 70 70 70)
        (Printf.sprintf "%s   %.0f BPM   cutoff %.0f Hz   letters: play along   space: run / stop"
           (if Tb303_voice.running voice then Printf.sprintf "step %d" (Tb303_voice.step voice +.. 1) else "stopped")
           m.patch.bpm (Tb303_voice.cutoff_now voice))
      |> scale 1.3 |> move 0. (-360.);
    ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app app
