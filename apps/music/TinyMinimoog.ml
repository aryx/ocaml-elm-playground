(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of the Minimoog Model D (Moog Music, 1970), the
 * synthesizer that took the modular synthesizer's sound out of the studio
 * and onto the stage: three oscillators, a mixer, the ladder filter, two
 * contours, glide and a modulation wheel, wired once and for all behind
 * a panel read left to right -- CONTROLLERS, OSCILLATOR BANK, MIXER,
 * MODIFIERS, OUTPUT -- in black between two wooden cheeks. The voice is
 * Voice_minimoog.ml, over audio/'s blocks; this is the synthesizer: its
 * panel and its keyboard.
 *
 * The panel is a part, Part_minimoog.ml (Component.mli, the office's
 * idea, plan_tiny_reason.md), the same panel a rack would hold. This
 * program is its host at full size, with the keyboard (Piano) and the
 * wheels, the scope and the spectrum (Meters), the effects rack under
 * the panel, the presets, the teaching switches.
 *
 * The knobs turn by dragging them up or down, the rotary switches (the
 * oscillators' ranges and waveforms) by dragging or a click, the rockers
 * by a click (Gui's knob, selector and rocker). The keyboard plays with
 * the mouse, or with the letters: a s d f g h j k the white keys from C,
 * w e t y u the black ones, z and x an octave down and up. Left of it,
 * the two wheels: pitch (it springs back; the arrows up and down too) and
 * modulation (it stays). Hold a key and press another: the lowest sounds
 * (low-note priority), and the contours go on (legato). Under the panel,
 * what the voice just played: an oscilloscope and a spectrum.
 *
 * Keys 1 to 4 flip the teaching switches: the ladder (naive,
 * zero-delay, nonlinear), the contours' curves (straight or
 * exponential), the oscillators' drift, and their band-limiting -- the
 * simple and the better versions of notes_synth.md, on the same patch,
 * heard and seen on the spectrum. Key 5 puts the reverb before the
 * drive in the effects rack: the order's lesson (Rack.mli), the mud.
 * The effects, under the panel in place of the scope (the button above
 * the panel, pressed again for the second page, then back to the
 * scope): drive, EQ, delay, reverb; then the modulation (a chorus, a
 * flanger or a phaser) and the dynamics (a compressor, a limiter or a
 * gate), with the compressor's needle, its gain reduction -- each
 * switched on by its rocker. Above the panel, the latency: how late a
 * key's note is, the sound queued ahead of the card (Audio.latency; 0
 * in a golden run, which has no card).
 *
 * Uses: Voice_minimoog (the voice), Audio's instruments (the voice
 * played live), Part_minimoog (the panel, over Panel's widgets),
 * Component, Piano, Meters, Rack (the effects, audio/effects/), Gui
 * (the rack's knobs, rockers and rotary switches, the menu), Spectrum
 * (the display). Not: Scene2d, Sprite, the physics, File_menu yet.
 *
 * Exercises: saving and opening patches with the File menu
 * (appkits/file_menu; the text is Voice_minimoog.to_string); the
 * reissue's additions (a separate LFO, a choice of note priority, the
 * filter contour as a modulation source); velocity on the filter, from
 * a MIDI keyboard; a second voice, the Minimoog made duophonic like the
 * ARP Odyssey.
 *)
open Playground
open Basics (* float arithmetics *)

type model = {
  panel : Component.part; (* the Model D's panel, Part_minimoog *)
  preset : int; (* an index in Voice_minimoog.presets *)
  keys : Piano.t;
  mod_wheel : float;
  pitch_wheel : float;
  options : Voice_minimoog.options;
  lower : int; (* under the panel: 0 the scope, 1 and 2 the rack's pages *)
  reverb_first : bool; (* the rack's order: the mud of a drive after a reverb *)
  held_keys : string list; (* the keys held at the last frame: 1 to 5 *)
}

let presets = Voice_minimoog.presets

(* the voice lives with the sound, not in the model: the mixer pulls
 * its blocks between frames (Instrument.mli) *)
let voice = Voice_minimoog.create (snd (List.hd presets))
let inst : Instrument.t = Voice_minimoog.instrument voice

let initial_model : model =
  {
    panel = Part_minimoog.make voice;
    preset = 0;
    keys = Piano.initial ~octave:3;
    mod_wheel = 0.;
    pitch_wheel = 0.;
    options = Voice_minimoog.analog;
    lower = 0;
    reverb_first = false;
    held_keys = [];
  }

(* the panel at its own size, under the title *)
let panel_box : Widget.box = { x = 0.; y = 245.; w = fst Part_minimoog.natural; h = snd Part_minimoog.natural }

let next_ladder (l : Moog_ladder.model) : Moog_ladder.model =
  match l with Naive -> Zero_delay | Zero_delay -> Nonlinear | Nonlinear -> Naive

(*****************************************************************************)
(* The effects rack *)
(*****************************************************************************)

(* where each control sits, and the word under it *)
type place = { name : string; x : number; y : number; label : string }

let place name x y label = { name; x; y; label }

(* the effects rack, in the strip under the panel, in the rack's order,
 * on two pages *)
let rack_y = -60.

let first_page =
  [
    place "drive.on" (-455.) rack_y "";
    place "drive.shape" (-390.) rack_y "SHAPE";
    place "drive.gain" (-315.) rack_y "DRIVE";
    place "drive.oversampling" (-275.) rack_y "X4";
    place "eq.on" (-240.) rack_y "";
    place "eq.bass" (-206.) rack_y "BASS";
    place "eq.middle" (-160.) rack_y "MID";
    place "eq.treble" (-114.) rack_y "TREBLE";
    place "delay.on" (-62.) rack_y "";
    place "delay.time" (-22.) rack_y "TIME";
    place "delay.feedback" 26. rack_y "REPEAT";
    place "delay.tone" 74. rack_y "TONE";
    place "delay.pingpong" 112. rack_y "PING";
    place "delay.mix" 150. rack_y "MIX";
    place "reverb.on" 190. rack_y "";
    place "reverb.kind" 255. rack_y "ROOM";
    place "reverb.time" 330. rack_y "TIME";
    place "reverb.damping" 378. rack_y "DAMP";
    place "reverb.mix" 426. rack_y "MIX";
  ]

let second_page =
  [
    place "modulation.on" (-455.) rack_y "";
    place "modulation.kind" (-390.) rack_y "KIND";
    place "modulation.rate" (-315.) rack_y "RATE";
    place "modulation.depth" (-267.) rack_y "DEPTH";
    place "modulation.feedback" (-219.) rack_y "FDBK";
    place "modulation.mix" (-171.) rack_y "MIX";
    place "dynamics.on" (-125.) rack_y "";
    place "dynamics.mode" (-60.) rack_y "MODE";
    place "dynamics.threshold" 15. rack_y "THRESH";
    place "dynamics.ratio" 63. rack_y "RATIO";
    place "dynamics.attack" 111. rack_y "ATK";
    place "dynamics.release" 159. rack_y "REL";
    place "dynamics.makeup" 207. rack_y "GAIN";
  ]

(* each page: its controls, its sections' headers, the lines between *)
let pages =
  [|
    (first_page, [ ("DRIVE", -365.); ("EQ", -170.); ("DELAY", 44.); ("REVERB", 308.) ], [ -258.; -80.; 170. ]);
    (second_page, [ ("MODULATION", -320.); ("DYNAMICS", 40.); ("GAIN REDUCTION", 365.) ], [ -145.; 245. ]);
  |]

(* the rack's order, and the order of key 5, the reverb first *)
let usual_order = [ "drive"; "eq"; "modulation"; "delay"; "reverb"; "dynamics" ]
let reverb_first_order = "reverb" :: List.filter (fun n -> n <> "reverb") usual_order

(* the rotary switches' positions, short enough to fit around them *)
let short (name : string) : string list =
  if name = "drive.shape" then [ "hard"; "tanh"; "x3"; "asym" ]
  else if name = "reverb.kind" then [ "1962"; "free"; "plate" ]
  else if name = "modulation.kind" then [ "chor"; "flan"; "phas" ]
  else [ "comp"; "lim"; "gate" ]

let control (computer : computer) (p : Voice_minimoog.patch) (pl : place) : Voice_minimoog.patch =
  match List.find_opt (fun (k : Voice_minimoog.knob) -> k.name = pl.name) Voice_minimoog.knobs with
  | None -> p
  | Some k ->
      let v = k.get p in
      let v' =
        match k.control with
        | Knob (from, to_) -> Gui.knob computer ~at:(pl.x, pl.y) ~from ~to_ v
        | Switch -> if Gui.rocker computer ~at:(pl.x, pl.y) (v >= 0.5) then 1. else 0.
        | Selector _ -> float_of_int (Gui.selector computer ~at:(pl.x, pl.y) (short pl.name) (int_of_float v))
      in
      if v' <> v then k.put p v' else p

(*****************************************************************************)
(* The keyboard and the wheels *)
(*****************************************************************************)

(* two octaves and a C *)
let look : Piano.look =
  {
    keys = 25;
    left = -360.;
    top = -165.;
    white_width = 56.;
    white_height = 270.;
    black_height = 165.;
    letters_from = 0;
    velocity = 1.;
    octaves = (1, 5);
    by_depth = false;
    white_key = rgb 250 250 245;
    black_key = rgb 20 20 20;
    letter_on_white = rgb 120 120 120;
    letter_scale = 1.6;
    letter_lift = 18.;
  }

let wheel_height = 200.
let wheel_y = look.top - (look.white_height / 2.)
let pitch_wheel_x = -455.
let mod_wheel_x = -405.
let in_wheel (x0 : number) (m : mouse) = Float.abs (m.mx - x0) <= 18. && Float.abs (m.my - wheel_y) <= wheel_height / 2.

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  (* playing from the first frame, and kept playing *)
  ignore (Audio.instrument "minimoog" (fun () -> inst));
  (* the preset menu, in the light theme, then the panel in its own *)
  Gui.set_theme Theme.default;
  let preset = Gui.menu computer ~at:(330., 482.) (List.map fst presets) m.preset in
  if preset <> m.preset then Voice_minimoog.set_patch voice (snd (List.nth presets preset));
  let label = match m.lower with 0 -> "effects" | 1 -> "more" | _ -> "scope" in
  let lower = if Gui.button computer ~at:(95., 482.) label then (m.lower +.. 1) mod 3 else m.lower in
  (* the panel, unless the menu has the mouse *)
  let panel = if Gui.modal () then m.panel else Component.input_in ~scaled:false m.panel computer panel_box in
  (* the rack's controls only when shown: hidden, they'd still take the
   * mouse *)
  Gui.set_theme Part_minimoog.theme;
  if m.lower > 0 then begin
    let shown, _, _ = pages.(m.lower -.. 1) in
    Voice_minimoog.set_patch voice (List.fold_left (control computer) (Voice_minimoog.patch voice) shown)
  end;
  (* the letters, and the mouse on the keyboard *)
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held_keys) in
  let keys = Piano.update look computer m.keys inst in
  let mouse = computer.mouse in
  (* the wheels: the pitch wheel springs back, the mod wheel stays *)
  let wheel_value = (mouse.my - wheel_y) / (wheel_height / 2.) in
  let pitch_wheel =
    if mouse.mdown && in_wheel pitch_wheel_x mouse then Float.max (-1.) (Float.min 1. wheel_value)
    else if computer.keyboard.kup then 1.
    else if computer.keyboard.kdown then -1.
    else 0.
  in
  let mod_wheel =
    if mouse.mdown && in_wheel mod_wheel_x mouse then Float.max 0. (Float.min 1. ((wheel_value + 1.) / 2.)) else m.mod_wheel
  in
  (* the teaching switches *)
  let o = m.options in
  let options =
    if pressed "1" then { o with ladder = next_ladder o.ladder }
    else if pressed "2" then { o with curve = (match o.curve with Linear -> Exponential | Exponential -> Linear) }
    else if pressed "3" then { o with drift = not o.drift }
    else if pressed "4" then { o with band_limited = not o.band_limited }
    else o
  in
  let reverb_first = if pressed "5" then not m.reverb_first else m.reverb_first in
  if reverb_first <> m.reverb_first then
    Rack.reorder (Voice_minimoog.rack voice) (if reverb_first then reverb_first_order else usual_order);
  Voice_minimoog.set_options voice options;
  inst.set "mod_wheel" mod_wheel;
  inst.set "pitch_wheel" pitch_wheel;
  { panel; preset; keys; mod_wheel; pitch_wheel; held_keys = now; options; lower; reverb_first }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let white_ink = rgb 235 235 235
let text (s : string) : shape = words white_ink s |> scale 1.2

let segment (color : color) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  rectangle color (Float.hypot (x2 - x1) (y2 - y1) + 1.) 2.
  |> rotate (Float.atan2 (y2 - y1) (x2 - x1) * 180. / Float.pi)
  |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

(* the last 2048 samples: the oscilloscope, from a rising zero crossing,
 * so a steady note stands still (the spectrum is Meters') *)
let scope_view (samples : Signal.t) : shape list =
  let cx = -235. and cy = -55. and w = 440. and h = 120. in
  let start = ref 0 in
  (try
     for i = 1 to 1023 do
       if samples.(i -.. 1) < 0. && samples.(i) >= 0. then (
         start := i;
         raise Exit)
     done
   with Exit -> ());
  let point j = (cx - (w / 2.) + (float_of_int j * w / 256.), cy + (samples.(!start +.. (j *.. 4)) * h)) in
  [ rectangle (rgb 15 25 15) w h |> move cx cy; segment (rgb 40 70 40) (cx - (w / 2.), cy) (cx + (w / 2.), cy) ]
  @ List.init 256 (fun j -> segment (rgb 120 255 140) (point j) (point (j +.. 1)))

(* the compressor's needle, as a bar: its gain reduction, 0 to 24 dB,
 * growing from the left as the sound is turned down *)
let reduction_view (db : number) : shape list =
  let x0 = 275. and w = 190. and y = rack_y + 5. in
  let filled = Float.min 1. (db / 24.) * w in
  [ rectangle (rgb 15 15 15) w 22. |> move (x0 + (w / 2.)) y; rectangle (rgb 250 190 80) (Float.max 1. filled) 22. |> move (x0 + (filled / 2.)) y ]
  @ List.map (fun k -> text (string_of_int (6 *.. k)) |> move (x0 + (float_of_int k * w / 4.)) (y - 25.)) [ 0; 1; 2; 3; 4 ]
  @ [ text (Printf.sprintf "%.1f dB" db) |> move (x0 + (w / 2.)) (y + 25.) ]

(* the rack's page: its sections, black as the panel, between the same
 * lines; the rockers switch each stage on *)
let rack_view (page : int) (reduction : number) : shape list =
  let shown, headers, lines = pages.(page) in
  [ rectangle (rgb 25 25 25) 960. 140. |> move 0. (-55.) ]
  @ List.map (fun (h, x) -> words white_ink h |> scale 1.3 |> move x 0.) headers
  @ List.map (fun x -> rectangle (rgb 90 90 90) 2. 120. |> move x (-55.)) lines
  @ List.filter_map
      (fun pl ->
        if pl.label = "" then None
        else
          let selector = List.exists (Filename.check_suffix pl.name) [ ".shape"; ".kind"; ".mode" ] in
          Some (text pl.label |> move pl.x (pl.y - if selector then 30. else 38.)))
      shown
  @ if page = 1 then reduction_view reduction else []

let wheel_view (x : number) (value : number) (label : string) : shape list =
  [
    rectangle (rgb 25 25 25) 36. (wheel_height + 10.) |> move x wheel_y;
    rectangle (rgb 80 80 80) 24. wheel_height |> move x wheel_y;
    rectangle white_ink 24. 6. |> move x (wheel_y + (value * wheel_height / 2.));
    words black label |> scale 1.2 |> move x (wheel_y - (wheel_height / 2.) - 20.);
  ]

let status (m : model) : string =
  let o = m.options in
  Printf.sprintf "1 ladder: %s   2 contours: %s   3 drift: %s   4 oscillators: %s   5 rack: %s" (Moog_ladder.name o.ladder)
    (match o.curve with Linear -> "straight" | Exponential -> "exponential")
    (if o.drift then "on" else "off")
    (if o.band_limited then "band-limited" else "naive")
    (if m.reverb_first then "reverb first" else "drive first")

let view (computer : computer) (m : model) : shape list =
  let samples = Voice_minimoog.recent voice in
  [ rectangle (rgb 215 205 190) computer.screen.width computer.screen.height ]
  @ [ words black "TinyMinimoog" |> scale 2.4 |> move (-360.) 482.; words black "preset" |> scale 1.5 |> move 230. 482. ]
  (* how late a key's note is, as far as the program knows (Audio.mli) *)
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-110.) 482. ]
  @ Component.draw_in ~scaled:false m.panel panel_box ~active:true
  @ (if m.lower > 0 then rack_view (m.lower -.. 1) (Rack.meter (Voice_minimoog.rack voice) "dynamics.reduction")
     else
       scope_view samples
       @ Meters.spectrum ~at:(235., -55.) ~size:(440., 120.) ~color:(rgb 250 190 80) ~back:(rgb 25 20 10) ~bars:60 samples)
  @ [ words (rgb 70 70 70) (status m) |> scale 1.4 |> move 0. (-137.) ]
  @ wheel_view pitch_wheel_x m.pitch_wheel "PITCH"
  @ wheel_view mod_wheel_x ((m.mod_wheel * 2.) - 1.) "MOD"
  @ Piano.view look computer m.keys ~lit:(rgb 250 190 80)
  @ [ words black (Printf.sprintf "C%d" (Piano.octave m.keys)) |> scale 1.4 |> move (look.left + 20.) (look.top + 14.) ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
