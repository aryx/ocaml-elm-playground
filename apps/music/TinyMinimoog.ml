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
 * Minimoog_voice.ml, over audio/'s blocks; this is its panel and its
 * keyboard.
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
 * The effects (the button above the panel): drive, EQ, delay, reverb,
 * each switched on by its rocker.
 *
 * Uses: Minimoog_voice (the voice), Audio's instruments (the voice
 * played live), Rack (the effects, audio/effects/), Gui (the knobs,
 * rockers and rotary switches), Spectrum (the display). Not: Scene2d, Sprite, the physics, File_menu yet.
 *
 * Exercises: saving and opening patches with the File menu
 * (appkits/file_menu; the text is Minimoog_voice.to_string); the
 * reissue's additions (a separate LFO, a choice of note priority, the
 * filter contour as a modulation source); velocity on the filter, from
 * a MIDI keyboard; a second voice, the Minimoog made duophonic like the
 * ARP Odyssey.
 *)
open Playground
open Basics (* float arithmetics *)

(* the letters: each the semitone above the octave's C *)
let letters =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

type model = {
  patch : Minimoog_voice.patch;
  preset : int; (* an index in Minimoog_voice.presets *)
  octave : int; (* the drawn keyboard's lowest C, and the letter a's *)
  mod_wheel : float;
  pitch_wheel : float;
  held : string list; (* the letters held at the last frame *)
  mouse_note : int option; (* the key the mouse holds down *)
  options : Minimoog_voice.options;
  rack_shown : bool; (* under the panel, the effects instead of the scope *)
  reverb_first : bool; (* the rack's order: the mud of a drive after a reverb *)
}

let presets = Minimoog_voice.presets

let initial_model : model =
  {
    patch = snd (List.hd presets);
    preset = 0;
    octave = 3;
    mod_wheel = 0.;
    pitch_wheel = 0.;
    held = [];
    mouse_note = None;
    options = Minimoog_voice.analog;
    rack_shown = false;
    reverb_first = false;
  }

(* the voice lives with the sound, not in the model: the mixer pulls
 * its blocks between frames (Instrument.mli) *)
let voice = Minimoog_voice.create initial_model.patch
let inst : Instrument.t = Minimoog_voice.instrument voice
let note (octave : int) (semitone : int) : int = (12 *.. (octave +.. 1)) +.. semitone

let next_ladder (l : Moog_ladder.model) : Moog_ladder.model =
  match l with Naive -> Zero_delay | Zero_delay -> Nonlinear | Nonlinear -> Naive

(* {1 The panel} *)

(* white on black, the knobs black with a white pointer, the lit half
 * of a rocker the Model D's blue *)
let panel_theme : Theme.t =
  {
    Theme.default with
    text = rgb 235 235 235;
    accent = rgb 110 170 235;
    edge = rgb 150 150 150;
    face = rgb 70 70 70;
    face_hot = rgb 95 95 95;
    face_down = rgb 50 50 50;
    text_size = 15.;
    dial = 40.;
    dial_face = rgb 20 20 20;
    pointer = rgb 245 245 245;
  }

(* where each control sits, and the word under it *)
type place = { name : string; x : number; y : number; label : string }

let place name x y label = { name; x; y; label }

let places =
  [
    (* CONTROLLERS *)
    place "tune" (-425.) 360. "TUNE";
    place "glide" (-425.) 265. "GLIDE";
    place "mod.mix" (-425.) 170. "MOD MIX";
    place "mod.oscillators" (-450.) 80. "OSC";
    place "mod.filter" (-400.) 80. "FILTER";
    (* OSCILLATOR BANK *)
    place "osc1.range" (-300.) 360. "RANGE";
    place "osc1.wave" (-115.) 360. "WAVEFORM";
    place "osc2.range" (-300.) 255. "";
    place "osc2.frequency" (-210.) 255. "FREQUENCY";
    place "osc2.wave" (-115.) 255. "";
    place "osc3.range" (-300.) 150. "";
    place "osc3.frequency" (-210.) 150. "FREQUENCY";
    place "osc3.wave" (-115.) 150. "";
    place "osc3.keyboard" (-210.) 75. "OSC 3 CTRL";
    (* MIXER *)
    place "osc1.level" (-10.) 360. "OSC 1";
    place "osc1.on" (40.) 360. "";
    place "osc2.level" (-10.) 270. "OSC 2";
    place "osc2.on" (40.) 270. "";
    place "osc3.level" (-10.) 180. "OSC 3";
    place "osc3.on" (40.) 180. "";
    place "noise.level" (-10.) 90. "NOISE";
    place "noise.on" (40.) 90. "";
    (* MODIFIERS: the filter, then its contour, then the loudness's *)
    place "filter.cutoff" 115. 360. "CUTOFF";
    place "filter.emphasis" 195. 360. "EMPHASIS";
    place "filter.contour" 275. 360. "CONTOUR";
    place "filter.keyboard1" 335. 360. "KB 1";
    place "filter.keyboard2" 370. 360. "KB 2";
    place "filter.attack" 115. 260. "ATTACK";
    place "filter.decay" 195. 260. "DECAY";
    place "filter.sustain" 275. 260. "SUSTAIN";
    place "loudness.attack" 115. 150. "ATTACK";
    place "loudness.decay" 195. 150. "DECAY";
    place "loudness.sustain" 275. 150. "SUSTAIN";
    (* OUTPUT, and the Model D's two switches left of its keyboard *)
    place "volume" 430. 360. "VOLUME";
    place "decay.on" 410. 250. "DECAY";
    place "glide.on" 455. 250. "GLIDE";
  ]

(* the effects rack, in the strip under the panel, in the rack's order *)
let rack_y = -60.

let rack_places =
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

let rack_headers = [ ("DRIVE", -365.); ("EQ", -170.); ("DELAY", 44.); ("REVERB", 308.) ]

let headers =
  [ ("CONTROLLERS", -425.); ("OSCILLATOR BANK", -210.); ("MIXER", 15.); ("MODIFIERS", 195.); ("OUTPUT", 430.) ]

(* the rotary switches' positions, short enough to fit around them *)
let short (name : string) : string list =
  if name = "drive.shape" then [ "hard"; "tanh"; "x3"; "asym" ]
  else if name = "reverb.kind" then [ "1962"; "free"; "plate" ]
  else if Filename.check_suffix name ".range" then [ "LO"; "32"; "16"; "8"; "4"; "2" ]
  else if name = "osc3.wave" then [ "tri"; "rev"; "saw"; "sq"; "wide"; "narr" ]
  else [ "tri"; "shark"; "saw"; "sq"; "wide"; "narr" ]

let control (computer : computer) (p : Minimoog_voice.patch) (pl : place) : Minimoog_voice.patch =
  match List.find_opt (fun (k : Minimoog_voice.knob) -> k.name = pl.name) Minimoog_voice.knobs with
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

(* {1 The keyboard and the wheels} *)

let keys_count = 25 (* two octaves and a C *)
let is_black (s : int) : bool = List.mem (s mod 12) [ 1; 3; 6; 8; 10 ]
let white_width = 56.
let keyboard_left = -360.
let keyboard_top = -165.
let white_height = 270.
let black_height = 165.

(* the whites before semitone [s]: where a white key is, or where a
 * black one sits (on the line after that white) *)
let whites_before (s : int) : int = List.length (List.filter (fun i -> not (is_black i)) (List.init s (fun i -> i)))

let key_x (s : int) : number =
  let w = float_of_int (whites_before s) in
  if is_black s then keyboard_left + (w * white_width) else keyboard_left + ((w + 0.5) * white_width)

(* the key under the mouse: a black key first, it's on top *)
let key_at (x : number) (y : number) : int option =
  let keys = List.init keys_count (fun s -> s) in
  let hit s =
    let w = if is_black s then white_width * 0.6 else white_width in
    let h = if is_black s then black_height else white_height in
    Float.abs (x - key_x s) <= w / 2. && y <= keyboard_top && y >= keyboard_top - h
  in
  match List.find_opt (fun s -> is_black s && hit s) keys with Some s -> Some s | None -> List.find_opt hit keys

let wheel_height = 200.
let wheel_y = keyboard_top - (white_height / 2.)
let pitch_wheel_x = -455.
let mod_wheel_x = -405.
let in_wheel (x0 : number) (m : mouse) = Float.abs (m.mx - x0) <= 18. && Float.abs (m.my - wheel_y) <= wheel_height / 2.

(* {1 update} *)

let update (computer : computer) (m : model) : model =
  (* playing from the first frame, and kept playing *)
  ignore (Audio.instrument "minimoog" (fun () -> inst));
  (* the preset menu, in the light theme, then the panel in its own *)
  Gui.set_theme Theme.default;
  let preset = Gui.menu computer ~at:(330., 482.) (List.map fst presets) m.preset in
  let patch = if preset <> m.preset then snd (List.nth presets preset) else m.patch in
  let rack_shown = if Gui.button computer ~at:(95., 482.) (if m.rack_shown then "scope" else "effects") then not m.rack_shown else m.rack_shown in
  Gui.set_theme panel_theme;
  let patch = List.fold_left (control computer) patch places in
  (* the rack's controls only when shown: hidden, they'd still take the
   * mouse *)
  let patch = if m.rack_shown then List.fold_left (control computer) patch rack_places else patch in
  (* the letters: pressed and let go since the last frame *)
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) and released k = List.mem k m.held && not (List.mem k now) in
  let octave = if pressed "z" then max 1 (m.octave -.. 1) else if pressed "x" then min 5 (m.octave +.. 1) else m.octave in
  List.iter
    (fun (k, semitone) ->
      let n = note m.octave semitone in
      if pressed k then inst.note_on n 1.;
      if released k then inst.note_off n)
    letters;
  (* the mouse on the keyboard: a key down while pressed, sliding from
   * key to key *)
  let mouse = computer.mouse in
  let under = if mouse.mdown then Option.map (note m.octave) (key_at mouse.mx mouse.my) else None in
  if under <> m.mouse_note then (
    Option.iter inst.note_off m.mouse_note;
    Option.iter (fun n -> inst.note_on n 1.) under);
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
    Rack.reorder (Minimoog_voice.rack voice) (if reverb_first then [ "reverb"; "drive"; "eq"; "delay" ] else [ "drive"; "eq"; "delay"; "reverb" ]);
  Minimoog_voice.set_patch voice patch;
  Minimoog_voice.set_options voice options;
  inst.set "mod_wheel" mod_wheel;
  inst.set "pitch_wheel" pitch_wheel;
  { patch; preset; octave; mod_wheel; pitch_wheel; held = now; mouse_note = under; options; rack_shown; reverb_first }

(* {1 view} *)

let white_ink = rgb 235 235 235
let text (s : string) : shape = words white_ink s |> scale 1.2

let segment (color : color) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  rectangle color (Float.hypot (x2 - x1) (y2 - y1) + 1.) 2.
  |> rotate (Float.atan2 (y2 - y1) (x2 - x1) * 180. / Float.pi)
  |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

let panel_view : shape list =
  let wood = rgb 120 70 35 in
  [
    rectangle (rgb 25 25 25) 960. 440. |> move 0. 245.;
    rectangle wood 22. 470. |> move (-489.) 245.;
    rectangle wood 22. 470. |> move 489. 245.;
  ]
  @ List.map (fun (h, x) -> words white_ink h |> scale 1.5 |> move x 440.) headers
  (* the lines between the sections *)
  @ List.map (fun x -> rectangle (rgb 90 90 90) 2. 400. |> move x 240.) [ -375.; -60.; 70.; 390. ]
  @ List.filter_map
      (fun pl ->
        if pl.label = "" then None
        else
          let below = if Filename.check_suffix pl.name ".range" || Filename.check_suffix pl.name ".wave" then 30. else 38. in
          Some (text pl.label |> move pl.x (pl.y - below)))
      places
  @ [ text "FILTER" |> move 195. 410.; text "FILTER CONTOUR" |> move 195. 300.; text "LOUDNESS CONTOUR" |> move 195. 190. ]

(* the last 2048 samples: the oscilloscope (from a rising zero crossing,
 * so a steady note stands still) and the spectrum (20 Hz to 20 kHz on a
 * log axis, -80 to 0 dB) *)
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

let spectrum_view (samples : Signal.t) : shape list =
  let cx = 235. and cy = -55. and w = 440. and h = 120. in
  let mags = Spectrum.of_signal samples in
  let n = 2 *.. (Array.length mags -.. 1) in
  let bars = 60 in
  let freq b = 20. * (1000. ** (float_of_int b / float_of_int bars)) in
  let bar b =
    let lo = freq b and hi = freq (b +.. 1) in
    let top = ref 0. in
    Array.iteri (fun k v -> let f = Spectrum.bin_frequency ~n k in if f >= lo && f < hi && v > !top then top := v) mags;
    let db = if !top <= 0. then -80. else Float.max (-80.) (20. * log10 !top) in
    let bh = (db + 80.) / 80. * h in
    let bw = w / float_of_int bars in
    rectangle (rgb 250 190 80) (bw - 1.) (Float.max 1. bh) |> move (cx - (w / 2.) + ((float_of_int b + 0.5) * bw)) (cy - (h / 2.) + (bh / 2.))
  in
  (rectangle (rgb 25 20 10) w h |> move cx cy) :: List.init bars bar

(* the rack: its sections, black as the panel, between the same lines;
 * the rockers switch each stage on *)
let rack_view : shape list =
  [ rectangle (rgb 25 25 25) 960. 140. |> move 0. (-55.) ]
  @ List.map (fun (h, x) -> words white_ink h |> scale 1.3 |> move x 0.) rack_headers
  @ List.map (fun x -> rectangle (rgb 90 90 90) 2. 120. |> move x (-55.)) [ -258.; -80.; 170. ]
  @ List.filter_map
      (fun pl ->
        if pl.label = "" then None
        else
          let below = if Filename.check_suffix pl.name ".shape" || Filename.check_suffix pl.name ".kind" then 30. else 38. in
          Some (text pl.label |> move pl.x (pl.y - below)))
      rack_places

let keyboard_view (computer : computer) (m : model) : shape list =
  let letter_of s = List.find_map (fun (k, s') -> if s' = s then Some k else None) letters in
  let down s =
    let n = note m.octave s in
    m.mouse_note = Some n || match letter_of s with Some k -> Set_.mem k computer.keyboard.keys | None -> false
  in
  let key s =
    let black = is_black s in
    let w = if black then white_width * 0.6 else white_width - 3. in
    let h = if black then black_height else white_height in
    let color = if down s then rgb 250 190 80 else if black then rgb 20 20 20 else rgb 250 250 245 in
    let label = match letter_of s with Some k -> [ words (if black then white else rgb 120 120 120) k |> scale 1.6 |> move_y ((-.h / 2.) + 18.) ] | None -> [] in
    group (rectangle color w h :: label) |> move (key_x s) (keyboard_top - (h / 2.))
  in
  let keys = List.init keys_count (fun s -> s) in
  List.map key (List.filter (fun s -> not (is_black s)) keys)
  @ List.map key (List.filter is_black keys)
  @ [ words black (Printf.sprintf "C%d" m.octave) |> scale 1.4 |> move (keyboard_left + 20.) (keyboard_top + 14.) ]

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
  let samples = Minimoog_voice.recent voice in
  [ rectangle (rgb 215 205 190) computer.screen.width computer.screen.height ]
  @ [ words black "TinyMinimoog" |> scale 2.4 |> move (-360.) 482.; words black "preset" |> scale 1.5 |> move 230. 482. ]
  @ panel_view
  @ (if m.rack_shown then rack_view else scope_view samples @ spectrum_view samples)
  @ [ words (rgb 70 70 70) (status m) |> scale 1.4 |> move 0. (-137.) ]
  @ wheel_view pitch_wheel_x m.pitch_wheel "PITCH"
  @ wheel_view mod_wheel_x ((m.mod_wheel * 2.) - 1.) "MOD"
  @ keyboard_view computer m @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app app
