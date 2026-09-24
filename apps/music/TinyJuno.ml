(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of the Roland Juno-106 (1984), the polyphonic
 * synthesizer everyone could afford: six voices of one oscillator each,
 * made big by its chorus. The voice is Voice_juno.ml over Vco,
 * Moog_ladder and Envelope; this is its panel and a keyboard.
 *
 * The panel is the 106's: vertical sliders in its sections, left to
 * right -- LFO (rate, delay), DCO (the LFO's vibrato, the pulse's width,
 * the sub-oscillator, the noise), HPF (four positions, the slider
 * snapping to them), VCF (cutoff, resonance, the envelope, the LFO, the
 * keyboard's tracking), VCA (level), ENV (A, D, S, R) -- and under them
 * its buttons, each with its light: the range (16', 8', 4'), the pulse
 * and the sawtooth, the width by the LFO or by hand, the envelope's
 * polarity into the filter, the VCA by the envelope or a gate, the
 * chorus (off, I, II, and I+II). A slider follows the mouse while it is
 * held.
 *
 * The keys play with the mouse or the letters (a s d f g h j k the
 * white keys from C, w e t y u the black ones, z and x an octave down
 * and up), every key at full: the Juno-106 has no velocity (a cheap
 * keyboard was part of the price). Under the panel, the spectrum and the
 * scope, both sides of the chorus.
 *
 * Uses: Voice_juno (the voice), Vco, Moog_ladder, Envelope, Polyphony,
 * Audio's instruments, Gui (the buttons, the menu), Spectrum. Not: the
 * effects rack (the chorus is the Juno's own), Scene2d, Sprite,
 * File_menu.
 *
 * Exercises: the 106's 128 patches as banks (File_menu to save them,
 * its SysEx to read a real one's); the bender's lever (pitch and the
 * filter, the 106's); the hold button; the 60's arpeggiator.
 *)
open Playground
open Basics (* float arithmetics *)

(* the letters: each the semitone above the octave's C *)
let letters =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

type model = {
  patch : Voice_juno.patch;
  preset : int;
  octave : int;
  held : string list;
  mouse_note : int option;
  sliding : string option; (* the slider the mouse holds *)
}

let presets = Voice_juno.presets
let initial_model : model = { patch = snd (List.hd presets); preset = 0; octave = 4; held = []; mouse_note = None; sliding = None }

(* the synthesizer lives with the sound, not in the model: the mixer
 * pulls its blocks between frames (Instrument.mli) *)
let juno = Voice_juno.create initial_model.patch
let inst : Instrument.t = Voice_juno.instrument juno
let note (octave : int) (semitone : int) : int = (12 *.. (octave +.. 1)) +.. semitone

(*****************************************************************************)
(* The sliders *)
(*****************************************************************************)

(* a slider: its control's name, its x, its label; the tracks all
 * between [bottom] and [bottom + track] *)
type slider = { name : string; x : number; label : string }

let bottom = 210.
let track = 160.

let sections : (string * (string * string) list) list =
  [
    ("LFO", [ ("lfo.rate", "RATE"); ("lfo.delay", "DELAY") ]);
    ("DCO", [ ("dco.lfo", "LFO"); ("dco.pwm", "PWM"); ("dco.sub", "SUB"); ("dco.noise", "NOISE") ]);
    ("HPF", [ ("hpf", "FREQ") ]);
    ("VCF", [ ("vcf.cutoff", "FREQ"); ("vcf.resonance", "RES"); ("vcf.env", "ENV"); ("vcf.lfo", "LFO"); ("vcf.key", "KYBD") ]);
    ("VCA", [ ("vca.level", "LEVEL") ]);
    ("ENV", [ ("env.attack", "A"); ("env.decay", "D"); ("env.sustain", "S"); ("env.release", "R") ]);
  ]

let step = 44.
let gap = 24.

(* the sliders' x, section after section, a gap between them *)
let sliders : slider list =
  let x = ref (-440.) in
  List.concat_map
    (fun (_, controls) ->
      let s = List.map (fun (name, label) -> let sl = { name; x = !x; label } in x := !x + step; sl) controls in
      x := !x + gap;
      s)
    sections

let knob (name : string) : Voice_juno.knob = List.find (fun (k : Voice_juno.knob) -> k.name = name) Voice_juno.knobs

(* a control's value as a slider's position, 0 to 1, and back (a
 * selector snapping to its positions) *)
let position (p : Voice_juno.patch) (name : string) : number =
  let k = knob name in
  match k.control with
  | Selector labels -> k.get p / float_of_int (List.length labels -.. 1)
  | _ -> k.get p

let set_position (p : Voice_juno.patch) (name : string) (pos : number) : Voice_juno.patch =
  let k = knob name in
  let pos = Float.max 0. (Float.min 1. pos) in
  match k.control with
  | Selector labels -> k.put p (Float.round (pos * float_of_int (List.length labels -.. 1)))
  | _ -> k.put p pos

let slider_at (x : number) (y : number) : string option =
  List.find_map (fun s -> if Float.abs (x - s.x) <= 14. && y >= bottom - 12. && y <= bottom + track + 12. then Some s.name else None) sliders

(* the buttons: a label, where, whether lit, and what a click does *)
type button = { text : string; bx : number; by : number; lit : Voice_juno.patch -> bool; click : Voice_juno.patch -> Voice_juno.patch }

let x_of name = (List.find (fun s -> s.name = name) sliders).x

let buttons : button list =
  let b text bx by lit click = { text; bx; by; lit; click } in
  let row = 150. in
  List.mapi (fun i r -> b r (x_of "dco.lfo" + (float_of_int i * 40.)) row (fun p -> p.range = i) (fun p -> { p with range = i })) Voice_juno.ranges
  @ [
      b "PULSE" (x_of "dco.lfo" + 10.) 95. (fun p -> p.pulse) (fun p -> { p with pulse = not p.pulse });
      b "SAW" (x_of "dco.lfo" + 80.) 95. (fun p -> p.saw) (fun p -> { p with saw = not p.saw });
      b "PWM LFO" (x_of "dco.sub" + 40.) 150. (fun p -> p.pwm_lfo) (fun p -> { p with pwm_lfo = not p.pwm_lfo });
      b "ENV -" (x_of "vcf.env") 150. (fun p -> p.env_invert) (fun p -> { p with env_invert = not p.env_invert });
      b "GATE" (x_of "vca.level") 150. (fun p -> p.gate) (fun p -> { p with gate = not p.gate });
    ]
  @ List.mapi
      (fun i c -> b c (x_of "env.attack" - 10. + (float_of_int i * 44.)) 95. (fun p -> p.chorus = i) (fun p -> { p with chorus = i }))
      Voice_juno.choruses

(*****************************************************************************)
(* The keyboard *)
(*****************************************************************************)

let keys_count = 25 (* two octaves and a C *)
let is_black (s : int) : bool = List.mem (s mod 12) [ 1; 3; 6; 8; 10 ]
let white_width = 56.
let keyboard_left = -420.
let keyboard_top = -175.
let white_height = 270.
let black_height = 165.
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

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "juno" (fun () -> inst));
  let preset = Gui.menu computer ~at:(330., 482.) (List.map fst presets) m.preset in
  let patch = if preset <> m.preset then snd (List.nth presets preset) else m.patch in
  let mouse = computer.mouse in
  (* a slider held follows the mouse until let go *)
  let sliding =
    if not mouse.mdown then None else match m.sliding with Some s -> Some s | None -> if m.mouse_note = None then slider_at mouse.mx mouse.my else None
  in
  let patch = match sliding with Some name -> set_position patch name ((mouse.my - bottom) / track) | None -> patch in
  (* the buttons, on a click *)
  let patch =
    if mouse.mclick then
      List.fold_left
        (fun p b -> if Float.abs (mouse.mx - b.bx) <= 20. && Float.abs (mouse.my - b.by) <= 12. then b.click p else p)
        patch buttons
    else patch
  in
  (* the letters, several at once; every key at full *)
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) and released k = List.mem k m.held && not (List.mem k now) in
  let octave = if pressed "z" then max 1 (m.octave -.. 1) else if pressed "x" then min 6 (m.octave +.. 1) else m.octave in
  List.iter
    (fun (k, semitone) ->
      let n = note m.octave semitone in
      if pressed k then inst.note_on n 1.;
      if released k then inst.note_off n)
    letters;
  let under = if mouse.mdown && sliding = None then Option.map (note m.octave) (key_at mouse.mx mouse.my) else None in
  if under <> m.mouse_note then begin
    Option.iter inst.note_off m.mouse_note;
    Option.iter (fun n -> inst.note_on n 1.) under
  end;
  Voice_juno.set_patch juno patch;
  { patch; preset; octave; held = now; mouse_note = under; sliding }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 230 230 230
let orange = rgb 235 120 50
let green = rgb 120 220 160

(* a line from one point to another, [w] wide *)
let segment (c : color) (w : number) (x1, y1) (x2, y2) : shape =
  let dx = x2 - x1 and dy = y2 - y1 in
  rectangle c (sqrt ((dx * dx) + (dy * dy))) w |> rotate (atan2 dy dx * 180. / Float.pi) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

let slider_view (p : Voice_juno.patch) (s : slider) : shape list =
  let y = bottom + (position p s.name * track) in
  [
    rectangle (rgb 10 10 10) 6. track |> move s.x (bottom + (track / 2.));
    rectangle (rgb 240 240 240) 26. 12. |> move s.x y;
    rectangle (rgb 30 30 30) 26. 2. |> move s.x y;
    words ink s.label |> scale 0.9 |> move s.x (bottom - 22.);
  ]

let button_view (p : Voice_juno.patch) (b : button) : shape list =
  [
    circle (if b.lit p then rgb 255 60 40 else rgb 70 20 15) 4. |> move b.bx (b.by + 18.);
    rectangle (rgb 90 90 95) 38. 20. |> move b.bx b.by;
    words ink b.text |> scale 0.8 |> move b.bx b.by;
  ]

(* the 106's front: black, its sections named in orange over lines *)
let panel_view (m : model) : shape list =
  let titles =
    List.map
      (fun (title, controls) ->
        let xs = List.map (fun (name, _) -> x_of name) controls in
        let lo = List.fold_left Float.min 1e9 xs and hi = List.fold_left Float.max (-1e9) xs in
        group [ segment orange 2. (lo - 16., 400.) (hi + 16., 400.); words orange title |> scale 1.2 |> move ((lo + hi) / 2.) 414. ])
      sections
  in
  [ rectangle (rgb 30 30 32) 960. 420. |> move 0. 250.; rectangle (rgb 150 150 155) 960. 16. |> move 0. 452. ]
  @ [ words (rgb 30 30 30) "Roland  JUNO-106" |> scale 1.3 |> move 330. 452.; words orange "CHORUS" |> scale 1.1 |> move (x_of "env.attack" + 56.) 125. ]
  @ titles
  @ List.concat_map (slider_view m.patch) sliders
  @ List.concat_map (button_view m.patch) buttons

let spectrum_view (samples : Signal.t) : shape list =
  let cx = -160. and cy = -70. and w = 620. and h = 100. in
  let mags = Spectrum.of_signal samples in
  let n = 2 *.. (Array.length mags -.. 1) in
  let bars = 90 in
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
    rectangle green (bw - 1.) (Float.max 1. bh) |> move (cx - (w / 2.) + ((float_of_int b + 0.5) * bw)) (cy - (h / 2.) + (bh / 2.))
  in
  (rectangle (rgb 20 25 20) w h |> move cx cy) :: List.init bars bar

let scope_view (samples : Signal.t) : shape list =
  let cx = 330. and cy = -70. and w = 300. and h = 100. in
  let points = 150 in
  let at i = samples.(Array.length samples -.. 1024 +.. (i *.. 1024 /.. points)) in
  (rectangle (rgb 20 25 20) w h |> move cx cy)
  :: List.init (points -.. 1) (fun i ->
         let x i = cx - (w / 2.) + (float_of_int i / float_of_int points * w) in
         let y i = cy + (Float.max (-1.) (Float.min 1. (at i * 3.)) * h / 2.) in
         segment green 2. (x i, y i) (x (i +.. 1), y (i +.. 1)))

let keyboard_view (computer : computer) (m : model) : shape list =
  let letter_of s = List.find_map (fun (k, s') -> if s' = s then Some k else None) letters in
  let down s =
    m.mouse_note = Some (note m.octave s) || match letter_of s with Some k -> Set_.mem k computer.keyboard.keys | None -> false
  in
  let key s =
    let black = is_black s in
    let w = if black then white_width * 0.6 else white_width - 3. in
    let h = if black then black_height else white_height in
    let color = if down s then orange else if black then rgb 20 20 20 else rgb 250 250 245 in
    let label = match letter_of s with Some k -> [ words (if black then white else rgb 120 120 120) k |> scale 1.6 |> move_y ((-.h / 2.) + 18.) ] | None -> [] in
    group (rectangle color w h :: label) |> move (key_x s) (keyboard_top - (h / 2.))
  in
  let keys = List.init keys_count (fun s -> s) in
  List.map key (List.filter (fun s -> not (is_black s)) keys)
  @ List.map key (List.filter is_black keys)
  @ [ words black (Printf.sprintf "C%d" m.octave) |> scale 1.4 |> move (keyboard_left + 20.) (keyboard_top + 14.) ]

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 200 195 185) computer.screen.width computer.screen.height ]
  @ [ words black "TinyJuno" |> scale 2.4 |> move (-370.) 482.; words black "preset" |> scale 1.5 |> move 230. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-110.) 482. ]
  @ panel_view m
  @ spectrum_view (Voice_juno.recent juno)
  @ scope_view (Voice_juno.recent juno)
  @ [
      words (rgb 70 70 70) (Printf.sprintf "voices %d of 6   every key at full: the 106 has no velocity" (Voice_juno.voices juno))
      |> scale 1.3 |> move 0. (-137.);
    ]
  @ keyboard_view computer m @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app app
