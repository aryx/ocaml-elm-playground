(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of the Yamaha CS-80 (1977), Vangelis's synthesizer:
 * eight voices of two synthesizers each, a keyboard that feels how
 * hard each key is pressed, and a ribbon. The voice is Voice_cs80.ml
 * over Vco, Svf, Envelope and Lfo; this is its panel and a keyboard.
 *
 * The panel: section I's two rows of knobs, then section II's (the
 * sound -- feet, sawtooth, pulse, its width and modulation, noise, the
 * high-pass and the low-pass with their resonance, the pure sine; then
 * the filter envelope's IL, AL and times, the amplifier's ADSR, the
 * level, the touch: velocity and pressure into brilliance and level),
 * then the controls both share (the mix of I and II, II's detune, the
 * sub-oscillator, the ring modulator, the chorus and tremolo, the
 * volume).
 *
 * The touch is the lesson, and the mouse plays it: pressed on a key, the
 * velocity is where (soft at its back, hard at its front, as in
 * TinyRhodes); dragged down while held, the key is pressed harder, its
 * pressure a bar on it -- that note's filter opening, and no other's:
 * hold a chord on the letters (a s d f g h j k the white keys from C,
 * w e t y u the black ones, z and x an octave down and up), press one of
 * its notes with the mouse, and only that one swells. The ribbon above
 * the keys bends every held note from where it is first touched, an
 * octave across its width, back when let go.
 *
 * Uses: Voice_cs80 (the voice), Vco, Svf, Envelope, Lfo,
 * Modulated_delay, Polyphony, Audio's instruments, Gui (the knobs, the
 * selectors, the switches, the menu), Spectrum. Not: the effects rack,
 * Scene2d, Sprite, File_menu.
 *
 * Exercises: the letters' pressure (a key held longer pressing harder,
 * say); the initial touch's pitch bend (a slide from a semitone below,
 * the CS-80's); portamento; the four memories (the CS-80's panel stored
 * in them, File_menu to save them); a MIDI keyboard's polyphonic
 * pressure, the real thing.
 *)
open Playground
open Basics (* float arithmetics *)

(* the letters: each the semitone above the octave's C *)
let letters =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

type model = {
  patch : Voice_cs80.patch;
  preset : int;
  octave : int;
  held : string list;
  mouse_note : int option;
  pressed_at : number; (* where the mouse went down on its key *)
  pressure : number; (* the mouse's key's pressure *)
  ribbon_from : number option; (* where the ribbon was first touched *)
  bend : number; (* semitones *)
}

let presets = Voice_cs80.presets

let initial_model : model =
  { patch = snd (List.hd presets); preset = 0; octave = 4; held = []; mouse_note = None; pressed_at = 0.; pressure = 0.; ribbon_from = None; bend = 0. }

(* the synthesizer lives with the sound, not in the model: the mixer
 * pulls its blocks between frames (Instrument.mli) *)
let cs80 = Voice_cs80.create initial_model.patch
let inst : Instrument.t = Voice_cs80.instrument cs80
let note (octave : int) (semitone : int) : int = (12 *.. (octave +.. 1)) +.. semitone

(*****************************************************************************)
(* The panel's knobs *)
(*****************************************************************************)

let panel_theme : Theme.t =
  {
    Theme.default with
    text = rgb 225 225 225;
    accent = rgb 210 60 50;
    edge = rgb 110 110 110;
    face = rgb 50 50 55;
    face_hot = rgb 70 70 75;
    face_down = rgb 35 35 40;
    text_size = 12.;
    dial = 26.;
    dial_face = rgb 20 20 22;
    pointer = rgb 240 240 240;
  }

(* a control of Voice_cs80.knobs, where it sits, the word under it *)
type place = { name : string; x : number; y : number; label : string }

let row (y : number) (prefix : string) (controls : (string * string) list) : place list =
  let n = List.length controls in
  let step = 900. / float_of_int n in
  List.mapi (fun i (name, label) -> { name = prefix ^ name; x = -450. + (step * (float_of_int i + 0.5)); y; label }) controls

let sound =
  [
    ("feet", "FEET");
    ("saw", "SAW");
    ("pulse", "PULSE");
    ("width", "WIDTH");
    ("pwm", "PWM");
    ("pwm_speed", "SPEED");
    ("noise", "NOISE");
    ("hpf", "HPF");
    ("hpf_res", "RES");
    ("lpf", "LPF");
    ("lpf_res", "RES");
    ("sine", "SINE");
  ]

let shape =
  [
    ("il", "IL");
    ("al", "AL");
    ("f_attack", "F.ATK");
    ("f_decay", "F.DEC");
    ("f_release", "F.REL");
    ("attack", "ATK");
    ("decay", "DEC");
    ("sustain", "SUS");
    ("release", "REL");
    ("level", "LEVEL");
    ("initial.brilliance", "V.BRIL");
    ("initial.level", "V.LVL");
    ("after.brilliance", "P.BRIL");
    ("after.level", "P.LVL");
  ]

let shared =
  [
    ("mix", "MIX I/II");
    ("detune", "DETUNE");
    ("sub.wave", "SUB");
    ("sub.speed", "SPEED");
    ("sub.vco", "VCO");
    ("sub.vcf", "VCF");
    ("sub.vca", "VCA");
    ("ring.speed", "RING");
    ("ring.depth", "DEPTH");
    ("ring.attack", "ATK");
    ("ring.decay", "DEC");
    ("chorus", "CHORUS");
    ("tremolo", "TREM");
    ("volume", "VOLUME");
  ]

let places = row 400. "I." sound @ row 335. "I." shape @ row 255. "II." sound @ row 190. "II." shape @ row 110. "" shared

let control (computer : computer) (p : Voice_cs80.patch) (pl : place) : Voice_cs80.patch =
  match List.find_opt (fun (k : Voice_cs80.knob) -> k.name = pl.name) Voice_cs80.knobs with
  | None -> p
  | Some k ->
      let v = k.get p in
      let v' =
        match k.control with
        | Knob (from, to_) -> Gui.knob computer ~at:(pl.x, pl.y) ~from ~to_ v
        | Switch -> if Gui.rocker computer ~at:(pl.x, pl.y) (v >= 0.5) then 1. else 0.
        | Selector labels -> float_of_int (Gui.selector computer ~at:(pl.x, pl.y) labels (int_of_float v))
      in
      if v' <> v then k.put p v' else p

(*****************************************************************************)
(* The ribbon and the keyboard *)
(*****************************************************************************)

let ribbon_y = 25.
let ribbon_width = 900.
let on_ribbon (x : number) (y : number) : bool = Float.abs (y - ribbon_y) <= 14. && Float.abs x <= ribbon_width / 2.

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

(* the key under the mouse (a black key first, it's on top), and the
 * velocity: how far down the key, 0.2 at its back to 1 at its front *)
let key_at (x : number) (y : number) : (int * number) option =
  let keys = List.init keys_count (fun s -> s) in
  let height s = if is_black s then black_height else white_height in
  let hit s =
    let w = if is_black s then white_width * 0.6 else white_width in
    Float.abs (x - key_x s) <= w / 2. && y <= keyboard_top && y >= keyboard_top - height s
  in
  let found = match List.find_opt (fun s -> is_black s && hit s) keys with Some s -> Some s | None -> List.find_opt hit keys in
  Option.map (fun s -> (s, 0.2 + (0.8 * (keyboard_top - y) / height s))) found

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "cs80" (fun () -> inst));
  Gui.set_theme Theme.default;
  let preset = Gui.menu computer ~at:(330., 482.) (List.map fst presets) m.preset in
  let patch = if preset <> m.preset then snd (List.nth presets preset) else m.patch in
  Gui.set_theme panel_theme;
  let patch = List.fold_left (control computer) patch places in
  (* the letters, several at once *)
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) and released k = List.mem k m.held && not (List.mem k now) in
  let octave = if pressed "z" then max 1 (m.octave -.. 1) else if pressed "x" then min 6 (m.octave +.. 1) else m.octave in
  List.iter
    (fun (k, semitone) ->
      let n = note m.octave semitone in
      if pressed k then inst.note_on n 0.7;
      if released k then inst.note_off n)
    letters;
  let mouse = computer.mouse in
  (* the ribbon: from where it's first touched, an octave across it *)
  let ribbon_from =
    match m.ribbon_from with
    | Some x when mouse.mdown -> Some x
    | _ -> if mouse.mdown && m.mouse_note = None && on_ribbon mouse.mx mouse.my then Some mouse.mx else None
  in
  let bend = match ribbon_from with Some x -> 12. * (mouse.mx - x) / ribbon_width | None -> 0. in
  if bend <> m.bend then Voice_cs80.bend cs80 bend;
  (* the mouse on a key: the velocity where it went down, the pressure
   * how far it's dragged down since, the same key kept while held *)
  let under = if mouse.mdown && ribbon_from = None then key_at mouse.mx mouse.my else None in
  let under_note = Option.map (fun (s, _) -> note m.octave s) under in
  let kept = mouse.mdown && m.mouse_note <> None && (under_note = m.mouse_note || under_note = None) in
  (* a key held on the letters is pressed harder by the mouse, not
   * struck again: a finger pushing on a key already down *)
  let by_letter n = List.exists (fun (k, s) -> List.mem k now && note m.octave s = n) letters in
  let mouse_note, pressed_at, pressure =
    if kept then (m.mouse_note, m.pressed_at, Float.max 0. (Float.min 1. ((m.pressed_at - mouse.my) / 120.)))
    else begin
      Option.iter (fun n -> if by_letter n then Voice_cs80.pressure cs80 n 0. else inst.note_off n) m.mouse_note;
      Option.iter (fun (s, velocity) -> if not (by_letter (note m.octave s)) then inst.note_on (note m.octave s) velocity) under;
      (under_note, mouse.my, 0.)
    end
  in
  Option.iter (fun n -> Voice_cs80.pressure cs80 n pressure) mouse_note;
  Voice_cs80.set_patch cs80 patch;
  { patch; preset; octave; held = now; mouse_note; pressed_at; pressure; ribbon_from; bend }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 225 225 225
let green = rgb 120 220 160

(* a line from one point to another, [w] wide *)
let segment (c : color) (w : number) (x1, y1) (x2, y2) : shape =
  let dx = x2 - x1 and dy = y2 - y1 in
  rectangle c (sqrt ((dx * dx) + (dy * dy))) w |> rotate (atan2 dy dx * 180. / Float.pi) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

(* the CS-80's black front, a stripe per section, the ribbon *)
let panel_view (m : model) : shape list =
  let stripe y label = [ rectangle (rgb 45 45 50) 950. 120. |> move 0. y; words (rgb 210 60 50) label |> scale 1.2 |> move (-465.) (y + 48.) ] in
  [ rectangle (rgb 25 25 28) 960. 460. |> move 0. 220.; rectangle (rgb 150 110 70) 960. 12. |> move 0. 446. ]
  @ stripe 368. "I" @ stripe 223. "II"
  @ [ words (rgb 200 200 200) "YAMAHA  CS-80" |> scale 1.2 |> move 380. 436. ]
  @ List.map (fun pl -> words ink pl.label |> scale 0.9 |> move pl.x (pl.y - 26.)) places
  @ [
      rectangle (rgb 60 60 65) ribbon_width 22. |> move 0. ribbon_y;
      (match m.ribbon_from with Some x -> circle (rgb 230 120 60) 9. |> move x ribbon_y | None -> group []);
      words (rgb 70 70 70) (Printf.sprintf "RIBBON   %+.1f semitones" m.bend) |> scale 1.1 |> move 0. (ribbon_y + 22.);
    ]

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

(* the keys, the mouse's one with its pressure as a bar *)
let keyboard_view (computer : computer) (m : model) : shape list =
  let letter_of s = List.find_map (fun (k, s') -> if s' = s then Some k else None) letters in
  let down s =
    m.mouse_note = Some (note m.octave s) || match letter_of s with Some k -> Set_.mem k computer.keyboard.keys | None -> false
  in
  let key s =
    let black = is_black s in
    let w = if black then white_width * 0.6 else white_width - 3. in
    let h = if black then black_height else white_height in
    let color = if down s then rgb 230 120 60 else if black then rgb 20 20 20 else rgb 250 250 245 in
    let label = match letter_of s with Some k -> [ words (if black then white else rgb 120 120 120) k |> scale 1.6 |> move_y ((-.h / 2.) + 18.) ] | None -> [] in
    let bar =
      if m.mouse_note = Some (note m.octave s) && m.pressure > 0. then
        [ rectangle (rgb 210 60 50) (w * 0.5) (h * 0.8 * m.pressure) |> move_y ((h / 2.) - (h * 0.1) - (h * 0.4 * m.pressure)) ]
      else []
    in
    group ((rectangle color w h :: label) @ bar) |> move (key_x s) (keyboard_top - (h / 2.))
  in
  let keys = List.init keys_count (fun s -> s) in
  List.map key (List.filter (fun s -> not (is_black s)) keys)
  @ List.map key (List.filter is_black keys)
  @ [ words black (Printf.sprintf "C%d" m.octave) |> scale 1.4 |> move (keyboard_left + 20.) (keyboard_top + 14.) ]

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 200 195 185) computer.screen.width computer.screen.height ]
  @ [ words black "TinyCS80" |> scale 2.4 |> move (-370.) 482.; words black "preset" |> scale 1.5 |> move 230. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-110.) 482. ]
  @ panel_view m
  @ spectrum_view (Voice_cs80.recent cs80)
  @ scope_view (Voice_cs80.recent cs80)
  @ [
      words (rgb 70 70 70)
        (Printf.sprintf "voices %d   a key: pressed where, the velocity; dragged down, the pressure   pressure %.2f" (Voice_cs80.voices cs80) m.pressure)
      |> scale 1.2 |> move 0. (-137.);
    ]
  @ keyboard_view computer m @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app app
