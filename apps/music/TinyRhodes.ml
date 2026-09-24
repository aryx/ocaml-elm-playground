(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of the Fender Rhodes (Mark I Stage 73, 1970) and its
 * Suitcase amplifier, with the Wurlitzer 200A (1974) and the Hohner
 * Clavinet D6 (1971) behind a switch: the electric pianos of soul,
 * jazz-funk and every ballad. The voice is Voice_rhodes.ml over Modal
 * and Pluck; this is its panel and a keyboard.
 *
 * The knobs: the model; the voicing (how far the tine sits off its
 * pickup's centre, a screwdriver's job on the real one; the Wurlitzer's
 * reed nearer its plate); the hammer's hardness; the decay; the
 * Suitcase's vibrato, rate and depth; the volume. Beside them what
 * can't be seen on the real one: the pickup's curve, the flux against
 * the tip's position (the Rhodes' bell, the Wurlitzer's capacitance),
 * and on it the span the last note's tip swings across now -- press a
 * key softly, a short span on the bell's side, nearly a sine; hard, the
 * span across its top: the bark. The Suitcase's two speakers light as
 * the tremolo moves the sound between them.
 *
 * The keys play with the mouse or the letters (a s d f g h j k the
 * white keys from C, w e t y u the black ones, z and x an octave down
 * and up); with the mouse, the velocity is where the key is pressed,
 * soft at its back, hard at its front. Under the panel, the spectrum
 * and the scope.
 *
 * Uses: Voice_rhodes (the voice), Modal, Pluck, Polyphony, Audio's
 * instruments, Gui (the knobs, the selector, the menu), Spectrum. Not:
 * the effects rack (the Reface CP's row of effects: an exercise),
 * Scene2d, Sprite, File_menu.
 *
 * Exercises: the effects row after it, as the Reface CP has (drive,
 * the wah, the phaser, the delay: the rack's); the tine's arc and its
 * two polarisations (Pfeifle's model); a sustain pedal (the dampers
 * held off); the 88-key Rhodes' range.
 *)
open Playground
open Basics (* float arithmetics *)

(* the letters: each the semitone above the octave's C *)
let letters =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

type model = {
  patch : Voice_rhodes.patch;
  preset : int;
  octave : int;
  held : string list;
  mouse_note : int option;
}

let presets = Voice_rhodes.presets
let initial_model : model = { patch = snd (List.hd presets); preset = 0; octave = 4; held = []; mouse_note = None }

(* the piano lives with the sound, not in the model: the mixer pulls
 * its blocks between frames (Instrument.mli) *)
let piano = Voice_rhodes.create initial_model.patch
let inst : Instrument.t = Voice_rhodes.instrument piano
let note (octave : int) (semitone : int) : int = (12 *.. (octave +.. 1)) +.. semitone

(*****************************************************************************)
(* The panel's knobs *)
(*****************************************************************************)

let panel_theme : Theme.t =
  {
    Theme.default with
    text = rgb 230 230 230;
    accent = rgb 200 60 50;
    edge = rgb 120 120 120;
    face = rgb 55 55 55;
    face_hot = rgb 75 75 75;
    face_down = rgb 40 40 40;
    text_size = 15.;
    dial = 44.;
    dial_face = rgb 25 25 25;
    pointer = rgb 235 235 235;
  }

(* a control of Voice_rhodes.knobs, where it sits, the word under it *)
type place = { name : string; x : number; y : number; label : string }

let place name x y label = { name; x; y; label }

let places =
  [
    place "model" (-380.) 340. "";
    place "voicing" (-230.) 380. "VOICING";
    place "hardness" (-140.) 380. "HAMMER";
    place "decay" (-50.) 380. "DECAY";
    place "tremolo.rate" (110.) 380. "RATE";
    place "tremolo.depth" (200.) 380. "DEPTH";
    place "volume" (360.) 380. "VOLUME";
  ]

let control (computer : computer) (p : Voice_rhodes.patch) (pl : place) : Voice_rhodes.patch =
  match List.find_opt (fun (k : Voice_rhodes.knob) -> k.name = pl.name) Voice_rhodes.knobs with
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
  ignore (Audio.instrument "rhodes" (fun () -> inst));
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
      if pressed k then inst.note_on n 0.8;
      if released k then inst.note_off n)
    letters;
  (* the mouse on the keyboard: the velocity where the key is pressed *)
  let mouse = computer.mouse in
  let under = if mouse.mdown then key_at mouse.mx mouse.my else None in
  let under_note = Option.map (fun (s, _) -> note m.octave s) under in
  if under_note <> m.mouse_note then begin
    Option.iter inst.note_off m.mouse_note;
    Option.iter (fun (s, velocity) -> inst.note_on (note m.octave s) velocity) under
  end;
  Voice_rhodes.set_patch piano patch;
  { patch; preset; octave; held = now; mouse_note = under_note }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 230 230 230
let green = rgb 120 220 160

(* a line from one point to another, [w] wide *)
let segment (c : color) (w : number) (x1, y1) (x2, y2) : shape =
  let dx = x2 - x1 and dy = y2 - y1 in
  rectangle c (sqrt ((dx * dx) + (dy * dy))) w |> rotate (atan2 dy dx * 180. / Float.pi) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

(* the Stage 73's top: black, its silver strip and nameplate *)
let panel_view (_m : model) : shape list =
  [
    rectangle (rgb 25 25 25) 960. 460. |> move 0. 200.;
    rectangle (rgb 180 180 185) 960. 20. |> move 0. 420.;
    words (rgb 30 30 30) "Rhodes  MARK I  STAGE PIANO" |> scale 1.3 |> move (-250.) 420.;
    words (rgb 200 60 50) "SUITCASE VIBRATO" |> scale 1.1 |> move 155. 335.;
  ]
  @ List.filter_map (fun pl -> if pl.label = "" then None else Some (words ink pl.label |> scale 1.1 |> move pl.x (pl.y - 34.))) places

(* the pickup's curve against the tip's position, and on it the span
 * the last note's tip went over: where the sound comes from *)
let pickup_view (m : model) : shape list =
  let cx = -200. and cy = 170. and w = 460. and h = 200. in
  let p = m.patch in
  let frame = [ rectangle (rgb 15 20 18) w h |> move cx cy ] in
  if p.model = 2 then frame @ [ words ink "the Clavinet: a string, its pickups under it" |> scale 1.2 |> move cx cy ]
  else begin
    let wurlitzer = p.model = 1 in
    (* the curve over the tip's positions shown, and its range *)
    let lo, hi = if wurlitzer then (-1., 0.95) else (-3., 3.) in
    let curve x = if wurlitzer then Voice_rhodes.capacitance x else Voice_rhodes.pickup ~voicing:p.voicing x in
    let top = if wurlitzer then curve hi else 1. and bottom = if wurlitzer then curve lo else 0. in
    let px x = cx - (w / 2.) + 20. + ((x - lo) / (hi - lo) * (w - 40.)) in
    let py v = cy - (h / 2.) + 25. + ((v - bottom) / (top - bottom) * (h - 55.)) in
    let steps = 80 in
    let xs = List.init (steps +.. 1) (fun i -> lo + ((hi - lo) * float_of_int i / float_of_int steps)) in
    let rec lines c width = function
      | a :: (b :: _ as rest) -> segment c width (px a, py (curve a)) (px b, py (curve b)) :: lines c width rest
      | _ -> []
    in
    let slo, shi = Voice_rhodes.span piano in
    let slo = Float.max lo slo and shi = Float.min hi shi in
    let swept = List.filter (fun x -> x >= slo && x <= shi) xs in
    let axis = segment (rgb 90 90 90) 2. (px lo, py bottom - 12.) (px hi, py bottom - 12.) in
    let span_bar = if shi > slo then [ segment (rgb 230 120 60) 6. (px slo, py bottom - 12.) (px shi, py bottom - 12.) ] else [] in
    frame
    @ [ words ink (if wurlitzer then "THE REED'S CAPACITOR: 1 / (1 - x)" else "THE PICKUP: FLUX AGAINST THE TIP") |> scale 1.1 |> move cx (cy + (h / 2.) - 14.) ]
    @ lines (rgb 120 150 130) 2. xs
    @ lines (rgb 230 120 60) 4. swept
    @ (axis :: span_bar)
    @ [ words (rgb 160 160 160) "the tip's swing now" |> scale 1. |> move cx (py bottom - 28.) ]
  end

(* the Suitcase's two speakers, each as bright as its side is loud *)
let speakers_view (m : model) : shape list =
  let s = Voice_rhodes.pan piano and d = m.patch.tremolo_depth in
  let left, right = if m.patch.model = 0 then (1. - (d * (1. + s) / 2.), 1. - (d * (1. - s) / 2.)) else (1. - (d * (1. + s) / 2.), 1. - (d * (1. + s) / 2.)) in
  let speaker x level =
    let c = int_of_float (40. + (180. * level)) in
    group [ circle (rgb 50 50 50) 62.; circle (rgb c (c /.. 2) (c /.. 3)) 50.; circle (rgb 20 20 20) 14. ] |> move x 170.
  in
  [ rectangle (rgb 40 32 28) 330. 180. |> move 250. 170.; speaker 170. left; speaker 330. right ]

let spectrum_view (samples : Signal.t) : shape list =
  let cx = -160. and cy = -95. and w = 620. and h = 110. in
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
  let cx = 330. and cy = -95. and w = 300. and h = 110. in
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
    let color = if down s then rgb 230 120 60 else if black then rgb 20 20 20 else rgb 250 250 245 in
    let label = match letter_of s with Some k -> [ words (if black then white else rgb 120 120 120) k |> scale 1.6 |> move_y ((-.h / 2.) + 18.) ] | None -> [] in
    group (rectangle color w h :: label) |> move (key_x s) (keyboard_top - (h / 2.))
  in
  let keys = List.init keys_count (fun s -> s) in
  List.map key (List.filter (fun s -> not (is_black s)) keys)
  @ List.map key (List.filter is_black keys)
  @ [ words black (Printf.sprintf "C%d" m.octave) |> scale 1.4 |> move (keyboard_left + 20.) (keyboard_top + 14.) ]

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 200 195 185) computer.screen.width computer.screen.height ]
  @ [ words black "TinyRhodes" |> scale 2.4 |> move (-360.) 482.; words black "preset" |> scale 1.5 |> move 230. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-110.) 482. ]
  @ panel_view m @ pickup_view m @ speakers_view m
  @ spectrum_view (Voice_rhodes.recent piano)
  @ scope_view (Voice_rhodes.recent piano)
  @ [
      words (rgb 70 70 70)
        (Printf.sprintf "voices %d   the mouse: soft at a key's back, hard at its front" (Voice_rhodes.voices piano))
      |> scale 1.3 |> move 0. (-162.);
    ]
  @ keyboard_view computer m @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app app
