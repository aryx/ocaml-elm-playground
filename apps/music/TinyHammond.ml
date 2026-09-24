(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of the Hammond B-3 (Hammond Organ Company, 1955) and
 * its Leslie 122 (1965), the jazz, gospel and rock organ: nine
 * drawbars on a keyboard, sines added (additive synthesis), a
 * percussion on the attack, a vibrato, and a speaker cabinet whose horn
 * and drum turn. The voice is Hammond_voice.ml over Tonewheel.ml, the
 * cabinet Leslie.mli; this is their panel and a keyboard.
 *
 * The drawbars pull down with the mouse, 0 (in) to 8 (all the way
 * out), their colours the B-3's: brown the two below the note (16', 5
 * 1/3'), white the octaves (8', 4', 2', 1'), black the others; the
 * registration they make is written as its nine digits. The percussion
 * tabs, the Leslie's rockers (on, fast) and the vibrato's rotary switch
 * click; the click and the volume knobs turn by dragging. The keys play
 * with the mouse, or with the letters, several at once (a chord is the
 * point of an organ): a s d f g h j k the white keys from C, w e t y u
 * the black ones, z and x an octave down and up; space flips the
 * Leslie between slow and fast, the way an organist's foot does. Under
 * the panel, the spectrum (each drawbar a line, the percussion a
 * flash, the Leslie's wobble) and the cabinet, its horn and drum drawn
 * turning at their speeds.
 *
 * Uses: Hammond_voice (the voice), Tonewheel, Leslie, Polyphony (a
 * voice per key), Audio's instruments (the organ played live), Gui
 * (the rockers, the selector, the knobs), Spectrum (the display). Not:
 * the effects rack, Scene2d, Sprite, File_menu.
 *
 * Exercises: the lower manual and the pedals (the B-3's second
 * keyboard, its own drawbars, and 25 pedals of 16' and 8'); the
 * drawbars heard while a note sounds (here from the next note); the
 * Leslie's brake (stopped rotors, a still sound between the speeds);
 * saving registrations with the File menu (appkits/file_menu).
 *)
open Playground
open Basics (* float arithmetics *)

(* the letters: each the semitone above the octave's C *)
let letters =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

type model = {
  patch : Hammond_voice.patch;
  preset : int; (* an index in Hammond_voice.presets *)
  octave : int; (* the drawn keyboard's lowest C, and the letter a's *)
  held : string list; (* the letters held at the last frame *)
  mouse_note : int option; (* the key the mouse holds down *)
  pulling : int option; (* the drawbar the mouse holds *)
  horn_angle : number; (* the Leslie's rotors, turns, drawn *)
  drum_angle : number;
  space : bool; (* space held at the last frame *)
}

let presets = Hammond_voice.presets

let initial_model : model =
  {
    patch = snd (List.hd presets);
    preset = 0;
    octave = 4;
    held = [];
    mouse_note = None;
    pulling = None;
    horn_angle = 0.;
    drum_angle = 0.;
    space = false;
  }

(* the organ lives with the sound, not in the model: the mixer pulls its
 * blocks between frames (Instrument.mli) *)
let organ = Hammond_voice.create initial_model.patch
let inst : Instrument.t = Hammond_voice.instrument organ
let note (octave : int) (semitone : int) : int = (12 *.. (octave +.. 1)) +.. semitone

(*****************************************************************************)
(* The panel *)
(*****************************************************************************)

let panel_theme : Theme.t =
  {
    Theme.default with
    text = rgb 240 230 210;
    accent = rgb 230 170 60;
    edge = rgb 150 130 110;
    face = rgb 80 60 45;
    face_hot = rgb 105 80 60;
    face_down = rgb 60 45 35;
    text_size = 15.;
    dial = 40.;
    dial_face = rgb 30 22 16;
    pointer = rgb 245 235 215;
  }

(* the drawbars: a column each, from the slot at the top down to the
 * tip, a step per level *)
let drawbar_x (i : int) : number = -300. + (float_of_int i * 54.)
let slot_y = 400.
let step = 30.
let drawbar_width = 34.

(* the B-3's colours: brown under the note, white the octaves, black
 * the rest *)
let drawbar_color (i : int) : color =
  match i with 0 | 1 -> rgb 120 70 40 | 2 | 3 | 5 | 8 -> rgb 240 235 225 | _ -> rgb 25 25 25

(* the level the mouse at [y] pulls a drawbar to *)
let level_at (y : number) : int = max 0 (min 8 (int_of_float (Float.round ((slot_y - y - (step / 2.)) / step))))

(* the tabs, switches and knobs: a control of Hammond_voice.knobs, where
 * it sits, the word under it *)
type place = { name : string; x : number; y : number; label : string }

let place name x y label = { name; x; y; label }

let places =
  [
    place "percussion" 180. 380. "ON";
    place "percussion.soft" 225. 380. "SOFT";
    place "percussion.fast" 270. 380. "FAST";
    place "percussion.third" 315. 380. "3RD";
    place "vibrato" 415. 370. "VIBRATO";
    place "leslie" 180. 250. "ON";
    place "leslie.fast" 225. 250. "FAST";
    place "click" 315. 250. "CLICK";
    place "volume" 415. 250. "VOLUME";
  ]

let headers = [ ("DRAWBARS", -84., 455.); ("PERCUSSION", 247., 425.); ("LESLIE", 202., 295.) ]

let control (computer : computer) (p : Hammond_voice.patch) (pl : place) : Hammond_voice.patch =
  match List.find_opt (fun (k : Hammond_voice.knob) -> k.name = pl.name) Hammond_voice.knobs with
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
let keyboard_top = -165.
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
  ignore (Audio.instrument "hammond" (fun () -> inst));
  Gui.set_theme Theme.default;
  let preset = Gui.menu computer ~at:(330., 482.) (List.map fst presets) m.preset in
  let patch = if preset <> m.preset then snd (List.nth presets preset) else m.patch in
  Gui.set_theme panel_theme;
  let patch = List.fold_left (control computer) patch places in
  (* the drawbars: pressed on one, it follows the mouse until let go *)
  let mouse = computer.mouse in
  let pulling =
    if not mouse.mdown then None
    else
      match m.pulling with
      | Some i -> Some i
      | None ->
          List.find_opt
            (fun i -> Float.abs (mouse.mx - drawbar_x i) <= drawbar_width / 2. && mouse.my <= slot_y + 10. && mouse.my >= slot_y - (10. * step))
            (List.init 9 (fun i -> i))
  in
  let patch =
    match pulling with
    | Some i when level_at mouse.my <> patch.drawbars.(i) ->
        let d = Array.copy patch.drawbars in
        d.(i) <- level_at mouse.my;
        { patch with drawbars = d }
    | _ -> patch
  in
  (* the letters, several at once *)
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) and released k = List.mem k m.held && not (List.mem k now) in
  let octave = if pressed "z" then max 2 (m.octave -.. 1) else if pressed "x" then min 6 (m.octave +.. 1) else m.octave in
  List.iter
    (fun (k, semitone) ->
      let n = note m.octave semitone in
      if pressed k then inst.note_on n 1.;
      if released k then inst.note_off n)
    letters;
  (* the mouse on the keyboard, when not pulling a drawbar *)
  let under = if mouse.mdown && pulling = None then Option.map (note m.octave) (key_at mouse.mx mouse.my) else None in
  if under <> m.mouse_note then (
    Option.iter inst.note_off m.mouse_note;
    Option.iter (fun n -> inst.note_on n 1.) under);
  (* space: the Leslie's speed, as the organist's foot switch *)
  let space = computer.keyboard.kspace in
  let patch = if space && not m.space then { patch with leslie_fast = not patch.leslie_fast } else patch in
  Hammond_voice.set_patch organ patch;
  let horn, drum = Hammond_voice.rotors organ in
  let turn a speed = Float.rem (a + (speed / 60.)) 1. in
  {
    patch;
    preset;
    octave;
    held = now;
    mouse_note = under;
    pulling;
    horn_angle = (if patch.leslie then turn m.horn_angle horn else m.horn_angle);
    drum_angle = (if patch.leslie then turn m.drum_angle (-.drum) else m.drum_angle);
    space;
  }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 240 230 210
let text (s : string) : shape = words ink s |> scale 1.2

let drawbars_view (p : Hammond_voice.patch) : shape list =
  List.concat
    (List.mapi
       (fun i footage ->
         let level = p.drawbars.(i) and x = drawbar_x i in
         let tip = slot_y - (float_of_int (level +.. 1) * step) in
         [
           (* the slot, and the bar out of it *)
           rectangle (rgb 20 14 10) (drawbar_width + 6.) 12. |> move x slot_y;
           rectangle (rgb 170 160 140) 10. (slot_y - tip) |> move x ((slot_y + tip) / 2.);
           rectangle (drawbar_color i) drawbar_width (step - 2.) |> move x tip;
           words (if i = 0 || i = 1 || drawbar_color i = rgb 25 25 25 then ink else rgb 30 30 30) (string_of_int level)
           |> scale 1.3 |> move x tip;
           text footage |> move x (slot_y + 22.);
         ])
       Hammond_voice.footages)
  @ [ words (rgb 230 170 60) (Hammond_voice.of_registration p) |> scale 2. |> move (-385.) 300.; text "REGISTRATION" |> move (-385.) 335. ]

let panel_view (p : Hammond_voice.patch) : shape list =
  let wood = rgb 110 65 35 in
  [ rectangle (rgb 45 30 20) 960. 440. |> move 0. 245.; rectangle wood 22. 470. |> move (-489.) 245.; rectangle wood 22. 470. |> move 489. 245. ]
  @ List.map (fun (h, x, y) -> words ink h |> scale 1.4 |> move x y) headers
  @ [ rectangle (rgb 90 70 55) 2. 400. |> move 152. 240. ]
  @ List.filter_map
      (fun pl -> if pl.label = "" then None else Some (text pl.label |> move pl.x (pl.y - if pl.name = "vibrato" then 32. else 38.)))
      places
  @ drawbars_view p

(* the spectrum of the last 2048 samples, 20 Hz to 20 kHz on a log
 * axis, -80 to 0 dB: each drawbar a line *)
let spectrum_view (samples : Signal.t) : shape list =
  let cx = -150. and cy = -55. and w = 640. and h = 120. in
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
    rectangle (rgb 230 170 60) (bw - 1.) (Float.max 1. bh) |> move (cx - (w / 2.) + ((float_of_int b + 0.5) * bw)) (cy - (h / 2.) + (bh / 2.))
  in
  (rectangle (rgb 25 18 12) w h |> move cx cy) :: List.init bars bar

(* the Leslie seen from above: the drum a disc with its opening, the
 * horn a bar with a mouth at one end, each at its angle *)
let leslie_view (m : model) : shape list =
  let cx = 360. and cy = -55. in
  let deg a = a * 360. in
  let on = m.patch.leslie in
  [
    rectangle (rgb 90 55 30) 200. 130. |> move cx cy;
    circle (rgb 50 35 25) 48. |> move cx cy;
    rectangle (rgb 200 190 170) 12. 44. |> move 0. 24. |> rotate (deg m.drum_angle) |> move cx cy;
    group [ rectangle (rgb 30 30 30) 84. 10.; rectangle (rgb 30 30 30) 12. 26. |> move 42. 0. ] |> rotate (deg m.horn_angle) |> move cx cy;
    words (if on then ink else rgb 150 130 110)
      (if not on then "LESLIE OFF" else if m.patch.leslie_fast then "FAST" else "SLOW")
    |> scale 1.1 |> move cx (cy - 52.);
  ]

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
    let color = if down s then rgb 230 170 60 else if black then rgb 20 20 20 else rgb 250 250 245 in
    let label = match letter_of s with Some k -> [ words (if black then white else rgb 120 120 120) k |> scale 1.6 |> move_y ((-.h / 2.) + 18.) ] | None -> [] in
    group (rectangle color w h :: label) |> move (key_x s) (keyboard_top - (h / 2.))
  in
  let keys = List.init keys_count (fun s -> s) in
  List.map key (List.filter (fun s -> not (is_black s)) keys)
  @ List.map key (List.filter is_black keys)
  @ [ words black (Printf.sprintf "C%d" m.octave) |> scale 1.4 |> move (keyboard_left + 20.) (keyboard_top + 14.) ]

let status (m : model) : string =
  let horn, drum = Hammond_voice.rotors organ in
  Printf.sprintf "%s   voices %d   horn %.1f, drum %.1f turns a second   space: the Leslie slow or fast"
    (Hammond_voice.of_registration m.patch) (Hammond_voice.voices organ) horn drum

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 215 205 190) computer.screen.width computer.screen.height ]
  @ [ words black "TinyHammond" |> scale 2.4 |> move (-360.) 482.; words black "preset" |> scale 1.5 |> move 230. 482. ]
  @ [ words (rgb 70 70 70) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-110.) 482. ]
  @ panel_view m.patch
  @ spectrum_view (Hammond_voice.recent organ)
  @ leslie_view m
  @ [ words (rgb 70 70 70) (status m) |> scale 1.3 |> move 0. (-137.) ]
  @ keyboard_view computer m @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app app
