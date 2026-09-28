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
 * over Vco, Svf, Envelope and Lfo; this is the synthesizer: its panel,
 * its ribbon and a keyboard.
 *
 * The panel is a part, Part_cs80.ml (Component.mli, the office's idea,
 * plan_tiny_reason.md): the same panel TinyReface's CS face shows
 * scaled in its case. This program is its host at full size, with what
 * the player touches: the ribbon, and the keyboard that feels each key.
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
 * Modulated_delay, Polyphony, Audio's instruments, Part_cs80 (the
 * panel, over Panel's widgets), Component, Meters, Gui (the menu),
 * Spectrum. Not: the effects rack,
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
  panel : Component.part; (* the CS-80's panel, Part_cs80 *)
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

(* the synthesizer lives with the sound, not in the model: the mixer
 * pulls its blocks between frames (Instrument.mli) *)
let cs80 = Voice_cs80.create (snd (List.hd presets))
let inst : Instrument.t = Voice_cs80.instrument cs80

let initial_model : model =
  { panel = Part_cs80.make cs80; preset = 0; octave = 4; held = []; mouse_note = None; pressed_at = 0.; pressure = 0.; ribbon_from = None; bend = 0. }

(* the panel at its own size, under the title *)
let panel_box : Widget.box = { x = 0.; y = 221.; w = fst Part_cs80.natural; h = snd Part_cs80.natural }
let note (octave : int) (semitone : int) : int = (12 *.. (octave +.. 1)) +.. semitone

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
  if preset <> m.preset then Voice_cs80.set_patch cs80 (snd (List.nth presets preset));
  (* the panel, unless the menu has the mouse *)
  let panel = if Gui.modal () then m.panel else Component.input_in ~scaled:false m.panel computer panel_box in
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
  { panel; preset; octave; held = now; mouse_note; pressed_at; pressure; ribbon_from; bend }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let green = rgb 120 220 160

(* the ribbon, on the panel over the keys *)
let ribbon_view (m : model) : shape list =
  [
      rectangle (rgb 60 60 65) ribbon_width 22. |> move 0. ribbon_y;
      (match m.ribbon_from with Some x -> circle (rgb 230 120 60) 9. |> move x ribbon_y | None -> group []);
      words (rgb 70 70 70) (Printf.sprintf "RIBBON   %+.1f semitones" m.bend) |> scale 1.1 |> move 0. (ribbon_y + 22.);
    ]

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
  @ Component.draw_in ~scaled:false m.panel panel_box ~active:true
  @ ribbon_view m
  @ Meters.spectrum ~at:(-160., -70.) ~size:(620., 100.) ~color:green ~back:(rgb 20 25 20) (Voice_cs80.recent cs80)
  @ Meters.scope ~at:(330., -70.) ~size:(300., 100.) ~points:150 ~color:green ~back:(rgb 20 25 20) ~gain:3. (Voice_cs80.recent cs80)
  @ [
      words (rgb 70 70 70)
        (Printf.sprintf "voices %d   a key: pressed where, the velocity; dragged down, the pressure   pressure %.2f" (Voice_cs80.voices cs80) m.pressure)
      |> scale 1.2 |> move 0. (-137.);
    ]
  @ keyboard_view computer m @ Gui.draw ()

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
