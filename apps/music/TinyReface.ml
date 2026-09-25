(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of Yamaha's Reface series (2015): four small keyboards,
 * the same case and 37 mini keys each, each a tribute to a family of
 * classic instruments -- the YC to the combo organs, the CP to the
 * electric pianos, the DX to FM, the CS to the analog polysynths. This
 * repository built the originals instead (plan_synth_teaching.md: the
 * machines with the history), and this is the Reface's idea over them:
 * one case, one keyboard, a switch, and the original behind each face.
 *
 *     YC  combo organ     TinyHammond's voice   Voice_hammond: the B-3
 *                                               and its Leslie
 *     CP  electric piano  TinyRhodes'           Voice_rhodes: the Rhodes,
 *                                               the Wurlitzer, the Clavinet
 *     DX  FM              TinyDX7's             Voice_dx7: six operators
 *     CS  virtual analog  TinyCS80's            Voice_cs80: two layers
 *
 * The Reface's panels, from its owner's manual, and what our originals
 * make of them: the YC's nine footage levers are the B-3's drawbars,
 * its rotary speaker our Leslie, its vibrato the scanner's; its five
 * organs (American tonewheel, English, Italian and Japanese transistor
 * organs, Yamaha's YC-45) only the first, our registrations in its
 * TYPE menu. The CP's six types (Rd I, Rd II, Wr, Clv, Toy, CP) are our
 * three models' presets; its effects row (drive, tremolo or wah, chorus
 * or phaser, delay, reverb) the voice's tremolo only. The DX's four
 * operators, twelve algorithms and a feedback per operator are the
 * DX7's six, its 32 algorithms and one feedback (the reduction is
 * TinyDX7's exercise). The CS's five oscillator types (multi saw,
 * pulse, sync, ring, FM) are the CS-80's two layers and its ring
 * modulator. Each face's controls are its voice's knobs, by name
 * (Voice.mli: every voice gives its knobs the same way, which is what
 * makes a hub thin), drawn by their kind: a knob, a rocker, a
 * selector as a knob in steps.
 *
 * The letters play (a s d f g h j k the white keys from C, w e t y u
 * the black ones, z and x an octave), and the mouse on the keys.
 *
 * Uses: Voice_hammond, Voice_rhodes, Voice_dx7, Voice_cs80 (the
 * voices, through Voice.S), Audio's instruments (one playing at a
 * time: the face left is stopped), Gui (the knobs, the rockers, the
 * menu). Not: Spectrum, Scene2d, Sprite, File_menu.
 *
 * Exercises: the Reface CP's effects row, audio/effects' Drive,
 * Modulated_delay, Delay and Reverb in a Rack after the voice; the YC's
 * transistor organs (dividers and square waves: a subtractive organ
 * beside the additive one); the DX's four operators; the CS's
 * oscillator types; the Reface's looper (TinyOp1's Tape, one track).
 *)
open Playground
open Basics (* float arithmetics *)

let letters =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

(*****************************************************************************)
(* The faces *)
(*****************************************************************************)

(* a face: a voice behind its controls, by the knobs' names *)
type face = {
  name : string; (* "YC" *)
  what : string;
  color : color;
  controls : (string * string) list; (* label, knob name *)
  presets : string list;
  preset : int -> unit;
  get : string -> float;
  set : string -> float -> unit;
  control : string -> Control.t;
  inst : Instrument.t;
  recent : unit -> Signal.t;
}

let face (type p v) (module V : Voice.S with type patch = p and type t = v) (voice : v) ~name ~what ~color controls : face =
  let find n =
    match List.find_opt (fun (k : V.knob) -> k.name = n) V.knobs with Some k -> k | None -> failwith ("TinyReface: no knob " ^ n)
  in
  List.iter (fun (_, n) -> ignore (find n)) controls;
  V.set_patch voice (snd (List.hd V.presets));
  {
    name;
    what;
    color;
    controls;
    presets = List.map fst V.presets;
    preset = (fun i -> V.set_patch voice (snd (List.nth V.presets i)));
    get = (fun n -> (find n).get (V.patch voice));
    set = (fun n x -> V.set_patch voice ((find n).put (V.patch voice) x));
    control = (fun n -> (find n).control);
    inst = V.instrument voice;
    recent = (fun () -> V.recent voice);
  }

(* the four, each made when first shown *)
let faces : face Lazy.t array =
  [|
    lazy
      (face
         (module Voice_hammond)
         (Voice_hammond.create Voice_hammond.initial)
         ~name:"YC" ~what:"combo organ: the Hammond B-3 and its Leslie" ~color:(rgb 225 90 50)
         (List.map2 (fun label n -> (label, "drawbar." ^ n)) Voice_hammond.footages [ "16"; "5-1/3"; "8"; "4"; "2-2/3"; "2"; "1-3/5"; "1-1/3"; "1" ]
         @ [ ("VIBRATO", "vibrato"); ("PERC", "percussion"); ("CLICK", "click"); ("ROTARY", "leslie"); ("FAST", "leslie.fast"); ("VOLUME", "volume") ]));
    lazy
      (face
         (module Voice_rhodes)
         (Voice_rhodes.create Voice_rhodes.initial)
         ~name:"CP" ~what:"electric piano: the Rhodes, the Wurlitzer, the Clavinet" ~color:(rgb 210 60 70)
         [ ("MODEL", "model"); ("VOICING", "voicing"); ("HARDNESS", "hardness"); ("DECAY", "decay"); ("TREMOLO", "tremolo.depth"); ("RATE", "tremolo.rate"); ("VOLUME", "volume") ]);
    lazy
      (face
         (module Voice_dx7)
         (Voice_dx7.create Voice_dx7.initial)
         ~name:"DX" ~what:"FM: the DX7's six operators" ~color:(rgb 60 120 210)
         ([ ("ALGO", "algorithm"); ("FB", "feedback") ] @ List.init 6 (fun k -> (Printf.sprintf "OP%d" (k +.. 1), Printf.sprintf "op%d.output" (k +.. 1)))));
    lazy
      (face
         (module Voice_cs80)
         (Voice_cs80.create Voice_cs80.initial)
         ~name:"CS" ~what:"virtual analog: the CS-80's two layers" ~color:(rgb 70 160 90)
         [
           ("CUTOFF", "I.lpf"); ("RESO", "I.lpf_res"); ("ATTACK", "I.attack"); ("DECAY", "I.decay"); ("SUSTAIN", "I.sustain"); ("RELEASE", "I.release");
           ("MIX", "mix"); ("DETUNE", "detune"); ("LFO", "sub.speed"); ("VIBRATO", "sub.vco"); ("CHORUS", "chorus"); ("VOLUME", "volume");
         ]);
  |]

type model = { face : int; presets : int array; octave : int; held : string list; mouse_note : int option }

let initial_model : model = { face = 0; presets = Array.make 4 0; octave = 4; held = []; mouse_note = None }
let current (m : model) : face = Lazy.force faces.(m.face)

(*****************************************************************************)
(* The keyboard: 37 mini keys, C to C *)
(*****************************************************************************)

let keys_count = 37
let is_black (s : int) : bool = List.mem (s mod 12) [ 1; 3; 6; 8; 10 ]
let white_width = 40.
let keyboard_left = -440.
let keyboard_top = 60.
let white_height = 200.
let black_height = 120.
let whites_before (s : int) : int = List.length (List.filter (fun i -> not (is_black i)) (List.init s (fun i -> i)))

let key_x (s : int) : number =
  let w = float_of_int (whites_before s) in
  if is_black s then keyboard_left + (w * white_width) else keyboard_left + ((w + 0.5) * white_width)

let key_at (x : number) (y : number) : int option =
  let keys = List.init keys_count (fun s -> s) in
  let height s = if is_black s then black_height else white_height in
  let hit s =
    let w = if is_black s then white_width * 0.6 else white_width in
    Float.abs (x - key_x s) <= w / 2. && y <= keyboard_top && y >= keyboard_top - height s
  in
  match List.find_opt (fun s -> is_black s && hit s) keys with Some s -> Some s | None -> List.find_opt hit keys

(* the keyboard starts an octave under the letters' *)
let note (octave : int) (s : int) : int = (12 *.. octave) +.. s

(*****************************************************************************)
(* update *)
(*****************************************************************************)

(* the controls in two rows of up to nine *)
let control_at (i : int) : number * number = (-400. + (float_of_int (i mod 9) * 100.), if i < 9 then 300. else 170.)

let control (computer : computer) (f : face) (i : int) ((_, name) : string * string) : unit =
  let at = control_at i in
  let v = f.get name in
  let v' =
    match f.control name with
    | Knob (from, to_) -> Gui.knob computer ~at ~from ~to_ v
    | Switch -> if Gui.rocker computer ~at (v >= 0.5) then 1. else 0.
    | Selector labels -> Float.round (Gui.knob computer ~at ~from:0. ~to_:(float_of_int (List.length labels -.. 1)) v)
  in
  if v' <> v then f.set name v'

let update (computer : computer) (m : model) : model =
  Gui.set_theme Theme.default;
  (* the switch's names apart: a face is made only when chosen *)
  let chosen = List.fold_left (fun c k -> if Gui.button computer ~at:(-100. + (float_of_int k * 70.), 420.) (List.nth [ "YC"; "CP"; "DX"; "CS" ] k) then k else c) m.face [ 0; 1; 2; 3 ] in
  (* one voice playing: the face left stopped, its notes with it *)
  if chosen <> m.face then Audio.stop ("reface" ^ (current m).name);
  let m = { m with face = chosen } in
  let f = current m in
  ignore (Audio.instrument ("reface" ^ f.name) (fun () -> f.inst));
  let preset = Gui.menu computer ~at:(360., 420.) f.presets m.presets.(m.face) in
  if preset <> m.presets.(m.face) then f.preset preset;
  let presets = Array.copy m.presets in
  presets.(m.face) <- preset;
  Gui.set_theme { Theme.default with dial = 32.; dial_face = rgb 35 35 38; pointer = f.color; face = rgb 225 225 222; text = rgb 30 30 30 };
  List.iteri (control computer f) f.controls;
  (* the letters, several at once *)
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) and released k = List.mem k m.held && not (List.mem k now) in
  let octave = if pressed "z" then max 2 (m.octave -.. 1) else if pressed "x" then min 6 (m.octave +.. 1) else m.octave in
  List.iter
    (fun (k, s) ->
      if pressed k then f.inst.note_on (note (m.octave +.. 1) s) 0.8;
      if released k then f.inst.note_off (note (m.octave +.. 1) s))
    letters;
  let mouse = computer.mouse in
  let under = if mouse.mdown then Option.map (note m.octave) (key_at mouse.mx mouse.my) else None in
  if under <> m.mouse_note then begin
    Option.iter f.inst.note_off m.mouse_note;
    Option.iter (fun n -> f.inst.note_on n 0.8) under
  end;
  { m with presets; octave; held = now; mouse_note = under }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 30 30 30

let segment (c : color) (w : number) (x1, y1) (x2, y2) : shape =
  let dx = x2 - x1 and dy = y2 - y1 in
  rectangle c (sqrt ((dx * dx) + (dy * dy))) w |> rotate (atan2 dy dx * 180. / Float.pi) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

let scope_view (f : face) : shape list =
  let samples = f.recent () in
  let cx = 0. and cy = -270. and w = 600. and h = 90. and points = 200 in
  let at i = samples.(Array.length samples -.. 1024 +.. (i *.. 1024 /.. points)) in
  let peak = List.fold_left (fun p i -> Float.max p (Float.abs (at i))) 1e-3 (List.init points (fun i -> i)) in
  let x i = cx - (w / 2.) + (float_of_int i / float_of_int points * w) and y i = cy + (at i / peak * h / 2.) in
  (rectangle (rgb 20 22 20) w h |> move cx cy) :: List.init (points -.. 1) (fun i -> segment f.color 2. (x i, y i) (x (i +.. 1), y (i +.. 1)))

let keyboard_view (computer : computer) (m : model) (f : face) : shape list =
  let letter_of s = if s >= 12 && s <= 24 then List.find_map (fun (k, s') -> if s' = s -.. 12 then Some k else None) letters else None in
  let down s = m.mouse_note = Some (note m.octave s) || match letter_of s with Some k -> Set_.mem k computer.keyboard.keys | None -> false in
  let key s =
    let black = is_black s in
    let w = if black then white_width * 0.6 else white_width - 3. in
    let h = if black then black_height else white_height in
    let color = if down s then f.color else if black then rgb 30 30 32 else rgb 245 245 242 in
    let label = match letter_of s with Some k -> [ words (if black then white else rgb 140 140 140) k |> scale 1.1 |> move_y ((-.h / 2.) + 14.) ] | None -> [] in
    group (rectangle color w h :: label) |> move (key_x s) (keyboard_top - (h / 2.))
  in
  let keys = List.init keys_count (fun s -> s) in
  List.map key (List.filter (fun s -> not (is_black s)) keys) @ List.map key (List.filter is_black keys)

let view (computer : computer) (m : model) : shape list =
  let f = current m in
  let labels =
    List.mapi
      (fun i (label, name) ->
        let x, y = control_at i in
        let value = match f.control name with Selector ls -> [ words ink (List.nth ls (int_of_float (f.get name))) |> scale 0.9 |> move x (y - 58.) ] | _ -> [] in
        (words ink label |> scale 1. |> move x (y - 44.)) :: value)
      f.controls
  in
  [ rectangle (rgb 60 60 64) computer.screen.width computer.screen.height ]
  @ [ words white "TinyReface" |> scale 2.4 |> move (-370.) 482.; words (rgb 200 200 200) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move 0. 482. ]
  (* the case, the face's colour a stripe *)
  @ [ rectangle (rgb 215 215 212) 960. 570. |> move 0. 100.; rectangle f.color 960. 10. |> move 0. 385. ]
  @ [ words f.color ("reface " ^ f.name) |> scale 2. |> move (-380.) 420.; words ink f.what |> scale 1.1 |> move (-380.) 395. |> move_x 120.; words ink "TYPE" |> scale 1. |> move 270. 420. ]
  @ List.concat labels
  @ keyboard_view computer m f
  @ scope_view f
  @ [ words (rgb 220 220 220) "letters: play (z x an octave)   the mouse: the keys" |> scale 1.1 |> move 0. (-340.) ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app app
