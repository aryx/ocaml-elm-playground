(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of Teenage Engineering's OP-1 (2011), the synthesizer,
 * sampler and four-track studio the size of a keyboard: a small screen,
 * four coloured encoders, and whatever the screen shows, its four
 * parameters drawn in the encoders' colours. The engines are
 * Op1_engine.ml, the sound and the tape Studio_op1.ml; this is its
 * panel.
 *
 * The OP-1's one idea about control, here as there: the four encoders,
 * blue, green, white and orange, and the screen saying what they turn.
 * In synth mode, the keys T1 to T4 choose the module on the screen:
 *
 *     T1 engine    its four values, the sound's shape drawn (the scope)
 *     T2 envelope  attack decay sustain release, the envelope drawn
 *     T3 effect    delay, spring, punch, nitro, or none
 *     T4 LFO       tremolo, value, or none
 *
 * and a module's T key pressed again changes it: the next engine, the
 * play mode (poly, mono), the next effect, the next LFO -- the OP-1 does
 * that with shift and a browser, ours is one key. The engines are ten
 * (Op1_engine.mli), the tenth the sampler: its screen its recording and
 * the four points on it, SAMPLE taking the recording from the armed
 * tape track (record on the tape, sample it, play it across the keys:
 * the OP-1's way round). The keys 1 to 8 are the eight sounds. In tape
 * mode, T1 to T4 arm a track, the four tracks
 * are drawn as the reels' contents with the head over them, and the
 * transport records (on the armed track, the others playing back), plays
 * and stops; the encoders there are ours: the armed track's level, the
 * tape's speed (backwards to twice as fast: the OP-1's tape tricks), the
 * volume, the head. The keyboard has no velocity, as the OP-1's.
 *
 * The keys play with the mouse or the letters (a s d f g h j k the white
 * keys from C, w e t y u the black ones, z and x an octave down and up);
 * space plays and stops the tape, r records.
 *
 * Uses: Studio_op1 (the sound, the effects, the LFOs, the tape),
 * Op1_engine (the engines' names and encoders), Tape (the reels drawn),
 * Audio's instruments, Gui (the encoders as knobs, the buttons). Not:
 * Spectrum, Scene2d, Sprite, File_menu.
 *
 * Exercises: the tape's lift and drop (Tape.mli: the OP-1's cut and
 * paste) as two buttons; the tape's loop; the mixer page (the four
 * tracks' levels and pans together); the OP-1's sequencers (endless,
 * pattern, tombola, ...); the drum sampler.
 *)
open Playground
open Basics (* float arithmetics *)

(* the letters: each the semitone above the octave's C *)
let letters =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

type mode = Synth | Tape_mode

type model = {
  patch : Studio_op1.patch;
  mode : mode;
  page : int; (* synth mode: T1 to T4, 0 to 3 *)
  track : int; (* tape mode: the armed track *)
  speed : float;
  octave : int;
  held : string list; (* the letters held at the last frame *)
  mouse_note : int option;
}

let initial_model : model =
  { patch = Studio_op1.initial; mode = Synth; page = 0; track = 0; speed = 1.; octave = 4; held = []; mouse_note = None }

(* the studio lives with the sound, not in the model: the mixer pulls
 * its blocks between frames (Instrument.mli) *)
let op1 = Studio_op1.create initial_model.patch
let inst : Instrument.t = Studio_op1.instrument op1
let note (octave : int) (semitone : int) : int = (12 *.. (octave +.. 1)) +.. semitone

(*****************************************************************************)
(* The encoders *)
(*****************************************************************************)

let blue = rgb 70 120 225
let green = rgb 70 185 95
let pale = rgb 235 235 235
let orange = rgb 240 115 40
let encoder_colors = [| blue; green; pale; orange |]
let encoder_x (k : int) : number = 130. + (float_of_int k * 85.)
let encoder_y = 250.

let knob_theme (c : color) : Theme.t =
  { Theme.default with dial = 30.; dial_face = c; pointer = (if c = pale then rgb 30 30 30 else white); edge = rgb 60 60 60 }

(* the four encoders over [values] (0 to 1), and their values after
 * this frame *)
let encoders (computer : computer) (values : float array) : float array =
  Array.mapi
    (fun k v ->
      Gui.set_theme (knob_theme encoder_colors.(k));
      Gui.knob computer ~at:(encoder_x k, encoder_y) ~from:0. ~to_:1. v)
    values

(* what the four encoders turn, and their values, in synth mode *)
let page_names (s : Studio_op1.sound) (page : int) : string array =
  match page with
  | 0 -> (List.nth Op1_engine.all s.engine).encoders
  | 1 -> Studio_op1.envelope_encoders
  | 2 -> Studio_op1.effect_encoders s.effect
  | _ -> Studio_op1.lfo_encoders s.lfo

let page_values (s : Studio_op1.sound) (page : int) : float array =
  match page with 0 -> s.engine_params | 1 -> s.envelope | 2 -> s.effect_params | _ -> s.lfo_params

let with_values (s : Studio_op1.sound) (page : int) (v : float array) : Studio_op1.sound =
  match page with
  | 0 -> { s with engine_params = v }
  | 1 -> { s with envelope = v }
  | 2 -> { s with effect_params = v }
  | _ -> { s with lfo_params = v }

(* a module's T key pressed again: the next of its kind, the effect and
 * the LFO going through "none" too *)
let next_kind (s : Studio_op1.sound) (page : int) : Studio_op1.sound =
  let cycle current on = if not on then 0 else current +.. 1 in
  match page with
  | 0 -> { s with engine = (s.engine +.. 1) mod List.length Op1_engine.all }
  | 1 -> { s with play_mode = (s.play_mode +.. 1) mod List.length Studio_op1.play_modes }
  | 2 ->
      let k = cycle s.effect s.effect_on in
      if k >= List.length Studio_op1.effects then { s with effect_on = false } else { s with effect = k; effect_on = true }
  | _ ->
      let k = cycle s.lfo s.lfo_on in
      if k >= List.length Studio_op1.lfos then { s with lfo_on = false } else { s with lfo = k; lfo_on = true }

(*****************************************************************************)
(* The keyboard *)
(*****************************************************************************)

let keys_count = 25 (* two octaves and a C *)
let is_black (s : int) : bool = List.mem (s mod 12) [ 1; 3; 6; 8; 10 ]
let white_width = 56.
let keyboard_left = -420.
let keyboard_top = -10.
let white_height = 290.
let black_height = 175.
let whites_before (s : int) : int = List.length (List.filter (fun i -> not (is_black i)) (List.init s (fun i -> i)))

let key_x (s : int) : number =
  let w = float_of_int (whites_before s) in
  if is_black s then keyboard_left + (w * white_width) else keyboard_left + ((w + 0.5) * white_width)

(* the key under the mouse, a black key first (it's on top) *)
let key_at (x : number) (y : number) : int option =
  let keys = List.init keys_count (fun s -> s) in
  let height s = if is_black s then black_height else white_height in
  let hit s =
    let w = if is_black s then white_width * 0.6 else white_width in
    Float.abs (x - key_x s) <= w / 2. && y <= keyboard_top && y >= keyboard_top - height s
  in
  match List.find_opt (fun s -> is_black s && hit s) keys with Some s -> Some s | None -> List.find_opt hit keys

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let t_x (k : int) : number = -250. + (float_of_int k * 62.)
let sound_x (k : int) : number = -250. + (float_of_int k * 62.)
let row_y = 110.
let sounds_y = 50.

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "op1" (fun () -> inst));
  Gui.set_theme { Theme.default with face = rgb 225 225 225; face_hot = rgb 240 240 240; face_down = rgb 200 200 200; text = rgb 30 30 30 };
  let tape = Studio_op1.tape op1 in
  (* the modes *)
  let mode = if Gui.button computer ~at:(-430., row_y) "SYNTH" then Synth else m.mode in
  let mode = if Gui.button computer ~at:(-350., row_y) "TAPE" then Tape_mode else mode in
  (* T1 to T4: the module, or the track *)
  let pressed_t = List.find_opt (fun k -> Gui.button computer ~at:(t_x k, row_y) (Printf.sprintf "T%d" (k +.. 1))) [ 0; 1; 2; 3 ] in
  (* the sounds *)
  let patch = m.patch in
  let current =
    List.fold_left (fun c k -> if Gui.button computer ~at:(sound_x k, sounds_y) (string_of_int (k +.. 1)) then k else c) patch.current
      (List.init 8 (fun k -> k))
  in
  let s = patch.sounds.(current) in
  let page, track, s =
    match (mode, pressed_t) with
    | Synth, Some k when k = m.page && m.mode = Synth -> (k, m.track, next_kind s k)
    | Synth, Some k -> (k, m.track, s)
    | Tape_mode, Some k -> (m.page, k, s)
    | _, None -> (m.page, m.track, s)
  in
  (* the transport *)
  let space = computer.keyboard.kspace in
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) and released k = List.mem k m.held && not (List.mem k now) in
  if Gui.button computer ~at:(80., row_y) "REC" || pressed "r" then Studio_op1.record op1 track;
  if Gui.button computer ~at:(150., row_y) "PLAY" then Studio_op1.play op1;
  if Gui.button computer ~at:(220., row_y) "STOP" then Studio_op1.stop op1;
  if Gui.button computer ~at:(280., row_y) "<<" then Tape.set_head tape 0.;
  (* the sampler engine on T1: its recording taken from the armed track *)
  if m.mode = Synth && m.page = 0 && (List.nth Op1_engine.all patch.sounds.(patch.current).engine).name = "sampler" then
    if Gui.button computer ~at:(380., row_y) (Printf.sprintf "SAMPLE T%d" (m.track +.. 1)) then ignore (Studio_op1.sample_track op1 m.track);
  if space && not (List.mem "space" m.held) then if Tape.moving tape then Studio_op1.stop op1 else Studio_op1.play op1;
  (* the encoders: the module's four, or the tape's *)
  let s, speed, levels, volume =
    match mode with
    | Synth -> (with_values s page (encoders computer (page_values s page)), m.speed, patch.levels, patch.volume)
    | Tape_mode ->
        let length = float_of_int (Tape.length tape) in
        let head = Tape.head tape / length in
        let v = encoders computer [| patch.levels.(track); (m.speed + 2.) / 4.; patch.volume; head |] in
        let levels = Array.copy patch.levels in
        levels.(track) <- v.(0);
        (* the speed in quarter steps, 1 easy to find again *)
        let speed = Float.round (((v.(1) * 4.) - 2.) * 4.) / 4. in
        Tape.set_speed tape speed;
        if v.(3) <> head then Tape.set_head tape (v.(3) * length);
        (s, speed, levels, v.(2))
  in
  let sounds = Array.copy patch.sounds in
  sounds.(current) <- s;
  let patch : Studio_op1.patch = { sounds; current; levels; volume } in
  (* the letters, several at once *)
  let octave = if pressed "z" then max 1 (m.octave -.. 1) else if pressed "x" then min 6 (m.octave +.. 1) else m.octave in
  List.iter
    (fun (k, semitone) ->
      let n = note m.octave semitone in
      if pressed k then inst.note_on n 0.8;
      if released k then inst.note_off n)
    letters;
  (* the mouse on the keyboard *)
  let mouse = computer.mouse in
  let under = if mouse.mdown then key_at mouse.mx mouse.my else None in
  let under_note = Option.map (note m.octave) under in
  if under_note <> m.mouse_note then begin
    Option.iter inst.note_off m.mouse_note;
    Option.iter (fun n -> inst.note_on n 0.8) under_note
  end;
  Studio_op1.set_patch op1 patch;
  let held = if space then "space" :: now else now in
  { patch; mode; page; track; speed; octave; held; mouse_note = under_note }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 30 30 30
let screen_x = -110.
let screen_y = 250.
let screen_w = 300.
let screen_h = 170.

(* a line from one point to another, [w] wide *)
let segment (c : color) (w : number) (x1, y1) (x2, y2) : shape =
  let dx = x2 - x1 and dy = y2 - y1 in
  rectangle c (sqrt ((dx * dx) + (dy * dy))) w |> rotate (atan2 dy dx * 180. / Float.pi) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

let polyline (c : color) (points : (number * number) list) : shape list =
  let rec go = function a :: (b :: _ as rest) -> segment c 2. a b :: go rest | _ -> [] in
  go points

(* the screen's bottom: the four values as bars in the encoders'
 * colours, their names under them on two rows (some are long) *)
let bars_view (names : string array) (values : float array) : shape list =
  List.concat
    (List.init 4 (fun k ->
         let x = screen_x - 105. + (float_of_int k * 70.) and bottom = screen_y - 55. in
         let h = 40. * values.(k) in
         [
           rectangle (rgb 50 50 50) 50. 40. |> move x (bottom + 20.);
           rectangle encoder_colors.(k) 50. (Float.max 1. h) |> move x (bottom + (h / 2.));
           let name = match String.index_opt names.(k) '(' with Some i -> String.trim (String.sub names.(k) 0 i) | None -> names.(k) in
           words encoder_colors.(k) name |> scale 0.8 |> move x (bottom - 10. - (float_of_int (k mod 2) * 12.));
         ]))

(* the middle of the screen: the module drawn *)
let scope_view (samples : Signal.t) : shape list =
  let w = 260. and h = 50. and cy = screen_y + 15. and points = 130 in
  let at i = samples.(Array.length samples -.. 1024 +.. (i *.. 1024 /.. points)) in
  (* scaled to its peak, as the OP-1 draws its waves *)
  let peak = List.fold_left (fun p i -> Float.max p (Float.abs (at i))) 1e-3 (List.init points (fun i -> i)) in
  let x i = screen_x - (w / 2.) + (float_of_int i / float_of_int points * w) in
  let y i = cy + (at i / peak * h / 2.) in
  polyline pale (List.init points (fun i -> (x i, y i)))

(* the envelope: each stage as long as its time, on a square-root
 * scale, the sustain held a fixed while *)
let envelope_view (s : Studio_op1.sound) : shape list =
  let e = s.envelope in
  let t k = sqrt (Studio_op1.seconds e.(k)) in
  let a = t 0 and d = t 1 and r = t 3 in
  let hold = 0.3 * (a + d + r + 0.1) in
  let total = a + d + hold + r in
  let w = 240. and h = 60. and bottom = screen_y - 5. in
  let px x = screen_x - (w / 2.) + (x / total * w) and py l = bottom + (l * h) in
  polyline pale [ (px 0., py 0.); (px a, py 1.); (px (a + d), py e.(2)); (px (a + d + hold), py e.(2)); (px total, py 0.) ]

(* the LFO: a few of its periods, or the effect's name big *)
(* the sampler's recording, a peak a column, its start, loop in, loop
 * out and end as lines in the encoders' colours *)
let sample_view (p : float array) : shape list =
  let data = (Op1_engine.sample ()).data in
  let n = Array.length data and columns = 130 and w = 260. and h = 50. and cy = screen_y + 15. in
  let x f = screen_x - (w / 2.) + (f * w) in
  let peaks =
    List.init columns (fun c ->
        let a = c *.. n /.. columns and b = (c +.. 1) *.. n /.. columns in
        let peak = ref 0. in
        let i = ref a in
        while !i < b do
          peak := Float.max !peak (Float.abs data.(!i));
          i := !i +.. max 1 ((b -.. a) /.. 32)
        done;
        rectangle pale 1.5 (Float.max 0.5 (Float.min h (h * !peak))) |> move (x (float_of_int c / float_of_int columns)) cy)
  in
  peaks @ Array.to_list (Array.mapi (fun k f -> segment encoder_colors.(k) 2. (x f, cy - 28.) (x f, cy + 28.)) (Op1_engine.sampler_points p))

let wave_view (speed : number) : shape list =
  let w = 260. and h = 50. and cy = screen_y + 25. and points = 100 in
  let cycles = 1. + (6. * speed) in
  let x i = screen_x - (w / 2.) + (float_of_int i / float_of_int points * w) in
  let y i = cy + (h / 2. * sin (2. * Float.pi * cycles * float_of_int i / float_of_int points)) in
  polyline pale (List.init (points +.. 1) (fun i -> (x i, y i)))

let synth_screen (m : model) : shape list =
  let s = m.patch.sounds.(m.patch.current) in
  let engine = List.nth Op1_engine.all s.engine in
  let title =
    match m.page with
    | 0 -> "T1 " ^ String.uppercase_ascii engine.name
    | 1 -> Printf.sprintf "T2 ENVELOPE  %s" (String.uppercase_ascii (List.nth Studio_op1.play_modes s.play_mode))
    | 2 -> if s.effect_on then "T3 " ^ String.uppercase_ascii (List.nth Studio_op1.effects s.effect) else "T3 NO EFFECT"
    | _ -> if s.lfo_on then "T4 " ^ String.uppercase_ascii (List.nth Studio_op1.lfos s.lfo) else "T4 NO LFO"
  in
  let middle =
    match m.page with
    | 0 when engine.name = "sampler" -> sample_view s.engine_params
    | 0 -> scope_view (Studio_op1.recent op1)
    | 1 -> envelope_view s
    | 2 -> scope_view (Studio_op1.recent op1)
    | _ -> wave_view (if s.lfo = 0 then s.lfo_params.(0) else s.lfo_params.(1))
  in
  let under = match m.page with 0 -> engine.kind | _ -> "" in
  [ words pale title |> scale 1.1 |> move screen_x (screen_y + 70.); words (rgb 150 150 150) under |> scale 0.8 |> move screen_x (screen_y + 55.) ]
  @ middle
  @ bars_view (page_names s m.page) (page_values s m.page)

(* the tape: the four tracks' contents around the head, 8 seconds of
 * them, a peak a column, the head over them (red while recording) *)
let tape_screen (m : model) : shape list =
  let tape = Studio_op1.tape op1 in
  let length = Tape.length tape and columns = 130 and window = 8 *.. Signal.rate in
  let from = max 0 (min (length -.. window) (Float.to_int (Tape.head tape) -.. (window /.. 4))) in
  let w = 260. and lane = 30. and top = screen_y + 50. in
  let x c = screen_x - (w / 2.) + (float_of_int c / float_of_int columns * w) in
  let lane_y k = top - (float_of_int k * (lane + 4.)) in
  let track k =
    let samples = Tape.track tape k in
    let per = window /.. columns in
    let c_color = encoder_colors.(k) in
    (rectangle (if k = m.track then rgb 45 45 45 else rgb 25 25 25) w lane |> move screen_x (lane_y k))
    :: List.init columns (fun c ->
           (* 32 samples a column, enough for its peak *)
           let peak = ref 0. in
           for i = 0 to 31 do
             peak := Float.max !peak (Float.abs samples.(from +.. (c *.. per) +.. (i *.. per /.. 32)))
           done;
           let h = Float.min lane (lane * 2. * !peak) in
           rectangle c_color 1.5 (Float.max 0.5 h) |> move (x c) (lane_y k))
  in
  let head = screen_x - (w / 2.) + ((Tape.head tape - float_of_int from) / float_of_int window * w) in
  let recording = Tape.recording tape <> None in
  let state = if recording then Printf.sprintf "REC T%d" (m.track +.. 1) else if Tape.moving tape then "PLAY" else "STOP" in
  [ words pale (Printf.sprintf "TAPE  %s  %.1f s  speed %.2f" state (Tape.head tape / float_of_int Signal.rate) (Tape.speed tape)) |> scale 1. |> move screen_x (screen_y + 70.) ]
  @ List.concat (List.init 4 track)
  @ [ segment (if recording then rgb 230 50 40 else pale) 2. (head, top + 17.) (head, lane_y 3 - 17.) ]
  @ List.init 4 (fun k -> words encoder_colors.(k) [| "level"; "speed"; "volume"; "head" |].(k) |> scale 0.8 |> move (screen_x - 105. + (float_of_int k * 70.)) (screen_y - 75.))

let keyboard_view (computer : computer) (m : model) : shape list =
  let letter_of s = List.find_map (fun (k, s') -> if s' = s then Some k else None) letters in
  let down s =
    m.mouse_note = Some (note m.octave s) || match letter_of s with Some k -> Set_.mem k computer.keyboard.keys | None -> false
  in
  let key s =
    let black = is_black s in
    let w = if black then white_width * 0.6 else white_width - 3. in
    let h = if black then black_height else white_height in
    (* the OP-1's keys: pale and dark grey, flat *)
    let color = if down s then encoder_colors.(m.page) else if black then rgb 70 70 72 else rgb 238 238 236 in
    let label = match letter_of s with Some k -> [ words (if black then white else rgb 130 130 130) k |> scale 1.4 |> move_y ((-.h / 2.) + 16.) ] | None -> [] in
    group (rectangle color w h :: label) |> move (key_x s) (keyboard_top - (h / 2.))
  in
  let keys = List.init keys_count (fun s -> s) in
  List.map key (List.filter (fun s -> not (is_black s)) keys)
  @ List.map key (List.filter is_black keys)
  @ [ words ink (Printf.sprintf "C%d" m.octave) |> scale 1.3 |> move (keyboard_left - 32.) (keyboard_top - 20.) ]

(* the speaker's grille, the OP-1's left *)
let speaker_view : shape list =
  List.concat (List.init 6 (fun i -> List.init 6 (fun j -> circle (rgb 150 150 152) 5. |> move (-420. + (float_of_int i * 18.)) (295. - (float_of_int j * 18.)))))

let view (computer : computer) (m : model) : shape list =
  let lit on = if on then rgb 240 115 40 else rgb 150 150 150 in
  [ rectangle (rgb 70 72 76) computer.screen.width computer.screen.height ]
  @ [ words white "TinyOp1" |> scale 2.4 |> move (-380.) 482.; words (rgb 200 200 200) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-150.) 482. ]
  @ [ rectangle (rgb 205 206 208) 960. 700. |> move 0. 20.; rectangle (rgb 185 186 188) 960. 6. |> move 0. 367. ]
  @ speaker_view
  @ [ rectangle (rgb 15 15 15) (screen_w + 16.) (screen_h + 16.) |> move screen_x screen_y; rectangle black screen_w screen_h |> move screen_x screen_y ]
  @ (match m.mode with Synth -> synth_screen m | Tape_mode -> tape_screen m)
  @ [
      words ink "OP-1" |> scale 1.6 |> move (encoder_x 0 - 15.) 320.;
      (* the lights over the mode keys, T keys and sound keys *)
      circle (lit (m.mode = Synth)) 4. |> move (-430.) (row_y + 22.);
      circle (lit (m.mode = Tape_mode)) 4. |> move (-350.) (row_y + 22.);
      circle (if Tape.recording (Studio_op1.tape op1) <> None then rgb 230 50 40 else rgb 150 150 150) 4. |> move 80. (row_y + 22.);
      circle (lit (Tape.moving (Studio_op1.tape op1))) 4. |> move 150. (row_y + 22.);
    ]
  @ List.init 4 (fun k -> circle (lit ((m.mode = Synth && m.page = k) || (m.mode = Tape_mode && m.track = k))) 4. |> move (t_x k) (row_y + 22.))
  @ List.init 8 (fun k -> circle (lit (m.patch.current = k)) 4. |> move (sound_x k) (sounds_y + 22.))
  @ [ words ink (Printf.sprintf "sound %d   voices %d" (m.patch.current +.. 1) (Studio_op1.voices op1)) |> scale 1.1 |> move 390. sounds_y;
      words (rgb 220 220 220) "T1-T4: the module (again: the next of its kind)   1-8: the sounds   space: play/stop   r: record   z x: octave"
      |> scale 1.1 |> move 0. (-375.) ]
  @ keyboard_view computer m @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app app
