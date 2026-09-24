(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of Teenage Engineering's OP-XY (2024), the groovebox:
 * eight tracks stepping together, each a 16-step pattern playing an
 * instrument, parameter locks, scenes, and the brain that moves the
 * song into another key and scale. The studio is Studio_opxy.ml (over
 * Sequencer, Sampler, and TinyOp1's sounds); this is its panel.
 *
 * The OP-XY's panel, and ours: a screen, four knobs in four greys (dark,
 * mid, light, white: the OP-1's colours gone to aluminium), the modules
 * M1 to M4 choosing what the screen and the knobs show, the eight track
 * buttons, and the keys, which are also the sequencer's 16 steps:
 *
 *     M1 engine    the track's sound: an OP-1 engine's four values
 *                  (again: the next engine), the kit, the keys
 *     M2 envelope  attack decay sustain release (again: the play mode)
 *     M3 filter    cutoff, resonance, volume, pan
 *     M4 brain     key, scale, the track linked to it, the tempo
 *
 * Under them, the steps: a click sets the step to the notes held on the
 * letters (a chord), or the last note played, or clears it; with shift,
 * it *holds* the step, and the knobs of M1 and M3 then set its locks
 * (the OP-XY's hold-a-step-and-turn), its dot lit. The scene buttons
 * choose a scene, heard from the next bar; P gives the track its next
 * pattern in this scene.
 *
 * The letters play the track selected (a s d f g h j k the white keys
 * from C, w e t y u the black ones, z and x an octave); a drum track's
 * are the pads at their General MIDI keys (a the kick, s the snare, t
 * the closed hat, u the open one). Space plays and stops.
 *
 * Uses: Studio_opxy (the tracks, scenes, brain), Sequencer (the locks),
 * Sampler (the kit, the keys), Op1_engine and Studio_op1 (the sounds),
 * Audio's instruments, Gui (the knobs, the buttons). Not: Spectrum,
 * Scene2d, Sprite, File_menu.
 *
 * Exercises: the step components (pulse, hold, multiply, skip: the
 * OP-XY's fourteen); the OP-XY's own engines (axis, dissolve, epiano,
 * hardsync, organ, prism, simple, wavetable); the filter's envelope and
 * key tracking; the LFOs; the effects sends (FX I, FX II) and punch-in
 * effects; patterns longer than a bar; songs, scenes in order; the
 * brain's key detected from the notes.
 *)
open Playground
open Basics (* float arithmetics *)

let letters =
  [ ("a", 0); ("w", 1); ("s", 2); ("e", 3); ("d", 4); ("f", 5); ("t", 6); ("g", 7); ("y", 8); ("h", 9); ("u", 10); ("j", 11); ("k", 12) ]

type model = {
  patch : Studio_opxy.patch;
  track : int;
  page : int; (* M1 to M4, 0 to 3 *)
  held_step : int option; (* the step whose locks the knobs set *)
  last : int list; (* the last notes played, for a step clicked *)
  octave : int;
  held : string list;
}

let initial_model : model = { patch = Studio_opxy.initial; track = 0; page = 0; held_step = None; last = [ 36 ]; octave = 4; held = [] }
let opxy = Studio_opxy.create initial_model.patch
let inst : Instrument.t = Studio_opxy.instrument opxy
let note (octave : int) (semitone : int) : int = (12 *.. (octave +.. 1)) +.. semitone

(* a drum track's letters start on the kick, C2 *)
let octave_of (m : model) : int = match m.patch.tracks.(m.track).kind with Drums -> 2 | _ -> m.octave

(*****************************************************************************)
(* The knobs *)
(*****************************************************************************)

let greys = [| rgb 60 60 62; rgb 120 120 124; rgb 180 180 184; rgb 245 245 245 |]
let knob_x (k : int) : number = 130. + (float_of_int k * 85.)
let knob_y = 300.

let knob_theme (c : color) : Theme.t =
  { Theme.default with dial = 30.; dial_face = c; pointer = (if c = greys.(3) then rgb 30 30 30 else white); edge = rgb 40 40 40 }

let knobs (computer : computer) (values : float array) : float array =
  Array.mapi
    (fun k v ->
      Gui.set_theme (knob_theme greys.(k));
      Gui.knob computer ~at:(knob_x k, knob_y) ~from:0. ~to_:1. v)
    values

(* what the page's knobs are, by name: a lockable one, or the patch's *)
let page_names (m : model) : string array =
  let tr = m.patch.tracks.(m.track) in
  match (m.page, tr.kind) with
  | 0, Synth s -> (List.nth Op1_engine.all s.engine).encoders
  | 0, _ -> [| ""; ""; ""; "" |]
  | 1, Synth _ -> Studio_op1.envelope_encoders
  | 1, _ -> [| ""; ""; ""; "" |]
  | 2, _ -> [| "cutoff"; "resonance"; "volume"; "pan" |]
  | _ -> [| "key"; "scale"; "linked"; "tempo" |]

(* the knobs' values, 0 to 1, for the page (a held step's locks shown) *)
let page_values (m : model) : float array =
  let p = m.patch in
  let tr = p.tracks.(m.track) in
  let lock name default =
    match m.held_step with
    | Some s -> Option.value (List.assoc_opt name (tr.patterns.(p.scenes.(p.scene).chosen.(m.track))).(s).locks) ~default
    | None -> default
  in
  let locked names = Array.map (fun n -> lock n (Studio_opxy.get tr n)) names in
  match (m.page, tr.kind) with
  | 0, Synth _ -> locked [| "p1"; "p2"; "p3"; "p4" |]
  | 1, Synth s -> Array.copy s.envelope
  | 2, _ ->
      (* the pan, -1 to 1, on a knob from 0 to 1 *)
      let v = locked [| "cutoff"; "resonance"; "volume"; "pan" |] in
      v.(3) <- (v.(3) + 1.) / 2.;
      v
  | 3, _ -> [| float_of_int p.key / 11.; float_of_int p.scale / 4.; (if tr.linked then 1. else 0.); (p.tempo - 60.) / 120. |]
  | _ -> [| 0.; 0.; 0.; 0. |]

(* the page's knobs turned: the track, the patch, or the held step's
 * locks *)
let apply (m : model) (before : float array) (after : float array) : Studio_opxy.patch =
  let p = m.patch in
  let tr = p.tracks.(m.track) in
  let chosen = p.scenes.(p.scene).chosen.(m.track) in
  let set_track tr = let tracks = Array.copy p.tracks in tracks.(m.track) <- tr; { p with tracks } in
  let changed = List.filter (fun k -> after.(k) <> before.(k)) [ 0; 1; 2; 3 ] in
  (* a lockable knob: on the held step, else the track *)
  let turn name v p =
    let tr = p.Studio_opxy.tracks.(m.track) in
    match m.held_step with
    | Some s ->
        let patterns = Array.map Array.copy tr.patterns in
        let st = patterns.(chosen).(s) in
        patterns.(chosen).(s) <- { st with locks = (name, v) :: List.remove_assoc name st.locks };
        let tracks = Array.copy p.tracks in
        tracks.(m.track) <- { tr with patterns };
        { p with tracks }
    | None -> let tracks = Array.copy p.tracks in tracks.(m.track) <- Studio_opxy.put tr name v; { p with tracks }
  in
  List.fold_left
    (fun p k ->
      let v = after.(k) in
      match (m.page, tr.kind) with
      | 0, Synth _ -> turn [| "p1"; "p2"; "p3"; "p4" |].(k) v p
      | 1, Synth s ->
          let envelope = Array.copy s.envelope in
          envelope.(k) <- v;
          set_track { tr with kind = Synth { s with envelope } }
      | 2, _ -> turn [| "cutoff"; "resonance"; "volume"; "pan" |].(k) (if k = 3 then (v * 2.) - 1. else v) p
      | 3, _ -> (
          match k with
          | 0 -> { p with key = int_of_float (Float.round (v * 11.)) }
          | 1 -> { p with scale = int_of_float (Float.round (v * 4.)) }
          | 2 -> set_track { tr with linked = v >= 0.5 }
          | _ -> { p with tempo = Float.round (60. + (v * 120.)) })
      | _ -> p)
    p changed

(* a module pressed again: the next engine, the next play mode *)
let next_kind (tr : Studio_opxy.track) (page : int) : Studio_opxy.track =
  match (tr.kind, page) with
  | Synth s, 0 -> { tr with kind = Synth { s with engine = (s.engine +.. 1) mod List.length Op1_engine.all } }
  | Synth s, 1 -> { tr with kind = Synth { s with play_mode = (s.play_mode +.. 1) mod List.length Studio_op1.play_modes } }
  | _ -> tr

(*****************************************************************************)
(* The keyboard *)
(*****************************************************************************)

let keys_count = 25
let is_black (s : int) : bool = List.mem (s mod 12) [ 1; 3; 6; 8; 10 ]
let white_width = 56.
let keyboard_left = -420.
let keyboard_top = -30.
let white_height = 240.
let black_height = 145.
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

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let row_y = 190.
let steps_y = 60.
let step_x (s : int) : number = -435. + (float_of_int s * 58.)
let track_x (k : int) : number = -320. + (float_of_int k * 58.)
let scene_x (k : int) : number = 260. + (float_of_int k * 55.)

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "opxy" (fun () -> inst));
  Gui.set_theme { Theme.default with face = rgb 215 215 215; face_hot = rgb 235 235 235; face_down = rgb 190 190 190; text = rgb 30 30 30 };
  let p = m.patch in
  let space = computer.keyboard.kspace in
  if Gui.button computer ~at:(-430., row_y) (if Studio_opxy.running opxy then "STOP" else "PLAY") || (space && not (List.mem "space" m.held)) then
    Studio_opxy.run opxy (not (Studio_opxy.running opxy));
  (* the tracks, the modules, the scenes *)
  let track = List.fold_left (fun t k -> if Gui.button computer ~at:(track_x k, row_y) (string_of_int (k +.. 1)) then k else t) m.track (List.init 8 (fun k -> k)) in
  let pressed_m = List.find_opt (fun k -> Gui.button computer ~at:(knob_x k, 230.) (Printf.sprintf "M%d" (k +.. 1))) [ 0; 1; 2; 3 ] in
  let p, page =
    match pressed_m with
    | Some k when k = m.page && track = m.track ->
        let tracks = Array.copy p.tracks in
        tracks.(track) <- next_kind p.tracks.(track) k;
        ({ p with tracks }, k)
    | Some k -> (p, k)
    | None -> (p, m.page)
  in
  let scene = List.fold_left (fun s k -> if Gui.button computer ~at:(scene_x k, row_y) (Printf.sprintf "S%d" (k +.. 1)) then k else s) p.scene [ 0; 1; 2; 3 ] in
  let p = { p with scene } in
  let p =
    if Gui.button computer ~at:(scene_x 0 - 60., row_y) "P" then begin
      let scenes = Array.copy p.scenes in
      let chosen = Array.copy scenes.(scene).chosen in
      chosen.(track) <- (chosen.(track) +.. 1) mod 4;
      scenes.(scene) <- { (scenes.(scene)) with chosen };
      { p with scenes }
    end
    else p
  in
  let m = { m with patch = p; track; page; held_step = (if track <> m.track then None else m.held_step) } in
  (* the knobs *)
  let before = page_values m in
  let after = knobs computer before in
  let p = apply m before after in
  let m = { m with patch = p } in
  (* the letters, and the notes they leave for a step *)
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.held) and released k = List.mem k m.held && not (List.mem k now) in
  let octave = if pressed "z" then max 1 (m.octave -.. 1) else if pressed "x" then min 6 (m.octave +.. 1) else m.octave in
  Studio_opxy.select opxy track;
  let o = octave_of m in
  List.iter
    (fun (k, semitone) ->
      if pressed k then inst.note_on (note o semitone) 0.8;
      if released k then inst.note_off (note o semitone))
    letters;
  let down = List.filter_map (fun (k, s) -> if List.mem k now then Some (note o s) else None) letters in
  let last = if down <> [] then down else m.last in
  (* the mouse on the keyboard *)
  let mouse = computer.mouse in
  let last = match (mouse.mclick, key_at mouse.mx mouse.my) with true, Some s -> inst.note_on (note o s) 0.8; [ note o s ] | _ -> last in
  (* the steps: a click sets or clears, with shift holds *)
  let chosen = p.scenes.(p.scene).chosen.(track) in
  let clicked = if mouse.mclick then List.find_opt (fun s -> Float.abs (mouse.mx - step_x s) <= 26. && Float.abs (mouse.my - steps_y) <= 26.) (List.init 16 (fun s -> s)) else None in
  let m, p =
    match clicked with
    | Some s when computer.keyboard.kshift -> ({ m with held_step = (if m.held_step = Some s then None else Some s) }, p)
    | Some s ->
        let tracks = Array.copy p.tracks in
        let tr = tracks.(track) in
        let patterns = Array.map Array.copy tr.patterns in
        let st = patterns.(chosen).(s) in
        patterns.(chosen).(s) <- (if st.notes = [] then { st with notes = last } else { st with notes = [] });
        tracks.(track) <- { tr with patterns };
        (m, { p with tracks })
    | None -> (m, p)
  in
  Studio_opxy.set_patch opxy p;
  let held = if space then "space" :: now else now in
  { m with patch = p; last; octave; held }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 30 30 30
let screen_x = -150.
let screen_y = 300.
let screen_w = 330.
let screen_h = 170.
let pale = rgb 235 235 235
let accent = rgb 255 120 40

let segment (c : color) (w : number) (x1, y1) (x2, y2) : shape =
  let dx = x2 - x1 and dy = y2 - y1 in
  rectangle c (sqrt ((dx * dx) + (dy * dy))) w |> rotate (atan2 dy dx * 180. / Float.pi) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

(* the screen: the track and its page on top, the pattern in the middle
 * (a note a dot, at its pitch; a drum's pads one a row), the knobs as
 * bars in their greys at the bottom *)
let screen_view (m : model) : shape list =
  let p = m.patch in
  let tr = p.tracks.(m.track) in
  let pattern = tr.patterns.(p.scenes.(p.scene).chosen.(m.track)) in
  let kind = match tr.kind with Synth s -> (List.nth Op1_engine.all s.engine).name | Drums -> "808 kit" | Keys -> "rhodes keys" in
  let pages = [| "ENGINE"; "ENVELOPE"; "FILTER"; "BRAIN" |] in
  let title = Printf.sprintf "%d %s  %s  %s" (m.track +.. 1) (String.uppercase_ascii tr.name) kind pages.(m.page) in
  let brain = Printf.sprintf "%s %s   %.0f BPM   scene %d" [| "C"; "C#"; "D"; "Eb"; "E"; "F"; "F#"; "G"; "Ab"; "A"; "Bb"; "B" |].(p.key) (fst (List.nth Studio_opxy.scales p.scale)) p.tempo (Studio_opxy.playing opxy +.. 1) in
  let playing = if Studio_opxy.running opxy then Some (Studio_opxy.step opxy) else None in
  let lo, hi = List.fold_left (fun (lo, hi) (s : Studio_opxy.step) -> List.fold_left (fun (lo, hi) n -> (min lo n, max hi n)) (lo, hi) s.notes) (127, 0) (Array.to_list pattern) in
  let span = float_of_int (max 12 (hi -.. lo)) in
  let cell s = screen_x - 150. + (float_of_int s * 20.) in
  let grid =
    List.concat
      (List.init 16 (fun s ->
           let st = pattern.(s) in
           (if playing = Some s then [ rectangle (rgb 60 60 60) 18. 70. |> move (cell s) (screen_y + 10.) ] else [])
           @ List.map (fun n -> rectangle (if st.locks <> [] then accent else pale) 14. 4. |> move (cell s) (screen_y - 22. + (64. * float_of_int (n -.. lo) / span))) st.notes
           @ (if m.held_step = Some s then [ rectangle accent 18. 3. |> move (cell s) (screen_y - 28.) ] else [])))
  in
  let names = page_names m and values = page_values m in
  let bars =
    List.concat
      (List.init 4 (fun k ->
           let x = screen_x - 120. + (float_of_int k * 80.) and bottom = screen_y - 72. in
           if names.(k) = "" then []
           else
             [
               rectangle (rgb 45 45 45) 60. 8. |> move x bottom;
               rectangle greys.(k) (Float.max 1. (60. * values.(k))) 8. |> move (x - 30. + (30. * values.(k))) bottom;
               (* the names light: the dark grey's unreadable on black *)
               words (rgb 200 200 200) names.(k) |> scale 0.75 |> move x (bottom + 11.);
             ]))
  in
  [ words pale title |> scale 1. |> move screen_x (screen_y + 70.); words (rgb 150 150 150) brain |> scale 0.8 |> move screen_x (screen_y + 54.) ]
  @ grid @ bars

let keyboard_view (computer : computer) (m : model) : shape list =
  let o = octave_of m in
  let letter_of s = List.find_map (fun (k, s') -> if s' = s then Some k else None) letters in
  let down s = match letter_of s with Some k -> Set_.mem k computer.keyboard.keys | None -> false in
  let key s =
    let black = is_black s in
    let w = if black then white_width * 0.6 else white_width - 3. in
    let h = if black then black_height else white_height in
    let color = if down s then accent else if black then rgb 40 40 42 else rgb 225 225 222 in
    let label = match letter_of s with Some k -> [ words (if black then white else rgb 130 130 130) k |> scale 1.3 |> move_y ((-.h / 2.) + 16.) ] | None -> [] in
    group (rectangle color w h :: label) |> move (key_x s) (keyboard_top - (h / 2.))
  in
  let keys = List.init keys_count (fun s -> s) in
  List.map key (List.filter (fun s -> not (is_black s)) keys)
  @ List.map key (List.filter is_black keys)
  @ [ words ink (Printf.sprintf "C%d" o) |> scale 1.2 |> move (keyboard_left - 32.) (keyboard_top - 20.) ]

let view (computer : computer) (m : model) : shape list =
  let p = m.patch in
  let lit on = if on then accent else rgb 150 150 150 in
  let tr = p.tracks.(m.track) in
  let pattern = tr.patterns.(p.scenes.(p.scene).chosen.(m.track)) in
  let playing = if Studio_opxy.running opxy then Some (Studio_opxy.step opxy) else None in
  [ rectangle (rgb 50 52 56) computer.screen.width computer.screen.height ]
  @ [ words white "TinyOpxy" |> scale 2.4 |> move (-380.) 482.; words (rgb 200 200 200) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-150.) 482. ]
  @ [ rectangle (rgb 200 202 205) 960. 730. |> move 0. 45. ]
  @ [ rectangle (rgb 15 15 15) (screen_w + 16.) (screen_h + 16.) |> move screen_x screen_y; rectangle black screen_w screen_h |> move screen_x screen_y ]
  @ screen_view m
  @ [ words ink "OP-XY" |> scale 1.5 |> move (knob_x 0 - 10.) 360. ]
  (* the lights: the mode, the tracks (each its level), the scenes *)
  @ [ circle (lit (Studio_opxy.running opxy)) 4. |> move (-430.) (row_y + 22.) ]
  @ List.init 4 (fun k -> circle (lit (m.page = k)) 4. |> move (knob_x k) (230. + 22.))
  @ List.init 8 (fun k ->
        let level = Float.min 1. (Studio_opxy.level opxy k * 8.) in
        let muted = p.scenes.(Studio_opxy.playing opxy).mutes.(k) in
        group
          [
            circle (if m.track = k then accent else rgb 150 150 150) 4. |> move_y 22.;
            rectangle (if muted then rgb 150 150 150 else rgb 90 170 110) (40. * level + 1.) 3. |> move_y (-22.);
            words ink p.tracks.(k).name |> scale 0.7 |> move_y (-31.);
          ]
        |> move (track_x k) row_y)
  @ List.init 4 (fun k -> circle (lit (p.scene = k)) 4. |> move (scene_x k) (row_y + 22.))
  @ [ words ink (Printf.sprintf "pattern %d" (p.scenes.(p.scene).chosen.(m.track) +.. 1)) |> scale 0.8 |> move (scene_x 0 - 60.) (row_y - 28.) ]
  (* the steps: set ones dark, the playing one lit, a held one ringed,
   * a dot for locks *)
  @ List.concat
      (List.init 16 (fun s ->
           let st = pattern.(s) in
           let face = if playing = Some s then accent else if st.notes <> [] then rgb 60 60 62 else rgb 235 235 232 in
           (if m.held_step = Some s then [ rectangle accent 52. 52. |> move (step_x s) steps_y ] else [])
           @ [ rectangle face 46. 46. |> move (step_x s) steps_y; words (if st.notes <> [] then white else ink) (string_of_int (s +.. 1)) |> scale 0.9 |> move (step_x s) steps_y ]
           @ if st.locks <> [] then [ circle (rgb 90 140 230) 4. |> move (step_x s + 16.) (steps_y + 16.) ] else []))
  @ keyboard_view computer m
  @ [ words (rgb 220 220 220) "click a step: set/clear   shift+click: hold it, the knobs lock it   M again: next engine/mode   space: play"
      |> scale 1.1 |> move 0. (-350.) ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Playground_platform.run_app app
