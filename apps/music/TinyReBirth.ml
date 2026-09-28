(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A toy version of Propellerhead's ReBirth RB-338 (1997), the techno
 * studio in a computer: two TB-303s, a TR-808 and a TR-909 on one
 * clock, a mixer and the effects. The machines are TinyTB303's and
 * TinyTR808's own voices (Voice_tb303.ml, Voice_tr808.ml), gathered by
 * Studio_rebirth.ml; this is its panel.
 *
 * The panel: a strip per machine, in the mixer's order -- its name,
 * < and > to change its pattern (each machine's own patterns), its
 * level and its mute, its 16 steps drawn as the original draws them
 * (the 303's notes a piano roll, accents orange, slides reaching the
 * next step; the 808's buttons in its colours by fours, the 909's light,
 * an LED over each lit for a hit of the instrument edited, BD, SD, ...,
 * a button under the name), the step playing lit: the four lit
 * together, the one clock. A click on a step edits it: on a 303 a note
 * at the click's height, or a rest if it is there already; on a drum
 * machine the instrument edited toggled. Each strip is a rack unit,
 * ears and screws.
 * Over them start/stop (and space), the tempo, the volume; under them
 * the effects: the distortion on the 303s, the delay, the compressor,
 * and the PCF, the pattern controlled filter, its 16 cutoffs bars to
 * click. Editing a pattern in depth (accents, slides, the drums' knobs)
 * is TinyTB303's and TinyTR808's job;
 * this one is the whole: which patterns, how loud, through what.
 *
 * Uses: Studio_rebirth (the four machines on one clock), Voice_tb303,
 * Voice_tr808, Drive, Delay, Dynamics, Svf, Audio's instruments, Gui
 * (the knobs, the rockers, the buttons, the menu), Spectrum. Not:
 * Scene2d, Sprite, File_menu.
 *
 * Exercises: the song mode (patterns chained, ReBirth's own); accents
 * and slides edited here; each machine's own effects sends; ReBirth's .rbs songs
 * saved (File_menu, the voices' text).
 *)
open Playground
open Basics (* float arithmetics *)

type model = {
  patch : Studio_rebirth.patch;
  song : int;
  patterns : int array; (* each machine's pattern, an index in its presets *)
  instruments : int array; (* each drum machine's instrument edited, in [Voice_tr808.instruments] *)
  space : bool;
}

let songs = Studio_rebirth.songs

(* each machine's pattern in a song, found among its presets *)
let patterns_of (p : Studio_rebirth.patch) : int array =
  let find x l = let rec go i = function [] -> 0 | (_, y) :: r -> if y = x then i else go (i +.. 1) r in go 0 l in
  [| find p.bass1 Voice_tb303.presets; find p.bass2 Voice_tb303.presets; find p.drums808 Voice_tr808.presets; find p.drums909 Voice_tr808.presets |]

let initial_model : model = { patch = snd (List.hd songs); song = 0; patterns = patterns_of (snd (List.hd songs)); instruments = [| 0; 0; 0; 0 |]; space = false }

(* the studio lives with the sound, not in the model: the mixer pulls
 * its blocks between frames (Instrument.mli) *)
let rebirth = Studio_rebirth.create initial_model.patch
let inst : Instrument.t = Studio_rebirth.instrument rebirth

(*****************************************************************************)
(* The panel *)
(*****************************************************************************)

let panel_theme : Theme.t =
  {
    Theme.default with
    text = rgb 30 30 30;
    accent = rgb 230 120 40;
    edge = rgb 110 110 110;
    face = rgb 225 225 220;
    face_hot = rgb 240 240 235;
    face_down = rgb 200 200 195;
    text_size = 12.;
    dial = 28.;
    dial_face = rgb 30 30 30;
    pointer = rgb 240 240 240;
  }

let strip_y (k : int) : number = 360. - (float_of_int k * 90.)
let cell_x (s : int) : number = -40. + (float_of_int s * 30.)

(* each machine's patterns: the 303s' and the drum machines' presets *)
let pattern_names (k : int) : string list = if k < 2 then List.map fst Voice_tb303.presets else List.map fst Voice_tr808.presets

let with_pattern (p : Studio_rebirth.patch) (k : int) (i : int) : Studio_rebirth.patch =
  match k with
  | 0 -> { p with bass1 = snd (List.nth Voice_tb303.presets i) }
  | 1 -> { p with bass2 = snd (List.nth Voice_tb303.presets i) }
  | 2 -> { p with drums808 = snd (List.nth Voice_tr808.presets i) }
  | _ -> { p with drums909 = snd (List.nth Voice_tr808.presets i) }

(* a drum step's LED: lit if the instrument edited plays on it *)
let step_on (d : Voice_tr808.patch) (i : int) (s : int) : bool = d.tracks.(Voice_tr808.index (List.nth Voice_tr808.instruments i)).(s)

(* the 303's piano roll: its notes' range, an octave at least, from the
 * cell's bottom to its top *)
let pitch_range (pattern : Sequencer.step array) : int * int =
  let notes = Array.to_list pattern |> List.filter_map (fun (st : Sequencer.step) -> st.note) in
  let lo = List.fold_left min 127 notes and hi = List.fold_left max 0 notes in
  if notes = [] then (36, 48) else (lo, max hi (lo +.. 12))

let pitch_y (lo, hi) (n : int) : number = -16. + (float_of_int (n -.. lo) / float_of_int (hi -.. lo) * 32.)

(* a click on a 303's cell: a note at the click's height, or a rest if
 * that note is there already *)
let click_303 (pattern : Sequencer.step array) (s : int) (dy : number) : Sequencer.step array =
  let lo, hi = pitch_range pattern in
  let n = lo +.. int_of_float (Float.round ((Float.min 1. (Float.max 0. ((dy + 16.) / 32.))) * float_of_int (hi -.. lo))) in
  let pattern = Array.copy pattern in
  let st = pattern.(s) in
  pattern.(s) <- (if st.note = Some n then Sequencer.rest else { st with note = Some n });
  pattern

(* a click on a drum machine's step: the instrument edited toggled there *)
let click_drum (d : Voice_tr808.patch) (i : int) (s : int) : Voice_tr808.patch =
  let t = Voice_tr808.index (List.nth Voice_tr808.instruments i) in
  let tracks = Array.copy d.tracks in
  tracks.(t) <- Array.copy tracks.(t);
  tracks.(t).(s) <- not tracks.(t).(s);
  { d with tracks }

let pcf_x (s : int) : number = -40. + (float_of_int s * 30.)
let pcf_bottom = -95.
let pcf_height = 70.

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  ignore (Audio.instrument "rebirth" (fun () -> inst));
  Gui.set_theme Theme.default;
  let song = Gui.menu computer ~at:(330., 482.) (List.map fst songs) m.song in
  let patch = if song <> m.song then snd (List.nth songs song) else m.patch in
  Gui.set_theme panel_theme;
  (* the transport *)
  let space = computer.keyboard.kspace in
  let button = Gui.button computer ~at:(-400., 430.) (if Studio_rebirth.running rebirth then "STOP" else "START") in
  if button || (space && not m.space) then Studio_rebirth.run rebirth (not (Studio_rebirth.running rebirth));
  let tempo = Float.round (Gui.knob computer ~at:(-300., 440.) ~from:60. ~to_:180. patch.tempo) in
  let volume = Gui.knob computer ~at:(-220., 440.) ~from:0. ~to_:1. patch.volume in
  let patch = { patch with tempo; volume } in
  (* the strips: the pattern, the level, the mute *)
  let patterns = if song <> m.song then patterns_of patch else Array.copy m.patterns in
  let instruments = Array.copy m.instruments in
  let mouse = computer.mouse in
  let patch =
    List.fold_left
      (fun (p : Studio_rebirth.patch) k ->
        let y = strip_y k and count = List.length (pattern_names k) in
        let p =
          if Gui.button computer ~at:(-330., y) "<" then begin
            patterns.(k) <- (patterns.(k) +.. count -.. 1) mod count;
            with_pattern p k patterns.(k)
          end
          else if Gui.button computer ~at:(-190., y) ">" then begin
            patterns.(k) <- (patterns.(k) +.. 1) mod count;
            with_pattern p k patterns.(k)
          end
          else p
        in
        let levels = Array.copy p.levels and mutes = Array.copy p.mutes in
        levels.(k) <- Gui.knob computer ~at:(-130., y) ~from:0. ~to_:1. p.levels.(k);
        mutes.(k) <- Gui.rocker computer ~at:(-80., y) p.mutes.(k);
        let p = { p with levels; mutes } in
        (* a drum machine's instrument edited, the next at each click *)
        if k >= 2 then begin
          let d = if k = 2 then p.drums808 else p.drums909 in
          let label = Voice_tr808.label d.machine (List.nth Voice_tr808.instruments instruments.(k)) in
          if Gui.button_in computer { x = -410.; y = y - 24.; w = 44.; h = 22. } label then
            instruments.(k) <- (instruments.(k) +.. 1) mod List.length Voice_tr808.instruments
        end;
        (* a click on a step edits it *)
        let s = int_of_float (Float.round ((mouse.mx - cell_x 0) / 30.)) in
        if mouse.mclick && s >= 0 && s < 16 && Float.abs (mouse.mx - cell_x s) <= 13. && Float.abs (mouse.my - y) <= 20. then
          match k with
          | 0 -> { p with bass1 = { p.bass1 with pattern = click_303 p.bass1.pattern s (mouse.my - y) } }
          | 1 -> { p with bass2 = { p.bass2 with pattern = click_303 p.bass2.pattern s (mouse.my - y) } }
          | 2 -> { p with drums808 = click_drum p.drums808 instruments.(k) s }
          | _ -> { p with drums909 = click_drum p.drums909 instruments.(k) s }
        else p)
      patch [ 0; 1; 2; 3 ]
  in
  (* the effects *)
  let distortion = Gui.knob computer ~at:(-400., -30.) ~from:0. ~to_:1. patch.distortion in
  let delay = Gui.knob computer ~at:(-320., -30.) ~from:0. ~to_:1. patch.delay in
  let compressor = Gui.rocker computer ~at:(-240., -30.) patch.compressor in
  let pcf_on = Gui.rocker computer ~at:(-160., -30.) patch.pcf_on in
  (* the PCF's bars: a click sets a step's cutoff *)
  let pcf = Array.copy patch.pcf in
  if mouse.mdown then
    Array.iteri
      (fun s _ ->
        if Float.abs (mouse.mx - pcf_x s) <= 13. && mouse.my >= pcf_bottom && mouse.my <= pcf_bottom + pcf_height then
          pcf.(s) <- (mouse.my - pcf_bottom) / pcf_height)
      pcf;
  let patch = { patch with distortion; delay; compressor; pcf_on; pcf } in
  Studio_rebirth.set_patch rebirth patch;
  { patch; song; patterns; instruments; space }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 30 30 30
let green = rgb 120 220 160

(* each machine's colours, the originals': the 303's silver, the 808's
 * black and orange, the 909's light grey *)
let colors (k : int) : color * color =
  match k with 0 | 1 -> (rgb 205 205 210, ink) | 2 -> (rgb 38 38 40, rgb 240 140 50) | _ -> (rgb 190 190 188, ink)

let led_on = rgb 255 60 40
let led_off = rgb 90 30 28
let playing = rgb 255 225 60

(* a rack unit's ears, a screw above and below at each end *)
let ears (y : number) (h : number) : shape list =
  List.concat_map
    (fun x ->
      [ rectangle (rgb 40 40 44) 22. h |> move x y ]
      @ List.map (fun dy -> circle (rgb 150 150 155) 4. |> move x (y + dy)) [ (h / 2.) - 12.; 12. - (h / 2.) ])
    [ -464.; 464. ]

(* the 303's steps as ReBirth's piano roll: each note a bar at its pitch
 * in the pattern's range, orange if accented, reaching into the next
 * step if it slides *)
let step_303 (pattern : Sequencer.step array) (y : number) (s : int) (lit : bool) : shape list =
  let range = pitch_range pattern in
  let st = pattern.(s mod Array.length pattern) in
  let x = cell_x s in
  [ rectangle (if lit then rgb 110 100 40 else rgb 45 45 48) 26. 40. |> move x y ]
  @
  match st.note with
  | None -> []
  | Some n ->
      let w = if st.slide then 30. else 20. in
      [ rectangle (if st.accent then rgb 240 140 50 else rgb 235 232 222) w 4. |> move (x + ((w - 20.) / 2.)) (y + pitch_y range n) ]

(* a drum machine's step: a button and its LED; the 808's buttons in
 * the colours of its groups of four, red, orange, yellow, cream; the
 * 909's all light, its LEDs above them *)
let step_drum (k : int) (y : number) (s : int) (hit : bool) (lit : bool) : shape list =
  let button = if k = 2 then [| rgb 205 55 45; rgb 235 120 40; rgb 235 200 60; rgb 235 230 215 |].(s /.. 4) else rgb 235 235 230 in
  [
    rectangle button 26. 28. |> move (cell_x s) (y - 6.);
    circle (if lit then playing else if hit then led_on else led_off) 4. |> move (cell_x s) (y + 16.);
  ]

let strips_view (m : model) : shape list =
  let steps = Studio_rebirth.steps rebirth and running = Studio_rebirth.running rebirth in
  List.concat
    (List.init 4 (fun k ->
         let y = strip_y k in
         let face, text = colors k in
         let machine = List.nth Studio_rebirth.machines k in
         let model = match k with 0 | 1 -> "Bass Line" | _ -> "Rhythm Composer" in
         (* the drum machines' name higher, their instrument button under it *)
         let dy = if k >= 2 then 10. else 0. in
         [
           rectangle face 950. 80. |> move 0. y;
           words text (Studio_rebirth.name machine) |> scale 1.5 |> move (-410.) (y + 8. + dy);
           words text model |> scale 0.8 |> move (-410.) (y - 20. + (2. * dy));
           words text (List.nth (pattern_names k) m.patterns.(k)) |> scale 1.1 |> move (-260.) y;
           words text "LEVEL" |> scale 0.9 |> move (-130.) (y - 30.);
           words text "MUTE" |> scale 0.9 |> move (-80.) (y - 30.);
         ]
         (* the 909's orange line under its steps *)
         @ (if k = 3 then [ rectangle (rgb 235 120 40) 480. 3. |> move (cell_x 0 + 225.) (y - 30.) ] else [])
         @ ears y 80.
         @
         let cells =
           List.concat
             (List.init 16 (fun s ->
                  let lit = running && steps.(k) = s in
                  match k with
                  | 0 -> step_303 m.patch.bass1.pattern y s lit
                  | 1 -> step_303 m.patch.bass2.pattern y s lit
                  | 2 -> step_drum k y s (step_on m.patch.drums808 m.instruments.(k) s) lit
                  | _ -> step_drum k y s (step_on m.patch.drums909 m.instruments.(k) s) lit))
         in
         (* a machine muted: its steps faded, said so -- edited, they are
          * heard once it is unmuted *)
         if m.patch.mutes.(k) then
           cells
           @ [ rectangle face 484. 76. |> fade 0.7 |> move (cell_x 0 + 225.) y; words (rgb 230 60 40) "MUTED" |> scale 2. |> move (cell_x 0 + 225.) y ]
         else cells))

let effects_view (m : model) : shape list =
  let label word x = words ink word |> scale 0.9 |> move x (-62.) in
  [ rectangle (rgb 205 205 210) 950. 150. |> move 0. (-40.) ]
  @ ears (-40.) 150.
  @ [ label "DIST" (-400.); label "DELAY" (-320.); label "COMP" (-240.); label "PCF" (-160.) ]
  @ [ words ink "PCF: the drums' filter, a cutoff a step" |> scale 1. |> move 180. 18. ]
  @ List.init 16 (fun s ->
        let h = m.patch.pcf.(s) * pcf_height in
        let lit = Studio_rebirth.running rebirth && (Studio_rebirth.steps rebirth).(2) = s in
        group
          [
            rectangle (rgb 235 232 222) 26. pcf_height |> move 0. (pcf_bottom + (pcf_height / 2.));
            rectangle (if lit then rgb 255 225 60 else if m.patch.pcf_on then rgb 60 60 60 else rgb 150 150 150) 26. (Float.max 2. h)
            |> move 0. (pcf_bottom + (h / 2.));
          ]
        |> move_x (pcf_x s))

(* a line from one point to another, [w] wide *)
let segment (c : color) (w : number) (x1, y1) (x2, y2) : shape =
  let dx = x2 - x1 and dy = y2 - y1 in
  rectangle c (sqrt ((dx * dx) + (dy * dy))) w |> rotate (atan2 dy dx * 180. / Float.pi) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

let spectrum_view (samples : Signal.t) : shape list =
  let cx = -160. and cy = -250. and w = 620. and h = 140. in
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
  let cx = 330. and cy = -250. and w = 300. and h = 140. in
  let points = 150 in
  let at i = samples.(Array.length samples -.. 1024 +.. (i *.. 1024 /.. points)) in
  (rectangle (rgb 20 25 20) w h |> move cx cy)
  :: List.init (points -.. 1) (fun i ->
         let x i = cx - (w / 2.) + (float_of_int i / float_of_int points * w) in
         let y i = cy + (Float.max (-1.) (Float.min 1. (at i * 2.)) * h / 2.) in
         segment green 2. (x i, y i) (x (i +.. 1), y (i +.. 1)))

let view (computer : computer) (m : model) : shape list =
  [ rectangle (rgb 60 60 65) computer.screen.width computer.screen.height ]
  @ [ words white "TinyReBirth" |> scale 2.4 |> move (-350.) 482.; words white "song" |> scale 1.5 |> move 240. 482. ]
  @ [ words (rgb 200 200 200) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move (-80.) 482. ]
  @ [ words white (Printf.sprintf "TEMPO %.0f" m.patch.tempo) |> scale 1. |> move (-300.) 412.; words white "VOLUME" |> scale 1. |> move (-220.) 412.;
      words (rgb 230 230 230) "RB-338  two 303s, an 808, a 909, one clock   space: start/stop" |> scale 1.1 |> move 150. 430. ]
  @ strips_view m @ effects_view m
  @ spectrum_view (Studio_rebirth.recent rebirth)
  @ scope_view (Studio_rebirth.recent rebirth)
  @ Gui.draw ()

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
