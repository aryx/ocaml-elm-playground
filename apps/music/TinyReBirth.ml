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
 * level and its mute, its 16 steps drawn (the 303's notes, the drums'
 * hits), the step playing lit: the four lit together, the one clock.
 * Over them start/stop (and space), the tempo, the volume; under them
 * the effects: the distortion on the 303s, the delay, the compressor,
 * and the PCF, the pattern controlled filter, its 16 cutoffs bars to
 * click. Editing a pattern in depth is TinyTB303's and TinyTR808's job;
 * this one is the whole: which patterns, how loud, through what.
 *
 * Uses: Studio_rebirth (the four machines on one clock), Voice_tb303,
 * Voice_tr808, Drive, Delay, Dynamics, Svf, Audio's instruments, Gui
 * (the knobs, the rockers, the buttons, the menu), Spectrum. Not:
 * Scene2d, Sprite, File_menu.
 *
 * Exercises: the song mode (patterns chained, ReBirth's own); a pattern
 * edited here; each machine's own effects sends; ReBirth's .rbs songs
 * saved (File_menu, the voices' text).
 *)
open Playground
open Basics (* float arithmetics *)

type model = {
  patch : Studio_rebirth.patch;
  song : int;
  patterns : int array; (* each machine's pattern, an index in its presets *)
  space : bool;
}

let songs = Studio_rebirth.songs

(* each machine's pattern in a song, found among its presets *)
let patterns_of (p : Studio_rebirth.patch) : int array =
  let find x l = let rec go i = function [] -> 0 | (_, y) :: r -> if y = x then i else go (i +.. 1) r in go 0 l in
  [| find p.bass1 Voice_tb303.presets; find p.bass2 Voice_tb303.presets; find p.drums808 Voice_tr808.presets; find p.drums909 Voice_tr808.presets |]

let initial_model : model = { patch = snd (List.hd songs); song = 0; patterns = patterns_of (snd (List.hd songs)); space = false }

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

(* a step's cell: lit if the machine plays on it *)
let step_on (p : Studio_rebirth.patch) (k : int) (s : int) : bool =
  match k with
  | 0 -> p.bass1.pattern.(s mod Array.length p.bass1.pattern).note <> None
  | 1 -> p.bass2.pattern.(s mod Array.length p.bass2.pattern).note <> None
  | 2 -> Array.exists (fun t -> t.(s)) p.drums808.tracks
  | _ -> Array.exists (fun t -> t.(s)) p.drums909.tracks

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
        { p with levels; mutes })
      patch [ 0; 1; 2; 3 ]
  in
  (* the effects *)
  let distortion = Gui.knob computer ~at:(-400., -30.) ~from:0. ~to_:1. patch.distortion in
  let delay = Gui.knob computer ~at:(-320., -30.) ~from:0. ~to_:1. patch.delay in
  let compressor = Gui.rocker computer ~at:(-240., -30.) patch.compressor in
  let pcf_on = Gui.rocker computer ~at:(-160., -30.) patch.pcf_on in
  (* the PCF's bars: a click sets a step's cutoff *)
  let mouse = computer.mouse in
  let pcf = Array.copy patch.pcf in
  if mouse.mdown then
    Array.iteri
      (fun s _ ->
        if Float.abs (mouse.mx - pcf_x s) <= 13. && mouse.my >= pcf_bottom && mouse.my <= pcf_bottom + pcf_height then
          pcf.(s) <- (mouse.my - pcf_bottom) / pcf_height)
      pcf;
  let patch = { patch with distortion; delay; compressor; pcf_on; pcf } in
  Studio_rebirth.set_patch rebirth patch;
  { patch; song; patterns; space }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 30 30 30
let green = rgb 120 220 160

(* each machine's colours: the 303's silver, the 808's dark, the 909's
 * grey *)
let colors (k : int) : color * color =
  match k with 0 | 1 -> (rgb 205 205 210, ink) | 2 -> (rgb 50 50 50, rgb 235 230 215) | _ -> (rgb 150 150 152, rgb 30 30 30)

let strips_view (m : model) : shape list =
  let steps = Studio_rebirth.steps rebirth and running = Studio_rebirth.running rebirth in
  List.concat
    (List.init 4 (fun k ->
         let y = strip_y k in
         let face, text = colors k in
         let machine = List.nth Studio_rebirth.machines k in
         [
           rectangle face 950. 80. |> move 0. y;
           words text (Studio_rebirth.name machine) |> scale 1.5 |> move (-420.) y;
           words text (List.nth (pattern_names k) m.patterns.(k)) |> scale 1.1 |> move (-260.) y;
           words text "LEVEL" |> scale 0.9 |> move (-130.) (y - 30.);
           words text "MUTE" |> scale 0.9 |> move (-80.) (y - 30.);
         ]
         @ List.init 16 (fun s ->
               let lit = running && steps.(k) = s in
               (* a hit in the machine's colour, the step playing yellow *)
               let on = match k with 0 | 1 -> rgb 60 60 60 | 2 -> rgb 205 55 45 | _ -> rgb 235 120 40 in
               let c = if lit then rgb 255 225 60 else if step_on m.patch k s then on else rgb 235 232 222 in
               rectangle c 26. 40. |> move (cell_x s) y)))

let effects_view (m : model) : shape list =
  let label word x = words ink word |> scale 0.9 |> move x (-62.) in
  [ rectangle (rgb 205 205 210) 950. 150. |> move 0. (-40.) ]
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
let main = Playground_platform.run_app app
