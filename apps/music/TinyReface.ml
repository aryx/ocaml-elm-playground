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
 * modulator.
 *
 * Each face is a panel embedded as a part (Component.mli, the office's
 * idea for music, plan_tiny_reason.md), this program their host: the
 * YC's is TinyHammond's own panel, Part_hammond, scaled into the case
 * (the mouse mapped back into it, so its drawbars still pull); the
 * others, until they have panels of their own, the grid of their
 * voice's knobs by name, Part_voice (Voice.mli: every voice gives its
 * knobs the same way). The TYPE menu is the panel's own menu, merged
 * into the case's, as a document's menu bar takes the active part's.
 *
 * The letters play (a s d f g h j k the white keys from C, w e t y u
 * the black ones, z and x an octave), and the mouse on the keys.
 *
 * Uses: Voice_hammond, Voice_rhodes, Voice_dx7, Voice_cs80 (the
 * voices), Part_hammond and Part_voice (their panels, over Panel's
 * widgets), Component, Piano, Meters (the scope), Audio's instruments
 * (one playing at a time: the face left is stopped), Gui (the switch,
 * the menu). Not: Spectrum, Scene2d, Sprite, File_menu.
 *
 * Exercises: the Reface CP's effects row, audio/effects' Drive,
 * Modulated_delay, Delay and Reverb in a Rack after the voice; the YC's
 * transistor organs (dividers and square waves: a subtractive organ
 * beside the additive one); the DX's four operators; the CS's
 * oscillator types; the Reface's looper (TinyOp1's Tape, one track).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The faces *)
(*****************************************************************************)

(* a face: a voice behind its panel, a part (Component.mli) *)
type face = {
  name : string; (* "YC" *)
  what : string;
  color : color;
  panel : Component.part; (* as made; the model has it as it is now *)
  inst : Instrument.t;
  recent : unit -> Signal.t;
}

(* the YC's is TinyHammond's own panel, Part_hammond, scaled into the
 * case; the others the grid of their knobs, Part_voice, until they have
 * a panel of their own *)
let yc () : face =
  let v = Voice_hammond.create (snd (List.hd Voice_hammond.presets)) in
  {
    name = "YC";
    what = "combo organ: the Hammond B-3 and its Leslie";
    color = rgb 225 90 50;
    panel = Part_hammond.make v;
    inst = Voice_hammond.instrument v;
    recent = (fun () -> Voice_hammond.recent v);
  }

let cp () : face =
  let v = Voice_rhodes.create (snd (List.hd Voice_rhodes.presets)) and color = rgb 210 60 70 in
  let voice : Voice_rhodes.patch Part_voice.voice =
    { knobs = Voice_rhodes.knobs; presets = Voice_rhodes.presets; patch = (fun () -> Voice_rhodes.patch v); set_patch = Voice_rhodes.set_patch v;
      to_string = Voice_rhodes.to_string; of_string = Voice_rhodes.of_string }
  in
  {
    name = "CP";
    what = "electric piano: the Rhodes, the Wurlitzer, the Clavinet";
    color;
    panel =
      Part_voice.make ~kind:"rhodes" ~color voice
        [ ("MODEL", "model"); ("VOICING", "voicing"); ("HARDNESS", "hardness"); ("DECAY", "decay"); ("TREMOLO", "tremolo.depth"); ("RATE", "tremolo.rate"); ("VOLUME", "volume") ];
    inst = Voice_rhodes.instrument v;
    recent = (fun () -> Voice_rhodes.recent v);
  }

let dx () : face =
  let v = Voice_dx7.create (snd (List.hd Voice_dx7.presets)) and color = rgb 60 120 210 in
  let voice : Voice_dx7.patch Part_voice.voice =
    { knobs = Voice_dx7.knobs; presets = Voice_dx7.presets; patch = (fun () -> Voice_dx7.patch v); set_patch = Voice_dx7.set_patch v;
      to_string = Voice_dx7.to_string; of_string = Voice_dx7.of_string }
  in
  {
    name = "DX";
    what = "FM: the DX7's six operators";
    color;
    panel =
      Part_voice.make ~kind:"dx7" ~color voice
        ([ ("ALGO", "algorithm"); ("FB", "feedback") ] @ List.init 6 (fun k -> (Printf.sprintf "OP%d" (k +.. 1), Printf.sprintf "op%d.output" (k +.. 1))));
    inst = Voice_dx7.instrument v;
    recent = (fun () -> Voice_dx7.recent v);
  }

let cs () : face =
  let v = Voice_cs80.create (snd (List.hd Voice_cs80.presets)) and color = rgb 70 160 90 in
  let voice : Voice_cs80.patch Part_voice.voice =
    { knobs = Voice_cs80.knobs; presets = Voice_cs80.presets; patch = (fun () -> Voice_cs80.patch v); set_patch = Voice_cs80.set_patch v;
      to_string = Voice_cs80.to_string; of_string = Voice_cs80.of_string }
  in
  {
    name = "CS";
    what = "virtual analog: the CS-80's two layers";
    color;
    panel =
      Part_voice.make ~kind:"cs80" ~color voice
        [
          ("CUTOFF", "I.lpf"); ("RESO", "I.lpf_res"); ("ATTACK", "I.attack"); ("DECAY", "I.decay"); ("SUSTAIN", "I.sustain"); ("RELEASE", "I.release");
          ("MIX", "mix"); ("DETUNE", "detune"); ("LFO", "sub.speed"); ("VIBRATO", "sub.vco"); ("CHORUS", "chorus"); ("VOLUME", "volume");
        ];
    inst = Voice_cs80.instrument v;
    recent = (fun () -> Voice_cs80.recent v);
  }

(* the four, each made when first shown *)
let faces : face Lazy.t array = [| lazy (yc ()); lazy (cp ()); lazy (dx ()); lazy (cs ()) |]

type model = {
  face : int;
  panels : Component.part option array; (* each face's panel as it is now, once shown *)
  presets : int array;
  piano : Piano.t;
}

let initial_model : model = { face = 0; panels = Array.make 4 None; presets = Array.make 4 0; piano = Piano.initial ~octave:4 }
let current (m : model) : face = Lazy.force faces.(m.face)
let panel (m : model) : Component.part = match m.panels.(m.face) with Some p -> p | None -> (current m).panel

(* the room over the keys, a panel as big as it fits, centred *)
let panel_box (p : Component.part) : Widget.box =
  let w, h = Part_voice.natural in
  match p.natural with
  | Some (nw, nh) ->
      let s = Float.min (w / nw) (h / nh) in
      { x = 0.; y = 235.; w = nw * s; h = nh * s }
  | None -> { x = 0.; y = 235.; w; h }

(*****************************************************************************)
(* The keyboard: 37 mini keys, C to C *)
(*****************************************************************************)

(* the keyboard starts an octave under the letters' *)
let look : Piano.look =
  {
    keys = 37;
    left = -440.;
    top = 60.;
    white_width = 40.;
    white_height = 200.;
    black_height = 120.;
    letters_from = 12;
    velocity = 0.8;
    octaves = (2, 6);
    white_key = rgb 245 245 242;
    black_key = rgb 30 30 32;
    letter_on_white = rgb 140 140 140;
    letter_scale = 1.1;
    letter_lift = 14.;
  }

(*****************************************************************************)
(* update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  Gui.set_theme Theme.default;
  (* the switch's names apart: a face is made only when chosen *)
  let chosen = List.fold_left (fun c k -> if Gui.button computer ~at:(-100. + (float_of_int k * 70.), 420.) (List.nth [ "YC"; "CP"; "DX"; "CS" ] k) then k else c) m.face [ 0; 1; 2; 3 ] in
  (* one voice playing: the face left stopped, its notes with it *)
  if chosen <> m.face then Audio.stop ("reface" ^ (current m).name);
  let m = { m with face = chosen } in
  let f = current m in
  ignore (Audio.instrument ("reface" ^ f.name) (fun () -> f.inst));
  (* the TYPE menu: the panel's presets, its menu (the office's menu
   * merging) *)
  let p = panel m in
  let names = List.tl p.menu in
  let preset = Gui.menu computer ~at:(360., 420.) names m.presets.(m.face) in
  let p = if preset <> m.presets.(m.face) then p.command (List.nth names preset) else p in
  let presets = Array.copy m.presets in
  presets.(m.face) <- preset;
  (* the panel, unless the menu has the mouse *)
  let p = if Gui.modal () then p else Component.input_in ~scaled:true p computer (panel_box p) in
  let panels = Array.copy m.panels in
  panels.(m.face) <- Some p;
  { m with panels; presets; piano = Piano.update look computer m.piano f.inst }

(*****************************************************************************)
(* view *)
(*****************************************************************************)

let ink = rgb 30 30 30

let view (computer : computer) (m : model) : shape list =
  let f = current m and p = panel m in
  [ rectangle (rgb 60 60 64) computer.screen.width computer.screen.height ]
  @ [ words white "TinyReface" |> scale 2.4 |> move (-370.) 482.; words (rgb 200 200 200) (Printf.sprintf "latency %.0f ms" (Audio.latency () * 1000.)) |> scale 1.3 |> move 0. 482. ]
  (* the case, the face's colour a stripe *)
  @ [ rectangle (rgb 215 215 212) 960. 570. |> move 0. 100.; rectangle f.color 960. 10. |> move 0. 385. ]
  @ [ words f.color ("reface " ^ f.name) |> scale 2. |> move (-380.) 420.; words ink f.what |> scale 1.1 |> move (-380.) 395. |> move_x 120.; words ink "TYPE" |> scale 1. |> move 270. 420. ]
  @ Component.draw_in ~scaled:true p (panel_box p) ~active:true
  @ Piano.view look computer m.piano ~lit:f.color
  @ Meters.scope ~at:(0., -270.) ~size:(600., 90.) ~points:200 ~color:f.color ~back:(rgb 20 22 20) (f.recent ())
  @ [ words (rgb 220 220 220) "letters: play (z x an octave)   the mouse: the keys" |> scale 1.1 |> move 0. (-340.) ]
  @ Gui.draw ()

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
