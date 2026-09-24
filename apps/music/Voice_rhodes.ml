(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Voice_rhodes.mli *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

type patch = {
  model : int;
  voicing : float;
  hardness : float;
  decay : float;
  tremolo_rate : float;
  tremolo_depth : float;
  volume : float;
}

let models = [ "Rhodes Mark I"; "Wurlitzer 200A"; "Clavinet D6" ]

let initial : patch =
  { model = 0; voicing = 0.5; hardness = 0.4; decay = 0.6; tremolo_rate = 0.4; tremolo_depth = 0.; volume = 0.7 }

type knob = patch Patch_text.knob

let knobs : knob list =
  [
    Patch_text.selector "model" models (fun p -> p.model) (fun p x -> { p with model = x });
    Patch_text.knob "voicing" (fun p -> p.voicing) (fun p x -> { p with voicing = x });
    Patch_text.knob "hardness" (fun p -> p.hardness) (fun p x -> { p with hardness = x });
    Patch_text.knob "decay" (fun p -> p.decay) (fun p x -> { p with decay = x });
    Patch_text.knob "tremolo.rate" (fun p -> p.tremolo_rate) (fun p x -> { p with tremolo_rate = x });
    Patch_text.knob "tremolo.depth" (fun p -> p.tremolo_depth) (fun p x -> { p with tremolo_depth = x });
    Patch_text.knob "volume" (fun p -> p.volume) (fun p x -> { p with volume = x });
  ]

let to_string (p : patch) : string = Patch_text.to_string knobs p
let of_string (text : string) : (patch, string) result = Patch_text.of_string knobs ~initial text

let presets : (string * patch) list =
  [
    ("mark I", initial);
    ("bark", { initial with voicing = 0.85; hardness = 0.9 });
    ("suitcase", { initial with voicing = 0.4; hardness = 0.3; tremolo_rate = 0.35; tremolo_depth = 0.8 });
    ("wurlitzer", { initial with model = 1; voicing = 0.5; hardness = 0.5; decay = 0.4; tremolo_rate = 0.5; tremolo_depth = 0.3 });
    ("clavinet", { initial with model = 2; hardness = 0.7; decay = 0.3 });
  ]

(*****************************************************************************)
(* The pickups *)
(*****************************************************************************)

let tine_modes = [ 1.; 6.27; 17.55 ]

(* the Rhodes': a bell of the tip's position, its peak [offset] away *)
let offset (voicing : float) : float = 0.1 +. (0.8 *. voicing)

let pickup ~(voicing : float) (x : float) : float =
  let d = x -. offset voicing in
  1. /. (1. +. (d *. d))

(* the Wurlitzer's: the reed [x] of the gap nearer the plate *)
let capacitance (x : float) : float = 1. /. (1. -. Float.min 0.95 x)

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

let rate = float_of_int Signal.rate
let frequency (key : int) : float = 440. *. Float.pow 2. (float_of_int (key - 69) /. 12.)

(* ours: how long the fundamental rings (-60 dB) at [f], longer low *)
let ring (p : patch) (f : float) : float = Float.max 0.5 ((1.5 +. (8. *. p.decay)) *. sqrt (110. /. f))

(* the felt, the yarn: -60 dB in these once the key is up *)
let damper = 0.12

(* a tine or a reed: its modes, struck, and a pickup reading their
 * sum; [bell] the pickup's curve, [swing] how far the tip goes *)
let struck (p : patch) (key : int) (velocity : float) : Polyphony.voice =
  let f = frequency key in
  let wurlitzer = p.model = 1 in
  let modes =
    if wurlitzer then [ (f, ring p f *. 0.6, 1.); (6.27 *. f, 0.15, 0.05 *. p.hardness) ]
    else
      (* the upper modes' motion small: the pickup, reading its rate of
       * change, makes them 6.27 and 17.55 times louder *)
      [ (f, ring p f, 1.); (6.27 *. f, 0.4 *. sqrt (110. /. f), 0.02 +. (0.08 *. p.hardness)); (17.55 *. f, 0.1, 0.015 *. p.hardness) ]
  in
  let modes =
    List.filter_map
      (fun (freq, t60, share) ->
        if freq > 0.45 *. rate then None
        else begin
          let m = Modal.create ~frequency:freq ~t60 in
          (* the upper modes' share grows with the velocity: the
           * hammer's contact shorter the harder it hits *)
          Modal.strike m (velocity *. if freq = f then share else share *. velocity);
          Some m
        end)
      modes
  in
  let swing = if wurlitzer then 0.4 +. (0.5 *. p.voicing) else 1.8 in
  let read x = if wurlitzer then capacitance (swing *. x) else pickup ~voicing:p.voicing (swing *. x) in
  (* the derivative scaled by the period, so every key is as loud *)
  let scale = rate /. (2. *. Float.pi *. f) in
  let last = ref (read 0.) and held = ref true in
  let fill (out : Signal.t) =
    for i = 0 to Array.length out - 1 do
      let x = List.fold_left (fun s m -> s +. Modal.next m) 0. modes in
      let v = read x in
      out.(i) <- (v -. !last) *. scale;
      last := v
    done
  in
  let release () =
    held := false;
    List.iter (fun m -> Modal.damp m ~t60:damper) modes
  in
  { release; fill; silent = (fun () -> (not !held) && List.for_all (fun m -> Modal.level m < 1e-4) modes) }

(* the Clavinet: a string, rendered ahead as Pluck does, a fade when
 * the key comes up *)
let plucked (p : patch) (key : int) (velocity : float) : Polyphony.voice =
  let seconds = 1. +. (4. *. p.decay) in
  let s = Pluck.render ~decay:(0.99 +. (0.009 *. p.decay)) ~frequency:(frequency key) seconds in
  let at = ref 0 and gain = ref velocity and fade = ref 1. in
  let fill (out : Signal.t) =
    for i = 0 to Array.length out - 1 do
      out.(i) <- (if !at < Array.length s then !gain *. s.(!at) else 0.);
      gain := !gain *. !fade;
      incr at
    done
  in
  let release () = fade := Float.pow 10. (-3. /. (damper *. rate)) in
  { release; fill; silent = (fun () -> !at >= Array.length s || !gain < 1e-4) }

type t = {
  mutable patch : patch;
  poly : Polyphony.t;
  mutable phase : float; (* the tremolo's *)
  mutable mono : Signal.t;
  ring : Signal.t;
  mutable at : int;
}

let create (patch : patch) : t = { patch; poly = Polyphony.create (); phase = 0.; mono = [||]; ring = Array.make 2048 0.; at = 0 }
let patch (t : t) : patch = t.patch
let set_patch (t : t) (p : patch) : unit = t.patch <- p
let voices (t : t) : int = Polyphony.voices t.poly
let recent (t : t) : Signal.t = Array.init 2048 (fun i -> t.ring.((t.at + i) mod 2048))
let pan (t : t) : float = sin (2. *. Float.pi *. t.phase)

(* ours: each model's level, a four-note chord and a line at full
 * peaking under 1 (Unit_rhodes) *)
let gains = [| 0.15; 0.15; 0.3 |]

let fill (t : t) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  if Array.length t.mono <> n then t.mono <- Array.make n 0.;
  Polyphony.fill t.poly t.mono;
  let p = t.patch in
  let gain = gains.(p.model) *. p.volume and hz = 1. +. (9. *. p.tremolo_rate) and depth = p.tremolo_depth in
  for i = 0 to n - 1 do
    let s = sin (2. *. Float.pi *. t.phase) in
    t.phase <- Float.rem (t.phase +. (hz /. rate)) 1.;
    let x = gain *. t.mono.(i) in
    (* the Suitcase: the sound between its speakers; the others' tremolo
     * the loudness *)
    let l, r =
      if p.model = 0 then (1. -. (depth *. (1. +. s) /. 2.), 1. -. (depth *. (1. -. s) /. 2.))
      else
        let a = 1. -. (depth *. (1. +. s) /. 2.) in
        (a, a)
    in
    out.left.(i) <- l *. x;
    out.right.(i) <- r *. x;
    t.ring.(t.at) <- out.left.(i);
    t.at <- (t.at + 1) mod 2048
  done

let instrument (t : t) : Instrument.t =
  {
    note_on =
      (fun key velocity ->
        let voice = if t.patch.model = 2 then plucked t.patch key velocity else struck t.patch key velocity in
        Polyphony.press t.poly key voice);
    note_off = (fun key -> Polyphony.release t.poly key);
    set = (fun name x -> Option.iter (fun (k : knob) -> t.patch <- k.put t.patch x) (List.find_opt (fun (k : knob) -> k.name = name) knobs));
    fill = fill t;
  }
