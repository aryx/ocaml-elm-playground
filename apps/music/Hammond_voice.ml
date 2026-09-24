(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Hammond_voice.mli *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

type patch = {
  drawbars : int array;
  percussion : bool;
  third : bool;
  fast : bool;
  soft : bool;
  click : float;
  vibrato : int;
  volume : float;
}

let vibratos = [ "off"; "V1"; "V2"; "V3"; "C1"; "C2"; "C3" ]
let footages = [ "16'"; "5 1/3'"; "8'"; "4'"; "2 2/3'"; "2'"; "1 3/5'"; "1 1/3'"; "1'" ]
let semitones = [ -12; 7; 0; 12; 19; 24; 28; 31; 36 ]

let registration (digits : string) (p : patch) : patch =
  { p with drawbars = Array.init 9 (fun i -> if i < String.length digits then max 0 (min 8 (Char.code digits.[i] - Char.code '0')) else 0) }

let of_registration (p : patch) : string = String.concat "" (Array.to_list (Array.map string_of_int p.drawbars))

let initial : patch =
  registration "888000000"
    { drawbars = [||]; percussion = false; third = true; fast = true; soft = false; click = 0.3; vibrato = 0; volume = 0.7 }

type knob = patch Patch_text.knob

(* the drawbars' names in the text: 16, 5-1/3, 8, ... *)
let drawbar_names = [ "16"; "5-1/3"; "8"; "4"; "2-2/3"; "2"; "1-3/5"; "1-1/3"; "1" ]
let levels = List.init 9 string_of_int

let knobs : knob list =
  List.mapi
    (fun i name ->
      Patch_text.selector ("drawbar." ^ name) levels
        (fun p -> p.drawbars.(i))
        (fun p x ->
          let d = Array.copy p.drawbars in
          d.(i) <- x;
          { p with drawbars = d }))
    drawbar_names
  @ [
      Patch_text.switch "percussion" (fun p -> p.percussion) (fun p x -> { p with percussion = x });
      Patch_text.switch "percussion.third" (fun p -> p.third) (fun p x -> { p with third = x });
      Patch_text.switch "percussion.fast" (fun p -> p.fast) (fun p x -> { p with fast = x });
      Patch_text.switch "percussion.soft" (fun p -> p.soft) (fun p x -> { p with soft = x });
      Patch_text.knob "click" (fun p -> p.click) (fun p x -> { p with click = x });
      Patch_text.selector "vibrato" vibratos (fun p -> p.vibrato) (fun p x -> { p with vibrato = x });
      Patch_text.knob "volume" (fun p -> p.volume) (fun p x -> { p with volume = x });
    ]

let to_string (p : patch) : string = Patch_text.to_string knobs p
let of_string (text : string) : (patch, string) result = Patch_text.of_string knobs ~initial text
let index_of x l = let rec find i = function [] -> 0 | y :: r -> if y = x then i else find (i + 1) r in find 0 l

let presets : (string * patch) list =
  [
    ("jazz", { (registration "888000000" initial) with percussion = true; third = true; fast = true; soft = true });
    ("full", registration "888888888" initial);
    ("gospel", { (registration "888808008" initial) with vibrato = index_of "C3" vibratos });
    ("ballad", { (registration "838000000" initial) with vibrato = index_of "C3" vibratos; click = 0.1 });
    ("flute", { (registration "008000000" initial) with vibrato = index_of "V2" vibratos; click = 0. });
  ]

let drawbar_gain (level : int) : float = if level <= 0 then 0. else Float.pow 10. (-3. *. float_of_int (8 - min 8 level) /. 20.)

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

let rate = float_of_int Signal.rate

(* ours: the contacts' ramp, the percussion's times to -60 dB and soft's
 * level, the click's length, the scanner's rate and depths *)
let contact = 0.002
let percussion_fast = 0.3
let percussion_slow = 1.2
let click_seconds = 0.004
let scanner_rate = 6.9
let scanner_depths = [| 0.; 0.0002; 0.00045; 0.0008 |] (* off, 1, 2, 3 *)

(* the scanner's delay line: 2 ms is enough for V3 *)
type scanner = { line : float array; mutable at : int; mutable phase : float }

type t = {
  mutable patch : patch;
  poly : Polyphony.t;
  mutable time : int; (* samples since the organ started: the wheels' clock *)
  mutable random : int; (* the clicks' noise *)
  scanner : scanner;
  mutable mixed : Signal.t;
}

let create (patch : patch) : t =
  {
    patch;
    poly = Polyphony.create ();
    time = 0;
    random = 1;
    scanner = { line = Array.make (Signal.samples 0.004) 0.; at = 0; phase = 0. };
    mixed = [||];
  }

let patch (t : t) : patch = t.patch
let set_patch (t : t) (p : patch) : unit = t.patch <- p
let voices (t : t) : int = Polyphony.voices t.poly

(* a wheel at the organ's clock, [time] samples in: its phase from the
 * clock alone, so every key sharing it is in step *)
let wheel (frequency : float) (time : int) : float =
  let turns = frequency *. float_of_int time /. rate in
  sin (2. *. Float.pi *. (turns -. Float.of_int (Float.to_int turns)))

(* the voice of [key]: its wheels and their levels, fixed at the key
 * (a drawbar pulled while a note sounds is heard from the next note:
 * ours, simpler than the organ, where it is heard at once) *)
let voice (t : t) (key : int) ~(percussion : bool) : Polyphony.voice =
  let p = t.patch in
  let partials =
    List.concat
      (List.mapi
         (fun i st ->
           let level = p.drawbars.(i) in
           (* the percussion takes the 1' drawbar's circuit *)
           if level = 0 || (p.percussion && i = 8) then []
           else [ (Tonewheel.frequency (Tonewheel.of_note (key + st)), drawbar_gain level) ])
         semitones)
  in
  let struck =
    if percussion then
      let f = Tonewheel.frequency (Tonewheel.of_note (key + if p.third then 19 else 12)) in
      let t60 = if p.fast then percussion_fast else percussion_slow in
      Some (f, (if p.soft then 0.5 else 1.), log 1000. /. t60)
    else None
  in
  let click = p.click in
  let age = ref 0 (* samples since the key *) and gate = ref 0. and held = ref true in
  let ramp = 1. /. (contact *. rate) in
  let fill (out : Signal.t) =
    for i = 0 to Array.length out - 1 do
      let now = t.time + i in
      gate := if !held then Float.min 1. (!gate +. ramp) else Float.max 0. (!gate -. ramp);
      let x = List.fold_left (fun s (f, g) -> s +. (g *. wheel f now)) 0. partials in
      let seconds = float_of_int !age /. rate in
      let x =
        match struck with None -> x | Some (f, level, k) -> x +. (level *. exp (-.k *. seconds) *. wheel f now)
      in
      let x =
        if click > 0. && seconds < click_seconds then begin
          t.random <- Noise.lcg t.random;
          x +. (click *. 2. *. Noise.uniform t.random *. (1. -. (seconds /. click_seconds)))
        end
        else x
      in
      out.(i) <- !gate *. x;
      incr age
    done
  in
  { release = (fun () -> held := false); fill; silent = (fun () -> (not !held) && !gate = 0.) }

(* the scanner: the sound read at a delay a triangle moves; a vibrato
 * alone, a chorus mixed with the dry *)
let scan (t : t) (s : Signal.t) : unit =
  let v = t.patch.vibrato in
  if v > 0 then begin
    let depth = scanner_depths.(((v - 1) mod 3) + 1) *. rate and chorus = v > 3 in
    let sc = t.scanner and n = Array.length t.scanner.line in
    Array.iteri
      (fun i x ->
        sc.line.(sc.at) <- x;
        let tri = (4. *. Float.abs (sc.phase -. 0.5)) -. 1. in
        let d = 1. +. depth +. (depth *. tri) in
        let whole = Float.to_int d in
        let frac = d -. float_of_int whole in
        let at k = sc.line.((sc.at - k + (2 * n)) mod n) in
        let y = ((1. -. frac) *. at whole) +. (frac *. at (whole + 1)) in
        s.(i) <- (if chorus then (x +. y) /. 2. else y);
        sc.at <- (sc.at + 1) mod n;
        sc.phase <- sc.phase +. (scanner_rate /. rate);
        if sc.phase >= 1. then sc.phase <- sc.phase -. 1.)
      s
  end

let fill (t : t) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  if Array.length t.mixed <> n then t.mixed <- Array.make n 0.;
  Polyphony.fill t.poly t.mixed;
  t.time <- t.time + n;
  scan t t.mixed;
  (* nine drawbars on a three-note chord peak near 1 at 0.1: at 0.05 the
   * full organ stays under clipping, and a flute registration is as
   * much quieter as the organ's own is *)
  let gain = 0.05 *. t.patch.volume in
  Array.iteri
    (fun i x ->
      out.left.(i) <- gain *. x;
      out.right.(i) <- gain *. x)
    t.mixed

let instrument (t : t) : Instrument.t =
  {
    note_on =
      (fun key _velocity ->
        (* single-trigger: the percussion only for a key pressed alone *)
        let percussion = t.patch.percussion && Polyphony.held t.poly = [] in
        Polyphony.press t.poly key (voice t key ~percussion));
    note_off = (fun key -> Polyphony.release t.poly key);
    set = (fun name x -> Option.iter (fun (k : knob) -> t.patch <- k.put t.patch x) (List.find_opt (fun (k : knob) -> k.name = name) knobs));
    fill = fill t;
  }
