(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Studio_rebirth.mli *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

type machine = Bass1 | Bass2 | Drums808 | Drums909

let machines = [ Bass1; Bass2; Drums808; Drums909 ]
let name = function Bass1 -> "303 #1" | Bass2 -> "303 #2" | Drums808 -> "808" | Drums909 -> "909"
let index = function Bass1 -> 0 | Bass2 -> 1 | Drums808 -> 2 | Drums909 -> 3

type patch = {
  bass1 : Voice_tb303.patch;
  bass2 : Voice_tb303.patch;
  drums808 : Voice_tr808.patch;
  drums909 : Voice_tr808.patch;
  levels : float array;
  mutes : bool array;
  distortion : float;
  delay : float;
  compressor : bool;
  pcf : float array;
  pcf_on : bool;
  tempo : float;
  volume : float;
}

let tb303 name = List.assoc name Voice_tb303.presets
let tr808 name = List.assoc name Voice_tr808.presets

let initial : patch =
  {
    bass1 = tb303 "acid";
    bass2 = tb303 "bass";
    drums808 = tr808 "electro";
    drums909 = tr808 "909 techno";
    levels = [| 0.8; 0.7; 0.8; 0.8 |];
    mutes = [| false; false; false; true |];
    distortion = 0.;
    delay = 0.;
    compressor = true;
    pcf = Array.init 16 (fun k -> if k mod 4 = 0 then 1. else 0.4 +. (0.15 *. float_of_int (k mod 4)));
    pcf_on = false;
    tempo = 130.;
    (* ours: the songs peaking at 0.8 at most (Unit_rebirth) *)
    volume = 0.6;
  }

(* ours: three songs in the styles ReBirth was made for *)
let songs : (string * patch) list =
  [
    (* the two lines against the 808, the distortion on them *)
    ("acid", { initial with distortion = 0.4; delay = 0.2 });
    (* the 909, the accents line, the bass under; the PCF on the drums *)
    ( "techno",
      { initial with bass1 = tb303 "accents"; mutes = [| false; false; true; false |]; pcf_on = true; tempo = 132.; delay = 0.15 } );
    (* the 909's house, the bass alone, the delay wide *)
    ( "house",
      { initial with drums909 = tr808 "909 house"; mutes = [| true; false; true; false |]; delay = 0.3; tempo = 122. } );
  ]

let pcf_hz (k : float) : float = 200. *. Float.pow 100. k

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

type t = {
  mutable patch : patch;
  basses : Voice_tb303.t array;
  drums : Voice_tr808.t array;
  instruments : Instrument.t array; (* by [index] *)
  drive : Drive.t;
  pcf_filter : Svf.t;
  echo : Delay.t;
  comp : Dynamics.t;
  mutable buffers : Signal.stereo array;
  ring : Signal.t;
  mutable at : int;
}

(* each machine given the one tempo *)
let with_tempo (p : patch) : patch =
  {
    p with
    bass1 = { p.bass1 with bpm = p.tempo };
    bass2 = { p.bass2 with bpm = p.tempo };
    drums808 = { p.drums808 with tempo = p.tempo; machine = 0 };
    drums909 = { p.drums909 with tempo = p.tempo; machine = 1 };
  }

let create (patch : patch) : t =
  let patch = with_tempo patch in
  let basses = [| Voice_tb303.create patch.bass1; Voice_tb303.create patch.bass2 |] in
  let drums = [| Voice_tr808.create patch.drums808; Voice_tr808.create patch.drums909 |] in
  {
    patch;
    basses;
    drums;
    instruments = Array.append (Array.map Voice_tb303.instrument basses) (Array.map Voice_tr808.instrument drums);
    drive = Drive.create ~oversampling:2 ();
    pcf_filter = Svf.create ();
    echo = Delay.create ();
    comp = Dynamics.create ();
    buffers = [||];
    ring = Array.make 2048 0.;
    at = 0;
  }

let patch (t : t) : patch = t.patch

let set_patch (t : t) (p : patch) : unit =
  let p = with_tempo p in
  Voice_tb303.set_patch t.basses.(0) p.bass1;
  Voice_tb303.set_patch t.basses.(1) p.bass2;
  Voice_tr808.set_patch t.drums.(0) p.drums808;
  Voice_tr808.set_patch t.drums.(1) p.drums909;
  t.patch <- p

(* all four at once: in the same update, so their sequencers start at
 * the same next sample *)
let run (t : t) (on : bool) : unit =
  Array.iter (fun b -> Voice_tb303.run b on) t.basses;
  Array.iter (fun d -> Voice_tr808.run d on) t.drums

let running (t : t) : bool = Voice_tr808.running t.drums.(0)
let steps (t : t) : int array = [| Voice_tb303.step t.basses.(0); Voice_tb303.step t.basses.(1); Voice_tr808.step t.drums.(0); Voice_tr808.step t.drums.(1) |]
let bass (t : t) (k : int) : Voice_tb303.t = t.basses.(k)
let drums (t : t) (k : int) : Voice_tr808.t = t.drums.(k)
let recent (t : t) : Signal.t = Array.init 2048 (fun i -> t.ring.((t.at + i) mod 2048))

(* the delay three sixteenths, its echoes darkened, from side to side *)
let delay_settings (p : patch) : Delay.settings =
  { time = 3. *. 60. /. (p.tempo *. 4.); feedback = 0.4; tone = 3000.; ping_pong = true; mix = p.delay }

let fill (t : t) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  let p = t.patch in
  if Array.length t.buffers <> 4 || Array.length t.buffers.(0).left <> n then
    t.buffers <- Array.init 4 (fun _ -> { Signal.left = Array.make n 0.; right = Array.make n 0. });
  (* every machine runs, muted or not: their clocks go on *)
  Array.iteri (fun k (i : Instrument.t) -> i.fill t.buffers.(k)) t.instruments;
  let level k = if p.mutes.(k) then 0. else p.levels.(k) in
  let bass = Array.init n (fun i -> (level 0 *. t.buffers.(0).left.(i)) +. (level 1 *. t.buffers.(1).left.(i))) in
  if p.distortion > 0. then Drive.process t.drive Tanh ~drive:(1. +. (20. *. p.distortion)) ~mix:1. bass;
  let drums = Array.init n (fun i -> (level 2 *. t.buffers.(2).left.(i)) +. (level 3 *. t.buffers.(3).left.(i))) in
  (* the PCF: the drums' low-pass, its cutoff the step's *)
  if p.pcf_on then begin
    let step = Voice_tr808.step t.drums.(0) in
    Svf.process t.pcf_filter Zero_delay Low_pass ~cutoff:(Array.make n (pcf_hz p.pcf.(step))) ~q:1.2 drums
  end;
  for i = 0 to n - 1 do
    let x = bass.(i) +. drums.(i) in
    out.left.(i) <- x;
    out.right.(i) <- x
  done;
  if p.delay > 0. then Delay.process t.echo (delay_settings p) out;
  if p.compressor then Dynamics.process t.comp { Dynamics.compressor with makeup = 6. } out;
  for i = 0 to n - 1 do
    out.left.(i) <- p.volume *. out.left.(i);
    out.right.(i) <- p.volume *. out.right.(i);
    t.ring.(t.at) <- out.left.(i);
    t.at <- (t.at + 1) mod 2048
  done

(* a hub: nothing played from the keys (the panel strikes the machines) *)
let instrument (t : t) : Instrument.t = { note_on = (fun _ _ -> ()); note_off = (fun _ -> ()); set = (fun _ _ -> ()); fill = fill t }
