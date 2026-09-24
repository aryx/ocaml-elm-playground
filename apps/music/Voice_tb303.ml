(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Voice_tb303.mli *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

type patch = {
  tuning : float;
  cutoff : float;
  resonance : float;
  env_mod : float;
  decay : float;
  accent : float;
  square : bool;
  bpm : float;
  volume : float;
  pattern : Sequencer.step array;
  locks : int;
  smoothing : float;
}

let cutoff_hz (k : float) : float = 100. *. Float.pow 25. k
let decay_seconds (k : float) : float = 0.2 *. Float.pow 10. k
let resonance_k (k : float) : float = 0.9 *. 22.1 *. k
let env_octaves (k : float) : float = 1. +. (4. *. k)

(* the patterns as text: a step a word *)
let names = [| "C"; "C#"; "D"; "Eb"; "E"; "F"; "F#"; "G"; "Ab"; "A"; "Bb"; "B" |]

(* a step's locks: "[cutoff=0.8,decay=0.2]", in the order locked *)
let locks_to_string (l : (string * float) list) : string =
  if l = [] then "" else "[" ^ String.concat "," (List.rev_map (fun (k, v) -> Printf.sprintf "%s=%g" k v) l) ^ "]"

let step_to_string (s : Sequencer.step) : string =
  (match s.note with
  | None -> "."
  | Some n -> names.(n mod 12) ^ string_of_int ((n / 12) - 1) ^ (if s.accent then "*" else "") ^ if s.slide then "~" else "")
  ^ locks_to_string s.locks

let pattern_to_string (p : Sequencer.step array) : string = String.concat " " (Array.to_list (Array.map step_to_string p))

let locks_of_string (text : string) : ((string * float) list, string) result =
  List.fold_left
    (fun acc pair ->
      match (acc, String.split_on_char '=' pair) with
      | Ok l, [ k; v ] -> (
          match float_of_string_opt v with Some x -> Ok ((String.trim k, x) :: l) | None -> Error ("not a lock: " ^ pair))
      | Ok _, _ -> Error ("not a lock: " ^ pair)
      | e, _ -> e)
    (Ok []) (String.split_on_char ',' text)

let step_of_string (w : string) : (Sequencer.step, string) result =
  (* the locks apart, in brackets at the end *)
  let w, locks =
    match String.index_opt w '[' with
    | Some i when w.[String.length w - 1] = ']' -> (String.sub w 0 i, locks_of_string (String.sub w (i + 1) (String.length w - i - 2)))
    | _ -> (w, Ok [])
  in
  let with_locks (s : Sequencer.step) = Result.map (fun locks -> { s with locks }) locks in
  if w = "." || w = "-" then with_locks Sequencer.rest
  else
    let strip c s = if String.contains s c then (true, String.concat "" (String.split_on_char c s)) else (false, s) in
    let accent, w' = strip '*' w in
    let slide, w' = strip '~' w' in
    match Music.midi_number w' with Some n -> with_locks (Sequencer.note ~accent ~slide n) | None -> Error ("not a step: " ^ w)

let pattern_of_string (text : string) : (Sequencer.step array, string) result =
  let words = List.filter (fun w -> w <> "") (String.split_on_char ' ' (String.trim text)) in
  List.fold_right
    (fun w acc -> match (acc, step_of_string w) with Ok l, Ok s -> Ok (s :: l) | (Error _ as e), _ -> e | _, Error e -> Error e)
    words (Ok [])
  |> Result.map Array.of_list

let parse (text : string) : Sequencer.step array =
  match pattern_of_string text with Ok p -> p | Error e -> failwith ("Voice_tb303: " ^ e)

let initial : patch =
  {
    tuning = 0.;
    cutoff = 0.35;
    resonance = 0.6;
    env_mod = 0.5;
    decay = 0.4;
    accent = 0.6;
    square = false;
    bpm = 125.;
    volume = 0.7;
    pattern = parse "C2 C2 C3* C2 . Eb2 C2~ G2* . C2 Bb1~ C2* C2 . F2* Eb2~";
    locks = 0;
    smoothing = 0.5;
  }

type knob = patch Patch_text.knob

let knobs : knob list =
  [
    Patch_text.detune "tuning" (fun p -> p.tuning) (fun p x -> { p with tuning = x });
    Patch_text.knob "cutoff" (fun p -> p.cutoff) (fun p x -> { p with cutoff = x });
    Patch_text.knob "resonance" (fun p -> p.resonance) (fun p x -> { p with resonance = x });
    Patch_text.knob "env.mod" (fun p -> p.env_mod) (fun p x -> { p with env_mod = x });
    Patch_text.knob "decay" (fun p -> p.decay) (fun p x -> { p with decay = x });
    Patch_text.knob "accent" (fun p -> p.accent) (fun p x -> { p with accent = x });
    Patch_text.selector "waveform" [ "saw"; "square" ] (fun p -> if p.square then 1 else 0) (fun p x -> { p with square = x = 1 });
    { name = "tempo"; control = Knob (60., 200.); get = (fun p -> p.bpm); put = (fun p x -> { p with bpm = x }) };
    Patch_text.knob "volume" (fun p -> p.volume) (fun p x -> { p with volume = x });
    Patch_text.selector "locks" [ "step"; "points" ] (fun p -> p.locks) (fun p x -> { p with locks = x });
    Patch_text.knob "smoothing" (fun p -> p.smoothing) (fun p x -> { p with smoothing = x });
  ]

let lockable = [ "tuning"; "cutoff"; "resonance"; "env.mod"; "decay"; "accent" ]
let locks (p : patch) : Sequencer.locks = if p.locks = 1 then Points p.smoothing else Per_step

let to_string (p : patch) : string = Patch_text.to_string knobs p ^ "pattern = " ^ pattern_to_string p.pattern ^ "\n"

(* the pattern's line apart, the knobs' to Patch_text *)
let of_string (text : string) : (patch, string) result =
  let is_pattern l = match String.index_opt l '=' with Some i -> String.trim (String.sub l 0 i) = "pattern" | None -> false in
  let lines = String.split_on_char '\n' text in
  let knob_lines = List.filter (fun l -> not (is_pattern l)) lines in
  match Patch_text.of_string knobs ~initial (String.concat "\n" knob_lines) with
  | Error e -> Error e
  | Ok p -> (
      match List.find_opt is_pattern lines with
      | None -> Ok p
      | Some l -> (
          let v = String.sub l (String.index l '=' + 1) (String.length l - String.index l '=' - 1) in
          let v = match String.index_opt v '#' with Some i -> String.sub v 0 i | None -> v in
          match pattern_of_string v with Ok pattern -> Ok { p with pattern } | Error e -> Error ("pattern: " ^ e)))

let presets : (string * patch) list =
  [
    ("acid", initial);
    ( "bass",
      { initial with resonance = 0.2; env_mod = 0.25; decay = 0.2; accent = 0.3; pattern = parse "C2 . C2 . G1 . Bb1 C2 C2 . C2 . Eb2 . F2 G1" } );
    ( "accents",
      { initial with cutoff = 0.25; resonance = 0.75; accent = 1.; bpm = 110.; pattern = parse "C2* C2* C2* . C2 . . . C2* C2* C2* . C2 . . ." } );
    (* the cutoff climbing over the bar and falling back, the decay
     * long on one note: the OP-XY's points, gliding *)
    ( "locks",
      {
        initial with
        locks = 1;
        smoothing = 1.;
        pattern =
          parse
            "C2[cutoff=0.05] C2 C3* C2 . Eb2 C2~ G2* . C2[cutoff=0.6,decay=0.9] Bb1~ C2* C2[decay=0.2] . F2* Eb2~[cutoff=0.3]";
      } );
  ]

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

let rate = float_of_int Signal.rate

(* ours: the accent sweep's charge (47k x 1 uF) and drain (200k x 1 uF),
 * the slide's time constant, the volume envelope's attack, long decay
 * and close *)
let sweep_charge = 0.047
let sweep_drain = 0.2
let slide_seconds = 0.06
let attack_seconds = 0.003
let hold_seconds = 3.
let close_seconds = 0.003
let per_sample (seconds : float) : float = 1. -. exp (-1. /. (seconds *. rate))

type t = {
  mutable patch : patch;
  seq : Sequencer.t;
  keys : Voicing.t;
  glide : Voicing.glide;
  vco : Vco.t;
  ladder : Diode_ladder.t;
  mutable meg : float; (* the filter's envelope, 1 at a note's start *)
  mutable meg_seconds : float; (* its decay, to -60 dB *)
  mutable accented : bool;
  mutable sweep : float; (* the accent sweep's capacitor *)
  mutable hold : float; (* the volume envelope's long decay *)
  mutable level : float; (* the volume envelope *)
  mutable gate : bool;
  mutable sliding : bool;
  mutable cutoff_now : float;
  ring : Signal.t;
  mutable at : int;
}

let create (patch : patch) : t =
  {
    patch;
    seq = Sequencer.create ~bpm:patch.bpm patch.pattern;
    keys = Voicing.create ~priority:Last ();
    glide = Voicing.glide ~note:36. ();
    vco = Vco.create ();
    ladder = Diode_ladder.create ();
    meg = 0.;
    meg_seconds = 0.2;
    accented = false;
    sweep = 0.;
    hold = 0.;
    level = 0.;
    gate = false;
    sliding = false;
    cutoff_now = cutoff_hz patch.cutoff;
    ring = Array.make 2048 0.;
    at = 0;
  }

let patch (t : t) : patch = t.patch

let set_patch (t : t) (p : patch) : unit =
  t.patch <- p;
  Sequencer.set_pattern t.seq p.pattern;
  Sequencer.set_bpm t.seq p.bpm

let run (t : t) (on : bool) : unit =
  if on && not (Sequencer.running t.seq) then Sequencer.start t.seq
  else if (not on) && Sequencer.running t.seq then begin
    Sequencer.stop t.seq;
    t.gate <- false
  end

let running (t : t) : bool = Sequencer.running t.seq
let step (t : t) : int = Sequencer.step t.seq
let envelope (t : t) : float = t.meg
let sweep (t : t) : float = t.sweep
let pitch (t : t) : float = Voicing.pitch t.glide
let cutoff_now (t : t) : float = t.cutoff_now
let recent (t : t) : Signal.t = Array.init 2048 (fun i -> t.ring.((t.at + i) mod 2048))

(* the knobs locked where the pattern is, read at the start of each
 * piece rendered: a gliding lock moves a block at a time, as a hand
 * on a knob does *)
let locked (t : t) (offset : int) : patch =
  let locks = locks t.patch in
  List.fold_left
    (fun p (k : knob) ->
      if List.mem k.name lockable then match Sequencer.locked t.seq locks k.name offset with Some v -> k.put p v | None -> p else p)
    t.patch knobs

(* a note: slid into (the gate held, the pitch gliding, the envelopes
 * going on), or begun (the pitch jumping, the envelopes starting);
 * [p] the patch as locked then *)
let note_on ?(p : patch option) (t : t) (note : int) ~(accent : bool) ~(glide : bool) : unit =
  let p = Option.value p ~default:t.patch in
  Voicing.glide_to t.glide note;
  t.accented <- accent;
  t.sliding <- glide;
  if not glide then begin
    t.meg <- 1.;
    t.meg_seconds <- (if accent then decay_seconds 0. else decay_seconds p.decay);
    t.hold <- 1.;
    t.gate <- true
  end

(* [n] samples of the voice into [out] from [from] *)
let render (t : t) (out : Signal.t) (from : int) (n : int) : unit =
  if n > 0 then begin
    let p = locked t from in
    let pitch = Array.make n 0. and wave = Array.make n 0. and cutoff = Array.make n 0. in
    Voicing.fill_pitch t.glide ~seconds:(if t.sliding then slide_seconds else 0.) pitch;
    let frequency = Array.map (fun m -> Voicing.frequency (m +. (12. *. p.tuning))) pitch in
    Vco.fill t.vco (if p.square then Pulse else Sawtooth) ~frequency wave;
    let meg_k = exp (-.log 1000. /. (t.meg_seconds *. rate)) in
    let charge = per_sample sweep_charge and drain = per_sample sweep_drain in
    let attack = per_sample attack_seconds and hold_k = exp (-1. /. (hold_seconds *. rate)) and close = per_sample close_seconds in
    let gains = Array.make n 0. in
    for i = 0 to n - 1 do
      t.meg <- t.meg *. meg_k;
      (* the sweep: charged by an accented note's envelope through the
       * diode, drained all the time *)
      let a = if t.accented then t.meg else 0. in
      if a > t.sweep then t.sweep <- t.sweep +. ((a -. t.sweep) *. charge) else t.sweep <- t.sweep -. (t.sweep *. drain);
      (* the volume: towards the long decay while the gate is open, closed
       * fast after *)
      if t.gate then begin
        t.hold <- t.hold *. hold_k;
        t.level <- t.level +. ((t.hold -. t.level) *. attack)
      end
      else t.level <- t.level -. (t.level *. close);
      let octaves = (env_octaves p.env_mod *. t.meg) +. (2. *. p.accent *. t.sweep) in
      cutoff.(i) <- Float.min 18000. (cutoff_hz p.cutoff *. Float.pow 2. octaves);
      gains.(i) <- t.level *. (1. +. (p.accent *. t.sweep))
    done;
    t.cutoff_now <- cutoff.(n - 1);
    Diode_ladder.process t.ladder ~cutoff ~resonance:(resonance_k p.resonance) wave;
    for i = 0 to n - 1 do
      let x = 4. *. p.volume *. gains.(i) *. wave.(i) in
      out.(from + i) <- x;
      t.ring.(t.at) <- x;
      t.at <- (t.at + 1) mod 2048
    done
  end

let fill (t : t) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  (* the sequencer's events in this block, then the block rendered in
   * pieces between them *)
  let events = ref [] in
  Sequencer.advance t.seq n (fun offset e -> events := (offset, e) :: !events);
  let from =
    List.fold_left
      (fun from (offset, e) ->
        render t out.left from (offset - from);
        (match (e : Sequencer.event) with
        | Note_on { note; accent; glide } -> note_on ~p:(locked t offset) t note ~accent ~glide
        | Note_off -> t.gate <- false);
        offset)
      0 (List.rev !events)
  in
  render t out.left from (n - from);
  Array.blit out.left 0 out.right 0 n

let instrument (t : t) : Instrument.t =
  let key (e : Voicing.event) =
    match e with
    | Begin n -> note_on t n ~accent:false ~glide:false
    | Change n -> note_on t n ~accent:false ~glide:true
    | End -> t.gate <- false
    | Nothing -> ()
  in
  let set name x =
    if name = "run" then run t (Control.on x)
    else
      Option.iter (fun (k : knob) -> set_patch t (k.put t.patch x)) (List.find_opt (fun (k : knob) -> k.name = name) knobs)
  in
  { note_on = (fun n _ -> key (Voicing.press t.keys n)); note_off = (fun n -> key (Voicing.release t.keys n)); set; fill = fill t }
