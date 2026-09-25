(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Studio_opxy.mli *)

(*****************************************************************************)
(* The brain *)
(*****************************************************************************)

let scales =
  [
    ("major", [ 0; 2; 4; 5; 7; 9; 11 ]);
    ("minor", [ 0; 2; 3; 5; 7; 8; 10 ]);
    ("dorian", [ 0; 2; 3; 5; 7; 9; 10 ]);
    ("mixolydian", [ 0; 2; 4; 5; 7; 9; 10 ]);
    ("pentatonic", [ 0; 3; 5; 7; 10 ]);
  ]

let brain ~(from : int) ~(key : int) ~(scale : int) (note : int) : int =
  let src = snd (List.nth scales from) and dst = Array.of_list (snd (List.nth scales scale)) in
  let octave = if note >= 0 then note / 12 else ((note + 1) / 12) - 1 in
  let pc = note - (12 * octave) in
  (* the degree: the last of the scale at or under the note *)
  let i = List.fold_left (fun (i, k) d -> if d <= pc then (k, k + 1) else (i, k + 1)) (0, 0) src |> fst in
  (* the same degree in a scale as long; in one of another size (a
   * pentatonic's five), the nearest note to it, the lower on a tie --
   * a degree's rank there would move a fifth *)
  let d = List.nth src i in
  let near =
    if Array.length dst = List.length src then dst.(i)
    else Array.fold_left (fun best x -> if abs (x - d) < abs (best - d) then x else best) dst.(0) dst
  in
  (12 * octave) + key + near

(*****************************************************************************)
(* The tracks *)
(*****************************************************************************)

type kind = Synth of Studio_op1.sound | Drums | Keys
type component = Multiply of int | Pulse of int | Hold of int | Skip of int
type step = { notes : int list; velocity : float; locks : (string * float) list; components : component list }

type track = {
  name : string;
  kind : kind;
  patterns : step array array;
  cutoff : float;
  resonance : float;
  volume : float;
  pan : float;
  linked : bool;
  smoothing : float;
}

type scene = { chosen : int array; mutes : bool array }

type patch = {
  tracks : track array;
  scenes : scene array;
  scene : int;
  tempo : float;
  written : int;
  key : int;
  scale : int;
  volume : float;
}

let rest = { notes = []; velocity = 0.8; locks = []; components = [] }
let note ?(velocity = 0.8) (notes : int list) : step = { notes; velocity; locks = []; components = [] }

(* a step's component's number, 1 without it *)
let multiply (s : step) : int = List.fold_left (fun a c -> match c with Multiply n -> n | _ -> a) 1 s.components
let pulse (s : step) : int = List.fold_left (fun a c -> match c with Pulse n -> n | _ -> a) 1 s.components
let hold (s : step) : int = List.fold_left (fun a c -> match c with Hold n -> n | _ -> a) 1 s.components
let skip (s : step) : int = List.fold_left (fun a c -> match c with Skip n -> n | _ -> a) 1 s.components
let lockable = [ "p1"; "p2"; "p3"; "p4"; "cutoff"; "resonance"; "volume"; "pan" ]

let get (tr : track) (name : string) : float =
  match name with
  | "cutoff" -> tr.cutoff
  | "resonance" -> tr.resonance
  | "volume" -> tr.volume
  | "pan" -> tr.pan
  | _ -> (
      match (tr.kind, int_of_string_opt (String.sub name 1 (String.length name - 1))) with
      | Synth s, Some i when i >= 1 && i <= 4 -> s.engine_params.(i - 1)
      | _ -> 0.5)

let put (tr : track) (name : string) (v : float) : track =
  match name with
  | "cutoff" -> { tr with cutoff = v }
  | "resonance" -> { tr with resonance = v }
  | "volume" -> { tr with volume = v }
  | "pan" -> { tr with pan = v }
  | _ -> (
      match (tr.kind, int_of_string_opt (String.sub name 1 (String.length name - 1))) with
      | Synth s, Some i when i >= 1 && i <= 4 ->
          let engine_params = Array.copy s.engine_params in
          engine_params.(i - 1) <- v;
          { tr with kind = Synth { s with engine_params } }
      | _ -> tr)

let cutoff_hz (k : float) : float = 80. *. Float.pow 250. k

(*****************************************************************************)
(* Our song *)
(*****************************************************************************)

let bars = 16

(* a pattern from its notes by step: [(step, notes)] *)
let steps ?(velocity = 0.8) (l : (int * int list) list) : step array =
  Array.init bars (fun k -> match List.assoc_opt k l with Some n -> note ~velocity n | None -> rest)

(* a drum pattern as the TR-808's lines: a key and "x...x..." *)
let drums (lines : (int * string) list) : step array =
  Array.init bars (fun k ->
      match List.filter_map (fun (key, s) -> if s.[k] = 'x' then Some key else None) lines with [] -> rest | keys -> note ~velocity:0.9 keys)

let midi (s : string) : int = match Music.midi_number s with Some n -> n | None -> failwith ("Studio_opxy: " ^ s)
let line (l : (int * string list) list) : step array = steps (List.map (fun (k, names) -> (k, List.map midi names)) l)
let empty () : step array = Array.make bars rest

let track ?(linked = true) ?(cutoff = 0.8) ?(volume = 0.7) ?(pan = 0.) name kind (patterns : step array list) : track =
  let patterns = Array.of_list (patterns @ List.init (4 - List.length patterns) (fun _ -> empty ())) in
  { name; kind; patterns; cutoff; resonance = 0.2; volume; pan; linked; smoothing = 0.5 }

let op1 k = Studio_op1.initial.sounds.(k)

let initial : patch =
  let kick = 36 and snare = 38 and clap = 39 and closed = 42 and opened = 46 in
  let busy = drums [ (kick, "x...x...x..xx..."); (clap, "....x.......x..."); (closed, "x.x.x.x.x.x.x..."); (opened, "..............x.") ] in
  (* the bar's last step a closed hat struck three times: a ratchet *)
  busy.(15) <- { (note ~velocity:0.7 [ closed ]) with components = [ Multiply 3 ] };
  let lead =
    let s = line [ (0, [ "G4" ]); (3, [ "Bb4" ]); (6, [ "C5" ]); (8, [ "Eb5" ]); (11, [ "D5" ]); (14, [ "Bb4" ]) ] in
    (* the cutoff opening over the bar, the OP-XY's way: two points *)
    s.(0) <- { (s.(0)) with locks = [ ("cutoff", 0.3) ] };
    s.(8) <- { (s.(8)) with locks = [ ("cutoff", 0.9) ] };
    s
  in
  {
    tracks =
      [|
        track ~linked:false ~volume:0.8 "drums" Drums
          [
            drums [ (kick, "x...x...x...x..."); (snare, "....x.......x..."); (closed, "..x...x...x...x.") ];
            busy;
          ];
        track ~cutoff:0.6 "bass" (Synth (op1 4))
          [
            line [ (0, [ "C3" ]); (3, [ "C3" ]); (6, [ "Eb3" ]); (8, [ "C3" ]); (11, [ "G2" ]); (14, [ "Bb2" ]) ];
            line [ (0, [ "C3" ]); (2, [ "C3" ]); (4, [ "Eb3" ]); (6, [ "F3" ]); (8, [ "G3" ]); (10, [ "F3" ]); (12, [ "Eb3" ]); (14, [ "Bb2" ]) ];
          ];
        track ~volume:0.5 ~pan:(-0.3) "keys" Keys
          [ line [ (0, [ "C4"; "Eb4"; "G4"; "Bb4" ]); (8, [ "Ab3"; "C4"; "Eb4"; "G4" ]) ] ];
        track ~volume:0.4 ~pan:0.3 "lead" (Synth (op1 3)) [ lead ];
        track "bell" (Synth (op1 0)) [];
        track "pad" (Synth (op1 1)) [];
        track "string" (Synth (op1 2)) [];
        track "digital" (Synth (op1 5)) [];
      |];
    scenes =
      [|
        (* the intro: drums, bass, keys *)
        { chosen = Array.make 8 0; mutes = [| false; false; false; true; false; false; false; false |] };
        (* the lead comes in, the drums and bass busier *)
        { chosen = [| 1; 1; 0; 0; 0; 0; 0; 0 |]; mutes = Array.make 8 false };
        (* the break: no drums *)
        { chosen = [| 0; 1; 0; 0; 0; 0; 0; 0 |]; mutes = [| true; false; false; false; false; false; false; false |] };
        (* the end: drums and keys *)
        { chosen = Array.make 8 0; mutes = [| false; true; false; true; false; false; false; false |] };
      |];
    scene = 0;
    tempo = 112.;
    written = 1;
    key = 0;
    scale = 1;
    volume = 0.6;
  }

(*****************************************************************************)
(* The samples *)
(*****************************************************************************)

(* [render inst start seconds]: an instrument's output, [start] it *)
let render (inst : Instrument.t) (start : unit -> unit) (seconds : float) : Signal.t =
  start ();
  let n = Signal.samples seconds in
  let out = Array.make n 0. and at = ref 0 in
  while !at < n do
    let m = min 735 (n - !at) in
    let b = { Signal.left = Array.make m 0.; right = Array.make m 0. } in
    inst.fill b;
    Array.iteri (fun i l -> out.(!at + i) <- 0.5 *. (l +. b.right.(i))) b.left;
    at := !at + m
  done;
  out

let pads = List.map (fun i -> (Voice_tr808.key i, Voice_tr808.name i)) Voice_tr808.instruments

(* the kit: each TR-808 instrument hit once and recorded, long enough
 * for its decay, at its General MIDI key; computed once, on first use *)
let kit_pads : Sampler.pad array Lazy.t =
  lazy
    (Array.init 24 (fun k ->
         match Voice_tr808.of_key (36 + k) with
         | None -> Sampler.pad { data = [||]; root = 60 }
         | Some i ->
             let v = Voice_tr808.create Voice_tr808.initial in
             let seconds = match i with CY | OH -> 1. | CH | RS | CP -> 0.25 | _ -> 0.6 in
             let data = render (Voice_tr808.instrument v) (fun () -> Voice_tr808.hit v i ~accent:false) seconds in
             let play : Sampler.play = match i with CH | OH -> Mute_group | _ -> Oneshot in
             Sampler.pad ~play { data; root = 60 }))

(* the keys: our Rhodes' C4, two seconds, played across the keyboard *)
let epiano : Sampler.sample Lazy.t =
  lazy
    (let v = Voice_rhodes.create Voice_rhodes.initial in
     let inst = Voice_rhodes.instrument v in
     { data = render inst (fun () -> inst.note_on 60 0.8) 2.; root = 60 })

let epiano_settings = { Sampler.default with release = Some 0.3 }

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

type state = {
  seq : Sequencer.t;
  mutable poly : Polyphony.t;
  mutable mode : int; (* the play mode [poly] was made for *)
  mutable kit : Sampler.kit option;
  params : float array; (* the engine's four, locked, the notes read *)
  filter : Svf.t;
  mutable held : int list; (* the notes of the step sounding *)
  mutable level : float;
  (* where the track is in its pattern: the clock's step, but for a
   * pulse or a hold, which stay on a step for several ticks *)
  mutable started : bool;
  mutable pos : int;
  mutable remaining : int; (* the ticks still to stay on [pos] *)
  plays : int array; (* each step's arrivals, for Skip *)
  mutable ratchet : (float * bool) list; (* a Multiply's events: exact samples, true an on *)
  mutable triggers : int list; (* the samples the notes were pressed at, the last first *)
}

type t = {
  mutable patch : patch;
  states : state array;
  mutable playing : int;
  mutable live : int;
  mutable left : Signal.t;
  mutable right : Signal.t;
  ring : Signal.t;
  mutable at : int;
  mutable clock : int; (* samples since created *)
}

(* the sequencers are the clock: a tick each step, a gate each half
 * step; the tracks play their patterns from them (the pulses and holds
 * making a track's step lag the clock's) *)
let ticks : Sequencer.step array = Array.make bars (Sequencer.note 0)

(* a pattern's locks, as Sequencer.lock_value reads them *)
let seq_locks (p : step array) : Sequencer.step array = Array.map (fun (s : step) -> { Sequencer.rest with locks = s.locks }) p
let pattern_of (p : patch) (scene : int) (k : int) : step array = p.tracks.(k).patterns.(p.scenes.(scene).chosen.(k))

let create (patch : patch) : t =
  {
    patch;
    states =
      Array.init 8 (fun _ ->
          {
            seq = Sequencer.create ~bpm:patch.tempo ticks;
            poly = Polyphony.create ();
            mode = 0;
            kit = None;
            params = Array.make 4 0.5;
            filter = Svf.create ();
            held = [];
            level = 0.;
            started = false;
            pos = 0;
            remaining = 0;
            plays = Array.make bars 0;
            ratchet = [];
            triggers = [];
          });
    playing = patch.scene;
    live = 0;
    left = [||];
    right = [||];
    ring = Array.make 2048 0.;
    at = 0;
    clock = 0;
  }

let patch (t : t) : patch = t.patch
let set_patch (t : t) (p : patch) : unit = t.patch <- p

(* the tracks back to their patterns' first step, their counts to 0 *)
let rewind (t : t) : unit =
  Array.iter
    (fun st ->
      st.started <- false;
      st.remaining <- 0;
      st.ratchet <- [];
      Array.fill st.plays 0 bars 0)
    t.states

let run (t : t) (on : bool) : unit =
  Array.iter (fun st -> if on then Sequencer.start st.seq else Sequencer.stop st.seq) t.states;
  rewind t;
  if not on then Array.iter (fun st -> st.held <- []) t.states

let running (t : t) : bool = Sequencer.running t.states.(0).seq
let step (t : t) : int = Sequencer.step t.states.(0).seq
let position (t : t) (k : int) : int = t.states.(k).pos
let triggers (t : t) (k : int) : int list = t.states.(k).triggers
let playing (t : t) : int = t.playing
let select (t : t) (k : int) : unit = t.live <- k
let voices (t : t) (k : int) : int = match t.states.(k).kit with Some kit -> Sampler.sounding kit | None -> Polyphony.voices t.states.(k).poly
let level (t : t) (k : int) : float = t.states.(k).level
let recent (t : t) : Signal.t = Array.init 2048 (fun i -> t.ring.((t.at + i) mod 2048))

(* a note of a track, pressed *)
let press (t : t) (k : int) (n : int) (velocity : float) : unit =
  let tr = t.patch.tracks.(k) and st = t.states.(k) in
  match tr.kind with
  | Drums ->
      let kit = match st.kit with Some kit -> kit | None -> let kit = Sampler.kit (Lazy.force kit_pads) in st.kit <- Some kit; kit in
      Sampler.press kit n velocity
  | Keys -> Polyphony.press st.poly n (Sampler.voice (Lazy.force epiano) epiano_settings ~key:n ~velocity)
  | Synth s ->
      if s.play_mode <> st.mode then begin
        st.poly <- (if s.play_mode = 1 then Polyphony.create ~voices:1 () else Polyphony.create ());
        st.mode <- s.play_mode
      end;
      Polyphony.press st.poly n (Studio_op1.voice s st.params n velocity)

let release (t : t) (k : int) (n : int) : unit =
  match t.states.(k).kit with Some kit -> Sampler.release kit n | None -> Polyphony.release t.states.(k).poly n

(* the track as locked [offset] samples into the block: at its own
 * step, the clock's fraction of a step past it *)
let locked (t : t) (k : int) (offset : int) : track =
  let tr = t.patch.tracks.(k) and st = t.states.(k) in
  match Sequencer.position st.seq offset with
  | Some p when st.started ->
      let at = float_of_int st.pos +. (p -. Float.of_int (Float.to_int p)) in
      let locks = seq_locks (pattern_of t.patch t.playing k) in
      List.fold_left
        (fun tr name -> match Sequencer.lock_value (Points tr.smoothing) locks name at with Some v -> put tr name v | None -> tr)
        tr lockable
  | _ -> tr

(* [n] samples of track [k] from [from], added into the mix *)
let render_piece (t : t) (k : int) (from : int) (n : int) (mute : bool) : unit =
  if n > 0 then begin
    let tr = locked t k from and st = t.states.(k) in
    (match tr.kind with Synth s -> Array.blit s.engine_params 0 st.params 0 4 | _ -> ());
    let x = Array.make n 0. in
    (match st.kit with
    | Some kit ->
        let b = { Signal.left = x; right = Array.make n 0. } in
        Sampler.fill kit b
    | None -> Polyphony.fill st.poly x);
    Svf.process st.filter Zero_delay Low_pass ~cutoff:(Array.make n (cutoff_hz tr.cutoff)) ~q:(0.707 +. (6. *. tr.resonance)) x;
    let sum = Array.fold_left (fun a v -> a +. (v *. v)) 0. x in
    st.level <- sqrt (sum /. float_of_int n);
    if not mute then begin
      (* the pan as the kit's: the middle at full on both sides *)
      let l = tr.volume *. Float.min 1. (1. -. tr.pan) and r = tr.volume *. Float.min 1. (1. +. tr.pan) in
      Array.iteri
        (fun i v ->
          t.left.(from + i) <- t.left.(from + i) +. (l *. v);
          t.right.(from + i) <- t.right.(from + i) +. (r *. v))
        x
    end
  end

let fill (t : t) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  let p = t.patch in
  if Array.length t.left <> n then begin
    t.left <- Array.make n 0.;
    t.right <- Array.make n 0.
  end;
  Array.fill t.left 0 n 0.;
  Array.fill t.right 0 n 0.;
  (* a new scene waits for the bar's last step: set now, the tracks
   * start its patterns from the next step, the bar's first *)
  if t.playing <> p.scene && ((not (running t)) || step t = bars - 1) then begin
    t.playing <- p.scene;
    rewind t
  end;
  let scene = p.scenes.(t.playing) in
  let sps = Sequencer.samples_per_step p.tempo in
  Array.iteri
    (fun k st ->
      let pattern = pattern_of p t.playing k in
      let tr = p.tracks.(k) in
      Sequencer.set_bpm st.seq p.tempo;
      (* the clock's ticks and half steps in this block *)
      let clock = ref [] in
      Sequencer.advance st.seq n (fun offset e -> clock := (offset, `Clock e) :: !clock);
      let clock = List.rev !clock in
      let sample_of x = int_of_float (Float.ceil (x -. 1e-9)) in
      (* the next event: the clock's, or the ratchet's -- scheduled by a
       * trigger in this very block, so looked for each time; the clock's
       * first on a tie *)
      let next clock =
        let ratchet = List.filter (fun (x, _) -> sample_of x < t.clock + n) st.ratchet in
        let earliest = List.fold_left (fun a r -> match a with Some (y, _) when y <= fst r -> a | _ -> Some r) None ratchet in
        match (clock, earliest) with
        | (o, e) :: rest, Some (x, _) when o <= sample_of x - t.clock -> Some (o, e, rest)
        | _, Some ((x, on) as r) ->
            st.ratchet <- List.filter (fun r' -> r' != r) st.ratchet;
            Some (sample_of x - t.clock, (if on then `Ratchet_on else `Ratchet_off), clock)
        | (o, e) :: rest, None -> Some (o, e, rest)
        | [], None -> None
      in
      let mute = scene.mutes.(k) in
      let release_all () =
        List.iter (fun m -> release t k m) st.held;
        st.held <- []
      in
      let trigger offset (s : step) =
        release_all ();
        let notes = if tr.linked then List.map (brain ~from:p.written ~key:p.key ~scale:p.scale) s.notes else s.notes in
        List.iter (fun m -> press t k m s.velocity) notes;
        st.held <- notes;
        st.triggers <- (t.clock + offset) :: List.filteri (fun i _ -> i < 63) st.triggers
      in
      let rec loop from clock =
        match next clock with
        | None -> from
        | Some (offset, e, clock) ->
            render_piece t k from (offset - from) mute;
            (match e with
            | `Clock (Sequencer.Note_on _) ->
                (* the track's step: the next, or the same while a pulse
                 * or a hold stays on it *)
                let repeat =
                  if not st.started then (st.started <- true; st.pos <- 0; false)
                  else if st.remaining > 0 then (st.remaining <- st.remaining - 1; true)
                  else (st.pos <- (st.pos + 1) mod bars; false)
                in
                let s = pattern.(st.pos) in
                if not repeat then begin
                  st.plays.(st.pos) <- st.plays.(st.pos) + 1;
                  st.remaining <- max (pulse s) (hold s) - 1
                end;
                (* skip: one arrival in n; a hold's repeats don't strike
                 * again, a pulse's do *)
                let skipped = (st.plays.(st.pos) - 1) mod skip s <> 0 in
                if s.notes <> [] && (not skipped) && ((not repeat) || pulse s > 1) then begin
                  trigger offset s;
                  (* multiply: the step's time cut in n, each a note
                   * with its half-length gate *)
                  let m = multiply s in
                  if m > 1 then begin
                    let a = float_of_int (t.clock + offset) and d = sps /. float_of_int m in
                    st.ratchet <- List.init (m - 1) (fun i -> (a +. (float_of_int (i + 1) *. d), true)) @ List.init m (fun i -> (a +. ((float_of_int i +. 0.5) *. d), false))
                  end
                end
                else if not repeat then release_all ()
            | `Clock Sequencer.Note_off ->
                let s = pattern.(st.pos) in
                (* the gate: not a ratchet's (its own offs), not a hold's
                 * before its last step *)
                if multiply s = 1 && not (hold s > 1 && st.remaining > 0) then release_all ()
            | `Ratchet_on -> trigger offset pattern.(st.pos)
            | `Ratchet_off -> release_all ());
            loop offset clock
      in
      let from = loop 0 clock in
      render_piece t k from (n - from) mute)
    t.states;
  for i = 0 to n - 1 do
    out.left.(i) <- p.volume *. t.left.(i);
    out.right.(i) <- p.volume *. t.right.(i);
    t.ring.(t.at) <- out.left.(i);
    t.at <- (t.at + 1) mod 2048
  done;
  t.clock <- t.clock + n

let instrument (t : t) : Instrument.t =
  {
    note_on = (fun n v -> press t t.live n v);
    note_off = (fun n -> release t t.live n);
    set = (fun name x -> if name = "run" then run t (Control.on x));
    fill = fill t;
  }
