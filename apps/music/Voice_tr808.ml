(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Voice_tr808.mli *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

type instrument = BD | SD | LT | MT | HT | RS | CP | CB | CY | OH | CH

let instruments = [ BD; SD; LT; MT; HT; RS; CP; CB; CY; OH; CH ]
let name = function BD -> "BD" | SD -> "SD" | LT -> "LT" | MT -> "MT" | HT -> "HT" | RS -> "RS" | CP -> "CP" | CB -> "CB" | CY -> "CY" | OH -> "OH" | CH -> "CH"

let index (i : instrument) : int =
  let rec find k = function [] -> 0 | x :: rest -> if x = i then k else find (k + 1) rest in
  find 0 instruments

type drum = { level : float; tone : float; decay : float; tuning : float; snappy : float }
type patch = { drums : drum array; tracks : bool array array; accents : bool array; accent : float; tempo : float; volume : float }

let drum0 = { level = 0.8; tone = 0.5; decay = 0.5; tuning = 0.5; snappy = 0.5 }

let initial : patch =
  {
    drums = Array.make 11 drum0;
    tracks = Array.init 11 (fun _ -> Array.make 16 false);
    accents = Array.make 16 false;
    accent = 0.5;
    tempo = 120.;
    volume = 0.7;
  }

type knob = patch Patch_text.knob

let knobs : knob list =
  List.concat_map
    (fun i ->
      let k = index i in
      let knob field get set =
        Patch_text.knob
          (name i ^ "." ^ field)
          (fun p -> get p.drums.(k))
          (fun p x ->
            let d = Array.copy p.drums in
            d.(k) <- set d.(k) x;
            { p with drums = d })
      in
      [
        knob "level" (fun d -> d.level) (fun d x -> { d with level = x });
        knob "tone" (fun d -> d.tone) (fun d x -> { d with tone = x });
        knob "decay" (fun d -> d.decay) (fun d x -> { d with decay = x });
        knob "tuning" (fun d -> d.tuning) (fun d x -> { d with tuning = x });
        knob "snappy" (fun d -> d.snappy) (fun d x -> { d with snappy = x });
      ])
    instruments
  @ [
      Patch_text.knob "accent" (fun p -> p.accent) (fun p x -> { p with accent = x });
      (* whole beats a minute, so a pattern's tempo is written exactly *)
      Patch_text.selector "tempo" (List.init 121 (fun k -> string_of_int (60 + k)))
        (fun p -> Float.to_int p.tempo - 60)
        (fun p x -> { p with tempo = float_of_int (60 + x) });
      Patch_text.knob "volume" (fun p -> p.volume) (fun p x -> { p with volume = x });
    ]

(* the tracks as text: 16 characters, x a hit, . a rest *)
let steps_to_string (s : bool array) : string = String.init 16 (fun k -> if s.(k) then 'x' else '.')
let steps_of_string (s : string) : bool array = Array.init 16 (fun k -> k < String.length s && s.[k] = 'x')

let to_string (p : patch) : string =
  Patch_text.to_string knobs p
  ^ String.concat "" (List.map (fun i -> Printf.sprintf "%s.steps = %s\n" (name i) (steps_to_string p.tracks.(index i))) instruments)
  ^ Printf.sprintf "accent.steps = %s\n" (steps_to_string p.accents)

(* the ".steps" lines read first, the rest by Patch_text *)
let of_string (text : string) : (patch, string) result =
  let lines = String.split_on_char '\n' text in
  let steps l =
    match String.index_opt l '=' with
    | Some i ->
        let key = String.trim (String.sub l 0 i) and value = String.trim (String.sub l (i + 1) (String.length l - i - 1)) in
        let suffix = ".steps" in
        let n = String.length key and m = String.length suffix in
        if n > m && String.sub key (n - m) m = suffix then Some (String.sub key 0 (n - m), value) else None
    | None -> None
  in
  let others = List.filter (fun l -> steps l = None) lines in
  Result.map
    (fun p ->
      List.fold_left
        (fun (p : patch) l ->
          match steps l with
          | Some ("accent", v) -> { p with accents = steps_of_string v }
          | Some (who, v) -> (
              match List.find_opt (fun i -> name i = who) instruments with
              | Some i ->
                  let tracks = Array.copy p.tracks in
                  tracks.(index i) <- steps_of_string v;
                  { p with tracks }
              | None -> p)
          | None -> p)
        p lines)
    (Patch_text.of_string knobs ~initial (String.concat "\n" others))

(* ours: patterns in the styles the 808 made *)
let pattern ?(tempo = 120.) ?(accents = "................") (tracks : (instrument * string) list) : patch =
  {
    initial with
    tempo;
    accents = steps_of_string accents;
    tracks = Array.init 11 (fun k -> match List.assoc_opt (List.nth instruments k) tracks with Some s -> steps_of_string s | None -> Array.make 16 false);
  }

let presets : (string * patch) list =
  [
    (* after Planet Rock (1982): the kick syncopated, the clap and the
     * cowbell *)
    ( "electro",
      pattern ~tempo:127. ~accents:"x...x...x...x..."
        [ (BD, "x.....x...x....."); (SD, "....x.......x..."); (CP, "....x.......x..."); (CH, "x.x.x.x.x.x.x.x."); (CB, "..x..x....x..x..") ] );
    (* four on the floor, the open hat between *)
    ("house", pattern ~tempo:124. [ (BD, "x...x...x...x..."); (CP, "....x.......x..."); (OH, "..x...x...x...x."); (CH, "x...x...x...x...") ]);
    (* a long kick, the snare on two and four, the hats *)
    ( "hip hop",
      { (pattern ~tempo:92. [ (BD, "x......x..x....."); (SD, "....x.......x..."); (CH, "x.x.x.x.x.x.xxx.") ]) with drums = Array.init 11 (fun k -> if k = 0 then { drum0 with decay = 0.9 } else drum0) } );
    (* the toms, the rim shot, the cowbell, a cymbal to start *)
    ( "latin",
      pattern ~tempo:112.
        [ (BD, "x..x..x.x..x..x."); (RS, "..x..x....x..x.."); (LT, "......x.......x."); (MT, "....x.......x..."); (HT, "x.......x......."); (CB, "x.x.x.x.x.x.x.x."); (CY, "x...............") ] );
  ]

(* the General MIDI drum map *)
let key = function BD -> 36 | SD -> 38 | LT -> 41 | MT -> 45 | HT -> 48 | RS -> 37 | CP -> 39 | CB -> 56 | CY -> 49 | OH -> 46 | CH -> 42
let of_key (k : int) : instrument option = List.find_opt (fun i -> key i = k) instruments

(*****************************************************************************)
(* The hits *)
(*****************************************************************************)

let rate = float_of_int Signal.rate

(* -60 dB after [t60] seconds, [age] samples in *)
let decay_env (t60 : float) (age : int) : float = exp (-6.9 *. float_of_int age /. (t60 *. rate))

(* the metal's buffers for the segment being rendered: the upper and
 * lower band-passes of the six squares, and the cowbell's two squares *)
type metal = { mutable low : Signal.t; mutable high : Signal.t; mutable bell : Signal.t }

(* a hit: it adds [m] samples of itself into [out] from [at], reading
 * the metal's buffers from 0; alive until silent *)
type hit = { who : instrument; add : metal -> Signal.t -> int -> int -> unit; mutable alive : bool; mutable choked : bool }

let drum_frequency ~(f0 : float) ~(sigh : float) ~(punch : float) ~(age : float) (level : float) : float =
  if age < 0.006 then punch *. f0 else f0 *. (1. +. (sigh *. level))

(* a sine whose frequency the circuit moves: [punch] times higher its
 * first 6 ms, then [f0] raised by [sigh] times its level (the sigh) *)
let sine_drum ~(f0 : float) ~(t60 : float) ~(sigh : float) ~(punch : float) ~(tone_hz : float) (amp : float) (who : instrument) : hit =
  let phase = ref 0. and age = ref 0 and lp = ref 0. in
  let a = 1. -. exp (-2. *. Float.pi *. tone_hz /. rate) in
  let rec h = { who; add = (fun _ out at m -> add out at m); alive = true; choked = false }
  and add out at m =
    for i = at to at + m - 1 do
      if h.alive then begin
      let env = decay_env t60 !age in
      let f = drum_frequency ~f0 ~sigh ~punch ~age:(float_of_int !age /. rate) env in
      phase := Float.rem (!phase +. (f /. rate)) 1.;
      (* the trigger pulse, 1 ms, clicking through the tone's low-pass *)
      let click = if float_of_int !age < 0.001 *. rate then 0.5 else 0. in
      lp := !lp +. (a *. ((env *. sin (2. *. Float.pi *. !phase)) +. click -. !lp));
      out.(i) <- out.(i) +. (amp *. !lp);
      incr age;
      if env < 1e-4 then h.alive <- false
      end
    done
  in
  h

(* tones struck (Modal), and noise (the voice's own generator, so two
 * voices don't disturb each other's) through [filter] under [envelope] *)
let tones_and_noise ~(noise : unit -> float) ~(tones : (float * float * float) list) ~(noise_level : float)
    ~(filter : Svf.mode * float * float) ~(envelope : int -> float) (amp : float) (who : instrument) : hit =
  let modes = List.map (fun (f, t60, a) -> let m = Modal.create ~frequency:f ~t60 in Modal.strike m a; m) tones in
  let svf = Svf.create () and age = ref 0 in
  let mode, cutoff, q = filter in
  let rec h = { who; add = (fun _ out at m -> add out at m); alive = true; choked = false }
  and add out at m =
    let n = Array.init m (fun _ -> noise ()) in
    Svf.process svf Zero_delay mode ~cutoff:(Array.make m cutoff) ~q n;
    for i = 0 to m - 1 do
      if h.alive then begin
        let x = List.fold_left (fun s md -> s +. Modal.next md) 0. modes in
        out.(at + i) <- out.(at + i) +. (amp *. (x +. (noise_level *. envelope !age *. n.(i))));
        incr age;
        (* silent, decided at the sample: the same whatever the blocks *)
        if envelope !age < 1e-4 && List.for_all (fun md -> Modal.level md < 1e-4) modes then h.alive <- false
      end
    done
  in
  h

(* the metal: a band of the squares (a buffer of [metal]) through a
 * high-pass, under an envelope; choked, gone in 10 ms *)
let metal_hit ~(bands : ((metal -> Signal.t) * float * float * (int -> float)) list) (amp : float) (who : instrument) : hit =
  let filters = List.map (fun _ -> Svf.create ()) bands and age = ref 0 and choke = ref 1. in
  let rec h = { who; add = (fun metal out at m -> add metal out at m); alive = true; choked = false }
  and add metal out at m =
    if h.alive then begin
      let mix = Array.make m 0. in
      List.iter2
        (fun (band, hp, q, envelope) svf ->
          let x = Array.sub (band metal) 0 m in
          if hp > 0. then Svf.process svf Zero_delay High_pass ~cutoff:(Array.make m hp) ~q x;
          Array.iteri (fun i v -> mix.(i) <- mix.(i) +. (envelope (!age + i) *. v)) x)
        bands filters;
      for i = 0 to m - 1 do
        if h.alive then begin
          if h.choked then choke := !choke *. exp (-6.9 /. (0.01 *. rate));
          out.(at + i) <- out.(at + i) +. (amp *. !choke *. mix.(i));
          incr age;
          (* silent, decided at the sample: the same whatever the blocks *)
          if !choke < 1e-4 || List.for_all (fun (_, _, _, e) -> e !age < 1e-4) bands then h.alive <- false
        end
      done
    end
  in
  h

(* an instrument struck at [amp] (its level and the accent counted) *)
let strike ~(noise : unit -> float) (p : patch) (who : instrument) (amp : float) : hit =
  let tones_and_noise = tones_and_noise ~noise in
  let d = p.drums.(index who) in
  let tune base = base *. Float.pow 2. ((d.tuning -. 0.5) *. 0.5) in
  match who with
  | BD -> sine_drum ~f0:49.5 ~t60:(0.15 +. (1.5 *. d.decay)) ~sigh:0.12 ~punch:2.2 ~tone_hz:(200. *. Float.pow 25. d.tone) (1.2 *. amp) BD
  | LT | MT | HT ->
      let f = match who with LT -> 90. | MT -> 130. | _ -> 190. in
      let t60 = match who with LT -> 0.6 | MT -> 0.45 | _ -> 0.35 in
      sine_drum ~f0:(tune f) ~t60 ~sigh:0.08 ~punch:1.6 ~tone_hz:3000. (0.8 *. amp) who
  | SD ->
      tones_and_noise
        ~tones:[ (tune 180., 0.12, 0.5 *. (1. -. d.tone)); (tune 330., 0.1, 0.5 *. d.tone) ]
        ~noise_level:(0.8 *. d.snappy) ~filter:(High_pass, 1800., 0.7)
        ~envelope:(decay_env 0.25) (0.8 *. amp) SD
  | RS -> tones_and_noise ~tones:[ (500., 0.03, 0.6); (1700., 0.02, 0.4) ] ~noise_level:0. ~filter:(High_pass, 1000., 0.7) ~envelope:(fun _ -> 0.) (0.6 *. amp) RS
  | CP ->
      (* three hands 10 ms apart, then the tail *)
      let burst age = List.fold_left (fun s k -> let t = age - Signal.samples k in if t >= 0 then s +. decay_env 0.03 t else s) 0. [ 0.; 0.01; 0.02 ] in
      let envelope age = burst age +. if age >= Signal.samples 0.025 then 0.4 *. decay_env 0.3 (age - Signal.samples 0.025) else 0. in
      tones_and_noise ~tones:[] ~noise_level:1. ~filter:(Band_pass, 1100., 1.2) ~envelope (0.9 *. amp) CP
  | CB ->
      let envelope age = (0.6 *. decay_env 0.04 age) +. (0.4 *. decay_env 0.4 age) in
      metal_hit ~bands:[ ((fun m -> m.bell), 0., 0.7, envelope) ] (0.3 *. amp) CB
  | CH -> metal_hit ~bands:[ ((fun m -> m.high), 6000., 0.7, decay_env 0.07) ] (1.2 *. amp) CH
  | OH -> metal_hit ~bands:[ ((fun m -> m.high), 6000., 0.7, decay_env (0.25 +. d.decay)) ] (1.2 *. amp) OH
  | CY ->
      metal_hit
        ~bands:
          [
            ((fun m -> m.low), 0., 0.7, fun a -> (1. -. d.tone) *. decay_env (0.6 +. (3. *. d.decay)) a);
            ((fun m -> m.high), 0., 0.7, decay_env (0.4 +. (1.5 *. d.decay)));
            ((fun m -> m.high), 10500., 1.5, fun a -> (0.5 +. d.tone) *. decay_env (0.3 +. d.decay) a);
          ]
        (0.6 *. amp) CY

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

(* the six squares, 47.98% duty; the cowbell's the last two *)
let square_hz = [| 205.3; 304.4; 369.6; 522.7; 540.; 800. |]

type t = {
  mutable patch : patch;
  seq : Sequencer.t;
  mutable hits : hit list;
  mutable pending : (instrument * bool) list; (* struck live, at the next block *)
  squares : Vco.t array;
  bp_low : Svf.t;
  bp_high : Svf.t;
  bp_bell : Svf.t;
  metal : metal;
  mutable random : int; (* the noise's generator *)
  ring : Signal.t;
  mutable at : int;
}

let seq_pattern (p : patch) : Sequencer.step array = Array.init 16 (fun k -> Sequencer.note ~accent:p.accents.(k) k)

let create (patch : patch) : t =
  {
    patch;
    seq = Sequencer.create ~bpm:patch.tempo (seq_pattern patch);
    hits = [];
    pending = [];
    squares = Array.init 6 (fun _ -> Vco.create ());
    bp_low = Svf.create ();
    bp_high = Svf.create ();
    bp_bell = Svf.create ();
    metal = { low = [||]; high = [||]; bell = [||] };
    random = 1;
    ring = Array.make 2048 0.;
    at = 0;
  }

let patch (t : t) : patch = t.patch

let set_patch (t : t) (p : patch) : unit =
  if p.accents <> t.patch.accents then Sequencer.set_pattern t.seq (seq_pattern p);
  if p.tempo <> t.patch.tempo then Sequencer.set_bpm t.seq p.tempo;
  t.patch <- p

let run (t : t) (on : bool) : unit = if on then Sequencer.start t.seq else Sequencer.stop t.seq
let running (t : t) : bool = Sequencer.running t.seq
let step (t : t) : int = Sequencer.step t.seq
let hit (t : t) (who : instrument) ~(accent : bool) : unit = t.pending <- t.pending @ [ (who, accent) ]
let sounding (t : t) : int = List.length t.hits
let recent (t : t) : Signal.t = Array.init 2048 (fun i -> t.ring.((t.at + i) mod 2048))

let start_hit (t : t) (who : instrument) (accent : bool) : unit =
  let p = t.patch in
  let amp = p.drums.(index who).level *. if accent then 1. else 1. -. (0.5 *. p.accent) in
  (* the closed hat stops the open one *)
  if who = CH then List.iter (fun h -> if h.who = OH then h.choked <- true) t.hits;
  (* each hit its own noise, seeded from the voice's generator when
   * struck: how much a hit draws can't change another's *)
  t.random <- Noise.lcg t.random;
  let state = ref t.random in
  let noise () =
    state := Noise.lcg !state;
    Noise.uniform !state
  in
  t.hits <- t.hits @ [ strike ~noise p who amp ]

let is_metal = function CB | CY | OH | CH -> true | _ -> false

(* [m] samples of the squares and their two bands, always: the 808's
 * oscillators run all the time, and a hat struck finds them where they
 * are -- stopped between hits, where they'd be would depend on how the
 * blocks fell *)
let render_metal (t : t) (m : int) : unit =
  let sum = Array.make m 0. and bell = Array.make m 0. and buf = Array.make m 0. in
  Array.iteri
    (fun k osc ->
      Vco.fill ~width:(Array.make m 0.4798) osc Pulse ~frequency:(Array.make m square_hz.(k)) buf;
      Array.iteri (fun i x -> sum.(i) <- sum.(i) +. x) buf;
      if k >= 4 then Array.iteri (fun i x -> bell.(i) <- bell.(i) +. x) buf)
    t.squares;
  let low = Array.copy sum and high = Array.copy sum in
  Svf.process t.bp_low Zero_delay Band_pass ~cutoff:(Array.make m 3440.) ~q:1.5 low;
  Svf.process t.bp_high Zero_delay Band_pass ~cutoff:(Array.make m 7100.) ~q:1.5 high;
  Svf.process t.bp_bell Zero_delay Band_pass ~cutoff:(Array.make m 800.) ~q:0.7 bell;
  t.metal.low <- low;
  t.metal.high <- high;
  t.metal.bell <- bell

let render (t : t) (mono : Signal.t) (at : int) (m : int) : unit =
  if m > 0 then begin
    render_metal t m;
    List.iter (fun h -> h.add t.metal mono at m) t.hits;
    t.hits <- List.filter (fun h -> h.alive) t.hits
  end

(* ours: the patterns peaking under 1 (Unit_tr808) *)
let gain = 0.5

let fill (t : t) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  let mono = Array.make n 0. in
  List.iter (fun (who, accent) -> start_hit t who accent) t.pending;
  t.pending <- [];
  let events = ref [] in
  if running t then Sequencer.advance t.seq n (fun off ev -> events := (off, ev) :: !events);
  let pos = ref 0 in
  List.iter
    (fun (off, ev) ->
      render t mono !pos (off - !pos);
      pos := off;
      match ev with
      | Sequencer.Note_on { note = k; accent; _ } -> List.iter (fun i -> if t.patch.tracks.(index i).(k) then start_hit t i accent) instruments
      | Note_off -> ())
    (List.rev !events);
  render t mono !pos (n - !pos);
  let g = gain *. t.patch.volume in
  Array.iteri
    (fun i x ->
      out.left.(i) <- g *. x;
      out.right.(i) <- g *. x;
      t.ring.(t.at) <- g *. x;
      t.at <- (t.at + 1) mod 2048)
    mono

let instrument (t : t) : Instrument.t =
  {
    note_on = (fun key velocity -> Option.iter (fun who -> hit t who ~accent:(velocity > 0.8)) (of_key key));
    note_off = (fun _ -> ());
    set = (fun name x -> Option.iter (fun (k : knob) -> set_patch t (k.put t.patch x)) (List.find_opt (fun (k : knob) -> k.name = name) knobs));
    fill = fill t;
  }
