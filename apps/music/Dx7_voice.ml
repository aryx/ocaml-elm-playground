(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Dx7_voice.mli *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

type operator = {
  rates : int array;
  levels : int array;
  break_point : int;
  left_depth : int;
  right_depth : int;
  left_curve : int;
  right_curve : int;
  rate_scaling : int;
  amp_sensitivity : int;
  velocity : int;
  level : int;
  fixed : bool;
  coarse : int;
  fine : int;
  detune : int;
}

type patch = {
  name : string;
  operators : operator array;
  pitch_rates : int array;
  pitch_levels : int array;
  algorithm : int;
  feedback : int;
  key_sync : bool;
  lfo_speed : int;
  lfo_delay : int;
  lfo_pitch_depth : int;
  lfo_amp_depth : int;
  lfo_sync : bool;
  lfo_wave : int;
  pitch_sensitivity : int;
  transpose : int;
}

let waves = [ "triangle"; "saw down"; "saw up"; "square"; "sine"; "s&h" ]
let curves = [ "-lin"; "-exp"; "+exp"; "+lin" ]

let op ?(rates = [| 99; 99; 99; 99 |]) ?(levels = [| 99; 99; 99; 0 |]) ?(level = 99) ?(coarse = 1) ?(fine = 0) ?(detune = 7)
    ?(velocity = 0) ?(rate_scaling = 0) ?(fixed = false) ?(break_point = 39) ?(left_depth = 0) ?(right_depth = 0)
    ?(left_curve = 0) ?(right_curve = 0) ?(amp_sensitivity = 0) () : operator =
  {
    rates;
    levels;
    break_point;
    left_depth;
    right_depth;
    left_curve;
    right_curve;
    rate_scaling;
    amp_sensitivity;
    velocity;
    level;
    fixed;
    coarse;
    fine;
    detune;
  }

let initial : patch =
  {
    name = "INIT VOICE";
    operators = Array.init 6 (fun i -> op ~level:(if i = 0 then 99 else 0) ());
    pitch_rates = [| 99; 99; 99; 99 |];
    pitch_levels = [| 50; 50; 50; 50 |];
    algorithm = 1;
    feedback = 0;
    key_sync = true;
    lfo_speed = 35;
    lfo_delay = 0;
    lfo_pitch_depth = 0;
    lfo_amp_depth = 0;
    lfo_sync = true;
    lfo_wave = 0;
    pitch_sensitivity = 3;
    transpose = 24;
  }

(*****************************************************************************)
(* The bytes *)
(*****************************************************************************)

(* the packed voice (Dexed's unpackProgram): op 6 first, 17 bytes each *)
let of_packed (s : string) : patch =
  let byte i = if i < String.length s then Char.code s.[i] land 0x7f else 0 in
  let clamp hi x = max 0 (min hi x) in
  let operator o =
    let b = (6 - o) * 17 in
    {
      rates = Array.init 4 (fun i -> clamp 99 (byte (b + i)));
      levels = Array.init 4 (fun i -> clamp 99 (byte (b + 4 + i)));
      break_point = clamp 99 (byte (b + 8));
      left_depth = clamp 99 (byte (b + 9));
      right_depth = clamp 99 (byte (b + 10));
      left_curve = byte (b + 11) land 3;
      right_curve = (byte (b + 11) lsr 2) land 3;
      rate_scaling = byte (b + 12) land 7;
      detune = clamp 14 (byte (b + 12) lsr 3);
      amp_sensitivity = byte (b + 13) land 3;
      velocity = (byte (b + 13) lsr 2) land 7;
      level = clamp 99 (byte (b + 14));
      fixed = byte (b + 15) land 1 = 1;
      coarse = (byte (b + 15) lsr 1) land 31;
      fine = clamp 99 (byte (b + 16));
    }
  in
  {
    name = String.init 10 (fun i -> match byte (118 + i) with c when c >= 32 && c < 127 -> Char.chr c | _ -> ' ');
    operators = Array.init 6 (fun i -> operator (i + 1));
    pitch_rates = Array.init 4 (fun i -> clamp 99 (byte (102 + i)));
    pitch_levels = Array.init 4 (fun i -> clamp 99 (byte (106 + i)));
    algorithm = (byte 110 land 31) + 1;
    feedback = byte 111 land 7;
    key_sync = (byte 111 lsr 3) land 1 = 1;
    lfo_speed = clamp 99 (byte 112);
    lfo_delay = clamp 99 (byte 113);
    lfo_pitch_depth = clamp 99 (byte 114);
    lfo_amp_depth = clamp 99 (byte 115);
    lfo_sync = byte 116 land 1 = 1;
    lfo_wave = clamp 5 ((byte 116 lsr 1) land 7);
    pitch_sensitivity = (byte 116 lsr 4) land 7;
    transpose = clamp 48 (byte 117);
  }

let to_packed (p : patch) : string =
  let b = Bytes.make 128 '\000' in
  let set i x = Bytes.set b i (Char.chr (x land 0x7f)) in
  Array.iteri
    (fun i (o : operator) ->
      let at = (5 - i) * 17 in
      Array.iteri (fun k r -> set (at + k) r) o.rates;
      Array.iteri (fun k l -> set (at + 4 + k) l) o.levels;
      set (at + 8) o.break_point;
      set (at + 9) o.left_depth;
      set (at + 10) o.right_depth;
      set (at + 11) (o.left_curve lor (o.right_curve lsl 2));
      set (at + 12) (o.rate_scaling lor (o.detune lsl 3));
      set (at + 13) (o.amp_sensitivity lor (o.velocity lsl 2));
      set (at + 14) o.level;
      set (at + 15) ((if o.fixed then 1 else 0) lor (o.coarse lsl 1));
      set (at + 16) o.fine)
    p.operators;
  Array.iteri (fun k r -> set (102 + k) r) p.pitch_rates;
  Array.iteri (fun k l -> set (106 + k) l) p.pitch_levels;
  set 110 (p.algorithm - 1);
  set 111 (p.feedback lor ((if p.key_sync then 1 else 0) lsl 3));
  set 112 p.lfo_speed;
  set 113 p.lfo_delay;
  set 114 p.lfo_pitch_depth;
  set 115 p.lfo_amp_depth;
  set 116 ((if p.lfo_sync then 1 else 0) lor (p.lfo_wave lsl 1) lor (p.pitch_sensitivity lsl 4));
  set 117 p.transpose;
  String.iteri (fun i c -> if i < 10 then set (118 + i) (Char.code c)) (Printf.sprintf "%-10s" p.name);
  Bytes.to_string b

(* a bulk dump of 32 voices: F0 43 00 09 20 00, 4096 bytes, the
 * checksum (the bytes' sum negated, 7 bits), F7 *)
let header = "\xf0\x43\x00\x09\x20\x00"
let checksum (data : string) : int = -String.fold_left (fun s c -> s + Char.code c) 0 data land 0x7f

let cartridge (s : string) : (patch array, string) result =
  if String.length s < 4104 then Error (Printf.sprintf "%d bytes, not a cartridge's 4104" (String.length s))
  else if String.sub s 0 6 <> header then Error "not a DX7 32-voice bulk dump (its header)"
  else
    let data = String.sub s 6 4096 in
    if Char.code s.[4102] <> checksum data then Error "the checksum is wrong"
    else Ok (Array.init 32 (fun i -> of_packed (String.sub data (i * 128) 128)))

let to_cartridge (voices : patch array) : string =
  let data = String.concat "" (List.init 32 (fun i -> to_packed (if i < Array.length voices then voices.(i) else initial))) in
  header ^ data ^ String.make 1 (Char.chr (checksum data)) ^ "\xf7"

(*****************************************************************************)
(* The patch as text *)
(*****************************************************************************)

type knob = patch Patch_text.knob

let numbers hi = List.init (hi + 1) string_of_int

(* an integer 0 to [hi], written as itself *)
let value name hi get put = Patch_text.selector name (numbers hi) get put

let operator_knobs (i : int) : knob list =
  let name s = Printf.sprintf "op%d.%s" (i + 1) s in
  let get f (p : patch) = f p.operators.(i) in
  let put f (p : patch) x =
    let ops = Array.copy p.operators in
    ops.(i) <- f ops.(i) x;
    { p with operators = ops }
  in
  let four label field set =
    List.init 4 (fun k ->
        value (name (Printf.sprintf "%s%d" label (k + 1))) 99
          (get (fun o -> (field o).(k)))
          (put (fun o x ->
               let a = Array.copy (field o) in
               a.(k) <- x;
               set o a)))
  in
  four "rate" (fun o -> o.rates) (fun o a -> { o with rates = a })
  @ four "level" (fun o -> o.levels) (fun o a -> { o with levels = a })
  @ [
      value (name "output") 99 (get (fun o -> o.level)) (put (fun o x -> { o with level = x }));
      Patch_text.switch (name "fixed") (get (fun o -> o.fixed)) (put (fun o x -> { o with fixed = x }));
      value (name "coarse") 31 (get (fun o -> o.coarse)) (put (fun o x -> { o with coarse = x }));
      value (name "fine") 99 (get (fun o -> o.fine)) (put (fun o x -> { o with fine = x }));
      Patch_text.selector (name "detune") (List.init 15 (fun d -> string_of_int (d - 7))) (get (fun o -> o.detune))
        (put (fun o x -> { o with detune = x }));
      value (name "break_point") 99 (get (fun o -> o.break_point)) (put (fun o x -> { o with break_point = x }));
      value (name "left_depth") 99 (get (fun o -> o.left_depth)) (put (fun o x -> { o with left_depth = x }));
      value (name "right_depth") 99 (get (fun o -> o.right_depth)) (put (fun o x -> { o with right_depth = x }));
      Patch_text.selector (name "left_curve") curves (get (fun o -> o.left_curve)) (put (fun o x -> { o with left_curve = x }));
      Patch_text.selector (name "right_curve") curves (get (fun o -> o.right_curve)) (put (fun o x -> { o with right_curve = x }));
      value (name "rate_scaling") 7 (get (fun o -> o.rate_scaling)) (put (fun o x -> { o with rate_scaling = x }));
      value (name "amp_sensitivity") 3 (get (fun o -> o.amp_sensitivity)) (put (fun o x -> { o with amp_sensitivity = x }));
      value (name "velocity") 7 (get (fun o -> o.velocity)) (put (fun o x -> { o with velocity = x }));
    ]

let knobs : knob list =
  let four label get set =
    List.init 4 (fun k ->
        value (Printf.sprintf "pitch.%s%d" label (k + 1)) 99
          (fun p -> (get p).(k))
          (fun p x ->
            let a = Array.copy (get p) in
            a.(k) <- x;
            set p a))
  in
  [
    value "algorithm" 32 (fun p -> p.algorithm) (fun p x -> { p with algorithm = max 1 x });
    value "feedback" 7 (fun p -> p.feedback) (fun p x -> { p with feedback = x });
    Patch_text.switch "key_sync" (fun p -> p.key_sync) (fun p x -> { p with key_sync = x });
  ]
  @ List.concat (List.init 6 operator_knobs)
  @ four "rate" (fun p -> p.pitch_rates) (fun p a -> { p with pitch_rates = a })
  @ four "level" (fun p -> p.pitch_levels) (fun p a -> { p with pitch_levels = a })
  @ [
      Patch_text.selector "lfo.wave" waves (fun p -> p.lfo_wave) (fun p x -> { p with lfo_wave = x });
      value "lfo.speed" 99 (fun p -> p.lfo_speed) (fun p x -> { p with lfo_speed = x });
      value "lfo.delay" 99 (fun p -> p.lfo_delay) (fun p x -> { p with lfo_delay = x });
      value "lfo.pitch_depth" 99 (fun p -> p.lfo_pitch_depth) (fun p x -> { p with lfo_pitch_depth = x });
      value "lfo.amp_depth" 99 (fun p -> p.lfo_amp_depth) (fun p x -> { p with lfo_amp_depth = x });
      Patch_text.switch "lfo.sync" (fun p -> p.lfo_sync) (fun p x -> { p with lfo_sync = x });
      value "pitch_sensitivity" 7 (fun p -> p.pitch_sensitivity) (fun p x -> { p with pitch_sensitivity = x });
      Patch_text.selector "transpose" (List.init 49 (fun d -> string_of_int (d - 24))) (fun p -> p.transpose)
        (fun p x -> { p with transpose = x });
    ]

(* the name is no control: its own line, read before the others *)
let to_string (p : patch) : string = Printf.sprintf "name = %s\n" (String.trim p.name) ^ Patch_text.to_string knobs p

let of_string (text : string) : (patch, string) result =
  let lines = String.split_on_char '\n' text in
  let is_name l = match String.index_opt l '=' with Some i -> String.trim (String.sub l 0 i) = "name" | None -> false in
  let name =
    match List.find_opt is_name lines with
    | Some l ->
        let i = String.index l '=' in
        String.trim (String.sub l (i + 1) (String.length l - i - 1))
    | None -> initial.name
  in
  Result.map
    (fun p -> { p with name = String.sub (Printf.sprintf "%-10s" name) 0 10 })
    (Patch_text.of_string knobs ~initial (String.concat "\n" (List.filter (fun l -> not (is_name l)) lines)))

(*****************************************************************************)
(* Our patches *)
(*****************************************************************************)

(* ours, after the kinds of sound the DX7 was known for, not copies of
 * Yamaha's: each says the algorithm's shape it uses *)
let presets : (string * patch) list =
  let voice name algorithm feedback ops =
    { initial with name = Printf.sprintf "%-10s" name; algorithm; feedback; operators = Array.of_list ops }
  in
  [
    (* three pairs side by side (algorithm 5): two soft piano pairs,
     * detuned, and a pair whose modulator at 14 times, quickly gone,
     * is the tine's metallic strike; velocity brightens them *)
    ( "e.piano",
      voice "E.PIANO" 5 6
        [
          op ~rates:[| 96; 25; 25; 67 |] ~levels:[| 99; 75; 0; 0 |] ~velocity:2 ~rate_scaling:3 ();
          op ~rates:[| 95; 50; 35; 78 |] ~levels:[| 99; 75; 0; 0 |] ~level:72 ~velocity:7 ~rate_scaling:3 ();
          op ~rates:[| 95; 20; 20; 50 |] ~levels:[| 99; 95; 0; 0 |] ~level:90 ~detune:10 ~velocity:2 ~rate_scaling:3 ();
          op ~rates:[| 95; 29; 20; 50 |] ~levels:[| 99; 95; 0; 0 |] ~level:58 ~coarse:14 ~velocity:6 ~rate_scaling:3 ();
          op ~rates:[| 95; 20; 20; 50 |] ~levels:[| 99; 95; 0; 0 |] ~level:90 ~detune:4 ~velocity:2 ~rate_scaling:3 ();
          op ~rates:[| 95; 50; 35; 78 |] ~levels:[| 99; 75; 0; 0 |] ~level:66 ~velocity:7 ~rate_scaling:3 ();
        ] );
    (* one modulator, 6 fed back, on three carriers (algorithm 22), a
     * slower attack on the modulators than the carriers': the index
     * swells after the note starts, as a brass player's tone opens *)
    ( "brass",
      {
        (voice "BRASS" 22 7
           [
             op ~rates:[| 72; 76; 99; 71 |] ~levels:[| 99; 88; 96; 0 |] ();
             op ~rates:[| 62; 51; 29; 71 |] ~levels:[| 82; 95; 96; 0 |] ~level:80 ~velocity:3 ();
             op ~rates:[| 77; 36; 41; 71 |] ~levels:[| 99; 98; 98; 0 |] ~level:90 ~detune:5 ();
             op ~rates:[| 77; 36; 41; 71 |] ~levels:[| 99; 98; 98; 0 |] ~level:90 ~detune:9 ();
             op ~rates:[| 77; 36; 41; 71 |] ~levels:[| 99; 98; 98; 0 |] ~level:88 ~detune:7 ();
             op ~rates:[| 49; 99; 28; 68 |] ~levels:[| 98; 98; 91; 0 |] ~level:78 ~velocity:3 ();
           ])
        with
        pitch_rates = [| 84; 95; 95; 60 |];
        pitch_levels = [| 47; 50; 50; 50 |];
      } );
    (* a stack of four and a pair (algorithm 1), all an octave down: the
     * modulators fall fast, a pluck, then a round tone *)
    ( "bass",
      voice "BASS" 1 5
        [
          op ~rates:[| 99; 40; 20; 70 |] ~levels:[| 99; 70; 0; 0 |] ~coarse:0 ();
          op ~rates:[| 99; 60; 30; 80 |] ~levels:[| 99; 50; 0; 0 |] ~level:85 ~coarse:0 ~velocity:5 ();
          op ~rates:[| 99; 40; 20; 70 |] ~levels:[| 99; 70; 0; 0 |] ~level:80 ~coarse:0 ~detune:9 ();
          op ~rates:[| 99; 55; 30; 80 |] ~levels:[| 99; 40; 0; 0 |] ~level:76 ();
          op ~rates:[| 99; 55; 30; 80 |] ~levels:[| 99; 30; 0; 0 |] ~level:60 ();
          op ~rates:[| 99; 55; 30; 80 |] ~levels:[| 99; 30; 0; 0 |] ~level:50 ();
        ] );
    (* three pairs (algorithm 5) at ratios that aren't whole numbers:
     * 3.5, 1.41, the sidebands between the harmonics, inharmonic, a bell;
     * long decays *)
    ( "bells",
      voice "BELLS" 5 0
        [
          op ~rates:[| 99; 24; 20; 30 |] ~levels:[| 99; 60; 0; 0 |] ~rate_scaling:2 ();
          op ~rates:[| 99; 30; 25; 30 |] ~levels:[| 99; 50; 0; 0 |] ~level:75 ~coarse:3 ~fine:50 ~velocity:4 ();
          op ~rates:[| 99; 28; 22; 30 |] ~levels:[| 99; 55; 0; 0 |] ~level:82 ~coarse:2 ~detune:9 ~rate_scaling:2 ();
          op ~rates:[| 99; 35; 25; 30 |] ~levels:[| 99; 40; 0; 0 |] ~level:68 ~coarse:1 ~fine:41 ~velocity:4 ();
          op ~rates:[| 99; 32; 24; 30 |] ~levels:[| 99; 50; 0; 0 |] ~level:72 ~coarse:4 ~detune:11 ~rate_scaling:2 ();
          op ~rates:[| 99; 40; 25; 30 |] ~levels:[| 99; 30; 0; 0 |] ~level:66 ~coarse:1 ~fine:41 ();
        ] );
    (* two pairs (algorithm 5, 5 and 6 silent): a bar struck, its sound
     * gone in a moment, the modulator at 4 its hard mallet *)
    ( "marimba",
      voice "MARIMBA" 5 0
        [
          op ~rates:[| 99; 45; 30; 60 |] ~levels:[| 99; 0; 0; 0 |] ~rate_scaling:4 ();
          op ~rates:[| 99; 70; 40; 70 |] ~levels:[| 99; 0; 0; 0 |] ~level:70 ~coarse:4 ~velocity:5 ~rate_scaling:4 ();
          op ~rates:[| 99; 60; 40; 60 |] ~levels:[| 99; 0; 0; 0 |] ~level:66 ~coarse:4 ~rate_scaling:4 ();
          op ~rates:[| 99; 60; 40; 70 |] ~levels:[| 99; 0; 0; 0 |] ~level:50 ~rate_scaling:4 ();
          op ~level:0 ();
          op ~level:0 ();
        ] );
    (* six carriers (algorithm 32), the Hammond's footages as ratios:
     * 0.5 (16'), 1 (8'), 1.5 (5 1/3'), 2 (4'), 3 (2 2/3'), 4 (2'):
     * additive synthesis on an FM machine *)
    ( "organ",
      let organ level coarse fine = op ~rates:[| 99; 99; 99; 90 |] ~levels:[| 99; 99; 99; 0 |] ~level ~coarse ~fine () in
      voice "ORGAN" 32 0 [ organ 90 0 0; organ 99 1 0; organ 84 1 50; organ 88 2 0; organ 80 3 0; organ 78 4 0 ] );
  ]

(*****************************************************************************)
(* The formulas (Dexed's dx7note.cc) *)
(*****************************************************************************)

let log2_of_note (note : int) : float = Float.log2 440. +. (float_of_int (note - 69) /. 12.)

let frequency (o : operator) (note : int) : float =
  if o.fixed then
    Float.pow 10. (float_of_int (o.coarse land 3) +. (float_of_int o.fine /. 100.))
    *. (if o.detune > 7 then Float.pow 2. (13457. *. float_of_int (o.detune - 7) /. 16777216.) else 1.)
  else
    let octaves = log2_of_note note in
    let detune = 0.0209 *. exp (-0.396 *. octaves) /. 7. *. octaves *. float_of_int (o.detune - 7) in
    let ratio = if o.coarse = 0 then 0.5 else float_of_int o.coarse in
    Float.pow 2. (octaves +. detune) *. ratio *. (1. +. (float_of_int o.fine /. 100.))

let exp_scale = [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 11; 14; 16; 19; 23; 27; 33; 39; 47; 56; 66; 80; 94; 110; 126; 142; 158; 174; 190; 206; 222; 238; 250 |]

let scale_curve (group : int) (depth : int) (curve : int) : int =
  let scale =
    if curve = 0 || curve = 3 then (group * depth * 329) lsr 12
    else (exp_scale.(min group (Array.length exp_scale - 1)) * depth * 329) lsr 15
  in
  if curve < 2 then -scale else scale

let level_scaling (o : operator) (note : int) : int =
  let offset = note - o.break_point - 17 in
  if offset >= 0 then scale_curve ((offset + 1) / 3) o.right_depth o.right_curve
  else scale_curve (-(offset - 1) / 3) o.left_depth o.left_curve

let velocity_table =
  [| 0; 70; 86; 97; 106; 114; 121; 126; 132; 138; 142; 148; 152; 156; 160; 163; 166; 170; 173; 174; 178; 181; 184; 186; 189; 190; 194; 196;
     198; 200; 202; 205; 206; 209; 211; 214; 216; 218; 220; 222; 224; 225; 227; 229; 230; 232; 233; 235; 237; 238; 240; 241; 242; 243; 244;
     246; 246; 248; 249; 250; 251; 252; 253; 254 |]

let scale_velocity (velocity : int) (sensitivity : int) : int =
  let v = velocity_table.(max 0 (min 127 velocity) lsr 1) - 239 in
  (((sensitivity * v) + 7) asr 3) lsl 4

let output_level (o : operator) ~(note : int) ~(velocity : int) : int =
  let level = min 127 (Dx_envelope.scale_output_level o.level + level_scaling o note) in
  max 0 ((level lsl 5) + scale_velocity velocity o.velocity)

let rate_scaling (o : operator) (note : int) : int =
  let x = min 31 (max 0 ((note / 3) - 7)) in
  (o.rate_scaling * x) lsr 3

(*****************************************************************************)
(* The pitch envelope and the LFO *)
(*****************************************************************************)

(* the pitch envelope's levels, in 32nds of an octave, and its rates,
 * in 21.3rds of an octave a second (Dexed's pitchenv.cc) *)
let pitch_table =
  [| -128; -116; -104; -95; -85; -76; -68; -61; -56; -52; -49; -46; -43; -41; -39; -37; -35; -33; -32; -31; -30; -29; -28; -27; -26; -25; -24;
     -23; -22; -21; -20; -19; -18; -17; -16; -15; -14; -13; -12; -11; -10; -9; -8; -7; -6; -5; -4; -3; -2; -1; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9;
     10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 38; 40; 43; 46; 49; 53; 58; 65;
     73; 82; 92; 103; 115; 127 |]

let pitch_rate_table =
  [| 1; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 13; 13; 14; 14; 15; 16; 16; 17; 18; 18; 19; 20; 21; 22; 23; 24; 25;
     26; 27; 28; 30; 31; 33; 34; 36; 37; 38; 39; 41; 42; 44; 46; 47; 49; 51; 53; 54; 56; 58; 60; 62; 64; 66; 68; 70; 72; 74; 76; 79; 82; 85; 88;
     91; 94; 98; 102; 106; 110; 115; 120; 125; 130; 135; 141; 147; 153; 159; 165; 171; 178; 185; 193; 202; 211; 232; 243; 254; 255 |]

let rate = float_of_int Signal.rate

type pitch_envelope = {
  p_rates : int array;
  p_levels : int array;
  mutable p_stage : int;
  mutable p_level : float; (* octaves *)
  mutable p_target : float;
  mutable p_speed : float; (* octaves a sample *)
  mutable p_down : bool;
}

let pitch_advance (e : pitch_envelope) (stage : int) : unit =
  e.p_stage <- stage;
  if stage < 4 then begin
    e.p_target <- float_of_int pitch_table.(e.p_levels.(stage)) /. 32.;
    e.p_speed <- float_of_int pitch_rate_table.(e.p_rates.(stage)) /. 21.3 /. rate
  end

let pitch_envelope (p : patch) : pitch_envelope =
  let e =
    {
      p_rates = p.pitch_rates;
      p_levels = p.pitch_levels;
      p_stage = 0;
      p_level = float_of_int pitch_table.(p.pitch_levels.(3)) /. 32.;
      p_target = 0.;
      p_speed = 0.;
      p_down = true;
    }
  in
  pitch_advance e 0;
  e

(* the pitch envelope [n] samples on; in octaves *)
let pitch_step (e : pitch_envelope) (n : int) : float =
  for _ = 1 to n do
    if e.p_stage < 3 || (e.p_stage = 3 && not e.p_down) then
      if e.p_level < e.p_target then begin
        e.p_level <- Float.min e.p_target (e.p_level +. e.p_speed);
        if e.p_level >= e.p_target then pitch_advance e (e.p_stage + 1)
      end
      else begin
        e.p_level <- Float.max e.p_target (e.p_level -. e.p_speed);
        if e.p_level <= e.p_target then pitch_advance e (e.p_stage + 1)
      end
  done;
  e.p_level

(* the LFO's speeds, Hz before Dexed's 4437500000 / 2^32 (lfo.cc) *)
let lfo_speeds =
  [| 0.062541; 0.125031; 0.312393; 0.437120; 0.624610; 0.750694; 0.936330; 1.125302; 1.249609; 1.436782; 1.560915; 1.752081; 1.875117;
     2.062494; 2.247191; 2.374451; 2.560492; 2.686728; 2.873976; 2.998950; 3.188013; 3.369840; 3.500175; 3.682224; 3.812065; 4.000800;
     4.186202; 4.310716; 4.501260; 4.623209; 4.814636; 4.930480; 5.121901; 5.315191; 5.434783; 5.617346; 5.750431; 5.946717; 6.062811;
     6.248438; 6.431695; 6.564264; 6.749460; 6.868132; 7.052186; 7.250580; 7.375719; 7.556294; 7.687577; 7.877738; 7.993605; 8.181967;
     8.372405; 8.504848; 8.685079; 8.810573; 8.986341; 9.122423; 9.300595; 9.500285; 9.607994; 9.798158; 9.950249; 10.117361; 11.251125;
     11.384335; 12.562814; 13.676149; 13.904338; 15.092062; 16.366612; 16.638935; 17.869907; 19.193858; 19.425019; 20.833333; 21.034918;
     22.502250; 24.003841; 24.260068; 25.746653; 27.173913; 27.578599; 29.052876; 30.693677; 31.191516; 32.658393; 34.317090; 34.674064;
     36.416606; 38.197097; 38.550501; 40.387722; 40.749796; 42.625746; 44.326241; 44.883303; 46.772685; 48.590865; 49.261084 |]

let lfo_hz (speed : int) : float = lfo_speeds.(max 0 (min 99 speed)) *. 4437500000. /. 4294967296.

(* the LFO, one for the instrument: its phase, its delay's ramp (0 to
 * 1: silent below 1/2, then ramping in, Dexed's two speeds), the
 * sample and hold's random byte *)
type lfo = { mutable phase : float; mutable delay : float; mutable random : int; mutable held : float }

let lfo_key (l : lfo) (p : patch) : unit =
  if p.lfo_sync then l.phase <- 0.5;
  l.delay <- 0.

(* the delay's speeds, in its [0, 1] a sample *)
let delay_speeds (p : patch) : float * float =
  let a = 99 - p.lfo_delay in
  if a = 99 then (infinity, infinity)
  else
    let a = (16 + (a land 15)) lsl (1 + (a lsr 4)) in
    let unit = 25190424. /. rate /. 4294967296. in
    (unit *. float_of_int a, unit *. float_of_int (max 0x80 (a land 0xff80)))

(* [lfo_step l p n]: [n] samples on; the value, 0 to 1, and the delay's
 * ramp, 0 to 1 *)
let lfo_step (l : lfo) (p : patch) (n : int) : float * float =
  l.phase <- l.phase +. (lfo_hz p.lfo_speed *. float_of_int n /. rate);
  let wrapped = l.phase >= 1. in
  l.phase <- l.phase -. Float.of_int (Float.to_int l.phase);
  let first, second = delay_speeds p in
  for _ = 1 to n do
    l.delay <- l.delay +. if l.delay < 0.5 then first else second
  done;
  let ramp = if l.delay >= 1. then 1. else if l.delay < 0.5 then 0. else (2. *. l.delay) -. 1. in
  let x = l.phase in
  let value =
    match p.lfo_wave with
    | 0 -> if x < 0.5 then 2. *. x else 2. -. (2. *. x)
    | 1 -> Float.rem (1.5 -. x) 1.
    | 2 -> Float.rem (x +. 0.5) 1.
    | 3 -> if x < 0.5 then 1. else 0.
    | 4 -> 0.5 +. (0.5 *. sin (2. *. Float.pi *. x))
    | _ ->
        (* a new random value each time round *)
        if wrapped then begin
          l.random <- ((l.random * 179) + 17) land 0xff;
          l.held <- float_of_int ((l.random lxor 0x80) + 1) /. 256.
        end;
        l.held
  in
  (value, ramp)

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

(* the pitch LFO's sensitivities, and the amplitude's (Dexed's tables:
 * 255 and 2^24 the most) *)
let pitch_sensitivities = [| 0; 10; 20; 33; 55; 92; 153; 255 |]
let amp_sensitivities = [| 0.; 0.259; 0.427; 1. |]

(* a note's LFO, the same for all its operators: the pitch in octaves,
 * the amplitude's cut in steps before each operator's sensitivity *)
type modulation = { mutable pitch : float; mutable amp : float }

type t = {
  mutable patch : patch;
  poly : Polyphony.t;
  lfo : lfo;
  modulation : modulation;
  mutable volume : float;
  meters : float array;
  mutable block : Signal.t;
  (* the last samples played, a ring, [at] the next to write *)
  ring : Signal.t;
  mutable at : int;
}

(* a note's samples come 64 at a time, as Dexed's do: the pitch (its
 * envelope and the LFO) computed once per block, the levels every
 * sample *)
let block = 64

let voice (t : t) (key : int) (velocity : int) : Polyphony.voice =
  let p = t.patch in
  let note = key + p.transpose - 24 in
  let alg = Fm_algorithm.get p.algorithm in
  let ops = p.operators in
  let envelopes =
    Array.map
      (fun o ->
        Dx_envelope.create ~rates:o.rates ~levels:o.levels ~output_level:(output_level o ~note ~velocity)
          ~rate_scaling:(rate_scaling o note) ())
      ops
  in
  let frequencies = Array.map (fun o -> frequency o note) ops in
  let pitch = pitch_envelope p in
  (* ours: each note's operators start at phase 0, key sync on or off
   * (off, the DX7's run freely, so a note starts where they are) *)
  let state = Fm_algorithm.create () in
  let increments = Array.make 6 0. and amplitudes = Array.make 6 0. and steps = Array.make 6 0. in
  (* each operator's gain at the block's start and end: the envelope
   * moved once a block, its gain, a power of 2, computed there and
   * ramped between, as Dexed's are *)
  let starts = Array.make 6 0. and ends = Array.make 6 0. in
  let held = ref true in
  let fill (out : Signal.t) =
    let n = Array.length out in
    let octaves = pitch_step pitch n +. t.modulation.pitch in
    let shift = Float.pow 2. octaves in
    Array.iteri (fun i o -> increments.(i) <- (if o.fixed then frequencies.(i) else frequencies.(i) *. shift) /. rate) ops;
    for i = 0 to 5 do
      steps.(i) <- Dx_envelope.run envelopes.(i) n;
      let cut = t.modulation.amp *. amp_sensitivities.(ops.(i).amp_sensitivity) in
      starts.(i) <- ends.(i);
      ends.(i) <- (if ops.(i).level = 0 then 0. else Dx_envelope.gain (steps.(i) -. cut))
    done;
    for s = 0 to n - 1 do
      let x = float_of_int (s + 1) /. float_of_int n in
      for i = 0 to 5 do
        amplitudes.(i) <- starts.(i) +. (x *. (ends.(i) -. starts.(i)))
      done;
      out.(s) <- Fm_algorithm.sample alg state ~feedback:p.feedback ~increments ~amplitudes
    done;
    (* the meters: each operator's amplitude, its envelope, the loudest
     * of the voices *)
    for i = 0 to 5 do
      t.meters.(i) <- Float.max t.meters.(i) ends.(i)
    done
  in
  let release () =
    held := false;
    Array.iter Dx_envelope.key_up envelopes;
    pitch.p_down <- false;
    pitch_advance pitch 3
  in
  (* silent: let go, every carrier's envelope at its end, at the floor *)
  let silent () =
    (not !held)
    && List.for_all (fun c -> Dx_envelope.stage envelopes.(c - 1) = 4 && steps.(c - 1) <= Dx_envelope.floor +. 1.) alg.carriers
  in
  { release; fill; silent }

let create ?(voices = 16) (patch : patch) : t =
  {
    patch;
    poly = Polyphony.create ~voices ();
    lfo = { phase = 0.; delay = 1.; random = 0; held = 0.5 };
    modulation = { pitch = 0.; amp = 0. };
    volume = 0.7;
    meters = Array.make 6 0.;
    block = Array.make block 0.;
    ring = Array.make 2048 0.;
    at = 0;
  }

let patch (t : t) : patch = t.patch
let set_patch (t : t) (p : patch) : unit = t.patch <- p
let voices (t : t) : int = Polyphony.voices t.poly
let recent (t : t) : Signal.t = Array.init 2048 (fun i -> t.ring.((t.at + i) mod 2048))
let levels (t : t) : float array = Array.copy t.meters

(* a carrier at full is 2 cycles; an eighth of it heard, so eight
 * carriers at full reach 1: a four-note chord of the brass's four
 * carriers peaks at 0.69 (Unit_dx7) *)
let gain = 0.0625

let fill (t : t) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  let p = t.patch in
  Array.fill t.meters 0 6 0.;
  let at = ref 0 in
  while !at < n do
    let m = min block (n - !at) in
    if Array.length t.block <> m then t.block <- Array.make m 0.;
    (* the LFO: into the pitch, depth times sensitivity, up to about an
     * octave (Dexed's), into the level, ours: to silence at full *)
    let value, ramp = lfo_step t.lfo p m in
    let depth = (p.lfo_pitch_depth * 165) lsr 6 in
    t.modulation.pitch <-
      float_of_int depth *. ramp *. float_of_int pitch_sensitivities.(p.pitch_sensitivity) *. (value -. 0.5) /. 32768.;
    t.modulation.amp <- float_of_int ((p.lfo_amp_depth * 165) lsr 6) /. 255. *. ramp *. (1. -. value) *. 3840.;
    Polyphony.fill t.poly t.block;
    for i = 0 to m - 1 do
      let x = gain *. t.volume *. t.block.(i) in
      out.left.(!at + i) <- x;
      out.right.(!at + i) <- x;
      t.ring.(t.at) <- x;
      t.at <- (t.at + 1) mod 2048
    done;
    at := !at + m
  done

let instrument (t : t) : Instrument.t =
  {
    note_on =
      (fun key velocity ->
        lfo_key t.lfo t.patch;
        Polyphony.press t.poly key (voice t key (Float.to_int (Float.round (velocity *. 127.)))));
    note_off = (fun key -> Polyphony.release t.poly key);
    set =
      (fun name x ->
        if name = "volume" then t.volume <- x
        else Option.iter (fun (k : knob) -> t.patch <- k.put t.patch x) (List.find_opt (fun (k : knob) -> k.name = name) knobs));
    fill = fill t;
  }
