(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let steps = 16
let note_out = 0
let gate_out = 1
let curve_out = 2

let position ~tempo (pos : float) : int * float =
  (* a sixteenth: a quarter of a beat *)
  let per_step = float_of_int Signal.rate *. 60. /. tempo /. 4. in
  let s = pos /. per_step in
  (int_of_float s mod steps, s -. Float.of_int (int_of_float s))

(* a bass line to start with: C, its octave, a fifth, tied into the
 * next *)
let first_notes = [| 36; 48; 36; 43; 36; 48; 46; 48; 36; 48; 36; 43; 39; 41; 43; 48 |]

let create () : Rack_device.t =
  let note = Array.copy first_notes and gate = Array.make steps 0.8 in
  let tie = Array.make steps false and curve = Array.init steps (fun k -> float_of_int k /. float_of_int (steps - 1)) in
  tie.(7) <- true;
  gate.(10) <- 0.;
  let running = ref false and tempo = ref 120. and pos = ref 0. in
  let run (io : Rack_device.io) =
    if !running then begin
      let s, frac = position ~tempo:!tempo !pos in
      let open_ = gate.(s) > 0. && (tie.(s) || frac < 0.5) in
      io.cv_out note_out (float_of_int note.(s) /. 127.);
      io.cv_out gate_out (if open_ then gate.(s) else 0.);
      io.cv_out curve_out curve.(s);
      pos := !pos +. float_of_int Rack_device.chunk
    end
    else io.cv_out gate_out 0.
  in
  (* "step3.note": the field and the step *)
  let find name =
    match String.index_opt name '.' with
    | Some dot when String.length name > 4 && String.sub name 0 4 = "step" -> (
        match int_of_string_opt (String.sub name 4 (dot - 4)) with
        | Some k when k >= 1 && k <= steps -> Some (String.sub name (dot + 1) (String.length name - dot - 1), k - 1)
        | _ -> None)
    | _ -> None
  in
  let set name v =
    match find name with
    | Some ("note", k) -> note.(k) <- max 0 (min 127 (int_of_float (Float.round v)))
    | Some ("gate", k) -> gate.(k) <- v
    | Some ("tie", k) -> tie.(k) <- v >= 0.5
    | Some ("curve", k) -> curve.(k) <- v
    | _ -> ()
  in
  let get name =
    match find name with
    | Some ("note", k) -> float_of_int note.(k)
    | Some ("gate", k) -> gate.(k)
    | Some ("tie", k) -> if tie.(k) then 1. else 0.
    | Some ("curve", k) -> curve.(k)
    | _ -> 0.
  in
  {
    kind = "matrix";
    role = Sequencer;
    jacks =
      [|
        { label = "Note CV"; dir = Out; signal = Cv }; { label = "Gate CV"; dir = Out; signal = Cv }; { label = "Curve CV"; dir = Out; signal = Cv };
      |];
    stages = [ { reads = []; writes = [ note_out; gate_out; curve_out ]; run } ];
    set;
    get;
    note_on = (fun _ _ -> ());
    note_off = (fun _ -> ());
    run =
      (fun on ->
        running := on;
        if on then pos := 0.);
    tempo = (fun t -> tempo := t);
    step = (fun () -> if !running then Some (fst (position ~tempo:!tempo !pos)) else None);
  }
