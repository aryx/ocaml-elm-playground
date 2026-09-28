(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let chunk = 64

type signal = Audio | Cv
type dir = In | Out
type jack = { label : string; dir : dir; signal : signal }
type role = Hardware | Mixer | Instrument | Effect | Sequencer

type io = {
  audio_in : int -> Signal.stereo;
  cv_in : int -> float option;
  audio_out : int -> Signal.stereo;
  cv_out : int -> float -> unit;
}

type stage = { reads : int list; writes : int list; run : io -> unit }

type t = {
  kind : string;
  role : role;
  jacks : jack array;
  stages : stage list;
  set : string -> float -> unit;
  get : string -> float;
  note_on : int -> float -> unit;
  note_off : int -> unit;
  run : bool -> unit;
  tempo : float -> unit;
  step : unit -> int option;
}

let jack (d : t) (label : string) : int option =
  let rec go i = if i >= Array.length d.jacks then None else if d.jacks.(i).label = label then Some i else go (i + 1) in
  go 0

let of_instrument ~kind ?(cv = []) ?transport (inst : Instrument.t) : t =
  let jacks =
    Array.of_list
      ({ label = "Audio Out"; dir = Out; signal = Audio }
      :: { label = "Seq Note"; dir = In; signal = Cv }
      :: { label = "Seq Gate"; dir = In; signal = Cv }
      :: List.map (fun (label, _, _) -> { label; dir = In; signal = Cv }) cv)
  in
  (* the note the gate plays, and what each modulation input last set *)
  let playing = ref None in
  let last = Array.make (List.length cv) nan in
  let run (io : io) =
    let gate = Option.value (io.cv_in 2) ~default:0. in
    let note = match io.cv_in 1 with Some v -> int_of_float (Float.round (v *. 127.)) | None -> 60 in
    (match (!playing, gate > 0.) with
    | None, true ->
        inst.note_on note gate;
        playing := Some note
    | Some n, true when n <> note ->
        (* legato: the next note on before the last one off *)
        inst.note_on note gate;
        inst.note_off n;
        playing := Some note
    | Some n, false ->
        inst.note_off n;
        playing := None
    | _ -> ());
    List.iteri
      (fun k (_, knob, (from, to_)) ->
        match io.cv_in (3 + k) with
        | Some v when v <> last.(k) ->
            last.(k) <- v;
            inst.set knob (from +. (v *. (to_ -. from)))
        | _ -> ())
      cv;
    inst.fill (io.audio_out 0)
  in
  let run_t, tempo, step = match transport with Some t -> t | None -> ((fun _ -> ()), (fun _ -> ()), fun () -> None) in
  {
    kind;
    role = Instrument;
    jacks;
    stages = [ { reads = List.init (Array.length jacks - 1) (fun j -> j + 1); writes = [ 0 ]; run } ];
    set = inst.set;
    get = (fun _ -> 0.);
    note_on = inst.note_on;
    note_off = inst.note_off;
    run = run_t;
    tempo;
    step;
  }

let of_effect ~kind ~bypass (fx : Effect.t) : t =
  let run (io : io) =
    let src = io.audio_in 0 and dst = io.audio_out 1 in
    Array.blit src.left 0 dst.left 0 (Array.length dst.left);
    Array.blit src.right 0 dst.right 0 (Array.length dst.right);
    if not (bypass ()) then fx.process dst
  in
  {
    kind;
    role = Effect;
    jacks = [| { label = "Audio In"; dir = In; signal = Audio }; { label = "Audio Out"; dir = Out; signal = Audio } |];
    stages = [ { reads = [ 0 ]; writes = [ 1 ]; run } ];
    set = fx.set;
    get = (fun _ -> 0.);
    note_on = (fun _ _ -> ());
    note_off = (fun _ -> ());
    run = (fun _ -> ());
    tempo = (fun _ -> ());
    step = (fun () -> None);
  }
