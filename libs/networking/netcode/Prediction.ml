(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Prediction.mli *)

type 'model t = {
  me : int;
  update : string array -> 'model -> 'model;
  mutable model : 'model;
  (* the others' inputs, guessed: their latest *)
  mutable latest : string array;
  (* mine not yet confirmed: their number, the input, and the model I
   * predicted after it (to compare with the server's) *)
  mutable pending : (int * string * 'model) list;
  mutable mispredictions : int;
}

let create ~(me : int) ~(players : int) ~update (model : 'model) : 'model t =
  { me; update; model; latest = Array.make players ""; pending = []; mispredictions = 0 }

(* one tick: my input, the others' latest *)
let play (t : 'model t) (input : string) (model : 'model) : 'model =
  t.update (Array.mapi (fun p latest -> if p = t.me then input else latest) t.latest) model

let step (t : 'model t) ~(seq : int) (input : string) : unit =
  t.model <- play t input t.model;
  t.pending <- t.pending @ [ (seq, input, t.model) ]

let correct (t : 'model t) ~(world : 'model) ~(acked : int) ~(latest : string array) : unit =
  (* what I had predicted after [acked], against the server's *)
  (match List.find_opt (fun (seq, _, _) -> seq = acked) t.pending with
  | Some (_, _, predicted) when Checksum.of_model predicted <> Checksum.of_model world ->
      t.mispredictions <- t.mispredictions + 1
  | _ -> ());
  t.latest <- latest;
  t.pending <- List.filter (fun (seq, _, _) -> seq > acked) t.pending;
  (* the server's world, mine not yet applied played again on it *)
  let model, pending =
    List.fold_left
      (fun (model, acc) (seq, input, _) ->
        let model = play t input model in
        (model, (seq, input, model) :: acc))
      (world, []) t.pending
  in
  t.model <- model;
  t.pending <- List.rev pending

let model (t : 'model t) : 'model = t.model
let mispredictions (t : 'model t) : int = t.mispredictions
