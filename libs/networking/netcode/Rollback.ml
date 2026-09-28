(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Rollback.mli *)

type stats = { stalls : int; rollbacks : int; replayed : int; deepest : int; dropped : int }

(* a tick played but not confirmed: the model before it, and the
 * inputs it was played with *)
type 'model played = { at : int; before : 'model; used : string array }

type 'model t = {
  inputs : Inputs.t;
  delay : int;
  max_ahead : int;
  update : string array -> 'model -> 'model;
  on_confirm : int -> 'model -> unit;
  mutable model : 'model;
  (* the ticks from [confirmed] to [tick] - 1, oldest first *)
  mutable saved : 'model played list;
  mutable tick : int;
  mutable confirmed : int;
  mutable stats : stats;
}

let create ~(me : int) ~(players : int) ?(delay = 0) ?(max_ahead = 8) ~update ~on_confirm (model : 'model) : 'model t =
  {
    inputs = Inputs.create ~me ~players ~delay;
    delay;
    max_ahead;
    update;
    on_confirm;
    model;
    saved = [];
    tick = 0;
    confirmed = 0;
    stats = { stalls = 0; rollbacks = 0; replayed = 0; deepest = 0; dropped = 0 };
  }

(*****************************************************************************)
(* Guessing *)
(*****************************************************************************)

(* [player]'s input for [tick]: the real one if known, else the guess --
 * the last one known before it (no key held if none) *)
let best (t : 'model t) (tick : int) (player : int) : string =
  match Inputs.find t.inputs ~tick player with
  | Some input -> input
  | None -> (
      let last = min (Inputs.known_upto t.inputs player) (tick - 1) in
      match if last >= 0 then Inputs.find t.inputs ~tick:last player else None with Some input -> input | None -> "")

let inputs_for (t : 'model t) (tick : int) : string array = Array.init (Inputs.players t.inputs) (best t tick)

(* play [tick] from [model], saved for a later rollback *)
let play (t : 'model t) (tick : int) (model : 'model) : 'model played * 'model =
  let used = inputs_for t tick in
  ({ at = tick; before = model; used }, t.update used model)

(*****************************************************************************)
(* Fixing *)
(*****************************************************************************)

(* a saved tick one of whose guesses a real input now contradicts *)
let wrong (t : 'model t) (p : 'model played) : bool =
  let wrong_for player guess =
    match Inputs.find t.inputs ~tick:p.at player with Some real -> real <> guess | None -> false
  in
  Array.exists Fun.id (Array.mapi wrong_for p.used)

(* back to the earliest wrong tick, and every tick since played again *)
let roll_back (t : 'model t) : unit =
  let rec split kept = function
    | [] -> None
    | p :: rest -> if wrong t p then Some (List.rev kept, p) else split (p :: kept) rest
  in
  match split [] t.saved with
  | None -> ()
  | Some (kept, first) ->
      let rec replay tick model acc =
        if tick = t.tick then (List.rev acc, model)
        else
          let p, model' = play t tick model in
          replay (tick + 1) model' (p :: acc)
      in
      let redone, model = replay first.at first.before [] in
      let n = List.length redone in
      t.saved <- kept @ redone;
      t.model <- model;
      t.stats <-
        { t.stats with rollbacks = t.stats.rollbacks + 1; replayed = t.stats.replayed + n; deepest = max t.stats.deepest n }

(* the oldest ticks whose inputs are all real: final, handed over, and
 * no longer needed for a rollback *)
let confirm (t : 'model t) : unit =
  let all_real tick = List.for_all (fun p -> Inputs.known_upto t.inputs p >= tick) (List.init (Inputs.players t.inputs) Fun.id) in
  let rec go () =
    match t.saved with
    | p :: rest when all_real p.at ->
        (* the model after p: the next one's before, or the latest *)
        let after = match rest with q :: _ -> q.before | [] -> t.model in
        t.on_confirm p.at after;
        t.saved <- rest;
        t.confirmed <- p.at + 1;
        go ()
    | _ -> ()
  in
  go ()

let settle (t : 'model t) : unit =
  roll_back t;
  confirm t

let step (t : 'model t) (input : string) : unit =
  settle t;
  if t.tick - t.confirmed >= t.max_ahead then t.stats <- { t.stats with stalls = t.stats.stalls + 1 }
  else begin
    Inputs.add_mine t.inputs ~tick:(t.tick + t.delay) input;
    let p, model = play t t.tick t.model in
    t.saved <- t.saved @ [ p ];
    t.model <- model;
    t.tick <- t.tick + 1;
    confirm t
  end

let model (t : 'model t) : 'model = t.model
let tick (t : 'model t) : int = t.tick
let confirmed (t : 'model t) : int = t.confirmed
let packet (t : 'model t) : string = Inputs.packet t.inputs
let receive (t : 'model t) (bytes : string) : unit = Inputs.receive t.inputs bytes
let checksum (t : 'model t) ~(tick : int) (sum : int32) : unit = Inputs.checksum t.inputs ~tick sum
let desync (t : 'model t) : (int * int) option = Inputs.desync t.inputs
let stats (t : 'model t) : stats = { t.stats with dropped = Inputs.dropped t.inputs }
