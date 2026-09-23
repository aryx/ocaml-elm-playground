(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Rack.mli *)

type stage = { effect : Effect.t; mutable on : bool }
type t = { mutable stages : stage list }

let create (effects : Effect.t list) : t = { stages = List.map (fun effect -> { effect; on = false }) effects }
let standard () : t = create [ Drive.effect (); Eq.effect (); Delay.effect (); Reverb.effect () ]

(* an effect's knobs under its name, its switch first *)
let prefixed (name : string) (knobs : Effect.knob list) : Effect.knob list =
  { name = name ^ ".on"; control = Switch; initial = 0. }
  :: List.map (fun (k : Effect.knob) -> { k with name = name ^ "." ^ k.name }) knobs

let standard_knobs : Effect.knob list =
  List.concat_map (fun (name, knobs) -> prefixed name knobs) [ ("drive", Drive.knobs); ("eq", Eq.knobs); ("delay", Delay.knobs); ("reverb", Reverb.knobs) ]

let knobs (t : t) : Effect.knob list = List.concat_map (fun s -> prefixed s.effect.name s.effect.knobs) t.stages

let set (t : t) (name : string) (x : float) : unit =
  match String.index_opt name '.' with
  | None -> ()
  | Some i ->
      let effect = String.sub name 0 i and knob = String.sub name (i + 1) (String.length name - i - 1) in
      List.iter
        (fun s -> if s.effect.name = effect then if knob = "on" then s.on <- Control.on x else s.effect.set knob x)
        t.stages

let order (t : t) : string list = List.map (fun s -> s.effect.name) t.stages

let reorder (t : t) (names : string list) : unit =
  let named = List.filter_map (fun n -> List.find_opt (fun s -> s.effect.name = n) t.stages) names in
  t.stages <- named @ List.filter (fun s -> not (List.memq s named)) t.stages

let process (t : t) (out : Signal.stereo) : unit = List.iter (fun s -> if s.on then s.effect.process out) t.stages
