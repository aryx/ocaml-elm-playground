(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Lockstep.mli *)

type stats = { stalls : int; dropped : int; inputs_sent : int }

type t = {
  inputs : Inputs.t;
  delay : int;
  (* the next tick to simulate *)
  mutable tick : int;
  mutable stalls : int;
}

let create ~(me : int) ~(players : int) ~(delay : int) : t =
  { inputs = Inputs.create ~me ~players ~delay; delay; tick = 0; stalls = 0 }

let tick (t : t) : int = t.tick

(* wait for every player's input of the tick: lockstep's one rule *)
let step (t : t) (input : string) : string array option =
  let players = Inputs.players t.inputs in
  (* no delay (1997's way): my input of this very tick is sent first,
   * then the tick waits for everybody else's; kept as first read, since
   * it may be sent already when the tick stalls *)
  if t.delay = 0 && Inputs.find t.inputs ~tick:t.tick (Inputs.me t.inputs) = None then
    Inputs.add_mine t.inputs ~tick:t.tick input;
  if List.for_all (fun p -> Inputs.known_upto t.inputs p >= t.tick) (List.init players Fun.id) then begin
    let inputs = Array.init players (fun p -> Option.get (Inputs.find t.inputs ~tick:t.tick p)) in
    if t.delay > 0 then Inputs.add_mine t.inputs ~tick:(t.tick + t.delay) input;
    t.tick <- t.tick + 1;
    Some inputs
  end
  else begin
    t.stalls <- t.stalls + 1;
    None
  end

let packet (t : t) : string = Inputs.packet t.inputs
let receive (t : t) (bytes : string) : unit = Inputs.receive t.inputs bytes
let checksum (t : t) ~(tick : int) (sum : int32) : unit = Inputs.checksum t.inputs ~tick sum
let desync (t : t) : (int * int) option = Inputs.desync t.inputs

let stats (t : t) : stats =
  { stalls = t.stalls; dropped = Inputs.dropped t.inputs; inputs_sent = Inputs.inputs_sent t.inputs }
