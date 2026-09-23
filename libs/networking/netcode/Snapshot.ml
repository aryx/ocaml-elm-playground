(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Snapshot.mli *)

type state = { tick : int; acked : int; latest : string array; world : string }

(* at most this many inputs in a packet (a second's worth) *)
let max_inputs = 60

let read_inputs (players : int) (r : Wire.reader) : int * int * string list =
  if Wire.get_u8 r <> 1 then Wire.fail r "not an inputs packet";
  let player = Wire.get_varint r in
  if player >= players then Wire.fail r "no such player";
  let first = Wire.get_varint r in
  let count = Wire.get_varint r in
  if count > max_inputs then Wire.fail r "too many inputs";
  (player, first, List.init count (fun _ -> Wire.get_string r))

let read_state (players : int) (r : Wire.reader) : state =
  if Wire.get_u8 r <> 2 then Wire.fail r "not a snapshot";
  let tick = Wire.get_varint r in
  let acked = Wire.get_signed r in
  let n = Wire.get_varint r in
  if n <> players then Wire.fail r "not our number of players";
  let latest = Array.init n (fun _ -> Wire.get_string r) in
  let world = Wire.get_string r in
  { tick; acked; latest; world }

(*****************************************************************************)
(* The server *)
(*****************************************************************************)

module Server = struct
  type t = {
    players : int;
    (* by player: every input received, by number *)
    received : (int * int, string) Hashtbl.t;
    (* by player: the number of its last input applied (-1: none), and
     * that input *)
    applied : int array;
    last : string array;
  }

  let create ~(players : int) : t =
    { players; received = Hashtbl.create 256; applied = Array.make players (-1); last = Array.make players "" }

  let receive (t : t) (bytes : string) : unit =
    match Wire.parse (read_inputs t.players) bytes with
    | Error _ -> ()
    | Ok (player, first, inputs) ->
        List.iteri
          (fun i input ->
            let seq = first + i in
            if seq > t.applied.(player) then Hashtbl.replace t.received (player, seq) input)
          inputs

  let inputs (t : t) : string array =
    Array.init t.players (fun p ->
        let next = t.applied.(p) + 1 in
        match Hashtbl.find_opt t.received (p, next) with
        | Some input ->
            Hashtbl.remove t.received (p, next);
            t.applied.(p) <- next;
            t.last.(p) <- input;
            input
        | None -> t.last.(p))

  let packet (t : t) ~(tick : int) ~(world : string) (player : int) : string =
    Wire.to_bytes (fun w ->
        Wire.put_u8 w 2;
        Wire.put_varint w tick;
        Wire.put_signed w t.applied.(player);
        Wire.put_varint w t.players;
        Array.iter (Wire.put_string w) t.last;
        Wire.put_string w world)
end

(*****************************************************************************)
(* A client *)
(*****************************************************************************)

module Client = struct
  type t = {
    me : int;
    players : int;
    mutable next : int; (* the number of my next input *)
    mutable acked : int; (* the server applied mine up to this one *)
    pending : (int, string) Hashtbl.t; (* mine, not yet applied *)
    mutable newest : int; (* the tick of the newest snapshot received *)
  }

  let create ~(me : int) ~(players : int) : t =
    { me; players; next = 0; acked = -1; pending = Hashtbl.create 64; newest = -1 }

  let record (t : t) (input : string) : int =
    let seq = t.next in
    Hashtbl.replace t.pending seq input;
    t.next <- seq + 1;
    seq

  let packet (t : t) : string =
    let first = t.acked + 1 in
    let count = min max_inputs (t.next - first) in
    Wire.to_bytes (fun w ->
        Wire.put_u8 w 1;
        Wire.put_varint w t.me;
        Wire.put_varint w first;
        Wire.put_varint w count;
        for seq = first to first + count - 1 do
          Wire.put_string w (Hashtbl.find t.pending seq)
        done)

  let receive (t : t) (bytes : string) : state option =
    match Wire.parse (read_state t.players) bytes with
    | Error _ -> None
    | Ok s when s.tick <= t.newest -> None
    | Ok s ->
        t.newest <- s.tick;
        (* mine up to [acked] applied: forgotten *)
        for seq = t.acked + 1 to s.acked do
          Hashtbl.remove t.pending seq
        done;
        t.acked <- max t.acked s.acked;
        Some s
end
