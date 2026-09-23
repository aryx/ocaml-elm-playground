(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Inputs.mli *)

type t = {
  me : int;
  players : int;
  (* every input known, by (tick, player) *)
  inputs : (int * int, string) Hashtbl.t;
  (* by player: every input known up to this tick (-1: none yet) *)
  contiguous : int array;
  (* by player: they have all my inputs up to this tick *)
  acked : int array;
  (* my checksums, and theirs, by tick *)
  mine : (int, int32) Hashtbl.t;
  mutable latest : (int * int32) option;
  theirs : (int * int, int32) Hashtbl.t;
  mutable desync : (int * int) option;
  mutable dropped : int;
  mutable inputs_sent : int;
}

(* at most this many inputs in a packet: the ones older wait for the
 * next packets (after a long silence, the other side catches up) *)
let max_inputs = 60

let create ~(me : int) ~(players : int) ~(delay : int) : t =
  let inputs = Hashtbl.create 1024 in
  for tick = 0 to delay - 1 do
    for p = 0 to players - 1 do
      Hashtbl.replace inputs (tick, p) ""
    done
  done;
  {
    me;
    players;
    inputs;
    contiguous = Array.make players (delay - 1);
    acked = Array.make players (delay - 1);
    mine = Hashtbl.create 64;
    latest = None;
    theirs = Hashtbl.create 64;
    desync = None;
    dropped = 0;
    inputs_sent = 0;
  }

let me (t : t) : int = t.me
let players (t : t) : int = t.players
let find (t : t) ~(tick : int) (player : int) : string option = Hashtbl.find_opt t.inputs (tick, player)
let known_upto (t : t) (player : int) : int = t.contiguous.(player)
let desync (t : t) : (int * int) option = t.desync
let dropped (t : t) : int = t.dropped
let inputs_sent (t : t) : int = t.inputs_sent

(* how far [player]'s inputs go without a hole *)
let extend (t : t) (player : int) : unit =
  while Hashtbl.mem t.inputs (t.contiguous.(player) + 1, player) do
    t.contiguous.(player) <- t.contiguous.(player) + 1
  done

let add_mine (t : t) ~(tick : int) (input : string) : unit =
  Hashtbl.replace t.inputs (tick, t.me) input;
  extend t t.me

(*****************************************************************************)
(* Checksums *)
(*****************************************************************************)

(* a disagreement, if both checksums of [tick] are known *)
let compare (t : t) (player : int) (tick : int) : unit =
  match (t.desync, Hashtbl.find_opt t.mine tick, Hashtbl.find_opt t.theirs (player, tick)) with
  | None, Some a, Some b when a <> b -> t.desync <- Some (tick, player)
  | _ -> ()

let checksum (t : t) ~(tick : int) (sum : int32) : unit =
  Hashtbl.replace t.mine tick sum;
  t.latest <- Some (tick, sum);
  for p = 0 to t.players - 1 do
    if p <> t.me then compare t p tick
  done

(*****************************************************************************)
(* Packets *)
(*****************************************************************************)

let others (t : t) : int list = List.filter (( <> ) t.me) (List.init t.players Fun.id)

let packet (t : t) : string =
  (* every input of mine one of them may still miss *)
  let first = 1 + List.fold_left (fun m p -> min m t.acked.(p)) max_int (others t) in
  let last = min t.contiguous.(t.me) (first + max_inputs - 1) in
  let count = max 0 (last - first + 1) in
  t.inputs_sent <- t.inputs_sent + count;
  Wire.to_bytes (fun w ->
      Wire.put_u8 w 1;
      Wire.put_varint w t.me;
      Wire.put_varint w (t.players - 1);
      List.iter
        (fun p ->
          Wire.put_varint w p;
          Wire.put_signed w t.contiguous.(p))
        (others t);
      Wire.put_varint w first;
      Wire.put_varint w count;
      for tick = first to first + count - 1 do
        Wire.put_string w (Hashtbl.find t.inputs (tick, t.me))
      done;
      match t.latest with
      | None -> Wire.put_u8 w 0
      | Some (tick, sum) ->
          Wire.put_u8 w 1;
          Wire.put_varint w tick;
          Wire.put_u16 w (Int32.to_int (Int32.shift_right_logical sum 16));
          Wire.put_u16 w (Int32.to_int (Int32.logand sum 0xffffl)))

type message = { from : int; acks : (int * int) list; first : int; sent : string list; sum : (int * int32) option }

let read (players : int) (r : Wire.reader) : message =
  if Wire.get_u8 r <> 1 then Wire.fail r "not an inputs packet";
  let player r = match Wire.get_varint r with p when p < players -> p | p -> Wire.fail r (Printf.sprintf "no player %d" p) in
  let from = player r in
  let n = Wire.get_varint r in
  if n >= players then Wire.fail r "too many acks";
  let acks =
    List.init n (fun _ ->
        let p = player r in
        (p, Wire.get_signed r))
  in
  let first = Wire.get_varint r in
  let count = Wire.get_varint r in
  if count > max_inputs then Wire.fail r "too many inputs";
  let sent = List.init count (fun _ -> Wire.get_string r) in
  let sum =
    match Wire.get_u8 r with
    | 0 -> None
    | 1 ->
        let tick = Wire.get_varint r in
        let hi = Wire.get_u16 r in
        let lo = Wire.get_u16 r in
        Some (tick, Int32.logor (Int32.shift_left (Int32.of_int hi) 16) (Int32.of_int lo))
    | _ -> Wire.fail r "bad checksum flag"
  in
  { from; acks; first; sent; sum }

let receive (t : t) (bytes : string) : unit =
  match Wire.parse (read t.players) bytes with
  | Error _ -> t.dropped <- t.dropped + 1
  | Ok m when m.from = t.me -> t.dropped <- t.dropped + 1
  | Ok m ->
      List.iteri
        (fun i input -> if not (Hashtbl.mem t.inputs (m.first + i, m.from)) then Hashtbl.replace t.inputs (m.first + i, m.from) input)
        m.sent;
      extend t m.from;
      List.iter (fun (p, ack) -> if p = t.me then t.acked.(m.from) <- max t.acked.(m.from) ack) m.acks;
      Option.iter
        (fun (tick, sum) ->
          Hashtbl.replace t.theirs (m.from, tick) sum;
          compare t m.from tick)
        m.sum
