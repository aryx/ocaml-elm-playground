(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sim_net.mli *)

type config = { latency : float; jitter : float; loss : float; duplication : float }

let perfect = { latency = 0.; jitter = 0.; loss = 0.; duplication = 0. }

type packet = {
  src : int;
  dst : int;
  bytes : string;
  arrival : float;
  (* the order it was sent in, between its two peers: to count the
   * packets overtaken *)
  number : int;
}

type stats = { sent : int; lost : int; duplicated : int; delivered : int; reordered : int }

type t = {
  mutable config : config;
  mutable seed : Lehmer.t;
  (* in flight, sorted by arrival (ties: in the order sent) *)
  mutable in_flight : packet list;
  (* per (src, dst): packets sent so far, and the highest number
   * delivered *)
  sent_between : (int * int, int) Hashtbl.t;
  last_delivered : (int * int, int) Hashtbl.t;
  mutable stats : stats;
}

let create ~(seed : int) (config : config) : t =
  {
    config;
    seed = Lehmer.scramble seed;
    in_flight = [];
    sent_between = Hashtbl.create 8;
    last_delivered = Hashtbl.create 8;
    stats = { sent = 0; lost = 0; duplicated = 0; delivered = 0; reordered = 0 };
  }

let config (net : t) : config = net.config
let set_config (net : t) (config : config) : unit = net.config <- config

(* a number in [0, 1), from the network's own seed *)
let draw (net : t) : float =
  net.seed <- Lehmer.next net.seed;
  Lehmer.to_unit net.seed

(* after the packets arriving at the same time or before: a stable
 * insertion keeps the sending order among equals *)
let rec insert (p : packet) (ps : packet list) : packet list =
  match ps with
  | q :: rest when q.arrival <= p.arrival -> q :: insert p rest
  | _ -> p :: ps

let send (net : t) ~(now : float) ~(src : int) ~(dst : int) (bytes : string) : unit =
  let c = net.config in
  let number = 1 + Option.value (Hashtbl.find_opt net.sent_between (src, dst)) ~default:0 in
  Hashtbl.replace net.sent_between (src, dst) number;
  net.stats <- { net.stats with sent = net.stats.sent + 1 };
  (* the draws, always in the same order, so that a seed is a network *)
  let lost = draw net < c.loss in
  let delay () = c.latency +. (c.jitter *. draw net) in
  let first = delay () in
  let twice = draw net < c.duplication in
  let second = delay () in
  if lost then net.stats <- { net.stats with lost = net.stats.lost + 1 }
  else begin
    let put arrival = net.in_flight <- insert { src; dst; bytes; arrival; number } net.in_flight in
    put (now +. first);
    if twice then begin
      put (now +. second);
      net.stats <- { net.stats with duplicated = net.stats.duplicated + 1 }
    end
  end

let receive (net : t) ~(now : float) (dst : int) : (int * string) list =
  let arrived, rest = List.partition (fun p -> p.dst = dst && p.arrival <= now) net.in_flight in
  net.in_flight <- rest;
  List.iter
    (fun p ->
      let last = Option.value (Hashtbl.find_opt net.last_delivered (p.src, p.dst)) ~default:0 in
      let reordered = if p.number < last then 1 else 0 in
      Hashtbl.replace net.last_delivered (p.src, p.dst) (max last p.number);
      net.stats <-
        { net.stats with delivered = net.stats.delivered + 1; reordered = net.stats.reordered + reordered })
    arrived;
  List.map (fun p -> (p.src, p.bytes)) arrived

let stats (net : t) : stats = net.stats
