(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_sim_net.mli *)

let frame = 1. /. 60.

(* the receiver looks 34 times a frame (every 0.5 ms): a delay is
 * measured within half a millisecond *)
let polls = 34

(* [n] packets from peer 0 to peer 1, one a frame: the (send time,
 * arrival time, bytes) of each delivered *)
let run ?(seed = 1) (config : Sim_net.config) (n : int) : Sim_net.t * (float * float * string) list =
  let net = Sim_net.create ~seed config in
  let arrivals = ref [] in
  let sent_at = Hashtbl.create n in
  let deliver now = List.iter (fun (_, b) -> arrivals := (Hashtbl.find sent_at b, now, b) :: !arrivals) (Sim_net.receive net ~now 1) in
  for i = 0 to n - 1 do
    let now = float_of_int i *. frame in
    let b = string_of_int i in
    Hashtbl.replace sent_at b now;
    Sim_net.send net ~now ~src:0 ~dst:1 b;
    for k = 0 to polls - 1 do deliver (now +. (float_of_int k *. frame /. float_of_int polls)) done
  done;
  (* then long enough for the last ones to land *)
  for j = 0 to 120 * polls do deliver ((float_of_int n *. frame) +. (float_of_int j *. frame /. float_of_int polls)) done;
  (net, List.rev !arrivals)

let tests =
  Testo.categorize "Sim_net"
    [
      Testo.create "perfect: everything, at once, in order" (fun () ->
          let net, arrivals = run Sim_net.perfect 100 in
          Alcotest.(check int) "delivered" 100 (Sim_net.stats net).delivered;
          Alcotest.(check (list string)) "in order" (List.init 100 string_of_int) (List.map (fun (_, _, b) -> b) arrivals);
          Alcotest.(check bool) "no delay" true (List.for_all (fun (s, r, _) -> r = s) arrivals));
      Testo.create "the worked example: 10% loss, 1,000 lost give or take 90" (fun () ->
          let net, _ = run { Sim_net.perfect with loss = 0.1 } 10_000 in
          let s = Sim_net.stats net in
          Alcotest.(check bool) (Printf.sprintf "%d lost" s.lost) true (abs (s.lost - 1000) <= 90);
          Alcotest.(check int) "the others delivered" (10_000 - s.lost) s.delivered);
      Testo.create "the worked example: 30 ms + up to 20 ms, 40 ms on average" (fun () ->
          let net, arrivals = run { Sim_net.perfect with latency = 0.030; jitter = 0.020 } 2000 in
          let delays = List.map (fun (s, r, _) -> r -. s) arrivals in
          let mean = List.fold_left ( +. ) 0. delays /. float_of_int (List.length delays) in
          Alcotest.(check int) "all delivered" 2000 (Sim_net.stats net).delivered;
          Alcotest.(check bool) "none before 30 ms" true (List.for_all (fun d -> d >= 0.030 -. 1e-9) delays);
          Alcotest.(check bool) "none after 50 ms (+ a poll)" true (List.for_all (fun d -> d < 0.0505) delays);
          (* the polls round up by a quarter of a millisecond on average *)
          Alcotest.(check bool) (Printf.sprintf "mean %.2f ms" (mean *. 1000.)) true (Float.abs (mean -. 0.04025) < 0.001));
      Testo.create "reordering: none without jitter, some with it" (fun () ->
          let calm, _ = run { Sim_net.perfect with latency = 0.1 } 1000 in
          let rough, _ = run { Sim_net.perfect with latency = 0.1; jitter = 0.05 } 1000 in
          Alcotest.(check int) "no jitter" 0 (Sim_net.stats calm).reordered;
          Alcotest.(check bool) (Printf.sprintf "50 ms of jitter: %d" (Sim_net.stats rough).reordered) true
            ((Sim_net.stats rough).reordered > 50));
      Testo.create "duplication: 5%, the copies delivered too" (fun () ->
          let net, arrivals = run { Sim_net.perfect with duplication = 0.05 } 10_000 in
          let s = Sim_net.stats net in
          Alcotest.(check bool) (Printf.sprintf "%d duplicated" s.duplicated) true (abs (s.duplicated - 500) <= 66);
          Alcotest.(check int) "delivered" (10_000 + s.duplicated) (List.length arrivals));
      Testo.create "a seed is a network" (fun () ->
          let config = { Sim_net.latency = 0.03; jitter = 0.04; loss = 0.1; duplication = 0.05 } in
          let _, a = run ~seed:7 config 1000 and _, b = run ~seed:7 config 1000 and _, c = run ~seed:8 config 1000 in
          let order xs = List.map (fun (_, _, b) -> b) xs in
          Alcotest.(check (list string)) "same seed, same packets" (order a) (order b);
          Alcotest.(check bool) "another seed, another network" true (order a <> order c));
    ]
