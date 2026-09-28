(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_lockstep.mli *)

(*****************************************************************************)
(* A tiny game, and its players *)
(*****************************************************************************)

(* each player a position moved by its input byte, and a running mix
 * of everything that happened: two models agree only if every input
 * was the same *)
type model = { positions : int array; mix : int }

let initial (players : int) : model = { positions = Array.make players 0; mix = 17 }

let update (inputs : string array) (m : model) : model =
  let byte s = if s = "" then 0 else Char.code s.[0] in
  let positions = Array.mapi (fun p x -> x + (byte inputs.(p) mod 3) - 1) m.positions in
  let mix = Array.fold_left (fun h x -> ((h * 31) + x) land 0xffffff) m.mix positions in
  { positions; mix }

(* player [p]'s input for [tick]: a byte from a formula, the same
 * wherever it's asked for (as the keys held at that moment would be) *)
let input (p : int) (tick : int) : string =
  let s = Lehmer.next (Lehmer.scramble ((p * 100_003) + tick)) in
  String.make 1 (Char.chr (int_of_float (256. *. Lehmer.to_unit s)))

(* the game on one machine, every input at hand: the checksum of the
 * model after each tick *)
let alone ~(players : int) ~(delay : int) (ticks : int) : int32 array =
  let sums = Array.make ticks 0l in
  let m = ref (initial players) in
  for tick = 0 to ticks - 1 do
    m := update (Array.init players (fun p -> if tick < delay then "" else input p tick)) !m;
    sums.(tick) <- Checksum.of_model !m
  done;
  sums

(*****************************************************************************)
(* The game on several machines, through Sim_net *)
(*****************************************************************************)

type run = { sums : int32 array array; (* by peer, by tick *) peers : Lockstep.t array; frames : int }

(* every peer, each frame: read its packets, simulate the next tick if
 * it can, send its packet to the others. [cheat] changes one peer's
 * model after one tick, as a nondeterministic update would *)
let together ?(cheat : (int * int) option) ~(players : int) ~(delay : int) ~(seed : int) (config : Sim_net.config)
    (ticks : int) : run =
  let net = Sim_net.create ~seed config in
  let peers = Array.init players (fun me -> Lockstep.create ~me ~players ~delay) in
  let models = Array.init players (fun _ -> initial players) in
  let sums = Array.init players (fun _ -> Array.make ticks 0l) in
  let frame = ref 0 in
  let all_done () = Array.for_all (fun p -> Lockstep.tick p >= ticks) peers in
  while (not (all_done ())) && !frame < 20 * ticks do
    let now = float_of_int !frame /. 60. in
    Array.iteri
      (fun me peer ->
        List.iter (fun (_, bytes) -> Lockstep.receive peer bytes) (Sim_net.receive net ~now me);
        let tick = Lockstep.tick peer in
        (if tick < ticks then
           match Lockstep.step peer (input me (tick + delay)) with
           | None -> ()
           | Some inputs ->
               let m = update inputs models.(me) in
               let m = if cheat = Some (me, tick) then { m with mix = m.mix + 1 } else m in
               models.(me) <- m;
               let sum = Checksum.of_model m in
               sums.(me).(tick) <- sum;
               if tick mod 60 = 0 then Lockstep.checksum peer ~tick sum);
        let packet = Lockstep.packet peer in
        for other = 0 to players - 1 do
          if other <> me then Sim_net.send net ~now ~src:me ~dst:other packet
        done)
      peers;
    incr frame
  done;
  { sums; peers; frames = !frame }

let rough = { Sim_net.latency = 0.030; jitter = 0.040; loss = 0.1; duplication = 0.05 }

let same_as_alone (players : int) (config : Sim_net.config) () =
  let delay = 3 and ticks = 1000 in
  let r = together ~players ~delay ~seed:players config ticks in
  let expected = Array.map Checksum.to_hex (alone ~players ~delay ticks) in
  Array.iteri
    (fun me sums -> Alcotest.(check (array string)) (Printf.sprintf "peer %d, every tick" me) expected (Array.map Checksum.to_hex sums))
    r.sums;
  Array.iter (fun p -> Alcotest.(check (option (pair int int))) "no desync" None (Lockstep.desync p)) r.peers

let stalls (latency : float) : int =
  let r = together ~players:2 ~delay:3 ~seed:1 { Sim_net.perfect with latency } 600 in
  Array.fold_left (fun n p -> n + (Lockstep.stats p).stalls) 0 r.peers

(* 1997's way: no delay, a trip across the network every tick *)
let no_delay_frames (latency : float) : int =
  (together ~players:2 ~delay:0 ~seed:1 { Sim_net.perfect with latency } 100).frames

let tests =
  Testo.categorize "Lockstep"
    [
      Testo.create "two peers, a perfect network: the game alone" (same_as_alone 2 Sim_net.perfect);
      Testo.create "the worked example: two peers, latency, jitter, 10% loss, duplication" (same_as_alone 2 rough);
      Testo.create "the worked example: three peers, the same" (same_as_alone 3 rough);
      Testo.create "the worked example: 30 ms never stalls a delay of 3, 100 ms does" (fun () ->
          Alcotest.(check int) "30 ms" 0 (stalls 0.030);
          let n = stalls 0.100 in
          Alcotest.(check bool) (Printf.sprintf "100 ms: %d stalls" n) true (n > 0));
      Testo.create "the worked example: no delay, 1997's way: a trip every tick" (fun () ->
          let r = together ~players:2 ~delay:0 ~seed:3 rough 300 in
          let expected = Array.map Checksum.to_hex (alone ~players:2 ~delay:0 300) in
          Alcotest.(check (array string)) "the game alone, still" expected (Array.map Checksum.to_hex r.sums.(0));
          let n = no_delay_frames 0.100 in
          Alcotest.(check bool) (Printf.sprintf "100 ticks at 100 ms: %d frames" n) true (n >= 600 && n <= 800));
      Testo.create "the worked example: a disagreement at tick 500, caught at 540" (fun () ->
          let r = together ~cheat:(1, 500) ~players:2 ~delay:3 ~seed:5 rough 700 in
          Alcotest.(check (option (pair int int))) "peer 0 sees peer 1 differ" (Some (540, 1)) (Lockstep.desync r.peers.(0));
          Alcotest.(check (option (pair int int))) "peer 1 sees peer 0 differ" (Some (540, 0)) (Lockstep.desync r.peers.(1)));
      Testo.create "garbage and its own packets dropped" (fun () ->
          let a = Lockstep.create ~me:0 ~players:2 ~delay:3 in
          List.iter (Lockstep.receive a) [ ""; "\x01"; "\x02\x00"; "\x01\x07\x00\x00\x00\x00" ];
          Lockstep.receive a (Lockstep.packet a);
          Alcotest.(check int) "5 dropped" 5 (Lockstep.stats a).dropped;
          Alcotest.(check int) "still at tick 0" 0 (Lockstep.tick a));
    ]
