(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_rollback.mli *)

(* Unit_lockstep's tiny game, with its tick in the model (for a
 * disagreement injected at a tick, replays included) *)
type model = { tick : int; positions : int array; mix : int }

let initial (players : int) : model = { tick = 0; positions = Array.make players 0; mix = 17 }

let update (inputs : string array) (m : model) : model =
  let byte s = if s = "" then 0 else Char.code s.[0] in
  let positions = Array.mapi (fun p x -> x + (byte inputs.(p) mod 3) - 1) m.positions in
  { tick = m.tick + 1; positions; mix = Array.fold_left (fun h x -> ((h * 31) + x) land 0xffffff) m.mix positions }

(* keys held: a player's input changes every 20 ticks (a byte from a
 * formula), as fingers do -- what rollback's guess bets on *)
let input (p : int) (tick : int) : string =
  let s = Lehmer.next (Lehmer.scramble ((p * 100_003) + (tick / 20))) in
  String.make 1 (Char.chr (int_of_float (256. *. Lehmer.to_unit s)))

let alone ~(players : int) (ticks : int) : string array =
  let m = ref (initial players) in
  Array.init ticks (fun tick ->
      m := update (Array.init players (fun p -> input p tick)) !m;
      Checksum.to_hex (Checksum.of_model !m))

type run = { sums : string array array; peers : model Rollback.t array; frames : int }

(* every peer, each frame: read its packets, play (or settle, its
 * ticks done), send; [cheat] alters one peer's update at one tick *)
let together ?(cheat : (int * int) option) ~(players : int) ~(seed : int) (config : Sim_net.config) (ticks : int) : run =
  let net = Sim_net.create ~seed config in
  let sums = Array.init players (fun _ -> Array.make ticks "") in
  let peers =
    Array.init players (fun me ->
        let update inputs m =
          let m' = update inputs m in
          if cheat = Some (me, m.tick) then { m' with mix = m'.mix + 1 } else m'
        in
        let rec peer =
          lazy
            (Rollback.create ~me ~players ~update
               ~on_confirm:(fun tick m ->
                 if tick < ticks then sums.(me).(tick) <- Checksum.to_hex (Checksum.of_model m);
                 if tick mod 60 = 0 then Rollback.checksum (Lazy.force peer) ~tick (Checksum.of_model m))
               (initial players))
        in
        Lazy.force peer)
  in
  let frame = ref 0 in
  let all_confirmed () = Array.for_all (fun p -> Rollback.confirmed p >= ticks) peers in
  while (not (all_confirmed ())) && !frame < 20 * ticks do
    let now = float_of_int !frame /. 60. in
    Array.iteri
      (fun me peer ->
        List.iter (fun (_, bytes) -> Rollback.receive peer bytes) (Sim_net.receive net ~now me);
        let tick = Rollback.tick peer in
        if tick < ticks then Rollback.step peer (input me tick) else Rollback.settle peer;
        let packet = Rollback.packet peer in
        for other = 0 to players - 1 do
          if other <> me then Sim_net.send net ~now ~src:me ~dst:other packet
        done)
      peers;
    incr frame
  done;
  { sums; peers; frames = !frame }

let rough = { Sim_net.latency = 0.030; jitter = 0.040; loss = 0.1; duplication = 0.05 }

let same_as_alone (players : int) (config : Sim_net.config) () =
  let ticks = 1000 in
  let r = together ~players ~seed:players config ticks in
  let expected = alone ~players ticks in
  Array.iteri (fun me sums -> Alcotest.(check (array string)) (Printf.sprintf "peer %d, every confirmed tick" me) expected sums) r.sums;
  Array.iter (fun p -> Alcotest.(check (option (pair int int))) "no desync" None (Rollback.desync p)) r.peers;
  Alcotest.(check bool) "some guesses were wrong, and fixed" true
    (Array.exists (fun p -> (Rollback.stats p).rollbacks > 0) r.peers)

let tests =
  Testo.categorize "Rollback"
    [
      Testo.create "two peers, a perfect network: the game alone" (same_as_alone 2 Sim_net.perfect);
      Testo.create "the worked example: two peers, latency, jitter, 10% loss, duplication" (same_as_alone 2 rough);
      Testo.create "the worked example: three peers, the same" (same_as_alone 3 rough);
      Testo.create "the worked example: 100 ms, full speed, paid in replays" (fun () ->
          let r = together ~players:2 ~seed:1 { Sim_net.perfect with latency = 0.100 } 600 in
          let s = Rollback.stats r.peers.(0) in
          Alcotest.(check bool) (Printf.sprintf "600 ticks in %d frames" r.frames) true (r.frames < 640);
          Alcotest.(check int) "no stall" 0 s.stalls;
          Alcotest.(check bool)
            (Printf.sprintf "%d rollbacks, %d ticks replayed, %d at most" s.rollbacks s.replayed s.deepest)
            true (s.rollbacks > 0 && s.deepest <= 8));
      Testo.create "a disagreement at tick 500, caught at 540" (fun () ->
          let r = together ~cheat:(1, 500) ~players:2 ~seed:5 rough 700 in
          Alcotest.(check (option (pair int int))) "peer 0 sees peer 1 differ" (Some (540, 1)) (Rollback.desync r.peers.(0)));
    ]
