(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_udp.mli *)

(* what arrives within half a second, polling like frames would *)
let gather (t : Transport.t) (n : int) : string list =
  let rec go acc tries =
    if List.length acc >= n || tries = 0 then acc
    else (
      Unix.sleepf 0.005;
      go (acc @ t.receive ()) (tries - 1))
  in
  go [] 100

(* Unit_lockstep's tiny game: positions moved by input bytes, a mix of
 * everything that happened *)
type model = { positions : int array; mix : int }

let update (inputs : string array) (m : model) : model =
  let byte s = if s = "" then 0 else Char.code s.[0] in
  let positions = Array.mapi (fun p x -> x + (byte inputs.(p) mod 3) - 1) m.positions in
  { positions; mix = Array.fold_left (fun h x -> ((h * 31) + x) land 0xffffff) m.mix positions }

let input (p : int) (tick : int) : string = String.make 1 (Char.chr (((p * 7919) + (tick * 104729)) land 255))

let tests =
  Testo.categorize "Udp"
    [
      Testo.create "the host learns its player from the first datagram" (fun () ->
          let host, port = Udp.host ~bind:"127.0.0.1" ~port:0 in
          let player = Udp.join ~host:"127.0.0.1" ~port in
          host.send "lost: nobody to send to yet";
          player.send "hello";
          Alcotest.(check (list string)) "the host hears" [ "hello" ] (gather host 1);
          host.send "welcome";
          Alcotest.(check (list string)) "the player hears back" [ "welcome" ] (gather player 1);
          Alcotest.(check bool) "the status says so" true (String.length (host.status ()) > 0));
      Testo.create "a third sender, ignored" (fun () ->
          let host, port = Udp.host ~bind:"127.0.0.1" ~port:0 in
          let player = Udp.join ~host:"127.0.0.1" ~port and intruder = Udp.join ~host:"127.0.0.1" ~port in
          player.send "me";
          ignore (gather host 1);
          intruder.send "let me in";
          player.send "me again";
          Alcotest.(check (list string)) "only the player" [ "me again" ] (gather host 2));
      Testo.create "Lockstep over real sockets: 300 ticks, one game" (fun () ->
          let ticks = 300 and delay = 3 in
          let host, port = Udp.host ~bind:"127.0.0.1" ~port:0 in
          let player = Udp.join ~host:"127.0.0.1" ~port in
          let transports = [| host; player |] in
          let peers = Array.init 2 (fun me -> Lockstep.create ~me ~players:2 ~delay) in
          let models = Array.make 2 { positions = [| 0; 0 |]; mix = 17 } in
          let sums = Array.make_matrix 2 ticks 0l in
          let frames = ref 0 in
          while Array.exists (fun p -> Lockstep.tick p < ticks) peers && !frames < 3000 do
            Array.iteri
              (fun me peer ->
                List.iter (Lockstep.receive peer) (transports.(me).receive ());
                let tick = Lockstep.tick peer in
                (if tick < ticks then
                   match Lockstep.step peer (input me (tick + delay)) with
                   | Some inputs ->
                       models.(me) <- update inputs models.(me);
                       sums.(me).(tick) <- Checksum.of_model models.(me)
                   | None -> ());
                transports.(me).send (Lockstep.packet peer))
              peers;
            Unix.sleepf 0.001;
            incr frames
          done;
          (* the same inputs on one machine *)
          let m = ref { positions = [| 0; 0 |]; mix = 17 } in
          let alone =
            Array.init ticks (fun tick ->
                m := update (Array.init 2 (fun p -> if tick < delay then "" else input p tick)) !m;
                Checksum.to_hex (Checksum.of_model !m))
          in
          Alcotest.(check (array string)) "the host's game" alone (Array.map Checksum.to_hex sums.(0));
          Alcotest.(check (array string)) "the player's game" alone (Array.map Checksum.to_hex sums.(1)));
    ]
