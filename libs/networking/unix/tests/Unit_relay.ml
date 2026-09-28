(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_relay.mli *)

(* the relay's event loop and the clients' frames, a few milliseconds *)
let pump (relay : Relay.t) (clients : Transport.t list) (n : int) : string list list =
  let got = Array.make (List.length clients) [] in
  for _ = 1 to n do
    Relay.step relay;
    List.iteri (fun i (c : Transport.t) -> got.(i) <- got.(i) @ c.receive ()) clients;
    Unix.sleepf 0.001
  done;
  Array.to_list got

(* Unit_lockstep's tiny game *)
type model = { positions : int array; mix : int }

let update (inputs : string array) (m : model) : model =
  let byte s = if s = "" then 0 else Char.code s.[0] in
  let positions = Array.mapi (fun p x -> x + (byte inputs.(p) mod 3) - 1) m.positions in
  { positions; mix = Array.fold_left (fun h x -> ((h * 31) + x) land 0xffffff) m.mix positions }

let input (p : int) (tick : int) : string = String.make 1 (Char.chr (((p * 7919) + (tick * 104729)) land 255))

let tests (caps : < Cap.network ; .. >) =
  Testo.categorize "Relay"
    [
      Testo.create "the seats: 0, 1, and a third refused" (fun () ->
          let relay, port = Relay.listen caps ~bind:"127.0.0.1" ~port:0 ~players:2 in
          let a = Relay_client.connect caps ~host:"127.0.0.1" ~port in
          let b = Relay_client.connect caps ~host:"127.0.0.1" ~port in
          ignore (pump relay [ a; b ] 50);
          let c = Relay_client.connect caps ~host:"127.0.0.1" ~port in
          ignore (pump relay [ a; b; c ] 50);
          Alcotest.(check (list (option int))) "numbers" [ Some 0; Some 1; None ] (List.map (fun (t : Transport.t) -> t.player ()) [ a; b; c ]);
          Alcotest.(check bool) "the third told no" true (String.length (c.status ()) > 0 && c.player () = None);
          Alcotest.(check (list int)) "the relay's seats" [ 0; 1 ] (List.sort compare (Relay.players relay)));
      Testo.create "a packet copied to the others only" (fun () ->
          let relay, port = Relay.listen caps ~bind:"127.0.0.1" ~port:0 ~players:3 in
          let clients = List.init 3 (fun _ -> Relay_client.connect caps ~host:"127.0.0.1" ~port) in
          ignore (pump relay clients 50);
          (List.hd clients).send "from 0";
          Alcotest.(check (list (list string))) "the others" [ []; [ "from 0" ]; [ "from 0" ] ] (pump relay clients 50));
      Testo.create "Lockstep through the relay: 300 ticks, one game" (fun () ->
          let ticks = 300 and delay = 3 in
          let relay, port = Relay.listen caps ~bind:"127.0.0.1" ~port:0 ~players:2 in
          let transports = Array.init 2 (fun _ -> Relay_client.connect caps ~host:"127.0.0.1" ~port) in
          ignore (pump relay (Array.to_list transports) 50);
          (* each its number from the relay *)
          let me i = Option.get ((transports.(i) : Transport.t).player ()) in
          let peers = Array.init 2 (fun i -> Lockstep.create ~me:(me i) ~players:2 ~delay) in
          let models = Array.make 2 { positions = [| 0; 0 |]; mix = 17 } in
          let sums = Array.make_matrix 2 ticks "" in
          let frames = ref 0 in
          while Array.exists (fun p -> Lockstep.tick p < ticks) peers && !frames < 5000 do
            Relay.step relay;
            Array.iteri
              (fun i peer ->
                let t : Transport.t = transports.(i) in
                List.iter (Lockstep.receive peer) (t.receive ());
                let tick = Lockstep.tick peer in
                (if tick < ticks then
                   match Lockstep.step peer (input (me i) (tick + delay)) with
                   | Some inputs ->
                       models.(i) <- update inputs models.(i);
                       sums.(i).(tick) <- Checksum.to_hex (Checksum.of_model models.(i))
                   | None -> ());
                t.send (Lockstep.packet peer))
              peers;
            Unix.sleepf 0.001;
            incr frames
          done;
          let m = ref { positions = [| 0; 0 |]; mix = 17 } in
          let alone =
            Array.init ticks (fun tick ->
                m := update (Array.init 2 (fun p -> if tick < delay then "" else input p tick)) !m;
                Checksum.to_hex (Checksum.of_model !m))
          in
          Alcotest.(check (array string)) "player 0's game" alone sums.(0);
          Alcotest.(check (array string)) "player 1's game" alone sums.(1);
          Alcotest.(check bool) "copied by the relay" true (Relay.forwarded relay > 0));
    ]
