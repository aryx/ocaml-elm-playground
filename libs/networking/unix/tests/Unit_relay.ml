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

(* the relay's event loop and the clients' frames, a few milliseconds:
 * n rounds at least, then until what is awaited is there (5 seconds at
 * most).
 * claude: awaited, since 50 ms is not enough on every machine (three
 * tests failed on opam's FreeBSD builder, 0.3.1); the n rounds are kept
 * for what must not arrive, which cannot be awaited *)
let pump ?(until : string list list -> bool = fun _ -> true) (relay : Relay.t) (clients : Transport.t list) (n : int) : string list list =
  let got = Array.make (List.length clients) [] in
  let rec go i =
    if i < n || (i < 5000 && not (until (Array.to_list got))) then begin
      Relay.step relay;
      List.iteri (fun i (c : Transport.t) -> got.(i) <- got.(i) @ c.receive ()) clients;
      Unix.sleepf 0.001;
      go (i + 1)
    end
  in
  go 0;
  Array.to_list got

(* each told its number by the relay *)
let seated (clients : Transport.t list) (_ : string list list) : bool = List.for_all (fun (c : Transport.t) -> c.player () <> None) clients

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
          ignore (pump ~until:(seated [ a; b ]) relay [ a; b ] 50);
          let c = Relay_client.connect caps ~host:"127.0.0.1" ~port in
          ignore (pump relay [ a; b; c ] 50);
          Alcotest.(check (list (option int))) "numbers" [ Some 0; Some 1; None ] (List.map (fun (t : Transport.t) -> t.player ()) [ a; b; c ]);
          Alcotest.(check bool) "the third told no" true (String.length (c.status ()) > 0 && c.player () = None);
          Alcotest.(check (list int)) "the relay's seats" [ 0; 1 ] (List.sort compare (Relay.players relay)));
      Testo.create "a packet copied to the others only" (fun () ->
          let relay, port = Relay.listen caps ~bind:"127.0.0.1" ~port:0 ~players:3 in
          let clients = List.init 3 (fun _ -> Relay_client.connect caps ~host:"127.0.0.1" ~port) in
          ignore (pump ~until:(seated clients) relay clients 50);
          (List.hd clients).send "from 0";
          let others = [ []; [ "from 0" ]; [ "from 0" ] ] in
          Alcotest.(check (list (list string))) "the others" others (pump ~until:(( = ) others) relay clients 50));
      Testo.create "Lockstep through the relay: 300 ticks, one game" (fun () ->
          let ticks = 300 and delay = 3 in
          let relay, port = Relay.listen caps ~bind:"127.0.0.1" ~port:0 ~players:2 in
          let transports = Array.init 2 (fun _ -> Relay_client.connect caps ~host:"127.0.0.1" ~port) in
          ignore (pump ~until:(seated (Array.to_list transports)) relay (Array.to_list transports) 50);
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
