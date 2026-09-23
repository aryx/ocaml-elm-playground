(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_client_server.mli *)

(* a tiny game: each player a position moved by its input byte ("" does
 * not move) *)
type model = { positions : int array }

let update (inputs : string array) (m : model) : model =
  { positions = Array.mapi (fun p x -> if inputs.(p) = "" then x else x + (Char.code inputs.(p).[0] mod 3) - 1) m.positions }

(* keys held: an input changing every 20 ticks, "" at the end *)
let input (p : int) (tick : int) (last : int) : string =
  if tick >= last - 10 then ""
  else String.make 1 (Char.chr (int_of_float (256. *. Lehmer.to_unit (Lehmer.next (Lehmer.scramble ((p * 7919) + (tick / 20)))))))

type run = { server_world : model; clients : model Prediction.t array; acked : int array; last_seq : int array }

(* a server (Sim_net's peer [players]) and [players] clients, [ticks]
 * frames of inputs, then a second of settling *)
let run ~(players : int) ~(seed : int) (config : Sim_net.config) (ticks : int) : run =
  let net = Sim_net.create ~seed config in
  let server = Snapshot.Server.create ~players in
  let world = ref { positions = Array.make players 0 } in
  let clients = Array.init players (fun me -> Snapshot.Client.create ~me ~players) in
  let predictions = Array.init players (fun me -> Prediction.create ~me ~players ~update !world) in
  let acked = Array.make players (-1) and last_seq = Array.make players (-1) in
  for frame = 0 to ticks + 60 do
    let now = float_of_int frame /. 60. in
    (* the server: inputs in, a tick, the world out every 3 ticks *)
    List.iter (fun (_, bytes) -> Snapshot.Server.receive server bytes) (Sim_net.receive net ~now players);
    world := update (Snapshot.Server.inputs server) !world;
    if frame mod 3 = 0 then
      for p = 0 to players - 1 do
        Sim_net.send net ~now ~src:players ~dst:p
          (Snapshot.Server.packet server ~tick:frame ~world:(Marshal.to_string !world []) p)
      done;
    (* the clients: the world in, an input (while there are some), out *)
    Array.iteri
      (fun me c ->
        List.iter
          (fun (_, bytes) ->
            match Snapshot.Client.receive c bytes with
            | Some s ->
                acked.(me) <- s.acked;
                Prediction.correct predictions.(me) ~world:(Marshal.from_string s.world 0) ~acked:s.acked ~latest:s.latest
            | None -> ())
          (Sim_net.receive net ~now me);
        if frame < ticks then begin
          let i = input me frame ticks in
          let seq = Snapshot.Client.record c i in
          last_seq.(me) <- seq;
          Prediction.step predictions.(me) ~seq i
        end;
        Sim_net.send net ~now ~src:me ~dst:players (Snapshot.Client.packet c))
      clients
  done;
  { server_world = !world; clients = predictions; acked; last_seq }

let rough = { Sim_net.latency = 0.050; jitter = 0.020; loss = 0.1; duplication = 0. }

let tests =
  Testo.categorize "Client-server"
    [
      Testo.create "the server's queue: in order, the last repeated when late" (fun () ->
          let server = Snapshot.Server.create ~players:1 and client = Snapshot.Client.create ~me:0 ~players:1 in
          List.iter (fun i -> ignore (Snapshot.Client.record client i)) [ "a"; "b"; "c" ];
          Snapshot.Server.receive server (Snapshot.Client.packet client);
          Snapshot.Server.receive server (Snapshot.Client.packet client);
          let next () = (Snapshot.Server.inputs server).(0) in
          let got = List.init 5 (fun _ -> next ()) in
          Alcotest.(check (list string)) "a b c, then c again" [ "a"; "b"; "c"; "c"; "c" ] got);
      Testo.create "the worked example: every input applied, once, in order" (fun () ->
          let r = run ~players:2 ~seed:1 rough 600 in
          Alcotest.(check (array int)) "all acknowledged" r.last_seq r.acked);
      Testo.create "alone: never mispredicted" (fun () ->
          let r = run ~players:1 ~seed:2 { rough with loss = 0. } 600 in
          Alcotest.(check int) "mispredictions" 0 (Prediction.mispredictions r.clients.(0));
          Alcotest.(check (array int)) "the server's world" r.server_world.positions (Prediction.model r.clients.(0)).positions);
      Testo.create "two players: mispredicted, corrected, the worlds agreeing" (fun () ->
          let r = run ~players:2 ~seed:3 rough 600 in
          let wrong = Array.map Prediction.mispredictions r.clients in
          Alcotest.(check bool) (Printf.sprintf "%d and %d mispredictions" wrong.(0) wrong.(1)) true (wrong.(0) > 0 && wrong.(1) > 0);
          Array.iteri
            (fun me p -> Alcotest.(check (array int)) (Printf.sprintf "client %d" me) r.server_world.positions (Prediction.model p).positions)
            r.clients);
      Testo.create "the worked example: interpolation, 100 ms behind" (fun () ->
          let buf = Interpolation.create ~delay:0.100 in
          Alcotest.(check bool) "nothing yet" true (Interpolation.sample buf ~now:0. = None);
          List.iter (fun (t, x) -> Interpolation.add buf ~time:t x) [ (0., 0.); (0.050, 10.); (0.100, 20.) ];
          let at now = match Interpolation.sample buf ~now with Some (a, b, f) -> a +. ((b -. a) *. f) | None -> nan in
          Alcotest.(check (float 1e-9)) "125 ms: halfway 0 -> 50" 5. (at 0.125);
          Alcotest.(check (float 1e-9)) "before: the first" 0. (at 0.050);
          Alcotest.(check (float 1e-9)) "past the newest: the newest" 20. (at 1.0));
    ]
