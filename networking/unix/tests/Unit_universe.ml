(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_universe.mli *)

(* the universe's event loop and the worlds' frames, a few milliseconds *)
let pump (u : 'u Universe_server.t) (worlds : Transport.t list) (n : int) : string list list =
  let got = Array.make (List.length worlds) [] in
  for _ = 1 to n do
    Universe_server.step u;
    List.iteri (fun i (w : Transport.t) -> got.(i) <- got.(i) @ w.receive ()) worlds;
    Unix.sleepf 0.001
  done;
  Array.to_list got

(* a tiny universe: the worlds connected, in order *)
let on_new (u : int list) (w : int) = (u @ [ w ], (w, "welcome") :: List.map (fun o -> (o, "someone came")) u, [])
let on_msg (u : int list) (w : int) (m : string) = (u, [ (w, String.uppercase_ascii m) ], [])
let on_disconnect (u : int list) (w : int) = (let u = List.filter (( <> ) w) u in (u, List.map (fun o -> (o, "someone left")) u, []))

let tests (caps : < Cap.network ; .. >) =
  Testo.categorize "Universe_server"
    [
      Testo.create "on_new, on_msg, on_disconnect" (fun () ->
          let u, port = Universe_server.create caps ~port:0 [] ~on_new ~on_msg ~on_disconnect () in
          let a = Relay_client.connect caps ~host:"127.0.0.1" ~port in
          Alcotest.(check (list (list string))) "a greeted" [ [ "welcome" ] ] (pump u [ a ] 50);
          let b = Relay_client.connect caps ~host:"127.0.0.1" ~port in
          Alcotest.(check (list (list string))) "b greeted, a told" [ [ "someone came" ]; [ "welcome" ] ] (pump u [ a; b ] 50);
          b.send "hello";
          Alcotest.(check (list (list string))) "the answer to b only" [ []; [ "HELLO" ] ] (pump u [ a; b ] 50);
          Alcotest.(check int) "two worlds" 2 (List.length (Universe_server.state u));
          (* a third world that shakes hands, then hangs up: a raw
           * socket, since a Transport has no close *)
          let fd = Tcp.connect caps ~host:"127.0.0.1" ~port () in
          Tcp.send_all fd (Websocket.request ~host:"127.0.0.1" ~path:"/" ~key:"dGhlIHNhbXBsZSBub25jZQ==");
          ignore (pump u [ a; b ] 50);
          Alcotest.(check int) "three worlds" 3 (List.length (Universe_server.state u));
          Unix.close fd;
          Alcotest.(check (list (list string))) "the others told" [ [ "someone left" ]; [ "someone left" ] ] (pump u [ a; b ] 50);
          Alcotest.(check int) "two again" 2 (List.length (Universe_server.state u)));
    ]
