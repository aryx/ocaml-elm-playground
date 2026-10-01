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

(* the universe's event loop and the worlds' frames, a few milliseconds:
 * n rounds at least, then until what is awaited is there (5 seconds at
 * most), as Unit_relay's pump *)
let pump ?(until : string list list -> bool = fun _ -> true) (u : 'u Universe_server.t) (worlds : Transport.t list) (n : int) : string list list =
  let got = Array.make (List.length worlds) [] in
  let rec go i =
    if i < n || (i < 5000 && not (until (Array.to_list got))) then begin
      Universe_server.step u;
      List.iteri (fun i (w : Transport.t) -> got.(i) <- got.(i) @ w.receive ()) worlds;
      Unix.sleepf 0.001;
      go (i + 1)
    end
  in
  go 0;
  Array.to_list got

(* what the worlds should have received, awaited then checked *)
let received (label : string) (expected : string list list) (u : 'u Universe_server.t) (worlds : Transport.t list) : unit =
  Alcotest.(check (list (list string))) label expected (pump ~until:(( = ) expected) u worlds 50)

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
          received "a greeted" [ [ "welcome" ] ] u [ a ];
          let b = Relay_client.connect caps ~host:"127.0.0.1" ~port in
          received "b greeted, a told" [ [ "someone came" ]; [ "welcome" ] ] u [ a; b ];
          b.send "hello";
          received "the answer to b only" [ []; [ "HELLO" ] ] u [ a; b ];
          Alcotest.(check int) "two worlds" 2 (List.length (Universe_server.state u));
          (* a third world that shakes hands, then hangs up: a raw
           * socket, since a Transport has no close *)
          let fd = Tcp.connect caps ~host:"127.0.0.1" ~port () in
          Tcp.send_all fd (Websocket.request ~host:"127.0.0.1" ~path:"/" ~key:"dGhlIHNhbXBsZSBub25jZQ==");
          received "the others told of it" [ [ "someone came" ]; [ "someone came" ] ] u [ a; b ];
          Alcotest.(check int) "three worlds" 3 (List.length (Universe_server.state u));
          Unix.close fd;
          received "the others told" [ [ "someone left" ]; [ "someone left" ] ] u [ a; b ];
          Alcotest.(check int) "two again" 2 (List.length (Universe_server.state u)));
    ]
