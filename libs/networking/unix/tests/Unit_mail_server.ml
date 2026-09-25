(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_mail_server.mli *)

(* 2026-09-25 12:00 in Greenwich *)
let now = 1790337600.

(* the server and a client machine, stepped together until the client
 * is finished (or 300 rounds): what the machine said at the end *)
let drive (server : Mail_server.t) (conn : Transport.t) (step : 'c -> string -> 'c * string list) (finished : 'c -> 'r option) (c : 'c) : 'r =
  let rec go c n =
    match finished c with
    | Some r -> r
    | None when n = 0 -> Alcotest.fail "the session never ended"
    | None ->
        Mail_server.step server ~now;
        let c = List.fold_left (fun c l -> let c, out = step c l in List.iter conn.send out; c) c (conn.receive ()) in
        Unix.sleepf 0.001;
        go c (n - 1)
  in
  go c 300

let send caps server port (envelopes : Smtp.envelope list) =
  drive server (Relay_client.connect caps ~host:"127.0.0.1" ~port) Smtp.step Smtp.finished (Smtp.client ~hello:"eudora" envelopes)

let check caps server port ~user ~pass =
  drive server (Relay_client.connect caps ~host:"127.0.0.1" ~port) Pop3.step Pop3.finished (Pop3.client ~user ~pass ~leave:false ~known:[] ())

let letter = Mail.make [ ("From", "Alice <alice@tiny>"); ("To", "bob@tiny"); ("Subject", "lunch") ] "Noon?\n.a dot\n"

let tests (caps : < Cap.network ; .. >) =
  Testo.categorize "Mail_server"
    [
      Testo.create "alice sends, bob checks; checked again, nothing" (fun () ->
          let server, smtp, pop = Mail_server.create caps ~smtp_port:0 ~pop_port:0 () in
          (match send caps server smtp [ Smtp.envelope ~sender:"alice@tiny" letter ] with
          | Ok [ Smtp.Sent (1, []) ] -> ()
          | _ -> Alcotest.fail "not sent");
          Alcotest.(check int) "in bob's maildrop" 1 (List.length (Mail_server.maildrop server "bob"));
          (match check caps server pop ~user:"bob" ~pass:"x" with
          | Ok [ (_, text) ] ->
              let m = Mail.parse text in
              Alcotest.(check (option string)) "the subject" (Some "lunch") (Mail.get m "subject");
              Alcotest.(check string) "the body, its dot kept" "Noon?\n.a dot\n" m.body;
              Alcotest.(check bool) "a Received: line on top" true ((List.hd m.fields).name = "Received")
          | _ -> Alcotest.fail "not fetched");
          Alcotest.(check int) "deleted at QUIT" 0 (List.length (Mail_server.maildrop server "bob"));
          match check caps server pop ~user:"bob" ~pass:"x" with Ok [] -> () | _ -> Alcotest.fail "fetched twice");
      Testo.create "not a relay: mail for elsewhere refused, 550" (fun () ->
          let server, smtp, _ = Mail_server.create caps ~smtp_port:0 ~pop_port:0 () in
          match send caps server smtp [ { Smtp.sender = "alice@tiny"; recipients = [ "someone@elsewhere.example" ]; text = Mail.to_string letter } ] with
          | Ok [ Smtp.Refused _ ] -> ()
          | _ -> Alcotest.fail "relayed");
      Testo.create "dropped after DELE, before QUIT: nothing deleted" (fun () ->
          let server, smtp, pop = Mail_server.create caps ~smtp_port:0 ~pop_port:0 () in
          ignore (send caps server smtp [ Smtp.envelope ~sender:"alice@tiny" letter ]);
          (* a WebSocket by hand, to close it: the handshake, masked frames *)
          let session lines =
            let fd = Tcp.connect caps ~host:"127.0.0.1" ~port:pop () in
            let pump () = for _ = 1 to 20 do Mail_server.step server ~now; Unix.sleepf 0.001 done in
            Tcp.send_all fd (Websocket.request ~host:"127.0.0.1" ~path:"/" ~key:"dGhlIHNhbXBsZSBub25jZQ==");
            pump ();
            List.iter (fun l -> Tcp.send_all fd (Websocket.encode ~mask:"abcd" { fin = true; opcode = Websocket.Binary; payload = l })) lines;
            pump ();
            Unix.close fd;
            pump ()
          in
          session [ "USER bob"; "PASS x"; "DELE 1" ];
          Alcotest.(check int) "still there" 1 (List.length (Mail_server.maildrop server "bob"));
          (* the same, with QUIT: deleted -- so the session above did reach it *)
          session [ "USER bob"; "PASS x"; "DELE 1"; "QUIT" ];
          Alcotest.(check int) "gone after QUIT" 0 (List.length (Mail_server.maildrop server "bob")));
      Testo.create "a wrong password" (fun () ->
          let server, _, pop = Mail_server.create caps ~smtp_port:0 ~pop_port:0 ~passwords:[ ("bob", "secret") ] () in
          match check caps server pop ~user:"bob" ~pass:"guess" with Error _ -> () | Ok _ -> Alcotest.fail "let in");
    ]
